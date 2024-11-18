open Lwt.Syntax

module Header = struct
  type t = {
    length : int;
    file_crc : Checkseum.Crc32.t;
  }
  (* structure:
       magic header '\x1f\x1f'
       file length : uint64;
       file_crc32 : uint32;
       header_crc32 : uint32;
       reserved : remaining space of sector
  *)

  let magic = 0x1F1F

  let digest_size = 4

  let length = 2 + 8 + digest_size + digest_size

  let empty = "\x1f\x1f" ^ String.init (length - 2) (Fun.const '\000')

  let create data =
    let length = String.length data in
    let file_crc = Checkseum.Crc32.digest_string data 0 length Checkseum.Crc32.default in
    { length; file_crc }

  let unmarshal buf =
    if Cstruct.length buf < length then raise (Invalid_argument "Header.unmarshal: Buffer too short");
    let magic' = Cstruct.BE.get_uint16 buf 0 in
    let data_length = Cstruct.BE.get_uint64 buf 2 in
    let file_crc = Optint.of_unsigned_int32 (Cstruct.BE.get_uint32 buf (2 + 8)) in
    let crc = Optint.of_unsigned_int32 (Cstruct.BE.get_uint32 buf (2 + 8 + digest_size)) in
    let crc' =
      Checkseum.Crc32.digest_bigstring buf.buffer buf.off
        (2 + 8 + digest_size) Checkseum.Crc32.default
    in
    (* XXX: check whole buffer is zero? *)
    if (magic' = 0 || magic' = magic) &&
       data_length = 0L && Optint.(equal zero file_crc) && Optint.(equal zero crc) then
      Ok None (* if it's all zeroed we treat it as empty *)
    else if magic' <> magic then
      Error "Not a OneFFS"
    else if not (Checkseum.Crc32.equal crc crc') then
      Error "Bad CRC"
    else
      match Int64.unsigned_to_int data_length with
      | Some length when length >= 0 ->
        Ok (Some { length; file_crc })
      | _ ->
        Error "Length too long"

  let marshal t buf =
    if Cstruct.length buf < length then raise (Invalid_argument "Header.marshal: Buffer too short");
    Cstruct.BE.set_uint16 buf 0 magic;
    Cstruct.BE.set_uint64 buf 2 (Int64.of_int t.length);
    Cstruct.BE.set_uint32 buf (2 + 8) (Optint.to_int32 t.file_crc);
    let crc = Checkseum.Crc32.digest_bigstring buf.buffer buf.off (2 + 8 + digest_size) Checkseum.Crc32.default in
    Cstruct.BE.set_uint32 buf (2 + 8 + digest_size) (Optint.to_int32 crc)
end

module Make(B : Mirage_block.S) = struct
  type t = {
    b : B.t;
    info : Mirage_block.info;
    mutable f : Header.t option;
    empty_header : Cstruct.t;
    mutex : Lwt_mutex.t;
  }

  type error = [ `Block of B.error | `Bad_checksum ]

  type write_error = B.write_error

  let pp_error ppf = function
    | `Block e -> B.pp_error ppf e
    | `Bad_checksum -> Fmt.pf ppf "Bad checksum"

  let pp_write_error = B.pp_write_error

  let is_set t = Option.is_some t.f

  let write t s =
    let (let*?) = Lwt_result.bind in
    Lwt_mutex.with_lock t.mutex @@ fun () ->
    (* First invalidate current file *)
    t.f <- None;
    let*? () =
      B.write t.b 0L [t.empty_header]
    in
    let sectors =
      let sector_size = Int64.of_int t.info.sector_size in
      Int64.(to_int (div (add (of_int (String.length s)) (pred sector_size))
                       sector_size))
    in
    let buf = Cstruct.create (succ sectors * t.info.sector_size) in
    Cstruct.blit_from_string s 0 buf t.info.sector_size (String.length s);
    let bufs = List.init sectors (fun i -> Cstruct.sub buf (succ i * t.info.sector_size) t.info.sector_size) in
    let*? () = B.write t.b 1L bufs in
    let header = Header.create s in
    let buf = Cstruct.sub buf 0 t.info.sector_size in
    Header.marshal header buf;
    let*? () = B.write t.b 0L [buf] in
    t.f <- Some header;
    Lwt_result.return ()

  let stream_write t f =
    let (let*?) = Lwt_result.bind in
    Lwt_mutex.with_lock t.mutex @@ fun () ->
    (* First invalidate current file *)
    t.f <- None;
    let*? () =
      B.write t.b 0L [t.empty_header]
    in
    let mutex = Lwt_mutex.create () in
    let sector_size = t.info.sector_size in
    let buf = Cstruct.create sector_size
    and written = ref 0
    and crc = ref Checkseum.Crc32.default
    and pending = ref 0 in
    let rec append data off =
      if String.length data = off then
        Lwt.return (Ok ())
      else
        let l = min (sector_size - !pending) (String.length data - off) in
        Cstruct.blit_from_string data 0 buf !pending l;
        pending := !pending + l;
        if !pending = sector_size then
          let sector = Int64.of_int (!written / sector_size + 1) in
          let*? () = B.write t.b sector [ buf ] in
          pending := 0;
          written := !written + sector_size;
          crc := Checkseum.Crc32.digest_bigstring buf.buffer buf.off sector_size !crc;
          append data (off + l)
        else
          Lwt.return (Ok ())
    in
    let append data =
      Lwt_mutex.with_lock mutex @@ fun () ->
      append data 0
    in
    let*? v = f append in
    let*? () =
      if !pending > 0 then
        let sector = Int64.of_int (!written / sector_size + 1) in
        Cstruct.memset (Cstruct.shift buf !pending) 0;
        let*? () = B.write t.b sector [ buf ] in
        written := !written + !pending;
        crc := Checkseum.Crc32.digest_bigstring buf.buffer buf.off sector_size !crc;
        Lwt.return (Ok ())
      else
        Lwt.return (Ok ())
    in
    let hdr = { Header.length = !written; file_crc = !crc } in
    Header.marshal hdr buf;
    let*? () = B.write t.b 0L [ buf ] in
    t.f <- Some hdr;
    Lwt.return (Ok v)

  let b_read b sector_start bufs =
    Lwt_result.map_error (fun e -> `Block e)
      (B.read b sector_start bufs)

  let read t =
    let (let*?) = Lwt_result.bind in
    match t.f with
    | None -> Lwt_result.return None
    | Some { Header.length; file_crc } ->
      let sector_size = t.info.Mirage_block.sector_size in
      let sectors =
        let sector_size = Int64.of_int sector_size in
        Int64.(to_int (div (add (of_int length) (pred sector_size)) sector_size))
      in
      let buf = Cstruct.create (sectors * sector_size) in
      let bufs =
        List.init sectors
          (fun i -> Cstruct.sub buf (i * sector_size) sector_size)
      in
      let*? () = b_read t.b 1L bufs in
      let crc =
        Checkseum.Crc32.digest_bigstring buf.buffer buf.off length
          Checkseum.Crc32.default
      in
      if Optint.equal crc file_crc then
        let s = Cstruct.to_string ~len:length buf in
        Lwt_result.return (Some s)
      else
        Lwt_result.fail `Bad_checksum

  let stream_read t =
    let (let*?) = Lwt_result.bind in
    match t.f with
    | None -> None
    | Some { Header.length; file_crc } ->
      let off = ref 0
      and crc = ref Checkseum.Crc32.default in
      let buf = Cstruct.create t.info.sector_size in
      let read () =
        if !off = length then
          if Checkseum.Crc32.equal file_crc !crc then
            Lwt.return (Ok `End_of_file)
          else
            Lwt.return (Ok `Bad_checksum)
        else
          let sector = Int64.of_int (!off / t.info.sector_size) in
          let len = min t.info.sector_size (length - !off) in
          off := !off + len;
          let*? () = b_read t.b sector [ buf ] in
          let data = Cstruct.to_string ~off:0 ~len buf in
          crc := Checkseum.Crc32.digest_string data 0 (String.length data) !crc;
          Lwt.return (Ok (`Data data))
      in
      Some read

  let format b =
    let* info = B.get_info b in
    let buf = Cstruct.create info.sector_size in
    Cstruct.blit_from_string Header.empty 0 buf 0 (String.length Header.empty);
    B.write b 0L [buf]

  let size t =
    Option.map (fun h -> h.Header.length) t.f

  let reset t =
    B.write t.b 0L [t.empty_header]

  let connect b =
    let* info = B.get_info b in
    if info.Mirage_block.sector_size < Header.length
    then raise (Invalid_argument "Block size too small");
    let buf = Cstruct.create info.sector_size in
    let* r = B.read b 0L [buf] in
    let () =
      match r with
      | Ok () -> ()
      | Error e -> Format.kasprintf failwith "OneFFS.connect: %a" B.pp_error e
    in
    match Header.unmarshal buf with
    | Error msg ->
      Printf.ksprintf Lwt.fail_with "bad header: %s" msg
    | Ok f ->
      (* Reuse the buffer for the empty header *)
      Cstruct.memset buf 0;
      Cstruct.blit_from_string Header.empty 0 buf 0 (String.length Header.empty);
      let mutex = Lwt_mutex.create () in
      Lwt.return { b; info; f; empty_header = buf; mutex }
end
