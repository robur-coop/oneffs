open Lwt.Syntax

let error_msgf fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt

module FS = OneFFS.Make(Block)

let connect block_size file =
  let* b = Block.connect ~prefered_sector_size:(Some block_size) file in
  FS.connect b

let get block_size file =
  let* b = connect block_size file in
  let* r = FS.read b in
  match r with
  | Ok Some data ->
    (* XXX: print newline or not? *)
    print_string data;
    Lwt.return_unit
  | Ok None ->
    prerr_endline "No data.";
    exit 1
  | Error e ->
    FS.pp_error Format.err_formatter e;
    exit 2

let set block_size file =
  let* b = connect block_size file in
  let r = Buffer.create 4096 in
  let buf = Bytes.create 4096 in
  let rec loop () =
    let len = input stdin buf 0 4096 in
    if len = 0 then
      Buffer.contents r
    else begin
      Buffer.add_subbytes r buf 0 len;
      loop ()
    end
  in
  let* r = FS.write b (loop ()) in
  match r with
  | Ok () -> Lwt.return_unit
  | Error e ->
    FS.pp_write_error Format.err_formatter e;
    exit 2

let get_info block_size file =
  let+ b = connect block_size file in
  match FS.size b with
  | Some size ->
    Printf.printf "Set and contains %d bytes of data.\n" size
  | None ->
    print_endline "Unset."

let crc_of_file filename =
  let ic = open_in filename in
  let finally () = close_in ic in
  Fun.protect ~finally @@ fun () ->
  let buf = Bytes.create 0x7ff in
  let rec go crc = match input ic buf 0 (Bytes.length buf) with
    | 0 -> crc
    | len -> go (Checkseum.Crc32.digest_bytes buf 0 len crc)
    | exception End_of_file -> crc in
  go Checkseum.Crc32.default

let create block_size oneffs file =
  let ic = open_in file in
  let finally () = close_in ic in
  Fun.protect ~finally @@ fun () ->
  let length = in_channel_length ic in
  (* let length = ((ln + (block_size - 1)) / block_size) * block_size in *)
  let file_crc = crc_of_file file in
  let hdr = OneFFS.Header.{ length; file_crc } in
  let sector0 = Cstruct.create block_size in
  OneFFS.Header.marshal hdr sector0;
  let oc = open_out oneffs in
  let finally () = close_out oc in
  Fun.protect ~finally @@ fun () ->
  output_string oc (Cstruct.to_string sector0);
  let buf = Bytes.make block_size '\000' in
  let rec go off =
    if off < length then begin
      let len = min (Bytes.length buf) (length - off) in
      really_input ic buf 0 len;
      if len < block_size
      then Bytes.fill buf len (block_size - len) '\000';
      output_bytes oc buf; go (off + len)
    end in
  go 0

open Cmdliner

let block_size =
  let doc = "Block size of filesystem." in
  Arg.(value & opt int 512 & info ["b"; "block-size"] ~docv:"BLOCKSIZE" ~doc)

let oneffs_file =
  let doc = "File with OneFFS filesystem." in
  Arg.(required & pos 0 (some file) None & info [] ~docv:"ONEFFS" ~doc)

let new_oneffs_file =
  let doc = "New file with OneFFS filesystem." in
  let parser str =
    if Sys.file_exists str
    then error_msgf "%s already exists" str
    else Ok str in
  Arg.(required & opt (some (conv (parser, Fmt.string))) None & info [ "o"; "output" ] ~docv:"ONEFFS" ~doc)

let existing_file =
  let doc = "The input file to fill a OneFFS filesystem with." in
  Arg.(required & opt (some file) None & info [ "i"; "input" ] ~docv:"FILE" ~doc)

let get_cmd =
  let doc = "Get contents, if any, of OneFFS." in
  let man = [
    `S Manpage.s_description;
    `P "Get the contents of a OneFFS filesystem. If the filesystem is unset a
    message is printed on stderr and the exit status is 1.";
  ] in
  let info = Cmd.info "get" ~doc ~man in
  Cmd.v info Term.(const Lwt_main.run $ (const get $ block_size $ oneffs_file))

let set_cmd =
  let doc = "Set contents of OneFFS." in
  let info = Cmd.info "set" ~doc in
  Cmd.v info Term.(const Lwt_main.run $ (const set $ block_size $ oneffs_file))

let info_cmd =
  let doc = "Get information of OneFFS." in
  let info = Cmd.info "info" ~doc in
  Cmd.v info Term.(const Lwt_main.run $ (const get_info $ block_size $ oneffs_file))

let create_cmd =
  let doc = "Create a new OneFFS image from a file." in
  let info = Cmd.info "create" ~doc in
  Cmd.v info Term.(const create $ block_size $ new_oneffs_file $ existing_file)

let main_cmd =
  let doc = "OneFFS tool" in
  let info = Cmd.info "oneffs" ~version:"%%VERSION%%" ~doc in
  let default = Term.(ret (const (`Help (`Pager, None)))) in
  Cmd.group info ~default [get_cmd; set_cmd; info_cmd; create_cmd]

let () =
  exit (Cmd.eval main_cmd)
