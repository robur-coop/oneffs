open Lwt.Syntax

module Make(Pclock : Mirage_clock.PCLOCK)(Block : Mirage_block.S) = struct
  module FS = Filesystem.Make(Pclock)(Block)

  type t = {
    b : Block.t;
    mutable superblock : FS.superblock;
  }

  let connect b =
    let* info = Block.get_info b in
    if info.sector_size < FS.superblock_size then
      failwith "sector size too small for this filesystem";
    let* r = FS.read_data b in
    match r with
    | Error `Bad_checksum ->
      let* r = FS.init b in
      (match r with
       | Ok superblock ->
         Lwt.return ({ superblock; b }, None)
       | Error `Msg e ->
         Printf.ksprintf failwith "error initializing the block device: %s" e)
    | Error `Msg e ->
      Printf.ksprintf failwith "error reading block device: %s" e
    | Error (#FS.decode_err as e) ->
      Format.kasprintf failwith "error reading block device: %a" FS.pp_decode_err e
    | Ok (superblock, data) ->
      let t = { superblock; b } in
      Lwt.return (t, Some data)

  let write t data =
    let+ r = FS.write_data t.b t.superblock data in
    Result.map (fun superblock -> t.superblock <- superblock) r

  let read t =
    let+ r = FS.read_data t.b in
    match r with
    | Ok (_superblock, data) ->
      (* The read [_superblock] /should/ be equal to [t.superblock] *)
      Ok data
    | Error (`Msg _ as e) -> Error e
    | Error (#FS.decode_err as e) ->
      Error (`Msg (Format.asprintf "%a" FS.pp_decode_err e))
end
