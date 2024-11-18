module Header : sig
  type t =
    { length : int
    ; file_crc : Checkseum.Crc32.t }

  val create : string -> t
  val unmarshal : Cstruct.t -> (t option, string) result
  val marshal : t -> Cstruct.t -> unit
end

module Make(B : Mirage_block.S) : sig
  type t

  type error = [ `Block of B.error | `Bad_checksum ]

  type write_error = B.write_error

  val pp_error : error Fmt.t

  val pp_write_error : write_error Fmt.t

  val write : t -> string -> (unit, write_error) result Lwt.t
  (** [write fs data] stores [data]. An error is returned if writing to the
      underlying block device fails. *)

  val read : t -> (string option, error) result Lwt.t
  (** [read fs] reads the data stored if any. An error is returned if the
      checksum is bad or if the read fails. *)

  val stream_write : t -> ((string -> (unit, write_error) result Lwt.t) ->
                           ('a, write_error) result Lwt.t) ->
    ('a, write_error) result Lwt.t
  (** [stream_write t f] calls [f append] where [append] is a function that
      appends data to a fresh file. Once the [f append] task is resolved the file
      is committed and the result is returned. *)

  val stream_read : t ->
    (unit ->
     ([> `Bad_checksum | `Data of string | `End_of_file ],
      [> `Block of B.error ])
       Lwt_result.t)
      option
  (** [stream_read t] is [None] if [is_set t] is false. Otherwise, it is
      [Some read] where [read] can be called repeatedly to read sectors of the
      file at a time. [read ()] returns [Ok (`Data data)] when there is more
      data to be read. At the end of the file either [Ok `End_of_file] or [Ok
      `Bad_checksum] is returned depending on whether the checksum for the data
      was as expected.

      It is an error to write while reading. *)

  val is_set : t -> bool
  (**  [is_set fs] is true if [fs] has any data. *)

  val size : t -> int option
  (** [size fs] is [Some size] if the contents is [size] bytes long, or [None] if unset. *)

  val reset : t -> (unit, write_error) result Lwt.t
  (** [reset fs] sets [fs] to no data. *)

  val format : B.t -> (unit, write_error) result Lwt.t
  (** [format b] writes an empty header at the beginning of [b]. Note that this
      is destructive. *)

  val connect : B.t -> t Lwt.t
end
