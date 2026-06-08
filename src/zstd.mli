(** Zstandard - fast lossless compression algorithm *)

exception Error of string

val version : unit -> (int * int * int)

(** [dict] optional pre-defined dictionary content (see dictBuilder) *)
val compress : level:int -> ?dict:string -> string -> string

(**
  [decompress orig_size ?dict s]

  [orig_size] specifies size of buffer for decompression (not less than original size of uncompressed [s])
  [dict] must be identical to the one used during compression, otherwise uncompressed data will be corrupted.
*)
val decompress : int -> ?dict:string -> string -> string

val get_decompressed_size : string -> int

(** {1 Streaming Interface} *)

(** {2 Streaming Compression} *)

module Compress_stream : sig
  type t

  val in_size : unit -> int
  val out_size : unit -> int

  (** [create ?level ?dict ~writer ()] creates a streaming compressor.

      [writer] receives compressed output chunks. The [bytes] buffer passed to
      [writer] is reused across calls; the contents are only valid for the
      duration of the callback. If [writer] raises an exception, the stream is
      automatically closed and the exception is re-raised. *)
  val create : ?level:int -> ?dict:string ->
    writer:(bytes -> int -> int -> unit) -> unit -> t

  val is_closed : t -> bool

  (** [write stream buf off len] compresses [len] bytes from [buf] starting
      at [off]. Compressed output is passed to the [writer] callback. *)
  val write : t -> bytes -> int -> int -> unit

  (** [flush stream] flushes buffered data. *)
  val flush : t -> unit

  (** [close stream] ends the frame and frees the context. Idempotent. *)
  val close : t -> unit
end

(** {2 Streaming Decompression} *)

module Decompress_stream : sig
  type t

  val in_size : unit -> int
  val out_size : unit -> int

  (** [create ?dict ~reader ()] creates a streaming decompressor.

      [reader] is called to obtain compressed input. If [reader] raises an
      exception, the stream is automatically closed and the exception is
      re-raised. *)
  val create : ?dict:string ->
    reader:(bytes -> int -> int -> int) -> unit -> t

  val is_closed : t -> bool

  (** [read stream buf off len] decompresses up to [len] bytes into [buf]
      starting at [off]. Returns 0 at end of stream. *)
  val read : t -> bytes -> int -> int -> int

  (** [close stream] frees the context. Idempotent. *)
  val close : t -> unit
end

(**/**)

(** Internal helpers shared with [Zstd.Seekable]. Not part of the public API;
    signatures and existence may change without notice. *)
module Internal : sig

  (** Low-level compression engine shared by [Compress_stream] and the
      seekable encoder. Owns a [ZSTD_CCtx] plus in/out buffers and exposes
      the per-frame control op [end_frame]. Not part of the public API. *)
  module Compress_engine : sig
    type t

    val create : ?level:int -> ?dict:string -> ?checksum:bool ->
                 writer:(bytes -> int -> int -> unit) ->
                 unit -> t

    val is_closed : t -> bool

    (** Caller is responsible for bounds-checking [off]/[len]. *)
    val write : t -> bytes -> int -> int -> unit

    val flush : t -> unit

    (** End the current frame with [ZSTD_e_end] then perform a
        session-only reset on the cctx. The engine remains live and is
        ready to start a new frame on the next [write]. Dictionary and
        compression parameters are preserved. *)
    val end_frame : t -> unit

    (** Idempotent. Drains the current frame with [ZSTD_e_end] before
        freeing the cctx — but only if a frame is actually open (i.e. the
        last operation was a [write], not an [end_frame]). After
        [end_frame], [close] just frees the cctx without emitting any
        further bytes. *)
    val close : t -> unit

    (** Idempotent. Frees the cctx without draining the current frame.
        Used by the seekable encoder: after writing the seek table footer,
        the engine must be torn down without emitting more bytes. *)
    val close_no_drain : t -> unit
  end

  (** Low-level decompression engine shared by [Decompress_stream] and the
      seekable decoder. Owns a [ZSTD_DCtx], a fixed-size input buffer and
      a small output cache. The engine is push/pull: callers refill the
      input via [push_input], advance one [decompress_stream] call with
      [step], then drain produced bytes with [drain] or skip them with
      [discard]. Not part of the public API. *)
  module Decompress_engine : sig
    type t

    val in_size : unit -> int
    val out_size : unit -> int

    val create : ?dict:string -> unit -> t
    val is_closed : t -> bool
    val close : t -> unit

    (** Capacity of the engine's input buffer (== [in_size ()]). *)
    val in_capacity : t -> int

    (** [true] iff the engine's input buffer is empty and ready to be
        [push_input]ed again. *)
    val needs_input : t -> bool

    (** Number of decoded bytes still sitting in the engine's output
        cache, available via [drain] / [discard]. *)
    val pending_output : t -> int

    (** Value returned by the most recent [decompress_stream] call:
        libzstd's hint for the minimum bytes needed to continue, or [0]
        at a clean frame boundary. *)
    val last_ret : t -> int

    (** Drop frame state: clears the input/output buffers and calls
        [ZSTD_DCtx_reset] with [reset_session_only]. Used by the
        seekable decoder after a [seek]. *)
    val session_reset : t -> unit

    (** [push_input engine buf off len] blits [len] bytes from [buf] into
        the engine's input buffer. Precondition: [needs_input engine] is
        [true] (otherwise existing buffered input would be overwritten).
        [len] must be in [\[0, in_capacity engine\]]. *)
    val push_input : t -> bytes -> int -> int -> unit

    (** [drain engine buf off cap] copies up to [cap] bytes from the
        output cache into [buf] at [off]; returns the number copied. *)
    val drain : t -> bytes -> int -> int -> int

    (** [discard engine n] drops up to [n] bytes from the output cache
        without copying them anywhere; returns the number dropped. *)
    val discard : t -> int -> int

    (** Runs one [decompress_stream] call. Returns
        [(consumed_new, produced)]: how many input bytes were consumed
        from the engine's input buffer just now, and how many new bytes
        landed in the output cache. Raises [Zstd.Error] on libzstd
        errors. *)
    val step : t -> int * int
  end
end

(**/**)
