(** Zstandard {{:https://github.com/facebook/zstd/blob/dev/contrib/seekable_format/zstd_seekable_compression_format.md}seekable format}.

    A seekable file is a sequence of independent zstd frames followed by a
    skippable frame holding a seek table. It remains a valid plain zstd
    stream, so any standard decoder (including {!Zstd.Decompress_stream}) can
    decode it linearly. *)

(** Seekable Compression *)
module Compress : sig
  type t

  type frame_size_policy =
    | Uncompressed of int  (** end a frame after writing this many uncompressed bytes *)
    | Compressed of int    (** end a frame after producing this many compressed bytes *)
    | Manual               (** never end a frame automatically; the caller drives
                               framing with {!force_end_frame}, typically based on
                               {!current_frame_size} *)

  (** [create ?level ?frame_size ?checksum ~writer ()] creates a seekable
      compressor.

      [frame_size] defaults to [Uncompressed (2 * 1024 * 1024)], matching the
      [zeekstd] default.

      [checksum] defaults to [false]. When [true], libzstd's
      [ZSTD_c_checksumFlag] is enabled, appending a 4-byte XXH64 digest to
      each zstd frame's epilogue.

      [writer] receives compressed output chunks. The [bytes] buffer passed to
      [writer] is reused across calls; the contents are only valid for the
      duration of the callback. *)
  val create :
    ?level:int ->
    ?frame_size:frame_size_policy ->
    ?checksum:bool ->
    writer:(bytes -> int -> int -> unit) ->
    unit -> t

  val is_closed : t -> bool

  (** [write t buf off len] compresses [len] bytes from [buf] starting at [off]. *)
  val write : t -> bytes -> int -> int -> unit

  (** [force_end_frame t] ends the current zstd frame now. A no-op when no
      uncompressed bytes have been buffered for the current frame. *)
  val force_end_frame : t -> unit

  (** [current_frame_size t] returns [(compressed, uncompressed)] byte counts
      accumulated in the frame currently being built, i.e. since the last
      [force_end_frame] or [create] or automatically emitted frame.

      The [compressed] figure tracks bytes already handed to the [writer]
      callback for this frame; libzstd may still be holding additional buffered
      bytes that will only flush when the frame ends. Useful with
      [frame_size = Manual] to align frame boundaries to item boundaries in
      structured input: write an item, check the size, and call
      {!force_end_frame} before writing the next one if it would push the
      frame past a target threshold. *)
  val current_frame_size : t -> int * int

  (** [add_skippable_frame t ~magic content i len] writes a zstd skippable frame
      (an 8-byte header followed by [content[i..i+len]] verbatim) to the output.
      [magic] must be in the skippable range [[0x184D2A50, 0x184D2A5F]].

      Skippable frames are ignored by every zstd decoder, so this is a way
      to embed arbitrary metadata (a custom header, dictionary id, …) in the
      stream. It may be called at any point — before, between, or after data
      frames. Each skippable frame is recorded in the seek table as its own
      entry (with a decompressed size of 0), so the cumulative frame offsets
      stay consistent and the decoder skips over it transparently. If a data
      frame is currently open, it is ended first so that frame order in the
      file matches seek-table-entry order. Raises {!Zstd.Error} if [t] is
      closed, and [Invalid_argument] if [magic] is out of range. *)
  val add_skippable_frame : t -> magic:int -> bytes -> int -> int -> unit

  (** [close t] ends the current frame and writes the seek table skippable
      frame. Idempotent. *)
  val close : t -> unit
end

(** Seekable Decompression *)
module Decompress : sig
  module Reader : sig
    (** An abstraction over the underlying file to read. We require the file to
        be seekable. If you can only stream then use [Zstd] since a seekable
        zstd file is also a valid zstd file. *)
    type t = {
      read : bytes -> int -> int -> int;
        (** [read buf off len] reads at most [len] bytes from the current
            position, returns the number of bytes actually read (0 on EOF). *)
      seek : int64 -> unit;
        (** [seek pos] moves the cursor to absolute byte offset [pos]. The
            offset is an [int64] so inputs larger than [max_int] are
            representable. *)
      length : unit -> int64;
        (** [length ()] returns the total size of the input in bytes. *)
    }

    val of_in_channel : in_channel -> t
    (** Use a (seekable) in channel, eg from [open_in_bin]. *)

    val of_string : string -> t
    (** Read from a byte string *)
  end

  (** Parsed seek table. *)
  type table

  (** [find_table reader] reads the trailing skippable frame and returns
      [Some table] when present. Returns [None] for any plain zstd file or
      malformed footer — this includes: file too short, missing or wrong
      magic, inconsistent skippable [Frame_Size], or declared frame sizes
      that do not fit in the space before the skippable frame (base < 0).
      Arbitrary leading content before the data frames is tolerated: the
      seek table records all indexed frames (data and skippable), and any
      unindexed bytes form a leading region at offset [base]. For files
      written by this library, every frame is indexed and [base = 0].
      Trailing bytes after the seek-table footer are NOT tolerated (the
      footer must be the last bytes of the file). [None] is therefore
      "no usable seek table here", not strictly "no magic present".
      Raises {!Zstd.Error} only on IO or libzstd errors. *)
  val find_table : Reader.t -> table option

  val num_frames : table -> int
  val decompressed_size : table -> int

  (** Sum of frame compressed sizes; excludes the seek-table skippable frame. *)
  val compressed_size : table -> int

  type frame_info = {
    compressed : int;
    decompressed : int;
    comp_offset : int;     (** absolute byte offset of the frame's start in the input *)
    uncomp_offset : int;   (** start offset of the frame in the uncompressed stream *)
  }

  val frame_info : table -> int -> frame_info

  val pp_frame_info : Format.formatter -> frame_info -> unit

  val pp_table : Format.formatter -> table -> unit
  (** Prints all frames of the table, for debug purposes. *)

  type t

  val create : Reader.t -> table -> t

  (** [seek t off] positions the decoder at uncompressed offset [off]. Offsets
      beyond [decompressed_size] are clamped to EOF. *)
  val seek : t -> int -> unit

  (** [read t buf off len] decompresses up to [len] bytes into [buf]. Returns
      [0] at EOF. *)
  val read : t -> bytes -> int -> int -> int

  val is_closed : t -> bool
  val close : t -> unit
end
