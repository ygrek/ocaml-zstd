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

(**
   [decompress orig_size ?dict s output]
   decompress into the given [output] string. @raise [Invalid_argument] if the string is not large enough *)
val decompress_blit : int -> ?dict:string -> string -> string -> int
