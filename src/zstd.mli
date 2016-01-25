(** Zstandard - fast lossless compression algorithm *)

exception Error of string

val version : unit -> (int * int * int)

val compress : level:int -> string -> string

(**
  [decompress orig_size s]

  [orig_size] specifies size of buffer for decompression (not less than original size of uncompressed [s])
*)
val decompress : int -> string -> string
