module Z = Zstd_stubs.Bindings(Zstd_generated)
include Z

module Size_t = Unsigned.Size_t

exception Error of string

let version () =
  let n = versionNumber () in
  (n/10_000, (n/100) mod 100, n mod 100)

let compress ~level s =
  let open Ctypes in
  let len = Size_t.of_int (String.length s) in
  let dst_size = compressBound len in
  let dst = allocate_n char ~count:(Size_t.to_int dst_size) in
  let r = do_compress (to_voidp dst) dst_size s len level in
  if isError r then raise (Error (getErrorName r));
  string_from_ptr dst (Size_t.to_int r)

let decompress orig s =
  let open Ctypes in
  let dst = allocate_n char ~count:orig in
  let r = do_decompress (to_voidp dst) (Size_t.of_int orig) s (Size_t.of_int @@ String.length s) in
  if isError r then raise (Error (getErrorName r));
  string_from_ptr dst (Size_t.to_int r)
