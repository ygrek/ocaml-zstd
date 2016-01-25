open Ctypes

module Bindings
  (F : sig type _ fn
           val foreign : string -> ('a -> 'b) Ctypes.fn -> ('a -> 'b) fn end) =
struct
  open F

  let versionNumber = foreign "ZSTD_versionNumber" (void @-> returning int)
  let compressBound = foreign "ZSTD_compressBound" (size_t @-> returning size_t)
  let getErrorName = foreign "ZSTD_getErrorName" (size_t @-> returning string)
  let isError = foreign "ZSTD_isError" (size_t @-> returning bool)

  let do_compress = foreign "ZSTD_compress" (ptr void @-> size_t @-> string @-> size_t @-> int @-> returning size_t)
  let do_decompress = foreign "ZSTD_decompress" (ptr void @-> size_t @-> string @-> size_t @-> returning size_t)
end
