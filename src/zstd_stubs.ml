open Ctypes

module Bindings
  (F : Cstubs.FOREIGN) =
struct
  open F

  let versionNumber = foreign "ZSTD_versionNumber" (void @-> returning int)
  let compressBound = foreign "ZSTD_compressBound" (size_t @-> returning size_t)
  let getErrorName = foreign "ZSTD_getErrorName" (size_t @-> returning string)
  let isError = foreign "ZSTD_isError" (size_t @-> returning bool)

  let do_compress = foreign "ZSTD_compress" (ptr void @-> size_t @-> string @-> size_t @-> int @-> returning size_t)
  let do_decompress = foreign "ZSTD_decompress" (ptr void @-> size_t @-> string @-> size_t @-> returning size_t)

  let cctx : [`CCtx] structure typ = structure "ZSTD_CCtx_s"
  let create_cctx = foreign "ZSTD_createCCtx" (void @-> returning (ptr cctx))
  let free_cctx = foreign "ZSTD_freeCCtx" (ptr cctx @-> returning size_t)

  let do_compress_cctx = foreign "ZSTD_compressCCtx" (ptr cctx @-> ptr void @-> size_t @-> string @-> size_t @-> int @-> returning size_t)

  let dctx : [`DCtx] structure typ = structure "ZSTD_DCtx_s"
  let create_dctx = foreign "ZSTD_createDCtx" (void @-> returning (ptr dctx))
  let free_dctx = foreign "ZSTD_freeDCtx" (ptr dctx @-> returning size_t)

  let do_decompress_dctx = foreign "ZSTD_decompressDCtx" (ptr dctx @-> ptr void @-> size_t @-> string @-> size_t @-> returning size_t)

  let do_compress_dict = foreign "ZSTD_compress_usingDict" (ptr cctx @-> ptr void @-> size_t @-> string @-> size_t @->
    string @-> size_t @-> int @-> returning size_t)

  let do_decompress_dict = foreign "ZSTD_decompress_usingDict" (ptr dctx @-> ptr void @-> size_t @-> string @-> size_t @->
    string @-> size_t @-> returning size_t)

end
