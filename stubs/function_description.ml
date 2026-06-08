open Ctypes

module Types = Types_generated

module Functions (F : Cstubs.FOREIGN) = struct
  open F

  let version_number = foreign "ZSTD_versionNumber" (void @-> returning int)
  let compress_bound = foreign "ZSTD_compressBound" (size_t @-> returning size_t)
  let get_error_name = foreign "ZSTD_getErrorName" (size_t @-> returning string)
  let is_error = foreign "ZSTD_isError" (size_t @-> returning bool)

  let get_frame_content_size = foreign "ZSTD_getFrameContentSize" (string @-> size_t @-> returning ullong)

  let compress = foreign "ZSTD_compress" (ptr void @-> size_t @-> string @-> size_t @-> int @-> returning size_t)
  let decompress = foreign "ZSTD_decompress" (ptr void @-> size_t @-> string @-> size_t @-> returning size_t)

  let cctx : [`CCtx] structure typ = structure "ZSTD_CCtx_s"
  let create_cctx = foreign "ZSTD_createCCtx" (void @-> returning (ptr cctx))
  let free_cctx = foreign "ZSTD_freeCCtx" (ptr cctx @-> returning size_t)

  let compress_cctx = foreign "ZSTD_compressCCtx" (ptr cctx @-> ptr void @-> size_t @-> string @-> size_t @-> int @-> returning size_t)

  let dctx : [`DCtx] structure typ = structure "ZSTD_DCtx_s"
  let create_dctx = foreign "ZSTD_createDCtx" (void @-> returning (ptr dctx))
  let free_dctx = foreign "ZSTD_freeDCtx" (ptr dctx @-> returning size_t)

  let decompress_dctx = foreign "ZSTD_decompressDCtx" (ptr dctx @-> ptr void @-> size_t @-> string @-> size_t @-> returning size_t)

  let compress_using_dict = foreign "ZSTD_compress_usingDict" (ptr cctx @-> ptr void @-> size_t @-> string @-> size_t @->
    string @-> size_t @-> int @-> returning size_t)

  let decompress_using_dict = foreign "ZSTD_decompress_usingDict" (ptr dctx @-> ptr void @-> size_t @-> string @-> size_t @->
    string @-> size_t @-> returning size_t)

  let in_buffer : [`InBuffer] structure typ = structure "ZSTD_inBuffer_s"
  let in_buffer_src = field in_buffer "src" (ptr void)
  let in_buffer_size = field in_buffer "size" size_t
  let in_buffer_pos = field in_buffer "pos" size_t
  let () = seal in_buffer

  let out_buffer : [`OutBuffer] structure typ = structure "ZSTD_outBuffer_s"
  let out_buffer_dst = field out_buffer "dst" (ptr void)
  let out_buffer_size = field out_buffer "size" size_t
  let out_buffer_pos = field out_buffer "pos" size_t
  let () = seal out_buffer

  let cstream_in_size = foreign "ZSTD_CStreamInSize" (void @-> returning size_t)
  let cstream_out_size = foreign "ZSTD_CStreamOutSize" (void @-> returning size_t)
  let dstream_in_size = foreign "ZSTD_DStreamInSize" (void @-> returning size_t)
  let dstream_out_size = foreign "ZSTD_DStreamOutSize" (void @-> returning size_t)

  let compress_stream2 = foreign "ZSTD_compressStream2"
    (ptr cctx @-> ptr out_buffer @-> ptr in_buffer @-> int @-> returning size_t)
  let decompress_stream = foreign "ZSTD_decompressStream"
    (ptr dctx @-> ptr out_buffer @-> ptr in_buffer @-> returning size_t)

  let cctx_set_parameter = foreign "ZSTD_CCtx_setParameter"
    (ptr cctx @-> int @-> int @-> returning size_t)
  let cctx_load_dictionary = foreign "ZSTD_CCtx_loadDictionary"
    (ptr cctx @-> string @-> size_t @-> returning size_t)
  let dctx_load_dictionary = foreign "ZSTD_DCtx_loadDictionary"
    (ptr dctx @-> string @-> size_t @-> returning size_t)

  let dctx_reset = foreign "ZSTD_DCtx_reset"
    (ptr dctx @-> int @-> returning size_t)

  let cctx_reset = foreign "ZSTD_CCtx_reset"
    (ptr cctx @-> int @-> returning size_t)
end
