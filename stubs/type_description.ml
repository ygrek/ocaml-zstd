module Types (F : Ctypes.TYPE) = struct
  open F

  let content_size_unknown = constant "ZSTD_CONTENTSIZE_UNKNOWN" ullong
  let content_size_error = constant "ZSTD_CONTENTSIZE_ERROR" ullong

  let e_continue = constant "ZSTD_e_continue" int
  let e_flush = constant "ZSTD_e_flush" int
  let e_end = constant "ZSTD_e_end" int
  let c_compressionLevel = constant "ZSTD_c_compressionLevel" int
  let c_checksumFlag = constant "ZSTD_c_checksumFlag" int

  let reset_session_only = constant "ZSTD_reset_session_only" int
end
