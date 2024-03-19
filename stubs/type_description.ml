module Types (F : Ctypes.TYPE) = struct
  open F

  let content_size_unknown = constant "ZSTD_CONTENTSIZE_UNKNOWN" ullong
  let content_size_error = constant "ZSTD_CONTENTSIZE_ERROR" ullong
end
