#include <stdint.h>
#include <zstd.h>

size_t OCAMLZSTD_compress_offset(void *dst, size_t dstCapacity, const void *src,
                                 size_t srcOffset, size_t srcSize,
                                 int compressionLevel) {
  return ZSTD_compress(dst, dstCapacity, src + srcOffset, srcSize,
                       compressionLevel);
}

size_t OCAMLZSTD_decompress_offset(void *dst, size_t dstCapacity,
                                   const void *src, size_t srcOffset,
                                   size_t compressedSize) {

  return ZSTD_decompress(dst, dstCapacity, src + srcOffset, compressedSize);
}

size_t OCAMLZSTD_compress_usingDict_offset(ZSTD_CCtx *ctx, void *dst,
                                           size_t dstCapacity, const void *src,
                                           size_t srcOffset, size_t srcSize,
                                           const void *dict, size_t dictSize,
                                           int compressionLevel) {

  return ZSTD_compress_usingDict(ctx, dst, dstCapacity, src + srcOffset,
                                 srcSize, dict, dictSize, compressionLevel);
}

size_t OCAMLZSTD_decompress_usingDict_offset(ZSTD_DCtx *dctx, void *dst,
                                             size_t dstCapacity,
                                             const void *src, size_t srcOffset,
                                             size_t srcSize, const void *dict,
                                             size_t dictSize) {
  ZSTD_decompress_usingDict(dctx, dst, dstCapacity, src + srcOffset, srcSize,
                            dict, dictSize);
}
