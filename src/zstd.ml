open Zstd_stubs

module Size_t = Unsigned.Size_t
module F = C.Functions
module T = C.Types

exception Error of string

let version () =
  let n = F.version_number () in
  (n / 10_000, (n / 100) mod 100, n mod 100)

let bracket res destroy k =
  let r = try k res with exn -> let () = destroy res in raise exn in
  let () = destroy res in
  r

let check r = if F.is_error r then raise (Error (F.get_error_name r))

let free_cctx x = check (F.free_cctx x)
let free_dctx x = check (F.free_dctx x)

let compress ~level ?dict s =
  let open Ctypes in
  let len = Size_t.of_int (String.length s) in
  let dst_size = F.compress_bound len in
  let dst = allocate_n char ~count:(Size_t.to_int dst_size) in
  let r =
    match dict with
    | None -> F.compress (to_voidp dst) dst_size s len level
    | Some dict ->
      let dlen = Size_t.of_int (String.length dict) in
      bracket (F.create_cctx ()) free_cctx begin fun cctx ->
        F.compress_using_dict cctx (to_voidp dst) dst_size s len dict dlen level
      end
  in
  check r;
  string_from_ptr dst ~length:(Size_t.to_int r)

let decompress orig ?dict s =
  let open Ctypes in
  let dst = allocate_n char ~count:orig in
  let r =
    match dict with
    | None -> F.decompress (to_voidp dst) (Size_t.of_int orig) s (Size_t.of_int (String.length s))
    | Some dict ->
      let dlen = Size_t.of_int (String.length dict) in
      bracket (F.create_dctx ()) free_dctx begin fun dctx ->
        F.decompress_using_dict dctx (to_voidp dst) (Size_t.of_int orig) s (Size_t.of_int (String.length s)) dict dlen
      end
  in
  check r;
  string_from_ptr dst ~length:(Size_t.to_int r)

let get_decompressed_size s =
  let r = F.get_frame_content_size s (Size_t.of_int (String.length s)) in
  if r = T.content_size_error then
    raise (Error "content size error")
  else if r = T.content_size_unknown then
    raise (Error "content size unknown")
  else
    Unsigned.ULLong.to_int r
