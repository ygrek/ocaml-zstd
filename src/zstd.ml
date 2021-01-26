module Z = Zstd_stubs.Bindings(Zstd_generated)
include Z

module Size_t = Unsigned.Size_t

exception Error of string

let version () =
  let n = versionNumber () in
  (n/10_000, (n/100) mod 100, n mod 100)

let bracket res destroy k =
  let r = try k res with exn -> let () = destroy res in raise exn in
  let () = destroy res in
  r

let check r = if isError r then raise (Error (getErrorName r))

let free_cctx x = check (free_cctx x)
let free_dctx x = check (free_dctx x)

let check_size_offset size offset len =
  if offset < 0 then raise (Invalid_argument "negative offset");
  let size = match size with None -> len - offset | Some s when s < 0 -> raise (Invalid_argument "negative size") | Some s -> s in
  if size + offset > len then raise (Invalid_argument "given size + offset is bigger than string len");
  size, offset

let compress ~level ?dict ?(offset=0) ?size s =
  let open Ctypes in
  let len = (String.length s) in
  let dst_size = compressBound (Size_t.of_int len) in
  let dst = allocate_n char ~count:(Size_t.to_int dst_size) in
  let size, offset = check_size_offset size offset len in
  let len = Size_t.of_int size in
  let offset = Size_t.of_int offset in
  let r =
    match dict with
    | None -> do_compress (to_voidp dst) dst_size (ocaml_string_start s) offset len level
    | Some dict ->
      let dlen = Size_t.of_int (String.length dict) in
      bracket (create_cctx ()) free_cctx begin fun cctx ->
        do_compress_dict cctx (to_voidp dst) dst_size (ocaml_string_start s) offset len dict dlen level
      end
  in
  check r;
  string_from_ptr dst (Size_t.to_int r)

let decompress orig ?dict ?(offset=0) ?size s =
  let open Ctypes in
  let size, offset = check_size_offset size offset (String.length s) in
  let offset = Size_t.of_int offset in
  let dst = allocate_n char ~count:orig in
  let r =
    match dict with
    | None -> do_decompress (to_voidp dst) (Size_t.of_int orig) (ocaml_string_start s) offset (Size_t.of_int size)
    | Some dict ->
      let dlen = Size_t.of_int (String.length dict) in
      bracket (create_dctx ()) free_dctx begin fun dctx ->
        do_decompress_dict dctx (to_voidp dst) (Size_t.of_int orig) (ocaml_string_start s) offset (Size_t.of_int size) dict dlen
      end
  in
  check r;
  string_from_ptr dst (Size_t.to_int r)
