module Z = Zstd_stubs.Bindings(Zstd_generated)
include Z

module Size_t = Unsigned.Size_t
module UInt = Unsigned.UInt

exception Error of string

let version () =
  let n = versionNumber () in
  (n/10_000, (n/100) mod 100, n mod 100)

let bracket res destroy k =
  let r = try k res with exn -> let () = destroy res in raise exn in
  let () = destroy res in
  r

let check r = if isError r then raise (Error (getErrorName r))
let checkdict r = if isDictError r then raise (Error (getDictErrorName r))

let free_cctx x = check (free_cctx x)
let free_dctx x = check (free_dctx x)

let generate_dictionary dict_size samples =
  let open Ctypes in
  let dst = allocate_n char ~count:dict_size in
  let nbSamples = UInt.of_int ( List.length samples) in
  let sizes = (CArray.of_list size_t) @@ List.map (fun x -> Size_t.of_int @@ String.length x) samples in
  let sizes = CArray.start sizes in
  let samples = samples |> String.concat "" in
  checkdict (do_train_from_buffer (to_voidp dst) (Size_t.of_int dict_size) samples sizes nbSamples);
  string_from_ptr dst dict_size

let compress ~level ?dict s =
  let open Ctypes in
  let len = Size_t.of_int (String.length s) in
  let dst_size = compressBound len in
  let dst = allocate_n char ~count:(Size_t.to_int dst_size) in
  let r =
    match dict with
    | None -> do_compress (to_voidp dst) dst_size s len level
    | Some dict ->
      let dlen = Size_t.of_int (String.length dict) in
      bracket (create_cctx ()) free_cctx begin fun cctx ->
        do_compress_dict cctx (to_voidp dst) dst_size s len dict dlen level
      end
  in
  check r;
  string_from_ptr dst (Size_t.to_int r)

let decompress orig ?dict s =
  let open Ctypes in
  let dst = allocate_n char ~count:orig in
  let r =
    match dict with
    | None -> do_decompress (to_voidp dst) (Size_t.of_int orig) s (Size_t.of_int @@ String.length s)
    | Some dict ->
      let dlen = Size_t.of_int (String.length dict) in
      bracket (create_dctx ()) free_dctx begin fun dctx ->
        do_decompress_dict dctx (to_voidp dst) (Size_t.of_int orig) s (Size_t.of_int @@ String.length s) dict dlen
      end
  in
  check r;
  string_from_ptr dst (Size_t.to_int r)
