(* Encoding throughput: raw zstd vs seekable zstd at several frame sizes.

   The input is produced on the fly by a streaming [source] that pushes 64 KiB
   chunks to a consumer callback (no large buffer is ever materialised beyond
   the 32 MiB pattern). Each compressor's [write] is plugged in as the
   consumer. The compressed output is discarded but counted, so we also report
   the compression ratio and the framing overhead of the seekable format.

   Usage: bench_seekable [SIZE_MiB=2048] [LEVEL=4] *)

let mib = 1024 * 1024
let chunk_bytes = 64 * 1024
let frame_sizes_mib = [ 2; 4; 16 ]

let lorem =
  "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod \
   tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim \
   veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea \
   commodo consequat. Duis aute irure dolor in reprehenderit in voluptate \
   velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint \
   occaecat cupidatat non proident, sunt in culpa qui officia deserunt \
   mollit anim id est laborum."

(* A 32 MiB block of pseudo-randomly ordered lorem-ipsum words, built once. *)
let source_size = 32 * mib

let pattern = begin
  let words =
    String.split_on_char ' ' lorem
    |> List.filter (fun w -> w <> "")
    |> Array.of_list
  in
  let nw = Array.length words in
  let b = Bytes.create source_size in
  let rng = Random.State.make [| 0x2545F491 |] in
  let pos = ref 0 in
  while !pos < source_size do
    let idx = Random.State.int rng nw in
    let w = words.(idx) in
    let wl = String.length w in
    if !pos + wl + 1 <= source_size then begin
      Bytes.blit_string w 0 b !pos wl;
      Bytes.set b (!pos + wl) ' ';
      pos := !pos + wl + 1
    end else begin
      Bytes.fill b !pos (source_size - !pos) ' ';
      pos := source_size
    end
  done;
  b
end

(* [source ~total consume] streams exactly [total] deterministic bytes to
   [consume] in 64 KiB chunks. The chunk buffer is reused, so [consume] must
   use the bytes before returning (both compressors copy synchronously). *)
let source ~total consume: unit =
  let src = pattern in
  let slen = Bytes.length src in
  let chunk = Bytes.create chunk_bytes in
  let sp = ref 0 in
  let remaining = ref total in
  while !remaining > 0 do
    let len = min chunk_bytes !remaining in
    let i = ref 0 in
    while !i < len do
      let n = min (len - !i) (slen - !sp) in
      Bytes.blit src !sp chunk !i n;
      i := !i + n;
      sp := !sp + n;
      if !sp = slen then sp := 0
    done;
    consume chunk 0 len;
    remaining := !remaining - len
  done

(* [make writer] builds a compressor over [writer] and returns its
   (write, close) pair, where [write] has the consumer signature. *)
let run ~name ~total ~make =
  let out = ref 0 in
  let writer _ _ l = out := !out + l in
  let write, close = make writer in
  let t0 = Unix.gettimeofday () in
  source ~total write;
  close ();
  let secs = Unix.gettimeofday () -. t0 in
  let mib_in = float_of_int total /. float_of_int mib in
  Printf.printf
    "%-18s  %7.2f s  %8.1f MiB/s in  out=%7.1f MiB  ratio=%.2fx\n%!"
    name secs (mib_in /. secs)
    (float_of_int !out /. float_of_int mib)
    (float_of_int total /. float_of_int !out)

let () =
  let arg i default = try int_of_string Sys.argv.(i) with _ -> default in
  let total = arg 1 2048 * mib in
  let level = arg 2 4 in
  Printf.printf "encoding %d MiB, level %d\n%!" (total / mib) level;

  run ~name:"raw zstd" ~total ~make:(fun writer ->
    let s = Zstd.Compress_stream.create ~level ~writer () in
    ( (fun buf off len -> Zstd.Compress_stream.write s buf off len),
      (fun () -> Zstd.Compress_stream.close s) ));

  List.iter (fun mb ->
    let frame_size = mb * mib in
    run ~name:(Printf.sprintf "seekable %dMiB" mb) ~total ~make:(fun writer ->
      let s =
        Zstd_seekable.Compress.create ~level
          ~frame_size:(Zstd_seekable.Compress.Uncompressed frame_size) ~writer ()
      in
      ( (fun buf off len -> Zstd_seekable.Compress.write s buf off len),
        (fun () -> Zstd_seekable.Compress.close s) )))
    frame_sizes_mib
