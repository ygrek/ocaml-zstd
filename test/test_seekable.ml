open ExtLib
open Printf

let stream_compress ?level ?dict src =
  let buf = Buffer.create 256 in
  let writer b off len = Buffer.add_subbytes buf b off len in
  let s = Zstd.Compress_stream.create ?level ?dict ~writer () in
  Zstd.Compress_stream.write s (Bytes.of_string src) 0 (String.length src);
  Zstd.Compress_stream.close s;
  Buffer.contents buf

let stream_decompress_reader ?dict ?(buf_size=Zstd.Decompress_stream.out_size ()) ~reader () =
  let s = Zstd.Decompress_stream.create ~reader ?dict () in
  let out = Buffer.create 256 in
  let tmp = Bytes.create buf_size in
  let rec loop () =
    let n = Zstd.Decompress_stream.read s tmp 0 (Bytes.length tmp) in
    if n > 0 then begin
      Buffer.add_subbytes out tmp 0 n;
      loop ()
    end
  in
  loop ();
  Zstd.Decompress_stream.close s;
  Buffer.contents out

let string_reader src =
  let pos = ref 0 in
  fun b off len ->
    let n = min len (String.length src - !pos) in
    Bytes.blit_string src !pos b off n;
    pos := !pos + n;
    n

let stream_decompress ?dict ?buf_size src =
  stream_decompress_reader ?dict ?buf_size ~reader:(string_reader src) ()


let seekable_compress ?level ?frame_size ?checksum src =
  let buf = Buffer.create 256 in
  let writer b off len = Buffer.add_subbytes buf b off len in
  let s = Zstd_seekable.Compress.create ?level ?frame_size ?checksum ~writer () in
  Zstd_seekable.Compress.write s (Bytes.of_string src) 0 (String.length src);
  Zstd_seekable.Compress.close s;
  Buffer.contents buf

let seekable_read_all dec =
  let buf = Buffer.create 256 in
  let tmp = Bytes.create 65536 in
  let rec loop () =
    let n = Zstd_seekable.Decompress.read dec tmp 0 (Bytes.length tmp) in
    if n > 0 then begin
      Buffer.add_subbytes buf tmp 0 n;
      loop ()
    end
  in
  loop ();
  Buffer.contents buf

let test_seekable () =
  let module CS = Zstd_seekable.Compress in
  let module DS = Zstd_seekable.Decompress in
  (* Deterministic fixture data, big enough for several frames at 64K. *)
  let mk_data size =
    String.init size (fun i ->
      if i mod 256 < 200 then Char.chr ((i * 31 + i / 256) land 0xff)
      else Char.chr (65 + (i / 4096) mod 26))
  in
  let fixtures =
    [ "synthetic-1.5MB", mk_data (1_500_000)
    ; "synthetic-200K",  mk_data 200_000
    ; "synthetic-tiny",  mk_data 17
    ]
    @ (match (try Some (Std.input_file "/bin/bash") with _ -> None) with
       | Some s -> ["/bin/bash", s]
       | None -> [])
  in
  let policies = [
    "Uncompressed 64K", CS.Uncompressed (64 * 1024);
    "Uncompressed 2M",  CS.Uncompressed (2 * 1024 * 1024);
    "Compressed 16K",   CS.Compressed (16 * 1024);
  ] in
  (* 1. Round-trip our encoder <-> our decoder, with several policies. *)
  List.iter (fun (fname, data) ->
    List.iter (fun (pname, frame_size) ->
      let compressed = seekable_compress ~frame_size data in
      let reader = DS.Reader.of_string compressed in
      let table = match DS.find_table reader with
        | Some t -> t
        | None -> failwith (sprintf "%s / %s: find_table = None" fname pname)
      in
      assert (DS.decompressed_size table = String.length data);
      let nf = DS.num_frames table in
      assert (nf >= (if String.length data = 0 then 0 else 1));
      let dec = DS.create reader table in
      let got = seekable_read_all dec in
      DS.close dec;
      if got <> data then
        failwith (sprintf "%s / %s: linear read mismatch (got %d, expected %d)"
                    fname pname (String.length got) (String.length data));
    ) policies
  ) fixtures;
  printf "  seekable 1. round-trip (encoder/decoder): OK\n";

  (* 2. Seek correctness. For each fixture, several offsets. *)
  List.iter (fun (fname, data) ->
    let n = String.length data in
    let compressed = seekable_compress ~frame_size:(CS.Uncompressed (64 * 1024)) data in
    let reader = DS.Reader.of_string compressed in
    let table = match DS.find_table reader with Some t -> t | None -> failwith "find_table" in
    let dec = DS.create reader table in
    let offsets =
      let nf = DS.num_frames table in
      let mid_frame =
        if nf > 0 then
          let fi = DS.frame_info table (nf / 2) in
          fi.DS.uncomp_offset + fi.DS.decompressed / 2
        else 0
      in
      let frame_boundary =
        if nf >= 2 then (DS.frame_info table 1).DS.uncomp_offset
        else 0
      in
      List.sort_uniq compare [0; mid_frame; frame_boundary; max 0 (n - 1); n; n + 100]
    in
    List.iter (fun u ->
      Zstd_seekable.Decompress.seek dec u;
      let span = 257 in
      let tmp = Bytes.create span in
      let got = Zstd_seekable.Decompress.read dec tmp 0 span in
      if u >= n then begin
        if got <> 0 then failwith (sprintf "%s @%d: expected 0 at/after EOF, got %d" fname u got)
      end else begin
        let expected = min span (n - u) in
        if got <> expected then
          failwith (sprintf "%s @%d: expected to read %d, got %d" fname u expected got);
        let want = String.sub data u expected in
        let have = Bytes.sub_string tmp 0 got in
        if want <> have then
          failwith (sprintf "%s @%d: span mismatch" fname u)
      end
    ) offsets;
    DS.close dec
  ) fixtures;
  printf "  seekable 2. seek correctness: OK\n";

  (* 3. Plain-decompress compatibility: existing Decompress_stream must
     consume the seekable file and produce the original. *)
  List.iter (fun (fname, data) ->
    let compressed = seekable_compress ~frame_size:(CS.Uncompressed (64 * 1024)) data in
    let decompressed = stream_decompress compressed in
    if decompressed <> data then
      failwith (sprintf "%s: plain Decompress_stream did not recover original" fname)
  ) fixtures;
  printf "  seekable 3. plain Decompress_stream compatibility: OK\n";

  (* 4. Per-frame checksum: with ~checksum:true, libzstd appends a 4-byte XXH64
     to each frame epilogue. Round-trip must succeed and the compressed output
     must be exactly 4*num_frames bytes longer than without the checksum. *)
  let () =
    let data = mk_data 300_000 in
    (* Frame size chosen so the input produces several frames (>= 2). *)
    let frame_size = CS.Uncompressed (64 * 1024) in
    let c_no  = seekable_compress ~frame_size ~checksum:false data in
    let c_yes = seekable_compress ~frame_size ~checksum:true  data in
    (* round-trip with checksum *)
    let reader = DS.Reader.of_string c_yes in
    let table = match DS.find_table reader with
      | Some t -> t
      | None -> failwith "checksum: find_table = None"
    in
    let nf = DS.num_frames table in
    assert (nf >= 2);
    let dec = DS.create reader table in
    let got = seekable_read_all dec in
    DS.close dec;
    if got <> data then failwith "checksum: round-trip mismatch";
    let diff = String.length c_yes - String.length c_no in
    if diff <> 4 * nf then
      failwith (sprintf "checksum: size diff = %d, expected %d (= 4 * %d frames)" diff (4 * nf) nf)
  in
  printf "  seekable 4. per-frame checksum: OK\n";

  (* 5. Edge cases: find_table on plain zstd returns None; seek past EOF
     then read returns 0. *)
  let () =
    let plain = stream_compress (mk_data 1024) in
    let reader = DS.Reader.of_string plain in
    (match DS.find_table reader with
     | None -> ()
     | Some _ -> failwith "find_table on plain zstd should be None")
  in
  let () =
    (* truly tiny non-zstd input *)
    let reader = DS.Reader.of_string "x" in
    (match DS.find_table reader with
     | None -> ()
     | Some _ -> failwith "find_table on tiny garbage should be None")
  in
  let () =
    let data = mk_data 5000 in
    let compressed = seekable_compress ~frame_size:(CS.Uncompressed 1024) data in
    let reader = DS.Reader.of_string compressed in
    let table = match DS.find_table reader with Some t -> t | None -> assert false in
    let dec = DS.create reader table in
    Zstd_seekable.Decompress.seek dec (String.length data + 1_000_000);
    let tmp = Bytes.create 16 in
    let n = Zstd_seekable.Decompress.read dec tmp 0 16 in
    assert (n = 0);
    DS.close dec
  in
  printf "  seekable 5. edge cases: OK\n";

  (* 6. Manual frame policy + current_frame_size: write fixed-size "items" and
     close the frame whenever adding the next item would push the frame past a
     target uncompressed size. Verify item-aligned frame boundaries. *)
  let () =
    let item = Bytes.make 137 'A' in
    let item_len = Bytes.length item in
    let n_items = 500 in
    let target = 8 * 1024 in
    let buf = Buffer.create 64 in
    let writer b o l = Buffer.add_subbytes buf b o l in
    let enc = CS.create ~frame_size:CS.Manual ~writer () in
    let frame_uncomp_sizes = ref [] in
    for _ = 1 to n_items do
      let (_, u) = CS.current_frame_size enc in
      if u > 0 && u + item_len > target then begin
        frame_uncomp_sizes := u :: !frame_uncomp_sizes;
        CS.force_end_frame enc
      end;
      CS.write enc item 0 item_len
    done;
    let (_, last_u) = CS.current_frame_size enc in
    if last_u > 0 then frame_uncomp_sizes := last_u :: !frame_uncomp_sizes;
    CS.close enc;
    let frame_uncomp_sizes = List.rev !frame_uncomp_sizes in
    List.iter (fun u ->
      if u mod item_len <> 0 then
        failwith (sprintf "manual: frame uncomp size %d not item-aligned" u)
    ) frame_uncomp_sizes;
    let compressed = Buffer.contents buf in
    let reader = DS.Reader.of_string compressed in
    let table = match DS.find_table reader with Some t -> t | None -> failwith "manual: find_table" in
    assert (DS.num_frames table = List.length frame_uncomp_sizes);
    assert (DS.decompressed_size table = n_items * item_len);
    let dec = DS.create reader table in
    let got = seekable_read_all dec in
    DS.close dec;
    let expected = String.concat "" (List.init n_items (fun _ -> Bytes.to_string item)) in
    if got <> expected then failwith "manual: round-trip mismatch"
  in
  printf "  seekable 6. manual policy + current_frame_size: OK\n";

  (* 7. Skippable frames interleaved with data frames. The seekable format
     records every frame in the seek table (skippable frames carry a
     decompressed size of 0), so they may appear anywhere. We embed:
       - 2 leading skippable frames (before any data),
       - 3 skippable frames after each content frame — the first two groups
         sit *between* content frames, the last group is *trailing*.
     Not all of them are empty. find_table, linear read, seek, and plain
     [Decompress_stream] must all still recover the original data. *)
  let () =
    let magic = 0x184D2A50 in
    let chunks = [ mk_data 120_000; mk_data 90_000; mk_data 17 ] in
    let data = String.concat "" chunks in
    (* Each group is 3 skippable frames; deliberately mix empty and non-empty. *)
    let group tag = [ ""; sprintf "skip-%s" tag; tag ^ String.make 300 'Z' ] in
    let leading = [ ""; "leading skippable payload" ] in
    let buf = Buffer.create 256 in
    let writer b o l = Buffer.add_subbytes buf b o l in
    (* [Manual] policy: a content frame ends only when we add a skippable
       frame (which closes the open data frame first) or close the encoder,
       so each chunk maps to exactly one data frame. *)
    let enc = CS.create ~frame_size:CS.Manual ~writer () in
    let add_skips payloads =
      List.iter (fun p ->
        let b = Bytes.of_string p in
        CS.add_skippable_frame enc ~magic b 0 (Bytes.length b)) payloads
    in
    add_skips leading;
    List.iteri (fun i chunk ->
      CS.write enc (Bytes.of_string chunk) 0 (String.length chunk);
      add_skips (group (sprintf "g%d" i))
    ) chunks;
    CS.close enc;
    let stream = Buffer.contents buf in

    let n_data = List.length chunks in
    let n_skip = List.length leading + (3 * List.length chunks) in

    let reader = DS.Reader.of_string stream in
    let table = match DS.find_table reader with
      | Some t -> t
      | None -> failwith "interleaved-skippable: find_table = None"
    in
    assert (DS.decompressed_size table = String.length data);
    let nf = DS.num_frames table in
    if nf <> n_data + n_skip then
      failwith (sprintf "interleaved-skippable: num_frames = %d, expected %d"
                  nf (n_data + n_skip));
    (* Skippable frames are indexed with decompressed_size = 0; count them and
       check their compressed size is exactly 8 + payload_len for the leading
       ones, and that the two leading frames sit at the very front. *)
    let zero_dsize = ref 0 in
    for i = 0 to nf - 1 do
      if (DS.frame_info table i).DS.decompressed = 0 then incr zero_dsize
    done;
    if !zero_dsize <> n_skip then
      failwith (sprintf "interleaved-skippable: %d zero-size frames, expected %d"
                  !zero_dsize n_skip);
    let fi0 = DS.frame_info table 0 and fi1 = DS.frame_info table 1 in
    assert (fi0.DS.comp_offset = 0 && fi0.DS.compressed = 8 (* empty *));
    assert (fi1.DS.comp_offset = 8
            && fi1.DS.compressed = 8 + String.length "leading skippable payload");
    (* First data frame is frame 2 (after the 2 leading skippable frames). *)
    let fd = DS.frame_info table 2 in
    if fd.DS.decompressed <> String.length (List.nth chunks 0) then
      failwith (sprintf "interleaved-skippable: first data frame decompressed = %d, expected %d"
                  fd.DS.decompressed (String.length (List.nth chunks 0)));
    if fd.DS.comp_offset <> fi1.DS.comp_offset + fi1.DS.compressed then
      failwith "interleaved-skippable: first data frame not contiguous with leading frames";

    (* linear read *)
    let dec = DS.create reader table in
    let got = seekable_read_all dec in
    if got <> data then failwith "interleaved-skippable: linear read mismatch";
    (* seek to several offsets, including frame boundaries between which
       skippable frames live, and verify spans. *)
    let bnd0 = String.length (List.nth chunks 0) in
    let bnd1 = bnd0 + String.length (List.nth chunks 1) in
    List.iter (fun u ->
      Zstd_seekable.Decompress.seek dec u;
      let span = 1000 in
      let tmp = Bytes.create span in
      let n = Zstd_seekable.Decompress.read dec tmp 0 span in
      let expected = min span (String.length data - u) in
      if n <> expected then
        failwith (sprintf "interleaved-skippable: seek @%d read %d, expected %d" u n expected);
      if Bytes.sub_string tmp 0 n <> String.sub data u n then
        failwith (sprintf "interleaved-skippable: seek @%d span mismatch" u)
    ) [ 0; bnd0 - 10; bnd0; bnd1 - 1; bnd1; String.length data / 2 ];
    DS.close dec;
    (* plain Decompress_stream also skips the interleaved skippable frames *)
    if stream_decompress stream <> data then
      failwith "interleaved-skippable: plain Decompress_stream mismatch";

    (* Error cases: out-of-range magic, and use after close. *)
    let enc2 = CS.create ~writer:(fun _ _ _ -> ()) () in
    (match CS.add_skippable_frame enc2 ~magic:0x184D2A60 Bytes.empty 0 0 with
     | exception Invalid_argument _ -> ()
     | _ -> failwith "interleaved-skippable: out-of-range magic should raise");
    CS.close enc2;
    (match CS.add_skippable_frame enc2 ~magic Bytes.empty 0 0 with
     | exception Zstd.Error _ -> ()
     | exception _ -> failwith "interleaved-skippable: wrong exn after close"
     | () -> failwith "interleaved-skippable: add_skippable_frame after close should raise")
  in
  printf "  seekable 7. interleaved skippable frames: OK\n";
  printf "All seekable tests passed.\n"

;;

let () =
  printf "\nZstd_seekable tests:\n";
  test_seekable ()
