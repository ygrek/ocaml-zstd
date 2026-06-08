
(* This module assumes a 64-bit OCaml runtime: the u32 byte arithmetic below
   would overflow on large u32 values. *)
let () = assert (Sys.word_size >= 64)

(* ----- Seekable format constants ------------------------------------------ *)

let seekable_magic = 0x8F92EAB1
let skippable_magic = 0x184D2A5E
let footer_size = 9 (** Number_Of_Frames u32 + Descriptor u8 + Magic u32 *)

let entry_size_no_checksum = 8 (** Compressed_Size u32 + Decompressed_Size u32 *)

let entry_size_with_checksum = 12 (** + Frame_Checksum u32, not validated by reader *)

let u32_le_of_bytes b off =
  let b0 = Char.code (Bytes.unsafe_get b off) in
  let b1 = Char.code (Bytes.unsafe_get b (off + 1)) in
  let b2 = Char.code (Bytes.unsafe_get b (off + 2)) in
  let b3 = Char.code (Bytes.unsafe_get b (off + 3)) in
  b0 lor (b1 lsl 8) lor (b2 lsl 16) lor (b3 lsl 24)

let bytes_set_u32_le b off v =
  if v < 0 || v > 0xffff_ffff then
    raise (Zstd.Error (Printf.sprintf "Seekable: value %d does not fit in u32" v));
  Bytes.unsafe_set b off (Char.unsafe_chr (v land 0xff));
  Bytes.unsafe_set b (off + 1) (Char.unsafe_chr ((v lsr 8) land 0xff));
  Bytes.unsafe_set b (off + 2) (Char.unsafe_chr ((v lsr 16) land 0xff));
  Bytes.unsafe_set b (off + 3) (Char.unsafe_chr ((v lsr 24) land 0xff))

(** A tiny growable byte buffer. Unlike [Buffer], it exposes its backing
   [bytes] to avoid copies. *)
module Byte_buffer = struct
  type t = { mutable bytes: bytes; mutable len: int }

  let create n = { bytes = Bytes.create (max 8 n); len = 0 }

  (** Ensure room for at least [extra] more bytes past [len], growing
      geometrically and preserving the existing contents. *)
  let ensure t extra =
    let need = t.len + extra in
    if need > Bytes.length t.bytes then begin
      assert (need <= Sys.max_string_length);
      let cap = ref (Bytes.length t.bytes) in
      while !cap < need do cap := !cap * 2 done;
      let new_bytes = Bytes.create !cap in
      Bytes.blit t.bytes 0 new_bytes 0 t.len;
      t.bytes <- new_bytes
    end
end

module Compress = struct
  module Compress_engine = Zstd.Internal.Compress_engine

  type frame_size_policy =
    | Uncompressed of int
    | Compressed of int
    | Manual

  type t = {
    engine: Compress_engine.t;
    user_writer: bytes -> int -> int -> unit;
    policy: frame_size_policy;
    mutable comp_in_frame: int;
    (** per-frame counters. [comp_in_frame] is updated from the wrapper
        writer that sits between the engine and the user. *)
    mutable uncomp_in_frame: int;
    entries: Byte_buffer.t;
    (** collected entries: concatenation of (comp_u32 ; uncomp_u32),
        8 bytes per entry. *)
  }

  let create ?level ?(frame_size=Uncompressed (2 * 1024 * 1024)) ?(checksum=false) ~writer () =
    (match frame_size with
     | Uncompressed n | Compressed n ->
       if n <= 0 then invalid_arg "Seekable.Compress.create: frame_size must be > 0"
     | Manual -> ());
    let self = ref None in
    let wrapped_writer b off len =
      (match !self with
       | Some s -> s.comp_in_frame <- s.comp_in_frame + len
       | None -> ());
      writer b off len
    in
    (* [?checksum] enables libzstd's per-frame XXH64, not the seektable checksum *)
    let engine = Compress_engine.create ?level ~checksum ~writer:wrapped_writer () in
    let s =
      { engine; user_writer = writer; policy = frame_size;
        comp_in_frame = 0; uncomp_in_frame = 0;
        entries = Byte_buffer.create 64 }
    in
    self := Some s;
    s

  let is_closed s = Compress_engine.is_closed s.engine

  (* Append one 8-byte seek-table entry (Compressed_Size ; Decompressed_Size)
     in place, no temporary. Shared by data frames and skippable frames (the
     latter carry [decomp = 0]). *)
  let add_seektable_entry self ~comp ~decomp : unit =
    let e = self.entries in
    Byte_buffer.ensure e entry_size_no_checksum;
    bytes_set_u32_le e.bytes e.len comp;
    bytes_set_u32_le e.bytes (e.len + 4) decomp;
    e.len <- e.len + entry_size_no_checksum

  let force_end_frame self =
    if Compress_engine.is_closed self.engine then
      raise (Zstd.Error "stream is closed");
    if self.uncomp_in_frame = 0 then ()
    else begin
      Compress_engine.end_frame self.engine;
      add_seektable_entry self ~comp:self.comp_in_frame ~decomp:self.uncomp_in_frame;
      self.comp_in_frame <- 0;
      self.uncomp_in_frame <- 0
    end

  let should_end_frame self : bool =
    match self.policy with
    | Uncompressed n -> self.uncomp_in_frame >= n
    | Compressed n -> self.comp_in_frame >= n
    | Manual -> false

  let current_frame_size s = (s.comp_in_frame, s.uncomp_in_frame)

  let write self buf off len =
    if Compress_engine.is_closed self.engine then
      raise (Zstd.Error "stream is closed");
    if off < 0 || len < 0 || off > Bytes.length buf - len then
      invalid_arg "Seekable.Compress.write";
    if len = 0 then ()
    else begin
      (* split input in frames according to policy *)
      let chunk_cap = match self.policy with
        | Uncompressed n -> n
        | Compressed n -> n
        | Manual -> len
      in
      let pos = ref 0 in
      while !pos < len do
        let remaining = len - !pos in
        let chunk = min remaining (max 1 chunk_cap) in
        Compress_engine.write self.engine buf (off + !pos) chunk;
        self.uncomp_in_frame <- self.uncomp_in_frame + chunk;
        pos := !pos + chunk;
        if should_end_frame self then force_end_frame self
      done
    end

  (** Emit one skippable frame. It gets its own entry if [add_to_index=true]. *)
  let write_skippable_frame self ~magic ~add_to_index (content : bytes) off len : unit =
    if off < 0 || len < 0 || off > Bytes.length content - len then
      invalid_arg "Seekable.Compress: skippable frame slice out of bounds";
    let header = Bytes.create 8 in
    bytes_set_u32_le header 0 magic;
    bytes_set_u32_le header 4 len;
    self.user_writer header 0 8;
    if len > 0 then self.user_writer content off len;
    if add_to_index then add_seektable_entry self ~comp:(8 + len) ~decomp:0

  (** User visible version of [write_skippable_frame], with additional bound checking *)
  let add_skippable_frame self ~magic content off_content len_content =
    if Compress_engine.is_closed self.engine then
      raise (Zstd.Error "stream is closed");
    if magic < 0x184D2A50 || magic > 0x184D2A5F then
      invalid_arg "Seekable.Compress.add_skippable_frame: magic not in \
                   the skippable range [0x184D2A50, 0x184D2A5F]";
    (* End any in-progress data frame first so that the order of frames in the
       file matches the order of their seek-table entries: the skippable frame
       is logged after the data frame it follows. A skippable frame may appear
       anywhere (before, between, or after data frames). *)
    if self.uncomp_in_frame > 0 then force_end_frame self;
    write_skippable_frame self ~magic ~add_to_index:true content off_content len_content

  (** The seek table is a skippable frame appended after the data
      frames, containing concatenation of the per-frame entries and the footer. *)
  let write_seek_table self : unit =
    let e = self.entries in
    let entries_size = e.len in
    let n = entries_size / entry_size_no_checksum in
    let total_payload = entries_size + footer_size in
    (* Append the footer in place so the whole payload is contiguous in the
       entries buffer, then emit it as one skippable frame.
       footer: Number_Of_Frames u32 + Seek_Table_Descriptor u8 + Seekable_Magic u32 *)
    Byte_buffer.ensure e footer_size;
    bytes_set_u32_le e.bytes entries_size n;
    Bytes.unsafe_set e.bytes (entries_size + 4) (Char.unsafe_chr 0); (* no checksum/reserved *)
    bytes_set_u32_le e.bytes (entries_size + 5) seekable_magic;
    e.len <- total_payload;
    (* The seek-table frame is the one skippable frame that is not itself indexed. *)
    write_skippable_frame self ~magic:skippable_magic ~add_to_index:false e.bytes 0 total_payload

  let close self =
    if not (Compress_engine.is_closed self.engine) then begin
      if self.uncomp_in_frame > 0 then force_end_frame self;
      (* Use [close_no_drain] (rather than [close]) so a never-written
         engine does not emit a spurious empty zstd frame after the seek
         table footer. *)
      Fun.protect
        ~finally:(fun () -> Compress_engine.close_no_drain self.engine)
        (fun () -> write_seek_table self)
    end
end

module Decompress = struct
  module Reader = struct
    type t = {
      read : bytes -> int -> int -> int;
      seek : int64 -> unit;       (* absolute file offset; int64 for large files *)
      length : unit -> int64;
    }

    let of_string src =
      let pos = ref 0 in
      let len = String.length src in
      {
        read = (fun b off n ->
          let avail = len - !pos in
          let k = min n avail in
          if k > 0 then Bytes.blit_string src !pos b off k;
          pos := !pos + k;
          k);
        seek = (fun p ->
          if p < 0L || p > Int64.of_int len then
            invalid_arg "Seekable.Decompress.Reader.of_string: seek out of bounds";
          pos := Int64.to_int p);
        length = (fun () -> Int64.of_int len);
      }

    let of_in_channel ic =
      let read b off n =
        let rec loop got =
          if got = n then got
          else
            let r = Stdlib.input ic b (off + got) (n - got) in
            if r = 0 then got
            else loop (got + r)
        in
        loop 0
      in
      {
        read;
        seek = (fun p -> Stdlib.LargeFile.seek_in ic p);
        length = (fun () -> Stdlib.LargeFile.in_channel_length ic);
      }
  end

  type table = {
    n_frames: int;
    comp_offsets: int array;
      (** absolute file offsets of frame boundaries in the compressed stream,
          length = n_frames + 1;
          per-frame compressed size = [comp_offsets.(i+1) - comp_offsets.(i)].
          [comp_offsets.(0)] is the absolute start of the first data frame
          (may be > 0 when there is skippable content before the data frames). *)
    uncomp_offsets: int array;
    total_comp: int; (** sum of compressed sizes over all indexed frames
                         (data and skippable); excludes the seek-table frame *)
    total_uncomp: int;
  }

  let num_frames t = t.n_frames
  let decompressed_size t = t.total_uncomp
  let compressed_size t = t.total_comp

  type frame_info = {
    compressed : int;
    decompressed : int;
    comp_offset : int;
    uncomp_offset : int;
  }

  let frame_info t i =
    if i < 0 || i >= t.n_frames then invalid_arg "Seekable.Decompress.frame_info";
    {
      compressed = t.comp_offsets.(i + 1) - t.comp_offsets.(i);
      decompressed = t.uncomp_offsets.(i + 1) - t.uncomp_offsets.(i);
      comp_offset = t.comp_offsets.(i);
      uncomp_offset = t.uncomp_offsets.(i);
    }

  let pp_frame_info fmt fi =
    Format.fprintf fmt
      "{@[compressed=%d; decompressed=%d;@ comp_offset=%d; uncomp_offset=%d@]}"
      fi.compressed fi.decompressed fi.comp_offset fi.uncomp_offset

  (* Debug-only: prints every frame, no truncation. For tables with very many
     frames this can produce a lot of output. *)
  let pp_table fmt t =
    Format.fprintf fmt "@[<hov>";
    for i = 0 to t.n_frames - 1 do
      if i > 0 then Format.fprintf fmt "@ ";
      pp_frame_info fmt (frame_info t i)
    done;
    Format.fprintf fmt "@]"

  let read_exact (r : Reader.t) pos len =
    r.seek (Int64.of_int pos);
    let b = Bytes.create len in
    let i = ref 0 in
    try
      while !i < len do
        let n = r.read b !i (len - !i) in
        if n = 0 then raise Exit;
        i := !i + n
      done;
      Some b
    with Exit -> None

  let[@inline] ( let* ) o f = match o with None -> None | Some x -> f x
  let[@inline] guard b = if b then Some () else None

  let find_table (r : Reader.t) : table option =
    (* Offsets are kept as [int] internally; the assert at the top of the
       module guarantees a 64-bit runtime where that is wide enough. *)
    let total_len = Int64.to_int (r.length ()) in
    let* () = guard (total_len >= footer_size) in
    let* footer = read_exact r (total_len - footer_size) footer_size in
    let* () = guard (u32_le_of_bytes footer 5 = seekable_magic) in
    let n = u32_le_of_bytes footer 0 in
    let* () = guard (n >= 0) in
    let desc = Char.code (Bytes.unsafe_get footer 4) in
    let entry_size =
      if desc land 0x80 <> 0 then entry_size_with_checksum
      else entry_size_no_checksum
    in
    let entries_size = n * entry_size in
    let frame_size = entries_size + footer_size in
    let skippable_start = total_len - frame_size - 8 in
    let* () = guard (skippable_start >= 0) in
    let* hdr = read_exact r skippable_start 8 in
    let* () = guard (u32_le_of_bytes hdr 0 = skippable_magic
                     && u32_le_of_bytes hdr 4 = frame_size) in
    let* entries =
      if entries_size = 0 then Some Bytes.empty
      else read_exact r (skippable_start + 8) entries_size
    in
    let comp_offsets = Array.make (n + 1) 0 in
    let uncomp_offsets = Array.make (n + 1) 0 in
    let cc = ref 0 and cu = ref 0 in
    for i = 0 to n - 1 do
      let off = i * entry_size in
      let cs = u32_le_of_bytes entries off in
      let us = u32_le_of_bytes entries (off + 4) in
      comp_offsets.(i) <- !cc;
      uncomp_offsets.(i) <- !cu;
      cc := !cc + cs;
      cu := !cu + us
    done;
    comp_offsets.(n) <- !cc;
    uncomp_offsets.(n) <- !cu;
    (* [base] is the absolute file offset of frame 0. For files written by this
       library, every frame (including skippable frames) is indexed in the seek
       table, so [base = 0]. A positive [base] only arises for inputs with leading
       bytes not covered by indexed frames (e.g., foreign/custom content); such
       leading content is tolerated as long as it fits before the indexed frames. *)
    let base = skippable_start - !cc in
    let* () = guard (base >= 0) in
    for i = 0 to n do
      comp_offsets.(i) <- comp_offsets.(i) + base
    done;
    Some {
      n_frames = n;
      comp_offsets; uncomp_offsets;
      total_comp = !cc;
      total_uncomp = !cu;
    }

  module E = Zstd.Internal.Decompress_engine

  type t = {
    engine: E.t;
    reader: Reader.t;
    scratch: bytes;
    table: table;
    mutable uncomp_pos: int; (** next uncompressed byte to emit, absolute *)
    mutable skip_remaining: int; (** bytes still to discard from decoded output *)
    mutable comp_pos: int; (** current absolute read position in source *)
    comp_end: int; (** absolute offset at which user data ends (= comp_offsets.(n_frames)) *)
  }

  let create (reader : Reader.t) table : t =
    let engine = E.create () in
    let scratch = Bytes.create (E.in_capacity engine) in
    (* [reader.seek] runs after [E.create] has registered its GC
       finaliser. If [seek] raises (closed fd, IO error), tear down the
       engine synchronously so we don't leak a dctx into the finaliser.
       [comp_offsets.(0)] is the absolute file offset of frame 0 (may be > 0
       when there is leading content before the data frames). *)
    try
      reader.seek (Int64.of_int table.comp_offsets.(0));
      { engine; reader; scratch; table;
        uncomp_pos = 0; skip_remaining = 0;
        comp_pos = table.comp_offsets.(0);
        comp_end = table.comp_offsets.(table.n_frames) }
    with exn ->
      E.close engine;
      raise exn

  let[@inline] is_closed self = E.is_closed self.engine
  let close self = E.close self.engine

  (** Largest i such that [uncomp_offsets[i] <= off], with i < n_frames. *)
  let find_idx_of_frame_for_offset (t : table) off : int =
    let n = t.n_frames in
    let lo = ref 0 and hi = ref (n - 1) in
    while !lo < !hi do
      let mid = (!lo + !hi + 1) / 2 in
      if t.uncomp_offsets.(mid) <= off then lo := mid
      else hi := mid - 1
    done;
    !lo

  (** Seek to the given uncompressed offset *)
  let seek self target_off : unit =
    if E.is_closed self.engine then raise (Zstd.Error "stream is closed");
    if target_off < 0 then invalid_arg "Seekable.Decompress.seek: negative offset";
    let t = self.table in
    E.session_reset self.engine;
    if target_off >= t.total_uncomp || t.n_frames = 0 then begin
      self.uncomp_pos <- t.total_uncomp;
      self.skip_remaining <- 0;
      self.comp_pos <- self.comp_end
    end else begin
      let frame = find_idx_of_frame_for_offset t target_off in
      self.reader.seek (Int64.of_int t.comp_offsets.(frame));
      self.comp_pos <- t.comp_offsets.(frame);
      self.uncomp_pos <- target_off;
      (* skip bytes in the frame until we reach [target_off] *)
      self.skip_remaining <- target_off - t.uncomp_offsets.(frame)
    end

  let read self buf off len =
    if E.is_closed self.engine then raise (Zstd.Error "stream is closed");
    if off < 0 || len < 0 || off > Bytes.length buf - len then
      invalid_arg "Seekable.Decompress.read";
    if len = 0 then 0
    else
      (* Tear down the engine on any error from the reader callback, from
         the truncation/no-progress raises below, or from libzstd via
         [E.step]. Without this, exceptions leak the dctx until GC and
         trigger the 'closing in GC finalizer' warning. *)
      try
        let t = self.table in
        let total = ref 0 in
        let continue = ref true in
        while !total < len && !continue do
          if self.uncomp_pos >= t.total_uncomp then continue := false
          else if E.pending_output self.engine > 0 then begin
            (* Discard buffered output toward [skip_remaining] before
               copying anything. [E.discard] only drops what is currently
               buffered (at most one output buffer, ~ZSTD_DStreamOutSize);
               if [skip_remaining] is larger we drop what we can and fall
               through — [drain] then returns 0 (output is empty), the loop
               refills/steps to produce more, and we discard again. *)
            if self.skip_remaining > 0 then
              self.skip_remaining <-
                self.skip_remaining - E.discard self.engine self.skip_remaining;
            let cap = min (len - !total) (t.total_uncomp - self.uncomp_pos) in
            let n = E.drain self.engine buf (off + !total) cap in
            self.uncomp_pos <- self.uncomp_pos + n;
            total := !total + n
          end else begin
            (* Refill, bounded by the table's compressed extent. Skippable
               frames (between/around data frames) are indexed in the seek
               table just like data frames, so [comp_end] already includes
               them; libzstd's streaming decoder skips them transparently,
               producing no output. We can therefore feed the input in
               arbitrary chunks rather than one frame at a time. *)
            if E.needs_input self.engine then begin
              let want = min (E.in_capacity self.engine) (self.comp_end - self.comp_pos) in
              if want = 0 then begin
                (* All compressed bytes the table promised are consumed.
                   If libzstd still wants more input ([last_ret <> 0])
                   the final frame is truncated relative to the table. *)
                if E.last_ret self.engine <> 0 then
                  raise (Zstd.Error "truncated compressed data");
                continue := false
              end else begin
                let n = self.reader.read self.scratch 0 want in
                if n <= 0 then raise (Zstd.Error "unexpected end of compressed input");
                E.push_input self.engine self.scratch 0 n;
                self.comp_pos <- self.comp_pos + n
              end
            end;
            if !continue then begin
              let (consumed, produced) = E.step self.engine in
              (* [(0, 0)] with [last_ret = 0] just means libzstd is at a
                 clean frame boundary and the next frame's magic hasn't
                 been read yet — don't mistake it for a stall. *)
              if produced = 0 && consumed = 0 && E.last_ret self.engine <> 0 then
                raise (Zstd.Error "decompression made no progress")
            end
          end
        done;
        !total
      with exn ->
        E.close self.engine;
        raise exn
end
