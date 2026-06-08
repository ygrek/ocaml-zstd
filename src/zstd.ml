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

let seti s f i = Ctypes.setf s f (Size_t.of_int i)
let geti s f = Size_t.to_int (Ctypes.getf s f)

type bigstring = Bigstringaf.t

let bigstring_create n = Bigstringaf.create n

let bigstring_start ba = Ctypes.bigarray_start Ctypes.array1 ba

let make_zstd_buffers in_buf out_buf =
  let open Ctypes in
  let zstd_in = make F.in_buffer in
  let zstd_out = make F.out_buffer in
  setf zstd_in F.in_buffer_src (to_voidp (bigstring_start in_buf));
  setf zstd_out F.out_buffer_dst (to_voidp (bigstring_start out_buf));
  (zstd_in, zstd_out)


(* Low-level compression engine. Owns a [ZSTD_CCtx] and the in/out
   buffers, and exposes [write]/[flush]/[end_frame]/[close]. Reused by
   both the regular streaming compressor and the seekable encoder. *)
module Compress_engine = struct
  type t = {
    cctx: [`CCtx] Ctypes.structure Ctypes.ptr;
    writer: bytes -> int -> int -> unit;
    in_buf: bigstring;
    in_size: int;
    out_buf: bigstring;
    out_size: int;
    out_bytes: bytes;
    zstd_in: [`InBuffer] Ctypes.structure;
    zstd_out: [`OutBuffer] Ctypes.structure;
    mutable closed: bool;
    (* True iff a zstd frame is currently being built (initial state, and
       after any [write] that pushed input). Cleared by [end_frame]. Lets
       [close] skip the [e_end] drain when the last frame has already been
       terminated, so the seekable encoder doesn't need a separate [free]
       op to avoid emitting a spurious empty trailing frame. *)
    mutable frame_open: bool;
  }

  let create ?level ?dict ?(checksum=false) ~writer () =
    let open Ctypes in
    let cctx = F.create_cctx () in
    if is_null cctx then raise (Error "ZSTD_createCCtx failed");
    try
      (match level with
       | Some l -> check (F.cctx_set_parameter cctx T.c_compressionLevel l)
       | None -> ());
      if checksum then
        check (F.cctx_set_parameter cctx T.c_checksumFlag 1);
      (match dict with
       | Some d -> check (F.cctx_load_dictionary cctx d (Size_t.of_int (String.length d)))
       | None -> ());
      let in_size = Size_t.to_int (F.cstream_in_size ()) in
      let out_size = Size_t.to_int (F.cstream_out_size ()) in
      let in_buf = bigstring_create in_size in
      let out_buf = bigstring_create out_size in
      let out_bytes = Bytes.create out_size in
      let zstd_in, zstd_out = make_zstd_buffers in_buf out_buf in
      (* zstd_in/zstd_out are custom blocks (Ctypes.make), freed by GC.
         cctx is C-allocated by libzstd and must be freed explicitly. *)
      let s = { cctx; writer; in_buf; in_size; out_buf; out_size; out_bytes;
                zstd_in; zstd_out; closed = false; frame_open = true } in
      Gc.finalise (fun s ->
        if not s.closed then begin
          prerr_endline "W: closing Zstd.Compress_engine in GC finalizer, this is likely benign but could indicate a bug in the program using ocaml-zstd library";
          s.closed <- true;
          ignore (F.free_cctx s.cctx)
        end) s;
      s
    with exn ->
      ignore (F.free_cctx cctx);
      raise exn

  let is_closed s = s.closed

  let compress_step s directive =
    let open Ctypes in
    seti s.zstd_out F.out_buffer_size s.out_size;
    seti s.zstd_out F.out_buffer_pos 0;
    let remaining = F.compress_stream2 s.cctx (addr s.zstd_out) (addr s.zstd_in) directive in
    check remaining;
    let out_pos = geti s.zstd_out F.out_buffer_pos in
    if out_pos > 0 then begin
      Bigstringaf.blit_to_bytes s.out_buf ~src_off:0 s.out_bytes ~dst_off:0 ~len:out_pos;
      s.writer s.out_bytes 0 out_pos
    end;
    Size_t.to_int remaining

  let drain_with_directive s directive =
    seti s.zstd_in F.in_buffer_size 0;
    seti s.zstd_in F.in_buffer_pos 0;
    while 0 < compress_step s directive do () done

  let close_on_exn s f =
    try f () with exn ->
      if not s.closed then begin
        s.closed <- true;
        ignore (F.free_cctx s.cctx)
      end;
      raise exn

  (* Precondition: [buf]/[off]/[len] are valid (caller is responsible
     for bounds checking with its own [invalid_arg] message). *)
  let write s buf off len =
    if s.closed then raise (Error "stream is closed");
    if len = 0 then ()
    else close_on_exn s begin fun () ->
      s.frame_open <- true;
      let pos = ref 0 in
      while !pos < len do
        let chunk = min s.in_size (len - !pos) in
        Bigstringaf.blit_from_bytes buf ~src_off:(off + !pos) s.in_buf ~dst_off:0 ~len:chunk;
        seti s.zstd_in F.in_buffer_size chunk;
        seti s.zstd_in F.in_buffer_pos 0;
        while geti s.zstd_in F.in_buffer_pos < chunk do
          let (_remaining : int) = compress_step s T.e_continue in
          ()
        done;
        pos := !pos + chunk
      done
    end

  let flush s =
    if s.closed then raise (Error "stream is closed");
    close_on_exn s begin fun () -> drain_with_directive s T.e_flush end

  let end_frame s =
    if s.closed then raise (Error "stream is closed");
    close_on_exn s begin fun () ->
      drain_with_directive s T.e_end;
      check (F.cctx_reset s.cctx T.reset_session_only);
      s.frame_open <- false
    end

  let close s =
    if s.closed then ()
    else begin
      s.closed <- true;
      Fun.protect
        ~finally:(fun () -> ignore (F.free_cctx s.cctx))
        (fun () -> if s.frame_open then drain_with_directive s T.e_end)
    end

  (* Free the cctx without draining the current frame. Used by the
     seekable encoder, which must not emit any bytes after its seek
     table. Any in-flight frame data is discarded; the caller is
     responsible for having already ended frames it cares about. *)
  let close_no_drain s =
    if not s.closed then begin
      s.closed <- true;
      ignore (F.free_cctx s.cctx)
    end
end

(* Low-level decompression engine. Owns a [ZSTD_DCtx], a refillable input
   buffer, and a small output cache. Exposes [push_input]/[step]/[drain]/
   [discard]/[session_reset]/[close]. Reused by both the regular streaming
   decompressor and the seekable decoder. *)
module Decompress_engine = struct
  type t = {
    dctx: [`DCtx] Ctypes.structure Ctypes.ptr;
    in_buf: bigstring;
    in_size: int;
    out_buf: bigstring;
    out_size: int;
    zstd_in: [`InBuffer] Ctypes.structure;
    zstd_out: [`OutBuffer] Ctypes.structure;
    mutable in_filled: int;
    mutable in_consumed: int;
    mutable out_pos: int;
    mutable out_avail: int;
    mutable last_ret: int;
    mutable closed: bool;
  }

  let in_size () = Size_t.to_int (F.dstream_in_size ())
  let out_size () = Size_t.to_int (F.dstream_out_size ())

  let create ?dict () =
    let open Ctypes in
    let dctx = F.create_dctx () in
    if is_null dctx then raise (Error "ZSTD_createDCtx failed");
    try
      (match dict with
       | Some d -> check (F.dctx_load_dictionary dctx d (Size_t.of_int (String.length d)))
       | None -> ());
      let in_size = in_size () in
      let out_size = out_size () in
      let in_buf = bigstring_create in_size in
      let out_buf = bigstring_create out_size in
      let zstd_in, zstd_out = make_zstd_buffers in_buf out_buf in
      (* zstd_in/zstd_out are custom blocks (Ctypes.make), freed by GC.
         dctx is C-allocated by libzstd and must be freed explicitly. *)
      let s = { dctx; in_buf; in_size; out_buf; out_size;
                zstd_in; zstd_out;
                in_filled = 0; in_consumed = 0;
                out_pos = 0; out_avail = 0;
                last_ret = 0; closed = false } in
      Gc.finalise (fun s ->
        if not s.closed then begin
          prerr_endline "W: closing Zstd.Decompress_engine in GC finalizer, this is likely benign but could indicate a bug in the program using ocaml-zstd library";
          s.closed <- true;
          ignore (F.free_dctx s.dctx)
        end) s;
      s
    with exn ->
      ignore (F.free_dctx dctx);
      raise exn

  let is_closed s = s.closed

  let close s =
    if not s.closed then begin
      s.closed <- true;
      ignore (F.free_dctx s.dctx)
    end

  let close_on_exn s f =
    try f () with exn ->
      if not s.closed then begin
        s.closed <- true;
        ignore (F.free_dctx s.dctx)
      end;
      raise exn

  let in_capacity s = s.in_size
  let needs_input s = s.in_consumed >= s.in_filled
  let pending_output s = s.out_avail
  let last_ret s = s.last_ret

  let session_reset s =
    if s.closed then raise (Error "decompressor is closed");
    s.in_filled <- 0;
    s.in_consumed <- 0;
    s.out_pos <- 0;
    s.out_avail <- 0;
    s.last_ret <- 0;
    check (F.dctx_reset s.dctx T.reset_session_only)

  let push_input s buf off len =
    if s.closed then raise (Error "decompressor is closed");
    if len = 0 then ()
    else begin
      Bigstringaf.blit_from_bytes buf ~src_off:off s.in_buf ~dst_off:0 ~len;
      s.in_filled <- len;
      s.in_consumed <- 0
    end

  let drain s buf off cap =
    if s.out_avail = 0 || cap <= 0 then 0
    else begin
      let n = min s.out_avail cap in
      Bigstringaf.blit_to_bytes s.out_buf ~src_off:s.out_pos buf ~dst_off:off ~len:n;
      s.out_pos <- s.out_pos + n;
      s.out_avail <- s.out_avail - n;
      n
    end

  let discard s n =
    let drop = min s.out_avail (max 0 n) in
    s.out_pos <- s.out_pos + drop;
    s.out_avail <- s.out_avail - drop;
    drop

  (* Run one [decompress_stream] iteration over whatever input was last
     [push_input]ed (and not yet fully consumed). Returns
     [(consumed_new, produced)]. The produced bytes are placed in the
     engine's output cache and can be retrieved via [drain]/[discard]. *)
  let step s =
    if s.closed then raise (Error "decompressor is closed");
    close_on_exn s begin fun () ->
      let open Ctypes in
      let consumed_before = s.in_consumed in
      seti s.zstd_in F.in_buffer_size s.in_filled;
      seti s.zstd_in F.in_buffer_pos s.in_consumed;
      seti s.zstd_out F.out_buffer_size s.out_size;
      seti s.zstd_out F.out_buffer_pos 0;
      let ret = F.decompress_stream s.dctx (addr s.zstd_out) (addr s.zstd_in) in
      check ret;
      s.in_consumed <- geti s.zstd_in F.in_buffer_pos;
      let produced = geti s.zstd_out F.out_buffer_pos in
      s.last_ret <- Size_t.to_int ret;
      s.out_pos <- 0;
      s.out_avail <- produced;
      (s.in_consumed - consumed_before, produced)
    end
end

module Internal = struct
  module Compress_engine = Compress_engine
  module Decompress_engine = Decompress_engine
end

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

(* Streaming API *)

module Compress_stream = struct
  type t = Internal.Compress_engine.t

  let in_size () = Size_t.to_int (F.cstream_in_size ())
  let out_size () = Size_t.to_int (F.cstream_out_size ())

  let create ?level ?dict ~writer () =
    Internal.Compress_engine.create ?level ?dict ~writer ()

  let is_closed = Internal.Compress_engine.is_closed

  let write s buf off len =
    if off < 0 || len < 0 || off > Bytes.length buf - len then
      invalid_arg "Zstd.Compress_stream.write";
    Internal.Compress_engine.write s buf off len

  let flush = Internal.Compress_engine.flush
  let close = Internal.Compress_engine.close
end

module Decompress_stream = struct
  module E = Internal.Decompress_engine

  type t = {
    engine: E.t;
    reader: bytes -> int -> int -> int;
    scratch: bytes;        (* read target for the user [reader] callback *)
    mutable eof: bool;     (* reader has returned 0 *)
  }

  let in_size = E.in_size
  let out_size = E.out_size

  let create ?dict ~reader () =
    let engine = E.create ?dict () in
    let scratch = Bytes.create (E.in_capacity engine) in
    { engine; reader; scratch; eof = false }

  let is_closed s = E.is_closed s.engine
  let close s = E.close s.engine

  let read s buf off len =
    if E.is_closed s.engine then raise (Error "stream is closed");
    if off < 0 || len < 0 || off > Bytes.length buf - len then
      invalid_arg "Zstd.Decompress_stream.read";
    if len = 0 then 0
    else
      try
        let total = ref 0 in
        let continue = ref true in
        while !total < len && !continue do
          if E.pending_output s.engine > 0 then
            total := !total + E.drain s.engine buf (off + !total) (len - !total)
          else begin
            if E.needs_input s.engine && not s.eof then begin
              let cap = E.in_capacity s.engine in
              let n = s.reader s.scratch 0 cap in
              if n < 0 || n > cap then
                invalid_arg "Zstd.Decompress_stream.read: reader returned invalid length";
              if n = 0 then s.eof <- true
              else E.push_input s.engine s.scratch 0 n
            end;
            if E.needs_input s.engine && s.eof then begin
              if E.last_ret s.engine <> 0 then
                raise (Error "truncated compressed data");
              continue := false
            end else begin
              let (consumed, produced) = E.step s.engine in
              if produced = 0 && consumed = 0 && E.last_ret s.engine <> 0 then
                raise (Error "decompression made no progress")
            end
          end
        done;
        !total
      with exn ->
        E.close s.engine;
        raise exn
end
