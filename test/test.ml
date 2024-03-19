open ExtLib
open Printf

let test (name,src) =
  let orig = String.length src in
  let a = Array.init 20 (fun level -> Zstd.compress ~level src) in
  a |> Array.iteri begin fun i s ->
    assert (Zstd.get_decompressed_size s = orig);
    let s = Zstd.decompress orig s in
    if s <> src then failwith @@ sprintf "%s : level %d failed" name i;
  end;
  let best = Array.fold_left (fun m s -> min m (String.length s)) (String.length a.(0)) a in
  let best_level = Array.findi (fun s -> String.length s = best) a in
  printf "%50s : best compression %02.1fx at level %d : %d -> %d\n" name (float orig /. float best) best_level orig best

let () =
  let file f = try Some (f, Std.input_file f) with _ -> None in
  let inputs = [
    file "/bin/bash";
    file Sys.executable_name;
    file "/etc/ld.so.cache";
    file "/etc/mailcap";
    Some ("environment", String.concat " " @@ Array.to_list @@ Unix.environment ());
  ] |> List.filter_map (fun x -> x)
  in
  List.iter test inputs
