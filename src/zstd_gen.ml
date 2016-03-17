
let c_headers = "#include <zstd.h> \n #include \"zstd.h\""

let () =
  let ml_out = open_out "src/zstd_generated.ml"
  and c_out = open_out "src/zstd_cstubs.c" in
  let ml_fmt = Format.formatter_of_out_channel ml_out
  and c_fmt = Format.formatter_of_out_channel c_out in
  Format.fprintf c_fmt "%s@\n" c_headers;
  Cstubs.write_c c_fmt ~prefix:"zstd_stub_" (module Zstd_stubs.Bindings);
  Cstubs.write_ml ml_fmt ~prefix:"zstd_stub_" (module Zstd_stubs.Bindings);
  Format.pp_print_flush ml_fmt ();
  Format.pp_print_flush c_fmt ();
  close_out ml_out;
  close_out c_out
