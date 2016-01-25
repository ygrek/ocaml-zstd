open Ocamlbuild_plugin
;;

rule ("cstubs: *_stubs.ml -> *_generated.ml") ~deps:["%_stubs.ml";"%_gen.native"] ~prods:["%_generated.ml";"%_cstubs.c"]
begin fun env _ ->
  let gen = env "%_gen.native" in
  Cmd (S[ P gen ])
end
;;

(* fix oasis not passing ocamlfind packages to ocamlc when compiling c code *)
flag ["c"; "compile";  "pkg_ctypes.stubs"] & S [ A "-package"; A "ctypes.stubs" ]
;;
