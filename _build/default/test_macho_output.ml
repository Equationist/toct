(* Simple test to generate a Mach-O object file *)

open Compilerkit_pir
open Compilerkit_backend
module Builder = Compilerkit_pir.Builder

let () =
  (* Create a simple add function *)
  let func = Builder.(
    let params = [("a", Types.Scalar Types.I64); ("b", Types.Scalar Types.I64)] in
    
    build_function "add" params (Some (Types.Scalar Types.I64)) (
      let* a = lookup_value "a" in
      let* b = lookup_value "b" in
      let* result = add "result" a b in
      ret (Some result)
    )
  ) in
  
  let m = {
    Module_ir.items = [Module_ir.FuncDecl func];
    Module_ir.module_attrs = Attributes.empty ();
  } in
  
  (* Generate object file *)
  print_endline "Generating Mach-O object file...";
  (try
    Codegen.generate_object_file ~target:ARM64 ~filename:"test_output.o" m;
    print_endline "✓ Successfully generated test_output.o"
  with e ->
    Printf.printf "Error: %s\n" (Printexc.to_string e))