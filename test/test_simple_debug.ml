(* Simple test to debug pattern matching *)

open Compilerkit_pir
open Compilerkit_backend
module Builder = Compilerkit_pir.Builder

let () =
  (* Create module with a simple function *)
  let func = Builder.(
    build_function "test" [] (Some (Types.Scalar Types.I32)) (
      (* Create constant values *)
      let* a = const_int (Types.Scalar Types.I32) 10L in
      let* b = const_int (Types.Scalar Types.I32) 5L in
      
      (* Add them *)
      let* result = add "result" a b in
      
      (* Return the result *)
      ret (Some result)
    )
  ) in
  
  let m = {
    Module_ir.items = [Module_ir.FuncDecl func];
    Module_ir.module_attrs = Attributes.empty ();
  } in
  
  (* Try to generate ARM64 code *)
  print_endline "\nGenerating ARM64 code:";
  try
    let arm64_asm = Codegen.generate_for_target Codegen.ARM64 m in
    print_endline arm64_asm
  with e ->
    Printf.printf "Error: %s\n" (Printexc.to_string e);
    Printexc.print_backtrace stdout