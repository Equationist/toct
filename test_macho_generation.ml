(* Test Mach-O object file generation *)

open Compilerkit_pir
open Compilerkit_backend
module Builder = Compilerkit_pir.Builder

let () =
  (* Create a simple function that uses various instruction patterns *)
  let func = Builder.(
    let params = [("a", Types.Scalar Types.I64); ("b", Types.Scalar Types.I64)] in
    
    build_function "compute" params (Some (Types.Scalar Types.I64)) (
      let* a = lookup_value "a" in
      let* b = lookup_value "b" in
      
      (* Various operations to test patterns *)
      let* sum = add "sum" a b in
      let* diff = sub "diff" sum b in
      let* prod = mul "prod" diff a in
      
      (* Test remainder *)
      let* rem = Builder.emit_instr ~result:"rem"
        (Instructions.Binop (Instructions.Srem, Instructions.NoFlag, prod, b))
        (Attributes.empty ()) in
      
      (* Test bit counting *)
      let* clz = Builder.emit_instr ~result:"clz"
        (Instructions.Binop (Instructions.Clz, Instructions.NoFlag, rem, rem))
        (Attributes.empty ()) in
      
      ret (Some clz)
    )
  ) in
  
  let m = {
    Module_ir.items = [Module_ir.FuncDecl func];
    Module_ir.module_attrs = Attributes.empty ();
  } in
  
  (* Generate object file *)
  print_endline "Generating Mach-O object file...";
  (try
    Codegen.generate_object_file ~target:ARM64 ~filename:"test_complete.o" m;
    print_endline "✓ Successfully generated test_complete.o";
    
    (* Check if the file was created *)
    if Sys.file_exists "test_complete.o" then begin
      let stats = Unix.stat "test_complete.o" in
      Printf.printf "✓ Object file size: %d bytes\n" stats.Unix.st_size;
      
      (* Optionally run otool to examine it *)
      print_endline "\nRunning otool -h to check Mach-O header:";
      ignore (Sys.command "otool -h test_complete.o 2>/dev/null || echo 'otool not available'");
    end
  with e ->
    Printf.printf "Error: %s\n" (Printexc.to_string e))