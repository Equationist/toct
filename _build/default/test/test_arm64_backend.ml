(* Comprehensive tests for ARM64 backend *)

open Compilerkit_pir
open Compilerkit_backend
module Builder = Compilerkit_pir.Builder

let test_arm64_integer_ops () =
  print_endline "Testing ARM64 integer operations...";
  
  (* Test various integer operations *)
  let func = Builder.(
    let params = [("a", Types.Scalar Types.I64); ("b", Types.Scalar Types.I64)] in
    
    build_function "test_int_ops" params (Some (Types.Scalar Types.I64)) (
      let* a = lookup_value "a" in
      let* b = lookup_value "b" in
      
      (* Test various operations *)
      let* sum = add "sum" a b in
      let* diff = sub "diff" sum b in
      let* prod = mul "prod" diff a in
      let* quot = Builder.emit_instr ~result:"quot" 
        (Instructions.Binop (Instructions.Sdiv, Instructions.NoFlag, prod, b)) 
        (Attributes.empty ()) in
      let* shifted = Builder.emit_instr ~result:"shifted"
        (Instructions.Binop (Instructions.Shl, Instructions.NoFlag, quot, 
          Values.create_simple_value (Types.Scalar Types.I64)))
        (Attributes.empty ()) in
      
      ret (Some shifted)
    )
  ) in
  
  let m = {
    Module_ir.items = [Module_ir.FuncDecl func];
    Module_ir.module_attrs = Attributes.empty ();
  } in
  
  print_endline "\nGenerating ARM64 assembly:";
  (try
    let arm64_asm = Codegen.generate_for_target Codegen.ARM64 m in
    print_endline arm64_asm
  with e ->
    Printf.printf "Error: %s\n" (Printexc.to_string e));
  
  print_endline "✓ Integer operations test completed"

let test_arm64_float_ops () =
  print_endline "\nTesting ARM64 floating-point operations...";
  
  let func = Builder.(
    let params = [("x", Types.Scalar Types.F64); ("y", Types.Scalar Types.F64)] in
    
    build_function "test_float_ops" params (Some (Types.Scalar Types.F64)) (
      let* x = lookup_value "x" in
      let* y = lookup_value "y" in
      
      (* Test floating-point operations *)
      let* sum = Builder.emit_instr ~result:"sum"
        (Instructions.Binop (Instructions.Fadd, Instructions.NoFlag, x, y))
        (Attributes.empty ()) in
      let* prod = Builder.emit_instr ~result:"prod"
        (Instructions.Binop (Instructions.Fmul, Instructions.NoFlag, sum, x))
        (Attributes.empty ()) in
      let* quot = Builder.emit_instr ~result:"quot"
        (Instructions.Binop (Instructions.Fdiv, Instructions.NoFlag, prod, y))
        (Attributes.empty ()) in
      
      ret (Some quot)
    )
  ) in
  
  let m = {
    Module_ir.items = [Module_ir.FuncDecl func];
    Module_ir.module_attrs = Attributes.empty ();
  } in
  
  print_endline "\nGenerating ARM64 assembly:";
  (try
    let arm64_asm = Codegen.generate_for_target Codegen.ARM64 m in
    print_endline arm64_asm
  with e ->
    Printf.printf "Error: %s\n" (Printexc.to_string e));
  
  print_endline "✓ Floating-point operations test completed"

let test_arm64_comparisons () =
  print_endline "\nTesting ARM64 comparison operations...";
  
  let func = Builder.(
    let params = [("a", Types.Scalar Types.I64); ("b", Types.Scalar Types.I64)] in
    
    build_function "test_compare" params (Some (Types.Scalar Types.I64)) (
      let* a = lookup_value "a" in
      let* b = lookup_value "b" in
      
      (* Integer comparison *)
      let* cmp = icmp "cmp" Instructions.Sgt a b in
      
      (* Conditional select based on comparison *)
      let* result = select "result" cmp a b in
      
      ret (Some result)
    )
  ) in
  
  let m = {
    Module_ir.items = [Module_ir.FuncDecl func];
    Module_ir.module_attrs = Attributes.empty ();
  } in
  
  print_endline "\nGenerating ARM64 assembly:";
  (try
    let arm64_asm = Codegen.generate_for_target Codegen.ARM64 m in
    print_endline arm64_asm
  with e ->
    Printf.printf "Error: %s\n" (Printexc.to_string e));
  
  print_endline "✓ Comparison operations test completed"

let test_arm64_conversions () =
  print_endline "\nTesting ARM64 type conversions...";
  
  let func = Builder.(
    let params = [("i", Types.Scalar Types.I32); ("f", Types.Scalar Types.F32)] in
    
    build_function "test_convert" params (Some (Types.Scalar Types.F64)) (
      let* i = lookup_value "i" in
      let* f = lookup_value "f" in
      
      (* Int to float conversion *)
      let* f_from_i = Builder.emit_instr ~result:"f_from_i"
        (Instructions.Cast (Instructions.Sitofp (i, Types.Scalar Types.F64)))
        (Attributes.empty ()) in
      
      (* Float to float conversion (extend) *)
      let* f64 = Builder.emit_instr ~result:"f64"
        (Instructions.Cast (Instructions.Fpext (f, Types.Scalar Types.F64)))
        (Attributes.empty ()) in
      
      (* Add the results *)
      let* result = Builder.emit_instr ~result:"result"
        (Instructions.Binop (Instructions.Fadd, Instructions.NoFlag, f_from_i, f64))
        (Attributes.empty ()) in
      
      ret (Some result)
    )
  ) in
  
  let m = {
    Module_ir.items = [Module_ir.FuncDecl func];
    Module_ir.module_attrs = Attributes.empty ();
  } in
  
  print_endline "\nGenerating ARM64 assembly:";
  (try
    let arm64_asm = Codegen.generate_for_target Codegen.ARM64 m in
    print_endline arm64_asm
  with e ->
    Printf.printf "Error: %s\n" (Printexc.to_string e));
  
  print_endline "✓ Type conversion test completed"

let test_arm64_constant_materialization () =
  print_endline "\nTesting ARM64 constant materialization...";
  
  (* Just test that we can generate assembly for functions with constants *)
  let func = Builder.(
    let params = [] in
    
    build_function "test_constants" params (Some (Types.Scalar Types.I64)) (
      (* Create some constants *)
      let* zero = const_int (Types.Scalar Types.I64) 0 in
      let* small = const_int (Types.Scalar Types.I64) 42 in
      let* medium = const_int (Types.Scalar Types.I64) 0xFFFF in
      let* large = const_int (Types.Scalar Types.I64) 0x1234_5678 in
      
      (* Do some operations with them *)
      let* result = add "result" small medium in
      let* result2 = add "result2" result large in
      
      ret (Some result2)
    )
  ) in
  
  let m = {
    Module_ir.items = [Module_ir.FuncDecl func];
    Module_ir.module_attrs = Attributes.empty ();
  } in
  
  print_endline "\nGenerating ARM64 assembly:";
  (try
    let arm64_asm = Codegen.generate_for_target Codegen.ARM64 m in
    print_endline arm64_asm
  with e ->
    Printf.printf "Error: %s\n" (Printexc.to_string e));
  
  print_endline "\n✓ Constant materialization test completed"

let () =
  print_endline "=== ARM64 Backend Tests ===";
  test_arm64_integer_ops ();
  test_arm64_float_ops ();
  test_arm64_comparisons ();
  test_arm64_conversions ();
  test_arm64_constant_materialization ();
  print_endline "\nAll ARM64 backend tests completed! ✓"