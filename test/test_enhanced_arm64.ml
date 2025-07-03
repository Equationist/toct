(* Test enhanced ARM64 instruction patterns *)

open Compilerkit_pir
open Compilerkit_backend
module Builder = Compilerkit_pir.Builder

let test_remainder_ops () =
  print_endline "Testing ARM64 remainder operations...";
  
  let func = Builder.(
    let params = [("a", Types.Scalar Types.I64); ("b", Types.Scalar Types.I64)] in
    
    build_function "test_remainder" params (Some (Types.Scalar Types.I64)) (
      let* a = lookup_value "a" in
      let* b = lookup_value "b" in
      
      (* Test signed remainder *)
      let* srem = Builder.emit_instr ~result:"srem" 
        (Instructions.Binop (Instructions.Srem, Instructions.NoFlag, a, b))
        (Attributes.empty ()) in
      
      (* Test unsigned remainder *)
      let* urem = Builder.emit_instr ~result:"urem"
        (Instructions.Binop (Instructions.Urem, Instructions.NoFlag, a, b))
        (Attributes.empty ()) in
      
      (* Add them for result *)
      let* result = add "result" srem urem in
      
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
  
  print_endline "✓ Remainder operations test completed"

let test_bit_counting () =
  print_endline "\nTesting ARM64 bit counting operations...";
  
  let func = Builder.(
    let params = [("x", Types.Scalar Types.I64)] in
    
    build_function "test_bitcount" params (Some (Types.Scalar Types.I64)) (
      let* x = lookup_value "x" in
      
      (* Test CLZ (count leading zeros) *)
      let* clz = Builder.emit_instr ~result:"clz"
        (Instructions.Binop (Instructions.Clz, Instructions.NoFlag, x, x))
        (Attributes.empty ()) in
      
      (* Test CTZ (count trailing zeros) *)
      let* ctz = Builder.emit_instr ~result:"ctz"
        (Instructions.Binop (Instructions.Ctz, Instructions.NoFlag, x, x))
        (Attributes.empty ()) in
      
      (* Test POPCNT (population count) *)
      let* popcnt = Builder.emit_instr ~result:"popcnt"
        (Instructions.Binop (Instructions.Popcnt, Instructions.NoFlag, x, x))
        (Attributes.empty ()) in
      
      (* Add them all *)
      let* tmp = add "tmp" clz ctz in
      let* result = add "result" tmp popcnt in
      
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
  
  print_endline "✓ Bit counting operations test completed"

let test_extended_conversions () =
  print_endline "\nTesting ARM64 extended type conversions...";
  
  let func = Builder.(
    let params = [("u", Types.Scalar Types.I32); ("f", Types.Scalar Types.F32)] in
    
    build_function "test_ext_conv" params (Some (Types.Scalar Types.F64)) (
      let* u = lookup_value "u" in
      let* f = lookup_value "f" in
      
      (* Test unsigned int to float *)
      let* uf = Builder.emit_instr ~result:"uf"
        (Instructions.Cast (Instructions.Uitofp (u, Types.Scalar Types.F64)))
        (Attributes.empty ()) in
      
      (* Test float extend *)
      let* f_ext = Builder.emit_instr ~result:"f_ext"
        (Instructions.Cast (Instructions.Fpext (f, Types.Scalar Types.F64)))
        (Attributes.empty ()) in
      
      (* Test float to unsigned int *)
      let* fu = Builder.emit_instr ~result:"fu"
        (Instructions.Cast (Instructions.Fptoui (f, Types.Scalar Types.I32)))
        (Attributes.empty ()) in
      
      (* Convert back to float and add *)
      let* fu_f = Builder.emit_instr ~result:"fu_f"
        (Instructions.Cast (Instructions.Uitofp (fu, Types.Scalar Types.F64)))
        (Attributes.empty ()) in
      
      let* tmp = Builder.emit_instr ~result:"tmp"
        (Instructions.Binop (Instructions.Fadd, Instructions.NoFlag, uf, f_ext))
        (Attributes.empty ()) in
      
      let* result = Builder.emit_instr ~result:"result"
        (Instructions.Binop (Instructions.Fadd, Instructions.NoFlag, tmp, fu_f))
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
  
  print_endline "✓ Extended conversions test completed"

let test_small_int_ops () =
  print_endline "\nTesting ARM64 small integer operations...";
  
  let func = Builder.(
    let params = [("a", Types.Scalar Types.I8); ("b", Types.Scalar Types.I16)] in
    
    build_function "test_small_int" params (Some (Types.Scalar Types.I32)) (
      let* a = lookup_value "a" in
      let* b = lookup_value "b" in
      
      (* Extend i8 to i16 *)
      let* a_ext = Builder.emit_instr ~result:"a_ext"
        (Instructions.Cast (Instructions.Sext (a, Types.Scalar Types.I16)))
        (Attributes.empty ()) in
      
      (* Add as i16 *)
      let* sum16 = Builder.emit_instr ~result:"sum16"
        (Instructions.Binop (Instructions.Add, Instructions.NoFlag, a_ext, b))
        (Attributes.empty ()) in
      
      (* Zero extend to i32 *)
      let* result = Builder.emit_instr ~result:"result"
        (Instructions.Cast (Instructions.Zext (sum16, Types.Scalar Types.I32)))
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
  
  print_endline "✓ Small integer operations test completed"

let () =
  print_endline "=== Enhanced ARM64 Backend Tests ===";
  test_remainder_ops ();
  test_bit_counting ();
  test_extended_conversions ();
  test_small_int_ops ();
  print_endline "\nAll enhanced ARM64 tests completed! ✓"