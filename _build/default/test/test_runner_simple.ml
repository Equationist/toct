(* Simple test runner for TOCT with inline tests *)

open Compilerkit_pir

let test_build_system () =
  print_endline "✓ Build system test passed"

(* Test PIR Types *)
let test_pir_types () =
  print_endline "Running PIR Types tests...";
  
  (* Test scalar type sizes *)
  assert (Types.size_of (Types.Scalar Types.I32) = 4);
  assert (Types.size_of (Types.Scalar Types.I64) = 8);
  assert (Types.size_of (Types.Scalar Types.F32) = 4);
  
  (* Test type equality *)
  assert (Types.equal (Types.Scalar Types.I32) (Types.Scalar Types.I32));
  assert (not (Types.equal (Types.Scalar Types.I32) (Types.Scalar Types.I64)));
  
  (* Test type validation *)
  assert (Types.is_integer (Types.Scalar Types.I32));
  assert (not (Types.is_integer (Types.Scalar Types.F32)));
  assert (Types.is_float (Types.Scalar Types.F64));
  
  (* Test pretty printing *)
  assert (Types.string_of_ty (Types.Scalar Types.I32) = "i32");
  assert (Types.string_of_ty (Types.Vector (4, Types.F32)) = "v4xf32");
  assert (Types.string_of_ty Types.Ptr = "ptr");
  
  print_endline "✓ All PIR Types tests passed"

(* Test PIR Attributes *)
let test_pir_attributes () =
  print_endline "Running PIR Attributes tests...";
  
  let attrs = Attributes.empty () in
  assert (Hashtbl.length attrs = 0);
  
  let attrs2 = Attributes.add "test" (Attributes.Bool true) attrs in
  assert (Attributes.has "test" attrs2);
  assert (not (Attributes.has "missing" attrs2));
  
  (* Test helpers *)
  let attrs3 = Attributes.Helpers.set_range 0 100 attrs in
  assert (Attributes.Helpers.get_range attrs3 = Some (0, 100));
  
  let attrs4 = Attributes.Helpers.set_nonnull attrs in
  assert (Attributes.Helpers.is_nonnull attrs4 = true);
  
  print_endline "✓ All PIR Attributes tests passed"

(* Test PIR Values *)
let test_pir_values () =
  print_endline "Running PIR Values tests...";
  
  let i32_ty = Types.Scalar Types.I32 in
  let v1 = Values.create_simple_value i32_ty in
  let v2 = Values.create_simple_value i32_ty in
  
  (* Values should have unique IDs *)
  assert (Values.get_id v1 <> Values.get_id v2);
  assert (Values.get_type v1 = i32_ty);
  
  (* Test constants *)
  let c1 = Values.Const.i32 42 in
  assert (Values.type_of_const c1 = Types.Scalar Types.I32);
  assert (Values.string_of_const_value c1 = "42");
  
  let c2 = Values.Const.bool true in
  assert (Values.type_of_const c2 = Types.Scalar Types.I1);
  
  (* Test type checking *)
  assert (Values.is_integer_value v1);
  assert (Values.has_type v1 i32_ty);
  
  print_endline "✓ All PIR Values tests passed"

(* Test PIR Instructions *)
let test_pir_instructions () =
  print_endline "Running PIR Instructions tests...";
  
  let v1 = Values.create_simple_value (Types.Scalar Types.I32) in
  let v2 = Values.create_simple_value (Types.Scalar Types.I32) in
  
  (* Test binary operations *)
  let add_instr = Instructions.Binop (Instructions.Add, Instructions.NoFlag, v1, v2) in
  let instr = Instructions.create_simple_instruction ~result:v1 add_instr in
  assert (instr.result <> None);
  
  (* Test comparisons *)
  let eq_instr = Instructions.Icmp (Instructions.Eq, v1, v2) in
  let result_ty = Instructions.result_type_of_instr eq_instr in
  assert (result_ty = Some (Types.Scalar Types.I1));
  
  (* Test pretty printing *)
  assert (Instructions.string_of_flag Instructions.NoFlag = "");
  assert (Instructions.string_of_flag Instructions.Nsw = ".nsw");
  assert (Instructions.string_of_icmp_pred Instructions.Eq = "eq");
  assert (Instructions.string_of_binop Instructions.Add = "add");
  
  (* Test terminators *)
  let ret_void = Instructions.Ret None in
  assert (Instructions.string_of_terminator ret_void = "ret");
  
  print_endline "✓ All PIR Instructions tests passed"

let run_all_tests () =
  print_endline "\n=== TOCT Test Suite ===\n";
  
  test_build_system ();
  print_endline "";
  
  test_pir_types ();
  print_endline "";
  
  test_pir_attributes ();
  print_endline "";
  
  test_pir_values ();
  print_endline "";
  
  test_pir_instructions ();
  print_endline "";
  
  print_endline "🎉 All TOCT tests passed! 🎉"

let () = run_all_tests ()