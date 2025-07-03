(* Unit tests for PIR Instructions module *)

open Compilerkit_pir.Types
open Compilerkit_pir.Values
open Compilerkit_pir.Instructions
open Compilerkit_pir.Attributes

(* Test binary operations *)
let test_binary_operations () =
  let v1 = create_simple_value (Scalar I32) in
  let v2 = create_simple_value (Scalar I32) in
  
  let add_instr = Binop (Add, NoFlag, v1, v2) in
  let instr = create_simple_instruction ~result:(create_simple_value (Scalar I32)) add_instr in
  
  assert (instr.result <> None);
  assert (instr.instr = add_instr);
  
  (* Test with flags *)
  let add_nsw = Binop (Add, Nsw, v1, v2) in
  let add_carry = Binop (Add, Carry, v1, v2) in
  assert (add_nsw <> add_instr);
  assert (add_carry <> add_instr);
  print_endline "✓ Binary operations work correctly"

(* Test comparison operations *)
let test_comparisons () =
  let v1 = create_simple_value (Scalar I32) in
  let v2 = create_simple_value (Scalar I32) in
  
  let eq_instr = Icmp (Eq, v1, v2) in
  let result_ty = result_type_of_instr eq_instr in
  assert (result_ty = Some (Scalar I1));
  
  let ne_instr = Icmp (Ne, v1, v2) in
  let slt_instr = Icmp (Slt, v1, v2) in
  assert (ne_instr <> eq_instr);
  assert (slt_instr <> eq_instr);
  print_endline "✓ Comparison operations work correctly"

(* Test memory operations *)
let test_memory_operations () =
  let ptr_val = create_simple_value Ptr in
  let i32_val = create_simple_value (Scalar I32) in
  let size_val = create_simple_value (Scalar I64) in
  
  (* Load operation *)
  let load_instr = Memory (Load (Scalar I32)) in
  let load_result_ty = result_type_of_instr load_instr in
  assert (load_result_ty = Some (Scalar I32));
  
  (* Store operation *)
  let store_instr = Memory (Store (i32_val, ptr_val)) in
  let store_result_ty = result_type_of_instr store_instr in
  assert (store_result_ty = None); (* Store has no result *)
  
  (* Alloca operation *)
  let alloca_instr = Memory (Alloca (size_val, 8)) in
  let alloca_result_ty = result_type_of_instr alloca_instr in
  assert (alloca_result_ty = Some Ptr);
  print_endline "✓ Memory operations work correctly"

(* Test address calculations *)
let test_address_operations () =
  let ptr_val = create_simple_value Ptr in
  let idx_val = create_simple_value (Scalar I32) in
  let offset_val = create_simple_value (Scalar I64) in
  
  (* GEP operation *)
  let gep_instr = Address (Gep (ptr_val, idx_val)) in
  let gep_result_ty = result_type_of_instr gep_instr in
  assert (gep_result_ty = Some Ptr);
  
  (* Field address *)
  let field_instr = Address (FieldAddr (ptr_val, 2)) in
  let field_result_ty = result_type_of_instr field_instr in
  assert (field_result_ty = Some Ptr);
  
  (* Pointer add *)
  let ptradd_instr = Address (PtrAdd (ptr_val, offset_val)) in
  let ptradd_result_ty = result_type_of_instr ptradd_instr in
  assert (ptradd_result_ty = Some Ptr);
  print_endline "✓ Address operations work correctly"

(* Test select operation *)
let test_select_operation () =
  let cond_val = create_simple_value (Scalar I1) in
  let true_val = create_simple_value (Scalar I32) in
  let false_val = create_simple_value (Scalar I32) in
  
  let select_instr = Select (cond_val, true_val, false_val) in
  let select_result_ty = result_type_of_instr select_instr in
  assert (select_result_ty = Some (Scalar I32));
  print_endline "✓ Select operation works correctly"

(* Test terminators *)
let test_terminators () =
  let ret_void = Ret None in
  let ret_val = Ret (Some (create_simple_value (Scalar I32))) in
  
  let cond_val = create_simple_value (Scalar I1) in
  let br_instr = Br (cond_val, "then_block", "else_block") in
  let jmp_instr = Jmp "target_block" in
  let unreachable_instr = Unreachable in
  
  assert (ret_void <> ret_val);
  assert (br_instr <> jmp_instr);
  assert (jmp_instr <> unreachable_instr);
  print_endline "✓ Terminator operations work correctly"

(* Test basic block creation *)
let test_basic_blocks () =
  let params = [("x", Scalar I32); ("y", Scalar I32)] in
  let v1 = create_simple_value (Scalar I32) in
  let v2 = create_simple_value (Scalar I32) in
  let add_instr = create_simple_instruction (Binop (Add, NoFlag, v1, v2)) in
  let ret_instr = Ret (Some v1) in
  
  let block = create_block "entry" params [add_instr] ret_instr in
  assert (block.label = "entry");
  assert (List.length block.params = 2);
  assert (List.length block.instructions = 1);
  assert (block.terminator = ret_instr);
  print_endline "✓ Basic block creation works correctly"

(* Test function creation *)
let test_functions () =
  let params = [("a", Scalar I32); ("b", Scalar I32)] in
  let return_ty = Some (Scalar I32) in
  let entry_block = create_block "entry" [] [] (Ret None) in
  
  let func = create_func "test_function" params return_ty [entry_block] in
  assert (func.name = "test_function");
  assert (List.length func.params = 2);
  assert (func.return_ty = return_ty);
  assert (List.length func.blocks = 1);
  print_endline "✓ Function creation works correctly"

(* Test pretty printing *)
let test_pretty_printing () =
  (* Test flag printing *)
  assert (string_of_flag NoFlag = "");
  assert (string_of_flag Nsw = ".nsw");
  assert (string_of_flag Carry = ".carry");
  assert (string_of_flag Sat = ".sat");
  
  (* Test predicate printing *)
  assert (string_of_icmp_pred Eq = "eq");
  assert (string_of_icmp_pred Ne = "ne");
  assert (string_of_icmp_pred Slt = "slt");
  
  (* Test binop printing *)
  assert (string_of_binop Add = "add");
  assert (string_of_binop Mul = "mul");
  assert (string_of_binop And = "and");
  
  (* Test terminator printing *)
  assert (string_of_terminator (Ret None) = "ret");
  assert (string_of_terminator Unreachable = "unreachable");
  print_endline "✓ Pretty printing works correctly"

(* Test instruction type inference *)
let test_type_inference () =
  let v1 = create_simple_value (Scalar I32) in
  let v2 = create_simple_value (Scalar I32) in
  
  (* Binary operation should have same type as operands *)
  let add_instr = Binop (Add, NoFlag, v1, v2) in
  assert (result_type_of_instr add_instr = Some (Scalar I32));
  
  (* Comparison should return i1 *)
  let cmp_instr = Icmp (Eq, v1, v2) in
  assert (result_type_of_instr cmp_instr = Some (Scalar I1));
  
  (* Select should have type of selected values *)
  let cond = create_simple_value (Scalar I1) in
  let select_instr = Select (cond, v1, v2) in
  assert (result_type_of_instr select_instr = Some (Scalar I32));
  print_endline "✓ Type inference works correctly"

(* Run all tests *)
let run_tests () =
  print_endline "Running PIR Instructions tests...";
  test_binary_operations ();
  test_comparisons ();
  test_memory_operations ();
  test_address_operations ();
  test_select_operation ();
  test_terminators ();
  test_basic_blocks ();
  test_functions ();
  test_pretty_printing ();
  test_type_inference ();
  print_endline "All PIR Instructions tests passed! ✅"