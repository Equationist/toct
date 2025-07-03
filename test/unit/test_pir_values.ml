(* Unit tests for PIR Values module *)

open Compilerkit_pir.Types
open Compilerkit_pir.Values
open Compilerkit_pir.Attributes

(* Test value creation *)
let test_value_creation () =
  let i32_ty = Scalar I32 in
  let attrs = empty () in
  let v1 = create_value i32_ty attrs in
  let v2 = create_value i32_ty attrs in
  
  (* Values should have unique IDs *)
  assert (get_id v1 <> get_id v2);
  assert (get_type v1 = i32_ty);
  assert (get_type v2 = i32_ty);
  
  let v3 = create_simple_value i32_ty in
  assert (get_type v3 = i32_ty);
  print_endline "✓ Value creation works correctly"

(* Test constant values *)
let test_constants () =
  (* Integer constants *)
  let c1 = Const.i32 42 in
  assert (type_of_const c1 = Scalar I32);
  assert (string_of_const_value c1 = "42");
  
  let c2 = Const.i64 1000L in
  assert (type_of_const c2 = Scalar I64);
  
  (* Float constants *)
  let c3 = Const.f32 3.14 in
  assert (type_of_const c3 = Scalar F32);
  
  let c4 = Const.f64 2.718 in
  assert (type_of_const c4 = Scalar F64);
  
  (* Boolean constants *)
  let c5 = Const.bool true in
  assert (type_of_const c5 = Scalar I1);
  assert (string_of_const_value c5 = "true");
  
  (* Null pointer *)
  let c6 = Const.null_ptr in
  assert (type_of_const c6 = Ptr);
  assert (string_of_const_value c6 = "null");
  print_endline "✓ Constant values work correctly"

(* Test aggregate constants *)
let test_aggregate_constants () =
  (* Array constant *)
  let elements = [Const.i32 1; Const.i32 2; Const.i32 3] in
  let arr_const = Const.array elements in
  let expected_ty = Array (3, Scalar I32) in
  assert (type_of_const arr_const = expected_ty);
  
  (* Struct constant *)
  let fields = [Const.i32 42; Const.f64 3.14] in
  let struct_const = Const.struct_val fields in
  let expected_struct_ty = Struct [Scalar I32; Scalar F64] in
  assert (type_of_const struct_const = expected_struct_ty);
  print_endline "✓ Aggregate constants work correctly"

(* Test zero and undef *)
let test_special_constants () =
  let i32_ty = Scalar I32 in
  let zero_const = Const.zero i32_ty in
  assert (type_of_const zero_const = i32_ty);
  assert (string_of_const_value zero_const = "zeroinitializer");
  
  let undef_const = Const.undef i32_ty in
  assert (type_of_const undef_const = i32_ty);
  assert (string_of_const_value undef_const = "undef");
  print_endline "✓ Special constants work correctly"

(* Test value attributes *)
let test_value_attributes () =
  let v = create_simple_value (Scalar I32) in
  assert (Hashtbl.length (get_attrs v) = 0);
  
  (* Add range attribute *)
  let v2 = add_attr "range" (Array [Int 0; Int 100]) v in
  assert (has "range" (get_attrs v2));
  
  (* Update attributes *)
  let new_attrs = Helpers.set_nonnull (get_attrs v) in
  let v3 = with_attrs v new_attrs in
  assert (Helpers.is_nonnull (get_attrs v3));
  print_endline "✓ Value attributes work correctly"

(* Test type checking utilities *)
let test_type_checking () =
  let i32_val = create_simple_value (Scalar I32) in
  let f64_val = create_simple_value (Scalar F64) in
  let ptr_val = create_simple_value Ptr in
  let vec_val = create_simple_value (Vector (4, I32)) in
  
  assert (is_integer_value i32_val);
  assert (not (is_integer_value f64_val));
  assert (is_float_value f64_val);
  assert (not (is_float_value i32_val));
  assert (is_pointer_value ptr_val);
  assert (not (is_pointer_value i32_val));
  assert (is_vector_value vec_val);
  assert (not (is_vector_value i32_val));
  
  assert (has_type i32_val (Scalar I32));
  assert (not (has_type i32_val (Scalar I64)));
  print_endline "✓ Type checking utilities work correctly"

(* Test pretty printing *)
let test_pretty_printing () =
  let v = create_simple_value (Scalar I32) in
  let v_str = string_of_value v in
  
  (* Should contain %ID:i32 format *)
  assert (String.contains v_str '%');
  assert (String.contains v_str ':');
  assert (String.contains_substr v_str "i32");
  
  (* Test with attributes *)
  let attrs = Helpers.set_range 0 100 (empty ()) in
  let v_with_attrs = with_attrs v attrs in
  let v_attr_str = string_of_value v_with_attrs in
  assert (String.contains v_attr_str '@');
  print_endline "✓ Value pretty printing works correctly"

(* Helper function for substring checking *)
let string_contains_substr str substr =
  let len_str = String.length str in
  let len_sub = String.length substr in
  let rec search i =
    if i + len_sub > len_str then false
    else if String.sub str i len_sub = substr then true
    else search (i + 1)
  in
  search 0

(* Test validation *)
let test_validation () =
  let i32_const = Const.i32 42 in
  assert (validate_const_value i32_const (Scalar I32));
  assert (not (validate_const_value i32_const (Scalar I64)));
  
  let arr_const = Const.array [Const.i32 1; Const.i32 2] in
  assert (validate_const_value arr_const (Array (2, Scalar I32)));
  assert (not (validate_const_value arr_const (Array (3, Scalar I32))));
  print_endline "✓ Constant validation works correctly"

(* Run all tests *)
let run_tests () =
  print_endline "Running PIR Values tests...";
  test_value_creation ();
  test_constants ();
  test_aggregate_constants ();
  test_special_constants ();
  test_value_attributes ();
  test_type_checking ();
  test_pretty_printing ();
  test_validation ();
  print_endline "All PIR Values tests passed! ✅"