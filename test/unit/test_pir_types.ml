(* Unit tests for PIR Types module *)

open Compilerkit_pir.Types

(* Test scalar type sizes *)
let test_scalar_sizes () =
  assert (size_of (Scalar I1) = 1);
  assert (size_of (Scalar I8) = 1);
  assert (size_of (Scalar I16) = 2);
  assert (size_of (Scalar I32) = 4);
  assert (size_of (Scalar I64) = 8);
  assert (size_of (Scalar F32) = 4);
  assert (size_of (Scalar F64) = 8);
  print_endline "✓ Scalar type sizes correct"

(* Test type equality *)
let test_type_equality () =
  assert (equal (Scalar I32) (Scalar I32));
  assert (not (equal (Scalar I32) (Scalar I64)));
  assert (equal (Vector (4, I32)) (Vector (4, I32)));
  assert (not (equal (Vector (4, I32)) (Vector (8, I32))));
  assert (equal (Array (10, Scalar I8)) (Array (10, Scalar I8)));
  assert (not (equal (Array (10, Scalar I8)) (Array (5, Scalar I8))));
  print_endline "✓ Type equality works correctly"

(* Test type validation *)
let test_type_validation () =
  assert (is_integer (Scalar I32));
  assert (is_integer (Scalar I1));
  assert (not (is_integer (Scalar F32)));
  assert (is_float (Scalar F64));
  assert (not (is_float (Scalar I32)));
  assert (is_vector (Vector (4, I32)));
  assert (not (is_vector (Scalar I32)));
  assert (is_aggregate (Array (10, Scalar I8)));
  assert (is_aggregate (Struct [Scalar I32; Scalar F64]));
  assert (not (is_aggregate (Scalar I32)));
  print_endline "✓ Type validation predicates work"

(* Test struct layout *)
let test_struct_layout () =
  (* Simple struct: {i32, i64} *)
  let struct_ty = Struct [Scalar I32; Scalar I64] in
  let expected_size = 16 in (* 4 bytes i32 + 4 padding + 8 bytes i64 *)
  assert (size_of struct_ty = expected_size);
  
  (* Packed struct: no padding *)
  let packed_ty = PackedStruct [Scalar I32; Scalar I64] in
  let expected_packed_size = 12 in (* 4 + 8 = 12, no padding *)
  assert (size_of packed_ty = expected_packed_size);
  print_endline "✓ Struct layout calculations correct"

(* Test vector types *)
let test_vector_types () =
  let vec4_i32 = Vector (4, I32) in
  assert (size_of vec4_i32 = 16); (* 4 * 4 bytes *)
  assert (alignment_of vec4_i32 = 4); (* Alignment of scalar type *)
  
  let vec8_f32 = Vector (8, F32) in
  assert (size_of vec8_f32 = 32); (* 8 * 4 bytes *)
  print_endline "✓ Vector types work correctly"

(* Test array types *)
let test_array_types () =
  let arr10_i8 = Array (10, Scalar I8) in
  assert (size_of arr10_i8 = 10);
  assert (alignment_of arr10_i8 = 1);
  
  let arr5_i64 = Array (5, Scalar I64) in
  assert (size_of arr5_i64 = 40); (* 5 * 8 bytes *)
  assert (alignment_of arr5_i64 = 8);
  print_endline "✓ Array types work correctly"

(* Test pretty printing *)
let test_pretty_printing () =
  assert (string_of_ty (Scalar I32) = "i32");
  assert (string_of_ty (Vector (4, F32)) = "v4xf32");
  assert (string_of_ty (Array (10, Scalar I8)) = "array[10]i8");
  assert (string_of_ty Ptr = "ptr");
  
  let struct_str = string_of_ty (Struct [Scalar I32; Scalar F64]) in
  assert (struct_str = "struct<<i32, f64>>");
  print_endline "✓ Pretty printing works correctly"

(* Run all tests *)
let run_tests () =
  print_endline "Running PIR Types tests...";
  test_scalar_sizes ();
  test_type_equality ();
  test_type_validation ();
  test_struct_layout ();
  test_vector_types ();
  test_array_types ();
  test_pretty_printing ();
  print_endline "All PIR Types tests passed! ✅"