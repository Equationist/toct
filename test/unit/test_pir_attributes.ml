(* Unit tests for PIR Attributes module *)

open Compilerkit_pir.Attributes

(* Test basic attribute operations *)
let test_basic_operations () =
  let attrs = empty () in
  assert (Hashtbl.length attrs = 0);
  
  let attrs2 = add "test" (Bool true) attrs in
  assert (has "test" attrs2);
  assert (not (has "missing" attrs2));
  
  match get_opt "test" attrs2 with
  | Some (Bool true) -> ()
  | _ -> failwith "Expected Bool true";
  
  assert (get_opt "missing" attrs2 = None);
  print_endline "✓ Basic attribute operations work"

(* Test JSON values *)
let test_json_values () =
  let attrs = empty () in
  let attrs = add "int" (Int 42) attrs in
  let attrs = add "float" (Float 3.14) attrs in  
  let attrs = add "string" (String "hello") attrs in
  let attrs = add "bool" (Bool false) attrs in
  let attrs = add "null" Null attrs in
  let attrs = add "array" (Array [Int 1; Int 2; Int 3]) attrs in
  
  assert (get_opt "int" attrs = Some (Int 42));
  assert (get_opt "float" attrs = Some (Float 3.14));
  assert (get_opt "string" attrs = Some (String "hello"));
  assert (get_opt "bool" attrs = Some (Bool false));
  assert (get_opt "null" attrs = Some Null);
  print_endline "✓ JSON value types work correctly"

(* Test attribute helpers *)
let test_helpers () =
  let attrs = empty () in
  
  (* Test range helper *)
  let attrs = Helpers.set_range 0 100 attrs in
  assert (Helpers.get_range attrs = Some (0, 100));
  
  (* Test nonnull helper *)
  let attrs = Helpers.set_nonnull attrs in
  assert (Helpers.is_nonnull attrs = true);
  
  (* Test pure helper *)
  let attrs = Helpers.set_pure attrs in
  assert (Helpers.is_pure attrs = true);
  
  (* Test type helper *)
  let attrs = Helpers.set_type "i32" attrs in
  assert (Helpers.get_type attrs = Some "i32");
  print_endline "✓ Attribute helpers work correctly"

(* Test contract helpers *)
let test_contracts () =
  let attrs = empty () in
  let attrs = Helpers.set_precondition "x > 0" attrs in
  let attrs = Helpers.set_postcondition "ret > x" attrs in
  let attrs = Helpers.set_invariant "acc >= 0" attrs in
  
  assert (Helpers.get_precondition attrs = Some "x > 0");
  assert (Helpers.get_postcondition attrs = Some "ret > x");
  assert (Helpers.get_invariant attrs = Some "acc >= 0");
  print_endline "✓ Contract attributes work correctly"

(* Test merging *)
let test_merging () =
  let attrs1 = empty () in
  let attrs1 = add "a" (Int 1) attrs1 in
  let attrs1 = add "b" (Int 2) attrs1 in
  
  let attrs2 = empty () in
  let attrs2 = add "b" (Int 20) attrs2 in  (* Override *)
  let attrs2 = add "c" (Int 3) attrs2 in
  
  let merged = merge attrs1 attrs2 in
  assert (get_opt "a" merged = Some (Int 1));
  assert (get_opt "b" merged = Some (Int 20)); (* attrs2 overrides *)
  assert (get_opt "c" merged = Some (Int 3));
  print_endline "✓ Attribute merging works correctly"

(* Test JSON pretty printing *)
let test_json_printing () =
  assert (string_of_json (Bool true) = "true");
  assert (string_of_json (Int 42) = "42");
  assert (string_of_json (String "test") = "\"test\"");
  assert (string_of_json Null = "null");
  
  let arr = Array [Int 1; Int 2] in
  assert (string_of_json arr = "[1,2]");
  
  let obj = Object [("key", String "value")] in
  assert (string_of_json obj = "{\"key\":\"value\"}");
  print_endline "✓ JSON pretty printing works"

(* Test attribute pretty printing *)
let test_attr_printing () =
  let attrs = empty () in
  assert (string_of_attrs attrs = "");
  
  let attrs = add "range" (Array [Int 0; Int 255]) attrs in
  let attrs = add "nonnull" (Bool true) attrs in
  let result = string_of_attrs attrs in
  
  (* Should contain both attributes in sorted order *)
  assert (String.contains result 'n'); (* nonnull *)
  assert (String.contains result 'r'); (* range *)
  assert (String.contains result '@'); (* @{ prefix *)
  print_endline "✓ Attribute pretty printing works"

(* Run all tests *)
let run_tests () =
  print_endline "Running PIR Attributes tests...";
  test_basic_operations ();
  test_json_values ();
  test_helpers ();
  test_contracts ();
  test_merging ();
  test_json_printing ();
  test_attr_printing ();
  print_endline "All PIR Attributes tests passed! ✅"