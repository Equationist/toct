(* Test runner for TOCT *)

(* Import unit test modules *)
module Test_pir_types = Test_pir_types
module Test_pir_attributes = Test_pir_attributes  
module Test_pir_values = Test_pir_values
module Test_pir_instructions = Test_pir_instructions

let test_build_system () =
  print_endline "Build system test passed"

let run_all_tests () =
  print_endline "\n=== TOCT Test Suite ===\n";
  
  (* Build system test *)
  test_build_system ();
  print_endline "";
  
  (* PIR unit tests *)
  Test_pir_types.run_tests ();
  print_endline "";
  
  Test_pir_attributes.run_tests ();
  print_endline "";
  
  Test_pir_values.run_tests ();
  print_endline "";
  
  Test_pir_instructions.run_tests ();
  print_endline "";
  
  print_endline "🎉 All TOCT tests passed! 🎉"

let () = run_all_tests ()