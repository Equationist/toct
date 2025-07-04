(** Simple test to verify the type system modules compile *)

open C_type_system

let test_basic_types () =
  Printf.printf "Testing basic C type system...\n";
  
  (* Test basic integer types *)
  let int_type = Int c_int in
  let float_type = Float c_float in
  let ptr_type = Pointer (int_type, []) in
  
  Printf.printf "Created types:\n";
  Printf.printf "  int: %s\n" (string_of_c_type int_type);
  Printf.printf "  float: %s\n" (string_of_c_type float_type);
  Printf.printf "  int*: %s\n" (string_of_c_type ptr_type);
  
  (* Test type compatibility *)
  let env = create_type_env () in
  let compatible = types_compatible env int_type int_type in
  Printf.printf "  int compatible with int: %b\n" compatible;
  
  (* Test integer promotion *)
  let promoted = integer_promote (Int c_char) in
  Printf.printf "  char promoted to: %s\n" (string_of_c_type promoted);
  
  Printf.printf "Basic type system test completed successfully!\n"

let () =
  try
    test_basic_types ()
  with e ->
    Printf.printf "Error: %s\n" (Printexc.to_string e)