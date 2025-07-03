(** Test preprocessor with actual test files *)

open C_frontend

let test_file path =
  Printf.printf "\n=== Testing %s ===\n" path;
  
  try
    let output = Preprocessor.preprocess_file path in
    Printf.printf "Success! Output length: %d chars\n" (String.length output);
    
    (* Save output for inspection *)
    let out_path = path ^ ".preprocessed" in
    let oc = open_out out_path in
    output_string oc output;
    close_out oc;
    Printf.printf "Output saved to: %s\n" out_path
  with e ->
    Printf.printf "Error: %s\n" (Printexc.to_string e)

let () =
  let test_files = [
    "/Users/ummonkarpe/Development/compilerkit/test/c_frontend/preprocessor/basic/simple_defines_no_includes.c";
    "/Users/ummonkarpe/Development/compilerkit/test/c_frontend/preprocessor/functions/macro_expansion_test.c";
    "/Users/ummonkarpe/Development/compilerkit/test/c_frontend/preprocessor/conditional/conditional_test.c";
  ] in
  
  List.iter test_file test_files