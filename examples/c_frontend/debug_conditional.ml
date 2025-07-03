open C_frontend

let () =
  (* Test each conditional separately *)
  
  Printf.printf "Test 1: Simple ifdef\n";
  let input1 = {|#define FEATURE_A
#ifdef FEATURE_A
int feature_a = 1;
#endif
int after_ifdef = 2;|} in
  let output1 = Preprocessor.preprocess_string ~filename:"test.c" input1 in
  Printf.printf "Output:\n%s\n\n" output1;
  
  Printf.printf "Test 2: Simple ifndef\n";
  let input2 = {|#ifndef FEATURE_B
int feature_b = 0;
#endif
int after_ifndef = 3;|} in
  let output2 = Preprocessor.preprocess_string ~filename:"test.c" input2 in
  Printf.printf "Output:\n%s\n\n" output2;
  
  Printf.printf "Test 3: Simple if\n";
  let input3 = {|#define VERSION 2
#if VERSION >= 2
int new_version = 1;
#endif
int after_if = 4;|} in
  let output3 = Preprocessor.preprocess_string ~filename:"test.c" input3 in
  Printf.printf "Output:\n%s\n\n" output3