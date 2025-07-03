open C_frontend

let () =
  (* Minimal test case *)
  let test1 = {|#define A
#ifdef A
one
#endif
two|} in
  
  Printf.printf "Test 1:\n";
  let out1 = Preprocessor.preprocess_string ~filename:"test.c" test1 in
  Printf.printf "Output: [%s]\n" out1;
  Printf.printf "Length: %d\n\n" (String.length out1);
  
  (* Two conditionals *)
  let test2 = {|#define A
#ifdef A
one
#endif
#ifndef B
two
#endif
three|} in
  
  Printf.printf "Test 2:\n";
  let out2 = Preprocessor.preprocess_string ~filename:"test.c" test2 in
  Printf.printf "Output: [%s]\n" out2;
  Printf.printf "Length: %d\n" (String.length out2)