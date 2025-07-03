open C_frontend

let () =
  (* Test what happens after endif *)
  let test1 = {|#ifdef A
content
#endif
after_endif|} in
  
  Printf.printf "Test 1 (undefined A):\n";
  let out1 = Preprocessor.preprocess_string ~filename:"test.c" test1 in
  Printf.printf "Output: [%s]\n" out1;
  Printf.printf "Length: %d\n\n" (String.length out1);
  
  (* With defined A *)
  let test2 = {|#define A
#ifdef A
content
#endif
after_endif|} in
  
  Printf.printf "Test 2 (defined A):\n";
  let out2 = Preprocessor.preprocess_string ~filename:"test.c" test2 in
  Printf.printf "Output: [%s]\n" out2;
  Printf.printf "Length: %d\n\n" (String.length out2);
  
  (* With else branch *)
  let test3 = {|#define A
#ifdef A
true_branch
#else
false_branch
#endif
after_endif|} in
  
  Printf.printf "Test 3 (with else):\n";
  let out3 = Preprocessor.preprocess_string ~filename:"test.c" test3 in
  Printf.printf "Output: [%s]\n" out3;
  Printf.printf "Length: %d\n" (String.length out3)