open C_frontend

let () =
  (* Test each conditional one by one, all in same input *)
  let input = {|START
#define FEATURE_A
#ifdef FEATURE_A
inside_ifdef_a
#endif
AFTER_IFDEF

#ifndef FEATURE_B
inside_ifndef_b
#endif
AFTER_IFNDEF

#define VERSION 2
#if VERSION >= 2
inside_if
#endif
AFTER_IF
END|} in
  
  let output = Preprocessor.preprocess_string ~filename:"test.c" input in
  Printf.printf "Output:\n%s\n" output