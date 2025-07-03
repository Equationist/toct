open C_frontend

let () =
  let input = {|#define FEATURE_A
#define VERSION 2

#ifdef FEATURE_A
int feature_a = 1;
#else
int feature_a = 0;
#endif

#ifndef FEATURE_B
int feature_b = 0;
#endif

#if VERSION >= 2
int new_version = 1;
#else
int old_version = 1;
#endif|} in
  
  let output = Preprocessor.preprocess_string ~filename:"test.c" input in
  Printf.printf "Output:\n%s\n" output