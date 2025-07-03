open C_frontend

let () =
  (* The exact test that's failing *)
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
  
  (* Process in parts to see where it stops *)
  Printf.printf "=== Part 1: Just first conditional ===\n";
  let part1 = {|#define FEATURE_A
#define VERSION 2

#ifdef FEATURE_A
int feature_a = 1;
#else
int feature_a = 0;
#endif|} in
  let out1 = Preprocessor.preprocess_string ~filename:"test.c" part1 in
  Printf.printf "Output: [%s]\n" out1;
  Printf.printf "Length: %d\n\n" (String.length out1);
  
  Printf.printf "=== Part 2: First two conditionals ===\n";
  let part2 = {|#define FEATURE_A
#define VERSION 2

#ifdef FEATURE_A
int feature_a = 1;
#else
int feature_a = 0;
#endif

#ifndef FEATURE_B
int feature_b = 0;
#endif|} in
  let out2 = Preprocessor.preprocess_string ~filename:"test.c" part2 in
  Printf.printf "Output: [%s]\n" out2;
  Printf.printf "Length: %d\n\n" (String.length out2);
  
  Printf.printf "=== Full test ===\n";
  let out_full = Preprocessor.preprocess_string ~filename:"test.c" input in
  Printf.printf "Output: [%s]\n" out_full;
  Printf.printf "Length: %d\n" (String.length out_full)