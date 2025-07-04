(** Test program for the C preprocessor *)

open C_frontend

(* Helper function to check if a string contains a substring *)
let string_contains_substring str sub =
  let len_str = String.length str in
  let len_sub = String.length sub in
  let rec check pos =
    if pos + len_sub > len_str then false
    else if String.sub str pos len_sub = sub then true
    else check (pos + 1)
  in
  check 0

let test_basic_defines () =
  let input = {|#define MAX_SIZE 100
#define PI 3.14159
#define GREETING "Hello, World!"

int size = MAX_SIZE;
float pi_val = PI;
char* msg = GREETING;|} in
  
  let output = Preprocessor.preprocess_string ~filename:"test.c" input in
  Printf.printf "Test basic defines:\n%s\n\n" output;
  
  (* Check that macros were expanded *)
  assert (String.contains output '1' && String.contains output '0');
  assert (String.contains output '3' && String.contains output '.');
  assert (String.contains output 'H' && String.contains output 'e')

let test_function_macros () =
  let input = {|#define MAX(a, b) ((a) > (b) ? (a) : (b))
#define SQUARE(x) ((x) * (x))

int max_val = MAX(10, 20);
int squared = SQUARE(5);
int complex = SQUARE(x + 1);|} in
  
  let output = Preprocessor.preprocess_string ~filename:"test.c" input in
  Printf.printf "Test function macros:\n%s\n\n" output;
  
  (* Check that function macros were expanded *)
  assert (String.contains output '?' && String.contains output ':');
  assert (String.contains output '*')

let test_stringification () =
  let input = {|#define STRINGIFY(x) #x
#define PRINT_VAR(var) printf(#var " = %d\n", var)

char* s1 = STRINGIFY(hello);
char* s2 = STRINGIFY(123);
PRINT_VAR(value);|} in
  
  let output = Preprocessor.preprocess_string ~filename:"test.c" input in
  Printf.printf "Test stringification:\n%s\n\n" output;
  
  (* Check that stringification worked *)
  assert (String.contains output '"');
  assert (string_contains_substring output "\"hello\"");
  assert (string_contains_substring output "\"123\"");
  assert (string_contains_substring output "\"value\"")

let test_token_pasting () =
  let input = {|#define CONCAT(a, b) a##b
#define MAKE_VAR(name) int var_##name

MAKE_VAR(counter);
int x = CONCAT(12, 34);|} in
  
  let output = Preprocessor.preprocess_string ~filename:"test.c" input in
  Printf.printf "Test token pasting:\n%s\n\n" output;
  
  (* Check that token pasting worked *)
  assert (string_contains_substring output "var_counter");
  assert (string_contains_substring output "1234")

let test_conditionals () =
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
  Printf.printf "Test conditionals:\n%s\n\n" output;
  
  (* Check that conditionals worked correctly *)
  assert (string_contains_substring output "feature_a = 1");
  assert (string_contains_substring output "feature_b = 0");
  assert (string_contains_substring output "new_version = 1");
  assert (not (string_contains_substring output "old_version"))

let test_predefined_macros () =
  let input = {|__FILE__
__LINE__
__DATE__
__TIME__|} in
  
  let output = Preprocessor.preprocess_string ~filename:"test.c" input in
  Printf.printf "Test predefined macros:\n%s\n\n" output;
  
  (* Check that predefined macros were expanded *)
  assert (string_contains_substring output "\"test.c\"");
  assert (String.contains output '1' || String.contains output '2');
  assert (String.contains output '"')

let test_nested_macros () =
  let input = {|#define A 1
#define B A + 2
#define C B * 3

int value = C;|} in
  
  let output = Preprocessor.preprocess_string ~filename:"test.c" input in
  Printf.printf "Test nested macros:\n%s\n\n" output;
  
  (* Check that nested expansion worked *)
  assert (string_contains_substring output "1 + 2 * 3")

let test_complex_expression () =
  let input = {|#define MAX_SIZE 100
#define MIN_SIZE 10
#define MID_SIZE ((MAX_SIZE + MIN_SIZE) / 2)

#if MAX_SIZE > 50 && MIN_SIZE < 20
int size_ok = 1;
#endif

#if MID_SIZE == 55
int mid_correct = 1;
#endif|} in
  
  let output = Preprocessor.preprocess_string ~filename:"test.c" input in
  Printf.printf "Test complex expressions:\n%s\n\n" output;
  
  (* Check that expressions were evaluated correctly *)
  assert (string_contains_substring output "size_ok = 1");
  assert (string_contains_substring output "mid_correct = 1")

let run_tests () =
  Printf.printf "Running C preprocessor tests...\n\n";
  
  try
    test_basic_defines ();
    Printf.printf "✓ Basic defines test passed\n";
    
    test_function_macros ();
    Printf.printf "✓ Function macros test passed\n";
    
    test_stringification ();
    Printf.printf "✓ Stringification test passed\n";
    
    test_token_pasting ();
    Printf.printf "✓ Token pasting test passed\n";
    
    test_conditionals ();
    Printf.printf "✓ Conditionals test passed\n";
    
    test_predefined_macros ();
    Printf.printf "✓ Predefined macros test passed\n";
    
    test_nested_macros ();
    Printf.printf "✓ Nested macros test passed\n";
    
    test_complex_expression ();
    Printf.printf "✓ Complex expression test passed\n";
    
    Printf.printf "\nAll tests passed! ✨\n"
  with e ->
    Printf.printf "Test failed: %s\n" (Printexc.to_string e);
    exit 1

let () = run_tests ()