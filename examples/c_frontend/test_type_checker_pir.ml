(** Test the C type checker and PIR generator *)

open C_frontend

(** Test simple arithmetic function *)
let test_simple_function () =
  let source = {|
int add(int a, int b) {
    return a + b;
}

int main() {
    int result = add(5, 3);
    return result;
}
|} in

  Printf.printf "=== Testing Type Checker and PIR Generator ===\n\n";
  Printf.printf "Source code:\n%s\n" source;
  
  try
    (* Preprocess *)
    let preprocessed = Preprocessor.preprocess_string ~filename:"test.c" source in
    Printf.printf "✓ Preprocessing successful\n";
    
    (* Lex *)
    let tokens = Lexer.lex_string "test.c" preprocessed in
    Printf.printf "✓ Lexing successful (%d tokens)\n" (List.length tokens);
    
    (* Parse *)
    let ast = Parser.parse tokens in
    Printf.printf "✓ Parsing successful\n";
    
    (* Type check *)
    (match C_type_checker.type_check ast with
     | Ok symbol_table ->
       Printf.printf "✓ Type checking successful\n";
       
       (* Generate PIR *)
       (match C_pir_generator.generate_pir symbol_table ast with
        | Ok functions ->
          Printf.printf "✓ PIR generation successful\n";
          Printf.printf "Generated %d functions:\n" (List.length functions);
          
          List.iteri (fun i func ->
            let open Compilerkit_pir.Instructions in
            Printf.printf "  %d. %s (params: %d, blocks: %d)\n" 
              (i + 1) 
              func.name 
              (List.length func.params)
              (List.length func.blocks)
          ) functions;
        
        | Error msg ->
          Printf.printf "✗ PIR generation failed: %s\n" msg)
     
     | Error msg ->
       Printf.printf "✗ Type checking failed:\n%s\n" msg)
  
  with e ->
    Printf.printf "✗ Error: %s\n" (Printexc.to_string e)

(** Test with type errors *)
let test_type_errors () =
  let source = {|
int main() {
    int x = 5;
    char* str = "hello";
    int result = x + str;  // Type error: int + pointer
    return result;
}
|} in

  Printf.printf "\n=== Testing Type Error Detection ===\n\n";
  Printf.printf "Source code (with type error):\n%s\n" source;
  
  try
    let preprocessed = Preprocessor.preprocess_string ~filename:"test_error.c" source in
    let tokens = Lexer.lex_string "test_error.c" preprocessed in
    let ast = Parser.parse tokens in
    
    (match C_type_checker.type_check ast with
     | Ok _ ->
       Printf.printf "✗ Expected type error but type checking succeeded\n"
     | Error msg ->
       Printf.printf "✓ Type error correctly detected:\n%s\n" msg)
  
  with e ->
    Printf.printf "Error during processing: %s\n" (Printexc.to_string e)

(** Test complex control flow *)
let test_control_flow () =
  let source = {|
int fibonacci(int n) {
    if (n <= 1) {
        return n;
    } else {
        return fibonacci(n - 1) + fibonacci(n - 2);
    }
}

int main() {
    int i;
    int sum = 0;
    
    for (i = 0; i < 10; i++) {
        sum += fibonacci(i);
    }
    
    return sum;
}
|} in

  Printf.printf "\n=== Testing Control Flow (Fibonacci) ===\n\n";
  Printf.printf "Source code:\n%s\n" source;
  
  try
    let preprocessed = Preprocessor.preprocess_string ~filename:"fibonacci.c" source in
    let tokens = Lexer.lex_string "fibonacci.c" preprocessed in
    let ast = Parser.parse tokens in
    
    (match C_type_checker.type_check ast with
     | Ok symbol_table ->
       Printf.printf "✓ Type checking successful\n";
       
       (match C_pir_generator.generate_pir symbol_table ast with
        | Ok functions ->
          Printf.printf "✓ PIR generation successful\n";
          Printf.printf "Generated functions with control flow:\n";
          
          List.iter (fun func ->
            let open Compilerkit_pir.Instructions in
            Printf.printf "  - %s: %d basic blocks\n" func.name (List.length func.blocks);
            
            List.iter (fun block ->
              Printf.printf "    * %s: %d instructions\n" 
                block.label 
                (List.length block.instructions)
            ) func.blocks
          ) functions
        
        | Error msg ->
          Printf.printf "✗ PIR generation failed: %s\n" msg)
     
     | Error msg ->
       Printf.printf "✗ Type checking failed:\n%s\n" msg)
  
  with e ->
    Printf.printf "Error: %s\n" (Printexc.to_string e)

(** Test struct types *)
let test_struct_types () =
  let source = {|
struct Point {
    int x;
    int y;
};

struct Point make_point(int x, int y) {
    struct Point p;
    p.x = x;
    p.y = y;
    return p;
}

int main() {
    struct Point origin = make_point(0, 0);
    struct Point p1 = make_point(3, 4);
    
    int distance_squared = (p1.x - origin.x) * (p1.x - origin.x) + 
                          (p1.y - origin.y) * (p1.y - origin.y);
    
    return distance_squared;
}
|} in

  Printf.printf "\n=== Testing Struct Types ===\n\n";
  Printf.printf "Source code:\n%s\n" source;
  
  try
    let preprocessed = Preprocessor.preprocess_string ~filename:"struct_test.c" source in
    let tokens = Lexer.lex_string "struct_test.c" preprocessed in
    let ast = Parser.parse tokens in
    
    (match C_type_checker.type_check ast with
     | Ok symbol_table ->
       Printf.printf "✓ Type checking successful (struct types)\n";
       
       (match C_pir_generator.generate_pir symbol_table ast with
        | Ok functions ->
          Printf.printf "✓ PIR generation successful (struct operations)\n";
          Printf.printf "Generated %d functions with struct handling\n" (List.length functions)
        
        | Error msg ->
          Printf.printf "✗ PIR generation failed: %s\n" msg)
     
     | Error msg ->
       Printf.printf "✗ Type checking failed:\n%s\n" msg)
  
  with e ->
    Printf.printf "Error: %s\n" (Printexc.to_string e)

(** Main test runner *)
let () =
  Printf.printf "C Type Checker and PIR Generator Tests\n";
  Printf.printf "=====================================\n";
  
  test_simple_function ();
  test_type_errors ();
  test_control_flow ();
  test_struct_types ();
  
  Printf.printf "\n=== Test Summary ===\n";
  Printf.printf "All tests completed. Check output for results.\n"