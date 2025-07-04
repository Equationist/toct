(** Test the annotated AST to PIR generation pipeline *)

open C_frontend

(** Test simple arithmetic function with annotated AST *)
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

  Printf.printf "=== Testing Annotated AST to PIR Pipeline ===\n\n";
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
       
       (* Annotate AST *)
       (match C_ast_annotator.annotate_translation_unit ast with
        | Ok (annotated_ast, _) ->
          Printf.printf "✓ AST annotation successful\n";
          
          (* Generate PIR from annotated AST *)
          (match C_annotated_pir_generator.generate_pir_from_annotated annotated_ast symbol_table with
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
             
             (* Pretty print the PIR *)
             Printf.printf "\nGenerated PIR:\n";
             List.iter (fun func ->
               Printf.printf "%s\n" (Compilerkit_pir.Pretty_printer.function_to_string func)
             ) functions
           
           | Error msg ->
             Printf.printf "✗ PIR generation failed: %s\n" msg)
          
        | Error msg ->
          Printf.printf "✗ AST annotation failed: %s\n" msg)
     
     | Error msg ->
       Printf.printf "✗ Type checking failed:\n%s\n" msg)
  
  with e ->
    Printf.printf "✗ Error: %s\n" (Printexc.to_string e)

(** Test control flow with annotated AST *)
let test_control_flow () =
  let source = {|
int max(int a, int b) {
    if (a > b) {
        return a;
    } else {
        return b;
    }
}

int main() {
    int x = 10;
    int y = 20;
    int result = max(x, y);
    return result;
}
|} in

  Printf.printf "\n=== Testing Control Flow with Annotated AST ===\n\n";
  Printf.printf "Source code:\n%s\n" source;
  
  try
    let preprocessed = Preprocessor.preprocess_string ~filename:"test_control.c" source in
    let tokens = Lexer.lex_string "test_control.c" preprocessed in
    let ast = Parser.parse tokens in
    
    (match C_type_checker.type_check ast with
     | Ok symbol_table ->
       Printf.printf "✓ Type checking successful\n";
       
       (match C_ast_annotator.annotate_translation_unit ast with
        | Ok (annotated_ast, _) ->
          Printf.printf "✓ AST annotation successful\n";
          
          (match C_annotated_pir_generator.generate_pir_from_annotated annotated_ast symbol_table with
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
          Printf.printf "✗ AST annotation failed: %s\n" msg)
     
     | Error msg ->
       Printf.printf "✗ Type checking failed:\n%s\n" msg)
  
  with e ->
    Printf.printf "Error: %s\n" (Printexc.to_string e)

(** Test type information in annotated AST *)
let test_type_information () =
  let source = {|
int main() {
    int x = 5;
    float y = 3.14;
    char c = 'A';
    char* str = "hello";
    
    int sum = x + 10;
    float product = y * 2.0;
    
    return sum;
}
|} in

  Printf.printf "\n=== Testing Type Information in Annotated AST ===\n\n";
  Printf.printf "Source code:\n%s\n" source;
  
  try
    let preprocessed = Preprocessor.preprocess_string ~filename:"test_types.c" source in
    let tokens = Lexer.lex_string "test_types.c" preprocessed in
    let ast = Parser.parse tokens in
    
    (match C_type_checker.type_check ast with
     | Ok symbol_table ->
       Printf.printf "✓ Type checking successful\n";
       
       (match C_ast_annotator.annotate_translation_unit ast with
        | Ok (annotated_ast, _) ->
          Printf.printf "✓ AST annotation successful\n";
          
          (* Display type annotations *)
          Printf.printf "\nType annotations:\n";
          
          (match C_annotated_pir_generator.generate_pir_from_annotated annotated_ast symbol_table with
           | Ok functions ->
             Printf.printf "✓ PIR generation with type information successful\n";
             Printf.printf "Generated %d functions with proper types\n" (List.length functions)
           
           | Error msg ->
             Printf.printf "✗ PIR generation failed: %s\n" msg)
        
        | Error msg ->
          Printf.printf "✗ AST annotation failed: %s\n" msg)
     
     | Error msg ->
       Printf.printf "✗ Type checking failed:\n%s\n" msg)
  
  with e ->
    Printf.printf "Error: %s\n" (Printexc.to_string e)

(** Main test runner *)
let () =
  Printf.printf "Annotated AST to PIR Pipeline Tests\n";
  Printf.printf "===================================\n";
  
  test_simple_function ();
  test_control_flow ();
  test_type_information ();
  
  Printf.printf "\n=== Test Summary ===\n";
  Printf.printf "All tests completed. Check output for results.\n"