(** Simple test for C to PIR generation using the old generator *)

open C_frontend

let test_simple_function () =
  let source = {|
int main() {
    return 42;
}
|} in

  Printf.printf "=== Testing Simple C to PIR Generation ===\n\n";
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
       
       (* Generate PIR using the old generator *)
       (match C_pir_generator.generate_pir symbol_table ast with
        | Ok functions ->
          Printf.printf "✓ PIR generation successful\n";
          Printf.printf "Generated %d functions:\n" (List.length functions);
          
          List.iter (fun func ->
            Printf.printf "\n%s\n" (Compilerkit_pir.Pretty_printer.function_to_string func)
          ) functions
        
        | Error msg ->
          Printf.printf "✗ PIR generation failed: %s\n" msg)
     
     | Error msg ->
       Printf.printf "✗ Type checking failed:\n%s\n" msg)
  
  with e ->
    Printf.printf "✗ Error: %s\n" (Printexc.to_string e)

let () =
  test_simple_function ()