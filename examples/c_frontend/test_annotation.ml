(** Test the C AST annotation system *)

open C_frontend
open C_ast_annotator

let test_annotation () =
  let code = {|
int main() {
    int x = 5;
    int y = 10;
    int z = x + y;
    return z;
}
|} in
  
  Printf.printf "=== Testing C AST Annotation System ===\n";
  Printf.printf "Input code:\n%s\n" code;
  
  try
    (* Preprocess the code *)
    let current_dir = Sys.getcwd () in
    let include_dir = Filename.concat current_dir "include" in
    let include_paths = [include_dir; "/usr/include"; "/usr/local/include"] in
    
    let preprocessed_code = Preprocessor.preprocess_string ~include_paths code ~filename:"<input>" in
    Printf.printf "Preprocessed code:\n%s\n" preprocessed_code;
    
    (* Tokenize *)
    let tokens = Lexer.lex_string "<input>" preprocessed_code in
    Printf.printf "Tokens: %d\n" (List.length tokens);
    
    (* Parse *)
    let ast = Parser.parse tokens in
    Printf.printf "AST parsed successfully\n";
    
    (* Annotate AST *)
    let annotation_result = annotate_translation_unit ast in
    (match annotation_result with
     | Ok (annotated_ast, symbol_table) ->
       Printf.printf "AST annotated successfully\n";
       Printf.printf "Number of annotated declarations: %d\n" (List.length annotated_ast);
       Printf.printf "Symbol table contains %d symbols in current scope\n" 
         (List.length (C_scoped_symbol_table.symbols_in_current_scope symbol_table));
       Printf.printf "=== SUCCESS ===\n"
     | Error error_msg ->
       Printf.printf "Annotation failed: %s\n" error_msg;
       Printf.printf "=== ANNOTATION FAILED ===\n")
  with
  | e -> 
    Printf.printf "Exception: %s\n" (Printexc.to_string e);
    Printf.printf "=== EXCEPTION ===\n"

let () = test_annotation ()