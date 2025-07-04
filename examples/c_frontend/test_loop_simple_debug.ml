(* Debug the loop compilation issue *)

open C_frontend
open Compilerkit_pir.Pretty_printer

let test_code = {|
/* Simple while loop */
int main() {
    int sum = 0;
    int i = 1;
    
    while (i <= 5) {
        sum = sum + i;
        i = i + 1;
    }
    
    return sum;  /* Should return 15 */
}
|}

let () =
  (* Preprocess *)
  let preprocessed = Preprocessor.preprocess_string 
    ~include_paths:[] ~filename:"test.c" test_code in
  
  (* Tokenize *)
  let token_list = Lexer.lex_string "test.c" preprocessed in
  
  (* Parse *)
  let parser_state = Parser.create_parser token_list in
  let ast = Parser.parse_translation_unit parser_state in
    Printf.printf "Parse successful!\n\n";
    
    (* Type check - this creates the symbol table *)
    match C_type_checker.type_check ast with
    | Ok symbol_table ->
      (* Annotate AST *)
      let annotated_ast = C_annotated_ast.annotate_translation_unit ast symbol_table in
      
      (* Generate PIR *)
      (match C_annotated_pir_generator.generate_pir_from_annotated annotated_ast symbol_table with
       | Ok functions ->
         Printf.printf "Generated PIR:\n";
         List.iter (fun func ->
           print_endline (pp_func func);
           
           (* Debug: print block parameter info *)
           Printf.printf "\nBlock Parameter Analysis:\n";
           List.iter (fun block ->
             if block.Compilerkit_pir.Instructions.params <> [] then begin
               Printf.printf "Block %s has %d parameters:\n" 
                 block.Compilerkit_pir.Instructions.label 
                 (List.length block.Compilerkit_pir.Instructions.params);
               List.iter (fun (name, ty) ->
                 Printf.printf "  - %s: %s\n" name 
                   (Compilerkit_pir.Pretty_printer.pp_ty ty)
               ) block.Compilerkit_pir.Instructions.params
             end
           ) func.Compilerkit_pir.Instructions.blocks
         ) functions
       | Error msg ->
         Printf.eprintf "PIR generation error: %s\n" msg)
    | Error msg ->
      Printf.eprintf "Type check error: %s\n" msg