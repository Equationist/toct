(** Test program to dump PIR for debugging *)

open Compilerkit_pir

let test_simple_function () =
  let source = {|
int main() {
    return 42;
}
|} in

  Printf.printf "=== Testing Simple PIR Generation ===\n\n";
  Printf.printf "Source code:\n%s\n" source;
  
  try
    (* Frontend *)
    let preprocessed = C_frontend.Preprocessor.preprocess_string ~filename:"test.c" source in
    let tokens = C_frontend.Lexer.lex_string "test.c" preprocessed in
    let ast = C_frontend.Parser.parse tokens in
    
    let symbol_table = match C_frontend.C_type_checker.type_check ast with
      | Ok st -> st
      | Error msg -> failwith ("Type checking failed: " ^ msg)
    in
    
    let annotated_ast, _ = match C_frontend.C_ast_annotator.annotate_translation_unit ast with
      | Ok (ast, st) -> ast, st
      | Error msg -> failwith ("AST annotation failed: " ^ msg)
    in
    
    let pir_functions = match C_frontend.C_annotated_pir_generator.generate_pir_from_annotated annotated_ast symbol_table with
      | Ok funcs -> funcs
      | Error msg -> failwith ("PIR generation failed: " ^ msg)
    in
    
    Printf.printf "\nGenerated %d PIR functions\n" (List.length pir_functions);
    
    (* Print PIR using pretty printer *)
    List.iter (fun func ->
      let func_str = Pretty_printer.function_to_string func in
      Printf.printf "\n%s\n" func_str
    ) pir_functions
    
  with e ->
    Printf.printf "Error: %s\n" (Printexc.to_string e)

let () =
  test_simple_function ()