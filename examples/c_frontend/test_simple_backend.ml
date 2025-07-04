(** Simple test of backend code generation *)

open C_frontend
open Compilerkit_pir
open Compilerkit_backend

let test_simple_function () =
  let source = {|
int main() {
    return 42;
}
|} in

  Printf.printf "=== Testing Simple Backend Code Generation ===\n\n";
  Printf.printf "Source code:\n%s\n" source;
  
  try
    (* Frontend *)
    let preprocessed = Preprocessor.preprocess_string ~filename:"test.c" source in
    let tokens = Lexer.lex_string "test.c" preprocessed in
    let ast = Parser.parse tokens in
    
    let symbol_table = match C_type_checker.type_check ast with
      | Ok st -> st
      | Error msg -> failwith ("Type checking failed: " ^ msg)
    in
    
    let annotated_ast, _ = match C_ast_annotator.annotate_translation_unit ast with
      | Ok (ast, st) -> ast, st
      | Error msg -> failwith ("AST annotation failed: " ^ msg)
    in
    
    let pir_functions = match C_annotated_pir_generator.generate_pir_from_annotated annotated_ast symbol_table with
      | Ok funcs -> funcs
      | Error msg -> failwith ("PIR generation failed: " ^ msg)
    in
    
    (* Create PIR module *)
    let pir_module = Module_ir.create_module "test.c" in
    List.iter (Module_ir.add_function pir_module) pir_functions;
    
    Printf.printf "\nPIR generated successfully!\n";
    
    (* Try backend - catch any errors *)
    (try
      let asm_code = Codegen.generate_for_target Codegen.ARM64 pir_module in
      Printf.printf "\nGenerated assembly:\n%s\n" asm_code
    with e ->
      Printf.printf "\nBackend error: %s\n" (Printexc.to_string e))
    
  with e ->
    Printf.printf "Error: %s\n" (Printexc.to_string e)

let () =
  test_simple_function ()