(** Simple test of backend code generation *)

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
    
    (* Create PIR module *)
    let module_items = List.map (fun f -> Module_ir.FuncDecl f) pir_functions in
    let pir_module = Module_ir.create_module module_items (Attributes.empty ()) in
    
    Printf.printf "\nPIR generated successfully!\n";
    
    (* Print the PIR first *)
    List.iter (fun func ->
      let func_str = Pretty_printer.function_to_string func in
      Printf.printf "\nPIR:\n%s\n" func_str
    ) pir_functions;
    
    (* Try backend - catch any errors *)
    (try
      let asm_code = Codegen.generate_for_target Codegen.ARM64 pir_module in
      Printf.printf "\nGenerated assembly:\n%s\n" asm_code
    with e ->
      Printf.printf "\nBackend error: %s\n" (Printexc.to_string e);
      Printf.printf "Stack trace:\n%s\n" (Printexc.get_backtrace ()))
    
  with e ->
    Printf.printf "Error: %s\n" (Printexc.to_string e)

let () =
  test_simple_function ()