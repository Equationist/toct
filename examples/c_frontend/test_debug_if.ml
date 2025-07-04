(** Debug if statement code generation **)

open Compilerkit_pir

let test_if () =
  let source = {|
int main() {
    int x = 10;
    int y = 5;
    int result;
    
    if (x > y) {
        result = x - y;
    } else {
        result = y - x;
    }
    
    return result;
}
|} in

  Printf.printf "=== Testing If Statement ===\n\n";
  
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
    
    (* Print PIR *)
    Printf.printf "=== PIR ===\n";
    List.iter (fun func ->
      let func_str = Pretty_printer.function_to_string func in
      Printf.printf "%s\n" func_str
    ) pir_functions;
    
    (* Create module *)
    let module_items = List.map (fun f -> Module_ir.FuncDecl f) pir_functions in
    let pir_module = Module_ir.create_module module_items (Attributes.empty ()) in
    
    (* Generate assembly *)
    Printf.printf "\n=== Assembly ===\n";
    let asm = Compilerkit_backend.Codegen.generate_for_target 
      Compilerkit_backend.Codegen.ARM64 pir_module in
    print_endline asm
    
  with e ->
    Printf.printf "Error: %s\n" (Printexc.to_string e);
    Printexc.print_backtrace stdout

let () = test_if ()