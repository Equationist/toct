(* Test loops with PIR generation *)

open C_integrated_parser
open C_annotated_pir_generator
open Compilerkit_pir.Pretty_printer

let test_loop_code = {|
int main() {
    int sum = 0;
    int i = 1;
    
    while (i <= 5) {
        sum = sum + i;
        i = i + 1;
    }
    
    return sum;  // Should return 15
}
|}

let test_for_loop_code = {|
int main() {
    int sum = 0;
    int i;
    
    for (i = 0; i < 10; i = i + 1) {
        sum = sum + i;
    }
    
    return sum;  // Should return 45
}
|}

let () =
  (* Test while loop *)
  Printf.printf "=== Testing While Loop ===\n";
  (match parse_c_code test_loop_code "test_while.c" with
   | Ok (ast, _errors, symbol_table) ->
     Printf.printf "Parse successful!\n";
     
     (* Annotate AST *)
     let annotated_ast = C_annotated_ast.annotate_translation_unit ast symbol_table in
     
     (* Generate PIR *)
     (match generate_pir_from_annotated annotated_ast symbol_table with
      | Ok functions ->
        Printf.printf "\nGenerated PIR:\n";
        List.iter (fun func ->
          print_endline (pp_func func)
        ) functions
      | Error msg ->
        Printf.eprintf "PIR generation error: %s\n" msg)
   | Error msgs ->
     Printf.eprintf "Parse error: %s\n" (String.concat "\n" msgs));
  
  Printf.printf "\n=== Testing For Loop ===\n";
  (match parse_c_code test_for_loop_code "test_for.c" with
   | Ok (ast, _errors, symbol_table) ->
     Printf.printf "Parse successful!\n";
     
     (* Annotate AST *)
     let annotated_ast = C_annotated_ast.annotate_translation_unit ast symbol_table in
     
     (* Generate PIR *)
     (match generate_pir_from_annotated annotated_ast symbol_table with
      | Ok functions ->
        Printf.printf "\nGenerated PIR:\n";
        List.iter (fun func ->
          print_endline (pp_func func)
        ) functions
      | Error msg ->
        Printf.eprintf "PIR generation error: %s\n" msg)
   | Error msgs ->
     Printf.eprintf "Parse error: %s\n" (String.concat "\n" msgs))