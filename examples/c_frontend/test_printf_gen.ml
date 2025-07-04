(* Test program to generate PIR and assembly for printf test *)
open C_frontend
open Compilerkit_frontend.Position
open Compilerkit_frontend.Error_reporter
open Compilerkit_frontend.Ast_annotation

let test_file = "../../test/c_frontend/test_printf_simple.c"

let () =
  (* Create error reporter *)
  let reporter = create_error_reporter () in
  
  (* Read and process file *)
  let ic = open_in test_file in
  let content = really_input_string ic (in_channel_length ic) in
  close_in ic;
  
  (* Create positions *)
  let start_pos = create_position test_file 1 1 0 in
  let input = create_lexer_input content start_pos in
  
  (* Preprocess *)
  let preprocessed = Preprocessor.preprocess ~reporter ~search_paths:["../../test/c_frontend"] input in
  
  (* Lex *)
  let tokens = C_lexer_utils.lex_all preprocessed in
  
  (* Parse *)
  let parse_result = Parser.parse_translation_unit tokens in
  
  match parse_result with
  | Error msg -> 
    Printf.printf "Parse error: %s\n" msg
  | Ok ast ->
    (* Type check and annotate *)
    let symbol_table = create_c_symbol_table () in
    let annotated_ast = type_check_and_annotate symbol_table ast in
    
    (* Generate PIR *)
    match generate_pir_from_annotated annotated_ast symbol_table with
    | Error msg -> 
      Printf.printf "PIR generation error: %s\n" msg
    | Ok functions ->
      (* Print PIR *)
      Printf.printf "=== PIR ===\n";
      List.iter (fun func ->
        Printf.printf "%s\n" (Compilerkit_pir.Pretty_printer.pp_func func)
      ) functions;
      
      (* Generate assembly *)
      Printf.printf "\n=== Assembly ===\n";
      let module_ir = Compilerkit_pir.Module_ir.create_module "test_printf" in
      let module_ir = List.fold_left (fun m f ->
        Compilerkit_pir.Module_ir.add_func m f
      ) module_ir functions in
      
      let asm = Compilerkit_backend.Codegen.generate_for_target 
        Compilerkit_backend.Codegen.ARM64 module_ir in
      print_endline asm