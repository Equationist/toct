(** Test annotated AST PIR generation with arrays *)

open C_frontend

let () =
  if Array.length Sys.argv < 3 then begin
    Printf.eprintf "Usage: %s <input.c> <output.s>\n" Sys.argv.(0);
    exit 1
  end;
  
  let input_file = Sys.argv.(1) in
  let output_file = Sys.argv.(2) in
  
  try
    (* Read source file *)
    let source = 
      let ic = open_in input_file in
      let content = really_input_string ic (in_channel_length ic) in
      close_in ic;
      content
    in
    
    (* Preprocess *)
    let preprocessed = Preprocessor.preprocess_string ~filename:input_file source in
    Printf.printf "✓ Preprocessing successful\n";
    
    (* Lex *)
    let tokens = Lexer.lex_string input_file preprocessed in
    Printf.printf "✓ Lexing successful (%d tokens)\n" (List.length tokens);
    
    (* Parse *)
    let ast = Parser.parse tokens in
    Printf.printf "✓ Parsing successful\n";
    
    (* Type check *)
    let _symbol_table = C_type_checker.type_check ast in
    Printf.printf "✓ Type checking successful\n";
    
    (* Annotate AST *)
    match C_ast_annotator.annotate_translation_unit ast with
    | Ok (annotated_ast, updated_symbol_table) ->
      Printf.printf "✓ AST annotation successful\n";
      
      (* Generate PIR *)
      begin match C_annotated_pir_generator.generate_pir_from_annotated annotated_ast updated_symbol_table with
    | Ok functions ->
      Printf.printf "✓ PIR generation successful\n";
      Printf.printf "Generated %d functions\n\n" (List.length functions);
      
      (* Pretty print PIR *)
      let pir_str = Compilerkit_pir.Pretty_printer.pp_program functions in
      Printf.printf "%s\n" pir_str;
      
      (* Generate assembly *)
      Printf.printf "\nGenerating ARM64 assembly...\n";
      
      (* Create PIR module *)
      let module_items = List.map (fun f -> Compilerkit_pir.Module_ir.FuncDecl f) functions in
      let pir_module = Compilerkit_pir.Module_ir.create_module module_items (Compilerkit_pir.Attributes.empty ()) in
      
      (* Generate assembly *)
      let asm_code = Compilerkit_backend.generate_arm64 pir_module in
      Printf.printf "✓ Assembly generation successful\n";
      
      (* Write assembly to file *)
      let oc = open_out output_file in
      output_string oc asm_code;
      close_out oc;
      Printf.printf "✓ Assembly written to %s\n" output_file
      | Error e ->
        Printf.printf "✗ PIR generation error: %s\n" e
      end
      
    | Error e ->
      Printf.printf "✗ AST annotation error: %s\n" e
      
  with e ->
    Printf.eprintf "✗ Error: %s\n" (Printexc.to_string e);
    exit 1