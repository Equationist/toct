(** End-to-end test for C to PIR generation using annotated AST *)

open C_frontend

let test_file filename =
  Printf.printf "=== Testing %s ===\n\n" filename;
  
  try
    (* Read source file *)
    let source = 
      let ic = open_in filename in
      let content = really_input_string ic (in_channel_length ic) in
      close_in ic;
      content
    in
    Printf.printf "Source code:\n%s\n" source;
    
    (* Preprocess *)
    let preprocessed = Preprocessor.preprocess_string ~filename source in
    Printf.printf "✓ Preprocessing successful\n";
    
    (* Lex *)
    let tokens = Lexer.lex_string filename preprocessed in
    Printf.printf "✓ Lexing successful (%d tokens)\n" (List.length tokens);
    
    (* Parse *)
    let ast = Parser.parse tokens in
    Printf.printf "✓ Parsing successful\n";
    
    (* Type check *)
    (match C_type_checker.type_check ast with
     | Ok symbol_table ->
       Printf.printf "✓ Type checking successful\n";
       
       (* Annotate AST *)
       (match C_ast_annotator.annotate_translation_unit ast with
        | Ok (annotated_ast, _) ->
          Printf.printf "✓ AST annotation successful\n";
          
          (* Generate PIR using annotated AST *)
          (match C_annotated_pir_generator.generate_pir_from_annotated annotated_ast symbol_table with
           | Ok functions ->
             Printf.printf "✓ PIR generation successful\n";
             Printf.printf "Generated %d functions:\n" (List.length functions);
             
             List.iter (fun func ->
               Printf.printf "\n%s\n" (Compilerkit_pir.Pretty_printer.function_to_string func)
             ) functions;
             
             (* Also show what the old PIR generator produces for comparison *)
             Printf.printf "\n--- For comparison, old PIR generator output: ---\n";
             (match C_pir_generator.generate_pir symbol_table ast with
              | Ok old_functions ->
                List.iter (fun func ->
                  Printf.printf "\n%s\n" (Compilerkit_pir.Pretty_printer.function_to_string func)
                ) old_functions
              | Error msg ->
                Printf.printf "Old PIR generation failed: %s\n" msg)
           
           | Error msg ->
             Printf.printf "✗ PIR generation failed: %s\n" msg)
        
        | Error msg ->
          Printf.printf "✗ AST annotation failed: %s\n" msg)
     
     | Error msg ->
       Printf.printf "✗ Type checking failed:\n%s\n" msg)
  
  with e ->
    Printf.printf "✗ Error: %s\n" (Printexc.to_string e)

let () =
  let files = [
    "test_simple_arithmetic.c";
    "test_control_flow.c";
    "test_simple_pir.c";  (* If it exists *)
  ] in
  
  List.iter (fun file ->
    if Sys.file_exists file then
      test_file file
    else
      Printf.printf "File %s not found, skipping\n" file
  ) files