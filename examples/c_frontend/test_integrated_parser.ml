(* Test program demonstrating integrated parser with shared infrastructure *)

open C_frontend

let test_parse_with_error_reporting filename =
  try
    (* Read file *)
    let ic = open_in filename in
    let content = really_input_string ic (in_channel_length ic) in
    close_in ic;
    
    (* Preprocess *)
    let preprocessed = Preprocessor.preprocess_string 
      ~include_paths:[] ~filename content in
    
    (* Tokenize using the lexer *)
    let token_list = Lexer.lex_string filename preprocessed in
    
    (* Create integrated parser *)
    let parser_state = Parser.create_parser token_list in
    
    (* Attempt to parse *)
    Printf.printf "Parsing %s with integrated error reporting...\n" filename;
    
    (* In a real implementation, we would parse and collect errors *)
    (* For now, just demonstrate the error reporting capability *)
    
    (* Create a test error *)
    let test_loc = { 
      Lexer.filename = filename; 
      line = 10; 
      column = 5
    } in
    
    (* Report various types of errors *)
    C_errors.parse_error parser_state.error_reporter test_loc 
      "Unexpected token in declaration";
    
    C_errors.warning parser_state.error_reporter test_loc
      "Implicit int is deprecated in C99";
    
    (* Check error status *)
    if C_errors.has_errors parser_state.error_reporter then begin
      Printf.printf "Parse failed with %d errors and %d warnings\n"
        (C_errors.error_count parser_state.error_reporter)
        (C_errors.warning_count parser_state.error_reporter);
      
      (* Print formatted diagnostics *)
      let file_contents = Hashtbl.create 1 in
      Hashtbl.add file_contents filename content;
      C_errors.print_diagnostics parser_state.error_reporter file_contents;
    end else begin
      Printf.printf "Parse succeeded with %d warnings\n"
        (C_errors.warning_count parser_state.error_reporter);
    end;
    
    (* Demonstrate symbol table usage *)
    Printf.printf "\nDemonstrating symbol table integration:\n";
    
    (* Add some symbols *)
    let test_loc2 = { test_loc with line = 15; column = 10 } in
    (match C_symbol_table.add_typedef parser_state.symbol_table "size_t" 
        [Ast.Unsigned; Ast.Long] [] test_loc2 with
    | Ok () -> Printf.printf "  Added typedef 'size_t'\n"
    | Error msg -> Printf.printf "  Error adding typedef: %s\n" msg);
    
    (* Check typedef *)
    if C_symbol_table.is_typedef parser_state.symbol_table "size_t" then
      Printf.printf "  'size_t' is recognized as a typedef\n";
    
    (* Enter a new scope *)
    C_symbol_table.enter_scope parser_state.symbol_table;
    Printf.printf "  Entered new scope\n";
    
    (* Add a variable in the new scope *)
    let test_loc3 = { test_loc with line = 20; column = 5 } in
    (match C_symbol_table.add_variable parser_state.symbol_table "count"
        [Ast.Int] [] test_loc3 with
    | Ok () -> Printf.printf "  Added variable 'count'\n"
    | Error msg -> Printf.printf "  Error adding variable: %s\n" msg);
    
    (* Exit scope *)
    C_symbol_table.exit_scope parser_state.symbol_table;
    Printf.printf "  Exited scope\n";
    
    Printf.printf "\nIntegration test completed.\n"
    
  with
  | Failure msg ->
      Printf.printf "Error: %s\n" msg
  | e ->
      Printf.printf "Error: %s\n" (Printexc.to_string e)

let () =
  if Array.length Sys.argv < 2 then
    Printf.printf "Usage: %s <c_file>\n" Sys.argv.(0)
  else
    test_parse_with_error_reporting Sys.argv.(1)