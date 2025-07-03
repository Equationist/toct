(** Trace typedef parsing *)

open C_frontend

let trace_tokens state pos =
  if pos < Array.length state.Parser.tokens then begin
    let tok = state.Parser.tokens.(pos) in
    Printf.printf "  [%d] %s at %d:%d\n" pos 
      (Lexer.token_to_string tok.Lexer.token)
      tok.Lexer.loc.line
      tok.Lexer.loc.column
  end

let test_typedef () =
  let code = "typedef int Integer; Integer x = 42;" in
  
  Printf.printf "Code: %s\n\n" code;
  
  (* Preprocess *)
  let preprocessed = Preprocessor.preprocess_string ~filename:"test.c" code in
  
  (* Lex *)
  let tokens = Lexer.lex_string "test.c" preprocessed in
  
  (* Create parser state *)
  let state = Parser.create_parser tokens in
  
  Printf.printf "Initial tokens:\n";
  for i = 0 to min 5 (Array.length state.Parser.tokens - 1) do
    trace_tokens state i
  done;
  
  (* Parse first declaration *)
  Printf.printf "\nParsing first declaration...\n";
  let _decl1 = Parser.parse_decl state in
  Printf.printf "First declaration parsed. Current position: %d\n" state.Parser.pos;
  Printf.printf "Current token:\n";
  trace_tokens state state.Parser.pos;
  
  (* Check typedef *)
  Printf.printf "\nIs 'Integer' a typedef? %b\n" 
    (Symbol_table.is_typedef state.Parser.symbol_table "Integer");
  
  (* Check if current token is recognized as typedef *)
  Printf.printf "\nChecking current token...\n";
  let cur_tok = (Parser.peek state).Lexer.token in
  (match cur_tok with
   | Lexer.Identifier name -> 
       Printf.printf "Current token is identifier '%s'\n" name;
       Printf.printf "Is it a typedef? %b\n" (Parser.is_typedef_name state name)
   | _ -> Printf.printf "Current token is not an identifier\n");
  
  (* Try to parse as external declaration *)
  Printf.printf "\nTrying to parse as external declaration...\n";
  try
    let _ext_decl = Parser.parse_external_decl state in
    Printf.printf "External declaration parsed successfully!\n"
  with e ->
    Printf.printf "Parse error: %s\n" (Printexc.to_string e)

let () = 
  Printexc.record_backtrace true;
  test_typedef ()