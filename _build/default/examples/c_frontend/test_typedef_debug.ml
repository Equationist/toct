(** Test typedef parsing with debug output *)

open C_frontend

let test_typedef () =
  let code = "typedef int Integer; Integer x = 42;" in
  
  Printf.printf "Testing: %s\n" code;
  
  (* Preprocess *)
  let preprocessed = Preprocessor.preprocess_string ~filename:"test.c" code in
  Printf.printf "Preprocessed: %s\n" preprocessed;
  
  (* Lex *)
  let tokens = Lexer.lex_string "test.c" preprocessed in
  Printf.printf "Tokens:\n";
  List.iter (fun tok ->
    Printf.printf "  %s\n" (Lexer.token_to_string tok.Lexer.token)
  ) tokens;
  
  (* Parse with debug *)
  let state = Parser.create_parser tokens in
  
  (* First declaration *)
  Printf.printf "\nParsing first declaration...\n";
  let _decl1 = Parser.parse_decl state in
  Printf.printf "First declaration parsed successfully\n";
  
  (* Check symbol table *)
  Printf.printf "Is 'Integer' a typedef? %b\n" 
    (Symbol_table.is_typedef state.Parser.symbol_table "Integer");
  
  (* Second declaration *)
  Printf.printf "\nParsing second declaration...\n";
  try
    let _decl2 = Parser.parse_decl state in
    Printf.printf "Second declaration parsed successfully!\n"
  with e ->
    Printf.printf "Parse error: %s\n" (Printexc.to_string e)

let () = test_typedef ()