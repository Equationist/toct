(** Test typedef scope handling *)

open C_frontend

let test_typedef () =
  let code = "typedef int Integer; int main() { Integer x = 42; return 0; }" in
  
  Printf.printf "Testing: %s\n\n" code;
  
  (* Preprocess *)
  let preprocessed = Preprocessor.preprocess_string ~filename:"test.c" code in
  
  (* Lex *)
  let tokens = Lexer.lex_string "test.c" preprocessed in
  
  (* Create parser state *)
  let state = Parser.create_parser tokens in
  
  (* Parse typedef declaration *)
  Printf.printf "Parsing typedef declaration...\n";
  let _typedef_decl = Parser.parse_external_decl state in
  Printf.printf "Typedef parsed. Is 'Integer' a typedef? %b\n" 
    (Symbol_table.is_typedef state.Parser.symbol_table "Integer");
  
  (* Try to parse function *)
  Printf.printf "\nParsing function definition...\n";
  Printf.printf "Current token: %s\n" 
    (Lexer.token_to_string (Parser.peek state).Lexer.token);
  
  try
    let _func_decl = Parser.parse_external_decl state in
    Printf.printf "Function parsed successfully!\n"
  with e ->
    Printf.printf "Parse error: %s\n" (Printexc.to_string e);
    Printf.printf "At token: %s\n" 
      (Lexer.token_to_string (Parser.peek state).Lexer.token);
    Printf.printf "Is 'Integer' still a typedef? %b\n" 
      (Symbol_table.is_typedef state.Parser.symbol_table "Integer")

let () = 
  Printexc.record_backtrace true;
  test_typedef ()