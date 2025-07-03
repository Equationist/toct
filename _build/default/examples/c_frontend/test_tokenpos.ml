(** Test token positions *)

open C_frontend

let test_tokens () =
  let code = "typedef int Integer; Integer x = 42;" in
  
  Printf.printf "Code: %s\n\n" code;
  
  (* Preprocess *)
  let preprocessed = Preprocessor.preprocess_string ~filename:"test.c" code in
  
  (* Lex *)
  let tokens = Lexer.lex_string "test.c" preprocessed in
  Printf.printf "Token positions:\n";
  List.iteri (fun i tok ->
    Printf.printf "%2d: %-15s at %d:%d\n" i 
      (Lexer.token_to_string tok.Lexer.token)
      tok.Lexer.loc.line
      tok.Lexer.loc.column
  ) tokens

let () = test_tokens ()