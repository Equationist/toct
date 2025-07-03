(** Test typedef parsing *)

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
  
  (* Parse *)
  try
    let _ast = Parser.parse tokens in
    Printf.printf "Parsed successfully!\n"
  with e ->
    Printf.printf "Parse error: %s\n" (Printexc.to_string e)

let () = test_typedef ()