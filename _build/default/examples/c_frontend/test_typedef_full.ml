(** Full typedef test *)

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
  
  (* Parse full translation unit *)
  Printf.printf "\nParsing as translation unit...\n";
  try
    let ast = Parser.parse tokens in
    Printf.printf "Parsed successfully!\n";
    Printf.printf "Number of declarations: %d\n" (List.length ast)
  with e ->
    Printf.printf "Parse error: %s\n" (Printexc.to_string e);
    Printf.printf "Stack trace:\n%s\n" (Printexc.get_backtrace ())

let () = 
  Printexc.record_backtrace true;
  test_typedef ()