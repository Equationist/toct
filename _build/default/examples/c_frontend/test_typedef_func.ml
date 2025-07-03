(** Test typedef in function *)

open C_frontend

let test_typedef () =
  let code = "typedef int Integer; int main() { Integer x = 42; return 0; }" in
  
  Printf.printf "Testing: %s\n\n" code;
  
  (* Preprocess *)
  let preprocessed = Preprocessor.preprocess_string ~filename:"test.c" code in
  
  (* Lex and parse *)
  let tokens = Lexer.lex_string "test.c" preprocessed in
  
  try
    let _ast = Parser.parse tokens in
    Printf.printf "Parsed successfully!\n"
  with e ->
    Printf.printf "Parse error: %s\n" (Printexc.to_string e)

let () = test_typedef ()