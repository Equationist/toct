open Lexer
open Parser

let test_nested_init () =
  let code = {|
    struct Person person = {"John Doe", 35, {"Main Street", 123}};
  |} in
  
  let tokens = lex code in
  Printf.printf "Tokens:\n";
  List.iteri (fun i tok -> 
    Printf.printf "%d: %s at %d:%d\n" i (token_to_string tok.token) tok.loc.line tok.loc.column
  ) tokens;
  
  try
    let _ = parse tokens in
    Printf.printf "Parse successful\n"
  with
  | Failure msg -> Printf.printf "Parse failed: %s\n" msg
  | e -> Printf.printf "Parse failed with exception: %s\n" (Printexc.to_string e)

let () = test_nested_init ()