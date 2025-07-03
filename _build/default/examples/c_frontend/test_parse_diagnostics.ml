(* Test parser with diagnostic collection *)

open C_frontend

let test_parse_file file =
  try
    (* Read file *)
    let ic = open_in file in
    let content = really_input_string ic (in_channel_length ic) in
    close_in ic;
    
    (* Preprocess *)
    let preprocessed = Preprocessor.preprocess_string 
      ~include_paths:[] ~filename:file content in
    
    (* Lex *)
    let tokens = Lexer.lex_string file preprocessed in
    
    (* Parse *)
    let _ast = Parser.parse tokens in
    Printf.printf "%s: OK\n" file;
    true
  with
  | Failure msg ->
      Printf.printf "%s: Error: %s\n" file msg;
      false
  | e ->
      Printf.printf "%s: Error: %s\n" file (Printexc.to_string e);
      false

let () =
  if Array.length Sys.argv < 2 then
    Printf.printf "Usage: %s <c_file>\n" Sys.argv.(0)
  else
    let passed = test_parse_file Sys.argv.(1) in
    exit (if passed then 0 else 1)