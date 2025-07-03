(** Simple preprocessor test to debug tokenization *)

open C_frontend

let show_token = function
  | Preprocessor.Identifier s -> Printf.sprintf "Id(%s)" s
  | Preprocessor.Number s -> Printf.sprintf "Num(%s)" s
  | Preprocessor.String s -> Printf.sprintf "Str(%s)" s
  | Preprocessor.CharConst s -> Printf.sprintf "Chr(%s)" s
  | Preprocessor.Punctuator s -> Printf.sprintf "Punct(%s)" s
  | Preprocessor.Whitespace s -> Printf.sprintf "WS(%d)" (String.length s)
  | Preprocessor.Newline -> "NL"
  | Preprocessor.Other c -> Printf.sprintf "Other(%c)" c
  | Preprocessor.Eof -> "EOF"

let () =
  let input = "MAX(10, 20)" in
  Printf.printf "Tokenizing: %s\n\n" input;
  
  let tokenizer = Preprocessor.Tokenizer.create "test.c" input in
  let rec loop () =
    let tok = Preprocessor.Tokenizer.next_token tokenizer in
    Printf.printf "%s " (show_token tok.token);
    match tok.token with
    | Preprocessor.Eof -> Printf.printf "\n"
    | _ -> loop ()
  in
  loop ()