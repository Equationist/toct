open C_frontend

let _show_token tok =
  match tok.Preprocessor.token with
  | Preprocessor.Identifier s -> Printf.sprintf "Id(%s)" s
  | Preprocessor.Number s -> Printf.sprintf "Num(%s)" s
  | Preprocessor.String s -> Printf.sprintf "Str(%s)" s
  | Preprocessor.CharConst s -> Printf.sprintf "Chr(%s)" s
  | Preprocessor.Punctuator s -> Printf.sprintf "Punct(%s)" s
  | Preprocessor.Whitespace _ -> "WS"
  | Preprocessor.Newline -> "NL"
  | Preprocessor.Other c -> Printf.sprintf "Other(%c)" c
  | Preprocessor.Eof -> "EOF"

let () =
  let input = {|#define FEATURE_A
#define VERSION 2

#ifdef FEATURE_A
int feature_a = 1;
#else
int feature_a = 0;
#endif

#ifndef FEATURE_B
int feature_b = 0;
#endif

#if VERSION >= 2
int new_version = 1;
#else
int old_version = 1;
#endif|} in
  
  Printf.printf "Input:\n%s\n\n" input;
  
  (* Tokenize *)
  let tokenizer = Preprocessor.Tokenizer.create "test.c" input in
  let rec collect acc count =
    if count > 200 then List.rev acc  (* safety limit *)
    else match Preprocessor.Tokenizer.next_token tokenizer with
    | { token = Preprocessor.Eof; _ } -> List.rev acc
    | tok -> collect (tok :: acc) (count + 1)
  in
  let tokens = collect [] 0 in
  
  Printf.printf "Total tokens: %d\n\n" (List.length tokens);
  
  (* Process *)
  let output = Preprocessor.preprocess_string ~filename:"test.c" input in
  Printf.printf "Preprocessed output:\n%s\n" output;
  Printf.printf "Output length: %d chars\n" (String.length output)