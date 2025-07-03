(** Trace macro expansion step by step *)

open C_frontend

let input = {|#define ADD(a, b) ((a) + (b))
int result = ADD(5, 3);|}

let () =
  Printf.printf "Input:\n%s\n\n" input;
  
  (* First tokenize the whole input *)
  let tokenizer = Preprocessor.Tokenizer.create "test.c" input in
  let rec collect_all acc =
    match Preprocessor.Tokenizer.next_token tokenizer with
    | { token = Preprocessor.Eof; _ } -> List.rev acc
    | tok -> collect_all (tok :: acc)
  in
  let tokens = collect_all [] in
  
  Printf.printf "Tokens (%d total):\n" (List.length tokens);
  List.iteri (fun i tok ->
    let tok_str = match tok.Preprocessor.token with
      | Preprocessor.Identifier s -> Printf.sprintf "Id(%s)" s
      | Preprocessor.Number s -> Printf.sprintf "Num(%s)" s 
      | Preprocessor.Punctuator s -> Printf.sprintf "Punct(%s)" s
      | Preprocessor.Whitespace _ -> "WS"
      | Preprocessor.Newline -> "NL"
      | _ -> "?"
    in
    Printf.printf "%2d: %s\n" i tok_str
  ) tokens;
  
  (* Now try to preprocess *)
  Printf.printf "\nPreprocessing...\n";
  try
    let output = Preprocessor.preprocess_string ~filename:"test.c" input in
    Printf.printf "Success!\nOutput:\n%s\n" output
  with e ->
    Printf.printf "Error: %s\n" (Printexc.to_string e)