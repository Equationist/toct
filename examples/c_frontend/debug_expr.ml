open C_frontend

let () =
  (* Test expression evaluation *)
  
  Printf.printf "Testing expression: VERSION >= 2\n";
  Printf.printf "With VERSION defined as 2\n\n";
  
  (* Create a simple state with VERSION defined *)
  let state = Preprocessor.create_pp_state "test.c" in
  let loc = { Preprocessor.filename = "test"; line = 1; column = 1 } in
  Hashtbl.add state.Preprocessor.macros "VERSION" 
    (Preprocessor.ObjectLike [{ Preprocessor.token = Preprocessor.Number "2"; loc }], loc);
  
  (* Tokenize the expression *)
  let expr = "VERSION >= 2" in
  let tokenizer = Preprocessor.Tokenizer.create "test.c" expr in
  let rec collect acc =
    match Preprocessor.Tokenizer.next_token tokenizer with
    | { token = Preprocessor.Eof; _ } -> List.rev acc
    | tok -> collect (tok :: acc)
  in
  let tokens = collect [] in
  
  Printf.printf "Tokens:\n";
  List.iter (fun tok ->
    let s = match tok.Preprocessor.token with
      | Preprocessor.Identifier s -> Printf.sprintf "Id(%s)" s
      | Preprocessor.Number s -> Printf.sprintf "Num(%s)" s
      | Preprocessor.Punctuator s -> Printf.sprintf "Punct(%s)" s
      | Preprocessor.Whitespace _ -> "WS"
      | Preprocessor.Newline -> "NL"
      | _ -> "?"
    in
    Printf.printf "  %s\n" s
  ) tokens;
  
  (* Try to evaluate *)
  try
    (* Filter out whitespace tokens *)
    let filtered_tokens = List.filter (fun tok ->
      match tok.Preprocessor.token with
      | Preprocessor.Whitespace _ | Preprocessor.Newline -> false
      | _ -> true
    ) tokens in
    
    let result = Preprocessor.ExprEval.evaluate filtered_tokens state in
    Printf.printf "\nResult: %d\n" result
  with e ->
    Printf.printf "\nError: %s\n" (Printexc.to_string e)