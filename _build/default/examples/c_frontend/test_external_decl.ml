(** Test external declaration parsing *)

open C_frontend

let test_external_decl () =
  let code = "typedef int Integer;" in
  
  Printf.printf "Testing: %s\n\n" code;
  
  (* Preprocess *)
  let preprocessed = Preprocessor.preprocess_string ~filename:"test.c" code in
  
  (* Lex *)
  let tokens = Lexer.lex_string "test.c" preprocessed in
  
  (* Create parser state *)
  let state = Parser.create_parser tokens in
  
  Printf.printf "Initial typedef status: %b\n" 
    (Symbol_table.is_typedef state.Parser.symbol_table "Integer");
  
  (* Parse as external declaration *)
  Printf.printf "\nParsing as external declaration...\n";
  let ext_decl = Parser.parse_external_decl state in
  
  Printf.printf "Parsed successfully!\n";
  Printf.printf "After parsing typedef status: %b\n" 
    (Symbol_table.is_typedef state.Parser.symbol_table "Integer");
  
  (* Check what we got *)
  match ext_decl with
  | Ast.Decl decl ->
      Printf.printf "\nGot declaration with:\n";
      Printf.printf "  Storage: ";
      List.iter (fun s ->
        match s with
        | Ast.Typedef -> Printf.printf "typedef "
        | _ -> Printf.printf "other "
      ) decl.Ast.storage;
      Printf.printf "\n";
      Printf.printf "  Declarators: %d\n" (List.length decl.Ast.init_decls)
  | Ast.FuncDef _ ->
      Printf.printf "\nGot function definition (unexpected)\n"

let () = test_external_decl ()