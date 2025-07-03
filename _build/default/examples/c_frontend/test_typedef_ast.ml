(** Debug typedef AST *)

open C_frontend

let rec print_declarator = function
  | Ast.DirectDecl dd -> 
      Printf.printf "DirectDecl(";
      print_direct_declarator dd;
      Printf.printf ")"
  | Ast.PointerDecl (quals, decl) ->
      Printf.printf "PointerDecl([";
      List.iter (fun q ->
        match q with
        | Ast.Const -> Printf.printf "const "
        | Ast.Volatile -> Printf.printf "volatile "
      ) quals;
      Printf.printf "], ";
      print_declarator decl;
      Printf.printf ")"

and print_direct_declarator = function
  | Ast.Ident name -> Printf.printf "Ident(%s)" name
  | Ast.ArrayDecl (_, _) -> Printf.printf "ArrayDecl(...)"
  | Ast.FuncDecl (_, _) -> Printf.printf "FuncDecl(...)"
  | Ast.ParenDecl _ -> Printf.printf "ParenDecl(...)"

let test_typedef () =
  let code = "typedef int Integer;" in
  
  Printf.printf "Testing: %s\n\n" code;
  
  (* Preprocess *)
  let preprocessed = Preprocessor.preprocess_string ~filename:"test.c" code in
  
  (* Lex *)
  let tokens = Lexer.lex_string "test.c" preprocessed in
  
  (* Create parser state *)
  let state = Parser.create_parser tokens in
  
  (* Parse typedef declaration *)
  Printf.printf "Parsing typedef declaration...\n";
  let decl = Parser.parse_decl state in
  
  Printf.printf "\nDeclaration structure:\n";
  Printf.printf "Storage classes: ";
  List.iter (fun s ->
    match s with
    | Ast.Typedef -> Printf.printf "typedef "
    | Ast.Extern -> Printf.printf "extern "
    | Ast.Static -> Printf.printf "static "
    | Ast.Auto -> Printf.printf "auto "
    | Ast.Register -> Printf.printf "register "
  ) decl.Ast.storage;
  Printf.printf "\n";
  
  Printf.printf "Init declarators:\n";
  List.iter (fun init_decl ->
    Printf.printf "  Declarator: ";
    print_declarator init_decl.Ast.decl;
    Printf.printf "\n"
  ) decl.Ast.init_decls;
  
  Printf.printf "\nIs 'Integer' a typedef? %b\n" 
    (Symbol_table.is_typedef state.Parser.symbol_table "Integer")

let () = test_typedef ()