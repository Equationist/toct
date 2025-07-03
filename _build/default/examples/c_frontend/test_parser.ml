(** Test program for C89 parser *)

open C_frontend

let test_parser () =
  let test_cases = [
    (* Simple function *)
    ("int main() { return 0; }", "simple main");
    
    (* Variable declarations *)
    ("int x = 42;\nfloat y = 3.14;", "variable declarations");
    
    (* Function with parameters *)
    ("int add(int a, int b) { return a + b; }", "function with params");
    
    (* Struct declaration *)
    ("struct point { int x; int y; };", "struct declaration");
    
    (* Complex example *)
    ("int factorial(int n) {\n\
      \  if (n <= 1)\n\
      \    return 1;\n\
      \  else\n\
      \    return n * factorial(n - 1);\n\
      }", "recursive function");
  ] in
  
  List.iter (fun (input, desc) ->
    Printf.printf "\n=== Testing: %s ===\n" desc;
    Printf.printf "Input:\n%s\n\n" input;
    
    try
      (* Preprocess *)
      let preprocessed = Preprocessor.preprocess_string ~filename:"test.c" input in
      
      (* Lex *)
      let tokens = Lexer.lex_string "test.c" preprocessed in
      Printf.printf "Lexed %d tokens\n" (List.length tokens);
      
      (* Parse *)
      let ast = Parser.parse tokens in
      Printf.printf "Parsed successfully! %d top-level declarations\n" 
        (List.length ast)
        
    with e ->
      Printf.printf "Error: %s\n" (Printexc.to_string e)
  ) test_cases

let () = test_parser ()