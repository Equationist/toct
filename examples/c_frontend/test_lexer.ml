(** Test program for C89 lexer *)

open C_frontend.Lexer

let test_lexer () =
  let test_cases = [
    (* Simple keywords and identifiers *)
    ("int main() { return 0; }", "basic function");
    
    (* Various constants *)
    ("42 0x2A 052 3.14 2.5e-3 'a' \"hello\"", "constants");
    
    (* Operators *)
    ("a + b * c << 2", "operators");
    ("x += y && z || w", "compound operators");
    
    (* Comments *)
    ("int x; // comment\nint y; /* block */ int z;", "comments");
    
    (* Complex example *)
    ("struct point { float x, y; };\n\
      int main() {\n\
      \  struct point p = {1.0, 2.0};\n\
      \  return p.x > 0 ? 1 : 0;\n\
      }", "struct example");
  ] in
  
  List.iter (fun (input, desc) ->
    Printf.printf "\n=== Testing: %s ===\n" desc;
    Printf.printf "Input:\n%s\n\n" input;
    
    try
      let tokens = lex_string "test.c" input in
      Printf.printf "Tokens:\n";
      List.iter (fun tok ->
        if tok.token <> Newline then
          Printf.printf "  %d:%d %s\n" tok.loc.line tok.loc.column 
            (token_to_string tok.token)
      ) tokens
    with e ->
      Printf.printf "Error: %s\n" (Printexc.to_string e)
  ) test_cases

let () = test_lexer ()