(* C lexer utilities using shared infrastructure *)

open Compilerkit_frontend

(* Note: Use Lexer_utils.Buffer directly for lexing buffers *)

(* Character classification - reuse from shared library *)
let is_whitespace = Lexer_utils.is_whitespace
let is_digit = Lexer_utils.is_digit
let is_alpha = Lexer_utils.is_alpha
let is_alnum c = is_alpha c || is_digit c
let is_ident_start c = is_alpha c || c = '_'
let is_ident_cont c = is_alnum c || c = '_'

(* Hex digit classification *)
let is_hex_digit = Lexer_utils.is_hex_digit
let hex_value c =
  if c >= '0' && c <= '9' then Char.code c - Char.code '0'
  else if c >= 'a' && c <= 'f' then Char.code c - Char.code 'a' + 10
  else if c >= 'A' && c <= 'F' then Char.code c - Char.code 'A' + 10
  else failwith "Invalid hex digit"

(* C-specific escape sequences beyond what Lexer_utils handles *)
let parse_c_escape_sequence chars =
  match chars with
  | '\\' :: c :: rest ->
      let ch = match c with
        | 'b' -> '\b'      (* Backspace - C specific *)
        | 'f' -> '\012'    (* Form feed - C specific *)
        | 'v' -> '\011'    (* Vertical tab - C specific *)
        | 'a' -> '\007'    (* Alert/bell - C specific *)
        | '?' -> '?'       (* Question mark - C specific *)
        (* Octal and hex escapes would go here *)
        | _ -> failwith (Printf.sprintf "Unknown escape sequence: \\%c" c)
      in
      (ch, rest)
  | _ -> failwith "Invalid escape sequence"

(* C-specific suffix checking *)
let is_integer_suffix_char c =
  match c with
  | 'u' | 'U' | 'l' | 'L' -> true
  | _ -> false

let is_float_suffix_char c =
  match c with
  | 'f' | 'F' | 'l' | 'L' -> true
  | _ -> false

(* Parse integer suffix *)
let parse_integer_suffix chars =
  let rec parse_suffix chars suffix =
    match chars with
    | [] -> (suffix, [])
    | c :: rest when is_integer_suffix_char c ->
        parse_suffix rest (suffix ^ String.make 1 c)
    | _ -> (suffix, chars)
  in
  parse_suffix chars ""

(* Parse float suffix *)
let parse_float_suffix chars =
  match chars with
  | ('f' | 'F') :: rest -> ("f", rest)
  | ('l' | 'L') :: rest -> ("l", rest)
  | _ -> ("", chars)

(* C keyword table *)
let keyword_table = Hashtbl.create 32

let () = List.iter (fun (name, tok) -> Hashtbl.add keyword_table name tok) [
  ("auto", Lexer.Auto);
  ("break", Lexer.Break);
  ("case", Lexer.Case);
  ("char", Lexer.Char);
  ("const", Lexer.Const);
  ("continue", Lexer.Continue);
  ("default", Lexer.Default);
  ("do", Lexer.Do);
  ("double", Lexer.Double);
  ("else", Lexer.Else);
  ("enum", Lexer.Enum);
  ("extern", Lexer.Extern);
  ("float", Lexer.Float);
  ("for", Lexer.For);
  ("goto", Lexer.Goto);
  ("if", Lexer.If);
  ("int", Lexer.Int);
  ("long", Lexer.Long);
  ("register", Lexer.Register);
  ("return", Lexer.Return);
  ("short", Lexer.Short);
  ("signed", Lexer.Signed);
  ("sizeof", Lexer.Sizeof);
  ("static", Lexer.Static);
  ("struct", Lexer.Struct);
  ("switch", Lexer.Switch);
  ("typedef", Lexer.Typedef);
  ("union", Lexer.Union);
  ("unsigned", Lexer.Unsigned);
  ("void", Lexer.Void);
  ("volatile", Lexer.Volatile);
  ("while", Lexer.While);
]

(* Lookup keyword or return identifier *)
let lookup_keyword name =
  try Hashtbl.find keyword_table name
  with Not_found -> Lexer.Identifier name