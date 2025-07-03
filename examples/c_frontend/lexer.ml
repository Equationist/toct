(** C89/90 Lexer Implementation

    This module implements a lexer for C89 that tokenizes C source code
    according to the ANSI C specification. It handles:
    - Keywords and identifiers
    - Integer, floating-point, character, and string constants
    - Operators and punctuators
    - Comments (single-line and multi-line)
    - Whitespace and line tracking
*)

(** Token types for C89 *)
type token =
  (* Keywords *)
  | Auto | Break | Case | Char | Const | Continue | Default | Do
  | Double | Else | Enum | Extern | Float | For | Goto | If
  | Int | Long | Register | Return | Short | Signed | Sizeof | Static
  | Struct | Switch | Typedef | Union | Unsigned | Void | Volatile | While
  
  (* Identifiers and constants *)
  | Identifier of string
  | IntConstant of int64 * string  (* value, suffix *)
  | FloatConstant of float * string  (* value, suffix *)
  | CharConstant of char
  | StringLiteral of string
  
  (* Operators *)
  | Plus | Minus | Star | Slash | Percent
  | Ampersand | Pipe | Caret | Tilde | Bang
  | Less | Greater | LessEqual | GreaterEqual | EqualEqual | BangEqual
  | LeftShift | RightShift
  | PlusPlus | MinusMinus
  | AmpAmp | PipePipe
  | Question | Colon
  | Equal | PlusEqual | MinusEqual | StarEqual | SlashEqual | PercentEqual
  | AmpEqual | PipeEqual | CaretEqual | LeftShiftEqual | RightShiftEqual
  | Arrow | Dot
  
  (* Punctuators *)
  | LeftParen | RightParen
  | LeftBracket | RightBracket
  | LeftBrace | RightBrace
  | Comma | Semicolon
  | Ellipsis
  | Hash | HashHash  (* For preprocessor, though usually handled separately *)
  
  (* Special *)
  | Eof
  | Newline

(** Location information *)
type location = {
  filename: string;
  line: int;
  column: int;
}

(** Token with location *)
type located_token = {
  token: token;
  loc: location;
}

(** Lexer state *)
type lexer_state = {
  input: string;
  mutable pos: int;
  mutable line: int;
  mutable column: int;
  filename: string;
}

(** Create initial lexer state *)
let create_lexer filename input = {
  input;
  pos = 0;
  line = 1;
  column = 1;
  filename;
}

(** Get current location *)
let current_location state = {
  filename = state.filename;
  line = state.line;
  column = state.column;
}

(** Peek at current character *)
let peek_char state =
  if state.pos < String.length state.input then
    Some state.input.[state.pos]
  else
    None

(** Peek at character n positions ahead *)
let peek_char_n state n =
  let pos = state.pos + n in
  if pos < String.length state.input then
    Some state.input.[pos]
  else
    None

(** Advance position *)
let advance state =
  if state.pos < String.length state.input then begin
    if state.input.[state.pos] = '\n' then begin
      state.line <- state.line + 1;
      state.column <- 1
    end else
      state.column <- state.column + 1;
    state.pos <- state.pos + 1
  end

(** Skip whitespace *)
let rec skip_whitespace state =
  match peek_char state with
  | Some (' ' | '\t' | '\r') ->
      advance state;
      skip_whitespace state
  | _ -> ()

(** Skip single-line comment *)
let skip_line_comment state =
  let rec skip () =
    match peek_char state with
    | None -> ()
    | Some '\n' -> ()
    | _ -> advance state; skip ()
  in
  skip ()

(** Skip multi-line comment *)
let skip_block_comment state =
  let rec skip () =
    match peek_char state, peek_char_n state 1 with
    | Some '*', Some '/' ->
        advance state; advance state
    | None, _ -> failwith "Unterminated comment"
    | _ -> advance state; skip ()
  in
  skip ()

(** Skip comments *)
let skip_comments state =
  match peek_char state, peek_char_n state 1 with
  | Some '/', Some '/' ->
      advance state; advance state;
      skip_line_comment state;
      true
  | Some '/', Some '*' ->
      advance state; advance state;
      skip_block_comment state;
      true
  | _ -> false

(** Check if character is digit *)
let is_digit = function '0'..'9' -> true | _ -> false

(** Check if character is octal digit *)
let is_octal_digit = function '0'..'7' -> true | _ -> false

(** Check if character is hex digit *)
let is_hex_digit = function
  | '0'..'9' | 'a'..'f' | 'A'..'F' -> true
  | _ -> false

(** Check if character is letter *)
let is_letter = function
  | 'a'..'z' | 'A'..'Z' -> true
  | _ -> false

(** Check if character is identifier start *)
let is_ident_start c = is_letter c || c = '_'

(** Check if character is identifier continuation *)
let is_ident_continue c = is_ident_start c || is_digit c

(** Lex identifier or keyword *)
let lex_identifier state =
  let start_pos = state.pos in
  let rec collect () =
    match peek_char state with
    | Some c when is_ident_continue c ->
        advance state;
        collect ()
    | _ -> ()
  in
  collect ();
  let ident = String.sub state.input start_pos (state.pos - start_pos) in
  match ident with
  | "auto" -> Auto
  | "break" -> Break
  | "case" -> Case
  | "char" -> Char
  | "const" -> Const
  | "continue" -> Continue
  | "default" -> Default
  | "do" -> Do
  | "double" -> Double
  | "else" -> Else
  | "enum" -> Enum
  | "extern" -> Extern
  | "float" -> Float
  | "for" -> For
  | "goto" -> Goto
  | "if" -> If
  | "int" -> Int
  | "long" -> Long
  | "register" -> Register
  | "return" -> Return
  | "short" -> Short
  | "signed" -> Signed
  | "sizeof" -> Sizeof
  | "static" -> Static
  | "struct" -> Struct
  | "switch" -> Switch
  | "typedef" -> Typedef
  | "union" -> Union
  | "unsigned" -> Unsigned
  | "void" -> Void
  | "volatile" -> Volatile
  | "while" -> While
  | _ -> Identifier ident

(** Parse integer value from string *)
let parse_int_value str base =
  let rec parse acc i =
    if i >= String.length str then acc
    else
      let c = str.[i] in
      let digit =
        match c with
        | '0'..'9' -> Char.code c - Char.code '0'
        | 'a'..'f' -> Char.code c - Char.code 'a' + 10
        | 'A'..'F' -> Char.code c - Char.code 'A' + 10
        | _ -> failwith "Invalid digit"
      in
      if digit >= base then failwith "Invalid digit for base"
      else parse (Int64.add (Int64.mul acc (Int64.of_int base)) (Int64.of_int digit)) (i + 1)
  in
  parse 0L 0

(** Lex numeric constant *)
let lex_number state =
  let start_pos = state.pos in
  
  (* Check for hex prefix *)
  if peek_char state = Some '0' && 
     (peek_char_n state 1 = Some 'x' || peek_char_n state 1 = Some 'X') then begin
    (* Hexadecimal *)
    advance state; advance state;  (* Skip 0x *)
    let hex_start = state.pos in
    let rec collect_hex () =
      match peek_char state with
      | Some c when is_hex_digit c -> advance state; collect_hex ()
      | _ -> ()
    in
    collect_hex ();
    if state.pos = hex_start then failwith "Invalid hexadecimal constant";
    let hex_str = String.sub state.input hex_start (state.pos - hex_start) in
    let value = parse_int_value hex_str 16 in
    
    (* Check for suffix *)
    let suffix = ref "" in
    (match peek_char state with
     | Some ('u' | 'U') -> suffix := !suffix ^ "u"; advance state
     | _ -> ());
    (match peek_char state with
     | Some ('l' | 'L') -> suffix := !suffix ^ "l"; advance state
     | _ -> ());
    
    IntConstant (value, !suffix)
  end else if peek_char state = Some '0' && 
              Option.map is_octal_digit (peek_char_n state 1) = Some true then begin
    (* Octal *)
    advance state;  (* Skip leading 0 *)
    let oct_start = state.pos in
    let rec collect_oct () =
      match peek_char state with
      | Some c when is_octal_digit c -> advance state; collect_oct ()
      | _ -> ()
    in
    collect_oct ();
    let oct_str = String.sub state.input oct_start (state.pos - oct_start) in
    let value = if oct_str = "" then 0L else parse_int_value oct_str 8 in
    
    (* Check for suffix *)
    let suffix = ref "" in
    (match peek_char state with
     | Some ('u' | 'U') -> suffix := !suffix ^ "u"; advance state
     | _ -> ());
    (match peek_char state with
     | Some ('l' | 'L') -> suffix := !suffix ^ "l"; advance state
     | _ -> ());
    
    IntConstant (value, !suffix)
  end else begin
    (* Decimal number - could be int or float *)
    let rec collect_digits () =
      match peek_char state with
      | Some c when is_digit c -> advance state; collect_digits ()
      | _ -> ()
    in
    collect_digits ();
    
    (* Check for decimal point or exponent *)
    let is_float = ref false in
    
    (* Decimal point? *)
    if peek_char state = Some '.' && 
       Option.map is_digit (peek_char_n state 1) = Some true then begin
      is_float := true;
      advance state;  (* Skip . *)
      collect_digits ()
    end;
    
    (* Exponent? *)
    if peek_char state = Some 'e' || peek_char state = Some 'E' then begin
      is_float := true;
      advance state;  (* Skip e/E *)
      (match peek_char state with
       | Some ('+' | '-') -> advance state
       | _ -> ());
      let exp_start = state.pos in
      collect_digits ();
      if state.pos = exp_start then failwith "Invalid exponent"
    end;
    
    let num_str = String.sub state.input start_pos (state.pos - start_pos) in
    
    if !is_float then begin
      (* Float constant *)
      let suffix = ref "" in
      (match peek_char state with
       | Some ('f' | 'F') -> suffix := "f"; advance state
       | Some ('l' | 'L') -> suffix := "l"; advance state
       | _ -> ());
      FloatConstant (float_of_string num_str, !suffix)
    end else begin
      (* Integer constant *)
      let value = parse_int_value num_str 10 in
      let suffix = ref "" in
      (match peek_char state with
       | Some ('u' | 'U') -> suffix := !suffix ^ "u"; advance state
       | _ -> ());
      (match peek_char state with
       | Some ('l' | 'L') -> suffix := !suffix ^ "l"; advance state
       | _ -> ());
      IntConstant (value, !suffix)
    end
  end

(** Parse escape sequence *)
let parse_escape state =
  match peek_char state with
  | Some 'n' -> advance state; '\n'
  | Some 't' -> advance state; '\t'
  | Some 'r' -> advance state; '\r'
  | Some 'b' -> advance state; '\b'
  | Some 'f' -> advance state; '\012'  (* form feed *)
  | Some 'v' -> advance state; '\011'  (* vertical tab *)
  | Some 'a' -> advance state; '\007'  (* alert/bell *)
  | Some '\\' -> advance state; '\\'
  | Some '\'' -> advance state; '\''
  | Some '"' -> advance state; '"'
  | Some '?' -> advance state; '?'
  | Some ('0'..'7' as c) ->
      (* Octal escape *)
      advance state;
      let v1 = Char.code c - Char.code '0' in
      let v2 = match peek_char state with
        | Some ('0'..'7' as c2) -> advance state; Char.code c2 - Char.code '0'
        | _ -> 0
      in
      let v3 = match peek_char state with
        | Some ('0'..'7' as c3) when v2 > 0 -> advance state; Char.code c3 - Char.code '0'
        | _ -> 0
      in
      Char.chr ((v1 * 64) + (v2 * 8) + v3)
  | Some 'x' ->
      (* Hex escape *)
      advance state;
      let rec collect_hex acc count =
        if count >= 2 then acc
        else match peek_char state with
          | Some c when is_hex_digit c ->
              advance state;
              let digit = 
                match c with
                | '0'..'9' -> Char.code c - Char.code '0'
                | 'a'..'f' -> Char.code c - Char.code 'a' + 10
                | 'A'..'F' -> Char.code c - Char.code 'A' + 10
                | _ -> 0
              in
              collect_hex (acc * 16 + digit) (count + 1)
          | _ -> acc
      in
      let value = collect_hex 0 0 in
      if value = 0 then failwith "Invalid hex escape";
      Char.chr (value land 0xFF)
  | _ -> failwith "Invalid escape sequence"

(** Lex character constant *)
let lex_char_constant state =
  advance state;  (* Skip opening ' *)
  let c = match peek_char state with
    | Some '\\' -> advance state; parse_escape state
    | Some '\'' -> failwith "Empty character constant"
    | Some c -> advance state; c
    | None -> failwith "Unterminated character constant"
  in
  match peek_char state with
  | Some '\'' -> advance state; CharConstant c
  | _ -> failwith "Unterminated character constant"

(** Lex string literal *)
let lex_string_literal state =
  advance state;  (* Skip opening double-quote *)
  let buf = Buffer.create 64 in
  let rec collect () =
    match peek_char state with
    | None -> failwith "Unterminated string literal"
    | Some '"' -> advance state
    | Some '\\' ->
        advance state;
        Buffer.add_char buf (parse_escape state);
        collect ()
    | Some c ->
        advance state;
        Buffer.add_char buf c;
        collect ()
  in
  collect ();
  StringLiteral (Buffer.contents buf)

(** Lex single token *)
let lex_token state =
  skip_whitespace state;
  
  (* Skip comments *)
  if skip_comments state then begin
    skip_whitespace state;
    (* Try again after skipping comments *)
    if skip_comments state then
      skip_whitespace state
  end;
  
  let loc = current_location state in
  
  match peek_char state with
  | None -> { token = Eof; loc }
  | Some '\n' -> 
      advance state;
      { token = Newline; loc }
      
  (* Identifiers and keywords *)
  | Some c when is_ident_start c ->
      { token = lex_identifier state; loc }
      
  (* Numbers *)
  | Some c when is_digit c ->
      { token = lex_number state; loc }
      
  (* Character constants *)
  | Some '\'' ->
      { token = lex_char_constant state; loc }
      
  (* String literals *)
  | Some '"' ->
      { token = lex_string_literal state; loc }
      
  (* Two-character operators *)
  | Some '-' when peek_char_n state 1 = Some '>' ->
      advance state; advance state;
      { token = Arrow; loc }
  | Some '+' when peek_char_n state 1 = Some '+' ->
      advance state; advance state;
      { token = PlusPlus; loc }
  | Some '-' when peek_char_n state 1 = Some '-' ->
      advance state; advance state;
      { token = MinusMinus; loc }
  | Some '<' when peek_char_n state 1 = Some '<' ->
      advance state; advance state;
      if peek_char state = Some '=' then begin
        advance state;
        { token = LeftShiftEqual; loc }
      end else
        { token = LeftShift; loc }
  | Some '>' when peek_char_n state 1 = Some '>' ->
      advance state; advance state;
      if peek_char state = Some '=' then begin
        advance state;
        { token = RightShiftEqual; loc }
      end else
        { token = RightShift; loc }
  | Some '<' when peek_char_n state 1 = Some '=' ->
      advance state; advance state;
      { token = LessEqual; loc }
  | Some '>' when peek_char_n state 1 = Some '=' ->
      advance state; advance state;
      { token = GreaterEqual; loc }
  | Some '=' when peek_char_n state 1 = Some '=' ->
      advance state; advance state;
      { token = EqualEqual; loc }
  | Some '!' when peek_char_n state 1 = Some '=' ->
      advance state; advance state;
      { token = BangEqual; loc }
  | Some '&' when peek_char_n state 1 = Some '&' ->
      advance state; advance state;
      { token = AmpAmp; loc }
  | Some '|' when peek_char_n state 1 = Some '|' ->
      advance state; advance state;
      { token = PipePipe; loc }
  | Some '+' when peek_char_n state 1 = Some '=' ->
      advance state; advance state;
      { token = PlusEqual; loc }
  | Some '-' when peek_char_n state 1 = Some '=' ->
      advance state; advance state;
      { token = MinusEqual; loc }
  | Some '*' when peek_char_n state 1 = Some '=' ->
      advance state; advance state;
      { token = StarEqual; loc }
  | Some '/' when peek_char_n state 1 = Some '=' ->
      advance state; advance state;
      { token = SlashEqual; loc }
  | Some '%' when peek_char_n state 1 = Some '=' ->
      advance state; advance state;
      { token = PercentEqual; loc }
  | Some '&' when peek_char_n state 1 = Some '=' ->
      advance state; advance state;
      { token = AmpEqual; loc }
  | Some '|' when peek_char_n state 1 = Some '=' ->
      advance state; advance state;
      { token = PipeEqual; loc }
  | Some '^' when peek_char_n state 1 = Some '=' ->
      advance state; advance state;
      { token = CaretEqual; loc }
  | Some '#' when peek_char_n state 1 = Some '#' ->
      advance state; advance state;
      { token = HashHash; loc }
  | Some '.' when peek_char_n state 1 = Some '.' && peek_char_n state 2 = Some '.' ->
      advance state; advance state; advance state;
      { token = Ellipsis; loc }
      
  (* Single-character tokens *)
  | Some '+' -> advance state; { token = Plus; loc }
  | Some '-' -> advance state; { token = Minus; loc }
  | Some '*' -> advance state; { token = Star; loc }
  | Some '/' -> advance state; { token = Slash; loc }
  | Some '%' -> advance state; { token = Percent; loc }
  | Some '&' -> advance state; { token = Ampersand; loc }
  | Some '|' -> advance state; { token = Pipe; loc }
  | Some '^' -> advance state; { token = Caret; loc }
  | Some '~' -> advance state; { token = Tilde; loc }
  | Some '!' -> advance state; { token = Bang; loc }
  | Some '<' -> advance state; { token = Less; loc }
  | Some '>' -> advance state; { token = Greater; loc }
  | Some '?' -> advance state; { token = Question; loc }
  | Some ':' -> advance state; { token = Colon; loc }
  | Some '=' -> advance state; { token = Equal; loc }
  | Some '.' -> advance state; { token = Dot; loc }
  | Some ',' -> advance state; { token = Comma; loc }
  | Some ';' -> advance state; { token = Semicolon; loc }
  | Some '(' -> advance state; { token = LeftParen; loc }
  | Some ')' -> advance state; { token = RightParen; loc }
  | Some '[' -> advance state; { token = LeftBracket; loc }
  | Some ']' -> advance state; { token = RightBracket; loc }
  | Some '{' -> advance state; { token = LeftBrace; loc }
  | Some '}' -> advance state; { token = RightBrace; loc }
  | Some '#' -> advance state; { token = Hash; loc }
  
  | Some c ->
      advance state;
      failwith (Printf.sprintf "Unexpected character: %c" c)

(** Lex all tokens *)
let lex_all state =
  let rec collect acc =
    let tok = lex_token state in
    match tok.token with
    | Eof -> List.rev (tok :: acc)
    | _ -> collect (tok :: acc)
  in
  collect []

(** Concatenate adjacent string literals *)
let rec concat_string_literals tokens =
  match tokens with
  | { token = StringLiteral s1; loc = loc1 } :: 
    { token = StringLiteral s2; _ } :: rest ->
      (* Check if tokens are adjacent or only separated by whitespace *)
      let combined = { token = StringLiteral (s1 ^ s2); loc = loc1 } in
      concat_string_literals (combined :: rest)
  | tok :: rest ->
      tok :: concat_string_literals rest
  | [] -> []

(** Lex string into tokens *)
let lex_string filename input =
  let state = create_lexer filename input in
  let tokens = lex_all state in
  (* Post-process to concatenate adjacent string literals *)
  concat_string_literals tokens

(** Token to string for debugging *)
let token_to_string = function
  | Auto -> "auto"
  | Break -> "break"
  | Case -> "case"
  | Char -> "char"
  | Const -> "const"
  | Continue -> "continue"
  | Default -> "default"
  | Do -> "do"
  | Double -> "double"
  | Else -> "else"
  | Enum -> "enum"
  | Extern -> "extern"
  | Float -> "float"
  | For -> "for"
  | Goto -> "goto"
  | If -> "if"
  | Int -> "int"
  | Long -> "long"
  | Register -> "register"
  | Return -> "return"
  | Short -> "short"
  | Signed -> "signed"
  | Sizeof -> "sizeof"
  | Static -> "static"
  | Struct -> "struct"
  | Switch -> "switch"
  | Typedef -> "typedef"
  | Union -> "union"
  | Unsigned -> "unsigned"
  | Void -> "void"
  | Volatile -> "volatile"
  | While -> "while"
  | Identifier s -> Printf.sprintf "IDENT(%s)" s
  | IntConstant (v, s) -> Printf.sprintf "INT(%Ld%s)" v s
  | FloatConstant (v, s) -> Printf.sprintf "FLOAT(%g%s)" v s
  | CharConstant c -> Printf.sprintf "CHAR('%c')" c
  | StringLiteral s -> Printf.sprintf "STRING(\"%s\")" (String.escaped s)
  | Plus -> "+"
  | Minus -> "-"
  | Star -> "*"
  | Slash -> "/"
  | Percent -> "%"
  | Ampersand -> "&"
  | Pipe -> "|"
  | Caret -> "^"
  | Tilde -> "~"
  | Bang -> "!"
  | Less -> "<"
  | Greater -> ">"
  | LessEqual -> "<="
  | GreaterEqual -> ">="
  | EqualEqual -> "=="
  | BangEqual -> "!="
  | LeftShift -> "<<"
  | RightShift -> ">>"
  | PlusPlus -> "++"
  | MinusMinus -> "--"
  | AmpAmp -> "&&"
  | PipePipe -> "||"
  | Question -> "?"
  | Colon -> ":"
  | Equal -> "="
  | PlusEqual -> "+="
  | MinusEqual -> "-="
  | StarEqual -> "*="
  | SlashEqual -> "/="
  | PercentEqual -> "%="
  | AmpEqual -> "&="
  | PipeEqual -> "|="
  | CaretEqual -> "^="
  | LeftShiftEqual -> "<<="
  | RightShiftEqual -> ">>="
  | Arrow -> "->"
  | Dot -> "."
  | LeftParen -> "("
  | RightParen -> ")"
  | LeftBracket -> "["
  | RightBracket -> "]"
  | LeftBrace -> "{"
  | RightBrace -> "}"
  | Comma -> ","
  | Semicolon -> ";"
  | Ellipsis -> "..."
  | Hash -> "#"
  | HashHash -> "##"
  | Eof -> "EOF"
  | Newline -> "\\n"