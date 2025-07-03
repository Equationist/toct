(** C89/90 Lexer Interface *)

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
  | Hash | HashHash
  
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
type lexer_state

(** Create initial lexer state *)
val create_lexer : string -> string -> lexer_state

(** Lex single token *)
val lex_token : lexer_state -> located_token

(** Lex all tokens *)
val lex_all : lexer_state -> located_token list

(** Lex string into tokens *)
val lex_string : string -> string -> located_token list

(** Token to string for debugging *)
val token_to_string : token -> string