(* PIR Lexer - Tokenization for PIR text format per spec *)

(* Token types *)
type token =
  (* Module-level keywords *)
  | TYPE | GLOBAL | CONST | FUNC | ENDFUNC | ALIGN | INIT
  (* Instructions *)
  | ADD | SUB | MUL | SDIV | UDIV | SREM | UREM
  | AND | OR | XOR | NOT | SHL | LSHR | ASHR | ROL | ROR
  | FADD | FSUB | FMUL | FDIV | FREM | FNEG | FMA
  | CLZ | CTZ | POPCNT
  | ICMP | FCMP | SELECT
  | LOAD | STORE | ALLOCA | MEMCPY | MEMSET
  | GEP | FIELDADDR | PTRADD | BITCAST
  | TRUNC | ZEXT | SEXT | FPTRUNC | FPEXT
  | FPTOUI | FPTOSI | UITOFP | SITOFP
  | SPLAT | SHUFFLE | EXTRACTLANE | INSERTLANE
  | CALL | TAILCALL
  (* Control flow *)
  | RET | BR | JMP | SWITCH | UNREACHABLE
  (* Types *)
  | I1 | I8 | I16 | I32 | I64 | F32 | F64 | PTR | VOID
  | STRUCT | PACKED_STRUCT | ARRAY
  (* Comparison predicates *)
  | EQ | NE | LT | LE | GT | GE  (* Can be prefixed with s/u *)
  | OEQ | OGT | OGE | OLT | OLE | ONE | ORD
  | UEQ | UNE | UNO
  (* Flags *)
  | NSW | CARRY | SAT
  (* Literals *)
  | INT of int64
  | FLOAT of float
  | STRING of string
  | IDENT of string
  | VECTYPE of int * string  (* v4xi32 *)
  (* Delimiters *)
  | LPAREN | RPAREN | LBRACKET | RBRACKET | LCHEVRON | RCHEVRON
  | LBRACE | RBRACE
  | COMMA | COLON | ARROW | EQUALS | DOT | AT
  (* Special *)
  | TRUE | FALSE | NULL | UNDEF | TO
  | EOF

(* Position information *)
type position = {
  line: int;
  column: int;
  offset: int;
}

(* Lexer state *)
type lexer_state = {
  input: string;
  mutable pos: int;
  mutable line: int;
  mutable column: int;
}

(* Create a new lexer state *)
let create_lexer input = {
  input;
  pos = 0;
  line = 1;
  column = 1;
}

(* Get current position *)
let current_position state = {
  line = state.line;
  column = state.column;
  offset = state.pos;
}

(* Peek at current character *)
let peek state =
  if state.pos >= String.length state.input then
    None
  else
    Some state.input.[state.pos]

(* Peek ahead n characters *)
let peek_n state n =
  let pos = state.pos + n in
  if pos >= String.length state.input then
    None
  else
    Some state.input.[pos]

(* Advance position *)
let advance state =
  if state.pos < String.length state.input then begin
    if state.input.[state.pos] = '\n' then begin
      state.line <- state.line + 1;
      state.column <- 1
    end else
      state.column <- state.column + 1;
    state.pos <- state.pos + 1
  end

(* Skip whitespace and comments *)
let rec skip_whitespace state =
  match peek state with
  | Some ' ' | Some '\t' | Some '\n' | Some '\r' ->
    advance state;
    skip_whitespace state
  | Some ';' ->
    (* Skip line comment per spec *)
    advance state;
    skip_line_comment state
  | _ -> ()

and skip_line_comment state =
  match peek state with
  | Some '\n' | None -> skip_whitespace state
  | _ ->
    advance state;
    skip_line_comment state

(* Read while predicate holds *)
let read_while pred state =
  let start = state.pos in
  let rec loop () =
    match peek state with
    | Some c when pred c ->
      advance state;
      loop ()
    | _ -> ()
  in
  loop ();
  String.sub state.input start (state.pos - start)

(* Character predicates *)
let is_alpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
let is_digit c = c >= '0' && c <= '9'
let is_alnum c = is_alpha c || is_digit c
let is_ident_start c = is_alpha c || c = '_'
let is_ident_cont c = is_alnum c || c = '_'

(* Read identifier or keyword *)
let read_ident state =
  let ident = read_while is_ident_cont state in
  match ident with
  (* Module-level keywords *)
  | "type" -> TYPE
  | "global" -> GLOBAL
  | "const" -> CONST
  | "func" -> FUNC
  | "endfunc" -> ENDFUNC
  | "align" -> ALIGN
  | "init" -> INIT
  (* Instructions *)
  | "add" -> ADD
  | "sub" -> SUB
  | "mul" -> MUL
  | "sdiv" -> SDIV
  | "udiv" -> UDIV
  | "srem" -> SREM
  | "urem" -> UREM
  | "and" -> AND
  | "or" -> OR
  | "xor" -> XOR
  | "not" -> NOT
  | "shl" -> SHL
  | "lshr" -> LSHR
  | "ashr" -> ASHR
  | "rol" -> ROL
  | "ror" -> ROR
  | "fadd" -> FADD
  | "fsub" -> FSUB
  | "fmul" -> FMUL
  | "fdiv" -> FDIV
  | "frem" -> FREM
  | "fneg" -> FNEG
  | "fma" -> FMA
  | "clz" -> CLZ
  | "ctz" -> CTZ
  | "popcnt" -> POPCNT
  (* Comparisons *)
  | "icmp" -> ICMP
  | "fcmp" -> FCMP
  | "select" -> SELECT
  (* Memory operations *)
  | "load" -> LOAD
  | "store" -> STORE
  | "alloca" -> ALLOCA
  | "memcpy" -> MEMCPY
  | "memset" -> MEMSET
  (* Address operations *)
  | "gep" -> GEP
  | "fieldaddr" -> FIELDADDR
  | "ptradd" -> PTRADD
  | "bitcast" -> BITCAST
  (* Cast operations *)
  | "trunc" -> TRUNC
  | "zext" -> ZEXT
  | "sext" -> SEXT
  | "fptrunc" -> FPTRUNC
  | "fpext" -> FPEXT
  | "fptoui" -> FPTOUI
  | "fptosi" -> FPTOSI
  | "uitofp" -> UITOFP
  | "sitofp" -> SITOFP
  (* Vector operations *)
  | "splat" -> SPLAT
  | "shuffle" -> SHUFFLE
  | "extractlane" -> EXTRACTLANE
  | "insertlane" -> INSERTLANE
  (* Call operations *)
  | "call" -> CALL
  | "tailcall" -> TAILCALL
  (* Control flow *)
  | "ret" -> RET
  | "br" -> BR
  | "jmp" -> JMP
  | "switch" -> SWITCH
  | "unreachable" -> UNREACHABLE
  (* Types *)
  | "i1" -> I1
  | "i8" -> I8
  | "i16" -> I16
  | "i32" -> I32
  | "i64" -> I64
  | "f32" -> F32
  | "f64" -> F64
  | "ptr" -> PTR
  | "void" -> VOID
  | "struct" -> STRUCT
  | "packed_struct" -> PACKED_STRUCT
  | "array" -> ARRAY
  (* Comparison predicates *)
  | "eq" -> EQ
  | "ne" -> NE
  | "lt" -> LT
  | "le" -> LE
  | "gt" -> GT
  | "ge" -> GE
  | "slt" -> LT  (* Will need context to distinguish *)
  | "sle" -> LE
  | "sgt" -> GT
  | "sge" -> GE
  | "ult" -> LT
  | "ule" -> LE
  | "ugt" -> GT
  | "uge" -> GE
  | "oeq" -> OEQ
  | "ogt" -> OGT
  | "oge" -> OGE
  | "olt" -> OLT
  | "ole" -> OLE
  (* | "one" -> ONE  -- Conflicts with common variable name *)
  (* | "ord" -> ORD  -- Conflicts with common variable name *)
  | "ueq" -> UEQ
  | "une" -> UNE
  | "uno" -> UNO
  (* Flags *)
  | "nsw" -> NSW
  | "carry" -> CARRY
  | "sat" -> SAT
  (* Special values *)
  | "true" -> TRUE
  | "false" -> FALSE
  | "null" -> NULL
  | "undef" -> UNDEF
  | "to" -> TO
  (* Regular identifier *)
  | _ -> IDENT ident

(* Read number *)
let read_number state =
  let is_hex = peek state = Some '0' && peek_n state 1 = Some 'x' in
  if is_hex then begin
    advance state; advance state; (* Skip 0x *)
    let hex = read_while (fun c -> is_digit c || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')) state in
    INT (Int64.of_string ("0x" ^ hex))
  end else begin
    let num = read_while is_digit state in
    match peek state with
    | Some '.' when peek_n state 1 <> Some '.' ->
      advance state;
      let frac = read_while is_digit state in
      let float_str = num ^ "." ^ frac in
      (* Handle scientific notation *)
      (match peek state with
       | Some ('e' | 'E') ->
         advance state;
         let sign = match peek state with
           | Some ('+' | '-' as s) -> advance state; String.make 1 s
           | _ -> "" in
         let exp = read_while is_digit state in
         FLOAT (float_of_string (float_str ^ "e" ^ sign ^ exp))
       | _ -> FLOAT (float_of_string float_str))
    | _ -> INT (Int64.of_string num)
  end

(* Read string literal *)
let read_string state =
  advance state; (* Skip opening quote *)
  let buf = Buffer.create 16 in
  let rec loop () =
    match peek state with
    | None -> failwith "Unterminated string"
    | Some '"' ->
      advance state;
      STRING (Buffer.contents buf)
    | Some '\\' ->
      advance state;
      (match peek state with
       | Some 'n' -> advance state; Buffer.add_char buf '\n'; loop ()
       | Some 't' -> advance state; Buffer.add_char buf '\t'; loop ()
       | Some 'r' -> advance state; Buffer.add_char buf '\r'; loop ()
       | Some '\\' -> advance state; Buffer.add_char buf '\\'; loop ()
       | Some '"' -> advance state; Buffer.add_char buf '"'; loop ()
       | Some c -> advance state; Buffer.add_char buf c; loop ()
       | None -> failwith "Unterminated string escape")
    | Some c ->
      advance state;
      Buffer.add_char buf c;
      loop ()
  in
  loop ()

(* Read vector type like v4xi32 *)
let read_vector_type state =
  advance state; (* Skip 'v' *)
  let num_str = read_while is_digit state in
  if String.length num_str = 0 then
    failwith "Invalid vector type: missing element count";
  let n = int_of_string num_str in
  (* Expect 'x' *)
  match peek state with
  | Some 'x' -> 
    advance state;
    let scalar_ty = read_while is_alnum state in
    VECTYPE (n, scalar_ty)
  | Some c -> failwith (Printf.sprintf "Invalid vector type: expected 'x' after count but got '%c'" c)
  | None -> failwith "Invalid vector type: expected 'x' after count but got EOF"

(* Get next token *)
let next_token state =
  skip_whitespace state;
  match peek state with
  | None -> EOF
  | Some '"' -> read_string state
  | Some 'v' when (match peek_n state 1 with Some c -> is_digit c | _ -> false) ->
    (* Check if this is really a vector type or just an identifier starting with v *)
    let saved_pos = state.pos in
    let saved_line = state.line in
    let saved_col = state.column in
    advance state; (* Skip 'v' *)
    let _num_str = read_while is_digit state in
    let is_vector = match peek state with
      | Some 'x' -> true  (* This looks like a vector type *)
      | _ -> false
    in
    (* Restore position *)
    state.pos <- saved_pos;
    state.line <- saved_line;
    state.column <- saved_col;
    if is_vector then
      read_vector_type state
    else
      (* Just a regular identifier starting with v *)
      read_ident state
  | Some c when is_digit c -> read_number state
  | Some c when is_ident_start c ->
    let ident_start = state.pos in
    let _ident = read_while is_ident_cont state in
    (* Check for label (identifier followed by colon) *)
    if peek state = Some ':' then begin
      advance state; (* Skip : *)
      (* Return as identifier - parser will handle label context *)
      state.pos <- ident_start;
      read_ident state
    end else begin
      state.pos <- ident_start;
      read_ident state
    end
  | Some '-' ->
    advance state;
    (match peek state with
     | Some '>' -> advance state; ARROW
     | Some c when is_digit c ->
       let num_tok = read_number state in
       (match num_tok with
        | INT n -> INT (Int64.neg n)
        | FLOAT f -> FLOAT (-.f)
        | _ -> failwith "Unexpected number token")
     | _ -> failwith "Unexpected character after -")
  | Some '<' ->
    advance state;
    (match peek state with
     | Some '<' -> advance state; LCHEVRON
     | _ -> failwith "Expected << for chevron")
  | Some '>' ->
    advance state;
    (match peek state with
     | Some '>' -> advance state; RCHEVRON
     | _ -> failwith "Expected >> for chevron")
  | Some '(' -> advance state; LPAREN
  | Some ')' -> advance state; RPAREN
  | Some '[' -> advance state; LBRACKET
  | Some ']' -> advance state; RBRACKET
  | Some '{' -> advance state; LBRACE
  | Some '}' -> advance state; RBRACE
  | Some ',' -> advance state; COMMA
  | Some ':' -> advance state; COLON
  | Some '=' -> advance state; EQUALS
  | Some '.' -> advance state; DOT
  | Some '@' -> advance state; AT
  | Some c -> failwith (Printf.sprintf "Unexpected character: %c" c)

(* Tokenize entire input *)
let tokenize input =
  let state = create_lexer input in
  let rec loop acc =
    match next_token state with
    | EOF -> List.rev (EOF :: acc)
    | tok -> loop (tok :: acc)
  in
  loop []

(* Token to string for debugging *)
let string_of_token = function
  (* Module-level *)
  | TYPE -> "TYPE"
  | GLOBAL -> "GLOBAL"
  | CONST -> "CONST"
  | FUNC -> "FUNC"
  | ENDFUNC -> "ENDFUNC"
  | ALIGN -> "ALIGN"
  | INIT -> "INIT"
  (* Instructions *)
  | ADD -> "ADD"
  | SUB -> "SUB"
  | MUL -> "MUL"
  | SDIV -> "SDIV"
  | UDIV -> "UDIV"
  | SREM -> "SREM"
  | UREM -> "UREM"
  | AND -> "AND"
  | OR -> "OR"
  | XOR -> "XOR"
  | NOT -> "NOT"
  | SHL -> "SHL"
  | LSHR -> "LSHR"
  | ASHR -> "ASHR"
  | ROL -> "ROL"
  | ROR -> "ROR"
  | FADD -> "FADD"
  | FSUB -> "FSUB"
  | FMUL -> "FMUL"
  | FDIV -> "FDIV"
  | FREM -> "FREM"
  | FNEG -> "FNEG"
  | FMA -> "FMA"
  | CLZ -> "CLZ"
  | CTZ -> "CTZ"
  | POPCNT -> "POPCNT"
  | ICMP -> "ICMP"
  | FCMP -> "FCMP"
  | SELECT -> "SELECT"
  | LOAD -> "LOAD"
  | STORE -> "STORE"
  | ALLOCA -> "ALLOCA"
  | MEMCPY -> "MEMCPY"
  | MEMSET -> "MEMSET"
  | GEP -> "GEP"
  | FIELDADDR -> "FIELDADDR"
  | PTRADD -> "PTRADD"
  | BITCAST -> "BITCAST"
  | TRUNC -> "TRUNC"
  | ZEXT -> "ZEXT"
  | SEXT -> "SEXT"
  | FPTRUNC -> "FPTRUNC"
  | FPEXT -> "FPEXT"
  | FPTOUI -> "FPTOUI"
  | FPTOSI -> "FPTOSI"
  | UITOFP -> "UITOFP"
  | SITOFP -> "SITOFP"
  | SPLAT -> "SPLAT"
  | SHUFFLE -> "SHUFFLE"
  | EXTRACTLANE -> "EXTRACTLANE"
  | INSERTLANE -> "INSERTLANE"
  | CALL -> "CALL"
  | TAILCALL -> "TAILCALL"
  (* Control flow *)
  | RET -> "RET"
  | BR -> "BR"
  | JMP -> "JMP"
  | SWITCH -> "SWITCH"
  | UNREACHABLE -> "UNREACHABLE"
  (* Types *)
  | I1 -> "I1"
  | I8 -> "I8"
  | I16 -> "I16"
  | I32 -> "I32"
  | I64 -> "I64"
  | F32 -> "F32"
  | F64 -> "F64"
  | PTR -> "PTR"
  | VOID -> "VOID"
  | STRUCT -> "STRUCT"
  | PACKED_STRUCT -> "PACKED_STRUCT"
  | ARRAY -> "ARRAY"
  (* Comparisons *)
  | EQ -> "EQ"
  | NE -> "NE"
  | LT -> "LT"
  | LE -> "LE"
  | GT -> "GT"
  | GE -> "GE"
  | OEQ -> "OEQ"
  | OGT -> "OGT"
  | OGE -> "OGE"
  | OLT -> "OLT"
  | OLE -> "OLE"
  | ONE -> "ONE"
  | ORD -> "ORD"
  | UEQ -> "UEQ"
  | UNE -> "UNE"
  | UNO -> "UNO"
  (* Flags *)
  | NSW -> "NSW"
  | CARRY -> "CARRY"
  | SAT -> "SAT"
  (* Literals *)
  | INT n -> Printf.sprintf "INT(%Ld)" n
  | FLOAT f -> Printf.sprintf "FLOAT(%f)" f
  | STRING s -> Printf.sprintf "STRING(%S)" s
  | IDENT s -> Printf.sprintf "IDENT(%s)" s
  | VECTYPE (n, s) -> Printf.sprintf "VECTYPE(v%dx%s)" n s
  (* Delimiters *)
  | LPAREN -> "LPAREN"
  | RPAREN -> "RPAREN"
  | LBRACKET -> "LBRACKET"
  | RBRACKET -> "RBRACKET"
  | LCHEVRON -> "LCHEVRON"
  | RCHEVRON -> "RCHEVRON"
  | LBRACE -> "LBRACE"
  | RBRACE -> "RBRACE"
  | COMMA -> "COMMA"
  | COLON -> "COLON"
  | ARROW -> "ARROW"
  | EQUALS -> "EQUALS"
  | DOT -> "DOT"
  | AT -> "AT"
  (* Special *)
  | TRUE -> "TRUE"
  | FALSE -> "FALSE"
  | NULL -> "NULL"
  | UNDEF -> "UNDEF"
  | TO -> "TO"
  | EOF -> "EOF"