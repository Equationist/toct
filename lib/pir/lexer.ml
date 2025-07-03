(* PIR Lexer - Tokenization for PIR text format *)

(* Token types *)
type token =
  (* Keywords *)
  | FUNC | RET | BR | JMP | SWITCH | UNREACHABLE
  | ADD | SUB | MUL | SDIV | UDIV | SREM | UREM
  | AND | OR | XOR | SHL | LSHR | ASHR | ROL | ROR
  | FADD | FSUB | FMUL | FDIV | FREM | FMA
  | CLZ | CTZ | POPCNT
  | ICMP | FCMP | SELECT
  | LOAD | STORE | ALLOCA | MEMCPY | MEMSET
  | GEP | FIELDADDR | PTRADD
  | BITCAST | TRUNC | ZEXT | SEXT | FPTRUNC | FPEXT
  | FPTOUI | FPTOSI | UITOFP | SITOFP
  | SPLAT | SHUFFLE | EXTRACTLANE | INSERTLANE
  | CALL | TAILCALL | PHI
  | CONST
  (* Types *)
  | I1 | I8 | I16 | I32 | I64 | F32 | F64 | PTR
  | VEC | STRUCT | PACKED_STRUCT | ARRAY
  (* Comparison predicates *)
  | EQ | NE | SLT | SLE | SGT | SGE | ULT | ULE | UGT | UGE
  | OEQ | OGT | OGE | OLT | OLE | ONE | ORD
  | UEQ | UGT_F | UGE_F | ULT_F | ULE_F | UNE | UNO
  (* Flags *)
  | NSW | CARRY | SAT
  (* Literals *)
  | INT of int64
  | FLOAT of float
  | STRING of string
  | IDENT of string
  | LABEL of string
  | VALUE of string  (* %123 *)
  (* Delimiters *)
  | LPAREN | RPAREN | LBRACE | RBRACE | LBRACKET | RBRACKET
  | COMMA | COLON | SEMICOLON | ARROW | EQUALS | DOT | AT
  (* Special *)
  | TRUE | FALSE | NULL | UNDEF | ZEROINITIALIZER
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
    (* Skip line comment *)
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
let is_ident_cont c = is_alnum c || c = '_' || c = '.'

(* Read identifier or keyword *)
let read_ident state =
  let ident = read_while is_ident_cont state in
  match ident with
  (* Keywords *)
  | "func" -> FUNC
  | "ret" -> RET
  | "br" -> BR
  | "jmp" -> JMP
  | "switch" -> SWITCH
  | "unreachable" -> UNREACHABLE
  (* Binary operations *)
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
  (* Cast operations *)
  | "bitcast" -> BITCAST
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
  | "phi" -> PHI
  | "const" -> CONST
  (* Types *)
  | "i1" -> I1
  | "i8" -> I8
  | "i16" -> I16
  | "i32" -> I32
  | "i64" -> I64
  | "f32" -> F32
  | "f64" -> F64
  | "ptr" -> PTR
  | "vec" -> VEC
  | "struct" -> STRUCT
  | "packed_struct" -> PACKED_STRUCT
  | "array" -> ARRAY
  (* Comparison predicates *)
  | "eq" -> EQ
  | "ne" -> NE
  | "slt" -> SLT
  | "sle" -> SLE
  | "sgt" -> SGT
  | "sge" -> SGE
  | "ult" -> ULT
  | "ule" -> ULE
  | "ugt" -> UGT
  | "uge" -> UGE
  | "oeq" -> OEQ
  | "ogt" -> OGT
  | "oge" -> OGE
  | "olt" -> OLT
  | "ole" -> OLE
  | "one" -> ONE
  | "ord" -> ORD
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
  | "zeroinitializer" -> ZEROINITIALIZER
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

(* Read value reference *)
let read_value state =
  advance state; (* Skip % *)
  let name = read_while is_alnum state in
  VALUE name

(* Get next token *)
let next_token state =
  skip_whitespace state;
  match peek state with
  | None -> EOF
  | Some '%' -> read_value state
  | Some '"' -> read_string state
  | Some c when is_digit c -> read_number state
  | Some c when is_ident_start c ->
    let ident_start = state.pos in
    let ident = read_while is_ident_cont state in
    (* Only treat as label if it's at start of line (for block labels) *)
    if peek state = Some ':' then begin
      (* Check if this looks like a block label by seeing if we're at line start
         or if the next line starts with whitespace (indicating block content) *)
      let is_block_label = 
        state.column = 1 || (* At start of line *)
        (match peek_n state 1 with
         | Some '\n' | Some '\r' -> true (* Followed by newline *)
         | _ -> false) in
      if is_block_label then begin
        advance state; (* Skip : *)
        LABEL ident
      end else begin
        state.pos <- ident_start; (* Reset to read as ident *)
        read_ident state
      end
    end else begin
      state.pos <- ident_start; (* Reset to read as ident *)
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
  | Some '(' -> advance state; LPAREN
  | Some ')' -> advance state; RPAREN
  | Some '{' -> advance state; LBRACE
  | Some '}' -> advance state; RBRACE
  | Some '[' -> advance state; LBRACKET
  | Some ']' -> advance state; RBRACKET
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
  | FUNC -> "FUNC"
  | RET -> "RET"
  | BR -> "BR"
  | JMP -> "JMP"
  | SWITCH -> "SWITCH"
  | UNREACHABLE -> "UNREACHABLE"
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
  | PHI -> "PHI"
  | CONST -> "CONST"
  | I1 -> "I1"
  | I8 -> "I8"
  | I16 -> "I16"
  | I32 -> "I32"
  | I64 -> "I64"
  | F32 -> "F32"
  | F64 -> "F64"
  | PTR -> "PTR"
  | VEC -> "VEC"
  | STRUCT -> "STRUCT"
  | PACKED_STRUCT -> "PACKED_STRUCT"
  | ARRAY -> "ARRAY"
  | EQ -> "EQ"
  | NE -> "NE"
  | SLT -> "SLT"
  | SLE -> "SLE"
  | SGT -> "SGT"
  | SGE -> "SGE"
  | ULT -> "ULT"
  | ULE -> "ULE"
  | UGT -> "UGT"
  | UGE -> "UGE"
  | OEQ -> "OEQ"
  | OGT -> "OGT"
  | OGE -> "OGE"
  | OLT -> "OLT"
  | OLE -> "OLE"
  | ONE -> "ONE"
  | ORD -> "ORD"
  | UEQ -> "UEQ"
  | UGT_F -> "UGT_F"
  | UGE_F -> "UGE_F"
  | ULT_F -> "ULT_F"
  | ULE_F -> "ULE_F"
  | UNE -> "UNE"
  | UNO -> "UNO"
  | NSW -> "NSW"
  | CARRY -> "CARRY"
  | SAT -> "SAT"
  | INT n -> Printf.sprintf "INT(%Ld)" n
  | FLOAT f -> Printf.sprintf "FLOAT(%f)" f
  | STRING s -> Printf.sprintf "STRING(%S)" s
  | IDENT s -> Printf.sprintf "IDENT(%s)" s
  | LABEL s -> Printf.sprintf "LABEL(%s)" s
  | VALUE s -> Printf.sprintf "VALUE(%%%s)" s
  | LPAREN -> "LPAREN"
  | RPAREN -> "RPAREN"
  | LBRACE -> "LBRACE"
  | RBRACE -> "RBRACE"
  | LBRACKET -> "LBRACKET"
  | RBRACKET -> "RBRACKET"
  | COMMA -> "COMMA"
  | COLON -> "COLON"
  | SEMICOLON -> "SEMICOLON"
  | ARROW -> "ARROW"
  | EQUALS -> "EQUALS"
  | DOT -> "DOT"
  | AT -> "AT"
  | TRUE -> "TRUE"
  | FALSE -> "FALSE"
  | NULL -> "NULL"
  | UNDEF -> "UNDEF"
  | ZEROINITIALIZER -> "ZEROINITIALIZER"
  | EOF -> "EOF"