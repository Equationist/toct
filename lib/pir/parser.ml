(* PIR Parser - Parsing PIR text format to IR structures *)

open Types
open Values
open Instructions
open Attributes
open Lexer

(* Parser state *)
type parser_state = {
  tokens: token list;
  mutable pos: int;
  (* Symbol tables *)
  values: (string, value) Hashtbl.t;
  blocks: (string, string) Hashtbl.t; (* label -> label (for forward refs) *)
  (* Current function context *)
  mutable current_func: string option;
}

(* Parser errors *)
type parse_error =
  | UnexpectedToken of token * string
  | UnexpectedEOF of string
  | UndefinedValue of string
  | DuplicateDefinition of string
  | TypeError of string
  | SyntaxError of string

exception ParseError of parse_error

let string_of_parse_error = function
  | UnexpectedToken (tok, expected) ->
    Printf.sprintf "Unexpected token %s, expected %s" (string_of_token tok) expected
  | UnexpectedEOF expected ->
    Printf.sprintf "Unexpected end of file, expected %s" expected
  | UndefinedValue name ->
    Printf.sprintf "Undefined value: %s" name
  | DuplicateDefinition name ->
    Printf.sprintf "Duplicate definition: %s" name
  | TypeError msg ->
    Printf.sprintf "Type error: %s" msg
  | SyntaxError msg ->
    Printf.sprintf "Syntax error: %s" msg

(* Create parser state *)
let create_parser tokens = {
  tokens;
  pos = 0;
  values = Hashtbl.create 64;
  blocks = Hashtbl.create 16;
  current_func = None;
}

(* Current token *)
let current state =
  if state.pos >= List.length state.tokens then EOF
  else List.nth state.tokens state.pos

(* Peek ahead *)
let peek state n =
  let pos = state.pos + n in
  if pos >= List.length state.tokens then EOF
  else List.nth state.tokens pos

(* Advance to next token *)
let advance state =
  if state.pos < List.length state.tokens then
    state.pos <- state.pos + 1

(* Consume expected token *)
let expect state tok =
  if current state = tok then
    advance state
  else
    raise (ParseError (UnexpectedToken (current state, string_of_token tok)))

(* Consume any of expected tokens *)
let expect_one_of state toks =
  let curr = current state in
  if List.mem curr toks then begin
    advance state;
    curr
  end else
    let expected = String.concat " or " (List.map string_of_token toks) in
    raise (ParseError (UnexpectedToken (curr, expected)))

(* Parse helpers *)
let parse_ident state =
  match current state with
  | IDENT name -> advance state; name
  | tok -> raise (ParseError (UnexpectedToken (tok, "identifier")))

let parse_label state =
  match current state with
  | LABEL name -> advance state; name
  | IDENT name ->
    (match peek state 1 with
     | COLON -> advance state; advance state; name
     | _ -> raise (ParseError (UnexpectedToken (IDENT name, "label with colon"))))
  | tok -> raise (ParseError (UnexpectedToken (tok, "label")))

let parse_value_name state =
  match current state with
  | VALUE name -> advance state; "%" ^ name
  | tok -> raise (ParseError (UnexpectedToken (tok, "value name")))

(* Type parsing *)
let rec parse_type state =
  match current state with
  | I1 -> advance state; Scalar I1
  | I8 -> advance state; Scalar I8
  | I16 -> advance state; Scalar I16
  | I32 -> advance state; Scalar I32
  | I64 -> advance state; Scalar I64
  | F32 -> advance state; Scalar F32
  | F64 -> advance state; Scalar F64
  | PTR -> advance state; Ptr
  | VEC ->
    advance state;
    expect state LBRACKET;
    let n = parse_int state in
    (match current state with
     | IDENT "x" -> advance state
     | _ -> raise (ParseError (UnexpectedToken (current state, "'x'"))));
    let elem_ty = parse_scalar_type state in
    expect state RBRACKET;
    Vector (n, elem_ty)
  | ARRAY ->
    advance state;
    expect state LBRACKET;
    let n = parse_int state in
    (* Optional 'x' separator *)
    (match current state with
     | IDENT "x" -> advance state
     | _ -> ());
    let elem_ty = parse_type state in
    expect state RBRACKET;
    Array (n, elem_ty)
  | STRUCT ->
    advance state;
    expect state LBRACE;
    let fields = parse_type_list state in
    expect state RBRACE;
    Struct fields
  | PACKED_STRUCT ->
    advance state;
    expect state LBRACE;
    let fields = parse_type_list state in
    expect state RBRACE;
    PackedStruct fields
  | tok -> raise (ParseError (UnexpectedToken (tok, "type")))

and parse_scalar_type state =
  match parse_type state with
  | Scalar s -> s
  | _ -> raise (ParseError (TypeError "Expected scalar type"))

and parse_type_list state =
  let rec loop acc =
    match current state with
    | RBRACE ->
      List.rev acc
    | _ -> begin
      let ty = parse_type state in
      let acc' = ty :: acc in
      match current state with
      | COMMA -> begin
        advance state;
        loop acc'
      end
      | _ ->
        List.rev acc'
    end
  in
  loop []

and parse_int state =
  match current state with
  | INT n -> advance state; Int64.to_int n
  | tok -> raise (ParseError (UnexpectedToken (tok, "integer")))

(* Value parsing *)
let parse_value state =
  match current state with
  | VALUE name ->
    advance state;
    let full_name = "%" ^ name in
    (try Hashtbl.find state.values full_name
     with Not_found -> 
       (* Try without % prefix for parameters *)
       try Hashtbl.find state.values name
       with Not_found -> raise (ParseError (UndefinedValue full_name)))
  | INT _n ->
    advance state;
    (* Infer type from context or default to i32 *)
    create_simple_value (Scalar I32)
  | FLOAT _f ->
    advance state;
    create_simple_value (Scalar F64)
  | TRUE ->
    advance state;
    create_simple_value (Scalar I1)
  | FALSE ->
    advance state;
    create_simple_value (Scalar I1)
  | NULL ->
    advance state;
    create_simple_value Ptr
  | UNDEF ->
    advance state;
    let ty = parse_type state in
    create_simple_value ty
  | IDENT name ->
    advance state;
    (try Hashtbl.find state.values name
     with Not_found -> raise (ParseError (UndefinedValue name)))
  | tok -> raise (ParseError (UnexpectedToken (tok, "value")))

(* Define a new value *)
let define_value state name ty =
  if Hashtbl.mem state.values name then
    raise (ParseError (DuplicateDefinition name));
  let value = create_simple_value ty in
  Hashtbl.add state.values name value;
  value

(* Instruction parsing *)
let parse_binop_kind state =
  match current state with
  | ADD -> advance state; Add
  | SUB -> advance state; Sub
  | MUL -> advance state; Mul
  | SDIV -> advance state; Sdiv
  | UDIV -> advance state; Udiv
  | SREM -> advance state; Srem
  | UREM -> advance state; Urem
  | AND -> advance state; And
  | OR -> advance state; Or
  | XOR -> advance state; Xor
  | SHL -> advance state; Shl
  | LSHR -> advance state; Lshr
  | ASHR -> advance state; Ashr
  | ROL -> advance state; Rol
  | ROR -> advance state; Ror
  | FADD -> advance state; Fadd
  | FSUB -> advance state; Fsub
  | FMUL -> advance state; Fmul
  | FDIV -> advance state; Fdiv
  | FREM -> advance state; Frem
  | FMA -> advance state; Fma
  | CLZ -> advance state; Clz
  | CTZ -> advance state; Ctz
  | POPCNT -> advance state; Popcnt
  | tok -> raise (ParseError (UnexpectedToken (tok, "binary operation")))

let parse_icmp_pred state =
  match current state with
  | EQ -> advance state; Eq
  | NE -> advance state; Ne
  | SLT -> advance state; Slt
  | SLE -> advance state; Sle
  | SGT -> advance state; Sgt
  | SGE -> advance state; Sge
  | ULT -> advance state; Ult
  | ULE -> advance state; Ule
  | UGT -> advance state; Ugt
  | UGE -> advance state; Uge
  | tok -> raise (ParseError (UnexpectedToken (tok, "icmp predicate")))

let parse_fcmp_pred state =
  match current state with
  | OEQ -> advance state; Oeq
  | OGT -> advance state; Ogt
  | OGE -> advance state; Oge
  | OLT -> advance state; Olt
  | OLE -> advance state; Ole
  | ONE -> advance state; One
  | ORD -> advance state; Ord
  | UEQ -> advance state; Ueq
  | UGT -> advance state; Ugt
  | UGE -> advance state; Uge
  | ULT -> advance state; Ult
  | ULE -> advance state; Ule
  | UNE -> advance state; Une
  | UNO -> advance state; Uno
  | tok -> raise (ParseError (UnexpectedToken (tok, "fcmp predicate")))

let parse_flag state =
  match current state with
  | DOT ->
    advance state;
    (match current state with
     | NSW -> advance state; Nsw
     | CARRY -> advance state; Carry
     | SAT -> advance state; Sat
     | tok -> raise (ParseError (UnexpectedToken (tok, "flag"))))
  | _ -> NoFlag

let rec parse_instruction state =
  (* Check for result assignment *)
  let result_name, has_result =
    match current state, peek state 1 with
    | VALUE _, EQUALS -> begin
      let name = parse_value_name state in
      expect state EQUALS;
      (Some name, true)
    end
    | _ ->
      (None, false) in
  
  (* Parse the instruction *)
  let instr = parse_instr_body state in
  
  (* Parse attributes *)
  let attrs = parse_attributes state in
  
  (* Create instruction with result *)
  let result_value = match result_name with
    | Some name when has_result ->
      (match result_type_of_instr instr with
       | Some ty -> Some (define_value state name ty)
       | None -> raise (ParseError (TypeError "Instruction does not produce a result")))
    | _ -> None in
  
  create_instruction ?result:result_value instr attrs

and parse_instr_body state =
  match current state with
  (* Binary operations *)
  | ADD | SUB | MUL | SDIV | UDIV | SREM | UREM
  | AND | OR | XOR | SHL | LSHR | ASHR | ROL | ROR
  | FADD | FSUB | FMUL | FDIV | FREM | FMA
  | CLZ | CTZ | POPCNT ->
    let op = parse_binop_kind state in
    let flag = parse_flag state in
    let v1 = parse_value state in
    expect state COMMA;
    let v2 = parse_value state in
    Binop (op, flag, v1, v2)
  
  (* Comparisons *)
  | ICMP ->
    advance state;
    let pred = parse_icmp_pred state in
    let v1 = parse_value state in
    expect state COMMA;
    let v2 = parse_value state in
    Icmp (pred, v1, v2)
  
  | FCMP ->
    advance state;
    let pred = parse_fcmp_pred state in
    let v1 = parse_value state in
    expect state COMMA;
    let v2 = parse_value state in
    Fcmp (pred, v1, v2)
  
  | SELECT ->
    advance state;
    let cond = parse_value state in
    expect state COMMA;
    let v_true = parse_value state in
    expect state COMMA;
    let v_false = parse_value state in
    Select (cond, v_true, v_false)
  
  (* Memory operations *)
  | LOAD ->
    advance state;
    let ty = parse_type state in
    let _ptr = parse_value state in
    Memory (Load ty)
  
  | STORE ->
    advance state;
    let value = parse_value state in
    expect state COMMA;
    let ptr = parse_value state in
    Memory (Store (value, ptr))
  
  | ALLOCA ->
    advance state;
    let size = parse_value state in
    expect state COMMA;
    let align = parse_int state in
    Memory (Alloca (size, align))
  
  (* Address operations *)
  | GEP ->
    advance state;
    let base = parse_value state in
    expect state COMMA;
    let idx = parse_value state in
    Address (Gep (base, idx))
  
  | FIELDADDR ->
    advance state;
    let base = parse_value state in
    expect state COMMA;
    let idx = parse_int state in
    Address (FieldAddr (base, idx))
  
  | PTRADD ->
    advance state;
    let base = parse_value state in
    expect state COMMA;
    let offset = parse_value state in
    Address (PtrAdd (base, offset))
  
  (* Cast operations *)
  | BITCAST ->
    advance state;
    let v = parse_value state in
    Cast (Bitcast v)
  
  | (TRUNC | ZEXT | SEXT | FPTRUNC | FPEXT
    | FPTOUI | FPTOSI | UITOFP | SITOFP) ->
    let cast_kind = current state in
    advance state;
    let v = parse_value state in
    (match current state with
     | IDENT "to" -> advance state
     | _ -> raise (ParseError (UnexpectedToken (current state, "'to'"))));
    let target_ty = parse_type state in
    let cast_op = match cast_kind with
      | TRUNC -> Trunc (v, target_ty)
      | ZEXT -> Zext (v, target_ty)
      | SEXT -> Sext (v, target_ty)
      | FPTRUNC -> Fptrunc (v, target_ty)
      | FPEXT -> Fpext (v, target_ty)
      | FPTOUI -> Fptoui (v, target_ty)
      | FPTOSI -> Fptosi (v, target_ty)
      | UITOFP -> Uitofp (v, target_ty)
      | SITOFP -> Sitofp (v, target_ty)
      | _ -> failwith "Unreachable" in
    Cast cast_op
  
  (* Call operations *)
  | CALL ->
    advance state;
    let callee = parse_value state in
    expect state LPAREN;
    let args = parse_value_list state in
    expect state RPAREN;
    Call (Call (callee, args))
  
  | tok -> raise (ParseError (UnexpectedToken (tok, "instruction")))

and parse_value_list state =
  let rec loop acc =
    match current state with
    | RPAREN ->
      List.rev acc
    | _ -> begin
      let v = parse_value state in
      let acc' = v :: acc in
      match current state with
      | COMMA -> begin
        advance state;
        loop acc'
      end
      | _ ->
        List.rev acc'
    end
  in
  loop []

and parse_attributes state =
  match current state with
  | AT -> begin
    advance state;
    expect state LBRACE;
    (* For now, skip attribute parsing *)
    let rec skip_until_rbrace () =
      match current state with
      | RBRACE ->
        advance state
      | EOF ->
        raise (ParseError (UnexpectedEOF "attribute closing brace"))
      | _ -> begin
        advance state;
        skip_until_rbrace ()
      end
    in
    skip_until_rbrace ();
    empty ()
  end
  | _ ->
    empty ()

(* Terminator parsing *)
let rec parse_terminator state =
  match current state with
  | RET ->
    advance state;
    (match current state with
     | VALUE _ | INT _ | FLOAT _ | TRUE | FALSE | NULL ->
       let v = parse_value state in
       Ret (Some v)
     | _ ->
       Ret None)
  
  | BR ->
    advance state;
    let cond = parse_value state in
    expect state COMMA;
    let then_label = parse_ident state in
    expect state COMMA;
    let else_label = parse_ident state in
    Br (cond, then_label, else_label)
  
  | JMP ->
    advance state;
    let label = parse_ident state in
    Jmp label
  
  | UNREACHABLE ->
    advance state;
    Unreachable
  
  | SWITCH ->
    advance state;
    let v = parse_value state in
    expect state COMMA;
    let default = parse_ident state in
    expect state LBRACKET;
    let cases = parse_switch_cases state in
    expect state RBRACKET;
    Switch (v, default, cases)
  
  | tok -> raise (ParseError (UnexpectedToken (tok, "terminator")))

and parse_switch_cases state =
  let rec loop acc =
    match current state with
    | RBRACKET ->
      List.rev acc
    | _ -> begin
      let const_val = parse_const_value state in
      expect state COLON;
      let label = parse_ident state in
      let acc' = (const_val, label) :: acc in
      match current state with
      | COMMA -> begin
        advance state;
        loop acc'
      end
      | _ ->
        List.rev acc'
    end
  in
  loop []

and parse_const_value state =
  match current state with
  | INT n ->
    advance state;
    ConstInt (n, I32) (* Default to i32 *)
  | FLOAT f ->
    advance state;
    ConstFloat (f, F64) (* Default to f64 *)
  | TRUE ->
    advance state;
    ConstBool true
  | FALSE ->
    advance state;
    ConstBool false
  | NULL ->
    advance state;
    ConstNull
  | tok -> raise (ParseError (UnexpectedToken (tok, "constant value")))

(* Block parsing *)
let parse_block_params state =
  match current state with
  | LPAREN -> begin
    advance state;
    let rec loop acc =
      match current state with
    | RPAREN -> begin
        advance state;
        List.rev acc
      end
      | _ -> begin
        let name = parse_ident state in
        expect state COLON;
        let ty = parse_type state in
        let acc' = (name, ty) :: acc in
        match current state with
      | COMMA -> begin
          advance state;
          loop acc'
        end
      | _ -> begin
          expect state RPAREN;
          List.rev acc'
        end
      end
    in
    loop []
  end
  | _ ->
    []

let parse_block state =
  let label = parse_label state in
  let params = parse_block_params state in
  
  (* Add block parameters to value table *)
  List.iter (fun (name, ty) ->
    ignore (define_value state name ty)
  ) params;
  
  (* Parse instructions *)
  let rec parse_instrs acc =
    match current state with
    | LABEL _ | RET | BR | JMP | UNREACHABLE | SWITCH | RBRACE | EOF ->
      List.rev acc
    | _ ->
      let instr = parse_instruction state in
      parse_instrs (instr :: acc)
  in
  let instructions = parse_instrs [] in
  
  (* Parse terminator *)
  let terminator = parse_terminator state in
  
  create_block label params instructions terminator

(* Function parsing *)
let parse_function_params state =
  expect state LPAREN;
  let rec loop acc =
    match current state with
    | RPAREN -> begin
      advance state;
      List.rev acc
    end
    | _ -> begin
      let name = parse_ident state in
      expect state COLON;
      let ty = parse_type state in
      let acc' = (name, ty) :: acc in
      match current state with
      | COMMA -> begin
        advance state;
        loop acc'
      end
      | _ -> begin
        expect state RPAREN;
        List.rev acc'
      end
    end
  in
  loop []

let parse_function state =
  expect state FUNC;
  let name = parse_ident state in
  state.current_func <- Some name;
  
  (* Clear value table for new function *)
  Hashtbl.clear state.values;
  Hashtbl.clear state.blocks;
  
  let params = parse_function_params state in
  
  (* Add parameters to value table *)
  List.iter (fun (pname, pty) ->
    ignore (define_value state pname pty)
  ) params;
  
  (* Parse return type *)
  let return_ty =
    match current state with
    | ARROW -> begin
      advance state;
      Some (parse_type state)
    end
    | _ ->
      None in
  
  (* Parse attributes *)
  let attrs = parse_attributes state in
  
  expect state LBRACE;
  
  (* Parse blocks *)
  let rec parse_blocks acc =
    match current state with
    | RBRACE -> begin
      advance state;
      List.rev acc
    end
    | _ ->
      let block = parse_block state in
      parse_blocks (block :: acc)
  in
  let blocks = parse_blocks [] in
  
  state.current_func <- None;
  let func = create_func name params return_ty blocks in
  { func with attrs = attrs }

(* Program parsing *)
let parse_program state =
  let rec loop acc =
    match current state with
    | EOF -> List.rev acc
    | FUNC ->
      let func = parse_function state in
      loop (func :: acc)
    | tok -> raise (ParseError (UnexpectedToken (tok, "function or EOF")))
  in
  loop []

(* Main parse function *)
let parse tokens =
  let state = create_parser tokens in
  parse_program state

(* Parse from string *)
let parse_string input =
  let tokens = tokenize input in
  parse tokens

(* Parse from file *)
let parse_file filename =
  let ic = open_in filename in
  let input = really_input_string ic (in_channel_length ic) in
  close_in ic;
  parse_string input