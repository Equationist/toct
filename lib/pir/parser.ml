(* PIR Parser - Conforming to portable_ir_spec.md *)

open Types
open Values  
open Instructions
open Module_ir
open Lexer

(* Parser state *)
type parser_state = {
  tokens: token list;
  mutable pos: int;
  (* Symbol tables *)
  types: (string, ty) Hashtbl.t;          (* Named types *)
  globals: (string, object_decl) Hashtbl.t; (* Global/const objects *)
  locals: (string, value) Hashtbl.t;       (* Function-local values *)
  blocks: (string, string) Hashtbl.t;      (* Block labels *)
}

(* Parser errors *)
type parse_error =
  | UnexpectedToken of token * string
  | UnexpectedEOF of string
  | UndefinedType of string
  | UndefinedValue of string
  | UndefinedBlock of string
  | DuplicateDefinition of string
  | TypeError of string
  | SyntaxError of string

exception ParseError of parse_error

let string_of_parse_error = function
  | UnexpectedToken (tok, expected) ->
    Printf.sprintf "Unexpected token %s, expected %s" (string_of_token tok) expected
  | UnexpectedEOF expected ->
    Printf.sprintf "Unexpected end of file, expected %s" expected
  | UndefinedType name ->
    Printf.sprintf "Undefined type: %s" name
  | UndefinedValue name ->
    Printf.sprintf "Undefined value: %s" name
  | UndefinedBlock name ->
    Printf.sprintf "Undefined block: %s" name
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
  types = Hashtbl.create 16;
  globals = Hashtbl.create 64;
  locals = Hashtbl.create 64;
  blocks = Hashtbl.create 16;
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

(* Parse helpers *)
let parse_ident state =
  match current state with
  | IDENT name -> advance state; name
  | tok -> raise (ParseError (UnexpectedToken (tok, "identifier")))

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
  | VECTYPE (n, scalar_str) ->
    advance state;
    let scalar_ty = match scalar_str with
      | "i1" -> Types.I1 | "i8" -> Types.I8 | "i16" -> Types.I16 
      | "i32" -> Types.I32 | "i64" -> Types.I64
      | "f32" -> Types.F32 | "f64" -> Types.F64
      | _ -> raise (ParseError (TypeError ("Unknown scalar type in vector: " ^ scalar_str)))
    in
    Vector (n, scalar_ty)
  | ARRAY ->
    advance state;
    expect state LBRACKET;
    let n = parse_int state in
    expect state RBRACKET;
    let elem_ty = parse_type state in
    Array (n, elem_ty)
  | STRUCT ->
    advance state;
    expect state LCHEVRON;
    let fields = parse_type_list state RCHEVRON in
    Struct fields
  | PACKED_STRUCT ->
    advance state;
    expect state LCHEVRON;
    let fields = parse_type_list state RCHEVRON in
    PackedStruct fields
  | IDENT name ->
    (* Lookup named type *)
    advance state;
    (try Hashtbl.find state.types name
     with Not_found -> raise (ParseError (UndefinedType name)))
  | tok -> raise (ParseError (UnexpectedToken (tok, "type")))

and parse_type_list state end_tok =
  let rec loop acc =
    if current state = end_tok then begin
      advance state;
      List.rev acc
    end else begin
      let ty = parse_type state in
      let acc' = ty :: acc in
      match current state with
      | COMMA -> advance state; loop acc'
      | tok when tok = end_tok -> advance state; List.rev acc'
      | tok -> raise (ParseError (UnexpectedToken (tok, "comma or " ^ string_of_token end_tok)))
    end
  in
  loop []

and parse_int state =
  match current state with
  | INT n -> advance state; Int64.to_int n
  | tok -> raise (ParseError (UnexpectedToken (tok, "integer")))

(* Parse comma-separated list of integers in brackets *)
and parse_int_list state =
  expect state LBRACKET;
  let rec loop acc =
    if current state = RBRACKET then
      List.rev acc
    else
      let n = parse_int state in
      if current state = COMMA then begin
        advance state;
        loop (n :: acc)
      end else
        List.rev (n :: acc)
  in
  let result = loop [] in
  expect state RBRACKET;
  result

(* Parse constant expression for initializers *)
let rec parse_const_expr state =
  match current state with
  | INT n when n = 0L && (peek state 1 = EOF || peek state 1 = COMMA) ->
    (* Zero initializer shorthand *)
    advance state;
    ConstZero
  | INT n -> 
    advance state;
    ConstValue (ConstInt (n, I32)) (* Default to i32 *)
  | FLOAT f ->
    advance state;
    ConstValue (ConstFloat (f, F64)) (* Default to f64 *)
  | TRUE ->
    advance state;
    ConstValue (ConstBool true)
  | FALSE ->
    advance state;
    ConstValue (ConstBool false)
  | NULL ->
    advance state;
    ConstValue ConstNull
  | LCHEVRON ->
    (* Aggregate initializer *)
    advance state;
    let elems = parse_const_expr_list state in
    ConstAggregate elems
  | tok -> raise (ParseError (UnexpectedToken (tok, "constant expression")))

and parse_const_expr_list state =
  let rec loop acc =
    if current state = RCHEVRON then begin
      advance state;
      List.rev acc
    end else begin
      let expr = parse_const_expr state in
      let acc' = expr :: acc in
      match current state with
      | COMMA -> advance state; loop acc'
      | RCHEVRON -> advance state; List.rev acc'
      | tok -> raise (ParseError (UnexpectedToken (tok, "comma or >>")))
    end
  in
  loop []

(* Parse optional alignment *)
let parse_opt_align state =
  if current state = ALIGN then begin
    advance state;
    Some (parse_int state)
  end else
    None

(* Parse type declaration *)
let parse_type_decl state =
  expect state TYPE;
  let name = parse_ident state in
  expect state EQUALS;
  let ty = parse_type state in
  (* Register type *)
  if Hashtbl.mem state.types name then
    raise (ParseError (DuplicateDefinition name));
  Hashtbl.add state.types name ty;
  TypeDecl (create_type_decl name ty)

(* Parse attributes @{...} *)
let parse_attributes state =
  if current state = AT then begin
    advance state;
    expect state LBRACE;
    (* For now, skip attribute parsing - just consume until closing brace *)
    let rec skip_until_rbrace () =
      match current state with
      | RBRACE -> advance state
      | EOF -> raise (ParseError (UnexpectedEOF "attribute closing"))
      | _ -> advance state; skip_until_rbrace ()
    in
    skip_until_rbrace ();
    Attributes.empty ()
  end else
    Attributes.empty ()

(* Parse object declaration *)
let parse_object_decl state is_const =
  expect state (if is_const then CONST else GLOBAL);
  let name = parse_ident state in
  expect state COLON;
  let ty = parse_type state in
  let align = parse_opt_align state in
  expect state INIT;
  let init = parse_const_expr state in
  let attrs = parse_attributes state in
  let obj = create_object_decl ~is_const name ty ?align init attrs in
  (* Register global *)
  if Hashtbl.mem state.globals name then
    raise (ParseError (DuplicateDefinition name));
  Hashtbl.add state.globals name obj;
  ObjectDecl obj

(* Parse label *)
let parse_label state =
  let name = parse_ident state in
  expect state COLON;
  name

(* Value parsing *)
let parse_value state =
  match current state with
  | IDENT name ->
    advance state;
    (* Look up in locals first, then globals *)
    (try Hashtbl.find state.locals name
     with Not_found ->
       try
         let obj = Hashtbl.find state.globals name in
         create_simple_value obj.obj_ty (* Global reference *)
       with Not_found ->
         (* Could be a function name - create a dummy value for now *)
         create_simple_value Ptr)
  | INT _n ->
    advance state;
    create_simple_value (Scalar I32) (* Default to i32 *)
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
    create_simple_value (Scalar I32) (* Need type context *)
  | tok -> raise (ParseError (UnexpectedToken (tok, "value")))

(* Define a new local value *)
let define_value state name ty =
  if Hashtbl.mem state.locals name then
    raise (ParseError (DuplicateDefinition name));
  let value = create_simple_value ty in
  Hashtbl.add state.locals name value;
  value

(* Parse instruction with spec syntax: result = opcode.flag.type operands *)
let rec parse_instruction state =
  (* Check for result assignment *)
  let result_name = 
    match current state, peek state 1 with
    | IDENT name, EQUALS ->
      advance state; (* name *)
      advance state; (* = *)
      Some name
    | _ -> None in
  
  (* Parse opcode *)
  let opcode = current state in
  advance state;
  
  (* Handle instruction-specific parsing based on opcode - some have flags, some have type suffixes *)
  let flag, _ty_suffix = match opcode with
    (* Binary operations can have flags and/or type suffix *)
    | ADD | SUB | MUL | SDIV | UDIV | SREM | UREM
    | AND | OR | XOR | SHL | LSHR | ASHR | ROL | ROR ->
      let flag = 
        if current state = DOT && 
           (peek state 1 = NSW || peek state 1 = CARRY || peek state 1 = SAT) then begin
          advance state;
          match current state with
          | NSW -> advance state; Nsw
          | CARRY -> advance state; Carry
          | SAT -> advance state; Sat
          | _ -> NoFlag
        end else NoFlag in
      let ty_suffix = 
        if current state = DOT then begin
          advance state;
          Some (parse_type state)
        end else None in
      (flag, ty_suffix)
    (* Other instructions handle their own DOT and type parsing *)
    | _ -> (NoFlag, None) in
  
  (* Parse instruction based on opcode *)
  let instr = match opcode with
    (* Binary operations *)
    | ADD | SUB | MUL | SDIV | UDIV | SREM | UREM
    | AND | OR | XOR | SHL | LSHR | ASHR | ROL | ROR ->
      let v1 = parse_value state in
      expect state COMMA;
      let v2 = parse_value state in
      let binop = match opcode with
        | ADD -> Add | SUB -> Sub | MUL -> Mul
        | SDIV -> Sdiv | UDIV -> Udiv | SREM -> Srem | UREM -> Urem
        | AND -> And | OR -> Or | XOR -> Xor
        | SHL -> Shl | LSHR -> Lshr | ASHR -> Ashr
        | ROL -> Rol | ROR -> Ror
        | _ -> failwith "Unreachable"
      in
      Binop (binop, flag, v1, v2)
    
    (* Float operations *)
    | FADD | FSUB | FMUL | FDIV | FREM ->
      let v1 = parse_value state in
      expect state COMMA;
      let v2 = parse_value state in
      let binop = match opcode with
        | FADD -> Fadd | FSUB -> Fsub | FMUL -> Fmul
        | FDIV -> Fdiv | FREM -> Frem
        | _ -> failwith "Unreachable"
      in
      Binop (binop, NoFlag, v1, v2)
    
    (* Comparisons *)
    | ICMP ->
      expect state DOT;
      let pred_tok = current state in
      advance state;
      let pred = parse_icmp_pred pred_tok in
      expect state DOT;
      let _ty = parse_type state in (* Type suffix *)
      let v1 = parse_value state in
      expect state COMMA;
      let v2 = parse_value state in
      Icmp (pred, v1, v2)
    
    | FCMP ->
      expect state DOT;
      let pred = parse_fcmp_pred (current state) in
      advance state;
      expect state DOT;
      let _ty = parse_type state in
      let v1 = parse_value state in
      expect state COMMA;
      let v2 = parse_value state in
      Fcmp (pred, v1, v2)
    
    (* Memory operations *)
    | LOAD ->
      expect state DOT;
      let ty = parse_type state in
      expect state LBRACKET;
      let _ptr = parse_value state in
      expect state RBRACKET;
      Memory (Load ty)
    
    | STORE ->
      expect state DOT;
      let _ty = parse_type state in
      let value = parse_value state in
      expect state COMMA;
      expect state LBRACKET;
      let ptr = parse_value state in
      expect state RBRACKET;
      Memory (Store (value, ptr))
    
    | ALLOCA ->
      let size = parse_value state in
      expect state ALIGN;
      let align = parse_int state in
      Memory (Alloca (size, align))
    
    (* Address operations *)
    | GEP ->
      let base = parse_value state in
      expect state COMMA;
      let idx = parse_value state in
      Address (Gep (base, idx))
    
    | FIELDADDR ->
      let base = parse_value state in
      expect state COMMA;
      let idx = parse_int state in
      Address (FieldAddr (base, idx))
    
    | PTRADD ->
      let base = parse_value state in
      expect state COMMA;
      let offset = parse_value state in
      Address (PtrAdd (base, offset))
    
    (* Bitcast *)
    | BITCAST ->
      let v = parse_value state in
      Cast (Bitcast v)
    
    (* Other casts *)
    | TRUNC | ZEXT | SEXT | FPTRUNC | FPEXT
    | FPTOUI | FPTOSI | UITOFP | SITOFP ->
      let v = parse_value state in
      expect state TO;
      let target_ty = parse_type state in
      let cast_op = match opcode with
        | TRUNC -> Trunc (v, target_ty)
        | ZEXT -> Zext (v, target_ty)
        | SEXT -> Sext (v, target_ty)
        | FPTRUNC -> Fptrunc (v, target_ty)
        | FPEXT -> Fpext (v, target_ty)
        | FPTOUI -> Fptoui (v, target_ty)
        | FPTOSI -> Fptosi (v, target_ty)
        | UITOFP -> Uitofp (v, target_ty)
        | SITOFP -> Sitofp (v, target_ty)
        | _ -> failwith "Unreachable"
      in
      Cast cast_op
    
    (* Call *)
    | CALL ->
      expect state DOT;
      let _ret_ty = 
        if current state = VOID then begin
          advance state;
          None
        end else
          Some (parse_type state) in
      let callee = parse_value state in
      expect state COMMA;
      let args = parse_value_list state in
      Call (Call (callee, args))
    
    | TAILCALL ->
      expect state DOT;
      let _ret_ty = 
        if current state = VOID then begin
          advance state;
          None
        end else
          Some (parse_type state) in
      let callee = parse_value state in
      expect state COMMA;
      let args = parse_value_list state in
      Call (TailCall (callee, args))
    
    (* Select *)
    | SELECT ->
      let cond = parse_value state in
      expect state COMMA;
      let v_true = parse_value state in
      expect state COMMA;
      let v_false = parse_value state in
      Select (cond, v_true, v_false)
    
    (* Vector operations *)
    | SPLAT ->
      let scalar = parse_value state in
      expect state COMMA;
      let count = parse_int state in
      Vector (Splat (scalar, count))
    
    | SHUFFLE ->
      let v1 = parse_value state in
      expect state COMMA;
      let v2 = parse_value state in
      expect state COMMA;
      let mask = parse_int_list state in
      Vector (Shuffle (v1, v2, mask))
    
    | EXTRACTLANE ->
      let vector = parse_value state in
      expect state COMMA;
      let idx = parse_int state in
      Vector (ExtractLane (vector, idx))
    
    | INSERTLANE ->
      let vector = parse_value state in
      expect state COMMA;
      let idx = parse_int state in
      expect state COMMA;
      let value = parse_value state in
      Vector (InsertLane (vector, idx, value))
    
    | _ -> raise (ParseError (UnexpectedToken (opcode, "instruction opcode")))
  in
  
  (* Parse attributes *)
  let attrs = parse_attributes state in
  
  (* Create instruction with result if needed *)
  let result_value = match result_name with
    | Some name ->
      (match result_type_of_instr instr with
       | Some ty -> Some (define_value state name ty)
       | None -> 
         (* Special case for call instructions - they can have results *)
         match instr with
         | Call _ -> Some (define_value state name Ptr) (* Dummy type for now *)
         | _ -> raise (ParseError (TypeError "Instruction does not produce a result")))
    | None -> None in
  
  create_instruction ?result:result_value instr attrs

and parse_icmp_pred = function
  | EQ -> Eq | NE -> Ne 
  | LT -> Slt | LE -> Sle | GT -> Sgt | GE -> Sge
  | tok -> raise (ParseError (UnexpectedToken (tok, "icmp predicate")))

and parse_fcmp_pred = function
  | OEQ -> Oeq | OGT -> Ogt | OGE -> Oge | OLT -> Olt | OLE -> Ole
  | ONE -> One | ORD -> Ord | UEQ -> Ueq | UNE -> Une | UNO -> Uno
  (* Handle 'one' and 'ord' as identifiers that get converted to predicates *)
  | IDENT "one" -> One | IDENT "ord" -> Ord
  | tok -> raise (ParseError (UnexpectedToken (tok, "fcmp predicate")))

and parse_value_list state =
  let rec loop acc =
    match current state with
    | EOF | RET | BR | JMP | UNREACHABLE | SWITCH ->
      List.rev acc
    | _ ->
      let v = parse_value state in
      let acc' = v :: acc in
      match current state with
      | COMMA -> advance state; loop acc'
      | _ -> List.rev acc'
  in
  loop []

(* Parse terminator *)
and parse_terminator state =
  match current state with
  | RET ->
    advance state;
    (match current state with
     | EOF | ENDFUNC ->
       Ret None
     | IDENT _ when peek state 1 = COLON ->
       (* Next line is a label *)
       Ret None
     | _ ->
       let v = parse_value state in
       Ret (Some v))
  
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
  
  | SWITCH ->
    advance state;
    let v = parse_value state in
    expect state COMMA;
    let default = parse_ident state in
    expect state COMMA;
    expect state LBRACKET;
    let cases = parse_switch_cases state in
    expect state RBRACKET;
    Switch (v, default, cases)
  
  | UNREACHABLE ->
    advance state;
    Unreachable
  
  | tok -> raise (ParseError (UnexpectedToken (tok, "terminator")))

and parse_switch_cases state =
  let rec loop acc =
    match current state with
    | RBRACKET -> List.rev acc
    | _ ->
      expect state LPAREN;
      let const_val = parse_const_value state in
      expect state COMMA;
      let label = parse_ident state in
      expect state RPAREN;
      let acc' = (const_val, label) :: acc in
      match current state with
      | COMMA -> advance state; loop acc'
      | _ -> List.rev acc'
  in
  loop []

and parse_const_value state =
  match current state with
  | INT n -> advance state; ConstInt (n, I32)
  | FLOAT f -> advance state; ConstFloat (f, F64)
  | TRUE -> advance state; ConstBool true
  | FALSE -> advance state; ConstBool false
  | NULL -> advance state; ConstNull
  | tok -> raise (ParseError (UnexpectedToken (tok, "constant value")))

(* Parse basic block *)
let parse_block state =
  let label = parse_label state in
  
  (* Register block *)
  if Hashtbl.mem state.blocks label then
    raise (ParseError (DuplicateDefinition label));
  Hashtbl.add state.blocks label label;
  
  (* Parse instructions until terminator *)
  let rec parse_instrs acc =
    match current state with
    | RET | BR | JMP | UNREACHABLE | SWITCH ->
      List.rev acc
    | _ ->
      let instr = parse_instruction state in
      parse_instrs (instr :: acc)
  in
  let instructions = parse_instrs [] in
  
  (* Parse terminator *)
  let terminator = parse_terminator state in
  
  (* Parse optional attributes *)
  let attrs = parse_attributes state in
  
  { label; params = []; instructions; terminator; attrs }

(* Parse function *)
let rec parse_function state =
  expect state FUNC;
  let name = parse_ident state in
  
  (* Clear local state for new function *)
  Hashtbl.clear state.locals;
  Hashtbl.clear state.blocks;
  
  (* Parse parameters *)
  expect state LPAREN;
  let params = parse_param_list state in
  expect state RPAREN;
  
  (* Add parameters to locals *)
  List.iter (fun (pname, pty) ->
    ignore (define_value state pname pty)
  ) params;
  
  (* Parse return type *)
  let return_ty =
    if current state = ARROW then begin
      advance state;
      match current state with
      | VOID -> 
        advance state;
        None
      | _ ->
        Some (parse_type state)
    end else
      None in
  
  (* Parse optional attributes *)
  let attrs = parse_attributes state in
  
  (* Parse blocks until endfunc *)
  let rec parse_blocks acc =
    match current state with
    | ENDFUNC ->
      advance state;
      List.rev acc
    | _ ->
      let block = parse_block state in
      parse_blocks (block :: acc)
  in
  let blocks = parse_blocks [] in
  
  FuncDecl { name; params; return_ty; blocks; attrs }

and parse_param_list state =
  let rec loop acc =
    match current state with
    | RPAREN -> List.rev acc
    | _ ->
      let name = parse_ident state in
      expect state COLON;
      let ty = parse_type state in
      let acc' = (name, ty) :: acc in
      match current state with
      | COMMA -> advance state; loop acc'
      | _ -> List.rev acc'
  in
  loop []

(* Parse module *)
let parse_module state =
  let rec loop acc =
    match current state with
    | EOF -> List.rev acc
    | TYPE -> 
      let item = parse_type_decl state in
      loop (item :: acc)
    | GLOBAL ->
      let item = parse_object_decl state false in
      loop (item :: acc)
    | CONST ->
      let item = parse_object_decl state true in
      loop (item :: acc)
    | FUNC ->
      let item = parse_function state in
      loop (item :: acc)
    | tok -> raise (ParseError (UnexpectedToken (tok, "top-level declaration")))
  in
  let items = loop [] in
  create_module items (Attributes.empty ())

(* Main parse function *)
let parse tokens =
  let state = create_parser tokens in
  parse_module state

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