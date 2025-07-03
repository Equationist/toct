(** C89/90 Parser Implementation
    
    This module implements a recursive descent parser for C89.
    It follows the grammar from the ANSI C specification.
    This version uses the shared frontend infrastructure.
*)

open Ast
open Lexer

(** Parser state *)
type parser_state = {
  tokens: located_token array;
  mutable pos: int;
  symbol_table: C_symbol_table.t;
  error_reporter: C_errors.t;
  mutable error_recovery: bool;
}

(** Create parser state from token list *)
let create_parser tokens =
  { 
    tokens = Array.of_list tokens; 
    pos = 0; 
    symbol_table = C_symbol_table.create ();
    error_reporter = C_errors.create ();
    error_recovery = false;
  }

(** Peek at current token *)
let peek state =
  if state.pos < Array.length state.tokens then
    state.tokens.(state.pos)
  else
    state.tokens.(Array.length state.tokens - 1)  (* EOF *)

(** Peek at token n positions ahead *)
let peek_n state n =
  let pos = state.pos + n in
  if pos < Array.length state.tokens then
    state.tokens.(pos)
  else
    state.tokens.(Array.length state.tokens - 1)

(** Advance to next token *)
let advance state =
  if state.pos < Array.length state.tokens then
    state.pos <- state.pos + 1

(** Check if current token matches *)
let check state tok =
  (peek state).token = tok

(** Consume token if it matches *)
let consume state tok =
  if check state tok then begin
    advance state;
    true
  end else
    false

(** Expect and consume token *)
let expect state tok =
  let curr = peek state in
  if curr.token = tok then
    advance state
  else begin
    C_errors.parse_error_token state.error_reporter curr.loc 
      (token_to_string tok) curr.token;
    if not state.error_recovery then
      failwith (Printf.sprintf "Expected %s but got %s at %d:%d"
        (token_to_string tok)
        (token_to_string curr.token)
        curr.loc.line curr.loc.column)
  end

(** Parse error with location *)
let parse_error state msg =
  let tok = peek state in
  C_errors.parse_error state.error_reporter tok.loc msg;
  if not state.error_recovery then
    failwith (Printf.sprintf "%s at %d:%d (token: %s)"
      msg tok.loc.line tok.loc.column (token_to_string tok.token))


(** Forward declarations for mutual recursion *)
let expr_ref : (parser_state -> expr) ref = ref (fun _ -> assert false)
let stmt_ref : (parser_state -> stmt) ref = ref (fun _ -> assert false)
let decl_ref : (parser_state -> declaration) ref = ref (fun _ -> assert false)
let type_spec_ref : (parser_state -> type_spec list * type_qualifier list) ref = 
  ref (fun _ -> assert false)
let declarator_ref : (parser_state -> declarator) ref = ref (fun _ -> assert false)

(** Parse helpers *)
let parse_expr state = !expr_ref state
let parse_stmt state = !stmt_ref state
let parse_decl state = !decl_ref state
let parse_type_specs state = !type_spec_ref state
let parse_declarator state = !declarator_ref state

(** Forward declaration for abstract declarator *)
let abstract_declarator_ref : (parser_state -> declarator) ref = ref (fun _ -> assert false)
let parse_abstract_declarator state = !abstract_declarator_ref state

(** Forward declaration for assignment expression *)
let assignment_expr_ref : (parser_state -> expr) ref = ref (fun _ -> assert false)
let parse_assignment_expr state = !assignment_expr_ref state

(** Check if name is a typedef *)
let is_typedef_name state name =
  C_symbol_table.is_typedef state.symbol_table name

(** Parse storage class specifier *)
let parse_storage_class state =
  match (peek state).token with
  | Auto -> advance state; Some Ast.Auto
  | Register -> advance state; Some Ast.Register
  | Static -> advance state; Some Ast.Static
  | Extern -> advance state; Some Ast.Extern
  | Typedef -> advance state; Some Ast.Typedef
  | _ -> None

(** Parse type qualifier *)
let parse_type_qualifier state =
  match (peek state).token with
  | Const -> advance state; Some Ast.Const
  | Volatile -> advance state; Some Ast.Volatile
  | _ -> None

(** Parse struct or union specifier *)
let rec parse_struct_or_union state =
  let is_struct = check state Struct in
  advance state;  (* Skip struct/union *)
  
  let tag = 
    match (peek state).token with
    | Identifier name -> advance state; Some name
    | _ -> None
  in
  
  let decls =
    if check state LeftBrace then begin
      advance state;  (* Skip { *)
      let rec parse_members acc =
        if check state RightBrace then
          List.rev acc
        else
          let spec_quals = parse_type_specs state in
          let declarators = parse_struct_declarator_list state in
          expect state Semicolon;
          let decl = { Ast.spec_quals; Ast.declarators } in
          parse_members (decl :: acc)
      in
      let members = parse_members [] in
      expect state RightBrace;
      Some members
    end else
      None
  in
  
  if is_struct then
    Ast.StructType (tag, decls)
  else
    Ast.UnionType (tag, decls)

and parse_struct_declarator_list state =
  let rec parse_list acc =
    let decl =
      if check state Colon then begin
        (* Bit field without declarator *)
        advance state;
        let width = parse_expr state in
        Ast.BitField (None, width)
      end else begin
        let d = parse_declarator state in
        if check state Colon then begin
          advance state;
          let width = parse_expr state in
          Ast.BitField (Some d, width)
        end else
          Ast.StructDecl d
      end
    in
    if consume state Comma then
      parse_list (decl :: acc)
    else
      List.rev (decl :: acc)
  in
  parse_list []

(** Parse enum specifier *)
let parse_enum state =
  advance state;  (* Skip enum *)
  
  let tag =
    match (peek state).token with
    | Identifier name -> advance state; Some name
    | _ -> None
  in
  
  let items =
    if check state LeftBrace then begin
      advance state;
      let rec parse_items acc =
        if check state RightBrace then
          List.rev acc
        else
          match (peek state).token with
          | Identifier name ->
              advance state;
              let value =
                if consume state Equal then
                  Some (parse_expr state)
                else
                  None
              in
              let item = (name, value) in
              if consume state Comma then
                parse_items (item :: acc)
              else
                List.rev (item :: acc)
          | _ -> 
              parse_error state "Expected identifier in enum";
              List.rev acc  (* Return accumulated items on error *)
      in
      let items = parse_items [] in
      expect state RightBrace;
      Some items
    end else
      None
  in
  
  Ast.EnumType (tag, items)

(** Parse type specifier *)
let parse_type_specifier state =
  match (peek state).token with
  | Void -> advance state; Some Ast.Void
  | Char -> advance state; Some Ast.Char
  | Short -> advance state; Some Ast.Short
  | Int -> advance state; Some Ast.Int
  | Long -> advance state; Some Ast.Long
  | Float -> advance state; Some Ast.Float
  | Double -> advance state; Some Ast.Double
  | Signed -> advance state; Some Ast.Signed
  | Unsigned -> advance state; Some Ast.Unsigned
  | Struct -> Some (parse_struct_or_union state)
  | Union -> Some (parse_struct_or_union state)
  | Enum -> Some (parse_enum state)
  | Identifier name when is_typedef_name state name -> 
      advance state; Some (TypeName name)
  | _ -> None


(** Parse declaration specifiers *)
let parse_decl_specifiers state =
  let storage = ref [] in
  let specs = ref [] in
  let quals = ref [] in
  
  let rec parse () =
    let found = ref false in
    
    (* Try storage class *)
    (match parse_storage_class state with
     | Some sc -> storage := sc :: !storage; found := true
     | None -> ());
    
    (* Try type qualifier *)
    (match parse_type_qualifier state with
     | Some tq -> quals := tq :: !quals; found := true
     | None -> ());
    
    (* Try type specifier *)
    (match parse_type_specifier state with
     | Some ts -> specs := ts :: !specs; found := true
     | None -> ());
    
    if !found then parse ()
  in
  
  parse ();
  (List.rev !storage, List.rev !specs, List.rev !quals)

(** Implementation of parse_type_specs *)
let parse_type_specs_impl state =
  let specs = ref [] in
  let quals = ref [] in
  
  let rec parse () =
    let found = ref false in
    
    (* Try type qualifier *)
    (match parse_type_qualifier state with
     | Some tq -> quals := tq :: !quals; found := true
     | None -> ());
    
    (* Try type specifier *)
    (match parse_type_specifier state with
     | Some ts -> specs := ts :: !specs; found := true
     | None -> ());
    
    if !found then parse ()
  in
  
  parse ();
  (List.rev !specs, List.rev !quals)

(** Parse pointer *)
let parse_pointer state =
  let rec parse_ptr decl =
    if consume state Star then begin
      let quals = ref [] in
      let rec parse_quals () =
        match parse_type_qualifier state with
        | Some q -> quals := q :: !quals; parse_quals ()
        | None -> ()
      in
      parse_quals ();
      parse_ptr (Ast.PointerDecl (List.rev !quals, decl))
    end else
      decl
  in
  parse_ptr

(** Parse direct declarator *)
let rec parse_direct_declarator state : Ast.direct_declarator =
  let base : Ast.direct_declarator =
    match (peek state).token with
    | Identifier name ->
        advance state;
        Ast.Ident name
    | LeftParen when is_declarator_start (peek_n state 1) ->
        advance state;
        let decl = parse_declarator state in
        expect state RightParen;
        Ast.ParenDecl decl
    | _ -> 
        parse_error state "Expected identifier or (";
        Ast.Ident ""  (* Return dummy identifier *)
  in
  
  (* Parse suffixes *)
  let rec parse_suffix decl =
    match (peek state).token with
    | LeftBracket ->
        advance state;
        let size =
          if check state RightBracket then None
          else Some (parse_expr state)
        in
        expect state RightBracket;
        parse_suffix (Ast.ArrayDecl (decl, size))
    | LeftParen ->
        advance state;
        let params = parse_parameter_list state in
        expect state RightParen;
        parse_suffix (Ast.FuncDecl (decl, params))
    | _ -> decl
  in
  
  parse_suffix base

(** Parse abstract direct declarator *)
and parse_abstract_direct_declarator state : Ast.direct_declarator option =
  let base =
    match (peek state).token with
    | LeftParen when is_abstract_declarator_start (peek_n state 1) ->
        advance state;
        let decl = parse_abstract_declarator state in
        expect state RightParen;
        Some (Ast.ParenDecl decl)
    | LeftBracket | LeftParen ->
        None  (* No base, just suffixes *)
    | _ -> None
  in
  
  (* Parse suffixes *)
  let rec parse_suffix (decl_opt : Ast.direct_declarator option) =
    match (peek state).token with
    | LeftBracket ->
        advance state;
        let size =
          if check state RightBracket then None
          else Some (parse_expr state)
        in
        expect state RightBracket;
        let decl = match decl_opt with
          | Some d -> d
          | None -> Ast.Ident ""  (* Empty identifier for abstract declarator *)
        in
        parse_suffix (Some (Ast.ArrayDecl (decl, size) : Ast.direct_declarator))
    | LeftParen when (peek_n state 1).token <> RightParen ->
        advance state;
        let params = parse_parameter_list state in
        expect state RightParen;
        let decl = match decl_opt with
          | Some d -> d
          | None -> Ast.Ident ""
        in
        parse_suffix (Some (Ast.FuncDecl (decl, params) : Ast.direct_declarator))
    | _ -> decl_opt
  in
  
  parse_suffix base

and is_abstract_declarator_start tok =
  match tok.token with
  | Star | LeftParen | LeftBracket -> true
  | _ -> false

and is_declarator_start tok =
  match tok.token with
  | Star | Identifier _ | LeftParen -> true
  | _ -> false

(** Parse parameter list *)
and parse_parameter_list state =
  if check state RightParen then
    None
  else if check state Void && (peek_n state 1).token = RightParen then begin
    advance state;  (* Skip void *)
    None
  end else begin
    let params = ref [] in
    let has_ellipsis = ref false in
    
    let rec parse_params () =
      if check state Ellipsis then begin
        advance state;
        has_ellipsis := true
      end else begin
        let (storage, specs, quals) = parse_decl_specifiers state in
        let _ = storage in  (* Ignore storage class in params *)
        let decl =
          if is_declarator_start (peek state) then
            Some (parse_declarator state)
          else
            None
        in
        params := { Ast.param_specs = specs; Ast.param_quals = quals; Ast.param_decl = decl } :: !params;
        
        if consume state Comma then
          parse_params ()
      end
    in
    
    parse_params ();
    Some (Ast.ParamList (List.rev !params, !has_ellipsis))
  end

(** Implementation of parse_declarator *)
let parse_declarator_impl state =
  let ptr_decl = parse_pointer state (Ast.DirectDecl (Ast.Ident "")) in
  match ptr_decl with
  | Ast.DirectDecl (Ast.Ident "") -> Ast.DirectDecl (parse_direct_declarator state)
  | Ast.PointerDecl (quals, Ast.DirectDecl (Ast.Ident "")) ->
      Ast.PointerDecl (quals, Ast.DirectDecl (parse_direct_declarator state))
  | _ -> 
      if is_declarator_start (peek state) then
        parse_pointer state (Ast.DirectDecl (parse_direct_declarator state))
      else
        ptr_decl

(** Implementation of parse_abstract_declarator *)
let parse_abstract_declarator_impl state =
  let ptr_decl = parse_pointer state (Ast.DirectDecl (Ast.Ident "")) in
  match ptr_decl with
  | Ast.DirectDecl (Ast.Ident "") ->
      (* No pointer, check for abstract direct declarator *)
      (match parse_abstract_direct_declarator state with
       | Some dd -> Ast.DirectDecl dd
       | None -> Ast.DirectDecl (Ast.Ident ""))  (* Empty abstract declarator *)
  | Ast.PointerDecl (quals, Ast.DirectDecl (Ast.Ident "")) ->
      (* Pointer with possible abstract direct declarator *)
      (match parse_abstract_direct_declarator state with
       | Some dd -> Ast.PointerDecl (quals, Ast.DirectDecl dd)
       | None -> Ast.PointerDecl (quals, Ast.DirectDecl (Ast.Ident "")))
  | _ -> ptr_decl

(** Parse initializer *)
let rec parse_initializer state =
  if check state LeftBrace then begin
    advance state;
    let items = parse_initializer_list state in
    expect state RightBrace;
    Ast.ListInit items
  end else
    Ast.ExprInit (parse_assignment_expr state)

and parse_initializer_list state =
  let rec parse_list acc =
    if check state RightBrace then
      List.rev acc
    else
      let init =
        if check state LeftBracket || check state Dot then
          parse_designated_initializer state
        else
          Ast.SimpleInit (parse_initializer state)
      in
      if consume state Comma then
        parse_list (init :: acc)
      else
        List.rev (init :: acc)
  in
  parse_list []

and parse_designated_initializer state =
  let designators = ref [] in
  
  let rec parse_designators () =
    match (peek state).token with
    | LeftBracket ->
        advance state;
        let idx = parse_expr state in
        expect state RightBracket;
        designators := Ast.ArrayDesignator idx :: !designators;
        parse_designators ()
    | Dot ->
        advance state;
        (match (peek state).token with
         | Identifier name ->
             advance state;
             designators := Ast.MemberDesignator name :: !designators;
             parse_designators ()
         | _ -> 
             parse_error state "Expected identifier after .";
             parse_designators ())
    | _ -> ()
  in
  
  parse_designators ();
  expect state Equal;
  let init = parse_initializer state in
  Ast.DesignatedInit (List.rev !designators, init)

(** Parse primary expression *)
let rec parse_primary_expr state =
  match (peek state).token with
  | IntConstant (v, s) -> advance state; Ast.IntLit (v, s)
  | FloatConstant (v, s) -> advance state; Ast.FloatLit (v, s)
  | CharConstant c -> advance state; Ast.CharLit c
  | StringLiteral s -> advance state; Ast.StringLit s
  | Identifier name -> advance state; Ast.Ident name
  | LeftParen ->
      advance state;
      let e = parse_expr state in
      expect state RightParen;
      e
  | _ -> 
      parse_error state "Expected primary expression";
      Ast.Ident ""  (* Return dummy identifier *)

(** Parse postfix expression *)
and parse_postfix_expr state =
  let rec parse_suffix expr =
    match (peek state).token with
    | LeftBracket ->
        advance state;
        let idx = parse_expr state in
        expect state RightBracket;
        parse_suffix (Ast.ArrayRef (expr, idx))
    | LeftParen ->
        advance state;
        let args = parse_arg_list state in
        expect state RightParen;
        parse_suffix (Ast.FuncCall (expr, args))
    | Dot ->
        advance state;
        (match (peek state).token with
         | Identifier name ->
             advance state;
             parse_suffix (Ast.Member (expr, name))
         | _ -> 
             parse_error state "Expected identifier after .";
             expr)
    | Arrow ->
        advance state;
        (match (peek state).token with
         | Identifier name ->
             advance state;
             parse_suffix (Ast.PtrMember (expr, name))
         | _ -> 
             parse_error state "Expected identifier after ->";
             expr)
    | PlusPlus ->
        advance state;
        parse_suffix (Ast.UnOp (Ast.PostInc, expr))
    | MinusMinus ->
        advance state;
        parse_suffix (Ast.UnOp (Ast.PostDec, expr))
    | _ -> expr
  in
  parse_suffix (parse_primary_expr state)

and parse_arg_list state =
  if check state RightParen then
    []
  else
    let rec parse_args acc =
      let arg = parse_assignment_expr state in
      if consume state Comma then
        parse_args (arg :: acc)
      else
        List.rev (arg :: acc)
    in
    parse_args []

(** Parse unary expression *)
and parse_unary_expr state =
  match (peek state).token with
  | PlusPlus -> advance state; Ast.UnOp (Ast.PreInc, parse_unary_expr state)
  | MinusMinus -> advance state; Ast.UnOp (Ast.PreDec, parse_unary_expr state)
  | Plus -> advance state; Ast.UnOp (Ast.Plus, parse_cast_expr state)
  | Minus -> advance state; Ast.UnOp (Ast.Minus, parse_cast_expr state)
  | Bang -> advance state; Ast.UnOp (Ast.Not, parse_cast_expr state)
  | Tilde -> advance state; Ast.UnOp (Ast.BitNot, parse_cast_expr state)
  | Star -> advance state; Ast.UnOp (Ast.Deref, parse_cast_expr state)
  | Ampersand -> advance state; Ast.UnOp (Ast.AddrOf, parse_cast_expr state)
  | Sizeof ->
      advance state;
      if check state LeftParen && is_type_name state (peek_n state 1) then begin
        advance state;  (* Skip ( *)
        let (specs, quals) = parse_type_specs state in
        let decl = 
          if is_abstract_declarator_start (peek state) || check state RightParen then
            parse_abstract_declarator state
          else
            parse_declarator state
        in
        expect state RightParen;
        Ast.SizeofType { specs; quals; decl }
      end else
        Ast.SizeofExpr (parse_unary_expr state)
  | _ -> parse_postfix_expr state

and is_type_name state tok =
  match tok.token with
  | Void | Char | Short | Int | Long | Float | Double
  | Signed | Unsigned | Struct | Union | Enum | Const | Volatile -> true
  | Identifier name -> is_typedef_name state name
  | _ -> false

(** Parse cast expression *)
and parse_cast_expr state =
  if check state LeftParen && is_type_name state (peek_n state 1) then begin
    advance state;  (* Skip ( *)
    let (specs, quals) = parse_type_specs state in
    let decl = 
      if is_abstract_declarator_start (peek state) || check state RightParen then
        parse_abstract_declarator state
      else
        parse_declarator state
    in
    expect state RightParen;
    let expr = parse_cast_expr state in
    Ast.Cast ({ specs; quals; decl }, expr)
  end else
    parse_unary_expr state

(** Parse binary expressions with precedence *)
and parse_binary_expr state min_prec =
  let get_prec = function
    | Comma -> 1
    | Equal | PlusEqual | MinusEqual | StarEqual | SlashEqual
    | PercentEqual | LeftShiftEqual | RightShiftEqual
    | AmpEqual | CaretEqual | PipeEqual -> 2
    | Question -> 3
    | PipePipe -> 4
    | AmpAmp -> 5
    | Pipe -> 6
    | Caret -> 7
    | Ampersand -> 8
    | EqualEqual | BangEqual -> 9
    | Less | Greater | LessEqual | GreaterEqual -> 10
    | LeftShift | RightShift -> 11
    | Plus | Minus -> 12
    | Star | Slash | Percent -> 13
    | _ -> 0
  in
  
  let token_to_binop = function
    | Plus -> Ast.Add | Minus -> Ast.Sub | Star -> Ast.Mul | Slash -> Ast.Div | Percent -> Ast.Mod
    | Less -> Ast.Lt | Greater -> Ast.Gt | LessEqual -> Ast.Le | GreaterEqual -> Ast.Ge
    | EqualEqual -> Ast.Eq | BangEqual -> Ast.Ne
    | Ampersand -> Ast.BitAnd | Pipe -> Ast.BitOr | Caret -> Ast.BitXor
    | LeftShift -> Ast.Shl | RightShift -> Ast.Shr
    | AmpAmp -> Ast.LogAnd | PipePipe -> Ast.LogOr
    | Equal -> Ast.Assign | PlusEqual -> Ast.AddAssign | MinusEqual -> Ast.SubAssign
    | StarEqual -> Ast.MulAssign | SlashEqual -> Ast.DivAssign | PercentEqual -> Ast.ModAssign
    | AmpEqual -> Ast.AndAssign | PipeEqual -> Ast.OrAssign | CaretEqual -> Ast.XorAssign
    | LeftShiftEqual -> Ast.ShlAssign | RightShiftEqual -> Ast.ShrAssign
    | _ -> failwith "Not a binary operator"
  in
  
  let rec parse_rhs lhs min_prec =
    let tok = (peek state).token in
    let prec = get_prec tok in
    
    if prec >= min_prec then begin
      match tok with
      | Question ->
          advance state;
          let true_expr = parse_expr state in
          expect state Colon;
          let false_expr = parse_conditional_expr state in
          parse_rhs (Ast.TernOp (lhs, true_expr, false_expr)) min_prec
      | Comma ->
          let rec parse_comma_list acc =
            if consume state Comma then
              parse_comma_list (parse_assignment_expr state :: acc)
            else
              List.rev acc
          in
          let exprs = parse_comma_list [lhs] in
          if List.length exprs > 1 then
            Ast.Comma exprs
          else
            lhs
      | _ ->
          advance state;
          let op = token_to_binop tok in
          let rhs = parse_cast_expr state in
          let rhs' = parse_prec rhs (prec + 1) in
          parse_rhs (Ast.BinOp (op, lhs, rhs')) min_prec
    end else
      lhs
      
  and parse_prec lhs prec =
    let tok = (peek state).token in
    let tok_prec = get_prec tok in
    
    if tok_prec >= prec then
      parse_rhs lhs prec
    else
      lhs
  in
  
  let lhs = parse_cast_expr state in
  parse_rhs lhs min_prec

(** Parse conditional expression *)
and parse_conditional_expr state =
  parse_binary_expr state 3  (* Start at ternary precedence *)

(** Parse assignment expression *)
and parse_assignment_expr_impl state =
  parse_binary_expr state 2  (* Start at assignment precedence *)

(** Implementation of parse_expr *)
let parse_expr_impl state =
  parse_binary_expr state 1  (* Start at comma precedence *)

(** Parse statement *)
let rec parse_compound_stmt state =
  expect state LeftBrace;
  let items : Ast.block_item list ref = ref [] in
  
  while not (check state RightBrace) do
    if is_declaration_start state then
      items := Ast.Decl (parse_decl state) :: !items
    else
      items := Ast.Stmt (parse_stmt state) :: !items
  done;
  
  expect state RightBrace;
  Ast.CompoundStmt (List.rev !items)

and is_declaration_start state =
  match (peek state).token with
  | Auto | Register | Static | Extern | Typedef
  | Void | Char | Short | Int | Long | Float | Double
  | Signed | Unsigned | Struct | Union | Enum
  | Const | Volatile -> true
  | Identifier name -> is_typedef_name state name
  | _ -> false

(** Parse selection statement *)
and parse_selection_stmt state =
  match (peek state).token with
  | If ->
      advance state;
      expect state LeftParen;
      let cond = parse_expr state in
      expect state RightParen;
      let then_stmt = parse_stmt state in
      let else_stmt =
        if consume state Else then
          Some (parse_stmt state)
        else
          None
      in
      Ast.IfStmt (cond, then_stmt, else_stmt)
  | Switch ->
      advance state;
      expect state LeftParen;
      let expr = parse_expr state in
      expect state RightParen;
      let body = parse_stmt state in
      Ast.SwitchStmt (expr, body)
  | _ -> 
      parse_error state "Expected if or switch";
      Ast.ExprStmt None

(** Parse iteration statement *)
and parse_iteration_stmt state =
  match (peek state).token with
  | While ->
      advance state;
      expect state LeftParen;
      let cond = parse_expr state in
      expect state RightParen;
      let body = parse_stmt state in
      Ast.WhileStmt (cond, body)
  | Do ->
      advance state;
      let body = parse_stmt state in
      expect state While;
      expect state LeftParen;
      let cond = parse_expr state in
      expect state RightParen;
      expect state Semicolon;
      Ast.DoWhileStmt (body, cond)
  | For ->
      advance state;
      expect state LeftParen;
      let init =
        if check state Semicolon then None
        else Some (parse_expr state)
      in
      expect state Semicolon;
      let cond =
        if check state Semicolon then None
        else Some (parse_expr state)
      in
      expect state Semicolon;
      let update =
        if check state RightParen then None
        else Some (parse_expr state)
      in
      expect state RightParen;
      let body = parse_stmt state in
      Ast.ForStmt (init, cond, update, body)
  | _ -> 
      parse_error state "Expected while, do, or for";
      Ast.ExprStmt None

(** Parse jump statement *)
and parse_jump_stmt state =
  match (peek state).token with
  | Goto ->
      advance state;
      (match (peek state).token with
       | Identifier label ->
           advance state;
           expect state Semicolon;
           Ast.GotoStmt label
       | _ -> 
           parse_error state "Expected label after goto";
           Ast.GotoStmt "")
  | Continue ->
      advance state;
      expect state Semicolon;
      Ast.ContinueStmt
  | Break ->
      advance state;
      expect state Semicolon;
      Ast.BreakStmt
  | Return ->
      advance state;
      let expr =
        if check state Semicolon then None
        else Some (parse_expr state)
      in
      expect state Semicolon;
      Ast.ReturnStmt expr
  | _ -> 
      parse_error state "Expected goto, continue, break, or return";
      Ast.ExprStmt None

(** Parse labeled statement *)
and parse_labeled_stmt state =
  match (peek state).token with
  | Identifier label when (peek_n state 1).token = Colon ->
      advance state;
      advance state;  (* Skip : *)
      let stmt = parse_stmt state in
      Ast.LabeledStmt (label, stmt)
  | Case ->
      advance state;
      let expr = parse_expr state in
      expect state Colon;
      let stmt = parse_stmt state in
      Ast.CaseStmt (expr, stmt)
  | Default ->
      advance state;
      expect state Colon;
      let stmt = parse_stmt state in
      Ast.DefaultStmt stmt
  | _ -> 
      parse_error state "Expected label, case, or default";
      Ast.ExprStmt None

(** Implementation of parse_stmt *)
let parse_stmt_impl state =
  match (peek state).token with
  | LeftBrace -> parse_compound_stmt state
  | If | Switch -> parse_selection_stmt state
  | While | Do | For -> parse_iteration_stmt state
  | Goto | Continue | Break | Return -> parse_jump_stmt state
  | Identifier _ when (peek_n state 1).token = Colon -> parse_labeled_stmt state
  | Case | Default -> parse_labeled_stmt state
  | Semicolon ->
      advance state;
      Ast.ExprStmt None
  | _ ->
      let expr = parse_expr state in
      expect state Semicolon;
      Ast.ExprStmt (Some expr)

(** Parse declaration *)
let parse_decl_impl state =
  let (storage, specs, quals) = parse_decl_specifiers state in
  
  let init_decls = ref [] in
  
  (* Check if this is just a type declaration (e.g., struct point { ... };) *)
  if check state Semicolon then begin
    (* Just a type declaration, no variables *)
    advance state;  (* Consume semicolon *)
  end else begin
    (* Parse declarators *)
    let rec parse_init_decls () =
      let decl = parse_declarator state in
      
      (* Register typedef name immediately after parsing declarator *)
      if List.mem Ast.Typedef storage then begin
        match decl with
        | Ast.DirectDecl (Ast.Ident name) 
        | Ast.PointerDecl (_, Ast.DirectDecl (Ast.Ident name)) ->
            let loc = (peek state).loc in
            let _ = C_symbol_table.add_typedef state.symbol_table name [] [] loc in ()
        | _ -> ()
      end;
      
      let init =
        if consume state Equal then
          Some (parse_initializer state)
        else
          None
      in
      init_decls := { Ast.decl; Ast.init } :: !init_decls;
      
      if consume state Comma then
        parse_init_decls ()
    in
    parse_init_decls ();
    expect state Semicolon;
  end;
  
  let declaration = { Ast.storage; Ast.specs; Ast.quals; Ast.init_decls = List.rev !init_decls } in
  declaration

(** Parse function definition *)
let parse_function_def state storage specs quals declarator =
  let old_style_params = ref [] in
  
  (* Check for K&R style parameter list *)
  if check state (Identifier "") && not (check state LeftBrace) then begin
    (* Parse identifier list *)
    while check state (Identifier "") do
      match (peek state).token with
      | Identifier name ->
          advance state;
          old_style_params := name :: !old_style_params;
          if not (consume state Comma) then
            ()
      | _ -> ()
    done;
    
    (* Parse parameter declarations *)
    while is_declaration_start state do
      let _ = parse_decl state in
      ()
    done
  end;
  
  let body = parse_compound_stmt state in
  { Ast.storage; Ast.specs; Ast.quals; Ast.declarator; 
    Ast.old_style_params = List.rev !old_style_params; Ast.body }

(** Parse external declaration *)
let parse_external_decl state =
  let (storage, specs, quals) = parse_decl_specifiers state in
  
  (* Check if this is just a type declaration (struct/union/enum without variables) *)
  if check state Semicolon then begin
    advance state;  (* Consume semicolon *)
    Ast.Decl { Ast.storage; Ast.specs; Ast.quals; Ast.init_decls = [] }
  end
  (* Check if this might be a function definition *)
  else if specs <> [] || quals <> [] then begin
    let saved_pos = state.pos in
    
    (* Try to parse a declarator *)
    let decl_opt = 
      try Some (parse_declarator state)
      with _ -> None
    in
    
    match decl_opt with
    | Some decl ->
        (* Check if this is a function definition *)
        let is_func_def =
          match decl with
          | Ast.DirectDecl (Ast.FuncDecl _) | Ast.PointerDecl (_, Ast.DirectDecl (Ast.FuncDecl _)) ->
              check state LeftBrace || 
              (match (peek state).token with Identifier _ -> true | _ -> false)
          | _ -> false
        in
        
        if is_func_def then
          Ast.FuncDef (parse_function_def state storage specs quals decl)
        else begin
          (* Not a function definition, parse rest as declaration *)
          (* Register typedef if needed *)
          if List.mem Ast.Typedef storage then begin
            match decl with
            | Ast.DirectDecl (Ast.Ident name) 
            | Ast.PointerDecl (_, Ast.DirectDecl (Ast.Ident name)) ->
                let loc = (peek state).loc in
            let _ = C_symbol_table.add_typedef state.symbol_table name [] [] loc in ()
            | _ -> ()
          end;
          
          (* Parse initializer if present *)
          let init =
            if consume state Equal then
              Some (parse_initializer state)
            else
              None
          in
          
          let init_decls = [{ Ast.decl; Ast.init }] in
          
          (* Parse additional declarators if comma-separated *)
          let rec parse_more_decls acc =
            if consume state Comma then
              let decl = parse_declarator state in
              (* Register typedef for this declarator too *)
              if List.mem Ast.Typedef storage then begin
                match decl with
                | Ast.DirectDecl (Ast.Ident name) 
                | Ast.PointerDecl (_, Ast.DirectDecl (Ast.Ident name)) ->
                    let loc = (peek state).loc in
            let _ = C_symbol_table.add_typedef state.symbol_table name [] [] loc in ()
                | _ -> ()
              end;
              let init =
                if consume state Equal then
                  Some (parse_initializer state)
                else
                  None
              in
              parse_more_decls ({ Ast.decl; Ast.init } :: acc)
            else
              acc
          in
          
          let all_init_decls = parse_more_decls init_decls in
          expect state Semicolon;
          
          Ast.Decl { Ast.storage; Ast.specs; Ast.quals; Ast.init_decls = List.rev all_init_decls }
        end
    | None ->
        (* No declarator, just parse rest as declaration *)
        state.pos <- saved_pos;
        Ast.Decl (parse_decl state)
  end else
    Ast.Decl (parse_decl state)

(** Parse translation unit *)
let parse_translation_unit state =
  let decls = ref [] in
  
  (* Filter out newlines from the beginning *)
  while check state Newline do
    advance state
  done;
  
  while not (check state Eof) do
    if check state Newline then
      advance state
    else
      decls := parse_external_decl state :: !decls
  done;
  
  List.rev !decls

(** Initialize forward references *)
let _ =
  expr_ref := parse_expr_impl;
  stmt_ref := parse_stmt_impl;
  decl_ref := parse_decl_impl;
  type_spec_ref := parse_type_specs_impl;
  declarator_ref := parse_declarator_impl;
  abstract_declarator_ref := parse_abstract_declarator_impl;
  assignment_expr_ref := parse_assignment_expr_impl

(** Main parse function *)
let parse tokens =
  (* Filter out newline tokens *)
  let filtered_tokens = List.filter (fun tok -> tok.token <> Newline) tokens in
  let state = create_parser filtered_tokens in
  parse_translation_unit state