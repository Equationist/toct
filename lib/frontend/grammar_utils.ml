(* Common grammar patterns for frontend parsers *)

open Parser_utils
open Position

(* Common AST node types that many languages share *)
module Common_ast = struct
  (* Binary operators *)
  type binop =
    | Add | Sub | Mul | Div | Mod
    | Lt | Le | Gt | Ge | Eq | Ne
    | And | Or
    | BitAnd | BitOr | BitXor
    | Shl | Shr

  (* Unary operators *)
  type unop =
    | Plus | Minus | Not | BitNot

  (* Common expression patterns *)
  type 'a expr =
    | Literal of 'a * span
    | Ident of string * span
    | Binary of binop * 'a expr * 'a expr * span
    | Unary of unop * 'a expr * span
    | Call of 'a expr * 'a expr list * span
    | Index of 'a expr * 'a expr * span
    | Member of 'a expr * string * span

  (* Common statement patterns *)
  type ('expr, 'stmt) stmt =
    | Block of 'stmt list * span
    | If of 'expr * 'stmt * 'stmt option * span
    | While of 'expr * 'stmt * span
    | For of 'stmt option * 'expr option * 'expr option * 'stmt * span
    | Return of 'expr option * span
    | Break of span
    | Continue of span
    | Expr of 'expr * span
end

(* Parser combinators for common patterns *)
module Combinators = struct
  (* Optional parsing *)
  let optional state parse_fn =
    match parse_fn state with
    | Ok result -> Some result
    | Error _ -> None

  (* Many (zero or more) *)
  let many state parse_fn =
    let rec loop acc =
      match parse_fn state with
      | Ok result -> loop (result :: acc)
      | Error _ -> List.rev acc
    in
    loop []

  (* Many1 (one or more) *)
  let many1 state parse_fn =
    match parse_fn state with
    | Error _ -> Error ()
    | Ok first ->
        let rest = many state parse_fn in
        Ok (first :: rest)

  (* Choice (try alternatives) *)
  let choice state parsers =
    let start_pos = state.pos in
    let rec try_parsers = function
      | [] -> 
          state.pos <- start_pos;
          Error ()
      | parser :: rest ->
          state.pos <- start_pos;
          match parser state with
          | Ok result -> Ok result
          | Error _ -> try_parsers rest
    in
    try_parsers parsers

  (* Sequence *)
  let seq2 state p1 p2 =
    match p1 state with
    | Error _ -> Error ()
    | Ok r1 ->
        match p2 state with
        | Error _ -> Error ()
        | Ok r2 -> Ok (r1, r2)

  let seq3 state p1 p2 p3 =
    match seq2 state p1 p2 with
    | Error _ -> Error ()
    | Ok (r1, r2) ->
        match p3 state with
        | Error _ -> Error ()
        | Ok r3 -> Ok (r1, r2, r3)

  (* Map result *)
  let map f parser state =
    match parser state with
    | Error _ -> Error ()
    | Ok result -> Ok (f result)

  (* Bind (monadic) *)
  let bind parser f state =
    match parser state with
    | Error _ -> Error ()
    | Ok result -> f result state
end

(* Expression parsing utilities *)
module Expression = struct
  (* Precedence levels (C-like) *)
  type precedence = int
  
  let prec_lowest = 0
  let prec_comma = 10
  let prec_assign = 20
  let prec_ternary = 30
  let prec_logical_or = 40
  let prec_logical_and = 50
  let prec_bitwise_or = 60
  let prec_bitwise_xor = 70
  let prec_bitwise_and = 80
  let prec_equality = 90
  let prec_relational = 100
  let prec_shift = 110
  let prec_additive = 120
  let prec_multiplicative = 130
  let prec_unary = 140
  let prec_postfix = 150
  let prec_primary = 160

  (* Binary operator precedence *)
  let binop_precedence = function
    | Common_ast.Add | Common_ast.Sub -> prec_additive
    | Common_ast.Mul | Common_ast.Div | Common_ast.Mod -> prec_multiplicative
    | Common_ast.Lt | Common_ast.Le | Common_ast.Gt | Common_ast.Ge -> prec_relational
    | Common_ast.Eq | Common_ast.Ne -> prec_equality
    | Common_ast.And -> prec_logical_and
    | Common_ast.Or -> prec_logical_or
    | Common_ast.BitAnd -> prec_bitwise_and
    | Common_ast.BitOr -> prec_bitwise_or
    | Common_ast.BitXor -> prec_bitwise_xor
    | Common_ast.Shl | Common_ast.Shr -> prec_shift

  (* Associativity *)
  type assoc = Left | Right

  let binop_assoc = function
    | Common_ast.Add | Common_ast.Sub | Common_ast.Mul | Common_ast.Div | Common_ast.Mod
    | Common_ast.Lt | Common_ast.Le | Common_ast.Gt | Common_ast.Ge
    | Common_ast.Eq | Common_ast.Ne | Common_ast.And | Common_ast.Or
    | Common_ast.BitAnd | Common_ast.BitOr | Common_ast.BitXor
    | Common_ast.Shl | Common_ast.Shr -> Left
end

(* Declaration parsing patterns *)
module Declaration = struct
  (* Common declaration specifiers *)
  type storage_class = Auto | Static | Extern | Register
  type type_qualifier = Const | Volatile | Restrict
  
  (* Parse comma-separated declarators *)
  let parse_declarator_list state parse_declarator separator to_string =
    let declarators = ref [] in
    match parse_declarator state with
    | Error _ -> []
    | Ok first ->
        declarators := first :: !declarators;
        while consume state separator do
          match parse_declarator state with
          | Ok decl -> declarators := decl :: !declarators
          | Error _ -> ()
        done;
        List.rev !declarators
end

(* Statement parsing patterns *)
module Statement = struct
  (* Parse statement list until terminator *)
  let parse_statement_list state parse_stmt terminator =
    let stmts = ref [] in
    while not (check state terminator) && not (at_end state) do
      match parse_stmt state with
      | Ok stmt -> stmts := stmt :: !stmts
      | Error _ -> 
          (* Error recovery: skip to next likely statement start *)
          recover state (SkipTo [terminator])
    done;
    List.rev !stmts

  (* Parse block statement *)
  let parse_block state open_tok close_tok parse_stmt to_string =
    parse_bracketed state open_tok close_tok to_string (fun state ->
      Ok (parse_statement_list state parse_stmt close_tok)
    )
end

(* Type parsing patterns *)
module Types = struct
  (* Common type categories *)
  type type_category =
    | Primitive of string
    | Pointer of type_category
    | Array of type_category * int option
    | Function of type_category * type_category list
    | Struct of string option
    | Union of string option
    | Enum of string option
    | Typedef of string

  (* Parse type with optional qualifiers *)
  let parse_qualified_type state parse_base_type parse_qualifier =
    let quals = ref [] in
    (* Leading qualifiers *)
    while parse_qualifier state do
      ()
    done;
    match parse_base_type state with
    | Error _ -> Error ()
    | Ok base_ty ->
        (* Trailing qualifiers *)
        while parse_qualifier state do
          ()
        done;
        Ok (base_ty, !quals)
end

(* Diagnostic helpers *)
module Diagnostics = struct
  (* Report unexpected token *)
  let unexpected_token state expected =
    match peek state with
    | Some tok ->
        let span = tok.Token_location.span in
        let msg = Printf.sprintf "Unexpected token: expected %s" expected in
        Error_reporter.report state.reporter Error_reporter.Error span msg [];
        Error ()
    | None ->
        let span = current_span state in
        let msg = Printf.sprintf "Unexpected end of input: expected %s" expected in
        Error_reporter.report state.reporter Error_reporter.Error span msg [];
        Error ()

  (* Report with recovery suggestion *)
  let error_with_recovery state msg recovery_tokens =
    let span = current_span state in
    let note = Printf.sprintf "Skipping to one of: %s" 
      (String.concat ", " recovery_tokens) in
    Error_reporter.report state.reporter Error span msg [(span, note)];
    Error ()
end