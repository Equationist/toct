(** C-specific Annotated AST using the generic annotation framework *)

open Ast
open C_type_system
open C_scoped_symbol_table
open Compilerkit_frontend.Ast_annotation

(** C-specific annotation information *)
type c_annotation_info = {
  c_type: c_type option;                      (* Type of expression/declaration *)
  symbol_table: c_symbol_table option;       (* Symbol table for scope-introducing nodes *)
  scope_info: Common.scope_info option;      (* Scope information *)
  is_lvalue: bool;                           (* Whether this expression is an lvalue *)
  is_constant: bool;                         (* Whether this is a compile-time constant *)
  variable_symbol: c_symbol option;          (* For identifiers, the resolved symbol *)
}

(** Create empty C annotation info *)
let empty_c_info = {
  c_type = None;
  symbol_table = None;
  scope_info = None;
  is_lvalue = false;
  is_constant = false;
  variable_symbol = None;
}

(** Type aliases for C annotated AST nodes *)
type c_annotated_expr = (expr, c_annotation_info) annotated_node
type c_annotated_stmt = (stmt, c_annotation_info) annotated_node
type c_annotated_block_item = (block_item, c_annotation_info) annotated_node
type c_annotated_func_def = (func_def, c_annotation_info) annotated_node
type c_annotated_external_decl = (external_decl, c_annotation_info) annotated_node
type c_annotated_translation_unit = c_annotated_external_decl list

(** Helper functions for C annotations *)
let with_c_type info c_type = { info with c_type = Some c_type }
let with_symbol_table info symbol_table = { info with symbol_table = Some symbol_table }
let with_scope_info info scope_info = { info with scope_info = Some scope_info }
let with_lvalue info is_lvalue = { info with is_lvalue = is_lvalue }
let with_constant info is_constant = { info with is_constant = is_constant }
let with_variable_symbol info symbol = { info with variable_symbol = Some symbol }

(** Extract C type from annotated expression *)
let get_c_type annotated_expr =
  (get_info annotated_expr).c_type

(** Extract symbol table from annotated node *)
let get_c_symbol_table annotated_node =
  (get_info annotated_node).symbol_table

(** Extract scope info from annotated node *)
let get_c_scope_info annotated_node =
  (get_info annotated_node).scope_info

(** Check if annotated expression is an lvalue *)
let is_lvalue annotated_expr =
  (get_info annotated_expr).is_lvalue

(** Check if annotated expression is constant *)
let is_constant annotated_expr =
  (get_info annotated_expr).is_constant

(** Get variable symbol from annotated identifier *)
let get_variable_symbol annotated_expr =
  (get_info annotated_expr).variable_symbol

(** Create annotated expression with type *)
let make_typed_expr expr c_type =
  annotate expr (with_c_type empty_c_info c_type)

(** Create annotated lvalue expression *)
let make_lvalue_expr expr c_type =
  annotate expr (with_lvalue (with_c_type empty_c_info c_type) true)

(** Create annotated constant expression *)
let make_constant_expr expr c_type _value =
  annotate expr (with_constant (with_c_type empty_c_info c_type) true)

(** Create annotated statement with scope *)
let make_scoped_stmt stmt symbol_table scope_info =
  annotate stmt (with_scope_info (with_symbol_table empty_c_info symbol_table) scope_info)

(** Annotation utilities *)
module Annotation_utils = struct
  (** Derive result type for binary operations *)
  let derive_binary_op_type op left_type right_type =
    match op with
    | Add | Sub | Mul | Div | Mod ->
      (* Arithmetic operations: use usual arithmetic conversions *)
      let promoted_left, _promoted_right = usual_arithmetic_conversions left_type right_type in
      promoted_left
    | Lt | Gt | Le | Ge | Eq | Ne ->
      (* Comparison operations always return bool *)
      Bool
    | LogAnd | LogOr ->
      (* Logical operations return bool *)
      Bool
    | BitAnd | BitOr | BitXor | Shl | Shr ->
      (* Bitwise operations: use usual arithmetic conversions *)
      let promoted_left, _promoted_right = usual_arithmetic_conversions left_type right_type in
      promoted_left
    | Assign ->
      (* Assignment returns the type of the left operand *)
      left_type
    | _ ->
      (* For other assignment operators, return left type *)
      left_type

  (** Check if expression can be an lvalue *)
  let can_be_lvalue = function
    | Ident _ -> true
    | ArrayRef (_, _) -> true
    | Member (_, _) -> true
    | PtrMember (_, _) -> true
    | UnOp (Deref, _) -> true
    | _ -> false

  (** Check if expression is a compile-time constant *)
  let is_compile_time_constant = function
    | IntLit _ | FloatLit _ | CharLit _ | StringLit _ -> true
    | _ -> false
end