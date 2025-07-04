(** Annotated AST - AST nodes with type and scope information *)

open Ast
open C_type_system
open C_scoped_symbol_table

(** Annotation attached to AST nodes *)
type annotation = {
  node_type: c_type option;           (* Type of this node (for expressions) *)
  symbol_table: c_symbol_table option; (* Symbol table for scope-introducing nodes *)
  location: Lexer.location option;    (* Source location *)
}

(** Create empty annotation *)
let empty_annotation = {
  node_type = None;
  symbol_table = None;
  location = None;
}

(** Create annotation with type *)
let with_type ty = {
  empty_annotation with node_type = Some ty
}

(** Create annotation with symbol table *)
let with_symbol_table st = {
  empty_annotation with symbol_table = Some st
}

(** Annotated expression *)
type annotated_expr = {
  expr: expr;
  annotation: annotation;
}

(** Annotated statement *)
type annotated_stmt = {
  stmt: stmt;
  annotation: annotation;
}

(** Annotated block item *)
type annotated_block_item = {
  item: block_item;
  annotation: annotation;
}

(** Annotated function definition *)
type annotated_func_def = {
  func_def: func_def;
  annotation: annotation;
}

(** Annotated external declaration *)
type annotated_external_decl = {
  external_decl: external_decl;
  annotation: annotation;
}

(** Annotated translation unit *)
type annotated_translation_unit = annotated_external_decl list

(** Helper to create annotated expressions *)
let make_annotated_expr expr annotation = {
  expr; annotation
}

(** Helper to create annotated statements *)
let make_annotated_stmt stmt annotation = {
  stmt; annotation
}

(** Helper to extract type from annotated expression *)
let get_expr_type annotated_expr =
  annotated_expr.annotation.node_type

(** Helper to extract symbol table from annotated node *)
let get_symbol_table annotated_node_annotation =
  annotated_node_annotation.symbol_table