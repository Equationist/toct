(** Generic AST Annotation Framework
    
    This module provides a generic framework for annotating AST nodes with
    additional information like types, symbol tables, source locations, etc.
    It can be used by any language frontend.
*)

(** Generic annotation that can hold arbitrary information *)
type 'info annotation = {
  info: 'info;                        (* Language-specific annotation data *)
  location: Position.span option;     (* Source location span *)
  mutable metadata: (string * string) list; (* Additional key-value metadata *)
}

(** Create an empty annotation with given info *)
val create_annotation : 'info -> 'info annotation

(** Create annotation with location *)
val with_location : 'info annotation -> Position.span -> 'info annotation

(** Add metadata to annotation *)
val add_metadata : 'info annotation -> string -> string -> unit

(** Get metadata from annotation *)
val get_metadata : 'info annotation -> string -> string option

(** Generic annotated node *)
type ('ast, 'info) annotated_node = {
  node: 'ast;                         (* The original AST node *)
  annotation: 'info annotation;      (* The annotation *)
}

(** Create an annotated node *)
val annotate : 'ast -> 'info -> ('ast, 'info) annotated_node

(** Create an annotated node with location *)
val annotate_with_location : 'ast -> 'info -> Position.span -> ('ast, 'info) annotated_node

(** Extract the original AST node *)
val get_node : ('ast, 'info) annotated_node -> 'ast

(** Extract the annotation info *)
val get_info : ('ast, 'info) annotated_node -> 'info

(** Extract the location *)
val get_location : ('ast, 'info) annotated_node -> Position.span option

(** Update the annotation info *)
val update_info : ('ast, 'info) annotated_node -> 'info -> ('ast, 'info) annotated_node

(** Map over the AST node while preserving annotation *)
val map_node : ('ast1 -> 'ast2) -> ('ast1, 'info) annotated_node -> ('ast2, 'info) annotated_node

(** Map over the annotation info *)
val map_info : ('info1 -> 'info2) -> ('ast, 'info1) annotated_node -> ('ast, 'info2) annotated_node

(** Fold over a list of annotated nodes *)
val fold_annotated : ('acc -> 'ast -> 'info -> 'acc) -> 'acc -> ('ast, 'info) annotated_node list -> 'acc

(** Map over a list of annotated nodes *)
val map_annotated : ('ast -> 'info -> 'result) -> ('ast, 'info) annotated_node list -> 'result list

(** Common annotation patterns *)
module Common : sig
  (** Type annotation for expressions *)
  type type_info = {
    expr_type: string option;         (* Type of expression *)
    is_lvalue: bool;                  (* Whether this is an lvalue *)
    is_constant: bool;                (* Whether this is a compile-time constant *)
  }

  (** Scope annotation for statements/declarations *)
  type scope_info = {
    scope_id: int;                    (* Unique scope identifier *)
    scope_type: [ `Global | `Function | `Block | `Prototype ]; (* Type of scope *)
    parent_scope: int option;         (* Parent scope ID *)
  }

  (** Symbol table annotation *)
  type symbol_table_info = {
    symbols: (string * string) list;  (* symbol name -> type *)
    scope_info: scope_info;
  }

  (** Combined annotation for language frontends *)
  type frontend_info = {
    type_info: type_info option;
    scope_info: scope_info option;
    symbol_table: symbol_table_info option;
  }

  val empty_type_info : type_info
  val empty_frontend_info : frontend_info
  val with_type_info : frontend_info -> type_info -> frontend_info
  val with_scope_info : frontend_info -> scope_info -> frontend_info
  val with_symbol_table : frontend_info -> symbol_table_info -> frontend_info
end

(** Visitor pattern for traversing annotated ASTs *)
module Visitor : sig
  type ('ast, 'info, 'acc) visitor = {
    visit_node: 'acc -> ('ast, 'info) annotated_node -> 'acc;
    visit_info: 'acc -> 'info -> 'acc;
  }

  val create_visitor : 
    ('acc -> ('ast, 'info) annotated_node -> 'acc) -> 
    ('acc -> 'info -> 'acc) -> 
    ('ast, 'info, 'acc) visitor

  val visit : ('ast, 'info, 'acc) visitor -> 'acc -> ('ast, 'info) annotated_node -> 'acc
end