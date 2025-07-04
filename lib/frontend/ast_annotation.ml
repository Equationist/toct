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
let create_annotation info = {
  info;
  location = None;
  metadata = [];
}

(** Create annotation with location *)
let with_location annotation location = {
  annotation with location = Some location
}

(** Add metadata to annotation *)
let add_metadata annotation key value =
  annotation.metadata <- (key, value) :: annotation.metadata

(** Get metadata from annotation *)
let get_metadata annotation key =
  try Some (List.assoc key annotation.metadata)
  with Not_found -> None

(** Generic annotated node *)
type ('ast, 'info) annotated_node = {
  node: 'ast;                         (* The original AST node *)
  annotation: 'info annotation;      (* The annotation *)
}

(** Create an annotated node *)
let annotate node info = {
  node;
  annotation = create_annotation info;
}

(** Create an annotated node with location *)
let annotate_with_location node info location = {
  node;
  annotation = with_location (create_annotation info) location;
}

(** Extract the original AST node *)
let get_node annotated = annotated.node

(** Extract the annotation info *)
let get_info annotated = annotated.annotation.info

(** Extract the location *)
let get_location annotated = annotated.annotation.location

(** Update the annotation info *)
let update_info annotated new_info = {
  annotated with annotation = { annotated.annotation with info = new_info }
}

(** Map over the AST node while preserving annotation *)
let map_node f annotated = {
  annotated with node = f annotated.node
}

(** Map over the annotation info *)
let map_info f annotated = {
  annotated with annotation = { annotated.annotation with info = f annotated.annotation.info }
}

(** Fold over a list of annotated nodes *)
let fold_annotated f acc annotated_list =
  List.fold_left (fun acc annotated -> f acc annotated.node annotated.annotation.info) acc annotated_list

(** Map over a list of annotated nodes *)
let map_annotated f annotated_list =
  List.map (fun annotated -> f annotated.node annotated.annotation.info) annotated_list

(** Common annotation patterns *)
module Common = struct
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

  let empty_type_info = {
    expr_type = None;
    is_lvalue = false;
    is_constant = false;
  }

  let empty_frontend_info = {
    type_info = None;
    scope_info = None;
    symbol_table = None;
  }

  let with_type_info info type_info = {
    info with type_info = Some type_info
  }

  let with_scope_info info scope_info = {
    info with scope_info = Some scope_info
  }

  let with_symbol_table info symbol_table = {
    info with symbol_table = Some symbol_table
  }
end

(** Visitor pattern for traversing annotated ASTs *)
module Visitor = struct
  type ('ast, 'info, 'acc) visitor = {
    visit_node: 'acc -> ('ast, 'info) annotated_node -> 'acc;
    visit_info: 'acc -> 'info -> 'acc;
  }

  let create_visitor visit_node visit_info = {
    visit_node; visit_info;
  }

  let visit visitor acc annotated =
    let acc' = visitor.visit_info acc annotated.annotation.info in
    visitor.visit_node acc' annotated
end