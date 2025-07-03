(* C-specific symbol table interface *)

(* C-specific type information *)
type c_type_info =
  | Typedef of Ast.type_spec list * Ast.type_qualifier list
  | Variable of Ast.type_spec list * Ast.type_qualifier list
  | Function of Ast.type_spec list * Ast.type_qualifier list * Ast.param_list option
  | EnumConstant of int64 option
  | StructTag of string option
  | UnionTag of string option
  | EnumTag of string option

(* Symbol table type *)
type t

(* Create a new C symbol table *)
val create : unit -> t

(* Scope management *)
val enter_scope : t -> unit
val exit_scope : t -> unit
val with_scope : t -> (unit -> 'a) -> 'a

(* Symbol definition *)
val add_typedef : t -> string -> Ast.type_spec list -> Ast.type_qualifier list -> 
                  Lexer.location -> (unit, string) result
val add_variable : t -> string -> Ast.type_spec list -> Ast.type_qualifier list -> 
                   Lexer.location -> (unit, string) result
val add_function : t -> string -> Ast.type_spec list -> Ast.type_qualifier list -> 
                   Ast.param_list option -> Lexer.location -> (unit, string) result
val add_struct_tag : t -> string -> string option -> Lexer.location -> (unit, string) result
val add_union_tag : t -> string -> string option -> Lexer.location -> (unit, string) result
val add_enum_tag : t -> string -> string option -> Lexer.location -> (unit, string) result
val add_enum_constant : t -> string -> int64 option -> Lexer.location -> (unit, string) result

(* Symbol lookup *)
val is_typedef : t -> string -> bool
val lookup : t -> string -> c_type_info Compilerkit_frontend.Symbol_table.symbol option
val mark_used : t -> string -> unit

(* Analysis *)
val get_unused_symbols : t -> (string * c_type_info Compilerkit_frontend.Symbol_table.symbol) list