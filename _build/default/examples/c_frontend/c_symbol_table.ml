(* C-specific symbol table using shared frontend infrastructure *)

open Compilerkit_frontend

(* C-specific type information *)
type c_type_info =
  | Typedef of Ast.type_spec list * Ast.type_qualifier list
  | Variable of Ast.type_spec list * Ast.type_qualifier list
  | Function of Ast.type_spec list * Ast.type_qualifier list * Ast.param_list option
  | EnumConstant of int64 option
  | StructTag of string option (* optional tag name *)
  | UnionTag of string option
  | EnumTag of string option

(* Create a span from C lexer location *)
let span_of_location (loc: Lexer.location) =
  let pos = Position.create loc.filename loc.line loc.column 0 in
  Position.span pos pos

(* C symbol table type *)
type t = c_type_info Symbol_table.t

(* Create a new C symbol table *)
let create () = Symbol_table.create ()

(* Enter a new scope *)
let enter_scope table = 
  let _ = Symbol_table.enter_scope table in
  ()

(* Exit current scope *)
let exit_scope table = 
  Symbol_table.exit_scope table

(* Add a typedef *)
let add_typedef table name type_specs qualifiers loc =
  let span = span_of_location loc in
  let info = Typedef (type_specs, qualifiers) in
  match Symbol_table.define table name Symbol_table.Type info span with
  | Ok _ -> Ok ()
  | Error (`DuplicateDefinition (name, _)) -> 
      Error (Printf.sprintf "Typedef '%s' already defined" name)

(* Check if a name is a typedef in current scope *)
let is_typedef table name =
  match Symbol_table.lookup table name with
  | Some sym -> 
      (match sym.Symbol_table.kind, sym.Symbol_table.ty with
      | Symbol_table.Type, Typedef _ -> true
      | _ -> false)
  | None -> false

(* Add a variable *)
let add_variable table name type_specs qualifiers loc =
  let span = span_of_location loc in
  let info = Variable (type_specs, qualifiers) in
  match Symbol_table.define table name Symbol_table.Variable info span with
  | Ok _ -> Ok ()
  | Error (`DuplicateDefinition (name, _)) -> 
      Error (Printf.sprintf "Variable '%s' already defined" name)

(* Add a function *)
let add_function table name type_specs qualifiers params loc =
  let span = span_of_location loc in
  let info = Function (type_specs, qualifiers, params) in
  match Symbol_table.define table name Symbol_table.Function info span with
  | Ok _ -> Ok ()
  | Error (`DuplicateDefinition (name, _)) -> 
      Error (Printf.sprintf "Function '%s' already defined" name)

(* Add struct/union/enum tags *)
let add_struct_tag table name tag loc =
  let span = span_of_location loc in
  let info = StructTag tag in
  match Symbol_table.define table name Symbol_table.Type info span with
  | Ok _ -> Ok ()
  | Error (`DuplicateDefinition (name, _)) -> 
      Error (Printf.sprintf "Struct tag '%s' already defined" name)

let add_union_tag table name tag loc =
  let span = span_of_location loc in
  let info = UnionTag tag in
  match Symbol_table.define table name Symbol_table.Type info span with
  | Ok _ -> Ok ()
  | Error (`DuplicateDefinition (name, _)) -> 
      Error (Printf.sprintf "Union tag '%s' already defined" name)

let add_enum_tag table name tag loc =
  let span = span_of_location loc in
  let info = EnumTag tag in
  match Symbol_table.define table name Symbol_table.Type info span with
  | Ok _ -> Ok ()
  | Error (`DuplicateDefinition (name, _)) -> 
      Error (Printf.sprintf "Enum tag '%s' already defined" name)

(* Add enum constant *)
let add_enum_constant table name value loc =
  let span = span_of_location loc in
  let info = EnumConstant value in
  match Symbol_table.define table name Symbol_table.Constructor info span with
  | Ok _ -> Ok ()
  | Error (`DuplicateDefinition (name, _)) -> 
      Error (Printf.sprintf "Enum constant '%s' already defined" name)

(* Lookup any symbol *)
let lookup table name = Symbol_table.lookup table name

(* Mark symbol as used *)
let mark_used table name = Symbol_table.mark_used table name

(* Get unused symbols for warnings *)
let get_unused_symbols _table = 
  (* TODO: Implement when Symbol_table provides this functionality *)
  []

(* Scope guard for automatic scope management *)
let with_scope table f =
  Symbol_table.with_scope table f