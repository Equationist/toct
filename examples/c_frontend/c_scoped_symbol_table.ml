(** C-specific symbol table with proper C89 scoping rules *)

open Ast
open C_type_system

(** C89 scoping rules:
    - File scope: identifiers declared outside any block
    - Function scope: labels have function scope  
    - Function prototype scope: parameter names in function prototypes
    - Block scope: identifiers declared inside a block
*)
type c_scope_kind =
  | FileScope
  | FunctionScope
  | PrototypeScope
  | BlockScope

(** C symbol information *)
type c_symbol = {
  name: string;
  c_type: c_type;
  storage_class: storage_class list;
  span: Lexer.location;
  scope_kind: c_scope_kind;
  mutable used: bool;
  mutable defined: bool;  (* For functions and variables with extern *)
}

(** C-specific scope *)
type c_scope = {
  id: int;
  kind: c_scope_kind;
  parent: int option;
  symbols: (string, c_symbol) Hashtbl.t;
  tags: (string, c_type) Hashtbl.t;  (* struct/union/enum tags have separate namespace *)
  mutable children: int list;
}

(** C symbol table *)
type c_symbol_table = {
  mutable scopes: (int, c_scope) Hashtbl.t;
  mutable current_scope: int;
  mutable next_scope_id: int;
  type_env: type_env;
  mutable current_function: c_symbol option;  (* Track current function for return type checking *)
}

(** Create new C symbol table *)
let create () =
  let file_scope = {
    id = 0;
    kind = FileScope;
    parent = None;
    symbols = Hashtbl.create 64;
    tags = Hashtbl.create 32;
    children = [];
  } in
  let scopes = Hashtbl.create 32 in
  Hashtbl.add scopes 0 file_scope;
  {
    scopes;
    current_scope = 0;
    next_scope_id = 1;
    type_env = create_type_env ();
    current_function = None;
  }

(** Get current scope *)
let current_scope table =
  Hashtbl.find table.scopes table.current_scope

(** Enter new scope *)
let enter_scope table kind =
  let parent_scope = current_scope table in
  let new_scope = {
    id = table.next_scope_id;
    kind;
    parent = Some table.current_scope;
    symbols = Hashtbl.create 16;
    tags = Hashtbl.create 8;
    children = [];
  } in
  parent_scope.children <- new_scope.id :: parent_scope.children;
  Hashtbl.add table.scopes new_scope.id new_scope;
  table.current_scope <- new_scope.id;
  table.next_scope_id <- table.next_scope_id + 1;
  new_scope.id

(** Exit current scope *)
let exit_scope table =
  let scope = current_scope table in
  match scope.parent with
  | Some parent_id -> table.current_scope <- parent_id
  | None -> failwith "Cannot exit file scope"

(** Scope management helpers *)
let with_scope table kind f =
  let _scope_id = enter_scope table kind in
  try
    let result = f () in
    exit_scope table;
    result
  with e ->
    exit_scope table;
    raise e

(** Find symbol in scope chain with C scoping rules *)
let rec lookup_symbol_in_scope table scope_id name =
  match Hashtbl.find_opt table.scopes scope_id with
  | None -> None
  | Some scope ->
    match Hashtbl.find_opt scope.symbols name with
    | Some sym -> Some sym
    | None ->
      match scope.parent with
      | Some parent_id ->
        (* Skip prototype scopes for variable lookup *)
        if scope.kind = PrototypeScope then
          lookup_symbol_in_scope table parent_id name
        else
          lookup_symbol_in_scope table parent_id name
      | None -> None

let lookup_symbol table name =
  lookup_symbol_in_scope table table.current_scope name

(** Lookup symbol only in current scope *)
let lookup_symbol_local table name =
  let scope = current_scope table in
  Hashtbl.find_opt scope.symbols name

(** Find tag (struct/union/enum) in scope chain *)
let rec lookup_tag_in_scope table scope_id name =
  match Hashtbl.find_opt table.scopes scope_id with
  | None -> None
  | Some scope ->
    match Hashtbl.find_opt scope.tags name with
    | Some ty -> Some ty
    | None ->
      match scope.parent with
      | Some parent_id -> lookup_tag_in_scope table parent_id name
      | None -> None

let lookup_tag table name =
  lookup_tag_in_scope table table.current_scope name

(** Define symbol with C89 rules *)
let define_symbol table name c_type storage_classes span =
  let scope = current_scope table in
  
  (* Check for redefinition in current scope *)
  match Hashtbl.find_opt scope.symbols name with
  | Some existing ->
    (* Check if it's a compatible redeclaration *)
    if types_compatible table.type_env existing.c_type c_type then (
      (* Compatible redeclaration - update information *)
      existing.defined <- true;
      Ok existing
    ) else
      Error (`IncompatibleRedeclaration (name, span, existing.span))
  | None ->
    let symbol = {
      name;
      c_type;
      storage_class = storage_classes;
      span;
      scope_kind = scope.kind;
      used = false;
      defined = true;
    } in
    Hashtbl.add scope.symbols name symbol;
    Ok symbol

(** Define tag (struct/union/enum) *)
let define_tag table name tag_type span =
  let scope = current_scope table in
  
  (* Tags have their own namespace *)
  match Hashtbl.find_opt scope.tags name with
  | Some existing ->
    if types_compatible table.type_env existing tag_type then
      Ok existing
    else
      Error (`IncompatibleTagRedefinition (name, span))
  | None ->
    Hashtbl.add scope.tags name tag_type;
    (* Also add to type environment for global access *)
    (match tag_type with
     | Struct (_, Some fields) -> Hashtbl.replace table.type_env.struct_tags name fields
     | Union (_, Some fields) -> Hashtbl.replace table.type_env.union_tags name fields
     | Enum (_, Some values) -> Hashtbl.replace table.type_env.enum_tags name values
     | _ -> ());
    Ok tag_type

(** Define typedef *)
let define_typedef table name target_type span =
  let scope = current_scope table in
  
  (* Check for redefinition *)
  match Hashtbl.find_opt scope.symbols name with
  | Some existing ->
    Error (`TypedefRedefinition (name, span, existing.span))
  | None ->
    let typedef_type = Typedef (name, target_type) in
    let symbol = {
      name;
      c_type = typedef_type;
      storage_class = [Typedef];
      span;
      scope_kind = scope.kind;
      used = false;
      defined = true;
    } in
    Hashtbl.add scope.symbols name symbol;
    Hashtbl.replace table.type_env.typedefs name target_type;
    Ok symbol

(** Mark symbol as used *)
let mark_used table name =
  match lookup_symbol table name with
  | Some sym -> sym.used <- true
  | None -> ()

(** Enter function scope *)
let enter_function_scope table func_symbol =
  let _scope_id = enter_scope table FunctionScope in
  table.current_function <- Some func_symbol

(** Exit function scope *)
let exit_function_scope table =
  exit_scope table;
  table.current_function <- None

(** Get current function return type *)
let current_function_return_type table =
  match table.current_function with
  | Some func_sym ->
    (match func_sym.c_type with
     | Function (ret_ty, _, _) -> ret_ty
     | _ -> None)
  | None -> None

(** Check if we're in global scope *)
let in_global_scope table =
  let scope = current_scope table in
  scope.kind = FileScope

(** Get all symbols in current scope *)
let symbols_in_current_scope table =
  let scope = current_scope table in
  Hashtbl.fold (fun _ sym acc -> sym :: acc) scope.symbols []

(** Get unused symbols in table *)
let unused_symbols table =
  let unused = ref [] in
  Hashtbl.iter (fun _ scope ->
    Hashtbl.iter (fun _ sym ->
      if not sym.used then unused := sym :: !unused
    ) scope.symbols
  ) table.scopes;
  !unused

(** Pretty printing *)
let string_of_scope_kind = function
  | FileScope -> "file"
  | FunctionScope -> "function"
  | PrototypeScope -> "prototype"
  | BlockScope -> "block"

let string_of_storage_class = function
  | Auto -> "auto"
  | Register -> "register"
  | Static -> "static"
  | Extern -> "extern"
  | Typedef -> "typedef"

let pp_c_symbol sym =
  let storage_str = String.concat " " (List.map string_of_storage_class sym.storage_class) in
  Printf.sprintf "%s%s %s: %s (%s scope)%s"
    (if storage_str = "" then "" else storage_str ^ " ")
    sym.name
    (string_of_c_type sym.c_type)
    (string_of_scope_kind sym.scope_kind)
    (if sym.used then "" else " (unused)")

(** Symbol table statistics *)
type c_symbol_stats = {
  total_scopes: int;
  file_scope_symbols: int;
  function_scope_symbols: int;
  block_scope_symbols: int;
  unused_symbols: int;
  undefined_functions: int;
}

let get_stats table =
  let stats = {
    total_scopes = Hashtbl.length table.scopes;
    file_scope_symbols = 0;
    function_scope_symbols = 0;
    block_scope_symbols = 0;
    unused_symbols = 0;
    undefined_functions = 0;
  } in
  let stats = ref stats in
  
  Hashtbl.iter (fun _ scope ->
    Hashtbl.iter (fun _ sym ->
      if not sym.used then 
        stats := { !stats with unused_symbols = !stats.unused_symbols + 1 };
      
      (match sym.c_type with
       | Function _ when not sym.defined ->
         stats := { !stats with undefined_functions = !stats.undefined_functions + 1 }
       | _ -> ());
      
      match scope.kind with
      | FileScope -> 
        stats := { !stats with file_scope_symbols = !stats.file_scope_symbols + 1 }
      | FunctionScope -> 
        stats := { !stats with function_scope_symbols = !stats.function_scope_symbols + 1 }
      | BlockScope -> 
        stats := { !stats with block_scope_symbols = !stats.block_scope_symbols + 1 }
      | PrototypeScope -> ()
    ) scope.symbols
  ) table.scopes;
  
  !stats