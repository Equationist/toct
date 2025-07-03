(* Symbol Table - Scoped symbol management *)

open Position

(* Symbol kinds *)
type symbol_kind =
  | Variable
  | Function
  | Type
  | Module
  | Constructor
  | Field

(* Symbol information *)
type 'ty symbol = {
  name: string;
  kind: symbol_kind;
  ty: 'ty;
  span: span;
  mutable used: bool;
}

(* Scope identifier *)
type scope_id = int

(* Scope *)
type 'ty scope = {
  id: scope_id;
  parent: scope_id option;
  symbols: (string, 'ty symbol) Hashtbl.t;
  mutable children: scope_id list;
}

(* Symbol table *)
type 'ty t = {
  mutable scopes: (scope_id, 'ty scope) Hashtbl.t;
  mutable current_scope: scope_id;
  mutable next_scope_id: scope_id;
}

(* Create a new symbol table *)
let create () = 
  let root_scope = {
    id = 0;
    parent = None;
    symbols = Hashtbl.create 16;
    children = [];
  } in
  let scopes = Hashtbl.create 16 in
  Hashtbl.add scopes 0 root_scope;
  {
    scopes;
    current_scope = 0;
    next_scope_id = 1;
  }

(* Get current scope *)
let current_scope table =
  Hashtbl.find table.scopes table.current_scope

(* Enter a new scope *)
let enter_scope table =
  let parent_scope = current_scope table in
  let new_scope = {
    id = table.next_scope_id;
    parent = Some table.current_scope;
    symbols = Hashtbl.create 16;
    children = [];
  } in
  parent_scope.children <- new_scope.id :: parent_scope.children;
  Hashtbl.add table.scopes new_scope.id new_scope;
  table.current_scope <- new_scope.id;
  table.next_scope_id <- table.next_scope_id + 1;
  new_scope.id

(* Exit current scope *)
let exit_scope table =
  let scope = current_scope table in
  match scope.parent with
  | Some parent_id ->
    table.current_scope <- parent_id
  | None ->
    failwith "Cannot exit root scope"

(* Define a symbol in current scope *)
let define table name kind ty span =
  let scope = current_scope table in
  if Hashtbl.mem scope.symbols name then
    Error (`DuplicateDefinition (name, span))
  else
    let symbol = { name; kind; ty; span; used = false } in
    Hashtbl.add scope.symbols name symbol;
    Ok symbol

(* Lookup symbol in scope chain *)
let rec lookup_in_scope table scope_id name =
  match Hashtbl.find_opt table.scopes scope_id with
  | None -> None
  | Some scope ->
    match Hashtbl.find_opt scope.symbols name with
    | Some sym -> Some sym
    | None ->
      match scope.parent with
      | Some parent_id -> lookup_in_scope table parent_id name
      | None -> None

let lookup table name =
  lookup_in_scope table table.current_scope name

(* Lookup only in current scope (no parent lookup) *)
let lookup_local table name =
  let scope = current_scope table in
  Hashtbl.find_opt scope.symbols name

(* Mark symbol as used *)
let mark_used table name =
  match lookup table name with
  | Some sym -> sym.used <- true
  | None -> ()

(* Get all symbols in current scope *)
let symbols_in_scope table =
  let scope = current_scope table in
  Hashtbl.fold (fun _ sym acc -> sym :: acc) scope.symbols []

(* Get unused symbols in a scope *)
let unused_symbols table scope_id =
  match Hashtbl.find_opt table.scopes scope_id with
  | None -> []
  | Some scope ->
    Hashtbl.fold (fun _ sym acc ->
      if not sym.used then sym :: acc else acc
    ) scope.symbols []

(* Walk all scopes with a function *)
let walk_scopes table f =
  Hashtbl.iter (fun id scope -> f id scope) table.scopes

(* Scope guard for automatic scope management *)
let with_scope table f =
  let _scope_id = enter_scope table in
  try
    let result = f () in
    exit_scope table;
    result
  with e ->
    exit_scope table;
    raise e

(* Pretty printing *)
let pp_symbol_kind = function
  | Variable -> "variable"
  | Function -> "function"
  | Type -> "type"
  | Module -> "module"
  | Constructor -> "constructor"
  | Field -> "field"

let pp_symbol pp_ty sym =
  Printf.sprintf "%s %s: %s at %s%s"
    (pp_symbol_kind sym.kind)
    sym.name
    (pp_ty sym.ty)
    (span_to_string sym.span)
    (if sym.used then "" else " (unused)")

(* Symbol table statistics *)
type stats = {
  total_scopes: int;
  total_symbols: int;
  unused_symbols: int;
  max_scope_depth: int;
}

let stats table =
  let total_scopes = Hashtbl.length table.scopes in
  let total_symbols = ref 0 in
  let unused_symbols = ref 0 in
  let max_depth = ref 0 in
  
  let rec compute_depth scope_id depth =
    max_depth := max !max_depth depth;
    match Hashtbl.find_opt table.scopes scope_id with
    | None -> ()
    | Some scope ->
      List.iter (fun child_id -> compute_depth child_id (depth + 1)) scope.children
  in
  
  Hashtbl.iter (fun _ scope ->
    Hashtbl.iter (fun _ sym ->
      incr total_symbols;
      if not sym.used then incr unused_symbols
    ) scope.symbols
  ) table.scopes;
  
  compute_depth 0 0;
  
  {
    total_scopes;
    total_symbols = !total_symbols;
    unused_symbols = !unused_symbols;
    max_scope_depth = !max_depth;
  }