(** Simple symbol table for C frontend *)

type symbol_kind =
  | TypedefName
  | Variable
  | Function
  | EnumConstant

type symbol_info = {
  name: string;
  kind: symbol_kind;
  (* Type information would go here in a full implementation *)
}

type t = {
  mutable scopes: (string, symbol_info) Hashtbl.t list;
}

let create () = {
  scopes = [Hashtbl.create 16];
}

let push_scope table =
  table.scopes <- Hashtbl.create 16 :: table.scopes

let pop_scope table =
  match table.scopes with
  | [] -> failwith "No scope to pop"
  | _ :: rest -> table.scopes <- rest

let add_symbol table name info =
  match table.scopes with
  | [] -> failwith "No current scope"
  | scope :: _ -> Hashtbl.add scope name info

let lookup table name =
  let rec search = function
    | [] -> None
    | scope :: rest ->
        match Hashtbl.find_opt scope name with
        | Some info -> Some info
        | None -> search rest
  in
  search table.scopes

let is_typedef table name =
  match lookup table name with
  | Some { kind = TypedefName; _ } -> true
  | _ -> false

let add_typedef table name =
  add_symbol table name { name; kind = TypedefName }