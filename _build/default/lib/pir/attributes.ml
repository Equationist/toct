(* PIR Attribute System - Based on pir_annotations.md *)

(* JSON-based attribute storage *)
type json = 
  | Null
  | Bool of bool
  | Int of int
  | Float of float
  | String of string
  | Array of json list
  | Object of (string * json) list

(* Attribute bag - maps string keys to JSON values *)
type t = (string, json) Hashtbl.t

(* Create empty attribute bag *)
let empty () = Hashtbl.create 16

(* Add an attribute *)
let add key value attrs =
  let new_attrs = Hashtbl.copy attrs in
  Hashtbl.replace new_attrs key value;
  new_attrs

(* Get an attribute (returns None if not found) *)
let get_opt key attrs = 
  Hashtbl.find_opt attrs key

(* Get an attribute with default *)
let get_default key default attrs =
  match get_opt key attrs with
  | Some value -> value
  | None -> default

(* Check if attribute exists *)
let has key attrs = Hashtbl.mem attrs key

(* Check if attribute bag is empty *)
let is_empty attrs = Hashtbl.length attrs = 0

(* Remove an attribute *)
let remove key attrs =
  let new_attrs = Hashtbl.copy attrs in
  Hashtbl.remove new_attrs key;
  new_attrs

(* Get all keys *)
let keys attrs = 
  Hashtbl.fold (fun k _ acc -> k :: acc) attrs []

(* Convert to association list *)
let to_list attrs =
  Hashtbl.fold (fun k v acc -> (k, v) :: acc) attrs []

(* Convert from association list *)
let of_list pairs =
  let attrs = empty () in
  List.iter (fun (k, v) -> Hashtbl.replace attrs k v) pairs;
  attrs

(* Merge two attribute bags (second overrides first) *)
let merge attrs1 attrs2 =
  let merged = Hashtbl.copy attrs1 in
  Hashtbl.iter (Hashtbl.replace merged) attrs2;
  merged

(* JSON utilities *)
let rec string_of_json = function
  | Null -> "null"
  | Bool true -> "true"
  | Bool false -> "false"
  | Int i -> string_of_int i
  | Float f -> string_of_float f
  | String s -> Printf.sprintf "\"%s\"" (String.escaped s)
  | Array items -> 
    let item_strs = List.map string_of_json items in
    Printf.sprintf "[%s]" (String.concat "," item_strs)
  | Object pairs ->
    let pair_strs = List.map (fun (k, v) -> 
      Printf.sprintf "\"%s\":%s" k (string_of_json v)) pairs in
    Printf.sprintf "{%s}" (String.concat "," pair_strs)

(* Pretty print attributes as @{...} *)
let string_of_attrs attrs =
  if Hashtbl.length attrs = 0 then ""
  else
    let pairs = to_list attrs in
    let sorted_pairs = List.sort (fun (k1, _) (k2, _) -> String.compare k1 k2) pairs in
    let pair_strs = List.map (fun (k, v) -> 
      Printf.sprintf "%s:%s" k (string_of_json v)) sorted_pairs in
    Printf.sprintf "@{%s}" (String.concat "," pair_strs)

(* Helper functions for common attribute types *)
module Helpers = struct
  (* Range attribute: [min, max] *)
  let set_range min_val max_val attrs =
    add "range" (Array [Int min_val; Int max_val]) attrs
  
  let get_range attrs =
    match get_opt "range" attrs with
    | Some (Array [Int min_val; Int max_val]) -> Some (min_val, max_val)
    | _ -> None

  (* Nonnull attribute *)
  let set_nonnull attrs = add "nonnull" (Bool true) attrs
  
  let is_nonnull attrs =
    match get_opt "nonnull" attrs with
    | Some (Bool true) -> true
    | _ -> false

  (* Pure function attribute *)
  let set_pure attrs = add "pure" (Bool true) attrs
  
  let is_pure attrs =
    match get_opt "pure" attrs with
    | Some (Bool true) -> true
    | _ -> false

  (* Type attribute (for proof export) *)
  let set_type type_str attrs = add "type" (String type_str) attrs
  
  let get_type attrs =
    match get_opt "type" attrs with
    | Some (String s) -> Some s
    | _ -> None

  (* Contract attributes *)
  let set_precondition expr attrs = add "pre" (String expr) attrs
  let set_postcondition expr attrs = add "post" (String expr) attrs
  let set_invariant expr attrs = add "invariant" (String expr) attrs

  let get_precondition attrs =
    match get_opt "pre" attrs with
    | Some (String s) -> Some s
    | _ -> None

  let get_postcondition attrs =
    match get_opt "post" attrs with  
    | Some (String s) -> Some s
    | _ -> None

  let get_invariant attrs =
    match get_opt "invariant" attrs with
    | Some (String s) -> Some s
    | _ -> None
end