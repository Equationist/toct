(* PIR Module Structure - Based on portable_ir_spec.md section 3 *)

open Types
open Values
open Instructions

(* Top-level type declaration: type <name> = <agg_type> *)
type type_decl = {
  type_name: string;
  type_def: ty;
}

(* Object initialization expression *)
type const_expr = 
  | ConstValue of const_value
  | ConstAggregate of const_expr list    (* For arrays/structs *)
  | ConstZero                            (* Zero initializer *)

(* Global/const object declaration *)
type object_decl = {
  obj_name: string;
  obj_ty: ty;
  obj_align: int option;                 (* Optional alignment *)
  obj_init: const_expr;
  obj_is_const: bool;                    (* true = const, false = global *)
  obj_attrs: Attributes.t;
}

(* Module top-level item *)
type top_item = 
  | TypeDecl of type_decl
  | ObjectDecl of object_decl  
  | FuncDecl of func

(* Complete PIR module *)
type pir_module = {
  items: top_item list;
  module_attrs: Attributes.t;
}

(* Module creation utilities *)
let create_type_decl name def = 
  { type_name = name; type_def = def }

let create_object_decl ~is_const name ty ?align init attrs = 
  { obj_name = name; obj_ty = ty; obj_align = align; 
    obj_init = init; obj_is_const = is_const; obj_attrs = attrs }

let create_global name ty ?align init attrs = 
  create_object_decl ~is_const:false name ty ?align init attrs

let create_const name ty ?align init attrs = 
  create_object_decl ~is_const:true name ty ?align init attrs

let create_module items attrs = 
  { items; module_attrs = attrs }

(* Find declarations in module *)
let find_type_decl name module_ = 
  List.find_map (function
    | TypeDecl td when td.type_name = name -> Some td
    | _ -> None) module_.items

let find_object_decl name module_ = 
  List.find_map (function
    | ObjectDecl od when od.obj_name = name -> Some od  
    | _ -> None) module_.items

let find_func_decl name module_ = 
  List.find_map (function
    | FuncDecl fd when fd.name = name -> Some fd
    | _ -> None) module_.items

(* Module validation *)
let validate_module module_ = 
  (* Check for duplicate names *)
  let names = ref [] in
  let check_unique name = 
    if List.mem name !names then
      failwith (Printf.sprintf "Duplicate declaration: %s" name)
    else
      names := name :: !names in
  
  List.iter (function
    | TypeDecl td -> check_unique td.type_name
    | ObjectDecl od -> check_unique od.obj_name
    | FuncDecl fd -> check_unique fd.name
  ) module_.items;
  true