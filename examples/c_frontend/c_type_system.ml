(** C89/90 Type System with proper integer promotion and type checking *)

open Ast

(** C89 integer types with their properties *)
type c_int_type = {
  name: string;
  size: int;        (* size in bytes *)
  signed: bool;
  rank: int;        (* for integer promotion: char < short < int < long *)
}

(** C89 floating point types *)
type c_float_type = {
  name: string;
  size: int;
  precision: [`Single | `Double];
}

(** Resolved C types - after processing declarators and type specifiers *)
type c_type =
  | Void
  | Bool                                    (* _Bool in C99, but we'll support as extension *)
  | Int of c_int_type                      (* All integer types *)
  | Float of c_float_type                  (* All floating point types *)
  | Pointer of c_type * type_qualifier list
  | Array of c_type * int option * type_qualifier list  (* type, size, qualifiers *)
  | Function of c_type option * param_type list * bool   (* return, params, varargs *)
  | Struct of string option * (string * c_type * type_qualifier list) list option  (* tag, fields *)
  | Union of string option * (string * c_type * type_qualifier list) list option   (* tag, fields *)
  | Enum of string option * (string * int64) list option                          (* tag, values *)
  | Typedef of string * c_type            (* typedef name and resolved type *)

and param_type = {
  param_type: c_type;
  param_quals: type_qualifier list;
  param_name: string option;
}

(** Standard C89 integer types *)
let c_char = { name = "char"; size = 1; signed = true; rank = 1 }
let c_uchar = { name = "unsigned char"; size = 1; signed = false; rank = 1 }
let c_short = { name = "short"; size = 2; signed = true; rank = 2 }
let c_ushort = { name = "unsigned short"; size = 2; signed = false; rank = 2 }
let c_int = { name = "int"; size = 4; signed = true; rank = 3 }
let c_uint = { name = "unsigned int"; size = 4; signed = false; rank = 3 }
let c_long = { name = "long"; size = 8; signed = true; rank = 4 }
let c_ulong = { name = "unsigned long"; size = 8; signed = false; rank = 4 }

(** Standard C89 floating point types *)
let c_float = { name = "float"; size = 4; precision = `Single }
let c_double = { name = "double"; size = 8; precision = `Double }

(** Type environment for typedefs and tags *)
type type_env = {
  typedefs: (string, c_type) Hashtbl.t;
  struct_tags: (string, (string * c_type * type_qualifier list) list) Hashtbl.t;
  union_tags: (string, (string * c_type * type_qualifier list) list) Hashtbl.t;
  enum_tags: (string, (string * int64) list) Hashtbl.t;
}

let create_type_env () = {
  typedefs = Hashtbl.create 32;
  struct_tags = Hashtbl.create 32;
  union_tags = Hashtbl.create 32;
  enum_tags = Hashtbl.create 32;
}

(** C89 Integer promotion rules *)
let integer_promote = function
  | Int int_type when int_type.rank < c_int.rank -> Int c_int
  | Int int_type when int_type.rank = c_int.rank && not int_type.signed -> 
    (* If unsigned int can represent all values, promote to int, else unsigned int *)
    Int c_uint
  | other -> other

(** Check if type is arithmetic *)
let is_arithmetic = function
  | Int _ | Float _ -> true
  | _ -> false

(** Check if type is integral *)
let is_integral = function
  | Int _ | Bool -> true
  | _ -> false

(** Check if type is scalar *)
let is_scalar = function
  | Int _ | Float _ | Bool | Pointer _ | Enum _ -> true
  | _ -> false

(** Check if type is complete (has known size) *)
let rec is_complete env = function
  | Void -> false
  | Bool | Int _ | Float _ -> true
  | Pointer _ -> true  (* Pointers are always complete *)
  | Array (_, None, _) -> false  (* Incomplete array *)
  | Array (elem_ty, Some _, _) -> is_complete env elem_ty
  | Function _ -> false  (* Functions are never complete *)
  | Struct (Some tag, None) -> 
    (* Forward declaration - check if defined *)
    Hashtbl.mem env.struct_tags tag
  | Struct (None, None) -> false  (* Anonymous struct without definition *)
  | Struct (_, Some fields) -> 
    List.for_all (fun (_, ty, _) -> is_complete env ty) fields
  | Union (Some tag, None) ->
    Hashtbl.mem env.union_tags tag
  | Union (None, None) -> false  (* Anonymous union without definition *)
  | Union (_, Some fields) ->
    List.for_all (fun (_, ty, _) -> is_complete env ty) fields
  | Enum _ -> true  (* Enums are always complete *)
  | Typedef (name, _) ->
    (* For typedefs, check the underlying type *)
    match Hashtbl.find_opt env.typedefs name with
    | Some underlying -> is_complete env underlying
    | None -> false

(** Type compatibility for C89 *)
let types_compatible env t1 t2 =
  let rec compatible t1 t2 =
    match t1, t2 with
    | Void, Void | Bool, Bool -> true
    | Int it1, Int it2 -> it1.size = it2.size && it1.signed = it2.signed
    | Float ft1, Float ft2 -> ft1.precision = ft2.precision
    | Pointer (pt1, q1), Pointer (pt2, q2) ->
      (* Pointer qualifiers must be compatible *)
      compatible pt1 pt2 && qualifiers_compatible q1 q2
    | Array (et1, s1, q1), Array (et2, s2, q2) ->
      compatible et1 et2 && sizes_compatible s1 s2 && qualifiers_compatible q1 q2
    | Function (rt1, p1, v1), Function (rt2, p2, v2) ->
      ret_types_compatible rt1 rt2 && params_compatible p1 p2 && v1 = v2
    | Struct (tag1, _), Struct (tag2, _) when tag1 = tag2 && tag1 <> None -> true
    | Union (tag1, _), Union (tag2, _) when tag1 = tag2 && tag1 <> None -> true
    | Enum (tag1, _), Enum (tag2, _) when tag1 = tag2 && tag1 <> None -> true
    | Typedef (n1, _), Typedef (n2, _) when n1 = n2 -> true
    | Typedef (n, _), other | other, Typedef (n, _) ->
      (* Resolve typedef and compare *)
      (match Hashtbl.find_opt env.typedefs n with
       | Some resolved -> compatible resolved other
       | None -> false)
    | _ -> false
  
  and qualifiers_compatible q1 q2 =
    (* For now, just check subset relationship - target must have at least source qualifiers *)
    List.for_all (fun q -> List.mem q q2) q1
  
  and sizes_compatible s1 s2 =
    match s1, s2 with
    | None, _ | _, None -> true  (* Incomplete arrays compatible with any *)
    | Some i1, Some i2 -> i1 = i2
  
  and ret_types_compatible rt1 rt2 =
    match rt1, rt2 with
    | None, None -> true
    | Some t1, Some t2 -> compatible t1 t2
    | _ -> false
  
  and params_compatible p1 p2 =
    List.length p1 = List.length p2 &&
    List.for_all2 (fun pt1 pt2 ->
      compatible pt1.param_type pt2.param_type &&
      qualifiers_compatible pt1.param_quals pt2.param_quals
    ) p1 p2
  in
  compatible t1 t2

(** Usual arithmetic conversions for C89 *)
let usual_arithmetic_conversions t1 t2 =
  let promote_both = (integer_promote t1, integer_promote t2) in
  match promote_both with
  (* If either operand is floating, convert both to floating *)
  | Float _, Int _it -> Float c_double, Float c_double
  | Int _it, Float _ -> Float c_double, Float c_double
  | Float f1, Float f2 when f1.precision = `Double || f2.precision = `Double ->
    Float c_double, Float c_double
  | Float _, Float _ -> Float c_float, Float c_float
  
  (* Both operands are integral after promotion *)
  | Int it1, Int it2 ->
    if it1.signed = it2.signed then
      (* Same signedness - promote to higher rank *)
      let target = if it1.rank >= it2.rank then it1 else it2 in
      Int target, Int target
    else
      (* Different signedness - follow C89 rules *)
      let unsigned, signed = if it1.signed then it2, it1 else it1, it2 in
      if unsigned.rank >= signed.rank then
        Int unsigned, Int unsigned
      else if signed.size > unsigned.size then
        Int signed, Int signed
      else
        (* Convert to unsigned version of signed type *)
        let unsigned_signed = { signed with signed = false } in
        Int unsigned_signed, Int unsigned_signed
  
  | Bool, Int it | Int it, Bool -> Int it, Int it
  | Bool, Bool -> Bool, Bool
  | _ -> t1, t2  (* Non-arithmetic types unchanged *)

(** Check if conversion is allowed *)
let conversion_allowed env from_ty to_ty =
  match from_ty, to_ty with
  (* Identity conversion *)
  | t1, t2 when types_compatible env t1 t2 -> true
  
  (* Arithmetic conversions *)
  | Int _, Int _ | Float _, Float _ | Bool, Int _ | Int _, Bool -> true
  | Int _, Float _ | Float _, Int _ -> true
  
  (* Pointer conversions *)
  | Pointer (Void, _), Pointer (_, _) | Pointer (_, _), Pointer (Void, _) -> true
  | Array (elem_ty, _, _), Pointer (ptr_ty, _) -> types_compatible env elem_ty ptr_ty
  | Function _, Pointer (Function _, _) -> true
  
  (* Null pointer constant to any pointer *)
  | Int _it, Pointer _ when true (* TODO: check if it's a null constant *) -> true
  
  | _ -> false

(** Convert AST type specifiers to resolved C type *)
let resolve_type_specs _env (specs : type_spec list) (quals : type_qualifier list) =
  let has_spec spec = List.mem spec specs in
  let _has_qual qual = List.mem qual quals in
  
  (* Check for void *)
  if has_spec Void then Void
  
  (* Check for struct/union/enum *)
  else match List.find_opt (function StructType _ | UnionType _ | EnumType _ -> true | _ -> false) specs with
  | Some (StructType (tag, fields)) ->
    (match fields with
     | Some _field_list -> 
       (* TODO: Process field declarations *)
       Struct (tag, Some [])  (* Simplified for now *)
     | None -> Struct (tag, None))
  | Some (UnionType (tag, fields)) ->
    (match fields with
     | Some _field_list ->
       Union (tag, Some [])  (* Simplified for now *)
     | None -> Union (tag, None))
  | Some (EnumType (tag, items)) ->
    (match items with
     | Some _enum_items ->
       (* TODO: Process enum items with values *)
       Enum (tag, Some [])  (* Simplified for now *)
     | None -> Enum (tag, None))
  | _ ->
    (* Integer/floating point types *)
    let is_signed = not (has_spec Unsigned) in
    let is_long = has_spec Long in
    let is_short = has_spec Short in
    let is_char = has_spec Char in
    let is_int = has_spec Int || (not is_char && not is_short && not is_long) in
    let is_float = has_spec Float in
    let is_double = has_spec Double in
    
    if is_float then Float c_float
    else if is_double then Float c_double
    else if is_char then Int (if is_signed then c_char else c_uchar)
    else if is_short then Int (if is_signed then c_short else c_ushort)
    else if is_long then Int (if is_signed then c_long else c_ulong)
    else if is_int then Int (if is_signed then c_int else c_uint)
    else Void  (* Default case - shouldn't happen with valid C *)

(** Process declarator to get full type *)
let rec process_declarator env base_type quals = function
  | DirectDecl (Ident name) -> base_type, name
  | DirectDecl (ArrayDecl (direct_decl, _size_expr)) ->
    let elem_type, name = process_declarator env base_type quals (DirectDecl direct_decl) in
    let size = None in  (* TODO: Evaluate size_expr *)
    Array (elem_type, size, quals), name
  | DirectDecl (FuncDecl (direct_decl, param_list)) ->
    let ret_type, name = process_declarator env base_type quals (DirectDecl direct_decl) in
    let params, varargs = match param_list with
      | Some (ParamList (_params, has_ellipsis)) ->
        (* TODO: Process parameter declarations *)
        [], has_ellipsis
      | None -> [], false
    in
    Function (Some ret_type, params, varargs), name
  | PointerDecl (ptr_quals, inner_decl) ->
    let inner_type, name = process_declarator env base_type quals inner_decl in
    Pointer (inner_type, ptr_quals), name
  | DirectDecl (ParenDecl inner_decl) ->
    process_declarator env base_type quals inner_decl

(** Convert PIR type to C type (for interfacing) *)
let rec c_type_to_pir_type = function
  | Void -> None  (* No PIR equivalent for void *)
  | Bool -> Some (Compilerkit_pir.Types.Scalar I1)
  | Int int_type when int_type.size = 1 -> Some (Compilerkit_pir.Types.Scalar I8)
  | Int int_type when int_type.size = 2 -> Some (Compilerkit_pir.Types.Scalar I16)
  | Int int_type when int_type.size = 4 -> Some (Compilerkit_pir.Types.Scalar I32)
  | Int int_type when int_type.size = 8 -> Some (Compilerkit_pir.Types.Scalar I64)
  | Float float_type when float_type.precision = `Single -> Some (Compilerkit_pir.Types.Scalar F32)
  | Float float_type when float_type.precision = `Double -> Some (Compilerkit_pir.Types.Scalar F64)
  | Pointer _ -> Some Compilerkit_pir.Types.Ptr
  | Array (elem_ty, Some size, _) ->
    (match c_type_to_pir_type elem_ty with
     | Some pir_elem_ty -> Some (Compilerkit_pir.Types.Array (size, pir_elem_ty))
     | None -> None)
  | Struct (_, Some fields) ->
    let pir_fields = List.filter_map (fun (_, ty, _) -> c_type_to_pir_type ty) fields in
    if List.length pir_fields = List.length fields then
      Some (Compilerkit_pir.Types.Struct pir_fields)
    else None
  | _ -> None  (* Incomplete or unsupported types *)

(** Pretty printing *)
let rec string_of_c_type = function
  | Void -> "void"
  | Bool -> "_Bool"
  | Int int_type -> int_type.name
  | Float float_type -> float_type.name
  | Pointer (ty, quals) ->
    let qual_str = String.concat " " (List.map (function Const -> "const" | Volatile -> "volatile") quals) in
    Printf.sprintf "%s%s*" (string_of_c_type ty) (if qual_str = "" then "" else " " ^ qual_str)
  | Array (ty, size, quals) ->
    let size_str = match size with Some s -> string_of_int s | None -> "" in
    let qual_str = String.concat " " (List.map (function Const -> "const" | Volatile -> "volatile") quals) in
    Printf.sprintf "%s%s[%s]" (string_of_c_type ty) (if qual_str = "" then "" else " " ^ qual_str) size_str
  | Function (ret_ty, params, varargs) ->
    let ret_str = match ret_ty with Some ty -> string_of_c_type ty | None -> "void" in
    let param_strs = List.map (fun p -> string_of_c_type p.param_type) params in
    let all_params = if varargs then param_strs @ ["..."] else param_strs in
    Printf.sprintf "%s(%s)" ret_str (String.concat ", " all_params)
  | Struct (tag, _) ->
    (match tag with Some t -> "struct " ^ t | None -> "struct <anonymous>")
  | Union (tag, _) ->
    (match tag with Some t -> "union " ^ t | None -> "union <anonymous>")
  | Enum (tag, _) ->
    (match tag with Some t -> "enum " ^ t | None -> "enum <anonymous>")
  | Typedef (name, _) -> name