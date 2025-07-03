(* PIR Value System *)

open Types

(* Unique identifier for values *)
type value_id = int

(* Value representation *)
type value = {
  id: value_id;
  ty: ty;
  attrs: Attributes.t;
}

(* Constant values *)
type const_value =
  | ConstInt of int64 * scalar_ty  (* Integer constants with type *)
  | ConstFloat of float * scalar_ty (* Float constants with type *)
  | ConstBool of bool              (* i1 boolean constants *)
  | ConstNull                      (* Null pointer constant *)
  | ConstUndef of ty               (* Undefined value of given type *)
  | ConstZero of ty                (* Zero initializer *)
  | ConstArray of const_value list (* Array constant *)
  | ConstStruct of const_value list (* Struct constant *)

(* Value counter for generating unique IDs *)
let value_counter = ref 0

(* Create a new value with unique ID *)
let create_value ty attrs =
  incr value_counter;
  { id = !value_counter; ty; attrs }

(* Create value with empty attributes *)
let create_simple_value ty = 
  create_value ty (Attributes.empty ())

(* Get the type of a constant value *)
let rec type_of_const = function
  | ConstInt (_, scalar_ty) -> Scalar scalar_ty
  | ConstFloat (_, scalar_ty) -> Scalar scalar_ty  
  | ConstBool _ -> Scalar I1 (* Assuming i1 for booleans *)
  | ConstNull -> Ptr
  | ConstUndef ty -> ty
  | ConstZero ty -> ty
  | ConstArray values -> 
    (match values with
     | [] -> failwith "Empty array constant"
     | v :: _ -> Array (List.length values, type_of_const v))
  | ConstStruct values ->
    Struct (List.map type_of_const values)

(* Value utilities *)
let get_id value = value.id
let get_type value = value.ty  
let get_attrs value = value.attrs

(* Update value attributes *)
let with_attrs value new_attrs = 
  { value with attrs = new_attrs }

let add_attr key json_value value =
  let new_attrs = Attributes.add key json_value value.attrs in
  { value with attrs = new_attrs }

(* Type checking *)
let has_type value expected_ty = 
  Types.equal value.ty expected_ty

let is_integer_value value = Types.is_integer value.ty
let is_float_value value = Types.is_float value.ty
let is_pointer_value value = value.ty = Ptr
let is_vector_value value = Types.is_vector value.ty

(* Pretty printing *)
let rec string_of_const_value = function
  | ConstInt (i, _) -> Int64.to_string i
  | ConstFloat (f, _) -> string_of_float f
  | ConstBool true -> "true"
  | ConstBool false -> "false"  
  | ConstNull -> "null"
  | ConstUndef _ -> "undef"
  | ConstZero _ -> "zeroinitializer"
  | ConstArray values ->
    let value_strs = List.map string_of_const_value values in
    Printf.sprintf "[%s]" (String.concat ", " value_strs)
  | ConstStruct values ->
    let value_strs = List.map string_of_const_value values in
    Printf.sprintf "{%s}" (String.concat ", " value_strs)

let string_of_value value =
  (* For spec compliance, just use the numeric id without % or type *)
  Printf.sprintf "v%d" value.id

(* Validation *)
let validate_const_value const_val expected_ty =
  Types.equal (type_of_const const_val) expected_ty

(* Helper constructors for common constants *)
module Const = struct
  let i8 i = ConstInt (Int64.of_int i, I8)
  let i16 i = ConstInt (Int64.of_int i, I16)  
  let i32 i = ConstInt (Int64.of_int i, I32)
  let i64 i = ConstInt (i, I64)
  
  let f32 f = ConstFloat (f, F32)
  let f64 f = ConstFloat (f, F64)
  
  let bool b = ConstBool b
  let null_ptr = ConstNull
  
  let zero ty = ConstZero ty
  let undef ty = ConstUndef ty
  
  let array values = ConstArray values
  let struct_val values = ConstStruct values
end