(* PIR Type System - Based on portable_ir_spec.md *)

(* Primitive scalar types *)
type scalar_ty = 
  | I1 | I8 | I16 | I32 | I64 (* Two's complement integers *)
  | F32 | F64                 (* IEEE-754 floating point *)

(* Main type representation *)
type ty = 
  | Scalar of scalar_ty       (* Basic scalar types *)
  | Vector of int * scalar_ty (* v<N>x<ScalarTy> packed vectors *)
  | Array of int * ty         (* array[N]T fixed-size arrays *)
  | Struct of ty list         (* struct<<T1, T2, ...>> natural layout *)
  | PackedStruct of ty list   (* packed_struct<<...>> no padding *)
  | Ptr                       (* Opaque untyped pointer *)

(* Type utilities *)
let rec size_of = function
  | Scalar I1 -> 1   (* i1 stored as byte *)
  | Scalar I8 -> 1
  | Scalar I16 -> 2  
  | Scalar I32 -> 4
  | Scalar I64 -> 8
  | Scalar F32 -> 4
  | Scalar F64 -> 8
  | Vector (n, scalar) -> n * (size_of (Scalar scalar))
  | Array (n, ty) -> n * (size_of ty)
  | Struct tys -> 
    (* Natural layout with alignment *)
    let align_to alignment offset = 
      (offset + alignment - 1) land (lnot (alignment - 1)) in
    let folder (offset, max_align) ty =
      let ty_size = size_of ty in
      let ty_align = alignment_of ty in
      let aligned_offset = align_to ty_align offset in
      (aligned_offset + ty_size, max max_align ty_align) in
    let (total_size, max_align) = List.fold_left folder (0, 1) tys in
    align_to max_align total_size
  | PackedStruct tys ->
    (* No padding - just sum of sizes *)
    List.fold_left (fun acc ty -> acc + (size_of ty)) 0 tys
  | Ptr -> 8 (* Assume 64-bit pointers *)

and alignment_of = function
  | Scalar I1 -> 1
  | Scalar I8 -> 1
  | Scalar I16 -> 2
  | Scalar I32 -> 4  
  | Scalar I64 -> 8
  | Scalar F32 -> 4
  | Scalar F64 -> 8
  | Vector (_, scalar) -> alignment_of (Scalar scalar)
  | Array (_, ty) -> alignment_of ty
  | Struct tys ->
    (* Natural alignment = max field alignment, power of 2 *)
    let max_align = List.fold_left (fun acc ty -> 
      max acc (alignment_of ty)) 1 tys in
    (* Round up to next power of 2 *)
    let rec next_pow2 n acc = if acc >= n then acc else next_pow2 n (acc * 2) in
    next_pow2 max_align 1
  | PackedStruct _ -> 1 (* Always byte-aligned *)
  | Ptr -> 8

(* Type equality *)
let rec equal t1 t2 = match (t1, t2) with
  | (Scalar s1, Scalar s2) -> s1 = s2
  | (Vector (n1, s1), Vector (n2, s2)) -> n1 = n2 && s1 = s2
  | (Array (n1, t1), Array (n2, t2)) -> n1 = n2 && equal t1 t2
  | (Struct ts1, Struct ts2) | (PackedStruct ts1, PackedStruct ts2) ->
    (try List.for_all2 equal ts1 ts2 with Invalid_argument _ -> false)
  | (Ptr, Ptr) -> true
  | _ -> false

(* Type validation *)
let is_integer = function
  | Scalar (I1 | I8 | I16 | I32 | I64) -> true
  | _ -> false

let is_float = function
  | Scalar (F32 | F64) -> true
  | _ -> false

let is_vector = function
  | Vector _ -> true
  | _ -> false

let is_aggregate = function
  | Array _ | Struct _ | PackedStruct _ -> true
  | _ -> false

(* Pretty printing *)
let string_of_scalar = function
  | I1 -> "i1" | I8 -> "i8" | I16 -> "i16" | I32 -> "i32" | I64 -> "i64"
  | F32 -> "f32" | F64 -> "f64"

let rec string_of_ty = function
  | Scalar s -> string_of_scalar s
  | Vector (n, s) -> Printf.sprintf "v%dx%s" n (string_of_scalar s)
  | Array (n, ty) -> Printf.sprintf "array[%d]%s" n (string_of_ty ty)
  | Struct tys -> 
    let ty_strs = List.map string_of_ty tys in
    Printf.sprintf "struct<<%s>>" (String.concat ", " ty_strs)
  | PackedStruct tys ->
    let ty_strs = List.map string_of_ty tys in
    Printf.sprintf "packed_struct<<%s>>" (String.concat ", " ty_strs)
  | Ptr -> "ptr"

(* Get size of scalar type only *)
let size_of_scalar = function
  | I1 -> 1
  | I8 -> 1  
  | I16 -> 2
  | I32 -> 4
  | I64 -> 8
  | F32 -> 4
  | F64 -> 8