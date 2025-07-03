(* PIR Instruction System - Based on portable_ir_spec.md *)

open Types
open Values

(* Instruction flags for overflow/precision control *)
type instr_flag = 
  | NoFlag        (* Default wrapping behavior *)
  | Nsw           (* No signed wrap - UB on overflow *)
  | Carry         (* Produces (result, carry) tuple *)
  | Sat           (* Saturates to min/max on overflow *)

(* Comparison predicates *)
type icmp_pred = 
  | Eq | Ne | Slt | Sle | Sgt | Sge | Ult | Ule | Ugt | Uge

type fcmp_pred = 
  | Oeq | Ogt | Oge | Olt | Ole | One | Ord | Ueq | Ugt | Uge | Ult | Ule | Une | Uno

(* Binary operations *)
type binop = 
  (* Integer arithmetic *)
  | Add | Sub | Mul | Sdiv | Udiv | Srem | Urem
  (* Bitwise operations *)  
  | And | Or | Xor | Shl | Lshr | Ashr | Rol | Ror
  (* Float arithmetic *)
  | Fadd | Fsub | Fmul | Fdiv | Frem | Fma
  (* Population/bit counting *)
  | Clz | Ctz | Popcnt

(* Memory operations *)
type memop = 
  | Load of ty                           (* load.Ty [ptr] *)
  | Store of value * value               (* store.Ty val, [ptr] *)
  | Alloca of value * int                (* alloca size align *)
  | Memcpy of value * value * value      (* memcpy dst, src, bytes *)
  | Memset of value * value * value      (* memset dst, byte, bytes *)

(* Address calculation *)
type addrop = 
  | Gep of value * value                 (* gep base, idx - array element *)
  | FieldAddr of value * int             (* fieldaddr base, k - struct field *)
  | PtrAdd of value * value              (* ptradd base, offset - raw bytes *)

(* Conversion operations *)
type castop = 
  | Bitcast of value                     (* reinterpret bits *)
  | Trunc of value * ty                  (* truncate to smaller type *)
  | Zext of value * ty                   (* zero extend *)
  | Sext of value * ty                   (* sign extend *)
  | Fptrunc of value * ty                (* float truncate *)
  | Fpext of value * ty                  (* float extend *)
  | Fptoui of value * ty                 (* float to unsigned int *)
  | Fptosi of value * ty                 (* float to signed int *)
  | Uitofp of value * ty                 (* unsigned int to float *)
  | Sitofp of value * ty                 (* signed int to float *)

(* Vector operations *)
type vecop = 
  | Splat of value * int                 (* splat scalar, lanes *)
  | Shuffle of value * value * int list  (* shuffle vA, vB, mask *)
  | ExtractLane of value * int           (* extractlane v, idx *)
  | InsertLane of value * int * value    (* insertlane v, idx, scalar *)

(* Control flow operations *)
type terminator = 
  | Ret of value option                  (* ret [val] *)
  | Br of value * string * string        (* br cond, then_label, else_label *)
  | Jmp of string                        (* jmp label *)
  | Switch of value * string * (const_value * string) list (* switch val, default, cases *)
  | Unreachable                          (* unreachable *)

(* Call operations *)
type callop = 
  | Call of value * value list           (* call.retTy callee, args *)
  | TailCall of value * value list       (* tailcall.retTy callee, args *)

(* Main instruction type *)
type instr = 
  | Binop of binop * instr_flag * value * value   (* Binary operations *)
  | Icmp of icmp_pred * value * value              (* Integer comparison *)
  | Fcmp of fcmp_pred * value * value              (* Float comparison *)
  | Select of value * value * value                (* select cond, vtrue, vfalse *)
  | Memory of memop                                (* Memory operations *)
  | Address of addrop                              (* Address calculation *)
  | Cast of castop                                 (* Type conversions *)
  | Vector of vecop                                (* Vector operations *)
  | Call of callop                                 (* Function calls *)
  | Phi of (value * string) list                   (* SSA phi node *)
  | Const of const_value                           (* Constant values *)

(* Instruction with result binding *)
type instruction = {
  result: value option;      (* Optional result value *)
  instr: instr;             (* The instruction *)
  attrs: Attributes.t;      (* Instruction attributes *)
}

(* Basic block structure *)
type basic_block = {
  label: string;                        (* Block label *)
  params: (string * ty) list;           (* Block parameters for SSA *)
  instructions: instruction list;       (* List of instructions *)
  terminator: terminator;               (* Block terminator *)
  attrs: Attributes.t;                  (* Block attributes *)
}

(* Function representation *)
type func = {
  name: string;                         (* Function name *)
  params: (string * ty) list;           (* Function parameters *)
  return_ty: ty option;                 (* Return type (None = void) *)
  blocks: basic_block list;             (* Basic blocks *)
  attrs: Attributes.t;                  (* Function attributes *)
}

(* Utility functions *)
let create_instruction ?result instr attrs = 
  { result; instr; attrs }

let create_simple_instruction ?result instr = 
  create_instruction ?result instr (Attributes.empty ())

let create_block label params instructions terminator = 
  { label; params; instructions; terminator; attrs = Attributes.empty () }

let create_func name params return_ty blocks = 
  { name; params; return_ty; blocks; attrs = Attributes.empty () }

(* Type checking utilities *)
let result_type_of_instr = function
  | Binop (_, _, v1, _) -> Some (get_type v1)  (* Same type as operands *)
  | Icmp _ -> Some (Scalar I1)                 (* Boolean result *)
  | Fcmp _ -> Some (Scalar I1)                 (* Boolean result *)
  | Select (_, v_true, _) -> Some (get_type v_true)  (* Type of selected values *)
  | Memory (Load ty) -> Some ty                (* Loaded type *)
  | Memory (Store _) -> None                   (* No result *)
  | Memory (Alloca _) -> Some Ptr              (* Pointer result *)
  | Memory (Memcpy _) -> None                  (* No result *)
  | Memory (Memset _) -> None                  (* No result *)
  | Address _ -> Some Ptr                      (* Address operations return pointers *)
  | Cast (Bitcast v) -> Some (get_type v)      (* Same size, different interpretation *)
  | Cast (Trunc (_, ty)) -> Some ty           (* Target type *)
  | Cast (Zext (_, ty)) -> Some ty            (* Target type *)
  | Cast (Sext (_, ty)) -> Some ty            (* Target type *)
  | Cast (Fptrunc (_, ty)) -> Some ty         (* Target type *)
  | Cast (Fpext (_, ty)) -> Some ty           (* Target type *)
  | Cast (Fptoui (_, ty)) -> Some ty          (* Target type *)
  | Cast (Fptosi (_, ty)) -> Some ty          (* Target type *)
  | Cast (Uitofp (_, ty)) -> Some ty          (* Target type *)
  | Cast (Sitofp (_, ty)) -> Some ty          (* Target type *)
  | Vector (Splat (_, _)) -> 
    None (* Would need scalar type to determine vector type *)
  | Vector (Shuffle (v1, _, _)) -> Some (get_type v1) (* Same type as first vector *)
  | Vector (ExtractLane (v, _)) -> 
    (match get_type v with
     | Vector (_, scalar) -> Some (Scalar scalar)
     | _ -> None)
  | Vector (InsertLane (v, _, _)) -> Some (get_type v)
  | Call _ | Const _ -> None (* Type depends on specific context *)
  | Phi ((v, _) :: _) -> Some (get_type v)     (* All phi values same type *)
  | Phi [] -> None

(* Pretty printing *)
let string_of_flag = function
  | NoFlag -> ""
  | Nsw -> ".nsw"
  | Carry -> ".carry" 
  | Sat -> ".sat"

let string_of_icmp_pred = function
  | Eq -> "eq" | Ne -> "ne" | Slt -> "slt" | Sle -> "sle"
  | Sgt -> "sgt" | Sge -> "sge" | Ult -> "ult" | Ule -> "ule"
  | Ugt -> "ugt" | Uge -> "uge"

let string_of_binop = function
  | Add -> "add" | Sub -> "sub" | Mul -> "mul"
  | Sdiv -> "sdiv" | Udiv -> "udiv" | Srem -> "srem" | Urem -> "urem"
  | And -> "and" | Or -> "or" | Xor -> "xor" 
  | Shl -> "shl" | Lshr -> "lshr" | Ashr -> "ashr"
  | Rol -> "rol" | Ror -> "ror"
  | Fadd -> "fadd" | Fsub -> "fsub" | Fmul -> "fmul"
  | Fdiv -> "fdiv" | Frem -> "frem" | Fma -> "fma"
  | Clz -> "clz" | Ctz -> "ctz" | Popcnt -> "popcnt"

let string_of_instr instr = 
  match instr with
  | Binop (op, flag, v1, v2) ->
    Printf.sprintf "%s%s %s, %s" 
      (string_of_binop op) (string_of_flag flag)
      (string_of_value v1) (string_of_value v2)
  | Icmp (pred, v1, v2) -> 
    Printf.sprintf "icmp.%s %s, %s" 
      (string_of_icmp_pred pred) (string_of_value v1) (string_of_value v2)
  | Const const_val -> string_of_const_value const_val
  | _ -> "..." (* TODO: Add remaining instruction pretty printing *)

let string_of_terminator = function
  | Ret None -> "ret"
  | Ret (Some v) -> Printf.sprintf "ret %s" (string_of_value v)
  | Br (cond, then_lbl, else_lbl) -> 
    Printf.sprintf "br %s, %s, %s" (string_of_value cond) then_lbl else_lbl
  | Jmp lbl -> Printf.sprintf "jmp %s" lbl
  | Unreachable -> "unreachable"
  | Switch _ -> "switch ..." (* TODO: Complete switch pretty printing *)