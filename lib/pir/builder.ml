(* PIR Builder - Monadic interface for constructing PIR CFGs *)

open Types
open Values
open Instructions
open Attributes

(* Builder state *)
type builder_state = {
  current_block: string;
  blocks: basic_block list;
  current_instructions: instruction list;
  next_var_id: int;
  value_names: (string, value) Hashtbl.t;
  block_params: (string * ty) list;
}

(* Builder monad *)
type 'a builder = builder_state -> (builder_state * 'a)

(* Monad operations *)
let return x state = (state, x)

let bind m f state =
  let (state', x) = m state in
  f x state'

let (>>=) = bind

let (let*) = bind

let map f m = m >>= fun x -> return (f x)

let (>|=) m f = map f m

(* State manipulation *)
let get_state state = (state, state)

let set_state new_state _state = (new_state, ())

let modify_state f state = (f state, ())

(* Builder initialization *)
let init_builder entry_label =
  {
    current_block = entry_label;
    blocks = [];
    current_instructions = [];
    next_var_id = 0;
    value_names = Hashtbl.create 64;
    block_params = [];
  }

(* Variable naming *)
let fresh_var_name prefix =
  get_state >>= fun state ->
  let id = state.next_var_id in
  modify_state (fun s -> { s with next_var_id = id + 1 }) >>= fun () ->
  return (Printf.sprintf "%s%d" prefix id)

let define_value name value =
  modify_state (fun state ->
    Hashtbl.replace state.value_names name value;
    state)

let lookup_value name =
  get_state >>= fun state ->
  match Hashtbl.find_opt state.value_names name with
  | Some v -> return v
  | None -> failwith (Printf.sprintf "Undefined value: %s" name)

(* Block management *)
let set_current_block label =
  get_state >>= fun state ->
  (* Save current block if it has instructions *)
  let state' = 
    if state.current_instructions <> [] then
      let block = create_block state.current_block state.block_params
        (List.rev state.current_instructions) Unreachable in
      { state with blocks = block :: state.blocks }
    else state in
  set_state { state' with 
    current_block = label;
    current_instructions = [];
    block_params = [] }

let add_block_param name ty =
  modify_state (fun state ->
    { state with block_params = (name, ty) :: state.block_params })

let finalize_block terminator =
  get_state >>= fun state ->
  let block = create_block state.current_block state.block_params
    (List.rev state.current_instructions) terminator in
  modify_state (fun s -> 
    { s with 
      blocks = block :: s.blocks;
      current_instructions = [] })

(* Instruction building *)
let emit_instr ?result instr attrs =
  let* result_value = 
    match result with
    | Some name ->
      let* ty = match result_type_of_instr instr with
        | Some t -> return t
        | None -> failwith "Instruction does not produce a result" in
      let value = create_simple_value ty in
      let* () = define_value name value in
      return (Some value)
    | None -> return None in
  
  let instruction = create_instruction ?result:result_value instr attrs in
  modify_state (fun state ->
    { state with current_instructions = instruction :: state.current_instructions }) >>= fun () ->
  match result_value with
  | Some v -> return v
  | None -> return (create_simple_value (Scalar I32)) (* Dummy for void results *)

(* Common instruction builders *)
let add ?flag ?(attrs=empty ()) result v1 v2 =
  emit_instr ~result (Binop (Add, Option.value flag ~default:NoFlag, v1, v2)) attrs

let sub ?flag ?(attrs=empty ()) result v1 v2 =
  emit_instr ~result (Binop (Sub, Option.value flag ~default:NoFlag, v1, v2)) attrs

let mul ?flag ?(attrs=empty ()) result v1 v2 =
  emit_instr ~result (Binop (Mul, Option.value flag ~default:NoFlag, v1, v2)) attrs

let icmp ?(attrs=empty ()) result pred v1 v2 =
  emit_instr ~result (Icmp (pred, v1, v2)) attrs

let load ?(attrs=empty ()) result ty _ptr =
  emit_instr ~result (Memory (Load ty)) attrs

let store ?(attrs=empty ()) value ptr =
  emit_instr (Memory (Store (value, ptr))) attrs >>= fun _ ->
  return ()

let alloca ?(attrs=empty ()) result size align =
  emit_instr ~result (Memory (Alloca (size, align))) attrs

let gep ?(attrs=empty ()) result base idx =
  emit_instr ~result (Address (Gep (base, idx))) attrs

let select ?(attrs=empty ()) result cond v_true v_false =
  emit_instr ~result (Select (cond, v_true, v_false)) attrs

let call ?(attrs=empty ()) result callee args =
  emit_instr ?result (Call (Call (callee, args))) attrs

let const_int ty _value =
  return (create_simple_value ty)

let const_bool b =
  const_int (Scalar I1) (if b then 1 else 0)

(* Control flow builders *)
let ret value =
  finalize_block (Ret value)

let br cond then_label else_label =
  finalize_block (Br (cond, then_label, else_label))

let jmp label =
  finalize_block (Jmp label)

let unreachable () =
  finalize_block Unreachable

(* Function building *)
let build_function name params return_ty builder_body =
  let init_state = init_builder "entry" in
  
  (* Add parameters to value table *)
  let add_params state =
    List.fold_left (fun s (pname, pty) ->
      let value = create_simple_value pty in
      Hashtbl.add s.value_names pname value;
      s) state params in
  
  let init_state' = add_params init_state in
  let (final_state, _) = builder_body init_state' in
  
  (* Create function with blocks in correct order *)
  let blocks = List.rev final_state.blocks in
  create_func name params return_ty blocks

(* Convenience functions for running builders *)
let run_builder builder initial_state =
  let (final_state, result) = builder initial_state in
  (final_state, result)

let extract_blocks builder =
  let (final_state, _) = builder (init_builder "entry") in
  List.rev final_state.blocks