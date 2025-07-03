(* SSA Transformation using Block Parameters (PIR style) *)

open Compilerkit_pir

module VarMap = Map.Make(String)
module BlockMap = Map.Make(String)
module VarSet = Set.Make(String)
module ValueMap = Map.Make(Int)

type rename_state = {
  mutable counter: int;  (* For generating fresh SSA values *)
  mutable value_map: Values.value ValueMap.t;  (* old value id -> new value *)
  mutable block_params: (string * Types.ty) list BlockMap.t;  (* block -> parameters *)
  mutable block_args: Values.value list BlockMap.t BlockMap.t;  (* pred -> succ -> args *)
  dom_info: Dominance.dominance_info;
}

(* Generate a fresh SSA value *)
let fresh_value (state: rename_state) (ty: Types.ty) : Values.value =
  Values.create_simple_value ty

(* Get or create renamed value *)
let get_renamed_value (state: rename_state) (value: Values.value) : Values.value =
  let id = Values.get_id value in
  match ValueMap.find_opt id state.value_map with
  | Some new_value -> new_value
  | None -> 
    (* Create new SSA value *)
    let new_value = fresh_value state (Values.get_type value) in
    state.value_map <- ValueMap.add id new_value state.value_map;
    new_value

(* Rename instruction *)
let rename_instruction (state: rename_state) (inst: Instructions.instruction) : Instructions.instruction =
  let rename_value v = get_renamed_value state v in
  
  let new_instr = match inst.instr with
    | Instructions.Binop (op, flag, v1, v2) ->
      Instructions.Binop (op, flag, rename_value v1, rename_value v2)
    
    | Instructions.Icmp (pred, v1, v2) ->
      Instructions.Icmp (pred, rename_value v1, rename_value v2)
    
    | Instructions.Fcmp (pred, v1, v2) ->
      Instructions.Fcmp (pred, rename_value v1, rename_value v2)
    
    | Instructions.Select (cond, v_true, v_false) ->
      Instructions.Select (rename_value cond, rename_value v_true, rename_value v_false)
    
    | Instructions.Memory memop ->
      let new_memop = match memop with
        | Instructions.Load ty -> Instructions.Load ty
        | Instructions.Store (value, ptr) ->
          Instructions.Store (rename_value value, rename_value ptr)
        | Instructions.Alloca (size, align) ->
          Instructions.Alloca (rename_value size, align)
        | Instructions.Memcpy (dst, src, bytes) ->
          Instructions.Memcpy (rename_value dst, rename_value src, rename_value bytes)
        | Instructions.Memset (dst, byte, bytes) ->
          Instructions.Memset (rename_value dst, rename_value byte, rename_value bytes)
      in
      Instructions.Memory new_memop
    
    | Instructions.Address addrop ->
      let new_addrop = match addrop with
        | Instructions.Gep (base, idx) ->
          Instructions.Gep (rename_value base, rename_value idx)
        | Instructions.FieldAddr (base, field) ->
          Instructions.FieldAddr (rename_value base, field)
        | Instructions.PtrAdd (base, offset) ->
          Instructions.PtrAdd (rename_value base, rename_value offset)
      in
      Instructions.Address new_addrop
    
    | Instructions.Cast castop ->
      let new_castop = match castop with
        | Instructions.Bitcast v -> Instructions.Bitcast (rename_value v)
        | Instructions.Trunc (v, ty) -> Instructions.Trunc (rename_value v, ty)
        | Instructions.Zext (v, ty) -> Instructions.Zext (rename_value v, ty)
        | Instructions.Sext (v, ty) -> Instructions.Sext (rename_value v, ty)
        | Instructions.Fptrunc (v, ty) -> Instructions.Fptrunc (rename_value v, ty)
        | Instructions.Fpext (v, ty) -> Instructions.Fpext (rename_value v, ty)
        | Instructions.Fptoui (v, ty) -> Instructions.Fptoui (rename_value v, ty)
        | Instructions.Fptosi (v, ty) -> Instructions.Fptosi (rename_value v, ty)
        | Instructions.Uitofp (v, ty) -> Instructions.Uitofp (rename_value v, ty)
        | Instructions.Sitofp (v, ty) -> Instructions.Sitofp (rename_value v, ty)
      in
      Instructions.Cast new_castop
    
    | Instructions.Vector vecop ->
      let new_vecop = match vecop with
        | Instructions.Splat (v, lanes) ->
          Instructions.Splat (rename_value v, lanes)
        | Instructions.Shuffle (v1, v2, mask) ->
          Instructions.Shuffle (rename_value v1, rename_value v2, mask)
        | Instructions.ExtractLane (v, idx) ->
          Instructions.ExtractLane (rename_value v, idx)
        | Instructions.InsertLane (v, idx, scalar) ->
          Instructions.InsertLane (rename_value v, idx, rename_value scalar)
      in
      Instructions.Vector new_vecop
    
    | Instructions.Call callop ->
      let new_callop = match callop with
        | Call (callee, args) ->
          let new_callee = rename_value callee in
          let new_args = List.map rename_value args in
          Call (new_callee, new_args)
        | TailCall (callee, args) ->
          let new_callee = rename_value callee in
          let new_args = List.map rename_value args in
          TailCall (new_callee, new_args)
      in
      Instructions.Call new_callop
    
    | Instructions.Phi operands ->
      Instructions.Phi (List.map (fun (v, label) -> (rename_value v, label)) operands)
    
    | Instructions.Const _ as c -> c
  in
  
  (* Create new result if needed *)
  let new_result = match inst.result with
    | Some old_result ->
      let new_result = fresh_value state (Values.get_type old_result) in
      state.value_map <- ValueMap.add (Values.get_id old_result) new_result state.value_map;
      Some new_result
    | None -> None
  in
  
  { inst with instr = new_instr; result = new_result }

(* Rename terminator *)
let rename_terminator (state: rename_state) (term: Instructions.terminator) : Instructions.terminator =
  let rename_value v = get_renamed_value state v in
  
  match term with
  | Instructions.Ret (Some v) -> Instructions.Ret (Some (rename_value v))
  | Instructions.Br (cond, then_lbl, else_lbl) ->
    Instructions.Br (rename_value cond, then_lbl, else_lbl)
  | Instructions.Switch (value, default, cases) ->
    Instructions.Switch (rename_value value, default, cases)
  | _ -> term

(* Identify join points that need block parameters *)
let find_join_points (func: func) : BlockMap.key list =
  let join_points = ref [] in
  
  List.iter (fun block ->
    let preds = Dominance.get_predecessors func block.label in
    if List.length preds > 1 then
      join_points := block.label :: !join_points
  ) func.blocks;
  
  !join_points

(* Determine which values need block parameters at join points *)
let compute_block_parameters (func: func) (join_points: string list) : (string * Types.ty) list BlockMap.t =
  let block_params = ref BlockMap.empty in
  
  (* For simplicity, we'll add parameters for values used in the block *)
  (* In a real implementation, we'd do a more sophisticated analysis *)
  List.iter (fun block_label ->
    let block = List.find (fun b -> b.label = block_label) func.blocks in
    let params = ref [] in
    
    (* Collect values used in this block *)
    List.iter (fun inst ->
      match inst.instr with
      | Instructions.Binop (_, _, v1, v2) ->
        (* In real SSA, we'd check if these are defined in different predecessors *)
        ()
      | _ -> ()
    ) block.instructions;
    
    if !params <> [] then
      block_params := BlockMap.add block_label !params !block_params
  ) join_points;
  
  !block_params

(* Transform function to SSA form *)
let transform_to_ssa (func: func) : func =
  (* Compute dominance information *)
  let dom_info = Dominance.compute_dominance_info func in
  
  (* Find join points *)
  let join_points = find_join_points func in
  
  (* Compute block parameters *)
  let block_params = compute_block_parameters func join_points in
  
  (* Initialize rename state *)
  let state = {
    counter = 0;
    value_map = ValueMap.empty;
    block_params;
    block_args = BlockMap.empty;
    dom_info;
  } in
  
  (* Rename all blocks *)
  let new_blocks = List.map (fun block ->
    (* Get block parameters *)
    let params = 
      match BlockMap.find_opt block.label block_params with
      | Some ps -> ps
      | None -> []
    in
    
    (* Rename instructions *)
    let new_instructions = List.map (rename_instruction state) block.instructions in
    
    (* Rename terminator *)
    let new_terminator = rename_terminator state block.terminator in
    
    { block with 
      params = params;
      instructions = new_instructions;
      terminator = new_terminator }
  ) func.blocks in
  
  { func with blocks = new_blocks }

(* Pretty print SSA transformation statistics *)
let pp_ssa_stats (original: func) (transformed: func) : string =
  let count_values func =
    let values = ref 0 in
    List.iter (fun block ->
      List.iter (fun inst ->
        match inst.result with
        | Some _ -> incr values
        | None -> ()
      ) block.instructions
    ) func.blocks;
    !values
  in
  
  let original_values = count_values original in
  let ssa_values = count_values transformed in
  
  let count_block_params func =
    List.fold_left (fun acc block ->
      acc + List.length block.params
    ) 0 func.blocks
  in
  
  let block_params = count_block_params transformed in
  
  Printf.sprintf "SSA Transformation Statistics:\n  Original values: %d\n  SSA values: %d\n  Block parameters: %d\n  Increase: %.1f%%"
    original_values ssa_values block_params
    ((float_of_int (ssa_values - original_values) /. float_of_int original_values) *. 100.0)