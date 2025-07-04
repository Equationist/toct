(* SSA Transformation using Block Parameters (PIR style) *)

open Compilerkit_pir
module I = Instructions

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
let fresh_value (_state: rename_state) (ty: Types.ty) : Values.value =
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
let rename_instruction (state: rename_state) (inst: I.instruction) : I.instruction =
  let rename_value v = get_renamed_value state v in
  
  let new_instr = match inst.I.instr with
    | I.Binop (op, flag, v1, v2) ->
      I.Binop (op, flag, rename_value v1, rename_value v2)
    
    | I.Icmp (pred, v1, v2) ->
      I.Icmp (pred, rename_value v1, rename_value v2)
    
    | I.Fcmp (pred, v1, v2) ->
      I.Fcmp (pred, rename_value v1, rename_value v2)
    
    | I.Select (cond, v_true, v_false) ->
      I.Select (rename_value cond, rename_value v_true, rename_value v_false)
    
    | I.Memory memop ->
      let new_memop = match memop with
        | I.Load (ty, ptr) -> I.Load (ty, rename_value ptr)
        | I.Store (value, ptr) ->
          I.Store (rename_value value, rename_value ptr)
        | I.Alloca (size, align) ->
          I.Alloca (rename_value size, align)
        | I.Memcpy (dst, src, bytes) ->
          I.Memcpy (rename_value dst, rename_value src, rename_value bytes)
        | I.Memset (dst, byte, bytes) ->
          I.Memset (rename_value dst, rename_value byte, rename_value bytes)
      in
      I.Memory new_memop
    
    | I.Address addrop ->
      let new_addrop = match addrop with
        | I.Gep (base, idx) ->
          I.Gep (rename_value base, rename_value idx)
        | I.FieldAddr (base, field) ->
          I.FieldAddr (rename_value base, field)
        | I.PtrAdd (base, offset) ->
          I.PtrAdd (rename_value base, rename_value offset)
      in
      I.Address new_addrop
    
    | I.Cast castop ->
      let new_castop = match castop with
        | I.Bitcast v -> I.Bitcast (rename_value v)
        | I.Trunc (v, ty) -> I.Trunc (rename_value v, ty)
        | I.Zext (v, ty) -> I.Zext (rename_value v, ty)
        | I.Sext (v, ty) -> I.Sext (rename_value v, ty)
        | I.Fptrunc (v, ty) -> I.Fptrunc (rename_value v, ty)
        | I.Fpext (v, ty) -> I.Fpext (rename_value v, ty)
        | I.Fptoui (v, ty) -> I.Fptoui (rename_value v, ty)
        | I.Fptosi (v, ty) -> I.Fptosi (rename_value v, ty)
        | I.Uitofp (v, ty) -> I.Uitofp (rename_value v, ty)
        | I.Sitofp (v, ty) -> I.Sitofp (rename_value v, ty)
      in
      I.Cast new_castop
    
    | I.Vector vecop ->
      let new_vecop = match vecop with
        | I.Splat (v, lanes) ->
          I.Splat (rename_value v, lanes)
        | I.Shuffle (v1, v2, mask) ->
          I.Shuffle (rename_value v1, rename_value v2, mask)
        | I.ExtractLane (v, idx) ->
          I.ExtractLane (rename_value v, idx)
        | I.InsertLane (v, idx, scalar) ->
          I.InsertLane (rename_value v, idx, rename_value scalar)
      in
      I.Vector new_vecop
    
    | I.Call callop ->
      let open I in
      let new_callop : callop = match callop with
        | Call (callee, args) ->
          let new_callee = rename_value callee in
          let new_args = List.map rename_value args in
          Call (new_callee, new_args)
        | TailCall (callee, args) ->
          let new_callee = rename_value callee in
          let new_args = List.map rename_value args in
          TailCall (new_callee, new_args)
      in
      I.Call new_callop
    
    | I.Phi operands ->
      I.Phi (List.map (fun (v, label) -> (rename_value v, label)) operands)
    
    | I.Const _ as c -> c
    
    | I.Freeze v -> I.Freeze (rename_value v)
    
    | I.ExtractValue (agg, indices) ->
      I.ExtractValue (rename_value agg, indices)
    
    | I.InsertValue (agg, v, indices) ->
      I.InsertValue (rename_value agg, rename_value v, indices)
    
    | I.VaArg (va_list, ty) ->
      I.VaArg (rename_value va_list, ty)
    
    | I.Fence ordering -> I.Fence ordering
  in
  
  (* Create new result if needed *)
  let new_result = match inst.I.result with
    | Some old_result ->
      let new_result = fresh_value state (Values.get_type old_result) in
      state.value_map <- ValueMap.add (Values.get_id old_result) new_result state.value_map;
      Some new_result
    | None -> None
  in
  
  { inst with instr = new_instr; result = new_result }

(* Rename terminator *)
let rename_terminator (state: rename_state) (term: I.terminator) : I.terminator =
  let rename_value v = get_renamed_value state v in
  
  match term with
  | I.Ret (Some v) -> I.Ret (Some (rename_value v))
  | I.Br (cond, then_lbl, else_lbl) ->
    I.Br (rename_value cond, then_lbl, else_lbl)
  | I.Switch (value, default, cases) ->
    I.Switch (rename_value value, default, cases)
  | _ -> term

(* Identify join points that need block parameters *)
let find_join_points (func: I.func) : BlockMap.key list =
  let join_points = ref [] in
  
  List.iter (fun (block : I.basic_block) ->
    let preds = Dominance.get_predecessors func block.I.label in
    if List.length preds > 1 then
      join_points := block.I.label :: !join_points
  ) func.I.blocks;
  
  !join_points

(* Determine which values need block parameters at join points *)
let compute_block_parameters (func: I.func) (join_points: string list) : (string * Types.ty) list BlockMap.t =
  let block_params = ref BlockMap.empty in
  
  (* For simplicity, we'll add parameters for values used in the block *)
  (* In a real implementation, we'd do a more sophisticated analysis *)
  List.iter (fun block_label ->
    let block = List.find (fun b -> b.I.label = block_label) func.I.blocks in
    let params = ref [] in
    
    (* Collect values used in this block *)
    List.iter (fun inst ->
      match inst.I.instr with
      | I.Binop (_, _, _v1, _v2) ->
        (* In real SSA, we'd check if these are defined in different predecessors *)
        ()
      | _ -> ()
    ) block.I.instructions;
    
    if !params <> [] then
      block_params := BlockMap.add block_label !params !block_params
  ) join_points;
  
  !block_params

(* Transform function to SSA form *)
let transform_to_ssa (func: I.func) : I.func =
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
  let new_blocks = List.map (fun (block : I.basic_block) ->
    (* Get block parameters *)
    let params = 
      match BlockMap.find_opt block.I.label block_params with
      | Some ps -> ps
      | None -> []
    in
    
    (* Rename instructions *)
    let new_instructions = List.map (rename_instruction state) block.I.instructions in
    
    (* Rename terminator *)
    let new_terminator = rename_terminator state block.I.terminator in
    
    { block with 
      params = params;
      instructions = new_instructions;
      terminator = new_terminator }
  ) func.I.blocks in
  
  { func with blocks = new_blocks }

(* Pretty print SSA transformation statistics *)
let pp_ssa_stats (original: I.func) (transformed: I.func) : string =
  let count_values func =
    let values = ref 0 in
    List.iter (fun (block : I.basic_block) ->
      List.iter (fun inst ->
        match inst.I.result with
        | Some _ -> incr values
        | None -> ()
      ) block.I.instructions
    ) func.I.blocks;
    !values
  in
  
  let original_values = count_values original in
  let ssa_values = count_values transformed in
  
  let count_block_params func =
    List.fold_left (fun acc (block : I.basic_block) ->
      acc + List.length block.I.params
    ) 0 func.I.blocks
  in
  
  let block_params = count_block_params transformed in
  
  Printf.sprintf "SSA Transformation Statistics:\n  Original values: %d\n  SSA values: %d\n  Block parameters: %d\n  Increase: %.1f%%"
    original_values ssa_values block_params
    ((float_of_int (ssa_values - original_values) /. float_of_int original_values) *. 100.0)