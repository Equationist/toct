(* Instruction Selection via BURS (Bottom-Up Rewrite System) *)

open Machine
open Compilerkit_pir

(* Tree pattern matching for instruction selection *)

(* Node in the instruction tree *)
type tree_node = {
  instr: Instructions.instr;
  children: tree_node list;
  mutable selected_pattern: pattern option;
  mutable min_cost: int;
  mutable result_reg: reg option;
}

(* Build tree from instruction *)
let rec build_tree (instr: Instructions.instr) : tree_node =
  let children = match instr with
    | Instructions.Binop (_, _, v1, v2) ->
      [value_to_tree v1; value_to_tree v2]
    | Instructions.Icmp (_, v1, v2) ->
      [value_to_tree v1; value_to_tree v2]
    | Instructions.Fcmp (_, v1, v2) ->
      [value_to_tree v1; value_to_tree v2]
    | Instructions.Select (cond, v1, v2) ->
      [value_to_tree cond; value_to_tree v1; value_to_tree v2]
    | Instructions.Memory (Instructions.Load _) -> []
    | Instructions.Memory (Instructions.Store (v1, v2)) ->
      [value_to_tree v1; value_to_tree v2]
    | Instructions.Memory (Instructions.Alloca (size, _)) ->
      [value_to_tree size]
    | Instructions.Memory (Instructions.Memcpy (dst, src, size)) ->
      [value_to_tree dst; value_to_tree src; value_to_tree size]
    | Instructions.Memory (Instructions.Memset (dst, byte, size)) ->
      [value_to_tree dst; value_to_tree byte; value_to_tree size]
    | Instructions.Address (Instructions.Gep (base, idx)) ->
      [value_to_tree base; value_to_tree idx]
    | Instructions.Address (Instructions.FieldAddr (base, _)) ->
      [value_to_tree base]
    | Instructions.Address (Instructions.PtrAdd (base, offset)) ->
      [value_to_tree base; value_to_tree offset]
    | Instructions.Cast cast_op ->
      (match cast_op with
       | Instructions.Bitcast v -> [value_to_tree v]
       | Instructions.Trunc (v, _) -> [value_to_tree v]
       | Instructions.Zext (v, _) -> [value_to_tree v]
       | Instructions.Sext (v, _) -> [value_to_tree v]
       | Instructions.Fptrunc (v, _) -> [value_to_tree v]
       | Instructions.Fpext (v, _) -> [value_to_tree v]
       | Instructions.Fptoui (v, _) -> [value_to_tree v]
       | Instructions.Fptosi (v, _) -> [value_to_tree v]
       | Instructions.Uitofp (v, _) -> [value_to_tree v]
       | Instructions.Sitofp (v, _) -> [value_to_tree v])
    | Instructions.Vector vec_op ->
      (match vec_op with
       | Instructions.Splat (v, _) -> [value_to_tree v]
       | Instructions.Shuffle (v1, v2, _) -> [value_to_tree v1; value_to_tree v2]
       | Instructions.ExtractLane (v, _) -> [value_to_tree v]
       | Instructions.InsertLane (v, _, scalar) -> [value_to_tree v; value_to_tree scalar])
    | Instructions.Call _ -> []  (* Handle separately *)
    | Instructions.Phi _ -> []   (* Handle in SSA destruction *)
    | Instructions.Const _ -> []
    | Instructions.Freeze v -> [value_to_tree v]
    | Instructions.ExtractValue (v, _) -> [value_to_tree v]
    | Instructions.InsertValue (agg, v, _) -> [value_to_tree agg; value_to_tree v]
    | Instructions.VaArg (v, _) -> [value_to_tree v]
    | Instructions.Fence _ -> []
  in
  { instr; children; selected_pattern = None; min_cost = max_int; result_reg = None }

and value_to_tree (v: Values.value) : tree_node =
  (* Convert value to a tree node - may need to load from memory *)
  let dummy_instr = Instructions.Const (Values.const_of_value v) in
  { instr = dummy_instr; children = []; selected_pattern = None; min_cost = 0; result_reg = None }

(* Pattern matching *)
let match_pattern (pattern: pattern) (node: tree_node) : bool =
  (* Simple pattern matching - in real implementation would be more sophisticated *)
  match pattern.pir_pattern, node.instr with
  | Instructions.Binop (op1, _, _, _), Instructions.Binop (op2, _, _, _) -> op1 = op2
  | Instructions.Memory (Instructions.Load ty1), Instructions.Memory (Instructions.Load ty2) -> ty1 = ty2
  | Instructions.Memory (Instructions.Store _), Instructions.Memory (Instructions.Store _) -> true
  | Instructions.Const (Values.ConstInt (_, ty1)), Instructions.Const (Values.ConstInt (_, ty2)) -> ty1 = ty2
  | Instructions.Const (Values.ConstFloat (_, ty1)), Instructions.Const (Values.ConstFloat (_, ty2)) -> ty1 = ty2
  | Instructions.Const Values.ConstNull, Instructions.Const Values.ConstNull -> true
  | _ -> false

(* BURS algorithm - compute minimum cost cover *)
let rec compute_costs (patterns: pattern list) (node: tree_node) : unit =
  (* First compute costs for children *)
  List.iter (compute_costs patterns) node.children;
  
  (* Try each pattern *)
  List.iter (fun pattern ->
    if match_pattern pattern node then
      let children_cost = List.fold_left (fun acc child -> acc + child.min_cost) 0 node.children in
      let total_cost = pattern.cost + children_cost in
      if total_cost < node.min_cost then begin
        node.min_cost <- total_cost;
        node.selected_pattern <- Some pattern
      end
  ) patterns

(* Main instruction selection *)
module InstructionSelector (M: MACHINE) = struct
  
  (* Code emission with access to machine module and instruction context *)
  let rec emit_code_with_machine (node: tree_node) (reg_alloc: Values.value -> reg) (instr_context: Instructions.instruction option) : machine_instr list =
    match node.selected_pattern with
    | None -> failwith "No pattern selected for instruction"
    | Some pattern ->
      (* First emit code for children *)
      let child_code = List.concat (List.map (fun child -> emit_code_with_machine child reg_alloc None) node.children) in
      (* Then emit code for this node *)
      (* Special handling for constants using materialize_constant *)
      let node_code = match node.instr with
        | Instructions.Const (Values.ConstInt (value, ty)) ->
          (* For constants, use the machine's materialize_constant function *)
          (* Get the destination register from the instruction's result value *)
          (match instr_context with
          | Some { result = Some result_val; _ } ->
            let dst_reg = reg_alloc result_val in
            M.materialize_constant value (Types.Scalar ty) dst_reg
          | _ -> 
            (* Fallback if no result value - shouldn't happen for constants *)
            failwith "Constant instruction without result value")
        | _ -> pattern.emit reg_alloc
      in
      child_code @ node_code
  
  (* Select instructions for a basic block *)
  let select_block (frame: frame_info) (block: Instructions.basic_block) : machine_instr list =
    let reg_alloc = ref [] in
    let next_reg = ref 0 in
    
    (* Simple register allocator for now *)
    let get_reg (v: Values.value) : reg =
      try
        List.assoc v !reg_alloc
      with Not_found ->
        (* Determine register size based on value type *)
        let size = match v.Values.ty with
          | Types.Scalar Types.I8 | Types.Scalar Types.I16 | Types.Scalar Types.I32 -> 4
          | Types.Scalar Types.I64 | Types.Ptr -> 8
          | Types.Scalar Types.I1 -> 1
          | Types.Scalar Types.F32 -> 4
          | Types.Scalar Types.F64 -> 8
          | _ -> M.config.ptr_size
        in
        let r = make_gpr !next_reg size in
        incr next_reg;
        reg_alloc := (v, r) :: !reg_alloc;
        r
    in
    
    (* Process each instruction *)
    let code = ref [] in
    
    List.iter (fun (instr: Instructions.instruction) ->
      (* Build tree *)
      let tree = build_tree instr.instr in
      
      (* Run BURS *)
      compute_costs M.patterns tree;
      
      (* Emit code *)
      let instr_code = emit_code_with_machine tree get_reg (Some instr) in
      code := !code @ instr_code
    ) block.instructions;
    
    (* Handle terminator *)
    let term_code = match block.terminator with
      | Instructions.Ret None -> 
        (* For void return, emit epilogue then return *)
        M.emit_epilogue frame @ [{ label = None; op = RET; comment = Some "return void" }]
      | Instructions.Ret (Some v) ->
        let r = get_reg v in
        (* Determine register size based on value type *)
        let ret_reg = match v.Values.ty with
          | Types.Scalar Types.I8 | Types.Scalar Types.I16 | Types.Scalar Types.I32 -> 
            make_gpr 0 4  (* Use w0 for 32-bit and smaller integers *)
          | _ -> 
            make_gpr 0 M.config.ptr_size  (* Use x0 for 64-bit values *)
        in
        (* Only emit MOV if value is not already in the return register *)
        let mov_instrs = 
          if r.reg_index = 0 && r.reg_class = GPR && r.reg_size = ret_reg.reg_size then
            [] (* Value already in correct return register, no move needed *)
          else
            [{ label = None; op = MOV (ret_reg, r); comment = Some "return value" }]
        in
        (* First move return value to return register, then emit epilogue *)
        mov_instrs @ M.emit_epilogue frame @ [{ label = None; op = RET; comment = Some "return" }]
      | Instructions.Br (cond, then_lbl, else_lbl) ->
        let r = get_reg cond in
        [{ label = None; op = TEST (r, r); comment = Some "test condition" };
         { label = None; op = JCC (NE, then_lbl); comment = Some "branch if true" };
         { label = None; op = JMP else_lbl; comment = Some "branch if false" }]
      | Instructions.Jmp lbl ->
        [{ label = None; op = JMP lbl; comment = Some "unconditional jump" }]
      | Instructions.Switch _ -> failwith "Switch not implemented yet"
      | Instructions.Unreachable -> []
    in
    
    !code @ term_code
  
  (* Select instructions for a function *)
  let select_function (func: Instructions.func) : machine_instr list =
    (* For simple functions, use minimal frame *)
    (* TODO: Proper frame size calculation based on actual locals and spills *)
    let is_simple_function = 
      List.length func.blocks = 1 &&
      List.length (List.hd func.blocks).instructions <= 2
    in
    let frame = {
      frame_size = if is_simple_function then 16 else 64;  (* Minimal for simple functions *)
      locals_offset = 16;
      spill_offset = 32;
      callee_save_offset = 48;
      alignment = 16;
    } in
    
    (* Emit prologue *)
    let prologue = M.emit_prologue frame in
    
    (* Select instructions for each block *)
    let blocks_code = List.concat (List.map (fun block ->
      (* First instruction of block gets the label *)
      let block_instrs = select_block frame block in
      match block_instrs with
      | [] -> []
      | first :: rest -> 
        { first with label = Some block.label } :: rest
    ) func.blocks) in
    
    (* Note: epilogue is now emitted as part of return instructions *)
    prologue @ blocks_code
end

(* Helper to create common patterns *)
let make_binop_pattern op cost =
  {
    pir_pattern = Instructions.Binop (op, Instructions.NoFlag, 
      Values.create_simple_value (Types.Scalar Types.I32),
      Values.create_simple_value (Types.Scalar Types.I32));
    cost;
    emit = (fun reg_alloc ->
      let dummy_reg = make_gpr 0 4 in
      match op with
      | Instructions.Add -> [{ label = None; op = ADD (dummy_reg, dummy_reg, dummy_reg); comment = None }]
      | Instructions.Sub -> [{ label = None; op = SUB (dummy_reg, dummy_reg, dummy_reg); comment = None }]
      | Instructions.Mul -> [{ label = None; op = MUL (dummy_reg, dummy_reg, dummy_reg); comment = None }]
      | _ -> []
    );
    constraints = [RegClass GPR; RegClass GPR; RegClass GPR];
    hints = [];
  }

(* Common patterns for different architectures *)
let common_patterns = [
  make_binop_pattern Instructions.Add 1;
  make_binop_pattern Instructions.Sub 1;
  make_binop_pattern Instructions.Mul 3;
  (* Add more patterns... *)
]