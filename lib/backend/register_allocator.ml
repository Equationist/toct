(* Linear Scan Register Allocator - Portable across architectures *)

open Machine

(* Live interval for a value *)
type live_interval = {
  value_id: int;           (* Unique ID for the value *)
  start_pos: int;          (* Start position in program *)
  end_pos: int;            (* End position in program *)
  reg_class: reg_class;    (* Required register class *)
  reg_size: int;           (* Required register size *)
  mutable assigned_reg: reg option;  (* Assigned physical register *)
  mutable spill_slot: int option;    (* Spill slot if spilled *)
}

(* Interference graph edge *)
type interference = {
  interval1: live_interval;
  interval2: live_interval;
}

(* Register allocator state *)
type allocator_state = {
  (* Machine configuration *)
  config: machine_config;
  (* All intervals sorted by start position *)
  intervals: live_interval array;
  (* Active intervals (currently live) *)
  mutable active: live_interval list;
  (* Available registers by class *)
  mutable free_regs: (reg_class * reg list) list;
  (* Next spill slot *)
  mutable next_spill_slot: int;
  (* Spill cost estimates *)
  spill_costs: (int, float) Hashtbl.t;
}

(* Initialize free register lists from machine config *)
let init_free_regs (config: machine_config) : (reg_class * reg list) list =
  List.map (fun (cls, count) ->
    let regs = List.init count (fun i ->
      match cls with
      | GPR -> make_gpr i config.ptr_size
      | FPR -> make_fpr i 8
      | VEC -> make_vec i 16
      | FLAGS -> { reg_class = FLAGS; reg_index = i; reg_size = config.ptr_size }
    ) in
    (* Remove reserved registers (stack pointer, frame pointer, etc.) *)
    let available = match cls with
      | GPR -> List.filter (fun r -> r.reg_index > 2) regs  (* Skip SP, FP *)
      | _ -> regs
    in
    (cls, available)
  ) config.available_regs

(* Compare intervals by start position *)
let compare_intervals i1 i2 =
  compare i1.start_pos i2.start_pos

(* Check if two intervals overlap *)
let intervals_overlap i1 i2 =
  not (i1.end_pos < i2.start_pos || i2.end_pos < i1.start_pos)

(* Expire old intervals that are no longer active *)
let expire_old_intervals state current_pos =
  let still_active, expired = List.partition (fun interval ->
    interval.end_pos >= current_pos
  ) state.active in
  
  (* Return registers of expired intervals to free list *)
  List.iter (fun interval ->
    match interval.assigned_reg with
    | Some reg ->
      let cls = reg.reg_class in
      let free_list = List.assoc cls state.free_regs in
      let new_free = reg :: free_list in
      state.free_regs <- List.map (fun (c, regs) ->
        if c = cls then (c, new_free) else (c, regs)
      ) state.free_regs
    | None -> ()
  ) expired;
  
  state.active <- still_active

(* Find a free register of the required class and size *)
let find_free_reg state reg_class reg_size =
  try
    let free_list : reg list = List.assoc reg_class state.free_regs in
    match List.find_opt (fun (r : reg) -> r.reg_size >= reg_size) free_list with
    | Some reg ->
      (* Remove from free list *)
      let new_free = List.filter (fun r -> r.reg_index <> reg.reg_index) free_list in
      state.free_regs <- List.map (fun (c, regs) ->
        if c = reg_class then (c, new_free) else (c, regs)
      ) state.free_regs;
      Some reg
    | None -> None
  with Not_found -> None

(* Spill the interval with lowest priority *)
let spill_at_interval state current =
  (* Find interval to spill (using simple heuristic) *)
  let spill_candidate = 
    let active_same_class = List.filter (fun i -> 
      i.reg_class = current.reg_class
    ) state.active in
    
    (* Spill the one that ends last (lives longest) *)
    match List.sort (fun i1 i2 -> compare i2.end_pos i1.end_pos) active_same_class with
    | [] -> failwith "No spillable interval found"
    | hd :: _ -> hd
  in
  
  (* If current ends after spill candidate, spill current instead *)
  if current.end_pos > spill_candidate.end_pos then begin
    (* Spill current *)
    current.spill_slot <- Some state.next_spill_slot;
    state.next_spill_slot <- state.next_spill_slot + current.reg_size
  end else begin
    (* Spill the candidate *)
    spill_candidate.spill_slot <- Some state.next_spill_slot;
    state.next_spill_slot <- state.next_spill_slot + spill_candidate.reg_size;
    
    (* Assign candidate's register to current *)
    current.assigned_reg <- spill_candidate.assigned_reg;
    spill_candidate.assigned_reg <- None;
    
    (* Update active list *)
    state.active <- List.filter (fun i -> i.value_id <> spill_candidate.value_id) state.active;
    state.active <- current :: state.active
  end

(* Main linear scan algorithm *)
let allocate_registers (config: machine_config) (intervals: live_interval list) : unit =
  (* Sort intervals by start position *)
  let sorted_intervals = Array.of_list intervals in
  Array.sort compare_intervals sorted_intervals;
  
  (* Initialize state *)
  let state = {
    config;
    intervals = sorted_intervals;
    active = [];
    free_regs = init_free_regs config;
    next_spill_slot = 0;
    spill_costs = Hashtbl.create 100;
  } in
  
  (* Process each interval *)
  Array.iter (fun current ->
    (* Expire old intervals *)
    expire_old_intervals state current.start_pos;
    
    (* Try to find a free register *)
    match find_free_reg state current.reg_class current.reg_size with
    | Some reg ->
      (* Assign the register *)
      current.assigned_reg <- Some reg;
      state.active <- current :: state.active
    | None ->
      (* Need to spill *)
      spill_at_interval state current
  ) sorted_intervals

(* Coalescing - merge intervals that can share the same register *)
let coalesce_intervals intervals =
  (* Build interference graph *)
  let interferences = ref [] in
  
  (* Check all pairs of intervals *)
  for i = 0 to Array.length intervals - 1 do
    for j = i + 1 to Array.length intervals - 1 do
      let i1 = intervals.(i) in
      let i2 = intervals.(j) in
      if intervals_overlap i1 i2 && i1.reg_class = i2.reg_class then
        interferences := { interval1 = i1; interval2 = i2 } :: !interferences
    done
  done;
  
  (* TODO: Implement graph coloring for coalescing *)
  ()

(* Compute live intervals from instructions *)
let compute_live_intervals (instrs: machine_instr list) : live_interval list =
  let intervals = ref [] in
  let value_counter = ref 0 in
  let position = ref 0 in
  
  (* Simple liveness analysis - in practice would be more sophisticated *)
  List.iter (fun instr ->
    incr position;
    
    (* Create intervals for operands *)
    match instr.op with
    | MOV (dst, src) ->
      let interval = {
        value_id = !value_counter;
        start_pos = !position;
        end_pos = !position + 1;
        reg_class = dst.reg_class;
        reg_size = dst.reg_size;
        assigned_reg = None;
        spill_slot = None;
      } in
      incr value_counter;
      intervals := interval :: !intervals
    | ADD (dst, src1, src2) | SUB (dst, src1, src2) | MUL (dst, src1, src2) ->
      let interval = {
        value_id = !value_counter;
        start_pos = !position;
        end_pos = !position + 2;
        reg_class = dst.reg_class;
        reg_size = dst.reg_size;
        assigned_reg = None;
        spill_slot = None;
      } in
      incr value_counter;
      intervals := interval :: !intervals
    | _ -> ()
  ) instrs;
  
  List.rev !intervals

(* Insert spill/reload code *)
let insert_spill_code (config: machine_config) (instrs: machine_instr list) (intervals: live_interval list) : machine_instr list =
  let result = ref [] in
  
  List.iter (fun instr ->
    (* Check if any operand needs reload *)
    (* TODO: Implement spill code insertion *)
    result := instr :: !result
  ) instrs;
  
  List.rev !result

(* Main entry point *)
let allocate (config: machine_config) (instrs: machine_instr list) : machine_instr list * int =
  (* Compute live intervals *)
  let intervals = compute_live_intervals instrs in
  
  (* Run linear scan *)
  allocate_registers config intervals;
  
  (* Optional: Run coalescing *)
  coalesce_intervals (Array.of_list intervals);
  
  (* Insert spill/reload code *)
  let final_instrs = insert_spill_code config instrs intervals in
  
  (* Return instructions and frame size needed for spills *)
  let spill_size = List.fold_left (fun acc interval ->
    match interval.spill_slot with
    | Some slot -> max acc (slot + interval.reg_size)
    | None -> acc
  ) 0 intervals in
  
  (final_instrs, spill_size)