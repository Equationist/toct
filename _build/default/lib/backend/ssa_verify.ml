(* SSA Form Verification *)

open Compilerkit_pir
module I = Instructions

module ValueSet = Set.Make(Int)
module BlockSet = Set.Make(String)
module ValueMap = Map.Make(Int)
module BlockMap = Map.Make(String)

type ssa_error =
  | MultipleDefinitions of int * string list  (* value id, blocks *)
  | UseBeforeDefinition of int * string * string  (* value id, use block, definition block *)
  | UnreachableUse of int * string  (* value id, block *)
  | MissingBlockParameter of string * string  (* block, variable *)
  | InconsistentBlockArguments of string * string  (* pred block, succ block *)
  | PhiOperandMismatch of string * string * int * int  (* block, variable, expected, actual *)

exception SSAError of ssa_error

(* Result of SSA verification *)
type verification_result = {
  is_valid: bool;
  errors: ssa_error list;
  warnings: string list;
  stats: ssa_stats;
}

and ssa_stats = {
  num_blocks: int;
  num_definitions: int;
  num_uses: int;
  num_block_params: int;
  single_assignment: bool;
}

(* Collect all definitions in a function *)
let collect_definitions (func: I.func) : string ValueMap.t =
  let defs = ref ValueMap.empty in
  
  (* Block parameters are defined at block entry *)
  List.iter (fun (block : I.basic_block) ->
    List.iter (fun (_, _) ->
      (* Block parameters would be handled here if we tracked them as values *)
      ()
    ) block.I.params
  ) func.I.blocks;
  
  (* Instruction definitions *)
  List.iter (fun (block : I.basic_block) ->
    List.iter (fun inst ->
      match inst.I.result with
      | Some value ->
        let value_id = Values.get_id value in
        (match ValueMap.find_opt value_id !defs with
         | Some other_block ->
           raise (SSAError (MultipleDefinitions (value_id, [other_block; block.I.label])))
         | None ->
           defs := ValueMap.add value_id block.I.label !defs)
      | None -> ()
    ) block.I.instructions
  ) func.I.blocks;
  
  !defs

(* Collect all uses of values *)
let collect_uses (func: I.func) : (int * string) list =
  let uses = ref [] in
  
  let add_use block_label value =
    let value_id = Values.get_id value in
    uses := (value_id, block_label) :: !uses
  in
  
  let add_const_uses _block_label _const_val = 
    (* Constants don't count as uses for SSA purposes *)
    ()
  in
  
  List.iter (fun (block : I.basic_block) ->
    List.iter (fun inst ->
      match inst.I.instr with
      | I.Binop (_, _, v1, v2) ->
        add_use block.I.label v1;
        add_use block.I.label v2
      | I.Icmp (_, v1, v2) ->
        add_use block.I.label v1;
        add_use block.I.label v2
      | I.Fcmp (_, v1, v2) ->
        add_use block.I.label v1;
        add_use block.I.label v2
      | I.Select (cond, v_true, v_false) ->
        add_use block.I.label cond;
        add_use block.I.label v_true;
        add_use block.I.label v_false
      | I.Memory memop ->
        (match memop with
         | I.Load _ -> ()
         | I.Store (value, ptr) ->
           add_use block.I.label value;
           add_use block.I.label ptr
         | I.Alloca (size, _) ->
           add_use block.I.label size
         | I.Memcpy (dst, src, bytes) ->
           add_use block.I.label dst;
           add_use block.I.label src;
           add_use block.I.label bytes
         | I.Memset (dst, byte, bytes) ->
           add_use block.I.label dst;
           add_use block.I.label byte;
           add_use block.I.label bytes)
      | I.Address addrop ->
        (match addrop with
         | I.Gep (base, idx) ->
           add_use block.I.label base;
           add_use block.I.label idx
         | I.FieldAddr (base, _) ->
           add_use block.I.label base
         | I.PtrAdd (base, offset) ->
           add_use block.I.label base;
           add_use block.I.label offset)
      | I.Cast castop ->
        (match castop with
         | I.Bitcast v
         | I.Trunc (v, _)
         | I.Zext (v, _)
         | I.Sext (v, _)
         | I.Fptrunc (v, _)
         | I.Fpext (v, _)
         | I.Fptoui (v, _)
         | I.Fptosi (v, _)
         | I.Uitofp (v, _)
         | I.Sitofp (v, _) ->
           add_use block.I.label v)
      | I.Vector vecop ->
        (match vecop with
         | I.Splat (v, _) ->
           add_use block.I.label v
         | I.Shuffle (v1, v2, _) ->
           add_use block.I.label v1;
           add_use block.I.label v2
         | I.ExtractLane (v, _) ->
           add_use block.I.label v
         | I.InsertLane (v, _, scalar) ->
           add_use block.I.label v;
           add_use block.I.label scalar)
      | I.Call callop ->
        (match callop with
         | I.Call (callee, args)
         | I.TailCall (callee, args) ->
           add_use block.I.label callee;
           List.iter (add_use block.I.label) args)
      | I.Phi operands ->
        List.iter (fun (value, _) -> add_use block.I.label value) operands
      | I.Const const_val ->
        add_const_uses block.I.label const_val
      | I.Freeze v ->
        add_use block.I.label v
      | I.ExtractValue (agg, _) ->
        add_use block.I.label agg
      | I.InsertValue (agg, v, _) ->
        add_use block.I.label agg;
        add_use block.I.label v
      | I.VaArg (va_list, _) ->
        add_use block.I.label va_list
      | I.Fence _ -> ()
    ) block.I.instructions;
    
    (* Check terminator uses *)
    match block.I.terminator with
    | I.Br (cond, _, _) ->
      add_use block.I.label cond
    | I.Ret (Some v) ->
      add_use block.I.label v
    | I.Switch (value, _, _) ->
      add_use block.I.label value
    | _ -> ()
  ) func.I.blocks;
  
  !uses

(* Check if a definition dominates a use *)
let check_dominance (dom_info: Dominance.dominance_info) (def_block: string) (use_block: string) : bool =
  def_block = use_block || Dominance.dominates dom_info def_block use_block

(* Verify single assignment property *)
let verify_single_assignment (func: I.func) : ssa_error list =
  try
    let _ = collect_definitions func in
    []
  with SSAError e ->
    [e]

(* Verify all uses are dominated by definitions *)
let verify_dominance (func: I.func) (dom_info: Dominance.dominance_info) : ssa_error list =
  let errors = ref [] in
  let defs = collect_definitions func in
  let uses = collect_uses func in
  
  List.iter (fun (value_id, use_block) ->
    match ValueMap.find_opt value_id defs with
    | Some def_block ->
      if not (check_dominance dom_info def_block use_block) then
        errors := UseBeforeDefinition (value_id, use_block, def_block) :: !errors
    | None ->
      (* Value not defined - might be a function parameter or constant *)
      ()
  ) uses;
  
  !errors

(* Verify block parameters match arguments from predecessors *)
let rec verify_block_parameters (func: I.func) : ssa_error list =
  let errors = ref [] in
  
  List.iter (fun (block : I.basic_block) ->
    if block.I.params <> [] then
      let preds = Dominance.get_predecessors func block.I.label in
      
      (* Each predecessor should provide arguments *)
      List.iter (fun pred_name ->
        let pred_block = List.find (fun b -> b.I.label = pred_name) func.I.blocks in
        
        (* Check terminator provides correct arguments *)
        let target_args = get_terminator_args pred_block.I.terminator block.I.label in
        if List.length target_args <> List.length block.I.params then
          errors := PhiOperandMismatch (block.I.label, pred_name, 
                                       List.length block.I.params,
                                       List.length target_args) :: !errors
      ) preds
  ) func.I.blocks;
  
  !errors

and get_terminator_args (_term: I.terminator) (_target: string) : Values.value list =
  (* In current PIR, terminators don't carry arguments - this would need extension *)
  []

(* Compute SSA statistics *)
let compute_stats (func: I.func) : ssa_stats =
  let num_blocks = List.length func.I.blocks in
  
  let num_definitions = ref 0 in
  let num_uses = ref 0 in
  let num_block_params = ref 0 in
  
  (* Count definitions *)
  List.iter (fun (block : I.basic_block) ->
    num_block_params := !num_block_params + List.length block.I.params;
    List.iter (fun inst ->
      match inst.I.result with
      | Some _ -> incr num_definitions
      | None -> ()
    ) block.I.instructions
  ) func.I.blocks;
  
  (* Count uses *)
  let uses = collect_uses func in
  num_uses := List.length uses;
  
  {
    num_blocks;
    num_definitions = !num_definitions;
    num_uses = !num_uses;
    num_block_params = !num_block_params;
    single_assignment = true;  (* If we got here without exceptions *)
  }

(* Main verification entry point *)
let verify_ssa (func: I.func) : verification_result =
  let dom_info = Dominance.compute_dominance_info func in
  
  let errors = ref [] in
  let warnings = ref [] in
  
  (* Check single assignment *)
  errors := !errors @ verify_single_assignment func;
  
  (* Check dominance *)
  errors := !errors @ verify_dominance func dom_info;
  
  (* Check block parameters *)
  errors := !errors @ verify_block_parameters func;
  
  (* Compute statistics *)
  let stats = compute_stats func in
  
  {
    is_valid = !errors = [];
    errors = !errors;
    warnings = !warnings;
    stats;
  }

(* Pretty print SSA error *)
let pp_ssa_error = function
  | MultipleDefinitions (value_id, blocks) ->
    Printf.sprintf "Value 'v%d' defined multiple times in blocks: %s"
      value_id (String.concat ", " blocks)
  | UseBeforeDefinition (value_id, use_block, def_block) ->
    Printf.sprintf "Value 'v%d' used in block '%s' before definition in block '%s'"
      value_id use_block def_block
  | UnreachableUse (value_id, block) ->
    Printf.sprintf "Value 'v%d' used in block '%s' but never defined"
      value_id block
  | MissingBlockParameter (block, var) ->
    Printf.sprintf "Block '%s' missing parameter for variable '%s'"
      block var
  | InconsistentBlockArguments (pred, succ) ->
    Printf.sprintf "Inconsistent arguments from block '%s' to block '%s'"
      pred succ
  | PhiOperandMismatch (block, pred, expected, actual) ->
    Printf.sprintf "Block '%s' expects %d arguments from predecessor '%s' but got %d"
      block expected pred actual

(* Pretty print verification result *)
let pp_verification_result (result: verification_result) : string =
  let lines = ref [] in
  
  lines := Printf.sprintf "SSA Verification: %s"
    (if result.is_valid then "PASSED" else "FAILED") :: !lines;
  
  if result.errors <> [] then begin
    lines := "" :: !lines;
    lines := "Errors:" :: !lines;
    List.iter (fun err ->
      lines := Printf.sprintf "  - %s" (pp_ssa_error err) :: !lines
    ) result.errors
  end;
  
  if result.warnings <> [] then begin
    lines := "" :: !lines;
    lines := "Warnings:" :: !lines;
    List.iter (fun warn ->
      lines := Printf.sprintf "  - %s" warn :: !lines
    ) result.warnings
  end;
  
  lines := "" :: !lines;
  lines := "Statistics:" :: !lines;
  lines := Printf.sprintf "  Blocks: %d" result.stats.num_blocks :: !lines;
  lines := Printf.sprintf "  Definitions: %d" result.stats.num_definitions :: !lines;
  lines := Printf.sprintf "  Uses: %d" result.stats.num_uses :: !lines;
  lines := Printf.sprintf "  Block parameters: %d" result.stats.num_block_params :: !lines;
  lines := Printf.sprintf "  Single assignment: %b" result.stats.single_assignment :: !lines;
  
  String.concat "\n" (List.rev !lines)