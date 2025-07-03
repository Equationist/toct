(* Dominance Analysis for SSA Construction *)

open Compilerkit_pir
module I = Instructions

module BlockSet = Set.Make(String)
module BlockMap = Map.Make(String)

type dominance_info = {
  immediate_dominators: string BlockMap.t;  (* block -> immediate dominator *)
  dominance_frontiers: BlockSet.t BlockMap.t;  (* block -> dominance frontier *)
  dominator_tree_children: string list BlockMap.t;  (* block -> children in dom tree *)
}

(* Get predecessors of a block *)
let get_predecessors (func: I.func) (block_name: string) : string list =
  let preds = ref [] in
  List.iter (fun (block : I.basic_block) ->
    match block.I.terminator with
    | I.Jmp target when target = block_name ->
      preds := block.I.label :: !preds
    | I.Br (_, true_target, false_target) ->
      if true_target = block_name || false_target = block_name then
        preds := block.I.label :: !preds
    | I.Switch (_, default, cases) ->
      let targets = default :: List.map snd cases in
      if List.mem block_name targets then
        preds := block.I.label :: !preds
    | _ -> ()
  ) func.I.blocks;
  !preds

(* Get successors of a block *)
let get_successors (block: I.basic_block) : string list =
  match block.I.terminator with
  | I.Jmp target -> [target]
  | I.Br (_, true_target, false_target) -> [true_target; false_target]
  | I.Switch (_, default, cases) ->
    default :: List.map snd cases
  | I.Ret _ -> []
  | I.Unreachable -> []

(* Find the entry block of a function *)
let get_entry_block (func: I.func) : string =
  match func.I.blocks with
  | [] -> failwith "Function has no blocks"
  | first_block :: _ -> first_block.I.label

(* Compute immediate dominators using the Cooper-Harvey-Kennedy algorithm *)
let compute_immediate_dominators (func: I.func) : string BlockMap.t =
  let entry = get_entry_block func in
  let blocks = List.map (fun b -> b.I.label) func.I.blocks in
  
  (* Initialize: entry dominates itself, others undefined *)
  let idom = ref BlockMap.empty in
  idom := BlockMap.add entry entry !idom;
  
  (* Build predecessor map *)
  let predecessors = ref BlockMap.empty in
  List.iter (fun (block : I.basic_block) ->
    let preds = get_predecessors func block.I.label in
    predecessors := BlockMap.add block.I.label preds !predecessors
  ) func.I.blocks;
  
  (* Find common dominator *)
  let rec intersect b1 b2 =
    if b1 = b2 then b1
    else
      match BlockMap.find_opt b1 !idom, BlockMap.find_opt b2 !idom with
      | Some d1, Some d2 ->
        if d1 = b1 then intersect d1 b2
        else if d2 = b2 then intersect b1 d2
        else intersect d1 d2
      | _ -> entry
  in
  
  (* Iterate until fixed point *)
  let changed = ref true in
  while !changed do
    changed := false;
    List.iter (fun block_name ->
      if block_name <> entry then
        let preds = BlockMap.find block_name !predecessors in
        (* Find first processed predecessor *)
        let processed_preds = List.filter (fun p -> BlockMap.mem p !idom) preds in
        match processed_preds with
        | [] -> ()
        | first :: rest ->
          let new_idom = List.fold_left intersect first rest in
          match BlockMap.find_opt block_name !idom with
          | Some old_idom when old_idom = new_idom -> ()
          | _ ->
            idom := BlockMap.add block_name new_idom !idom;
            changed := true
    ) blocks
  done;
  
  !idom

(* Build dominator tree children from immediate dominators *)
let build_dominator_tree (idom: string BlockMap.t) : string list BlockMap.t =
  let children = ref BlockMap.empty in
  BlockMap.iter (fun block dom ->
    if block <> dom then
      let existing = 
        match BlockMap.find_opt dom !children with
        | Some cs -> cs
        | None -> []
      in
      children := BlockMap.add dom (block :: existing) !children
  ) idom;
  !children

(* Compute dominance frontiers *)
let compute_dominance_frontiers (func: I.func) (idom: string BlockMap.t) : BlockSet.t BlockMap.t =
  let df = ref BlockMap.empty in
  
  (* Initialize empty frontiers *)
  List.iter (fun (block : I.basic_block) ->
    df := BlockMap.add block.I.label BlockSet.empty !df
  ) func.I.blocks;
  
  (* For each block *)
  List.iter (fun (block : I.basic_block) ->
    let preds = get_predecessors func block.I.label in
    if List.length preds >= 2 then
      (* Join point - check dominance *)
      List.iter (fun pred ->
        let runner = ref pred in
        while !runner <> BlockMap.find block.I.label idom do
          let existing = 
            match BlockMap.find_opt !runner !df with
            | Some s -> s
            | None -> BlockSet.empty
          in
          df := BlockMap.add !runner (BlockSet.add block.I.label existing) !df;
          runner := BlockMap.find !runner idom
        done
      ) preds
  ) func.I.blocks;
  
  !df

(* Main entry point: compute all dominance information *)
let compute_dominance_info (func: I.func) : dominance_info =
  let idom = compute_immediate_dominators func in
  let children = build_dominator_tree idom in
  let df = compute_dominance_frontiers func idom in
  {
    immediate_dominators = idom;
    dominance_frontiers = df;
    dominator_tree_children = children;
  }

(* Check if block a dominates block b *)
let dominates (info: dominance_info) (a: string) (b: string) : bool =
  let rec check current =
    if current = a then true
    else
      match BlockMap.find_opt current info.immediate_dominators with
      | Some dom when dom = current -> false  (* Reached root *)
      | Some dom -> check dom
      | None -> false
  in
  check b

(* Get all blocks dominated by a given block *)
let get_dominated_blocks (info: dominance_info) (block: string) : string list =
  let rec collect acc b =
    let children = 
      match BlockMap.find_opt b info.dominator_tree_children with
      | Some cs -> cs
      | None -> []
    in
    List.fold_left collect (b :: acc) children
  in
  collect [] block

(* Pretty-print dominance information *)
let pp_dominance_info (info: dominance_info) : string =
  let lines = ref [] in
  lines := "Immediate Dominators:" :: !lines;
  BlockMap.iter (fun block dom ->
    if block <> dom then
      lines := Printf.sprintf "  %s -> %s" block dom :: !lines
  ) info.immediate_dominators;
  
  lines := "" :: !lines;
  lines := "Dominance Frontiers:" :: !lines;
  BlockMap.iter (fun block frontier ->
    if not (BlockSet.is_empty frontier) then
      let frontier_str = String.concat ", " (BlockSet.elements frontier) in
      lines := Printf.sprintf "  DF(%s) = {%s}" block frontier_str :: !lines
  ) info.dominance_frontiers;
  
  String.concat "\n" (List.rev !lines)