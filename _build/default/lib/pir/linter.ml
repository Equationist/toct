(* PIR Linter - SSA and Type Verification *)

open Types
open Values
open Instructions

(* Linter errors *)
type error =
  | UndefinedValue of string * string              (* value name, context *)
  | RedefinedValue of string * string              (* value name, context *)
  | TypeMismatch of string * ty * ty               (* context, expected, actual *)
  | SSAViolation of string * string                (* value name, reason *)
  | UnreachableBlock of string                      (* block label *)
  | MissingTerminator of string                     (* block label *)
  | InvalidTerminator of string * string            (* block label, reason *)
  | InvalidInstruction of string * string           (* instruction desc, reason *)
  | BlockParameterMismatch of string * int * int   (* block label, expected, actual *)
  | InvalidPhi of string * string                   (* phi desc, reason *)
  | DominanceViolation of string * string * string  (* use site, def site, reason *)
  | InvalidOperandType of string * string           (* instruction, reason *)
  | MissingEntryBlock
  | MultipleEntryBlocks
  | InvalidFunctionSignature of string              (* reason *)

(* Linter result *)
type result = {
  errors: error list;
  warnings: string list;
}

let empty_result = { errors = []; warnings = [] }

let add_error err result = { result with errors = err :: result.errors }
let add_warning warn result = { result with warnings = warn :: result.warnings }

(* Error formatting *)
let string_of_error = function
  | UndefinedValue (name, ctx) -> 
    Printf.sprintf "Undefined value '%s' in %s" name ctx
  | RedefinedValue (name, ctx) -> 
    Printf.sprintf "Value '%s' redefined in %s" name ctx
  | TypeMismatch (ctx, expected, actual) -> 
    Printf.sprintf "Type mismatch in %s: expected %s, got %s" 
      ctx (string_of_ty expected) (string_of_ty actual)
  | SSAViolation (name, reason) -> 
    Printf.sprintf "SSA violation for value '%s': %s" name reason
  | UnreachableBlock label -> 
    Printf.sprintf "Unreachable block '%s'" label
  | MissingTerminator label -> 
    Printf.sprintf "Block '%s' missing terminator" label
  | InvalidTerminator (label, reason) -> 
    Printf.sprintf "Invalid terminator in block '%s': %s" label reason
  | InvalidInstruction (desc, reason) -> 
    Printf.sprintf "Invalid instruction %s: %s" desc reason
  | BlockParameterMismatch (label, expected, actual) -> 
    Printf.sprintf "Block '%s' parameter count mismatch: expected %d, got %d" 
      label expected actual
  | InvalidPhi (desc, reason) -> 
    Printf.sprintf "Invalid phi %s: %s" desc reason
  | DominanceViolation (use_site, def_site, reason) -> 
    Printf.sprintf "Dominance violation: %s uses value from %s (%s)" 
      use_site def_site reason
  | InvalidOperandType (instr, reason) -> 
    Printf.sprintf "Invalid operand type in %s: %s" instr reason
  | MissingEntryBlock -> 
    "Function missing entry block"
  | MultipleEntryBlocks -> 
    "Function has multiple entry blocks"
  | InvalidFunctionSignature reason -> 
    Printf.sprintf "Invalid function signature: %s" reason

(* Value definition tracking *)
module ValueEnv = struct
  type t = {
    defined: (string, value * string) Hashtbl.t;  (* value name -> (value, defining block) *)
    block_values: (string, string list) Hashtbl.t; (* block -> list of values defined *)
  }
  
  let create () = {
    defined = Hashtbl.create 64;
    block_values = Hashtbl.create 16;
  }
  
  let define name value block env =
    if Hashtbl.mem env.defined name then
      Error (RedefinedValue (name, block))
    else begin
      Hashtbl.add env.defined name (value, block);
      let block_vals = 
        try Hashtbl.find env.block_values block 
        with Not_found -> [] in
      Hashtbl.replace env.block_values block (name :: block_vals);
      Ok ()
    end
  
  let lookup name env =
    try Some (Hashtbl.find env.defined name)
    with Not_found -> None
  
  let is_defined name env = Hashtbl.mem env.defined name
end

(* Check if a type is valid for an operation *)
let check_binop_types op ty1 ty2 =
  match op with
  | Add | Sub | Mul | And | Or | Xor ->
    equal ty1 ty2 && (is_integer ty1 || is_vector ty1)
  | Sdiv | Srem | Shl | Lshr | Ashr ->
    equal ty1 ty2 && is_integer ty1
  | Udiv | Urem ->
    equal ty1 ty2 && is_integer ty1
  | Fadd | Fsub | Fmul | Fdiv | Frem ->
    equal ty1 ty2 && (is_float ty1 || (is_vector ty1 && 
      match ty1 with Vector (_, s) -> is_float (Scalar s) | _ -> false))
  | _ -> equal ty1 ty2

(* Verify instruction operands and types *)
let verify_instruction_types _env instr result =
  let _check_value_type v expected_ty ctx =
    let actual_ty = get_type v in
    if not (equal actual_ty expected_ty) then
      add_error (TypeMismatch (ctx, expected_ty, actual_ty)) result
    else result in
  
  match instr.instr with
  | Binop (op, _, v1, v2) ->
    let ty1 = get_type v1 in
    let ty2 = get_type v2 in
    if not (check_binop_types op ty1 ty2) then
      add_error (InvalidOperandType (string_of_binop op, 
        Printf.sprintf "incompatible types %s and %s" 
          (string_of_ty ty1) (string_of_ty ty2))) result
    else result
  
  | Icmp (_, v1, v2) ->
    let ty1 = get_type v1 in
    let ty2 = get_type v2 in
    if not (equal ty1 ty2 && is_integer ty1) then
      add_error (InvalidOperandType ("icmp", "requires matching integer types")) result
    else result
  
  | Fcmp (_, v1, v2) ->
    let ty1 = get_type v1 in
    let ty2 = get_type v2 in
    if not (equal ty1 ty2 && is_float ty1) then
      add_error (InvalidOperandType ("fcmp", "requires matching float types")) result
    else result
  
  | Select (cond, v_true, v_false) ->
    let cond_ty = get_type cond in
    let true_ty = get_type v_true in
    let false_ty = get_type v_false in
    let result = 
      if cond_ty <> Scalar I1 then
        add_error (InvalidOperandType ("select", "condition must be i1")) result
      else result in
    if not (equal true_ty false_ty) then
      add_error (InvalidOperandType ("select", "true and false values must have same type")) result
    else result
  
  | Memory (Store (_value, ptr)) ->
    let ptr_ty = get_type ptr in
    if ptr_ty <> Ptr then
      add_error (InvalidOperandType ("store", "destination must be pointer")) result
    else result
  
  | Memory (Memcpy (dst, src, _) | Memset (dst, src, _)) ->
    let dst_ty = get_type dst in
    let src_ty = get_type src in
    let result =
      if dst_ty <> Ptr then
        add_error (InvalidOperandType ("memory op", "destination must be pointer")) result
      else result in
    if src_ty <> Ptr && instr.instr = Memory (Memcpy (dst, src, create_simple_value (Scalar I32))) then
      add_error (InvalidOperandType ("memcpy", "source must be pointer")) result
    else result
  
  | Address (Gep (base, _) | PtrAdd (base, _)) ->
    let base_ty = get_type base in
    if base_ty <> Ptr then
      add_error (InvalidOperandType ("address op", "base must be pointer")) result
    else result
  
  | Cast cast_op ->
    (match cast_op with
     | Trunc (v, target_ty) ->
       let src_ty = get_type v in
       if not (is_integer src_ty && is_integer target_ty && 
               size_of src_ty > size_of target_ty) then
         add_error (InvalidOperandType ("trunc", "invalid truncation")) result
       else result
     | Zext (v, target_ty) | Sext (v, target_ty) ->
       let src_ty = get_type v in
       if not (is_integer src_ty && is_integer target_ty && 
               size_of src_ty < size_of target_ty) then
         add_error (InvalidOperandType ("ext", "invalid extension")) result
       else result
     | _ -> result)
  
  | _ -> result

(* Verify SSA properties *)
let verify_ssa env block result =
  List.fold_left (fun result instr ->
    match instr.result with
    | Some v ->
      let name = string_of_value v in
      (match ValueEnv.define name v block.label env with
       | Error e -> add_error e result
       | Ok () -> result)
    | None -> result
  ) result block.instructions

(* Verify block terminator *)
let verify_terminator func block result =
  let block_map = 
    List.fold_left (fun m b -> 
      Hashtbl.add m b.label b; m
    ) (Hashtbl.create 16) func.blocks in
  
  let check_target label res =
    if not (Hashtbl.mem block_map label) then
      add_error (InvalidTerminator (block.label, 
        Printf.sprintf "target block '%s' does not exist" label)) res
    else res in
  
  match block.terminator with
  | Ret None ->
    if func.return_ty <> None then
      add_error (InvalidTerminator (block.label, "missing return value")) result
    else result
  | Ret (Some v) ->
    (match func.return_ty with
     | None -> add_error (InvalidTerminator (block.label, "unexpected return value")) result
     | Some ty ->
       let v_ty = get_type v in
       if not (equal ty v_ty) then
         add_error (TypeMismatch ("return", ty, v_ty)) result
       else result)
  | Br (cond, then_lbl, else_lbl) ->
    let cond_ty = get_type cond in
    let result = 
      if cond_ty <> Scalar I1 then
        add_error (InvalidOperandType ("br", "condition must be i1")) result
      else result in
    let result = check_target then_lbl result in
    check_target else_lbl result
  | Jmp target ->
    check_target target result
  | Switch (_v, default, cases) ->
    let result = check_target default result in
    List.fold_left (fun res (_, label) -> check_target label res) result cases
  | Unreachable -> result

(* Verify control flow graph properties *)
let verify_cfg func result =
  (* Check for entry block *)
  let result = 
    match func.blocks with
    | [] -> add_error MissingEntryBlock result
    | first :: _ when first.label <> "entry" ->
      add_warning "First block should be named 'entry'" result
    | _ -> result in
  
  (* Build predecessor map *)
  let preds = Hashtbl.create 16 in
  List.iter (fun block ->
    let add_pred target = 
      let pred_list = 
        try Hashtbl.find preds target 
        with Not_found -> [] in
      Hashtbl.replace preds target (block.label :: pred_list) in
    
    match block.terminator with
    | Ret _ | Unreachable -> ()
    | Br (_, then_lbl, else_lbl) ->
      add_pred then_lbl;
      add_pred else_lbl
    | Jmp target -> add_pred target
    | Switch (_, default, cases) ->
      add_pred default;
      List.iter (fun (_, lbl) -> add_pred lbl) cases
  ) func.blocks;
  
  (* Check for unreachable blocks (except entry) *)
  List.fold_left (fun result block ->
    if block.label <> "entry" && not (Hashtbl.mem preds block.label) then
      add_error (UnreachableBlock block.label) result
    else result
  ) result (List.tl func.blocks)

(* Main linting function *)
let lint_function func =
  let result = empty_result in
  let env = ValueEnv.create () in
  
  (* Add function parameters to environment *)
  let result = List.fold_left (fun result (name, ty) ->
    match ValueEnv.define name (create_simple_value ty) "entry" env with
    | Error e -> add_error e result
    | Ok () -> result
  ) result func.params in
  
  (* Verify each block *)
  let result = List.fold_left (fun result block ->
    (* Add block parameters to environment *)
    let result = List.fold_left (fun result (name, ty) ->
      match ValueEnv.define name (create_simple_value ty) block.label env with
      | Error e -> add_error e result
      | Ok () -> result
    ) result block.params in
    
    (* Verify SSA properties *)
    let result = verify_ssa env block result in
    
    (* Verify instruction types *)
    let result = List.fold_left (fun result instr ->
      verify_instruction_types env instr result
    ) result block.instructions in
    
    (* Verify terminator *)
    verify_terminator func block result
  ) result func.blocks in
  
  (* Verify CFG properties *)
  let result = verify_cfg func result in
  
  (* Return results in reverse order (oldest first) *)
  { errors = List.rev result.errors; 
    warnings = List.rev result.warnings }

(* Lint multiple functions *)
let lint_program funcs =
  List.map (fun func -> (func.name, lint_function func)) funcs

(* Check if linting passed *)
let is_valid result = result.errors = []

(* Pretty print linting results *)
let print_results name result =
  Printf.printf "=== Linting results for %s ===\n" name;
  
  if result.errors = [] && result.warnings = [] then
    Printf.printf "✓ No issues found\n"
  else begin
    if result.errors <> [] then begin
      Printf.printf "\nErrors (%d):\n" (List.length result.errors);
      List.iter (fun err -> 
        Printf.printf "  ❌ %s\n" (string_of_error err)
      ) result.errors
    end;
    
    if result.warnings <> [] then begin
      Printf.printf "\nWarnings (%d):\n" (List.length result.warnings);
      List.iter (fun warn -> 
        Printf.printf "  ⚠️  %s\n" warn
      ) result.warnings
    end
  end;
  
  Printf.printf "\n"