(** PIR Generator - Convert C AST to PIR with type information *)

open Ast
open C_type_system
open C_scoped_symbol_table
open Compilerkit_pir.Types
open Compilerkit_pir.Values
open Compilerkit_pir.Instructions

(** PIR generation context *)
type pir_context = {
  symbol_table: c_symbol_table;
  mutable current_function: func option;
  mutable current_block: basic_block option;
  mutable blocks: basic_block list;
  mutable value_map: (string, value) Hashtbl.t;  (* Map C variables to PIR values *)
  mutable next_label: int;
  mutable break_targets: string list;    (* Stack of break targets *)
  mutable continue_targets: string list; (* Stack of continue targets *)
}

(** Create PIR generation context *)
let create_pir_context symbol_table = {
  symbol_table;
  current_function = None;
  current_block = None;
  blocks = [];
  value_map = Hashtbl.create 64;
  next_label = 0;
  break_targets = [];
  continue_targets = [];
}

(** Generate fresh label *)
let fresh_label ctx =
  let label = "L" ^ string_of_int ctx.next_label in
  ctx.next_label <- ctx.next_label + 1;
  label

(** Add instruction to current block *)
let emit_instr ctx ?result instr =
  match ctx.current_block with
  | Some block ->
    let instruction = create_simple_instruction ?result instr in
    let updated_block = { block with instructions = block.instructions @ [instruction] } in
    ctx.current_block <- Some updated_block
  | None -> failwith "No current block to emit instruction"

(** Start new basic block *)
let start_block ctx label =
  (* Finish current block if exists *)
  (match ctx.current_block with
   | Some block -> ctx.blocks <- block :: ctx.blocks
   | None -> ());
  
  (* Create new block *)
  let new_block = create_block label [] [] Unreachable in
  ctx.current_block <- Some new_block

(** Finish current block with terminator *)
let finish_block ctx terminator =
  match ctx.current_block with
  | Some block ->
    let finished_block = { block with terminator } in
    ctx.blocks <- finished_block :: ctx.blocks;
    ctx.current_block <- None
  | None -> ()

(** Convert C binary operator to PIR binary operator *)
let c_binop_to_pir_binop c_op c_type =
  match c_op, c_type with
  | Ast.Add, Int _ -> Add
  | Ast.Sub, Int _ -> Sub
  | Ast.Mul, Int _ -> Mul
  | Ast.Div, Int int_type when int_type.signed -> Sdiv
  | Ast.Div, Int int_type when not int_type.signed -> Udiv
  | Ast.Mod, Int int_type when int_type.signed -> Srem
  | Ast.Mod, Int int_type when not int_type.signed -> Urem
  | Ast.BitAnd, Int _ -> And
  | Ast.BitOr, Int _ -> Or
  | Ast.BitXor, Int _ -> Xor
  | Ast.Shl, Int _ -> Shl
  | Ast.Shr, Int int_type when int_type.signed -> Ashr
  | Ast.Shr, Int int_type when not int_type.signed -> Lshr
  | Ast.Add, Float _ -> Fadd
  | Ast.Sub, Float _ -> Fsub
  | Ast.Mul, Float _ -> Fmul
  | Ast.Div, Float _ -> Fdiv
  | Ast.Mod, Float _ -> Frem
  | _ -> failwith ("Unsupported binary operator: " ^ (match c_op with 
    | Ast.Add -> "Add" 
    | Ast.Sub -> "Sub"
    | Ast.Mul -> "Mul" 
    | Ast.Div -> "Div"
    | Ast.Mod -> "Mod"
    | Ast.Assign -> "Assign"
    | Ast.Lt -> "Lt"
    | Ast.Gt -> "Gt"
    | Ast.Le -> "Le"
    | Ast.Ge -> "Ge"
    | Ast.Eq -> "Eq"
    | Ast.Ne -> "Ne"
    | _ -> "Other"))

(** Convert C comparison to PIR comparison *)
let c_cmp_to_pir_icmp c_op c_type =
  match c_op, c_type with
  | Ast.Eq, Int _ -> Eq
  | Ast.Ne, Int _ -> Ne
  | Ast.Lt, Int int_type when int_type.signed -> Slt
  | Ast.Lt, Int int_type when not int_type.signed -> Ult
  | Ast.Le, Int int_type when int_type.signed -> Sle
  | Ast.Le, Int int_type when not int_type.signed -> Ule
  | Ast.Gt, Int int_type when int_type.signed -> Sgt
  | Ast.Gt, Int int_type when not int_type.signed -> Ugt
  | Ast.Ge, Int int_type when int_type.signed -> Sge
  | Ast.Ge, Int int_type when not int_type.signed -> Uge
  | _ -> failwith "Unsupported comparison operator"

(** Generate PIR for expressions *)
let rec gen_expr ctx expr expected_type =
  match expr with
  | IntLit (value, _) ->
    let const_val = ConstInt (value, I64) in  (* Simplified - should use proper type *)
    let result_value = create_simple_value (Scalar I64) in
    emit_instr ctx ~result:result_value (Const const_val);
    result_value
  
  | FloatLit (value, _) ->
    let const_val = ConstFloat (value, F64) in
    let result_value = create_simple_value (Scalar F64) in
    emit_instr ctx ~result:result_value (Const const_val);
    result_value
  
  | CharLit c ->
    let const_val = ConstInt (Int64.of_int (Char.code c), I8) in
    let result_value = create_simple_value (Scalar I8) in
    emit_instr ctx ~result:result_value (Const const_val);
    result_value
  
  | StringLit s ->
    (* String literals become global arrays *)
    let const_val = ConstArray (List.init (String.length s + 1) (fun i ->
      if i < String.length s then
        ConstInt (Int64.of_int (Char.code s.[i]), I8)
      else
        ConstInt (0L, I8)  (* null terminator *)
    )) in
    let result_value = create_simple_value Ptr in
    emit_instr ctx ~result:result_value (Const const_val);
    result_value
  
  | Ident name ->
    (match Hashtbl.find_opt ctx.value_map name with
     | Some value -> value
     | None ->
       (* Variable not found in value map - create load from global/local *)
       match lookup_symbol ctx.symbol_table name with
       | Some sym ->
         (match c_type_to_pir_type sym.c_type with
          | Some pir_ty ->
            let result_value = create_simple_value pir_ty in
            (* For now, assume all identifiers are loads from memory *)
            let addr_value = create_simple_value Ptr in
            emit_instr ctx ~result:result_value (Memory (Load (pir_ty, addr_value)));
            result_value
          | None -> failwith ("Cannot convert C type to PIR: " ^ string_of_c_type sym.c_type))
       | None -> failwith ("Undeclared identifier: " ^ name))
  
  | BinOp (op, left, right) ->
    let left_val = gen_expr ctx left expected_type in
    let right_val = gen_expr ctx right expected_type in
    
    (match op with
     (* Arithmetic and bitwise operators *)
     | Add | Sub | Mul | Div | Mod | BitAnd | BitOr | BitXor | Shl | Shr ->
       (* TODO: Proper type derivation needed *)
       let pir_op = c_binop_to_pir_binop op expected_type in
       let result_value = create_simple_value (get_type left_val) in
       emit_instr ctx ~result:result_value (Binop (pir_op, NoFlag, left_val, right_val));
       result_value
     
     (* Comparison operators *)
     | Lt | Gt | Le | Ge | Eq | Ne ->
       let icmp_pred = c_cmp_to_pir_icmp op expected_type in
       let result_value = create_simple_value (Scalar I1) in
       emit_instr ctx ~result:result_value (Icmp (icmp_pred, left_val, right_val));
       result_value
     
     (* Logical operators *)
     | LogAnd ->
       (* Implement short-circuit evaluation *)
       let true_label = fresh_label ctx in
       let false_label = fresh_label ctx in
       let end_label = fresh_label ctx in
       
       (* Evaluate left operand *)
       finish_block ctx (Br (left_val, true_label, false_label));
       
       (* True branch: evaluate right operand *)
       start_block ctx true_label;
       let right_val = gen_expr ctx right expected_type in
       finish_block ctx (Jmp end_label);
       
       (* False branch: result is false *)
       start_block ctx false_label;
       let false_val = create_simple_value (Scalar I1) in
       emit_instr ctx ~result:false_val (Const (ConstBool false));
       finish_block ctx (Jmp end_label);
       
       (* End: phi node to select result *)
       start_block ctx end_label;
       let result_value = create_simple_value (Scalar I1) in
       emit_instr ctx ~result:result_value (Phi [(right_val, true_label); (false_val, false_label)]);
       result_value
     
     | LogOr ->
       (* Similar to LogAnd but with inverted logic *)
       let true_label = fresh_label ctx in
       let false_label = fresh_label ctx in
       let end_label = fresh_label ctx in
       
       finish_block ctx (Br (left_val, true_label, false_label));
       
       start_block ctx true_label;
       let true_val = create_simple_value (Scalar I1) in
       emit_instr ctx ~result:true_val (Const (ConstBool true));
       finish_block ctx (Jmp end_label);
       
       start_block ctx false_label;
       let right_val = gen_expr ctx right expected_type in
       finish_block ctx (Jmp end_label);
       
       start_block ctx end_label;
       let result_value = create_simple_value (Scalar I1) in
       emit_instr ctx ~result:result_value (Phi [(true_val, true_label); (right_val, false_label)]);
       result_value
     
     (* Assignment operators *)
     | Assign ->
       (* For assignment, left side should be an lvalue *)
       (* Simplified: assume left is identifier *)
       (match left with
        | Ident name ->
          Hashtbl.replace ctx.value_map name right_val;
          right_val
        | _ -> failwith "Complex assignment not yet implemented")
     
     | _ -> failwith ("Binary operator not implemented: " ^ (match op with Add -> "Add" | _ -> "Other")))
  
  | UnOp (op, operand) ->
    let operand_val = gen_expr ctx operand expected_type in
    
    (match op with
     | Minus ->
       let zero_val = create_simple_value (get_type operand_val) in
       emit_instr ctx ~result:zero_val (Const (ConstInt (0L, I64)));
       let result_value = create_simple_value (get_type operand_val) in
       emit_instr ctx ~result:result_value (Binop (Sub, NoFlag, zero_val, operand_val));
       result_value
     
     | Plus -> operand_val  (* Unary plus is no-op *)
     
     | Not ->
       let result_value = create_simple_value (Scalar I1) in
       let zero_val = create_simple_value (get_type operand_val) in
       emit_instr ctx ~result:zero_val (Const (ConstInt (0L, I64)));
       emit_instr ctx ~result:result_value (Icmp (Eq, operand_val, zero_val));
       result_value
     
     | BitNot ->
       let result_value = create_simple_value (get_type operand_val) in
       let all_ones = create_simple_value (get_type operand_val) in
       emit_instr ctx ~result:all_ones (Const (ConstInt (-1L, I64)));
       emit_instr ctx ~result:result_value (Binop (Xor, NoFlag, operand_val, all_ones));
       result_value
     
     | Deref ->
       (match get_type operand_val with
        | Ptr ->
          let result_type = expected_type |> c_type_to_pir_type |> function Some t -> t | None -> Scalar I32 in
          let result_value = create_simple_value result_type in
          emit_instr ctx ~result:result_value (Memory (Load (result_type, operand_val)));
          result_value
        | _ -> failwith "Dereference of non-pointer")
     
     | AddrOf ->
       (* Address-of: return pointer to operand *)
       let result_value = create_simple_value Ptr in
       (* For now, simplified - should handle different lvalue types *)
       result_value
     
     | _ -> failwith ("Unary operator not implemented"))
  
  | TernOp (cond, true_expr, false_expr) ->
    let cond_val = gen_expr ctx cond expected_type in
    let true_label = fresh_label ctx in
    let false_label = fresh_label ctx in
    let end_label = fresh_label ctx in
    
    finish_block ctx (Br (cond_val, true_label, false_label));
    
    start_block ctx true_label;
    let true_val = gen_expr ctx true_expr expected_type in
    finish_block ctx (Jmp end_label);
    
    start_block ctx false_label;
    let false_val = gen_expr ctx false_expr expected_type in
    finish_block ctx (Jmp end_label);
    
    start_block ctx end_label;
    let result_value = create_simple_value (get_type true_val) in
    emit_instr ctx ~result:result_value (Phi [(true_val, true_label); (false_val, false_label)]);
    result_value
  
  | FuncCall (func_expr, arg_exprs) ->
    let func_val = gen_expr ctx func_expr expected_type in
    let arg_vals = List.map (fun arg -> gen_expr ctx arg expected_type) arg_exprs in
    let result_type = expected_type |> c_type_to_pir_type |> function Some t -> t | None -> Scalar I32 in
    let result_value = create_simple_value result_type in
    emit_instr ctx ~result:result_value (Call (Call (func_val, arg_vals)));
    result_value
  
  | _ -> failwith "Expression not implemented"

(** Generate PIR for statements *)
let rec gen_stmt ctx = function
  | ExprStmt (Some expr) ->
    (* Evaluate expression but ignore result *)
    let _ = gen_expr ctx expr Void in
    ()
  
  | ExprStmt None -> ()
  
  | CompoundStmt block_items ->
    gen_block_items ctx block_items
  
  | IfStmt (cond, then_stmt, else_stmt) ->
    let cond_val = gen_expr ctx cond Bool in
    let then_label = fresh_label ctx in
    let else_label = fresh_label ctx in
    let end_label = fresh_label ctx in
    
    finish_block ctx (Br (cond_val, then_label, else_label));
    
    start_block ctx then_label;
    gen_stmt ctx then_stmt;
    finish_block ctx (Jmp end_label);
    
    start_block ctx else_label;
    (match else_stmt with
     | Some stmt -> gen_stmt ctx stmt
     | None -> ());
    finish_block ctx (Jmp end_label);
    
    start_block ctx end_label
  
  | WhileStmt (cond, body) ->
    let cond_label = fresh_label ctx in
    let body_label = fresh_label ctx in
    let end_label = fresh_label ctx in
    
    (* Set up break/continue targets *)
    ctx.break_targets <- end_label :: ctx.break_targets;
    ctx.continue_targets <- cond_label :: ctx.continue_targets;
    
    finish_block ctx (Jmp cond_label);
    
    start_block ctx cond_label;
    let cond_val = gen_expr ctx cond Bool in
    finish_block ctx (Br (cond_val, body_label, end_label));
    
    start_block ctx body_label;
    gen_stmt ctx body;
    finish_block ctx (Jmp cond_label);
    
    start_block ctx end_label;
    
    (* Restore break/continue targets *)
    ctx.break_targets <- List.tl ctx.break_targets;
    ctx.continue_targets <- List.tl ctx.continue_targets
  
  | ForStmt (init, cond, update, body) ->
    let cond_label = fresh_label ctx in
    let body_label = fresh_label ctx in
    let update_label = fresh_label ctx in
    let end_label = fresh_label ctx in
    
    (* Set up break/continue targets *)
    ctx.break_targets <- end_label :: ctx.break_targets;
    ctx.continue_targets <- update_label :: ctx.continue_targets;
    
    (* Initialize *)
    (match init with
     | Some expr -> let _ = gen_expr ctx expr Void in ()
     | None -> ());
    
    finish_block ctx (Jmp cond_label);
    
    (* Condition check *)
    start_block ctx cond_label;
    (match cond with
     | Some expr ->
       let cond_val = gen_expr ctx expr Bool in
       finish_block ctx (Br (cond_val, body_label, end_label))
     | None ->
       finish_block ctx (Jmp body_label));
    
    (* Body *)
    start_block ctx body_label;
    gen_stmt ctx body;
    finish_block ctx (Jmp update_label);
    
    (* Update *)
    start_block ctx update_label;
    (match update with
     | Some expr -> let _ = gen_expr ctx expr Void in ()
     | None -> ());
    finish_block ctx (Jmp cond_label);
    
    start_block ctx end_label;
    
    (* Restore break/continue targets *)
    ctx.break_targets <- List.tl ctx.break_targets;
    ctx.continue_targets <- List.tl ctx.continue_targets
  
  | BreakStmt ->
    (match ctx.break_targets with
     | target :: _ -> finish_block ctx (Jmp target); start_block ctx (fresh_label ctx)
     | [] -> failwith "Break outside loop")
  
  | ContinueStmt ->
    (match ctx.continue_targets with
     | target :: _ -> finish_block ctx (Jmp target); start_block ctx (fresh_label ctx)
     | [] -> failwith "Continue outside loop")
  
  | ReturnStmt expr_opt ->
    (match expr_opt with
     | Some expr ->
       let return_val = gen_expr ctx expr Void in
       finish_block ctx (Ret (Some return_val))
     | None ->
       finish_block ctx (Ret None));
    start_block ctx (fresh_label ctx)  (* Dead code block *)
  
  | _ -> failwith "Statement not implemented"

and gen_block_items ctx = function
  | [] -> ()
  | item :: rest ->
    (match item with
     | Stmt stmt -> gen_stmt ctx stmt
     | Decl decl -> gen_declaration ctx decl);
    gen_block_items ctx rest

(** Generate PIR for declarations (add variables to symbol table) *)
and gen_declaration ctx decl =
  let { storage; specs; quals; init_decls } = decl in
  
  (* Resolve base type from type specifiers *)
  let base_type = resolve_type_specs ctx.symbol_table.type_env specs quals in
  
  (* Process each declarator *)
  List.iter (fun init_decl ->
    let { decl = declarator; init = _ } = init_decl in
    
    (* Process declarator to get full type and name *)
    let var_type, var_name = process_declarator ctx.symbol_table.type_env base_type quals declarator in
    
    (* Add symbol to symbol table *)
    match define_symbol ctx.symbol_table var_name var_type storage ({Lexer.filename = "<unknown>"; line = 0; column = 0}) with
    | Ok _ -> ()
    | Error _ -> failwith ("Failed to define variable: " ^ var_name)
  ) init_decls

(** Generate PIR for function definition *)
let gen_function_def ctx func_def =
  let { storage = _; specs; quals; declarator; body; _ } = func_def in
  
  (* Resolve function type *)
  let base_type = resolve_type_specs ctx.symbol_table.type_env specs quals in
  let func_type, func_name = process_declarator ctx.symbol_table.type_env base_type quals declarator in
  
  (* Find function symbol and enter its scope *)
  (match lookup_symbol ctx.symbol_table func_name with
   | Some func_symbol ->
     enter_function_scope ctx.symbol_table func_symbol
   | None ->
     failwith ("Function not found in symbol table: " ^ func_name));
  
  (* Extract function parameters and return type *)
  let (return_ty, params, _varargs) = match func_type with
    | Function (ret, params, va) -> (ret, params, va)
    | _ -> failwith "Not a function type"
  in
  
  (* Convert to PIR types *)
  let pir_return_ty = match return_ty with
    | Some ty -> c_type_to_pir_type ty
    | None -> None
  in
  
  let pir_params = List.filter_map (fun param ->
    match c_type_to_pir_type param.param_type with
    | Some pir_ty -> 
      let name = param.param_name |> function Some n -> n | None -> "param" in
      Some (name, pir_ty)
    | None -> None
  ) params in
  
  (* Create function entry block *)
  ctx.blocks <- [];
  start_block ctx "entry";
  
  (* Generate function body *)
  gen_stmt ctx body;
  
  (* Ensure function ends with return *)
  (match return_ty with
   | None -> finish_block ctx (Ret None)
   | Some _ ->
     (* If no explicit return, add return with default value *)
     let default_val = create_simple_value (Scalar I32) in
     emit_instr ctx ~result:default_val (Const (ConstInt (0L, I32)));
     finish_block ctx (Ret (Some default_val)));
  
  (* Create PIR function *)
  let pir_func = create_func func_name pir_params pir_return_ty (List.rev ctx.blocks) in
  ctx.current_function <- Some pir_func;
  
  (* Exit function scope *)
  exit_function_scope ctx.symbol_table;
  
  pir_func

(** Generate PIR for translation unit *)
let gen_translation_unit symbol_table translation_unit =
  let ctx = create_pir_context symbol_table in
  
  let functions = List.filter_map (function
    | FuncDef func_def -> Some (gen_function_def ctx func_def)
    | Decl _decl -> None  (* TODO: Handle global declarations *)
  ) translation_unit in
  
  functions

(** Main PIR generation interface *)
let generate_pir symbol_table translation_unit =
  try
    let functions = gen_translation_unit symbol_table translation_unit in
    Ok functions
  with
  | e -> Error (Printexc.to_string e)