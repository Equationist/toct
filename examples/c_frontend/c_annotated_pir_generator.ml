(** PIR Generator for Annotated C AST - Uses type and scope information from annotations *)

open Ast
open C_type_system
open C_scoped_symbol_table
open C_annotated_ast
open Compilerkit_frontend.Ast_annotation
open Compilerkit_pir.Types
open Compilerkit_pir.Values
open Compilerkit_pir.Instructions

(** PIR generation context for annotated AST *)
type annotated_pir_context = {
  symbol_table: c_symbol_table;
  mutable current_function: func option;
  mutable current_block: basic_block option;
  mutable blocks: basic_block list;
  mutable value_map: (string, value) Hashtbl.t;
  mutable next_label: int;
  mutable break_targets: string list;
  mutable continue_targets: string list;
  (* Track assignments per block for proper SSA form *)
  mutable block_assignments: (string, (string * value) list) Hashtbl.t; (* block_label -> [(var_name, value)] *)
  (* Track variable types for block parameters *)
  mutable var_types: (string, ty) Hashtbl.t; (* var_name -> PIR type *)
}

(** Create PIR generation context *)
let create_annotated_pir_context symbol_table = {
  symbol_table;
  current_function = None;
  current_block = None;
  blocks = [];
  value_map = Hashtbl.create 64;
  next_label = 0;
  break_targets = [];
  continue_targets = [];
  block_assignments = Hashtbl.create 16;
  var_types = Hashtbl.create 64;
}

(** Generate fresh label *)
let fresh_label ctx =
  let label = Printf.sprintf "L%d" ctx.next_label in
  ctx.next_label <- ctx.next_label + 1;
  label

(** Start new basic block *)
let start_block ctx label =
  let block = create_block label [] [] (Ret None) in
  ctx.current_block <- Some block

(** Start new basic block with parameters *)
let start_block_with_params ctx label params =
  let block = create_block label params [] (Ret None) in
  ctx.current_block <- Some block

(** Emit instruction to current block *)
let emit_instr ctx ?result instr =
  match ctx.current_block with
  | Some block ->
    let instruction = create_simple_instruction ?result instr in
    let updated_block = { block with instructions = instruction :: block.instructions } in
    ctx.current_block <- Some updated_block;
  | None -> failwith "No current block to emit instruction to"

(** Finish current block with terminator *)
let finish_block ctx terminator =
  match ctx.current_block with
  | Some block ->
    let finished_block = { block with 
      instructions = List.rev block.instructions;
      terminator = terminator 
    } in
    ctx.blocks <- finished_block :: ctx.blocks;
    ctx.current_block <- None
  | None -> ()

(** Convert C binary operator to PIR binary operator using type information *)
let c_binop_to_pir_binop_annotated c_op c_type =
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
  | _ -> failwith ("Unsupported binary operator combination: " ^ 
                   (match c_op with Ast.Add -> "Add" | Ast.Mul -> "Mul" | _ -> "Other") ^
                   " with type " ^ string_of_c_type c_type)

(** Convert C comparison to PIR comparison using type information *)
let c_cmp_to_pir_icmp_annotated c_op c_type =
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
  | _ -> failwith ("Unsupported comparison operator")

(** Generate PIR for annotated expressions *)
let rec gen_annotated_expr ctx annotated_expr =
  let expr = get_node annotated_expr in
  let _info = get_info annotated_expr in
  
  match expr with
  | IntLit (value, _) ->
    let result_value = create_simple_value (Scalar I32) in
    emit_instr ctx ~result:result_value (Const (ConstInt (value, I32)));
    result_value

  | Ident name ->
    (match Hashtbl.find_opt ctx.value_map name with
     | Some value -> value
     | None ->
       (* Try to look up in symbol table *)
       match lookup_symbol ctx.symbol_table name with
       | Some symbol ->
         (match symbol.c_type with
          | Function (_, _, _) ->
            (* For functions, create a pointer value representing the function *)
            let func_value = create_simple_value Ptr in
            (* Attach the function name as an attribute *)
            let func_value_with_name = 
              add_attr "function_name" (Compilerkit_pir.Attributes.String name) func_value in
            Hashtbl.add ctx.value_map name func_value_with_name;
            func_value_with_name
          | Array (_, _, _) ->
            (* For arrays, we should have already allocated them *)
            failwith (Printf.sprintf "Array '%s' not found in value map - was it properly allocated?" name)
          | _ ->
            (match c_type_to_pir_type symbol.c_type with
             | Some pir_ty ->
               let value = create_simple_value pir_ty in
               Hashtbl.add ctx.value_map name value;
               Hashtbl.replace ctx.var_types name pir_ty;
               value
             | None -> failwith ("Cannot convert C type to PIR: " ^ string_of_c_type symbol.c_type)))
       | None -> failwith ("Undeclared identifier: " ^ name))

  | BinOp (op, left, right) ->
    (* Generate PIR for raw sub-expressions *)
    let left_val = gen_annotated_expr_raw ctx left in
    let right_val = gen_annotated_expr_raw ctx right in
    
    (match op with
     (* Arithmetic and bitwise operators *)
     | Add | Sub | Mul | Div | Mod | BitAnd | BitOr | BitXor | Shl | Shr ->
       (* Infer type from operands since annotations aren't preserved *)
       let operand_type = match get_type left_val with
         | Scalar I8 -> Int c_char
         | Scalar I16 -> Int c_short
         | Scalar I32 -> Int c_int
         | Scalar I64 -> Int c_long
         | Scalar F32 -> Float c_float
         | Scalar F64 -> Float c_double
         | _ -> Int c_int  (* Default to int *)
       in
       let pir_op = c_binop_to_pir_binop_annotated op operand_type in
       let result_value = create_simple_value (get_type left_val) in
       emit_instr ctx ~result:result_value (Binop (pir_op, NoFlag, left_val, right_val));
       result_value
     
     (* Comparison operators *)
     | Lt | Gt | Le | Ge | Eq | Ne ->
       (* Infer type from operands *)
       let operand_type = match get_type left_val with
         | Scalar I8 -> Int c_char
         | Scalar I16 -> Int c_short
         | Scalar I32 -> Int c_int
         | Scalar I64 -> Int c_long
         | _ -> Int c_int  (* Default to int *)
       in
       let icmp_pred = c_cmp_to_pir_icmp_annotated op operand_type in
       let result_value = create_simple_value (Scalar I1) in
       emit_instr ctx ~result:result_value (Icmp (icmp_pred, left_val, right_val));
       result_value
     
     (* Assignment operators *)
     | Assign ->
       (match left with
        | Ident name ->
          Hashtbl.replace ctx.value_map name right_val;
          (* Track variable type *)
          Hashtbl.replace ctx.var_types name (get_type right_val);
          (* Track assignment in current block *)
          (match ctx.current_block with
           | Some block ->
             let assignments = 
               match Hashtbl.find_opt ctx.block_assignments block.label with
               | Some assns -> assns
               | None -> []
             in
             Hashtbl.replace ctx.block_assignments block.label ((name, right_val) :: (List.filter (fun (n, _) -> n <> name) assignments))
           | None -> ());
          right_val
        | ArrayRef (array_expr, index_expr) ->
          (* Generate array base address *)
          let array_ptr = gen_annotated_expr_raw ctx array_expr in
          (* Generate index *)
          let index_val = gen_annotated_expr_raw ctx index_expr in
          
          (* Calculate element address using GEP *)
          let elem_ptr = create_simple_value Ptr in
          emit_instr ctx ~result:elem_ptr (Address (Gep (array_ptr, index_val)));
          
          (* Store the value to the computed address *)
          emit_instr ctx (Memory (Store (right_val, elem_ptr)));
          right_val
        | _ -> failwith "Complex assignment not yet implemented")
     
     | _ -> failwith ("Binary operator not implemented"))

  | FuncCall (func_expr, arg_exprs) ->
    (* Generate value for the function *)
    let func_val = gen_annotated_expr_raw ctx func_expr in
    
    (* Generate PIR for arguments *)
    let arg_vals = List.map (gen_annotated_expr_raw ctx) arg_exprs in
    
    (* Determine return type - for now assume i32 *)
    let return_type = Scalar I32 in
    let result_value = create_simple_value return_type in
    
    (* Emit call instruction *)
    emit_instr ctx ~result:result_value (Call (Call (func_val, arg_vals)));
    result_value
    
  | ArrayRef (array_expr, index_expr) ->
    (* Generate array base address *)
    let array_ptr = gen_annotated_expr_raw ctx array_expr in
    (* Generate index *)
    let index_val = gen_annotated_expr_raw ctx index_expr in
    
    (* Calculate element address using GEP *)
    let elem_ptr = create_simple_value Ptr in
    emit_instr ctx ~result:elem_ptr (Address (Gep (array_ptr, index_val)));
    
    (* Load the value from the computed address *)
    let result_val = create_simple_value (Scalar I32) in
    emit_instr ctx ~result:result_val (Memory (Load (Scalar I32, elem_ptr)));
    result_val
    
  | StringLit str ->
    (* For string literals, we need to create a global string constant *)
    (* For now, just create a pointer value that represents the string *)
    let ptr_val = create_simple_value Ptr in
    (* Attach the string content as an attribute for the backend to handle *)
    let ptr_with_str = add_attr "string_literal" (Compilerkit_pir.Attributes.String str) ptr_val in
    ptr_with_str
    
  | _ ->
    failwith "Expression not implemented for annotated AST"

(** Generate PIR for raw expressions (creates minimal annotations) *)
and gen_annotated_expr_raw ctx expr =
  (* Create a minimal annotation and process *)
  let annotated = annotate expr empty_c_info in
  gen_annotated_expr ctx annotated

(** Generate PIR for annotated statements *)
let rec gen_annotated_stmt ctx annotated_stmt =
  let stmt = get_node annotated_stmt in
  
  match stmt with
  | ExprStmt (Some expr) ->
    let annotated_expr = annotate expr empty_c_info in
    let _ = gen_annotated_expr ctx annotated_expr in
    ()
  
  | ExprStmt None -> ()
  
  | CompoundStmt block_items ->
    List.iter (gen_annotated_block_item ctx) (List.map (fun item -> annotate item empty_c_info) block_items)
  
  | ReturnStmt expr_opt ->
    (match expr_opt with
     | Some expr ->
       let annotated_expr = annotate expr empty_c_info in
       let ret_val = gen_annotated_expr ctx annotated_expr in
       finish_block ctx (Ret (Some ret_val))
     | None ->
       finish_block ctx (Ret None))
  
  | IfStmt (cond_expr, then_stmt, else_stmt_opt) ->
    (* Save current variable bindings *)
    let initial_bindings = Hashtbl.copy ctx.value_map in
    
    (* Generate condition *)
    let cond_val = gen_annotated_expr_raw ctx cond_expr in
    
    (* Create labels for branches *)
    let then_label = fresh_label ctx in
    let else_label = fresh_label ctx in
    let end_label = fresh_label ctx in
    
    (* Branch on condition *)
    finish_block ctx (Br (cond_val, then_label, match else_stmt_opt with Some _ -> else_label | None -> end_label));
    
    (* Generate then branch *)
    start_block ctx then_label;
    gen_annotated_stmt ctx (annotate then_stmt empty_c_info);
    let then_assignments = 
      match Hashtbl.find_opt ctx.block_assignments then_label with
      | Some assns -> assns
      | None -> []
    in
    let then_terminated = ctx.current_block = None in
    
    (* Save then branch variable bindings *)
    let then_bindings = Hashtbl.copy ctx.value_map in
    
    (* Restore initial bindings for else branch *)
    ctx.value_map <- Hashtbl.copy initial_bindings;
    
    (* Generate else branch if present *)
    let else_assignments, else_terminated, else_bindings = match else_stmt_opt with
     | Some else_stmt ->
       start_block ctx else_label;
       gen_annotated_stmt ctx (annotate else_stmt empty_c_info);
       let assns = 
         match Hashtbl.find_opt ctx.block_assignments else_label with
         | Some assns -> assns
         | None -> []
       in
       let else_bindings = Hashtbl.copy ctx.value_map in
       (assns, ctx.current_block = None, Some else_bindings)
     | None -> ([], false, None)
    in
    
    (* Collect variables that were assigned in either branch *)
    let assigned_vars = 
      let vars = Hashtbl.create 16 in
      List.iter (fun (var, _) -> Hashtbl.replace vars var ()) then_assignments;
      List.iter (fun (var, _) -> Hashtbl.replace vars var ()) else_assignments;
      Hashtbl.fold (fun var _ acc -> var :: acc) vars []
    in
    
    
    (* Create block parameters for the merge block *)
    let block_params = List.map (fun var ->
      (* Get the type from var_types (tracked during assignment) *)
      match Hashtbl.find_opt ctx.var_types var with
      | Some ty -> (var, ty)
      | None -> 
        (* Try symbol table as fallback for function parameters *)
        match lookup_symbol ctx.symbol_table var with
        | Some sym -> 
          (match c_type_to_pir_type sym.c_type with
           | Some ty -> 
             (* Cache it for future use *)
             Hashtbl.replace ctx.var_types var ty;
             (var, ty)
           | None -> failwith ("Cannot determine PIR type for variable: " ^ var))
        | None -> failwith ("Variable not found: " ^ var)
    ) assigned_vars in
    
    (* Jump from then branch with appropriate values *)
    if not then_terminated then begin
      ctx.value_map <- then_bindings;
      let then_args = List.map (fun var ->
        match Hashtbl.find_opt ctx.value_map var with
        | Some v -> 
          v
        | None -> 
          (* Variable not assigned in this branch, use initial value *)
          match Hashtbl.find_opt initial_bindings var with
          | Some v -> v
          | None -> failwith ("Variable not found: " ^ var)
      ) assigned_vars in
      finish_block ctx (Jmp (end_label, then_args))
    end;
    
    (* Jump from else branch with appropriate values *)
    if else_stmt_opt <> None && not else_terminated then begin
      match else_bindings with
      | Some bindings ->
        ctx.value_map <- bindings;
        let else_args = List.map (fun var ->
          match Hashtbl.find_opt ctx.value_map var with
          | Some v -> 
            v
          | None ->
            (* Variable not assigned in this branch, use initial value *)
            match Hashtbl.find_opt initial_bindings var with
            | Some v -> v
            | None -> failwith ("Variable not found: " ^ var)
        ) assigned_vars in
        finish_block ctx (Jmp (end_label, else_args))
      | None -> ()
    end;
    
    (* If there's no else branch, create one that jumps to end with initial values *)
    if else_stmt_opt = None then begin
      start_block ctx else_label;
      let else_args = List.map (fun var ->
        match Hashtbl.find_opt initial_bindings var with
        | Some v -> v
        | None -> failwith ("Variable not found: " ^ var)
      ) assigned_vars in
      finish_block ctx (Jmp (end_label, else_args))
    end;
    
    (* Start merge block with parameters *)
    start_block_with_params ctx end_label block_params;
    
    (* Update value map with block parameters *)
    List.iter (fun (var, ty) ->
      (* Create a new SSA value for each block parameter *)
      let param_value = create_simple_value ty in
      Hashtbl.replace ctx.value_map var param_value
    ) block_params
  
  | WhileStmt (cond_expr, body_stmt) ->
    (* Save current variable bindings *)
    let initial_bindings = Hashtbl.copy ctx.value_map in
    
    (* Create labels *)
    let loop_header = fresh_label ctx in
    let loop_body = fresh_label ctx in
    let loop_end = fresh_label ctx in
    
    (* We need to do a preliminary analysis to find loop-carried variables *)
    (* For now, we'll collect all variables that might be modified in the loop *)
    let potential_loop_vars = ref [] in
    
    (* Simple heuristic: collect all variables referenced in the condition *)
    let rec collect_vars_in_expr expr =
      match expr with
      | Ident name -> 
        if not (List.mem name !potential_loop_vars) then
          potential_loop_vars := name :: !potential_loop_vars
      | BinOp (_, left, right) ->
        collect_vars_in_expr left;
        collect_vars_in_expr right
      | UnOp (_, operand) ->
        collect_vars_in_expr operand
      | _ -> ()
    in
    collect_vars_in_expr cond_expr;
    
    (* Create block parameters for loop header *)
    let loop_params = List.filter_map (fun var ->
      match Hashtbl.find_opt ctx.var_types var with
      | Some ty -> Some (var, ty)
      | None ->
        match lookup_symbol ctx.symbol_table var with
        | Some sym ->
          (match c_type_to_pir_type sym.c_type with
           | Some ty -> 
             Hashtbl.replace ctx.var_types var ty;
             Some (var, ty)
           | None -> None)
        | None -> None
    ) !potential_loop_vars in
    
    (* Initial jump to loop header with initial values *)
    let initial_args = List.map (fun var ->
      match Hashtbl.find_opt ctx.value_map var with
      | Some v -> v
      | None -> failwith ("Variable not found for loop: " ^ var)
    ) !potential_loop_vars in
    finish_block ctx (Jmp (loop_header, initial_args));
    
    (* Loop header - evaluate condition *)
    start_block_with_params ctx loop_header loop_params;
    
    (* Update value map with block parameters *)
    List.iter (fun (var, ty) ->
      let param_value = create_simple_value ty in
      Hashtbl.replace ctx.value_map var param_value
    ) loop_params;
    
    let cond_val = gen_annotated_expr_raw ctx cond_expr in
    finish_block ctx (Br (cond_val, loop_body, loop_end));
    
    (* Loop body *)
    start_block ctx loop_body;
    
    (* Track assignments in loop body *)
    let body_label = loop_body in
    Hashtbl.replace ctx.block_assignments body_label [];
    
    gen_annotated_stmt ctx (annotate body_stmt empty_c_info);
    
    (* Collect values to pass back to loop header *)
    let loop_args = List.map (fun var ->
      match Hashtbl.find_opt ctx.value_map var with
      | Some v -> v
      | None -> 
        (* Variable not modified in loop, use value from header *)
        match Hashtbl.find_opt initial_bindings var with
        | Some v -> v
        | None -> failwith ("Variable not found: " ^ var)
    ) !potential_loop_vars in
    
    (* Jump back to header if block wasn't terminated *)
    (match ctx.current_block with
     | Some _ -> finish_block ctx (Jmp (loop_header, loop_args))
     | None -> ());
    
    (* Continue after loop *)
    start_block ctx loop_end;
    
    (* Restore variable mappings after loop *)
    (* Variables modified in loop should use their final values *)
    List.iter (fun var ->
      match List.assoc_opt var loop_params with
      | Some ty ->
        (* This variable was a loop parameter, keep its last value *)
        let param_value = create_simple_value ty in
        Hashtbl.replace ctx.value_map var param_value
      | None -> ()
    ) !potential_loop_vars
  
  | ForStmt (init_opt, cond_opt, update_opt, body_stmt) ->
    (* Generate initialization if present *)
    (match init_opt with
     | Some init_expr -> 
       let _ = gen_annotated_expr_raw ctx init_expr in ()
     | None -> ());
    
    (* Create labels *)
    let loop_header = fresh_label ctx in
    let loop_body = fresh_label ctx in
    let loop_continue = fresh_label ctx in
    let loop_end = fresh_label ctx in
    
    (* Collect potential loop variables from condition, update, and body *)
    let potential_loop_vars = ref [] in
    
    let rec collect_vars_in_expr expr =
      match expr with
      | Ident name -> 
        if not (List.mem name !potential_loop_vars) then
          potential_loop_vars := name :: !potential_loop_vars
      | BinOp (_, left, right) ->
        collect_vars_in_expr left;
        collect_vars_in_expr right
      | UnOp (_, operand) ->
        collect_vars_in_expr operand
      | FuncCall (func_expr, arg_exprs) ->
        collect_vars_in_expr func_expr;
        List.iter collect_vars_in_expr arg_exprs
      | _ -> ()
    in
    
    (* Collect from condition *)
    (match cond_opt with
     | Some cond_expr -> collect_vars_in_expr cond_expr
     | None -> ());
    
    (* Collect from update *)
    (match update_opt with
     | Some update_expr -> collect_vars_in_expr update_expr
     | None -> ());
    
    (* Create block parameters for loop header *)
    let loop_params = List.filter_map (fun var ->
      match Hashtbl.find_opt ctx.var_types var with
      | Some ty -> Some (var, ty)
      | None ->
        match lookup_symbol ctx.symbol_table var with
        | Some sym ->
          (match c_type_to_pir_type sym.c_type with
           | Some ty -> 
             Hashtbl.replace ctx.var_types var ty;
             Some (var, ty)
           | None -> None)
        | None -> None
    ) !potential_loop_vars in
    
    (* Initial jump to loop header with initial values *)
    let initial_args = List.map (fun var ->
      match Hashtbl.find_opt ctx.value_map var with
      | Some v -> v
      | None -> failwith ("Variable not found for loop: " ^ var)
    ) !potential_loop_vars in
    finish_block ctx (Jmp (loop_header, initial_args));
    
    (* Loop header - evaluate condition *)
    start_block_with_params ctx loop_header loop_params;
    
    (* Update value map with block parameters *)
    List.iter (fun (var, ty) ->
      let param_value = create_simple_value ty in
      Hashtbl.replace ctx.value_map var param_value
    ) loop_params;
    
    let cond_val = match cond_opt with
      | Some cond_expr -> gen_annotated_expr_raw ctx cond_expr
      | None -> 
        (* No condition means always true *)
        let true_val = create_simple_value (Scalar I1) in
        emit_instr ctx ~result:true_val (Const (ConstInt (1L, I1)));
        true_val
    in
    finish_block ctx (Br (cond_val, loop_body, loop_end));
    
    (* Loop body *)
    start_block ctx loop_body;
    gen_annotated_stmt ctx (annotate body_stmt empty_c_info);
    
    (* Collect current values for continue block *)
    let body_end_values = List.map (fun var ->
      match Hashtbl.find_opt ctx.value_map var with
      | Some v -> v
      | None -> failwith ("Variable not found after body: " ^ var)
    ) !potential_loop_vars in
    
    (* Jump to continue label if block wasn't terminated *)
    (match ctx.current_block with
     | Some _ -> finish_block ctx (Jmp (loop_continue, body_end_values))
     | None -> ());
    
    (* Continue label - update expression *)
    start_block_with_params ctx loop_continue loop_params;
    
    (* Update value map with block parameters from body *)
    List.iter (fun (var, ty) ->
      let param_value = create_simple_value ty in
      Hashtbl.replace ctx.value_map var param_value
    ) loop_params;
    
    (match update_opt with
     | Some update_expr -> 
       let _ = gen_annotated_expr_raw ctx update_expr in ()
     | None -> ());
    
    (* Collect values after update to pass back to header *)
    let continue_end_values = List.map (fun var ->
      match Hashtbl.find_opt ctx.value_map var with
      | Some v -> v
      | None -> failwith ("Variable not found after update: " ^ var)
    ) !potential_loop_vars in
    
    finish_block ctx (Jmp (loop_header, continue_end_values));
    
    (* Continue after loop *)
    start_block ctx loop_end;
    
    (* Restore variable mappings after loop *)
    List.iter (fun var ->
      match List.assoc_opt var loop_params with
      | Some ty ->
        (* This variable was a loop parameter, keep its last value *)
        let param_value = create_simple_value ty in
        Hashtbl.replace ctx.value_map var param_value
      | None -> ()
    ) !potential_loop_vars
  
  | _ -> failwith "Statement not implemented for annotated AST"

(** Generate PIR for annotated block items *)
and gen_annotated_block_item ctx annotated_item =
  let item = get_node annotated_item in
  
  match item with
  | Stmt stmt ->
    let annotated_stmt = annotate stmt empty_c_info in
    gen_annotated_stmt ctx annotated_stmt
    
  | Decl decl ->
    (* Process declaration - create variables in value map *)
    let { storage = _; specs; quals; init_decls } = decl in
    let base_type = resolve_type_specs ctx.symbol_table.type_env specs quals in
    
    List.iter (fun init_decl ->
      let { decl = declarator; init } = init_decl in
      let var_type, var_name = process_declarator ctx.symbol_table.type_env base_type quals declarator in
      
      (* Create PIR value for the variable *)
      (match var_type with
       | Array (_elem_type, size_opt, _quals) ->
         let size = match size_opt with Some s -> s | None -> 1 in
         (* For arrays, allocate space on the stack *)
         let size_val = create_simple_value (Scalar I64) in
         emit_instr ctx ~result:size_val (Const (ConstInt (Int64.of_int size, I64)));
         
         let array_ptr = create_simple_value Ptr in
         (* Alloca needs size and alignment - use default alignment of 8 *)
         emit_instr ctx ~result:array_ptr (Memory (Alloca (size_val, 8)));
         Hashtbl.add ctx.value_map var_name array_ptr;
         Hashtbl.replace ctx.var_types var_name Ptr
       | _ ->
         (match c_type_to_pir_type var_type with
          | Some pir_ty ->
            let var_value = create_simple_value pir_ty in
            Hashtbl.add ctx.value_map var_name var_value;
            Hashtbl.replace ctx.var_types var_name pir_ty;
            
            (* If there's an initializer, generate code for it *)
            (match init with
             | Some (ExprInit init_expr) ->
               let init_val = gen_annotated_expr_raw ctx init_expr in
               (* For now, just update the value map - proper SSA would need alloca/store *)
               Hashtbl.replace ctx.value_map var_name init_val
             | Some (ListInit _) ->
               failwith "List initializers not yet implemented"
             | None -> ())
          | None -> failwith ("Cannot convert variable type to PIR: " ^ string_of_c_type var_type)))
    ) init_decls

(** Extract function name from declarator *)
let rec extract_function_name declarator =
  match declarator with
  | DirectDecl (Ident name) -> name
  | DirectDecl (FuncDecl (inner, _)) -> extract_direct_name inner
  | DirectDecl (ParenDecl inner) -> extract_function_name inner
  | PointerDecl (_, inner) -> extract_function_name inner
  | _ -> failwith "Cannot extract function name"

and extract_direct_name direct_decl =
  match direct_decl with
  | Ident name -> name
  | ParenDecl inner -> extract_function_name inner
  | _ -> failwith "Cannot extract function name from direct declarator"

(** Generate PIR for annotated function definition *)
let gen_annotated_func_def ctx annotated_func_def =
  let func_def = get_node annotated_func_def in
  let info = get_info annotated_func_def in
  
  let { storage = _; specs = _; quals = _; declarator; body; _ } = func_def in
  
  (* Extract function information from annotation *)
  let func_type = match info.c_type with
    | Some ty -> ty
    | None -> failwith "Missing type information for function"
  in
  
  let func_name = extract_function_name declarator in
  
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
  
  (* Add function parameters to value map *)
  List.iter2 (fun param pir_param ->
    match param.param_name with
    | Some name ->
      let (param_pir_name, pir_ty) = pir_param in
      (* Create parameter value - these are special values that represent function parameters *)
      let param_val = create_simple_value pir_ty in
      (* Add to value map using the C parameter name *)
      Hashtbl.add ctx.value_map name param_val;
      Hashtbl.replace ctx.var_types name pir_ty;
      (* Also add using the PIR parameter name for consistency *)
      Hashtbl.add ctx.value_map param_pir_name param_val;
      Hashtbl.replace ctx.var_types param_pir_name pir_ty
    | None -> ()
  ) params pir_params;
  
  (* Generate function body *)
  gen_annotated_stmt ctx (annotate body empty_c_info);
  
  (* Ensure function ends with return if not already terminated *)
  (match ctx.current_block with
   | Some _ ->
     (match return_ty with
      | None -> finish_block ctx (Ret None)
      | Some _ ->
        let default_val = create_simple_value (Scalar I32) in
        emit_instr ctx ~result:default_val (Const (ConstInt (0L, I32)));
        finish_block ctx (Ret (Some default_val)))
   | None -> ());
  
  (* Create PIR function *)
  let pir_func = create_func func_name pir_params pir_return_ty (List.rev ctx.blocks) in
  ctx.current_function <- Some pir_func;
  pir_func

(** Generate PIR for annotated translation unit *)
let gen_annotated_translation_unit symbol_table annotated_translation_unit =
  let ctx = create_annotated_pir_context symbol_table in
  
  let functions = List.filter_map (fun annotated_external_decl ->
    let external_decl = get_node annotated_external_decl in
    match external_decl with
    | FuncDef func_def -> 
      (* Use the annotation from the external declaration *)
      let func_info = get_info annotated_external_decl in
      let annotated_func_def = annotate func_def func_info in
      Some (gen_annotated_func_def ctx annotated_func_def)
    | Decl _decl -> None
  ) annotated_translation_unit in
  
  functions

(** Main PIR generation interface for annotated AST *)
let generate_pir_from_annotated annotated_translation_unit symbol_table =
  try
    let functions = gen_annotated_translation_unit symbol_table annotated_translation_unit in
    Ok functions
  with
  | e -> Error (Printexc.to_string e)