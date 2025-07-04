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

(** Emit instruction to current block *)
let emit_instr ctx ?result instr =
  match ctx.current_block with
  | Some block ->
    let instruction = create_simple_instruction ?result instr in
    let updated_block = { block with instructions = instruction :: block.instructions } in
    ctx.current_block <- Some updated_block
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
         (match c_type_to_pir_type symbol.c_type with
          | Some pir_ty ->
            let value = create_simple_value pir_ty in
            Hashtbl.add ctx.value_map name value;
            value
          | None -> failwith ("Cannot convert C type to PIR: " ^ string_of_c_type symbol.c_type))
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
          right_val
        | _ -> failwith "Complex assignment not yet implemented")
     
     | _ -> failwith ("Binary operator not implemented"))

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
    (* Only add branch if block wasn't already terminated *)
    (match ctx.current_block with
     | Some _ -> finish_block ctx (Jmp end_label)
     | None -> ());
    
    (* Generate else branch if present *)
    (match else_stmt_opt with
     | Some else_stmt ->
       start_block ctx else_label;
       gen_annotated_stmt ctx (annotate else_stmt empty_c_info);
       (* Only add branch if block wasn't already terminated *)
       (match ctx.current_block with
        | Some _ -> finish_block ctx (Jmp end_label)
        | None -> ())
     | None -> ());
    
    (* Continue with end block *)
    start_block ctx end_label
  
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
      (match c_type_to_pir_type var_type with
       | Some pir_ty ->
         let var_value = create_simple_value pir_ty in
         Hashtbl.add ctx.value_map var_name var_value;
         
         (* If there's an initializer, generate code for it *)
         (match init with
          | Some (ExprInit init_expr) ->
            let init_val = gen_annotated_expr_raw ctx init_expr in
            (* For now, just update the value map - proper SSA would need alloca/store *)
            Hashtbl.replace ctx.value_map var_name init_val
          | Some (ListInit _) ->
            failwith "List initializers not yet implemented"
          | None -> ())
       | None -> failwith ("Cannot convert variable type to PIR: " ^ string_of_c_type var_type))
    ) init_decls

(** Generate PIR for annotated function definition *)
let gen_annotated_func_def ctx annotated_func_def =
  let func_def = get_node annotated_func_def in
  let info = get_info annotated_func_def in
  
  let { storage = _; specs = _; quals = _; declarator = _; body; _ } = func_def in
  
  (* Extract function information from annotation *)
  let func_type = match info.c_type with
    | Some ty -> ty
    | None -> failwith "Missing type information for function"
  in
  
  let func_name = match func_type with
    | Function (_, _, _) ->
      (* Extract function name from declarator - simplified for now *)
      "main"
    | _ -> failwith "Not a function type"
  in
  
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
      let (_, pir_ty) = pir_param in
      let param_val = create_simple_value pir_ty in
      Hashtbl.add ctx.value_map name param_val
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