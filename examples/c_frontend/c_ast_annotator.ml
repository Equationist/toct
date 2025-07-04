(** C AST Annotator - Converts raw AST to annotated AST with types and scopes *)

open Ast
open C_type_system
open C_scoped_symbol_table
open C_annotated_ast
open C_annotated_ast.Annotation_utils
open Compilerkit_frontend
open Compilerkit_frontend.Ast_annotation

(** Annotation context *)
type annotation_context = {
  symbol_table: c_symbol_table;
  error_reporter: Error_reporter.t;
  next_scope_id: int ref;
  current_scope_id: int;
}

(** Create annotation context *)
let create_annotation_context () = {
  symbol_table = C_scoped_symbol_table.create ();
  error_reporter = Error_reporter.create ();
  next_scope_id = ref 0;
  current_scope_id = 0;
}

(** Generate unique scope ID *)
let next_scope_id ctx =
  let id = !(ctx.next_scope_id) in
  incr ctx.next_scope_id;
  id

(** Report annotation error *)
let annotation_error ctx msg loc =
  let pos = Position.create loc.Lexer.filename loc.Lexer.line loc.Lexer.column 0 in
  let span = Position.span pos pos in
  Error_reporter.report ctx.error_reporter Error_reporter.Error span msg [];
  failwith ("Annotation error: " ^ msg)

(** Annotate expressions with types *)
let rec annotate_expr ctx expr =
  match expr with
  | IntLit (value, suffix) ->
    let int_type = (* Determine int type from suffix *)
      if String.contains suffix 'L' || String.contains suffix 'l' then
        Int c_long
      else
        Int c_int
    in
    make_constant_expr expr int_type (Int64.to_int value)

  | FloatLit (_, suffix) ->
    let float_type = 
      if String.contains suffix 'f' || String.contains suffix 'F' then
        Float c_float
      else
        Float c_double
    in
    make_constant_expr expr float_type 0

  | CharLit _ ->
    let char_type = Int c_char in
    make_constant_expr expr char_type 0

  | StringLit _ ->
    let char_type = Int c_char in
    let string_type = Array (char_type, None, []) in
    make_constant_expr expr string_type 0

  | Ident name ->
    (match lookup_symbol ctx.symbol_table name with
     | Some symbol ->
       let info = empty_c_info 
         |> (fun info -> with_c_type info symbol.c_type)
         |> (fun info -> with_lvalue info (can_be_lvalue expr))
         |> (fun info -> with_variable_symbol info symbol)
       in
       annotate expr info
     | None ->
       annotation_error ctx ("Undeclared identifier: " ^ name) {Lexer.filename = "<unknown>"; line = 0; column = 0})

  | BinOp (op, left, right) ->
    let annotated_left = annotate_expr ctx left in
    let annotated_right = annotate_expr ctx right in
    
    let left_type = match get_c_type annotated_left with
      | Some ty -> ty
      | None -> annotation_error ctx "Cannot determine left operand type" {Lexer.filename = "<unknown>"; line = 0; column = 0}
    in
    let right_type = match get_c_type annotated_right with
      | Some ty -> ty
      | None -> annotation_error ctx "Cannot determine right operand type" {Lexer.filename = "<unknown>"; line = 0; column = 0}
    in
    
    let result_type = derive_binary_op_type op left_type right_type in
    let is_lvalue = (op = Assign) && is_lvalue annotated_left in
    
    let info = empty_c_info
      |> (fun info -> with_c_type info result_type)
      |> (fun info -> with_lvalue info is_lvalue)
    in
    annotate expr info

  | UnOp (op, operand) ->
    let annotated_operand = annotate_expr ctx operand in
    let operand_type = match get_c_type annotated_operand with
      | Some ty -> ty
      | None -> annotation_error ctx "Cannot determine operand type" {Lexer.filename = "<unknown>"; line = 0; column = 0}
    in
    
    let result_type = match op with
      | Plus | Minus -> operand_type
      | Not -> Bool
      | BitNot -> operand_type
      | PreInc | PreDec | PostInc | PostDec -> operand_type
      | Deref -> 
        (match operand_type with
         | Pointer (target_type, _) -> target_type
         | _ -> annotation_error ctx "Cannot dereference non-pointer" {Lexer.filename = "<unknown>"; line = 0; column = 0})
      | AddrOf -> Pointer (operand_type, [])
    in
    
    let is_lvalue = match op with
      | PreInc | PreDec | Deref -> true
      | _ -> false
    in
    
    let info = empty_c_info
      |> (fun info -> with_c_type info result_type)
      |> (fun info -> with_lvalue info is_lvalue)
    in
    annotate expr info

  | FuncCall (func_expr, args) ->
    let annotated_func = annotate_expr ctx func_expr in
    let _ = List.map (annotate_expr ctx) args in
    
    let return_type = match get_c_type annotated_func with
      | Some (Function (ret_ty, _, _)) -> 
        (match ret_ty with Some ty -> ty | None -> Void)
      | _ -> annotation_error ctx "Cannot call non-function" {Lexer.filename = "<unknown>"; line = 0; column = 0}
    in
    
    make_typed_expr expr return_type

  | _ ->
    (* For other expression types, create with unknown type for now *)
    let info = empty_c_info |> (fun info -> with_c_type info Void) in
    annotate expr info

(** Annotate statements with scope information *)
let rec annotate_stmt ctx stmt =
  match stmt with
  | CompoundStmt block_items ->
    (* Compound statements introduce new block scope *)
    let scope_id = next_scope_id ctx in
    let scope_info = {
      Common.scope_id = scope_id;
      scope_type = `Block;
      parent_scope = Some ctx.current_scope_id;
    } in
    
    (* Enter block scope *)
    let _scope_id = enter_scope ctx.symbol_table BlockScope in
    
    (* Annotate block items *)
    let _ = List.map (annotate_block_item ctx) block_items in
    
    (* Exit block scope *)
    exit_scope ctx.symbol_table;
    
    (* Create annotated statement with scope info *)
    let info = empty_c_info 
      |> (fun info -> with_scope_info info scope_info)
      |> (fun info -> with_symbol_table info ctx.symbol_table)
    in
    annotate stmt info

  | ExprStmt (Some expr) ->
    let _ = annotate_expr ctx expr in
    annotate stmt empty_c_info

  | ExprStmt None ->
    annotate stmt empty_c_info

  | _ ->
    (* For other statement types, create with empty info for now *)
    annotate stmt empty_c_info

(** Annotate block items *)
and annotate_block_item ctx item =
  match item with
  | Stmt stmt ->
    let _ = annotate_stmt ctx stmt in
    annotate item empty_c_info

  | Decl decl ->
    (* Process declaration and add symbols to symbol table *)
    let { storage; specs; quals; init_decls } = decl in
    
    (* Resolve base type *)
    let base_type = resolve_type_specs ctx.symbol_table.type_env specs quals in
    
    (* Process each declarator *)
    List.iter (fun init_decl ->
      let { decl = declarator; init = _ } = init_decl in
      let var_type, var_name = process_declarator ctx.symbol_table.type_env base_type quals declarator in
      
      (* Add symbol to symbol table *)
      match define_symbol ctx.symbol_table var_name var_type storage {Lexer.filename = "<unknown>"; line = 0; column = 0} with
      | Ok _ -> ()
      | Error _ -> annotation_error ctx "Declaration failed" {Lexer.filename = "<unknown>"; line = 0; column = 0}
    ) init_decls;
    
    annotate item empty_c_info

(** Annotate function definitions *)
let annotate_func_def ctx func_def =
  let { storage; specs; quals; declarator; body; _ } = func_def in
  
  (* Resolve function type *)
  let base_type = resolve_type_specs ctx.symbol_table.type_env specs quals in
  let func_type, func_name = process_declarator ctx.symbol_table.type_env base_type quals declarator in
  
  (* Add function to symbol table *)
  (match define_symbol ctx.symbol_table func_name func_type storage {Lexer.filename = "<unknown>"; line = 0; column = 0} with
   | Ok func_symbol ->
     (* Enter function scope *)
     enter_function_scope ctx.symbol_table func_symbol;
     
     (* Add function parameters to the scope *)
     (match func_type with
      | Function (_, params, _) ->
        List.iter (fun param ->
          match param.param_name with
          | Some name ->
            let _ = define_symbol ctx.symbol_table name param.param_type [] {Lexer.filename = "<unknown>"; line = 0; column = 0} in
            ()
          | None -> ()
        ) params
      | _ -> ());
     
     (* Annotate function body *)
     let _ = annotate_stmt ctx body in
     
     (* Exit function scope *)
     exit_function_scope ctx.symbol_table;
     
     (* Create function scope info *)
     let scope_info = {
       Common.scope_id = next_scope_id ctx;
       scope_type = `Function;
       parent_scope = Some 0; (* Parent is global scope *)
     } in
     
     let info = empty_c_info
       |> (fun info -> with_c_type info func_type)
       |> (fun info -> with_scope_info info scope_info)
       |> (fun info -> with_symbol_table info ctx.symbol_table)
     in
     annotate func_def info
     
   | Error _ ->
     annotation_error ctx "Function definition failed" {Lexer.filename = "<unknown>"; line = 0; column = 0})

(** Annotate external declarations *)
let annotate_external_decl ctx external_decl =
  match external_decl with
  | FuncDef func_def ->
    let annotated_func_def = annotate_func_def ctx func_def in
    (* Extract the annotation info from the annotated function *)
    let func_info = get_info annotated_func_def in
    annotate external_decl func_info

  | Decl decl ->
    (* Process global declaration *)
    let _ = annotate_block_item ctx (Ast.Decl decl) in
    annotate external_decl empty_c_info

(** Main annotation interface *)
let annotate_translation_unit translation_unit =
  let ctx = create_annotation_context () in
  let annotated_decls = List.map (annotate_external_decl ctx) translation_unit in
  
  (* Check for errors *)
  if Error_reporter.has_errors ctx.error_reporter then
    let error_msg = "Annotation errors occurred" in
    Error error_msg
  else
    Ok (annotated_decls, ctx.symbol_table)