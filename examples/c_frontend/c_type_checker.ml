(** C89/90 Type Checker with proper scoping and type inference *)

open Ast
open C_type_system
open C_scoped_symbol_table
open Compilerkit_frontend

(** Type checking result *)
type type_check_result = 
  | Ok of c_type
  | Error of string * Lexer.location

(** Type checking context *)
type type_check_context = {
  symbol_table: c_symbol_table;
  error_reporter: Error_reporter.t;
  mutable in_loop: bool;           (* For break/continue validation *)
  mutable in_switch: bool;         (* For case/default validation *)
}

(** Create type checking context *)
let create_context () = {
  symbol_table = C_scoped_symbol_table.create ();
  error_reporter = Error_reporter.create ();
  in_loop = false;
  in_switch = false;
}

(** Report type error *)
let type_error ctx msg loc =
  let pos = Position.create loc.Lexer.filename loc.Lexer.line loc.Lexer.column 0 in
  let span = Position.span pos pos in
  Error_reporter.report ctx.error_reporter Error_reporter.Error span msg [];
  Error (msg, loc)

(** Type check binary operator *)
let rec check_binop ctx op left_ty right_ty loc =
  match op with
  (* Arithmetic operators require arithmetic types *)
  | Add | Sub | Mul | Div | Mod ->
    if is_arithmetic left_ty && is_arithmetic right_ty then
      let promoted_left, _promoted_right = usual_arithmetic_conversions left_ty right_ty in
      Ok promoted_left
    else
      type_error ctx "Arithmetic operator requires arithmetic operands" loc
  
  (* Relational operators *)
  | Lt | Gt | Le | Ge ->
    if (is_arithmetic left_ty && is_arithmetic right_ty) ||
       (match left_ty, right_ty with
        | Pointer _, Pointer _ -> types_compatible ctx.symbol_table.type_env left_ty right_ty
        | _ -> false) then
      Ok Bool
    else
      type_error ctx "Relational operator requires compatible types" loc
  
  (* Equality operators *)
  | Eq | Ne ->
    if (is_arithmetic left_ty && is_arithmetic right_ty) ||
       (is_scalar left_ty && is_scalar right_ty) then
      Ok Bool
    else
      type_error ctx "Equality operator requires scalar types" loc
  
  (* Bitwise operators require integral types *)
  | BitAnd | BitOr | BitXor | Shl | Shr ->
    if is_integral left_ty && is_integral right_ty then
      let promoted_left, _promoted_right = usual_arithmetic_conversions left_ty right_ty in
      Ok promoted_left
    else
      type_error ctx "Bitwise operator requires integral operands" loc
  
  (* Logical operators *)
  | LogAnd | LogOr ->
    if is_scalar left_ty && is_scalar right_ty then
      Ok Bool
    else
      type_error ctx "Logical operator requires scalar operands" loc
  
  (* Assignment operators *)
  | Assign ->
    if conversion_allowed ctx.symbol_table.type_env right_ty left_ty then
      Ok left_ty
    else
      type_error ctx "Invalid assignment conversion" loc
  
  | AddAssign | SubAssign | MulAssign | DivAssign | ModAssign
  | AndAssign | OrAssign | XorAssign | ShlAssign | ShrAssign ->
    (* Compound assignment: check if operation is valid, then check assignment *)
    let base_op = match op with
      | AddAssign -> Add | SubAssign -> Sub | MulAssign -> Mul
      | DivAssign -> Div | ModAssign -> Mod | AndAssign -> BitAnd
      | OrAssign -> BitOr | XorAssign -> BitXor | ShlAssign -> Shl
      | ShrAssign -> Shr | _ -> failwith "Invalid compound assignment"
    in
    (match check_binop ctx base_op left_ty right_ty loc with
     | Ok result_ty ->
       if conversion_allowed ctx.symbol_table.type_env result_ty left_ty then
         Ok left_ty
       else
         type_error ctx "Invalid compound assignment conversion" loc
     | Error _ as e -> e)

(** Type check unary operator *)
let check_unop ctx op operand_ty loc =
  match op with
  | Plus | Minus ->
    if is_arithmetic operand_ty then
      Ok (integer_promote operand_ty)
    else
      type_error ctx "Unary +/- requires arithmetic operand" loc
  
  | Not ->
    if is_scalar operand_ty then
      Ok Bool
    else
      type_error ctx "Logical NOT requires scalar operand" loc
  
  | BitNot ->
    if is_integral operand_ty then
      Ok (integer_promote operand_ty)
    else
      type_error ctx "Bitwise NOT requires integral operand" loc
  
  | PreInc | PreDec | PostInc | PostDec ->
    if is_scalar operand_ty then
      Ok operand_ty
    else
      type_error ctx "Increment/decrement requires scalar operand" loc
  
  | Deref ->
    (match operand_ty with
     | Pointer (pointee_ty, _) -> Ok pointee_ty
     | Array (elem_ty, _, _) -> Ok elem_ty
     | _ -> type_error ctx "Dereference requires pointer operand" loc)
  
  | AddrOf ->
    (* Address-of always produces a pointer *)
    Ok (Pointer (operand_ty, []))

(** Type check expressions *)
let rec check_expr ctx = function
  | IntLit (value, suffix) ->
    (* Determine type based on value and suffix *)
    let ty = match suffix with
      | "" -> if Int64.abs value <= Int64.of_int32 Int32.max_int then Int c_int else Int c_long
      | "L" | "l" -> Int c_long  
      | "U" | "u" -> Int c_uint
      | "UL" | "ul" | "Ul" | "uL" -> Int c_ulong
      | _ -> Int c_int
    in
    Ok ty
  
  | FloatLit (_, suffix) ->
    let ty = match suffix with
      | "f" | "F" -> Float c_float
      | _ -> Float c_double
    in
    Ok ty
  
  | CharLit _ -> Ok (Int c_char)
  | StringLit s -> Ok (Array (Int c_char, Some (String.length s + 1), []))
  
  | Ident name ->
    (match lookup_symbol ctx.symbol_table name with
     | Some sym ->
       mark_used ctx.symbol_table name;
       Ok sym.c_type
     | None ->
       type_error ctx ("Undeclared identifier: " ^ name) {Lexer.filename = "<unknown>"; line = 0; column = 0})
  
  | BinOp (op, left, right) ->
    (match check_expr ctx left, check_expr ctx right with
     | Ok left_ty, Ok right_ty -> check_binop ctx op left_ty right_ty ({Lexer.filename = "<unknown>"; line = 0; column = 0})
     | Error _ as e, _ | _, (Error _ as e) -> e)
  
  | UnOp (op, operand) ->
    (match check_expr ctx operand with
     | Ok operand_ty -> check_unop ctx op operand_ty ({Lexer.filename = "<unknown>"; line = 0; column = 0})
     | Error _ as e -> e)
  
  | TernOp (cond, true_expr, false_expr) ->
    (match check_expr ctx cond, check_expr ctx true_expr, check_expr ctx false_expr with
     | Ok cond_ty, Ok true_ty, Ok false_ty ->
       if not (is_scalar cond_ty) then
         type_error ctx "Ternary condition must be scalar" ({Lexer.filename = "<unknown>"; line = 0; column = 0})
       else if types_compatible ctx.symbol_table.type_env true_ty false_ty then
         Ok true_ty
       else
         type_error ctx "Ternary branches must have compatible types" ({Lexer.filename = "<unknown>"; line = 0; column = 0})
     | Error _ as e, _, _ | _, (Error _ as e), _ | _, _, (Error _ as e) -> e)
  
  | FuncCall (func_expr, arg_exprs) ->
    (match check_expr ctx func_expr with
     | Ok (Function (ret_ty, _param_tys, _varargs)) ->
       (* Check argument count and types *)
       let arg_results = List.map (check_expr ctx) arg_exprs in
       if List.exists (function Error _ -> true | _ -> false) arg_results then
         List.find (function Error _ -> true | _ -> false) arg_results
       else
         let _arg_tys = List.map (function Ok ty -> ty | _ -> assert false) arg_results in
         (* TODO: Check parameter compatibility *)
         (match ret_ty with Some ty -> Ok ty | None -> Ok Void)
     | Ok _ ->
       type_error ctx "Function call on non-function type" ({Lexer.filename = "<unknown>"; line = 0; column = 0})
     | Error _ as e -> e)
  
  | ArrayRef (array_expr, index_expr) ->
    (match check_expr ctx array_expr, check_expr ctx index_expr with
     | Ok array_ty, Ok index_ty ->
       if not (is_integral index_ty) then
         type_error ctx "Array index must be integral" ({Lexer.filename = "<unknown>"; line = 0; column = 0})
       else
         (match array_ty with
          | Array (elem_ty, _, _) -> Ok elem_ty
          | Pointer (pointee_ty, _) -> Ok pointee_ty
          | _ -> type_error ctx "Array subscript on non-array type" ({Lexer.filename = "<unknown>"; line = 0; column = 0}))
     | Error _ as e, _ | _, (Error _ as e) -> e)
  
  | Member (struct_expr, field_name) ->
    (match check_expr ctx struct_expr with
     | Ok (Struct (_, Some fields)) ->
       (match List.find_opt (fun (name, _, _) -> name = field_name) fields with
        | Some (_, field_ty, _) -> Ok field_ty
        | None -> type_error ctx ("No such field: " ^ field_name) ({Lexer.filename = "<unknown>"; line = 0; column = 0}))
     | Ok (Union (_, Some fields)) ->
       (match List.find_opt (fun (name, _, _) -> name = field_name) fields with
        | Some (_, field_ty, _) -> Ok field_ty
        | None -> type_error ctx ("No such field: " ^ field_name) ({Lexer.filename = "<unknown>"; line = 0; column = 0}))
     | Ok _ ->
       type_error ctx "Member access on non-struct/union type" ({Lexer.filename = "<unknown>"; line = 0; column = 0})
     | Error _ as e -> e)
  
  | PtrMember (ptr_expr, field_name) ->
    (match check_expr ctx ptr_expr with
     | Ok (Pointer (Struct (_, Some fields), _)) ->
       (match List.find_opt (fun (name, _, _) -> name = field_name) fields with
        | Some (_, field_ty, _) -> Ok field_ty
        | None -> type_error ctx ("No such field: " ^ field_name) ({Lexer.filename = "<unknown>"; line = 0; column = 0}))
     | Ok (Pointer (Union (_, Some fields), _)) ->
       (match List.find_opt (fun (name, _, _) -> name = field_name) fields with
        | Some (_, field_ty, _) -> Ok field_ty
        | None -> type_error ctx ("No such field: " ^ field_name) ({Lexer.filename = "<unknown>"; line = 0; column = 0}))
     | Ok _ ->
       type_error ctx "Pointer member access on non-pointer-to-struct/union" ({Lexer.filename = "<unknown>"; line = 0; column = 0})
     | Error _ as e -> e)
  
  | SizeofExpr expr ->
    (match check_expr ctx expr with
     | Ok ty ->
       if is_complete ctx.symbol_table.type_env ty then
         Ok (Int c_long)  (* sizeof returns size_t, which we model as long *)
       else
         type_error ctx "sizeof on incomplete type" ({Lexer.filename = "<unknown>"; line = 0; column = 0})
     | Error _ as e -> e)
  
  | SizeofType _ctype ->
    (* TODO: Convert ctype to c_type and check completeness *)
    Ok (Int c_long)
  
  | Cast (_target_ctype, expr) ->
    (* TODO: Convert ctype to c_type and check cast validity *)
    (match check_expr ctx expr with
     | Ok _ -> Ok (Int c_int)  (* Simplified - should resolve target_ctype *)
     | Error _ as e -> e)
  
  | Comma exprs ->
    (* Comma operator evaluates all expressions, returns type of last *)
    let results = List.map (check_expr ctx) exprs in
    (match List.rev results with
     | [] -> Ok Void
     | last_result :: _ -> last_result)

(** Type check statements *)
let rec check_stmt ctx = function
  | ExprStmt (Some expr) ->
    (match check_expr ctx expr with
     | Ok _ -> ((Ok () : (unit, unit) result) : (unit, unit) result)
     | Error _ -> ((Error () : (unit, unit) result) : (unit, unit) result))
  | ExprStmt None -> (Ok () : (unit, unit) result)
  
  | CompoundStmt block_items ->
    with_scope ctx.symbol_table BlockScope (fun () ->
      check_block_items ctx block_items)
  
  | IfStmt (cond, then_stmt, else_stmt) ->
    (match check_expr ctx cond with
     | Ok cond_ty ->
       if is_scalar cond_ty then
         let then_result = check_stmt ctx then_stmt in
         let else_result = match else_stmt with
           | Some stmt -> check_stmt ctx stmt
           | None -> (Ok () : (unit, unit) result)
         in
         (match then_result, else_result with
          | (Ok () : (unit, unit) result), (Ok () : (unit, unit) result) -> (Ok () : (unit, unit) result)
          | _ -> (Error () : (unit, unit) result))
       else
         (ignore (type_error ctx "If condition must be scalar" ({Lexer.filename = "<unknown>"; line = 0; column = 0})); (Error () : (unit, unit) result))
     | Error _ -> (Error () : (unit, unit) result))
  
  | WhileStmt (cond, body) ->
    (match check_expr ctx cond with
     | Ok cond_ty ->
       if is_scalar cond_ty then
         let old_in_loop = ctx.in_loop in
         ctx.in_loop <- true;
         let result = check_stmt ctx body in
         ctx.in_loop <- old_in_loop;
         result
       else
         (ignore (type_error ctx "While condition must be scalar" ({Lexer.filename = "<unknown>"; line = 0; column = 0})); (Error () : (unit, unit) result))
     | Error _ -> (Error () : (unit, unit) result))
  
  | DoWhileStmt (body, cond) ->
    let old_in_loop = ctx.in_loop in
    ctx.in_loop <- true;
    let body_result = check_stmt ctx body in
    ctx.in_loop <- old_in_loop;
    (match body_result, check_expr ctx cond with
     | (Ok () : (unit, unit) result), Ok cond_ty ->
       if is_scalar cond_ty then (Ok () : (unit, unit) result)
       else (ignore (type_error ctx "Do-while condition must be scalar" ({Lexer.filename = "<unknown>"; line = 0; column = 0})); (Error () : (unit, unit) result))
     | _ -> (Error () : (unit, unit) result))
  
  | ForStmt (init, cond, update, body) ->
    with_scope ctx.symbol_table BlockScope (fun () ->
      (* Check init expression *)
      let init_result = match init with
        | Some expr -> (match check_expr ctx expr with Ok _ -> ((Ok () : (unit, unit) result) : (unit, unit) result) | Error _ -> ((Error () : (unit, unit) result) : (unit, unit) result))
        | None -> (Ok () : (unit, unit) result)
      in
      (* Check condition *)
      let cond_result = match cond with
        | Some expr ->
          (match check_expr ctx expr with
           | Ok cond_ty ->
             if is_scalar cond_ty then ((Ok () : (unit, unit) result) : (unit, unit) result)
             else (ignore (type_error ctx "For condition must be scalar" ({Lexer.filename = "<unknown>"; line = 0; column = 0})); ((Error () : (unit, unit) result) : (unit, unit) result))
           | Error _ -> ((Error () : (unit, unit) result) : (unit, unit) result))
        | None -> (Ok () : (unit, unit) result)
      in
      (* Check update expression *)
      let update_result = match update with
        | Some expr -> (match check_expr ctx expr with Ok _ -> (Ok () : (unit, unit) result) | Error _ -> (Error () : (unit, unit) result))
        | None -> (Ok () : (unit, unit) result)
      in
      (* Check body *)
      let old_in_loop = ctx.in_loop in
      ctx.in_loop <- true;
      let body_result = check_stmt ctx body in
      ctx.in_loop <- old_in_loop;
      
      match init_result, cond_result, update_result, body_result with
      | (Ok () : (unit, unit) result), (Ok () : (unit, unit) result), (Ok () : (unit, unit) result), (Ok () : (unit, unit) result) -> (Ok () : (unit, unit) result)
      | _ -> (Error () : (unit, unit) result))
  
  | BreakStmt ->
    if ctx.in_loop || ctx.in_switch then (Ok () : (unit, unit) result)
    else (ignore (type_error ctx "Break statement outside loop or switch" ({Lexer.filename = "<unknown>"; line = 0; column = 0})); (Error () : (unit, unit) result))
  
  | ContinueStmt ->
    if ctx.in_loop then (Ok () : (unit, unit) result)
    else (ignore (type_error ctx "Continue statement outside loop" ({Lexer.filename = "<unknown>"; line = 0; column = 0})); (Error () : (unit, unit) result))
  
  | ReturnStmt expr_opt ->
    let expected_return_ty = current_function_return_type ctx.symbol_table in
    (match expr_opt, expected_return_ty with
     | None, None -> (Ok () : (unit, unit) result)  (* void return *)
     | None, Some _ -> (ignore (type_error ctx "Return value required" ({Lexer.filename = "<unknown>"; line = 0; column = 0})); (Error () : (unit, unit) result))
     | Some _, None -> (ignore (type_error ctx "Return value in void function" ({Lexer.filename = "<unknown>"; line = 0; column = 0})); (Error () : (unit, unit) result))
     | Some expr, Some expected_ty ->
       (match check_expr ctx expr with
        | Ok expr_ty ->
          if conversion_allowed ctx.symbol_table.type_env expr_ty expected_ty then (Ok () : (unit, unit) result)
          else (ignore (type_error ctx "Invalid return type" ({Lexer.filename = "<unknown>"; line = 0; column = 0})); (Error () : (unit, unit) result))
        | Error _ -> (Error () : (unit, unit) result)))
  
  | _ -> (Ok () : (unit, unit) result)  (* TODO: Implement remaining statement types *)

and check_block_items ctx = function
  | [] -> (Ok () : (unit, unit) result)
  | item :: rest ->
    let item_result = match item with
      | Decl decl -> check_declaration ctx decl
      | Stmt stmt -> (match check_stmt ctx stmt with (Ok () : (unit, unit) result) -> (Ok () : (unit, unit) result) | (Error () : (unit, unit) result) -> (Error () : (unit, unit) result))
    in
    (match item_result with
     | (Ok () : (unit, unit) result) -> check_block_items ctx rest
     | (Error () : (unit, unit) result) -> (Error () : (unit, unit) result))

and check_declaration ctx decl =
  let { storage; specs; quals; init_decls } = decl in
  
  (* Resolve base type from type specifiers *)
  let base_type = resolve_type_specs ctx.symbol_table.type_env specs quals in
  
  (* Process each declarator *)
  let process_init_declarator init_decl =
    let { decl = declarator; init } = init_decl in
    
    (* Process declarator to get full type and name *)
    let var_type, var_name = process_declarator ctx.symbol_table.type_env base_type quals declarator in
    
    (* Check initializer if present *)
    let init_result = match init with
      | Some _init_expr ->
        (* TODO: Check initializer type compatibility *)
        (Ok () : (unit, unit) result)
      | None -> (Ok () : (unit, unit) result)
    in
    
    (* Add symbol to symbol table *)
    (match init_result with
     | (Ok () : (unit, unit) result) ->
       (match define_symbol ctx.symbol_table var_name var_type storage ({Lexer.filename = "<unknown>"; line = 0; column = 0}) with
        | Ok _ -> (Ok () : (unit, unit) result)
        | Error _ -> (Error () : (unit, unit) result))
     | (Error () : (unit, unit) result) -> (Error () : (unit, unit) result))
  in
  
  (* Process all declarators *)
  let results = List.map process_init_declarator init_decls in
  if List.for_all (function (Ok () : (unit, unit) result) -> true | _ -> false) results then
    (Ok () : (unit, unit) result)
  else
    (Error () : (unit, unit) result)

(** Check function definition *)
let check_function_def ctx func_def =
  let { storage; specs; quals; declarator; body; _ } = func_def in
  
  (* Resolve function type *)
  let base_type = resolve_type_specs ctx.symbol_table.type_env specs quals in
  let func_type, func_name = process_declarator ctx.symbol_table.type_env base_type quals declarator in
  
  (* Add function to symbol table *)
  (match define_symbol ctx.symbol_table func_name func_type storage ({Lexer.filename = "<unknown>"; line = 0; column = 0}) with
   | Ok func_symbol ->
     (* Enter function scope and check body *)
     enter_function_scope ctx.symbol_table func_symbol;
     
     (* Add function parameters to the scope *)
     (match func_type with
      | Function (_, params, _) ->
        List.iter (fun param ->
          match param.param_name with
          | Some name ->
            let _ = define_symbol ctx.symbol_table name param.param_type [] ({Lexer.filename = "<unknown>"; line = 0; column = 0}) in
            ()
          | None -> ()
        ) params
      | _ -> ());
     
     let body_result = check_stmt ctx body in
     exit_function_scope ctx.symbol_table;
     (match body_result with (Ok () : (unit, unit) result) -> (Ok () : (unit, unit) result) | (Error () : (unit, unit) result) -> (Error () : (unit, unit) result))
   | Error _ -> (Error () : (unit, unit) result))

(** Check translation unit *)
let check_translation_unit ctx translation_unit =
  let check_external_decl = function
    | FuncDef func_def -> check_function_def ctx func_def
    | Decl decl -> check_declaration ctx decl
  in
  
  let results = List.map check_external_decl translation_unit in
  if List.for_all (function (Ok () : (unit, unit) result) -> true | _ -> false) results then
    (Ok () : (unit, unit) result)
  else
    (Error () : (unit, unit) result)

(** Main type checking interface *)
let type_check translation_unit =
  let ctx = create_context () in
  let _result = check_translation_unit ctx translation_unit in
  
  (* Return results *)
  let has_errors = Error_reporter.has_errors ctx.error_reporter in
  if has_errors then
    Result.Error (Error_reporter.format_diagnostics ctx.error_reporter (Hashtbl.create 0))
  else
    Result.Ok ctx.symbol_table