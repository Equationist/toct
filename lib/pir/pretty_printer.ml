(* PIR Pretty Printer with syntax highlighting support *)

open Types
open Values
open Instructions
open Module_ir

(* Color codes for terminal output *)
type color = 
  | Reset | Red | Green | Yellow | Blue | Magenta | Cyan | White
  | BrightRed | BrightGreen | BrightYellow | BrightBlue

let color_code = function
  | Reset -> "\027[0m"
  | Red -> "\027[31m"
  | Green -> "\027[32m"
  | Yellow -> "\027[33m"
  | Blue -> "\027[34m"
  | Magenta -> "\027[35m"
  | Cyan -> "\027[36m"
  | White -> "\027[37m"
  | BrightRed -> "\027[91m"
  | BrightGreen -> "\027[92m"
  | BrightYellow -> "\027[93m"
  | BrightBlue -> "\027[94m"

let colorize color text =
  Printf.sprintf "%s%s%s" (color_code color) text (color_code Reset)

(* Configuration *)
type config = {
  use_colors: bool;
  indent_size: int;
  show_types: bool;
  show_attributes: bool;
}

let default_config = {
  use_colors = true;
  indent_size = 2;
  show_types = true;
  show_attributes = true;
}

(* Pretty printing context *)
type context = {
  config: config;
  indent_level: int;
}

let make_context ?(config=default_config) () = {
  config;
  indent_level = 0;
}

let indent ctx = { ctx with indent_level = ctx.indent_level + 1 }

let get_indent ctx =
  String.make (ctx.indent_level * ctx.config.indent_size) ' '

(* Color helpers *)
let keyword ctx s = 
  if ctx.config.use_colors then colorize Blue s else s

let type_color ctx s = 
  if ctx.config.use_colors then colorize Green s else s

let value_color ctx s = 
  if ctx.config.use_colors then colorize Yellow s else s

let label_color ctx s = 
  if ctx.config.use_colors then colorize Magenta s else s

let comment_color ctx s = 
  if ctx.config.use_colors then colorize BrightBlue s else s

let attr_color ctx s = 
  if ctx.config.use_colors then colorize Cyan s else s

(* Type pretty printing *)
let rec pp_type ctx = function
  | Scalar s -> type_color ctx (string_of_scalar s)
  | Vector (n, s) -> 
    type_color ctx (Printf.sprintf "v%dx%s" n (string_of_scalar s))
  | Ptr -> type_color ctx "ptr"
  | Struct fields ->
    let field_strs = List.map (pp_type ctx) fields in
    Printf.sprintf "%s<<%s>>" 
      (type_color ctx "struct") (String.concat ", " field_strs)
  | Array (n, ty) ->
    Printf.sprintf "%s[%d]%s" 
      (type_color ctx "array") n (pp_type ctx ty)
  | PackedStruct fields ->
    let field_strs = List.map (pp_type ctx) fields in
    Printf.sprintf "%s<<%s>>" 
      (type_color ctx "packed_struct") (String.concat ", " field_strs)

(* Value pretty printing *)
let pp_value ctx v =
  value_color ctx (string_of_value v)

(* Constant value pretty printing *)
let pp_const_value ctx = function
  | ConstInt (i, _ty) -> value_color ctx (Int64.to_string i)
  | ConstFloat (f, _ty) -> value_color ctx (string_of_float f)
  | ConstBool b -> value_color ctx (if b then "true" else "false")
  | ConstNull -> value_color ctx "null"
  | ConstUndef _ty -> value_color ctx "undef"
  | ConstZero _ty -> value_color ctx "0"
  | ConstArray _ -> value_color ctx "[...]"
  | ConstStruct _ -> value_color ctx "{...}"

(* Instruction pretty printing *)
let pp_icmp_pred = function
  | Eq -> "eq" | Ne -> "ne" | Slt -> "slt" | Sle -> "sle"
  | Sgt -> "sgt" | Sge -> "sge" | Ult -> "ult" | Ule -> "ule"
  | Ugt -> "ugt" | Uge -> "uge"

let pp_fcmp_pred = function
  | Oeq -> "oeq" | Ogt -> "ogt" | Oge -> "oge" | Olt -> "olt" | Ole -> "ole"
  | One -> "one" | Ord -> "ord" | Ueq -> "ueq" | Ugt -> "ugt" | Uge -> "uge"
  | Ult -> "ult" | Ule -> "ule" | Une -> "une" | Uno -> "uno"

let pp_binop_name = function
  | Add -> "add" | Sub -> "sub" | Mul -> "mul"
  | Sdiv -> "sdiv" | Udiv -> "udiv" | Srem -> "srem" | Urem -> "urem"
  | And -> "and" | Or -> "or" | Xor -> "xor"
  | Shl -> "shl" | Lshr -> "lshr" | Ashr -> "ashr"
  | Rol -> "rol" | Ror -> "ror"
  | Fadd -> "fadd" | Fsub -> "fsub" | Fmul -> "fmul"
  | Fdiv -> "fdiv" | Frem -> "frem" | Fma -> "fma"
  | Clz -> "clz" | Ctz -> "ctz" | Popcnt -> "popcnt"

let pp_flag = function
  | NoFlag -> ""
  | Nsw -> ".nsw"
  | Carry -> ".carry"
  | Sat -> ".sat"

let pp_instr ctx = function
  | Binop (op, flag, v1, v2) ->
    let ty = get_type v1 in
    let no_color_ctx = { ctx with config = { ctx.config with use_colors = false } } in
    Printf.sprintf "%s%s.%s %s, %s"
      (keyword ctx (pp_binop_name op))
      (pp_flag flag)
      (pp_type no_color_ctx ty)
      (pp_value ctx v1)
      (pp_value ctx v2)
  
  | Icmp (pred, v1, v2) ->
    let ty = get_type v1 in
    let no_color_ctx = { ctx with config = { ctx.config with use_colors = false } } in
    Printf.sprintf "%s.%s.%s %s, %s"
      (keyword ctx "icmp")
      (pp_icmp_pred pred)
      (pp_type no_color_ctx ty)
      (pp_value ctx v1)
      (pp_value ctx v2)
  
  | Fcmp (pred, v1, v2) ->
    let ty = get_type v1 in
    let no_color_ctx = { ctx with config = { ctx.config with use_colors = false } } in
    Printf.sprintf "%s.%s.%s %s, %s"
      (keyword ctx "fcmp")
      (pp_fcmp_pred pred)
      (pp_type no_color_ctx ty)
      (pp_value ctx v1)
      (pp_value ctx v2)
  
  | Select (cond, v_true, v_false) ->
    Printf.sprintf "%s %s, %s, %s"
      (keyword ctx "select")
      (pp_value ctx cond)
      (pp_value ctx v_true)
      (pp_value ctx v_false)
  
  | Memory (Load ty) ->
    let no_color_ctx = { ctx with config = { ctx.config with use_colors = false } } in
    Printf.sprintf "%s.%s [...]"
      (keyword ctx "load")
      (pp_type no_color_ctx ty)
  
  | Memory (Store (val_, ptr)) ->
    let ty = get_type val_ in
    let no_color_ctx = { ctx with config = { ctx.config with use_colors = false } } in
    Printf.sprintf "%s.%s %s, [%s]"
      (keyword ctx "store")
      (pp_type no_color_ctx ty)
      (pp_value ctx val_)
      (pp_value ctx ptr)
  
  | Memory (Alloca (size, align)) ->
    Printf.sprintf "%s %s align %d"
      (keyword ctx "alloca")
      (pp_value ctx size)
      align
  
  | Memory (Memcpy (dst, src, bytes)) ->
    Printf.sprintf "%s %s, %s, %s"
      (keyword ctx "memcpy")
      (pp_value ctx dst)
      (pp_value ctx src)
      (pp_value ctx bytes)
  
  | Memory (Memset (dst, byte, bytes)) ->
    Printf.sprintf "%s %s, %s, %s"
      (keyword ctx "memset")
      (pp_value ctx dst)
      (pp_value ctx byte)
      (pp_value ctx bytes)
  
  | Address (Gep (base, idx)) ->
    Printf.sprintf "%s %s, %s"
      (keyword ctx "gep")
      (pp_value ctx base)
      (pp_value ctx idx)
  
  | Address (FieldAddr (base, idx)) ->
    Printf.sprintf "%s %s, %d"
      (keyword ctx "fieldaddr")
      (pp_value ctx base)
      idx
  
  | Address (PtrAdd (base, offset)) ->
    Printf.sprintf "%s %s, %s"
      (keyword ctx "ptradd")
      (pp_value ctx base)
      (pp_value ctx offset)
  
  | Cast cast_op ->
    (match cast_op with
     | Bitcast v -> Printf.sprintf "%s %s" (keyword ctx "bitcast") (pp_value ctx v)
     | Trunc (v, ty) -> Printf.sprintf "%s %s to %s" (keyword ctx "trunc") (pp_value ctx v) (pp_type ctx ty)
     | Zext (v, ty) -> Printf.sprintf "%s %s to %s" (keyword ctx "zext") (pp_value ctx v) (pp_type ctx ty)
     | Sext (v, ty) -> Printf.sprintf "%s %s to %s" (keyword ctx "sext") (pp_value ctx v) (pp_type ctx ty)
     | Fptrunc (v, ty) -> Printf.sprintf "%s %s to %s" (keyword ctx "fptrunc") (pp_value ctx v) (pp_type ctx ty)
     | Fpext (v, ty) -> Printf.sprintf "%s %s to %s" (keyword ctx "fpext") (pp_value ctx v) (pp_type ctx ty)
     | Fptoui (v, ty) -> Printf.sprintf "%s %s to %s" (keyword ctx "fptoui") (pp_value ctx v) (pp_type ctx ty)
     | Fptosi (v, ty) -> Printf.sprintf "%s %s to %s" (keyword ctx "fptosi") (pp_value ctx v) (pp_type ctx ty)
     | Uitofp (v, ty) -> Printf.sprintf "%s %s to %s" (keyword ctx "uitofp") (pp_value ctx v) (pp_type ctx ty)
     | Sitofp (v, ty) -> Printf.sprintf "%s %s to %s" (keyword ctx "sitofp") (pp_value ctx v) (pp_type ctx ty))
  
  | Vector vec_op ->
    (match vec_op with
     | Splat (v, lanes) -> Printf.sprintf "%s %s, %d" (keyword ctx "splat") (pp_value ctx v) lanes
     | Shuffle (v1, v2, mask) ->
       let mask_str = String.concat ", " (List.map string_of_int mask) in
       Printf.sprintf "%s %s, %s, [%s]" (keyword ctx "shuffle") (pp_value ctx v1) (pp_value ctx v2) mask_str
     | ExtractLane (v, idx) -> Printf.sprintf "%s %s, %d" (keyword ctx "extractlane") (pp_value ctx v) idx
     | InsertLane (v, idx, scalar) -> Printf.sprintf "%s %s, %d, %s" (keyword ctx "insertlane") (pp_value ctx v) idx (pp_value ctx scalar))
  
  | Call (Call (callee, args)) ->
    let args_str = String.concat ", " (List.map (pp_value ctx) args) in
    (* TODO: Proper return type handling *)
    Printf.sprintf "%s.void %s, %s" (keyword ctx "call") (pp_value ctx callee) args_str
  
  | Call (TailCall (callee, args)) ->
    let args_str = String.concat ", " (List.map (pp_value ctx) args) in
    (* TODO: Proper return type handling *)
    Printf.sprintf "%s.void %s, %s" (keyword ctx "tailcall") (pp_value ctx callee) args_str
  
  | Phi phi_list ->
    let phi_entries = List.map (fun (v, label) ->
      Printf.sprintf "[%s, %s]" (pp_value ctx v) (label_color ctx label)
    ) phi_list in
    Printf.sprintf "%s %s" (keyword ctx "phi") (String.concat ", " phi_entries)
  
  | Const c -> pp_const_value ctx c

(* Terminator pretty printing *)
let pp_terminator ctx = function
  | Ret None -> keyword ctx "ret"
  | Ret (Some v) -> Printf.sprintf "%s %s" (keyword ctx "ret") (pp_value ctx v)
  | Br (cond, then_lbl, else_lbl) ->
    Printf.sprintf "%s %s, %s, %s"
      (keyword ctx "br")
      (pp_value ctx cond)
      (label_color ctx then_lbl)
      (label_color ctx else_lbl)
  | Jmp lbl -> Printf.sprintf "%s %s" (keyword ctx "jmp") (label_color ctx lbl)
  | Switch (v, default, cases) ->
    let case_strs = List.map (fun (const_val, label) ->
      Printf.sprintf "%s: %s" (pp_const_value ctx const_val) (label_color ctx label)
    ) cases in
    Printf.sprintf "%s %s, %s [%s]"
      (keyword ctx "switch")
      (pp_value ctx v)
      (label_color ctx default)
      (String.concat ", " case_strs)
  | Unreachable -> keyword ctx "unreachable"

(* Instruction with result pretty printing *)
let pp_instruction ctx (instr : Instructions.instruction) =
  let indent_str = get_indent ctx in
  let attrs_str = 
    if ctx.config.show_attributes && not (Attributes.is_empty instr.attrs) then
      " " ^ attr_color ctx (Attributes.string_of_attrs instr.attrs)
    else "" in
  
  match instr.result with
  | Some res ->
    Printf.sprintf "%s%s = %s%s"
      indent_str
      (pp_value ctx res)
      (pp_instr ctx instr.instr)
      attrs_str
  | None ->
    Printf.sprintf "%s%s%s"
      indent_str
      (pp_instr ctx instr.instr)
      attrs_str

(* Basic block pretty printing *)
let pp_basic_block ctx (block : Instructions.basic_block) =
  let header = 
    if block.params = [] then
      Printf.sprintf "%s:" (label_color ctx block.label)
    else
      let params_str = List.map (fun (name, ty) ->
        Printf.sprintf "%s:%s" (value_color ctx name) (pp_type ctx ty)
      ) block.params |> String.concat ", " in
      Printf.sprintf "%s(%s):" (label_color ctx block.label) params_str in
  
  let attrs_str =
    if ctx.config.show_attributes && not (Attributes.is_empty block.attrs) then
      " " ^ attr_color ctx (Attributes.string_of_attrs block.attrs)
    else "" in
  
  let header_line = header ^ attrs_str in
  let ctx' = indent ctx in
  let instr_lines = List.map (pp_instruction ctx') block.instructions in
  let term_line = get_indent ctx' ^ pp_terminator ctx' block.terminator in
  
  String.concat "\n" (header_line :: instr_lines @ [term_line])

(* Function pretty printing *)
let pp_function ctx (func : Instructions.func) =
  let params_str = List.map (fun (name, ty) ->
    Printf.sprintf "%s:%s" (value_color ctx name) (pp_type ctx ty)
  ) func.params |> String.concat ", " in
  
  let return_str = match func.return_ty with
    | None -> "void"
    | Some ty -> pp_type ctx ty in
  
  let attrs_str =
    if ctx.config.show_attributes && not (Attributes.is_empty func.attrs) then
      " " ^ attr_color ctx (Attributes.string_of_attrs func.attrs)
    else "" in
  
  let header = Printf.sprintf "%s %s(%s) -> %s%s"
    (keyword ctx "func")
    (value_color ctx func.name)
    params_str
    return_str
    attrs_str in
  
  let block_strs = List.map (pp_basic_block ctx) func.blocks in
  let body = String.concat "\n" block_strs in
  
  Printf.sprintf "%s\n%s\n%s" header body (keyword ctx "endfunc")

(* Constant expression pretty printing *)
let rec pp_const_expr ctx = function
  | ConstZero -> value_color ctx "0"
  | ConstValue cv -> pp_const_value ctx cv
  | ConstAggregate exprs ->
    let expr_strs = List.map (pp_const_expr ctx) exprs in
    Printf.sprintf "<<%s>>" (String.concat ", " expr_strs)

(* Type declaration pretty printing *)
let pp_type_decl ctx (decl : type_decl) =
  Printf.sprintf "%s %s = %s"
    (keyword ctx "type")
    (value_color ctx decl.type_name)
    (pp_type ctx decl.type_def)

(* Object declaration pretty printing *)
let pp_object_decl ctx (obj : object_decl) =
  let kind = if obj.obj_is_const then keyword ctx "const" else keyword ctx "global" in
  let align_str = match obj.obj_align with
    | Some a -> Printf.sprintf " align %d" a
    | None -> "" in
  let attrs_str =
    if ctx.config.show_attributes && not (Attributes.is_empty obj.obj_attrs) then
      " " ^ attr_color ctx (Attributes.string_of_attrs obj.obj_attrs)
    else "" in
  Printf.sprintf "%s %s:%s%s init %s%s"
    kind
    (value_color ctx obj.obj_name)
    (pp_type ctx obj.obj_ty)
    align_str
    (pp_const_expr ctx obj.obj_init)
    attrs_str

(* Top-level item pretty printing *)
let pp_top_item ctx = function
  | TypeDecl td -> pp_type_decl ctx td
  | ObjectDecl od -> pp_object_decl ctx od
  | FuncDecl f -> pp_function ctx f

(* Module pretty printing *)
let pp_module ?(config=default_config) (m : pir_module) =
  let ctx = make_context ~config () in
  let header = comment_color ctx "; PIR v0.9" in
  let item_strs = List.map (pp_top_item ctx) m.items in
  String.concat "\n\n" (header :: item_strs)

(* Module/program pretty printing *)
let pp_program ?(config=default_config) funcs =
  let ctx = make_context ~config () in
  let func_strs = List.map (pp_function ctx) funcs in
  String.concat "\n\n" func_strs

(* Pretty print to file/channel *)
let print_to_channel ?(config=default_config) channel funcs =
  output_string channel (pp_program ~config funcs);
  output_char channel '\n';
  flush channel

let print_to_file ?(config=default_config) filename funcs =
  let channel = open_out filename in
  try
    print_to_channel ~config channel funcs;
    close_out channel
  with e ->
    close_out channel;
    raise e

(* String conversion for debugging *)
let function_to_string ?(config=default_config) func =
  let ctx = make_context ~config () in
  pp_function ctx func

let block_to_string ?(config=default_config) block =
  let ctx = make_context ~config () in
  pp_basic_block ctx block

let instruction_to_string ?(config=default_config) instr =
  let ctx = make_context ~config () in
  pp_instruction ctx instr

(* Module pretty printing *)
let module_to_string ?(config=default_config) m =
  pp_module ~config m

let print_module_to_channel ?(config=default_config) channel m =
  output_string channel (pp_module ~config m);
  output_char channel '\n';
  flush channel

let print_module_to_file ?(config=default_config) filename m =
  let channel = open_out filename in
  try
    print_module_to_channel ~config channel m;
    close_out channel
  with e ->
    close_out channel;
    raise e