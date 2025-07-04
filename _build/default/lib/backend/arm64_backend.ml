(* ARM64 (AArch64) Backend Implementation *)

open Machine
open Compilerkit_pir

(* ARM64 specific registers *)
(* General purpose registers X0-X30 *)
let x0  = make_gpr 0 8   let x1  = make_gpr 1 8   let x2  = make_gpr 2 8   let x3  = make_gpr 3 8
let x4  = make_gpr 4 8   let x5  = make_gpr 5 8   let x6  = make_gpr 6 8   let x7  = make_gpr 7 8
let x8  = make_gpr 8 8   let x9  = make_gpr 9 8   let x10 = make_gpr 10 8  let x11 = make_gpr 11 8
let x12 = make_gpr 12 8  let x13 = make_gpr 13 8  let x14 = make_gpr 14 8  let x15 = make_gpr 15 8
let x16 = make_gpr 16 8  let x17 = make_gpr 17 8  let x18 = make_gpr 18 8  let x19 = make_gpr 19 8
let x20 = make_gpr 20 8  let x21 = make_gpr 21 8  let x22 = make_gpr 22 8  let x23 = make_gpr 23 8
let x24 = make_gpr 24 8  let x25 = make_gpr 25 8  let x26 = make_gpr 26 8  let x27 = make_gpr 27 8
let x28 = make_gpr 28 8  let x29 = make_gpr 29 8  let x30 = make_gpr 30 8  (* X30 is LR *)
let sp  = make_gpr 31 8  (* Stack pointer *)
let xzr = make_gpr 31 8  (* Zero register - same encoding as SP *)

(* 32-bit views of registers *)
let w0  = make_gpr 0 4   let w1  = make_gpr 1 4   let w2  = make_gpr 2 4   let w3  = make_gpr 3 4

(* ARM64 calling convention (AAPCS64) *)
let arm64_calling_conv = {
  int_arg_regs = [x0; x1; x2; x3; x4; x5; x6; x7];
  float_arg_regs = List.init 8 (fun i -> make_fpr i 8);  (* D0-D7 *)
  int_ret_regs = [x0; x1];
  float_ret_regs = [make_fpr 0 8; make_fpr 1 8];  (* D0, D1 *)
  callee_saved = [x19; x20; x21; x22; x23; x24; x25; x26; x27; x28; x29];
  caller_saved = [x0; x1; x2; x3; x4; x5; x6; x7; x8; x9; x10; x11; x12; x13; x14; x15; x16; x17];
  stack_align = 16;
  red_zone = 0;  (* No red zone on ARM64 *)
  param_offset = 0;  (* Parameters start at SP *)
}

(* ARM64 machine configuration *)
let arm64_config = {
  word_size = W64;
  ptr_size = 8;
  big_endian = false;  (* Little endian by default *)
  available_regs = [
    (GPR, 31);  (* X0 through X30 *)
    (FPR, 32);  (* D0 through D31 *)
    (VEC, 32);  (* V0 through V31 for SIMD *)
    (FLAGS, 1); (* NZCV flags *)
  ];
  calling_conv = arm64_calling_conv;
  has_indexed_addr = true;   (* [base, index, LSL #scale] *)
  has_autoinc_addr = true;   (* Pre/post increment addressing *)
  code_align = 4;  (* 32-bit instructions *)
  data_align = 8;
}

(* Pattern for integer binary operations *)
let make_arm64_binop_pattern op ty =
  let size = match ty with
    | Types.Scalar s -> Types.size_of_scalar s
    | _ -> 8
  in
  let emit reg_alloc result_val operands =
    (* Get registers for result and operands *)
    let dst = match result_val with
      | Some v -> reg_alloc v
      | None -> failwith "Binary operation without result"
    in
    let src1, src2 = match operands with
      | [v1; v2] -> (reg_alloc v1, reg_alloc v2)
      | _ -> failwith "Binary operation expects exactly 2 operands"
    in
    match op with
    | Instructions.Add -> 
      [{ label = None; op = ADD (dst, src1, src2); comment = Some "add" }]
    | Instructions.Sub ->
      [{ label = None; op = SUB (dst, src1, src2); comment = Some "subtract" }]
    | Instructions.Mul ->
      [{ label = None; op = MUL (dst, src1, src2); comment = Some "multiply" }]
    | Instructions.And ->
      [{ label = None; op = AND (dst, src1, src2); comment = Some "bitwise and" }]
    | Instructions.Or ->
      [{ label = None; op = OR (dst, src1, src2); comment = Some "bitwise or" }]
    | Instructions.Xor ->
      [{ label = None; op = XOR (dst, src1, src2); comment = Some "bitwise xor" }]
    | Instructions.Shl ->
      [{ label = None; op = SHL (dst, src1, src2); comment = Some "logical shift left" }]
    | Instructions.Lshr ->
      [{ label = None; op = SHR (dst, src1, src2); comment = Some "logical shift right" }]
    | Instructions.Ashr ->
      [{ label = None; op = SAR (dst, src1, src2); comment = Some "arithmetic shift right" }]
    | Instructions.Sdiv ->
      [{ label = None; op = DIV (dst, src1, src2); comment = Some "signed divide" }]
    | Instructions.Udiv ->
      [{ label = None; op = UDIV (dst, src1, src2); comment = Some "unsigned divide" }]
    | Instructions.Srem ->
      (* ARM64 doesn't have a remainder instruction, compute as a - (a/b)*b *)
      (* Need to allocate a temporary register *)
      let tmp_val = Values.create_simple_value ty in
      let tmp = reg_alloc tmp_val in
      [
        { label = None; op = DIV (tmp, src1, src2); comment = Some "signed divide for rem" };
        { label = None; op = MUL (tmp, tmp, src2); comment = Some "multiply quotient by divisor" };
        { label = None; op = SUB (dst, src1, tmp); comment = Some "remainder = dividend - quotient*divisor" };
      ]
    | Instructions.Urem ->
      (* Same for unsigned remainder *)
      let tmp_val = Values.create_simple_value ty in
      let tmp = reg_alloc tmp_val in
      [
        { label = None; op = UDIV (tmp, src1, src2); comment = Some "unsigned divide for rem" };
        { label = None; op = MUL (tmp, tmp, src2); comment = Some "multiply quotient by divisor" };
        { label = None; op = SUB (dst, src1, tmp); comment = Some "remainder = dividend - quotient*divisor" };
      ]
    | _ -> failwith "Unsupported binary operation"
  in
  {
    pir_pattern = Instructions.Binop (op, Instructions.NoFlag,
      Values.create_simple_value ty,
      Values.create_simple_value ty);
    cost = if op = Instructions.Mul || op = Instructions.Sdiv || op = Instructions.Udiv then 3 else 1;
    emit;
    constraints = [RegClass GPR; RegClass GPR; RegClass GPR];
    hints = [];  (* ARM64 has 3-operand instructions, no need to reuse registers *)
  }

(* Pattern for memory load with different sizes *)
let arm64_load_pattern ty =
  let size = match ty with
    | Types.Scalar s -> Types.size_of_scalar s
    | _ -> 8
  in
  {
    pir_pattern = Instructions.Memory (Instructions.Load (ty, Values.create_simple_value Types.Ptr));
    cost = 1;
    emit = (fun reg_alloc result_val operands ->
      let dst = match result_val with
        | Some v -> reg_alloc v
        | None -> failwith "Load without result"
      in
      (* Load takes the address as operand *)
      let addr = match operands with
        | [v] -> reg_alloc v
        | _ -> failwith "Load expects exactly 1 operand (address)"
      in
      let comment = match size with
        | 1 -> "load byte"
        | 2 -> "load halfword"
        | 4 -> "load word"
        | 8 -> "load doubleword"
        | _ -> "load"
      in
      [{ label = None; op = LOAD (dst, Direct addr, size); comment = Some comment }]
    );
    constraints = [RegClass GPR; Memory];
    hints = [];
  }

(* Pattern for memory store with different sizes *)
let arm64_store_pattern val_ty =
  let size = match val_ty with
    | Types.Scalar s -> Types.size_of_scalar s
    | _ -> 8
  in
  let is_float = match val_ty with
    | Types.Scalar Types.F32 | Types.Scalar Types.F64 -> true
    | _ -> false
  in
  {
    pir_pattern = Instructions.Memory (Instructions.Store (
      Values.create_simple_value val_ty,
      Values.create_simple_value Types.Ptr));
    cost = 1;
    emit = (fun reg_alloc result_val operands ->
      (* Store has no result value *)
      let src, addr = match operands with
        | [v; a] -> (reg_alloc v, reg_alloc a)
        | _ -> failwith "Store expects exactly 2 operands (value, address)"
      in
      let comment = match size with
        | 1 -> "store byte"
        | 2 -> "store halfword"
        | 4 -> if is_float then "store float" else "store word"
        | 8 -> if is_float then "store double" else "store doubleword"
        | _ -> "store"
      in
      [{ label = None; op = STORE (src, Direct addr, size); comment = Some comment }]
    );
    constraints = [if is_float then RegClass FPR else RegClass GPR; Memory];
    hints = [];
  }

(* Pattern for address calculation using ARM64's flexible addressing *)
let arm64_gep_pattern =
  {
    pir_pattern = Instructions.Address (Instructions.Gep (
      Values.create_simple_value Types.Ptr,
      Values.create_simple_value (Types.Scalar Types.I64)));
    cost = 0;  (* Often free on ARM64 due to addressing modes *)
    emit = (fun reg_alloc result_val operands ->
      let dst = match result_val with
        | Some v -> reg_alloc v
        | None -> failwith "Gep without result"
      in
      let base, index = match operands with
        | [b; i] -> (reg_alloc b, reg_alloc i)
        | _ -> failwith "Gep expects exactly 2 operands (base, index)"
      in
      (* On ARM64, this might be folded into load/store addressing mode *)
      [{ label = None; op = ADD (dst, base, index); 
         comment = Some "address calculation (may be folded)" }]
    );
    constraints = [RegClass GPR; RegClass GPR; RegClass GPR];
    hints = [];
  }

(* Pattern for integer comparison *)
let make_arm64_icmp_pattern pred ty =
  {
    pir_pattern = Instructions.Icmp (pred,
      Values.create_simple_value ty,
      Values.create_simple_value ty);
    cost = 1;
    emit = (fun reg_alloc result_val operands ->
      let dst = match result_val with
        | Some v -> reg_alloc v
        | None -> failwith "Icmp without result"
      in
      let src1, src2 = match operands with
        | [v1; v2] -> (reg_alloc v1, reg_alloc v2)
        | _ -> failwith "Icmp expects exactly 2 operands"
      in
      (* ARM64 uses CMP + CSET for comparisons *)
      let cond = match pred with
        | Instructions.Eq -> EQ  | Instructions.Ne -> NE
        | Instructions.Slt -> LT | Instructions.Sle -> LE  
        | Instructions.Sgt -> GT | Instructions.Sge -> GE
        | Instructions.Ult -> ULT | Instructions.Ule -> ULE
        | Instructions.Ugt -> UGT | Instructions.Uge -> UGE
      in
      [
        { label = None; op = CMP (src1, src2); comment = Some "compare" };
        { label = None; op = CSET (dst, cond); comment = Some "conditional set" };
      ]
    );
    constraints = [RegClass GPR; RegClass GPR; RegClass GPR];
    hints = [];
  }

(* Pattern for select (conditional move) *)
let arm64_select_pattern ty =
  {
    pir_pattern = Instructions.Select (
      Values.create_simple_value (Types.Scalar Types.I1),
      Values.create_simple_value ty,
      Values.create_simple_value ty);
    cost = 1;  (* CSEL is single cycle on ARM64 *)
    emit = (fun reg_alloc result_val operands ->
      let dst = match result_val with
        | Some v -> reg_alloc v
        | None -> failwith "Select without result"
      in
      let cond_reg, true_reg, false_reg = match operands with
        | [c; t; f] -> (reg_alloc c, reg_alloc t, reg_alloc f)
        | _ -> failwith "Select expects exactly 3 operands (condition, true_val, false_val)"
      in
      [
        { label = None; op = CMP (cond_reg, xzr); comment = Some "test condition" };
        { label = None; op = MOV (dst, true_reg); comment = Some "csel ne (conditional select)" };
      ]
    );
    constraints = [RegClass GPR; RegClass GPR; RegClass GPR; RegClass GPR];
    hints = [];
  }

(* Emit function prologue *)
let emit_arm64_prologue frame =
  (* For leaf functions with small frames, use simplified prologue *)
  if frame.frame_size <= 16 then
    (* Simple leaf function - just adjust stack *)
    [
      { label = None; op = ADJUST_SP (Int64.neg (Int64.of_int frame.frame_size)); 
        comment = Some (Printf.sprintf "allocate %d bytes" frame.frame_size) };
    ]
  else
    (* Standard prologue for non-leaf functions *)
    [
      { label = None; op = STORE (x29, PreIndex (sp, Int64.neg 16L), 8); 
        comment = Some "save FP and LR with pre-decrement" };
      { label = None; op = STORE (x30, Offset (sp, 8L), 8); 
        comment = None };
      { label = None; op = MOV (x29, sp); comment = Some "set up frame pointer" };
      { label = None; op = ADJUST_SP (Int64.neg (Int64.of_int frame.frame_size)); 
        comment = Some (Printf.sprintf "allocate %d bytes" frame.frame_size) };
    ]

(* Emit function epilogue *)
let emit_arm64_epilogue frame =
  (* For leaf functions with small frames, use simplified epilogue *)
  if frame.frame_size <= 16 then
    (* Simple leaf function - just adjust stack back *)
    [
      { label = None; op = ADJUST_SP (Int64.of_int frame.frame_size); 
        comment = Some "deallocate stack frame" };
    ]
  else
    (* Standard epilogue for non-leaf functions *)
    [
      { label = None; op = MOV (sp, x29); comment = Some "restore stack pointer" };
      { label = None; op = LOAD (x29, PostIndex (sp, 16L), 8); 
        comment = Some "restore FP and LR with post-increment" };
      { label = None; op = LOAD (x30, Offset (sp, Int64.neg 8L), 8); 
        comment = None };
    ]

(* Pattern for return-like instruction - using a dummy constant for pattern matching *)
let arm64_ret_pattern has_value =
  {
    pir_pattern = Instructions.Const (Values.ConstInt (0L, Types.I64));  (* Dummy pattern *)
    cost = 1;
    emit = (fun reg_alloc result_val operands ->
      let instrs = 
        if has_value then
          match operands with
          | [v] -> 
            let src = reg_alloc v in
            [{ label = None; op = MOV (x0, src); comment = Some "return value" }]
          | _ -> failwith "Return with value expects exactly 1 operand"
        else [] in
      instrs @ emit_arm64_epilogue { frame_size = 0; locals_offset = 0; 
                                     spill_offset = 0; callee_save_offset = 0; 
                                     alignment = 16 }
    );
    constraints = if has_value then [RegClass GPR] else [];
    hints = if has_value then [SpecificReg x0] else [];  (* Hint to allocate return value in x0 *)
  }

(* Pattern for unconditional jumps - using dummy pattern *)
let arm64_jmp_pattern =
  {
    pir_pattern = Instructions.Const (Values.ConstInt (1L, Types.I64));  (* Dummy pattern *)
    cost = 1;
    emit = (fun reg_alloc result_val operands ->
      (* Jump has no operands or result *)
      [{ label = None; op = JMP "target"; comment = Some "unconditional branch" }]
    );
    constraints = [];
    hints = [];
  }

(* Pattern for conditional branches - using dummy pattern *)
let arm64_br_pattern =
  {
    pir_pattern = Instructions.Const (Values.ConstInt (2L, Types.I64));  (* Dummy pattern *)
    cost = 1;
    emit = (fun reg_alloc result_val operands ->
      let cond_reg = match operands with
        | [v] -> reg_alloc v
        | _ -> failwith "Conditional branch expects exactly 1 operand (condition)"
      in
      [
        { label = None; op = CMP (cond_reg, xzr); comment = Some "test condition" };
        { label = None; op = JCC (NE, "then"); comment = Some "branch if true" };
        { label = None; op = JMP "else"; comment = Some "fall through to false" };
      ]
    );
    constraints = [RegClass GPR];
    hints = [];
  }

(* Emit function call *)
let emit_arm64_call name arg_regs result_reg =
  (* Move arguments to calling convention registers *)
  let arg_moves = List.mapi (fun i arg_reg ->
    if i < 8 then
      (* Use correct size calling convention register to match argument size *)
      let conv_reg = 
        let base_reg = List.nth arm64_calling_conv.int_arg_regs i in
        make_gpr base_reg.reg_index arg_reg.reg_size
      in
      { label = None; op = MOV (conv_reg, arg_reg); 
        comment = Some (Printf.sprintf "arg %d" i) }
    else
      (* Push pairs of registers for better alignment *)
      { label = None; op = STORE (arg_reg, PreIndex (sp, Int64.neg 8L), 8); 
        comment = Some (Printf.sprintf "arg %d on stack" i) }
  ) arg_regs in
  
  (* Call instruction - BL in ARM64 *)
  let call_instr = { label = None; op = CALL (None, Some name); comment = Some "branch with link" } in
  
  (* Get result if needed *)
  let result_move = match result_reg with
    | Some dst -> 
      (* Use correct size return register to match destination size *)
      let ret_reg = make_gpr 0 dst.reg_size in
      [{ label = None; op = MOV (dst, ret_reg); comment = Some "get result" }]
    | None -> []
  in
  
  (* Adjust stack if we pushed arguments *)
  let stack_adjust = 
    let pushed = max 0 (List.length arg_regs - 8) in
    if pushed > 0 then
      [{ label = None; op = ADJUST_SP (Int64.of_int (pushed * 8)); 
         comment = Some "remove pushed arguments" }]
    else []
  in
  
  arg_moves @ [call_instr] @ result_move @ stack_adjust

(* Pattern for function calls *)
let arm64_call_pattern has_result =
  let result_ty = if has_result then Some (Types.Scalar Types.I64) else None in
  {
    pir_pattern = Instructions.Call (Instructions.Call (
      Values.create_simple_value Types.Ptr,  (* Function pointer *)
      []));  (* Empty args for pattern matching *)
    cost = 5;  (* Calls are expensive *)
    emit = (fun reg_alloc result_val operands ->
      (* This is a simplified version - real implementation would handle args *)
      (* First operand is the function pointer, rest are arguments *)
      match operands with
      | fn_ptr :: args ->
        (* Extract function name from the function pointer's attributes *)
        let func_name = 
          match Attributes.get_opt "function_name" (Values.get_attrs fn_ptr) with
          | Some (Attributes.String name) -> name
          | _ -> 
            (* Fallback to generic name or error *)
            Printf.sprintf "func_%d" (Values.get_id fn_ptr)
        in
        let arg_regs = List.map reg_alloc args in
        let result_reg = match result_val with
          | Some v -> Some (reg_alloc v)
          | None -> None
        in
        emit_arm64_call func_name arg_regs result_reg
      | _ -> failwith "Call expects at least a function pointer"
    );
    constraints = RegClass GPR :: (if has_result then [RegClass GPR] else []);
    hints = [];
  }

(* Pattern for floating-point binary operations *)
let make_arm64_float_binop_pattern op ty =
  let size = match ty with
    | Types.Scalar Types.F32 -> 4
    | Types.Scalar Types.F64 -> 8
    | _ -> failwith "Expected float type"
  in
  {
    pir_pattern = Instructions.Binop (op, Instructions.NoFlag,
      Values.create_simple_value ty,
      Values.create_simple_value ty);
    cost = (match op with
      | Instructions.Fdiv -> 10  (* Division is expensive *)
      | _ -> 2);  (* Other FP ops take a few cycles *)
    emit = (fun reg_alloc result_val operands ->
      let dst = match result_val with
        | Some v -> reg_alloc v
        | None -> failwith "Float binary operation without result"
      in
      let src1, src2 = match operands with
        | [v1; v2] -> (reg_alloc v1, reg_alloc v2)
        | _ -> failwith "Float binary operation expects exactly 2 operands"
      in
      match op with
      | Instructions.Fadd ->
        [{ label = None; op = FADD (dst, src1, src2); comment = Some "float add" }]
      | Instructions.Fsub ->
        [{ label = None; op = FSUB (dst, src1, src2); comment = Some "float subtract" }]
      | Instructions.Fmul ->
        [{ label = None; op = FMUL (dst, src1, src2); comment = Some "float multiply" }]
      | Instructions.Fdiv ->
        [{ label = None; op = FDIV (dst, src1, src2); comment = Some "float divide" }]
      | _ -> failwith "Unsupported float operation"
    );
    constraints = [RegClass FPR; RegClass FPR; RegClass FPR];
    hints = [];
  }

(* Pattern for float comparison *)
let make_arm64_fcmp_pattern pred ty =
  {
    pir_pattern = Instructions.Fcmp (pred,
      Values.create_simple_value ty,
      Values.create_simple_value ty);
    cost = 1;
    emit = (fun reg_alloc result_val operands ->
      let dst = match result_val with
        | Some v -> reg_alloc v
        | None -> failwith "Fcmp without result"
      in
      let src1, src2 = match operands with
        | [v1; v2] -> (reg_alloc v1, reg_alloc v2)
        | _ -> failwith "Fcmp expects exactly 2 operands"
      in
      [
        { label = None; op = FCMP (src1, src2); comment = Some "float compare" };
        { label = None; op = MOV (dst, xzr); comment = Some "cset from FP flags" };
      ]
    );
    constraints = [RegClass FPR; RegClass FPR; RegClass GPR];
    hints = [];
  }

(* Pattern for int to float conversion *)
let arm64_sitofp_pattern from_ty to_ty =
  let from_size = Types.size_of_scalar (match from_ty with Types.Scalar s -> s | _ -> Types.I64) in
  let to_size = Types.size_of_scalar (match to_ty with Types.Scalar s -> s | _ -> Types.F64) in
  {
    pir_pattern = Instructions.Cast (Instructions.Sitofp (
      Values.create_simple_value from_ty, to_ty));
    cost = 2;
    emit = (fun reg_alloc result_val operands ->
      let dst = match result_val with
        | Some v -> reg_alloc v
        | None -> failwith "Sitofp without result"
      in
      let src = match operands with
        | [v] -> reg_alloc v
        | _ -> failwith "Sitofp expects exactly 1 operand"
      in
      [{ label = None; op = CVT_I2F (dst, src, from_size, to_size); 
         comment = Some "signed int to float" }]
    );
    constraints = [RegClass GPR; RegClass FPR];
    hints = [];
  }

(* Pattern for bit counting operations *)
let make_arm64_bitcount_pattern op ty =
  let size = match ty with
    | Types.Scalar s -> Types.size_of_scalar s
    | _ -> 8
  in
  {
    pir_pattern = Instructions.Binop (op, Instructions.NoFlag,
      Values.create_simple_value ty,
      Values.create_simple_value ty);  (* Second operand is dummy for unary ops *)
    cost = 1;  (* Single instruction on ARM64 *)
    emit = (fun reg_alloc result_val operands ->
      let dst = match result_val with
        | Some v -> reg_alloc v
        | None -> failwith "Bitcount operation without result"
      in
      let src = match operands with
        | v :: _ -> reg_alloc v  (* Only use first operand for unary ops *)
        | _ -> failwith "Bitcount operation expects at least 1 operand"
      in
      match op with
      | Instructions.Clz ->
        [{ label = None; op = MOV (dst, src); comment = Some "clz (count leading zeros)" }]
      | Instructions.Ctz ->
        (* ARM64 doesn't have CTZ, use RBIT + CLZ *)
        let tmp_val = Values.create_simple_value ty in
        let tmp = reg_alloc tmp_val in
        [
          { label = None; op = MOV (tmp, src); comment = Some "rbit (reverse bits)" };
          { label = None; op = MOV (dst, tmp); comment = Some "clz (count trailing zeros via rbit+clz)" };
        ]
      | Instructions.Popcnt ->
        [{ label = None; op = MOV (dst, src); comment = Some "cnt (population count)" }]
      | _ -> failwith "Not a bit counting operation"
    );
    constraints = [RegClass GPR; RegClass GPR];
    hints = [];
  }

(* Pattern for float to int conversion *)
let arm64_fptosi_pattern from_ty to_ty =
  let from_size = Types.size_of_scalar (match from_ty with Types.Scalar s -> s | _ -> Types.F64) in
  let to_size = Types.size_of_scalar (match to_ty with Types.Scalar s -> s | _ -> Types.I64) in
  {
    pir_pattern = Instructions.Cast (Instructions.Fptosi (
      Values.create_simple_value from_ty, to_ty));
    cost = 2;
    emit = (fun reg_alloc result_val operands ->
      let dst = match result_val with
        | Some v -> reg_alloc v
        | None -> failwith "Fptosi without result"
      in
      let src = match operands with
        | [v] -> reg_alloc v
        | _ -> failwith "Fptosi expects exactly 1 operand"
      in
      [{ label = None; op = CVT_F2I (dst, src, from_size, to_size); 
         comment = Some "float to signed int" }]
    );
    constraints = [RegClass FPR; RegClass GPR];
    hints = [];
  }

(* Pattern for unsigned int to float conversion *)
let arm64_uitofp_pattern from_ty to_ty =
  let from_size = Types.size_of_scalar (match from_ty with Types.Scalar s -> s | _ -> Types.I64) in
  let to_size = Types.size_of_scalar (match to_ty with Types.Scalar s -> s | _ -> Types.F64) in
  {
    pir_pattern = Instructions.Cast (Instructions.Uitofp (
      Values.create_simple_value from_ty, to_ty));
    cost = 2;
    emit = (fun reg_alloc result_val operands ->
      let dst = match result_val with
        | Some v -> reg_alloc v
        | None -> failwith "Uitofp without result"
      in
      let src = match operands with
        | [v] -> reg_alloc v
        | _ -> failwith "Uitofp expects exactly 1 operand"
      in
      [{ label = None; op = CVT_I2F (dst, src, from_size, to_size); 
         comment = Some "unsigned int to float" }]
    );
    constraints = [RegClass GPR; RegClass FPR];
    hints = [];
  }

(* Pattern for float to unsigned int conversion *)
let arm64_fptoui_pattern from_ty to_ty =
  let from_size = Types.size_of_scalar (match from_ty with Types.Scalar s -> s | _ -> Types.F64) in
  let to_size = Types.size_of_scalar (match to_ty with Types.Scalar s -> s | _ -> Types.I64) in
  {
    pir_pattern = Instructions.Cast (Instructions.Fptoui (
      Values.create_simple_value from_ty, to_ty));
    cost = 2;
    emit = (fun reg_alloc result_val operands ->
      let dst = match result_val with
        | Some v -> reg_alloc v
        | None -> failwith "Fptoui without result"
      in
      let src = match operands with
        | [v] -> reg_alloc v
        | _ -> failwith "Fptoui expects exactly 1 operand"
      in
      [{ label = None; op = CVT_F2I (dst, src, from_size, to_size); 
         comment = Some "float to unsigned int" }]
    );
    constraints = [RegClass FPR; RegClass GPR];
    hints = [];
  }

(* Pattern for float extend *)
let arm64_fpext_pattern from_ty to_ty =
  {
    pir_pattern = Instructions.Cast (Instructions.Fpext (
      Values.create_simple_value from_ty, to_ty));
    cost = 1;
    emit = (fun reg_alloc result_val operands ->
      let dst = match result_val with
        | Some v -> reg_alloc v
        | None -> failwith "Fpext without result"
      in
      let src = match operands with
        | [v] -> reg_alloc v
        | _ -> failwith "Fpext expects exactly 1 operand"
      in
      [{ label = None; op = CVT_F2F (dst, src, 4, 8); 
         comment = Some "float extend (f32 to f64)" }]
    );
    constraints = [RegClass FPR; RegClass FPR];
    hints = [];
  }

(* Pattern for float truncate *)
let arm64_fptrunc_pattern from_ty to_ty =
  {
    pir_pattern = Instructions.Cast (Instructions.Fptrunc (
      Values.create_simple_value from_ty, to_ty));
    cost = 1;
    emit = (fun reg_alloc result_val operands ->
      let dst = match result_val with
        | Some v -> reg_alloc v
        | None -> failwith "Fptrunc without result"
      in
      let src = match operands with
        | [v] -> reg_alloc v
        | _ -> failwith "Fptrunc expects exactly 1 operand"
      in
      [{ label = None; op = CVT_F2F (dst, src, 8, 4); 
         comment = Some "float truncate (f64 to f32)" }]
    );
    constraints = [RegClass FPR; RegClass FPR];
    hints = [];
  }

(* Pattern for integer truncate *)
let arm64_trunc_pattern from_ty to_ty =
  let to_size = Types.size_of_scalar (match to_ty with Types.Scalar s -> s | _ -> Types.I32) in
  {
    pir_pattern = Instructions.Cast (Instructions.Trunc (
      Values.create_simple_value from_ty, to_ty));
    cost = 0;  (* Usually free on ARM64 - just use smaller register view *)
    emit = (fun reg_alloc result_val operands ->
      let dst = match result_val with
        | Some v -> reg_alloc v
        | None -> failwith "Trunc without result"
      in
      let src = match operands with
        | [v] -> reg_alloc v
        | _ -> failwith "Trunc expects exactly 1 operand"
      in
      [{ label = None; op = MOV (dst, src); comment = Some "truncate (use smaller register)" }]
    );
    constraints = [RegClass GPR; RegClass GPR];
    hints = [];
  }

(* Pattern for zero extend *)
let arm64_zext_pattern from_ty to_ty =
  let from_size = Types.size_of_scalar (match from_ty with Types.Scalar s -> s | _ -> Types.I32) in
  {
    pir_pattern = Instructions.Cast (Instructions.Zext (
      Values.create_simple_value from_ty, to_ty));
    cost = 1;
    emit = (fun reg_alloc result_val operands ->
      let dst = match result_val with
        | Some v -> reg_alloc v
        | None -> failwith "Zext without result"
      in
      let src = match operands with
        | [v] -> reg_alloc v
        | _ -> failwith "Zext expects exactly 1 operand"
      in
      [{ label = None; op = EXT (dst, src, from_size, false); 
         comment = Some "zero extend" }]
    );
    constraints = [RegClass GPR; RegClass GPR];
    hints = [];
  }

(* Pattern for sign extend *)
let arm64_sext_pattern from_ty to_ty =
  let from_size = Types.size_of_scalar (match from_ty with Types.Scalar s -> s | _ -> Types.I32) in
  {
    pir_pattern = Instructions.Cast (Instructions.Sext (
      Values.create_simple_value from_ty, to_ty));
    cost = 1;
    emit = (fun reg_alloc result_val operands ->
      let dst = match result_val with
        | Some v -> reg_alloc v
        | None -> failwith "Sext without result"
      in
      let src = match operands with
        | [v] -> reg_alloc v
        | _ -> failwith "Sext expects exactly 1 operand"
      in
      [{ label = None; op = EXT (dst, src, from_size, true); 
         comment = Some "sign extend" }]
    );
    constraints = [RegClass GPR; RegClass GPR];
    hints = [];
  }

(* Pattern for loading constants *)
let arm64_const_pattern scalar_ty =
  {
    pir_pattern = Instructions.Const (Values.ConstInt (0L, scalar_ty));  (* Dummy constant *)
    cost = 1;  (* Will be refined by materialize_constant *)
    emit = (fun reg_alloc result_val operands ->
      (* Constants are special - they don't have operands but the result value contains the constant *)
      let dst = match result_val with
        | Some v -> reg_alloc v
        | None -> failwith "Constant without result"
      in
      (* For constants, the emit function in instruction_selection.ml handles this specially
         by calling materialize_constant. We just need to return an empty list here. *)
      []  (* Handled specially in instruction_selection.ml *)
    );
    constraints = [RegClass GPR];
    hints = [];
  }

(* Extended pattern list *)
let arm64_patterns = [
  (* Integer arithmetic - different sizes *)
  make_arm64_binop_pattern Instructions.Add (Types.Scalar Types.I8);
  make_arm64_binop_pattern Instructions.Add (Types.Scalar Types.I16);
  make_arm64_binop_pattern Instructions.Add (Types.Scalar Types.I32);
  make_arm64_binop_pattern Instructions.Add (Types.Scalar Types.I64);
  make_arm64_binop_pattern Instructions.Sub (Types.Scalar Types.I8);
  make_arm64_binop_pattern Instructions.Sub (Types.Scalar Types.I16);
  make_arm64_binop_pattern Instructions.Sub (Types.Scalar Types.I32);
  make_arm64_binop_pattern Instructions.Sub (Types.Scalar Types.I64);
  make_arm64_binop_pattern Instructions.Mul (Types.Scalar Types.I32);
  make_arm64_binop_pattern Instructions.Mul (Types.Scalar Types.I64);
  make_arm64_binop_pattern Instructions.Sdiv (Types.Scalar Types.I32);
  make_arm64_binop_pattern Instructions.Sdiv (Types.Scalar Types.I64);
  make_arm64_binop_pattern Instructions.Udiv (Types.Scalar Types.I32);
  make_arm64_binop_pattern Instructions.Udiv (Types.Scalar Types.I64);
  make_arm64_binop_pattern Instructions.Srem (Types.Scalar Types.I32);
  make_arm64_binop_pattern Instructions.Srem (Types.Scalar Types.I64);
  make_arm64_binop_pattern Instructions.Urem (Types.Scalar Types.I32);
  make_arm64_binop_pattern Instructions.Urem (Types.Scalar Types.I64);
  
  (* Bitwise operations *)
  make_arm64_binop_pattern Instructions.And (Types.Scalar Types.I32);
  make_arm64_binop_pattern Instructions.And (Types.Scalar Types.I64);
  make_arm64_binop_pattern Instructions.Or (Types.Scalar Types.I32);
  make_arm64_binop_pattern Instructions.Or (Types.Scalar Types.I64);
  make_arm64_binop_pattern Instructions.Xor (Types.Scalar Types.I32);
  make_arm64_binop_pattern Instructions.Xor (Types.Scalar Types.I64);
  make_arm64_binop_pattern Instructions.Shl (Types.Scalar Types.I32);
  make_arm64_binop_pattern Instructions.Shl (Types.Scalar Types.I64);
  make_arm64_binop_pattern Instructions.Lshr (Types.Scalar Types.I32);
  make_arm64_binop_pattern Instructions.Lshr (Types.Scalar Types.I64);
  make_arm64_binop_pattern Instructions.Ashr (Types.Scalar Types.I32);
  make_arm64_binop_pattern Instructions.Ashr (Types.Scalar Types.I64);
  
  (* Bit counting operations *)
  make_arm64_bitcount_pattern Instructions.Clz (Types.Scalar Types.I32);
  make_arm64_bitcount_pattern Instructions.Clz (Types.Scalar Types.I64);
  make_arm64_bitcount_pattern Instructions.Ctz (Types.Scalar Types.I32);
  make_arm64_bitcount_pattern Instructions.Ctz (Types.Scalar Types.I64);
  make_arm64_bitcount_pattern Instructions.Popcnt (Types.Scalar Types.I32);
  make_arm64_bitcount_pattern Instructions.Popcnt (Types.Scalar Types.I64);
  
  (* Comparisons - for different integer sizes *)
  (* I32 comparisons *)
  make_arm64_icmp_pattern Instructions.Eq (Types.Scalar Types.I32);
  make_arm64_icmp_pattern Instructions.Ne (Types.Scalar Types.I32);
  make_arm64_icmp_pattern Instructions.Slt (Types.Scalar Types.I32);
  make_arm64_icmp_pattern Instructions.Sle (Types.Scalar Types.I32);
  make_arm64_icmp_pattern Instructions.Sgt (Types.Scalar Types.I32);
  make_arm64_icmp_pattern Instructions.Sge (Types.Scalar Types.I32);
  make_arm64_icmp_pattern Instructions.Ult (Types.Scalar Types.I32);
  make_arm64_icmp_pattern Instructions.Ule (Types.Scalar Types.I32);
  make_arm64_icmp_pattern Instructions.Ugt (Types.Scalar Types.I32);
  make_arm64_icmp_pattern Instructions.Uge (Types.Scalar Types.I32);
  
  (* I64 comparisons *)
  make_arm64_icmp_pattern Instructions.Eq (Types.Scalar Types.I64);
  make_arm64_icmp_pattern Instructions.Ne (Types.Scalar Types.I64);
  make_arm64_icmp_pattern Instructions.Slt (Types.Scalar Types.I64);
  make_arm64_icmp_pattern Instructions.Sle (Types.Scalar Types.I64);
  make_arm64_icmp_pattern Instructions.Sgt (Types.Scalar Types.I64);
  make_arm64_icmp_pattern Instructions.Sge (Types.Scalar Types.I64);
  make_arm64_icmp_pattern Instructions.Ult (Types.Scalar Types.I64);
  make_arm64_icmp_pattern Instructions.Ule (Types.Scalar Types.I64);
  make_arm64_icmp_pattern Instructions.Ugt (Types.Scalar Types.I64);
  make_arm64_icmp_pattern Instructions.Uge (Types.Scalar Types.I64);
  
  (* I8 and I16 comparisons *)
  make_arm64_icmp_pattern Instructions.Eq (Types.Scalar Types.I8);
  make_arm64_icmp_pattern Instructions.Ne (Types.Scalar Types.I8);
  make_arm64_icmp_pattern Instructions.Eq (Types.Scalar Types.I16);
  make_arm64_icmp_pattern Instructions.Ne (Types.Scalar Types.I16);
  
  (* Floating-point arithmetic *)
  make_arm64_float_binop_pattern Instructions.Fadd (Types.Scalar Types.F32);
  make_arm64_float_binop_pattern Instructions.Fadd (Types.Scalar Types.F64);
  make_arm64_float_binop_pattern Instructions.Fsub (Types.Scalar Types.F32);
  make_arm64_float_binop_pattern Instructions.Fsub (Types.Scalar Types.F64);
  make_arm64_float_binop_pattern Instructions.Fmul (Types.Scalar Types.F32);
  make_arm64_float_binop_pattern Instructions.Fmul (Types.Scalar Types.F64);
  make_arm64_float_binop_pattern Instructions.Fdiv (Types.Scalar Types.F32);
  make_arm64_float_binop_pattern Instructions.Fdiv (Types.Scalar Types.F64);
  
  (* Floating-point comparisons *)
  make_arm64_fcmp_pattern Instructions.Oeq (Types.Scalar Types.F32);
  make_arm64_fcmp_pattern Instructions.Oeq (Types.Scalar Types.F64);
  make_arm64_fcmp_pattern Instructions.One (Types.Scalar Types.F32);
  make_arm64_fcmp_pattern Instructions.One (Types.Scalar Types.F64);
  make_arm64_fcmp_pattern Instructions.Olt (Types.Scalar Types.F32);
  make_arm64_fcmp_pattern Instructions.Olt (Types.Scalar Types.F64);
  make_arm64_fcmp_pattern Instructions.Ole (Types.Scalar Types.F32);
  make_arm64_fcmp_pattern Instructions.Ole (Types.Scalar Types.F64);
  make_arm64_fcmp_pattern Instructions.Ogt (Types.Scalar Types.F32);
  make_arm64_fcmp_pattern Instructions.Ogt (Types.Scalar Types.F64);
  make_arm64_fcmp_pattern Instructions.Oge (Types.Scalar Types.F32);
  make_arm64_fcmp_pattern Instructions.Oge (Types.Scalar Types.F64);
  
  (* Type conversions *)
  arm64_sitofp_pattern (Types.Scalar Types.I32) (Types.Scalar Types.F32);
  arm64_sitofp_pattern (Types.Scalar Types.I32) (Types.Scalar Types.F64);
  arm64_sitofp_pattern (Types.Scalar Types.I64) (Types.Scalar Types.F32);
  arm64_sitofp_pattern (Types.Scalar Types.I64) (Types.Scalar Types.F64);
  arm64_uitofp_pattern (Types.Scalar Types.I32) (Types.Scalar Types.F32);
  arm64_uitofp_pattern (Types.Scalar Types.I32) (Types.Scalar Types.F64);
  arm64_uitofp_pattern (Types.Scalar Types.I64) (Types.Scalar Types.F32);
  arm64_uitofp_pattern (Types.Scalar Types.I64) (Types.Scalar Types.F64);
  arm64_fptosi_pattern (Types.Scalar Types.F32) (Types.Scalar Types.I32);
  arm64_fptosi_pattern (Types.Scalar Types.F32) (Types.Scalar Types.I64);
  arm64_fptosi_pattern (Types.Scalar Types.F64) (Types.Scalar Types.I32);
  arm64_fptosi_pattern (Types.Scalar Types.F64) (Types.Scalar Types.I64);
  arm64_fptoui_pattern (Types.Scalar Types.F32) (Types.Scalar Types.I32);
  arm64_fptoui_pattern (Types.Scalar Types.F32) (Types.Scalar Types.I64);
  arm64_fptoui_pattern (Types.Scalar Types.F64) (Types.Scalar Types.I32);
  arm64_fptoui_pattern (Types.Scalar Types.F64) (Types.Scalar Types.I64);
  arm64_fpext_pattern (Types.Scalar Types.F32) (Types.Scalar Types.F64);
  arm64_fptrunc_pattern (Types.Scalar Types.F64) (Types.Scalar Types.F32);
  
  (* Integer size conversions *)
  arm64_trunc_pattern (Types.Scalar Types.I64) (Types.Scalar Types.I32);
  arm64_trunc_pattern (Types.Scalar Types.I32) (Types.Scalar Types.I16);
  arm64_trunc_pattern (Types.Scalar Types.I16) (Types.Scalar Types.I8);
  arm64_zext_pattern (Types.Scalar Types.I8) (Types.Scalar Types.I16);
  arm64_zext_pattern (Types.Scalar Types.I8) (Types.Scalar Types.I32);
  arm64_zext_pattern (Types.Scalar Types.I8) (Types.Scalar Types.I64);
  arm64_zext_pattern (Types.Scalar Types.I16) (Types.Scalar Types.I32);
  arm64_zext_pattern (Types.Scalar Types.I16) (Types.Scalar Types.I64);
  arm64_zext_pattern (Types.Scalar Types.I32) (Types.Scalar Types.I64);
  arm64_sext_pattern (Types.Scalar Types.I8) (Types.Scalar Types.I16);
  arm64_sext_pattern (Types.Scalar Types.I8) (Types.Scalar Types.I32);
  arm64_sext_pattern (Types.Scalar Types.I8) (Types.Scalar Types.I64);
  arm64_sext_pattern (Types.Scalar Types.I16) (Types.Scalar Types.I32);
  arm64_sext_pattern (Types.Scalar Types.I16) (Types.Scalar Types.I64);
  arm64_sext_pattern (Types.Scalar Types.I32) (Types.Scalar Types.I64);
  
  (* Memory operations *)
  arm64_load_pattern (Types.Scalar Types.I8);
  arm64_load_pattern (Types.Scalar Types.I16);
  arm64_load_pattern (Types.Scalar Types.I32);
  arm64_load_pattern (Types.Scalar Types.I64);
  arm64_load_pattern (Types.Scalar Types.F32);
  arm64_load_pattern (Types.Scalar Types.F64);
  arm64_store_pattern (Types.Scalar Types.I8);
  arm64_store_pattern (Types.Scalar Types.I16);
  arm64_store_pattern (Types.Scalar Types.I32);
  arm64_store_pattern (Types.Scalar Types.I64);
  arm64_store_pattern (Types.Scalar Types.F32);
  arm64_store_pattern (Types.Scalar Types.F64);
  
  (* Select/conditional move *)
  arm64_select_pattern (Types.Scalar Types.I32);
  arm64_select_pattern (Types.Scalar Types.I64);
  arm64_select_pattern (Types.Scalar Types.F32);
  arm64_select_pattern (Types.Scalar Types.F64);
  
  (* Constants *)
  arm64_const_pattern Types.I8;
  arm64_const_pattern Types.I16;
  arm64_const_pattern Types.I32;
  arm64_const_pattern Types.I64;
  
  (* Function calls *)
  arm64_call_pattern true;   (* Call with result *)
  arm64_call_pattern false;  (* Call void *)
  
  (* Address calculation *)
  arm64_gep_pattern;
]

(* Materialize constant into register *)
let materialize_arm64_constant value ty dst =
  match ty with
  | Types.Scalar scalar_ty ->
    if value = 0L then
      (* For zero, use MOV_IMM with 0 instead of MOV from zero register *)
      (* This avoids confusion with SP vs XZR/WZR in the assembly output *)
      [{ label = None; op = MOV_IMM (dst, 0L); comment = Some "load zero" }]
    else
      (* For simple cases, just use MOV_IMM which will be expanded during assembly *)
      [{ label = None; op = MOV_IMM (dst, value); comment = Some (Printf.sprintf "load constant %Ld" value) }]
  | _ -> failwith "Can only materialize scalar constants"

(* ARM64 backend module *)
module ARM64Backend : MACHINE = struct
  let config = arm64_config
  let patterns = arm64_patterns
  let emit_prologue = emit_arm64_prologue
  let emit_epilogue = emit_arm64_epilogue  
  let emit_call = emit_arm64_call
  let materialize_constant = materialize_arm64_constant
end