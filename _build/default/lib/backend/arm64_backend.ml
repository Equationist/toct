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
  let emit reg_alloc =
    let dst = make_gpr 0 size in
    let src1 = make_gpr 1 size in
    let src2 = make_gpr 2 size in
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
      let tmp = make_gpr 3 size in
      [
        { label = None; op = DIV (tmp, src1, src2); comment = Some "signed divide for rem" };
        { label = None; op = MUL (tmp, tmp, src2); comment = Some "multiply quotient by divisor" };
        { label = None; op = SUB (dst, src1, tmp); comment = Some "remainder = dividend - quotient*divisor" };
      ]
    | Instructions.Urem ->
      (* Same for unsigned remainder *)
      let tmp = make_gpr 3 size in
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
    pir_pattern = Instructions.Memory (Instructions.Load ty);
    cost = 1;
    emit = (fun reg_alloc ->
      let dst = make_gpr 0 size in
      let addr = make_gpr 1 8 in
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
    emit = (fun reg_alloc ->
      let src = if is_float then make_fpr 0 size else make_gpr 0 size in
      let addr = make_gpr 1 8 in
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
    emit = (fun reg_alloc ->
      let dst = make_gpr 0 8 in
      let base = make_gpr 1 8 in
      let index = make_gpr 2 8 in
      (* On ARM64, this might be folded into load/store addressing mode *)
      [{ label = None; op = ADD (dst, base, index); 
         comment = Some "address calculation (may be folded)" }]
    );
    constraints = [RegClass GPR; RegClass GPR; RegClass GPR];
    hints = [];
  }

(* Pattern for integer comparison *)
let make_arm64_icmp_pattern pred =
  {
    pir_pattern = Instructions.Icmp (pred,
      Values.create_simple_value (Types.Scalar Types.I64),
      Values.create_simple_value (Types.Scalar Types.I64));
    cost = 1;
    emit = (fun reg_alloc ->
      let src1 = make_gpr 0 8 in
      let src2 = make_gpr 1 8 in
      let dst = make_gpr 2 1 in  (* Result is boolean *)
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
        { label = None; op = MOV (dst, xzr); comment = Some "cset (conditional set)" };
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
    emit = (fun reg_alloc ->
      let cond_reg = make_gpr 0 1 in
      let true_reg = make_gpr 1 8 in
      let false_reg = make_gpr 2 8 in
      let dst = make_gpr 3 8 in
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
  (* ARM64 typically saves FP and LR together *)
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
  [
    { label = None; op = MOV (sp, x29); comment = Some "restore stack pointer" };
    { label = None; op = LOAD (x29, PostIndex (sp, 16L), 8); 
      comment = Some "restore FP and LR with post-increment" };
    { label = None; op = LOAD (x30, Offset (sp, Int64.neg 8L), 8); 
      comment = None };
    { label = None; op = RET; comment = Some "return" };
  ]

(* Pattern for return-like instruction - using a dummy constant for pattern matching *)
let arm64_ret_pattern has_value =
  {
    pir_pattern = Instructions.Const (Values.ConstInt (0L, Types.I64));  (* Dummy pattern *)
    cost = 1;
    emit = (fun reg_alloc ->
      let instrs = 
        if has_value then
          [{ label = None; op = MOV (x0, make_gpr 0 8); comment = Some "return value" }]
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
    emit = (fun reg_alloc ->
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
    emit = (fun reg_alloc ->
      let cond_reg = make_gpr 0 1 in
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
      { label = None; op = MOV (List.nth arm64_calling_conv.int_arg_regs i, arg_reg); 
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
    | Some dst -> [{ label = None; op = MOV (dst, x0); comment = Some "get result" }]
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
    emit = (fun reg_alloc ->
      (* This is a simplified version - real implementation would handle args *)
      let call_target = make_gpr 0 8 in
      emit_arm64_call "function" [] 
        (if has_result then Some (make_gpr 1 8) else None)
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
    emit = (fun reg_alloc ->
      let dst = make_fpr 0 size in
      let src1 = make_fpr 1 size in
      let src2 = make_fpr 2 size in
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
    emit = (fun reg_alloc ->
      let src1 = make_fpr 0 8 in
      let src2 = make_fpr 1 8 in
      let dst = make_gpr 2 1 in  (* Result is boolean in GPR *)
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
    emit = (fun reg_alloc ->
      let src = make_gpr 0 from_size in
      let dst = make_fpr 1 to_size in
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
    emit = (fun reg_alloc ->
      let src = make_gpr 0 size in
      let dst = make_gpr 1 size in
      match op with
      | Instructions.Clz ->
        [{ label = None; op = MOV (dst, src); comment = Some "clz (count leading zeros)" }]
      | Instructions.Ctz ->
        (* ARM64 doesn't have CTZ, use RBIT + CLZ *)
        let tmp = make_gpr 2 size in
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
    emit = (fun reg_alloc ->
      let src = make_fpr 0 from_size in
      let dst = make_gpr 1 to_size in
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
    emit = (fun reg_alloc ->
      let src = make_gpr 0 from_size in
      let dst = make_fpr 1 to_size in
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
    emit = (fun reg_alloc ->
      let src = make_fpr 0 from_size in
      let dst = make_gpr 1 to_size in
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
    emit = (fun reg_alloc ->
      let src = make_fpr 0 4 in  (* F32 *)
      let dst = make_fpr 1 8 in  (* F64 *)
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
    emit = (fun reg_alloc ->
      let src = make_fpr 0 8 in  (* F64 *)
      let dst = make_fpr 1 4 in  (* F32 *)
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
    emit = (fun reg_alloc ->
      let src = make_gpr 0 8 in
      let dst = make_gpr 1 to_size in
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
    emit = (fun reg_alloc ->
      let src = make_gpr 0 from_size in
      let dst = make_gpr 1 8 in
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
    emit = (fun reg_alloc ->
      let src = make_gpr 0 from_size in
      let dst = make_gpr 1 8 in
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
    emit = (fun reg_alloc ->
      (* This will be handled by materialize_constant *)
      []
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
  
  (* Comparisons *)
  make_arm64_icmp_pattern Instructions.Eq;
  make_arm64_icmp_pattern Instructions.Ne;
  make_arm64_icmp_pattern Instructions.Slt;
  make_arm64_icmp_pattern Instructions.Sle;
  make_arm64_icmp_pattern Instructions.Sgt;
  make_arm64_icmp_pattern Instructions.Sge;
  make_arm64_icmp_pattern Instructions.Ult;
  make_arm64_icmp_pattern Instructions.Ule;
  make_arm64_icmp_pattern Instructions.Ugt;
  make_arm64_icmp_pattern Instructions.Uge;
  
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
      (* Use zero register *)
      [{ label = None; op = MOV (dst, xzr); comment = Some "load zero" }]
    else
      (* ARM64 constant loading strategy:
         - For 16-bit values: single MOVZ
         - For 32-bit values: MOVZ + optional MOVK
         - For 64-bit values: MOVZ + up to 3 MOVK instructions *)
      let instrs = ref [] in
      let add_instr op comment = instrs := !instrs @ [{ label = None; op; comment = Some comment }] in
      
      (* Extract 16-bit chunks from the value *)
      let chunk0 = Int64.logand value 0xFFFFL in
      let chunk1 = Int64.logand (Int64.shift_right_logical value 16) 0xFFFFL in
      let chunk2 = Int64.logand (Int64.shift_right_logical value 32) 0xFFFFL in
      let chunk3 = Int64.logand (Int64.shift_right_logical value 48) 0xFFFFL in
      
      (* Determine if we can use MOVN (bitwise NOT) for better encoding *)
      let use_movn = 
        let inverted = Int64.lognot value in
        let inv_chunks = [
          Int64.logand inverted 0xFFFFL;
          Int64.logand (Int64.shift_right_logical inverted 16) 0xFFFFL;
          Int64.logand (Int64.shift_right_logical inverted 32) 0xFFFFL;
          Int64.logand (Int64.shift_right_logical inverted 48) 0xFFFFL;
        ] in
        (* Count non-zero chunks in original vs inverted *)
        let count_nonzero chunks = List.fold_left (fun acc c -> if c = 0L then acc else acc + 1) 0 chunks in
        count_nonzero inv_chunks < count_nonzero [chunk0; chunk1; chunk2; chunk3]
      in
      
      if use_movn then begin
        (* Use MOVN for efficiency *)
        let inv = Int64.lognot value in
        add_instr (MOV (dst, dst)) (Printf.sprintf "movn 0x%Lx (NOT of 0x%Lx)" (Int64.logand inv 0xFFFFL) value)
      end else begin
        (* Standard MOVZ/MOVK sequence *)
        (* Find first non-zero chunk for MOVZ *)
        if chunk0 <> 0L then
          add_instr (MOV (dst, dst)) (Printf.sprintf "movz 0x%Lx" chunk0)
        else if chunk1 <> 0L then
          add_instr (MOV (dst, dst)) (Printf.sprintf "movz 0x%Lx, lsl 16" chunk1)
        else if chunk2 <> 0L then
          add_instr (MOV (dst, dst)) (Printf.sprintf "movz 0x%Lx, lsl 32" chunk2)
        else
          add_instr (MOV (dst, dst)) (Printf.sprintf "movz 0x%Lx, lsl 48" chunk3);
        
        (* Add MOVK for remaining non-zero chunks *)
        if chunk0 <> 0L && (chunk1 <> 0L || chunk2 <> 0L || chunk3 <> 0L) then
          (); (* chunk0 already loaded with MOVZ *)
        if chunk1 <> 0L && (chunk0 <> 0L || chunk2 <> 0L || chunk3 <> 0L) then
          add_instr (MOV (dst, dst)) (Printf.sprintf "movk 0x%Lx, lsl 16" chunk1);
        if chunk2 <> 0L && (chunk0 <> 0L || chunk1 <> 0L || chunk3 <> 0L) then
          add_instr (MOV (dst, dst)) (Printf.sprintf "movk 0x%Lx, lsl 32" chunk2);
        if chunk3 <> 0L && (chunk0 <> 0L || chunk1 <> 0L || chunk2 <> 0L) then
          add_instr (MOV (dst, dst)) (Printf.sprintf "movk 0x%Lx, lsl 48" chunk3)
      end;
      !instrs
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