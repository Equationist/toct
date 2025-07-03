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
let make_arm64_binop_pattern op =
  let emit reg_alloc =
    let dst = make_gpr 0 8 in
    let src1 = make_gpr 1 8 in
    let src2 = make_gpr 2 8 in
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
    | _ -> failwith "Unsupported binary operation"
  in
  {
    pir_pattern = Instructions.Binop (op, Instructions.NoFlag,
      Values.create_simple_value (Types.Scalar Types.I64),
      Values.create_simple_value (Types.Scalar Types.I64));
    cost = 1;  (* All basic ops are single cycle on ARM64 *)
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

(* Pattern for memory store *)
let arm64_store_pattern =
  {
    pir_pattern = Instructions.Memory (Instructions.Store (
      Values.create_simple_value (Types.Scalar Types.I64),
      Values.create_simple_value Types.Ptr));
    cost = 1;
    emit = (fun reg_alloc ->
      let src = make_gpr 0 8 in
      let addr = make_gpr 1 8 in
      [{ label = None; op = STORE (src, Direct addr, 8); comment = Some "store doubleword" }]
    );
    constraints = [RegClass GPR; Memory];
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

(* All ARM64 patterns *)
let arm64_patterns = [
  (* Integer arithmetic *)
  make_arm64_binop_pattern Instructions.Add;
  make_arm64_binop_pattern Instructions.Sub;
  make_arm64_binop_pattern Instructions.Mul;
  make_arm64_binop_pattern Instructions.And;
  make_arm64_binop_pattern Instructions.Or;
  make_arm64_binop_pattern Instructions.Xor;
  make_arm64_binop_pattern Instructions.Shl;
  make_arm64_binop_pattern Instructions.Lshr;
  make_arm64_binop_pattern Instructions.Ashr;
  
  (* Memory operations *)
  arm64_load_pattern (Types.Scalar Types.I8);
  arm64_load_pattern (Types.Scalar Types.I16);
  arm64_load_pattern (Types.Scalar Types.I32);
  arm64_load_pattern (Types.Scalar Types.I64);
  arm64_store_pattern;
  
  (* Address calculation *)
  arm64_gep_pattern;
]

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

(* Materialize constant into register *)
let materialize_arm64_constant value ty dst =
  match ty with
  | Types.Scalar scalar_ty ->
    if value = 0L then
      (* Use zero register *)
      [{ label = None; op = MOV (dst, xzr); comment = Some "load zero" }]
    else if value > 0L && value <= 0xFFFFL then
      (* MOVZ - move with zero extend *)
      [{ label = None; op = MOV (dst, dst); comment = Some (Printf.sprintf "movz %Ld" value) }]
    else if value = Int64.lognot 0L then
      (* MOVN - move with NOT *)
      [{ label = None; op = MOV (dst, dst); comment = Some "movn 0 (all ones)" }]
    else
      (* For larger constants, ARM64 uses a sequence of MOVZ/MOVK instructions *)
      (* Here we'll just show the concept *)
      [{ label = None; op = MOV (dst, dst); 
         comment = Some (Printf.sprintf "load constant %Ld (may need multiple instructions)" value) }]
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