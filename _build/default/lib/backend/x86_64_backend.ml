(* x86_64 Backend Implementation *)

open Machine
open Compilerkit_pir

(* x86_64 specific registers *)
let rax = make_gpr 0 8   let rcx = make_gpr 1 8   let rdx = make_gpr 2 8   let rbx = make_gpr 3 8
let rsp = make_gpr 4 8   let rbp = make_gpr 5 8   let rsi = make_gpr 6 8   let rdi = make_gpr 7 8
let r8  = make_gpr 8 8   let r9  = make_gpr 9 8   let r10 = make_gpr 10 8  let r11 = make_gpr 11 8
let r12 = make_gpr 12 8  let r13 = make_gpr 13 8  let r14 = make_gpr 14 8  let r15 = make_gpr 15 8

(* x86_64 calling convention (System V AMD64 ABI) *)
let x64_calling_conv = {
  int_arg_regs = [rdi; rsi; rdx; rcx; r8; r9];
  float_arg_regs = List.init 8 (fun i -> make_fpr i 8);  (* xmm0-xmm7 *)
  int_ret_regs = [rax; rdx];
  float_ret_regs = [make_fpr 0 8; make_fpr 1 8];  (* xmm0, xmm1 *)
  callee_saved = [rbx; rbp; r12; r13; r14; r15];
  caller_saved = [rax; rcx; rdx; rsi; rdi; r8; r9; r10; r11];
  stack_align = 16;
  red_zone = 128;
  param_offset = 16;  (* After return address and saved RBP *)
}

(* x86_64 machine configuration *)
let x64_config = {
  word_size = W64;
  ptr_size = 8;
  big_endian = false;
  available_regs = [
    (GPR, 16);  (* RAX through R15 *)
    (FPR, 16);  (* XMM0 through XMM15 *)
    (VEC, 16);  (* YMM0 through YMM15 for AVX *)
    (FLAGS, 1); (* RFLAGS *)
  ];
  calling_conv = x64_calling_conv;
  has_indexed_addr = true;   (* [base + index*scale + disp] *)
  has_autoinc_addr = false;  (* No auto-increment on x86 *)
  code_align = 16;
  data_align = 8;
}

(* Helper to emit MOV for different sizes *)
let emit_mov_sized dst src size =
  match size with
  | 1 -> MOV (dst, src)  (* movb *)
  | 2 -> MOV (dst, src)  (* movw *)
  | 4 -> MOV (dst, src)  (* movl *)
  | 8 -> MOV (dst, src)  (* movq *)
  | _ -> failwith (Printf.sprintf "Unsupported move size: %d" size)

(* Pattern for integer binary operations *)
let make_x64_binop_pattern op =
  let emit reg_alloc =
    (* In a real implementation, reg_alloc would map PIR values to registers *)
    let dst = make_gpr 0 8 in
    let src1 = make_gpr 1 8 in
    let src2 = make_gpr 2 8 in
    match op with
    | Instructions.Add -> 
      [{ label = None; op = MOV (dst, src1); comment = Some "move first operand" };
       { label = None; op = ADD (dst, dst, src2); comment = Some "add" }]
    | Instructions.Sub ->
      [{ label = None; op = MOV (dst, src1); comment = Some "move first operand" };
       { label = None; op = SUB (dst, dst, src2); comment = Some "subtract" }]
    | Instructions.Mul ->
      [{ label = None; op = MOV (rax, src1); comment = Some "move to RAX for multiply" };
       { label = None; op = MUL (rdx, rax, src2); comment = Some "multiply (result in RDX:RAX)" };
       { label = None; op = MOV (dst, rax); comment = Some "move result" }]
    | Instructions.And ->
      [{ label = None; op = MOV (dst, src1); comment = Some "move first operand" };
       { label = None; op = AND (dst, dst, src2); comment = Some "bitwise and" }]
    | Instructions.Or ->
      [{ label = None; op = MOV (dst, src1); comment = Some "move first operand" };
       { label = None; op = OR (dst, dst, src2); comment = Some "bitwise or" }]
    | Instructions.Xor ->
      [{ label = None; op = MOV (dst, src1); comment = Some "move first operand" };
       { label = None; op = XOR (dst, dst, src2); comment = Some "bitwise xor" }]
    | _ -> failwith "Unsupported binary operation"
  in
  {
    pir_pattern = Instructions.Binop (op, Instructions.NoFlag,
      Values.create_simple_value (Types.Scalar Types.I64),
      Values.create_simple_value (Types.Scalar Types.I64));
    cost = (match op with Instructions.Mul -> 3 | _ -> 1);
    emit;
    constraints = [RegClass GPR; RegClass GPR; RegClass GPR];
    hints = (match op with
      | Instructions.Mul -> [SpecificReg rax; SameAs 0]  (* Result in RAX *)
      | _ -> [SameAs 0]);  (* Reuse first operand register *)
  }

(* Pattern for memory load *)
let x64_load_pattern ty =
  let size = match ty with
    | Types.Scalar s -> Types.size_of_scalar s
    | _ -> 8  (* Default to 8 bytes *)
  in
  {
    pir_pattern = Instructions.Memory (Instructions.Load ty);
    cost = 1;
    emit = (fun reg_alloc ->
      let dst = make_gpr 0 size in
      let addr = make_gpr 1 8 in
      [{ label = None; op = LOAD (dst, Direct addr, size); comment = Some "load from memory" }]
    );
    constraints = [RegClass GPR; Memory];
    hints = [];
  }

(* Pattern for memory store *)
let x64_store_pattern =
  {
    pir_pattern = Instructions.Memory (Instructions.Store (
      Values.create_simple_value (Types.Scalar Types.I64),
      Values.create_simple_value Types.Ptr));
    cost = 1;
    emit = (fun reg_alloc ->
      let src = make_gpr 0 8 in
      let addr = make_gpr 1 8 in
      [{ label = None; op = STORE (src, Direct addr, 8); comment = Some "store to memory" }]
    );
    constraints = [RegClass GPR; Memory];
    hints = [];
  }

(* All x86_64 patterns *)
let x64_patterns = [
  (* Integer arithmetic *)
  make_x64_binop_pattern Instructions.Add;
  make_x64_binop_pattern Instructions.Sub;
  make_x64_binop_pattern Instructions.Mul;
  make_x64_binop_pattern Instructions.And;
  make_x64_binop_pattern Instructions.Or;
  make_x64_binop_pattern Instructions.Xor;
  
  (* Memory operations *)
  x64_load_pattern (Types.Scalar Types.I8);
  x64_load_pattern (Types.Scalar Types.I16);
  x64_load_pattern (Types.Scalar Types.I32);
  x64_load_pattern (Types.Scalar Types.I64);
  x64_store_pattern;
  
  (* TODO: Add more patterns *)
]

(* Emit function prologue *)
let emit_x64_prologue frame =
  [
    { label = None; op = PUSH rbp; comment = Some "save frame pointer" };
    { label = None; op = MOV (rbp, rsp); comment = Some "set up frame pointer" };
    { label = None; op = ADJUST_SP (Int64.neg (Int64.of_int frame.frame_size)); 
      comment = Some (Printf.sprintf "allocate %d bytes" frame.frame_size) };
  ]

(* Emit function epilogue *)
let emit_x64_epilogue frame =
  [
    { label = None; op = MOV (rsp, rbp); comment = Some "restore stack pointer" };
    { label = None; op = POP rbp; comment = Some "restore frame pointer" };
    { label = None; op = RET; comment = Some "return" };
  ]

(* Emit function call *)
let emit_x64_call name arg_regs result_reg =
  (* Move arguments to calling convention registers *)
  let arg_moves = List.mapi (fun i arg_reg ->
    if i < 6 then
      { label = None; op = MOV (List.nth x64_calling_conv.int_arg_regs i, arg_reg); 
        comment = Some (Printf.sprintf "arg %d" i) }
    else
      { label = None; op = PUSH arg_reg; comment = Some (Printf.sprintf "arg %d on stack" i) }
  ) arg_regs in
  
  (* Call instruction *)
  let call_instr = { label = None; op = CALL (None, Some name); comment = Some "call function" } in
  
  (* Get result if needed *)
  let result_move = match result_reg with
    | Some dst -> [{ label = None; op = MOV (dst, rax); comment = Some "get result" }]
    | None -> []
  in
  
  (* Adjust stack if we pushed arguments *)
  let stack_adjust = 
    let pushed = max 0 (List.length arg_regs - 6) in
    if pushed > 0 then
      [{ label = None; op = ADJUST_SP (Int64.of_int (pushed * 8)); 
         comment = Some "remove pushed arguments" }]
    else []
  in
  
  arg_moves @ [call_instr] @ result_move @ stack_adjust

(* Materialize constant into register *)
let materialize_x64_constant value ty dst =
  match ty with
  | Types.Scalar _ ->
    if value = 0L then
      (* Optimize: xor reg, reg to zero *)
      [{ label = None; op = XOR (dst, dst, dst); comment = Some "zero register" }]
    else if Int64.abs value <= 0x7FFFFFFFL then
      (* Fits in 32 bits - use mov *)
      [{ label = None; op = MOV (dst, dst); comment = Some (Printf.sprintf "load constant %Ld" value) }]
    else
      (* Need full 64-bit constant - would use movabs on x64 *)
      [{ label = None; op = MOV (dst, dst); comment = Some (Printf.sprintf "load constant %Ld" value) }]
  | _ -> failwith "Can only materialize scalar constants"

(* x86_64 backend module *)
module X64Backend : MACHINE = struct
  let config = x64_config
  let patterns = x64_patterns
  let emit_prologue = emit_x64_prologue
  let emit_epilogue = emit_x64_epilogue
  let emit_call = emit_x64_call
  let materialize_constant = materialize_x64_constant
end