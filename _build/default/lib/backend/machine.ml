(* Abstract Machine Interface - Portable across architectures *)

(* Machine word size *)
type word_size = 
  | W32  (* 32-bit *)
  | W64  (* 64-bit *)

(* Register classes *)
type reg_class = 
  | GPR   (* General purpose registers *)
  | FPR   (* Floating point registers *)
  | VEC   (* Vector registers *)
  | FLAGS (* Condition/flag registers *)

(* Abstract register - architecture will map to concrete registers *)
type reg = {
  reg_class: reg_class;
  reg_index: int;  (* Virtual register number *)
  reg_size: int;   (* Size in bytes: 1, 2, 4, 8, 16, 32 *)
}

(* Memory addressing modes *)
type addr_mode = 
  | Direct of reg                        (* [reg] *)
  | Offset of reg * int64                (* [reg + offset] *)
  | Indexed of reg * reg * int          (* [base + index * scale] *)
  | PreIndex of reg * int64              (* [reg + offset]! *)
  | PostIndex of reg * int64             (* [reg], offset *)

(* Condition codes - portable subset *)
type cond = 
  | EQ | NE           (* Equal, Not equal *)
  | LT | LE | GT | GE (* Signed comparisons *)
  | ULT | ULE | UGT | UGE (* Unsigned comparisons *)
  | O | NO            (* Overflow, No overflow *)

(* Abstract machine operations *)
type machine_op = 
  (* Data movement *)
  | MOV of reg * reg                     (* mov dst, src *)
  | MOV_IMM of reg * int64               (* mov dst, #imm *)
  | LOAD of reg * addr_mode * int        (* load dst, [addr], size *)
  | STORE of reg * addr_mode * int       (* store src, [addr], size *)
  | LEA of reg * addr_mode               (* load effective address *)
  
  (* Arithmetic *)
  | ADD of reg * reg * reg               (* dst = src1 + src2 *)
  | SUB of reg * reg * reg               (* dst = src1 - src2 *)
  | MUL of reg * reg * reg               (* dst = src1 * src2 *)
  | DIV of reg * reg * reg               (* dst = src1 / src2 (signed) *)
  | UDIV of reg * reg * reg              (* dst = src1 / src2 (unsigned) *)
  | MOD of reg * reg * reg               (* dst = src1 % src2 (signed) *)
  | UMOD of reg * reg * reg              (* dst = src1 % src2 (unsigned) *)
  | NEG of reg * reg                     (* dst = -src *)
  
  (* Bitwise *)
  | AND of reg * reg * reg               (* dst = src1 & src2 *)
  | OR of reg * reg * reg                (* dst = src1 | src2 *)
  | XOR of reg * reg * reg               (* dst = src1 ^ src2 *)
  | NOT of reg * reg                     (* dst = ~src *)
  | SHL of reg * reg * reg               (* dst = src1 << src2 *)
  | SHR of reg * reg * reg               (* dst = src1 >> src2 (logical) *)
  | SAR of reg * reg * reg               (* dst = src1 >> src2 (arithmetic) *)
  
  (* Comparison *)
  | CMP of reg * reg                     (* Compare and set flags *)
  | TEST of reg * reg                    (* Bitwise AND and set flags *)
  
  (* Control flow *)
  | JMP of string                        (* Unconditional jump *)
  | JCC of cond * string                 (* Conditional jump *)
  | CALL of reg option * string option   (* Call function *)
  | RET                                  (* Return *)
  
  (* Stack operations *)
  | PUSH of reg                          (* Push register *)
  | POP of reg                           (* Pop register *)
  | ADJUST_SP of int64                   (* Adjust stack pointer *)
  
  (* Floating point *)
  | FMOV of reg * reg                    (* FP move *)
  | FADD of reg * reg * reg              (* FP add *)
  | FSUB of reg * reg * reg              (* FP subtract *)
  | FMUL of reg * reg * reg              (* FP multiply *)
  | FDIV of reg * reg * reg              (* FP divide *)
  | FCMP of reg * reg                    (* FP compare *)
  
  (* Conversions *)
  | CVT_I2F of reg * reg * int * int     (* Int to float: dst, src, int_size, fp_size *)
  | CVT_F2I of reg * reg * int * int     (* Float to int: dst, src, fp_size, int_size *)
  | CVT_F2F of reg * reg * int * int     (* Float to float: dst, src, from_size, to_size *)
  | EXT of reg * reg * int * bool        (* Sign/zero extend: dst, src, from_size, is_signed *)

(* Machine instruction with optional label *)
type machine_instr = {
  label: string option;
  op: machine_op;
  comment: string option;
}

(* Calling convention abstraction *)
type calling_conv = {
  (* Integer argument registers *)
  int_arg_regs: reg list;
  (* Float argument registers *)
  float_arg_regs: reg list;
  (* Integer return registers *)
  int_ret_regs: reg list;
  (* Float return registers *)
  float_ret_regs: reg list;
  (* Callee-saved registers *)
  callee_saved: reg list;
  (* Caller-saved registers *)
  caller_saved: reg list;
  (* Stack alignment requirement *)
  stack_align: int;
  (* Red zone size (if any) *)
  red_zone: int;
  (* Parameter passing area offset *)
  param_offset: int;
}

(* Frame layout information *)
type frame_info = {
  (* Total frame size *)
  frame_size: int;
  (* Offset to local variables *)
  locals_offset: int;
  (* Offset to spill slots *)
  spill_offset: int;
  (* Offset to callee-saved registers *)
  callee_save_offset: int;
  (* Alignment requirement *)
  alignment: int;
}

(* Target machine configuration *)
type machine_config = {
  (* Word size *)
  word_size: word_size;
  (* Pointer size in bytes *)
  ptr_size: int;
  (* Endianness *)
  big_endian: bool;
  (* Available registers by class *)
  available_regs: (reg_class * int) list;
  (* Calling convention *)
  calling_conv: calling_conv;
  (* Can use indexed addressing? *)
  has_indexed_addr: bool;
  (* Can use pre/post increment addressing? *)
  has_autoinc_addr: bool;
  (* Preferred alignment for code *)
  code_align: int;
  (* Preferred alignment for data *)
  data_align: int;
}

(* Helper functions *)
let word_size_bytes = function
  | W32 -> 4
  | W64 -> 8

let make_gpr idx size = 
  { reg_class = GPR; reg_index = idx; reg_size = size }

let make_fpr idx size = 
  { reg_class = FPR; reg_index = idx; reg_size = size }

let make_vec idx size = 
  { reg_class = VEC; reg_index = idx; reg_size = size }

(* Register allocation hints *)
type alloc_hint = 
  | PreferReg of reg
  | AvoidReg of reg
  | SpecificReg of reg  (* Must use this specific register *)
  | SameAs of int  (* Same register as operand N *)
  | Paired of int   (* Consecutive registers with operand N *)

(* Operand constraints for instruction selection *)
type operand_constraint = 
  | RegClass of reg_class
  | SpecificReg of reg
  | Memory
  | Immediate of int64 option * int64 option  (* min, max *)
  | RegOrImm of int64 option * int64 option
  | RegOrMem

(* Instruction pattern for BURS *)
type pattern = {
  (* Pattern to match *)
  pir_pattern: Compilerkit_pir.Instructions.instr;
  (* Cost estimate *)
  cost: int;
  (* Machine instructions to emit *)
  emit: (Compilerkit_pir.Values.value -> reg) -> machine_instr list;
  (* Operand constraints *)
  constraints: operand_constraint list;
  (* Allocation hints *)
  hints: alloc_hint list;
}

(* Abstract machine interface *)
module type MACHINE = sig
  val config : machine_config
  val patterns : pattern list
  val emit_prologue : frame_info -> machine_instr list
  val emit_epilogue : frame_info -> machine_instr list
  val emit_call : string -> reg list -> reg option -> machine_instr list
  val materialize_constant : int64 -> Compilerkit_pir.Types.ty -> reg -> machine_instr list
end