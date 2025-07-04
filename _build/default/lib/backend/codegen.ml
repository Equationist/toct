(* Main Code Generation Module *)

open Machine
open Compilerkit_pir

(* Code generation pipeline *)
module CodeGenerator (M: MACHINE) = struct
  
  module Selector = Instruction_selection.InstructionSelector(M)
  
  (* SSA destruction - convert from SSA form before register allocation *)
  let destruct_ssa (func: Instructions.func) : Instructions.func =
    (* For now, just return as-is - SSA destruction would insert copies *)
    func
  
  (* Lower PIR to machine code *)
  let lower_function (func: Instructions.func) : machine_instr list * frame_info =
    (* Phase 1: SSA destruction *)
    let func' = destruct_ssa func in
    
    (* Phase 2: Instruction selection *)
    let machine_instrs = Selector.select_function func' in
    
    (* Phase 3: Register allocation *)
    let allocated_instrs, spill_size = Register_allocator.allocate M.config machine_instrs in
    
    (* Phase 4: Compute final frame layout *)
    let frame = 
      let base = 32 in  (* Space for locals *)
      let spills = spill_size in
      let saves = List.length M.config.calling_conv.callee_saved * M.config.ptr_size in
      let total = base + spills + saves in
      (* Align to stack alignment *)
      let aligned_size = 
        (total + M.config.calling_conv.stack_align - 1) / 
        M.config.calling_conv.stack_align * M.config.calling_conv.stack_align in
      {
        frame_size = aligned_size;
        locals_offset = M.config.ptr_size * 2;  (* After saved FP/LR *)
        spill_offset = 32;
        callee_save_offset = 32 + spill_size;
        alignment = M.config.calling_conv.stack_align;
      } in
    
    (allocated_instrs, frame)
  
  (* Assembly output formatting *)
  (* Helper to determine if reg 31 should be sp or xzr in ARM64 *)
  let is_sp_context reg op =
    M.config.word_size = W64 && reg.reg_class = GPR && reg.reg_index = 31 &&
    match op with
    | MOV (dst, src) when dst = reg -> true  (* mov sp, ... *)
    | MOV (dst, src) when src = reg -> true  (* mov ..., sp *)
    | ADD (dst, _, _) when dst = reg -> true (* add sp, ... *)
    | SUB (dst, _, _) when dst = reg -> true (* sub sp, ... *)
    | ADD (_, src1, _) when src1 = reg -> true (* add ..., sp, ... *)
    | SUB (_, src1, _) when src1 = reg -> true (* sub ..., sp, ... *)
    | _ -> false
  
  let format_reg ?(is_addr_mode=false) ?(in_op=None) (r: reg) : string =
    match M.config.word_size, r.reg_class with
    | W64, GPR -> 
      (* On ARM64, register 31 is SP in addressing modes or specific contexts, XZR elsewhere *)
      if r.reg_index = 31 then
        if is_addr_mode then "sp"
        else match in_op with
        | Some op when is_sp_context r op -> "sp"
        | _ -> "xzr"
      else 
        (* Use w0-w30 for 32-bit operations, x0-x30 for 64-bit *)
        if r.reg_size = 4 then Printf.sprintf "w%d" r.reg_index
        else Printf.sprintf "x%d" r.reg_index
    | W32, GPR -> Printf.sprintf "r%d" r.reg_index
    | _, FPR -> Printf.sprintf "d%d" r.reg_index
    | _, VEC -> Printf.sprintf "v%d" r.reg_index
    | _, FLAGS -> "flags"
  
  let format_addr_mode = function
    | Direct r -> Printf.sprintf "[%s]" (format_reg ~is_addr_mode:true r)
    | Offset (r, off) -> Printf.sprintf "[%s, #%Ld]" (format_reg ~is_addr_mode:true r) off
    | Indexed (base, idx, scale) -> 
      Printf.sprintf "[%s, %s, LSL #%d]" (format_reg ~is_addr_mode:true base) (format_reg ~is_addr_mode:true idx) scale
    | PreIndex (r, off) -> Printf.sprintf "[%s, #%Ld]!" (format_reg ~is_addr_mode:true r) off
    | PostIndex (r, off) -> Printf.sprintf "[%s], #%Ld" (format_reg ~is_addr_mode:true r) off
  
  let format_cond = function
    | EQ -> "eq" | NE -> "ne" | LT -> "lt" | LE -> "le"
    | GT -> "gt" | GE -> "ge" | ULT -> "lo" | ULE -> "ls"
    | UGT -> "hi" | UGE -> "hs" | O -> "vs" | NO -> "vc"
  
  (* Need access to format_symbol from generate_module *)
  let format_machine_op ?(format_symbol=(fun x -> x)) op =
    let fmt_reg r = format_reg ~in_op:(Some op) r in
    match op with
    | MOV (dst, src) -> Printf.sprintf "mov %s, %s" (fmt_reg dst) (fmt_reg src)
    | MOV_IMM (dst, imm) -> 
      (* On ARM64, use wzr/xzr for zero instead of immediate *)
      if M.config.word_size = W64 && imm = 0L then
        let zr = if dst.reg_size = 4 then "wzr" else "xzr" in
        Printf.sprintf "mov %s, %s" (fmt_reg dst) zr
      else
        Printf.sprintf "mov %s, #%Ld" (fmt_reg dst) imm
    | LOAD (dst, addr, size) -> 
      let suffix = match size with 1 -> "b" | 2 -> "h" | 4 -> "w" | _ -> "" in
      Printf.sprintf "ldr%s %s, %s" suffix (fmt_reg dst) (format_addr_mode addr)
    | STORE (src, addr, size) ->
      let suffix = match size with 1 -> "b" | 2 -> "h" | 4 -> "w" | _ -> "" in
      Printf.sprintf "str%s %s, %s" suffix (fmt_reg src) (format_addr_mode addr)
    | LEA (dst, addr) -> Printf.sprintf "lea %s, %s" (fmt_reg dst) (format_addr_mode addr)
    | ADD (dst, src1, src2) -> Printf.sprintf "add %s, %s, %s" (fmt_reg dst) (fmt_reg src1) (fmt_reg src2)
    | SUB (dst, src1, src2) -> Printf.sprintf "sub %s, %s, %s" (fmt_reg dst) (fmt_reg src1) (fmt_reg src2)
    | MUL (dst, src1, src2) -> Printf.sprintf "mul %s, %s, %s" (fmt_reg dst) (fmt_reg src1) (fmt_reg src2)
    | AND (dst, src1, src2) -> Printf.sprintf "and %s, %s, %s" (fmt_reg dst) (fmt_reg src1) (fmt_reg src2)
    | OR (dst, src1, src2) -> Printf.sprintf "orr %s, %s, %s" (fmt_reg dst) (fmt_reg src1) (fmt_reg src2)
    | XOR (dst, src1, src2) -> Printf.sprintf "eor %s, %s, %s" (fmt_reg dst) (fmt_reg src1) (fmt_reg src2)
    | CMP (src1, src2) -> Printf.sprintf "cmp %s, %s" (fmt_reg src1) (fmt_reg src2)
    | JMP label -> Printf.sprintf "b %s" label
    | JCC (cond, label) -> Printf.sprintf "b.%s %s" (format_cond cond) label
    | CALL (_, Some name) -> Printf.sprintf "bl %s" (format_symbol name)
    | RET -> "ret"
    | PUSH r -> Printf.sprintf "push %s" (fmt_reg r)
    | POP r -> Printf.sprintf "pop %s" (fmt_reg r)
    | ADJUST_SP off -> 
      if off < 0L then Printf.sprintf "sub sp, sp, #%Ld" (Int64.neg off)
      else Printf.sprintf "add sp, sp, #%Ld" off
    | _ -> "TODO"
  
  let format_machine_instr ?(format_symbol=(fun x -> x)) (instr: machine_instr) : string =
    let label_str = match instr.label with
      | Some lbl -> Printf.sprintf "%s:\n" lbl
      | None -> ""
    in
    let op_str = "\t" ^ format_machine_op ~format_symbol instr.op in
    let comment_str = match instr.comment with
      | Some c -> Printf.sprintf "\t; %s" c
      | None -> ""
    in
    label_str ^ op_str ^ comment_str
  
  (* Generate assembly for a complete module *)
  let generate_module (m: Compilerkit_pir.Module_ir.pir_module) : string =
    let output = Buffer.create 4096 in
    
    (* Platform detection for symbol naming *)
    let is_macos = Sys.os_type = "Unix" && 
      try 
        (* Check OSTYPE environment variable - may start with "darwin" *)
        let ostype = Sys.getenv "OSTYPE" in
        String.length ostype >= 6 && String.sub ostype 0 6 = "darwin"
      with Not_found -> 
        (* For now, assume macOS if OSTYPE is not set and we're on Unix *)
        (* This avoids Unix dependency for tests *)
        true in
    
    (* Symbol name formatting - add underscore prefix on macOS *)
    let format_symbol name =
      if is_macos then "_" ^ name else name in
    
    (* File header *)
    Buffer.add_string output "; Generated by CompilerKit\n";
    Buffer.add_string output (Printf.sprintf "; Target: %s\n" 
      (match M.config.word_size with W32 -> "32-bit" | W64 -> "64-bit"));
    Buffer.add_string output "\n";
    
    (* Add section directive for macOS *)
    if is_macos && M.config.word_size = W64 then
      Buffer.add_string output ".section __TEXT,__text,regular,pure_instructions\n";
    
    (* Process each top-level item *)
    List.iter (function
      | Module_ir.FuncDecl func ->
        (* Generate code for function *)
        let instrs, frame = lower_function func in
        
        (* Function header - use .globl instead of .global *)
        Buffer.add_string output (Printf.sprintf "\n.globl %s\n" (format_symbol func.name));
        if is_macos && M.config.word_size = W64 then
          Buffer.add_string output ".p2align 2\n";
        Buffer.add_string output (Printf.sprintf "%s:\n" (format_symbol func.name));
        if is_macos && M.config.word_size = W64 then
          Buffer.add_string output ".cfi_startproc\n";
        
        (* Emit instructions *)
        List.iter (fun instr ->
          Buffer.add_string output (format_machine_instr ~format_symbol instr);
          Buffer.add_char output '\n'
        ) instrs;
        
        (* Add .cfi_endproc after function *)
        if is_macos && M.config.word_size = W64 then
          Buffer.add_string output ".cfi_endproc\n";
        
      | Module_ir.ObjectDecl obj ->
        (* Global variable *)
        Buffer.add_string output (Printf.sprintf "\n.data\n");
        Buffer.add_string output (Printf.sprintf ".globl %s\n" (format_symbol obj.obj_name));
        Buffer.add_string output (Printf.sprintf "%s:\n" (format_symbol obj.obj_name));
        (* TODO: Emit initializer *)
        Buffer.add_string output "\t.quad 0\n";
        
      | Module_ir.TypeDecl _ ->
        (* Type declarations don't generate code *)
        ()
    ) m.items;
    
    Buffer.contents output
end

(* Convenience functions for different targets *)
module X64CodeGen = CodeGenerator(X86_64_backend.X64Backend)
module ARM64CodeGen = CodeGenerator(Arm64_backend.ARM64Backend)

(* Target selection *)
type target = X64 | ARM64 | ARM32 | RISCV64

let generate_for_target target m =
  match target with
  | X64 -> X64CodeGen.generate_module m
  | ARM64 -> ARM64CodeGen.generate_module m
  | _ -> failwith "Target not implemented yet"

(* Generate object file *)
let generate_object_file ~target ~filename m =
  match target with
  | ARM64 -> Macho_writer.write_macho_file filename m
  | _ -> failwith "Object file generation not implemented for this target"