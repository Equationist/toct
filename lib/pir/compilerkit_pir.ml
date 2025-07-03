(* Portable Intermediate Representation *)

(* Export all PIR modules *)
module Types = Types
module Attributes = Attributes  
module Values = Values
module Instructions = Instructions
module Builder = Builder
module Pretty_printer = Pretty_printer

(* Re-export commonly used types *)
type ty = Types.ty
type scalar_ty = Types.scalar_ty
type value = Values.value
type const_value = Values.const_value
type instr = Instructions.instr
type instruction = Instructions.instruction
type basic_block = Instructions.basic_block
type func = Instructions.func
type terminator = Instructions.terminator

(* Main PIR module interface *)
let version = "0.1"

(* Convenience functions *)
let create_value = Values.create_value
let create_simple_value = Values.create_simple_value
let create_instruction = Instructions.create_instruction
let create_simple_instruction = Instructions.create_simple_instruction