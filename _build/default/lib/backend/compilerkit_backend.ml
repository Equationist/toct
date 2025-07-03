(* Backend optimization passes *)

(* SSA Transformation *)
module Dominance = Dominance
module Ssa_block_params = Ssa_block_params
module Ssa_verify = Ssa_verify

(* Code Generation *)
module Machine = Machine
module InstructionSelection = Instruction_selection
module RegisterAllocator = Register_allocator
module X86_64Backend = X86_64_backend
module ARM64Backend = Arm64_backend
module Codegen = Codegen

(* Export main transformation function *)
let transform_to_ssa = Ssa_block_params.transform_to_ssa

(* Export verification *)
let verify_ssa = Ssa_verify.verify_ssa

(* Export code generation *)
let generate_x64 = Codegen.generate_for_target Codegen.X64
let generate_arm64 = Codegen.generate_for_target Codegen.ARM64