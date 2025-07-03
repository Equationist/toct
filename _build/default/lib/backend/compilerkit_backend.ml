(* Backend optimization passes *)

(* SSA Transformation *)
module Dominance = Dominance
module Ssa_transform = Ssa_transform
module Ssa_block_params = Ssa_block_params
module Ssa_verify = Ssa_verify

(* Export main transformation function *)
let transform_to_ssa = Ssa_block_params.transform_to_ssa

(* Export verification *)
let verify_ssa = Ssa_verify.verify_ssa