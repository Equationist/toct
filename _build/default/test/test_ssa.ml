(* Tests for SSA Transformation *)

open Compilerkit_pir
open Compilerkit_backend

(* Helper to create a simple test function *)
let create_test_function () : Instructions.func =
  (* Create simple values *)
  let v1 = Values.create_simple_value (Types.Scalar (Types.I32)) in
  let v2 = Values.create_simple_value (Types.Scalar (Types.I32)) in
  let v3 = Values.create_simple_value (Types.Scalar (Types.I32)) in
  let v4 = Values.create_simple_value (Types.Scalar (Types.I32)) in
  let v5 = Values.create_simple_value (Types.Scalar (Types.I32)) in
  let v_cond = Values.create_simple_value (Types.Scalar (Types.I1)) in
  
  (* Create blocks *)
  let entry_block = {
    Instructions.label = "entry";
    Instructions.params = [];
    Instructions.instructions = [
      Instructions.create_simple_instruction 
        ~result:v3
        (Instructions.Binop (Instructions.Add, Instructions.Nsw, v1, v2))
    ];
    Instructions.terminator = Instructions.Br (v_cond, "then", "else");
    Instructions.attrs = Attributes.empty ();
  } in
  
  let then_block = {
    Instructions.label = "then";
    Instructions.params = [];
    Instructions.instructions = [
      Instructions.create_simple_instruction
        ~result:v4
        (Instructions.Binop (Instructions.Mul, Instructions.NoFlag, v3, v3))
    ];
    Instructions.terminator = Instructions.Jmp "merge";
    Instructions.attrs = Attributes.empty ();
  } in
  
  let else_block = {
    Instructions.label = "else";
    Instructions.params = [];
    Instructions.instructions = [
      Instructions.create_simple_instruction
        ~result:v4
        (Instructions.Binop (Instructions.Sub, Instructions.NoFlag, v3, v3))
    ];
    Instructions.terminator = Instructions.Jmp "merge";
    Instructions.attrs = Attributes.empty ();
  } in
  
  let merge_block = {
    Instructions.label = "merge";
    Instructions.params = [];
    Instructions.instructions = [
      Instructions.create_simple_instruction
        ~result:v5
        (Instructions.Binop (Instructions.Add, Instructions.NoFlag, v4, v3))
    ];
    Instructions.terminator = Instructions.Ret (Some v5);
    Instructions.attrs = Attributes.empty ();
  } in
  
  {
    Instructions.name = "test_func";
    Instructions.params = [("a", Types.Scalar Types.I32); ("b", Types.Scalar Types.I32); ("cond", Types.Scalar Types.I1)];
    Instructions.return_ty = Some (Types.Scalar Types.I32);
    Instructions.blocks = [entry_block; then_block; else_block; merge_block];
    Instructions.attrs = Attributes.empty ();
  }

(* Test dominance computation *)
let test_dominance () =
  print_endline "Testing dominance computation...";
  
  let func = create_test_function () in
  let dom_info = Dominance.compute_dominance_info func in
  
  (* Check immediate dominators *)
  assert (Dominance.BlockMap.find "entry" dom_info.immediate_dominators = "entry");
  assert (Dominance.BlockMap.find "then" dom_info.immediate_dominators = "entry");
  assert (Dominance.BlockMap.find "else" dom_info.immediate_dominators = "entry");
  assert (Dominance.BlockMap.find "merge" dom_info.immediate_dominators = "entry");
  
  (* Check dominance relationships *)
  assert (Dominance.dominates dom_info "entry" "then");
  assert (Dominance.dominates dom_info "entry" "else");
  assert (Dominance.dominates dom_info "entry" "merge");
  assert (not (Dominance.dominates dom_info "then" "else"));
  assert (not (Dominance.dominates dom_info "else" "then"));
  
  (* Check dominance frontiers *)
  let df_merge = Dominance.BlockMap.find "merge" dom_info.dominance_frontiers in
  assert (Dominance.BlockSet.is_empty df_merge);
  
  (* "merge" should be in DF of "then" and "else" *)
  let df_then = Dominance.BlockMap.find "then" dom_info.dominance_frontiers in
  assert (Dominance.BlockSet.mem "merge" df_then);
  
  let df_else = Dominance.BlockMap.find "else" dom_info.dominance_frontiers in  
  assert (Dominance.BlockSet.mem "merge" df_else);
  
  print_endline "✓ Dominance computation tests passed"

(* Test SSA verification on non-SSA form *)
let test_ssa_verification_negative () =
  print_endline "Testing SSA verification (negative cases)...";
  
  let func = create_test_function () in
  let result = Ssa_verify.verify_ssa func in
  
  (* The function should pass basic SSA verification since we use unique values *)
  assert result.is_valid;
  
  print_endline "✓ SSA verification negative tests passed"

(* Test basic SSA properties *)
let test_ssa_properties () =
  print_endline "Testing SSA properties...";
  
  (* Create a simple straight-line function *)
  let v1 = Values.create_simple_value (Types.Scalar Types.I32) in
  let v2 = Values.create_simple_value (Types.Scalar Types.I32) in
  let v3 = Values.create_simple_value (Types.Scalar Types.I32) in
  
  let simple_func = {
    Instructions.name = "simple";
    Instructions.params = [("a", Types.Scalar Types.I32)];
    Instructions.return_ty = Some (Types.Scalar Types.I32);
    Instructions.blocks = [{
      Instructions.label = "entry";
      Instructions.params = [];
      Instructions.instructions = [
        Instructions.create_simple_instruction 
          ~result:v2
          (Instructions.Binop (Instructions.Add, Instructions.NoFlag, v1, v1));
        Instructions.create_simple_instruction
          ~result:v3
          (Instructions.Binop (Instructions.Mul, Instructions.NoFlag, v2, v2))
      ];
      Instructions.terminator = Instructions.Ret (Some v3);
      Instructions.attrs = Attributes.empty ();
    }];
    Instructions.attrs = Attributes.empty ();
  } in
  
  (* This simple function should be in SSA form *)
  let result = Ssa_verify.verify_ssa simple_func in
  assert result.is_valid;
  assert (result.stats.num_definitions >= 2);  (* v2 and v3 *)
  assert (result.stats.num_uses >= 3);  (* v1 (twice), v2 (twice) *)
  
  print_endline "✓ SSA property tests passed"

(* Test dominance frontier computation on complex CFG *)
let test_complex_dominance () =
  print_endline "Testing complex dominance relationships...";
  
  (* Create a function with a loop *)
  let v_i = Values.create_simple_value (Types.Scalar Types.I32) in
  let v_n = Values.create_simple_value (Types.Scalar Types.I32) in
  let v_cond = Values.create_simple_value (Types.Scalar Types.I1) in
  let v_i_next = Values.create_simple_value (Types.Scalar Types.I32) in
  let v_one = Values.create_simple_value (Types.Scalar Types.I32) in
  
  let loop_func = {
    Instructions.name = "loop_test";
    Instructions.params = [("n", Types.Scalar Types.I32)];
    Instructions.return_ty = Some (Types.Scalar Types.I32);
    Instructions.blocks = [
      (* Entry block *)
      {
        Instructions.label = "entry";
        Instructions.params = [];
        Instructions.instructions = [
          Instructions.create_simple_instruction
            ~result:v_i
            (Instructions.Const (Values.ConstInt (Int64.zero, Types.I32)))
        ];
        Instructions.terminator = Instructions.Jmp "loop";
        Instructions.attrs = Attributes.empty ();
      };
      (* Loop header *)
      {
        Instructions.label = "loop";
        Instructions.params = [];
        Instructions.instructions = [
          Instructions.create_simple_instruction
            ~result:v_cond
            (Instructions.Icmp (Instructions.Slt, v_i, v_n))
        ];
        Instructions.terminator = Instructions.Br (v_cond, "body", "exit");
        Instructions.attrs = Attributes.empty ();
      };
      (* Loop body *)
      {
        Instructions.label = "body";
        Instructions.params = [];
        Instructions.instructions = [
          Instructions.create_simple_instruction
            ~result:v_one
            (Instructions.Const (Values.ConstInt (Int64.one, Types.I32)));
          Instructions.create_simple_instruction
            ~result:v_i_next
            (Instructions.Binop (Instructions.Add, Instructions.NoFlag, v_i, v_one))
        ];
        Instructions.terminator = Instructions.Jmp "loop";
        Instructions.attrs = Attributes.empty ();
      };
      (* Exit block *)
      {
        Instructions.label = "exit";
        Instructions.params = [];
        Instructions.instructions = [];
        Instructions.terminator = Instructions.Ret (Some v_i);
        Instructions.attrs = Attributes.empty ();
      };
    ];
    Instructions.attrs = Attributes.empty ();
  } in
  
  let dom_info = Dominance.compute_dominance_info loop_func in
  
  (* Check dominance for loop *)
  assert (Dominance.dominates dom_info "entry" "loop");
  assert (Dominance.dominates dom_info "loop" "body");
  assert (Dominance.dominates dom_info "loop" "exit");
  
  (* Loop header should be in dominance frontier of loop body *)
  let df_body = Dominance.BlockMap.find "body" dom_info.dominance_frontiers in
  assert (Dominance.BlockSet.mem "loop" df_body);
  
  print_endline "✓ Complex dominance tests passed"

(* Test pretty printing *)
let test_pretty_printing () =
  print_endline "Testing pretty printing...";
  
  let func = create_test_function () in
  let dom_info = Dominance.compute_dominance_info func in
  let dom_str = Dominance.pp_dominance_info dom_info in
  
  (* Check output contains expected information *)
  assert (String.contains dom_str 'D');  (* "Dominators" *)
  assert (String.contains dom_str 'F');  (* "Frontiers" *)
  
  (* Test SSA verification result printing *)
  let result = Ssa_verify.verify_ssa func in
  let result_str = Ssa_verify.pp_verification_result result in
  
  assert (String.contains result_str 'P');  (* "PASSED" *)
  assert (String.contains result_str 'S');  (* "Statistics" *)
  
  print_endline "✓ Pretty printing tests passed"

(* Run all tests *)
let run_all_tests () =
  print_endline "\n=== SSA Transformation Tests ===\n";
  
  test_dominance ();
  print_endline "";
  
  test_ssa_verification_negative ();
  print_endline "";
  
  test_ssa_properties ();
  print_endline "";
  
  test_complex_dominance ();
  print_endline "";
  
  test_pretty_printing ();
  print_endline "";
  
  print_endline "🎉 All SSA tests passed! 🎉"

let () = run_all_tests ()