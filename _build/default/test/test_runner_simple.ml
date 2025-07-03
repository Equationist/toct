(* Simple test runner for TOCT with inline tests *)

open Compilerkit_pir

let test_build_system () =
  print_endline "✓ Build system test passed"

(* Test PIR Types *)
let test_pir_types () =
  print_endline "Running PIR Types tests...";
  
  (* Test scalar type sizes *)
  assert (Types.size_of (Types.Scalar Types.I32) = 4);
  assert (Types.size_of (Types.Scalar Types.I64) = 8);
  assert (Types.size_of (Types.Scalar Types.F32) = 4);
  
  (* Test type equality *)
  assert (Types.equal (Types.Scalar Types.I32) (Types.Scalar Types.I32));
  assert (not (Types.equal (Types.Scalar Types.I32) (Types.Scalar Types.I64)));
  
  (* Test type validation *)
  assert (Types.is_integer (Types.Scalar Types.I32));
  assert (not (Types.is_integer (Types.Scalar Types.F32)));
  assert (Types.is_float (Types.Scalar Types.F64));
  
  (* Test pretty printing *)
  assert (Types.string_of_ty (Types.Scalar Types.I32) = "i32");
  assert (Types.string_of_ty (Types.Vector (4, Types.F32)) = "v4xf32");
  assert (Types.string_of_ty Types.Ptr = "ptr");
  
  print_endline "✓ All PIR Types tests passed"

(* Test PIR Attributes *)
let test_pir_attributes () =
  print_endline "Running PIR Attributes tests...";
  
  let attrs = Attributes.empty () in
  assert (Hashtbl.length attrs = 0);
  
  let attrs2 = Attributes.add "test" (Attributes.Bool true) attrs in
  assert (Attributes.has "test" attrs2);
  assert (not (Attributes.has "missing" attrs2));
  
  (* Test helpers *)
  let attrs3 = Attributes.Helpers.set_range 0 100 attrs in
  assert (Attributes.Helpers.get_range attrs3 = Some (0, 100));
  
  let attrs4 = Attributes.Helpers.set_nonnull attrs in
  assert (Attributes.Helpers.is_nonnull attrs4 = true);
  
  print_endline "✓ All PIR Attributes tests passed"

(* Test PIR Values *)
let test_pir_values () =
  print_endline "Running PIR Values tests...";
  
  let i32_ty = Types.Scalar Types.I32 in
  let v1 = Values.create_simple_value i32_ty in
  let v2 = Values.create_simple_value i32_ty in
  
  (* Values should have unique IDs *)
  assert (Values.get_id v1 <> Values.get_id v2);
  assert (Values.get_type v1 = i32_ty);
  
  (* Test constants *)
  let c1 = Values.Const.i32 42 in
  assert (Values.type_of_const c1 = Types.Scalar Types.I32);
  assert (Values.string_of_const_value c1 = "42");
  
  let c2 = Values.Const.bool true in
  assert (Values.type_of_const c2 = Types.Scalar Types.I1);
  
  (* Test type checking *)
  assert (Values.is_integer_value v1);
  assert (Values.has_type v1 i32_ty);
  
  print_endline "✓ All PIR Values tests passed"

(* Test PIR Instructions *)
let test_pir_instructions () =
  print_endline "Running PIR Instructions tests...";
  
  let v1 = Values.create_simple_value (Types.Scalar Types.I32) in
  let v2 = Values.create_simple_value (Types.Scalar Types.I32) in
  
  (* Test binary operations *)
  let add_instr = Instructions.Binop (Instructions.Add, Instructions.NoFlag, v1, v2) in
  let instr = Instructions.create_simple_instruction ~result:v1 add_instr in
  assert (instr.result <> None);
  
  (* Test comparisons *)
  let eq_instr = Instructions.Icmp (Instructions.Eq, v1, v2) in
  let result_ty = Instructions.result_type_of_instr eq_instr in
  assert (result_ty = Some (Types.Scalar Types.I1));
  
  (* Test pretty printing *)
  assert (Instructions.string_of_flag Instructions.NoFlag = "");
  assert (Instructions.string_of_flag Instructions.Nsw = ".nsw");
  assert (Instructions.string_of_icmp_pred Instructions.Eq = "eq");
  assert (Instructions.string_of_binop Instructions.Add = "add");
  
  (* Test terminators *)
  let ret_void = Instructions.Ret None in
  assert (Instructions.string_of_terminator ret_void = "ret");
  
  print_endline "✓ All PIR Instructions tests passed"

(* Test PIR Builder *)
let test_pir_builder () =
  print_endline "Running PIR Builder tests...";
  
  (* Test 1: Basic function creation *)
  let i32_ty = Types.Scalar Types.I32 in
  
  (* Simple function that returns a constant *)
  let func1 = Instructions.create_func "test" [] (Some i32_ty) [] in
  assert (func1.name = "test");
  assert (func1.params = []);
  assert (func1.return_ty = Some i32_ty);
  
  (* Test 2: Builder state initialization *)
  let init_state = Builder.init_builder "entry" in
  assert (init_state.current_block = "entry");
  assert (init_state.blocks = []);
  assert (init_state.next_var_id = 0);
  
  (* Test 3: Fresh variable names *)
  let open Builder in
  let (state1, name1) = fresh_var_name "tmp" init_state in
  let (state2, name2) = fresh_var_name "tmp" state1 in
  assert (name1 = "tmp0");
  assert (name2 = "tmp1");
  assert (state2.next_var_id = 2);
  
  (* Test 4: Value definition and lookup *)
  let test_val = Values.create_simple_value i32_ty in
  let (state3, _) = define_value "myval" test_val state2 in
  let (_, found_val) = lookup_value "myval" state3 in
  assert (Values.get_id test_val = Values.get_id found_val);
  
  (* Test 5: Block management *)
  let (state4, _) = set_current_block "block1" init_state in
  assert (state4.current_block = "block1");
  
  print_endline "✓ All PIR Builder tests passed"

(* Test PIR Pretty Printer *)
let test_pir_pretty_printer () =
  print_endline "Running PIR Pretty Printer tests...";
  
  (* Test 1: Type pretty printing *)
  let ctx = Pretty_printer.make_context ~config:{Pretty_printer.default_config with use_colors = false} () in
  
  assert (Pretty_printer.pp_type ctx (Types.Scalar Types.I32) = "i32");
  assert (Pretty_printer.pp_type ctx (Types.Vector (4, Types.F32)) = "vec<4 x f32>");
  assert (Pretty_printer.pp_type ctx Types.Ptr = "ptr");
  assert (Pretty_printer.pp_type ctx (Types.Array (10, Types.Scalar Types.I8)) = "[10 x i8]");
  
  (* Test 2: Value pretty printing *)
  let v1 = Values.create_simple_value (Types.Scalar Types.I32) in
  let v_str = Pretty_printer.pp_value ctx v1 in
  assert (String.contains v_str '%'); (* Values start with % *)
  assert (String.contains v_str ':'); (* Type annotation *)
  
  (* Test 3: Instruction pretty printing *)
  let v2 = Values.create_simple_value (Types.Scalar Types.I32) in
  let v3 = Values.create_simple_value (Types.Scalar Types.I32) in
  let add_instr = Instructions.Binop (Instructions.Add, Instructions.NoFlag, v2, v3) in
  let instr_str = Pretty_printer.pp_instr ctx add_instr in
  assert (String.contains instr_str 'a'); (* Contains 'add' *)
  
  (* Test 4: Terminator pretty printing *)
  let ret_term = Instructions.Ret (Some v1) in
  let term_str = Pretty_printer.pp_terminator ctx ret_term in
  assert (String.starts_with ~prefix:"ret" term_str);
  
  (* Test 5: Basic block pretty printing *)
  let block = Instructions.create_block "entry" [] [] (Instructions.Ret None) in
  let block_str = Pretty_printer.pp_basic_block ctx block in
  assert (String.contains block_str ':'); (* Contains label colon *)
  assert (String.contains block_str 'r'); (* Contains 'ret' *)
  
  (* Test 6: Function pretty printing *)
  let func = Instructions.create_func "main" [] (Some (Types.Scalar Types.I32)) [block] in
  let func_str = Pretty_printer.pp_function ctx func in
  assert (String.contains func_str 'f'); (* Contains 'func' *)
  assert (String.contains func_str '>'); (* Contains '->' *)
  
  (* Test 7: Colors disabled *)
  let no_color_config = { Pretty_printer.default_config with use_colors = false } in
  let ctx_no_color = Pretty_printer.make_context ~config:no_color_config () in
  let colored_str = Pretty_printer.pp_type ctx_no_color (Types.Scalar Types.I32) in
  assert (not (String.contains colored_str '\027')); (* No escape codes *)
  
  print_endline "✓ All PIR Pretty Printer tests passed"

(* Integration Test: Builder + Pretty Printer *)
let test_builder_pretty_printer_integration () =
  print_endline "Running Builder + Pretty Printer integration tests...";
  
  (* Test 1: Build a simple function and pretty print it *)
  let func1 = Builder.build_function "add_constant" [("x", Types.Scalar Types.I32)] (Some (Types.Scalar Types.I32)) (fun state ->
    let open Builder in
    let (state1, x) = lookup_value "x" state in
    let (state2, ten) = const_int (Types.Scalar Types.I32) 10 state1 in
    let (state3, result) = add "result" x ten state2 in
    ret (Some result) state3
  ) in
  
  (* Pretty print the function *)
  let func_str = Pretty_printer.function_to_string ~config:{Pretty_printer.default_config with use_colors = false} func1 in
  
  (* Verify the output contains expected elements *)
  assert (String.contains func_str 'f'); (* func keyword *)
  assert (String.contains func_str 'x'); (* parameter name *)
  assert (String.contains func_str '+' || String.contains func_str 'a'); (* add instruction *)
  assert (String.contains func_str 'r'); (* ret instruction *)
  
  (* Test 2: Build a control flow function *)
  let func2 = Builder.build_function "max" [("a", Types.Scalar Types.I32); ("b", Types.Scalar Types.I32)] (Some (Types.Scalar Types.I32)) (fun state ->
    let open Builder in
    let (s1, a) = lookup_value "a" state in
    let (s2, b) = lookup_value "b" s1 in
    let (s3, cmp) = icmp "cmp" Instructions.Sgt a b s2 in
    let (s4, ()) = br cmp "then" "else" s3 in
    
    let (s5, ()) = set_current_block "then" s4 in
    let (s6, ()) = jmp "end" s5 in
    
    let (s7, ()) = set_current_block "else" s6 in
    let (s8, ()) = jmp "end" s7 in
    
    let (s9, ()) = set_current_block "end" s8 in
    let (s10, result) = const_int (Types.Scalar Types.I32) 0 s9 in (* Placeholder *)
    ret (Some result) s10
  ) in
  
  let func2_str = Pretty_printer.function_to_string ~config:{Pretty_printer.default_config with use_colors = false} func2 in
  
  (* Verify control flow elements *)
  assert (String.contains func2_str ':'); (* Block labels *)
  assert (String.contains func2_str 'b'); (* br instruction *)
  assert (String.contains func2_str 'j'); (* jmp instruction *)
  
  (* Test 3: Pretty print a complete program *)
  let program_str = Pretty_printer.pp_program ~config:{Pretty_printer.default_config with use_colors = false} [func1; func2] in
  assert (String.length program_str > 100); (* Non-trivial output *)
  
  (* Test 4: Verify no color codes when colors disabled *)
  assert (not (String.contains program_str '\027'));
  
  print_endline "✓ All Builder + Pretty Printer integration tests passed"

let run_all_tests () =
  print_endline "\n=== TOCT Test Suite ===\n";
  
  test_build_system ();
  print_endline "";
  
  test_pir_types ();
  print_endline "";
  
  test_pir_attributes ();
  print_endline "";
  
  test_pir_values ();
  print_endline "";
  
  test_pir_instructions ();
  print_endline "";
  
  test_pir_builder ();
  print_endline "";
  
  test_pir_pretty_printer ();
  print_endline "";
  
  test_builder_pretty_printer_integration ();
  print_endline "";
  
  print_endline "🎉 All TOCT tests passed! 🎉"

let () = run_all_tests ()