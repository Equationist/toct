(* Example: Memory operations and struct manipulation in PIR *)

open Compilerkit_pir

let build_struct_example () =
  (* Define a Point struct type: { x: i32, y: i32 } *)
  let point_ty = Types.Struct [Types.Scalar Types.I32; Types.Scalar Types.I32] in
  
  Builder.build_function "create_point" 
    [("x", Types.Scalar Types.I32); ("y", Types.Scalar Types.I32)] 
    (Some Types.Ptr) 
    (fun state ->
      let open Builder in
      
      (* Get parameters *)
      let (s1, x) = lookup_value "x" state in
      let (s2, y) = lookup_value "y" s1 in
      
      (* Allocate space for struct *)
      let struct_size = Types.size_of point_ty in
      let (s3, size_val) = const_int (Types.Scalar Types.I32) struct_size s2 in
      let (s4, ptr) = alloca "point_ptr" size_val 8 s3 in
      
      (* Store x at offset 0 *)
      let (s5, zero) = const_int (Types.Scalar Types.I32) 0 s4 in
      let field0_instr = Instructions.Address (Instructions.FieldAddr (ptr, 0)) in
      let (s6, x_ptr) = emit_instr ~result:"x_ptr" field0_instr (Attributes.empty ()) s5 in
      let (s7, ()) = store x x_ptr s6 in
      
      (* Store y at offset 1 *)
      let field1_instr = Instructions.Address (Instructions.FieldAddr (ptr, 1)) in
      let (s8, y_ptr) = emit_instr ~result:"y_ptr" field1_instr (Attributes.empty ()) s7 in
      let (s9, ()) = store y y_ptr s8 in
      
      (* Return pointer to struct *)
      ret (Some ptr) s9
    )

let build_array_sum () =
  Builder.build_function "array_sum" 
    [("arr", Types.Ptr); ("len", Types.Scalar Types.I32)] 
    (Some (Types.Scalar Types.I32)) 
    (fun state ->
      let open Builder in
      
      (* Get parameters *)
      let (s1, arr) = lookup_value "arr" state in
      let (s2, len) = lookup_value "len" s1 in
      
      (* Initialize sum = 0 and i = 0 *)
      let (s3, zero) = const_int (Types.Scalar Types.I32) 0 s2 in
      let (s4, ()) = jmp "loop_header" s3 in
      
      (* Loop header: check i < len *)
      let (s5, ()) = set_current_block "loop_header" s4 in
      let (s6, ()) = add_block_param "i" (Types.Scalar Types.I32) s5 in
      let (s7, ()) = add_block_param "sum" (Types.Scalar Types.I32) s6 in
      
      (* For demonstration, we'll just return zero *)
      (* A real implementation would have the loop logic *)
      ret (Some zero) s7
    )

let () =
  print_endline "=== Memory Operations Examples ===\n";
  
  (* Build and display create_point *)
  let create_point = build_struct_example () in
  print_endline "--- Struct Creation ---";
  print_endline (Pretty_printer.function_to_string create_point);
  
  let lint_result1 = Linter.lint_function create_point in
  Linter.print_results "create_point" lint_result1;
  
  (* Build and display array_sum *)
  let array_sum = build_array_sum () in
  print_endline "\n--- Array Sum ---";
  print_endline (Pretty_printer.function_to_string array_sum);
  
  let lint_result2 = Linter.lint_function array_sum in
  Linter.print_results "array_sum" lint_result2