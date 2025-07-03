(* Example: Vector operations in PIR *)

open Compilerkit_pir

let build_vector_add () =
  Builder.build_function "vector_add" 
    [("a", Types.Vector (4, Types.F32)); ("b", Types.Vector (4, Types.F32))] 
    (Some (Types.Vector (4, Types.F32))) 
    (fun state ->
      let open Builder in
      
      (* Get parameters *)
      let (s1, a) = lookup_value "a" state in
      let (s2, b) = lookup_value "b" s1 in
      
      (* Vector addition *)
      let (s3, result) = add "result" a b s2 in
      
      (* Return result *)
      ret (Some result) s3
    )

let build_dot_product () =
  Builder.build_function "dot_product" 
    [("a", Types.Vector (4, Types.F32)); ("b", Types.Vector (4, Types.F32))] 
    (Some (Types.Scalar Types.F32)) 
    (fun state ->
      let open Builder in
      
      (* Get parameters *)
      let (s1, a) = lookup_value "a" state in
      let (s2, b) = lookup_value "b" s1 in
      
      (* Element-wise multiplication *)
      let (s3, prod) = mul "prod" a b s2 in
      
      (* In a real implementation, we'd extract and sum elements *)
      (* For now, just extract first element as placeholder *)
      let extract_instr = Instructions.Vector (Instructions.ExtractLane (prod, 0)) in
      let (s4, first_elem) = emit_instr ~result:"elem0" extract_instr (Attributes.empty ()) s3 in
      
      ret (Some first_elem) s4
    )

let () =
  print_endline "=== Vector Operations Examples ===\n";
  
  (* Build and display vector_add *)
  let vec_add = build_vector_add () in
  print_endline "--- Vector Addition ---";
  print_endline (Pretty_printer.function_to_string vec_add);
  
  let lint_result1 = Linter.lint_function vec_add in
  Linter.print_results "vector_add" lint_result1;
  
  (* Build and display dot_product *)
  let dot_prod = build_dot_product () in
  print_endline "\n--- Dot Product ---";
  print_endline (Pretty_printer.function_to_string dot_prod);
  
  let lint_result2 = Linter.lint_function dot_prod in
  Linter.print_results "dot_product" lint_result2;
  
  (* Display complete program *)
  print_endline "\n=== Complete Program ===";
  let program_str = Pretty_printer.pp_program [vec_add; dot_prod] in
  print_endline program_str