(* Example: Fibonacci function in PIR *)

open Compilerkit_pir

let build_fibonacci () =
  Builder.build_function "fibonacci" [("n", Types.Scalar Types.I32)] (Some (Types.Scalar Types.I32)) (fun state ->
    let open Builder in
    
    (* Get parameter n *)
    let (s1, n) = lookup_value "n" state in
    
    (* Check if n <= 1 *)
    let (s2, one) = const_int (Types.Scalar Types.I32) 1 s1 in
    let (s3, cmp) = icmp "cmp" Instructions.Sle n one s2 in
    let (s4, ()) = br cmp "base_case" "recursive_case" s3 in
    
    (* Base case: return n *)
    let (s5, ()) = set_current_block "base_case" s4 in
    let (s6, ()) = jmp "end" s5 in
    
    (* Recursive case: fib(n-1) + fib(n-2) *)
    let (s7, ()) = set_current_block "recursive_case" s6 in
    let (s8, n_minus_1) = sub "n_minus_1" n one s7 in
    let (s9, two) = const_int (Types.Scalar Types.I32) 2 s8 in
    let (s10, n_minus_2) = sub "n_minus_2" n two s9 in
    
    (* For demonstration, we'll just compute n-1 + n-2 *)
    (* In a real implementation, we would need function calls *)
    let (s11, _result) = add "result" n_minus_1 n_minus_2 s10 in
    let (s12, ()) = jmp "end" s11 in
    
    (* End block *)
    let (s13, ()) = set_current_block "end" s12 in
    let (s14, ()) = add_block_param "ret_val" (Types.Scalar Types.I32) s13 in
    
    (* In a real SSA form, we'd use phi nodes or block parameters *)
    (* For now, return n as placeholder *)
    ret (Some n) s14
  )

let () =
  print_endline "=== Fibonacci Function Example ===\n";
  
  (* Build the function *)
  let fib_func = build_fibonacci () in
  
  (* Pretty print it *)
  let func_str = Pretty_printer.function_to_string fib_func in
  print_endline func_str;
  
  (* Lint it *)
  print_endline "\n=== Linting Results ===";
  let lint_result = Linter.lint_function fib_func in
  Linter.print_results "fibonacci" lint_result;
  
  if Linter.is_valid lint_result then
    print_endline "✓ Function passed all validation checks!"
  else
    print_endline "✗ Function has validation errors"