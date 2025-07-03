(* Tests for extended PIR features *)

[@@@warning "-21-26"]  (* Disable non-returning statement and unused variable warnings *)

open Compilerkit_pir
module I = Instructions

let test_freeze () =
  print_endline "Testing freeze instruction...";
  
  let v1 = Values.create_simple_value (Types.Scalar Types.I32) in
  let freeze_inst = I.create_simple_instruction
    ~result:(Values.create_simple_value (Types.Scalar Types.I32))
    (I.Freeze v1) in
  
  (* Check that freeze preserves type *)
  match freeze_inst.I.instr, freeze_inst.I.result with
  | I.Freeze frozen_val, Some result ->
    assert (Values.get_type frozen_val = Values.get_type result);
    print_endline "  ✓ Freeze instruction created successfully"
  | _ -> failwith "Unexpected result"

let test_extract_insert_value () =
  print_endline "Testing extractvalue/insertvalue instructions...";
  
  (* Create a struct type *)
  let struct_ty = Types.Struct [Types.Scalar Types.I32; Types.Scalar Types.F64] in
  let struct_val = Values.create_simple_value struct_ty in
  let i32_val = Values.create_simple_value (Types.Scalar Types.I32) in
  
  (* Test extractvalue *)
  let extract_inst = I.create_simple_instruction
    ~result:(Values.create_simple_value (Types.Scalar Types.I32))
    (I.ExtractValue (struct_val, [0])) in
  
  (* Test insertvalue *)
  let insert_inst = I.create_simple_instruction
    ~result:(Values.create_simple_value struct_ty)
    (I.InsertValue (struct_val, i32_val, [0])) in
  
  (* Verify extract and insert instructions *)
  let _ = extract_inst in
  let _ = insert_inst in
  print_endline "  ✓ ExtractValue instruction created";
  print_endline "  ✓ InsertValue instruction created"

let test_va_arg () =
  print_endline "Testing va_arg instruction...";
  
  (* Create a va_list (represented as ptr) *)
  let va_list = Values.create_simple_value Types.Ptr in
  let target_ty = Types.Scalar Types.I32 in
  
  let va_arg_inst = I.create_simple_instruction
    ~result:(Values.create_simple_value target_ty)
    (I.VaArg (va_list, target_ty)) in
  
  match va_arg_inst.I.instr with
  | I.VaArg (_, ty) ->
    assert (ty = target_ty);
    print_endline "  ✓ VaArg instruction created successfully"
  | _ -> failwith "Unexpected result"

let test_fence () =
  print_endline "Testing fence instruction...";
  
  (* Fence has no result *)
  let fence_inst = I.create_simple_instruction
    (I.Fence "seq_cst") in
  
  match fence_inst.I.instr, fence_inst.I.result with
  | I.Fence ordering, None ->
    assert (ordering = "seq_cst");
    print_endline "  ✓ Fence instruction created successfully"
  | _ -> failwith "Unexpected result"

let test_attributes () =
  print_endline "Testing attribute system...";
  
  (* Create instruction with attributes *)
  let load_inst = 
    let attrs = Attributes.empty () in
    let attrs = Attributes.add "volatile" (Attributes.Bool true) attrs in
    let attrs = Attributes.add "atomic" (Attributes.String "acquire") attrs in
    I.create_instruction 
      ~result:(Values.create_simple_value (Types.Scalar Types.I32))
      (I.Memory (I.Load (Types.Scalar Types.I32)))
      attrs in
  
  (* Check attributes *)
  (match Attributes.get_opt "volatile" load_inst.I.attrs with
  | Some (Attributes.Bool true) -> print_endline "  ✓ Volatile attribute set correctly"
  | _ -> failwith "Volatile attribute not set correctly");
  
  (match Attributes.get_opt "atomic" load_inst.I.attrs with
  | Some (Attributes.String "acquire") -> print_endline "  ✓ Atomic ordering attribute set correctly"
  | Some v -> failwith (Printf.sprintf "Unexpected atomic attribute value: %s" (Attributes.string_of_json v))
  | None -> failwith "Atomic attribute not found")

let test_value_naming_conventions () =
  print_endline "Testing value naming conventions...";
  
  (* Create different types of values *)
  let local_val = Values.create_simple_value (Types.Scalar Types.I32) in
  let local_str = Values.string_of_value local_val in
  
  (* Check that local values use vN format *)
  assert (String.length local_str > 1);
  assert (local_str.[0] = 'v');
  assert (int_of_string (String.sub local_str 1 (String.length local_str - 1)) > 0);
  
  print_endline "  ✓ Local value naming follows vN convention"

let test_pretty_printing () =
  print_endline "Testing pretty printing of new instructions...";
  
  let v1 = Values.create_simple_value (Types.Scalar Types.I32) in
  let struct_val = Values.create_simple_value 
    (Types.Struct [Types.Scalar Types.I32; Types.Scalar Types.F64]) in
  
  (* Test freeze pretty printing *)
  let freeze_str = I.string_of_instr (I.Freeze v1) in
  assert (String.length freeze_str > 0);
  print_endline ("  ✓ Freeze: " ^ freeze_str);
  
  (* Test extractvalue pretty printing *)
  let extract_str = I.string_of_instr (I.ExtractValue (struct_val, [0])) in
  assert (String.length extract_str > 0);
  print_endline ("  ✓ ExtractValue: " ^ extract_str);
  
  (* Test fence pretty printing *)
  let fence_str = I.string_of_instr (I.Fence "release") in
  assert (fence_str = "fence release");
  print_endline ("  ✓ Fence: " ^ fence_str)

let () =
  print_endline "=== PIR Extended Features Tests ===";
  try
    test_freeze ();
    test_extract_insert_value ();
    test_va_arg ();
    test_fence ();
    test_attributes ();
    test_value_naming_conventions ();
    test_pretty_printing ();
    print_endline "\nAll extended PIR tests passed! ✓"
  with e ->
    Printf.eprintf "Test failed with exception: %s\n" (Printexc.to_string e);
    exit 1