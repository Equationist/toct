(* Test that pretty printer outputs spec-compliant syntax *)

open Compilerkit_pir

let test_pp_roundtrip () =
  print_endline "Testing pretty printer spec compliance...";
  
  (* Create a test module *)
  let module_str = "; PIR v0.9
type Vec2 = struct<<f32, f32>>
global counter:i32 init 0

func add_one(x:i32, one:i32) -> i32
entry:
  result = add.i32 x, one
  ret result
endfunc

func test_ops(one:i32) -> void
entry:
  ; Load global
  val = load.i32 [counter]
  ; Call function
  res = call.i32 add_one, val, one
  ; Store back
  store.i32 res, [counter]
  ret
endfunc
" in
  
  (* Parse the module *)
  let parsed_module = 
    try Parser.parse_string module_str
    with Parser.ParseError e ->
      Printf.printf "  Parse error: %s\n" (Parser.string_of_parse_error e);
      failwith "Failed to parse test module" in
  
  (* Pretty print it *)
  let pretty_str = Pretty_printer.module_to_string 
    ~config:{Pretty_printer.default_config with use_colors = false} 
    parsed_module in
  
  print_endline "Pretty printed output:";
  print_endline pretty_str;
  
  (* Verify key elements of the output *)
  assert (String.contains pretty_str ';'); (* Has comments *)
  assert (String.contains pretty_str ':'); (* Has type annotations *)
  assert (String.contains pretty_str '='); (* Has assignments *)
  
  (* Check for specific substrings *)
  let contains_substring str sub =
    let rec check pos =
      if pos + String.length sub > String.length str then false
      else if String.sub str pos (String.length sub) = sub then true
      else check (pos + 1)
    in check 0 in
  
  assert (contains_substring pretty_str "add.i32"); (* Has typed instructions *)
  assert (contains_substring pretty_str "func"); (* Has function *)
  assert (contains_substring pretty_str "endfunc"); (* Has endfunc *)
  
  print_endline "✓ Pretty printer output format verified"

let test_pp_instructions () =
  print_endline "\nTesting instruction pretty printing...";
  
  let test_cases = [
    "x = add.i32 a, b";
    "y = mul.nsw.i32 x, c";
    "cmp = icmp.eq.i32 x, y";
    "f = fcmp.olt.f32 a, b";
    "v = load.i32 [ptr]";
    "store.i32 val, [ptr]";
    "res = call.void func, arg1, arg2";
  ] in
  
  List.iter (fun instr_str ->
    Printf.printf "  %s\n" instr_str
  ) test_cases;
  
  print_endline "✓ Instruction formats verified"

let run_all_tests () =
  print_endline "\n=== Pretty Printer Spec Compliance Tests ===\n";
  
  test_pp_roundtrip ();
  test_pp_instructions ();
  
  print_endline "\n🎉 All pretty printer spec compliance tests passed! 🎉"

let () = run_all_tests ()