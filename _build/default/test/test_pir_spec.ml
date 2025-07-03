(* Test PIR parser against specification *)

open Compilerkit_pir

let test_lexer () =
  print_endline "Testing PIR lexer with spec syntax...";
  
  (* Test 1: Module-level tokens *)
  let tokens1 = Lexer.tokenize "type Vec3 = struct<<f32, f32, f32>>" in
  assert (List.hd tokens1 = Lexer.TYPE);
  assert (List.mem Lexer.LCHEVRON tokens1);
  assert (List.mem Lexer.RCHEVRON tokens1);
  
  (* Test 2: Vector types *)
  let tokens2 = Lexer.tokenize "v4xi32" in
  assert (match List.hd tokens2 with Lexer.VECTYPE (4, "i32") -> true | _ -> false);
  
  (* Test 3: Comments *)
  let tokens3 = Lexer.tokenize "; This is a comment\nfunc" in
  assert (List.hd tokens3 = Lexer.FUNC);
  
  (* Test 4: Instruction with type suffix *)
  let tokens4 = Lexer.tokenize "x = add.nsw.i32 a, b" in
  Printf.printf "  Test 4 tokens: ";
  List.iter (fun tok -> Printf.printf "%s " (Lexer.string_of_token tok)) tokens4;
  Printf.printf "\n";
  assert (List.mem Lexer.NSW tokens4);
  assert (List.mem Lexer.DOT tokens4);
  
  print_endline "✓ PIR lexer tests passed"

let test_parser_simple () =
  print_endline "Testing PIR parser with simple module...";
  
  let simple_module = "; PIR v0.9
global x:i32 init 42

func main() -> i32
entry:
  val = load.i32 [x]
  ret val
endfunc
" in
  
  (try
    let module_ = Parser.parse_string simple_module in
    assert (List.length module_.Module_ir.items = 2) (* global + func *)
  with Parser.ParseError e ->
    Printf.printf "  Parse error: %s\n" (Parser.string_of_parse_error e);
    failwith "Parser test failed");
  
  print_endline "✓ Simple parser test passed"

let test_parser_types () =
  print_endline "Testing PIR parser with type declarations...";
  
  let type_module = "
type Vec2 = struct<<f32, f32>>
type IntArray = array[10]i32
type PackedRGB = packed_struct<<i8, i8, i8>>

global origin:Vec2 init <<0.0, 0.0>>
" in
  
  let module_ = Parser.parse_string type_module in
  assert (List.length module_.items = 4);
  
  print_endline "✓ Type declaration tests passed"

let test_parser_instructions () =
  print_endline "Testing PIR parser with instructions...";
  
  let instr_module = "
func test_ops(x:i32, y:i32) -> void
entry:
  ; Arithmetic
  c = add.i32 x, y
  d = mul.nsw.i32 c, x
  
  ; Comparisons
  cmp1 = icmp.eq.i32 x, y
  
  ; Memory ops
  p = alloca x align 4
  store.i32 c, [p]
  v = load.i32 [p]
  
  ; Control flow
  br cmp1, then_block, else_block
  
then_block:
  ret
  
else_block:
  ret
endfunc
" in
  
  (try
    let module_ = Parser.parse_string instr_module in
    assert (List.length module_.items = 1);
    print_endline "✓ Instruction parsing tests passed"
  with Parser.ParseError e ->
    Printf.printf "  Parse error: %s\n" (Parser.string_of_parse_error e);
    failwith "Instruction parsing test failed")

let test_parser_vectors () =
  print_endline "Testing PIR parser with vector operations...";
  
  let vec_module = "
func vec_ops(v:v4xf32, x:f32) -> f32
entry:
  ; Splat
  vzero = splat x, 4
  
  ; Extract/insert  
  elem = extractlane v, 0
  v2 = insertlane v, 1, elem
  
  ; Vector arithmetic
  sum = fadd v, vzero
  
  ret elem
endfunc
" in
  
  (try
    let module_ = Parser.parse_string vec_module in
    assert (List.length module_.items = 1);
    print_endline "✓ Vector operation tests passed"
  with Parser.ParseError e ->
    Printf.printf "  Parse error: %s\n" (Parser.string_of_parse_error e);
    failwith "Vector operation test failed")

let test_parser_casts () =
  print_endline "Testing PIR parser with cast operations...";
  
  let cast_module = "
func test_casts(x:i64) -> i32
entry:
  ; Truncate
  y = trunc x to i32
  
  ; Sign extend
  z = sext y to i64
  
  ; Float conversions
  f = uitofp y to f32
  i = fptoui f to i32
  
  ret i
endfunc
" in
  
  (try
    let module_ = Parser.parse_string cast_module in
    assert (List.length module_.items = 1);
    print_endline "✓ Cast operation tests passed"
  with Parser.ParseError e ->
    Printf.printf "  Parse error: %s\n" (Parser.string_of_parse_error e);
    failwith "Cast operation test failed")

let test_parser_annotations () =
  print_endline "Testing PIR parser with annotations...";
  
  let annot_module = "
func pure_add(x:i32, y:i32) -> i32    @{\"pure\":true, \"pre\":\"x>0 && y>0\"}
entry:
  result = add.i32 x, y    @{\"range\":[0,2147483647]}
  ret result
endfunc
" in
  
  (* For now, we skip annotations in parsing, so this should work *)
  (try
    let _module = Parser.parse_string annot_module in
    print_endline "✓ Annotation tests passed (parsing only)"
  with Parser.ParseError e ->
    Printf.printf "  Parse error: %s\n" (Parser.string_of_parse_error e);
    failwith "Annotation test failed")

let test_parser_complete () =
  print_endline "Testing PIR parser with complete module...";
  
  let complete = "; PIR v0.9
; Complete test module

type Point = struct<<f32, f32>>

global counter:i32 init 0
const max_val:i32 init 100

func increment(one:i32) -> void
entry:
  val = load.i32 [counter]
  new_val = add.nsw.i32 val, one
  store.i32 new_val, [counter]
  ret
endfunc

func clamp(x:i32, zero:i32) -> i32
entry:
  too_small = icmp.slt.i32 x, zero
  br too_small, ret_zero, check_max
  
ret_zero:
  ret zero
  
check_max:
  max = load.i32 [max_val]
  too_big = icmp.sgt.i32 x, max
  br too_big, ret_max, ret_x
  
ret_max:
  ret max
  
ret_x:
  ret x
endfunc

func main(one:i32, zero:i32) -> i32
entry:
  call.void increment, one
  val = load.i32 [counter]
  clamped = call.i32 clamp, val, zero
  ret clamped
endfunc
" in
  
  (try
    let module_ = Parser.parse_string complete in
    assert (List.length module_.items >= 5); (* type + 2 globals + 3 funcs *)
    print_endline "✓ Complete module test passed"
  with Parser.ParseError e ->
    Printf.printf "  Parse error: %s\n" (Parser.string_of_parse_error e);
    failwith "Complete module test failed")

let run_all_tests () =
  print_endline "\n=== PIR Spec Compliance Tests ===\n";
  
  test_lexer ();
  print_endline "";
  
  test_parser_simple ();
  print_endline "";
  
  test_parser_types ();
  print_endline "";
  
  test_parser_instructions ();
  print_endline "";
  
  test_parser_vectors ();
  print_endline "";
  
  test_parser_casts ();
  print_endline "";
  
  test_parser_annotations ();
  print_endline "";
  
  test_parser_complete ();
  print_endline "";
  
  print_endline "🎉 All PIR spec compliance tests passed! 🎉"

let () = run_all_tests ()