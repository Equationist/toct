(* Common test utilities for frontend testing *)

open Error_reporter

(* Test result type *)
type test_result = 
  | Pass
  | Fail of string
  | Error of exn

(* Run a test and catch exceptions *)
let run_test name f =
  Printf.printf "Running %s... " name;
  flush stdout;
  try
    match f () with
    | Pass -> 
        Printf.printf "PASS\n";
        true
    | Fail msg ->
        Printf.printf "FAIL: %s\n" msg;
        false
    | Error e ->
        Printf.printf "ERROR: %s\n" (Printexc.to_string e);
        false
  with e ->
    Printf.printf "EXCEPTION: %s\n" (Printexc.to_string e);
    false

(* Assert equality with pretty printing *)
let assert_equal ~pp expected actual =
  if expected = actual then
    Pass
  else
    Fail (Printf.sprintf "Expected:\n%s\nActual:\n%s" (pp expected) (pp actual))

(* Assert string equality *)
let assert_string_equal expected actual =
  assert_equal ~pp:(fun s -> s) expected actual

(* Assert boolean *)
let assert_true msg condition =
  if condition then Pass else Fail msg

let assert_false msg condition =
  if not condition then Pass else Fail msg

(* Test lexing *)
module Lexing = struct
  (* Lex a string and collect tokens *)
  let lex_string lexer_fn source =
    let reporter = Error_reporter.create () in
    try
      let tokens = lexer_fn reporter source in
      if has_errors reporter then
        None
      else
        Some tokens
    with _e -> None

  (* Test that lexing succeeds *)
  let test_lex_success lexer_fn source =
    match lex_string lexer_fn source with
    | Some _ -> Pass
    | None -> Error (Failure "Lexing failed")

  (* Test that lexing produces expected tokens *)
  let test_lex_tokens lexer_fn source expected =
    match lex_string lexer_fn source with
    | Some tokens ->
        if tokens = expected then Pass
        else Fail "Token mismatch"
    | None -> Error (Failure "Lexing failed")
end

(* Test parsing *)
module Parsing = struct
  (* Parse tokens and collect AST *)
  let parse_tokens parser_fn tokens =
    let reporter = Error_reporter.create () in
    try
      let ast = parser_fn reporter tokens in
      if has_errors reporter then
        None
      else
        Some ast
    with _e -> None

  (* Test that parsing succeeds *)
  let test_parse_success parser_fn tokens =
    match parse_tokens parser_fn tokens with
    | Some _ -> Pass
    | None -> Error (Failure "Parsing failed")

  (* Test that parsing fails *)
  let test_parse_failure parser_fn tokens =
    match parse_tokens parser_fn tokens with
    | Some _ -> Fail "Expected parse failure"
    | None -> Pass
end

(* Test diagnostics *)
module Diagnostics = struct
  (* Check that specific diagnostics are reported *)
  let has_error_containing reporter substring =
    let diags = format_diagnostics reporter (Hashtbl.create 0) in
    String.contains diags substring

  (* Count diagnostics *)
  let count_errors = error_count
  let count_warnings = warning_count
end

(* Test fixtures *)
module Fixtures = struct
  (* Create temporary test file *)
  let with_temp_file content f =
    let filename = Filename.temp_file "test_" ".tmp" in
    let oc = open_out filename in
    output_string oc content;
    close_out oc;
    try
      let result = f filename in
      Sys.remove filename;
      result
    with e ->
      Sys.remove filename;
      raise e

  (* Read test file *)
  let read_test_file path =
    let ic = open_in path in
    let content = really_input_string ic (in_channel_length ic) in
    close_in ic;
    content
end

(* Test suite runner *)
let run_suite name tests =
  Printf.printf "\n=== %s ===\n" name;
  let passed = ref 0 in
  let total = List.length tests in
  List.iter (fun (test_name, test_fn) ->
    if run_test test_name test_fn then
      incr passed
  ) tests;
  Printf.printf "\nPassed %d/%d tests\n" !passed total;
  !passed = total

(* Benchmark utilities *)
module Benchmark = struct
  (* Time a function *)
  let time name f =
    let start = Sys.time () in
    let result = f () in
    let elapsed = Sys.time () -. start in
    Printf.printf "%s: %.3f seconds\n" name elapsed;
    result

  (* Memory usage *)
  let memory_usage () =
    Gc.compact ();
    let stat = Gc.stat () in
    stat.Gc.heap_words * (Sys.word_size / 8)
end