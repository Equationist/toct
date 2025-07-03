(* Tests for parser combinator DSL *)

open Compilerkit_frontend

(* Simple token type for testing *)
type test_token = 
  | TInt of int
  | TIdent of string
  | TPlus
  | TMinus
  | TStar
  | TSlash
  | TLParen
  | TRParen
  | TComma
  | TSemi
  | TEof

let _string_of_token = function
  | TInt n -> Printf.sprintf "INT(%d)" n
  | TIdent s -> Printf.sprintf "IDENT(%s)" s
  | TPlus -> "+"
  | TMinus -> "-"
  | TStar -> "*"
  | TSlash -> "/"
  | TLParen -> "("
  | TRParen -> ")"
  | TComma -> ","
  | TSemi -> ";"
  | TEof -> "EOF"

(* Test error type *)
type test_error = string
[@@warning "-34"]

(* Suppress unused constructor warnings *)
let _ = (TIdent "test", TSlash, TEof)

(* Helper to create tokens *)
let make_token kind text =
  let dummy_span = Position.span Position.dummy Position.dummy in
  { Pratt.kind; text; span = dummy_span }

let tokens_from_list lst =
  List.map (fun (kind, text) -> make_token kind text) lst

(* Test basic combinators *)
let test_basic_combinators () =
  print_endline "Testing basic combinators...";
  
  let open Pratt in
  
  (* Test return *)
  let p1 = return 42 in
  let tokens = tokens_from_list [(TInt 1, "1")] in
  let result = run_parser p1 tokens in
  assert (result = Ok 42);
  
  (* Test bind *)
  let p2 = 
    return 10 >>= fun x ->
    return (x * 2) in
  let result2 = run_parser p2 tokens in
  assert (result2 = Ok 20);
  
  (* Test map *)
  let p3 = return "hello" >>| String.uppercase_ascii in
  let result3 = run_parser p3 tokens in
  assert (result3 = Ok "HELLO");
  
  (* Test applicative *)
  let p4 = return (+) <*> return 5 <*> return 3 in
  let result4 = run_parser p4 tokens in
  assert (result4 = Ok 8);
  
  print_endline "✓ Basic combinators passed"

(* Test token operations *)
let test_token_operations () =
  print_endline "Testing token operations...";
  
  let open Pratt in
  
  let tokens = tokens_from_list [
    (TInt 42, "42");
    (TPlus, "+");
    (TInt 13, "13");
  ] in
  
  (* Test current *)
  let p1 = current in
  let result1 = run_parser p1 tokens in
  assert (match result1 with
    | Ok (Some tok) -> tok.kind = TInt 42
    | _ -> false);
  
  (* Test advance and consume *)
  let p2 = 
    consume >>= fun tok1 ->
    consume >>= fun tok2 ->
    return (tok1.kind, tok2.kind) in
  let result2 = run_parser p2 tokens in
  assert (result2 = Ok (TInt 42, TPlus));
  
  (* Test satisfy *)
  let is_int = function
    | { kind = TInt _; _ } -> true
    | _ -> false in
  let p3 = satisfy is_int "expected integer" in
  let result3 = run_parser p3 tokens in
  assert (match result3 with
    | Ok tok -> tok.kind = TInt 42
    | _ -> false);
  
  (* Test token *)
  let p4 = token TPlus "expected +" in
  let tokens2 = tokens_from_list [(TPlus, "+")] in
  let result4 = run_parser p4 tokens2 in
  assert (match result4 with
    | Ok tok -> tok.kind = TPlus
    | _ -> false);
  
  print_endline "✓ Token operations passed"

(* Test choice and repetition *)
let test_choice_and_repetition () =
  print_endline "Testing choice and repetition...";
  
  let open Pratt in
  
  (* Test choice *)
  let p1 = token TPlus "+" <|> token TMinus "-" in
  let tokens1 = tokens_from_list [(TMinus, "-")] in
  let result1 = run_parser p1 tokens1 in
  assert (match result1 with
    | Ok tok -> tok.kind = TMinus
    | _ -> false);
  
  (* Test many *)
  let p2 = many (token (TInt 1) "1") in
  let tokens2 = tokens_from_list [
    (TInt 1, "1"); (TInt 1, "1"); (TPlus, "+")
  ] in
  let result2 = run_parser p2 tokens2 in
  assert (match result2 with
    | Ok lst -> List.length lst = 2
    | _ -> false);
  
  (* Test many1 *)
  let p3 = many1 (token (TInt 2) "2") in
  let tokens3 = tokens_from_list [(TInt 2, "2"); (TInt 2, "2")] in
  let result3 = run_parser p3 tokens3 in
  assert (match result3 with
    | Ok lst -> List.length lst = 2
    | _ -> false);
  
  (* Test optional *)
  let p4 = optional (token TComma ",") in
  let tokens4 = tokens_from_list [(TSemi, ";")] in
  let result4 = run_parser p4 tokens4 in
  assert (result4 = Ok None);
  
  let tokens5 = tokens_from_list [(TComma, ",")] in
  let result5 = run_parser p4 tokens5 in
  assert (match result5 with
    | Ok (Some tok) -> tok.kind = TComma
    | _ -> false);
  
  print_endline "✓ Choice and repetition passed"

(* Test sequence combinators *)
let test_sequence_combinators () =
  print_endline "Testing sequence combinators...";
  
  let open Pratt in
  
  let tokens = tokens_from_list [
    (TLParen, "(");
    (TInt 42, "42");
    (TRParen, ")");
  ] in
  
  (* Test between *)
  let p1 = between 
    (token TLParen "(")
    (token TRParen ")")
    (token (TInt 42) "42") in
  let result1 = run_parser p1 tokens in
  assert (match result1 with
    | Ok tok -> tok.kind = TInt 42
    | _ -> false);
  
  (* Test sep_by *)
  let tokens2 = tokens_from_list [
    (TInt 1, "1"); (TComma, ",");
    (TInt 2, "2"); (TComma, ",");
    (TInt 3, "3")
  ] in
  let p2 = sep_by (token TComma ",") consume in
  let result2 = run_parser p2 tokens2 in
  assert (match result2 with
    | Ok lst -> List.length lst = 3
    | _ -> false);
  
  print_endline "✓ Sequence combinators passed"

(* Test Pratt expression parser *)
(* Simple expression type for testing *)
type expr =
  | EInt of int
  | EBinop of expr * string * expr
  | EUnop of string * expr

let test_pratt_parser () =
  print_endline "Testing Pratt expression parser...";
  
  let open Pratt in
  
  (* Build operator table *)
  let rec table = {
    prefix = (fun tok ->
      match tok with
      | TMinus -> Some ((fun _tok ->
          pratt_expr table 100 >>| fun e -> EUnop("-", e)), 100)
      | TInt _ -> Some ((fun tok ->
          match tok.kind with
          | TInt n -> return (EInt n)
          | _ -> fail "expected integer"), 0)
      | _ -> None);
    infix = (fun tok ->
      match tok with
      | TPlus -> Some ((fun left _tok ->
          pratt_expr table 20 >>| fun right ->
          EBinop(left, "+", right)), 10, 11)
      | TMinus -> Some ((fun left _tok ->
          pratt_expr table 20 >>| fun right ->
          EBinop(left, "-", right)), 10, 11)
      | TStar -> Some ((fun left _tok ->
          pratt_expr table 30 >>| fun right ->
          EBinop(left, "*", right)), 20, 21)
      | TSlash -> Some ((fun left _tok ->
          pratt_expr table 30 >>| fun right ->
          EBinop(left, "/", right)), 20, 21)
      | _ -> None)
  } in
  
  (* Test simple expression *)
  let tokens1 = tokens_from_list [(TInt 42, "42")] in
  let result1 = run_parser (pratt_expr table 0) tokens1 in
  assert (result1 = Ok (EInt 42));
  
  (* Test binary expression *)
  let tokens2 = tokens_from_list [
    (TInt 1, "1"); (TPlus, "+"); (TInt 2, "2")
  ] in
  let result2 = run_parser (pratt_expr table 0) tokens2 in
  assert (match result2 with
    | Ok (EBinop(EInt 1, "+", EInt 2)) -> true
    | _ -> false);
  
  (* Test precedence *)
  let tokens3 = tokens_from_list [
    (TInt 1, "1"); (TPlus, "+"); 
    (TInt 2, "2"); (TStar, "*"); 
    (TInt 3, "3")
  ] in
  let result3 = run_parser (pratt_expr table 0) tokens3 in
  assert (match result3 with
    | Ok (EBinop(EInt 1, "+", EBinop(EInt 2, "*", EInt 3))) -> true
    | _ -> false);
  
  (* Test unary *)
  let tokens4 = tokens_from_list [
    (TMinus, "-"); (TInt 5, "5")
  ] in
  let result4 = run_parser (pratt_expr table 0) tokens4 in
  assert (match result4 with
    | Ok (EUnop("-", EInt 5)) -> true
    | _ -> false);
  
  print_endline "✓ Pratt parser passed"

(* Test error handling *)
let test_error_handling () =
  print_endline "Testing error handling...";
  
  let open Pratt in
  
  (* Test fail *)
  let p1 = fail "test error" in
  let tokens = tokens_from_list [] in
  let result1 = run_parser p1 tokens in
  assert (match result1 with
    | Error errs -> List.mem "test error" errs
    | _ -> false);
  
  (* Test error accumulation *)
  let p2 = 
    fail "error1" >>= fun _ ->
    fail "error2" in
  let result2 = run_parser p2 tokens in
  assert (match result2 with
    | Error errs -> List.mem "error1" errs
    | _ -> false);
  
  print_endline "✓ Error handling passed"

(* Test position tracking *)
let test_position_tracking () =
  print_endline "Testing position tracking...";
  
  (* Test position advancement *)
  let pos1 = Position.create "test.ml" 1 1 0 in
  let pos2 = Position.advance_char pos1 'a' in
  assert (pos2.column = 2 && pos2.offset = 1);
  
  let pos3 = Position.advance_char pos2 '\n' in
  assert (pos3.line = 2 && pos3.column = 1 && pos3.offset = 2);
  
  (* Test span *)
  let span = Position.span pos1 pos3 in
  assert (span.start_pos = pos1 && span.end_pos = pos3);
  
  (* Test string advancement *)
  let pos4 = Position.advance_string pos1 "hello\nworld" in
  assert (pos4.line = 2 && pos4.column = 6);
  
  print_endline "✓ Position tracking passed"

(* Test lexer utilities *)
let test_lexer_utils () =
  print_endline "Testing lexer utilities...";
  
  (* Test character classification *)
  assert (Lexer_utils.is_whitespace ' ');
  assert (Lexer_utils.is_whitespace '\t');
  assert (not (Lexer_utils.is_whitespace 'a'));
  
  assert (Lexer_utils.is_digit '5');
  assert (not (Lexer_utils.is_digit 'a'));
  
  assert (Lexer_utils.is_hex_digit 'f');
  assert (Lexer_utils.is_hex_digit 'F');
  assert (Lexer_utils.is_hex_digit '9');
  
  assert (Lexer_utils.is_ident_start '_');
  assert (Lexer_utils.is_ident_start 'a');
  assert (not (Lexer_utils.is_ident_start '0'));
  
  (* Test lexer buffer *)
  let buf = Lexer_utils.Buffer.create "test.ml" "hello 123 world" in
  
  assert (Lexer_utils.Buffer.peek buf = Some 'h');
  assert (Lexer_utils.Buffer.advance buf = Some 'h');
  assert (Lexer_utils.Buffer.peek buf = Some 'e');
  
  Lexer_utils.Buffer.skip_while buf Lexer_utils.is_alpha;
  assert (Lexer_utils.Buffer.peek buf = Some ' ');
  
  Lexer_utils.Buffer.skip_whitespace buf;
  let digits = Lexer_utils.Buffer.take_while buf Lexer_utils.is_digit in
  assert (digits = "123");
  
  (* Test patterns *)
  let buf2 = Lexer_utils.Buffer.create "test.ml" "foo_bar 0xFF 3.14e-2" in
  
  let ident_result = Lexer_utils.Patterns.scan_ident buf2 in
  assert (match ident_result with
    | Some (text, _) -> text = "foo_bar"
    | None -> false);
  
  Lexer_utils.Buffer.skip_whitespace buf2;
  
  let int_result = Lexer_utils.Patterns.scan_int buf2 in
  assert (match int_result with
    | Some (text, _) -> text = "0xFF"
    | None -> false);
  
  Lexer_utils.Buffer.skip_whitespace buf2;
  
  let float_result = Lexer_utils.Patterns.scan_float buf2 in
  assert (match float_result with
    | Some (text, _) -> text = "3.14e-2"
    | None -> false);
  
  print_endline "✓ Lexer utilities passed"

(* Test error reporter *)
let test_error_reporter () =
  print_endline "Testing error reporter...";
  
  let span = Position.span
    (Position.create "test.ml" 1 5 4)
    (Position.create "test.ml" 1 10 9) in
  
  (* Test diagnostic creation *)
  let err = Error_reporter.error span "Type mismatch" in
  assert (err.severity = Error_reporter.Error);
  assert (err.message = "Type mismatch");
  
  let warn = Error_reporter.warning span "Unused variable" in
  assert (warn.severity = Error_reporter.Warning);
  
  (* Test diagnostic with notes *)
  let err_with_note = 
    Error_reporter.with_note err span "expected int" in
  let err_with_notes = 
    Error_reporter.with_note err_with_note span "found string" in
  assert (List.length err_with_notes.notes = 2);
  
  (* Test diagnostic collection *)
  let diags = Error_reporter.Diagnostics.create () in
  Error_reporter.Diagnostics.add diags err;
  Error_reporter.Diagnostics.add diags warn;
  
  assert (Error_reporter.Diagnostics.has_errors diags);
  assert (Error_reporter.Diagnostics.count_errors diags = 1);
  assert (Error_reporter.Diagnostics.count_warnings diags = 1);
  
  print_endline "✓ Error reporter passed"

let run_all_tests () =
  print_endline "\n=== Parser Combinator Tests ===\n";
  
  test_basic_combinators ();
  print_endline "";
  
  test_token_operations ();
  print_endline "";
  
  test_choice_and_repetition ();
  print_endline "";
  
  test_sequence_combinators ();
  print_endline "";
  
  test_pratt_parser ();
  print_endline "";
  
  test_error_handling ();
  print_endline "";
  
  test_position_tracking ();
  print_endline "";
  
  test_lexer_utils ();
  print_endline "";
  
  test_error_reporter ();
  print_endline "";
  
  print_endline "🎉 All parser combinator tests passed! 🎉"

let () = run_all_tests ()