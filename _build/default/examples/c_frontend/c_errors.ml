(* C-specific error reporting using shared infrastructure *)

open Compilerkit_frontend

(* Convert C lexer location to position *)
let position_of_location (loc: Lexer.location) =
  Position.create loc.filename loc.line loc.column 0

(* Convert C lexer location to span *)
let span_of_location (loc: Lexer.location) =
  let pos = position_of_location loc in
  Position.span pos pos

(* C error reporter type *)
type t = Error_reporter.t

(* Create a new error reporter *)
let create () = Error_reporter.create ()

(* Report parse error *)
let parse_error reporter loc message =
  let span = span_of_location loc in
  Error_reporter.report reporter Error_reporter.Error span message []

(* Report parse error with token *)
let parse_error_token reporter loc expected got =
  let span = span_of_location loc in
  let message = Printf.sprintf "Expected %s but got %s" expected (Lexer.token_to_string got) in
  Error_reporter.report reporter Error_reporter.Error span message []

(* Report lexer error *)
let lexer_error reporter loc message =
  let span = span_of_location loc in
  Error_reporter.report reporter Error_reporter.Error span message []

(* Report preprocessor error *)
let preprocessor_error reporter loc message =
  let span = span_of_location loc in
  Error_reporter.report reporter Error_reporter.Error span message []

(* Report warning *)
let warning reporter loc message =
  let span = span_of_location loc in
  Error_reporter.report reporter Error_reporter.Warning span message []

(* Report unused symbol warning *)
let unused_symbol reporter name symbol =
  let message = Printf.sprintf "Unused %s '%s'" 
    (match symbol.Symbol_table.kind with
     | Variable -> "variable"
     | Function -> "function"
     | Type -> "type"
     | _ -> "symbol")
    name in
  Error_reporter.report reporter Error_reporter.Warning symbol.Symbol_table.span message []

(* Check if there are errors *)
let has_errors reporter = Error_reporter.has_errors reporter

(* Format and print all diagnostics *)
let print_diagnostics reporter source_map =
  Error_reporter.format_diagnostics reporter source_map
  |> print_endline

(* Get diagnostic count *)
let error_count reporter = Error_reporter.error_count reporter
let warning_count reporter = Error_reporter.warning_count reporter