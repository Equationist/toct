(* Common parser utilities *)

(* Using Position module *)

(* Generic parser state *)
type ('token, 'state) parser_state = {
  tokens: 'token Token_location.located_token array;
  mutable pos: int;
  reporter: Error_reporter.t;
  custom_state: 'state;
}

(* Create parser state *)
let create_parser tokens reporter custom_state =
  {
    tokens = Array.of_list tokens;
    pos = 0;
    reporter;
    custom_state;
  }

(* Token operations *)
let at_end state =
  state.pos >= Array.length state.tokens

let peek state =
  if at_end state then
    None
  else
    Some state.tokens.(state.pos)

let peek_token state =
  match peek state with
  | Some tok -> Some tok.Token_location.token
  | None -> None

let peek_n state n =
  let pos = state.pos + n in
  if pos >= Array.length state.tokens then
    None
  else
    Some state.tokens.(pos)

let current_span state =
  match peek state with
  | Some tok -> tok.Token_location.span
  | None -> 
      if Array.length state.tokens > 0 then
        let last = state.tokens.(Array.length state.tokens - 1) in
        last.Token_location.span
      else
        Position.span (Position.create "" 1 1 0) (Position.create "" 1 1 0)

let advance state =
  if not (at_end state) then
    state.pos <- state.pos + 1

let check state expected =
  match peek_token state with
  | Some tok -> tok = expected
  | None -> false

let check_any state tokens =
  match peek_token state with
  | Some tok -> List.mem tok tokens
  | None -> false

let consume state expected =
  if check state expected then
    (advance state; true)
  else
    false

let consume_any state tokens =
  match peek_token state with
  | Some tok when List.mem tok tokens ->
      advance state;
      Some tok
  | _ -> None

(* Error recovery *)
type 'token recovery_strategy =
  | SkipTo of 'token list
  | SkipToAny of 'token list list
  | SkipN of int
  | SkipToBalanced of 'token * 'token

let recover state strategy =
  match strategy with
  | SkipTo tokens ->
      while not (at_end state) && not (check_any state tokens) do
        advance state
      done
  | SkipToAny token_lists ->
      while not (at_end state) && 
            not (List.exists (check_any state) token_lists) do
        advance state
      done
  | SkipN n ->
      for _ = 1 to n do
        if not (at_end state) then advance state
      done
  | SkipToBalanced (open_tok, close_tok) ->
      let depth = ref 1 in
      advance state; (* Skip initial open token *)
      while !depth > 0 && not (at_end state) do
        match peek_token state with
        | Some tok when tok = open_tok -> incr depth; advance state
        | Some tok when tok = close_tok -> decr depth; advance state
        | _ -> advance state
      done

(* Parse helpers with error reporting *)
let expect state expected to_string =
  match peek state with
  | Some tok when tok.Token_location.token = expected ->
      advance state;
      Ok tok
  | Some tok ->
      let span = tok.Token_location.span in
      let msg = Printf.sprintf "Expected %s but got %s" 
        (to_string expected) (to_string tok.Token_location.token) in
      Error_reporter.report state.reporter Error_reporter.Error span msg [];
      Error ()
  | None ->
      let span = current_span state in
      let msg = Printf.sprintf "Expected %s but reached end of input" 
        (to_string expected) in
      Error_reporter.report state.reporter Error_reporter.Error span msg [];
      Error ()

let expect_any state expected to_string =
  match peek state with
  | Some tok when List.mem tok.Token_location.token expected ->
      advance state;
      Ok tok
  | Some tok ->
      let span = tok.Token_location.span in
      let expected_str = String.concat " or " (List.map to_string expected) in
      let msg = Printf.sprintf "Expected %s but got %s" 
        expected_str (to_string tok.Token_location.token) in
      Error_reporter.report state.reporter Error_reporter.Error span msg [];
      Error ()
  | None ->
      let span = current_span state in
      let expected_str = String.concat " or " (List.map to_string expected) in
      let msg = Printf.sprintf "Expected %s but reached end of input" expected_str in
      Error_reporter.report state.reporter Error_reporter.Error span msg [];
      Error ()

(* Bracketed parsing *)
let parse_bracketed state open_tok close_tok to_string parse_content =
  match expect state open_tok to_string with
  | Error _ -> Error ()
  | Ok start_tok ->
      let content_result = parse_content state in
      match expect state close_tok to_string with
      | Error _ -> 
          recover state (SkipTo [close_tok]);
          consume state close_tok |> ignore;
          Error ()
      | Ok end_tok ->
          match content_result with
          | Ok content ->
              let span = Position.span_union start_tok.span end_tok.span in
              Ok (content, span)
          | Error _ -> Error ()

(* List parsing *)
let parse_list state parse_item separator _to_string allow_trailing =
  let items = ref [] in
  let rec parse_items () =
    match parse_item state with
    | Error _ -> ()
    | Ok item ->
        items := item :: !items;
        if consume state separator then
          if not allow_trailing || not (at_end state) then
            parse_items ()
  in
  parse_items ();
  List.rev !items

let parse_separated_list state parse_item separator _to_string =
  match parse_item state with
  | Error () -> []
  | Ok first ->
      let items = ref [first] in
      while consume state separator do
        match parse_item state with
        | Error _ -> ()
        | Ok item -> items := item :: !items
      done;
      List.rev !items