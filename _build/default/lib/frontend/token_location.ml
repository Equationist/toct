(* Generic token location tracking *)

open Position

(* Generic token with location *)
type 'token located_token = {
  token: 'token;
  span: span;
}

(* Create a located token *)
let make_token token span = { token; span }

(* Get token position *)
let token_span tok = tok.span
let token_start tok = tok.span.start_pos
let token_end tok = tok.span.end_pos

(* Combine spans of multiple tokens *)
let span_tokens tokens =
  match tokens with
  | [] -> failwith "Cannot get span of empty token list"
  | [tok] -> tok.span
  | first :: rest ->
      let last = List.fold_left (fun _ t -> t) first rest in
      Position.span_union first.span last.span