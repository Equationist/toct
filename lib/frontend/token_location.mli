(* Generic token location tracking *)

open Position

(* Generic token with location *)
type 'token located_token = {
  token: 'token;
  span: span;
}

(* Create a located token *)
val make_token : 'token -> span -> 'token located_token

(* Get token position *)
val token_span : 'token located_token -> span
val token_start : 'token located_token -> Position.t
val token_end : 'token located_token -> Position.t

(* Combine spans of multiple tokens *)
val span_tokens : 'token located_token list -> span