(* Pratt Parser Combinator DSL *)

open Position

(* Token type parameterized by the token kind *)
type 'a token = {
  kind: 'a;
  text: string;
  span: span;
}

(* Parser state *)
type ('tok, 'err) state = {
  tokens: 'tok token list;
  pos: int;
  errors: 'err list;
}

(* Parser result *)
type ('a, 'err) result = 
  | Ok of 'a
  | Error of 'err

(* Parser type: state -> (result * new_state) *)
type ('tok, 'a, 'err) parser = ('tok, 'err) state -> ('a, 'err) result * ('tok, 'err) state

(* Basic combinators *)
let return x : ('tok, 'a, 'err) parser = 
  fun state -> (Ok x, state)

let fail err : ('tok, 'a, 'err) parser =
  fun state -> (Error err, { state with errors = err :: state.errors })

let (>>=) (p: ('tok, 'a, 'err) parser) (f: 'a -> ('tok, 'b, 'err) parser) : ('tok, 'b, 'err) parser =
  fun state ->
    match p state with
    | (Ok x, state') -> f x state'
    | (Error e, state') -> (Error e, state')

let (>>|) p f = p >>= fun x -> return (f x)

let (<*>) pf px =
  pf >>= fun f ->
  px >>= fun x ->
  return (f x)

let (<$>) f p = return f <*> p

(* Choice combinator *)
let (<|>) (p1: ('tok, 'a, 'err) parser) (p2: ('tok, 'a, 'err) parser) : ('tok, 'a, 'err) parser =
  fun state ->
    let start_pos = state.pos in
    match p1 state with
    | (Ok x, state') -> (Ok x, state')
    | (Error _, _) ->
      (* Backtrack and try p2 *)
      let state' = { state with pos = start_pos } in
      p2 state'

(* Many combinator *)
let rec many p =
  (p >>= fun x ->
   many p >>= fun xs ->
   return (x :: xs))
  <|> return []

let many1 p =
  p >>= fun x ->
  many p >>= fun xs ->
  return (x :: xs)

(* Optional combinator *)
let optional p =
  (p >>| fun x -> Some x) <|> return None

(* Token operations *)
let current : ('tok, 'tok token option, 'err) parser =
  fun state ->
    if state.pos < List.length state.tokens then
      (Ok (Some (List.nth state.tokens state.pos)), state)
    else
      (Ok None, state)

let peek n : ('tok, 'tok token option, 'err) parser =
  fun state ->
    let pos = state.pos + n in
    if pos < List.length state.tokens then
      (Ok (Some (List.nth state.tokens pos)), state)
    else
      (Ok None, state)

let advance : ('tok, unit, 'err) parser =
  fun state ->
    if state.pos < List.length state.tokens then
      (Ok (), { state with pos = state.pos + 1 })
    else
      (Ok (), state)

let consume =
  fun state ->
    match current state with
    | (Ok (Some tok), state') -> 
      (match advance state' with
       | (Ok (), state'') -> (Ok tok, state'')
       | (Error e, state'') -> (Error e, state''))
    | (Ok None, state') -> 
      (Error (Obj.magic "Unexpected EOF"), state')
    | (Error e, state') -> (Error e, state')

(* Token matching *)
let satisfy pred err : ('tok, 'tok token, 'err) parser =
  current >>= function
  | Some tok when pred tok -> advance >>| fun () -> tok
  | _ -> fail err

let token kind err : ('tok, 'tok token, 'err) parser =
  satisfy (fun tok -> tok.kind = kind) err

let any_token : ('tok, 'tok token, 'err) parser =
  consume

(* Sequence combinators *)
let seq2 p1 p2 = 
  p1 >>= fun x1 ->
  p2 >>= fun x2 ->
  return (x1, x2)

let seq3 p1 p2 p3 =
  p1 >>= fun x1 ->
  p2 >>= fun x2 ->
  p3 >>= fun x3 ->
  return (x1, x2, x3)

let between open_p close_p p =
  open_p >>= fun _ ->
  p >>= fun x ->
  close_p >>= fun _ ->
  return x

let sep_by sep p =
  (p >>= fun x ->
   many (sep >>= fun _ -> p) >>= fun xs ->
   return (x :: xs))
  <|> return []

let sep_by1 sep p =
  p >>= fun x ->
  many (sep >>= fun _ -> p) >>= fun xs ->
  return (x :: xs)

(* Pratt parser types *)
type ('tok, 'expr, 'err) prefix_parser = 
  'tok token -> ('tok, 'expr, 'err) parser

type ('tok, 'expr, 'err) infix_parser = 
  'expr -> 'tok token -> ('tok, 'expr, 'err) parser

type precedence = int

type ('tok, 'expr, 'err) operator_table = {
  prefix: ('tok -> (('tok, 'expr, 'err) prefix_parser * precedence) option);
  infix: ('tok -> (('tok, 'expr, 'err) infix_parser * precedence * precedence) option);
}

(* Pratt expression parser *)
let rec pratt_expr table min_prec : ('tok, 'expr, 'err) parser =
  current >>= function
  | None -> fail (Obj.magic "Unexpected EOF in expression")
  | Some tok ->
    match table.prefix tok.kind with
    | None -> fail (Obj.magic ("No prefix parser for: " ^ tok.text))
    | Some (prefix_fn, _) ->
      consume >>= fun prefix_tok ->
      prefix_fn prefix_tok >>= fun left ->
      pratt_loop table left min_prec

and pratt_loop table left min_prec : ('tok, 'expr, 'err) parser =
  current >>= function
  | None -> return left
  | Some tok ->
    match table.infix tok.kind with
    | None -> return left
    | Some (infix_fn, prec, _right_prec) ->
      if prec < min_prec then
        return left
      else
        consume >>= fun op_tok ->
        infix_fn left op_tok >>= fun expr ->
        pratt_loop table expr min_prec

(* Helper to build operator tables *)
module Table = struct
  type ('tok, 'expr, 'err) t = ('tok, 'expr, 'err) operator_table

  let empty = {
    prefix = (fun _ -> None);
    infix = (fun _ -> None);
  }

  let add_prefix tok parser prec table = {
    table with
    prefix = fun t ->
      if t = tok then Some (parser, prec)
      else table.prefix t
  }

  let add_infix tok parser left_prec right_prec table = {
    table with
    infix = fun t ->
      if t = tok then Some (parser, left_prec, right_prec)
      else table.infix t
  }

  let add_left_assoc tok parser prec table =
    add_infix tok parser prec (prec + 1) table

  let add_right_assoc tok parser prec table =
    add_infix tok parser prec prec table
end

(* Run parser *)
let run_parser parser tokens =
  let initial_state = { tokens; pos = 0; errors = [] } in
  match parser initial_state with
  | (Ok result, _) -> Ok result
  | (Error e, state) -> Error (e :: state.errors)

(* Example usage helpers *)
let rec chainl1 p op =
  let rec rest x =
    (op >>= fun f ->
     p >>= fun y ->
     rest (f x y))
    <|> return x
  in
  p >>= rest

and chainr1 p op =
  p >>= fun x ->
  (op >>= fun f ->
   chainr1 p op >>= fun y ->
   return (f x y))
  <|> return x