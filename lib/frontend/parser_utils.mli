(* Common parser utilities *)

open Position

(* Generic parser state *)
type ('token, 'state) parser_state = {
  tokens: 'token Token_location.located_token array;
  mutable pos: int;
  reporter: Error_reporter.t;
  custom_state: 'state;
}

(* Create parser state *)
val create_parser : 'token Token_location.located_token list -> Error_reporter.t -> 'state -> 
                    ('token, 'state) parser_state

(* Token operations *)
val at_end : ('token, 'state) parser_state -> bool
val peek : ('token, 'state) parser_state -> 'token Token_location.located_token option
val peek_token : ('token, 'state) parser_state -> 'token option
val peek_n : ('token, 'state) parser_state -> int -> 'token Token_location.located_token option
val current_span : ('token, 'state) parser_state -> span
val advance : ('token, 'state) parser_state -> unit
val check : ('token, 'state) parser_state -> 'token -> bool
val check_any : ('token, 'state) parser_state -> 'token list -> bool
val consume : ('token, 'state) parser_state -> 'token -> bool
val consume_any : ('token, 'state) parser_state -> 'token list -> 'token option

(* Error recovery *)
type 'token recovery_strategy =
  | SkipTo of 'token list
  | SkipToAny of 'token list list
  | SkipN of int
  | SkipToBalanced of 'token * 'token

val recover : ('token, 'state) parser_state -> 'token recovery_strategy -> unit

(* Parse helpers with error reporting *)
val expect : ('token, 'state) parser_state -> 'token -> ('token -> string) -> 
             ('token Token_location.located_token, unit) result

val expect_any : ('token, 'state) parser_state -> 'token list -> ('token -> string) -> 
                 ('token Token_location.located_token, unit) result

(* Bracketed parsing *)
val parse_bracketed : ('token, 'state) parser_state -> 'token -> 'token -> 
                      ('token -> string) -> (('token, 'state) parser_state -> ('a, unit) result) -> 
                      ('a * span, unit) result

(* List parsing *)
val parse_list : ('token, 'state) parser_state -> 
                 (('token, 'state) parser_state -> ('a, unit) result) -> 
                 'token -> ('token -> string) -> bool -> 'a list

val parse_separated_list : ('token, 'state) parser_state -> 
                          (('token, 'state) parser_state -> ('a, unit) result) -> 
                          'token -> ('token -> string) -> 'a list