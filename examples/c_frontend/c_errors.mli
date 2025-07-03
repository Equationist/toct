(* C-specific error reporting interface *)

(* Error reporter type *)
type t

(* Create a new error reporter *)
val create : unit -> t

(* Error reporting functions *)
val parse_error : t -> Lexer.location -> string -> unit
val parse_error_token : t -> Lexer.location -> string -> Lexer.token -> unit
val lexer_error : t -> Lexer.location -> string -> unit
val preprocessor_error : t -> Lexer.location -> string -> unit
val warning : t -> Lexer.location -> string -> unit
val unused_symbol : t -> string -> 'a Compilerkit_frontend.Symbol_table.symbol -> unit

(* Error checking *)
val has_errors : t -> bool
val error_count : t -> int
val warning_count : t -> int

(* Format and print diagnostics *)
val print_diagnostics : t -> (string, string) Hashtbl.t -> unit