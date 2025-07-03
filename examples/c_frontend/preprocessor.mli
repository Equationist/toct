(** C89/90 Preprocessor Interface *)

(** Preprocessor token type *)
type pp_token =
  | Identifier of string
  | Number of string
  | String of string
  | CharConst of string
  | Punctuator of string
  | Whitespace of string
  | Newline
  | Other of char
  | Eof

(** Source location *)
type location = {
  filename: string;
  line: int;
  column: int;
}

(** Token with location *)
type located_token = {
  token: pp_token;
  loc: location;
}

(** Macro definition *)
type macro_def =
  | ObjectLike of located_token list
  | FunctionLike of {
      params: string list;
      variadic: bool;
      replacement: located_token list;
    }

and conditional_state = {
  active: bool;
  seen_true_branch: bool;
  in_else: bool;
}

(** Preprocessor state *)
and pp_state = {
  macros: (string, macro_def * location) Hashtbl.t;
  included_files: string list;
  include_paths: string list;
  current_file: string;
  current_line: int;
  in_macro_expansion: string list;
  conditional_stack: conditional_state list;
  date_string: string;
  time_string: string;
}

(** Create initial preprocessor state *)
val create_pp_state : ?include_paths:string list -> string -> pp_state

(** Tokenizer module for testing *)
module Tokenizer : sig
  type t
  val create : string -> string -> t
  val next_token : t -> located_token
end

(** Expression evaluator for testing *)
module ExprEval : sig
  val evaluate : located_token list -> pp_state -> int
end

(** Preprocess a string *)
val preprocess_string : ?include_paths:string list -> filename:string -> string -> string

(** Preprocess a file *)
val preprocess_file : ?include_paths:string list -> string -> string