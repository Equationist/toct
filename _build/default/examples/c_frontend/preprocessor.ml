(** C89/90 Preprocessor Implementation
    
    This module implements a C preprocessor that handles:
    - Macro definitions (#define, #undef)
    - File inclusion (#include)
    - Conditional compilation (#if, #ifdef, #ifndef, #else, #elif, #endif)
    - Line control (#line)
    - Error directive (#error)
    - Pragma directive (#pragma)
    - Predefined macros (__FILE__, __LINE__, __DATE__, __TIME__, __STDC__)
    - Stringification (#) and token pasting (##)
*)

(* open Compilerkit_common *)

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

(** Macro parameter *)
type macro_param = string

(** Macro definition *)
type macro_def =
  | ObjectLike of located_token list  (* replacement tokens *)
  | FunctionLike of {
      params: macro_param list;
      variadic: bool;  (* true if last param is ... *)
      replacement: located_token list;
    }

(** Preprocessor directive *)
type directive =
  | Define of string * macro_def
  | Undef of string
  | Include of include_type * string
  | If of string  (* condition expression *)
  | Ifdef of string
  | Ifndef of string
  | Else
  | Elif of string
  | Endif
  | Line of int * string option
  | Error of string
  | Pragma of string
  | NullDirective

and include_type = System | User

(** Preprocessor state *)
type pp_state = {
  macros: (string, macro_def * location) Hashtbl.t;
  included_files: string list;  (* for circular include detection *)
  include_paths: string list;   (* search paths for #include *)
  current_file: string;
  current_line: int;
  in_macro_expansion: string list;  (* prevents recursive expansion *)
  conditional_stack: conditional_state list;
  date_string: string;  (* __DATE__ *)
  time_string: string;  (* __TIME__ *)
}

and conditional_state = {
  active: bool;           (* current branch is active *)
  seen_true_branch: bool; (* already seen a true branch *)
  in_else: bool;          (* in #else part *)
}

(** Create initial preprocessor state *)
let create_pp_state ?(include_paths=[]) filename =
  let now = Unix.localtime (Unix.time ()) in
  let months = [| "Jan"; "Feb"; "Mar"; "Apr"; "May"; "Jun";
                  "Jul"; "Aug"; "Sep"; "Oct"; "Nov"; "Dec" |] in
  let date_string = Printf.sprintf "\"%s %2d %4d\"" 
    months.(now.Unix.tm_mon) now.Unix.tm_mday (now.Unix.tm_year + 1900) in
  let time_string = Printf.sprintf "\"%02d:%02d:%02d\""
    now.Unix.tm_hour now.Unix.tm_min now.Unix.tm_sec in
  
  let state = {
    macros = Hashtbl.create 64;
    included_files = [];
    include_paths = include_paths;
    current_file = filename;
    current_line = 1;
    in_macro_expansion = [];
    conditional_stack = [];
    date_string;
    time_string;
  } in
  
  (* Add predefined macros *)
  let loc = { filename = "<builtin>"; line = 0; column = 0 } in
  Hashtbl.add state.macros "__STDC__" (ObjectLike [{ token = Number "1"; loc }], loc);
  state

(** Tokenizer for preprocessor *)
module Tokenizer = struct
  type t = {
    input: string;
    mutable pos: int;
    mutable line: int;
    mutable column: int;
    filename: string;
  }

  let create filename input = {
    input;
    pos = 0;
    line = 1;
    column = 1;
    filename;
  }

  let current_loc t = {
    filename = t.filename;
    line = t.line;
    column = t.column;
  }

  let peek t =
    if t.pos >= String.length t.input then None
    else Some t.input.[t.pos]

  let advance t =
    if t.pos < String.length t.input then begin
      if t.input.[t.pos] = '\n' then begin
        t.line <- t.line + 1;
        t.column <- 1
      end else
        t.column <- t.column + 1;
      t.pos <- t.pos + 1
    end

  let peek_next t n =
    let pos = t.pos + n in
    if pos >= String.length t.input then None
    else Some t.input.[pos]

  (* Skip whitespace except newlines *)
  let skip_whitespace t =
    let buf = Buffer.create 16 in
    let start_loc = current_loc t in
    while match peek t with
    | Some (' ' | '\t' | '\r') -> 
        Buffer.add_char buf (Option.get (peek t));
        advance t; true
    | _ -> false
    do () done;
    if Buffer.length buf > 0 then
      Some { token = Whitespace (Buffer.contents buf); loc = start_loc }
    else
      None

  (* Read identifier or keyword *)
  let read_identifier t =
    let buf = Buffer.create 32 in
    let start_loc = current_loc t in
    while match peek t with
    | Some (('a'..'z' | 'A'..'Z' | '_' | '0'..'9') as c) ->
        Buffer.add_char buf c;
        advance t;
        true
    | _ -> false
    do () done;
    { token = Identifier (Buffer.contents buf); loc = start_loc }

  (* Read number (simplified - doesn't handle all C number formats) *)
  let read_number t =
    let buf = Buffer.create 32 in
    let start_loc = current_loc t in
    
    (* Handle hex numbers *)
    if peek t = Some '0' && (peek_next t 1 = Some 'x' || peek_next t 1 = Some 'X') then begin
      Buffer.add_char buf '0';
      advance t;
      Buffer.add_char buf (Option.get (peek t));
      advance t;
      while match peek t with
      | Some (('0'..'9' | 'a'..'f' | 'A'..'F') as c) ->
          Buffer.add_char buf c;
          advance t;
          true
      | _ -> false
      do () done
    end else begin
      (* Read integer part *)
      while match peek t with
      | Some ('0'..'9' as c) ->
          Buffer.add_char buf c;
          advance t;
          true
      | _ -> false
      do () done;
      
      (* Read decimal part if present *)
      if peek t = Some '.' && 
         (match peek_next t 1 with Some '0'..'9' -> true | _ -> false) then begin
        Buffer.add_char buf '.';
        advance t;
        while match peek t with
        | Some ('0'..'9' as c) ->
            Buffer.add_char buf c;
            advance t;
            true
        | _ -> false
        do () done
      end;
    end;
    
    (* Read exponent if present *)
    if peek t = Some 'e' || peek t = Some 'E' then begin
      Buffer.add_char buf (Option.get (peek t));
      advance t;
      if peek t = Some '+' || peek t = Some '-' then begin
        Buffer.add_char buf (Option.get (peek t));
        advance t
      end;
      while match peek t with
      | Some ('0'..'9' as c) ->
          Buffer.add_char buf c;
          advance t;
          true
      | _ -> false
      do () done
    end;
    
    (* Read suffix if present *)
    while match peek t with
    | Some (('u' | 'U' | 'l' | 'L' | 'f' | 'F') as c) ->
        Buffer.add_char buf c;
        advance t;
        true
    | _ -> false
    do () done;
    
    { token = Number (Buffer.contents buf); loc = start_loc }

  (* Read string literal *)
  let read_string t =
    let buf = Buffer.create 64 in
    let start_loc = current_loc t in
    let quote = Option.get (peek t) in (* double quote or single quote *)
    Buffer.add_char buf quote;
    advance t;
    
    let rec loop () =
      match peek t with
      | None -> failwith "Unterminated string literal"
      | Some '\\' ->
          Buffer.add_char buf '\\';
          advance t;
          (match peek t with
           | None -> failwith "Unterminated string literal"
           | Some c ->
               Buffer.add_char buf c;
               advance t;
               loop ())
      | Some c when c = quote ->
          Buffer.add_char buf c;
          advance t
      | Some c ->
          Buffer.add_char buf c;
          advance t;
          loop ()
    in
    loop ();
    
    if quote = '"' then
      { token = String (Buffer.contents buf); loc = start_loc }
    else
      { token = CharConst (Buffer.contents buf); loc = start_loc }

  (* Read C-style comment *)
  let read_c_comment t =
    let start_loc = current_loc t in
    advance t; (* skip / *)
    advance t; (* skip * *)
    
    let rec loop () =
      match peek t with
      | None -> failwith "Unterminated comment"
      | Some '*' when peek_next t 1 = Some '/' ->
          advance t; advance t
      | Some _ ->
          advance t;
          loop ()
    in
    loop ();
    
    (* Comments are replaced by a single space *)
    { token = Whitespace " "; loc = start_loc }

  (* Read C++ style comment *)
  let read_cpp_comment t =
    let start_loc = current_loc t in
    advance t; (* skip / *)
    advance t; (* skip / *)
    
    while match peek t with
    | Some '\n' | None -> false
    | Some _ -> advance t; true
    do () done;
    
    (* Comments are replaced by a single space *)
    { token = Whitespace " "; loc = start_loc }

  (* Read punctuator *)
  let read_punctuator t =
    let start_loc = current_loc t in
    let c = Option.get (peek t) in
    advance t;
    
    let two_char = match c, peek t with
      | '<', Some '<' -> advance t; Some "<<"
      | '>', Some '>' -> advance t; Some ">>"
      | '<', Some '=' -> advance t; Some "<="
      | '>', Some '=' -> advance t; Some ">="
      | '=', Some '=' -> advance t; Some "=="
      | '!', Some '=' -> advance t; Some "!="
      | '&', Some '&' -> advance t; Some "&&"
      | '|', Some '|' -> advance t; Some "||"
      | '+', Some '+' -> advance t; Some "++"
      | '-', Some '-' -> advance t; Some "--"
      | '-', Some '>' -> advance t; Some "->"
      | '#', Some '#' -> advance t; Some "##"
      | '.', Some '.' when peek_next t 1 = Some '.' ->
          advance t; advance t; Some "..."
      | _ -> None
    in
    
    let token_str = match two_char with
      | Some s -> s
      | None -> String.make 1 c
    in
    
    { token = Punctuator token_str; loc = start_loc }

  (* Get next token *)
  let rec next_token t =
    match peek t with
    | None -> { token = Eof; loc = current_loc t }
    | Some '\n' ->
        let loc = current_loc t in
        advance t;
        { token = Newline; loc }
    | Some (' ' | '\t' | '\r') ->
        (match skip_whitespace t with
         | Some tok -> tok
         | None -> next_token t)
    | Some ('a'..'z' | 'A'..'Z' | '_') ->
        read_identifier t
    | Some ('0'..'9') ->
        read_number t
    | Some '"' | Some '\'' ->
        read_string t
    | Some '/' when peek_next t 1 = Some '*' ->
        let tok = read_c_comment t in
        tok
    | Some '/' when peek_next t 1 = Some '/' ->
        let tok = read_cpp_comment t in
        tok
    | Some c when String.contains "+-*/%<>=!&|^~?:;,()[]{}#." c ->
        read_punctuator t
    | Some c ->
        let loc = current_loc t in
        advance t;
        { token = Other c; loc }
end

(** Expression evaluator for #if conditions *)
module ExprEval = struct
  (* Simple recursive descent parser for constant expressions *)
  
  (* type value = int *)
  
  (* Token stream for expression parsing *)
  type token_stream = {
    mutable tokens: located_token list;
    pp_state: pp_state;
  }
  
  let peek stream =
    match stream.tokens with
    | [] -> None
    | tok :: _ -> Some tok
    
  let advance stream =
    match stream.tokens with
    | [] -> ()
    | _ :: rest -> stream.tokens <- rest
  
  (* Check if identifier is defined *)
  let is_defined stream name =
    Hashtbl.mem stream.pp_state.macros name
  
  (* Primary expression *)
  let rec parse_primary stream =
    match peek stream with
    | None -> failwith "Unexpected end of expression"
    | Some { token = Number n; _ } ->
        advance stream;
        int_of_string n
    | Some { token = Identifier "defined"; _ } ->
        advance stream;
        (* Handle defined(X) or defined X *)
        let name = match peek stream with
          | Some { token = Punctuator "("; _ } ->
              advance stream;
              let name = match peek stream with
                | Some { token = Identifier id; _ } -> id
                | _ -> failwith "Expected identifier after 'defined('"
              in
              advance stream;
              (match peek stream with
               | Some { token = Punctuator ")"; _ } -> advance stream
               | _ -> failwith "Expected ')' after identifier");
              name
          | Some { token = Identifier id; _ } ->
              advance stream;
              id
          | _ -> failwith "Expected identifier after 'defined'"
        in
        if is_defined stream name then 1 else 0
    | Some { token = Identifier _; _ } ->
        advance stream;
        (* Undefined identifiers evaluate to 0 *)
        0
    | Some { token = Punctuator "("; _ } ->
        advance stream;
        let v = parse_or stream in
        (match peek stream with
         | Some { token = Punctuator ")"; _ } -> advance stream
         | _ -> failwith "Expected ')'");
        v
    | Some { token = Punctuator "!"; _ } ->
        advance stream;
        let v = parse_unary stream in
        if v = 0 then 1 else 0
    | Some { token = Punctuator "-"; _ } ->
        advance stream;
        let v = parse_unary stream in
        -v
    | Some { token = Punctuator "+"; _ } ->
        advance stream;
        parse_unary stream
    | _ -> failwith "Expected primary expression"
  
  and parse_unary stream = parse_primary stream
  
  (* Multiplicative: * / % *)
  and parse_multiplicative stream =
    let rec loop left =
      match peek stream with
      | Some { token = Punctuator "*"; _ } ->
          advance stream;
          let right = parse_unary stream in
          loop (left * right)
      | Some { token = Punctuator "/"; _ } ->
          advance stream;
          let right = parse_unary stream in
          if right = 0 then failwith "Division by zero"
          else loop (left / right)
      | Some { token = Punctuator "%"; _ } ->
          advance stream;
          let right = parse_unary stream in
          if right = 0 then failwith "Division by zero"
          else loop (left mod right)
      | _ -> left
    in
    loop (parse_unary stream)
  
  (* Additive: + - *)
  and parse_additive stream =
    let rec loop left =
      match peek stream with
      | Some { token = Punctuator "+"; _ } ->
          advance stream;
          let right = parse_multiplicative stream in
          loop (left + right)
      | Some { token = Punctuator "-"; _ } ->
          advance stream;
          let right = parse_multiplicative stream in
          loop (left - right)
      | _ -> left
    in
    loop (parse_multiplicative stream)
  
  (* Relational: < > <= >= *)
  and parse_relational stream =
    let rec loop left =
      match peek stream with
      | Some { token = Punctuator "<"; _ } ->
          advance stream;
          let right = parse_additive stream in
          loop (if left < right then 1 else 0)
      | Some { token = Punctuator ">"; _ } ->
          advance stream;
          let right = parse_additive stream in
          loop (if left > right then 1 else 0)
      | Some { token = Punctuator "<="; _ } ->
          advance stream;
          let right = parse_additive stream in
          loop (if left <= right then 1 else 0)
      | Some { token = Punctuator ">="; _ } ->
          advance stream;
          let right = parse_additive stream in
          loop (if left >= right then 1 else 0)
      | _ -> left
    in
    loop (parse_additive stream)
  
  (* Equality: == != *)
  and parse_equality stream =
    let rec loop left =
      match peek stream with
      | Some { token = Punctuator "=="; _ } ->
          advance stream;
          let right = parse_relational stream in
          loop (if left = right then 1 else 0)
      | Some { token = Punctuator "!="; _ } ->
          advance stream;
          let right = parse_relational stream in
          loop (if left <> right then 1 else 0)
      | _ -> left
    in
    loop (parse_relational stream)
  
  (* Logical AND: && *)
  and parse_and stream =
    let rec loop left =
      match peek stream with
      | Some { token = Punctuator "&&"; _ } ->
          advance stream;
          let right = parse_equality stream in
          loop (if left <> 0 && right <> 0 then 1 else 0)
      | _ -> left
    in
    loop (parse_equality stream)
  
  (* Logical OR: || *)
  and parse_or stream =
    let rec loop left =
      match peek stream with
      | Some { token = Punctuator "||"; _ } ->
          advance stream;
          let right = parse_and stream in
          loop (if left <> 0 || right <> 0 then 1 else 0)
      | _ -> left
    in
    loop (parse_and stream)
  
  (* Evaluate expression from token list *)
  let evaluate tokens pp_state =
    let stream = { tokens; pp_state } in
    let result = parse_or stream in
    match stream.tokens with
    | [] -> result
    | _ -> failwith "Extra tokens after expression"
end

(** Check if current conditional branch is active *)
let is_active state =
  List.for_all (fun cs -> cs.active) state.conditional_stack

(** List utility functions for compatibility *)
module ListUtil = struct
  let rec take n lst =
    if n <= 0 then []
    else match lst with
    | [] -> []
    | h :: t -> h :: take (n - 1) t
  
  let rec drop n lst =
    if n <= 0 then lst
    else match lst with
    | [] -> []
    | _ :: t -> drop (n - 1) t
end

(** Macro expansion *)
module MacroExpander = struct
  (* Check if token is a macro that should be expanded *)
  let should_expand state tok =
    match tok.token with
    | Identifier name ->
        Hashtbl.mem state.macros name &&
        not (List.mem name state.in_macro_expansion)
    | _ -> false
  
  (* Stringify a list of tokens *)
  let stringify tokens =
    let buf = Buffer.create 64 in
    Buffer.add_char buf '"';
    
    let rec loop = function
      | [] -> ()
      | { token; _ } :: rest ->
          (match token with
           | Identifier s | Number s | Punctuator s ->
               Buffer.add_string buf s
           | Other c ->
               Buffer.add_char buf c
           | String s | CharConst s ->
               (* Escape quotes and backslashes *)
               String.iter (fun c ->
                 if c = '"' || c = '\\' then Buffer.add_char buf '\\';
                 Buffer.add_char buf c
               ) s
           | Whitespace _ -> Buffer.add_char buf ' '
           | Newline -> Buffer.add_char buf ' '
           | Eof -> ());
          if rest <> [] then Buffer.add_char buf ' ';
          loop rest
    in
    loop tokens;
    
    Buffer.add_char buf '"';
    { token = String (Buffer.contents buf); 
      loc = (match tokens with [] -> {filename=""; line=0; column=0} | h::_ -> h.loc) }
  
  (* Concatenate two tokens *)
  let concat_tokens tok1 tok2 =
    let str1 = match tok1.token with
      | Identifier s | Number s | Punctuator s -> s
      | String s | CharConst s -> String.sub s 1 (String.length s - 2) (* remove quotes *)
      | Other c -> String.make 1 c
      | _ -> ""
    in
    let str2 = match tok2.token with
      | Identifier s | Number s | Punctuator s -> s
      | String s | CharConst s -> String.sub s 1 (String.length s - 2)
      | Other c -> String.make 1 c
      | _ -> ""
    in
    let combined = str1 ^ str2 in
    
    (* Re-tokenize the combined string *)
    let tokenizer = Tokenizer.create tok1.loc.filename combined in
    Tokenizer.next_token tokenizer
  
  (* Expand object-like macro *)
  let expand_object state name replacement _loc =
    let new_state = { state with 
      in_macro_expansion = name :: state.in_macro_expansion 
    } in
    
    (* Recursively expand tokens in replacement *)
    let rec expand_tokens = function
      | [] -> []
      | tok :: rest ->
          if should_expand new_state tok then
            match tok.token with
            | Identifier macro_name ->
                let (macro_def, _) = Hashtbl.find new_state.macros macro_name in
                (match macro_def with
                 | ObjectLike repl -> expand_tokens (repl @ rest)
                 | FunctionLike _ -> tok :: expand_tokens rest)
            | _ -> tok :: expand_tokens rest
          else
            tok :: expand_tokens rest
    in
    
    expand_tokens replacement
  
  (* Collect macro arguments - assumes opening '(' is already consumed *)
  let collect_arguments tokens =
    (* Skip whitespace *)
    let rec skip_ws = function
      | { token = Whitespace _; _ } :: rest -> skip_ws rest  
      | tokens -> tokens
    in
    
    (* Collect tokens for one argument *)
    let rec collect_arg depth acc tokens =
      match tokens with
      | [] -> failwith "Unterminated macro arguments"
      | { token = Punctuator "("; _ } as tok :: rest ->
          (* Nested parenthesis - increase depth *)
          collect_arg (depth + 1) (tok :: acc) rest
      | { token = Punctuator ")"; _ } :: rest when depth = 0 ->
          (* Found closing paren at depth 0 - end of this argument and all arguments *)
          (List.rev acc, [], rest)
      | { token = Punctuator ")"; _ } as tok :: rest ->
          (* Nested closing paren - decrease depth *)
          collect_arg (depth - 1) (tok :: acc) rest
      | { token = Punctuator ","; _ } :: rest when depth = 0 ->
          (* Found comma at depth 0 - end of this argument, more arguments follow *)
          (List.rev acc, skip_ws rest, [])
      | tok :: rest ->
          (* Regular token - add to current argument *)
          collect_arg depth (tok :: acc) rest
    in
    
    (* Collect all arguments *)
    let rec collect_all acc tokens =
      let tokens = skip_ws tokens in
      match tokens with
      | [] -> failwith "Unterminated macro arguments"
      | { token = Punctuator ")"; _ } :: rest ->
          (* Empty args or trailing comma case *)
          (List.rev acc, rest)
      | _ ->
          let (arg, more_tokens, final_rest) = collect_arg 0 [] tokens in
          if more_tokens = [] && final_rest <> [] then
            (* Found closing paren - we're done *)
            (List.rev (arg :: acc), final_rest)
          else
            (* More arguments to collect *)
            collect_all (arg :: acc) more_tokens
    in
    
    collect_all [] tokens
  
  (* Substitute macro parameters in replacement list *)
  let substitute_params params args replacement =
    let param_map = 
      try List.combine params args
      with Invalid_argument _ -> failwith "Wrong number of macro arguments"
    in
    
    let rec subst prev = function
      | [] -> List.rev prev
      | { token = Punctuator "#"; _ } :: { token = Punctuator "#"; _ } :: 
        { token = Identifier "__VA_ARGS__"; _ } :: rest ->
          (* Special case for ##__VA_ARGS__ - if VA_ARGS is empty, remove preceding comma *)
          (match List.assoc_opt "__VA_ARGS__" param_map with
           | Some [] | None ->
               (* Remove preceding comma if present *)
               let prev' = match prev with
                 | { token = Punctuator ","; _ } :: rest -> rest
                 | _ -> prev
               in
               subst prev' rest
           | Some arg_tokens ->
               subst (List.rev_append arg_tokens prev) rest)
      | { token = Punctuator "#"; _ } :: { token = Identifier param; _ } :: rest
        when List.mem_assoc param param_map ->
          (* Stringification *)
          let arg_tokens = List.assoc param param_map in
          subst (stringify arg_tokens :: prev) rest
      | { token = Identifier param; _ } :: { token = Punctuator "##"; _ } :: rest
        when List.mem_assoc param param_map ->
          (* Token pasting - left side *)
          let arg_tokens = List.assoc param param_map in
          (match arg_tokens with
           | [] -> subst prev rest
           | _ ->
               let left = List.hd (List.rev arg_tokens) in
               (match rest with
                | { token = Identifier param2; _ } :: rest2
                  when List.mem_assoc param2 param_map ->
                    let arg2_tokens = List.assoc param2 param_map in
                    (match arg2_tokens with
                     | [] -> subst (left :: prev) rest2
                     | right :: _ ->
                         subst (concat_tokens left right :: prev) rest2)
                | right :: rest2 ->
                    subst (concat_tokens left right :: prev) rest2
                | [] -> subst (left :: prev) []))
      | tok :: { token = Punctuator "##"; _ } :: rest ->
          (* Token pasting - general case *)
          (match rest with
           | [] -> List.rev (tok :: prev)
           | { token = Identifier param; _ } :: rest2
             when List.mem_assoc param param_map ->
               let arg_tokens = List.assoc param param_map in
               (match arg_tokens with
                | [] -> subst (tok :: prev) rest2
                | right :: _ ->
                    subst (concat_tokens tok right :: prev) rest2)
           | right :: rest2 ->
               subst (concat_tokens tok right :: prev) rest2)
      | { token = Identifier param; _ } :: rest
        when List.mem_assoc param param_map ->
          (* Parameter substitution *)
          let arg_tokens = List.assoc param param_map in
          subst (List.rev_append arg_tokens prev) rest
      | tok :: rest ->
          subst (tok :: prev) rest
    in
    
    subst [] replacement
  
  (* Expand function-like macro *)
  let expand_function state name params variadic replacement args _loc =
    (* Check argument count *)
    let expected = List.length params in
    let provided = List.length args in
    if not variadic && provided <> expected then
      failwith (Printf.sprintf "Macro %s expects %d arguments, got %d" 
                name expected provided);
    if variadic && provided < expected - 1 then
      failwith (Printf.sprintf "Variadic macro %s expects at least %d arguments, got %d"
                name (expected - 1) provided);
    
    (* Handle variadic macros *)
    let params, args =
      if variadic && List.length params > 0 then
        let regular_params = List.rev (List.tl (List.rev params)) in
        let regular_args = ListUtil.take (List.length regular_params) args in
        let va_args = ListUtil.drop (List.length regular_params) args in
        let va_args_combined = List.concat va_args in
        (params, regular_args @ [va_args_combined])
      else
        (params, args)
    in
    
    (* Substitute parameters *)
    let substituted = substitute_params params args replacement in
    
    (* Recursively expand macros in result *)
    let new_state = { state with 
      in_macro_expansion = name :: state.in_macro_expansion 
    } in
    
    let rec expand_tokens = function
      | [] -> []
      | tok :: rest ->
          if should_expand new_state tok then
            match tok.token with
            | Identifier macro_name ->
                let (macro_def, _) = Hashtbl.find new_state.macros macro_name in
                (match macro_def with
                 | ObjectLike repl -> expand_tokens (repl @ rest)
                 | FunctionLike _ -> tok :: expand_tokens rest)
            | _ -> tok :: expand_tokens rest
          else
            tok :: expand_tokens rest
    in
    
    expand_tokens substituted
end

(** Directive parser *)
module DirectiveParser = struct
  (* Skip whitespace tokens *)
  let rec skip_ws = function
    | { token = Whitespace _; _ } :: rest -> skip_ws rest
    | tokens -> tokens
  
  (* Take tokens until newline *)
  let take_until_newline tokens =
    let rec loop acc = function
      | [] -> (List.rev acc, [])
      | { token = Newline; _ } :: rest -> (List.rev acc, rest)
      | tok :: rest -> loop (tok :: acc) rest
    in
    loop [] tokens
  
  (* Parse #define directive *)
  let parse_define tokens =
    let tokens = skip_ws tokens in
    match tokens with
    | { token = Identifier name; _ } :: rest ->
        let rest = match rest with
          | { token = Punctuator "("; _ } :: rest2 ->
              (* Function-like macro *)
              let rec parse_params acc = function
                | { token = Identifier param; _ } :: 
                  { token = Punctuator ","; _ } :: rest ->
                    parse_params (param :: acc) (skip_ws rest)
                | { token = Identifier param; _ } :: 
                  { token = Punctuator ")"; _ } :: rest ->
                    (List.rev (param :: acc), false, rest)
                | { token = Punctuator "..."; _ } ::
                  { token = Punctuator ")"; _ } :: rest ->
                    (List.rev ("__VA_ARGS__" :: acc), true, rest)
                | { token = Punctuator ")"; _ } :: rest ->
                    ([], false, rest)
                | _ -> failwith "Invalid macro parameter list"
              in
              let (params, variadic, rest) = parse_params [] (skip_ws rest2) in
              let (replacement, rest) = take_until_newline rest in
              (name, FunctionLike { params; variadic; replacement }, rest)
          | _ ->
              (* Object-like macro *)
              let (replacement, rest) = take_until_newline rest in
              (name, ObjectLike replacement, rest)
        in
        rest
    | _ -> failwith "Expected identifier after #define"
  
  (* Parse #include directive *)
  let parse_include tokens =
    let tokens = skip_ws tokens in
    match tokens with
    | { token = String filename; _ } :: rest ->
        let filename = String.sub filename 1 (String.length filename - 2) in
        (User, filename, snd (take_until_newline rest))
    | { token = Punctuator "<"; _ } :: rest ->
        let rec collect acc = function
          | { token = Punctuator ">"; _ } :: rest ->
              (System, String.concat "" (List.rev acc), snd (take_until_newline rest))
          | { token = Identifier s; _ } :: rest ->
              collect (s :: acc) rest
          | { token = Punctuator s; _ } :: rest ->
              collect (s :: acc) rest
          | _ -> failwith "Invalid #include syntax"
        in
        collect [] rest
    | _ -> failwith "Expected filename after #include"
  
  (* Parse directive from tokens *)
  let parse_directive tokens =
    let tokens = skip_ws tokens in
    match tokens with
    | { token = Identifier "define"; _ } :: rest ->
        let (name, def, rest) = parse_define rest in
        (Define (name, def), rest)
    | { token = Identifier "undef"; _ } :: rest ->
        let rest = skip_ws rest in
        (match rest with
         | { token = Identifier name; _ } :: rest ->
             (Undef name, snd (take_until_newline rest))
         | _ -> failwith "Expected identifier after #undef")
    | { token = Identifier "include"; _ } :: rest ->
        let (inc_type, filename, rest) = parse_include rest in
        (Include (inc_type, filename), rest)
    | { token = Identifier "if"; _ } :: rest ->
        let (expr_tokens, rest) = take_until_newline rest in
        (If (String.concat " " (List.map (fun t -> 
          match t.token with
          | Identifier s | Number s | Punctuator s -> s
          | _ -> " ") expr_tokens)), rest)
    | { token = Identifier "ifdef"; _ } :: rest ->
        let rest = skip_ws rest in
        (match rest with
         | { token = Identifier name; _ } :: rest ->
             (Ifdef name, snd (take_until_newline rest))
         | _ -> failwith "Expected identifier after #ifdef")
    | { token = Identifier "ifndef"; _ } :: rest ->
        let rest = skip_ws rest in
        (match rest with
         | { token = Identifier name; _ } :: rest ->
             (Ifndef name, snd (take_until_newline rest))
         | _ -> failwith "Expected identifier after #ifndef")
    | { token = Identifier "else"; _ } :: rest ->
        (Else, snd (take_until_newline rest))
    | { token = Identifier "elif"; _ } :: rest ->
        let (expr_tokens, rest) = take_until_newline rest in
        (Elif (String.concat " " (List.map (fun t ->
          match t.token with
          | Identifier s | Number s | Punctuator s -> s
          | _ -> " ") expr_tokens)), rest)
    | { token = Identifier "endif"; _ } :: rest ->
        (Endif, snd (take_until_newline rest))
    | { token = Identifier "error"; _ } :: rest ->
        let (msg_tokens, rest) = take_until_newline rest in
        let msg = String.concat "" (List.map (fun t ->
          match t.token with
          | Identifier s | Number s | String s | Punctuator s -> s
          | Whitespace s -> s
          | _ -> "") msg_tokens) in
        (Error msg, rest)
    | { token = Identifier "pragma"; _ } :: rest ->
        let (pragma_tokens, rest) = take_until_newline rest in
        let pragma = String.concat " " (List.map (fun t ->
          match t.token with
          | Identifier s | Number s | String s | Punctuator s -> s
          | _ -> "") pragma_tokens) in
        (Pragma pragma, rest)
    | { token = Identifier "line"; _ } :: rest ->
        let rest = skip_ws rest in
        (match rest with
         | { token = Number n; _ } :: rest ->
             let line = int_of_string n in
             let rest = skip_ws rest in
             let (filename, rest) = match rest with
               | { token = String f; _ } :: rest ->
                   (Some (String.sub f 1 (String.length f - 2)), rest)
               | _ -> (None, rest)
             in
             (Line (line, filename), snd (take_until_newline rest))
         | _ -> failwith "Expected line number after #line")
    | [] | { token = Newline; _ } :: _ -> (NullDirective, tokens)
    | _ -> failwith "Unknown preprocessor directive"
end

(** Main preprocessor *)
let rec process_tokens state tokens =
  match tokens with
  | [] -> []
  | { token = Punctuator "#"; loc } :: rest when loc.column = 1 ->
      (* Preprocessor directive *)
      (* Always parse directives - some need to be processed even in inactive branches *)
      let (directive, rest) = DirectiveParser.parse_directive rest in
      (* Check if we should process this directive *)
      (match directive with
       | Ifdef _ | Ifndef _ | If _ | Else | Elif _ | Endif ->
           (* These directives affect conditional state, always process them *)
           process_directive state directive rest
       | _ ->
           (* Other directives are only processed in active branches *)
           if is_active state then
             process_directive state directive rest
           else
             process_tokens state rest)
  | tok :: rest ->
      if is_active state then
        (* Check for macro expansion *)
        if MacroExpander.should_expand state tok then
          match tok.token with
          | Identifier "__FILE__" ->
              (* Special handling for __FILE__ *)
              let file_tok = { 
                token = String (Printf.sprintf "\"%s\"" state.current_file); 
                loc = tok.loc 
              } in
              file_tok :: process_tokens state rest
          | Identifier "__LINE__" ->
              (* Special handling for __LINE__ *)
              let line_tok = { 
                token = Number (string_of_int tok.loc.line); 
                loc = tok.loc 
              } in
              line_tok :: process_tokens state rest
          | Identifier "__DATE__" ->
              (* Special handling for __DATE__ *)
              let date_tok = { token = String state.date_string; loc = tok.loc } in
              date_tok :: process_tokens state rest
          | Identifier "__TIME__" ->
              (* Special handling for __TIME__ *)
              let time_tok = { token = String state.time_string; loc = tok.loc } in
              time_tok :: process_tokens state rest
          | Identifier name ->
              let (macro_def, _def_loc) = Hashtbl.find state.macros name in
              (match macro_def with
               | ObjectLike replacement ->
                   let expanded = MacroExpander.expand_object state name replacement tok.loc in
                   process_tokens state (expanded @ rest)
               | FunctionLike { params; variadic; replacement } ->
                   (* Look for opening parenthesis *)
                   let rest = DirectiveParser.skip_ws rest in
                   (match rest with
                    | { token = Punctuator "("; _ } :: rest2 ->
                        let (args, rest3) = MacroExpander.collect_arguments rest2 in
                        let expanded = MacroExpander.expand_function 
                          state name params variadic replacement args tok.loc in
                        process_tokens state (expanded @ rest3)
                    | _ ->
                        (* Not a function call, just an identifier *)
                        tok :: process_tokens state rest))
          | _ -> tok :: process_tokens state rest
        else
          (* Handle special predefined macros that aren't in the macro table *)
          match tok.token with
          | Identifier "__FILE__" ->
              let file_tok = { 
                token = String (Printf.sprintf "\"%s\"" state.current_file); 
                loc = tok.loc 
              } in
              file_tok :: process_tokens state rest
          | Identifier "__LINE__" ->
              let line_tok = { 
                token = Number (string_of_int tok.loc.line); 
                loc = tok.loc 
              } in
              line_tok :: process_tokens state rest
          | Identifier "__DATE__" ->
              let date_tok = { token = String state.date_string; loc = tok.loc } in
              date_tok :: process_tokens state rest
          | Identifier "__TIME__" ->
              let time_tok = { token = String state.time_string; loc = tok.loc } in
              time_tok :: process_tokens state rest
          | _ -> tok :: process_tokens state rest
      else
        (* In false conditional branch, skip non-directive tokens *)
        process_tokens state rest

and process_directive state directive rest =
  match directive with
  | Define (name, macro_def) ->
      Hashtbl.add state.macros name (macro_def, 
        { filename = state.current_file; 
          line = state.current_line; 
          column = 0 });
      process_tokens state rest
  
  | Undef name ->
      Hashtbl.remove state.macros name;
      process_tokens state rest
  
  | Include (inc_type, filename) ->
      (* Check for circular includes *)
      if List.mem filename state.included_files then
        failwith (Printf.sprintf "Circular include detected: %s" filename);
      
      (* Find the file *)
      let search_paths = 
        match inc_type with
        | User -> 
            (* For user includes, search current directory first *)
            Filename.dirname state.current_file :: state.include_paths
        | System -> state.include_paths
      in
      
      let rec find_file = function
        | [] -> None
        | path :: rest ->
            let full_path = 
              if Filename.is_relative filename then
                Filename.concat path filename
              else
                filename
            in
            if Sys.file_exists full_path then
              Some full_path
            else
              find_file rest
      in
      
      (match find_file search_paths with
       | None -> 
           (* Skip missing includes for now - in real implementation would error *)
           process_tokens state rest
       | Some full_path ->
           (* Read and process the included file *)
           let content = 
             try
               let ic = open_in full_path in
               let content = really_input_string ic (in_channel_length ic) in
               close_in ic;
               content
             with _ -> ""
           in
           
           (* Tokenize included file *)
           let tokenizer = Tokenizer.create full_path content in
           let rec collect_tokens acc =
             match Tokenizer.next_token tokenizer with
             | { token = Eof; _ } -> List.rev acc
             | tok -> collect_tokens (tok :: acc)
           in
           let inc_tokens = collect_tokens [] in
           
           (* Process included file with updated state *)
           let new_state = { state with
             included_files = filename :: state.included_files;
             current_file = full_path;
           } in
           let processed = process_tokens new_state inc_tokens in
           
           (* Restore original file state and continue *)
           let restored_state = { state with
             included_files = filename :: state.included_files;
           } in
           processed @ process_tokens restored_state rest)
  
  | If expr ->
      let expr_tokens = 
        let tokenizer = Tokenizer.create state.current_file expr in
        let rec collect acc =
          match Tokenizer.next_token tokenizer with
          | { token = Eof; _ } -> List.rev acc
          | tok -> collect (tok :: acc)
        in
        collect []
      in
      (* Expand macros in the expression tokens first *)
      let expanded_tokens = process_tokens state expr_tokens in
      (* Filter out whitespace for evaluation *)
      let filtered_tokens = List.filter (fun tok ->
        match tok.token with
        | Whitespace _ | Newline -> false
        | _ -> true
      ) expanded_tokens in
      let value = try ExprEval.evaluate filtered_tokens state with _ -> 0 in
      let new_cond = {
        active = value <> 0;
        seen_true_branch = value <> 0;
        in_else = false;
      } in
      let new_state = { state with 
        conditional_stack = new_cond :: state.conditional_stack 
      } in
      process_tokens new_state rest
  
  | Ifdef name ->
      let defined = Hashtbl.mem state.macros name in
      let new_cond = {
        active = defined;
        seen_true_branch = defined;
        in_else = false;
      } in
      let new_state = { state with
        conditional_stack = new_cond :: state.conditional_stack
      } in
      process_tokens new_state rest
  
  | Ifndef name ->
      let defined = Hashtbl.mem state.macros name in
      let new_cond = {
        active = not defined;
        seen_true_branch = not defined;
        in_else = false;
      } in
      let new_state = { state with
        conditional_stack = new_cond :: state.conditional_stack
      } in
      process_tokens new_state rest
  
  | Else ->
      (match state.conditional_stack with
       | [] -> failwith "#else without matching #if"
       | cond :: rest_stack ->
           if cond.in_else then
             failwith "Multiple #else for same #if"
           else
             let new_cond = {
               active = not cond.seen_true_branch;
               seen_true_branch = cond.seen_true_branch;
               in_else = true;
             } in
             let new_state = { state with
               conditional_stack = new_cond :: rest_stack
             } in
             process_tokens new_state rest)
  
  | Elif expr ->
      (match state.conditional_stack with
       | [] -> failwith "#elif without matching #if"
       | cond :: rest_stack ->
           if cond.in_else then
             failwith "#elif after #else"
           else
             let expr_tokens =
               let tokenizer = Tokenizer.create state.current_file expr in
               let rec collect acc =
                 match Tokenizer.next_token tokenizer with
                 | { token = Eof; _ } -> List.rev acc
                 | tok -> collect (tok :: acc)
               in
               collect []
             in
             (* Expand macros in the expression tokens first *)
             let expanded_tokens = process_tokens state expr_tokens in
             (* Filter out whitespace for evaluation *)
             let filtered_tokens = List.filter (fun tok ->
               match tok.token with
               | Whitespace _ | Newline -> false
               | _ -> true
             ) expanded_tokens in
             let value = try ExprEval.evaluate filtered_tokens state with _ -> 0 in
             let new_active = not cond.seen_true_branch && value <> 0 in
             let new_cond = {
               active = new_active;
               seen_true_branch = cond.seen_true_branch || new_active;
               in_else = false;
             } in
             let new_state = { state with
               conditional_stack = new_cond :: rest_stack
             } in
             process_tokens new_state rest)
  
  | Endif ->
      (match state.conditional_stack with
       | [] -> failwith "#endif without matching #if"
       | _ :: rest_stack ->
           let new_state = { state with
             conditional_stack = rest_stack
           } in
           process_tokens new_state rest)
  
  | Error msg ->
      failwith (Printf.sprintf "#error: %s" msg)
  
  | Pragma _pragma ->
      (* Ignore pragmas for now *)
      process_tokens state rest
  
  | Line (line, filename) ->
      let new_state = { state with
        current_line = line;
        current_file = Option.value filename ~default:state.current_file
      } in
      process_tokens new_state rest
  
  | NullDirective ->
      process_tokens state rest

(** Preprocess a string *)
let preprocess_string ?(include_paths=[]) ~filename content =
  (* Create initial state *)
  let state = create_pp_state ~include_paths filename in
  
  (* Add __FILE__ and __LINE__ as special macros that expand dynamically *)
  (* These will be handled specially during expansion *)
  
  (* Tokenize input *)
  let tokenizer = Tokenizer.create filename content in
  let rec collect_tokens acc =
    match Tokenizer.next_token tokenizer with
    | { token = Eof; _ } -> List.rev acc
    | tok -> collect_tokens (tok :: acc)
  in
  let tokens = collect_tokens [] in
  
  (* Process tokens *)
  let output_tokens = process_tokens state tokens in
  
  (* Convert tokens back to string *)
  let buf = Buffer.create (String.length content) in
  let rec output_token_list = function
    | [] -> ()
    | { token; _ } :: rest ->
        (match token with
         | Identifier s | Number s | String s | CharConst s | Punctuator s ->
             Buffer.add_string buf s
         | Whitespace s -> Buffer.add_string buf s
         | Newline -> Buffer.add_char buf '\n'
         | Other c -> Buffer.add_char buf c
         | Eof -> ());
        output_token_list rest
  in
  output_token_list output_tokens;
  
  Buffer.contents buf

(** Preprocess a file *)
let preprocess_file ?(include_paths=[]) filename =
  let content = 
    let ic = open_in filename in
    let content = really_input_string ic (in_channel_length ic) in
    close_in ic;
    content
  in
  preprocess_string ~include_paths ~filename content