(* Common preprocessor utilities *)

open Position
open Error_reporter

(* Preprocessor directive type *)
type 'a directive =
  | Include of string * span
  | Define of string * string list * string * span
  | Undef of string * span
  | If of 'a * span
  | Ifdef of string * span
  | Ifndef of string * span
  | Elif of 'a * span
  | Else of span
  | Endif of span
  | Error of string * span
  | Warning of string * span
  | Pragma of string * span
  | Line of int * string option * span
  | Custom of string * string * span  (* For language-specific directives *)

(* Macro definition *)
type macro = {
  name: string;
  params: string list option;  (* None for object-like, Some for function-like *)
  body: string;
  span: span;
}

(* Preprocessor state *)
type 'a state = {
  macros: (string, macro) Hashtbl.t;
  include_paths: string list;
  included_files: (string, unit) Hashtbl.t;  (* For include guards *)
  conditional_stack: (bool * bool) list;  (* (condition_met, else_seen) *)
  reporter: Error_reporter.t;
  custom: 'a;  (* Language-specific state *)
}

(* Create preprocessor state *)
let create_state include_paths reporter custom = {
  macros = Hashtbl.create 256;
  include_paths;
  included_files = Hashtbl.create 32;
  conditional_stack = [];
  reporter;
  custom;
}

(* Macro operations *)
module Macros = struct
  (* Define a macro *)
  let define state name params body span =
    let macro = { name; params; body; span } in
    Hashtbl.replace state.macros name macro

  (* Undefine a macro *)
  let undef state name =
    Hashtbl.remove state.macros name

  (* Check if macro is defined *)
  let is_defined state name =
    Hashtbl.mem state.macros name

  (* Get macro definition *)
  let get_macro state name =
    Hashtbl.find_opt state.macros name

  (* Expand object-like macro *)
  let expand_object_macro _state macro =
    macro.body

  (* Expand function-like macro *)
  let expand_function_macro _state macro args =
    match macro.params with
    | None -> Result.Error ("Not a function-like macro", span dummy dummy)
    | Some params ->
        if List.length params <> List.length args then
          Result.Error ("Macro argument count mismatch", span dummy dummy)
        else
          (* Simple parameter substitution *)
          let substitutions = List.combine params args in
          let result = List.fold_left (fun body (param, arg) ->
            (* Replace all occurrences of param with arg *)
            let rec replace s =
              try
                let idx = String.index s param.[0] in
                if idx + String.length param <= String.length s &&
                   String.sub s idx (String.length param) = param then
                  (String.sub s 0 idx) ^ arg ^ 
                  replace (String.sub s (idx + String.length param) 
                          (String.length s - idx - String.length param))
                else
                  (String.sub s 0 (idx + 1)) ^ 
                  replace (String.sub s (idx + 1) (String.length s - idx - 1))
              with Not_found -> s
            in
            replace body
          ) macro.body substitutions in
          Result.Ok result

  (* Standard predefined macros *)
  let add_standard_macros state =
    let date = Unix.localtime (Unix.time ()) in
    define state "__DATE__" None 
      (Printf.sprintf "\"%s %2d %04d\"" 
        [|"Jan";"Feb";"Mar";"Apr";"May";"Jun";"Jul";"Aug";"Sep";"Oct";"Nov";"Dec"|].(date.Unix.tm_mon)
        date.Unix.tm_mday
        (date.Unix.tm_year + 1900))
      (span dummy dummy);
    define state "__TIME__" None
      (Printf.sprintf "\"%02d:%02d:%02d\""
        date.Unix.tm_hour date.Unix.tm_min date.Unix.tm_sec)
      (span dummy dummy);
    define state "__FILE__" None "\"<unknown>\"" 
      (span dummy dummy);
    define state "__LINE__" None "0"
      (span dummy dummy)
end

(* Conditional compilation *)
module Conditionals = struct
  (* Check if we're currently skipping *)
  let is_skipping state =
    List.exists (fun (cond_met, _) -> not cond_met) state.conditional_stack

  (* Push conditional *)
  let push_conditional state condition =
    { state with conditional_stack = (condition, false) :: state.conditional_stack }

  (* Handle #else *)
  let handle_else state span =
    match state.conditional_stack with
    | [] ->
        report state.reporter Error span "#else without matching #if" [];
        state
    | (cond_met, else_seen) :: rest ->
        if else_seen then begin
          report state.reporter Error span "Multiple #else for same #if" [];
          state
        end else
          { state with conditional_stack = (not cond_met, true) :: rest }

  (* Handle #endif *)
  let handle_endif state span =
    match state.conditional_stack with
    | [] ->
        report state.reporter Error span "#endif without matching #if" [];
        state
    | _ :: rest ->
        { state with conditional_stack = rest }

  (* Check conditional balance *)
  let check_balance state =
    if state.conditional_stack <> [] then
      report state.reporter Error 
        (span dummy dummy)
        "Unterminated conditional compilation directive" []
end

(* Include file handling *)
module Includes = struct
  (* Find include file *)
  let find_include_file state filename is_system =
    let paths = 
      if is_system then
        state.include_paths
      else
        "." :: state.include_paths
    in
    let rec search = function
      | [] -> None
      | path :: rest ->
          let full_path = Filename.concat path filename in
          if Sys.file_exists full_path then
            Some full_path
          else
            search rest
    in
    search paths

  (* Process include *)
  let process_include state filename is_system span process_file =
    match find_include_file state filename is_system with
    | None ->
        report state.reporter Error span 
          (Printf.sprintf "Cannot find include file: %s" filename) [];
        Result.Error ()
    | Some path ->
        if Hashtbl.mem state.included_files path then
          Result.Ok ()  (* Already included - include guard *)
        else begin
          Hashtbl.add state.included_files path ();
          process_file state path
        end
end

(* Token-based preprocessing *)
module Token_preprocessing = struct
  (* Token with preprocessing info *)
  type 'token preprocessed_token =
    | Token of 'token
    | Expanded of 'token list
    | Skipped

  (* Process token stream with preprocessing *)
  let preprocess_tokens state is_directive_token process_directive expand_macro tokens =
    let rec process_tokens tokens acc =
      match tokens with
      | [] -> List.rev acc
      | tok :: rest ->
          if is_directive_token tok then
            match process_directive state tok rest with
            | Result.Ok ((), remaining) -> process_tokens remaining acc
            | Result.Error remaining -> process_tokens remaining acc
          else if Conditionals.is_skipping state then
            process_tokens rest acc
          else
            match expand_macro state tok with
            | Some expanded -> process_tokens (expanded @ rest) acc
            | None -> process_tokens rest (tok :: acc)
    in
    process_tokens tokens []
end

(* Line-based preprocessing *)
module Line_preprocessing = struct
  (* Process lines with directives *)
  let process_lines state is_directive_line process_directive_line process_normal_line lines =
    let rec process lines acc =
      match lines with
      | [] -> List.rev acc
      | line :: rest ->
          if is_directive_line line then
            match process_directive_line state line with
            | Result.Ok _state' -> process rest acc
            | Result.Error _state' -> process rest acc
          else if Conditionals.is_skipping state then
            process rest acc
          else
            let processed = process_normal_line state line in
            process rest (processed :: acc)
    in
    process lines []
end

(* Common directive patterns *)
module Directive_patterns = struct
  (* Parse include directive *)
  let parse_include line =
    let line = String.trim line in
    if String.length line > 0 then
      match line.[0] with
      | '"' ->
          let close = String.index_from line 1 '"' in
          let filename = String.sub line 1 (close - 1) in
          Some (filename, false)
      | '<' ->
          let close = String.index line '>' in
          let filename = String.sub line 1 (close - 1) in
          Some (filename, true)
      | _ -> None
    else
      None

  (* Parse define directive *)
  let parse_define line =
    let parts = String.split_on_char ' ' (String.trim line) in
    match parts with
    | [] -> None
    | name :: rest ->
        (* Check for function-like macro *)
        if String.contains name '(' then
          let paren = String.index name '(' in
          let macro_name = String.sub name 0 paren in
          let params_str = String.sub name (paren + 1) (String.length name - paren - 1) in
          let params = 
            if String.contains params_str ')' then
              let close = String.index params_str ')' in
              let param_list = String.sub params_str 0 close in
              Some (String.split_on_char ',' param_list |> List.map String.trim)
            else
              None
          in
          Some (macro_name, params, String.concat " " rest)
        else
          Some (name, None, String.concat " " rest)
end