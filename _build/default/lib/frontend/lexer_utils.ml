(* Common lexer utilities *)

open Position

(* Character classification *)
let is_whitespace = function
  | ' ' | '\t' | '\r' | '\n' -> true
  | _ -> false

let is_digit = function
  | '0'..'9' -> true
  | _ -> false

let is_hex_digit = function
  | '0'..'9' | 'a'..'f' | 'A'..'F' -> true
  | _ -> false

let is_alpha = function
  | 'a'..'z' | 'A'..'Z' -> true
  | _ -> false

let is_ident_start = function
  | 'a'..'z' | 'A'..'Z' | '_' -> true
  | _ -> false

let is_ident_cont = function
  | 'a'..'z' | 'A'..'Z' | '0'..'9' | '_' -> true
  | _ -> false

(* Lexer buffer for efficient string building *)
module Buffer = struct
  type t = {
    mutable content: string;
    mutable pos: int;
    mutable line: int;
    mutable column: int;
    filename: string;
  }

  let create filename content = {
    content;
    pos = 0;
    line = 1;
    column = 1;
    filename;
  }

  let current_pos buf =
    Position.create buf.filename buf.line buf.column buf.pos

  let peek buf =
    if buf.pos < String.length buf.content then
      Some buf.content.[buf.pos]
    else
      None

  let peek_n buf n =
    let pos = buf.pos + n in
    if pos < String.length buf.content then
      Some buf.content.[pos]
    else
      None

  let advance buf =
    if buf.pos < String.length buf.content then begin
      let ch = buf.content.[buf.pos] in
      buf.pos <- buf.pos + 1;
      if ch = '\n' then begin
        buf.line <- buf.line + 1;
        buf.column <- 1
      end else
        buf.column <- buf.column + 1;
      Some ch
    end else
      None

  let skip_while buf pred =
    let rec loop () =
      match peek buf with
      | Some ch when pred ch -> 
        ignore (advance buf);
        loop ()
      | _ -> ()
    in
    loop ()

  let take_while buf pred =
    let start_pos = buf.pos in
    skip_while buf pred;
    String.sub buf.content start_pos (buf.pos - start_pos)

  let skip_whitespace buf =
    skip_while buf is_whitespace

  let remaining buf =
    String.length buf.content - buf.pos

  let is_eof buf =
    buf.pos >= String.length buf.content

  let slice buf start_pos end_pos =
    String.sub buf.content start_pos (end_pos - start_pos)
end

(* Token builder *)
let make_token kind text span =
  { Pratt.kind; text; span }

(* Common lexer patterns *)
module Patterns = struct
  (* Scan identifier *)
  let scan_ident buf =
    let start_pos = Buffer.current_pos buf in
    match Buffer.peek buf with
    | Some ch when is_ident_start ch ->
      let text = Buffer.take_while buf is_ident_cont in
      let end_pos = Buffer.current_pos buf in
      Some (text, span start_pos end_pos)
    | _ -> None

  (* Scan integer *)
  let scan_int buf =
    let start_pos = Buffer.current_pos buf in
    
    (* Check for hex prefix *)
    let is_hex = 
      match Buffer.peek buf, Buffer.peek_n buf 1 with
      | Some '0', Some ('x' | 'X') ->
        ignore (Buffer.advance buf);
        ignore (Buffer.advance buf);
        true
      | _ -> false
    in
    
    let digits = 
      if is_hex then
        Buffer.take_while buf is_hex_digit
      else
        Buffer.take_while buf is_digit
    in
    
    if String.length digits > 0 then
      let end_pos = Buffer.current_pos buf in
      let text = 
        if is_hex then "0x" ^ digits 
        else digits 
      in
      Some (text, span start_pos end_pos)
    else
      None

  (* Scan float *)
  let scan_float buf =
    let start_pos = Buffer.current_pos buf in
    let start_buf_pos = buf.Buffer.pos in
    
    (* Integer part *)
    let _int_part = Buffer.take_while buf is_digit in
    
    (* Check for decimal point *)
    let has_decimal = 
      match Buffer.peek buf with
      | Some '.' when Buffer.peek_n buf 1 <> Some '.' -> (* Not .. *)
        ignore (Buffer.advance buf);
        true
      | _ -> false
    in
    
    (* Fractional part *)
    let _frac_part = 
      if has_decimal then
        Buffer.take_while buf is_digit
      else
        ""
    in
    
    (* Exponent part *)
    let has_exp =
      match Buffer.peek buf with
      | Some ('e' | 'E') ->
        ignore (Buffer.advance buf);
        (* Optional sign *)
        (match Buffer.peek buf with
         | Some ('+' | '-') -> ignore (Buffer.advance buf)
         | _ -> ());
        true
      | _ -> false
    in
    
    let _exp_part =
      if has_exp then
        Buffer.take_while buf is_digit
      else
        ""
    in
    
    (* Must have decimal or exponent to be a float *)
    if has_decimal || has_exp then
      let end_pos = Buffer.current_pos buf in
      let text = Buffer.slice buf start_buf_pos buf.Buffer.pos in
      Some (text, span start_pos end_pos)
    else begin
      (* Backtrack *)
      buf.Buffer.pos <- start_buf_pos;
      None
    end

  (* Scan string literal *)
  let scan_string buf quote_char =
    let start_pos = Buffer.current_pos buf in
    
    match Buffer.advance buf with (* Skip opening quote *)
    | Some ch when ch = quote_char ->
      let text_buf = Stdlib.Buffer.create 64 in
      let rec loop () =
        match Buffer.advance buf with
        | None -> Error "Unterminated string literal"
        | Some '\\' ->
          (* Escape sequence *)
          (match Buffer.advance buf with
           | None -> Error "Unterminated string escape"
           | Some 'n' -> Stdlib.Buffer.add_char text_buf '\n'; loop ()
           | Some 't' -> Stdlib.Buffer.add_char text_buf '\t'; loop ()
           | Some 'r' -> Stdlib.Buffer.add_char text_buf '\r'; loop ()
           | Some '\\' -> Stdlib.Buffer.add_char text_buf '\\'; loop ()
           | Some '"' -> Stdlib.Buffer.add_char text_buf '"'; loop ()
           | Some '\'' -> Stdlib.Buffer.add_char text_buf '\''; loop ()
           | Some '0' -> Stdlib.Buffer.add_char text_buf '\000'; loop ()
           | Some ch -> 
             Stdlib.Buffer.add_char text_buf '\\';
             Stdlib.Buffer.add_char text_buf ch;
             loop ())
        | Some ch when ch = quote_char ->
          (* Closing quote *)
          let end_pos = Buffer.current_pos buf in
          Ok (Stdlib.Buffer.contents text_buf, span start_pos end_pos)
        | Some ch ->
          Stdlib.Buffer.add_char text_buf ch;
          loop ()
      in
      loop ()
    | _ -> Error "Expected string literal"

  (* Scan line comment *)
  let scan_line_comment buf =
    let start_pos = Buffer.current_pos buf in
    match Buffer.peek buf, Buffer.peek_n buf 1 with
    | Some '/', Some '/' ->
      ignore (Buffer.advance buf);
      ignore (Buffer.advance buf);
      let text = Buffer.take_while buf (fun ch -> ch <> '\n') in
      let end_pos = Buffer.current_pos buf in
      Some ("//" ^ text, span start_pos end_pos)
    | _ -> None

  (* Scan block comment *)
  let scan_block_comment buf =
    let start_pos = Buffer.current_pos buf in
    match Buffer.peek buf, Buffer.peek_n buf 1 with
    | Some '/', Some '*' ->
      ignore (Buffer.advance buf);
      ignore (Buffer.advance buf);
      let rec loop depth =
        match Buffer.peek buf, Buffer.peek_n buf 1 with
        | None, _ -> Error "Unterminated block comment"
        | Some '*', Some '/' ->
          ignore (Buffer.advance buf);
          ignore (Buffer.advance buf);
          if depth = 1 then
            let end_pos = Buffer.current_pos buf in
            Ok (span start_pos end_pos)
          else
            loop (depth - 1)
        | Some '/', Some '*' ->
          ignore (Buffer.advance buf);
          ignore (Buffer.advance buf);
          loop (depth + 1)
        | _ ->
          ignore (Buffer.advance buf);
          loop depth
      in
      (match loop 1 with
       | Ok span -> Some ("/* comment */", span)
       | Error _ -> None)
    | _ -> None
end

(* Keyword table builder *)
module Keywords = struct
  type 'tok t = (string, 'tok) Hashtbl.t

  let create keywords =
    let table = Hashtbl.create (List.length keywords) in
    List.iter (fun (text, tok) -> Hashtbl.add table text tok) keywords;
    table

  let lookup table text default =
    try Hashtbl.find table text
    with Not_found -> default
end