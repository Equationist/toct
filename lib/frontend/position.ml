(* Source position tracking *)

type t = {
  filename: string;
  line: int;
  column: int;
  offset: int;
}

let create filename line column offset = {
  filename; line; column; offset
}

let dummy = {
  filename = "<unknown>";
  line = 0;
  column = 0;
  offset = 0;
}

let advance_char pos ch =
  if ch = '\n' then
    { pos with line = pos.line + 1; column = 1; offset = pos.offset + 1 }
  else
    { pos with column = pos.column + 1; offset = pos.offset + 1 }

let advance_string pos str =
  String.fold_left advance_char pos str

(* Range between two positions *)
type span = {
  start_pos: t;
  end_pos: t;
}

let span start_pos end_pos = { start_pos; end_pos }

let span_union s1 s2 = {
  start_pos = if s1.start_pos.offset < s2.start_pos.offset then s1.start_pos else s2.start_pos;
  end_pos = if s1.end_pos.offset > s2.end_pos.offset then s1.end_pos else s2.end_pos;
}

(* Pretty printing *)
let to_string pos =
  Printf.sprintf "%s:%d:%d" pos.filename pos.line pos.column

let span_to_string span =
  Printf.sprintf "%s-%d:%d" 
    (to_string span.start_pos)
    span.end_pos.line 
    span.end_pos.column