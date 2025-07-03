(* Error reporting with position information *)

open Position

type severity = 
  | Error
  | Warning
  | Note

type diagnostic = {
  severity: severity;
  span: span;
  message: string;
  notes: (span * string) list;
}

(* Create diagnostic *)
let error span message = {
  severity = Error;
  span;
  message;
  notes = [];
}

let warning span message = {
  severity = Warning;
  span;
  message;
  notes = [];
}

let note span message = {
  severity = Note;
  span;
  message;
  notes = [];
}

let add_note diagnostic span message =
  { diagnostic with notes = (span, message) :: diagnostic.notes }


(* Pretty printing *)
let severity_to_string = function
  | Error -> "error"
  | Warning -> "warning"
  | Note -> "note"

let severity_color = function
  | Error -> "\027[31m"   (* Red *)
  | Warning -> "\027[33m" (* Yellow *)
  | Note -> "\027[36m"    (* Cyan *)

let reset_color = "\027[0m"

(* Extract source lines for display *)
let get_source_lines filename start_line end_line =
  try
    let ic = open_in filename in
    let rec read_lines n acc =
      if n > end_line then List.rev acc
      else
        try
          let line = input_line ic in
          if n >= start_line then
            read_lines (n + 1) ((n, line) :: acc)
          else
            read_lines (n + 1) acc
        with End_of_file -> 
          close_in ic;
          List.rev acc
    in
    let lines = read_lines 1 [] in
    close_in ic;
    lines
  with _ -> []

(* Format diagnostic with source context *)
let format_diagnostic ?(use_color=true) diagnostic =
  let buf = Buffer.create 256 in
  let color s c = if use_color then c ^ s ^ reset_color else s in
  
  (* Header *)
  Buffer.add_string buf (Printf.sprintf "%s: %s\n" 
    (color (severity_to_string diagnostic.severity) (severity_color diagnostic.severity))
    diagnostic.message);
  
  (* Location *)
  Buffer.add_string buf (Printf.sprintf " --> %s\n" (span_to_string diagnostic.span));
  
  (* Source context *)
  let lines = get_source_lines 
    diagnostic.span.start_pos.filename
    diagnostic.span.start_pos.line
    diagnostic.span.end_pos.line in
  
  if lines <> [] then begin
    Buffer.add_string buf " |\n";
    List.iter (fun (line_no, line_text) ->
      Buffer.add_string buf (Printf.sprintf "%4d | %s\n" line_no line_text);
      
      (* Underline the error span *)
      if line_no = diagnostic.span.start_pos.line then begin
        Buffer.add_string buf "     | ";
        let start_col = diagnostic.span.start_pos.column - 1 in
        let end_col = 
          if diagnostic.span.end_pos.line = line_no then
            diagnostic.span.end_pos.column - 1
          else
            String.length line_text in
        
        (* Add spaces before the caret *)
        for _ = 1 to start_col do
          Buffer.add_char buf ' '
        done;
        
        (* Add carets *)
        let caret_char = if diagnostic.severity = Error then '^' else '-' in
        let caret_color = severity_color diagnostic.severity in
        for i = start_col to end_col - 1 do
          if i < String.length line_text then
            Buffer.add_string buf (color (String.make 1 caret_char) caret_color)
        done;
        Buffer.add_char buf '\n'
      end
    ) lines;
  end;
  
  (* Add notes *)
  List.iter (fun (span, msg) ->
    Buffer.add_string buf (Printf.sprintf "     %s %s at %s\n" 
      (color "=" "\027[36m")
      msg 
      (span_to_string span))
  ) (List.rev diagnostic.notes);
  
  Buffer.contents buf

(* Diagnostic collection *)
type t = diagnostic list ref

let create () = ref []

let report diags severity span message notes =
  let diagnostic = { severity; span; message; notes } in
  diags := diagnostic :: !diags

let has_errors diags =
  List.exists (fun d -> d.severity = Error) !diags

let error_count diags =
  List.length (List.filter (fun d -> d.severity = Error) !diags)

let warning_count diags =
  List.length (List.filter (fun d -> d.severity = Warning) !diags)

let format_diagnostics diags _source_map =
  let buf = Buffer.create 1024 in
  List.iter (fun d ->
    Buffer.add_string buf (format_diagnostic ~use_color:true d);
    Buffer.add_char buf '\n'
  ) (List.rev !diags);
  Buffer.contents buf