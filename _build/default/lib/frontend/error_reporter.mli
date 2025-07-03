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

(* Create diagnostics *)
val error : span -> string -> diagnostic
val warning : span -> string -> diagnostic
val note : span -> string -> diagnostic
val add_note : diagnostic -> span -> string -> diagnostic

(* Diagnostic collection *)
type t

val create : unit -> t
val report : t -> severity -> span -> string -> (span * string) list -> unit
val has_errors : t -> bool
val error_count : t -> int
val warning_count : t -> int
val format_diagnostics : t -> (string, string) Hashtbl.t -> string