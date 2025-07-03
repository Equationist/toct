(** Debug the preprocessor *)

open C_frontend

let () =
  let input = {|#define MAX(a, b) ((a) > (b) ? (a) : (b))
int max_val = MAX(10, 20);|} in
  
  Printf.printf "Input:\n%s\n\n" input;
  
  try
    let output = Preprocessor.preprocess_string ~filename:"test.c" input in
    Printf.printf "Output:\n%s\n" output
  with e ->
    Printf.printf "Error: %s\n" (Printexc.to_string e);
    Printf.printf "Backtrace:\n%s\n" (Printexc.get_backtrace ())