open C_frontend

let () =
  (* Test with blank line after defines *)
  let test = {|#define FEATURE_A
#define VERSION 2

#ifdef FEATURE_A
int feature_a = 1;
#endif
more_content|} in
  
  Printf.printf "Input:\n%s\n\n" test;
  
  let output = Preprocessor.preprocess_string ~filename:"test.c" test in
  Printf.printf "Output:\n[%s]\n" output;
  Printf.printf "Length: %d\n" (String.length output);
  
  (* Print each character *)
  Printf.printf "\nCharacter codes:\n";
  String.iter (fun c ->
    Printf.printf "%3d " (Char.code c)
  ) output;
  Printf.printf "\n"