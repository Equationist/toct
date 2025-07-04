(** Test parser on all C test files *)

open C_frontend

let test_dir = "../../test/c_frontend"

let rec find_c_files dir =
  let entries = Sys.readdir dir in
  let files = ref [] in
  Array.iter (fun entry ->
    let path = Filename.concat dir entry in
    if Sys.is_directory path then
      files := !files @ find_c_files path
    else if Filename.check_suffix entry ".c" then
      files := path :: !files
  ) entries;
  !files

let test_parse_file file =
  try
    (* Read file *)
    let ic = open_in file in
    let content = really_input_string ic (in_channel_length ic) in
    close_in ic;
    
    (* Preprocess *)
    let preprocessed = 
      try Preprocessor.preprocess_string ~filename:file content
      with e -> 
        Printf.printf "  Preprocessor error: %s\n" (Printexc.to_string e);
        raise e
    in
    
    (* Lex *)
    let tokens = 
      try Lexer.lex_string file preprocessed
      with e ->
        Printf.printf "  Lexer error: %s\n" (Printexc.to_string e);
        raise e
    in
    
    (* Parse *)
    let _ast = 
      try Parser.parse tokens
      with e ->
        Printf.printf "  Parser error: %s\n" (Printexc.to_string e);
        raise e
    in
    
    true
  with _ ->
    false

let () =
  let c_files = find_c_files test_dir in
  let c_files = List.sort String.compare c_files in
  
  Printf.printf "Testing parser on %d C files...\n\n" (List.length c_files);
  
  let passed = ref 0 in
  let failed = ref 0 in
  
  List.iter (fun file ->
    let relative = 
      if String.starts_with ~prefix:test_dir file then
        String.sub file (String.length test_dir + 1) 
          (String.length file - String.length test_dir - 1)
      else
        file
    in
    Printf.printf "%-50s " relative;
    flush stdout;
    
    if test_parse_file file then begin
      Printf.printf "[\027[32mPASS\027[0m]\n";
      incr passed
    end else begin
      Printf.printf "[\027[31mFAIL\027[0m]\n";
      incr failed
    end
  ) c_files;
  
  Printf.printf "\n";
  Printf.printf "Summary: %d passed, %d failed\n" !passed !failed;
  
  if !failed > 0 then
    exit 1