open C_frontend

let test_file = "debug_preprocessing.c"

let () =
  let content = 
    let ic = open_in test_file in
    let content = really_input_string ic (in_channel_length ic) in
    close_in ic;
    content
  in
  
  let current_dir = Sys.getcwd () in
  let include_dir = Filename.concat current_dir "include" in
  let include_paths = [include_dir] in
  
  Printf.printf "Original content:\n%s\n\n" content;
  Printf.printf "Include paths: %s\n\n" (String.concat "; " include_paths);
  
  let preprocessed = Preprocessor.preprocess_string ~include_paths ~filename:test_file content in
  Printf.printf "Preprocessed content:\n%s\n" preprocessed