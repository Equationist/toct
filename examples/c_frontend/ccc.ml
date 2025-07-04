(** CCC - CompilerKit C Compiler 
    End-to-end C compiler using the annotated AST pipeline *)

open C_frontend
open Compilerkit_pir
open Compilerkit_backend

let compile_file input_file output_file =
  try
    (* Read source file *)
    let source = 
      let ic = open_in input_file in
      let content = really_input_string ic (in_channel_length ic) in
      close_in ic;
      content
    in
    
    (* Frontend: C -> PIR *)
    Printf.eprintf "Preprocessing %s...\n" input_file;
    let preprocessed = Preprocessor.preprocess_string ~filename:input_file source in
    
    Printf.eprintf "Lexing...\n";
    let tokens = Lexer.lex_string input_file preprocessed in
    
    Printf.eprintf "Parsing...\n";
    let ast = Parser.parse tokens in
    
    Printf.eprintf "Type checking...\n";
    let symbol_table = match C_type_checker.type_check ast with
      | Ok st -> st
      | Error msg -> failwith ("Type checking failed: " ^ msg)
    in
    
    Printf.eprintf "Annotating AST...\n";
    let annotated_ast, _ = match C_ast_annotator.annotate_translation_unit ast with
      | Ok (ast, st) -> ast, st
      | Error msg -> failwith ("AST annotation failed: " ^ msg)
    in
    
    Printf.eprintf "Generating PIR...\n";
    let pir_functions = match C_annotated_pir_generator.generate_pir_from_annotated annotated_ast symbol_table with
      | Ok funcs -> funcs
      | Error msg -> failwith ("PIR generation failed: " ^ msg)
    in
    
    (* Create PIR module *)
    let pir_module = Module_ir.create_module input_file in
    List.iter (Module_ir.add_function pir_module) pir_functions;
    
    (* Backend: PIR -> Assembly *)
    Printf.eprintf "Generating assembly...\n";
    let asm_code = Compilerkit_backend.generate_arm64 pir_module in
    
    (* Write assembly file *)
    let asm_file = output_file ^ ".s" in
    let oc = open_out asm_file in
    output_string oc asm_code;
    close_out oc;
    
    (* Assemble and link *)
    Printf.eprintf "Assembling and linking...\n";
    let cmd = Printf.sprintf "as -o %s.o %s && ld -o %s %s.o -lSystem -syslibroot `xcrun -sdk macosx --show-sdk-path` -arch arm64" 
      output_file asm_file output_file output_file in
    let exit_code = Sys.command cmd in
    
    if exit_code = 0 then
      Printf.printf "Compilation successful: %s\n" output_file
    else
      failwith "Assembly/linking failed"
      
  with e ->
    Printf.eprintf "Error: %s\n" (Printexc.to_string e);
    exit 1

let () =
  match Sys.argv with
  | [| _; input_file; "-o"; output_file |] ->
    compile_file input_file output_file
  | [| _; input_file |] ->
    let output_file = Filename.chop_extension input_file in
    compile_file input_file output_file
  | _ ->
    Printf.eprintf "Usage: %s <input.c> [-o <output>]\n" Sys.argv.(0);
    exit 1