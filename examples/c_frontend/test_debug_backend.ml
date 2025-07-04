(** Debug backend instruction selection *)

open Compilerkit_pir

let test_simple_function () =
  let source = {|
int main() {
    return 42;
}
|} in

  Printf.printf "=== Testing Backend Instruction Selection ===\n\n";
  
  try
    (* Frontend *)
    let preprocessed = C_frontend.Preprocessor.preprocess_string ~filename:"test.c" source in
    let tokens = C_frontend.Lexer.lex_string "test.c" preprocessed in
    let ast = C_frontend.Parser.parse tokens in
    
    let symbol_table = match C_frontend.C_type_checker.type_check ast with
      | Ok st -> st
      | Error msg -> failwith ("Type checking failed: " ^ msg)
    in
    
    let annotated_ast, _ = match C_frontend.C_ast_annotator.annotate_translation_unit ast with
      | Ok (ast, st) -> ast, st
      | Error msg -> failwith ("AST annotation failed: " ^ msg)
    in
    
    let pir_functions = match C_frontend.C_annotated_pir_generator.generate_pir_from_annotated annotated_ast symbol_table with
      | Ok funcs -> funcs
      | Error msg -> failwith ("PIR generation failed: " ^ msg)
    in
    
    (* Print detailed PIR info *)
    List.iter (fun func ->
      Printf.printf "Function: %s\n" func.Instructions.name;
      List.iter (fun block ->
        Printf.printf "\nBlock %s:\n" block.Instructions.label;
        List.iter (fun instr ->
          Printf.printf "  Instruction:\n";
          Printf.printf "    result: %s\n" (match instr.Instructions.result with
            | Some _ -> "some value"
            | None -> "none");
          Printf.printf "    instr type: ";
          (match instr.Instructions.instr with
          | Instructions.Const cv ->
            Printf.printf "Const ";
            (match cv with
            | Values.ConstInt (i, ty) -> 
              Printf.printf "(ConstInt %Ld " i;
              (match ty with
              | Types.I1 -> Printf.printf "I1"
              | Types.I8 -> Printf.printf "I8"
              | Types.I16 -> Printf.printf "I16"
              | Types.I32 -> Printf.printf "I32"
              | Types.I64 -> Printf.printf "I64"
              | Types.F32 -> Printf.printf "F32"
              | Types.F64 -> Printf.printf "F64");
              Printf.printf ")"
            | _ -> Printf.printf "(other const)")
          | _ -> Printf.printf "(other instr)");
          Printf.printf "\n"
        ) block.Instructions.instructions;
      ) func.Instructions.blocks
    ) pir_functions
    
  with e ->
    Printf.printf "Error: %s\n" (Printexc.to_string e);
    Printf.printf "Backtrace:\n%s\n" (Printexc.get_backtrace ())

let () =
  Printexc.record_backtrace true;
  test_simple_function ()