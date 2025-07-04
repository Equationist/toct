(** Debug array handling in annotated AST PIR generation *)

open C_frontend

let () =
  let source = {|
int main() {
    int arr[3];
    arr[0] = 10;
    return arr[0];
}
|} in

  try
    (* Preprocess *)
    let preprocessed = Preprocessor.preprocess_string ~filename:"test.c" source in
    
    (* Lex *)
    let tokens = Lexer.lex_string "test.c" preprocessed in
    
    (* Parse *)
    let ast = Parser.parse tokens in
    
    (* Type check *)
    let _symbol_table = C_type_checker.type_check ast in
    
    (* Annotate AST *)
    match C_ast_annotator.annotate_translation_unit ast with
    | Ok (annotated_ast, updated_symbol_table) ->
      
      (* Generate PIR with debug *)
      Printf.printf "=== Generating PIR ===\n";
      (match C_annotated_pir_generator.generate_pir_from_annotated annotated_ast updated_symbol_table with
       | Ok functions ->
         Printf.printf "Generated %d functions\n" (List.length functions);
         
         (* Pretty print PIR *)
         let pir_str = Compilerkit_pir.Pretty_printer.pp_program functions in
         Printf.printf "\n%s\n" pir_str;
         
         (* Analyze the PIR *)
         List.iter (fun func ->
           Printf.printf "\nAnalyzing function %s:\n" func.Compilerkit_pir.Instructions.name;
           Printf.printf "  Blocks: %d\n" (List.length func.blocks);
           List.iter (fun block ->
             Printf.printf "  Block %s:\n" block.Compilerkit_pir.Instructions.label;
             Printf.printf "    Instructions: %d\n" (List.length block.instructions);
             List.iteri (fun i instr ->
               Printf.printf "    [%d] " i;
               match instr.Compilerkit_pir.Instructions.result with
               | Some v -> 
                 Printf.printf "v%d = " (Compilerkit_pir.Values.get_id v);
                 (match instr.instr with
                  | Compilerkit_pir.Instructions.Memory (Compilerkit_pir.Instructions.Alloca _) ->
                    Printf.printf "alloca\n"
                  | Compilerkit_pir.Instructions.Const (Compilerkit_pir.Values.ConstInt (n, _)) ->
                    Printf.printf "const %Ld\n" n
                  | Compilerkit_pir.Instructions.Address (Compilerkit_pir.Instructions.Gep _) ->
                    Printf.printf "gep\n"
                  | Compilerkit_pir.Instructions.Memory (Compilerkit_pir.Instructions.Load _) ->
                    Printf.printf "load\n"
                  | Compilerkit_pir.Instructions.Memory (Compilerkit_pir.Instructions.Store _) ->
                    Printf.printf "store\n"
                  | _ -> Printf.printf "...\n")
               | None -> 
                 Printf.printf "(no result)\n"
             ) block.instructions
           ) func.blocks
         ) functions;
         
         (* Debug: Print raw values used in instructions *)
         Printf.printf "\n\nDEBUG: Checking value references:\n";
         List.iter (fun func ->
           List.iter (fun block ->
             List.iter (fun instr ->
               (match instr.Compilerkit_pir.Instructions.instr with
                | Compilerkit_pir.Instructions.Address (Compilerkit_pir.Instructions.Gep (base, idx)) ->
                  Printf.printf "GEP uses base v%d and index v%d\n" 
                    (Compilerkit_pir.Values.get_id base)
                    (Compilerkit_pir.Values.get_id idx)
                | _ -> ())
             ) block.Compilerkit_pir.Instructions.instructions
           ) func.Compilerkit_pir.Instructions.blocks
         ) functions
         
       | Error e ->
         Printf.printf "PIR generation error: %s\n" e)
      
    | Error e ->
      Printf.printf "AST annotation error: %s\n" e
      
  with e ->
    Printf.eprintf "Error: %s\n" (Printexc.to_string e);
    Printf.eprintf "%s\n" (Printexc.get_backtrace ())