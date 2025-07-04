# Pattern Implementation Guide

## Quick Reference: Adding a New Operation/Pattern

### 1. Frontend: AST → PIR Generation
**File: `examples/c_frontend/c_annotated_pir_generator.ml`**

```ocaml
(* In gen_annotated_expr, add new expression pattern *)
| NewExpr (...) ->
  let result_value = create_simple_value result_type in
  emit_instr ctx ~result:result_value (NewInstruction ...);
  result_value

(* For binary ops, add to c_binop_to_pir_binop_annotated *)
| Ast.NewOp, Int _ -> NewPirOp
```

### 2. Backend: Pattern Matching
**File: `lib/backend/instruction_selection.ml`**

```ocaml
(* In extract_operands, add operand extraction *)
| Instructions.NewInstr (...) -> [operand1; operand2; ...]

(* In build_tree children, add tree building *)
| Instructions.NewInstr (...) -> [value_to_tree v1; value_to_tree v2]

(* In match_pattern, add pattern matching *)
| Instructions.NewInstr (...), Instructions.NewInstr (...) -> 
  (* Match conditions *)
```

### 3. Backend: ARM64 Patterns
**File: `lib/backend/arm64_backend.ml`**

```ocaml
(* Create pattern function *)
let make_arm64_new_pattern ty =
  {
    pir_pattern = Instructions.NewInstr (...);
    cost = 1;
    emit = (fun reg_alloc result_val operands ->
      (* Extract operands and emit ARM64 instructions *)
      [{ label = None; op = NEWINSTR (...); comment = Some "new op" }]
    );
    constraints = [RegClass GPR; ...];
    hints = [];
  }

(* Add to arm64_patterns list *)
make_arm64_new_pattern (Types.Scalar Types.I32);
```

### 4. Backend: Machine Operations
**File: `lib/backend/machine.ml`**

```ocaml
(* Add new machine operation if needed *)
type machine_op =
  ...
  | NEWOP of reg * reg * reg  (* Define operands *)
```

### 5. Backend: Assembly Generation
**File: `lib/backend/codegen.ml`**

```ocaml
(* In Arm64CodeGen.format_instruction, add assembly formatting *)
| NEWOP (dst, src1, src2) -> 
  Printf.sprintf "newop %s, %s, %s" (fmt_reg dst) (fmt_reg src1) (fmt_reg src2)
```

## Common Patterns

### Binary Operation
1. Add to AST binary operators (`ast.ml`)
2. Add PIR conversion in `c_binop_to_pir_binop_annotated`
3. Create ARM64 pattern with `make_arm64_binop_pattern`
4. Add assembly formatting

### Comparison Operation
1. Add to AST comparison operators
2. Add PIR conversion in `c_cmp_to_pir_icmp_annotated`
3. Use existing `make_arm64_icmp_pattern` or create new
4. Emits CMP + CSET instructions

### Control Flow
1. Add statement pattern in `gen_annotated_stmt`
2. Create labels with `fresh_label`
3. Use `finish_block` with appropriate terminator (Br, Jmp)
4. Handle block transitions properly

### Function/Call Handling
1. For function references: Add attributes with function name
2. Extract operands in `extract_operands` for Call
3. Use `emit_arm64_call` with extracted function name from attributes

## Debug Tips
- Add `Printf.eprintf` in pattern matching to debug unmatched patterns
- Check PIR output to verify instruction generation
- Examine assembly output for instruction formatting issues
- Use `DEBUG emit_code:` messages to track pattern selection