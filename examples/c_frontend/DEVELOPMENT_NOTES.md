# C Frontend Development Notes

## Current Status (Updated)

### Completed Features
- ✅ Arithmetic operations (-, *, /, %)
- ✅ Bitwise operations (&, |, ^, <<, >>)  
- ✅ Comparison operations (<, >, <=, >=, ==, !=)
- ✅ Control flow (if/else)
- ✅ While and for loops (structure - needs SSA fixes)
- ✅ Function calls with proper name resolution
- ✅ Basic array declarations with alloca, Load/Store
- ✅ String literals (PIR generation only)

### Known Issues
1. **SSA Form**: Loops don't work correctly without phi nodes
2. **Function Parameters**: Parameters not properly connected to calling convention registers
3. **Const Handling**: ConstZero values not properly handled in backend
4. **String Literals**: Need to be emitted as global constants in data section
5. **External Functions**: Printf and other external functions need proper declarations
6. **Varargs**: Need proper calling convention support for variadic functions

### PIR Design Issues (FIXED)
The Load instruction in PIR had a fundamental design issue:
- Original definition: `Load of ty` (only took a type)
- Backend expected: address as an operand
- Store works correctly: `Store of value * value`  
- Solution: Changed Load to `Load of ty * value` to include the address
- Fixed in: instructions.ml, builder.ml, pretty_printer.ml, linter.ml, parser.ml, and all backends

## Components to Edit for Each Feature

### 1. Arithmetic Operations (-, *, /, %)

**Files to Edit:**
- `c_annotated_pir_generator.ml`: Add cases in `gen_annotated_expr_raw` for Sub, Mul, Div, Mod
- `lib/pir/instructions.ml`: Ensure sub, mul, div, rem instructions exist  
- `lib/backend/arm64_backend.ml`: Add patterns for SUB, MUL, DIV operations
- `lib/backend/instruction_selection.ml`: May need to update pattern matching

**PIR Instructions Needed:**
- `sub.i32`, `mul.i32`, `div.i32`, `rem.i32` (signed)
- `udiv.i32`, `urem.i32` (unsigned)

**ARM64 Instructions:**
- SUB: `sub rd, rn, rm`
- MUL: `mul rd, rn, rm`  
- DIV: `sdiv rd, rn, rm` (signed), `udiv rd, rn, rm` (unsigned)
- REM: Use combination of div and msub

### 2. Bitwise Operations (&, |, ^, ~, <<, >>)

**Files to Edit:**
- `c_annotated_pir_generator.ml`: Add cases for And, Or, Xor, Not, Shl, Shr
- `lib/backend/arm64_backend.ml`: Add patterns for AND, ORR, EOR, MVN, LSL, LSR/ASR

**PIR Instructions Needed:**
- `and.i32`, `or.i32`, `xor.i32`, `not.i32`
- `shl.i32`, `lshr.i32`, `ashr.i32`

### 3. Comparison Operations (<, >, <=, >=, ==, !=)

**Files to Edit:**
- `c_annotated_pir_generator.ml`: Generate icmp instructions
- `lib/backend/arm64_backend.ml`: Add CMP pattern and conditional move/branch

**PIR Instructions Needed:**
- `icmp.eq`, `icmp.ne`, `icmp.slt`, `icmp.sle`, `icmp.sgt`, `icmp.sge`

**ARM64 Instructions:**
- CMP: `cmp rn, rm`
- Conditional set: `cset rd, cond`

### 4. Control Flow (if/else with conditions, while, for)

**Files to Edit:**
- `c_annotated_pir_generator.ml`: 
  - Update If statement to use comparison result
  - Implement While and For loops
- `lib/backend/arm64_backend.ml`: Add conditional branch patterns

**PIR Instructions Needed:**
- `br cond, true_label, false_label` (conditional branch)
- `jmp label` (unconditional jump)

**ARM64 Instructions:**
- `b.cond label` (conditional branch)
- `b label` (unconditional branch)

### 5. Function Calls

**Files to Edit:**
- `c_annotated_pir_generator.ml`: Generate call instructions
- `lib/backend/arm64_backend.ml`: Add CALL pattern with ABI handling
- `lib/backend/codegen.ml`: Handle function prologue/epilogue properly

**PIR Instructions Needed:**
- `call func_value, [args]`
- `ret value`

**ARM64 Instructions:**
- `bl function` (branch with link)
- Function prologue/epilogue with proper stack frame

### 6. Printf Support

**Files to Edit:**
- `c_symbol_table.ml`: Add printf to builtin functions
- `c_annotated_pir_generator.ml`: Handle varargs calls
- `lib/backend/arm64_backend.ml`: Special handling for varargs ABI

**Special Considerations:**
- Need to handle format strings
- Varargs calling convention on ARM64
- External function declarations

### 7. Arrays and Pointers

**Files to Edit:**
- `c_type_checker.ml`: Already has array/pointer type support
- `c_annotated_pir_generator.ml`: 
  - Generate alloca for arrays
  - Handle address-of (&) and dereference (*)
  - Array indexing
- `lib/backend/arm64_backend.ml`: Add load/store patterns

**PIR Instructions Needed:**
- `alloca type, count` (stack allocation)
- `load ptr` (load from memory)
- `store value, ptr` (store to memory)
- `gep ptr, indices` (get element pointer)

**ARM64 Instructions:**
- `ldr rd, [rn, offset]` (load)
- `str rs, [rn, offset]` (store)
- Address calculation instructions

### 8. Floating Point Support

**Files to Edit:**
- `c_annotated_pir_generator.ml`: Handle float literals and operations
- `lib/backend/arm64_backend.ml`: Add FP register class and operations
- `lib/backend/register_allocator.ml`: Handle FP registers

**PIR Types:**
- `f32`, `f64`

**ARM64 Instructions:**
- FP arithmetic: `fadd`, `fsub`, `fmul`, `fdiv`
- FP compare: `fcmp`
- FP move: `fmov`

## Implementation Order

1. **Arithmetic Operations** - Essential for most tests
2. **Comparison Operations** - Needed for control flow
3. **Control Flow** - Complete if/else, add while/for
4. **Arrays and Pointers** - Many tests use arrays
5. **Function Calls** - Needed for modular programs
6. **Printf Support** - Common in test programs
7. **Bitwise Operations** - Less critical but needed
8. **Floating Point** - Can be done last

## Common Patterns

### Adding a Binary Operation
1. Add case in `gen_annotated_expr_raw` for the operation
2. Generate appropriate PIR instruction
3. Add machine operation in `arm64_backend.ml`
4. Add pattern matching rule
5. Test with simple program

### Adding a New PIR Instruction
1. Check if exists in `lib/pir/instructions.ml`
2. Add pattern in `lib/backend/instruction_selection.ml` if needed
3. Add ARM64 pattern in `arm64_backend.ml`
4. Handle in value type inference if needed

## Testing Strategy

Create test files for each feature:
- `test_arithmetic.c` - Test each arithmetic operation
- `test_bitwise.c` - Test bitwise operations
- `test_comparison.c` - Test all comparison operators
- `test_loops.c` - Test while and for loops
- `test_functions.c` - Test function calls
- `test_arrays.c` - Test array operations
- `test_pointers.c` - Test pointer operations