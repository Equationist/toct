# PIR Examples

This directory contains example programs demonstrating the use of the Portable Intermediate Representation (PIR) and its builder API.

## Examples

### 1. fibonacci.ml
Demonstrates basic control flow with conditional branches. Shows how to:
- Define function parameters
- Use comparison instructions
- Create conditional branches
- Handle multiple basic blocks

### 2. vector_ops.ml
Shows SIMD vector operations including:
- Vector addition
- Element-wise operations
- Vector type handling
- Element extraction

### 3. memory_ops.ml
Illustrates memory management and struct manipulation:
- Memory allocation with `alloca`
- Struct field access
- Load/store operations
- Pointer arithmetic

## Running the Examples

To build and run an example:

```bash
dune exec examples/fibonacci.exe
dune exec examples/vector_ops.exe
dune exec examples/memory_ops.exe
```

Each example will:
1. Build a PIR function using the monadic builder API
2. Pretty-print the generated IR
3. Run the linter to validate SSA form and type correctness
4. Display any validation errors or warnings

## Key Concepts Demonstrated

- **SSA Form**: All values are assigned exactly once
- **Type Safety**: The builder enforces type correctness at construction time
- **Control Flow**: Explicit basic blocks with terminators
- **Monadic Builder**: Clean API for constructing complex IR