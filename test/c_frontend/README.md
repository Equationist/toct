# C Frontend Test Suite

This directory contains comprehensive end-to-end tests for the C frontend implementation.

## Test Structure

### Positive Tests (Should Pass)
- **basic/**: Basic C constructs (arithmetic, variables, control flow)
- **functions/**: Function definitions and calls (including recursion)
- **pointers/**: Pointer and array operations
- **structs/**: Structure and typedef declarations

### Negative Tests (Should Fail)
- **negative/**: Test cases that should be rejected by the compiler

## Test Files

Each test consists of:
- `*.c` - The C source file to test
- `*.expected` - Expected execution output for positive tests
- `*.preprocessed` - Expected preprocessor output (optional)
- `*.expected_error` - Expected error description for negative tests

## Running Tests

```bash
# Run all tests (execution only)
./run_tests.py

# Run all tests with preprocessor output verification
./run_tests.py --include-preprocessor

# Run only preprocessor tests
./run_tests.py --preprocessor-only

# The test runner currently uses GCC/Clang to verify correctness
# Once the C frontend is implemented, modify run_tests.py to use our compiler
```

### Test Modes

**Standard Mode**: Compiles and runs C programs, verifying final execution output
**Preprocessor Mode**: Tests preprocessor output separately from compilation
**Combined Mode**: Tests both execution and preprocessor output

## Test Categories

### Basic Tests
- `arithmetic.c` - Basic arithmetic operations (+, -, *, /, %)
- `variables.c` - Variable declarations and assignments
- `if_else.c` - Conditional statements
- `loops.c` - For, while, and do-while loops
- `logical_ops.c` - Logical and bitwise operations

### Function Tests
- `simple_func.c` - Basic function definition and calling
- `recursion.c` - Recursive functions (factorial, fibonacci)
- `void_func.c` - Void functions and global variables
- `function_pointers.c` - Function pointers and higher-order functions

### Pointer Tests
- `basic_pointers.c` - Basic pointer operations
- `arrays.c` - Array operations and pointer arithmetic
- `pointer_arithmetic.c` - Pointer arithmetic operations
- `multidim_arrays.c` - Multi-dimensional arrays

### Struct Tests
- `basic_struct.c` - Basic struct operations
- `struct_pointers.c` - Struct pointers and arrow operator
- `typedef_test.c` - Typedef declarations
- `nested_struct.c` - Nested structures

### Negative Tests
- `syntax_errors.c` - Syntax errors (missing semicolons, etc.)
- `type_errors.c` - Type mismatches
- `undeclared_var.c` - Undeclared variables
- `function_mismatch.c` - Function argument mismatches
- `missing_return.c` - Missing return statements

## Adding New Tests

1. Create the `.c` file with the test code
2. Create the corresponding `.expected` file with expected output
3. For negative tests, create `.expected_error` with error description
4. Run the test suite to verify

## Integration with C Frontend

Once the C frontend is implemented, update `run_tests.py` to:
1. Use our C compiler instead of GCC/Clang
2. Test PIR generation in addition to final output
3. Add performance and optimization tests

## Test Coverage

The test suite covers:
- ✅ Basic arithmetic and logical operations
- ✅ Variable declarations and assignments
- ✅ Control flow (if/else, loops)
- ✅ Function definitions and calls
- ✅ Recursion
- ✅ Pointer operations and arithmetic
- ✅ Arrays (single and multi-dimensional)
- ✅ Structures and typedefs
- ✅ Function pointers
- ✅ Error conditions and edge cases

This provides a solid foundation for testing the C frontend implementation.