# C Frontend Test Catalog

This document catalogs all test cases in the C frontend test suite.

## Test Summary

**Total Tests**: 47 (37 positive + 10 negative)
**Test Categories**: 9 (Basic, Functions, Pointers, Structs, Preprocessor×4, Negative×2)

## Basic C Constructs (7 tests)

### `arithmetic.c`
- **Purpose**: Test basic arithmetic operations
- **Features**: Addition, subtraction, multiplication, division, modulo
- **Expected**: Correct arithmetic results

### `constants.c`
- **Purpose**: Test various constant literal types
- **Features**: Decimal, hexadecimal, octal integers; character constants; string literals; floating-point constants
- **Expected**: Proper parsing and representation of constants

### `if_else.c`
- **Purpose**: Test conditional statements
- **Features**: if statements, else clauses, else-if chains, comparison operators
- **Expected**: Correct conditional branching behavior

### `logical_ops.c`
- **Purpose**: Test logical and bitwise operations
- **Features**: &&, ||, !, &, |, ^, ~, <<, >>
- **Expected**: Correct logical/bitwise operation results

### `loops.c`
- **Purpose**: Test loop constructs
- **Features**: for loops, while loops, do-while loops
- **Expected**: Proper loop execution and termination

### `sizeof_test.c`
- **Purpose**: Test sizeof operator
- **Features**: sizeof on basic types, arrays, structs
- **Expected**: Correct size calculations

### `variables.c`
- **Purpose**: Test variable declarations and assignments
- **Features**: Variable declarations, initialization, assignment
- **Expected**: Proper variable handling and scoping

## Function Definitions and Calls (5 tests)

### `function_pointers.c`
- **Purpose**: Test function pointers and higher-order functions
- **Features**: Function pointer declarations, function pointer calls, passing functions as arguments
- **Expected**: Correct function pointer behavior

### `recursion.c`
- **Purpose**: Test recursive function calls
- **Features**: Recursive factorial and fibonacci functions
- **Expected**: Correct recursive computation results

### `simple_func.c`
- **Purpose**: Test basic function definition and calling
- **Features**: Function definitions, function calls, parameter passing, return values
- **Expected**: Proper function call mechanics

### `varargs.c`
- **Purpose**: Test variadic functions
- **Features**: Variable argument lists, va_list, va_start, va_arg, va_end
- **Expected**: Correct handling of variable arguments

### `void_func.c`
- **Purpose**: Test void functions and global variables
- **Features**: void functions, global variable access, function side effects
- **Expected**: Proper void function behavior and global state management

## Pointer and Array Operations (4 tests)

### `arrays.c`
- **Purpose**: Test array operations
- **Features**: Array declarations, array indexing, array initialization, pointer arithmetic equivalence
- **Expected**: Correct array access and manipulation

### `basic_pointers.c`
- **Purpose**: Test basic pointer operations
- **Features**: Pointer declarations, address-of operator, dereference operator, pointer assignment
- **Expected**: Proper pointer behavior and memory addressing

### `multidim_arrays.c`
- **Purpose**: Test multi-dimensional arrays
- **Features**: 2D array declarations, nested array access, array initialization
- **Expected**: Correct multi-dimensional array handling

### `pointer_arithmetic.c`
- **Purpose**: Test pointer arithmetic operations
- **Features**: Pointer increment/decrement, pointer addition/subtraction, pointer difference
- **Expected**: Correct pointer arithmetic behavior

## Struct and Typedef Declarations (5 tests)

### `basic_struct.c`
- **Purpose**: Test basic struct operations
- **Features**: Struct declarations, struct member access, struct assignment
- **Expected**: Proper struct handling and member access

### `nested_struct.c`
- **Purpose**: Test nested structures
- **Features**: Structs containing other structs, nested member access
- **Expected**: Correct nested structure behavior

### `struct_pointers.c`
- **Purpose**: Test struct pointers and arrow operator
- **Features**: Struct pointers, arrow operator (->), struct pointer parameter passing
- **Expected**: Proper struct pointer operations

### `typedef_test.c`
- **Purpose**: Test typedef declarations
- **Features**: Type aliases, typedef with basic types, typedef with structs, self-referential structs
- **Expected**: Correct type aliasing behavior

### `unions.c`
- **Purpose**: Test union types
- **Features**: Union declarations, union member access, union memory layout
- **Expected**: Proper union behavior and memory sharing

## Preprocessor Tests (16 tests)

### Basic Preprocessor Directives (4 tests)

#### `simple_defines.c`
- **Purpose**: Test basic #define directives
- **Features**: Object-like macros, constant definitions, simple text replacement
- **Expected**: Proper macro expansion and substitution

#### `macro_redefinition.c`
- **Purpose**: Test #undef and macro redefinition
- **Features**: Macro removal, redefinition of macros with different values/types
- **Expected**: Correct handling of macro lifecycle

#### `predefined_macros.c`
- **Purpose**: Test predefined macros
- **Features**: __FILE__, __LINE__, __DATE__, __TIME__, __STDC__, compiler-specific macros
- **Expected**: Proper predefined macro values

#### `include_test.c`
- **Purpose**: Test #include directive
- **Features**: File inclusion, include guards, header file processing
- **Expected**: Correct file inclusion and macro visibility

### Function-like Macros (4 tests)

#### `simple_function_macros.c`
- **Purpose**: Test function-like macros with parameters
- **Features**: Parameterized macros, conditional expressions, proper parenthesization
- **Expected**: Correct parameter substitution and expansion

#### `variadic_macros.c`
- **Purpose**: Test variadic macros (C99 feature)
- **Features**: __VA_ARGS__, variable argument lists in macros
- **Expected**: Proper handling of variable arguments

#### `nested_macros.c`
- **Purpose**: Test nested macro expansion
- **Features**: Macros calling other macros, complex expansion chains
- **Expected**: Correct nested expansion order

#### `macro_side_effects.c`
- **Purpose**: Test macro side effects and multiple evaluation
- **Features**: Demonstrates problems with macro parameter evaluation
- **Expected**: Shows multiple evaluation behavior

### Conditional Compilation (4 tests)

#### `ifdef_ifndef.c`
- **Purpose**: Test #ifdef and #ifndef directives
- **Features**: Conditional compilation based on macro definition
- **Expected**: Correct conditional inclusion/exclusion

#### `if_expressions.c`
- **Purpose**: Test #if with expressions
- **Features**: Mathematical expressions, logical operators, defined() operator
- **Expected**: Proper expression evaluation in preprocessor

#### `platform_specific.c`
- **Purpose**: Test platform-specific conditional compilation
- **Features**: Compiler detection, OS detection, architecture detection
- **Expected**: Correct platform-specific code selection

#### `complex_nesting.c`
- **Purpose**: Test complex nested conditional compilation
- **Features**: Multiple levels of nesting, complex condition combinations
- **Expected**: Proper nested conditional handling

### Complex Macro Features (4 tests)

#### `stringification.c`
- **Purpose**: Test stringification operator (#)
- **Features**: Converting macro parameters to string literals
- **Expected**: Correct string literal generation

#### `token_pasting.c`
- **Purpose**: Test token pasting operator (##)
- **Features**: Token concatenation, identifier generation
- **Expected**: Proper token concatenation

#### `advanced_macros.c`
- **Purpose**: Test advanced macro techniques
- **Features**: X-macro patterns, code generation, indirect expansion
- **Expected**: Complex macro pattern functionality

#### `macro_recursion.c`
- **Purpose**: Test macro recursion and self-reference
- **Features**: Self-referential macros, recursion prevention
- **Expected**: Prevention of infinite expansion

## C Language Negative Tests (5 tests)

### `function_mismatch.c`
- **Purpose**: Test function argument count mismatch
- **Features**: Function calls with wrong number of arguments
- **Expected**: Compilation error

### `missing_return.c`
- **Purpose**: Test missing return statements
- **Features**: Non-void functions without return, void functions with return values
- **Expected**: Compilation error

### `syntax_errors.c`
- **Purpose**: Test syntax error detection
- **Features**: Missing semicolons, malformed statements
- **Expected**: Compilation error

### `type_errors.c`
- **Purpose**: Test type mismatch detection
- **Features**: Incompatible type assignments
- **Expected**: Compilation error

### `undeclared_var.c`
- **Purpose**: Test undeclared variable detection
- **Features**: Usage of undeclared variables
- **Expected**: Compilation error

## Preprocessor Negative Tests (5 tests)

### `undefined_macro.c`
- **Purpose**: Test undefined macro usage
- **Features**: Reference to undefined macros
- **Expected**: Compilation error

### `macro_redefinition_error.c`
- **Purpose**: Test macro redefinition without #undef
- **Features**: Conflicting macro definitions
- **Expected**: Warning/error for redefinition

### `missing_include.c`
- **Purpose**: Test missing include file
- **Features**: #include with nonexistent file
- **Expected**: Include file not found error

### `unterminated_if.c`
- **Purpose**: Test unterminated #if directive
- **Features**: Missing #endif
- **Expected**: Unterminated conditional error

### `invalid_directive.c`
- **Purpose**: Test invalid preprocessor directive
- **Features**: Unknown preprocessor commands
- **Expected**: Invalid directive error

## Test Coverage Analysis

### Language Features Covered
- ✅ Basic arithmetic and logical operations
- ✅ Variable declarations and assignments
- ✅ Control flow (if/else, loops)
- ✅ Function definitions and calls
- ✅ Recursion
- ✅ Function pointers and higher-order functions
- ✅ Variadic functions
- ✅ Pointer operations and arithmetic
- ✅ Arrays (single and multi-dimensional)
- ✅ Structures and unions
- ✅ Typedefs and type aliases
- ✅ Constants and literals
- ✅ sizeof operator
- ✅ **Preprocessor directives (#define, #include, #if, etc.)**
- ✅ **Macro expansion (object-like and function-like)**
- ✅ **Conditional compilation**
- ✅ **Stringification and token pasting**
- ✅ **Predefined macros**
- ✅ Error detection and reporting

### Edge Cases and Error Conditions
- ✅ Syntax errors
- ✅ Type mismatches
- ✅ Undeclared variables
- ✅ Function signature mismatches
- ✅ Missing return statements
- ✅ Complex pointer arithmetic
- ✅ Nested data structures
- ✅ **Preprocessor errors (undefined macros, missing includes)**
- ✅ **Macro redefinition conflicts**
- ✅ **Unterminated preprocessor directives**
- ✅ **Complex macro expansion scenarios**

## Usage Instructions

1. **Run all tests**: `./run_tests.py`
2. **Run specific category**: Modify the test runner or run individual tests
3. **Add new tests**: Create `.c` and `.expected` files in appropriate directories
4. **Verify test correctness**: Tests are validated against GCC/Clang output

## Integration with C Frontend

Once the C frontend is implemented:
1. Update `run_tests.py` to use the new compiler
2. Add PIR generation verification
3. Add optimization and code generation tests
4. Expand test coverage for advanced C features

This comprehensive test suite provides complete coverage of both the C language and preprocessor, ensuring the C frontend implementation handles the full range of C89 features correctly.