# C Preprocessor Test Suite

This directory contains comprehensive tests for the C preprocessor implementation, covering all major C89 preprocessor features and directives.

## Test Structure

### Basic Preprocessor Directives (4 tests)
- **simple_defines.c**: Basic `#define` directives for constants and simple macros
- **macro_redefinition.c**: `#undef` and macro redefinition
- **predefined_macros.c**: Predefined macros (`__FILE__`, `__LINE__`, `__DATE__`, `__TIME__`, etc.)
- **include_test.c**: `#include` directive with header files and include guards

### Function-like Macros (4 tests)
- **simple_function_macros.c**: Basic function-like macros with parameters
- **variadic_macros.c**: Variadic macros using `__VA_ARGS__` (C99 feature)
- **nested_macros.c**: Nested macro expansion and complex macro interactions
- **macro_side_effects.c**: Demonstrates macro side effects and multiple evaluation

### Conditional Compilation (4 tests)
- **ifdef_ifndef.c**: `#ifdef`, `#ifndef`, `#else`, `#endif` directives
- **if_expressions.c**: `#if` with expressions, `#elif`, and `defined()` operator
- **platform_specific.c**: Platform and compiler detection using conditional compilation
- **complex_nesting.c**: Complex nested conditional compilation scenarios

### Complex Macro Features (4 tests)
- **stringification.c**: Stringification operator (`#`) for converting macro arguments to strings
- **token_pasting.c**: Token pasting operator (`##`) for concatenating tokens
- **advanced_macros.c**: X-macro patterns and advanced code generation techniques
- **macro_recursion.c**: Macro recursion handling and self-referential macro prevention

### Negative Tests (5 tests)
- **undefined_macro.c**: Usage of undefined macros
- **macro_redefinition_error.c**: Macro redefinition without `#undef`
- **missing_include.c**: Missing include files
- **unterminated_if.c**: Unterminated conditional directives
- **invalid_directive.c**: Invalid preprocessor directives

## Key Features Tested

### C89 Preprocessor Features
✅ **Object-like macros**: `#define CONSTANT value`  
✅ **Function-like macros**: `#define FUNC(x) expression`  
✅ **Macro replacement**: Text substitution and parameter replacement  
✅ **Conditional compilation**: `#if`, `#ifdef`, `#ifndef`, `#else`, `#elif`, `#endif`  
✅ **File inclusion**: `#include <system.h>` and `#include "local.h"`  
✅ **Macro removal**: `#undef MACRO`  
✅ **Predefined macros**: `__FILE__`, `__LINE__`, `__DATE__`, `__TIME__`, `__STDC__`  

### Advanced Features  
✅ **Stringification**: `#param` converts parameter to string literal  
✅ **Token pasting**: `token1##token2` concatenates tokens  
✅ **Nested expansion**: Macros expanding to other macros  
✅ **Macro recursion prevention**: Self-referential macros don't expand infinitely  
✅ **Variadic macros**: `#define MACRO(...)` with `__VA_ARGS__` (C99)  
✅ **Complex expressions**: Mathematical and logical expressions in `#if`  
✅ **Platform detection**: Compiler and OS-specific conditional compilation  

### Error Handling
✅ **Undefined macro detection**  
✅ **Invalid directive handling**  
✅ **Unterminated conditional blocks**  
✅ **Missing include files**  
✅ **Macro redefinition warnings**  

## Test Examples

### Basic Macro Definition
```c
#define MAX_SIZE 100
#define PI 3.14159
#define GREETING "Hello, World!"
```

### Function-like Macros
```c
#define MAX(a, b) ((a) > (b) ? (a) : (b))
#define SQUARE(x) ((x) * (x))
#define DEBUG_PRINT(fmt, ...) printf("DEBUG: " fmt "\n", ##__VA_ARGS__)
```

### Conditional Compilation
```c
#ifdef DEBUG_MODE
    #define LOG(msg) printf("LOG: %s\n", msg)
#else
    #define LOG(msg) /* no-op */
#endif

#if VERSION_MAJOR >= 2 && defined(NEW_FEATURES)
    // New API
#else
    // Legacy API
#endif
```

### Stringification and Token Pasting
```c
#define STRINGIFY(x) #x
#define CONCAT(a, b) a##b
#define MAKE_FUNCTION(name) int func_##name() { return name##_value; }
```

### X-Macro Pattern
```c
#define COLORS \
    X(RED, 1) \
    X(GREEN, 2) \
    X(BLUE, 3)

typedef enum {
#define X(name, value) COLOR_##name = value,
    COLORS
#undef X
} color_t;
```

## Running Tests

The preprocessor tests are integrated into the main test runner:

```bash
# Run all tests (including preprocessor execution tests)
./run_tests.py

# Run only preprocessor output verification
./run_tests.py --preprocessor-only

# Run both execution and preprocessor output tests
./run_tests.py --include-preprocessor

# The test runner uses GCC/Clang to verify preprocessor behavior
# Once the C frontend is implemented, modify to use our preprocessor
```

### Preprocessor-Only Testing

The test suite includes dedicated preprocessor output verification using the `-E` flag to test macro expansion, conditional compilation, and other preprocessor features independently of compilation and execution. This is crucial for validating the preprocessor implementation separately from the parser and code generator.

**Test Files**:
- `*.c` - Source files with preprocessor directives
- `*.expected` - Expected execution output (for complete programs)
- `*.preprocessed` - Expected preprocessor output (macro-expanded source)

## Integration with C Frontend

When implementing the C preprocessor:

1. **Text Processing**: Handle file inclusion, macro expansion, and conditional compilation
2. **Macro Table**: Maintain symbol table for defined macros with parameters
3. **Token Stream**: Process and expand macros in the token stream before parsing
4. **Error Reporting**: Provide clear error messages for preprocessor failures
5. **C89 Compliance**: Ensure compatibility with C89 preprocessor specification

## Test Coverage Analysis

### Comprehensive Coverage
- **26 Preprocessor Tests** (21 positive + 5 negative)
- **All Major Directives**: `#define`, `#include`, `#if*`, `#undef`
- **Advanced Features**: Stringification, token pasting, variadic macros
- **Edge Cases**: Recursion, side effects, complex nesting
- **Error Conditions**: Invalid directives, missing files, undefined macros

### Real-world Scenarios
- Platform-specific compilation
- Feature toggles and configuration
- Code generation patterns (X-macros)
- Debug/release mode handling
- API versioning and compatibility

This comprehensive test suite ensures the C preprocessor implementation handles the full range of C89 preprocessor features correctly and robustly.