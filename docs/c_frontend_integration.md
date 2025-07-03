# C Frontend Integration with Shared Infrastructure

## Overview

The C frontend has been refactored to use the shared frontend infrastructure from `compilerkit.frontend`, reducing code duplication and establishing patterns for other language frontends.

## Integration Components

### 1. Symbol Table Integration (`c_symbol_table.ml`)

Wraps the shared `Symbol_table` with C-specific type information:

```ocaml
type c_type_info =
  | Typedef of Ast.type_spec list * Ast.type_qualifier list
  | Variable of Ast.type_spec list * Ast.type_qualifier list  
  | Function of Ast.type_spec list * Ast.type_qualifier list * Ast.param_list option
  | EnumConstant of int64 option
  | StructTag of string option
  | UnionTag of string option
  | EnumTag of string option
```

Key features:
- Uses shared scope management
- Converts C lexer locations to shared position spans
- Provides C-specific API while using shared infrastructure

### 2. Error Reporting Integration (`c_errors.ml`)

Wraps the shared `Error_reporter` for structured error reporting:

```ocaml
(* Convert C lexer location to span *)
let span_of_location (loc: Lexer.location) =
  let pos = position_of_location loc in
  Position.span pos pos
```

Features:
- Parse errors with token information
- Lexer and preprocessor error reporting
- Warning support
- Formatted diagnostic output with source context

### 3. Lexer Utilities Integration (`c_lexer_utils.ml`)

Uses shared lexer utilities where possible:
- Character classification functions from `Lexer_utils`
- C-specific extensions (suffix parsing, keyword table)

### 4. Parser Integration (`parser_integrated.ml`)

Demonstrates how to integrate all components:
```ocaml
type parser_state = {
  tokens: located_token array;
  mutable pos: int;
  symbol_table: C_symbol_table.t;
  error_reporter: C_errors.t;
  mutable error_recovery: bool;
}
```

## Shared Library Enhancements

New generic modules added to `lib/frontend`:

### 1. Token Location (`token_location.ml`)
```ocaml
type 'token located_token = {
  token: 'token;
  span: span;
}
```

### 2. Parser Utilities (`parser_utils.ml`)
Common parser operations:
- State management
- Error recovery
- List parsing helpers
- Lookahead utilities

## Benefits

1. **Code Reuse**: Symbol tables, error reporting, and utilities are shared
2. **Consistency**: All frontends use the same infrastructure
3. **Maintainability**: Bug fixes and improvements benefit all frontends
4. **Type Safety**: Strongly-typed wrappers preserve language-specific semantics

## Usage Example

```ocaml
(* Create parser with integrated components *)
let parser_state = Parser_integrated.create_parser tokens in

(* Report errors with source context *)
C_errors.parse_error state.error_reporter loc "Unexpected token";

(* Manage symbols with scoping *)
C_symbol_table.enter_scope state.symbol_table;
C_symbol_table.add_typedef state.symbol_table "size_t" specs quals loc;
C_symbol_table.exit_scope state.symbol_table;

(* Format and display diagnostics *)
let files = Hashtbl.create 1 in
Hashtbl.add files filename content;
C_errors.print_diagnostics state.error_reporter files;
```

## Migration Guide

To integrate other language frontends:

1. Create language-specific wrappers for shared components
2. Define language-specific type information  
3. Convert between language-specific and shared position types
4. Use shared parser utilities for common operations
5. Leverage shared error reporting for consistent diagnostics

## Next Steps

1. Complete full parser integration (currently partial demonstration)
2. Add more shared utilities as patterns emerge
3. Document best practices for frontend integration
4. Consider adding more language-agnostic components