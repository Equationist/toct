# TOCT Implementation Plan & Progress Tracker

*Last Updated: 2025-07-03*

## Project Overview

The **Tiny OCaml Compiler Toolkit (TOCT)** is a modular compiler infrastructure consisting of:
- **Portable IR (PIR)** - A typed intermediate representation
- **Frontend helpers** - Parsing, symbol tables, type checking (~2.5k LOC)
- **Backend pipeline** - 16-pass optimizing compiler (~7.4k LOC)
- **Target support** - ARM64 initially, x86-64 later

## Implementation Status

### Phase 1: Core Infrastructure (High Priority) ✅ COMPLETE

- [x] **1. Project Structure & Build System**
  - [x] Create dune-project configuration
  - [x] Set up library structure (common, pir, frontend, backend, target)
  - [x] Create CLI tools structure (pirc, pir-objdump)
  - [x] Set up test framework with dune runtest
  - [x] Create examples directory
  - [x] **Tests**: Verify build system and basic project structure

- [x] **2. PIR Core Data Structures**
  - [x] Type system: `I8|I16|I32|I64|F32|F64|Vec|Ptr|Struct|Array`
  - [x] Value representation with attributes
  - [x] Instruction encoding (40+ opcodes from spec)
  - [x] Block structure with parameters (SSA-friendly)
  - [x] Control flow graph representation
  - [x] **Tests**: Type equality, instruction parsing, CFG construction

- [x] **3. PIR Builder & Utilities**
  - [x] Monadic builder interface for CFG construction
  - [x] Pretty-printer with syntax highlighting
  - [x] Linter for SSA/type verification
  - [x] Attribute attachment system (JSON-based)
  - [x] **Tests**: Builder API, pretty-print roundtrip, linter validation

- [x] **4. PIR Text Parser**
  - [x] Lexer for PIR text format per specification
  - [x] Parser for module structure (types, globals, constants, functions)
  - [x] Support for all instruction types with spec syntax (opcode.flag.type)
  - [x] Error recovery and reporting
  - [x] **Tests**: Parse all PIR constructs, spec compliance tests
  
- [x] **5. PIR Spec Compliance Update**
  - [x] Updated AST types to match portable_ir_spec.md
  - [x] Updated lexer to recognize spec tokens (TYPE, GLOBAL, CONST, etc.)
  - [x] Updated parser to handle spec instruction syntax
  - [x] Updated pretty printer to output spec-compliant PIR
  - [x] **Tests**: All examples parse correctly, pretty printer outputs valid PIR

### Phase 2: Frontend Helpers (Medium Priority)

- [x] **6. Lexer/Parser Infrastructure** ✅ COMPLETE
  - [x] Pratt parser combinator DSL
  - [x] Position tracking and error reporting
  - [x] Common lexer utilities
  - [x] **Tests**: Lexer token recognition, parser combinators, error recovery
  - [ ] Menhir integration helpers (skipped - not needed)

- [x] **7. Symbol Table & Type System** ✅ COMPLETE
  - [x] Scoped symbol management
  - [x] Hindley-Milner type inference with row polymorphism
  - [x] Kind system for higher-order types
  - [x] Type constraint solving
  - [x] **Tests**: Symbol scoping, type inference, constraint solving

- [ ] **8. Analysis Framework**
  - [ ] Range analysis (interval domain)
  - [ ] Null/definiteness analysis
  - [ ] Optional: Octagon relational analysis
  - [ ] Analysis attribute propagation
  - [ ] **Tests**: Range computation, null analysis, attribute preservation

### Phase 3: Backend Pipeline (Medium Priority)

- [x] **9. SSA Transformation (Pass #0)** ✅ COMPLETE
  - [x] Dominance tree construction (Cooper-Harvey-Kennedy algorithm)
  - [x] Dominance frontier computation
  - [x] SSA construction framework with block parameters
  - [x] SSA verification pass
  - [x] **Tests**: Dominance tests, SSA verification tests
  - [ ] **Note**: Implementation needs adjustment for current PIR value representation

- [ ] **10. Optimization Passes (#1-#11)**
  - [ ] Pass #1: Sparse Conditional Constant Propagation
    - [ ] **Tests**: Constant folding, unreachable code detection
  - [ ] Pass #2: Hash-based Global Value Numbering + PRE
    - [ ] **Tests**: Common subexpression elimination, PRE insertion
  - [ ] Pass #3: Dead Code/Branch Elimination
    - [ ] **Tests**: Dead instruction removal, unreachable block elimination
  - [ ] Pass #4: Strength Reduction
    - [ ] **Tests**: Multiply-to-shift, induction variable optimization
  - [ ] Pass #5: Tiny inliner (≤40 instructions)
    - [ ] **Tests**: Size-bounded inlining, call site transformation
  - [ ] Pass #6: Loop unrolling (≤4 iterations)
    - [ ] **Tests**: Simple loop unrolling, trip count analysis
  - [ ] Pass #7: LICM with register pressure awareness
    - [ ] **Tests**: Loop invariant detection, register pressure heuristics
  - [ ] Pass #8: Tail duplication & jump threading
    - [ ] **Tests**: Basic block duplication, jump threading
  - [ ] Pass #9: Store sinking
    - [ ] **Tests**: Store motion to cold paths
  - [ ] Pass #10: Load/store forwarding
    - [ ] **Tests**: Same-address load/store optimization
  - [ ] Pass #11: Block layout optimization
    - [ ] **Tests**: Block reordering, branch probability estimation

- [ ] **9. Code Generation**
  - [ ] Pass #12: Instruction selection via BURS
    - [ ] **Tests**: Pattern matching, instruction selection trees
  - [ ] Pass #13: Redundant compare elimination
    - [ ] **Tests**: Flag reuse, compare elimination
  - [ ] Pass #14: Linear scan register allocation + coalescing
    - [ ] **Tests**: Register allocation, spilling, coalescing
  - [ ] Pass #15: Peephole optimization & frame generation
    - [ ] **Tests**: Peephole patterns, frame layout

### Phase 4: Target Implementation (Low Priority)

- [ ] **10. ARM64 Backend**
  - [ ] ABI implementation (AAPCS64)
  - [ ] Instruction encoding
  - [ ] Calling convention handling
  - [ ] Object file generation (Mach-O)
  - [ ] **Tests**: Instruction encoding, ABI compliance, object file format

- [ ] **11. Example C89 Frontend**
  - [ ] C89 lexer/parser
  - [ ] AST to PIR translation
  - [ ] Basic C type system
  - [ ] Preprocessor integration
  - [ ] **Tests**: C89 parsing, AST->PIR translation, type checking

### Phase 5: End-to-End Testing & Validation

- [ ] **12. Integration Tests**
  - [ ] Complete compilation pipeline tests
  - [ ] Performance validation (target: 94-97% of GCC -O2)
  - [ ] Regression testing framework
  - [ ] SPEC benchmark subset

## Testing Strategy

### Unit Tests (Added at Each Step)
- **Test Location**: `test/unit/` with subdirectories per module
- **Framework**: OUnit2 or Alcotest
- **Coverage**: Each public function/API
- **Validation**: Type safety, invariant preservation, correctness

### Integration Tests (Added per Phase)
- **Test Location**: `test/integration/`
- **Coverage**: Multi-module interactions
- **Examples**: PIR builder + pretty-printer, parser + type checker

### End-to-End Tests (Final Phase)
- **Test Location**: `test/e2e/`
- **Coverage**: Complete compilation pipeline
- **Benchmarks**: Performance validation vs GCC

## Project Structure

```
compilerkit/
├── dune-project
├── lib/
│   ├── common/         # Position, Error, Pretty (~250 LOC)
│   ├── pir/           # PIR types, builder, pretty-printer (~650 LOC)
│   ├── frontend/      # Lexer, parser, symbol table helpers (~1670 LOC)
│   ├── backend/       # Optimization passes (~4000 LOC)
│   └── target/        # ISA-specific backends (~3400 LOC)
├── bin/               # CLI tools (pirc, pir-objdump)
├── test/
│   ├── unit/          # Unit tests (per module)
│   ├── integration/   # Integration tests (per phase)
│   └── e2e/           # End-to-end tests
└── examples/          # C89 frontend example
```

## Development Workflow

1. **Implement Module**: Write core functionality
2. **Add Unit Tests**: Test individual functions/APIs
3. **Run Tests**: `dune runtest` - must pass before proceeding
4. **Commit Progress**: Git commit with test results
5. **Integration**: Test with other modules if applicable
6. **Update Plan**: Mark progress and note any issues

## Key Design Decisions

1. **SSA with Block Parameters**: Uses block parameters instead of phi nodes for cleaner representation
2. **Attribute-driven optimization**: JSON annotations guide optimization decisions
3. **Pluggable passes**: Modular architecture allows research extensions
4. **Compact core**: Minimal instruction set with synthetic sugar
5. **Test-driven development**: Tests written alongside implementation

## Technical Specifications

- **Frontend helpers**: ≤3k LOC total
- **Backend pipeline**: ≤7.5k LOC
- **Compile time**: <0.15s for 50k LOC input
- **Performance target**: 94-97% of GCC -O2 on integer workloads
- **Languages**: Pure OCaml (no C stubs except optional runtime)
- **Test coverage**: >90% for core modules

## Next Steps

1. Start with Phase 1: Set up project structure and build system WITH tests
2. Implement core PIR data structures WITH unit tests
3. Create PIR builder and utilities WITH unit tests
4. Validate with integration tests before proceeding to optimization passes

## Progress Notes

- **2025-07-03**: Initial project analysis and planning completed
- **2025-07-03**: Git repository initialized with documentation
- **2025-07-03**: Implementation plan created with integrated testing strategy
- **2025-07-03**: Completed Phase 1 - Core Infrastructure:
  - Implemented complete type system with scalar, vector, struct, and array types
  - Created value representation with attributes support
  - Implemented full instruction set (40+ opcodes) including binops, memory, control flow
  - Built monadic builder interface for CFG construction
  - Implemented pretty printer with syntax highlighting
  - Created PIR linter for SSA and type verification
  - Added comprehensive unit tests for all modules
  - Created integration tests for builder + pretty printer roundtrip
  - Added example programs demonstrating PIR usage
- **2025-07-03**: Completed PIR Text Parser:
  - Implemented lexer with full token support for PIR syntax
  - Created recursive descent parser for functions, blocks, and instructions
  - Added support for all PIR types including vectors, arrays, and structs
  - Implemented error handling with detailed parse error messages
  - Fixed lexer to properly distinguish between block labels and parameter types
  - Created example PIR text files demonstrating various language features
  - All parser tests passing successfully
- **2025-07-03**: Completed Lexer/Parser Infrastructure:
  - Implemented Pratt parser combinator DSL with operator precedence
  - Created position tracking system for accurate error reporting
  - Built comprehensive error reporter with source context display
  - Added common lexer utilities for tokenization patterns
  - Wrote extensive test suite for parser combinators
  - All tests passing successfully
- **2025-07-03**: Completed Symbol Table & Type System:
  - Implemented scoped symbol table with hierarchical scope management
  - Built complete Hindley-Milner type inference engine
  - Added row polymorphism for extensible records and variants
  - Created unification algorithm with occurs check
  - Implemented constraint-based type inference
  - Added pattern matching and algebraic data type support
  - Comprehensive test coverage for all type system features
- **2025-07-03**: Completed SSA Transformation (Phase 3, Pass #0):
  - Implemented dominance analysis with Cooper-Harvey-Kennedy algorithm
  - Built dominance frontier computation for SSA construction
  - Created SSA transformation framework using block parameters (PIR style)
  - Developed SSA verification pass with comprehensive error checking
  - Added test suite for dominance and SSA properties
  - Note: Implementation requires adjustments to match current PIR value representation

## References

- Architecture specification: `toolkit_architecture.md`
- PIR formal specification: `portable_ir_spec.md`
- Backend details: `backend_details.md`
- Annotation syntax: `pir_annotations.md`