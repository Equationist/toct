# TOCT Implementation Plan & Progress Tracker

*Last Updated: 2025-07-03*

## Project Overview

The **Tiny OCaml Compiler Toolkit (TOCT)** is a modular compiler infrastructure consisting of:
- **Portable IR (PIR)** - A typed intermediate representation
- **Frontend helpers** - Parsing, symbol tables, type checking (~2.5k LOC)
- **Backend pipeline** - 16-pass optimizing compiler (~7.4k LOC)
- **Target support** - ARM64 initially, x86-64 later

## Implementation Status

### Phase 1: Core Infrastructure (High Priority)

- [ ] **1. Project Structure & Build System**
  - [ ] Create dune-project configuration
  - [ ] Set up library structure (common, pir, frontend, backend, target)
  - [ ] Create CLI tools structure (pirc, pir-objdump)
  - [ ] Set up test framework with dune runtest
  - [ ] Create examples directory
  - [ ] **Tests**: Verify build system and basic project structure

- [ ] **2. PIR Core Data Structures**
  - [ ] Type system: `I8|I16|I32|I64|F32|F64|Vec|Ptr|Struct|Array`
  - [ ] Value representation with attributes
  - [ ] Instruction encoding (40+ opcodes from spec)
  - [ ] Block structure with parameters (SSA-friendly)
  - [ ] Control flow graph representation
  - [ ] **Tests**: Type equality, instruction parsing, CFG construction

- [ ] **3. PIR Builder & Utilities**
  - [ ] Monadic builder interface for CFG construction
  - [ ] Pretty-printer with syntax highlighting
  - [ ] Linter for SSA/type verification
  - [ ] Attribute attachment system (JSON-based)
  - [ ] **Tests**: Builder API, pretty-print roundtrip, linter validation

### Phase 2: Frontend Helpers (Medium Priority)

- [ ] **4. Lexer/Parser Infrastructure**
  - [ ] Pratt parser combinator DSL
  - [ ] Menhir integration helpers
  - [ ] Position tracking and error reporting
  - [ ] Common lexer utilities
  - [ ] **Tests**: Lexer token recognition, parser combinators, error recovery

- [ ] **5. Symbol Table & Type System**
  - [ ] Scoped symbol management
  - [ ] Hindley-Milner type inference with row polymorphism
  - [ ] Kind system for higher-order types
  - [ ] Type constraint solving
  - [ ] **Tests**: Symbol scoping, type inference, constraint solving

- [ ] **6. Analysis Framework**
  - [ ] Range analysis (interval domain)
  - [ ] Null/definiteness analysis
  - [ ] Optional: Octagon relational analysis
  - [ ] Analysis attribute propagation
  - [ ] **Tests**: Range computation, null analysis, attribute preservation

### Phase 3: Backend Pipeline (Medium Priority)

- [ ] **7. SSA Transformation (Pass #0)**
  - [ ] Braun-Buchwald pruned SSA construction
  - [ ] Dominance frontier computation
  - [ ] Block parameter insertion
  - [ ] SSA verification
  - [ ] **Tests**: SSA construction on simple CFGs, dominance correctness

- [ ] **8. Optimization Passes (#1-#11)**
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

## References

- Architecture specification: `toolkit_architecture.md`
- PIR formal specification: `portable_ir_spec.md`
- Backend details: `backend_details.md`
- Annotation syntax: `pir_annotations.md`