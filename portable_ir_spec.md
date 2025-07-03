# Portable Intermediate Representation (PIR) – Formal Specification

*Version 0.9 • June 2025*

---

## 1  Purpose and Design Goals

1. **Front‑end simplicity.**  A compiler front‑end can emit PIR directly from an AST walk without platform knowledge.
2. **Back‑end portability.**  A small OCaml back‑end can lower PIR to many ISAs by filling in target‑specific layout and ABI details.
3. **Predictable semantics.**  Each instruction has fully‑specified behaviour—undefined behaviour (*UB*) only where it unlocks optimisation.
4. **Compact core.**  The entire instruction surface fits on a single page; everything else is syntactic sugar.

---

## 2  Lexical Conventions

| Element         | Regex / description                 |                              |
| --------------- | ----------------------------------- | ---------------------------- |
| Identifier      | `[A‑Z_a‑z][0‑9A‑Z_a‑z]*`            |                              |
| Integer literal | \`0                                 | [1‑9][0‑9]\*\`  (decimal)    |
| Float literal   | \`([0‑9]+.[0-9]\*                   | .[0‑9]+)([eE][+-]?[0‑9]+)?\` |
| Comment         | `;` to end of line                  |                              |
| Label           | identifier followed by `:`          |                              |
| Keyword         | lower‑case opcode (`add`, `jmp`, …) |                              |

All tokens are separated by whitespace except within brackets `[]`, chevrons `<<>>`, or commas.

---

## 3  Module Structure

A translation unit is a sequence (order‑independent) of the following top‑level items:

```ebnf
module      ::= { top_item }

top_item    ::= type_decl | global_decl | const_decl | func_decl
```

### 3.1  Type Declarations

```
type <name> = <agg_type>
```

*Example*: `type Vec3 = struct<<f32, f32, f32>>`

### 3.2  Object Declarations

```
global <id> : <Ty> [align N] init <const_expr>
const  <id> : <Ty> [align N] init <const_expr>
```

`global` objects are mutable; `const` objects are read‑only and link‑time foldable.

### 3.3  Function Definition Skeleton

```
func <name>(<param_list>) -> <retTy>
  <basic_block>*
endfunc
```

A *basic block* begins with a label and terminates with exactly one control‑transfer instruction (`br`, `jmp`, `ret`, `switch`, or `unreachable`).

---

## 4  Type System

| Category      | Syntax example                    | Notes                                                                                                              |
| ------------- | --------------------------------- | ------------------------------------------------------------------------------------------------------------------ |
| Integer       | `i8`, `i16`, `i32`, `i64`         | Two’s‑complement. Natural alignment = size.                                                                        |
| Float         | `f32`, `f64`                      | IEEE‑754 binary32/64.                                                                                              |
| Vector        | `v4xi32`, `v8xf32`                | `v<N>x<ScalarTy>` packed.                                                                                          |
| Array         | `array[16]i8`                     | Size known at IR time.                                                                                             |
| Struct        | `struct<<i32, f64>>`              | Natural layout: each field aligned to its natural alignment; overall align = next power‑of‑two of max field align. |
| Packed struct | `packed_struct<<u8, u8, u8, u8>>` | No implicit padding; always `align 1` unless override.                                                             |

`align N` may follow any aggregate or object; `N` must be a power of two.

Pointers are *opaque* and untyped (`ptr`). The back‑end assigns bit‑width.

---

## 5  Address‑Calculation Instructions

| Result | Opcode & Syntax       | Semantics                                                                        |
| ------ | --------------------- | -------------------------------------------------------------------------------- |
| `ptr`  | `gep base, idx`       | Pointer = `base + idx * sizeof(element(base))` (array element).                  |
| `ptr`  | `fieldaddr base, k`   | Pointer to field *k* of an aggregate. Uses field index, **never** a byte offset. |
| `ptr`  | `ptradd base, offset` | Raw byte addition (`offset` is `iN`). UB if result overflows pointer space.      |
| `T`    | `bitcast val`         | Reinterpret bits; only between equal‑sized types.                                |

---

## 6  Memory‑Access Instructions

| Result | Syntax                    | Description                                         |
| ------ | ------------------------- | --------------------------------------------------- |
| `Ty`   | `load.Ty [ptr]`           | Reads `sizeof(Ty)` bytes; UB if misaligned.         |
| —      | `store.Ty val, [ptr]`     | Writes bytes of `val` to memory.                    |
| `ptr`  | `alloca size align A`     | Allocates `size` bytes in the caller’s stack frame. |
| —      | `memcpy dst, src, bytes`  | Copy (may overlap).                                 |
| —      | `memset dst, byte, bytes` | Fill with byte value.                               |

---

## 7  Arithmetic & Bit‑Manipulation Families

Instruction spelling is `op[.flag].Ty`, where `flag` ∈ `{nsw, carry, sat}` or empty (wrapping).

### 7.1  Integer (`i*`) and Vector (`vNx i*`) Ops

| Family     | Opcodes                        | Notes                     |
| ---------- | ------------------------------ | ------------------------- |
| Add/Sub    | `add`, `sub`                   | With variants.            |
| Multiply   | `mul`                          | —                         |
| Div/Rem    | `sdiv`, `udiv`, `srem`, `urem` | Signed vs unsigned.       |
| Bitwise    | `and`, `or`, `xor`, `not`      | —                         |
| Shifts     | `shl`, `lshr`, `ashr`          | UB if shift ≥ bit‑width.  |
| Rotates    | `rol`, `ror`                   | Amt masked modulo width.  |
| Population | `clz`, `ctz`, `popcnt`         | Result type = input type. |

### 7.2  Floating‑Point (`f*`) Ops

`fadd`, `fsub`, `fmul`, `fdiv`, `frem`, `fneg`, `fma` obey IEEE‑754; no fast‑math flags are defined yet.

---

## 8  Vector Lane Ops

| Result  | Syntax                      | Meaning                                      |
| ------- | --------------------------- | -------------------------------------------- |
| `vNxTy` | `splat scalar, lanes`       | Replicate.                                   |
| `vNxTy` | `shuffle vA, vB, mask`      | Arbitrary permute; `mask` is constant array. |
| `Ty`    | `extractlane v, idx`        | —                                            |
| `vNxTy` | `insertlane v, idx, scalar` | —                                            |

---

## 9  Comparison and Selection

| Result | Syntax                       | Semantics                                                             |
| ------ | ---------------------------- | --------------------------------------------------------------------- |
| `i1`   | `icmp.<pred>.i32 a, b`       | `pred` ∈ `eq, ne, lt, le, gt, ge` + signed `s` / unsigned `u` prefix. |
| `i1`   | `fcmp.<fpred>.f64 a, b`      | IEEE ordered/unordered sets (`oeq`, `olt`, `une`, …).                 |
| `Ty`   | `select cond, vTrue, vFalse` | Value select (SSA‑friendly ternary).                                  |

---

## 10  Control Flow

| Syntax                                    | Terminates BB | Notes                                 |
| ----------------------------------------- | ------------- | ------------------------------------- |
| `br cond, label True, label False`        | ✔             | Conditional branch.                   |
| `jmp label`                               | ✔             | Unconditional.                        |
| `switch val, default, [(case0, lbl0), …]` | ✔             | Dense or sparse; lowered by back‑end. |
| `ret` / `ret val`                         | ✔             | Function return.                      |
| `unreachable`                             | ✔             | Semantic bottom.                      |

### 10.1  Calls

```
call.retTy callee, arg0, arg1, …
tailcall.retTy callee, …          ; eligible for TCO
```

The back‑end maps arguments/return to registers/stack per ABI.

---

## 11  Overflow Flag Semantics

- **Wrap (default).**  Two’s‑complement modulo 2^N.
- ``**\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*.**  UB on *signed* overflow (no‑signed‑wrap). Enables strength‑reduce, GVN, etc.
- ``**\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*.**  Produces tuple `(result, carry:i1)`.
- ``**\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*\*.**  Saturates to signed/unsigned min/max.

Vector instructions apply the flag lane‑wise.

---

## 12  Memory Model and UB Summary

- Loads/stores must be naturally aligned unless an explicit `align` allows larger.
- All pointer arithmetic (`ptradd`, `gep`) must stay within the originating object (*in‑bounds*) except past‑the‑end for arrays.
- Shift amounts ≥ bit‑width, or negative, are UB.
- `div`/`rem` by zero or signed min ÷ −1 are UB.
- `bitcast` between unequal sizes is UB.

No atomics, threads, or volatile semantics are defined in v0.9.

---

## 13  Extended BNF (excerpt)

```ebnf
<type>           ::= <prim> | ptr | array "[" <uint> "]" <type>
                   | struct "<<" <type_list> ">>"
                   | packed_struct "<<" <type_list> ">>"
<type_list>      ::= <type> { "," <type> }

<instr>          ::= <result>? "=" <opcode> [ "." <flag> ] "." <type> <operands>

<flag>           ::= "nsw" | "carry" | "sat"
<result>         ::= <id>
```

Full grammar is mechanically derivable from the tables above.

---

## 14  Illustrative Example

```ir
; --- module‑scope ---

global counter:i32 init 0

global wide_vec:v4xi32 align 16 init <<0,0,0,0>>

const packed_byte:i8 align 1 init 0

type Vec3 = struct<<f32, f32, f32>>

func foo() -> void
entry:
  buf = alloca 64 align 16
  unreachable
endfunc

func inc() -> void
entry:
  val = load.i32 [counter]
  val = add.nsw.i32 val, 1
  store.i32 val, [counter]
  ret
endfunc
```

---

## 15  Conformance & Versioning

A generator **must** emit IR conforming to this document. A consumer (optimizer, back‑end) **may** reject files with UB but **must not** silently miscompile conforming ones.

Version is embedded by a leading comment `; PIR‑v0.9`.

---

© 2025 Open Spec Authors.  Licensed CC BY 4.0.

Actually do the same but add atomic and volatiles to the semantics.
