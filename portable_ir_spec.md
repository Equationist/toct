# Portable Intermediate Representation (PIR) – Formal Specification

*Version 0.9.1 • June 2025*

---

## 1  Purpose and Design Goals

1. **Front‑end simplicity.**  A compiler front‑end can emit PIR directly from an AST walk without platform knowledge.
2. **Back‑end portability.**  A small OCaml back‑end can lower PIR to many ISAs by filling in target‑specific layout and ABI details.
3. **Predictable semantics.**  Each instruction has fully‑specified behaviour—undefined behaviour (*UB*) only where it unlocks optimisation.
4. **Compact core.**  The entire instruction surface fits on a single page; everything else is syntactic sugar.

---

## 2  Lexical Conventions

| Element         | Regex / description                 |                              |
| --------------- | ----------------------------------- | ---------------------------- |
| Identifier      | `[A‑Z_a‑z][0‑9A‑Z_a‑z]*`            |                              |
| Integer literal | \`0                                 | [1‑9][0‑9]\*\`  (decimal)    |
| Float literal   | \`([0‑9]+.[0-9]\*                   | .[0‑9]+)([eE][+-]?[0‑9]+)?\` |
| Comment         | `;` to end of line                  |                              |
| Label           | identifier followed by `:`          |                              |
| Keyword         | lower‑case opcode (`add`, `jmp`, …) |                              |

All tokens are separated by whitespace except within brackets `[]`, chevrons `<<>>`, or commas.

### 2.1  Value Naming Conventions

- **Local values**: `v0`, `v1`, `v2`, ... (SSA temporaries)
- **Global references**: `@global_name` (functions and global variables)
- **Parameters**: `%param0`, `%param1`, ... (function parameters)
- **Block parameters**: Handled via SSA block parameter lists

---

## 3  Module Structure

A translation unit is a sequence (order‑independent) of the following top‑level items:

```ebnf
module      ::= [module_decl] { top_item }

module_decl ::= MODULE <string_literal> [TARGET <string_literal>]

top_item    ::= type_decl | global_decl | const_decl | func_decl | extern_decl
```

### 3.1  Module Declaration (Optional)

```
MODULE "module_name" [TARGET "triple"]
```

*Example*: `MODULE "mylib" TARGET "arm64-apple-darwin"`

### 3.2  Type Declarations

```
type <name> = <agg_type>
```

*Example*: `type Vec3 = struct<<f32, f32, f32>>`

### 3.3  Object Declarations

```
global <id> : <Ty> [align N] init <const_expr>
const  <id> : <Ty> [align N] init <const_expr>
```

`global` objects are mutable; `const` objects are read‑only and link‑time foldable.

### 3.4  External Declarations

```
EXTERN FUNC <name>(<param_types>) -> <retTy> [attributes]
EXTERN GLOBAL <id> : <Ty> [attributes]
```

*Examples*:
- `EXTERN FUNC printf(ptr, ...) -> i32`
- `EXTERN GLOBAL @errno : i32`

### 3.5  Function Definition Skeleton

```
func <name>(<param_list>) -> <retTy>
  <basic_block>*
endfunc
```

A *basic block* begins with a label and terminates with exactly one control‑transfer instruction (`br`, `jmp`, `ret`, `switch`, or `unreachable`).

---

## 4  Type System

| Category      | Syntax example                    | Notes                                                                                                              |
| ------------- | --------------------------------- | ------------------------------------------------------------------------------------------------------------------ |
| Integer       | `i8`, `i16`, `i32`, `i64`         | Two's‑complement. Natural alignment = size.                                                                        |
| Float         | `f32`, `f64`                      | IEEE‑754 binary32/64.                                                                                              |
| Vector        | `v4xi32`, `v8xf32`                | `v<N>x<ScalarTy>` packed.                                                                                          |
| Array         | `array[16]i8`                     | Size known at IR time.                                                                                             |
| Struct        | `struct<<i32, f64>>`              | Natural layout: each field aligned to its natural alignment; overall align = next power‑of‑two of max field align. |
| Packed struct | `packed_struct<<u8, u8, u8, u8>>` | No implicit padding; always `align 1` unless override.                                                             |

`align N` may follow any aggregate or object; `N` must be a power of two.

Pointers are *opaque* and untyped (`ptr`). The back‑end assigns bit‑width.

---

## 5  Address‑Calculation Instructions

| Result | Opcode & Syntax       | Semantics                                                                        |
| ------ | --------------------- | -------------------------------------------------------------------------------- |
| `ptr`  | `gep base, idx`       | Pointer = `base + idx * sizeof(element(base))` (array element).                  |
| `ptr`  | `fieldaddr base, k`   | Pointer to field *k* of an aggregate. Uses field index, **never** a byte offset. |
| `ptr`  | `ptradd base, offset` | Raw byte addition (`offset` is `iN`). UB if result overflows pointer space.      |
| `T`    | `bitcast val`         | Reinterpret bits; only between equal‑sized types.                                |

---

## 6  Memory‑Access Instructions

| Result | Syntax                    | Description                                         |
| ------ | ------------------------- | --------------------------------------------------- |
| `T`    | `load.T [ptr]`            | Load value of type `T` from address. May have attributes for volatility/atomicity. |
| *()*   | `store.T val, [ptr]`      | Store `val : T` to address. May have attributes.   |
| `ptr`  | `alloca size align`       | Stack allocation returning pointer. `size` = element count. |
| *()*   | `memcpy dst, src, bytes`  | Copy `bytes` bytes from `src` to `dst`.            |
| *()*   | `memset dst, byte, bytes` | Set `bytes` bytes at `dst` to `byte`.              |

---

## 7  Integer‑Arithmetic Instructions

### 7.1  Binary Operations (all have `<op>.<flag>`)

| Result  | Opcode               | Semantics                                                        |
| ------- | -------------------- | ---------------------------------------------------------------- |
| `iN`    | `add`, `sub`, `mul`  | Wrapping arithmetic by default.                                  |
| `iN`    | `sdiv`, `udiv`       | Signed/unsigned division. `sdiv` rounds toward 0. UB if divisor = 0. |
| `iN`    | `srem`, `urem`       | Remainder after division. Same UB as division.                   |
| `iN`    | `and`, `or`, `xor`   | Bitwise operations.                                              |
| `iN`    | `shl`, `lshr`, `ashr`| Left shift, logical/arithmetic right shift. UB if shift ≥ width. |
| `iN`    | `rol`, `ror`         | Rotate left/right.                                               |
| `iN`    | `clz`, `ctz`         | Count leading/trailing zeros. UB if input = 0.                  |
| `iN`    | `popcnt`             | Population count (number of 1 bits).                             |

Flags:
- `.nsw` – No signed wrap (signed overflow is UB).
- `.carry` – Result is `{iN, i1}` tuple with carry‑out.
- `.sat` – Saturates to `[MIN, MAX]` on overflow (unsigned: `[0, MAX]`).

### 7.2  Comparison

| Result | Syntax               | Description                              |
| ------ | -------------------- | ---------------------------------------- |
| `i1`   | `icmp.<pred> v1, v2` | Integer comparison yielding 0 or 1.      |

Predicates: `eq`, `ne`, `slt`, `sle`, `sgt`, `sge`, `ult`, `ule`, `ugt`, `uge`.

---

## 8  Floating‑Point Instructions

| Result | Opcode                                | Semantics                                                   |
| ------ | ------------------------------------- | ----------------------------------------------------------- |
| `fN`   | `fadd`, `fsub`, `fmul`, `fdiv`, `frem`| IEEE‑754 operations.                                        |
| `fN`   | `fma v1, v2, v3`                      | Fused multiply‑add: `(v1 × v2) + v3` with single rounding. |
| `i1`   | `fcmp.<pred> v1, v2`                  | Comparison; ordered predicates yield 0 on NaN.              |

Predicates: `oeq`, `one`, `olt`, `ole`, `ogt`, `oge`, `ord` (ordered), `ueq`, `une`, `ult`, `ule`, `ugt`, `uge`, `uno` (unordered).

---

## 9  Type‑Conversion Instructions

| Result | Opcode | Input | Semantics                                            |
| ------ | ------ | ----- | ---------------------------------------------------- |
| `iM`   | `trunc`| `iN`  | Truncate to smaller integer (N > M).                 |
| `iM`   | `zext` | `iN`  | Zero‑extend to larger integer (N < M).               |
| `iM`   | `sext` | `iN`  | Sign‑extend to larger integer (N < M).               |
| `fM`   | `fptrunc`| `fN`| Round to smaller float type (N > M).                 |
| `fM`   | `fpext`| `fN`  | Extend to larger float type (N < M).                 |
| `iN`   | `fptoui`| `fM` | Float to unsigned int. UB if out of range or NaN.    |
| `iN`   | `fptosi`| `fM` | Float to signed int. UB if out of range or NaN.      |
| `fN`   | `uitofp`| `iM` | Unsigned int to float.                               |
| `fN`   | `sitofp`| `iM` | Signed int to float.                                 |

---

## 10  Vector Instructions

| Result        | Syntax                          | Description                                      |
| ------------- | ------------------------------- | ------------------------------------------------ |
| `v<N>x<T>`    | `splat scalar, N`               | Broadcast scalar to all N lanes.                |
| `v<N>x<T>`    | `shuffle vA, vB, mask`          | Select lanes via immediate mask.                |
| `T`           | `extractlane v, k`              | Extract lane k.                                  |
| `v<N>x<T>`    | `insertlane v, k, scalar`       | Replace lane k with scalar.                     |

---

## 11  Control‑Flow Instructions

### 11.1  Terminators

| Syntax                                    | Description                                                     |
| ----------------------------------------- | --------------------------------------------------------------- |
| `ret [val]`                               | Return from function. Omit `val` for void functions.           |
| `br cond, then_label, else_label`         | Conditional branch on `i1` condition.                           |
| `jmp label`                               | Unconditional jump.                                             |
| `switch val, default_label, [(k, label)*]`| Multi‑way branch on integer `val`.                              |
| `unreachable`                             | Marks unreachable code; UB if executed.                        |

### 11.2  Function Calls

| Result  | Syntax                        | Description                                    |
| ------- | ----------------------------- | ---------------------------------------------- |
| `T`     | `call.T callee(args)`         | Regular call returning `T`.                    |
| `T`     | `tailcall.T callee(args)`     | Tail call (reuses caller's frame if possible).|

### 11.3  Other Control Instructions

| Result  | Syntax                        | Description                                    |
| ------- | ----------------------------- | ---------------------------------------------- |
| `T`     | `select cond, vtrue, vfalse`  | Ternary operator: `cond ? vtrue : vfalse`.    |
| `T`     | `freeze val`                  | Convert undef/poison to arbitrary fixed value. |

---

## 12  Data‑Movement Instructions

| Result | Syntax                         | Description                                    |
| ------ | ------------------------------ | ---------------------------------------------- |
| `T`    | `phi [(val, pred_label)*]`     | SSA φ‑node selecting value based on CFG predecessor. |
| `T`    | `extractvalue agg, indices`    | Extract field from aggregate value.            |
| `agg`  | `insertvalue agg, val, indices`| Insert value into aggregate.                   |

---

## 13  Synchronization Instructions

| Result | Syntax                         | Description                                    |
| ------ | ------------------------------ | ---------------------------------------------- |
| *()*   | `fence <ordering>`             | Memory fence with specified ordering.          |

Ordering attributes can be specified via the attribute system (e.g., `<<atomic: "acquire">>`).

---

## 14  Variadic Support

| Result | Syntax                         | Description                                    |
| ------ | ------------------------------ | ---------------------------------------------- |
| `T`    | `va_arg va_list, T`            | Extract next argument of type T from va_list. |

---

## 15  Attributes

Attributes use JSON syntax and can be attached to functions, instructions, and values:

```
func foo(%x: i32) -> i32 <<linkage: "weak", call_conv: "c">>
v5 = load.i32 [%ptr] <<volatile: true, atomic: "acquire">>
```

### 15.1  Standard Attribute Namespaces

- `pir.*` - Core PIR attributes (e.g., `pir.inline`, `pir.nounwind`)
- `debug.*` - Debug information (e.g., `debug.loc`, `debug.var`)
- `opt.*` - Optimization hints (e.g., `opt.likely`, `opt.cold`)
- `lang.*` - Language-specific (e.g., `lang.swift.async`, `lang.rust.unsafe`)

### 15.2  Common Attributes

| Attribute | Applies to | Description |
| --------- | ---------- | ----------- |
| `linkage` | Functions, globals | Linkage type: `"external"`, `"internal"`, `"weak"`, `"linkonce"` |
| `visibility` | Functions, globals | Symbol visibility: `"default"`, `"hidden"`, `"protected"` |
| `call_conv` | Functions | Calling convention: `"c"`, `"fast"`, `"swift"`, etc. |
| `volatile` | Load/store | Volatile memory access |
| `atomic` | Load/store/fence | Memory ordering: `"acquire"`, `"release"`, `"seq_cst"`, etc. |
| `align` | Functions, globals, alloca | Alignment requirement |
| `section` | Functions, globals | Target section placement |

---

## 16  Example

```pir
MODULE "example" TARGET "x86_64-linux"

type Point = struct<<f32, f32>>

EXTERN FUNC printf(ptr, ...) -> i32

global @message : ptr align 8 init "Hello, %s!\n\0"

func greet(%name: ptr) -> i32 <<linkage: "external">>
entry:
  %fmt = load.ptr [@message]
  %result = call.i32 @printf(%fmt, %name)
  ret %result
endfunc

func distance(%p1: Point, %p2: Point) -> f32
entry:
  %x1 = extractvalue %p1, 0
  %y1 = extractvalue %p1, 1
  %x2 = extractvalue %p2, 0
  %y2 = extractvalue %p2, 1
  
  %dx = fsub %x2, %x1
  %dy = fsub %y2, %y1
  
  %dx2 = fmul %dx, %dx
  %dy2 = fmul %dy, %dy
  
  %sum = fadd %dx2, %dy2
  %dist = call.f32 @sqrtf(%sum)
  ret %dist
endfunc
```