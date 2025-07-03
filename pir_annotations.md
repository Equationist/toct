# PIR Annotation Syntax – Types, Contracts, Proof Hints

*Version 0.1 • June 2025*

---

## 1  Design Objectives

- **Human‑readable** and one‑line parsable.
- Zero ambiguity with existing brackets `[]` (memory) and `<<>>` (aggregates).
- Works on the three attachable entities: **value**, **instruction / block / function**, **global object**.
- Parser can ignore unknown keys but must preserve round‑trip.

---

## 2  Syntax Summary

```
<ir‑line>   ::= <core‑syntax> [ "@{" <attr‑list> "}" ]
<attr‑list> ::= <attr> { "," <attr> }
<attr>      ::= <ident> [ "=" <json> ]
```

- `@{ … }` **must** be the last token before any trailing comment `; …`.
- `<json>` is JSON (no whitespace restriction).  Primitive shorthand: `true`, `false`, `null`, numbers, strings.

> The IR reader may treat the whole slice between `@{` and the matching `}` as an opaque JSON string and feed it to `Yojson.Safe.from_string`.

---

## 3  Examples

### 3.1  Scalar value attributes

```ir
n  = load.i32 [@counter]           @{"range":[0,2147483647]}
ptr = bitcast some_anyptr          @{nonnull:true}
```

### 3.2  Function contracts

```ir
func add(x:i32, y:i32) -> i32      @{pure:true, pre:"x>0 && y>0", post:"ret > x && ret > y"}
entry: …
endfunc
```

### 3.3  Loop invariant

```ir
cond(i32 n, i32 acc):              @{invariant:"acc == sum(0,n)"}
  cmp = icmp.eq.i32 n, 0
  br cmp, exit, body
```

### 3.4  Global object proof hint

```ir
global min_val:i8 init 0           @{range:[0,0]}
```

---

## 4  Reserved Keys

| Key                | JSON type          | Applicable to        | Meaning                                                        |
| ------------------ | ------------------ | -------------------- | -------------------------------------------------------------- |
| `type`             | string             | value, global        | Canonical string from `Ty.show` (redundant, for proof export). |
| `range`            | `[min,max]` number | value, param, global | Closed interval of possible integer values.                    |
| `nonnull`          | bool               | pointer value        | Proven non‑null dereference.                                   |
| `pure`             | bool               | function             | No side‑effects except reading immutable memory.               |
| `pre` / `post`     | string (expr)      | function             | Contracts; `ret` refers to return value.                       |
| `invariant`        | string             | block label          | Loop invariant expression.                                     |
| `variant`          | string             | block label          | Ranking function for termination check.                        |
| `reads` / `writes` | string list        | function             | Memory effect summary.                                         |
| `skip‑verify`      | bool               | any                  | Tool hint to disable proof checks on this entity.              |

Tools **should** emit at least `type` when exporting PIR for external provers.

---

## 5  Grammar for Expressions in Contracts

*Subset of C boolean exprs* with identifiers bound to parameter names, block params, or `ret`.

```
expr ::=
  expr "&&" expr  |  expr "||" expr  |  "!" expr
| expr relop expr | arith
relop ::= "==" | "!=" | ">" | ">=" | "<" | "<="
arith ::= ident | number | arith op arith | "(" arith ")"
op ::= "+" | "-" | "*" | "/" | "%" | "<<" | ">>"
```

A proof pass translates to WhyML or Z3 via straightforward mapping.

---

## 6  Consuming Annotations

- **Pass‐agnostic rule:** Unknown keys are preserved.
- **Type checker** populates `type` and verifies that existing `type` matches.
- **Range analysis** writes `range`; SCCP may shrink intervals.
- **Why3 bridge** serialises `pre`, `post`, `invariant`, `variant`.
- **Optimiser** consults:
  - `nonnull=true` ➜ remove null checks.
  - `range=[0,255]` ➜ replace mod 256 with zero‑ext.

---

## 7  Extensions

- Future keys should use reverse‑DNS style to avoid collision: `"example.com/myattr": …`.
- `@{}` on an **empty** list is legal and ignored.

---

## 8  Implementation Hints

```ocaml
module Attr = struct
  let parse s : Yojson.Safe.t = Yojson.Safe.from_string s
  let print (j:Yojson.Safe.t) = Yojson.Safe.to_string ~std:true j
  (* attach helpers … *)
end
```

During pretty‑print: align `@{…}` to column 48 for readability.

---

© 2025 TOCT authors · CC BY 4.0

