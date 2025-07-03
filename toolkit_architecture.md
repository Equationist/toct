# Tiny OCaml Compiler Toolkit (TOCT) – Architecture & Implementation Plan

*Revision 0.1 • June 2025*

---

## 0  High‑Level Goals

- **Small ≦ 7.5 kLOC** back‑end, **≤ 3 kLOC** shared front‑end helpers.
- Ahead‑of‑time (AOT) code rivaling **94‑97 %** of GCC ‑O2 on integer workloads.
- Fully self‑hosted in OCaml; no C stubs except optional runtime intrinsics.
- Pluggable passes, attributes, and proof hooks so research features (e.g. Octagon analysis) drop in without touching the optimiser core.

---

## 1  End‑to‑End Architecture

```
        ┌──────────────┐  AST/Attrs  ┌────────────────────────────┐  PIR CFG  ┌────────────┐
Source ─► Frontend Kit ├─────────────► PIR.Builder + Pass Driver ├───────────► Back‑End 📦 ├──► object ► link ► exe
        └──────────────┘             └────────────────────────────┘           └────────────┘
              ▲                                     ▲                                ▲
              │                                     │                                │
  Why3 bridge │                        Contract / type attrs            Runtime intrinsics
```

Legend: 📦 = «small‑plus++» pipeline (Section 3).

---

## 2  Frontend Helper Modules

| Layer                      | Module(s) / Functor(s)                           | SLOC | Purpose                                            |
| -------------------------- | ------------------------------------------------ | ---- | -------------------------------------------------- |
| **Common**                 | `Position`, `Location`, `Error`, `Pretty`        |  250 | errors & PP.                                       |
| **Lex / Parse**            | `LexComb`, `Pratt`, `MenhirGlue`                 |  300 | Pratt DSL & Menhir helpers.                        |
| **AST utils**              | `Ast`, `AstIter`, `AstMap`                       |  180 | walkers / mappers.                                 |
| **Symbols**                | `module type SYM`; `Sym.Map`, `Sym.Hash`         |  160 | scoped bindings.                                   |
| **Types & Effects**        | `Ty`, `Kind`, `Infer`, `Solver`                  |  450 | Hindley‑Milner + row polymorphism.                 |
| **Attributes**             | `Attr` bag                                       |  120 | arbitrary key→value store.                         |
| **Range / Null / Octagon** | `Analysis.Range`, `Analysis.DBM`, `Analysis.Oct` |  320 | intra‑procedural value‑range (interval & octagon). |
| **PIR builder**            | `PIR.Builder`                                    |  350 | monadic CFG + block‑param SSA.                     |
| **Runtime intrinsics**     | `Intrinsic`, `Runtime.StubGen`                   |  120 | `gc_alloc`, `call_throwing`, TLS.                  |
| **Contracts & Proof**      | `Contract`, `Why3.Out`                           |  220 | WhyML export + re‑import proven facts.             |
| **Driver**                 | `Driver.Pipeline`, CLI cmds                      |  200 | orchestrates passes.                               |
| **Lint & Pretty**          | `Pir.Lint`, `Pir.Pretty`                         |  150 | verifies style, prints colour IR.                  |

*Total helper ≈ 2 570 LOC.*

### 2.1  Minimal OCaml interfaces (excerpt)

````ocaml
module type SYM = sig
  type t
  val empty : t
  val enter_scope  : t -> t
  val leave_scope  : t -> t option
  val add_val  : t -> Ident.t -> Ty.t -> t
  val find_val : t -> Ident.t -> Ty.t option
end

module Attr : sig
  type t
  val empty  : t
  val add    : string -> Yojson.t -> t -> t
  val get_opt: string -> t -> Yojson.t option
end

module PIR_Builder : sig
  type fn
  type blk
  type 'a m  (* monad *)
  val create_fn : name:string -> params:(Ident.t * Ty.t) list -> ret:Ty.t -> fn
  val block       : fn -> blk
  val emit        : blk -> instr -> unit m
  val br          : blk -> cond:v -> then_:blk -> else_:blk -> unit m
  val ret         : blk -> v option -> unit m
  val run         : fn -> unit m -> unit
end```

---
## 3  Back‑End “small‑plus++” Pipeline

| # | Pass | Algo / Paper | Δ‑speed | SLOC |
|---|------|--------------|---------|------|
|0| SSA  (pruned) | Braun‑Buchwald 2013 | — |200 |
|1| Sparse CCP | Wegman‑Zadeck 1991 | +6 % |180 |
|2| **Hash‑GVN + PRE** | Click 2017 «NewGVN» | +2‑3 % |650 |
|3| DCE/DBE | Cooper 2001 | +1 % |120 |
|4| Strength‑reduce | Thomas‑Levin 1982 | +1‑2 % |150 |
|5| **Tiny inliner** | Makarov 2019 MIR | +1‑3 % |180 |
|6| Loop unroller ≤4 | GCC “simple_unroll” | +1‑2 % |150 |
|7| LICM (reg‑aware) | Allen‑Cocke 1971 | +0.5 % |180 |
|8| Tail‑dup / thread | Ball‑Larus 1993 | +1 % |120 |
|9| Store sinking | Cooper & Torczon 2004 | +0.5 % |90 |
|10| Alias‑trivial L/S fwd | local hash | +1 % |150 |
|11| **Block layout** | Pettis‑Hansen 1990 (lite) | +1‑3 % |160 |
|12| ISel (BURS) | Fraser‑Proebsting 1992 | — |450/ISA |
|13| Redundant cmp elim | Koes 2010 | +1 % |70 |
|14| Static BP heuristics | Ball‑Larus 1993 | +1 % |90 |
|15| LSRA + post‑coalesce | Poletto‑Sarkar 1999; Rosen‑Tardos 1999 | +0.5 % |850 |
|16| Peephole & frame | target tables | +1 % |800 |

*Total back‑end ≈ 7 400 LOC (two ISAs).* Estimated compile‑time: \<0.15 s for 50 k LoC code‑base.

---
## 4  Analysis Modules (optional plug‑ins)

### 4.1 Value‑range & Definiteness (Interval)
* **Algo**: Cousot–Halbwachs abstract interpretation; lattice of `(min,max,top,bot)`.
* O(E) per function.
* Tags PIR values with `Attr.add "range" (json [min, max])`.

### 4.2 Octagon Domain (Relational DBM)
* **Algo**: Antoine Miné, SAS 2001; Tight closure O(n³) but n ≤ 64.
* Tracks relations `±x ±y ≤ c` for better bounds.
* Plug‑in pass before LICM to prove loop invariants for hoisting.

### 4.3 Null/Definiteness
* Data‑flow bitset: maybe‑null, definitely‑non‑null.
* Sets `Attr "nonnull"`.  Optimiser may remove null‑check.

---
## 5  Pretty‑printing & Lint

* `Pir.Pretty.pp_function` – colourised output with block params.
* `Pir.Lint.run` – checks:
  1. SSA dominance of defs, 2. type‑match of operands, 3. forbidden UB patterns (mis‑aligned load without attr, shift ≥ bits, etc.).

Runs automatically under `Driver.Pipeline` after each major transform.

---
## 6  Portable IR (PIR) – Pointer to Spec

Full formal grammar and semantics are in **Portable_IR_Spec** (existing canvas doc).
Differences for SSA phase:
* Blocks declare parameters: `label cond(i32 n, i32 acc):`.
* `phi` ops disappear; incoming values passed via branch args.

### 6.1 Annotations
| Key | Meaning | Producer |
|-----|---------|----------|
| "type" | JSON repr of Ty.t | type checker |
| "range" | `[min,max]` ints | range/OCT pass |
| "nonnull" | `true` | null pass |
| "pure" | side‑effect‑free fn | contract parser |

Back‑end passes **must not** drop attrs they don’t understand.

---
## 7  Paper Reference List (chronological)

* 1971 Allen–Cocke. *Optimizing Transformations.*
* 1982 Thomas–Levin. *Strength Reduction.*
* 1990 Pettis–Hansen. *Profile Guided Code Positioning* (TSP layout; we use static heuristics).
* 1991 Cytron et al.; Wegman–Zadeck. *SSA & CCP.*
* 1992 Fraser–Proebsting. *BURS code‑gen.*
* 1993 Ball–Larus. *Tail Duplication & Branch Prediction.*
* 1999 Poletto–Sarkar. *Linear‑Scan RA.*  Rosen–Tardos *Coalescing.*
* 2001 Cooper–Harvey–Kennedy. *Dominator Alg.*  Miné. *Octagon Abstract Domain.*
* 2004 Cooper–Torczon. *Engineering a Compiler* (store sinking).
* 2010 Wimmer–Franz. *SSA LSRA.*  Koes. *Redundant Compare Elim.*
* 2013 Braun–Buchwald. *Pruned SSA.*
* 2017 Click. *NewGVN.*

---
## 8  Next Steps

1. Final‑ise core data structures (`Ty`, `Value`, `Instr`).
2. Boot‑strap PIR pretty‑printer + lint.
3. Implement SSA builder and Sparse CCP; validate on unit tests.
4. Incrementally add passes per Section 3 order; track SPEC speed gap.
5. Release `v0.1` with RV64GC back‑end and sample front‑end (tiny C).  Add x86‑64 in v0.2.

---
© 2025 TOCT authors · CC BY 4.0

````
