# TOCT Back‑End – Detailed Architecture & Pass Specification

*Revision 0.1 • June 2025*

---

## 1  Bird’s‑Eye View

```
 ┌───────────────┐   PIR CFG      ┌───────────────────────┐  SSA‑PIR       ┌────────────────────┐
 │  PIR Ingress  │───────────────►│  #0  SSA Builder      │───────────────►│  Optimisation Core │
 └───────────────┘                └───────────────────────┘                │  (#1‑#11)          │
                                                                           └────────┬──────────┘
                                                                                    │ Machine‑SSA
                                                                                    ▼
                                                                           ┌────────────────────┐
                                                                           │ #12  ISel (BURS)   │
                                                                           └────────┬──────────┘
                                                                                    │ Machine‑SSA
                                                                                    ▼
                                                                           ┌────────────────────┐
                                                                           │ #13  Red.Cmp Elim  │
                                                                           └────────┬──────────┘
                                                                                    │ Machine‑SSA
                                                                                    ▼
                                                                           ┌────────────────────┐
                                                                           │ #14  LSRA + Coales │
                                                                           └────────┬──────────┘
                                                                                    │ Mach‑non‑SSA
                                                                                    ▼
                                                                           ┌────────────────────┐
                                                                           │ #15  Peephole & Fr │
                                                                           └────────┬──────────┘
                                                                                    │ Encoded text
                                                                                    ▼
                                                                           ELF / COFF / Mach‑O
```

*Light‑blue boxes are IR checkpoints dumped by **`pir-objdump`**.*

---

## 2  IR Milestones

| Stage | Name             | Form                                                      | Key invariants                                    |
| ----- | ---------------- | --------------------------------------------------------- | ------------------------------------------------- |
| P0    | **PIR‑CFG**      | blocks + block‑params, types, attrs                       | May contain non‑SSA defs; address‑calc ops typed. |
| P1    | **SSA‑PIR**      | pruned SSA, block‑params only, all defs dominate uses     | Each value has one def.                           |
| P2    | **Machine‑SSA**  | Target‑specific opcodes, virtual regs, preserved SSA form | No generic mem ops; addressing modes selected.    |
| P3    | **Mach non‑SSA** | Real regs + stack slots, copies resolved                  | Ready for encode.                                 |

Dump passes: `--dump=p0,p1,p2,p3`.

---

## 3  Pass Inventory (Backend «small‑plus++»)

| #  | Pass (stage)                | Algo / key idea                                                     | Inputs → outputs | Complexity        | LoC est. | Main citations          |
| -- | --------------------------- | ------------------------------------------------------------------- | ---------------- | ----------------- | -------- | ----------------------- |
| 0  | **SSA Builder**             | Braun‑Buchwald pruned SSA; dominance frontiers                      | P0 → P1          | O(E α)            | 200      | Braun ’13, Cooper ’01   |
| 1  | Sparse CCP                  | Wegman‑Zadeck lattice over SSA & CFG                                | P1→P1            | O(E)              | 180      | Wegman ’91              |
| 2  | Hash **GVN + PRE**          | Click NewGVN subset; value‑hash + congruence, PRE via phi‑translate | P1→P1            | O(E log E) htable | 650      | Click ’17, Rosen ’99    |
| 3  | DCE / DBE                   | Mark‑live via use‑def & reachability                                | P1→P1            | O(E)              | 120      | Cooper book             |
| 4  | Strength‑Reduction          | Pattern `(mul x,2^k)` → shl; loop ind‑var SR                        | P1→P1            | O(E)              | 150      | Thomas‑Levin ’82        |
| 5  | **Tiny Inliner**            | Size‑bounded (≤40 instr.) w/ call‑count heuristic                   | P1→P1            | O(N\_calls)       | 180      | Makarov MIR ’19         |
| 6  | Loop **Unroller**           | Trip‑count const ≤4, side‑effect free                               | P1→P1            | O(E)              | 150      | GCC simple\_unroll note |
| 7  | LICM (reg‑aware)            | Allen‑Cocke hoist; spare‑reg heuristic                              | P1→P1            | O(E)              | 180      | Allen ’71, Huang ’13    |
| 8  | Tail‑Dup & Jump Thread      | Ball‑Larus small‑BB duplication                                     | P1→P1            | O(E)              | 120      | Ball‑Larus ’93          |
| 9  | Store Sinking               | Move stores to cold exits                                           | P1→P1            | O(E)              | 90       | Cooper‑Torczon ’04      |
| 10 | Alias‑trivial L/S Fwd       | Same‑base+const‑off, intra‑BB                                       | P1→P1            | O(E)              | 150      | GCC “store‑to‑load”     |
| 11 | **Block Layout (TSP lite)** | Pettis‑Hansen static heuristics + 2‑opt                             | P1→P1            | O(E log E)        | 160      | P‑H ’90                 |
| 12 | **ISel (BURS)**             | Bottom‑up tree‑rewrite; pattern tables                              | P1→P2            | O(E)              | 450/ISA  | Fraser‑Hanson ’92       |
| 13 | Redundant Compare Elim      | Flag live‑range reuse                                               | P2→P2            | O(E)              | 70       | Koes ’10                |
| 14 | **LSRA + R‑T Coalesce**     | Poletto linear scan + Rosen‑Tardos post pass                        | P2→P3            | O(N regs)         | 850      | Poletto ’99, Rosen ’99  |
| 15 | Peephole + Frame            | 2‑insn peephole, stack frame gen                                    | P3→binary        | O(E)              | 800      | GCC peephole docs       |

> Complexity uses E = #edges ≈ #instr, α = Ackermann inverse.LoC are additive per ISA where noted.

---

## 4  Key Data Structures (OCaml snippets)

```ocaml
type ty = I8 | I16 | I32 | I64 | F32 | F64
         | Vec  of int * ty | Ptr | Struct of ty list | PackedStruct of ty list | Array of int * ty

type vreg = { id:int ; ty:ty ; attr:Attr.t }

type expr =
  | Bin  of binop * vreg * vreg
  | Load of vreg              (* addr *)
  | Call of { fn:vreg; args:vreg list; throwing:bool }
  | ...

module Block = struct
  type t = { id:int; params:vreg list; mutable instrs:instr list; mutable term:terminator }
end
```

---

## 5  Diagnostics & Dumps

- `pir-objdump -stage=p1` prints SSA‑PIR with colours and range/nonnull attrs.
- Each pass adds `debug_tag : string` allowing bisect: `--until=GVN`.
- `pir-lint --all` runs after major stages; fatal on UB patterns.

---

## 6  Extending or Disabling Passes

```ocaml
let backend_pipeline =
  Pipeline.[
    stage "ssa"          ssa_build;
    stage "ccp"          sparse_ccp;
    stage "gvn"          gvn_pre;
    (* use -Xno-gvn to skip *)
    ...
  ]
```

CLI: `pirc -O2 -Xno-unroll`.

---

## 7  Future Hook Points

- **Vectoriser:** insert before ISel; consumes SSA‑PIR.
- **Profile‑guided reorder:** replace static PH layout.
- **GC stack‑map emitter:** after RA, before frame pass.

---

© 2025 TOCT authors – CC BY 4.0

