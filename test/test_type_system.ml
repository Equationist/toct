(* Tests for Type System *)

[@@@warning "-21"]

open Compilerkit_frontend

(* Test Symbol Table *)
let test_symbol_table () =
  print_endline "Testing symbol table...";
  
  let table = Symbol_table.create () in
  let dummy_span = Position.span Position.dummy Position.dummy in
  
  (* Test basic operations *)
  let result1 = Symbol_table.define table "x" Symbol_table.Variable "int" dummy_span in
  assert (match result1 with Ok _ -> true | _ -> false);
  
  (* Test lookup *)
  let sym = Symbol_table.lookup table "x" in
  assert (match sym with Some s -> s.name = "x" | None -> false);
  
  (* Test duplicate definition *)
  let result2 = Symbol_table.define table "x" Symbol_table.Variable "int" dummy_span in
  assert (match result2 with Error (`DuplicateDefinition _) -> true | _ -> false);
  
  (* Test scoping *)
  let scope_id = Symbol_table.enter_scope table in
  assert (scope_id = 1);
  
  (* Define in inner scope *)
  let result3 = Symbol_table.define table "y" Symbol_table.Variable "bool" dummy_span in
  assert (match result3 with Ok _ -> true | _ -> false);
  
  (* Shadow outer variable *)
  let result4 = Symbol_table.define table "x" Symbol_table.Variable "bool" dummy_span in
  assert (match result4 with Ok _ -> true | _ -> false);
  
  (* Lookup finds inner binding *)
  let sym2 = Symbol_table.lookup table "x" in
  assert (match sym2 with Some s -> s.ty = "bool" | None -> false);
  
  (* Exit scope *)
  Symbol_table.exit_scope table;
  
  (* Now lookup finds outer binding again *)
  let sym3 = Symbol_table.lookup table "x" in
  assert (match sym3 with Some s -> s.ty = "int" | None -> false);
  
  (* y is no longer visible *)
  let sym4 = Symbol_table.lookup table "y" in
  assert (sym4 = None);
  
  (* Test with_scope *)
  let result = Symbol_table.with_scope table (fun () ->
    ignore (Symbol_table.define table "temp" Symbol_table.Variable "temp" dummy_span);
    Symbol_table.lookup table "temp"
  ) in
  assert (result <> None);
  assert (Symbol_table.lookup table "temp" = None);
  
  (* Test usage tracking *)
  Symbol_table.mark_used table "x";
  let unused = Symbol_table.unused_symbols table 0 in
  assert (List.length unused = 0);
  
  print_endline "✓ Symbol table tests passed"

(* Test Type System *)
let test_type_system () =
  print_endline "Testing type system...";
  
  let open Type_system in
  
  (* Test type construction *)
  let t1 = TBase TInt in
  let t2 = TBase TBool in
  let t3 = TArrow (t1, t2) in
  assert (pp_type t3 = "(int -> bool)");
  
  (* Test type variables *)
  let tv1 = TVar 0 in
  let tv2 = TVar 1 in
  assert (pp_type tv1 = "'a0");
  assert (not (equal tv1 tv2));
  
  (* Test compound types *)
  let list_int = TList t1 in
  assert (pp_type list_int = "int list");
  
  let tuple = TTuple [t1; t2; list_int] in
  assert (pp_type tuple = "(int * bool * int list)");
  
  (* Test record types *)
  let record_row = { fields = [("x", t1); ("y", t2)]; rest = Closed } in
  let record = TRecord record_row in
  assert (pp_type record = "{x: int; y: bool}");
  
  (* Test open record *)
  let open_row = { fields = [("x", t1)]; rest = Open 0 } in
  let open_record = TRecord open_row in
  assert (pp_type open_record = "{x: int; ..'r0}");
  
  (* Test type schemes *)
  let scheme1 = { vars = []; row_vars = []; body = t1 } in
  assert (pp_scheme scheme1 = "int");
  
  let scheme2 = { vars = [0]; row_vars = []; body = TArrow (tv1, tv1) } in
  assert (pp_scheme scheme2 = "forall 'a0. ('a0 -> 'a0)");
  
  (* Test instantiation *)
  let inst = instantiate scheme2 in
  assert (match inst with TArrow (TVar a, TVar b) -> a = b | _ -> false);
  
  (* Test free variables *)
  let fv1 = free_vars (TArrow (tv1, tv2)) in
  assert (VarSet.cardinal fv1 = 2);
  
  let fv2 = free_vars t1 in
  assert (VarSet.cardinal fv2 = 0);
  
  print_endline "✓ Type system tests passed"

(* Test Unification *)
let test_unification () =
  print_endline "Testing unification...";
  
  let open Type_system in
  let open Unify in
  
  (* Test simple unification *)
  let t1 = TBase TInt in
  let t2 = TBase TInt in
  let s1 = unify t1 t2 in
  assert (Hashtbl.length s1.Subst.vars = 0);
  
  (* Test variable unification *)
  let tv = TVar 0 in
  let s2 = unify tv t1 in
  assert (Subst.apply s2 tv = t1);
  
  (* Test function unification *)
  let f1 = TArrow (TVar 0, TVar 1) in
  let f2 = TArrow (TBase TInt, TBase TBool) in
  let s3 = unify f1 f2 in
  assert (Subst.apply s3 f1 = f2);
  
  (* Test occurs check *)
  let recursive = TArrow (tv, tv) in
  let failed = try ignore (unify tv recursive); false with UnifyError _ -> true in
  assert failed;
  
  (* Test record unification *)
  let r1 = TRecord { fields = [("x", TVar 0)]; rest = Closed } in
  let r2 = TRecord { fields = [("x", TBase TInt)]; rest = Closed } in
  let s4 = unify r1 r2 in
  assert (Subst.apply s4 r1 = r2);
  
  (* Test open record unification *)
  let r3 = TRecord { fields = [("x", TBase TInt)]; rest = Open 0 } in
  let r4 = TRecord { fields = [("x", TBase TInt); ("y", TBase TBool)]; rest = Closed } in
  let s5 = unify r3 r4 in
  let r3_unified = Subst.apply s5 r3 in
  assert (match r3_unified with
    | TRecord row -> List.length row.fields >= 1
    | _ -> false);
  
  (* Test constraint solver *)
  let constraints = [
    Unify (TVar 0, TBase TInt);
    Unify (TVar 1, TArrow (TVar 0, TBase TBool));
  ] in
  match Solver.solve_constraints constraints with
  | Ok s ->
    assert (Subst.apply s (TVar 0) = TBase TInt);
    assert (Subst.apply s (TVar 1) = TArrow (TBase TInt, TBase TBool))
  | Error _ -> failwith "Constraint solving failed";
  
  print_endline "✓ Unification tests passed"

(* Test Type Inference *)
let test_type_inference () =
  print_endline "Testing type inference...";
  
  let open Type_infer in
  
  let env = initial_env () in
  
  (* Test literals *)
  let e1 = ELit (LInt 42) in
  let t1 = type_check env e1 in
  assert (match t1 with Ok (Type_system.TBase Type_system.TInt) -> true | _ -> false);
  
  (* Test variables *)
  let e2 = EVar "+" in
  let t2 = type_check env e2 in
  assert (match t2 with Ok (Type_system.TArrow _) -> true | _ -> false);
  
  (* Test lambda *)
  let e3 = EAbs ("x", EVar "x") in
  let t3 = type_check env e3 in
  assert (match t3 with
    | Ok (Type_system.TArrow (Type_system.TVar a, Type_system.TVar b)) -> a = b
    | _ -> false);
  
  (* Test application *)
  let e4 = EApp (EAbs ("x", EVar "x"), ELit (LInt 42)) in
  let t4 = type_check env e4 in
  assert (match t4 with Ok (Type_system.TBase Type_system.TInt) -> true | _ -> false);
  
  (* Test let polymorphism *)
  let e5 = ELet ("id", EAbs ("x", EVar "x"),
                  ETuple [EApp (EVar "id", ELit (LInt 42));
                         EApp (EVar "id", ELit (LBool true))]) in
  let t5 = type_check env e5 in
  assert (match t5 with
    | Ok (Type_system.TTuple [Type_system.TBase Type_system.TInt; 
                              Type_system.TBase Type_system.TBool]) -> true
    | _ -> false);
  
  (* Test records *)
  let e6 = ERecord [("x", ELit (LInt 1)); ("y", ELit (LBool true))] in
  let t6 = type_check env e6 in
  assert (match t6 with
    | Ok (Type_system.TRecord row) ->
      List.length row.fields = 2 && row.rest = Type_system.Closed
    | _ -> false);
  
  (* Test field access *)
  let e7 = EField (e6, "x") in
  let t7 = type_check env e7 in
  assert (match t7 with Ok (Type_system.TBase Type_system.TInt) -> true | _ -> false);
  
  (* Test if-then-else *)
  let e8 = EIf (ELit (LBool true), ELit (LInt 1), ELit (LInt 2)) in
  let t8 = type_check env e8 in
  assert (match t8 with Ok (Type_system.TBase Type_system.TInt) -> true | _ -> false);
  
  (* Test pattern matching *)
  let e9 = EMatch (EVariant ("Some", Some (ELit (LInt 42))),
                   [(PVariant ("Some", Some (PVar "x")), EVar "x");
                    (PVariant ("None", None), ELit (LInt 0))]) in
  let t9 = type_check env e9 in
  assert (match t9 with Ok (Type_system.TBase Type_system.TInt) -> true | _ -> false);
  
  (* Test type error *)
  let e10 = EApp (ELit (LInt 42), ELit (LBool true)) in
  let t10 = type_check env e10 in
  assert (match t10 with Error _ -> true | _ -> false);
  
  print_endline "✓ Type inference tests passed"

(* Test error messages *)
let test_error_messages () =
  print_endline "Testing error messages...";
  
  let open Unify in
  
  (* Test type mismatch error *)
  let err1 = pp_unify_error (TypeMismatch (Type_system.TBase Type_system.TInt,
                                           Type_system.TBase Type_system.TBool)) in
  assert (String.contains err1 'i' && String.contains err1 'b');
  
  (* Test occurs check error *)
  let err2 = pp_unify_error (OccursCheck (0, Type_system.TArrow (Type_system.TVar 0,
                                                                 Type_system.TBase Type_system.TInt))) in
  assert (String.contains err2 '0');
  
  print_endline "✓ Error message tests passed"

let run_all_tests () =
  print_endline "\n=== Type System Tests ===\n";
  
  test_symbol_table ();
  print_endline "";
  
  test_type_system ();
  print_endline "";
  
  test_unification ();
  print_endline "";
  
  test_type_inference ();
  print_endline "";
  
  test_error_messages ();
  print_endline "";
  
  print_endline "🎉 All type system tests passed! 🎉"

let () = run_all_tests ()