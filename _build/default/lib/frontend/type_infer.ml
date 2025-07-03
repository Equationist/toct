(* Type Inference Engine *)

open Type_system
open Unify
open Symbol_table

(* Type environment *)
type env = scheme Symbol_table.t

(* Expression AST for testing *)
type expr =
  | EVar of string
  | ELit of lit
  | EApp of expr * expr
  | EAbs of string * expr
  | ELet of string * expr * expr
  | ELetRec of string * expr * expr
  | ETuple of expr list
  | ERecord of (string * expr) list
  | EField of expr * string
  | EVariant of string * expr option
  | EMatch of expr * (pattern * expr) list
  | EIf of expr * expr * expr

and lit =
  | LInt of int
  | LBool of bool
  | LString of string
  | LUnit

and pattern =
  | PVar of string
  | PLit of lit
  | PTuple of pattern list
  | PRecord of (string * pattern) list
  | PVariant of string * pattern option
  | PWildcard

(* Inference state *)
type infer_state = {
  env: env;
  constraints: constraint_t list;
}

(* Inference monad *)
type 'a infer = infer_state -> ('a * infer_state)

let return x state = (x, state)

let (>>=) m f state =
  let (x, state') = m state in
  f x state'

let (>>|) m f = m >>= fun x -> return (f x)

let get_env state = (state.env, state)

let with_env env f state =
  let old_env = state.env in
  let (result, state') = f { state with env } in
  (result, { state' with env = old_env })

let with_new_scope f =
  get_env >>= fun env ->
  let _scope_id = Symbol_table.enter_scope env in
  let result_m = f () in
  result_m >>= fun result ->
  get_env >>= fun env' ->
  Symbol_table.exit_scope env';
  return result

let add_constraint c state =
  ((), { state with constraints = c :: state.constraints })

let fresh () state =
  (fresh_type (), state)

(* Instantiate a type scheme *)
let inst scheme =
  return (instantiate scheme)

(* Generalize a type in the current environment *)
let gen ty =
  get_env >>= fun env ->
  let schemes = Symbol_table.symbols_in_scope env in
  let env_schemes = List.map (fun sym -> sym.ty) schemes in
  return (generalize env_schemes ty)

(* Lookup variable in environment *)
let lookup_var name =
  get_env >>= fun env ->
  match Symbol_table.lookup env name with
  | Some sym ->
    Symbol_table.mark_used env name;
    inst sym.ty
  | None ->
    failwith (Printf.sprintf "Unbound variable: %s" name)

(* Extend environment *)
let extend name scheme =
  get_env >>= fun env ->
  let span = Position.span Position.dummy Position.dummy in
  match Symbol_table.define env name Variable scheme span with
  | Ok _ -> return ()
  | Error (`DuplicateDefinition _) ->
    failwith (Printf.sprintf "Variable %s already defined" name)

(* Infer type of literal *)
let infer_lit = function
  | LInt _ -> return (TBase TInt)
  | LBool _ -> return (TBase TBool)
  | LString _ -> return (TBase TString)
  | LUnit -> return (TBase TUnit)

(* Infer type of expression *)
let rec infer = function
  | EVar name -> lookup_var name
  
  | ELit lit -> infer_lit lit
  
  | EApp (e1, e2) ->
    infer e1 >>= fun t1 ->
    infer e2 >>= fun t2 ->
    fresh () >>= fun tv ->
    add_constraint (Unify (t1, TArrow (t2, tv))) >>= fun () ->
    return tv
  
  | EAbs (x, e) ->
    fresh () >>= fun tv ->
    let scheme = { vars = []; row_vars = []; body = tv } in
    with_new_scope (fun () ->
      extend x scheme >>= fun () ->
      infer e
    ) >>= fun t ->
    return (TArrow (tv, t))
  
  | ELet (x, e1, e2) ->
    with_new_scope (fun () ->
      infer e1 >>= fun t1 ->
      gen t1
    ) >>= fun scheme ->
    with_new_scope (fun () ->
      extend x scheme >>= fun () ->
      infer e2
    )
  
  | ELetRec (x, e1, e2) ->
    fresh () >>= fun tv ->
    let scheme = { vars = []; row_vars = []; body = tv } in
    with_new_scope (fun () ->
      extend x scheme >>= fun () ->
      infer e1 >>= fun t1 ->
      add_constraint (Unify (tv, t1)) >>= fun () ->
      gen t1
    ) >>= fun gen_scheme ->
    with_new_scope (fun () ->
      extend x gen_scheme >>= fun () ->
      infer e2
    )
  
  | ETuple es ->
    infer_list es >>| fun ts -> TTuple ts
  
  | ERecord fields ->
    let (names, es) = List.split fields in
    infer_list es >>= fun ts ->
    let row = { fields = List.combine names ts; rest = Closed } in
    return (TRecord row)
  
  | EField (e, name) ->
    infer e >>= fun t ->
    fresh () >>= fun tv ->
    let rv = fresh_rvar () in
    let row = { fields = [(name, tv)]; rest = Open rv } in
    add_constraint (Unify (t, TRecord row)) >>= fun () ->
    return tv
  
  | EVariant (tag, None) ->
    let rv = fresh_rvar () in
    let row = { fields = [(tag, TBase TUnit)]; rest = Open rv } in
    return (TVariant row)
  
  | EVariant (tag, Some e) ->
    infer e >>= fun t ->
    let rv = fresh_rvar () in
    let row = { fields = [(tag, t)]; rest = Open rv } in
    return (TVariant row)
  
  | EMatch (e, cases) ->
    infer e >>= fun t ->
    fresh () >>= fun result_ty ->
    infer_cases t result_ty cases >>= fun () ->
    return result_ty
  
  | EIf (e1, e2, e3) ->
    infer e1 >>= fun t1 ->
    add_constraint (Unify (t1, TBase TBool)) >>= fun () ->
    infer e2 >>= fun t2 ->
    infer e3 >>= fun t3 ->
    add_constraint (Unify (t2, t3)) >>= fun () ->
    return t2

and infer_list = function
  | [] -> return []
  | e :: es ->
    infer e >>= fun t ->
    infer_list es >>= fun ts ->
    return (t :: ts)

and infer_cases scrutinee_ty result_ty = function
  | [] -> return ()
  | (pat, e) :: cases ->
    with_new_scope (fun () ->
      infer_pattern pat scrutinee_ty >>= fun () ->
      infer e >>= fun t ->
      add_constraint (Unify (t, result_ty))
    ) >>= fun () ->
    infer_cases scrutinee_ty result_ty cases

and infer_pattern pat expected_ty =
  match pat with
  | PVar x ->
    gen expected_ty >>= fun scheme ->
    extend x scheme
  
  | PLit lit ->
    infer_lit lit >>= fun t ->
    add_constraint (Unify (t, expected_ty))
  
  | PTuple pats ->
    fresh_list (List.length pats) >>= fun ts ->
    add_constraint (Unify (expected_ty, TTuple ts)) >>= fun () ->
    infer_patterns pats ts
  
  | PRecord fields ->
    let (names, pats) = List.split fields in
    fresh_list (List.length pats) >>= fun ts ->
    let row = { fields = List.combine names ts; rest = Closed } in
    add_constraint (Unify (expected_ty, TRecord row)) >>= fun () ->
    infer_patterns pats ts
  
  | PVariant (tag, None) ->
    let rv = fresh_rvar () in
    let row = { fields = [(tag, TBase TUnit)]; rest = Open rv } in
    add_constraint (Unify (expected_ty, TVariant row))
  
  | PVariant (tag, Some pat) ->
    fresh () >>= fun t ->
    let rv = fresh_rvar () in
    let row = { fields = [(tag, t)]; rest = Open rv } in
    add_constraint (Unify (expected_ty, TVariant row)) >>= fun () ->
    infer_pattern pat t
  
  | PWildcard ->
    return ()

and infer_patterns pats tys =
  match pats, tys with
  | [], [] -> return ()
  | p :: ps, t :: ts ->
    infer_pattern p t >>= fun () ->
    infer_patterns ps ts
  | _ -> failwith "Pattern/type list mismatch"

and fresh_list n =
  if n = 0 then return []
  else
    fresh () >>= fun t ->
    fresh_list (n - 1) >>= fun ts ->
    return (t :: ts)

(* Run type inference *)
let run_infer env m =
  let initial_state = { env; constraints = [] } in
  let (result, final_state) = m initial_state in
  match Solver.solve_constraints (List.rev final_state.constraints) with
  | Ok subst -> Ok (Subst.apply subst result, subst)
  | Error e -> Error e

(* Type check an expression *)
let type_check env expr =
  match run_infer env (infer expr) with
  | Ok (ty, _) -> Ok ty
  | Error e -> Error (pp_unify_error e)

(* Helper to create initial environment with built-ins *)
let initial_env () =
  let env = Symbol_table.create () in
  let def name ty =
    let scheme = { vars = []; row_vars = []; body = ty } in
    let span = Position.span Position.dummy Position.dummy in
    ignore (Symbol_table.define env name Function scheme span)
  in
  
  let def_poly name scheme =
    let span = Position.span Position.dummy Position.dummy in
    ignore (Symbol_table.define env name Function scheme span)
  in
  
  (* Arithmetic *)
  def "+" (TArrow (TTuple [TBase TInt; TBase TInt], TBase TInt));
  def "-" (TArrow (TTuple [TBase TInt; TBase TInt], TBase TInt));
  def "*" (TArrow (TTuple [TBase TInt; TBase TInt], TBase TInt));
  def "/" (TArrow (TTuple [TBase TInt; TBase TInt], TBase TInt));
  
  (* Comparison *)
  def_poly "=" (let a = fresh_tvar () in
    { vars = [a]; row_vars = []; 
      body = TArrow (TTuple [TVar a; TVar a], TBase TBool) });
  def "<" (TArrow (TTuple [TBase TInt; TBase TInt], TBase TBool));
  def ">" (TArrow (TTuple [TBase TInt; TBase TInt], TBase TBool));
  
  (* Boolean *)
  def "&&" (TArrow (TTuple [TBase TBool; TBase TBool], TBase TBool));
  def "||" (TArrow (TTuple [TBase TBool; TBase TBool], TBase TBool));
  def "not" (TArrow (TBase TBool, TBase TBool));
  
  (* List operations *)
  def_poly "cons" (let a = fresh_tvar () in
    { vars = [a]; row_vars = [];
      body = TArrow (TTuple [TVar a; TList (TVar a)], TList (TVar a)) });
  def_poly "nil" (let a = fresh_tvar () in
    { vars = [a]; row_vars = []; body = TList (TVar a) });
  
  env