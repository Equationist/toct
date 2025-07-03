(* Unification Algorithm for Type Inference *)

open Type_system

type unify_error =
  | OccursCheck of tvar * ty
  | TypeMismatch of ty * ty  
  | RowMismatch of ty row * ty row
  | MissingField of string
  | ExtraField of string
  | CannotUnify of string

exception UnifyError of unify_error

(* Occurs check - ensure type variable doesn't occur in type *)
let rec occurs tv ty =
  match ty with
  | TVar tv' -> tv = tv'
  | TBase _ -> false
  | TArrow (t1, t2) -> occurs tv t1 || occurs tv t2
  | TTuple ts -> List.exists (occurs tv) ts
  | TList t -> occurs tv t
  | TOption t -> occurs tv t
  | TRecord row | TVariant row ->
    List.exists (fun (_, t) -> occurs tv t) row.fields
  | TApp (_, ts) -> List.exists (occurs tv) ts

(* Sort fields by name for consistent comparison *)
let sort_fields fields =
  List.sort (fun (n1, _) (n2, _) -> String.compare n1 n2) fields

(* Unify two types *)
let rec unify t1 t2 =
  match t1, t2 with
  | TVar tv1, TVar tv2 when tv1 = tv2 ->
    Subst.empty ()
  
  | TVar tv, t | t, TVar tv ->
    if occurs tv t then
      raise (UnifyError (OccursCheck (tv, t)))
    else
      Subst.singleton tv t
  
  | TBase b1, TBase b2 when b1 = b2 ->
    Subst.empty ()
  
  | TArrow (a1, r1), TArrow (a2, r2) ->
    let s1 = unify a1 a2 in
    let s2 = unify (Subst.apply s1 r1) (Subst.apply s1 r2) in
    Subst.compose s2 s1
  
  | TTuple ts1, TTuple ts2 when List.length ts1 = List.length ts2 ->
    unify_list ts1 ts2
  
  | TList t1, TList t2 ->
    unify t1 t2
  
  | TOption t1, TOption t2 ->
    unify t1 t2
  
  | TRecord row1, TRecord row2 ->
    unify_rows row1 row2
  
  | TVariant row1, TVariant row2 ->
    unify_rows row1 row2
  
  | TApp (n1, ts1), TApp (n2, ts2) when n1 = n2 && List.length ts1 = List.length ts2 ->
    unify_list ts1 ts2
  
  | _ ->
    raise (UnifyError (TypeMismatch (t1, t2)))

(* Unify a list of type pairs *)
and unify_list ts1 ts2 =
  List.fold_left2 (fun s t1 t2 ->
    let s' = unify (Subst.apply s t1) (Subst.apply s t2) in
    Subst.compose s' s
  ) (Subst.empty ()) ts1 ts2

(* Unify row types *)
and unify_rows row1 row2 =
  (* Sort fields for consistent comparison *)
  let fields1 = sort_fields row1.fields in
  let fields2 = sort_fields row2.fields in
  
  (* Partition fields into common and unique *)
  let rec partition f1 f2 common unique1 unique2 =
    match f1, f2 with
    | [], [] -> (List.rev common, List.rev unique1, List.rev unique2)
    | [], rest -> (List.rev common, List.rev unique1, List.rev_append rest unique2)
    | rest, [] -> (List.rev common, List.rev_append rest unique1, List.rev unique2)
    | (n1, t1) :: r1, (n2, t2) :: r2 ->
      let cmp = String.compare n1 n2 in
      if cmp = 0 then
        partition r1 r2 ((n1, t1, t2) :: common) unique1 unique2
      else if cmp < 0 then
        partition r1 f2 common ((n1, t1) :: unique1) unique2
      else
        partition f1 r2 common unique1 ((n2, t2) :: unique2)
  in
  
  let (common, unique1, unique2) = partition fields1 fields2 [] [] [] in
  
  (* Unify common fields *)
  let subst = List.fold_left (fun s (_name, t1, t2) ->
    let s' = unify (Subst.apply s t1) (Subst.apply s t2) in
    Subst.compose s' s
  ) (Subst.empty ()) common in
  
  (* Handle unique fields based on row rest *)
  match row1.rest, row2.rest with
  | Closed, Closed ->
    if unique1 <> [] then
      raise (UnifyError (ExtraField (fst (List.hd unique1))))
    else if unique2 <> [] then
      raise (UnifyError (MissingField (fst (List.hd unique2))))
    else
      subst
  
  | Open rv1, Open rv2 when rv1 = rv2 && unique1 = [] && unique2 = [] ->
    subst
  
  | Open rv, Closed | Closed, Open rv ->
    (* Row variable can be instantiated to closed row with extra fields *)
    let extra_fields = if row1.rest = Closed then unique2 else unique1 in
    let _extra_row = { fields = extra_fields; rest = Closed } in
    let s = Subst.row_singleton rv (Closed) in
    Subst.compose s subst
  
  | Open rv1, Open rv2 ->
    (* Two open rows - create fresh row variable for common extension *)
    if rv1 = rv2 then
      if unique1 = [] && unique2 = [] then
        subst
      else
        raise (UnifyError (RowMismatch (row1, row2)))
    else
      (* Unify row variables *)
      let fresh_rv = fresh_rvar () in
      let s1 = Subst.row_singleton rv1 (Open fresh_rv) in
      let s2 = Subst.row_singleton rv2 (Open fresh_rv) in
      Subst.compose (Subst.compose s2 s1) subst

(* Unification with error context *)
let unify_with_context t1 t2 context =
  try unify t1 t2
  with UnifyError _ ->
    raise (UnifyError (CannotUnify context))

(* Pretty print unification errors *)
let pp_unify_error = function
  | OccursCheck (tv, ty) ->
    Printf.sprintf "Occurs check failed: 'a%d occurs in %s" tv (pp_type ty)
  | TypeMismatch (t1, t2) ->
    Printf.sprintf "Cannot unify types:\n  %s\nwith\n  %s"
      (pp_type t1) (pp_type t2)
  | RowMismatch (r1, r2) ->
    Printf.sprintf "Cannot unify rows:\n  {%s}\nwith\n  {%s}"
      (pp_row r1) (pp_row r2)
  | MissingField name ->
    Printf.sprintf "Missing field: %s" name
  | ExtraField name ->
    Printf.sprintf "Extra field: %s" name
  | CannotUnify context ->
    Printf.sprintf "Cannot unify: %s" context

(* Constraint solver *)
module Solver = struct
  type state = {
    constraints: constraint_t list;
    subst: Subst.t;
  }

  let initial constraints = {
    constraints;
    subst = Subst.empty ();
  }

  (* Apply substitution to constraint *)
  let subst_constraint s = function
    | Unify (t1, t2) ->
      Unify (Subst.apply s t1, Subst.apply s t2)
    | RowUnify (r1, r2) ->
      RowUnify (Subst.apply_row s r1, Subst.apply_row s r2)
    | Instance (t1, scheme, t2) ->
      Instance (Subst.apply s t1, Subst.apply_scheme s scheme, Subst.apply s t2)

  (* Solve one constraint *)
  let solve_one = function
    | Unify (t1, t2) -> unify t1 t2
    | RowUnify (r1, r2) -> unify_rows r1 r2
    | Instance (t, scheme, result_t) ->
      let instance = instantiate scheme in
      let s1 = unify t instance in
      let s2 = unify (Subst.apply s1 result_t) (Subst.apply s1 instance) in
      Subst.compose s2 s1

  (* Solve all constraints *)
  let rec solve state =
    match state.constraints with
    | [] -> state.subst
    | c :: rest ->
      let s' = solve_one (subst_constraint state.subst c) in
      let new_subst = Subst.compose s' state.subst in
      solve { constraints = rest; subst = new_subst }

  (* Solve with error handling *)
  let solve_constraints constraints =
    try Ok (solve (initial constraints))
    with UnifyError e -> Error e
end