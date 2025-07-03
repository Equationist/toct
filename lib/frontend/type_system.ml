(* Type System - Hindley-Milner with row polymorphism *)

(* Type variables *)
type tvar = int

(* Row variables for extensible records/variants *)
type rvar = int

(* Base types *)
type base_type =
  | TInt
  | TBool  
  | TFloat
  | TString
  | TUnit
  | TChar

(* Row types for records and variants *)
type 'a row = {
  fields: (string * 'a) list;
  rest: row_rest;
}
and row_rest =
  | Closed
  | Open of rvar

(* Types *)
type ty =
  | TVar of tvar
  | TBase of base_type
  | TArrow of ty * ty
  | TTuple of ty list
  | TList of ty
  | TOption of ty
  | TRecord of ty row
  | TVariant of ty row
  | TApp of string * ty list  (* Type application F<A,B> *)

(* Type schemes (polymorphic types) *)
type scheme = {
  vars: tvar list;      (* Quantified type variables *)
  row_vars: rvar list;  (* Quantified row variables *)
  body: ty;
}

(* Kinds for higher-order types *)
type kind =
  | Star                    (* Type *)
  | Arrow of kind * kind    (* Type constructor *)

(* Type constraints for inference *)
type constraint_t =
  | Unify of ty * ty
  | RowUnify of ty row * ty row
  | Instance of ty * scheme * ty

(* Substitution *)
module Subst = struct
  type t = {
    vars: (tvar, ty) Hashtbl.t;
    rows: (rvar, row_rest) Hashtbl.t;
  }

  let empty () = {
    vars = Hashtbl.create 16;
    rows = Hashtbl.create 16;
  }

  let singleton tvar ty =
    let s = empty () in
    Hashtbl.add s.vars tvar ty;
    s

  let row_singleton rvar rest =
    let s = empty () in
    Hashtbl.add s.rows rvar rest;
    s

  let lookup s tvar =
    Hashtbl.find_opt s.vars tvar

  let lookup_row s rvar =
    Hashtbl.find_opt s.rows rvar

  let rec apply s = function
    | TVar tv ->
      (match lookup s tv with
       | Some ty -> apply s ty
       | None -> TVar tv)
    | TBase b -> TBase b
    | TArrow (t1, t2) -> TArrow (apply s t1, apply s t2)
    | TTuple ts -> TTuple (List.map (apply s) ts)
    | TList t -> TList (apply s t)
    | TOption t -> TOption (apply s t)
    | TRecord row -> TRecord (apply_row s row)
    | TVariant row -> TVariant (apply_row s row)
    | TApp (name, ts) -> TApp (name, List.map (apply s) ts)

  and apply_row s row =
    let fields = List.map (fun (name, ty) -> (name, apply s ty)) row.fields in
    let rest = match row.rest with
      | Closed -> Closed
      | Open rv ->
        (match lookup_row s rv with
         | Some rest -> rest
         | None -> Open rv)
    in
    { fields; rest }

  let apply_scheme (s : t) (scheme : scheme) =
    (* Only apply to free variables, not quantified ones *)
    let s' = empty () in
    Hashtbl.iter (fun tv ty ->
      if not (List.mem tv scheme.vars) then
        Hashtbl.add s'.vars tv ty
    ) s.vars;
    Hashtbl.iter (fun rv rest ->
      if not (List.mem rv scheme.row_vars) then
        Hashtbl.add s'.rows rv rest
    ) s.rows;
    { scheme with body = apply s' scheme.body }

  let compose s1 s2 =
    (* s1 ∘ s2 = apply s1 to all values in s2, then union *)
    let result = empty () in
    
    (* Apply s1 to s2's bindings *)
    Hashtbl.iter (fun tv ty ->
      Hashtbl.add result.vars tv (apply s1 ty)
    ) s2.vars;
    
    Hashtbl.iter (fun rv rest ->
      Hashtbl.add result.rows rv rest
    ) s2.rows;
    
    (* Add s1's bindings (they override) *)
    Hashtbl.iter (fun tv ty ->
      Hashtbl.add result.vars tv ty
    ) s1.vars;
    
    Hashtbl.iter (fun rv rest ->
      Hashtbl.add result.rows rv rest
    ) s1.rows;
    
    result
end

(* Fresh variable generation *)
let tvar_counter = ref 0
let rvar_counter = ref 0

let fresh_tvar () =
  let v = !tvar_counter in
  incr tvar_counter;
  v

let fresh_rvar () =
  let v = !rvar_counter in
  incr rvar_counter;
  v

let fresh_type () = TVar (fresh_tvar ())

(* Free variables *)
module VarSet = Set.Make(Int)

let rec free_vars = function
  | TVar tv -> VarSet.singleton tv
  | TBase _ -> VarSet.empty
  | TArrow (t1, t2) -> VarSet.union (free_vars t1) (free_vars t2)
  | TTuple ts -> List.fold_left VarSet.union VarSet.empty (List.map free_vars ts)
  | TList t -> free_vars t
  | TOption t -> free_vars t
  | TRecord row | TVariant row ->
    List.fold_left VarSet.union VarSet.empty
      (List.map (fun (_, ty) -> free_vars ty) row.fields)
  | TApp (_, ts) -> List.fold_left VarSet.union VarSet.empty (List.map free_vars ts)

let free_row_vars row =
  match row.rest with
  | Closed -> VarSet.empty
  | Open rv -> VarSet.singleton rv

let free_vars_scheme scheme =
  let fv = free_vars scheme.body in
  List.fold_left (fun acc v -> VarSet.remove v acc) fv scheme.vars

(* Type equality *)
let rec equal t1 t2 =
  match t1, t2 with
  | TVar v1, TVar v2 -> v1 = v2
  | TBase b1, TBase b2 -> b1 = b2
  | TArrow (a1, r1), TArrow (a2, r2) -> equal a1 a2 && equal r1 r2
  | TTuple ts1, TTuple ts2 ->
    List.length ts1 = List.length ts2 &&
    List.for_all2 equal ts1 ts2
  | TList t1, TList t2 -> equal t1 t2
  | TOption t1, TOption t2 -> equal t1 t2
  | TRecord r1, TRecord r2 -> equal_row r1 r2
  | TVariant r1, TVariant r2 -> equal_row r1 r2
  | TApp (n1, ts1), TApp (n2, ts2) ->
    n1 = n2 && List.length ts1 = List.length ts2 &&
    List.for_all2 equal ts1 ts2
  | _ -> false

and equal_row r1 r2 =
  let fields_equal =
    List.length r1.fields = List.length r2.fields &&
    List.for_all2 (fun (n1, t1) (n2, t2) ->
      n1 = n2 && equal t1 t2
    ) r1.fields r2.fields
  in
  let rest_equal = match r1.rest, r2.rest with
    | Closed, Closed -> true
    | Open v1, Open v2 -> v1 = v2
    | _ -> false
  in
  fields_equal && rest_equal

(* Instantiation - replace quantified variables with fresh ones *)
let instantiate scheme =
  let var_map = Hashtbl.create 16 in
  let row_map = Hashtbl.create 16 in
  
  (* Create fresh variables for each quantified variable *)
  List.iter (fun tv ->
    Hashtbl.add var_map tv (fresh_type ())
  ) scheme.vars;
  
  List.iter (fun rv ->
    Hashtbl.add row_map rv (Open (fresh_rvar ()))
  ) scheme.row_vars;
  
  (* Apply substitution *)
  let rec subst_ty = function
    | TVar tv ->
      (try Hashtbl.find var_map tv with Not_found -> TVar tv)
    | TBase b -> TBase b
    | TArrow (t1, t2) -> TArrow (subst_ty t1, subst_ty t2)
    | TTuple ts -> TTuple (List.map subst_ty ts)
    | TList t -> TList (subst_ty t)
    | TOption t -> TOption (subst_ty t)
    | TRecord row -> TRecord (subst_row row)
    | TVariant row -> TVariant (subst_row row)
    | TApp (name, ts) -> TApp (name, List.map subst_ty ts)
  
  and subst_row row =
    let fields = List.map (fun (n, t) -> (n, subst_ty t)) row.fields in
    let rest = match row.rest with
      | Closed -> Closed
      | Open rv ->
        (try Hashtbl.find row_map rv with Not_found -> Open rv)
    in
    { fields; rest }
  in
  
  subst_ty scheme.body

(* Generalization - quantify free variables *)
let generalize env ty =
  let env_vars = 
    List.fold_left (fun acc scheme ->
      VarSet.union acc (free_vars_scheme scheme)
    ) VarSet.empty env
  in
  let ty_vars = free_vars ty in
  let gen_vars = VarSet.diff ty_vars env_vars in
  {
    vars = VarSet.elements gen_vars;
    row_vars = [];  (* TODO: Handle row variable generalization *)
    body = ty;
  }

(* Pretty printing *)
let rec pp_type = function
  | TVar tv -> Printf.sprintf "'a%d" tv
  | TBase TInt -> "int"
  | TBase TBool -> "bool"
  | TBase TFloat -> "float"
  | TBase TString -> "string"
  | TBase TUnit -> "unit"
  | TBase TChar -> "char"
  | TArrow (t1, t2) ->
    Printf.sprintf "(%s -> %s)" (pp_type t1) (pp_type t2)
  | TTuple [] -> "unit"
  | TTuple [t] -> pp_type t
  | TTuple ts ->
    Printf.sprintf "(%s)" (String.concat " * " (List.map pp_type ts))
  | TList t -> Printf.sprintf "%s list" (pp_type t)
  | TOption t -> Printf.sprintf "%s option" (pp_type t)
  | TRecord row -> Printf.sprintf "{%s}" (pp_row row)
  | TVariant row -> Printf.sprintf "[%s]" (pp_row row)
  | TApp (name, []) -> name
  | TApp (name, ts) ->
    Printf.sprintf "%s<%s>" name (String.concat ", " (List.map pp_type ts))

and pp_row row =
  let field_strs = List.map (fun (n, t) ->
    Printf.sprintf "%s: %s" n (pp_type t)
  ) row.fields in
  match row.rest with
  | Closed -> String.concat "; " field_strs
  | Open rv -> String.concat "; " field_strs ^ Printf.sprintf "; ..'r%d" rv

let pp_scheme scheme =
  if scheme.vars = [] && scheme.row_vars = [] then
    pp_type scheme.body
  else
    let vars = String.concat " " (List.map (Printf.sprintf "'a%d") scheme.vars) in
    let rvars = String.concat " " (List.map (Printf.sprintf "'r%d") scheme.row_vars) in
    let all_vars = 
      if rvars = "" then vars 
      else if vars = "" then rvars
      else vars ^ " " ^ rvars
    in
    Printf.sprintf "forall %s. %s" all_vars (pp_type scheme.body)