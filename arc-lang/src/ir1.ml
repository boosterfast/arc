open Utils
open Error

type ir1 = (path * item) list
and names = name list
and name = string
and paths = path list
and path = names
and arms = arm list
and arm = pattern * block
and params = param list
and param = name * ty
and index = int
and 't fields = 't field list
and 't field = name * 't
and 't record = 't fields * 't option
and variants = variant list
and variant = info * name * tys
and block = stmts * expr
and generics = generic list
and generic = name

and stmts = stmt list
and stmt = SExpr of expr

and decorator = Ast.decorator

and abstract = Ast.abstract

and items = item list
and item =
  | IClass        of info * decorator * generics * bounds * instances
  | IDef          of info * decorator * generics * params * ty * bounds * block
  | IAbstractDef  of info * decorator * abstract * generics * tys * ty * efs * bounds
  | IAbstractType of info * decorator * abstract * generics * bounds
  | IType         of info * decorator * generics * ty * bounds
  | IVal          of info * decorator * ty * block

and efs = ef list
and ef = string
and instances = instance list
and instance = generics * tys * bounds

and bounds = bound list
and bound = path * tys

and patterns = pattern list
and pattern =
  | PIgnore  of info * ty
  | POr      of info * ty * pattern * pattern
  | PAnnot   of info * ty * pattern
  | PRecord  of info * ty * pattern record
  | PConst   of info * ty * lit
  | PVar     of info * ty * name
  | PUnwrap  of info * ty * name * pattern

and tys = ty list
and ty =
  | TFunc      of tys * ty
  | TRecord    of ty
  | TEnum      of ty
  | TRowEmpty
  | TRowExtend of ty field * ty
  | TNominal   of path * tys
  | TGeneric   of name
  | TVar       of name

and lit =
  | LInt    of info * int
  | LFloat  of info * float
  | LBool   of info * bool
  | LString of info * string
  | LUnit   of info
  | LChar   of info * char

and exprs = expr list
and expr =
  | EAccessRecord of info * ty * expr * name
  | EBreak        of info * ty * expr
  | ECallExpr     of info * ty * expr * exprs
  | ECallItem     of info * ty * path * tys * exprs
  | EAnnot        of info * ty * expr
  | EContinue     of info * ty
  | EEnwrap       of info * ty * name * expr
  | EItem         of info * ty * path * tys
  | EVar          of info * ty * name
  | ELit          of info * ty * lit
  | ELoop         of info * ty * block
  | EMatch        of info * ty * expr * arms
  | ERecord       of info * ty * expr record
  | EReturn       of info * ty * expr
  | EUpdateRecord       of info * ty * expr * name * expr

let item_info i =
  match i with
  | IAbstractDef (info, _, _, _, _, _, _, _) -> info
  | IDef (info, _, _, _, _, _, _) -> info
  | IVal (info, _, _, _) -> info
  | IAbstractType (info, _, _, _, _) -> info
  | IClass (info, _, _, _, _) -> info
  | IType (info, _, _, _, _) -> info

(* Create a nominal type *)
let rec nominal x gs = TNominal (["std"; x], gs)

(* Create an atomic type. Atomic types are defined in the standard library. *)
and atom x = nominal x []

(* Returns the parent path of a path *)
and parent xs = xs |> rev |> tl |> rev

let map_fields f fs = map (fun (x, v) -> (x, f v)) fs
let map_opt f v = match v with Some v -> Some (f v) | None -> None
let map_record f (fs, t) = (map_fields f fs, map_opt f t)

(* Map types *)
let rec tmap_item f i =
  match i with
  | IClass (info, d, gs, bs, is) -> IClass (info, d, gs, map (tmap_bound f) bs, map (tmap_inst f) is)
  | IDef (info, d, gs, pts, ty, bs, b) -> IDef (info, d, gs, map (tmap_param f) pts, f ty, map (tmap_bound f) bs, tmap_block f b)
  | IAbstractDef (info, d, a, gs, tys, ty, efs, bs) -> IAbstractDef (info, d, a, gs, map f tys, f ty, efs, map (tmap_bound f) bs)
  | IAbstractType (info, d, a, gs, bs) -> IAbstractType (info, d, a, gs, map (tmap_bound f) bs)
  | IType (info, d, gs, ty, bs) -> IType (info, d, gs, f ty, map (tmap_bound f) bs)
  | IVal (info, d, ty, b) -> IVal (info, d, f ty, tmap_block f b)

and tmap_bound f (x, ts) = (x, map f ts)
and tmap_inst f (gs, ts, bs) = (gs, map f ts, map (tmap_bound f) bs)

and tmap_expr f e =
  match e with
  | ELoop (info, t, b) -> ELoop (info, f t, tmap_block f b)
  | EEnwrap (info, t, x, e) -> EEnwrap (info, f t, x, tmap_expr f e)
  | EItem (info, t, xs, ts) -> EItem (info, f t, xs, map f ts)
  | ECallItem (info, t, xs, ts, es) -> ECallItem (info, f t, xs, map f ts, map (tmap_expr f) es)
  | EMatch (info, t, e, arms) -> EMatch (info, f t, tmap_expr f e, map (tmap_arm f) arms)
  | EAccessRecord (info, t, e, x) -> EAccessRecord (info, f t, tmap_expr f e, x)
  | EBreak (info, t, e) -> EBreak (info, f t, tmap_expr f e)
  | ECallExpr (info, t, e, es) -> ECallExpr (info, f t, tmap_expr f e, map (tmap_expr f) es)
  | EAnnot (info, t0, e) -> EAnnot (info, f t0, tmap_expr f e)
  | EContinue (info, t) -> EContinue (info, f t)
  | ELit (info, t, l) -> ELit (info, f t, l)
  | EVar (info, t, x) -> EVar (info, f t, x)
  | ERecord (info, t, r) -> ERecord (info, f t, map_record (tmap_expr f) r)
  | EReturn (info, t, e) -> EReturn (info, f t, tmap_expr f e)
  | EUpdateRecord (info, t, e0, x, e1) -> EUpdateRecord (info, f t, tmap_expr f e0, x, tmap_expr f e1)

and tmap_type f t =
  match t with
  | TFunc (ts, t) -> TFunc (map f ts, f t)
  | TRecord t -> TRecord (f t)
  | TEnum t -> TEnum (f t)
  | TRowEmpty -> TRowEmpty
  | TRowExtend ((x, t), r) -> TRowExtend ((x, f t), f r)
  | TNominal (xs, ts) -> TNominal (xs, map f ts)
  | TGeneric x -> TGeneric x
  | TVar x -> TVar x

and tmap_pat f p =
  match p with
  | PIgnore (info, t) -> PIgnore (info, f t)
  | POr (info, t, p0, p1) -> POr (info, f t, tmap_pat f p0, tmap_pat f p1)
  | PAnnot (info, t0, p) -> PAnnot (info, f t0, tmap_pat f p)
  | PRecord (info, t, r) -> PRecord (info, f t, map_record (tmap_pat f) r)
  | PConst (info, t, l) -> PConst (info, f t, l)
  | PVar (info, t, x) -> PVar (info, f t, x)
  | PUnwrap (info, t, x, p) -> PUnwrap (info, f t, x, tmap_pat f p)

and tmap_arm f (p, b) = (tmap_pat f p, tmap_block f b)
and tmap_param f (x, t) = (x, f t)
and tmap_block f (es, e) = (map (tmap_stmt f) es, tmap_expr f e)
and tmap_stmt f s =
  match s with
  | SExpr e -> SExpr (tmap_expr f e)

(* Map expressions *)

and emap_item f i =
  match i with
  | IClass (info, d, gs, bs, is) -> IClass (info, d, gs, bs, is)
  | IDef (info, d, gs, pts, ty, bs, b) -> IDef (info, d, gs, pts, ty, bs, emap_block f b)
  | IAbstractDef (info, d, a, gs, tys, ty, efs, bs) -> IAbstractDef (info, d, a, gs, tys, ty, efs, bs)
  | IAbstractType (info, d, a, gs, bs) -> IAbstractType (info, d, a, gs, bs)
  | IType (info, d, gs, ty, bs) -> IType (info, d, gs, ty, bs)
  | IVal (info, d, ty, b) -> IVal (info, d, ty, emap_block f b)

and emap_expr f p =
  match p with
  | ELoop (info, t, b) -> ELoop (info, t, emap_block f b)
  | EEnwrap (info, t, x, e) -> EEnwrap (info, t, x, f e)
  | EItem (info, t, xs, ts) -> EItem (info, t, xs, ts)
  | ECallItem (info, t, xs, ts, es) -> ECallItem (info, t, xs, ts, map f es)
  | EMatch (info, t, e, arms) -> EMatch (info, t, f e, map (emap_arm f) arms)
  | EAccessRecord (info, t, e, x) -> EAccessRecord (info, t, f e, x)
  | EBreak (info, t, e) -> EBreak (info, t, f e)
  | ECallExpr (info, t, e, es) -> ECallExpr (info, t, f e, map f es)
  | EAnnot (info, t0, e) -> EAnnot (info, t0, f e)
  | EContinue (info, t) -> EContinue (info, t)
  | ELit (info, t, l) -> ELit (info, t, l)
  | EVar (info, t, x) -> EVar (info, t, x)
  | ERecord (info, t, r) -> ERecord (info, t, map_record (emap_expr f) r)
  | EReturn (info, t, e) -> EReturn (info, t, f e)
  | EUpdateRecord (info, t, e0, x, e1) -> EUpdateRecord (info, t, f e0, x, f e1)

and emap_block f (es, e) = (map (emap_stmt f) es, f e)
and emap_stmt f s =
  match s with
  | SExpr e -> SExpr (f e)
and emap_arm f (p, b) = (p, emap_block f b)

(* Map patterns *)

let rec pmap_item f i =
  match i with
  | IClass (info, d, gs, bs, is) -> IClass (info, d, gs, bs, is)
  | IDef (info, d, gs, pts, t, bs, b) -> IDef (info, d, gs, pts, t, bs, emap_block f b)
  | IAbstractDef (info, d, a, gs, ts, t, efs, bs) -> IAbstractDef (info, d, a, gs, ts, t, efs, bs)
  | IAbstractType (info, d, a, gs, bs) -> IAbstractType (info, d, a, gs, bs)
  | IType (info, d, gs, t, bs) -> IType (info, d, gs, t, bs)
  | IVal (info, d, t, b) -> IVal (info, d, t, emap_block f b)

and pmap_expr f p =
  match p with
  | ELoop (info, t, b) -> ELoop (info, t, pmap_block f b)
  | EEnwrap (info, t, x, e) -> EEnwrap (info, t, x, pmap_expr f e)
  | EItem (info, t, xs, ts) -> EItem (info, t, xs, ts)
  | ECallItem (info, t, xs, ts, es) -> ECallItem (info, t, xs, ts, map (pmap_expr f) es)
  | EMatch (info, t, e, arms) -> EMatch (info, t, pmap_expr f e, map (pmap_arm f) arms)
  | EAccessRecord (info, t, e, x) -> EAccessRecord (info, t, pmap_expr f e, x)
  | EBreak (info, t, e) -> EBreak (info, t, pmap_expr f e)
  | ECallExpr (info, t, e, es) -> ECallExpr (info, t, pmap_expr f e, map (pmap_expr f) es)
  | EAnnot (info, t0, e) -> EAnnot (info, t0, pmap_expr f e)
  | EContinue (info, t) -> EContinue (info, t)
  | ELit (info, t, l) -> ELit (info, t, l)
  | EVar (info, t, x) -> EVar (info, t, x)
  | ERecord (info, t, r) -> ERecord (info, t, map_record (pmap_expr f) r)
  | EReturn (info, t, e) -> EReturn (info, t, pmap_expr f e)
  | EUpdateRecord (info, t, e0, x, e1) -> EUpdateRecord (info, t, pmap_expr f e0, x, pmap_expr f e1)

and pmap_pat f p =
  match p with
  | PIgnore (info, t) -> PIgnore (info, t)
  | POr (info, t, p0, p1) -> POr (info, t, f p0, f p1)
  | PAnnot (info, t0, p) -> PAnnot (info, t0, f p)
  | PRecord (info, t, r) -> PRecord (info, t, map_record (pmap_pat f) r)
  | PConst (info, t, l) -> PConst (info, t, l)
  | PVar (info, t, x) -> PVar (info, t, x)
  | PUnwrap (info, t, x, p) -> PUnwrap (info, t, x, f p)

and pmap_arm f (p, b) = (f p, pmap_block f b)
and pmap_block f (es, e) = (map (pmap_stmt f) es, pmap_expr f e)
and pmap_stmt f s =
  match s with
  | SExpr e -> SExpr (pmap_expr f e)

(* Typeof *)

let typeof_expr e =
  match e with
  | EAccessRecord (_, t, _, _) -> t
  | EUpdateRecord (_, t, _, _, _) -> t
  | ECallExpr (_, t, _, _) -> t
  | ECallItem (_, t, _, _, _) -> t
  | EAnnot (_, t, _) -> t
  | EEnwrap (_, t, _, _) -> t
  | ELit (_, t, _) -> t
  | EVar (_, t, _) -> t
  | ELoop (_, t, _) -> t
  | ERecord (_, t, _) -> t
  | EReturn (_, t, _) -> t
  | EBreak (_, t, _) -> t
  | EContinue (_, t) -> t
  | EItem (_, t, _, _) -> t
  | EMatch (_, t, _, _) -> t

let typeof_pat p =
  match p with
  | PIgnore (_, t) -> t
  | POr (_, t, _, _) -> t
  | PAnnot (_, t, _) -> t
  | PRecord (_, t, _) -> t
  | PConst (_, t, _) -> t
  | PVar (_, t, _) -> t
  | PUnwrap (_, t, _, _) -> t

let typeof_block (_, e) = typeof_expr e

(* Convert an index of a tuple into a field of a record *)
let index_to_field i = Printf.sprintf "_%d" i

(* Convert indexes of a tuple to fields of a record *)
let indexes_to_fields is =
  is |> List.fold_left (fun (l, c) v -> ((index_to_field c, v)::l, c+1)) ([], 0)
     |> fst
     |> List.rev

(* Convert match arms to clauses. A clause has the following form: `(eqs, substs, expr)` where:
** - `eqs` are a set of equations of the form `(v, p)` where
**   - `v` is a variable
**   - `p` is a pattern match on the variable `v`
** - `substs` are a set of substitutions of the form `(v0, v1)` where
**   - `v0` is substituted for `v1` inside `expr`
** - `expr` is an expression which is evaluated if the clause succeeds
*)
let arms_to_clauses arms e =
  arms |> List.map (fun (p, b) -> ([(e, p)], [], b))

(* t is the tail, which could either be a Ir1.TVar or Ir1.TRowEmpty *)
let fields_to_rows t fs =
  fs |> List.fold_left (fun t f -> TRowExtend (f, t)) t

(* Converts a list [v0; v1; ..; vn] into [("_0", v0); ("_1", v1); ...; ("_n", vn)] *)
let indexes_to_rows t is =
  is |> indexes_to_fields |> fields_to_rows t

type ctx = {
  scopes: scopes;
  vars: name list
}
and scopes = scope list
and scope = name list

let bind_var v ctx =
  { ctx with scopes = (v::(hd ctx.scopes))::(tl ctx.scopes) }

let rec bind_pat ctx p =
  match p with
  | PIgnore _ -> ctx
  | POr (_, _, p0, p1) -> bind_pat (bind_pat ctx p1) p0
  | PAnnot (_, _, p) -> bind_pat ctx p
  | PRecord (_, _, (xps, p)) ->
      let ctx = foldl (fun ctx (_, p) -> bind_pat ctx p) ctx xps in
      begin match p with
      | Some p -> bind_pat ctx p
      | None -> ctx
      end
  | PConst _ -> ctx
  | PVar (_, _, x) -> bind_var x ctx
  | PUnwrap (_, _, _, ps) -> bind_pat ctx ps

let bound_vars ps =
  let ctx = { scopes = [[]]; vars = [] } in
  let ctx = ps |> foldl bind_pat ctx in
  ctx.scopes |> hd

(* Calculates the free variables of a block `b` parameterized by `vs` *)
let free_vars vs b =

  (* A variable is free if it is not bound in any scope *)
  let is_free v scopes = not (scopes |> exists (mem v)) in

  (* Convenience function *)
  let fv_var v ctx =
    if is_free v ctx.scopes then
      { ctx with vars = v::ctx.vars }
    else
      ctx
  in

  (* Push a new scope to the stack *)
  let push_scope ctx = { ctx with scopes = []::ctx.scopes } in

  (* Pop a scope off the stack *)
  let pop_scope ctx = { ctx with scopes = tl ctx.scopes } in

  (* Returns the list of free variables in a block *)
  let rec fv_block ctx (ss, e) =
    let ctx = push_scope ctx in
    let ctx = foldl fv_stmt ctx ss in
    let ctx = fv_expr ctx e in
    let ctx = pop_scope ctx in
    ctx

  and fv_stmt ctx s =
    match s with
    | SExpr e -> fv_expr ctx e

  and fv_arm ctx (p, b) =
    let ctx = push_scope ctx in
    let ctx = bind_pat ctx p in
    let ctx = fv_block ctx b in
    let ctx = pop_scope ctx in
    ctx

  (* Returns the list of free variables in an expression *)
  and fv_expr ctx e =
    match e with
    | EAccessRecord (_, _, e, _) -> fv_expr ctx e
    | EUpdateRecord (_, _, e0, _, e1) -> foldl fv_expr ctx [e0; e1]
    | ECallExpr (_, _, e, es) -> foldl fv_expr ctx (e::es)
    | ECallItem (_, _, _, _, es) -> foldl fv_expr ctx es
    | EAnnot (_, _, e) -> fv_expr ctx e
    | EEnwrap (_, _, _, e) -> fv_expr ctx e
    | ELit _ -> ctx
    | EVar (_, _, x) -> fv_var x ctx
    | ELoop (_, _, b) -> fv_block ctx b
    | ERecord (_, _, (xes, e)) ->
        let ctx = foldl (fun ctx (_, e) -> fv_expr ctx e) ctx xes in
        begin match e with
        | Some e -> fv_expr ctx e
        | None -> ctx
        end
    | EReturn (_, _, e) -> fv_expr ctx e
    | EBreak (_, _, e) -> fv_expr ctx e
    | EContinue _ -> ctx
    | EItem _ -> ctx
    | EMatch (_, _, e, arms) -> foldl fv_arm (fv_expr ctx e) arms
  in
  let ctx = { scopes=[vs]; vars=[] } in
  let ctx = fv_block ctx b in
  ctx.vars |> List.rev

(* Given a path, returns the corresponding item from Ir1 *)
and get_item info xs ir1 =
  match ir1 |> assoc_opt xs with
  | Some i -> i
  | None -> raise (Error.NamingError (info, "get_item: " ^ Pretty.path_to_str xs ^ " not found"))
