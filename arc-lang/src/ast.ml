open Utils
open Error

type ast = items

and names = name list
and name = string
and defname =
  | DName of name
  | DUnOp of unop * tys
  | DBinOp of binop * tys
and paths = path list
and path =
  | PAbs of names
  | PRel of names
and arms = arm list
and arm = pattern * expr
and params = param list
and param = pattern
and sinks = sink list
and sink = name * ty option
and index = int
and 't fields = 't field list
and 't field = name * 't option
and 't record = 't fields * 't option
and expr_record = expr_field list * expr option
and expr_field =
  | FName of name * expr option
  | FExpr of expr * name
and pat_record = pat_field list * pattern option
and pat_field = name * pattern option
and ty_record = ty_field list * ty option
and ty_field = name * ty option
and 't variants = 't variant list
and 't variant = name * 't
and 't enum = 't variants * 't option
and block = stmts * expr option
and generics = generic list
and generic = name

and decorator = const fields

and infix = bool

and items = item list
and item =
  (* | INoop         of info *)
  | IFrom         of info * decorator * sources * query_stmts
  | IAbstractDef  of info * decorator * abstract * defname * generics * tys * ty option * effects * bounds
  | IDef          of info * decorator * defname * generics * params * ty option * bounds * block
  | IVal          of info * decorator * name * ty option * expr
  | IAbstractType of info * decorator * abstract * name * generics * bounds
  | IClass        of info * decorator * name * generics * bounds * method_decls
  | IInstance     of info * decorator * generics * path * tys * bounds * method_defs
  | IMod          of info * decorator * name * items
  | IType         of info * decorator * name * generics * ty * bounds
  | IUse          of info * decorator * path * use_suffix option

and abstract =
  | AExtern
  | ABuiltin

and bounds = bound list
and bound = path * tys

and effects = effect list
and effect = name

and use_suffix =
  | UAlias of name
  | UGlob

and method_decls = method_decl list
and method_decl = name * generics * params * ty option * bounds

and method_defs = method_def list
and method_def = name * generics * params * ty option * bounds * block

and patterns = pattern list
and pattern =
  | PIgnore  of info
  | POr      of info * pattern * pattern
  | PAnnot   of info * pattern * ty
  | PRecord  of info * pat_record
  | PTuple   of info * patterns
  | PArray   of info * patterns * pattern option
  | PConst   of info * const
  | PVar     of info * name
  | PVariant of info * name * pattern

and tys = ty list
and ty =
  | TFunc   of info * tys * ty
  | TTuple  of info * tys
  | TEnum   of info * ty enum
  | TRecord of info * ty_record
  | TPath   of info * path * tys
  | TArray  of info * ty
  | TUnit   of info
  | TNever  of info

and binop =
  | BAdd
  | BAnd
  | BBand
  | BBor
  | BBxor
  | BDiv
  | BEq
  | BGeq
  | BGt
  | BLeq
  | BLt
  | BMod
  | BMul
  | BMut
  | BNeq
  | BOr
  | BPow
  | BSub
  | BXor
  | BIn
  | BRExc
  | BRInc
  | BBy
  | BNotIn
  | BMutAdd
  | BMutSub
  | BMutMul
  | BMutDiv
  | BMutMod

and unop =
  | UNeg
  | UNot

and lit =
  | LInt    of info * int * name option
  | LFloat  of info * float * name option
  | LBool   of info * bool
  | LString of info * string
  | LUnit   of info
  | LChar   of info * char

and const =
  | CInt    of info * int
  | CFloat  of info * float
  | CBool   of info * bool
  | CString of info * string
  | CUnit   of info
  | CChar   of info * char

and stmts = stmt list
and stmt =
  | SNoop of info
  | SVal  of info * param * expr
  | SVar  of info * (name * ty option) * expr
  | SExpr of info * expr

(* and args = arg list *)
(* and arg = *)
(*   | ANamed of name * expr *)
(*   | APos  of expr *)

and exprs = expr list
and expr =
  | EAccessRecord   of info * expr * name
  | EAccessRecordMulti   of info * expr * names
  | ESliceRecord   of info * expr * name
  | ECall     of info * expr * exprs
  | EAnnot    of info * expr * ty
  | EIf       of info * expr * block * block option
  | ELit      of info * lit
  | ELoop     of info * block
  | ERecord   of info * expr_record
  | EDynRecord of info * (expr * expr) list
  | EVariant  of info * expr variant
  | EReturn   of info * expr option
  | EBreak    of info * expr option
  | EContinue of info
  | EDict     of info * (expr * expr) list
  | ESet      of info * exprs
  | ESource   of info * ty * (expr * expr) list
  (* NB: These expressions are desugared *)
  | EBinOpRef of info * binop
  | EUnOp     of info * unop * tys * expr
  | EArray    of info * exprs * expr option
  | EBinOp    of info * binop * tys * expr * expr
  | EBlock    of info * block
  | EFor      of info * pattern * expr * block
  | EFunc     of info * params * block
  | EIfVal    of info * pattern * expr * block * block option
  | ECallItem of info * expr * name * exprs
  | EMatch    of info * expr * arms
  | EPath     of info * path * tys
  | EAccessTuple  of info * expr * index
  | EAccessArray of info * expr * exprs
  | EThrow    of info * expr
  | ETry      of info * block * arms * block option
  | ETuple    of info * exprs
  | EFrom     of info * sources * query_stmts
  | EAnon     of info
  | EWhile    of info * expr * block
  | EWhileVal of info * pattern * expr * block

and sources = source list
and source = pattern * sourcekind * expr
and sourcekind =
  | ScIn of info
  | ScEq of info

and query_stmts = query_stmt list
and query_stmt =
  | SWhere of {info:info; e:expr}
  | SJoin of {info:info; args:(pattern * expr) list; e:expr}
  | SGroup of {info:info; es:exprs; alias:name}
  | SWindow of {info:info; arg:expr; length:expr; alias:name; repeat:expr option; aggrs:aggr list}
  | SCompute of {info:info; aggrs:aggr list}
  | SOrder of {info:info; orders:(expr * ord) list}
  | SSelect of {info:info; e:expr}
  | SInto of {info:info; e:expr}

and aggr = (expr * expr option * name)

and ord =
  | OAsc
  | ODesc

let rec unop_name op =
  match op with
  | UNeg -> "neg"
  | UNot -> "not"

and binop_name op =
  match op with
  | BAdd -> "add"
  | BAnd -> "and"
  | BBand -> "band"
  | BBor -> "bor"
  | BBxor -> "bxor"
  | BDiv -> "div"
  | BGeq -> "geq"
  | BGt -> "gt"
  | BLeq -> "leq"
  | BLt -> "lt"
  | BMod -> "mod"
  | BMul -> "mul"
  | BNeq -> "neq"
  | BOr -> "or"
  | BPow -> "pow"
  | BSub -> "sub"
  | BXor -> "xor"
  | BIn -> "contains"
  | BNotIn -> "not_contains"
  | BRExc -> "rexc"
  | BRInc -> "rinc"
  | BEq -> "eq"
  | BMut -> "mut"
  | BBy -> "by"
  | BMutAdd -> "mut_add"
  | BMutSub -> "mut_sub"
  | BMutMul -> "mut_mul"
  | BMutDiv -> "mut_div"
  | BMutMod -> "mut_mod"

and def_name d =
  match d with
  | DName x -> x
  | DBinOp (op, _) -> binop_name op
  | DUnOp (op, _) -> unop_name op

and item_info p =
  match p with
  (* | INoop i -> i *)
  | IFrom (i, _, _, _) -> i
  | IAbstractDef (i, _, _, _, _, _, _, _, _) -> i
  | IDef (i, _, _, _, _, _, _, _) -> i
  | IVal (i, _, _, _, _) -> i
  | IAbstractType (i, _, _, _, _, _) -> i
  | IClass (i, _, _, _, _, _) -> i
  | IInstance (i, _, _, _, _, _, _) -> i
  | IMod (i, _, _, _) -> i
  | IType (i, _, _, _, _, _) -> i
  | IUse (i, _, _, _) -> i

and pat_info p =
  match p with
  | PIgnore (i) -> i
  | POr (i, _, _) -> i
  | PAnnot (i, _, _) -> i
  | PRecord (i, _) -> i
  | PTuple (i, _) -> i
  | PArray (i, _, _) -> i
  | PConst (i, _) -> i
  | PVar (i, _) -> i
  | PVariant (i, _, _) -> i

and ty_info t =
  match t with
  | TFunc (i, _, _) -> i
  | TTuple (i, _) -> i
  | TEnum (i, _) -> i
  | TRecord (i, _) -> i
  | TPath (i, _, _) -> i
  | TArray (i, _) -> i
  | TUnit i -> i
  | TNever i -> i

and expr_info e =
  match e with
  | EAccessRecord (i, _, _) -> i
  | EAccessRecordMulti (i, _, _) -> i
  | EAccessTuple (i, _, _) -> i
  | EAccessArray (i, _, _) -> i
  | ECall (i, _, _) -> i
  | EAnnot (i, _, _) -> i
  | EIf (i, _, _, _) -> i
  | ELit (i, _) -> i
  | ELoop (i, _) -> i
  | ERecord (i, _) -> i
  | EVariant (i, _) -> i
  | EReturn (i, _) -> i
  | EBreak (i, _) -> i
  | EContinue (i) -> i
  | EBinOpRef (i, _) -> i
  | EUnOp (i, _, _, _) -> i
  | EArray (i, _, _) -> i
  | EBinOp (i, _, _, _, _) -> i
  | EBlock (i, _) -> i
  | EFor (i, _, _, _) -> i
  | EFunc (i, _, _) -> i
  | EIfVal (i, _, _, _, _) -> i
  | ECallItem (i, _, _, _) -> i
  | EMatch (i, _, _) -> i
  | EPath (i, _, _) -> i
  | ETry (i, _, _, _) -> i
  | EThrow (i, _) -> i
  | ETuple (i, _) -> i
  | EFrom (i, _, _) -> i
  | EAnon (i) -> i
  | EWhile (i, _, _) -> i
  | EWhileVal (i, _, _, _) -> i
  | ESliceRecord (i, _, _) -> i
  | EDict (i, _) -> i
  | ESet (i, _) -> i
  | EDynRecord (i, _) -> i
  | ESource (i, _, _) -> i

and name_of_item i =
  match i with
  (* | INoop _ -> None *)
  | IFrom (_, _, _, _) -> None
  | IAbstractDef (_, _, _, d, _, _, _, _, _) -> Some (def_name d)
  | IDef (_, _, d, _, _, _, _, _) -> Some (def_name d)
  | IVal (_, _, x, _, _) -> Some x
  | IAbstractType (_, _, _, x, _, _) -> Some x
  | IClass (_, _, x, _, _, _) -> Some x
  | IInstance (_, _, _, _, _, _, _) -> None
  | IMod (_, _, x, _) -> Some x
  | IType (_, _, x, _, _, _) -> Some x
  | IUse (_, _, _, _) -> None

(* Extracts a list of uses for all items in this item *)
and extract_prelude (i:item) =
  let rec items_of_mod xs acc i =
    match i with
    | IMod (_, _, x, is) ->
        is |> foldl (items_of_mod (x::xs)) acc
    | _ ->
        match name_of_item i with
        | Some x -> (x::xs)::acc
        | None -> acc
  in
  items_of_mod [] [] i |> map (fun xs -> IUse ((Show, None), [], PAbs (List.rev xs), None))

(* Add items as a prelude to item *)
and add_prelude is0 i =
  match i with
  | IMod (info, d, x, is1) ->
      let is1' = is1 |> map (add_prelude is0) in
      IMod (info, d, x, is0 @ is1')
  | _ -> i

(* Hides an item definition *)
and hide i =
  match i with
  (* | INoop ((_, pos)) -> INoop ((Hide, pos)) *)
  | IAbstractDef ((_, pos), d, k, x, gs, ts, t, effs, bs) -> IAbstractDef ((Hide, pos), d, k, x, gs, ts, t, effs, bs)
  | IDef ((_, pos), d, x, gs, ps, t, bs, b) -> IDef ((Hide, pos), d, x, gs, ps, t, bs, b)
  | IVal ((_, pos), d, x, t, e) -> IVal ((Hide, pos), d, x, t, e)
  | IAbstractType ((_, pos), d, a, x, bs, gs) -> IAbstractType ((Hide, pos), d, a, x, bs, gs)
  | IClass ((_, pos), d, x, gs, bs, method_decls) -> IClass ((Hide, pos), d, x, gs, bs, method_decls)
  | IInstance ((_, pos), d, gs, xs, ts, bs, method_defs) -> IInstance ((Hide, pos), d, gs, xs, ts, bs, method_defs)
  | IMod ((_, pos), d, x, is) -> IMod ((Hide, pos), d, x, is |> map hide)
  | IType ((_, pos), d, x, gs, bs, t) -> IType ((Hide, pos), d, x, gs, bs, t)
  | IUse ((_, pos), d, xs, use_suffix) -> IUse ((Hide, pos), d, xs, use_suffix)
  | IFrom ((_, pos), d, sources, clauses) -> IFrom ((Hide, pos), d, sources, clauses)
