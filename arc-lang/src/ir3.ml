open Error

type ir3 = ((path * tys) * item) list
and names = name list
and name = string
and paths = path list
and path = names
and params = param list
and param = name * ty
and index = int
and 't fields = 't field list
and 't field = name * 't
and 't variants = 't variant list
and 't variant = name * 't
and block = stmts * var
and vars = var list
and var = name

and stmts = stmt list
and stmt = SVal of name * ty * expr

and decorator = Ast.decorator
and abstract = Ast.abstract

and items = item list
and item =
  | IDef          of info * decorator * params * ty * block
  | IAbstractDef  of info * decorator * abstract * tys * ty * effects
  | IAbstractType of info * decorator * abstract
  | IType         of info * decorator * ty
  | IVal          of info * decorator * ty * block

and effects = effect list
and effect = name

and tys = ty list
and ty =
  | TFunc      of tys * ty
  | TRecord    of ty fields
  | TEnum      of ty variants
  | TNominal   of path * tys

and lit = Ir2.lit

and exprs = expr list
and expr =
  | EAccessRecord of info * var * name
  | EUpdateRecord of info * var * name * var
  | ERecord       of info * var fields
  | EBreak        of info * var
  | ECallExpr     of info * var * vars
  | ECallItem     of info * path * tys * vars
  | EContinue     of info
  | EEnwrap       of info * name * var
  | EUnwrap       of info * name * var
  | ECheck        of info * name * var
  | EItem         of info * path * tys
  | EIf           of info * var * block * block
  | ELit          of info * lit
  | ELoop         of info * block
  | EReturn       of info * var
  | EBif          of info * bif

and bif =
  | BifAdd    of expr * expr
  | BifSub    of expr * expr
  | BifMul    of expr * expr
  | BifDiv    of expr * expr
  | BifMap    of expr * expr
  | BifFilter of expr * expr
  | BifGroup  of expr * expr
  | BifApply  of expr * expr

let item_info i =
  match i with
  | IAbstractDef (i, _, _, _, _, _) -> i
  | IDef (i, _, _, _, _) -> i
  | IVal (i, _, _, _) -> i
  | IAbstractType (i, _, _) -> i
  | IType (i, _, _) -> i

(* Returns the parent path of a path *)
let rec parent xs = xs |> List.rev |> List.tl |> List.rev

and atom x = TNominal (["std"; x], [])

and is_unit t =
  t = (atom "unit")
