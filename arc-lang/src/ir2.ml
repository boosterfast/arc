open Error

type ir2 = (path * item) list
and names = name list
and name = string
and paths = path list
and path = names
and params = param list
and param = name * ty
and index = int
and 't fields = 't field list
and 't field = name * 't
and 't record = 't fields * 't option
and variants = variant list
and variant = info * name * tys
and block = stmts * var
and vars = var list
and var = name
and generics = generic list
and generic = name

and stmts = stmt list
and stmt =
  | SVal of name * ty * expr

and decorator = Ast.decorator
and abstract = Ast.abstract

and items = item list
and item =
  | IClass        of info * decorator * generics * paths
  | IClassDef     of info * decorator * path * generics * params * ty
  | IDef          of info * decorator * generics * params * ty * block
  | IAbstractDef  of info * decorator * abstract * generics * tys * ty * efs
  | IAbstractType of info * decorator * abstract * generics
  | IInstance     of info * decorator * generics * path * tys * paths
  | IInstanceDef  of info * decorator * path * generics * params * ty * block
  | IType         of info * decorator * generics * ty
  | IVal          of info * decorator * ty * block

and efs = ef list
and ef = name

and tys = ty list
and ty =
  | TFunc      of tys * ty
  | TRecord    of ty
  | TEnum      of ty
  | TRowEmpty
  | TRowExtend of ty field * ty
  | TNominal   of path * tys
  | TGeneric   of name

and lit =
  | LInt    of info * int
  | LFloat  of info * float
  | LBool   of info * bool
  | LString of info * string
  | LUnit   of info
  | LChar   of info * char

and exprs = expr list
and expr =
  | EAccessRecord of info * var * name
  | ESubset   of info * var * ty
  | EBreak    of info * var
  | ECallExpr of info * var * vars
  | ECallItem of info * path * tys * vars
  | EContinue of info
  | EEnwrap   of info * name * var
  | EUnwrap   of info * name * var
  | ECheck    of info * name * var
  | EItem     of info * path * tys
  | EIf       of info * var * block * block
  | ELit      of info * lit
  | ELoop     of info * block
  | ERecord   of info * var record
  | EReturn   of info * var
  | EUpdateRecord   of info * var * name * var

let item_info i =
  match i with
  | IAbstractDef (i, _, _, _, _, _, _) -> i
  | IDef (i, _, _, _, _, _) -> i
  | IVal (i, _, _, _) -> i
  | IAbstractType (i, _, _, _) -> i
  | IClass (i, _, _, _) -> i
  | IClassDef (i, _, _, _, _, _) -> i
  | IInstanceDef (i, _, _, _, _, _, _) -> i
  | IInstance (i, _, _, _, _, _) -> i
  | IType (i, _, _, _) -> i

let atom x = TNominal (["std"; x], [])
