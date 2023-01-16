open Error

type mlir = (symbol * item) list
and names = name list
and name = string
and value = name
and symbol = name
and 't fields = 't field list
and 't field = name * 't
and 't variants = 't variant list
and 't variant = name * 't
and ssas = ssa list
and ssa = param option * op
and args = arg list
and arg = value * ty
and params = param list
and param = value * ty
and block = ssas
and effects = effect list
and effect = name
and item =
  | IExternFunc of info * symbol * params * ty * effects
  | IFunc       of info * params * ty option * block
and tys = ty list
and ty =
  | TFunc    of tys * ty
  | TRecord  of ty fields
  | TEnum    of ty fields
  | TAdt     of name * tys
  | TNative  of name
and ops = op list
and op =
  | OAccess   of info * arg * name
  | OUpdate   of info * arg * name * arg
  | OCallExpr of info * arg * args
  | OCallItem of info * symbol * args
  | OEnwrap   of info * name * arg option
  | OIf       of info * arg * block * block
  | OCheck    of info * name * arg
  | OConst    of info * const
  | OLoop     of info * block
  | ORecord   of info * names * tys
  | OUnwrap   of info * name * arg
  | OReturn   of info * arg option
  | OBreak    of info * arg option
  | OContinue of info
  | OYield    of info
  | OResult   of info * arg option
and const =
  | CBool  of bool
  | CFun   of symbol
  | CInt   of int
  | CFloat of float
  | CAdt   of string

let item_info i =
  match i with
  | IExternFunc (i, _, _, _, _) -> i
  | IFunc       (i, _, _, _) -> i
