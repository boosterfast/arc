type token =
  | ParenL
  | ParenR
  | BrackL
  | BrackR
  | BraceL
  | BraceR
  | AngleL
  | AngleR
(*= Operators ==============================================================*)
  | Neq
  | Percent
  | Star
  | StarStar
  | Plus
  | Comma
  | Minus
  | Dot
  | DotDot
  | DotDotEq
  | Slash
  | Colon
  | ColonColon
  | Semi
  | Leq
  | Eq
  | EqEq
  | Imply
  | Geq
  | AtSign
  | Underscore
  | Bar
  | PlusEq
  | MinusEq
  | StarEq
  | SlashEq
  | PercentEq
  | Tilde
  | Never
(*= Keywords ================================================================*)
  | And
  | As
  | Break
  | Band
  | Bor
  | Bxor
  | Builtin
  | Case
  | Catch
  | Class
  | Continue
  | Dict
  | Def
  | Desc
  | Do
  | Dyn
  | Else
  | Extern
  | Finally
  | For
  | From
  | Fun
  | Group
  | If
  | In
  | Infix
  | Into
  | Instance
  | Join
  | Length
  | Loop
  | Match
  | Mod
  | New
  | Not
  | On
  | Or
  | Of
  | Order
  | Return
  | Compute
  | Set
  | Select
  | Repeat
  | Throw
  | Try
  | Type
  | Val
  | Var
  | Where
  | Window
  | While
  | Use
  | Xor
(*= Identifiers and Literals ================================================*)
  | Name of string
  | Int of int
  | IntSuffix of (int * string)
  | Float of float
  | FloatSuffix of (float * string)
  | Bool of bool
  | Char of char
  | String of string
  | Unit
(*   | Date of string *)
(*   | DateTime of string *)
(*   | DateTimeZone of string *)
  | Eof
