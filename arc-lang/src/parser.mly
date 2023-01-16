%{
  open Error
  open Utils
%}

%start <Ast.ast> program
%start <Ast.expr> expr
%%

expr: expr0(expr17) Eof { $1 }
program: items Eof { $1 }

decorator: loption(snd("@", brace(seq(pair(name, snd(":", const)?))))) { $1 }

items: llist(item) { $1 }
item:
  | decorator "from" sources clauses ";"
    { Ast.IFrom (loc $loc, $1, $3, $4) }
  | decorator abstract "def" def_name generics paren_seq(ty0) ty_annot? loption(ef_annot) where ";"
    { Ast.IAbstractDef (loc $loc, $1, $2, $4, $5, $6, $7, $8, $9) }
  | decorator "def" def_name generics params ty_annot? where body(expr0(expr16))
    { Ast.IDef (loc $loc, $1, $3, $4, $5, $6, $7, $8) }
  | decorator "type" name generics "=" ty0 where ";"
    { Ast.IType (loc $loc, $1, $3, $4, $6, $7) }
  | decorator abstract "type" name generics where ";"
    { Ast.IAbstractType (loc $loc, $1, $2, $4, $5, $6) }
  | decorator "class" name generics where brace(llist(decl)) ";"
    { Ast.IClass (loc $loc, $1, $3, $4, $5, $6) }
  | decorator "instance" generics path loption(brack(seq(ty0))) where brace(llist(def)) ";"
    { Ast.IInstance (loc $loc, $1, $3, $4, $5, $6, $7) }
  | decorator "mod" name brace(items) ";"
    { Ast.IMod (loc $loc, $1, $3, $4) }
  | decorator "use" path use_suffix? ";"
    { Ast.IUse (loc $loc, $1, $3, $4) }
  | decorator "val" name ty_annot? "=" expr0(expr16) ";"
    { Ast.IVal (loc $loc, $1, $3, $4, $6) }

abstract:
  | "extern" { Ast.AExtern }
  | "builtin" { Ast.ABuiltin }

decl: "def" name generics params ty_annot? where ";"
  { ($2, $3, $4, $5, $6) }

def: "def" name generics params ty_annot? where body(expr0(expr16))
  { ($2, $3, $4, $5, $6, $7) }

body(expr):
  | "=" expr { ([], Some $2) }
  | block { $1 }

ef: name { $1 }

ef_annot: snd("~", brace(seq(ef))) { $1 }
ty_annot: ":" ty0 { $2 }

params: paren_seq(param) { $1 }
param: pat0 { $1 }

generics: loption(brack(seq(generic))) { $1 }
generic: name { $1 }

where: lopt(snd("where", seq(bound))) { $1 }
bound: path brack(seq(ty0)) { ($1, $2) }

name: Name { $1 }

binop:
  | op3 { $1 }
  | op4 { $1 }
  | op5 { $1 }
  | op6 { $1 }
  | op7 { $1 }
  | op8 { $1 }
  | op10 { $1 }

unop:
  | op9 { $1 }

def_name:
  | name { Ast.DName $1 }
  | unop lopt(qualify(seq(ty0))) { Ast.DUnOp ($1, $2) }
  | "infix" binop lopt(qualify(seq(ty0))) { Ast.DBinOp ($2, $3) }

index: Int { $1 }

%inline path:
  | separated_nonempty_llist("::", name) { Ast.PRel $1 }
  | "::" separated_nonempty_llist("::", name) { Ast.PAbs $2 }
use_suffix:
  | "*" { Ast.UGlob }
  | snd("as", name) { Ast.UAlias $1 }

(* Note: This cannot be succeeded by a brace *)
expr0(primary):
  | "from" sources clauses { Ast.EFrom (loc $loc, $2, $3) }
  | "return" expr1(primary)? { Ast.EReturn (loc $loc, $2) }
  | "break" expr1(primary)? { Ast.EBreak (loc $loc, $2) }
  | "continue" { Ast.EContinue (loc $loc) }
  | "throw" expr1(primary) { Ast.EThrow (loc $loc, $2) }
  | expr1(primary) { $1 }

(* Note: This cannot be succeeded by a brace *)
expr1(primary):
  | "fun" params ty_annot? body(expr1(expr17)) { Ast.EFunc (loc $loc, $2, $4) }
  | expr2(primary) { $1 }
  
op2:
  | "=" { Ast.BMut }
  | "+=" { Ast.BAdd }
  | "-=" { Ast.BSub }
  | "*=" { Ast.BMul }
  | "/=" { Ast.BDiv }
  | "%=" { Ast.BMod }
  | "in" { Ast.BIn }
  | "not" "in" { Ast.BNotIn }
expr2(primary):
  | expr3(primary) { $1 }
  | expr2(primary) op2 expr3(primary) { Ast.EBinOp (loc $loc, $2, [], $1, $3)}

op3:
  | ".." { Ast.BRExc }
  | "..=" { Ast.BRInc }
expr3(primary):
  | expr4(primary) { $1 }
  | expr4(primary) op3 lopt(qualify(seq(ty0))) expr4(primary) { Ast.EBinOp (loc $loc, $2, $3, $1, $4)}
  
op4:
  | "bor" { Ast.BBor }
  | "band" { Ast.BBand }
  | "bxor" { Ast.BBxor }
  | "or" { Ast.BOr }
  | "xor" { Ast.BXor }
  | "and" { Ast.BAnd }
expr4(primary):
  | expr5(primary) { $1 }
  | expr4(primary) op4 lopt(qualify(seq(ty0))) expr5(primary) { Ast.EBinOp (loc $loc, $2, $3, $1, $4)}

op5:
  | "==" { Ast.BEq }
  | "!=" { Ast.BNeq }
expr5(primary):
  | expr6(primary) { $1 }
  | expr5(primary) op5 lopt(qualify(seq(ty0))) expr6(primary) { Ast.EBinOp (loc $loc, $2, $3, $1, $4)}

op6:
  | "<" { Ast.BLt }
  | ">" { Ast.BGt }
  | "<=" { Ast.BLeq }
  | ">=" { Ast.BGeq }
expr6(primary):
  | expr7(primary) { $1 }
  | expr6(primary) op6 lopt(qualify(seq(ty0))) expr7(primary) { Ast.EBinOp (loc $loc, $2, $3, $1, $4)}

op7:
  | "+" { Ast.BAdd }
  | "-" { Ast.BSub }
  | "%" { Ast.BMod }
expr7(primary):
  | expr8(primary) { $1 }
  | expr7(primary) op7 lopt(qualify(seq(ty0))) expr8(primary) { Ast.EBinOp (loc $loc, $2, $3, $1, $4)}
  
op8:
  | "*" { Ast.BMul }
  | "/" { Ast.BDiv }
expr8(primary):
  | expr9(primary) { $1 }
  | expr8(primary) op8 lopt(qualify(seq(ty0))) expr9(primary) { Ast.EBinOp (loc $loc, $2, $3, $1, $4)}

op9:
  | "not" { Ast.UNot }
  | "-" { Ast.UNeg }
expr9(primary):
  | expr10(primary) { $1 }
  | op9 lopt(qualify(seq(ty0))) expr9(primary) { Ast.EUnOp (loc $loc, $1, $2, $3)}

op10:
  | "**" { Ast.BPow }
expr10(primary):
  | expr11(primary) { $1 }
  | expr11(primary) op10 lopt(qualify(seq(ty0))) expr10(primary) { Ast.EBinOp (loc $loc, $2, $3, $1, $4)}

expr11(primary):
  | expr12(primary) { $1 }
  | expr11(primary) ":" ty0 { Ast.EAnnot (loc $loc, $1, $3) }

expr12(primary):
  | expr13(primary) { $1 }
  | "new" variant(expr14(primary))
    { Ast.EVariant (loc $loc, $2) }

expr13(primary):
  | expr14(primary) { $1 }
  | expr15(primary) { $1 }

expr14(primary):
  | primary
    { $1 }
  | primary paren_seq(expr1(expr16))
    { Ast.ECall (loc $loc, $1, $2) }

(* TODO: Named arguments *)
(* arg: *)
(*   | name ":" expr1(expr16) { Ast.ANamed ($1, $3) } *)
(*   | expr1(expr16) { Ast.APos $1 } *)

expr15(primary):
  | expr13(primary) "." index
    { Ast.EAccessTuple (loc $loc, $1, $3) }
  | expr13(primary) "." name
    { Ast.EAccessRecord (loc $loc, $1, $3) }
  | expr13(primary) brack(seq(expr1(primary)))
    { Ast.EAccessArray (loc $loc, $1, $2) }
  | expr13(primary) "." brace(seq(name))
    { Ast.EAccessRecordMulti (loc $loc, $1, $3) }
  | expr13(primary) "." name paren_seq(expr1(expr16))
    { Ast.ECallItem (loc $loc, $1, $3, $4) }

%inline lopt(l): ioption(l) { match $1 with None -> [] | Some l -> l }

// This could allow `if Foo { ... } else { ... }`
// to be parsed as `if <Ident> { ... } else { ... }`
expr16:
  | expr16 "{" "}"
    { todo () }
  | expr17
    { $1 }

expr_record: brace(pair(seq(expr_field), tail(expr1(expr16))?)) { $1 }
expr_field:
  | name snd(":", expr1(expr16))?
    { Ast.FName ($1, $2) }
  | expr13(expr16) "." name
    { Ast.FExpr ($1, $3) }

dict(a, b): brace(seq_nonempty(pair(a, snd(":", b)))) { $1 }
set(a): brace(seq_nonempty(a)) { $1 }

ty_record: brace(pair(seq(ty_field), tail(ty0)?)) { $1 }
ty_field: pair(name, snd(":", ty0)?) { $1 }

pat_record: brace(pair(seq(pat_field), tail(pat0)?)) { $1 }
pat_field: pair(name, snd(":", pat0)?) { $1 }

expr_dyn_record: "dyn" brace(seq_nonempty(expr_dyn_record_item)) { $2 }
expr_dyn_record_item: expr17 snd(":", expr1(expr16)) { ($1, $2) }

expr17:
  | paren(expr1(expr16))
    { $1 }
  | lit
    { Ast.ELit (loc $loc, $1) }
  | path loption(qualify(seq(ty0)))
    { Ast.EPath (loc $loc, $1, $2) }
  | array(expr1(expr16))
    { Ast.EArray (loc $loc, fst $1, snd $1) }
  | tuple(expr1(expr16))
    { Ast.ETuple (loc $loc, $1) }
  | "_"
    { Ast.EAnon (loc $loc) }
  | expr18
    { $1 }

expr18:
  | "do" block
    { Ast.EBlock (loc $loc, $2) }
  | expr_record
    { Ast.ERecord (loc $loc, $1) }
  | "dict" dict(expr17, expr1(expr16))
    { Ast.EDict (loc $loc, $2) }
  | "set" set(expr1(expr16))
    { Ast.ESet (loc $loc, $2) }
  | expr_dyn_record
    { Ast.EDynRecord (loc $loc, $1) }
  | "if" expr2(expr17) block snd("else", block)?
    { Ast.EIf (loc $loc, $2, $3, $4) }
  | "if" "val" pat0 "=" expr2(expr17) block snd("else", block)?
    { Ast.EIfVal (loc $loc, $3, $5, $6, $7) }
  | "match" expr2(expr17) brace(arms)
    { Ast.EMatch (loc $loc, $2, $3) }
  | "loop" block
    { Ast.ELoop (loc $loc, $2) }
  | "while" expr2(expr17) block
    { Ast.EWhile (loc $loc, $2, $3) }
  | "while" "val" pat0 "=" expr2(expr17) block
    { Ast.EWhileVal (loc $loc, $3, $5, $6) }
  | "for" pat0 "in" expr2(expr17) block
    { Ast.EFor (loc $loc, $2, $4, $5) }
  | "try" block "catch" brace(arms) snd("finally", block)?
    { Ast.ETry (loc $loc, $2, $4, $5) }

sources: separated_nonempty_llist(",", source) { $1 }
source: pat0 source_kind expr1(expr17) { ($1, $2, $3) }
source_kind:
  | "in" { Ast.ScIn (loc $loc) }
  | "=" { Ast.ScEq (loc $loc) }

clauses: nonempty_llist(clause) { $1 }
clause:
  | "where" e=expr1(expr16)
    { Ast.SWhere {info=loc $loc; e} }
  | "join"
      args=seq(split(pat0, "in", expr1(expr17)))
      e=snd("on", expr1(expr16))
    { Ast.SJoin {info=loc $loc; args; e} }
  | "group"
      es=seq_nonempty(expr1(expr16))
      alias=snd("as", name)
    { Ast.SGroup {info=loc $loc; es; alias} }
  | "window"
      arg=expr1(expr16)
      alias=snd("as", name)
      "{"
        length=snd("length", expr1(expr16))
        repeat=snd("repeat", expr1(expr16))?
        aggrs=snd("compute", aggrs)
      "}"
  { Ast.SWindow {info=loc $loc; arg; alias; length; repeat; aggrs} }
  | "compute" aggrs=aggrs
    { Ast.SCompute {info=loc $loc; aggrs} }
  | "order" orders=seq_nonempty(pair(expr1(expr16), order))
    { Ast.SOrder {info=loc $loc; orders} }
  | "select" e=expr1(expr16)
    { Ast.SSelect {info=loc $loc; e} }
  | "into" e=expr1(expr16)
    { Ast.SInto {info=loc $loc; e} }

aggrs: seq_nonempty(aggr) { $1 }
aggr:
  func=expr1(expr16)
  arg=snd("of", expr1(expr16))?
  alias=snd("as", name)
  { (func, arg, alias) }

order:
  | epsilon { Ast.OAsc }
  | "desc" { Ast.ODesc }

qualify(x): "::" brack(x) { $2 }
tail(x): "|" x { $2 }

arms: seq_nonempty(arm) { $1 }
arm: pat0 "=>" expr1(expr16) { ($1, $3) }

block: brace(pair(stmts, expr1(expr16)?)) { $1 }

%inline stmts: llist(stmt) { $1 }
stmt:
  | ";"
    { Ast.SNoop (loc $loc) }
  | expr0(expr16) ";"
    { Ast.SExpr (loc $loc, $1) }
  | "val" param "=" expr0(expr16) ";"
    { Ast.SVal (loc $loc, $2, $4) }
  | "var" name ty_annot? "=" expr0(expr16) ";"
    { Ast.SVar (loc $loc, ($2, $3), $5) }

pat0:
  | pat0 "or" pat1 { Ast.POr (loc $loc, $1, $3) }
  | pat0 ":" ty0 { Ast.PAnnot (loc $loc, $1, $3) }
  | pat1 { $1 }
  
pat1:
  | const 
    { Ast.PConst (loc $loc, $1) }
  | name
    { Ast.PVar (loc $loc, $1) }
  | "case" name pat1
    { Ast.PVariant (loc $loc, $2, $3) }
  | tuple(pat0)
    { Ast.PTuple (loc $loc, $1) }
  | pat_record
    { Ast.PRecord (loc $loc, $1) }
  | array(pat0)
    { Ast.PArray (loc $loc, fst $1, snd $1) }
  | "_"
    { Ast.PIgnore (loc $loc) }

ty0:
  | "fun" paren_seq(ty0) ":" ty0
    { Ast.TFunc (loc $loc, $2, $4) }
  | ty1
    { $1 }

ty1:
  | path loption(brack(seq_nonempty(ty0)))
    { Ast.TPath (loc $loc, $1, $2) }
  | tuple(ty0)
    { Ast.TTuple (loc $loc, $1) }
  | ty_record
    { Ast.TRecord (loc $loc, $1) }
  | enum(ty0)
    { Ast.TEnum (loc $loc, $1) }
  | brack(ty0)
    { Ast.TArray (loc $loc, $1) }
  | "()"
    { Ast.TUnit (loc $loc) }
  | "!"
    { Ast.TNever (loc $loc) }

array(x): brack(pair(seq(x), tail(x)?)) { $1 }
tuple(x): paren(seq_explicit(x)) { $1 }
enum(x): angle(pair(seq(variant(ty0)), tail(x)?)) { $1 }
variant(x): name x { ($1, $2) }

lit:
  | Bool
    { Ast.LBool (loc $loc, $1) }
  | Char 
    { Ast.LChar (loc $loc, $1) }
  | Int 
    { Ast.LInt (loc $loc, $1, None) }
  | IntSuffix 
    { Ast.LInt (loc $loc, fst $1, Some (snd $1)) }
  | Float 
    { Ast.LFloat (loc $loc, $1, None) }
  | FloatSuffix 
    { Ast.LFloat (loc $loc, fst $1, Some (snd $1)) }
  | "()" 
    { Ast.LUnit (loc $loc) }
  | String 
    { Ast.LString (loc $loc, $1) }

const:
  | Bool 
    { Ast.CBool (loc $loc, $1) }
  | Char 
    { Ast.CChar (loc $loc, $1) }
  | Int 
    { Ast.CInt (loc $loc, $1) }
  | Float 
    { Ast.CFloat (loc $loc, $1) }
  | "()" 
    { Ast.CUnit (loc $loc) }
  | String 
    { Ast.CString (loc $loc, $1) }
