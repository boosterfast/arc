use pretty::Pretty;
use std::io;
use std::io::Write;

use crate::*;

use im_rc::Vector;

use crate::Item;

pub fn pr_ast(ctx: &mut Pretty<impl Write>, is: &Vector<Item>) -> io::Result<()> {
    for i in is {
        pr_item(ctx, i)?;
    }
    Ok(())
}

pub fn pr_item(ctx: &mut Pretty<impl Write>, item: &Item) -> io::Result<()> {
    match item.kind.as_ref() {
        IFrom(d, ss, qs) => {
            pr_decorator(ctx, d)?;
            pr_query(ctx, ss, qs)?;
        }
        IAbstractDef(d, a, x, gs, ts, t, efs, bs) => {
            pr_decorator(ctx, d)?;
            pr_abstract(ctx, a)?;
            ctx.lit("def ")?;
            pr_defname(ctx, x)?;
            pr_generics(ctx, gs)?;
            ctx.paren(|ctx| ctx.seq(ts, |ctx, t| pr_type(ctx, t)))?;
            ctx.opt(t, |ctx, t| {
                ctx.lit(" : ")?;
                pr_type(ctx, t)
            })?;
            pr_effects(ctx, efs)?;
            pr_bounds(ctx, bs)?;
        }
        IDef(d, x, gs, ps, t, bs, b) => {
            pr_decorator(ctx, d)?;
            ctx.lit("def ")?;
            pr_defname(ctx, x)?;
            pr_generics(ctx, gs)?;
            pr_patterns(ctx, ps)?;
            ctx.opt(t, |ctx, t| pr_type(ctx, t))?;
            pr_bounds(ctx, bs)?;
            ctx.lit(" ")?;
            pr_block(ctx, b)?;
        }
        IVal(d, x, t, e) => {
            pr_decorator(ctx, d)?;
            ctx.lit("val ")?;
            ctx.lit(x)?;
            ctx.opt(t, |ctx, t| pr_type(ctx, t))?;
            ctx.lit(" = ")?;
            pr_expr(ctx, e)?;
        }
        IAbstractType(d, a, x, gs, bs) => {
            pr_decorator(ctx, d)?;
            pr_abstract(ctx, a)?;
            ctx.lit("type ")?;
            ctx.lit(x)?;
            pr_generics(ctx, gs)?;
            pr_bounds(ctx, bs)?;
        }
        IClass(_d, _x, _gs, _bs, _decls) => todo!(),
        IInstance(_d, _gs, _xs, _ts, _bs, _defs) => todo!(),
        IMod(d, x, items) => {
            pr_decorator(ctx, d)?;
            ctx.lit("mod ")?;
            ctx.lit(x)?;
            ctx.lit(" {")?;
            ctx.indent();
            pr_ast(ctx, items)?;
            ctx.dedent();
            ctx.newline()?;
            ctx.lit("}")?;
        }
        IType(d, x, gs, t, bs) => {
            pr_decorator(ctx, d)?;
            ctx.lit("type ")?;
            ctx.lit(x)?;
            pr_generics(ctx, gs)?;
            pr_type(ctx, t)?;
            pr_bounds(ctx, bs)?;
        }
        IUse(d, x, suffix) => {
            pr_decorator(ctx, d)?;
            ctx.lit("use ")?;
            pr_path(ctx, x)?;
            if let Some(suffix) = suffix {
                match suffix {
                    UAlias(x) => {
                        ctx.lit(" as ")?;
                        ctx.lit(x)?;
                    }
                    UGlob => {
                        ctx.lit("::*")?;
                    }
                }
            }
        }
    }
    ctx.newline()?;
    Ok(())
}

pub fn pr_patterns(ctx: &mut Pretty<impl Write>, ps: &Vector<Pattern>) -> io::Result<()> {
    ctx.paren(|ctx| ctx.seq(ps, |ctx, p| pr_pat(ctx, p)))?;
    Ok(())
}

pub fn pr_effects(ctx: &mut Pretty<impl Write>, efs: &Vector<Effect>) -> io::Result<()> {
    if !efs.is_empty() {
        ctx.lit(" ~ ")?;
        ctx.brace(|ctx| ctx.seq(efs, |ctx, x| ctx.lit(x)))?;
    }
    Ok(())
}

pub fn pr_bounds(ctx: &mut Pretty<impl Write>, bs: &Vector<Bound>) -> io::Result<()> {
    if !bs.is_empty() {
        ctx.lit(" where ")?;
        ctx.lit(" : ")?;
        ctx.seq(bs, |ctx, (xs, ts)| {
            pr_path(ctx, xs)?;
            pr_types(ctx, ts)
        })?;
    }
    Ok(())
}

pub fn pr_abstract(ctx: &mut Pretty<impl Write>, a: &Abstract) -> io::Result<()> {
    match a {
        AExtern => ctx.lit("extern "),
        ABuiltin => ctx.lit("builtin "),
    }
}

pub fn pr_defname(ctx: &mut Pretty<impl Write>, x: &DefName) -> io::Result<()> {
    match x {
        DName(x) => ctx.lit(x),
        DUnop(unop) => pr_unop(ctx, unop),
        DBinop(binop) => pr_binop(ctx, binop),
    }
}

pub fn pr_generics(ctx: &mut Pretty<impl Write>, gs: &Vector<Generic>) -> io::Result<()> {
    if !gs.is_empty() {
        ctx.brack(|ctx| ctx.seq(gs, |ctx, x| ctx.lit(x)))?;
    }
    Ok(())
}

pub fn pr_types(ctx: &mut Pretty<impl Write>, ts: &Vector<Type>) -> io::Result<()> {
    if !ts.is_empty() {
        ctx.brack(|ctx| ctx.seq(ts, |ctx, t| pr_type(ctx, t)))?;
    }
    Ok(())
}

pub fn pr_source(ctx: &mut Pretty<impl Write>, (p, sk, e): &Source) -> io::Result<()> {
    pr_pat(ctx, p)?;
    ctx.lit(" ")?;
    pr_sourcekind(ctx, sk)?;
    ctx.lit(" ")?;
    pr_expr(ctx, e)
}

pub fn pr_sourcekind(ctx: &mut Pretty<impl Write>, sk: &SourceKind) -> io::Result<()> {
    match sk {
        ScIn => ctx.lit(" in "),
        ScEq => ctx.lit(" = "),
    }
}

pub fn pr_decorator(ctx: &mut Pretty<impl Write>, d: &Decorator) -> io::Result<()> {
    if !d.is_empty() {
        ctx.lit("@")?;
        ctx.brace(|ctx| ctx.seq(d, |ctx, a| pr_attr(ctx, a)))?;
        ctx.newline()?;
    }
    Ok(())
}

pub fn pr_attr(ctx: &mut Pretty<impl Write>, (x, c): &Attribute) -> io::Result<()> {
    ctx.lit(x)?;
    if let Some(c) = c {
        ctx.lit(":")?;
        pr_const(ctx, c)?;
    }
    Ok(())
}

pub fn pr_pat(ctx: &mut Pretty<impl Write>, p: &Pattern) -> io::Result<()> {
    match p.kind.as_ref() {
        PIgnore => {
            ctx.lit("_")?;
        }
        POr(p0, p1) => {
            pr_pat(ctx, p0)?;
            ctx.lit(" or ")?;
            pr_pat(ctx, p1)?;
        }
        PAnnot(p, t) => {
            pr_pat(ctx, p)?;
            ctx.lit(": ")?;
            pr_type(ctx, t)?;
        }
        PRecord(xps, p) => {
            ctx.brace(|ctx| {
                for (x, p) in xps {
                    ctx.lit(x)?;
                    if let Some(p) = p {
                        ctx.lit(": ")?;
                        pr_pat(ctx, p)?;
                    }
                }
                if let Some(p) = p {
                    ctx.lit("|")?;
                    pr_pat(ctx, p)?;
                }
                Ok(())
            })?;
        }
        PTuple(ps) => {
            ctx.paren(|ctx| ctx.seq(ps, |ctx, p| pr_pat(ctx, p)))?;
        }
        PArray(ps, p) => {
            ctx.brack(|ctx| {
                for p in ps {
                    pr_pat(ctx, p)?;
                }
                if let Some(p) = p {
                    ctx.lit("|")?;
                    pr_pat(ctx, p)?;
                }
                Ok(())
            })?;
        }
        PConst(c) => {
            pr_const(ctx, c)?;
        }
        PVar(x) => {
            ctx.lit(x)?;
        }
        PVariant(x, p) => {
            ctx.lit("case ")?;
            ctx.lit(x)?;
            pr_pat(ctx, p)?;
        }
        PError => {
            ctx.lit("<error>")?;
        }
    };
    Ok(())
}

pub fn pr_const(ctx: &mut Pretty<impl Write>, c: &Const) -> io::Result<()> {
    match c {
        CInt(i) => ctx.fmt(format_args!("{i}")),
        CFloat(f) => ctx.fmt(format_args!("{f}")),
        CString(s) => ctx.fmt(format_args!(r#""{s}""#)),
        CBool(b) => ctx.fmt(format_args!("{b}")),
        CUnit => ctx.lit("()"),
        CChar(c) => ctx.fmt(format_args!("'{c}'")),
    }
}

pub fn pr_lit(ctx: &mut Pretty<impl Write>, c: &Lit) -> io::Result<()> {
    match c {
        LInt(i, s) => {
            ctx.fmt(format_args!("{i}"))?;
            if let Some(s) = s {
                ctx.lit(s)?;
            }
            Ok(())
        }
        LFloat(f, s) => {
            ctx.fmt(format_args!("{f}"))?;
            if let Some(s) = s {
                ctx.lit(s)?;
            }
            Ok(())
        }
        LString(s) => ctx.fmt(format_args!(r#""{s}""#)),
        LBool(b) => ctx.fmt(format_args!("{b}")),
        LUnit => ctx.lit("()"),
        LChar(c) => ctx.fmt(format_args!("'{c}'")),
    }
}

pub fn pr_expr(ctx: &mut Pretty<impl Write>, e: &Expr) -> io::Result<()> {
    match e.kind.as_ref() {
        EAccessRecord(e, x) => {
            pr_expr(ctx, e)?;
            ctx.lit(".")?;
            ctx.lit(x)?;
        }
        EAccessRecordMulti(e, xs) => {
            pr_expr(ctx, e)?;
            ctx.lit(".")?;
            ctx.brace(|ctx| {
                ctx.seq(xs, |ctx, x| ctx.lit(x))?;
                Ok(())
            })?;
        }
        ECall(e, es) => {
            pr_expr(ctx, e)?;
            ctx.paren(|ctx| ctx.seq(es, |ctx, e| pr_expr(ctx, e)))?;
        }
        EAnnot(e, t) => {
            pr_expr(ctx, e)?;
            ctx.lit(": ")?;
            pr_type(ctx, t)?;
        }
        EIf(e, b0, b1) => {
            ctx.lit("if ")?;
            pr_expr(ctx, e)?;
            ctx.lit(" ")?;
            pr_block(ctx, b0)?;
            if let Some(b1) = b1 {
                ctx.lit(" else ")?;
                pr_block(ctx, b1)?;
            }
        }
        ELit(l) => {
            pr_lit(ctx, l)?;
        }
        ELoop(b) => {
            ctx.lit("loop ")?;
            pr_block(ctx, b)?;
        }
        ERecord(xes, e) => {
            ctx.brace(|ctx| {
                for xe in xes {
                    match xe {
                        FName(x, e) => {
                            ctx.lit(x)?;
                            ctx.lit(": ")?;
                            if let Some(e) = e {
                                pr_expr(ctx, e)?;
                            }
                        }
                        FExpr(e, x) => {
                            pr_expr(ctx, e)?;
                            ctx.lit(" as ")?;
                            ctx.lit(x)?;
                        }
                    }
                }
                if let Some(e) = e {
                    ctx.lit("|")?;
                    pr_expr(ctx, e)?;
                }
                Ok(())
            })?;
        }
        EDynRecord(ees) => {
            ctx.lit("dyn ")?;
            ctx.brace(|ctx| {
                for (e0, e1) in ees {
                    pr_expr(ctx, e0)?;
                    ctx.lit(": ")?;
                    pr_expr(ctx, e1)?;
                }
                Ok(())
            })?;
        }
        EVariant(x, e) => {
            ctx.lit("new ")?;
            ctx.lit(x)?;
            ctx.lit(" ")?;
            pr_expr(ctx, e)?;
        }
        EReturn(e) => {
            ctx.lit("return")?;
            if let Some(e) = e {
                ctx.lit(" ")?;
                pr_expr(ctx, e)?;
            }
        }
        EBreak(e) => {
            ctx.lit("break")?;
            if let Some(e) = e {
                ctx.lit(" ")?;
                pr_expr(ctx, e)?;
            }
        }
        EContinue => todo!(),
        EDict(ees) => {
            ctx.lit("dict ")?;
            ctx.brace(|ctx| {
                for (e0, e1) in ees {
                    pr_expr(ctx, e0)?;
                    ctx.lit(": ")?;
                    pr_expr(ctx, e1)?;
                }
                Ok(())
            })?;
        }
        ESet(es) => {
            ctx.lit("set ")?;
            ctx.brace(|ctx| ctx.seq(es, |ctx, e| pr_expr(ctx, e)))?;
        }
        EUnop(op, e) => {
            pr_unop(ctx, op)?;
            pr_expr(ctx, e)?;
        }
        EArray(es, e) => {
            ctx.brack(|ctx| {
                for e in es {
                    pr_expr(ctx, e)?;
                }
                if let Some(e) = e {
                    ctx.lit("|")?;
                    pr_expr(ctx, e)?;
                }
                Ok(())
            })?;
        }
        EBinop(e0, op, e1) => {
            pr_expr(ctx, e0)?;
            ctx.lit(" ")?;
            pr_binop(ctx, op)?;
            ctx.lit(" ")?;
            pr_expr(ctx, e1)?;
        }
        EDo(b) => {
            ctx.lit("do ")?;
            pr_block(ctx, b)?;
        }
        EFor(p, e, b) => {
            ctx.lit("for ")?;
            pr_pat(ctx, p)?;
            ctx.lit(" in ")?;
            pr_expr(ctx, e)?;
            ctx.lit(" ")?;
            pr_block(ctx, b)?;
        }
        EFunc(ps, t, b) => {
            ctx.lit("fun")?;
            ctx.paren(|ctx| ctx.seq(ps, |ctx, p| pr_pat(ctx, p)))?;
            ctx.opt(t, |ctx, t| {
                ctx.lit(": ")?;
                pr_type(ctx, t)
            })?;
            ctx.lit(" ")?;
            pr_block(ctx, b)?;
        }
        EIfVal(p, e, b0, b1) => {
            ctx.lit("if val ")?;
            pr_pat(ctx, p)?;
            ctx.lit(" = ")?;
            pr_expr(ctx, e)?;
            ctx.lit(" ")?;
            pr_block(ctx, b0)?;
            if let Some(b1) = b1 {
                ctx.lit(" else ")?;
                pr_block(ctx, b1)?;
            }
        }
        ECallItem(e, x, es) => {
            pr_expr(ctx, e)?;
            ctx.lit(".")?;
            ctx.lit(x)?;
            ctx.paren(|ctx| ctx.seq(es, |ctx, e| pr_expr(ctx, e)))?;
        }
        EMatch(e, arms) => {
            ctx.lit("match ")?;
            pr_expr(ctx, e)?;
            ctx.lit(" ")?;
            ctx.brace(|ctx| pr_arms(ctx, arms))?;
        }
        EPath(xs, ts) => {
            pr_path(ctx, xs)?;
            if !ts.is_empty() {
                ctx.lit("::")?;
                pr_types(ctx, ts)?;
            }
        }
        EAccessTuple(e, i) => {
            pr_expr(ctx, e)?;
            ctx.lit(".")?;
            ctx.fmt(format_args!("{i}"))?;
        }
        EAccessArray(e, es) => {
            pr_expr(ctx, e)?;
            ctx.brack(|ctx| ctx.seq(es, |ctx, e| pr_expr(ctx, e)))?;
        }
        EThrow(e) => {
            ctx.lit("throw ")?;
            pr_expr(ctx, e)?;
        }
        ETry(b0, arms, b1) => {
            ctx.lit("try ")?;
            pr_block(ctx, b0)?;
            pr_arms(ctx, arms)?;
            if let Some(b1) = b1 {
                ctx.lit(" finally ")?;
                pr_block(ctx, b1)?;
            }
        }
        ETuple(es) => {
            ctx.paren(|ctx| ctx.seq(es, |ctx, e| pr_expr(ctx, e)))?;
        }
        EFrom(ss, qs) => {
            pr_query(ctx, ss, qs)?;
        }
        EAnon => todo!(),
        EWhile(_, _) => todo!(),
        EWhileVal(_, _, _) => todo!(),
        EError => todo!(),
    }
    Ok(())
}

pub fn pr_arms(ctx: &mut Pretty<impl Write>, arms: &Vector<Arm>) -> io::Result<()> {
    for (p, e) in arms {
        pr_pat(ctx, p)?;
        ctx.lit(" => ")?;
        pr_expr(ctx, e)?;
    }
    Ok(())
}

pub fn pr_query(
    ctx: &mut Pretty<impl Write>,
    ss: &Vector<Source>,
    qs: &Vector<QueryStmt>,
) -> io::Result<()> {
    ctx.lit("from ")?;
    ctx.brace(|ctx| {
        ctx.seq(ss, |ctx, s| pr_source(ctx, s))?;
        for q in qs {
            ctx.newline()?;
            pr_query_stmt(ctx, q)?;
        }
        Ok(())
    })
}

pub fn pr_query_stmt(ctx: &mut Pretty<impl Write>, s: &QueryStmt) -> io::Result<()> {
    match s {
        SWhere { e } => {
            ctx.lit("where ")?;
            pr_expr(ctx, e)?;
        }
        SJoin { args, e } => {
            ctx.lit("join ")?;
            ctx.seq(args, |ctx, (p, e)| {
                pr_pat(ctx, p)?;
                ctx.lit(" in ")?;
                pr_expr(ctx, e)
            })?;
            pr_expr(ctx, e)?;
            ctx.lit(" on ")?;
            pr_expr(ctx, e)?;
        }
        SGroup { es, alias } => {
            ctx.lit("group ")?;
            for e in es {
                pr_expr(ctx, e)?;
            }
            ctx.lit(" as ")?;
            ctx.lit(alias)?;
        }
        SWindow {
            arg,
            length,
            alias,
            repeat,
            aggrs,
        } => {
            ctx.lit("window ")?;
            pr_expr(ctx, arg)?;
            ctx.lit(" length ")?;
            pr_expr(ctx, length)?;
            ctx.lit(" as ")?;
            ctx.lit(alias)?;
            ctx.lit(" repeat ")?;
            if let Some(repeat) = repeat {
                pr_expr(ctx, repeat)?;
            }
            ctx.indent();
            ctx.brace(|ctx| {
                ctx.lit(" compute ")?;
                ctx.seq(aggrs, |ctx, a| pr_aggr(ctx, a))?;
                Ok(())
            })?;
            ctx.dedent();
        }
        SCompute { aggrs } => {
            ctx.lit("compute ")?;
            for a in aggrs {
                pr_aggr(ctx, a)?;
            }
        }
        SOrder { orders } => {
            ctx.lit("order ")?;
            for (e, o) in orders {
                pr_expr(ctx, e)?;
                match o {
                    OAsc => ctx.lit(" asc")?,
                    ODesc => ctx.lit(" desc")?,
                }
            }
        }
        SSelect { e } => {
            ctx.lit("select ")?;
            pr_expr(ctx, e)?;
        }
        SInto { e } => {
            ctx.lit("into ")?;
            pr_expr(ctx, e)?;
        }
    }
    Ok(())
}

pub fn pr_aggr(ctx: &mut Pretty<impl Write>, (f, e, x): &Aggr) -> io::Result<()> {
    pr_expr(ctx, f)?;
    if let Some(e) = e {
        ctx.lit(" of ")?;
        pr_expr(ctx, e)?;
    }
    ctx.lit(" as ")?;
    ctx.lit(x)?;
    Ok(())
}

pub fn pr_block(ctx: &mut Pretty<impl Write>, (ss, e): &Block) -> io::Result<()> {
    ctx.brace(|ctx| {
        ctx.indent();
        for s in ss {
            ctx.newline()?;
            pr_stmt(ctx, s)?;
        }
        if let Some(e) = e {
            ctx.newline()?;
            pr_expr(ctx, e)?;
        }
        ctx.dedent();
        ctx.newline()
    })?;
    Ok(())
}

pub fn pr_stmt(ctx: &mut Pretty<impl Write>, s: &Stmt) -> io::Result<()> {
    match s {
        SNoop => ctx.lit(";"),
        SVal(p, e) => {
            ctx.lit("val ")?;
            pr_pat(ctx, p)?;
            ctx.lit(" = ")?;
            pr_expr(ctx, e)?;
            ctx.lit(";")
        }
        SVar(x, t, e) => {
            ctx.lit("var ")?;
            ctx.lit(x)?;
            if let Some(t) = t {
                ctx.lit(": ")?;
                pr_type(ctx, t)?;
            }
            ctx.lit(" = ")?;
            pr_expr(ctx, e)?;
            ctx.lit(";")
        }
        SExpr(e) => {
            pr_expr(ctx, e)?;
            ctx.lit(";")
        }
    }
}

pub fn pr_type(ctx: &mut Pretty<impl Write>, t: &Type) -> io::Result<()> {
    match t.kind.as_ref() {
        TFunc(ts, t) => {
            ctx.lit("fun")?;
            ctx.paren(|ctx| ctx.seq(ts, |ctx, t| pr_type(ctx, t)))?;
            ctx.lit(":")?;
            pr_type(ctx, t)?;
        }
        TTuple(ts) => {
            ctx.paren(|ctx| ctx.seq(ts, |ctx, t| pr_type(ctx, t)))?;
        }
        TEnum(xts, t) => {
            ctx.angle(|ctx| {
                ctx.seq(xts, |ctx, (x, t)| {
                    ctx.lit(x)?;
                    ctx.lit(": ")?;
                    pr_type(ctx, t)?;
                    Ok(())
                })?;
                if let Some(t) = t {
                    ctx.lit("|")?;
                    pr_type(ctx, t)?;
                }
                Ok(())
            })?;
        }
        TRecord(xts, t) => {
            ctx.brace(|ctx| {
                ctx.seq(xts, |ctx, (x, t)| {
                    ctx.lit(x)?;
                    if let Some(t) = t {
                        ctx.lit(": ")?;
                        pr_type(ctx, t)?;
                    }
                    Ok(())
                })?;
                if let Some(t) = t {
                    ctx.lit("|")?;
                    pr_type(ctx, t)?;
                }
                Ok(())
            })?;
        }
        TPath(xs, ts) => {
            pr_path(ctx, xs)?;
            if !ts.is_empty() {
                ctx.brack(|ctx| ctx.seq(ts, |ctx, t| pr_type(ctx, t)))?;
            }
        }
        TArray(t) => {
            ctx.brack(|ctx| pr_type(ctx, t))?;
        }
        TUnit => {
            ctx.lit("()")?;
        }
        TNever => {
            ctx.lit("!")?;
        }
        TError => {
            ctx.lit("<Error>")?;
        }
    }
    Ok(())
}

pub fn pr_path(ctx: &mut Pretty<impl Write>, xs: &Path) -> io::Result<()> {
    match xs {
        PAbs(xs) => {
            ctx.lit("::")?;
            ctx.sep(xs, "::", |ctx, x| ctx.lit(x))?;
        }
        PRel(xs) => {
            ctx.sep(xs, "::", |ctx, x| ctx.lit(x))?;
        }
    }
    Ok(())
}

pub fn pr_unop(ctx: &mut Pretty<impl Write>, op: &Unop) -> io::Result<()> {
    match op {
        UNot => ctx.lit("!"),
        UNeg => ctx.lit("-"),
    }
}

pub fn pr_binop(ctx: &mut Pretty<impl Write>, op: &Binop) -> io::Result<()> {
    match op {
        BAdd => ctx.lit("+"),
        BAnd => ctx.lit(" and "),
        BBand => ctx.lit(" band "),
        BBor => ctx.lit(" bor "),
        BBxor => ctx.lit(" bxor "),
        BDiv => ctx.lit("/"),
        BEq => ctx.lit("="),
        BGeq => ctx.lit(">="),
        BGt => ctx.lit(">"),
        BLeq => ctx.lit("<="),
        BLt => ctx.lit("<"),
        BMod => ctx.lit("%"),
        BMul => ctx.lit("*"),
        BMut => ctx.lit(" = "),
        BNeq => ctx.lit(" != "),
        BOr => ctx.lit(" or "),
        BPow => ctx.lit("**"),
        BSub => ctx.lit("-"),
        BXor => ctx.lit(" bxor "),
        BIn => ctx.lit(" in "),
        BRExc => ctx.lit(" .. "),
        BRInc => ctx.lit(" ..= "),
        BNotIn => ctx.lit(" not in "),
        BMutAdd => ctx.lit(" += "),
        BMutSub => ctx.lit(" -= "),
        BMutMul => ctx.lit(" *= "),
        BMutDiv => ctx.lit(" /= "),
        BMutMod => ctx.lit(" %= "),
    }
}
