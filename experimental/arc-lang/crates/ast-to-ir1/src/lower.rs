use ast::ExprField;
use im_rc::vector;
use info::Info;
use ir1::*;
use std::collections::HashMap;
use utils::OptionUtils;
use utils::VectorUtils;

use im_rc::Vector;

use crate::context::Context;
use crate::context::MVal;
use crate::context::MVar;

pub fn ast_to_ir1(ast: Vector<ast::Item>) -> HashMap<Path, Item> {
    let declarations = declare::declare_ast(ast.clone());
    let ctx = Context::new(declarations);
    let ctx = ast.into_iter().fold(ctx, lower_item);
    ctx.ir1
}

fn lower_item(ctx: Context, item: ast::Item) -> Context {
    match item.kind() {
        ast::IFrom(_, _, _) => {
            todo!()
        }
        ast::IAbstractDef(d, a, x, gs, ts, t, efs, bs) => {
            let x = x.name();
            let xs = ctx.item_path(x);
            let ctx = ctx.push_gscope();
            let (ctx, d) = d.mapm(ctx, lower_attribute);
            let (ctx, gs) = gs.mapm(ctx, lower_generic);
            let (ctx, ts) = ts.mapm(ctx, lower_type);
            let (ctx, t) = lower_type_or_unit(ctx, t);
            let (ctx, bs) = bs.mapm(ctx, lower_bound);
            let ctx = ctx.pop_gscope();
            let (ctx, a) = lower_abstract(ctx, a);
            let ctx = ctx.add_item(xs, IAbstractDef(d, a, gs, ts, t, efs, bs).with(item.info));
            ctx
        }
        ast::IDef(d, x, gs, ps, t, bs, b) => {
            let x = x.name();
            let xs = ctx.item_path(x);
            let ctx = ctx.push_gscope();
            let ctx = ctx.push_vscope();
            let (ctx, d) = d.mapm(ctx, lower_attribute);
            let (ctx, gs) = gs.mapm(ctx, lower_generic);
            let (ctx, ps) = ps.mapm(ctx, lower_pat);
            let (ctx, t) = lower_type_or_unit(ctx, t);
            let (ctx, bs) = bs.mapm(ctx, lower_bound);
            let (ctx, (ss0, e)) = lower_block(ctx, b);
            let (ctx, mut ss1) = ctx.pop_vscope();
            let ctx = ctx.pop_gscope();
            ss1.append(ss0);
            let b = (ss1, e);

            if ps.is_empty() {
                ctx.add_item(xs, IDef(d, gs, vector![], t, bs, b).with(item.info))
            } else {
                let (ctx, vs) = ps.clone().mapm(ctx, |ctx, _| ctx.fresh_x());
                let ts = ps.iter().map(|p| p.ty()).collect::<Vector<_>>();
                let xts = vs.clone().into_iter().zip(ts.into_iter()).collect();
                let xes = seq_to_fields(vs.into_iter().map(var_to_expr));
                let xps = seq_to_fields(ps.into_iter());
                let (ctx, e) = ctx.typed(|t| ERecord(t, xes, None).with(item.info));
                let (ctx, p) = ctx.typed(|t| PRecord(t, xps, None).with(item.info));
                let (ctx, e) = ctx.typed(|t| EMatch(t, e, vector![(p, b)]).with(item.info));
                ctx.add_item(xs, IDef(d, gs, xts, t, bs, (vector![], e)).with(item.info))
            }
        }
        ast::IVal(_, _, _, _) => todo!(),
        ast::IAbstractType(d, a, x, gs, bs) => {
            let xs = ctx.item_path(x);
            let ctx = ctx.push_gscope();
            let (ctx, d) = d.mapm(ctx, lower_attribute);
            let (ctx, a) = lower_abstract(ctx, a);
            let (ctx, gs) = gs.mapm(ctx, lower_generic);
            let (ctx, bs) = bs.mapm(ctx, lower_bound);
            let ctx = ctx.pop_gscope();
            let ctx = ctx.add_item(xs, IAbstractType(d, a, gs, bs).with(item.info));
            ctx
        }
        ast::IClass(_, _, _, _, _) => todo!(),
        ast::IInstance(_, _, _, _, _, _) => todo!(),
        ast::IMod(_, x, is) => {
            let ctx = ctx.push_namespace(x);
            let ctx = is.into_iter().fold(ctx, lower_item);
            let ctx = ctx.pop_namespace();
            ctx
        }
        ast::IType(d, x, gs, t, bs) => {
            let xs = ctx.item_path(x);
            let ctx = ctx.push_gscope();
            let (ctx, d) = d.mapm(ctx, lower_attribute);
            let (ctx, gs) = gs.mapm(ctx, lower_generic);
            let (ctx, t) = lower_type(ctx, t);
            let (ctx, bs) = bs.mapm(ctx, lower_bound);
            let ctx = ctx.pop_gscope();
            let ctx = ctx.add_item(xs, IType(d, gs, t, bs).with(item.info));
            ctx
        }
        ast::IUse(..) => ctx,
    }
}

fn lower_abstract(ctx: Context, a: ast::Abstract) -> (Context, Abstract) {
    match a {
        ast::AExtern => (ctx, AExtern),
        ast::ABuiltin => (ctx, ABuiltin),
    }
}

fn lower_attribute(ctx: Context, (x, c): ast::Attribute) -> (Context, Attribute) {
    let (ctx, c) = c.mapm(ctx, lower_const);
    (ctx, (x, c))
}

fn lower_generic(ctx: Context, x: ast::Generic) -> (Context, Name) {
    ctx.bind_gname(x)
}

fn lower_bound(ctx: Context, (xs, ts): ast::Bound) -> (Context, Bound) {
    let (ctx, ts) = ts.mapm(ctx, lower_type);
    if let Some((xs, item)) = ctx.resolve_path(xs) {
        if let ast::IClass(_, _, _, _, _) = item.kind() {
            (ctx, (xs, ts))
        } else {
            panic!("Bound path is not a class")
        }
    } else {
        panic!("Unbound path")
    }
}

fn lower_type(ctx: Context, t: ast::Type) -> (Context, Type) {
    match (*t.kind).clone() {
        ast::TFunc(ts, t) => {
            let (ctx, ts) = ts.mapm(ctx, lower_type);
            let (ctx, t) = lower_type(ctx, t);
            (ctx, TFunc(ts, t).into())
        }
        ast::TTuple(ts) => {
            let (ctx, ts) = ts.mapm(ctx, lower_type);
            (ctx, types_to_record(ts))
        }
        ast::TEnum(xts, t) => {
            let (ctx, xts) = xts.mapm(ctx, |ctx, (x, t)| {
                let (ctx, t) = lower_type(ctx, t);
                (ctx, (x, t))
            });
            let (ctx, t) = t.mapm_or_else(ctx, lower_type, |ctx| (ctx, TRowEmpty.into()));
            let t = fields_to_rows(xts, t);
            (ctx, TEnum(t).into())
        }
        ast::TRecord(xts, t) => {
            let (ctx, xts) = xts.mapm(ctx, |ctx, (x, t)| {
                let (ctx, t) = lower_type_or_fresh(ctx, t);
                (ctx, (x, t))
            });
            let (ctx, t) = match t {
                Some(t) => {
                    let (ctx, t) = lower_type(ctx, t.clone());
                    (ctx, t)
                }
                None => (ctx, TRowEmpty.into()),
            };
            let t = fields_to_rows(xts, t);
            (ctx, TRecord(t).into())
        }
        ast::TPath(xs, ts) => match &xs {
            ast::PRel(xs1) if xs1.len() == 1 && ts.is_empty() => {
                if let Some(x) = ctx.find_gname(&xs1.back().unwrap()) {
                    (ctx, TGeneric(x.clone()).into())
                } else {
                    lower_type_item_path(ctx, xs, ts)
                }
            }
            _ => lower_type_item_path(ctx, xs, ts),
        },
        ast::TArray(t) => {
            let (ctx, t) = lower_type(ctx, t);
            (ctx, TNominal(std("Array"), vector![t]).into())
        }
        ast::TUnit => (ctx, TNominal(std("Unit"), vector![]).into()),
        ast::TNever => (ctx, TNominal(std("Never"), vector![]).into()),
        ast::TError => panic!(),
    }
}

fn lower_pat(ctx: Context, p: ast::Pattern) -> (Context, Pattern) {
    match (*p.kind).clone() {
        ast::PIgnore => {
            let (ctx, t) = ctx.fresh_t();
            (ctx, PIgnore(t).into())
        }
        ast::POr(p0, p1) => {
            let (ctx, p0) = lower_pat(ctx, p0);
            let (ctx, p1) = lower_pat(ctx, p1);
            let (ctx, t) = ctx.fresh_t();
            (ctx, POr(t, p0, p1).into())
        }
        ast::PAnnot(p, t) => {
            let (ctx, p) = lower_pat(ctx, p);
            let (ctx, t) = lower_type(ctx, t);
            (ctx, PAnnot(t, p).into())
        }
        ast::PRecord(xps, p) => {
            let (ctx, xps) = xps.mapm(ctx, |ctx, (x, p)| match p {
                Some(p) => {
                    let (ctx, p) = lower_pat(ctx, p);
                    (ctx, (x, p))
                }
                None => {
                    let (ctx, x) = ctx.bind_vname(x, MVal);
                    let (ctx, p) = ctx.typed(|t| PVar(t, x.clone()).into());
                    (ctx, (x, p))
                }
            });
            let (ctx, t) = ctx.fresh_t();
            let (ctx, p) = p.mapm(ctx, lower_pat);
            (ctx, PRecord(t, xps, p).into())
        }
        ast::PTuple(ps) => {
            let xps = seq_to_fields(ps.into_iter().map(Some));
            let p = ast::Pattern {
                kind: ast::PRecord(xps, None).into(),
                info: p.info,
            };
            lower_pat(ctx, p)
        }
        ast::PArray(_, _) => todo!(),
        ast::PConst(c) => {
            let (ctx, l) = lower_const(ctx, c);
            let (ctx, t) = ctx.fresh_t();
            (ctx, PConst(t, l).into())
        }
        ast::PVar(x) => {
            let (ctx, x) = ctx.bind_vname(x, MVal);
            let (ctx, t) = ctx.fresh_t();
            (ctx, PVar(t, x).into())
        }
        ast::PVariant(x, p) => {
            let (ctx, p) = lower_pat(ctx, p);
            let (ctx, t) = ctx.fresh_t();
            (ctx, PVariant(t, x, p).into())
        }
        ast::PError => panic!(),
    }
}

fn lower_block_rec(
    ctx: Context,
    mut ss: Vector<ast::Stmt>,
    e: Option<ast::Expr>,
) -> (Context, Expr) {
    match ss.pop_front() {
        Some(s) => match s {
            ast::SNoop => lower_block_rec(ctx, ss, e),
            ast::SVal(p, e0) => {
                let (ctx, e0) = lower_expr(ctx, e0);
                let (ctx, p) = lower_pat(ctx, p);
                let ctx = ctx.push_vscope();
                let (ctx, e1) = lower_block_rec(ctx, ss, e);
                let (ctx, es) = ctx.pop_vscope();
                ctx.typed(|t1| EMatch(t1, e0, vector![(p, (es, e1))]).into())
            }
            ast::SVar(_, _, _) => {
                todo!()
            }
            ast::SExpr(e0) => match e0.kind.as_ref() {
                ast::EReturn(_) | ast::EBreak(_) | ast::EContinue => {
                    if ss.is_empty() && e.is_none() {
                        lower_expr(ctx, e0)
                    } else {
                        panic!("Found unreachable code")
                    }
                }
                _ => {
                    let (ctx, e1) = lower_expr(ctx, e0);
                    let ctx = ctx.add_stmt_expr(e1);
                    lower_block_rec(ctx, ss, e)
                }
            },
        },
        None => match e {
            Some(e) => lower_expr(ctx, e),
            None => ctx.typed(|t| EConst(t, CUnit).into()),
        },
    }
}

fn lower_block(ctx: Context, (ss, e): ast::Block) -> (Context, Block) {
    let ctx = ctx.push_vscope();
    let (ctx, e) = lower_block_rec(ctx, ss, e);
    let (ctx, ss) = ctx.pop_vscope();
    (ctx, (ss, e))
}

fn lower_call(ctx: Context, e: ast::Expr, args: Vector<Expr>) -> (Context, Expr) {
    if let ast::EPath(xs, ts) = e.kind() {
        // Indirect call (variable)
        if let ast::PRel(xs1) = &xs {
            if xs1.len() == 1 && ts.is_empty() {
                let x = xs1.back().unwrap();
                if let Some(_) = ctx.find_vname(x) {
                    let (ctx, e) = ctx.typed(|t| EVar(t, x.clone()).into());
                    return ctx.typed(|t| ECallExpr(t, e, args).into());
                }
            }
        }
        // Direct call
        if let Some((xs, item)) = ctx.resolve_path(xs) {
            if let ast::IDef(_, _, gs, ..) | ast::IAbstractDef(_, _, _, gs, ..) = item.kind() {
                let (ctx, ts) = lower_type_args(ctx, ts, gs);
                return ctx.typed(|t| ECallItem(t, xs, ts, args).into());
            } else {
                panic!("Tried calling non-function");
            }
        } else {
            panic!("Unresolved path")
        }
    } else {
        panic!("Indirect calls not supported");
    }
}

fn lower_expr_arg(ctx: Context, e: ast::Expr) -> (Context, Expr) {
    let info = e.info;
    let ctx = ctx.push_ascope();
    let ctx = ctx.push_vscope();
    let (ctx, e) = lower_expr(ctx, e);
    let (ctx, ss) = ctx.pop_vscope();
    let ctx = ctx.add_stmts(ss);
    let (ctx, vs) = ctx.pop_ascope();
    if vs.is_empty() {
        (ctx, e)
    } else {
        let (ctx, mut vts) = vs.mapm(ctx, |ctx, v| {
            let (ctx, t) = ctx.fresh_t();
            (ctx, (v, t))
        });
        let (ctx, xs) = ctx.fresh_f();
        let (ctx, t) = ctx.fresh_t();
        let (ctx, v_env) = ctx.fresh_x();
        let (ctx, t_env) = ctx.fresh_t();
        let vt_env = (v_env, t_env);
        vts.push_back(vt_env);
        let item = IDef(vector![], vector![], vts, t, vector![], (vector![], e)).with(info);
        let ctx = ctx.add_item(xs.clone(), item);
        let (ctx, ef) = ctx.typed(|t| EItem(t, xs, vector![]).into());
        let (ctx, er) = empty_expr_env(ctx, info);
        let xes = vector![("f".to_string(), ef), ("r".to_string(), er)];
        ctx.typed(|t| ERecord(t, xes, None).with(info))
    }
}

fn lower_expr(ctx: Context, e: ast::Expr) -> (Context, Expr) {
    let info = e.info;
    match e.kind() {
        ast::EAccessRecord(e, x) => {
            let (ctx, e) = lower_expr(ctx, e);
            ctx.typed(|t| EAccessRecord(t, e, x).with(info))
        }
        ast::EAccessRecordMulti(_, _) => todo!(),
        ast::ECall(e, es) => {
            let (ctx, es) = es.mapm(ctx, lower_expr_arg);
            lower_call(ctx, e, es)
        }
        ast::ECallItem(e, x, es) => {
            let (ctx, es) = es.mapm(ctx, lower_expr_arg);
            let e = ast::EPath(ast::PRel(vector![x]), vector![]).with(e.info);
            lower_call(ctx, e, es)
        }
        ast::EAnnot(e, t) => {
            let (ctx, e) = lower_expr(ctx, e);
            let (ctx, t) = lower_type(ctx, t);
            ctx.typed(|t1| EAnnot(t1, e, t).with(info))
        }
        ast::EIf(e, b0, b1) => {
            let (ctx, e) = lower_expr(ctx, e);
            let (ctx, b0) = lower_block(ctx, b0);
            let (ctx, b1) = b1.mapm_or_else(ctx, lower_block, |ctx| {
                let (ctx, e) = ctx.typed(|t| EConst(t, CUnit).into());
                (ctx, (vector![], e))
            });
            let (ctx, arm0) = ctx.typed(|t| (PConst(t, CBool(true)).into(), b0));
            let (ctx, arm1) = ctx.typed(|t| (PIgnore(t).into(), b1));
            ctx.typed(|t| EMatch(t, e, vector![arm0, arm1]).with(info))
        }
        ast::ELit(l) => lower_lit(ctx, l, e.info),
        ast::ELoop(b) => {
            let (ctx, b) = lower_block(ctx, b);
            ctx.typed(|t| ELoop(t, b).with(info))
        }
        ast::ERecord(xes, e) => {
            let (ctx, e) = e.mapm(ctx, lower_expr);
            let (ctx, xes) = xes.mapm(ctx, lower_expr_field);
            ctx.typed(|t| ERecord(t, xes, e).with(info))
        }
        ast::EDynRecord(_) => todo!(),
        ast::EVariant(x, e) => {
            let (ctx, e) = lower_expr(ctx, e);
            ctx.typed(|t| EEnwrap(t, x, e).with(info))
        }
        ast::EReturn(e) => {
            let (ctx, e) = lower_expr_or_unit(ctx, e);
            ctx.typed(|t| EReturn(t, e).with(info))
        }
        ast::EBreak(e) => {
            let (ctx, e) = lower_expr_or_unit(ctx, e);
            ctx.typed(|t| EBreak(t, e).with(info))
        }
        ast::EContinue => ctx.typed(|t| EContinue(t).with(info)),
        ast::EDict(_) => todo!(),
        ast::ESet(_) => todo!(),
        ast::EUnop(op, e) => {
            let x = op.name();
            let (ctx, e) = lower_expr(ctx, e);
            let xs = ast::EPath(ast::PRel(vector![x]), vector![]).with(e.info);
            lower_call(ctx, xs, vector![e])
        }
        ast::EArray(_es, _e) => {
            todo!()
        }
        ast::EBinop(e0, op, e1) => lower_binop(ctx, e0, op, e1),
        ast::EDo(b) => {
            let (ctx, (ss, e)) = lower_block(ctx, b);
            let ctx = ctx.add_stmts(ss);
            (ctx, e)
        }
        ast::EFor(_, _, _) => todo!(),
        ast::EFunc(_, _, _) => todo!(),
        ast::EIfVal(p, e, b0, b1) => {
            let ctx = ctx.push_vscope();
            let (ctx, e) = lower_expr(ctx, e);
            let (ctx, p0) = lower_pat(ctx, p);
            let (ctx, b0) = lower_block(ctx, b0);
            let (ctx, b1) = lower_block_opt(ctx, b1);
            let (ctx, ss) = ctx.pop_vscope();
            let ctx = ctx.add_stmts(ss);
            let (ctx, p1) = ctx.typed(|t| PIgnore(t).with(info));
            let arms = vector![(p0, b0), (p1, b1)];
            ctx.typed(|t| EMatch(t, e, arms).with(info))
        }
        ast::EMatch(e, arms) => {
            let (ctx, e) = lower_expr(ctx, e);
            let (ctx, arms) = arms.mapm(ctx, lower_arm);
            ctx.typed(|t| EMatch(t, e, arms).with(info))
        }
        ast::EPath(xs, ts) => lower_expr_path(ctx, xs, ts, e.info),
        ast::EAccessTuple(e, i) => {
            let (ctx, e) = lower_expr(ctx, e);
            ctx.typed(|t| EAccessRecord(t, e, index_to_field(i)).with(info))
        }
        ast::EAccessArray(_e, _es) => {
            todo!()
        }
        ast::EThrow(_e) => todo!(),
        ast::ETry(_b0, _arms, _b1) => todo!(),
        ast::ETuple(es) => {
            let (ctx, es) = es.mapm(ctx, lower_expr);
            let xes = seq_to_fields(es);
            ctx.typed(|t| ERecord(t, xes, None).with(info))
        }
        ast::EFrom(_, _) => todo!(),
        ast::EAnon => {
            let (ctx, x) = ctx.bind_anon();
            ctx.typed(|t| EVar(t, x).with(info))
        }
        ast::EWhile(e, b) => {
            // Condition
            let (ctx, e0) = lower_expr(ctx, e);
            // Then-branch
            let (ctx, b0) = lower_block(ctx, b);
            // Else-branch
            let (ctx, e1) = ctx.typed(|t| EConst(t, CUnit).with(info));
            let (ctx, e1) = ctx.typed(|t| EBreak(t, e1).with(info));
            let b1 = (vector![], e1);
            // Match
            let (ctx, arm0) = ctx.typed(|t| (PConst(t, CBool(true)).with(info), b0));
            let (ctx, arm1) = ctx.typed(|t| (PIgnore(t).with(info), b1));
            let (ctx, e2) = ctx.typed(|t| EMatch(t, e0, vector![arm0, arm1]).with(info));
            // Loop
            ctx.typed(|t| ELoop(t, (vector![], e2)).with(info))
        }
        ast::EWhileVal(_, _, _) => todo!(),
        ast::EError => todo!(),
    }
}

fn lower_expr_path(
    ctx: Context,
    xs: ast::Path,
    ts: Vector<ast::Type>,
    info: Info,
) -> (Context, Expr) {
    if let ast::PRel(xs1) = &xs {
        if xs1.len() == 1 && ts.is_empty() {
            match ctx.find_vname(&xs1[0]) {
                Some((x, MVal)) => return ctx.typed(|t| EVar(t, x).with(info)),
                Some((_, MVar)) => todo!(),
                None => {}
            }
        }
    }
    match ctx.resolve_path(xs) {
        Some((xs, i)) => match i.kind() {
            ast::IAbstractDef(_, _, _, gs, ..) | ast::IDef(_, _, gs, ..) => {
                let (ctx, ts) = lower_type_args(ctx, ts, gs);
                return ctx.typed(|t| EItem(t, xs, ts).with(info));
            }
            ast::IVal(_, _, _, _) => todo!(),
            _ => panic!("Not a value"),
        },
        None => panic!("unresolved path"),
    }
}

fn lower_arm(ctx: Context, (p, e): ast::Arm) -> (Context, Arm) {
    let ctx = ctx.push_vscope();
    let (ctx, p) = lower_pat(ctx, p);
    let (ctx, e) = lower_expr(ctx, e);
    let (ctx, ss) = ctx.pop_vscope();
    (ctx, (p, (ss, e)))
}

fn lower_binop(ctx: Context, e0: ast::Expr, op: ast::Binop, e1: ast::Expr) -> (Context, Expr) {
    let (ctx, e0) = lower_expr(ctx, e0);
    let (ctx, e1) = lower_expr(ctx, e1);
    match op {
        ast::BMut => todo!(),
        ast::BMutAdd => todo!(),
        ast::BMutSub => todo!(),
        ast::BMutMul => todo!(),
        ast::BMutDiv => todo!(),
        ast::BMutMod => todo!(),
        _ => {
            let x = op.name();
            let xs = ast::EPath(ast::PRel(vector![x]), vector![]).with(e0.info.join(e1.info));
            lower_call(ctx, xs, vector![e0, e1])
        }
    }
}

fn lower_expr_or_unit(ctx: Context, e: Option<ast::Expr>) -> (Context, Expr) {
    e.mapm_or_else(ctx, lower_expr, |ctx| {
        ctx.typed(|t| EConst(t, CUnit).into())
    })
}

fn lower_type_or_unit(ctx: Context, t: Option<ast::Type>) -> (Context, Type) {
    t.mapm_or_else(ctx, lower_type, |ctx| {
        (ctx, TNominal(std("Unit"), vector![]).into())
    })
}

fn lower_type_or_fresh(ctx: Context, t: Option<ast::Type>) -> (Context, Type) {
    t.mapm_or_else(ctx, lower_type, |ctx| ctx.fresh_t())
}

fn lower_expr_field(ctx: Context, f: ExprField) -> (Context, (Name, Expr)) {
    match f {
        ast::FName(x, e) => match e {
            Some(e) => {
                let (ctx, e) = lower_expr(ctx, e);
                (ctx, (x, e))
            }
            None => match ctx.find_vname(&x) {
                Some((v, MVal)) => {
                    let (ctx, e) = ctx.typed(|t| EVar(t, v).into());
                    return (ctx, (x, e));
                }
                Some((_, MVar)) => todo!(),
                None => panic!("Name not found"),
            },
        },
        ast::FExpr(e, x) => {
            let info = e.info;
            let (ctx, e) = lower_expr(ctx, e);
            let (ctx, e) = ctx.typed(|t| EAccessRecord(t, e, x.clone()).with(info));
            (ctx, (x, e))
        }
    }
}

fn lower_const(ctx: Context, c: ast::Const) -> (Context, Const) {
    match c {
        ast::CInt(c) => (ctx, CInt(c)),
        ast::CFloat(c) => (ctx, CFloat(c)),
        ast::CBool(c) => (ctx, CBool(c)),
        ast::CString(c) => (ctx, CString(c)),
        ast::CUnit => (ctx, CUnit),
        ast::CChar(c) => (ctx, CChar(c)),
    }
}

fn lower_lit(ctx: Context, l: ast::Lit, info: Info) -> (Context, Expr) {
    match l {
        ast::LInt(i, _s) => ctx.typed(|t| EConst(t, CInt(i)).with(info)),
        ast::LFloat(f, _s) => ctx.typed(|t| EConst(t, CFloat(f)).with(info)),
        ast::LBool(b) => ctx.typed(|t| EConst(t, CBool(b)).with(info)),
        ast::LString(s) => ctx.typed(|t| EConst(t, CString(s)).with(info)),
        ast::LUnit => ctx.typed(|t| EConst(t, CUnit).with(info)),
        ast::LChar(c) => ctx.typed(|t| EConst(t, CChar(c)).with(info)),
    }
}

fn std(x: impl Into<Name>) -> Path {
    vector!["std".into(), x.into()]
}

fn lower_type_item_path(ctx: Context, xs: ast::Path, ts: Vector<ast::Type>) -> (Context, Type) {
    if let Some((xs, i)) = ctx.resolve_path(xs) {
        if let ast::IType(_, _, gs, _, _) | ast::IAbstractType(_, _, _, gs, _) = (*i.kind).clone() {
            let (ctx, ts) = lower_type_args(ctx, ts, gs);
            (ctx, TNominal(xs, ts).into())
        } else {
            todo!();
        }
    } else {
        todo!()
    }
}

fn lower_type_args(
    ctx: Context,
    ts: Vector<ast::Type>,
    gs: Vector<ast::Generic>,
) -> (Context, Vector<Type>) {
    match (ts.len(), gs.len()) {
        (n, m) if n == m => {
            let (ctx, ts) = ts.mapm(ctx, lower_type);
            (ctx, ts)
        }
        (n, _) if n == 0 => {
            let (ctx, ts) = gs.mapm(ctx, |ctx, _| {
                let (ctx, t) = ctx.fresh_t();
                (ctx, t)
            });
            (ctx, ts)
        }
        _ => panic!(),
    }
}

fn types_to_record(ts: Vector<Type>) -> Type {
    let t = ts
        .into_iter()
        .enumerate()
        .rev()
        .fold(TRowEmpty.into(), |t0, (i, t1)| {
            TRowExtend((format!("l{}", i), t1), t0).into()
        });
    TRecord(t).into()
}

fn var_to_expr(x: Name) -> Expr {
    EVar(TVar(x.clone()).into(), x).into()
}

fn fields_to_rows(xs: Vector<(Name, Type)>, t: Type) -> Type {
    xs.into_iter()
        .rev()
        .fold(t, |t0, (x, t1)| TRowExtend((x, t1), t0).into())
}

fn seq_to_fields<T: Clone>(ts: impl IntoIterator<Item = T>) -> Vector<(Name, T)> {
    ts.into_iter()
        .enumerate()
        .map(|(i, t)| (index_to_field(i as i128), t))
        .collect()
}

fn index_to_field(i: i128) -> Name {
    format!("l{i}")
}

fn lower_block_opt(ctx: Context, b: Option<ast::Block>) -> (Context, Block) {
    b.mapm_or_else(ctx, lower_block, |ctx| {
        let (ctx, e) = ctx.typed(|t| EConst(t, CUnit).into());
        (ctx, (vector![], e))
    })
}

fn empty_expr_env(ctx: Context, info: Info) -> (Context, Expr) {
    ctx.typed(|t| ERecord(t, vector![], None).with(info))
}
