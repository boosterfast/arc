use crate::{info::Info, prelude::*, printer::Printer, typer::*};
use std::cell::RefMut;

impl Script<'_> {
    pub fn mlir(&self) -> String {
        let pr = Printer {
            info: &self.info,
            tabs: 2,
            verbose: false,
        };
        format!(
            "{}",
            self.ast
                .fundefs
                .values()
                .map(|fundef| fundef.to_func(&pr))
                .collect::<Vec<_>>()
                .join("\n")
        )
    }
}

impl Expr {
    pub fn mlir(&self, info: &Info) -> String {
        let pr = Printer {
            info,
            tabs: 2,
            verbose: false,
        };
        format!(
            "{s}{module}\n",
            module = self.to_module(&pr.tab()),
            s = pr.indent()
        )
    }

    pub fn to_module(&self, pr: &Printer) -> String {
        let main = format!(
            r#"{s}"std.func"() ({{{region}}}) {{ sym_name = "main", type = () -> {ty} }}"#,
            region = self.to_region("std.return", &pr.tab()),
            ty = self.tv.to_ty(pr),
            s = pr.indent(),
        );
        format!(
            r#""module" ({{{main}{s0}"module_terminator"() : () -> (){s1}}}) : () -> ()"#,
            main = main,
            s0 = pr.indent(),
            s1 = pr.untab().indent()
        )
    }

    fn to_region(&self, terminator: &str, pr: &Printer) -> String {
        match &self.kind {
            Let(id, e1, e2) => format!(
                "{s}{var} = {op}{next}",
                var = id.to_var(),
                op = e1.to_op(pr),
                next = e2.to_region(terminator, pr),
                s = pr.indent(),
            ),
            BinOp(lhs, Seq, rhs) => format!(
                "{s}{lhs}{rhs}",
                lhs = lhs.to_op(pr),
                rhs = rhs.to_region(terminator, pr),
                s = pr.indent(),
            ),
            Var(_) => format!(
                r#"{s0}"{t}"({var}) : ({ty}) -> (){s1}"#,
                t = terminator,
                var = self.to_var(),
                ty = self.tv.to_ty(pr),
                s0 = pr.indent(),
                s1 = pr.untab().indent(),
            ),
            _ => unreachable!(),
        }
    }

    fn to_op(&self, pr: &Printer) -> String {
        match &self.kind {
            Lit(kind) => {
                let lit = kind.to_lit();
                let ty = self.tv.to_ty(pr);
                format!(
                    r#""std.constant"() {{ value = {lit} : {ty} }}: () -> {ty}"#,
                    lit = lit,
                    ty = ty,
                )
            }
            BinOp(l, op, r) => {
                let l = l.to_var();
                let r = r.to_var();
                let ty = pr.info.typer.borrow_mut().lookup(self.tv);
                match (op, ty.kind) {
                    (Add, Scalar(I32)) => {
                        format!(r#""std.addi"({}, {}) : (i32, i32) -> i32"#, l, r)
                    }
                    (Add, Scalar(F32)) => {
                        format!(r#""std.addf"({}, {}) : (f32, f32) -> f32"#, l, r)
                    }
                    _ => todo!(),
                }
            }
            If(c, t, e) => format!(
                r#""arc.if"({var}) ({{{t}}},{{{e}}}) : ({arg_ty}) -> {out_ty}"#,
                var = c.to_var(),
                t = t.to_region("arc.yield", &pr.tab()),
                e = e.to_region("arc.yield", &pr.tab()),
                arg_ty = c.tv.to_ty(pr),
                out_ty = e.tv.to_ty(pr),
            ),
            UnOp(..) => todo!(),
            ConsArray(..) => todo!(),
            ConsStruct(..) => todo!(),
            ConsVariant(_, _) => todo!(),
            ConsTuple(..) => todo!(),
            Closure(..) => todo!(),
            Let(..) => panic!("[ICE] Attempted to generate MLIR SSA of Let"),
            Match(..) => panic!("[ICE] Attempted to generate MLIR SSA of Match"),
            Var(_) => panic!("[ICE] Attempted to generate MLIR SSA of Var"),
            Loop(_, _) => todo!(),
            For(..) => todo!(),
            ExprErr => "<ERROR>".to_owned(),
        }
    }

    fn to_var(&self) -> String {
        match &self.kind {
            Var(id) => id.to_var(),
            _ => unreachable!(),
        }
    }
}

impl Ident {
    fn to_var(&self) -> String {
        format!("%x_{}", self.0)
    }
}

impl TypeVar {
    fn to_ty(self, pr: &Printer) -> String {
        let typer = pr.info.typer.borrow_mut();
        self.to_ty_rec(typer)
    }
    fn to_ty_rec(self, mut typer: RefMut<Typer>) -> String {
        match typer.lookup(self).kind {
            Nominal(_) => todo!(),
            Scalar(kind) => match kind {
                I8 => "i8".to_owned(),
                I16 => "i16".to_owned(),
                I32 => "i32".to_owned(),
                I64 => "i64".to_owned(),
                F32 => "f32".to_owned(),
                F64 => "f64".to_owned(),
                Bool => "i1".to_owned(),
                Null => todo!(),
                Str => todo!(),
                Unit => todo!(),
            },
            Struct(_) => todo!(),
            Enum(_) => todo!(),
            Array(_, _) => todo!(),
            Stream(_) => todo!(),
            Map(_, _) => todo!(),
            Set(_) => todo!(),
            Vector(_) => todo!(),
            Tuple(_) => todo!(),
            Optional(_) => todo!(),
            Fun(_, _) => todo!(),
            Task(_) => todo!(),
            Unknown => "<UNKNOWN>".to_string(),
            TypeErr => "<ERROR>".to_string(),
        }
    }
}

impl LitKind {
    fn to_lit(&self) -> String {
        match self {
            LitI8(l) => l.to_string(),
            LitI16(l) => l.to_string(),
            LitI32(l) => l.to_string(),
            LitI64(l) => l.to_string(),
            LitF32(l) => l.to_string(),
            LitF64(l) => l.to_string(),
            LitBool(l) => l.to_string(),
            LitTime(_) => todo!(),
            LitUnit => todo!(),
            LitErr => "<ERROR>".to_string(),
        }
    }
}

impl FunDef {
    fn to_func(&self, pr: &Printer) -> String {
        let tv = pr.info.table.get_decl(&self.id).tv;
        let ty = { pr.info.typer.borrow_mut().lookup(tv) };
        if let Fun(_, ret_tv) = ty.kind {
            format!(
                "func @{id}({params}) -> ({ret_ty}) {{{s1}{body}{s0}}}",
                id = self.id.to_var(),
                params = self
                    .params
                    .iter()
                    .map(|id| pr.info.table.get_decl(id).tv.to_ty(pr))
                    .collect::<Vec<_>>()
                    .join(","),
                ret_ty = ret_tv.to_ty(&pr),
                body = self.body.to_region("std.return", &pr.tab()),
                s0 = pr.indent(),
                s1 = pr.tab().indent(),
            )
        } else {
            "<ERROR>".to_string()
        }
    }
}
