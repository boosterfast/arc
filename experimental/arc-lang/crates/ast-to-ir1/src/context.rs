use im_rc::Vector;
use im_rc::vector;
use ir1::*;
use std::collections::HashMap;
use std::collections::VecDeque;
use std::rc::Rc;

pub(crate) struct Context {
    declarations: declare::Context,
    next_def_uid: u64,
    next_type_uid: u64,
    next_generic_uid: u64,
    next_expr_uid: u64,
    vstack: VecDeque<VScope>,
    gstack: VecDeque<GScope>,
    astack: VecDeque<AScope>,
    namespace: Path,
    pub(crate) ir1: HashMap<Path, Item>,
}

pub(crate) use Mut::*;
#[derive(Clone, Debug)]
pub(crate) enum Mut {
    MVar,
    MVal,
}

pub(crate) type VScope = (HashMap<ast::Name, (Name, Mut)>, Vector<Stmt>);
pub(crate) type GScope = HashMap<ast::Name, Name>;
pub(crate) type AScope = Vector<Name>;

impl Context {
    pub(crate) fn new(declarations: declare::Context) -> Self {
        Context {
            declarations,
            next_def_uid: 0,
            next_type_uid: 0,
            next_generic_uid: 0,
            next_expr_uid: 0,
            vstack: VecDeque::new(),
            gstack: VecDeque::new(),
            astack: VecDeque::new(),
            namespace: Path::new(),
            ir1: Ir1::new(),
        }
    }

    pub(crate) fn add_item(mut self, xs: Path, i: Item) -> Self {
        self.ir1.insert(xs, i);
        self
    }

    pub(crate) fn typed<T>(self, f: impl FnOnce(Type) -> T) -> (Self, T) {
        let (ctx, t) = self.fresh_t();
        (ctx, f(t))
    }

    pub(crate) fn fresh_t(mut self) -> (Self, Type) {
        let uid = self.next_type_uid;
        self.next_expr_uid += 1;
        let t = Type {
            kind: Rc::new(TVar(Name::from(format!("t{}", uid)))),
        };
        (self, t)
    }

    pub(crate) fn fresh_x(mut self) -> (Self, Name) {
        let uid = self.next_expr_uid;
        self.next_expr_uid += 1;
        let x = Name::from(format!("x{}", uid));
        (self, x)
    }

    pub(crate) fn fresh_f(mut self) -> (Self, Vector<Name>) {
        let uid = self.next_def_uid;
        self.next_def_uid += 1;
        let f = vector![Name::from(format!("f{}", uid))];
        (self, f)
    }

    pub(crate) fn fresh_g(mut self) -> (Self, Name) {
        let uid = self.next_generic_uid;
        self.next_generic_uid += 1;
        let g = Name::from(format!("g{}", uid));
        (self, g)
    }

    pub(crate) fn push_vscope(mut self) -> Self {
        self.vstack.push_back((HashMap::new(), Vector::new()));
        self
    }

    pub(crate) fn pop_vscope(mut self) -> (Self, Vector<Stmt>) {
        let (_, ss) = self.vstack.pop_back().unwrap();
        (self, ss)
    }

    pub(crate) fn add_stmts(mut self, ss: Vector<Stmt>) -> Self {
        self.vstack.back_mut().unwrap().1.extend(ss);
        self
    }

    pub(crate) fn add_stmt_expr(mut self, e: Expr) -> Self {
        self.vstack.back_mut().unwrap().1.push_back(SExpr(e));
        self
    }

    pub(crate) fn push_gscope(mut self) -> Self {
        self.gstack.push_back(HashMap::new());
        self
    }

    pub(crate) fn pop_gscope(mut self) -> Self {
        self.gstack.pop_back();
        self
    }

    pub(crate) fn push_ascope(mut self) -> Self {
        self.astack.push_back(Vector::new());
        self
    }

    pub(crate) fn pop_ascope(mut self) -> (Self, Vector<Name>) {
        let xs = self.astack.pop_back().unwrap();
        (self, xs)
    }

    pub(crate) fn item_path(&self, x: Name) -> Path {
        let mut xs = self.namespace.clone();
        xs.push_back(x);
        xs
    }

    pub(crate) fn bind_vname(self, x0: ast::Name, m: Mut) -> (Self, Name) {
        let (mut ctx, x1) = self.fresh_x();
        ctx.vstack
            .back_mut()
            .unwrap()
            .0
            .insert(x0.clone(), (x1.clone(), m));
        (ctx, x1)
    }

    pub(crate) fn bind_gname(self, x0: ast::Name) -> (Self, Name) {
        let (mut ctx, x1) = self.fresh_g();
        ctx.gstack
            .back_mut()
            .unwrap()
            .insert(x0.clone(), x1.clone());
        (ctx, x1)
    }

    pub(crate) fn bind_anon(self) -> (Self, Name) {
        let (mut ctx, x) = self.fresh_x();
        ctx.astack.back_mut().unwrap().push_back(x.to_string());
        (ctx, x)
    }

    pub(crate) fn find_vname(&self, x0: &ast::Name) -> Option<(Name, Mut)> {
        self.vstack
            .iter()
            .rev()
            .find_map(|(substs, _)| substs.get(x0).cloned())
    }

    pub(crate) fn find_gname(&self, x0: &ast::Name) -> Option<Name> {
        self.gstack
            .iter()
            .rev()
            .find_map(|substs| substs.get(x0).cloned())
    }

    pub(crate) fn debug_vstack(&self) {
        for (substs, _) in self.vstack.iter().rev() {
            for (x0, (x1, m)) in substs.iter() {
                println!("{} -> {} ({:?})", x0, x1, m);
            }
        }
    }

    pub(crate) fn push_namespace(mut self, x: Name) -> Self {
        self.namespace.push_back(x);
        self
    }

    pub(crate) fn pop_namespace(mut self) -> Self {
        self.namespace.pop_back();
        self
    }

    pub(crate) fn resolve_path(&self, xs: ast::Path) -> Option<(Path, ast::Item)> {
        let xs = match xs {
            ast::PAbs(xs) => xs,
            ast::PRel(xs) => {
                let mut xs1 = self.namespace.clone();
                xs1.append(xs);
                xs1
            }
        };
        self.declarations.get(xs)
    }
}
