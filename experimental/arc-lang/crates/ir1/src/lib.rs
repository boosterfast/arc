use im_rc::Vector;
use info::Info;
use std::collections::HashMap;
use std::rc::Rc;

pub type Ir1 = HashMap<Path, Item>;

pub type Arm = (Pattern, Block);
pub type Name = String;
pub type Row<T> = (Name, T);

pub type Path = Vector<Name>;
pub type Index = i128;
pub type Block = (Vector<Stmt>, Expr);
pub type Generic = Name;
pub type Decorator = Vector<(Name, Option<Const>)>;
pub type Attribute = (Name, Option<Const>);
pub type Param = (Name, Type);

#[derive(Clone)]
pub struct Item {
    pub info: Info,
    pub kind: Rc<ItemKind>,
}
pub use ItemKind::*;
#[derive(Clone)]
pub enum ItemKind {
    IAbstractDef(
        Decorator,
        Abstract,
        Vector<Generic>,
        Vector<Type>,
        Type,
        Vector<Effect>,
        Vector<Bound>,
    ),
    IDef(
        Decorator,
        Vector<Generic>,
        Vector<Param>,
        Type,
        Vector<Bound>,
        Block,
    ),
    IVal(Decorator, Type, Expr),
    IAbstractType(Decorator, Abstract, Vector<Generic>, Vector<Bound>),
    IType(Decorator, Vector<Generic>, Type, Vector<Bound>),
}

pub use Abstract::*;
#[derive(Clone)]
pub enum Abstract {
    AExtern,
    ABuiltin,
}

pub type Bound = (Path, Vector<Type>);

pub type Effect = Name;

#[derive(Clone)]
pub struct Pattern {
    pub info: Info,
    pub kind: Rc<PatternKind>,
}

pub use PatternKind::*;
#[derive(Clone)]
pub enum PatternKind {
    PIgnore(Type),
    POr(Type, Pattern, Pattern),
    PAnnot(Type, Pattern),
    PRecord(Type, Vector<Row<Pattern>>, Option<Pattern>),
    PTuple(Type, Vector<Pattern>),
    PArray(Type, Vector<Pattern>, Option<Pattern>),
    PConst(Type, Const),
    PVar(Type, Name),
    PVariant(Type, Name, Pattern),
    PError(Type),
}

#[derive(Clone)]
pub struct Type {
    pub kind: Rc<TypeKind>,
}

pub use TypeKind::*;
#[derive(Clone)]
pub enum TypeKind {
    TFunc(Vector<Type>, Type),
    TRecord(Type),
    TEnum(Type),
    TRowEmpty,
    TRowExtend(Row<Type>, Type),
    TNominal(Path, Vector<Type>),
    TGeneric(Name),
    TVar(Name),
    TError,
}

pub use Const::*;
#[derive(Clone)]
pub enum Const {
    CInt(i128),
    CFloat(f64),
    CBool(bool),
    CString(String),
    CUnit,
    CChar(char),
}

pub use Stmt::*;
#[derive(Clone)]
pub enum Stmt {
    SExpr(Expr),
}

#[derive(Clone)]
pub struct Expr {
    pub info: Info,
    pub kind: Rc<ExprKind>,
}

pub use ExprKind::*;
#[derive(Clone)]
pub enum ExprKind {
    EAccessRecord(Type, Expr, Name),
    EAnnot(Type, Expr, Type),
    EBreak(Type, Expr),
    ECallExpr(Type, Expr, Vector<Expr>),
    ECallItem(Type, Path, Vector<Type>, Vector<Expr>),
    EContinue(Type),
    EEnwrap(Type, Name, Expr),
    EItem(Type, Path, Vector<Type>),
    EConst(Type, Const),
    ELoop(Type, Block),
    EMatch(Type, Expr, Vector<Arm>),
    ERecord(Type, Vector<Row<Expr>>, Option<Expr>),
    EReturn(Type, Expr),
    EUpdateRecord(Type, Expr, Name, Expr),
    EVar(Type, Name),
    EError(Type),
}

pub type Sources = Vector<Source>;
pub type Source = (Pattern, SourceKind, Expr);
pub use SourceKind::*;
#[derive(Clone)]
pub enum SourceKind {
    ScIn,
    ScEq,
}

pub type QueryStmts = Vector<QueryStmt>;

pub use QueryStmt::*;
#[derive(Clone)]
pub enum QueryStmt {
    SWhere {
        e: Expr,
    },
    SJoin {
        args: Vector<(Pattern, Expr)>,
        e: Expr,
    },
    SGroup {
        es: Vector<Expr>,
        alias: Name,
    },
    SWindow {
        arg: Expr,
        length: Expr,
        alias: Name,
        repeat: Option<Expr>,
        aggrs: Vector<Aggr>,
    },
    SCompute {
        aggrs: Vector<Aggr>,
    },
    SOrder {
        orders: Vector<(Expr, Order)>,
    },
    SSelect {
        e: Expr,
    },
    SInto {
        e: Expr,
    },
}

pub type Aggr = (Expr, Option<Expr>, Name);

pub use Order::*;
#[derive(Clone)]
pub enum Order {
    OAsc,
    ODesc,
}

impl From<ExprKind> for Expr {
    fn from(kind: ExprKind) -> Self {
        Expr {
            info: Info::Gen,
            kind: Rc::new(kind),
        }
    }
}

impl From<TypeKind> for Type {
    fn from(kind: TypeKind) -> Self {
        Type {
            kind: Rc::new(kind),
        }
    }
}

impl From<PatternKind> for Pattern {
    fn from(kind: PatternKind) -> Self {
        Pattern {
            info: Info::Gen,
            kind: Rc::new(kind),
        }
    }
}

impl ExprKind {
    pub fn with(self, info: Info) -> Expr {
        Expr {
            info,
            kind: Rc::new(self),
        }
    }
}

impl PatternKind {
    pub fn with(self, info: Info) -> Pattern {
        Pattern {
            info,
            kind: Rc::new(self),
        }
    }
}

impl TypeKind {
    pub fn with(self) -> Type {
        Type {
            kind: Rc::new(self),
        }
    }
}

impl ItemKind {
    pub fn with(self, info: Info) -> Item {
        Item {
            info,
            kind: Rc::new(self),
        }
    }
}

impl Pattern {
    pub fn ty(&self) -> Type {
        match self.kind.as_ref() {
            PIgnore(t) => t.clone(),
            POr(t, _, _) => t.clone(),
            PAnnot(t, _) => t.clone(),
            PRecord(t, _, _) => t.clone(),
            PTuple(t, _) => t.clone(),
            PArray(t, _, _) => t.clone(),
            PConst(t, _) => t.clone(),
            PVar(t, _) => t.clone(),
            PVariant(t, _, _) => t.clone(),
            PError(t) => t.clone(),
        }
    }
}
