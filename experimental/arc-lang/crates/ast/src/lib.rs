pub mod write;

use im_rc::Vector;
use info::Info;
use std::rc::Rc;

pub type Name = String;
pub type Arm = (Pattern, Expr);

pub use DefName::*;
#[derive(Clone, Debug)]
pub enum DefName {
    DName(Name),
    DUnop(Unop),
    DBinop(Binop),
}

pub use Path::*;
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum Path {
    PAbs(Vector<Name>),
    PRel(Vector<Name>),
}

pub type Index = i128;

pub use ExprField::*;
#[derive(Clone, Debug)]
pub enum ExprField {
    FName(Name, Option<Expr>),
    FExpr(Expr, Name),
}

pub type Block = (Stmts, Option<Expr>);
pub type Generic = Name;
pub type Decorator = Vector<Attribute>;
pub type Attribute = (Name, Option<Const>);

#[derive(Clone, Debug)]
pub struct Item {
    pub info: Info,
    pub kind: Rc<ItemKind>,
}

impl Item {
    pub fn kind(&self) -> ItemKind {
        (*self.kind).clone()
    }
}

pub use ItemKind::*;
#[derive(Clone, Debug)]
pub enum ItemKind {
    IFrom(Decorator, Sources, QueryStmts),
    IAbstractDef(
        Decorator,
        Abstract,
        DefName,
        Vector<Generic>,
        Vector<Type>,
        Option<Type>,
        Vector<Effect>,
        Vector<Bound>,
    ),
    IDef(
        Decorator,
        DefName,
        Vector<Generic>,
        Vector<Pattern>,
        Option<Type>,
        Vector<Bound>,
        Block,
    ),
    IVal(Decorator, Name, Option<Type>, Expr),
    IAbstractType(Decorator, Abstract, Name, Vector<Generic>, Vector<Bound>),
    IClass(
        Decorator,
        Name,
        Vector<Generic>,
        Vector<Bound>,
        Vector<MethodDecl>,
    ),
    IInstance(
        Decorator,
        Vector<Generic>,
        Path,
        Vector<Type>,
        Vector<Bound>,
        Vector<MethodDef>,
    ),
    IMod(Decorator, Name, Vector<Item>),
    IType(Decorator, Name, Vector<Generic>, Type, Vector<Bound>),
    IUse(Decorator, Path, Option<UseSuffix>),
}

pub use Abstract::*;
#[derive(Clone, Debug)]
pub enum Abstract {
    AExtern,
    ABuiltin,
}

pub type Bound = (Path, Vector<Type>);

pub type Effect = Name;

pub use UseSuffix::*;
#[derive(Clone, Debug)]
pub enum UseSuffix {
    UAlias(Name),
    UGlob,
}

pub type MethodDecl = (
    Name,
    Vector<Generic>,
    Vector<Pattern>,
    Option<Type>,
    Vector<Bound>,
);

pub type MethodDef = (
    Name,
    Vector<Generic>,
    Vector<Pattern>,
    Option<Type>,
    Vector<Bound>,
    Block,
);

#[derive(Clone, Debug)]
pub struct Pattern {
    pub info: Info,
    pub kind: Rc<PatternKind>,
}

pub use PatternKind::*;
#[derive(Clone, Debug)]
pub enum PatternKind {
    PIgnore,
    POr(Pattern, Pattern),
    PAnnot(Pattern, Type),
    PRecord(Vector<(Name, Option<Pattern>)>, Option<Pattern>),
    PTuple(Vector<Pattern>),
    PArray(Vector<Pattern>, Option<Pattern>),
    PConst(Const),
    PVar(Name),
    PVariant(Name, Pattern),
    PError,
}

#[derive(Clone, Debug)]
pub struct Type {
    pub info: Info,
    pub kind: Rc<TypeKind>,
}
pub use TypeKind::*;
#[derive(Clone, Debug)]
pub enum TypeKind {
    TFunc(Vector<Type>, Type),
    TTuple(Vector<Type>),
    TEnum(Vector<(Name, Type)>, Option<Type>),
    TRecord(Vector<(Name, Option<Type>)>, Option<Type>),
    TPath(Path, Vector<Type>),
    TArray(Type),
    TUnit,
    TNever,
    TError,
}

pub use Binop::*;
#[derive(Clone, Debug)]
pub enum Binop {
    BAdd,
    BAnd,
    BBand,
    BBor,
    BBxor,
    BDiv,
    BEq,
    BGeq,
    BGt,
    BLeq,
    BLt,
    BMod,
    BMul,
    BMut,
    BNeq,
    BOr,
    BPow,
    BSub,
    BXor,
    BIn,
    BRExc,
    BRInc,
    BNotIn,
    BMutAdd,
    BMutSub,
    BMutMul,
    BMutDiv,
    BMutMod,
}

pub use Unop::*;
#[derive(Clone, Debug)]
pub enum Unop {
    UNeg,
    UNot,
}

pub use Lit::*;
#[derive(Clone, Debug)]
pub enum Lit {
    LInt(i128, Option<Name>),
    LFloat(f64, Option<Name>),
    LBool(bool),
    LString(String),
    LUnit,
    LChar(char),
}

pub use Const::*;
#[derive(Clone, Debug)]
pub enum Const {
    CInt(i128),
    CFloat(f64),
    CBool(bool),
    CString(String),
    CUnit,
    CChar(char),
}

pub type Stmts = Vector<Stmt>;
pub use Stmt::*;
#[derive(Clone, Debug)]
pub enum Stmt {
    SNoop,
    SVal(Pattern, Expr),
    SVar(Name, Option<Type>, Expr),
    SExpr(Expr),
}

#[derive(Clone, Debug)]
pub struct Expr {
    pub info: Info,
    pub kind: Rc<ExprKind>,
}

impl Expr {
    pub fn kind(&self) -> ExprKind {
        (*self.kind).clone()
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
    pub fn with(self, info: Info) -> Type {
        Type {
            info,
            kind: Rc::new(self),
        }
    }
}

impl Type {
    pub fn kind(&self) -> TypeKind {
        (*self.kind).clone()
    }
}

impl Pattern {
    pub fn kind(&self) -> PatternKind {
        (*self.kind).clone()
    }
}

pub use ExprKind::*;
#[derive(Clone, Debug)]
pub enum ExprKind {
    EAccessRecord(Expr, Name),
    EAccessRecordMulti(Expr, Vector<Name>),
    ECall(Expr, Vector<Expr>),
    EAnnot(Expr, Type),
    EIf(Expr, Block, Option<Block>),
    ELit(Lit),
    ELoop(Block),
    ERecord(Vector<ExprField>, Option<Expr>),
    EDynRecord(Vector<(Expr, Expr)>),
    EVariant(Name, Expr),
    EReturn(Option<Expr>),
    EBreak(Option<Expr>),
    EContinue,
    EDict(Vector<(Expr, Expr)>),
    ESet(Vector<Expr>),
    //    (, NB: These expressions are desugared ,)
    EUnop(Unop, Expr),
    EArray(Vector<Expr>, Option<Expr>),
    EBinop(Expr, Binop, Expr),
    EDo(Block),
    EFor(Pattern, Expr, Block),
    EFunc(Vector<Pattern>, Option<Type>, Block),
    EIfVal(Pattern, Expr, Block, Option<Block>),
    ECallItem(Expr, Name, Vector<Expr>),
    EMatch(Expr, Vector<Arm>),
    EPath(Path, Vector<Type>),
    EAccessTuple(Expr, Index),
    EAccessArray(Expr, Vector<Expr>),
    EThrow(Expr),
    ETry(Block, Vector<Arm>, Option<Block>),
    ETuple(Vector<Expr>),
    EFrom(Sources, QueryStmts),
    EAnon,
    EWhile(Expr, Block),
    EWhileVal(Pattern, Expr, Block),
    EError,
}

pub type Sources = Vector<Source>;
pub type Source = (Pattern, SourceKind, Expr);
pub use SourceKind::*;
#[derive(Clone, Debug)]
pub enum SourceKind {
    ScIn,
    ScEq,
}

pub type QueryStmts = Vector<QueryStmt>;

pub use QueryStmt::*;
#[derive(Clone, Debug)]
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
#[derive(Clone, Debug)]
pub enum Order {
    OAsc,
    ODesc,
}

impl DefName {
    pub fn name(self) -> Name {
        match self {
            DName(x) => x,
            DUnop(unop) => unop.name(),
            DBinop(binop) => binop.name(),
        }
    }
}

impl Unop {
    pub fn name(self) -> Name {
        match self {
            UNeg => "neg".to_owned(),
            UNot => "not".to_owned(),
        }
    }
}

impl Binop {
    pub fn name(self) -> Name {
        match self {
            BAdd => "add".to_owned(),
            BAnd => "and".to_owned(),
            BBand => "band".to_owned(),
            BBor => "bor".to_owned(),
            BBxor => "bxor".to_owned(),
            BDiv => "div".to_owned(),
            BEq => "eq".to_owned(),
            BGeq => "geq".to_owned(),
            BGt => "gt".to_owned(),
            BLeq => "leq".to_owned(),
            BLt => "lt".to_owned(),
            BMod => "mod".to_owned(),
            BMul => "mul".to_owned(),
            BMut => "mut".to_owned(),
            BNeq => "neq".to_owned(),
            BOr => "or".to_owned(),
            BPow => "pow".to_owned(),
            BSub => "sub".to_owned(),
            BXor => "xor".to_owned(),
            BIn => "in".to_owned(),
            BRExc => "rexc".to_owned(),
            BRInc => "rinc".to_owned(),
            BNotIn => "notin".to_owned(),
            BMutAdd => "mutadd".to_owned(),
            BMutSub => "mutsub".to_owned(),
            BMutMul => "mutmul".to_owned(),
            BMutDiv => "mutdiv".to_owned(),
            BMutMod => "mutmod".to_owned(),
        }
    }
}
