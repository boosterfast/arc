use im_rc::HashMap;
use info::Info;
use std::rc::Rc;

pub type Ir3 = HashMap<Path, Item>;

pub type Name = String;
pub type Row<T> = (Name, T);

pub type Path = Vec<Name>;
pub type Index = i128;
pub type Block = (Vec<Stmt>, Var);
pub type Var = Name;
pub type Generic = Name;
pub type Decorator = Vec<(Name, Option<Const>)>;
pub type Param = (Name, Type);

pub struct Item {
    pub loc: Info,
    pub kind: ItemKind,
}
pub use ItemKind::*;
pub enum ItemKind {
    IAbstractDef(
        Decorator,
        Abstract,
        Vec<Generic>,
        Vec<Type>,
        Type,
        Vec<Effect>,
    ),
    IDef(Decorator, Vec<Generic>, Vec<Param>, Type, Block),
    IVal(Decorator, Name, Type, Expr),
    IAbstractType(Decorator, Abstract, Name, Vec<Generic>),
    IType(Decorator, Name, Vec<Generic>, Type),
}

pub use Abstract::*;
pub enum Abstract {
    AExtern,
    ABuiltin,
}

pub type Effect = Name;

pub struct Type {
    pub loc: Info,
    pub kind: Rc<TypeKind>,
}
pub use TypeKind::*;
pub enum TypeKind {
    TFunc(Vec<Type>, Type),
    TRecord(Type),
    TEnum(Vec<Row<Type>>, Type),
    TNominal(Path, Vec<Type>),
}

pub use Const::*;
pub enum Const {
    CInt(i128),
    CFloat(f64),
    CBool(bool),
    CString(String),
    CUnit,
    CChar(char),
}

pub use Stmt::*;
pub enum Stmt {
    SVal(Name, Type, Expr),
}

pub struct Expr {
    pub loc: Info,
    pub kind: Rc<ExprKind>,
}

pub use ExprKind::*;
pub enum ExprKind {
    EAccessRecord(Var, Name),
    EUpdateRecord(Expr, Name, Expr),
    ERecord(Vec<Row<Var>>),
    EBreak(Option<Expr>),
    ECallExpr(Var, Vec<Var>),
    ECallItem(Path, Vec<Type>, Vec<Var>),
    EContinue,
    EEnwrap(Name, Var),
    EUnwrap(Name, Var),
    ECheck(Name, Var),
    EItem(Path, Vec<Type>),
    EIf(Var, Block, Block),
    EConst(Const),
    ELoop(Block),
    EReturn(Expr),
    EBif(Bif),
}

pub enum Bif {
    BifAdd(Var, Var),
    BifSub(Var, Var),
    BifMul(Var, Var),
    BifDiv(Var, Var),
    BifMap(Var, Var),
    BifFilter(Var, Var),
    BifGroup(Var, Var),
    BifApply(Var, Var),
}

