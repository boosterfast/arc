use crate::repr::info::exprs::ExprId;
use crate::repr::info::files::Loc;
use crate::repr::info::names::NameId;
use crate::repr::info::paths::PathId;
use derive_more::From;
use std::collections::HashMap;

#[derive(Default, Debug)]
pub(crate) struct LocInterner {
    store: HashMap<LocId, Loc>,
}

#[derive(Debug, Hash, Eq, PartialEq, From)]
pub(crate) enum LocId {
    Expr(ExprId),
    Name(NameId),
    Path(PathId),
}

impl LocInterner {
    pub(crate) fn insert(&mut self, id: impl Into<LocId>, loc: Loc) {
        self.store.insert(id.into(), loc);
    }
}
