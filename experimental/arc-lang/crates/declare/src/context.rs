use ast::Item;
use ast::Name;
use im_rc::HashMap;
use im_rc::Vector;
use info::Info;

pub type Path = Vector<Name>;

#[derive(Clone)]
pub struct Context {
    pub(crate) graph: HashMap<Path, Decl>
}

pub use Decl::*;
#[derive(Clone)]
pub enum Decl {
    DItem(Item),
    DUse(Path),
}

impl Context {
    pub(crate) fn new() -> Self {
        Context {
            graph: HashMap::new(),
        }
    }

    pub(crate) fn add(mut self, _loc: Info, xs: Path, d: Decl) -> Self {
        self.graph.insert(xs, d);
        self
    }

    pub fn get(&self, xs: Path) -> Option<(Path, Item)> {
        match self.graph.get(&xs) {
            Some(DItem(i)) => Some((xs, i.clone())),
            Some(DUse(xs)) => self.clone().get(xs.clone()),
            None => None,
        }
    }
}
