/// Module for converting the [`crate::repr::dfg::DFG`] into MLIR.
pub(crate) mod lower;

use crate::compiler::hir::HIR;
use crate::compiler::info::Info;

use crate::compiler::mlir::MLIR;
use arc_script_core_shared::Lower;
use arc_script_core_shared::Map;

impl MLIR {
    #[allow(clippy::needless_pass_by_value)] // Temporary
    pub(crate) fn from(hir: &HIR, info: &mut Info) -> Self {
        let ctx = &mut lower::Context::new(hir, info);
        let defs = hir
            .items
            .iter()
            .filter_map(|x| Some((*x, hir.defs.get(x).unwrap().lower(ctx)?)))
            .collect::<Map<_, _>>();
        Self::new(hir.items.clone(), defs)
    }
}
