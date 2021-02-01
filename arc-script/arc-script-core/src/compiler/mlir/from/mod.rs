/// Module for converting the [`crate::repr::dfg::DFG`] into MLIR.
pub(crate) mod lower;

use crate::compiler::dfg::DFG;
use crate::compiler::hir;
use crate::compiler::hir::HIR;
use crate::compiler::info::Info;
use crate::compiler::mlir;
use crate::compiler::mlir::MLIR;
use crate::compiler::shared::{Lower, Map, New};

impl MLIR {
    pub(crate) fn from(hir: HIR, dfg: DFG, info: &mut Info) -> Self {
        let ctx = &mut lower::Context::new(&hir, info);
        let defs = hir
            .items
            .iter()
            .map(|x| (*x, hir.defs.get(x).unwrap().lower(ctx)))
            .collect::<Map<_, _>>();
        let main = dfg.lower(ctx);
        MLIR::new(hir.items, defs, main)
    }
}
