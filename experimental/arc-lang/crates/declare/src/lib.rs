use crate::context::DItem;
use crate::context::DUse;
use ast::*;
pub use context::Context;
use im_rc::Vector;

pub mod context;

pub fn declare_ast(ast: Vector<Item>) -> Context {
    let ctx = Context::new();
    let xs = Vector::<Name>::new();
    let ctx = ast
        .iter()
        .fold(ctx, |ctx, i| declare_item(i.clone(), ctx, xs.clone()));
    ctx
}

fn declare_item(item: Item, ctx: Context, mut xs: Vector<Name>) -> Context {
    match item.kind.as_ref().clone() {
        IFrom(_, _, _) => ctx,
        IAbstractDef(_, _, d, _, _, _, _, _) => {
            xs.push_back(d.name());
            ctx.add(item.info, xs, DItem(item))
        }
        IDef(_, d, _, _, _, _, _) => {
            xs.push_back(d.name());
            ctx.add(item.info, xs, DItem(item))
        }
        IVal(_, x, _, _) => {
            xs.push_back(x);
            ctx.add(item.info, xs, DItem(item))
        }
        IAbstractType(_, _, x, _, _) => {
            xs.push_back(x);
            ctx.add(item.info, xs, DItem(item))
        }
        IClass(_, x, _, _, _) => {
            xs.push_back(x);
            ctx.add(item.info, xs, DItem(item))
        }
        IInstance(_, _, _, _, _, _) => ctx,
        IMod(_, x, items) => {
            xs.push_back(x);
            items
                .into_iter()
                .fold(ctx, |ctx, i| declare_item(i, ctx, xs.clone()))
        }
        IType(_, x, _, _, _) => {
            xs.push_back(x);
            ctx.add(item.info, xs, DItem(item))
        }
        IUse(_, xs_aliased, alias) => {
            let xs_aliased = match xs_aliased {
                PAbs(xs_aliased) => {
                    let mut xs = xs.clone();
                    xs.append(xs_aliased);
                    xs
                }
                PRel(xs_aliased) => xs_aliased,
            };
            let xs_alias = match alias {
                Some(UAlias(x)) => {
                    let mut xs = xs.clone();
                    xs.push_back(x);
                    xs
                }
                Some(UGlob) => todo!(),
                None => {
                    let mut xs = xs.clone();
                    xs.push_back(xs_aliased.last().unwrap().clone());
                    xs
                }
            };
            ctx.add(item.info, xs_alias, DUse(xs_aliased))
        }
    }
}
