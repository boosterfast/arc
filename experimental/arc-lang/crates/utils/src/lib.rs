use im_rc::Vector;

pub trait VectorUtils<T0>
where
    Self: IntoIterator<Item = T0>,
{
    fn mapm<T1, F, C>(self, ctx: C, f: F) -> (C, Vector<T1>)
    where
        F: Fn(C, T0) -> (C, T1),
        T1: Clone;
}

impl<T0> VectorUtils<T0> for Vector<T0>
where
    T0: Clone,
{
    fn mapm<T1, F, C>(self, ctx: C, f: F) -> (C, Vector<T1>)
    where
        F: Fn(C, T0) -> (C, T1),
        T1: Clone,
    {
        let mut ctx = ctx;
        let mut v = Vector::new();
        for x in self {
            let (ctx1, y) = f(ctx, x);
            ctx = ctx1;
            v.push_back(y);
        }
        (ctx, v)
    }
}

pub trait OptionUtils<T0> {
    fn mapm<T1, F, C>(self, ctx: C, f: F) -> (C, Option<T1>)
    where
        F: Fn(C, T0) -> (C, T1);

    fn mapm_or_else<T1, F, G, C>(self, ctx: C, f: F, g: G) -> (C, T1)
    where
        F: Fn(C, T0) -> (C, T1),
        G: Fn(C) -> (C, T1);
}

impl<T0> OptionUtils<T0> for Option<T0> {
    fn mapm<T1, F, C>(self, ctx: C, f: F) -> (C, Option<T1>)
    where
        F: Fn(C, T0) -> (C, T1),
    {
        match self {
            None => (ctx, None),
            Some(x) => {
                let (ctx, y) = f(ctx, x);
                (ctx, Some(y))
            }
        }
    }

    fn mapm_or_else<T1, F, G, C>(self, ctx: C, f: F, g: G) -> (C, T1)
    where
        F: Fn(C, T0) -> (C, T1),
        G: Fn(C) -> (C, T1),
    {
        match self {
            None => g(ctx),
            Some(x) => f(ctx, x),
        }
    }
}
