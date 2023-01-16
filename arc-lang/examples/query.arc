# XFAIL: *
# RUN: arc -o %t run %s -- -rustinclude %s.rust-tests
# RUN: arc -o %t-canon run %s -- -rustinclude %s.rust-tests -canonicalize

# ANCHOR: implicit
from x: {k:i32,t:time,v:i32} in source({"topic":"mytopic"}) {
    where x.k != 1
    group x.k as k
    window x.t as w {
        length 5m
        stride 1m
        compute max of x.v as max
    }
    compute
        sum of x.v,
        count
};
# ANCHOR_END: implicit

def main() {}
