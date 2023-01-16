# XFAIL: *
# RUN: arc -o %t run %s -- -rustinclude %s.rust-tests
# RUN: arc -o %t-canon run %s -- -rustinclude %s.rust-tests -canonicalize

def main() = ()

def test0() {
# ANCHOR: expressions
val x = {a:1};
val y = {b:2|x};
val z = {y.a, y.b, c:3}; # Create a record `{a:x.a, b:y.b, c:3}`
val w = y.{a, b};        # Create a record `{a:y.a, b:y.b}`
# ANCHOR_END: expressions
}

def test1() {
# ANCHOR: patterns
val x = {a:1, b:2, c:3};
val {a, b, c} = x;   # Extract `a`, `b`, and `c`
val {a} = x;           # Extract only `a`
val {a, b, c:y} = x; # Extract `a` and `b`, and bind `c` to `y`
val {a, b|y} = x;     # Extract `a` and `b`, and bind `{c}` to `y`
# ANCHOR_END: patterns
}
