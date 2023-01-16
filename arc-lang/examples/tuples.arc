# XFAIL: *
# RUN: arc -o %t run %s -- -rustinclude %s.rust-tests
# RUN: arc -o %t-canon run %s -- -rustinclude %s.rust-tests -canonicalize

def main() = ()

def test1() {
# ANCHOR: expressions
val x = (1,);
val y = (1, "hello");
val z = (1, "world", 3.14);
# ANCHOR_END: expressions
}

def test2() {
# ANCHOR: patterns
val x = (1, "hello", 3.14);
val (a, b, c) = x;
# ANCHOR_END: patterns
}
