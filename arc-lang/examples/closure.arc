# XFAIL: *
# RUN: arc -o %t run %s -- -rustinclude %s.rust-tests
# RUN: arc -o %t-canon run %s -- -rustinclude %s.rust-tests -canonicalize

def main() {
    val a = 1;
    2.call(fun(x) = x + a)
}

def call(x, f) = f(x)
