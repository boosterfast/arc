# XFAIL: *
# RUN: arc -o %t run %s -- -rustinclude %s.rust-tests
# RUN: arc -o %t-canon run %s -- -rustinclude %s.rust-tests -canonicalize

def eval(e) = match e {
    case Num(x) => x,
    case Add(a, b) => eval(a) + eval(b),
}

def main() {
    val x = eval(new Num(1));
    val y = eval(new Add(1, 2));
    assert(x == 1);
    assert(y == 3);
}
