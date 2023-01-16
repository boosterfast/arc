# XFAIL: *
# RUN: arc -o %t run %s -- -rustinclude %s.rust-tests
# RUN: arc -o %t-canon run %s -- -rustinclude %s.rust-tests -canonicalize

def main() {
# ANCHOR: example
# Pattern matching on enums
val a = new Some(new Some(3));

match a {
    case Some(case Some(x)) => x,
    case Some(case None) => 2,
    case None => 1+1,
};
# ANCHOR_END: example
}
