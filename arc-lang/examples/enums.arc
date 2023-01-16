# RUN: arc -o %t run %s -- -rustinclude %s.rust-tests
# RUN: arc -o %t-canon run %s -- -rustinclude %s.rust-tests -canonicalize

def main() {
# ANCHOR: example
match new Some(x) {
    case Some(y) => y,
    case None => 1,
}
# ANCHOR_END: example
}
