# XFAIL: *
# RUN: arc -o %t run %s -- -rustinclude %s.rust-tests
# RUN: arc -o %t-canon run %s -- -rustinclude %s.rust-tests -canonicalize

# ANCHOR: example
def main() {

    match new Some(5) {
        case Some(x) => assert(x == 5),
        case None => assert(false),
    };

    match new Some(5.0) {
        case Some(x) => assert(x == 5.0),
        case None => assert(false),
    };

}
# ANCHOR_END: example

