# RUN: arc -o %t run %s -- -rustinclude %s.rust-tests
# RUN: arc -o %t-canon run %s -- -rustinclude %s.rust-tests -canonicalize

# ANCHOR: example
def apply(binop, l, r) = binop(l, r)

def main() {
    assert(apply((+), 1, 3) == 4);
}
# ANCHOR_END: example
