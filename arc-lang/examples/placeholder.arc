# RUN: arc -o %t run %s -- -rustinclude %s.rust-tests
# RUN: arc -o %t-canon run %s -- -rustinclude %s.rust-tests -canonicalize

# ANCHOR: example
def main() {
    apply(_ + _);
}

def apply(f) = f(1, 2)
# ANCHOR_END: example
