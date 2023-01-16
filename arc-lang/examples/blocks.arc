# RUN: arc -o %t run %s -- -rustinclude %s.rust-tests
# RUN: arc -o %t-canon run %s -- -rustinclude %s.rust-tests -canonicalize

def main() {
# ANCHOR: example
val a = do { do { do { do { do { 6 } } } } };
val b = do { 1; 2; 3; 4; 5; 6 };
val c = do { 1; do { 2; do { 3; do { 4; do { 5; do { 6 } } } } } };
val d = do { do { do { do { do { do { 1 }; 2 }; 3 }; 4 }; 5 }; 6 };
val e = a + b + c + d;
# ANCHOR_END: example
}
