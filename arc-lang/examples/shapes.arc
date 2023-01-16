# XFAIL: *
# RUN: arc -o %t run %s -- -rustinclude %s.rust-tests
# RUN: arc -o %t-canon run %s -- -rustinclude %s.rust-tests -canonicalize

# ANCHOR: example
def area(shape) = match shape {
    Rectangle {width, height} => width * height,
    Square {length} => 2 * length
}

def main() {
    val a0 = area(new Rectangle { width:5, height:3 });
    val a1 = area(new Square { length:3 });
}
# ANCHOR_END: example
