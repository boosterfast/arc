# XFAIL: *
# RUN: arc -o %t run %s -- -rustinclude %s.rust-tests
# RUN: arc -o %t-canon run %s -- -rustinclude %s.rust-tests -canonicalize

# ANCHOR: example
type A = {x:i32, y:str};   # Record-type
type B = (i32, str);       # Tuple-type
type C = [i32];            # Array-type
type D = <X(i32), Y(str)>; # Enum-type
type E = fun(i32): i32;    # Function-type
# ANCHOR_END: example

def main() = {}
