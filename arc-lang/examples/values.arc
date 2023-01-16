# XFAIL: *
# RUN: arc -o %t run %s -- -rustinclude %s.rust-tests
# RUN: arc -o %t-canon run %s -- -rustinclude %s.rust-tests -canonicalize

def main() {
# ANCHOR: example
val v0: i32            = 3;                   # Integer
val v1: f32            = 0.1;                 # Float
val v2: String         = 2020-12-16T16:00:00; # DateTime
val v3: char           = 'c';                 # Character
val v4: String         = "hello";             # String
val v5: String         = "$v6 world";         # String (Interpolated)
val v6: (i32, i32)     = (5, 8);              # Tuple
val v7: {x:i32, y:i32} = {x:5, y:8};          # Record
val v8: Option[i32]    = `Some(3);            # Enum variant
val v9: [i32]          = [1,2,3];             # Array
val v10: fun(i32): i32 = fun(x) = x;          # Function
# ANCHOR_END: example

# ANCHOR: suffixed
val v2: f32            = 10%;                 # Float (Percentage)
val v3: i32            = 100ms;               # Duration
# ANCHOR_END: suffixed
}
