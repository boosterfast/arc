# RUN: arc -o %t run %s -- -rustinclude %s.rust-tests
# RUN: arc -o %t-canon run %s -- -rustinclude %s.rust-tests -canonicalize

def main() {
# ANCHOR: example
assert("hello".str_eq("hello"));
assert(not "hello".str_eq("world"));

assert(i32_to_string(1).str_eq("1"));

val a2 = "world";
a2.push_char('!');
assert(a2.str_eq("world!"));

a2.insert_char(0u32, '!');
assert(str_eq(a2, "!world!"));

assert(not "hello".is_empty_str());
assert("".is_empty_str());

val a4 = "hey";
a4.clear_str();
assert(is_empty_str(a4));

assert("(((".concat(")))").str_eq("((()))"));
# ANCHOR_END: example
}
