# XFAIL: *
# RUN: arc -o %t run %s -- -rustinclude %s.rust-tests
# RUN: arc -o %t-canon run %s -- -rustinclude %s.rust-tests -canonicalize

@{rust: "source"}
extern def source(): PullChan[i32];

def main() {}

# ANCHOR: timer
task gap(i, f): (o) = {
    val timer = fun() = o.push(i.pull());
    loop {
        schedule 60s;
    } timer {
        o.push(f(i.pull()));
    }
}
# ANCHOR_END: timer
