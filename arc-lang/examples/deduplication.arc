# XFAIL: *
# RUN: arc -o %t run %s -- -rustinclude %s.rust-tests
# RUN: arc -o %t-canon run %s -- -rustinclude %s.rust-tests -canonicalize

# ANCHOR: example
def deduplicate(usernames, results) {
    var registered = set();
    loop {
        val username = usernames.pull();
        if registered.contains(username) {
            results.push(new Err(username));
        } else {
            registered.add(username);
            results.push(new Ok(username));
        }
    }
}
# ANCHOR_END: example

def main() = 1
