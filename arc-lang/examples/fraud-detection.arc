# XFAIL: *
# RUN: arc -o %t run %s -- -rustinclude %s.rust-tests
# RUN: arc -o %t-canon run %s -- -rustinclude %s.rust-tests -canonicalize

# ANCHOR: example
val large_amount = 1.0;
val small_amount = 500.0;

task detect(transactions): (alerts) {
    var t0 = transactions.pull();
    loop {
        val t1 = transactions.pull();
        if t1.amount < small_amount and
           t0.amount > large_amount and
           t1.time - t0.time < 60s {
            alerts.push(t1.id);
        }
        t0 = t1;
    }
}

def main() = transactions().keyby(_.id).detect().print();
# ANCHOR_END: example
