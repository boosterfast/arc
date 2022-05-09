# Deduplication

A common problem in data intensive computing is to deduplicate events of data streams {{#cite kleppmann-2017}}. As an example, a login system must ensure that all usernames uniquely identify user accounts. This can be implemented as follows.

```arc-lang
{{#include ../../../arc-lang/examples/deduplication.arc:example}}
```
