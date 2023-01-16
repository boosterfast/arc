# Arrays

An array is a collection of values which are stored contiguously.

```arc-lang
{{#exec grep 'extern type.*Array' ../arc-lang/stdlib/std.arc}}
```

Arrays are indexable, and may grow and shrink in size. The functions supported for arrays are listed as follows:

```arc-lang
{{#exec grep 'extern def.*Array' ../arc-lang/stdlib/std.arc}}
```

## Syntactic sugar

```arc-lang
{{#include ../../../arc-lang/examples/arrays.arc:example}}
```
