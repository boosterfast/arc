# Effects

Arc-Lang has an effect system which is used to capture information about side effects that occur in the program.

```
Effects ::= [[Effect]]"+"*
Effect ::=
  | async
  | spawn
  | io
```

The meaning of the effects are:
* `async` means the value must be awaited, which is only possible within the context of a task.
* `spawn` means the value must be spawned, which is only possible within the context of a driver.
* `io` means the expression may perform I/O, which is only allowed within the context of a driver.
