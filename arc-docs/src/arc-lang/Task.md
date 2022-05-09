# Tasks

A **task** is an asynchronous function which can suspend its execution to wait for events. Expressions with `async` effects can only be used within the context of a task.

```grammar
Task ::= "task" [Name] [Generics]? [Params] ":" [Sinks] [Where]? [Body]

Sinks ::= "(" [Sink]","+ ")"
Sink ::= [Name] (":" [Type])?
```

## Examples

### Basic task

```arc-lang
{{#include ../../../arc-lang/examples/task-identity.arc:example}}
```

### Lambda tasks

```arc-lang
{{#include ../../../arc-lang/examples/task-lambda.arc:example}}
```

### Multi-Input Tasks

```arc-lang
{{#include ../../../arc-lang/examples/task-merge.arc:example}}
```

### Multi-Output Tasks
