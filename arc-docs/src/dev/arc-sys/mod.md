# Arc-Sys Reference

Arc-Sys is a distributed system for executing Arc-Lang programs that focuses on flexibility, portability, native performance, and scalability. Arc-Sys is divided into a set of components:

```
Orchestration  Client <--> Coordinator <--> Worker
    Layer      (Rust)        (Rust)         (Rust)
                 ^             ^              ^
                 |             |              |
                 v             v              v
  Execution   Arc-Lang      Arc-MLIR       Dataflow
    Layer     Compiler      Compiler       Executor
              (Ocaml)        (C++)          (Rust)
The workflow is as follows:
```

Users write Arc-Lang programs on their local machine and execute them using a client program. The client communicates with a local Arc-Lang process using pipes to compile and interpret the program. During interpretation, the interpreter may construct dataflow graphs that should be executed on a distributed system. These logical dataflow graphs and their UDFs are holistically represented as Arc-MLIR code and sent back to the client process using standard output. The client process forwards the code to a coordinator process using TCP. The coordinator process uses an Arc-MLIR compiler to compile the MLIR source into a Rust module representing the UDFs, and a JSON object representing an optimised logical dataflow graph. The coordinator then compiles the logical graph into a physical graph mapped to specific hardware threads and sockets in a cluster of workers.

Arc-MLIR and Arc-Runtime share different responsibilities in making Arc-Lang programs execute efficiently. Arc-MLIR supports ahead-of-time standard compiler optimisations as well as logical and physical optimisations of streaming-relational operators. However, Arc-MLIR makes no assumptions of where programs will execute and what the input data will be. That is, all Arc-MLIR optimisations are based on the source code itself.

Arc-Runtime supports runtime optimisations which might rely on information that is only known during execution. Streaming programs are expected to run for an indefinite duration and must therefore be able to adapt to changes in their environment. Among its responsibilities, Arc-Runtime must therefore be able to take care of specialisation, scheduling, and scaling decisions.

For more information, see the [internal developer documentation of the Arc-Runtime](../../target/doc/index.html).
