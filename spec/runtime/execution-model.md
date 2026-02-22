# Execution Model

## Overview

A compiled morloc program executes as a set of cooperating processes: one nexus process and zero or more pool processes. The nexus is the entry point; pools execute language-specific code on demand.

## Nexus Lifecycle

1. **Startup.** The user invokes the program: `./foo subcommand [args]`. The nexus binary loads its manifest from `./foo.manifest`.

2. **CLI parsing.** The nexus parses command-line arguments according to the manifest's argument definitions for the named subcommand. Argument types, defaults, and help text are all manifest-driven.

3. **Pool startup.** The nexus starts the pool processes required for the subcommand. Each pool is a separate OS process communicating via a Unix domain socket. Only pools listed in the command's `needed_pools` are started.

4. **Argument serialization.** The nexus serializes parsed arguments into msgpack format using the argument schemas from the manifest.

5. **Dispatch.** The nexus sends the serialized arguments to the appropriate pool, identified by the command's pool index and manifold ID.

6. **Result handling.** The nexus receives the serialized result from the pool, deserializes it, and prints it to stdout.

7. **Shutdown.** After the command completes, the nexus terminates all pool processes and exits.

## Pure Commands

Some commands are *pure*: they require no pool and are evaluated entirely within the nexus using a built-in expression evaluator. Pure commands have an expression tree in the manifest instead of a pool reference. The nexus evaluates the tree directly, supporting literals, function application, lambda expressions, and string interpolation.

## Pool Process Management

Each pool is a long-running process that:

1. Starts up and loads its generated code (imports for Python/R, compiled code for C++).
2. Opens a Unix domain socket and waits for connections.
3. On each request: deserializes arguments, dispatches to the appropriate function by manifold ID, serializes the result, and sends it back.
4. Exits when the nexus closes the connection or sends a termination signal.

Pools are created in a temporary directory (`/tmp/morloc.XXXXXX/`) with socket files named by language (e.g., `pipe-py`, `pipe-cpp`).

## Worker Dispatch Strategy

Each pool forks multiple worker processes (typically `nproc - 1`) to handle requests concurrently. The dispatcher must route incoming client connections to an available worker.

### Shared Queue (Current Approach)

The Python and R pools use a single Unix socketpair as a shared job queue. The dispatcher writes client file descriptors (via `SCM_RIGHTS` / `sendmsg`) to one end; all workers block on `recvmsg` on the other end. The kernel delivers each fd to exactly one waiting worker.

This design handles re-entrant callbacks correctly. When a worker makes a `foreign_call` to another pool and that pool calls back, the callback arrives as a new client connection. Since the blocked worker never calls `recvmsg`, the callback is picked up by an idle worker. Busy workers are invisible to the dispatch mechanism.

The C++ pool achieves equivalent semantics using a shared queue protected by a mutex and condition variable, with threads instead of processes.

### Why Not Round-Robin

A naive round-robin dispatcher assigns connections to workers in fixed order (W0, W1, ..., W0, ...). This works for unidirectional calls but deadlocks under re-entrant callbacks:

1. Worker W0 receives a job and makes a `foreign_call` to Pool B.
2. Pool B processes the call and issues callbacks back to Pool A. Each callback is a new connection.
3. The round-robin dispatcher sends callbacks to W1, W2, ..., Wn in order.
4. When all N workers are blocked in `foreign_call`, the (N+1)th callback wraps around to W0 -- which is still blocked. The callback sits unread in W0's pipe while Pool B waits for the response.

This circular dependency hangs all workers. The failure threshold is exactly N simultaneous bidirectional calls where N equals the worker count.

### Depth Limitation

Even with the shared queue, deep cross-language call chains are limited by the worker count. Each hop in a chain like `rId (pyId (rId (pyId ...)))` blocks a worker until the deeper computation returns. A depth-D chain (D alternating cross-language calls) requires roughly D/2 workers in each pool simultaneously. Chains deeper than `2 * (nproc - 1)` will deadlock due to worker exhaustion.

## Dispatch Flow

```
User CLI input
  --> Nexus: parse args, serialize
  --> Socket: send to pool
  --> Pool: deserialize, call function
  --> Pool: serialize result
  --> Socket: send to nexus
  --> Nexus: deserialize, print
```

For cross-language function calls *within* the same command, the pool-to-pool path goes through the nexus:

```
Pool A: serialize result --> Nexus --> Pool B: deserialize, call, serialize --> Nexus --> Pool A
```

## Error Propagation

Errors at any stage propagate back to the user:

- **CLI parse errors**: the nexus prints usage information and exits.
- **Pool startup failure**: the nexus reports which pool failed to start.
- **Function execution errors**: the pool sends an error response (status code 1) with an error message; the nexus prints the message and exits with a non-zero code.
- **Communication errors**: socket failures or unexpected disconnections cause the nexus to report the failure and terminate.

## Resource Cleanup

The nexus is responsible for cleaning up all resources:

- Terminating pool processes (via signals)
- Removing Unix domain socket files
- Removing the temporary directory
