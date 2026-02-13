# Inter-Process Communication

The nexus and pools communicate over Unix domain sockets using a binary message protocol built on msgpack.

## Transport

Each pool opens a Unix domain socket in the program's temporary directory:

```
/tmp/morloc.XXXXXX/pipe-py
/tmp/morloc.XXXXXX/pipe-cpp
/tmp/morloc.XXXXXX/pipe-r
```

The nexus connects to these sockets to send requests and receive responses. Each socket carries bidirectional traffic for one pool.

## Request Format

A request from the nexus to a pool is a msgpack-encoded structure:

```
{
  function_id: <int>,         -- manifold ID identifying the function
  args: [<msgpack>, ...]      -- serialized arguments
}
```

The `function_id` is an integer assigned by the compiler. Each function in a pool has a unique manifold ID. The pool uses this ID to dispatch to the correct function.

Arguments are pre-serialized by the nexus according to the argument schemas in the manifest. Each argument is an opaque msgpack byte sequence from the pool's perspective until it deserializes with the expected schema.

## Response Format

A response from a pool to the nexus:

```
{
  status: <int>,              -- 0 = success, 1 = error
  result: <msgpack>           -- serialized return value or error message
}
```

On success, `result` contains the function's return value serialized according to the return schema. On error, `result` contains a string error message.

## Data Flow

### Simple Command

```
Nexus                              Pool
  |                                  |
  |-- request(mid=3, args=[...]) --> |
  |                                  | deserialize args
  |                                  | call function #3
  |                                  | serialize result
  | <-- response(status=0, ...) --   |
  |                                  |
```

### Cross-Language Call

When function A (in Pool X) calls function B (in Pool Y):

```
Nexus                Pool X              Pool Y
  |                    |                   |
  |-- request -------> |                   |
  |                    | call A            |
  |                    | A needs B         |
  | <-- call B ------- |                   |
  |-- request(B) -----------------------> |
  |                                        | call B
  | <-- response(B) --------------------- |
  |-- response(B) --> |                   |
  |                    | A continues      |
  | <-- response(A) - |                   |
```

The nexus mediates all cross-pool communication. Pools never communicate directly with each other.

## Packet Format

Messages are framed as length-prefixed packets:

1. **Length header**: 4-byte big-endian unsigned integer specifying the payload size.
2. **Payload**: msgpack-encoded request or response.

This framing allows the receiver to read exactly the right number of bytes for each message.

## Connection Lifecycle

1. The nexus starts a pool process.
2. The pool creates a Unix socket and begins listening.
3. The nexus connects to the socket.
4. Request/response pairs are exchanged.
5. On completion, the nexus closes the connection.
6. The pool detects the closed connection and exits.
