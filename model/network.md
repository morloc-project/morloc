# Network and local endpoints (NET)

A *remote* listener accepts TCP connections: the daemon's TCP and HTTP
listeners, the MCP HTTP server and the serving front end. Anything that can
reach its port may connect, so it treats every client as hostile: a client
may lie, stall, send slowly, open many connections or vanish without
closing. A *local* endpoint is a Unix socket. Its peers are processes on
this machine that hold a connected descriptor, and a peer that dies closes
its end, so the reader sees end of file at once.

The two get opposite rules for waiting. A remote client can vanish without
a trace, so a wait on one is bounded. A local peer's death is always seen,
and a suspended peer (Ctrl-Z, a paused batch job) will resume, so a wait on
one is not bounded by time.

### NET-1 A listener reachable from another machine requires a credential
Status: implemented
Checked by: an_open_bind_needs_a_token_or_an_explicit_waiver, only_the_exact_bearer_token_authorizes, a_header_is_found_by_name_in_any_case

A remote listener binds loopback unless told otherwise. Bound to any other
address, it refuses to start without an authentication token, unless the
caller explicitly opts out (as a container does when it decides
reachability with a published port), and then it warns once. With a token
set, every request without `Authorization: Bearer <token>` is answered 401;
the token is compared in constant time. This holds for the daemon's HTTP
listener, the MCP server and the serving front end; the daemon's TCP
listener binds loopback only. The daemon test group `http-auth` runs it end
to end.

### NET-2 A remote client's request arrives within a total deadline
Status: deviation

A request's line and headers arrive within one total deadline from its
first byte, and its body arrives at no less than a minimum rate. An idle
keep-alive connection is closed after a limit. A per-read timeout alone
does not meet this, because a client sending one byte just inside each
timeout would hold a worker forever.

Missing: the daemon sets a 30 s timeout on each read of an accepted
connection, and the MCP and front-end connections do the same; neither has
a total deadline or a minimum rate.

### NET-3 A remote client's share of the server is bounded
Status: deviation

The number of connections served at once, the length of a header line, the
number of headers and the size of a body are each capped. Past the
connection cap, a new connection is answered 503, or closed, before
anything is read from it.

Missing: header line length, header count and body size are capped
(`read_http_request_rejects_overlong_header_line`,
`read_http_request_rejects_header_flood`,
`read_http_request_body_bounded_to_content_length`). The MCP server and the
front end start a thread per connection with no cap, and the daemon queues
accepted connections without a cap.

### NET-4 A local endpoint lives in a directory only its user can enter
Status: deviation

Every Unix socket, port file and state directory a morloc process creates
lies under a directory owned by the running user with mode 0700: the run's
temporary directory, or a per-user runtime directory
(`$XDG_RUNTIME_DIR/morloc`, else `/tmp/morloc-<uid>`). A per-user directory
is created when missing, and is used only after its owner and mode have
been checked. Nothing is created at a fixed path in a directory other users
can write.

Missing: pool sockets live in the run's private directory. Router sockets
are created at `/tmp/morloc-router-<name>.sock`, and the daemon's binding
store at `/tmp/morloc-bindings`.

### NET-5 A request on a local socket waits as long as its sender lives
Status: deviation

A pool reading a request from a local socket waits until the request is
complete, the sender closes its end, or the sending process (its pid taken
from the socket's peer credentials when the connection is accepted) no
longer exists. A stopped sender counts as alive. There is no time limit.

Missing: a pool gives up on a request whose first byte, or whose next
byte, takes more than 30 s, so a sender suspended mid-send loses its call.

### NET-6 An endpoint path is taken only from no live listener
Status: deviation

A daemon takes its socket path, and its port file, only while it holds an
exclusive lock on a lock file beside the path; the lock lasts for the
daemon's life and is never inherited by a child. Holding the lock, it
replaces a socket file only when connecting to it is refused. If a live
listener answers, or another daemon holds the lock, it refuses to start.

Missing: a daemon unlinks its socket path before binding, so it takes the
path from a daemon still serving there; two daemons writing one port file
share one temporary name.
