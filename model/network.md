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
Status: implemented
Checked by: a_request_trickling_in_ends_at_its_deadline, a_request_head_trickling_in_ends_at_its_deadline, a_large_body_gets_time_in_proportion

A request's line and headers, or a length prefix, arrive within one total
deadline (30 s) of the server starting to wait for them, and its body
within 30 s plus a second for every 16 KiB. On a keep-alive connection the
wait for the next request shares that deadline. Every read waits only for
the time left, so a client sending one byte just inside each per-read
timeout still runs out of time. This holds for the daemon's HTTP and TCP
listeners, the MCP server and the serving front end.

### NET-3 A remote client's share of the server is bounded
Status: implemented
Checked by: connections_past_the_cap_get_no_slot, read_http_request_rejects_overlong_header_line, read_http_request_rejects_header_flood, read_http_request_body_bounded_to_content_length

The length of a header line, the number of headers and the size of a body
are each capped. The MCP server and the front end serve at most 128
connections at once; the daemon lets at most four remote connections per
worker wait in its queue. A connection past either cap is answered 503 (or,
on the daemon's TCP listener, closed) before anything is read from it, and
without blocking the accept loop. Local connections to the daemon are not
capped.

### NET-4 A local endpoint lives in a directory only its user can enter
Status: implemented
Checked by: a_missing_directory_is_created_private, a_directory_others_may_enter_is_refused, a_symlink_is_refused_even_to_a_private_directory

Every Unix socket, log and state directory a morloc process creates at a
name of its own choosing lies under a directory owned by the running user
with mode 0700: the run's temporary directory, or the per-user runtime
directory (`$XDG_RUNTIME_DIR/morloc`, else `/tmp/morloc-<uid>`). The
per-user directory is created when missing and used only after it is found
to be a real directory, owned by this user, that no one else may enter.
Router sockets, the daemon's binding store, the router's daemon logs (when
no state directory is set) and the cache (when no home directory is set)
live there. A temporary file in a shared directory is made only with a
unique name, created exclusively. Paths the user names (`--socket`,
`--port-file`) are the user's choice.

### NET-5 A request on a local socket waits as long as its sender lives
Status: implemented
Checked by: a_stopped_sender_keeps_its_request, a_request_whose_sender_is_gone_ends_though_its_fd_lives_on

A pool reading a request from a local socket waits until the request is
complete, the sender closes its end, or the sending process (its pid taken
from the socket's peer credentials, with its start time) no longer exists,
checking once a second. A stopped sender counts as alive; an exited one
does not, even unreaped. There is no time limit, so a sender suspended mid-send keeps its
call; a sender that died while a process it forked holds its descriptor
open is noticed within a second. A sender whose process cannot be named
(its pid is not visible here, as from another pid namespace) has each stall
bounded at 30 s instead.

### NET-6 An endpoint path is taken only from no live listener
Status: implemented
Checked by: tla:EndpointClaim, tla:EndpointClaim_unlocked.bug, tla:EndpointClaim_unconditional.bug, tla:EndpointClaim_release_then_unlink.bug, tla:EndpointClaim_unverified.bug, a_live_listener_keeps_its_path_and_a_stale_file_is_replaced

A daemon takes its socket path, and its port file, only while it holds an
exclusive lock on a lock file beside the path. It opens the lock file
(never following a symlink, never inherited by a child) and takes the lock
without waiting, refusing to start if another process holds it; if the
file it locked is no longer the one at that path, it starts over. Holding
the lock, it binds; when the path is in use it connects to it, refuses to
start if something answers, and otherwise unlinks the stale file and binds
again. A port file is written to a temporary name of its own and renamed.
On exit the daemon removes each endpoint it created and then its lock file
while it still holds the lock (DAEMON-10), so a daemon that opened the old
lock file finds it gone and starts over. A daemon that refuses removes nothing: it lets the lock go and leaves
the lock file in place. A held lock on the port file refuses the start
like one on the socket; a port file that cannot be written at all is
reported and the daemon serves without it.
