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
Checked by: an_open_bind_needs_a_token_or_an_explicit_waiver, only_the_exact_bearer_token_authorizes, a_header_is_found_by_name_in_any_case, a_liveness_probe_needs_no_token_and_says_only_that_the_daemon_answers, a_liveness_probe_needs_no_token_and_a_head_answer_has_no_body

A remote listener binds loopback unless told otherwise. Bound to any other
address, it refuses to start without an authentication token, unless the
caller explicitly opts out (as a container does when it decides
reachability with a published port), and then it warns once. With a token
set, every request but the liveness probe (IFACE-18) that lacks
`Authorization: Bearer <token>` is answered 401;
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
Checked by: a_stopped_sender_keeps_its_request, a_request_whose_sender_is_gone_ends_though_its_fd_lives_on, a_sender_counts_as_alive_until_it_exits_in_either_identity, a_reply_to_a_stopped_reader_waits_until_it_resumes, a_reply_to_a_reader_that_died_ends_though_its_fd_lives_on, a_caller_whose_callee_died_stops_waiting_though_the_fd_lives_on, a_reply_that_takes_its_time_is_read_and_a_closed_peer_ends_the_wait

A pool reading a request from a local socket waits until the request is
complete, the sender closes its end, or the sending process no longer
exists, checking once a second. The sending process is the one the socket's
peer credentials name. Where the kernel keeps that process's identity with
the socket (Linux 6.5 and later), it is asked only when a wait stalls and
cannot be confused with a later process given the same pid; elsewhere the
pid and its start time are read when the connection is taken. A stopped sender counts as alive; an exited one
does not, even unreaped. There is no time limit, so a sender suspended mid-send keeps its
call; a sender that died while a process it forked holds its descriptor
open is noticed within a second. A sender whose process cannot be named
(its pid is not visible here, as from another pid namespace) has each stall
bounded at 30 s instead. A caller waiting for a reply is held to the same
rule, the callee being the process listening on the socket it connected
to. Sending on a local socket is held to it too: a send that stalls because its reader is not reading waits as long
as the reader lives, checking once a second; one to a reader that cannot be
named fails after 120 s without progress.

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

### NET-7 A local connection carries one call at a time and is reused only when its callee keeps it
Status: implemented
Checked by: a_kept_connection_carries_the_next_calls, a_callee_that_does_not_keep_gets_a_connection_per_call, a_kept_connection_whose_callee_is_gone_is_retried_once_on_a_new_one, a_call_whose_callee_dies_after_reading_it_fails_without_a_retry, a_forked_child_does_not_hold_its_parents_connections_open, the_keep_bit_is_cleared_from_a_reply, tla:CrossingConn, tla:CrossingConn_fork, tla:CrossingConn_death, tla:CrossingConn_abandon, tla:CrossingConn_child_uses_inherited.bug, tla:CrossingConn_child_keeps_inherited.bug, tla:CrossingConn_retry_after_complete_send.bug, tla:CrossingConn_keep_after_error.bug

A caller thread keeps at most one connection per callee and sends the next
request on it only after it has read the whole reply to the last. The
request asks the callee to keep the connection (a mode bit, FMT-1); the
caller reuses it only when the reply says the callee keeps it, clears that
bit before handing the reply on, and drops the connection after any error.
C++ and Rust pools keep; Python and R pools close after each reply. The connection is owned by the caller's
fork generation: a forked child closes the cached connections it inherited
and never uses them (FORK-14). A callee that keeps a connection never
closes it while it lives. A send that fails because the callee's end is
closed delivered nothing, and is retried once on a new connection; a
connection that ends after a complete send means the callee is gone, and
the call fails without a retry, so a call runs at most once and a failure
is reported only when the callee died. Model: tla:CrossingConn with its
`.bug` variants `child_uses_inherited`, `child_keeps_inherited`,
`retry_after_complete_send` and `keep_after_error`.

### NET-8 A pool never leaves a call waiting while every worker is occupied
Status: implemented
Checked by: tla:CrossingConn, tla:CrossingConn_fork, tla:CrossingConn_finishing_counts_pinned.bug, tla:CrossingConn_join_pinned_without_shutdown_wake.bug, golden:pool-nested-callbacks-churn

A thread pool serves each new connection with a free worker, starting one
when none is free; a worker that waits for the next request on a kept
connection is pinned to it and is not free. A request on a kept connection
goes to its pinned worker, which can never be busy with that connection's
previous call, since the caller sends only after a reply. So a call made
while every worker waits on another pool, or on its own connection, still
gets a worker. A pinned worker is freed when its connection's caller end
closes or its caller's process is gone (NET-5), and leaves at shutdown
without waiting for either. A pool with a fixed number of workers keeps no
connection. Model:
tla:CrossingConn with its `.bug` variants `finishing_counts_pinned` and
`join_pinned_without_shutdown_wake`.

### NET-9 In a fork pool only an idle worker takes a connection
Status: implemented
Checked by: a_fork_pool_serves_calls_and_stops_on_sigterm, a_busy_worker_gets_a_second_worker, the_busy_count_stays_balanced_across_processes, tla:ForkPool, tla:ForkPool_exclusive, tla:ForkPool_noretire, tla:ForkPool_death, tla:ForkPool_emfile_exits.bug, tla:ForkPool_retire_before_accept.bug, tla:ForkPool_no_refork_after_retire.bug, tla:ForkPool_accept_while_busy.bug, tla:ForkPool_descendant_uncounted.bug, tla:ForkPool_no_life_pipe.bug, golden:pool-nested-callbacks-churn

A fork pool (Python, R) runs one scheduler in libmorloc; the language gives
only how to fork, how a worker loads its sources, how it serves one
connection, and whether its workers retire. The coordinator binds the
pool's socket before any worker exists, and every idle worker polls it and
accepts; a worker that loses the race to accept polls again. A worker
waiting on another pool does not poll. The pool counts the waits on
another pool in progress -- a foreign call's round trip, a channel read or
write that sleeps -- made by a worker or by a process the user's code
forked from one, counting nested waits on one thread once; the coordinator
forks a worker whenever that count reaches the number of live workers (or
none is left). A worker accepts before it considers retiring, so it never
retires with a connection queued, and an accept that fails for want of a
descriptor leaves the connection queued and the worker polling. A process
forked from a worker never accepts. The readiness probe is answered like
any call, by a worker whose sources have loaded. A worker leaves when the
read end of a pipe only the coordinator holds reaches end of file, so no
worker outlives its coordinator.
