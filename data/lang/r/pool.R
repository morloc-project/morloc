# The preamble (dyn.load of librmorloc.so, library(bit64), .morloc.source
# definition) and the generated schema/closure tables run at module top, in the
# coordinator parent -- they are thread-free and the parent's .Call wrappers and
# fork machinery depend on them (dyn.load in particular MUST run before the
# parent calls any morloc_* function).
# AUTO include preamble start
# <<<BREAK>>>
# AUTO include preamble end

# User `source` calls, by contrast, are deferred out of the parent and run in
# each worker AFTER fork (see mlc_load_user_sources). Loading a package that
# spins threads (an OpenMP / Accelerate-BLAS user library) in the parent and then
# forking is unsafe on macOS (fork-after-threads deadlock/abort). Deferring keeps
# the parent single-threaded; each worker loads its libraries in its own process,
# preserving per-worker thread parallelism. Held as an unevaluated function body
# and eval'd into globalenv post-fork, so sourced symbols land at global scope
# exactly as if run at load time (R's source() targets globalenv by default).
.mlc_sources_fn <- function() {
# AUTO include user-sources start
# <<<BREAK>>>
# AUTO include user-sources end
}

.mlc_sources_loaded <- FALSE
.mlc_source_error <- NULL

mlc_load_user_sources <- function() {
  if (.mlc_sources_loaded) return(invisible())
  # Import-time stdout must not corrupt the data stream shared with the nexus.
  sink(stderr(), type = "output")
  on.exit(sink(NULL, type = "output"), add = TRUE)
  tryCatch({
    eval(body(.mlc_sources_fn), envir = globalenv())
    .mlc_sources_loaded <<- TRUE
  }, error = function(e) {
    .mlc_source_error <<- paste("failed to load pool sources:", conditionMessage(e))
    cat(.mlc_source_error, "\n", file = stderr())
  })
  invisible()
}

morloc_is_ping                       <- function(...){ .Call("morloc_is_ping",                       ...) }
morloc_pong                          <- function(...){ .Call("morloc_pong",                          ...) }
morloc_is_local_call                 <- function(...){ .Call("morloc_is_local_call",                 ...) }
morloc_is_remote_call                <- function(...){ .Call("morloc_is_remote_call",                ...) }
morloc_make_fail_packet              <- function(...){ .Call("morloc_make_fail_packet",              ...) }
morloc_wait_for_client               <- function(...){ .Call("morloc_wait_for_client",               ...) }
morloc_stream_from_client            <- function(...){ .Call("morloc_stream_from_client",            ...) }
morloc_read_morloc_call_packet       <- function(...){ .Call("morloc_read_morloc_call_packet",       ...) }
morloc_send_packet_to_foreign_server <- function(...){ .Call("morloc_send_packet_to_foreign_server", ...) }
morloc_close_socket                  <- function(...){ .Call("morloc_close_socket",                  ...) }
morloc_start_daemon                  <- function(...){ .Call("morloc_start_daemon",                  ...) }
morloc_shinit                        <- function(...){ .Call("morloc_shinit",                        ...) }
morloc_foreign_call                  <- function(...){ .Call("morloc_foreign_call",                  ...) }
morloc_get_value                     <- function(...){ .Call("morloc_get_value",                     ...) }
morloc_put_value                     <- function(...){ .Call("morloc_put_value",                     ...) }
morloc_release_packet_shm            <- function(...){ .Call("morloc_release_packet_shm",            ...) }
morloc_mlc_show                      <- function(...){ .Call("morloc_mlc_show",                      ...) }
morloc_mlc_save                      <- function(...){ .Call("morloc_mlc_save",                      ...) }
morloc_mlc_save_voidstar             <- function(...){ .Call("morloc_mlc_save_voidstar",             ...) }
morloc_mlc_save_json                 <- function(...){ .Call("morloc_mlc_save_json",                 ...) }
morloc_mlc_load                      <- function(...){ .Call("morloc_mlc_load",                      ...) }
morloc_mlc_hash                      <- function(...){ .Call("morloc_mlc_hash",                      ...) }
morloc_mlc_read                      <- function(...){ .Call("morloc_mlc_read",                      ...) }
morloc_mlc_open                      <- function(...){ .Call("morloc_mlc_open",                      ...) }
morloc_mlc_close                     <- function(...){ .Call("morloc_mlc_close",                     ...) }
morloc_mlc_tmpfile                   <- function(...){ .Call("morloc_mlc_tmpfile",                   ...) }
morloc_mlc_unlink_tmp                <- function(...){ .Call("morloc_mlc_unlink_tmp",                ...) }
morloc_mlc_tell                      <- function(...){ .Call("morloc_mlc_tell",                      ...) }
morloc_mlc_fschema                   <- function(...){ .Call("morloc_mlc_fschema",                   ...) }
morloc_mlc_ifile_walk                <- function(...){ .Call("morloc_mlc_ifile_walk",                ...) }
morloc_mlc_ifile_length              <- function(...){ .Call("morloc_mlc_ifile_length",              ...) }
morloc_mlc_next                      <- function(...){ .Call("morloc_mlc_next",                      ...) }
morloc_mlc_stream_layout             <- function(...){ .Call("morloc_mlc_stream_layout",             ...) }
morloc_mlc_stream                    <- function(...){ .Call("morloc_mlc_stream",                    ...) }
morloc_mlc_open_ostream              <- function(...){ .Call("morloc_mlc_open_ostream",              ...) }
morloc_mlc_open_istream              <- function(...){ .Call("morloc_mlc_open_istream",              ...) }
morloc_mlc_open_stdout               <- function(...){ .Call("morloc_mlc_open_stdout",               ...) }
morloc_mlc_open_stdin                <- function(...){ .Call("morloc_mlc_open_stdin",                ...) }
morloc_mlc_open_stderr               <- function(...){ .Call("morloc_mlc_open_stderr",               ...) }
morloc_mlc_write                     <- function(...){ .Call("morloc_mlc_write",                     ...) }
morloc_mlc_append                    <- function(...){ .Call("morloc_mlc_append",                    ...) }
morloc_mlc_concat                    <- function(...){ .Call("morloc_mlc_concat",                    ...) }
morloc_mlc_flush                     <- function(...){ .Call("morloc_mlc_flush",                     ...) }
# @throw: raise a classed condition. The pool's manifold-level tryCatch
# (from ldErrorWrapOpen/Close in lang.yaml) catches all conditions and
# appends the frame info to `conditionMessage`, so the class tag survives
# only at the innermost frame; the message propagates outward.
morloc_mlc_throw <- function(msg) {
  stop(structure(
    class = c("MorlocException", "error", "condition"),
    list(message = msg, call = NULL)
  ))
}
# Raise a genuine morloc-invariant violation (compiler bug, contract
# violation, unreachable branch). Uses the MorlocInternalError class;
# morloc_mlc_catch inspects and re-raises so @catch cannot swallow it.
# The condition still derives from "error" so R's default handling
# prints a stacktrace; the class marker is what routes it past @catch.
morloc_mlc_internal_abort <- function(msg) {
  stop(structure(
    class = c("MorlocInternalError", "error", "condition"),
    list(message = paste0("morloc internal error (R pool): ", msg), call = NULL)
  ))
}
# @catch: evaluate fallible; on any error EXCEPT MorlocInternalError,
# evaluate fallback. MorlocInternalError bypasses -- genuine compiler
# bugs propagate past user @catch and terminate the pool.
morloc_mlc_catch <- function(fallible, fallback) {
  tryCatch(
    fallible(),
    error = function(e) {
      if (inherits(e, "MorlocInternalError")) stop(e)
      fallback()
    }
  )
}
morloc_socketpair                    <- function(...){ .Call("morloc_socketpair",                    ...) }
morloc_fork                          <- function(...){ .Call("morloc_fork",                          ...) }
morloc_send_fd                       <- function(...){ .Call("morloc_send_fd",                       ...) }
morloc_dispatch_fd                   <- function(...){ .Call("morloc_dispatch_fd",                   ...) }
morloc_reap_closes                   <- function(...){ .Call("morloc_reap_closes",                   ...) }
morloc_close_pending                 <- function( ){ .Call("morloc_close_pending"                 ) }
morloc_recv_fd                       <- function(...){ .Call("morloc_recv_fd",                       ...) }
morloc_kill                          <- function(...){ .Call("morloc_kill",                          ...) }
morloc_waitpid                       <- function(...){ .Call("morloc_waitpid",                       ...) }
morloc_install_sigterm_handler       <- function(...){ .Call("morloc_install_sigterm_handler",       ...) }
morloc_is_shutting_down              <- function(...){ .Call("morloc_is_shutting_down",              ...) }
morloc_waitpid_blocking              <- function(...){ .Call("morloc_waitpid_blocking",              ...) }
morloc_reap_worker                   <- function(...){ .Call("morloc_reap_worker",                   ...) }
morloc_detach_daemon                 <- function(...){ .Call("morloc_detach_daemon",                 ...) }
morloc_shared_counter_create         <- function(...){ .Call("morloc_shared_counter_create",         ...) }
morloc_shared_counter_inc            <- function(...){ .Call("morloc_shared_counter_inc",            ...) }
morloc_shared_counter_dec            <- function(...){ .Call("morloc_shared_counter_dec",            ...) }
morloc_shared_counter_read           <- function(...){ .Call("morloc_shared_counter_read",           ...) }
morloc_pipe                          <- function(...){ .Call("morloc_pipe",                          ...) }
morloc_write_byte                    <- function(...){ .Call("morloc_write_byte",                    ...) }
morloc_close_fd                      <- function(...){ .Call("morloc_close_fd",                      ...) }
morloc_worker_loop_c                 <- function(...){ .Call("morloc_worker_loop_c",                 ...) }
morloc_set_line_buffered             <- function(...){ .Call("morloc_set_line_buffered",             ...) }
morloc_exit                          <- function(...){ .Call("morloc_exit",                          ...) }
morloc_cache_key_compute             <- function(...){ .Call("r_morloc_cache_key_compute",           ...) }
morloc_debug_record_frame            <- function(...){ .Call("r_morloc_debug_record_frame",          ...) }
morloc_debug_flush_dispatch          <- function(  ){ .Call("r_morloc_debug_flush_dispatch"        ) }
morloc_cache_lookup                  <- function(...){ .Call("r_morloc_cache_lookup",                ...) }
morloc_cache_store                   <- function(...){ .Call("r_morloc_cache_store",                 ...) }
morloc_cache_record_hit              <- function( ){ .Call("r_morloc_cache_record_hit"             ) }
morloc_cache_record_miss             <- function( ){ .Call("r_morloc_cache_record_miss"            ) }
morloc_cache_record_store            <- function( ){ .Call("r_morloc_cache_record_store"           ) }

global_state <- list()

# Dynamic worker spawning: monkey-patch morloc_foreign_call to track busy workers.
# Workers atomically increment a shared counter before a foreign_call and
# decrement after. When all workers are busy, a byte is written to a wake-up
# pipe to tell the dispatcher to spawn a new worker.
.orig_foreign_call <- morloc_foreign_call
.busy_counter <- NULL
.wakeup_fd <- NULL
.n_workers_total <- 0L

morloc_foreign_call <- function(...) {
  val <- morloc_shared_counter_inc(.busy_counter)
  if (val >= .n_workers_total && !is.null(.wakeup_fd)) {
    tryCatch(morloc_write_byte(.wakeup_fd, as.raw(0x21)), error = function(e) NULL)
  }
  on.exit(morloc_shared_counter_dec(.busy_counter))
  .orig_foreign_call(...)
}

.mlc_wrap_log <- function(group, start_tmpl, pass_tmpl, fail_tmpl, fn) {
  # Eagerly resolve `fn` so the closure captures the ORIGINAL function. The
  # rebinding pattern `mN <- .mlc_wrap_log(..., mN)` reassigns the global
  # mN to this wrapper; without force(), R's lazy promise for `fn` only
  # resolves when first used inside the wrapper, by which point mN points
  # at the wrapper itself -- the wrapper calls itself, infinite recursion.
  force(group); force(start_tmpl); force(pass_tmpl); force(fail_tmpl); force(fn)
  function(...) {
    call_id <- .Call("r_morloc_log_next_id")
    t0 <- Sys.time()
    if (!is.null(start_tmpl)) {
      .Call("r_morloc_log_emit", start_tmpl, group, 0, call_id)
    }
    tryCatch({
      r <- fn(...)
      if (!is.null(pass_tmpl)) {
        dt <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
        .Call("r_morloc_log_emit", pass_tmpl, group, dt, call_id)
      }
      r
    }, error = function(e) {
      if (!is.null(fail_tmpl)) {
        dt <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
        .Call("r_morloc_log_emit", fail_tmpl, group, dt, call_id)
      }
      stop(e)
    })
  }
}

# Defunctionalized-closure support. An R closure produced by this pool carries
# its manifold id and captured values as attributes (attached at construction);
# when it crosses a language boundary it travels as the wire tuple
# (home_language, manifold_id, captured_packets) and is applied on the far side
# by calling back to this pool.

mlc_reify <- function(f, home_lang) {
  mid <- attr(f, "morloc_mid")
  captured <- attr(f, "morloc_captured")
  if (is.null(captured)) captured <- list()
  cap_schemas <- mlc_closure_table[[as.character(mid)]]
  if (is.null(cap_schemas)) cap_schemas <- list()
  packets <- lapply(seq_along(captured), function(i) morloc_put_value(captured[[i]], cap_schemas[[i]]))
  list(home_lang, as.integer(mid), packets)
}

# Rebuild a callable from an already-deserialized closure wire tuple
# (home_lang, mid, captured_packets), used when the closure is nested in an
# aggregate whose enclosing get_value has already parsed the tuple.
mlc_reflect_from_tuple <- function(tup, arg_schemas, res_schema) {
  home_lang <- tup[[1]]
  mid <- tup[[2]]
  captured <- tup[[3]]
  sock <- paste0(global_state$tmpdir, "/pipe-", home_lang)
  function(...) {
    args <- list(...)
    arg_packets <- lapply(seq_along(args), function(i) morloc_put_value(args[[i]], arg_schemas[[i]]))
    packets <- c(captured, arg_packets)
    morloc_get_value(morloc_foreign_call(sock, as.integer(mid), packets), res_schema)
  }
}

# Rebuild a callable from a raw incoming closure wire packet, used when the
# closure is the top-level crossing value (the whole packet is the tuple).
mlc_reflect <- function(pkt, tuple_schema, arg_schemas, res_schema) {
  mlc_reflect_from_tuple(morloc_get_value(pkt, tuple_schema), arg_schemas, res_schema)
}

mlc_make_closure_dispatch <- function(mid, arg_schemas, res_schema) {
  fn <- get(paste0("m", mid))
  function(...) {
    sargs <- list(...)
    args <- lapply(seq_along(sargs), function(i) morloc_get_value(sargs[[i]], arg_schemas[[i]]))
    morloc_put_value(do.call(fn, args), res_schema)
  }
}

# AUTO include manifolds start
# <<<BREAK>>>
# AUTO include manifolds end

# AUTO include dispatch start
# <<<BREAK>>>
# AUTO include dispatch end

worker_loop <- function(pipe_fd, ack_fd) {
  # If deferred source loading failed in this worker, make every dispatch entry
  # report the real cause as a clean fail packet. The C loop evaluates the
  # manifold via R_tryEvalSilent and returns the error text, so a stop() here
  # surfaces "there is no package called X" instead of a downstream
  # "could not find function ..." from a manifold referencing an unloaded symbol.
  # (Mirrors the Python pool's run_job _mlc_source_error guard.)
  if (!is.null(.mlc_source_error)) {
    fail_fn <- function(...) stop(.mlc_source_error)
    .dispatch <<- lapply(.dispatch, function(f) fail_fn)
    .remote_dispatch <<- lapply(.remote_dispatch, function(f) fail_fn)
  }
  morloc_worker_loop_c(pipe_fd, ack_fd, .dispatch, .remote_dispatch)
}

main <- function(socket_path, tmpdir, shm_basename) {
  # Force line-buffered stdout/stderr so output from user functions is not lost
  # when the nexus kills the pool process group.
  morloc_set_line_buffered()
  morloc_install_sigterm_handler()

  daemon <- morloc_start_daemon(socket_path, tmpdir, shm_basename, 0xffff)
  n_workers <- 1L

  # Shared job queue: dispatcher writes fds to fd[1], workers read from fd[2].
  # Only idle workers (blocked in recvmsg) pick up jobs, preventing the
  # round-robin deadlock where a callback gets dispatched to a busy worker.
  job_queue <- morloc_socketpair()

  # Shared counter for dynamic worker spawning
  busy_counter <- morloc_shared_counter_create()
  wakeup <- morloc_pipe()  # c(read_fd, write_fd)

  # Drain-ack pipe: a worker writes back the fd token once it has drained a
  # request off the socket, so the dispatcher can close its accepted copy
  # WITHOUT flushing unread request bytes (the macOS close-flushes-unread race).
  # Dispatcher reads ack_pipe[1L]; workers write ack_pipe[2L]. 4-byte tokens are
  # atomic pipe writes, so concurrent workers' acks never interleave.
  ack_pipe <- morloc_pipe()  # c(read_fd, write_fd)

  # Set globals so the monkey-patched morloc_foreign_call can use them.
  # Forked children inherit these values.
  .busy_counter <<- busy_counter
  .wakeup_fd <<- wakeup[2L]
  .n_workers_total <<- n_workers

  min_workers <- n_workers  # floor the pool never drops below (initial count)

  # Fork one worker: clean state -> load user sources -> serve. Returns its pid.
  spawn_worker <- function() {
    pid <- morloc_fork()
    if (pid == 0L) {
      morloc_detach_daemon(daemon)
      morloc_close_socket(job_queue[1L])  # child doesn't write fds
      morloc_close_fd(wakeup[1L])         # child doesn't read wakeup pipe
      morloc_close_fd(ack_pipe[1L])       # child doesn't read acks
      mlc_load_user_sources()             # load user sources post-fork
      worker_loop(job_queue[2L], ack_pipe[2L])
      morloc_exit(0L)
    }
    pid
  }

  # Reap dead workers (logging crashes), prune them from `pids`, and respawn up
  # to `min_workers`. Without respawn a crashing pool decays toward zero live
  # workers -- `send_fd` then black-holes every new client into a reader-less
  # queue and callers hang / see "Connection closed by peer". Pruning also keeps
  # the shutdown kill loop from SIGKILLing a reaped-and-reused pid. (The in-flight
  # job a crashed worker was serving is already lost; this restores future
  # service, not that job.)
  reap_and_respawn <- function(pids) {
    alive <- integer(0)
    for (pid in pids) {
      if (morloc_reap_worker(pid) == 0L) {
        alive <- c(alive, pid)
      }
    }
    while (length(alive) < min_workers) {
      alive <- c(alive, spawn_worker())
    }
    alive
  }

  pids <- integer(0)
  for (i in seq_len(n_workers)) {
    pids <- c(pids, spawn_worker())
  }
  # Keep job_queue[2L] open so dynamically spawned children can use it

  on.exit({
    # Close any accepted fds still awaiting a worker drain-ack (their workers
    # are about to be killed; the receive-buffer-flush is moot once the job is
    # abandoned).
    tryCatch(morloc_close_pending(), error = function(e) NULL)
    tryCatch(morloc_close_socket(job_queue[1L]), error = function(e) NULL)
    tryCatch(morloc_close_socket(job_queue[2L]), error = function(e) NULL)
    tryCatch(morloc_close_fd(wakeup[1L]), error = function(e) NULL)
    tryCatch(morloc_close_fd(wakeup[2L]), error = function(e) NULL)
    tryCatch(morloc_close_fd(ack_pipe[1L]), error = function(e) NULL)
    tryCatch(morloc_close_fd(ack_pipe[2L]), error = function(e) NULL)
    # Reap-and-log any final crash, then kill+reap the (pruned) live workers.
    for (pid in pids) {
      tryCatch(morloc_reap_worker(pid), error = function(e) NULL)
    }
    for (pid in pids) {
      if (pid > 0L) {
        tryCatch(morloc_kill(pid, 9L), error = function(e) NULL)
        tryCatch(morloc_waitpid_blocking(pid), error = function(e) NULL)
      }
    }
  })

  # Dispatch loop - idle workers pull from shared queue.
  # After each dispatch cycle, check if all workers are busy and spawn more.
  while (!morloc_is_shutting_down()) {
    # Reap dead workers, respawn to the floor, and keep the worker count honest
    # (a stale count would make the saturation gate below stop tripping).
    pids <- reap_and_respawn(pids)
    n_workers <- length(pids)
    .n_workers_total <<- n_workers

    # Close the dispatcher's accepted copy of any fd whose request a worker has
    # now drained (deferred from dispatch time to avoid the macOS
    # close-flushes-unread-request-bytes race). Cheap and non-blocking.
    morloc_reap_closes(ack_pipe[1L])

    client_fd <- morloc_wait_for_client(daemon)
    if (client_fd > 0L) {
      tryCatch({
        # Hand the fd to a worker and DEFER closing our copy: the worker acks
        # (over ack_pipe) once it has drained the request, and the next
        # reap_closes above releases the fd then.
        morloc_dispatch_fd(job_queue[1L], client_fd)
      }, error = function(e) {
        # The fd was never handed off; close it now (nothing to defer).
        cat(paste("Failed to dispatch job:", e$message, "\n"), file = stderr())
        morloc_close_socket(client_fd)
      })
    }

    # Dynamic worker spawning: if all workers are blocked in foreign_call,
    # spawn a new one so incoming callbacks can still be served. (A worker that
    # crashed mid-foreign-call leaves its busy increment un-decremented, so this
    # may over-count busy and over-spawn -- bounded and self-limiting, and far
    # safer than the under-spawn deadlock the reap_and_respawn count fix closes.)
    current_busy <- morloc_shared_counter_read(busy_counter)
    if (current_busy >= n_workers) {
      pids <- c(pids, spawn_worker())
      n_workers <- n_workers + 1L
      .n_workers_total <<- n_workers
    }
  }
}

args <- commandArgs(trailingOnly = TRUE)

# Health check: confirm sources loaded and print version
if (length(args) == 1 && args[1] == "--health") {
  # Actually load user sources so --health still validates that the program's
  # imports/sources resolve (they no longer run at script load).
  mlc_load_user_sources()
  if (!is.null(.mlc_source_error)) {
    cat('{"status":"error","version":"__MORLOC_VERSION__"}\n')
    quit(status = 1)
  }
  cat('{"status":"ok","version":"__MORLOC_VERSION__"}\n')
  quit(status = 0)
}

if (length(args) != 3) {
  cat("Usage: Rscript pool.R <socket_path> <tmpdir> <shm_basename>\n", file=stderr())
  quit(status = 1)
}

socket_path <- args[1]
tmpdir <- args[2]
shm_basename <- args[3]

global_state$tmpdir <- tmpdir

tryCatch(
  {
    main(socket_path, tmpdir, shm_basename)
  },  error = function(e) {
      stop(paste("Pool failed:", e$message))
  })

# Use _exit to avoid R cleanup which triggers heap corruption on glibc >= 2.39
# (R's finalizers attempt to free objects in SHM-related C extensions)
morloc_exit(0L)
