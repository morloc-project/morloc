# AUTO include morloc imports start
# <<<BREAK>>>
# AUTO include morloc imports end

# AUTO load dynamic libraries
# <<<BREAK>>>
# AUTO load dynamic libraries

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
morloc_socketpair                    <- function(...){ .Call("morloc_socketpair",                    ...) }
morloc_fork                          <- function(...){ .Call("morloc_fork",                          ...) }
morloc_send_fd                       <- function(...){ .Call("morloc_send_fd",                       ...) }
morloc_recv_fd                       <- function(...){ .Call("morloc_recv_fd",                       ...) }
morloc_kill                          <- function(...){ .Call("morloc_kill",                          ...) }
morloc_waitpid                       <- function(...){ .Call("morloc_waitpid",                       ...) }
morloc_install_sigterm_handler       <- function(...){ .Call("morloc_install_sigterm_handler",       ...) }
morloc_is_shutting_down              <- function(...){ .Call("morloc_is_shutting_down",              ...) }
morloc_waitpid_blocking              <- function(...){ .Call("morloc_waitpid_blocking",              ...) }
morloc_detach_daemon                 <- function(...){ .Call("morloc_detach_daemon",                 ...) }
morloc_shared_counter_create         <- function(...){ .Call("morloc_shared_counter_create",         ...) }
morloc_shared_counter_inc            <- function(...){ .Call("morloc_shared_counter_inc",            ...) }
morloc_shared_counter_dec            <- function(...){ .Call("morloc_shared_counter_dec",            ...) }
morloc_shared_counter_read           <- function(...){ .Call("morloc_shared_counter_read",           ...) }
morloc_pipe                          <- function(...){ .Call("morloc_pipe",                          ...) }
morloc_write_byte                    <- function(...){ .Call("morloc_write_byte",                    ...) }
morloc_close_fd                      <- function(...){ .Call("morloc_close_fd",                      ...) }
morloc_worker_loop_c                 <- function(...){ .Call("morloc_worker_loop_c",                 ...) }

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

# AUTO include manifolds start
# <<<BREAK>>>
# AUTO include manifolds end

# AUTO include dispatch start
# <<<BREAK>>>
# AUTO include dispatch end

worker_loop <- function(pipe_fd) {
  morloc_worker_loop_c(pipe_fd, .dispatch, .remote_dispatch)
}

main <- function(socket_path, tmpdir, shm_basename) {
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

  # Set globals so the monkey-patched morloc_foreign_call can use them.
  # Forked children inherit these values.
  .busy_counter <<- busy_counter
  .wakeup_fd <<- wakeup[2L]
  .n_workers_total <<- n_workers

  pids <- integer(n_workers)
  for (i in seq_len(n_workers)) {
    pid <- morloc_fork()
    if (pid == 0L) {
      morloc_detach_daemon(daemon)
      morloc_close_socket(job_queue[1L])  # child doesn't write
      morloc_close_fd(wakeup[1L])         # child doesn't read wakeup pipe
      worker_loop(job_queue[2L])
      quit(status = 0, save = "no")
    }
    pids[i] <- pid
  }
  # Keep job_queue[2L] open so dynamically spawned children can use it

  on.exit({
    tryCatch(morloc_close_socket(job_queue[1L]), error = function(e) NULL)
    tryCatch(morloc_close_socket(job_queue[2L]), error = function(e) NULL)
    tryCatch(morloc_close_fd(wakeup[1L]), error = function(e) NULL)
    tryCatch(morloc_close_fd(wakeup[2L]), error = function(e) NULL)
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
    client_fd <- morloc_wait_for_client(daemon)
    if (client_fd > 0L) {
      tryCatch({
        morloc_send_fd(job_queue[1L], client_fd)
      }, error = function(e) {
        cat(paste("Failed to dispatch job:", e$message, "\n"), file = stderr())
      }, finally = {
        morloc_close_socket(client_fd)
      })
    }

    # Dynamic worker spawning: if all workers are blocked in foreign_call,
    # spawn a new one so incoming callbacks can still be served.
    current_busy <- morloc_shared_counter_read(busy_counter)
    if (current_busy >= n_workers) {
      pid <- morloc_fork()
      if (pid == 0L) {
        morloc_detach_daemon(daemon)
        morloc_close_socket(job_queue[1L])
        morloc_close_fd(wakeup[1L])
        worker_loop(job_queue[2L])
        quit(status = 0, save = "no")
      }
      pids <- c(pids, pid)
      n_workers <- n_workers + 1L
      .n_workers_total <<- n_workers
    }
  }
}

args <- commandArgs(trailingOnly = TRUE)
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
