# AUTO include morloc imports start
# <<<BREAK>>>
# AUTO include morloc imports end

# AUTO load dynamic libraries
# <<<BREAK>>>
# AUTO load dynamic libraries

library(rlang)

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

global_state <- list()

# AUTO include manifolds start
# <<<BREAK>>>
# AUTO include manifolds end


run_job <- function(client_fd) {
  tryCatch({
    client_data <- morloc_stream_from_client(client_fd)

    is_local <- morloc_is_local_call(client_data)
    is_remote <- morloc_is_remote_call(client_data)

    if(morloc_is_ping(client_data)){
      result <- morloc_pong(client_data)
    }
    else if(is_local || is_remote){
      ext <- if(is_remote) { "_remote" } else { "" }
      call_packet <- morloc_read_morloc_call_packet(client_data)
      midx <- call_packet[[1]]
      args <- call_packet[[2]]

      mlc_pool_function_name <- paste0("m", midx, ext)

      if(exists(mlc_pool_function_name)){
        result <- tryCatch({
          mlc_pool_function <- get(mlc_pool_function_name)
          do.call(mlc_pool_function, args)
        }, error = function(e) {
          if(!is.null(e$fail_packet)) e$fail_packet else {
            morloc_make_fail_packet(e$message)
          }
        })
      } else {
        result <- morloc_make_fail_packet(paste("Function not found:", mlc_pool_function_name))
      }
    } else {
      result <- morloc_make_fail_packet("Unexpected packet type")
    }

    morloc_send_packet_to_foreign_server(client_fd, result)
  }, error = function(e) {
    tryCatch(
      morloc_send_packet_to_foreign_server(client_fd, morloc_make_fail_packet(paste("Job failed:", e$message))),
      error = function(e2) {}
    )
  }, finally = {
    morloc_close_socket(client_fd)
  })
}

worker_loop <- function(pipe_fd) {
  while (TRUE) {
    client_fd <- morloc_recv_fd(pipe_fd)
    if (client_fd < 0L) break
    run_job(client_fd)
  }
}

cleanup_workers <- function(pids, pipes) {
  # Close pipe ends to signal EOF to workers (causes worker_loop to break)
  for (i in seq_along(pipes)) {
    tryCatch(morloc_close_socket(pipes[[i]][1L]), error = function(e) NULL)
  }

  # SIGKILL all workers and blocking-reap to prevent zombies
  for (pid in pids) {
    if (pid > 0L) {
      tryCatch(morloc_kill(pid, 9L), error = function(e) NULL)  # SIGKILL
      tryCatch(morloc_waitpid_blocking(pid), error = function(e) NULL)
    }
  }
}

main <- function(socket_path, tmpdir, shm_basename) {
  morloc_install_sigterm_handler()

  daemon <- morloc_start_daemon(socket_path, tmpdir, shm_basename, 0xffff)
  n_workers <- max(1L, parallel::detectCores() - 1L)

  pipes <- lapply(seq_len(n_workers), function(i) morloc_socketpair())

  pids <- integer(n_workers)
  for (i in seq_len(n_workers)) {
    pid <- morloc_fork()
    if (pid == 0L) {
      # Child: close all parent-side fds
      for (j in seq_len(n_workers)) morloc_close_socket(pipes[[j]][1L])
      # Close child-side fds for other workers
      for (j in seq_len(n_workers)) {
        if (j != i) morloc_close_socket(pipes[[j]][2L])
      }
      worker_loop(pipes[[i]][2L])
      quit(status = 0, save = "no")
    }
    pids[i] <- pid
    morloc_close_socket(pipes[[i]][2L])  # Parent closes child end
  }

  on.exit(cleanup_workers(pids, pipes))

  # Main dispatch loop - round-robin
  current <- 1L
  while (!morloc_is_shutting_down()) {
    client_fd <- morloc_wait_for_client(daemon)
    if (client_fd > 0L) {
      tryCatch({
        morloc_send_fd(pipes[[current]][1L], client_fd)
        current <- (current %% n_workers) + 1L
      }, error = function(e) {
        cat(paste("Failed to dispatch job:", e$message, "\n"), file = stderr())
      }, finally = {
        morloc_close_socket(client_fd)  # Close parent's copy
      })
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
