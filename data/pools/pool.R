# AUTO include morloc imports start
# <<<BREAK>>>
# AUTO include morloc imports end

# AUTO load dynamic libraries
# <<<BREAK>>>
# AUTO load dynamic libraries

library(rlang)
library(mirai)

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

global_state <- list()

# AUTO include manifolds start
# <<<BREAK>>>
# AUTO include manifolds end


# Pure computation on raw vectors - runs in mirai worker
compute_job <- function(client_data) {
  tryCatch({
    is_local <- morloc_is_local_call(client_data)
    is_remote <- morloc_is_remote_call(client_data)

    if(morloc_is_ping(client_data)){
      morloc_pong(client_data)
    }
    else if(is_local || is_remote){
      ext <- if(is_remote) { "_remote" } else { "" }
      call_packet <- morloc_read_morloc_call_packet(client_data)
      midx <- call_packet[[1]]
      args <- call_packet[[2]]

      mlc_pool_function_name <- paste0("m", midx, ext)

      if(exists(mlc_pool_function_name)){
        tryCatch({
          mlc_pool_function <- get(mlc_pool_function_name)
          do.call(mlc_pool_function, args)
        }, error = function(e) {
          if(!is.null(e$fail_packet)) e$fail_packet else {
            morloc_make_fail_packet(e$message)
          }
        })
      } else {
        morloc_make_fail_packet(paste("Function not found:", mlc_pool_function_name))
      }
    } else {
      morloc_make_fail_packet("Unexpected packet type")
    }
  }, error = function(e) {
    morloc_make_fail_packet(paste("Job failed:", e$message))
  })
}

# Main event loop
main <- function(socket_path, tmpdir, shm_basename) {
  daemon <- morloc_start_daemon(socket_path, tmpdir, shm_basename, 0xffff)

  n_workers <- max(1L, parallel::detectCores() - 1L)
  daemons(n = n_workers)

  file_args <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  script_path <- normalizePath(sub("^--file=", "", file_args[1]))

  everywhere(
    {
      .morloc_worker_mode <<- TRUE
      source(script_path, local = FALSE)
      morloc_shinit(shm_basename, 0L, as.integer(0xffff))
      global_state$tmpdir <<- tmpdir
    },
    script_path = script_path,
    shm_basename = shm_basename,
    tmpdir = tmpdir
  )

  pending <- list()

  while (TRUE) {
    # Accept new client connections
    client_fd <- morloc_wait_for_client(daemon)
    if(client_fd > 0L) {
      client_data <- morloc_stream_from_client(client_fd)
      m <- mirai(compute_job(client_data), client_data = client_data)
      pending[[length(pending) + 1L]] <- list(mirai = m, client_fd = client_fd)
    }

    # Poll completed tasks and send responses
    keep <- rep(TRUE, length(pending))
    for(i in seq_along(pending)) {
      if(!unresolved(pending[[i]]$mirai)) {
        fd <- pending[[i]]$client_fd
        tryCatch({
          result <- pending[[i]]$mirai$data
          if(is_error_value(result)) {
            response <- morloc_make_fail_packet(as.character(result))
          } else {
            response <- result
          }
          morloc_send_packet_to_foreign_server(fd, response)
        }, error = function(e) {
          tryCatch(
            morloc_send_packet_to_foreign_server(fd, morloc_make_fail_packet(paste("Send failed:", e$message))),
            error = function(e2) {}
          )
        }, finally = {
          morloc_close_socket(fd)
        })
        keep[i] <- FALSE
      }
    }

    if(!all(keep)) {
      pending <- pending[keep]
    }
  }

  daemons(0)
  return(0)
}


if (!exists(".morloc_worker_mode") || !isTRUE(.morloc_worker_mode)) {
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
      exit_code <- main(socket_path, tmpdir, shm_basename)
      quit(status = exit_code)
    },  error = function(e) {
        stop(paste("Pool failed:", e$message))
    })
}
