# AUTO include morloc imports start
# <<<BREAK>>>
# AUTO include morloc imports end

# AUTO load dynamic libraries
# <<<BREAK>>>
# AUTO load dynamic libraries

library(rlang)
library(parallel)

morloc_is_ping                       <- function(...){ .Call("morloc_is_ping",                       ...) }
morloc_pong                          <- function(...){ .Call("morloc_pong",                          ...) }
morloc_is_call                       <- function(...){ .Call("morloc_is_call",                       ...) }
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

LISTENER_TICK <- 0.01

global_state <- list()

# AUTO include manifolds start
# <<<BREAK>>>
# AUTO include manifolds end



run_job <- function(client_fd) {
  tryCatch({
    client_data <- morloc_stream_from_client(client_fd)
    
    if(morloc_is_ping(client_data)){
      response <- morloc_pong(client_data)
    }
    else if(morloc_is_call(client_data)){
      call_packet <- morloc_read_morloc_call_packet(client_data)
      midx <- call_packet[[1]]
      args <- call_packet[[2]]
      
      mlc_pool_function_name <- paste0("m", midx)
      
      if(exists(mlc_pool_function_name)){
        response <- tryCatch({
          mlc_pool_function <- get(mlc_pool_function_name)
          do.call(mlc_pool_function, args)
        }, error = function(e) {
          if(!is.null(e$fail_packet)) e$fail_packet else {
            morloc_make_fail_packet(paste("Call failed:", e$message))
          }
        })
      } else {
        response <- morloc_make_fail_packet(paste("Function not found:", mlc_pool_function_name))
      }
    } else {
      response <- morloc_make_fail_packet("Unexpected packet type")
    }
    
    morloc_send_packet_to_foreign_server(client_fd, response)
  }, error = function(e) {
    errmsg <- paste("Job failed:", e$message)
    morloc_send_packet_to_foreign_server(client_fd, morloc_make_fail_packet(errmsg))
  }, finally = {
    morloc_close_socket(client_fd)
  })
}

# Listen for clients
main <- function(socket_path, tmpdir, shm_basename) {
  # Initialize the R daemon
  daemon <- morloc_start_daemon(socket_path, tmpdir, shm_basename, 0xffff)
  
  while (TRUE) {
    client_fd <- morloc_wait_for_client(daemon)
    if(client_fd > 0) {
      mcparallel(
        { run_job(client_fd) },
        detached = TRUE
      )
      morloc_close_socket(client_fd)
    }
  }
  
  return(0)
}


args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 3) {
  cat("Usage: Rscript pipe.R <socket_path> <tmpdir> <shm_basename>\n", file=stderr())
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
