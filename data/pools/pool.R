# AUTO include morloc imports start
# <<<BREAK>>>
# AUTO include morloc imports end

# AUTO load dynamic libraries
# <<<BREAK>>>
# AUTO load dynamic libraries

library(rlang)

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


run_job <- function(client_data){

  if(morloc_is_ping(client_data)){
    # create and return a response to the ping
    return( morloc_pong(client_data) )
  }
  
  if(!morloc_is_call(client_data)){
    # fail if this isn't a call packet
    errmsg = "Unexpected packet type"
    return( morloc_make_fail_packet(errmsg) )
  }

  call_packet <- morloc_read_morloc_call_packet(client_data)
  midx <- call_packet[[1]]
  args <- call_packet[[2]]

  mlc_pool_function_name <- paste0("m", midx)
  
  if(exists(mlc_pool_function_name)){
  
    result <- tryCatch(
      {
        mlc_pool_function <- eval(parse(text=mlc_pool_function_name))
        do.call(mlc_pool_function, args)
      }, error = function(e) {
        if(!is.null(e$fail_packet)){
          e$fail_packet
        } else {
            errmsg <- paste("Call to", mlc_pool_function_name, "in R failed with message:", e$message) 
            morloc_make_fail_packet(errmsg)
        }
      }
    )
    return(result)
  
  } else {
    errmsg <- paste("Could not find function", mlc_pool_function_name)
    return( morloc_make_fail_packet(errmsg) )
  }
}


check_for_new_client <- function(queue, daemon){

  client_fd <- morloc_wait_for_client(daemon)

  if (client_fd == -1) {
    # An error occurred, don't worry about it
    return(queue)
  } else if (client_fd == -2) {
    # A timeout occurred, certainly don't worry about
    return(queue)
  }

  # Pull data from the client
  tryCatch({
    packet <- morloc_stream_from_client(client_fd)
  }, error = function(e) {
    errmsg = paste(
      "Failed to read from socket with error message:",
      e$message
    )
    abort(errmsg)
  })

  # If any data was pulled, operate on it
  if (length(packet) > 0) {
    # Run the job in a newly forked process in the backgroun
    tryCatch(
      {
        work <- future::future(
          { tryCatch(
              { run_job(packet) },
              error = function(e) {
                  errmsg <- paste("run_job failed:", e$message)
                  morloc_make_fail_packet(errmsg)
              }
            )
          }, globals=FALSE
        )
      },
      error = function(e) {
        errmsg <- paste("Error preparing in future work:", e$message)
        abort(errmsg)
      }
    )

    # Add the job to the queue
    queue[[length(queue)+1]] <- list( client_fd = client_fd, work = work)
  }

  queue
}


job_has_finished <- function(job){
  future::resolved(job$work) 
}

handle_finished_client <- function(job){
  # get the result of the calculation
  data <- tryCatch(
    {
      future::value(job$work)
    },
    error = function(e){
      morloc_make_fail_packet(paste("Error retrieving work from job:", e$message))
    }
  )
  # Return the result to the client
  morloc_send_packet_to_foreign_server(job$client_fd, data)

  # Close the current client
  morloc_close_socket(job$client_fd)
}


main <- function(socket_path, tmpdir, shm_basename) {

  # Listen for clients
  tryCatch({
    # Initialize the R daemon
    daemon = morloc_start_daemon(socket_path, tmpdir, shm_basename, 0xffff)

    queue <- list()

    while (TRUE) {
      queue <- check_for_new_client(queue, daemon)

      job_idx = 1
      while(job_idx <= length(queue)){
        # check is the job has finished
        if(job_has_finished(queue[[job_idx]])){
          # send data back to the client and close the socket
          handle_finished_client(queue[[job_idx]])

          # remove this completed job from the queue
          # job_idx will now point to the next job
          queue[[job_idx]] <- NULL
        } else {
          # if this job is still running, move onto the next one
          job_idx <- job_idx + 1
        }
      }

      # sleep to avoid busy waiting
      Sys.sleep(LISTENER_TICK)
    }
  }, finally = {
      # no extra cleanup is needed just yet
      NULL
  })

  # Exit with success
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
