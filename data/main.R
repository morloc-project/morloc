processMessage <- function(msg){

  data <- msg[[1]]

  header <- read_header(data)

  if(header$cmd[1] == PACKET_TYPE_PING){
    # reflect the ping
    return(data[1:msg[[2]]])

  } else if(header$cmd[1] == PACKET_TYPE_CALL) {
    .log("Processing call")
    manifold_id <- read_int(header$cmd, 2, 5)
    .log(paste("Manifold id =", manifold_id))

    .log("Building args")
    packet_length <- 32 + header$length + header$offset
    args <- list()
    arg_start <- 32 + header$offset + 1
    while(arg_start < packet_length){
      arg_header <- read_header(data[arg_start:(arg_start + 32 - 1)])
      arg_end = arg_start + 32 + arg_header$offset + arg_header$length - 1
      arg_data <- data[arg_start:arg_end]
      args[[length(args)+1]] <- arg_data
      arg_start <- arg_end + 1
    }
    .log("Args built")

    mlc_pool_function_name <- paste0("m", manifold_id)

    if(exists(mlc_pool_function_name)){
      .log(paste("Calling function:", mlc_pool_function_name))

      result <- tryCatch(
        {
          mlc_pool_function <- eval(parse(text=mlc_pool_function_name))
          do.call(mlc_pool_function, args)
        }, error = function(e) {
          errmsg <- paste("Call to", mlc_pool_function_name, "failed with message:", e$message) 
          fail_packet(errmsg)
        }
      )

      .log(paste("result header:", paste(result[1:32], collapse=" ")))
      .log(paste("result tail:", paste(tail(result, 8), collapse=" ")))
      .log(paste("result length:", length(result)))
    } else {
      errmsg = paste("Could not find function", mlc_pool_function_name)
      result <- fail_packet(errmsg)
    }

    return(result)
  } else {
    errmsg = "Unexpected packet type"
    return(fail_packet(errmsg))
  }
}


check_for_new_client <- function(queue, server_fd){
  # Accept a connection from the top available client if one exists
  client_fd <- .Call("R_accept_client", server_fd, ACCEPT_READ_TIME)

  if (client_fd == -1) {
    # An error occurred, don't worry about it
    return(queue)
  } else if (client_fd == -2) {
    # A timeout occurred, certainly don't worry about
    return(queue)
  }

  .log(paste("Accepted new client", client_fd))

  # Pull data from the client
  tryCatch({
    msg <- .Call("R_get", client_fd)
  }, error = function(e) {
    errmsg = paste(
      "Failed to read from socket with error message:",
      e$message
    )
    .log(errmsg)
    return(NULL)
  })

  .log(paste("Message received from client", client_fd, "with length", msg[[2]]))

  # If any data was pulled, operate on it
  if (msg[[2]] > 0) {
    .log(paste("job", length(queue)+1, "starts"))
    # Run the job in a newly forked process in the backgroun
    tryCatch(
      {
        work <- future::future(
          { tryCatch(
              { processMessage(msg) },
              error = function(e) {
                  errmsg <- paste("processMessage failed:", e$message)
                  fail_packet(errmsg)
              }
            )
          }
        )
      },
      error = function(e) {
        errmsg <- paste("Error preparing in future work:", e$message)
        abort(errmsg)
      }
    )

    # Add the job to the queue
    queue[[length(queue)+1]] <- list( client_fd = client_fd, work = work)
  } else {
    .log(paste("Message from client", client_df, "was empty, so no job was started"))
  }

  queue
}


job_has_finished <- function(job){
  future::resolved(job$work) 
}

handle_finished_client <- function(job){
  .log(paste("finishing client_fd = ", job$client_fd))

  # get the result of the calculation
  data <- tryCatch(
    {
      future::value(job$work)
    },
    error = function(e){
      fail_packet(paste("Error retrieving work from job:", e$message))
    }
  )
  .log(paste("type(data) =", typeof(data)))
  .log(paste("length(data) =", length(data)))

  # Return the result to the client
  .Call("R_send_data", job$client_fd, list(data, length(data)))

  # Close the current client
  .Call("R_close_socket", job$client_fd)
}

main <- function(socket_path) {
  # Setup a new server that uses a given path for the socket address
  server_fd <- .Call("R_new_server", socket_path)

  if (server_fd == -1) {
    .log("Error creating server")
    return(1)
  } else {
    .log(paste("Successfully starting server on socket", socket_path))
  }

  queue <- list()

  # Listen for clients
  tryCatch({
    while (TRUE) {
      queue <- check_for_new_client(queue, server_fd)

      job_idx = 1
      while(job_idx <= length(queue)){
        # check is the job has finished
        if(job_has_finished(queue[[job_idx]])){
          print(paste("job", job_idx, "is finished"))

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
    # Close the server
    .Call("R_close_socket", server_fd)

    # Remove the socket file
    unlink(socket_path)
  })

  # Exit with success
  return(0)
}


args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1) {
  cat("Usage: Rscript pipe.R <socket_path>\n", file=stderr())
  quit(status = 1)
}

socket_path <- args[1]
result <- main(socket_path)
quit(status = result)
