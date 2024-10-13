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

      tryCatch({
        mlc_pool_function <- eval(parse(text=mlc_pool_function_name))
        result <- do.call(mlc_pool_function, args)
        .log(paste("Successfully ran mid", manifold_id))
      }, error = function(e) {
        errmsg = paste(
          "Call to function",
          mlc_pool_function_name,
          "failed with message:",
          e$message
        )
        .log(errmsg)
        return(make_data(errmsg, status = PACKET_STATUS_FAIL))
      })

      .log(paste("Got result:", result))
    } else {
      stop(paste("Could not find function", mlc_pool_function))
    }

    return(result)
  } else {
    stop("Unexpected packet type")
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
    work <- future::future({ processMessage(msg) })

    # Add the job to the queue
    queue[[length(queue)+1]] <- list( client_fd = client_fd, data = data, work = work)
  } else {
    .log(paste("Message from client", client_df, "was empty, so no job was started"))
  }

  queue
}


job_has_finished <- function(job){
  future::resolved(job$work) 
}

handle_finished_client <- function(job){
  # get the result of the calculation
  data <- future::value(job$work)
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
