# AUTO include morloc imports start
# <<<BREAK>>>
# AUTO include morloc imports end

dyn.load("~/.morloc/lib/libsocketr.so")

dyn.load("~/.morloc/lib/librmorloc.so")

msgpack_pack <- function(obj, schema) {
    .Call("r_to_mesgpack", obj, schema)
}

msgpack_unpack <- function(packed, schema) {
    .Call("mesgpack_to_r", packed, schema)
}

shm_start <- function(shm_basename, shm_size){
    .Call("shm_start", shm_basename, shm_size)
}

shm_close <- function(){
    .Call("shm_close")
}

to_shm <- function(x, schema_str){
    .Call("to_shm", x, schema_str)
}

from_shm <- function(relptr, schema_str){
    .Call("from_shm", relptr, schema_str)
}


library(rlang)

global_state <- new.env()

PACKET_TYPE_DATA <- 0x00
PACKET_TYPE_CALL <- 0x01
PACKET_TYPE_PING <- 0x02
PACKET_TYPE_GET  <- 0x03
PACKET_TYPE_POST <- 0x04
PACKET_TYPE_PUT  <- 0x05
PACKET_TYPE_DEL  <- 0x06

PACKET_SOURCE_MESG <- 0x00 # the message contains the data
PACKET_SOURCE_FILE <- 0x01 # the message is a path to a file of data
PACKET_SOURCE_RPTR <- 0x02 # the message is a path to a file of data

PACKET_FORMAT_JSON     <- 0x00
PACKET_FORMAT_MSGPACK  <- 0x01
PACKET_FORMAT_TEXT     <- 0x02
PACKET_FORMAT_DATA     <- 0x03
PACKET_FORMAT_VOIDSTAR <- 0x04

PACKET_COMPRESSION_NONE <- 0x00 # uncompressed

PACKET_ENCRYPTION_NONE  <- 0x00 # unencrypted

PACKET_STATUS_PASS <- 0x00
PACKET_STATUS_FAIL <- 0x01

MAGIC = c(0x6D, 0xF8, 0x07, 0x07)

read_int <- function(raw, start, end){
  sum(as.numeric(raw[start:end]) * (256 ^ ((end - start):0)))
}

write_int <- function(x, n){
  as.raw((x %% (256 ^ (n:1))) %/% (256 ^ ((n-1):0)))
}

int64 <- function(x) write_int(x, 8)
int32 <- function(x) write_int(x, 4)
int16 <- function(x) write_int(x, 2)

read_header <- function(data){
  # 1:4 | 5:6 | 7:8 | 9:10 | 11:12 | 13:20 | 21:24 | 25:32
  # mag   pln   ver   flav   mode    cmd     offset  length

  if(any(data[1:4] != MAGIC)){
    .log("Failed to read header: bad magic") 
  }

  if(length(data) < 32){
    .log("Failed to read header: too short")
  }

  cmd = data[13:20]
  offset = read_int(data, 21, 24)
  length = read_int(data, 25, 32)

  return(list(cmd = cmd, offset = offset, length = length))
}

make_header <- function(cmd, offset, length){
  if(length(cmd) != 8){
    abort("failed to make header, expected command to be exactly 8 bytes long")
  }
  as.raw(c(
    MAGIC,
    int16(0),
    int16(0),
    int16(0),
    int16(0),
    cmd,
    int32(offset),
    int64(length)
  ))
}

make_data <- function(
  value,
  notes = vector(0, mode="raw"),
  src = PACKET_SOURCE_MESG,
  fmt = PACKET_FORMAT_MSGPACK,
  cmpr = PACKET_COMPRESSION_NONE,
  encr = PACKET_ENCRYPTION_NONE,
  status = PACKET_STATUS_PASS
){
    cmd = c(
      PACKET_TYPE_DATA,
      src,
      fmt,
      cmpr,
      encr,
      status,
      0x00,
      0x00
    )
    header <- make_header(cmd, offset = length(notes), length = length(value))
    return(c(header, notes, value))
}

fail_packet <- function(errmsg, errobj = NULL){
  if(is.null(errobj$fail_packet)){
    make_data(charToRaw(errmsg), status = PACKET_STATUS_FAIL, fmt = PACKET_FORMAT_TEXT)
  } else {
    header <- read_header(errobj$fail_packet)
    if(header$offset > 0){
      notes <- suberr[32:(32+header$offset)]
    } else {
      notes <- vector(0, mode="raw")
    }
    prior_error <- suberr[(32 + header$offset + 1):(32 + header$offset + header$length)]
    new_error <- c(prior_error, charToRaw("\n"), charToRaw(errmsg))
    make_data(new_error, notes, status = PACKET_STATUS_FAIL)
  }
}


# Number of seconds to wait during each loop
LISTENER_TICK = 0.001

# Number of seconds to listen to the pipe during each loop
ACCEPT_READ_TIME = 0.0001

# The parallelism strategy (see documentation for the R `future` package)
# NOTE: This will not work on Windows, but it is the fastest option for local
# compute on a UNIX machine. The other approaches, like multisession, fail
# terribly because they can't find the appropriate globals or dynamically linked
# functions.
#
# Without the workers option, R will spin up as many workers as there are
# cores. Each will need to have global state sent to them each time. This leads
# to an overhead of ~22ms on my system. With workers=2, the overhead falls to
# ~13ms. In both cases, though, forking is being done which is slow. Setting
# workers=1 does no forking and runs jobs sequentially. This drops overhead to
# ~1ms per job.
future::plan(future::multicore)


.log <- function(msg, logfile="log"){
  cat(paste0("R: ", msg, "\n"), file=logfile, append=TRUE)
}


# takes a data packet and returns data that can be deserialized
.get_value <- function(key, schema){
  header <- read_header(key)

  if (header$cmd[6] == PACKET_STATUS_FAIL){
    abort("Forwarding from .git_value", fail_packet = key)
  }

  data_start <- 32 + header$offset + 1

  if (header$cmd[1] == PACKET_TYPE_DATA) {
    if (header$cmd[2] == PACKET_SOURCE_MESG) {
      if(header$cmd[3] == PACKET_FORMAT_MSGPACK) {
        # return the value as inside the packet
        msgpack_unpack(key[data_start:length(key)], schema)
      } else {
        abort("Unsupported data format")
      }
    } else if (header$cmd[2] == PACKET_SOURCE_FILE) {
      if(header$cmd[3] == PACKET_FORMAT_MSGPACK) {
        # return the value from a file
        filename <- rawToChar(key[data_start:length(key)])
        msgpack_data <- readBin(con = filename, what = "raw", n = file.info(filename)$size)
        msgpack_unpack(msgpack_data, schema)
      } else {
        abort("Unsupported data format")
      }
    } else if (header$cmd[2] == PACKET_SOURCE_RPTR) {
      if(header$cmd[3] == PACKET_FORMAT_VOIDSTAR) {
        # read a 64 bit integer
        relptr <- read_int(key, data_start, data_start + 7)
        from_shm(relptr, schema)
      } else {
        abort("Unsupported data format")
      }

    } else {
      abort("Unsupported data source")
    }
  } else {
    abort("Expected data packet")
  }
}

# take arbitrary R data and creates a data packet representing it
.put_value <- function(value, schema, format="shm"){
  
  if(format == "shm"){
    relptr <- to_shm(value, schema)
    return(make_data(int64(relptr), src = PACKET_SOURCE_RPTR, fmt = PACKET_FORMAT_VOIDSTAR))
  } else if(format == "mesgpack") {
    value_raw <- msgpack_pack(value, schema)

    if (length(value_raw) <= 65536 - 32) {
      return(make_data(value_raw))
    } else {

      key <- tempfile(pattern = "r_",
                  tmpdir = global_state.tmpdir,
                  fileext = "")

      .log(paste("Creating temporary file:", key))

      writeBin(value_raw, con=key)

      .log(paste("Wrote data to:", key))

      key_raw <- charToRaw(key)

      return(make_data(key_raw, src = PACKET_SOURCE_FILE))
    }
  } else {
    abort("Unexpected put_value format")
  }
}


.morloc_foreign_call <- function(pool_pipe, manifold_id, arg_keys) {
  if(length(arg_keys) > 0){
    packet_length <- sum(sapply(arg_keys, length))
  } else {
    packet_length <- 0
  }

  cmd <- c(PACKET_TYPE_CALL, int32(manifold_id), 0x00, 0x00, 0x00)
  header <- make_header(cmd, offset = 0, length = packet_length)

  call_packet <- c(header, unlist(arg_keys))

  response <- tryCatch(
    {
      .log(paste("R_ask send with packet of length", length(call_packet), "to the pipe", pool_pipe))
      .log(paste("Packet: ", paste(call_packet, collapse=" ")))
      .Call("R_ask", pool_pipe, list(call_packet, length(call_packet)))
    },
    error = function(e) {
      abort(paste("R_ask failed:", e$message))
    }
  )

  tryCatch(
    {
      data_raw <- response[[1]]
      data_length <- response[[2]]
      data_raw[1:data_length]
    },
    error = function(e) {
      abort(paste("Malformed response from foreign call:", e$message))
    }
  )
}



# AUTO include manifolds start
# <<<BREAK>>>
# AUTO include manifolds end



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
          if(!is.null(e$fail_packet)){
            e$fail_packet
          } else {
              errmsg <- paste("Call to", mlc_pool_function_name, "in R failed with message:", e$message) 
              fail_packet(errmsg)
          }
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

main <- function(socket_path, shm_basename) {
  # Setup a new server that uses a given path for the socket address
  server_fd <- .Call("R_new_server", socket_path)

  # START SHM shm_basename

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
if (length(args) != 3) {
  cat("Usage: Rscript pipe.R <socket_path> <tmpdir> <shm_basename>\n", file=stderr())
  quit(status = 1)
}

tryCatch(
  {
    socket_path <- args[1]
    global_state.tmpdir <- args[2]
    shm_basename <- args[3]

    shm_start(shm_basename, 0xffff)

    result <- main(socket_path, shm_basename)
    quit(status = result)
  },  error = function(e) {
      .log(paste("Pool failed:", e$message))
  })
