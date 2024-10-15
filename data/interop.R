# takes a data packet and returns data that can be deserialized
.get_value <- function(key){
  header <- read_header(key)

  if (header$cmd[6] == PACKET_STATUS_FAIL){
    abort("Forwarding from .git_value", fail_packet = key)
  }

  data_start <- 32 + header$offset + 1

  if (header$cmd[1] == PACKET_TYPE_DATA) {
    if (header$cmd[2] == PACKET_SOURCE_MESG) {
      if(header$cmd[3] == PACKET_FORMAT_JSON) {
        json_data <- rawToChar(key[data_start:length(key)])
        return(json_data)
      } else {
        abort("Unsupported data format")
      }
    } else if (header$cmd[2] == PACKET_SOURCE_FILE) {
      if(header$cmd[3] == PACKET_FORMAT_JSON) {
        filename <- rawToChar(key[data_start:length(key)])
        json_data <- readChar(filename, file.info(filename)$size, useBytes=TRUE)
        return(json_data)
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

# takes serialized data and creates a data packet representing it
.put_value <- function(value){
  if(typeof(value) != "character"){
    abort("In put_value, expected input to be a character")
  }
  value_raw <- charToRaw(value)

  if (length(value_raw) <= 65536 - 32) {
    return(make_data(value_raw))

  } else {
    key <- paste0("/tmp/morloc_r_", sub("0.", "", as.character(runif(1))))

    .log(paste("Creating temporary file:", key))

    cat(value, file=key)

    .log(paste("Wrote data to:", key))

    key_raw <- charToRaw(key)

    return(make_data(key_raw, src = PACKET_SOURCE_FILE))
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
