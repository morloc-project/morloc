# takes a data packet and returns data that can be deserialized
.get_value <- function(key){
  header <- read_header(key)

  data_start <- 32 + header$offset + 1

  if (header$cmd[1] == PACKET_TYPE_DATA) {
    if (header$cmd[2] == PACKET_SOURCE_MESG) {
      if(header$cmd[3] == PACKET_FORMAT_JSON) {
        json_data <- rawToChar(key[data_start:length(key)])
        return(json_data)
      } else {
        stop("Unsupported data format")
      }
    } else if (header$cmd[2] == PACKET_SOURCE_FILE) {
      if(header$cmd[3] == PACKET_FORMAT_JSON) {
        filename <- key[data_start:length(key)]
        json_data <- readChar(filename, file.info(key)$size, useBytes=TRUE)
        return(json_data)
      } else {
        stop("Unsupported data format")
      }
    } else {
      stop("Unsupported data source")
    }
  } else {
    stop("Expected data packet")
  }
}

# takes serialized data and creates a data packet representing it
.put_value <- function(value){
  value_raw <- charToRaw(value)

  if (length(value_raw) <= 65536 - 32) {
    cmd = c(
      PACKET_TYPE_DATA,
      PACKET_SOURCE_MESG,
      PACKET_FORMAT_JSON,
      PACKET_COMPRESSION_NONE,
      PACKET_ENCRYPTION_NONE,
      0x00,
      0x00,
      0x00
    )
    header <- make_header(cmd, offset = 0, length = length(value_raw))
    return(c(header, value_raw))
  } else {
    key <- paste0("/tmp/morloc_r_", sub("0.", "", as.character(runif(1))))

    .log(paste("Creating temporary file:", key))

    write(value, key)

    .log(paste("Wrote data to:", key))

    key_raw = charToRaw(key)

    cmd = c(
      PACKET_TYPE_DATA,
      PACKET_SOURCE_FILE,
      PACKET_FORMAT_JSON,
      PACKET_COMPRESSION_NONE,
      PACKET_ENCRYPTION_NONE,
      0x00,
      0x00,
      0x00
    )
    header <- make_header(cmd, offset = 0, length = length(key_raw))
    return(c(header, key_raw))
  }
}


.morloc_foreign_call <- function(pool_pipe, manifold_id, arg_keys){
  # msg_str <- paste(c(manifold_id, arg_keys), collapse=" ")
  # msg_raw <- charToRaw(msg_str)
  #
  # .log(paste0("Sending message: '", msg_str, "'"))
  #
  # response <- .Call("R_ask", pool_pipe, list(msg_raw, length(msg_raw)))
  #
  # data_raw <- response[[1]]
  # data_length <- response[[2]]
  #
  # response = rawToChar(data_raw[1:data_length])
  #
  # .log(paste0("From this message: '", msg_str, "' received the message '", response, "'"))
  #
  # response
  "ladida"
}
