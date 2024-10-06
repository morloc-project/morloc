.get_value <- function(key){
  readChar(key, file.info(key)$size, useBytes=TRUE)
}

.put_value <- function(value){
  key <- paste0("/tmp/morloc_r_", sub("0.", "", as.character(runif(1))))

  .log(paste("Creating temporary file:", key))

  write(value, key)

  .log(paste("Wrote data to:", key))
  return(key)
}

.morloc_foreign_call <- function(pool_pipe, manifold_id, arg_keys){
  msg_str <- paste(c(manifold_id, arg_keys), collapse=" ") 
  msg_raw <- charToRaw(msg_str)

  .log(paste0("Sending message: '", msg_str, "'"))

  response <- .Call("R_ask", pool_pipe, list(msg_raw, length(msg_raw)))

  data_raw <- response[[1]]
  data_length <- response[[2]]

  response = rawToChar(data_raw[1:data_length])

  .log(paste0("From this message: '", msg_str, "' received the message '", response, "'"))

  response
}
