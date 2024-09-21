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

