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

.morloc_unpack <- function(unpacker, x, .pool, .name){
  x <- .morloc_try(f=unpacker, args=list(as.character(x)), .pool=.pool, .name=.name)
  return(x)
}
