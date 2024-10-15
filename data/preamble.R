library(rlang)

PACKET_TYPE_DATA <- 0x00
PACKET_TYPE_CALL <- 0x01
PACKET_TYPE_GET  <- 0x02
PACKET_TYPE_PUT  <- 0x03
PACKET_TYPE_PING <- 0x04

PACKET_SOURCE_MESG <- 0x00 # the message contains the data
PACKET_SOURCE_FILE <- 0x01 # the message is a path to a file of data
PACKET_SOURCE_NXDB <- 0x02 # the message is a key to the nexus uses to access the data

PACKET_FORMAT_JSON <- 0x00

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
  fmt = PACKET_FORMAT_JSON,
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
    make_data(charToRaw(errmsg), status = PACKET_STATUS_FAIL)
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
future::plan(future::multicore)


.log <- function(msg, logfile="log"){
  cat(paste0("R: ", msg, "\n"), file=logfile, append=TRUE)
}
