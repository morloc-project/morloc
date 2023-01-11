#!/usr/bin/env Rscript

source("/home/z/.morloc/src/rbase/R/core.R")
source("/home/z/.morloc/src/rbase/data.R")

.morloc_run <- function(f, args){
  fails <- ""
  isOK <- TRUE
  warns <- list()
  notes <- capture.output(
    {
      value <- withCallingHandlers(
        tryCatch(
          do.call(f, args),
          error = function(e) {
            fails <<- e$message;
            isOK <<- FALSE
          }
        ),
        warning = function(w){
          warns <<- append(warns, w$message)
          invokeRestart("muffleWarning")
        }
      )
    },
    type="message"
  )
  list(
    value = value,
    isOK  = isOK,
    fails = fails,
    warns = warns,
    notes = notes
  )
}

# dies on error, ignores warnings and messages
.morloc_try <- function(f, args, .log=stderr(), .pool="_", .name="_"){
  x <- .morloc_run(f=f, args=args)
  location <- sprintf("%s::%s", .pool, .name)
  if(! x$isOK){
    cat("** R errors in ", location, "\n", file=stderr())
    cat(x$fails, "\n", file=stderr())
    stop(1)
  }
  if(! is.null(.log)){
    lines = c()
    if(length(x$warns) > 0){
      cat("** R warnings in ", location, "\n", file=stderr())
      cat(paste(unlist(x$warns), sep="\n"), file=stderr())
    }
    if(length(x$notes) > 0){
      cat("** R messages in ", location, "\n", file=stderr())
      cat(paste(unlist(x$notes), sep="\n"), file=stderr())
    }
  }
  x$value
}

.morloc_unpack <- function(unpacker, x, .pool, .name){
  x <- .morloc_try(f=unpacker, args=list(as.character(x)), .pool=.pool, .name=.name)
  return(x)
}

.morloc_foreign_call <- function(cmd, args, .pool, .name){
  .morloc_try(f=system2, args=list(cmd, args=args, stdout=TRUE), .pool=.pool, .name=.name)
}

# NOTE: bad inlining
# m3 <- function(x0)
# {
#     a1 <- .morloc_foreign_call("python3", list("pool.py", "18", x0), "_", "_")
#     a2 <- rmorlocinternals::mlc_deserialize(a1, '{"list":["numeric"]}');
#     a0 <- .morloc_foreign_call("python3", list("pool.py", "286", x0), "_", "_")
#     a3 <- mlc_fold(a0, 0.0, a2)
#     a4 <- rmorlocinternals::mlc_serialize(a3, '"numeric"')
#     a4
# }
m666 <- function(x,y){
    x1 <- rmorlocinternals::mlc_serialize(x, '"numeric"')
    y1 <- rmorlocinternals::mlc_serialize(y, '"numeric"')
    a0 <- .morloc_foreign_call("python3", list("pool.py", "286", x1, y1), "_", "_")
    a1 <- rmorlocinternals::mlc_deserialize(a0, '"numeric"')
    a1
}
m3 <- function(x0)
{
    a1 <- .morloc_foreign_call("python3", list("pool.py", "18", x0), "_", "_")
    a2 <- rmorlocinternals::mlc_deserialize(a1, '{"list":["numeric"]}');
    a3 <- mlc_fold(m666, 0.0, a2)
    a3
}

args <- as.list(commandArgs(trailingOnly=TRUE))
if(length(args) == 0){
  stop("Expected 1 or more arguments")
} else {
  cmdID <- args[[1]]
  f_str <- paste0("m", cmdID)
  if(exists(f_str)){
    f <- eval(parse(text=paste0("m", cmdID)))
    result <- do.call(f, args[-1])
    cat(result, "\n")
  } else {
    cat("Could not find manifold '", cmdID, "'\n", file=stderr())
  }
}
