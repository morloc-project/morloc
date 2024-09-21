# Number of seconds to wait during each loop
LISTENER_TICK = 0.001

# Number of seconds to listen to the pipe during each loop
ACCEPT_READ_TIME = 0.0001

# The parallelism strategy (see documentation for the R `future` package)
# NOTE: This will not work on Windows, but it is the fastest option for local
# compute on a UNIX machine
future::plan(future::multicore)


.log <- function(msg, logfile="log"){
  cat(paste0("R: ", msg, "\n"), file=logfile, append=TRUE)
}

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
            fails <<- e$message
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
.morloc_try <- function(f, args, .pool="_", .name="_"){
  x <- .morloc_run(f=f, args=args)
  location <- sprintf("%s::%s", .pool, .name)
  if(! x$isOK){
    .log(paste("** R errors in", location))
    .log(x$fails)
    stop(1)
  }
  if(! is.null(.log)){
    lines = c()
    if(length(x$warns) > 0){
      .log(paste("** R warnings in", location))
      .log(paste(unlist(x$warns), sep="\n"))
    }
    if(length(x$notes) > 0){
      .log(paste("** R messages in ", location))
      .log(paste(unlist(x$notes), sep="\n"))
    }
  }
  x$value
}
