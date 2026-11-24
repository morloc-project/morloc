seq_p <- function(sink) {
  sink(c(1L, 2L))
  p <- parallel::mcparallel({ sink(5L); NULL })
  parallel::mccollect(p)
  sink(3L)
  invisible(NULL)
}

returns_p <- function(sink) {
  sink(1L)
  pid <- parallel:::mcfork()
  if (inherits(pid, "masterProcess")) {
    sink(5L)
    return(invisible(NULL))
  }
  parallel::mccollect(pid)
  sink(3L)
  invisible(NULL)
}

mc_p <- function(sink) {
  parallel::mclapply(1:4, function(i) { sink(i); 0L }, mc.cores = 2L)
  invisible(NULL)
}

raises_p <- function(sink) {
  sink(1L)
  pid <- parallel:::mcfork()
  if (inherits(pid, "masterProcess")) {
    sink(5L)
    stop("the child raises")
  }
  parallel::mccollect(pid)
  sink(3L)
  invisible(NULL)
}
