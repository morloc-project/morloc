fork_val <- function(x) {
  p <- parallel:::mcfork()
  if (inherits(p, "masterProcess")) return(x)
  parallel:::mccollect(p, wait = TRUE)
  x
}

note_outer <- function(x) {
  cat("note_outer\n", file = "calls.log", append = TRUE)
  x
}

fork_fail <- function(x) {
  p <- parallel:::mcfork()
  if (inherits(p, "masterProcess")) stop("the child fails")
  parallel:::mccollect(p, wait = TRUE)
  x
}

catching <- function(f, x) {
  tryCatch(f(x), error = function(e) {
    cat("caught\n", file = "calls.log", append = TRUE)
    -1L
  })
}
