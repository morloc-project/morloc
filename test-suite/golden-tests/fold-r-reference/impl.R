ZERO <- new.env()
ZERO$n <- 0

zero <- function() ZERO

produce <- function(sink) {
  sink(1L)
  sink(2L)
  sink(3L)
  invisible(NULL)
}

produce_fork <- function(sink) {
  sink(1L)
  job <- parallel::mcparallel({ sink(2L); TRUE })
  parallel::mccollect(job)
  invisible(NULL)
}

count_into <- function(acc, xs) {
  acc$n <- acc$n + sum(as.numeric(xs))
  acc
}

merge <- function(a, b) {
  a$n <- a$n + b$n
  a
}

show_counter <- function(acc) paste0("r n=", acc$n, " init=", ZERO$n, "\n")

pack_counter <- function(n) {
  acc <- new.env()
  acc$n <- n
  acc
}

unpack_counter <- function(acc) acc$n
