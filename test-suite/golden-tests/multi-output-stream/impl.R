produce <- function(log, sink) {
  cat("ran\n", file = log, append = TRUE)
  sink(c(1L, 2L))
  sink(integer(0))
  sink(c(3L, 4L, 5L))
  sink(0:19999)
}

produce_tail <- function(sink) {
  sink(7L)
}

join_all <- function(xs) {
  paste0(length(xs), ": ", paste(head(xs, 8), collapse = ","), "\n")
}

show_batch <- function(xs) {
  paste0("batch of ", length(xs), "\n")
}

zero <- function() {
  list(0L, integer(0))
}

add_batch <- function(acc, xs) {
  list(acc[[1]] + sum(xs), c(acc[[2]], length(xs)))
}

merge <- function(a, b) {
  list(a[[1]] + b[[1]], c(a[[2]], b[[2]]))
}

show_acc <- function(acc) {
  paste0("sum=", format(acc[[1]], scientific = FALSE), " sizes=", paste(acc[[2]], collapse = ","), "\n")
}
