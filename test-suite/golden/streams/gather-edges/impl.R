produce_none <- function(sink) {
  invisible(NULL)
}

produce_fail <- function(sink) {
  sink(c(1, 2))
  sink(3)
  stop("producer failed after 2 batches")
}

produce_ten <- function(sink) {
  sink(c(0, 1, 2))
  sink(numeric(0))
  sink(3:9)
}

show_ints <- function(xs) {
  body <- vapply(as.character(xs), function(s) paste0(" ", s), "")
  paste0("r ", length(xs), ":", paste(body, collapse = ""), "\n")
}

mark_ran <- function(xs) {
  cat(length(xs), "\n", file = "handler-ran")
  "handler ran\n"
}
