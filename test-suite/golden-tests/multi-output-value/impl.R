build <- function(log, n) {
  cat("ran\n", file = log, append = TRUE)
  cat("building ", n, "\n", sep = "")
  as.integer((seq_len(n) - 1)^2)
}

as_lines <- function(xs) {
  paste0(paste0(xs, "\n"), collapse = "")
}

pad <- function(width, xs) {
  paste0(paste0(formatC(xs, width = width), "\n"), collapse = "")
}

sink_lines <- function(xs) {
  for (x in xs) cat("sink ", x, "\n", sep = "")
  invisible(NULL)
}

summary <- function(xs) {
  length(xs)
}
