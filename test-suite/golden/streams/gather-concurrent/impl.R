produce_forks <- function(nchildren, big, sink) {
  sink(0)
  status <- parallel::mclapply(seq_len(as.integer(nchildren)), function(c) {
    sink(c(10 * c + 1, 10 * c + 2))
    sink(10 * c + 3)
    if (big > 0) sink(rep(7, as.integer(big)))
    0L
  }, mc.cores = as.integer(nchildren))
  if (!all(vapply(status, identical, TRUE, 0L))) sink(-1)
  sink(99)
}

sorted_ints <- function(xs) {
  body <- vapply(as.character(sort(as.numeric(xs))), function(s) paste0(" ", s), "")
  paste0("r", paste(body, collapse = ""), "\n")
}

count <- function(xs) {
  paste0("r n=", length(xs), "\n")
}
