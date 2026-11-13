produce_rows <- function(b, sink) {
  for (i in seq_len(as.integer(b)) - 1) {
    sink(lapply(seq_len(i) - 1, function(j) c(i * 10 + j, j)))
  }
}

produce_strs <- function(b, sink) {
  for (i in seq_len(as.integer(b)) - 1) {
    batch <- vapply(seq_len(i) - 1, function(j) sprintf("s%d %d", i, j), "")
    if (i == 1) batch <- c(batch, "")
    sink(batch)
  }
}

show_rows <- function(xs) {
  body <- vapply(xs, function(r) paste0(" ", paste(as.character(r), collapse = ",")), "")
  paste0("rows ", length(xs), ":", paste(body, collapse = ""), "\n")
}

show_strs <- function(xs) {
  body <- vapply(xs, function(s) paste0(" [", s, "]"), "")
  paste0("strs ", length(xs), ":", paste(body, collapse = ""), "\n")
}
