slow <- function(path) {
  writeLines("started", path)
  Sys.sleep(60)
  1L
}

seqR <- function(n) rep(1L, n)

sumR <- function(xs) as.integer(sum(unlist(xs)))
