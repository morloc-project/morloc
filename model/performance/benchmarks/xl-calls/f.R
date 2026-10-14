inc <- function(x) x + 1L

mapf <- function(f, n) {
  s <- bit64::as.integer64(0L)
  for (i in seq_len(as.integer(n)) - 1L) s <- s + f(bit64::as.integer64(i))
  s
}

native <- function(n) {
  s <- bit64::as.integer64(0L)
  for (i in seq_len(as.integer(n)) - 1L) s <- s + inc(bit64::as.integer64(i))
  s
}
