r_len <- function(s) {
  nchar(s)
}

r_may_fail <- function(s) {
  if (nchar(s) == 0) {
    stop("r_may_fail: empty input")
  }
  nchar(s)
}
