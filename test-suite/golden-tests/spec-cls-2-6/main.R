rsum <- function(v) {
  stopifnot(is.numeric(v))
  as.integer(sum(v))
}
