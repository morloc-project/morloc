# Throws when n == 0; otherwise returns 10.0 / n. Used to exercise
# the R debug-trace wrap with a deterministic exception.
rThrowAtZero <- function(n) {
  if (n == 0) {
    stop("rThrowAtZero: n must be non-zero")
  }
  10.0 / n
}
