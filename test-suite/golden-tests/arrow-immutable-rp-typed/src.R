makeLargeTable <- function(n) {
  arrow::record_batch(
    idx = 0L:(n - 1L),
    value = as.double(0:(n - 1)) * 0.5
  )
}

makeIndices <- function(n) {
  as.integer(0:(n - 1))
}

sumReals <- function(xs) {
  sum(xs)
}
