makeXR <- function(n) {
  arrow::record_batch(x = 0L:(n - 1L))
}

makeYR <- function(n) {
  arrow::record_batch(y = as.character(0:(n - 1)))
}
