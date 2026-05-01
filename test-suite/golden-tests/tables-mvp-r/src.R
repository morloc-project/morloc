makeT_r <- function(n) {
  arrow::record_batch(
    x = 0L:(n - 1L),
    y = as.character(0:(n - 1))
  )
}
