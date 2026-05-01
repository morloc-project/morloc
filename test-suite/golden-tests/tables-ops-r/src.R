mkData <- function(n) {
  arrow::record_batch(x = as.integer(0:(n-1)), y = paste0("r", 0:(n-1)))
}
