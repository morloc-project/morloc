mkData <- function(n) {
  # ops fixture: x = 0..n-1, y = "r0".."r(n-1)"
  arrow::record_batch(x = as.integer(0:(n-1)), y = paste0("r", 0:(n-1)))
}

makeT_r <- function(n) {
  # mvp / rbind fixture: y = str(i)
  arrow::record_batch(
    x = 0L:(n - 1L),
    y = as.character(0:(n - 1))
  )
}

makeXR <- function(n) {
  arrow::record_batch(x = 0L:(n - 1L))
}

makeYR <- function(n) {
  arrow::record_batch(y = as.character(0:(n - 1)))
}
