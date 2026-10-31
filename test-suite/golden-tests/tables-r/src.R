mkData <- function(n) {
  # ops fixture: x = 0..n-1, y = "r0".."r(n-1)"
  arrow::record_batch(x = seq_len(as.integer(n)) - 1L, y = paste0("r", seq_len(as.integer(n)) - 1L))
}

makeT_r <- function(n) {
  # mvp / rbind fixture: y = str(i)
  arrow::record_batch(
    x = seq_len(as.integer(n)) - 1L,
    y = as.character(seq_len(as.integer(n)) - 1L)
  )
}

makeXR <- function(n) {
  arrow::record_batch(x = seq_len(as.integer(n)) - 1L)
}

makeYR <- function(n) {
  arrow::record_batch(y = as.character(seq_len(as.integer(n)) - 1L))
}
