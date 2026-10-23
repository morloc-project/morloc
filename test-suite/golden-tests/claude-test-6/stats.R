# sum, mean and sd accumulate in long double, which is 80 bits on x86-64 and
# 64 on arm64; a left fold in double gives the same bits on both.
dsum <- function(x) Reduce(`+`, x, 0)

computeColorStats <- function(pixels) {
  # Flatten the matrix
  flat <- unlist(pixels)
  n <- length(flat)

  # For grayscale, all channels are the same
  mean_val <- dsum(flat) / n
  sd_val <- sqrt(dsum((flat - mean_val)^2) / (n - 1))

  # Return as list (morloc record)
  list(
    meanRed = mean_val,
    meanGreen = mean_val,
    meanBlue = mean_val,
    stddevRed = sd_val,
    stddevGreen = sd_val,
    stddevBlue = sd_val
  )
}
