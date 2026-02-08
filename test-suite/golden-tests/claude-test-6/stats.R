computeColorStats <- function(pixels) {
  # Flatten the matrix
  flat <- unlist(pixels)

  # For grayscale, all channels are the same
  mean_val <- mean(flat)
  sd_val <- sd(flat)

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
