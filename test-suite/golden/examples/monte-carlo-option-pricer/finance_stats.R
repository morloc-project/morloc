analyzeDistribution <- function(prices) {
  meanVal <- mean(prices)
  medianVal <- median(prices)
  stddevVal <- sd(prices)
  p5 <- quantile(prices, 0.05)
  p95 <- quantile(prices, 0.95)

  # 95% confidence interval for mean
  se <- sd(prices) / sqrt(length(prices))
  ci_lower <- meanVal - 1.96 * se
  ci_upper <- meanVal + 1.96 * se

  list(
    mean = meanVal,
    median = medianVal,
    stddev = stddevVal,
    percentile5 = as.numeric(p5),
    percentile95 = as.numeric(p95),
    ciLower = ci_lower,
    ciUpper = ci_upper
  )
}

confidenceInterval <- function(values, alpha) {
  n <- length(values)
  meanVal <- mean(values)
  se <- sd(values) / sqrt(n)
  z <- qnorm(1 - alpha/2)

  list(meanVal - z * se, meanVal + z * se)
}

percentiles <- function(values, probs) {
  quantile(values, probs)
}
