# sum, mean and sd accumulate in long double, which is 80 bits on x86-64 and
# 64 on arm64; a left fold in double gives the same bits on both.
dsum <- function(x) Reduce(`+`, x, 0)

computeSummary <- function(values) {
  n <- length(values)
  m <- dsum(values) / n
  list(
    count = n,
    total = dsum(values),
    mean = m,
    stddev = sqrt(dsum((values - m)^2) / (n - 1)),
    minVal = min(values),
    maxVal = max(values)
  )
}

correlationAnalysis <- function(x, y) {
  cor(x, y)
}
