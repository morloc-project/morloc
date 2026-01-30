computeSummary <- function(values) {
  list(
    count = length(values),
    total = sum(values),
    mean = mean(values),
    stddev = sd(values),
    minVal = min(values),
    maxVal = max(values)
  )
}

correlationAnalysis <- function(x, y) {
  cor(x, y)
}
