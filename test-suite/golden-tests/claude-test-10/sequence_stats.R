calculateGC <- function(seq) {
  # Convert to uppercase
  seq <- toupper(seq)

  # Count G and C
  chars <- strsplit(seq, "")[[1]]
  gc_count <- sum(chars == "G" | chars == "C")
  total <- length(chars)

  gc_count / total
}

sequenceComplexity <- function(seq) {
  # Simple complexity measure: unique k-mers / total k-mers
  k <- 3
  chars <- strsplit(seq, "")[[1]]

  if (length(chars) < k) return(1.0)

  kmers <- character(length(chars) - k + 1)
  for (i in 1:(length(chars) - k + 1)) {
    kmers[i] <- paste(chars[i:(i+k-1)], collapse="")
  }

  unique_count <- length(unique(kmers))
  total_count <- length(kmers)

  unique_count / total_count
}

analyzeComposition <- function(seq) {
  seq <- toupper(seq)
  chars <- strsplit(seq, "")[[1]]

  gc <- sum(chars == "G" | chars == "C") / length(chars)
  at <- sum(chars == "A" | chars == "T") / length(chars)

  list(
    gcContent = gc,
    atContent = at,
    length = length(chars),
    complexity = sequenceComplexity(seq)
  )
}
