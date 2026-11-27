computeSentiment <- function(tokens) {
  # Simple sentiment based on word lists
  positive_words <- c("great", "wonderful", "amazing", "good", "happy", "excellent", "fantastic")
  negative_words <- c("terrible", "bad", "awful", "horrible", "sad", "miserable", "poor", "cold", "rainy")

  pos_count <- sum(tokens %in% positive_words)
  neg_count <- sum(tokens %in% negative_words)
  total <- length(tokens)

  positive_score <- pos_count / total
  negative_score <- neg_count / total
  neutral_score <- 1 - positive_score - negative_score
  overall <- positive_score - negative_score

  list(
    positive = positive_score,
    negative = negative_score,
    neutral = neutral_score,
    overall = overall
  )
}

wordDiversity <- function(tokens) {
  # Type-token ratio (unique words / total words)
  unique_count <- length(unique(tokens))
  total_count <- length(tokens)
  unique_count / total_count
}
