produceR <- function(path, sink) {
  batch <- c()
  for (line in readLines(path)) {
    if (line == "bad") stop(paste("bad line in", path))
    batch <- c(batch, as.integer(line))
    if (length(batch) == 2) {
      sink(batch)
      batch <- c()
    }
  }
  if (length(batch) > 0) sink(batch)
  writeLines("done", paste0(path, ".done"))
}

doneR <- function(path) file.exists(paste0(path, ".done"))

sumR <- function(xs) as.integer(sum(xs))
