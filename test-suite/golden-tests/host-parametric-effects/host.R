tick <- function(path) {
  cat("x", file = path, append = TRUE)
  as.integer(file.info(path)$size)
}

count <- function(path) if (file.exists(path)) as.integer(file.info(path)$size) else 0L

h_pass <- function(f, x) f(x)
