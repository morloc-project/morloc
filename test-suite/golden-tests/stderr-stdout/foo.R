rfoo <- function(name) {
  cat("STDERR Hello", name, "\n", file = stderr())
  cat("STDOUT Hello", name, "\n")
}
