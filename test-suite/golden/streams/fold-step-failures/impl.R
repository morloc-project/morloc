signal_step <- function(acc, xs) {
  stop(structure(class = c("notAnError", "condition"), list(message = "not an error", call = NULL)))
}

produce_caught <- function(sink) {
  tryCatch(sink(1L), notAnError = function(c) NULL)
  invisible(NULL)
}

merge <- function(a, b) a + b
show_n <- function(n) paste0("r n=", n, "\n")
