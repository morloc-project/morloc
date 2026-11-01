form <- function(x) {
  v <- if (inherits(x, "integer64")) as.character(x) else as.character(x)
  paste(paste(class(x), collapse = ","), typeof(x), paste(v, collapse = " "))
}
