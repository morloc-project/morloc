.form <- function(x) {
  attrs <- attributes(x)
  attrs$names <- NULL
  a <- if (length(attrs) == 0) "" else paste0("{", paste(names(attrs), vapply(attrs, function(v) paste(format(v), collapse = " "), character(1)), sep = "=", collapse = ";"), "}")
  head <- paste0(paste(class(x), collapse = "/"), ":", typeof(x), a)
  nm <- names(x)
  if (is.list(x)) {
    inner <- vapply(seq_along(x), function(i) paste0(if (is.null(nm)) "" else paste0(nm[i], "="), .form(x[[i]])), character(1))
    return(paste0(head, "[", paste(inner, collapse = ","), "]"))
  }
  vals <- if (inherits(x, "integer64")) as.character(x) else format(unclass(x))
  paste0(head, "[", paste0(if (is.null(nm)) "" else paste0(nm, "="), vals, collapse = ","), "]")
}

same <- function(name, a, b) {
  fa <- .form(a)
  fb <- .form(b)
  if (identical(fa, fb)) return(paste(name, "ok"))
  paste0(name, " DIFF literal=", fa, " received=", fb)
}

echo <- function(x) x
