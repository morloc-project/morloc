packS <- function(xs) { r <- lapply(xs, function(p) p[[2]]); names(r) <- vapply(xs, function(p) p[[1]], ""); r }
unpackS <- function(m) Map(function(k, v) list(k, v), names(m), m, USE.NAMES = FALSE)
shape <- function(m) if (is.null(names(m))) "pairs" else paste(names(m), collapse = ",")
idR <- function(m) m
