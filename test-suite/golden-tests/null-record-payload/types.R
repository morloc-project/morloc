count_inner <- function(xs) sum(vapply(xs, function(o) !is.null(o$inner), logical(1)))
count_pinner <- count_inner
count_shape <- function(xs) sum(vapply(xs, function(h) !is.null(h$shape), logical(1)))
