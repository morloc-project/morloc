list_is_empty <- function(xs) {
    length(xs) == 0
}

list_at <- function(i, xs) {
    xs[[i + 1]]
}

unwrap_or <- function(default_val, x) {
    if (is.null(x)) default_val else x
}
