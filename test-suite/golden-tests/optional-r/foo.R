isNull <- function(x) {
    is.null(x)
}

fromNull <- function(default_val, x) {
    if (is.null(x)) return(default_val)
    return(x)
}

toNull <- function(x) {
    return(x)
}

safeHead <- function(xs) {
    if (length(xs) == 0) return(NULL)
    return(xs[[1]])
}

optionalAdd <- function(x, y) {
    if (is.null(x) || is.null(y)) return(NULL)
    return(x + y)
}

optionalList <- function(xs) {
    lapply(xs, function(x) if (x < 0) NULL else x)
}

countNulls <- function(xs) {
    sum(sapply(xs, is.null))
}
