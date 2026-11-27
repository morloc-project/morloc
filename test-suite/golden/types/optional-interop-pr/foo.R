rSafeDiv <- function(x, y) {
    if (y == 0) return(NULL)
    return(as.integer(x %/% y))
}

rFromNull <- function(default_val, x) {
    if (is.null(x)) return(default_val)
    return(x)
}

rDouble <- function(x) {
    if (is.null(x)) return(NULL)
    return(as.integer(x * 2L))
}
