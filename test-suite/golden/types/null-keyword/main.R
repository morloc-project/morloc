fromNull <- function(default_val, x) {
    if (is.null(x)) return(default_val)
    return(x)
}
