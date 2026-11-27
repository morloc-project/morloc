sideEffectR <- function(x) {
    cat(paste0("EVAL ", x, "\n"), file = stderr())
    x * 2L
}
