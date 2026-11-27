uptimeSec <- function() {
    cat("UPTIME_QUERY\n")
    flush(stdout())
    86400L
}

fmtReport <- function(label, code, id) {
    paste0("[", label, "] code=", code, " pid=", id)
}
