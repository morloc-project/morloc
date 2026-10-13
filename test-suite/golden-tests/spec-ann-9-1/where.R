rnode <- function() { n <- Sys.getenv("MOCK_NODE"); if (n == "") "parent" else n }
rhere <- function(x) paste0(rnode(), ":", x, "r")
rtag <- function(s) paste0("rtag@", rnode(), "[", s, "]")
