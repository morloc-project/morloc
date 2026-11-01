path <- commandArgs(trailingOnly = TRUE)[1]
for (e in parse(file = path)) {
  if (identical(deparse(e)[1], ".__morloc$mlc_start()")) next
  eval(e, globalenv())
}
reify <- .__morloc$mlc_reify
r <- tryCatch({ reify(function(x) x + 1, "r"); "sent" }, error = function(e) conditionMessage(e))
cat(if (grepl("did not create", r)) "OK: r refuses a host function" else "UNEXPECTED: r sent a host function", "\n", sep = "")
f <- function(x) x
attr(f, "morloc_origin") <- list("cpp", 3L, list())
o <- reify(f, "r")
cat(if (identical(o, list("cpp", 3L, list()))) "OK: r passes an origin back" else "UNEXPECTED: r origin", "\n", sep = "")
