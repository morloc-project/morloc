# .morloc.source is defined in the R pool preamble. With the expose
# mechanism, the path "morloc-test-expose-r/util.R" resolves via the
# .morloc.libdir fallback to $MORLOC_HOME/lib/R/morloc-test-expose-r/util.R.
.morloc.source("morloc-test-expose-r/util.R")

compute <- function(x) {
    doubler(util_add_one(x))
}
