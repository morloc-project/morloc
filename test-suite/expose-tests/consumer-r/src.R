# .morloc.source is defined in the R pool preamble. With the expose
# mechanism, the path "exposetest/morloc-test-expose-r/util.R" resolves via the
# .morloc.libdir fallback to $MORLOC_STATE/modules/R/exposetest/morloc-test-expose-r/util.R.
.morloc.source("exposetest/morloc-test-expose-r/util.R")

compute <- function(x) {
    doubler(util_add_one(x))
}
