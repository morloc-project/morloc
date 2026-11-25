# Relative source() from one exposed R file to another. Resolves
# correctly because .morloc.source loads util.R with chdir=TRUE,
# which sets the working dir to util.R's directory for the duration
# of evaluation. helpers.R sits in the same exposed dir.
source("helpers.R")

util_add_one <- function(x) {
    add_one(x)
}

doubler <- function(x) {
    x * 2
}
