# A payload-bearing `data` value crosses as a structural pair: the
# constructor's name and a list of its fields. Interim representation; an S3
# classed list is the intended end state.
area <- function(s) {
    ctor <- s[[1]]
    fields <- s[[2]]
    if (ctor == "Circle") {
        3.0 * fields[[1]] * fields[[1]]
    } else if (ctor == "Rect") {
        fields[[1]] * fields[[2]]
    } else {
        0.0
    }
}
