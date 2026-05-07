## R implementations for the uint8-cross-language golden test.
##
## The morloc type [UInt8] maps to R's `raw` here (declared in main.loc
## as `type R => (List UInt8) = "raw" UInt8`). The R runtime always
## materialises a [UInt8] wire packet as a RAWSXP regardless of hint, so
## the type assertions below should always hold. If a future runtime
## regression delivers something else (e.g. an `integer` vector or a
## `list`), the echo and native functions stop with a clear message
## that the test will capture.

r_encode <- function(s) {
    charToRaw(s)
}

r_decode <- function(b) {
    if (typeof(b) != "raw") {
        stop(paste0("r_decode expected raw, got ", typeof(b)))
    }
    rawToChar(b)
}

r_echo <- function(b) {
    if (typeof(b) != "raw") {
        stop(paste0("r_echo expected raw, got ", typeof(b)))
    }
    b
}

r_native <- function(b) {
    paste0("r: ", typeof(b), ", length=", length(b))
}
