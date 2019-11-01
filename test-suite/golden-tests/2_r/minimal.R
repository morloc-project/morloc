library(jsonlite)

# Generic pack/unpack functions
packGeneric   <- toJSON
unpackGeneric <- function(s) {fromJSON(as.character(s))}
