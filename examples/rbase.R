library(jsonlite)
library(data.table)

# Extract R objects from JSON text
unpackDataFrame <- function(json){ as.data.frame(fromJSON(json)) }
unpackDataTable <- function(json){ as.data.table(fromJSON(json)) }
unpackCharacter <- fromJSON
unpackNumeric   <- fromJSON
unpackLogical   <- fromJSON
unpackList <- function(json) {fromJSON(json, simplifyVector = FALSE)}
unpackMatrix <- function(json){
  fromJSON(json, simplifyVector = FALSE, simplifyMatrix = TRUE)
}

# Serialize R objects into JSON text
packDataFrame <- function(json){ toJSON(json, dataframe="columns") }
packDataTable <- packDataFrame
packCharacter <- toJSON
packNumeric   <- toJSON
packLogical   <- toJSON

# Generic pack/unpack functions
packGeneric   <- toJSON
unpackGeneric <- fromJSON
