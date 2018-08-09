library(jsonlite)
library(data.table)

# Extract R objects from JSON text
unpackDataFrame <- function(json){ as.data.frame(fromJSON(json)) }
unpackDataTable <- function(json){ as.data.table(fromJSON(json)) }
unpackVector    <- fromJSON
unpackList      <- function(json) {fromJSON(json, simplifyVector = FALSE)}
unpackMatrix    <- function(json){ fromJSON(json, simplifyVector = FALSE, simplifyMatrix = TRUE) }

# Serialize R objects into JSON text
packDataFrame <- function(json){ toJSON(json, dataframe="columns") }
packDataTable <- packData.Frame
packVector    <- toJSON
packList      <- toJSON
packMatrix    <- toJSON
