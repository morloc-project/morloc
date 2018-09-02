library(taxizedb)

lineage <- function(taxid){
  classification(taxid)[[1]]$id
}
