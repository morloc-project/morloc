library(ape)

plotTree <- function(tree, outputpdffilename) {
  pdf(outputpdffilename)
  plot(tree, use.edge.length = TRUE)
  dev.off()
}
