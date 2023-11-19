library(ape)

plotTree <- function(outputpdffilename, tree) {
  pdf(outputpdffilename)
  plot(tree)
  dev.off()
}
