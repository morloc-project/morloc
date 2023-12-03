library(ape)

plotTree <- function(outputpdffilename, tree) {
  pdf(outputpdffilename, width = 8, height = length(tree$tip.label) * 0.1)
  par(cex = 0.7)  # Adjusts the label font size for readability
  plot(tree, show.tip.label = TRUE, cex = 0.7)  # Plots the tree with tip labels
  dev.off()  # Closes the PDF device
}
