# Look at zooming on plot
 library(ggplot2)
 
 zoom <- function() {
   # Uses locator function (not compatible with ggplot)
   plt <- recordPlot()
   pts <- locator(2)
   
   plt[[1]][[3]][[2]][[2]] <- sort(pts$x)
   plt[[1]][[3]][[2]][[3]] <- sort(pts$y)
   replayPlot(plt)
 }
 
 
 
 