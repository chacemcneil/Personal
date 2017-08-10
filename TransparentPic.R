# Code for creating an image
 library(abind)
 library(png)
 
 transparent <- function(filename, row = 1, col = 1, hmargin = NULL, vmargin = NULL, suffix = "_tp", repl.alpha = 0) {
   img <- readPNG(filename)
   img <- img[,,1:3]
   cols <- col(img[,,1])
   rows <- row(img[,,1])
   vert <- horiz <- 1
   if(!is.null(hmargin))
     horiz <- pmin(1 - (hmargin - cols)/hmargin, (ncol(cols) - cols)/hmargin, 1)
   if(!is.null(vmargin))
     vert <- pmin(1 - (vmargin - rows)/vmargin, (nrow(rows) - rows)/vmargin, 1)
   if(is.null(hmargin) & is.null(vmargin))
     horiz <- vert <- 0
   alpha <- pmax(ifelse(apply(img, 1:2, identical, img[row, col,]), repl.alpha, 1), pmin(horiz, vert))
   img <- abind(img, alpha)
   writePNG(img, gsub(".png", paste0(suffix, ".png"), filename))
 }
 
 png("Test.png")
 hist(rnorm(1e3))
 dev.off()
 
 transparent("Test.png")
 transparent("Test.png", hmargin = 100, vmargin = 20, suffix = "_tp2")
 
 file.remove("Test.png")
 file.remove("Test_tp.png")
 file.remove("Test_tp2.png")
 
 
 
# End script
 