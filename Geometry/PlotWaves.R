# Exploring the effects of standing waves
 library(animation)
 library(data.table)
 
 setwd("C:/Users/cmcneil/Documents/Projects/Miscellaneous/Personal/Geometry")
 
 t <- 1:1e3
 d <- 1:1e3
 
 rotate
 
 xmat <- matrix(0, length(t), length(d))
 xmat <- sin((1 - row(xmat) + col(xmat))/10)
 
 for (i in 1:nrow(xmat)) {
   cat("\rIteration: ", i)
   png(paste0("images/Xwave", i, ".png"), 400, 200)
   plot(xmat[,i], type = "l")
   dev.off()
 }
 saveHTML ({}, img.name = "Xwave", htmlfile = "Xwave.html", opts = ani.options(interval = .001), verbose = F)
 file.remove(dir("images", pattern = "Xwave", full.names = T))
 
 
 showWave <- function (mat, filename = "temp", clean = F, interval = .005) {
   
   for (i in 1:nrow(mat)) {
     cat("\rIteration: ", i, " of ", nrow(mat), "    ")
     png(paste0("images/", filename, i, ".png"), 400, 200)
     plot(mat[i,], type = "l", ylim = range(mat))
     dev.off()
   }
   saveHTML ({}, img.name = filename, htmlfile = paste0(filename, ".html"), opts = ani.options(interval = interval), verbose = F)
   if(clean)
     file.remove(dir("images", pattern = filename, full.names = T))
 }
 
 createWave <- function(FUN = function(x) sin(x/10), right = T, dim = c(1e3, 1e3)) {
   mat <- matrix(0, dim[1], dim[2])
   mat <- FUN((row(mat) - 1)*(-1)^right + col(mat))
   mat
 }
 
 showWave(createWave(right = T, dim = c(1e3, 5e2)))
 showWave(createWave(right = F, dim = c(1e3, 5e2)))
 showWave(createWave(right = F, dim = c(1e3, 5e2)) + createWave(right = T, dim = c(1e3, 5e2)))
 
 
 
 
 create2Dwave <- function(sources, length = 1e3, dim = rep(1e2, 2)) {
   arr <- array(0, dim = c(dim, length))
   for (i in 2:length) {
     arr[,,i] <- arr[]
   }
 }
 
 
 mat <- matrix(sin((1:1e3)/10))
 for (i in 1:500) {
   mat <- cbind(mat, c(rep(0, i), head(mat[,1]*.995^i, -i)))
 }
 
 
 showWave(mat, interval = 0.01)
 
 wave <- matrix(sin((1:1e3)/10))
 arr <- array(0, dim = c(3e2, 3e2, 1e3))
 cols <- array(col(arr[,,1]), dim = dim(arr))
 rows <- array(row(arr[,,1]), dim = dim(arr))
 slices <- array(rep(1:dim(arr)[3], each = prod(dim(arr)[1:2])), dim = dim(arr))
 
 FUN <- function(x) ifelse(x > 0, sin(x/10), 0)
 dist <- sqrt((rows-1)^2 + (cols-1)^2)
 rate <- 1
 alpha <- .997
 arr1 <- FUN(slices - dist/rate)*(alpha ^ dist)
 
 dist <- sqrt((rows-nrow(arr))^2 + (cols-1)^2)
 rate <- 1
 alpha <- .997
 arr2 <- FUN(slices - dist/rate)*(alpha ^ dist)
 
 image(arr[,,999])
 
 for (i in 1:dim(arr)[3]) {
   cat("\rIteration: ", i)
   png(paste0("images/temp", i, ".png"), 600, 400)
   print(ggplot(melt(arr1[, , i] + arr2[, , i]), aes(Var1, Var2, fill = value)) + geom_tile() + 
     scale_fill_gradientn(limits = c(-2,2), colors = c("navy", "white", "brown2")) + 
     scale_x_continuous(breaks = NULL) + scale_y_continuous(breaks = NULL) + labs(x = "", y = "", fill = ""))
   dev.off()
 }
 saveHTML("temp", htmlfile = "temp.html", opts = ani.options(interval = .02), verbose = F)
 
 
 #### Another method exists in PlotWaves2.R
 
# End script
 