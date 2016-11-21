# Functions for creating and manipulating mandelbrot sets
 library(animation)
 library(data.table)
 library(ggplot2)
 library(grid)
 library(gridExtra)
 library(Rmpfr)
 
 mand <- function (z, FUN = function(x, x0) {x^2 + x0}, max.iter = 100, rmax = 2, verbose = T) {
   # Calculates the number of sequence iterations (maximum of max.iter) required for the modulus to exceed a certain radius (rmax).
   z0 <- z
   iter <- 1
   result <- rep(NA, length(z))
   while(sum(is.na(result))) {
     if(verbose)
       cat("\rIteration: ", iter)
     ii <- which(is.na(result))
     z[ii] <- FUN(z[ii], z0[ii])
     result[ii][Mod(z[ii]) > rmax] <- iter
     if(iter == max.iter)
       result[is.na(result)] <- max.iter
     iter <- iter + 1
   }
   if (verbose)
     cat("\n")
   result
 }
 
 mand2 <- function (z, y = NULL, FUN = NULL, max.iter = 100, rmax = 2, verbose = T) {
   # Calculates the number of sequence iterations (maximum of max.iter) required for the modulus to exceed a certain radius (rmax).
   # This second version operates with mpfr values to allow greater floating point precision, but mpfr does not allow complex z.
   # Allows for a 2-column matrix z, or 2 inputs z and y where z is the real part and y is the imaginary part
   if(is.null(FUN))
     FUN <- function (x, x0) {
       #data.table(x[, Re]^2 - x[, Im]^2 + x0[, Re], 2*x[, Re]*x[, Im] + x0[, Im])
       x[, list(Re = Re^2 - Im^2 + x0$Re, Im = 2*Re*Im + x0$Im)]
     }
   if(!is.null(y) & is.null(dim(z))) {
     z <- data.table(Re = z, Im = y)
   }
   z0 <- z <- setnames(data.table(z), c("Re", "Im"))
   
   iter <- 1
   result <- rep(NA, nrow(z))
   while(sum(is.na(result))) {
     if(verbose)
       cat("\rIteration: ", iter)
     ii <- which(is.na(result))
     z[ii] <- FUN(z[ii], z0[ii])
     result[ii][z[ii, Re]^2 + z[ii, Im]^2 > rmax^2] <- iter
     if(iter >= max.iter)
       result[is.na(result)] <- max.iter
     iter <- iter + 1
   }
   if (verbose)
     cat("\n")
   result
 }
 
 
 
 mand3 <- function (z, y = NULL, FUN = NULL, max.iter = 100, rmax = 2, verbose = T, interval = 1e-10, basis.length = 10) {
   # Calculates the number of sequence iterations (maximum of max.iter) required for the modulus to exceed a certain radius (rmax).
   # This third version operates with matrices of values with columns added for higher precision.
   # Allows for a 2-column matrix z, or 2 inputs z and y where z is the real part and y is the imaginary part
   if(is.null(FUN))
     FUN <- function (x, x0) {
       #data.table(x[, Re]^2 - x[, Im]^2 + x0[, Re], 2*x[, Re]*x[, Im] + x0[, Im])
       x[, list(Re = Re^2 - Im^2 + x0$Re, Im = 2*Re*Im + x0$Im)]
     }
   if(!is.null(y) & is.null(dim(z))) {
     z <- data.table(Re = z, Im = y)
   }
   z0 <- z <- setnames(data.table(z), c("Re", "Im"))
   
   iter <- 1
   result <- rep(NA, nrow(z))
   while(sum(is.na(result))) {
     if(verbose)
       cat("\rIteration: ", round(iter))
     ii <- which(is.na(result))
     z[ii] <- FUN(z[ii], z0[ii])
     result[ii][z[ii, Re]^2 + z[ii, Im]^2 > rmax^2] <- iter
     if(iter >= max.iter)
       result[is.na(result)] <- max.iter
     iter <- iter + 1
   }
   if (verbose)
     cat("\n")
   result
 }
 
 
 # Find divergence counts for various functions.
 
 cgrid <- data.table(expand.grid(xseq <- seq(-2, 1, 0.01), yseq <- seq(-2, 2, 0.01)))
 setnames(cgrid, c("x", "y"))
 #cgrid[, Value := fun2(x + y * 1i, 5)]
 cgrid[, Count2 := mand(x + y * 1i)]
 cgrid[, Count2_ := mand(x + y * 1i, FUN = function(z, z0) z^2 + Conj(z0))]
 cgrid[, Count3 := mand(x + y * 1i, FUN = function(z, z0) z^3 + z0)]
 cgrid[, Count4 := mand(x + y * 1i, FUN = function(z, z0) z^4 + z0)]
 cgrid[, CountSin := mand(x + y * 1i, FUN = function(z, z0) sin(z/z0), rmax = 5)]
 cgrid[1:10]
 
 
 ggplot(cgrid, aes(x, y, col = log(Count2))) + geom_point()
 ggplot(cgrid, aes(x, y, col = log(Count2_))) + geom_point()
 ggplot(cgrid, aes(x, y, col = log(Count3))) + geom_point()
 ggplot(cgrid, aes(x, y, col = log(Count4))) + geom_point()
 ggplot(cgrid[x >= -1 & abs(y) <= 1], aes(x, y, col = log(CountSin))) + geom_point()
 , nrow = 1)
 
 cgrid[y == 0]
 
 # Can these be animated or manually zoomed.
 
 N <- 1325
 #xc <- -0.735010285109
 #yc <-  0.36025390802475
 xc <- mpfr("-0.735010285107191208245667883018055590009127920122111448264421", 210)
 yc <- mpfr( "0.360253908024380480417495867164111064063092802667010666726759", 210)
 wpx <- 250
 hpx <- 200
 xscale = mpfr(2, 80) #* mpfr(.9, 80)^1325
 yscale = mpfr(2, 80) #* mpfr(.9, 80)^1325
 plots <- NULL
 #  cgrid <- expand.grid(seq(as.numeric(xc - xscale), as.numeric(xc + xscale), length.out = wpx), 
 #                       seq(as.numeric(yc - yscale), as.numeric(yc + yscale), length.out = hpx))
 cgrid <- expand.grid(seq(0, 1, length.out = wpx), 
                      seq(0, 1, length.out = hpx) )
 for (i in 1:N) {
   cat("\rDepth: ", round(i), "\n")
   count <- data.table(expand.grid(seqMpfr(xc - xscale, xc + xscale, length.out = wpx), 
                                   seqMpfr(yc - yscale, yc + yscale, length.out = hpx)))[
                                     , Count := mand2(.SD, verbose = T, max.iter = 600) ]
   max(count$Count); sum(count$Count == max(count$Count))
   invisible(count[Count==max(Count)][1, print(c(Var1, Var2), digits = 70)])
   # table(count$Count)
   # count[,paste0(signif((which(Count == max(Count)) %/% hpx) / wpx *2, 3), ",", signif((which(Count == max(Count)) %% hpx) / hpx *2, 3))]
   # xc - xscale + .35*xscale; yc - yscale + yscale*.422
   
   png(paste("images/Mandelbrot", i, ".png", sep=""), 800, 600)
   print(
     ggplot(cgrid, aes(Var1, Var2, col = log(count$Count))) + geom_point() + 
       scale_x_continuous(breaks = NULL) + scale_y_continuous(breaks = NULL) + guides(col = F)
   )
   dev.off()
   xscale/wpx
   xscale <- xscale * 0.9
   yscale <- yscale * 0.9
   #log(xscale/2)/log(0.9)
 }
 
 
 saveHTML({}, img.name = "Mandelbrot", htmlfile = "Mandelbrot.html")
 ?saveHTML
 
 pal <- colorRampPalette(rainbow(12))(100)
 plot(cgrid$Var1, cgrid$Var2, cex = .2, col = pal[cgrid$Count])
 locator(1)
 
# End script
 