# Experimenting with correlation matrices
 library(readbitmap)
 library(data.table)
 library(MASS)
 library(Matrix)
 
 corm <- diag(rep(1, 5))
 corm[cbind(1:4, 2:5)] <- .5
 corm[cbind(2:5, 1:4)] <- .5
 corm[cbind(c(1:3, 3:5), c(3:5, 1:3))] <- .25
 corm[(cbind(c(1:2, 4:5), c(4:5, 1:2)))] <- .5
 corm[1, 5] <- 1
 corm[5, 1] <- 1
 
 sigdiag <- rep(1.2, 5)
 sig <- sqrt(solve(diag(sigdiag)))%*%corm%*%sqrt(solve(diag(sigdiag)))
 
 ro <- .9
 nd <- 49  # An odd number
 roseq <- ro^(-abs((1:nd)-(nd+1)/2)+(nd-1)/2)
 corm <- diag(rep(1, nd))
 corm[cbind(as.vector(row(corm)), as.vector(col(corm)))] <- roseq[abs(row(corm)-col(corm))+1]
 #corm[1:6, (nd-6):(nd)]
 
 sigdiag <- rep(1.2,nd)
 sig <- sqrt(solve(diag(sigdiag)))%*%corm%*%sqrt(solve(diag(sigdiag)))
 mu <- rep(0,nd)
 draw <- mvrnorm(400,mu,sig)[,-1]
 
 col <- heat.colors(12)
 col <- terrain.colors(10)
 col <- topo.colors(20)
 map <- cbind(draw, draw, draw, draw, draw, draw, draw, draw, draw, draw, draw)
 image(t(map[nrow(map):1,]), col = col, axes = F)
 
 dev.off()
 
 #' Rotated Image
 #' 
 #' Rotates matrix before passing to \code{image}
 #' @param mat Matrix to be displayed.
 #' @param ... Parameters passed to \code{image}
 #' @export
 
 imagerot <- function(mat, ...) {
   mat <- t(mat[nrow(mat):1,])
   image(mat, ...)
 }
 
 #' Draw matrix of correlated points
 #' 
 #' Creates a matrix of values correlated horizontally and (potentially) vertically.
 #' @param length Length of colored strip
 #' @param mu, sig Parameters of the normal distribution used to generate colors.
 #' @param m Width of colored strip.
 #' @param vert Logical. Whether to include vertical correlation. Defaults to \code{TRUE}
 #' @param rho1 Horizontal correlation of adjacent points.
 #' @param rho2 Vertical correlation of adjacent points
 #' @export
 #' @example
 #' mat <- rnormcor2(100, 0, 1, 50, rho1 = .9, vert = T)
 #' imagerot(cbind(mat, mat, mat, mat, mat, mat, mat))
 
 rnormcor <- function(n, mu, sig, m, vert = T, rho1 = .9, rho2 = .97) {
   roseq <- rho1^(-abs((1:m)-(m+1)/2)+(m-1)/2) #Smiling correlations (circular)
   corm <- diag(rep(1,m))
   corm[cbind(as.vector(row(corm)),as.vector(col(corm)))] <- roseq[abs(row(corm)-col(corm))+1]
   
   # corM <- kronecker(corm2,corm)
   # sigdiag <- diag(rep(sig,m*10))
   # Sig <- sqrt(solve(sigdiag))%*%corM%*%sqrt(solve(sigdiag))
   
   Sig <- corm/sig
   draw <- mvrnorm(n,rep(mu,m),Sig)[,-1]
   
   if(vert) {
     roseq2 <- c(rho2^(0:n)) # Decaying correlations
     corm2 <- diag(rep(1, n))
     corm2[cbind(as.vector(row(corm2)), as.vector(col(corm2)))] <- roseq2[abs(row(corm2)-col(corm2))+1]
     
     ch <- chol(corm2)
     draw <- t(ch) %*% draw
   }
   draw
 }
 
 #' Shorten line
 #' 
 #' Shortens the drawing line to make stereogram image appear closer.
 #' @param base Current line segment being drawn.
 #' @param ind Where in the line to begin shortening.
 #' @param num Number of pixels to remove.
 #' @export
 
 shorten <- function(base, ind, num) {
   n <- length(base)
   #cat(paste0("  Shorten ", n, " ", ind, " ", num, "\n"))
   if(num > n-ind)
     newbase <- shorten(base[1:(ind-1)], 1, num-(n-ind+1))
   else
     newbase <- base[c(0:(ind-1), (ind+num):n)]
   newbase
 }
 
 shortenList <- function(base, ind, num) {
   n <- length(base$inside)
   #cat(paste0("  Shorten ", n, " ", ind, " ", num, "\n"))
   if(num > n-ind) {
     tmp <- shortenList(list(inside = base$inside[1:(ind-1)], out = numeric()), 1, num-(n-ind+1))
     newbase <- list(inside = tmp$inside[(num-(n-ind)):(ind-1)],
                     out = c(base$out, tmp$out, base$inside[1:(num-(n-ind+1))]))
   } else {
     newbase <- list(inside = base$inside[c(0:(ind-1), (ind+num):n)],
                     out = c(base$out, base$inside[ind:(ind+num-1)]) )
   }
   newbase
 }
 
 #' Lengthen line
 #' 
 #' Lengthens the drawing line to make stereogram image appear farther.
 #' @param base Current line segment being drawn.
 #' @param ind Where in the line to begin lengthening.
 #' @param num Number of pixels to add.
 #' @param mu, sig Parameters used to generate new pixels.
 #' @export
 
 lengthen <- function(base, ind, num, mu, sig) {  # Needs fixing
   n <- length(base)
   
   # newseg <- rnorm(1, base[ind], sig)
   # if (num > 1)
   #   for (i in 2:num)
   #     newseg[i] <- rnorm(1, newseg[i-1], sig/4)
   
   newseg <- rnorm(num, mu, sig)
   #newseg <- sample(base, num, replace = T)
   #newseg <- sort(rnorm(num, mean(base), sd(base)), decreasing = runif(1) > .5)
   
   # newseg <- cumsum(rnorm(num, 0, sig/4)) + base[ind]
   newbase <- c(base[0:(ind-1)], newseg, base[ind:n])
   newbase
 }
 
 lengthenList <- function(base, ind, num, mu, sig) {  # Needs fixing
   n <- length(base$inside)
   
   # newseg <- rnorm(1, base[ind], sig)
   # if (num > 1)
   #   for (i in 2:num)
   #     newseg[i] <- rnorm(1, newseg[i-1], sig/4)
   
   newseg <- base$out[1:num]
   
   #newseg <- rnorm(num, mu, sig)
   #newseg <- sample(base, num, replace = T)
   #newseg <- sort(rnorm(num, mean(base), sd(base)), decreasing = runif(1) > .5)
   
   # newseg <- cumsum(rnorm(num, 0, sig/4)) + base[ind]
   newbase <- list(inside = c(base$inside[0:(ind-1)], newseg, base$inside[ind:n]),
                   out = base$out[-(1:num)] )
   newbase
 }
 
 #' Draw 3d Stereogram
 #' 
 #' Uses a black and white file template to create a 3d stereogram. Not very fast.
 #' @param infile Name of file containing grayscale template.
 #' @param bw Matrix of grayscale values representing depths. Used only if \code{infile} is NULL.
 #' @param col Palette of colors to be used.
 #' @param numcols Number of colors to select from \code{topo.colors()} if \code{col} is not given.
 #' @param repeats Not sure what this was supposed to be for. It is unused.
 #' @param levels Number of distinct depths created. 
 #' @param rho_h, rho_v Horizontal and vertical correlation of pixels.
 #' @param mu, sig Parameters of normal distribution used to generate random pattern.
 #' @param nd Initial number of pixels before repeating pattern.
 #' @param plot Whether to graph the resulting image. Default is \code{TRUE}
 #' @param outfile Optional name of a file for saving the image.
 #' @param clip If \code{TRUE} (the default), color values are clipped to within two standard deviations of the mean.
 #' @export
 #' @example
 
 draw3d <- function(infile = NULL, bw = NULL, col = NULL, numcols = 20, repeats = 10, levels = 10, rho1 = .9, rho2 = .97,
                    mu = 0, sig = 1.2, nd = 49, vert = T, plot = T, outfile = NULL, clip = T) {
   require(readbitmap)
   require(MASS)
   #require(data.table)
   
   if(is.null(infile) & is.null(bw))
     stop("infile and bw not specified, include one or the other")
   
   if(is.null(col))
     col <- topo.colors(numcols)
   
   # Read in black/white file containing depths
   if(!is.null(infile)) {
     bw <- read.bitmap(infile)[,,3]
     if(max(bw) > 1)
       bw <- bw/255
     bw <- round((1-bw)*levels)/levels
   }
   
   draw <- rnormcor(n = nrow(bw), mu = mu, sig = sig, m = nd, rho1 = rho1, rho2 = rho2, vert = vert)
   
   pic <- matrix(0, nrow = nrow(bw), ncol = ncol(bw))
   pic[,1] <- draw[,1]
   for (rw in 1:nrow(draw)) {
     # base <- list(in = draw[rw,], out = numeric())
     # if(bw[rw,1] > 0) {
     #   base <- shorten(base, ind = 1, round(levels*(bw[rw,1])))
     # }
     # pic[rw,1] <- base[1]
     # ind <- 2
     # for (cl in 2:ncol(pic)) {
     #   cat(paste0("\rRow: ", rw, " Col: ", cl))
     #   if(bw[rw, cl] > bw[rw, cl-1]) {
     #     base <- shorten(base, ind, round(levels*(bw[rw, cl] - bw[rw, cl-1])))
     #     if (ind > length(base))
     #       ind <- 1
     #   }
     #   if(bw[rw, cl] < bw[rw, cl-1]) {
     #     # base <- lengthen(base, ind, round(levels*(bw[rw, cl-1] - bw[rw, cl])), mu = mu, sig = sig/6)
     #     base <- lengthen(base, ind, round(levels*(bw[rw, cl-1] - bw[rw, cl])), mu = pic[rw - 1, pmin(cl:(cl + num - 1), ncol(pic))], sig = sig/4)
     #   }
     #   pic[rw, cl] <- base[ind]
     #   ind <- (ind%%length(base))+1
     # }
     
     base <- list(inside = draw[rw,], out = numeric())
     if(bw[rw,1] > 0) {
       base <- shortenList(base, ind = 1, round(levels*(bw[rw,1])))
     }
     pic[rw,1] <- base$inside[1]
     ind <- 2
     for (cl in 2:ncol(pic)) {
       cat(paste0("\rRow: ", rw, " Col: ", cl))
       if(bw[rw, cl] > bw[rw, cl-1]) {
         base <- shortenList(base, ind, round(levels*(bw[rw, cl] - bw[rw, cl-1])))
         if (ind > length(base$inside))
           ind <- 1
       }
       if(bw[rw, cl] < bw[rw, cl-1]) {
         # base <- lengthen(base, ind, round(levels*(bw[rw, cl-1] - bw[rw, cl])), mu = mu, sig = sig/6)
         base <- lengthenList(base, ind, round(levels*(bw[rw, cl-1] - bw[rw, cl])), mu = pic[rw - 1, pmin(cl:(cl + num - 1), ncol(pic))], sig = sig/4)
       }
       pic[rw, cl] <- base$inside[ind]
       ind <- (ind%%length(base$inside))+1
     }
     
   }
   img <- t(pic[nrow(pic):1,])
   if (clip) {
     img <- ifelse(img < mean(img)-2*sd(img), mean(img)-2*sd(img), img)
     img <- ifelse(img > mean(img)+2*sd(img), mean(img)+2*sd(img), img)
   }
   
   if(plot)
     image(img, col = col, axes = F)
   
   if(!is.null(outfile)){
     jpeg(outfile)
     image(img, col = col, axes = F)
     dev.off()
   }
   
   invisible(img)
 }
 
 
 dev.off()
 
 set.seed(87654)
 draw3d(bw = bw, col = rainbow(10))
 draw3d(bw = bw, col = topo.colors(20), vert = F)
 
 # Create black/white matrix from plot -- Possible?
 
 #bitmap("DonutBW.bmp")
 #dev.off()
 
 jpeg("B_W.jpg")
 bmp("B_W.bmp")
 #Plot something
 par(mai=rep(0,4))
 plot(c(1, 2), rep(1, 2), cex = 40, lwd = 60, axes = F, xlab = "", ylab = "", col = "gray90", xlim = c(0, 3))
 for(i in 1:19)
 points(c(1, 2), rep(1, 2), cex = 40, lwd = 60-3*i, col = paste0("gray", 95-5*i))
 dev.off()
 
 col <- heat.colors(12)
 col <- terrain.colors(10)
 col <- topo.colors(20)
 
 # Now need to leave R, open jpg and save as bmp
 bins <- 100
 bw <- round((1-read.bitmap("B_W.bmp")/255)*bins)/bins
 imagerot(bw)
 
 draw3d(bw = bw, col = rainbow(10), levels = 10)
 
 
 map2 <- map
 block <- map2[100:300,101:120]
 map2[100:300,101:400] <- cbind(map[100:300,121:400],block)
 image(t(map2[nrow(map2):1,]), col = col, axes = F)
 
 num <- 20
 pts <- data.table(x = runif(num), y = runif(num), z = round(runif(num)*9+1)/10)[order(z, decreasing = T)]
 jpeg("Rings.jpg", height = 500, width = 600)
 bmp("Rings.bmp", height = 500, width = 600)
 par(mai = rep(0, 4))
 plot(pts$x, pts$y, cex = 20, lwd = 25, axes = F, xlab = "", ylab = "", col = rgb(pts$z, pts$z, pts$z))
 dev.off()
 
 bw <- round((1-read.bitmap("Rings.bmp")/255)*scale)/scale
 img <- draw3d(infile = "Rings.bmp", outfile = "RingsO.jpg", plot = T, scale = 20)
 img <- draw3d(infile = "B_W.jpg", outfile = "VolcanoO.jpg", plot = T, scale = 20, rho2 = 1)
 
 image(img, col = col)
 
 
 
# End script
 