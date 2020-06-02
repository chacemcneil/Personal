# 2nd attempt at stereograms
 library(Matrix)
 library(data.table)
 library(readbitmap)
 library(jpeg)
 library(MASS)
 library(RColorBrewer)
 
 setwd("C:/Users/McNeil/Documents/Chace's files/Disorganized/Stereograms")
 
 imageR <- function(mat, col = heat.colors(12)) {
   # Rotate matrix clockwise quarter turn so that image() gives correct orientation
   matR <- t(mat)[, nrow(mat):1]
   image(matR, col = col)
 }
 
 getdraws <- function (rho.h = 0.95, rho.v = 0.95, length = 40, totallength = 480, width = 90, seed = NULL) {
   # rho.h:       correlation of horizontally adjacent pixels
   # rho.v:       correlation of vertically adjacent pixels
   # length:      maximum number of correlated rows, draws are repeated to reach totallength rows
   # totallength: final number of rows
   # width:       number of correlated columns (also final number of columns)
   # seed:        seed used for randomly generated data
   
   A <- diag(1, width)
   A <- rho.h^pmin( abs(row(A)-col(A)), nrow(A)-abs(row(A)-col(A)) )
   B <- diag(1,length)
   B <- rho.v^abs(row(B)-col(B))
   cAB <- t(chol(kronecker(A, B)))
   
   if(!is.null(seed))
     set.seed(seed)
   draw <- do.call(rbind, lapply(1:ceiling(totallength/length), function(i) matrix(cAB%*%rnorm(length*width), length, width)) )
   return(draw[1:totallength, 1:width])
 }
 
 createImage <- function(file, type = c("bmp", "jpeg"), blackdeep = T, draws = NULL,
                         totallength = 480, totalwidth = 640,
                         minwidth = ceiling(totalwidth/13), maxwidth = floor(totalwidth/7),
                         plot = T, col = heat.colors(maxwidth-minwidth), returnbw = F, ...) {
   # file:        "black and white" bitmap file defining depths
   # blackdeep:   boolean, if TRUE black is interpreted as far and white near
   # draws:       background image coloring to use, if NULL it is generated
   # plot:        boolean, if TRUE the final image is plotted
   # col:         color used if plot is TRUE
   
   type <- match.arg(type)
   print(paste("Reading depth information from file:", file))
   if(type=="bmp")
     bw <- read.bitmap(file)
   else if (type=="jpeg")
     bw <- readJPEG(file)
   if(length(dim(bw))==3)
     bw <- bw[,,1]
   if(ncol(bw) > totalwidth)
     bw <- bw[,1:totalwidth]
   if(ncol(bw) < totalwidth)
     bw <- cbind(matrix(bw[1, 1], nrow(bw), totalwidth-ncol(bw)), bw)
   # Assign depth b/w matrix to values in given range (e.g. linear Tx from 0 to 255 ~> 90 to 50)
   if(blackdeep)
     bw <- bw/diff(range(bw))*(minwidth-maxwidth) + maxwidth
   else
     bw <- bw/diff(range(bw))*(maxwidth-minwidth) + minwidth
   bw <- round(bw)
   
   if(is.null(draws)) {
     print("Creating background coloring")
     draws <- getdraws(..., totallength = totallength, width = maxwidth)
   }
   print("Drawing final image")
   img <- matrix(0, totallength, totalwidth)
   for (i in 1:nrow(img)) {
     draw <- draws[i,]
     index <- 1:length(draw)
     for (j in 1:ncol(bw)) {
       if(bw[i,j] < length(index)) {
         index <- tail(index, -(length(index)-bw[i, j]))
       }
       if(bw[i,j] > length(index)) {
         pot <- which(!1:length(draw) %in% index)
         index <- c(rev(head(pot[order(pot<index[1])], bw[i, j]-length(index))), index)
       }
       img[i, j] <- draw[index[1]]
       index <- c(index[-1], index[1])
       cat(paste0("\rRow: ", i, " / ", nrow(img)))
     }
   }
   if(plot)
     image(t(img)[, nrow(img):1], col = col)
   if(returnbw)
     return(list(img = img, bw = bw))
   else
     return(img)
 }
 
 im.globe <- createImage("D:/globe.jpg", type = "jpeg", totalwidth = 1024, totallength = 768, seed = 505)
 imageR(im.globe, col = colorRampPalette(c("white", "forestgreen", "skyblue", "white"))(50))
 hist(im.globe)
#  im.horse <- createImage("D:/horse.jpg",type="jpeg",totalwidth=1024,totallength=768,seed=505)
#  imageR(im.horse, col = colorRampPalette(c("white", "brown", "slateblue", "white"))(50))
 
 
 
# End script
 