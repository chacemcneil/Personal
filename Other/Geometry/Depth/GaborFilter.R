# Exploring Gabor Filter, edge detection
 library(grt)
 
 gabor <- function (sf, theta = 0, rad = (theta * pi)/180, pc = 1, sigma = 1/6, 
                    psi = 0, grating = c("cosine", "sine"), npoints = 100, trim = 0, 
                    trim.col = 0.5, plot=F, ...) 
 {
   if (length(npoints) == 1) 
     npoints <- rep(npoints, 2)
   if (length(sigma) == 1) 
     sigma <- rep(sigma, 2)
   X <- ((1:npoints[1L])/npoints[1L]) - 0.5
   Y <- ((1:npoints[2L])/npoints[2L]) - 0.5
   Xm <- matrix(rep(X, npoints[2L]), npoints[1L])
   Ym <- matrix(rep(Y, npoints[1L]), npoints[1L],byrow=T)
   #Ym <- t(Xm)
   Xt <- Xm * cos(rad) + Ym * sin(rad)
   Yt <- -Xm * sin(rad) + Ym * cos(rad)
   grating <- match.arg(grating)
   if (grating == "cosine") {
     wave <- pc * cos(2 * pi * Xt * sf + psi)
   }
   else {
     wave <- pc * sin(2 * pi * Xt * sf + psi)
   }
   gauss <- exp(-0.5 * ((Xm/sigma[1L])^2 + (Ym/sigma[2L])^2))
   gabor <- wave * gauss
   gabor[gauss < trim] <- (trim.col * 2 - 1)
   if(plot)
     image(z = gabor, zlim = c(-1, 1), col = gray((0:npoints[1L])/(npoints[1L])), 
           axes = FALSE, asp = npoints[2L]/npoints[1L], ...)
   invisible(gabor)
 }
 
 # Read black and white image (from Stereo.R)
 bw <- read.bitmap(file)[,,1]
 if(ncol(bw) > totalwidth)
   bw <- bw[,1:totalwidth]
 if(ncol(bw) < totalwidth)
   bw <- cbind(matrix(bw[1,1],nrow(bw),totalwidth-ncol(bw)),bw)
 # Assign depth b/w matrix to values in given range (e.g. linear Tx from 0 to 255 ~> 90 to 50)
 if(blackdeep)
   bw <- bw/diff(range(bw))*(minwidth-maxwidth) + maxwidth
 else
   bw <- bw/diff(range(bw))*(maxwidth-minwidth) + minwidth
 bw <- round(bw)
 
 rotate <- function(mat) {
   midrow <- ceiling(nrow(mat)/2)-1
   midcol <- ceiling(ncol(mat)/2)-1
   return(mat[c((midrow+1):nrow(mat),1:midrow),c((midcol+1):ncol(mat),1:midcol)])
 }
 
 gb  <- gabor(5,npoints=c(480,640),psi=-pi/2,sigma=1/150)
 gb  <- gabor(640,npoints=c(480,640),grating="sine",sigma=c(1/480,1/640))
 gb2 <- gabor(480,npoints=c(480,640),theta=90,grating="sine",sigma=c(1/480,1/640))
 
 gb <- rotate(gb)
 gb2 <- rotate(gb2)
 hist(gb2)
 
 tx1 <- Re(fft(fft(gb)*fft(bw),inverse=T))/prod(dim(gb))
 imageR(abs(tx1)>3)
 tx2 <- Re(fft(fft(gb2)*fft(bw),inverse=T))/prod(dim(gb))
 imageR(abs(tx2)>3)
 tx <- tx1 + tx2
 imageR(abs(tx)>10)
 imageR(Mod(fft(fft(gb2)*fft(bw)))/prod(dim(gb)))
 imageR(bw)
 
 
 hist(gb - Re(fft(fft(gb),inverse=T)/prod(dim(gb))))
 which(abs(gb) > .05,arr.ind=T)
 gb[236:244,316:324]
 gb[1:5,1:5]
 gb[475:480,1:5]
 
 
 # 1D filter
 N <- 1000
 f <- 15
 xseq <- (1:N)/N -.5
 wv <- sin(2*pi*f*xseq)
 gs <- dnorm(xseq,0,.2)
 gs <- dnorm(xseq,0,.001)
 plot(xseq,wv,type="l")
 plot(xseq,gs,type="l")
 gb <- wv*gs
 plot(gb,type="l")
 
 bin <- rbinom(100,1,.9)
 plot(bin,type="l")
 cn <- convolve(bin+.5,gb,type="open")
 plot(cn[500:600],type="l")
 
 
 mat1 <- matrix(1:9,3,3)
 mat2 <- matrix(c(0,1,0,2,0,-2,0,-1,0),3,3)
 mat2 <- matrix(c(0,0,0,0,1,0,0,0,0),3,3)
 Re(fft(fft(mat1)*fft(mat2),inverse=T)/9)
 
 
 # Use new images
 
 
 lim <- read.bitmap("c:/users/cmcneil/Pictures/Im3_L.png")[,,1]
 rim <- read.bitmap("c:/users/cmcneil/Pictures/Im3_R.png")[,,1]
 minrows <- min(nrow(lim),nrow(rim))
 mincols <- min(ncol(lim),ncol(rim))
 lim <- lim[1:minrows,1:mincols]
 rim <- rim[1:minrows,1:mincols]
 
 dim(lim);dim(rim)
 findEdges <- function (mat,prec=1,wind=1) {
   gb.v <- rotate(gabor(ncol(mat)/prec,npoints=dim(mat),grating="sine",sigma=wind/dim(mat)))
   gb.h <- rotate(gabor(nrow(mat)/prec,npoints=dim(mat),theta=90,grating="sine",sigma=wind/dim(mat)))
   tx.v <- Re(fft(fft(gb.v)*fft(mat),inverse=T))/prod(dim(gb.v))
   tx.h <- Re(fft(fft(gb.h)*fft(mat),inverse=T))/prod(dim(gb.h))*1i
   tx <- tx.v + tx.h
   return(tx)
 }
 
 txl <- findEdges(lim)
 txr <- findEdges(rim)
 
 imageR(abs(txl)>.1)
 imageR(abs(txr)>.1)
 hist(abs(txr))
 
 # Prep for depth algorithm
 lim <- abs(txl)>0.15
 rim <- abs(txr)>0.15
 
 
# End script
 