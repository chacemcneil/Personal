# Correlation of matrices
 library(MASS)
 library(data.table)
 
 setwd("C:/Users/cmcneil/Documents/Projects/Miscellaneous/Depth")
 
 sub <- matrix(rnorm(9),3,3)
 main <- matrix(rnorm(10000),100,100)
 main <- matrix(rnorm(100),10,10)
 
 veccor <- function(mat1,mat2) {
   return(cor(as.vector(mat1),as.vector(mat2)))
 }
 
 maxcor <- function() {
   
 }
 
 nc <- ncol(sub)
 nr <- nrow(sub)
 Nc <- ncol(main)
 Nr <- nrow(main)
 cors <- matrix(0,Nr-nr+1,Nc-nc+1)
 for (i in 1:(Nr-nr+1)) {
   for (j in 1:(Nc-nc+1)) {
     # i,j give the coordinates of the upper left corner of block
     cors[i,j] <- veccor(sub,main[(i):(i+nr-1),(j):(j+nc-1)])
     cat(paste0("\r",round(100*(i*(Nc-nc+1)+j)/(Nr-nr+1)/(Nc-nc+1),1),"% complete","\ti: ",i,"\tj: ",j,"\t"))
   }
 }
 image(cors)
 
 this <- read.bitmap("c:/users/cmcneil/downloads/photo.jpg")
 main <- this[,,1]
 sub <- main[745:755,795:805]
 image(sub)
 
 
 tail(sort(cors))
 
 rw <- apply(cors,1,sum,na.rm=T)
 erw <- exp(rw)
 mean((1:nrow(cors))*rw)
 
 # Interpolation function
 interp <- function(vec,round=T,ext.lin=T) {
   # ext.lin represents linear extrapolation to the ends of the vector, extrapolation from first and last valid points
   n <- length(vec)
   newvec <- vec
   if(ext.lin) {
     fi <- min(which(!is.na(vec)))
     fv <- vec[fi]
     li <- max(which(!is.na(vec)))
     lv <- vec[li]
     vec[1] <- fv + (1-fi)*(lv-fv)/(li-fi)
     vec[n] <- fv + (n-fi)*(lv-fv)/(li-fi)
   }
   for (i in which(is.na(vec))) {
     li <- max(which(!is.na(vec[1:(i-1)])))
     lv <- vec[li]
     ri <- i+min(which(!is.na(vec[(i+1):length(vec)])))
     rv <- vec[ri]
     newvec[i] <- lv + (i-li)*(rv-lv)/(ri-li)
   }
   if(round)
     newvec <- round(newvec)
   return(newvec)
 }
 
 
 ### Find depth from two images
 # Load two pictures
 library(readbitmap)
 lim <- read.bitmap("c:/users/cmcneil/Pictures/L.png")[,,1]
 rim <- read.bitmap("c:/users/cmcneil/Pictures/R.png")[,,1]
 image(lim)
 
 # Parallelize pictures (line up y axis)
 library(robustbase)
 minrows <- min(nrow(lim),nrow(rim))
 rows <- 100:(minrows-100)
 mincols <- min(ncol(lim),ncol(rim))
 cols <- 100:(mincols-100)
 mat <- cor(t(lim[rows,cols]),t(rim[rows,cols]))
 image(mat)
 { # Used for manually picking rows
   best <- apply(mat,1,which.max)
   coefs <- lmrob.lar(cbind(1,1:length(best)),best)$coefficients  # Not very useful, need exact row, not estimate
   fitted <- ifelse(abs(best-cbind(1,1:length(best))%*%(coefs))>2,round(cbind(1,1:length(best))%*%(coefs),0),best)
   better <- interp(ifelse(best==fitted,best,NA),round=T,ext,lin=T)
 }
 common <- intersect((1:nrow(lim))+14,(1:nrow(rim)))
 lim2 <- lim[common-14,]
 rim2 <- rim[common,]
 
 # By pixel, by row, find point on R map most correlated with L point, calculate D
 radius <- 7  # Radius of pixels in window calculating correlations
 margin <- 2  # Number of rows on either side of target row
 range  <- 7 # Number of pixels on either side of target pixel
 minrow <- margin+radius+1
 maxrow <- nrow(lim2)-margin-radius
 mincol <- range+radius+1
 maxcol <- ncol(lim2)-range-radius
 
 depth <- matrix(0,maxrow-minrow+1,maxcol-mincol+1)
 for (i in minrow:maxrow) {
   for (j in mincol:maxcol) {
     sub <- lim2[i+(-radius:radius),j+(-radius:radius)]
     cors <- matrix(0,2*margin+1,2*range+1)
     for (i2 in (-margin):margin) {
       for (j2 in (-range):range) {
         cors[i2+margin+1,j2+range+1] <- veccor(sub,rim2[i+i2+(-radius:radius),j+j2+(-radius:radius)])
         cat(paste0("\r",round(100*((i-mincol)*(maxcol-mincol+1)+j)/(maxrow-minrow+1)/(maxcol-mincol+1),1),"% complete","\ti: ",i,"\tj: ",j,"\ti2: ",i2,"\tj2: ",j2))
       }
     }
     cors[is.na(cors)] <- -1
     if(max(cors)==-1)
       depth[i-minrow+1,j-mincol+1] <- NA
     else
       depth[i-minrow+1,j-mincol+1] <- which(cors==max(cors),arr.ind=T)[1,2] #+ j - j
   }
 }
 
 depth <- matrix(0,maxrow-minrow+1,maxcol-mincol+1)
 for (i in minrow:maxrow) {
   for (j in mincol:maxcol) {
     sub <- lim2[i+(-radius:radius),j+(-radius:radius)]
     ss <- matrix(0,2*margin+1,2*range+1)
     for (i2 in (-margin):margin) {
       for (j2 in (-range):range) {
         ss[i2+margin+1,j2+range+1] <- sum((sub-rim2[i+i2+(-radius:radius),j+j2+(-radius:radius)])^2)
         cat(paste0("\r",round(100*((i-minrow)*(maxcol-mincol+1)+j)/(maxrow-minrow+1)/(maxcol-mincol+1),1),"% complete","\ti: ",i,"\tj: ",j,"\ti2: ",i2,"\tj2: ",j2))
       }
     }
     #ss[is.na(ss)] <- -1
     colmin <- sort(which(ss==min(ss),arr.ind=T)[,2]) #+ j - j
     if(length(colmin)%%2==1)
     depth[i-minrow+1,j-mincol+1] <- colmin[(length(colmin)+(length(colmin)%%2))/2]
   }
 }
 
 image(depth,col=gray(seq(0,1,.01)))
 
 
 # Manually create l and r
 l <- matrix(rnorm(1e4),100,100)
 r <- l
 r[25:75,] <- cbind(r[25:75,1:25],r[25:75,36:85],matrix(rnorm(51*10),51,10),r[25:75,86:100])
 
 
 ####
 ##    New fft method
 ####
 
 lim <- read.bitmap("RandImages/Chrysanthemum.jpg")[,,1]
 rim <- read.bitmap("RandImages/Chrysanthemum.jpg")[,,1]
 rim <- rim[,c(500:ncol(rim),1:499)]
 imageR(R <- Re(fft((fft(lim)*Conj(fft(rim)))/Mod(fft(lim)*Conj(fft(rim))),inverse=T)))
 hist(R <- Re(fft((fft(lim)*Conj(fft(rim)))/Mod(fft(lim)*Conj(fft(rim))),inverse=T)))
 which(R==max(R),arr.ind=T)
 
 Cor <- diag(1,51)
 rho <- .95
 Cor <- rho^abs(row(Cor)-col(Cor))
 Cor <- kronecker(Cor,Cor)
 draw <- rmvnorm(1,rep(0,nrow(Cor)),Cor)
 draw <- (draw-min(draw))/(max(draw)-min(draw))
 lim[50:100,150:200] <- draw
 rim[150:200,250:300] <- draw
 
 lim <- read.bitmap("c:/users/cmcneil/Pictures/Im3_L.png")[,,1]
 rim <- read.bitmap("c:/users/cmcneil/Pictures/Im3_R.png")[,,1]
 minrows <- min(nrow(lim),nrow(rim))
 mincols <- min(ncol(lim),ncol(rim))
 lim <- lim[1:minrows,1:mincols]
 rim <- rim[1:minrows,1:mincols]
 
 dim(lim);dim(rim)
 
 mywindow <- function(mat,ci,cj,rad=3,type=c("rect","normal"),permute=F,Win=NULL) {
   type <- match.arg(type)
   if(type=="rect") {
     if(permute)
       win <- matrix(sample(mat),nrow(mat),ncol(mat))
     else
       win <- matrix(0,nrow(mat),ncol(mat))
     win[max(ci-rad,1):min(ci+rad,nrow(mat)),max(cj-rad,1):min(cj+rad,ncol(mat))] <- 
       mat[max(ci-rad,1):min(ci+rad,nrow(mat)),max(cj-rad,1):min(cj+rad,ncol(mat))]
     return(win)
   }
   if(type=="normal") {
     if(is.null(Win))
       return(mat*dnorm((row(mat)-ci)^2+(col(mat)-cj)^2,0,rad^2))
     else {
       return(mat*Win[(nrow(mat)-ci+1):(2*nrow(mat)-ci),(ncol(mat)-cj+1):(2*ncol(mat)-cj)])
     }
   }
 }
 
 find_shift <- function(A=NULL,B=NULL,Af=NULL,Bf_c=NULL) {
   # A <- mywindow(lim,i,j,rad,type="normal",permute=T)
   # B <- lim
   if(is.null(Af))
     Af <- fft(A)
   if(is.null(Bf_c))
     Bf_c <- Conj(fft(B))
   Rf <- Af*Bf_c/Mod(Af*Bf_c)
   if(sum(is.na(Rf))>0)
     Rf[is.na(Rf)] <- 0
   R  <- Re(fft(Rf,inverse=T))
   coord <- which(R==max(R),arr.ind=T)
   return(coord)
 }
 
 imageR <- function(mat,col=heat.colors(12)) {
   # Rotate matrix clockwise quarter turn so that image() gives correct orientation
   matR <- t(mat)[,nrow(mat):1]
   image(matR,col=col,axes=F)
 }
 
 rim_fc <- Conj(fft(rim))
 Win <- matrix(0,2*nrow(lim)-1,2*ncol(lim)-1)
 Win <- dnorm((row(Win)-nrow(lim))^2+(col(Win)-ncol(lim))^2,0,rad^2)
 i1 <- c(1i,1)
 rad <- 20
   Radx <- 50
   Rady <- 40
   Win  <- matrix(0,2*Rady+1,2*Radx+1)
   Win  <- dnorm((row(Win)-Rady)^2+(col(Win)-Radx)^2,0,rad^2)
 shift  <- matrix(0,nrow(lim),ncol(rim))
 #ties  <- 0
 #fails <- 0
 grays  <- gray((1:100)/100)
 grays2 <- as.vector(matrix(grays,ncol=2)[,2:1])
 t0 <- proc.time()
 i= 111; j=234
 for (i in ((Rady+1):(nrow(lim)-Rady)) ) {
   for (j in ((Radx+1):(ncol(lim)-Radx)) ) {
     r <- rad
     diff <- T
     while  (diff) {
       #tmp <- find_shift(mywindow(lim,i,j,rad,type="rect",Win=Win),Bf_c=rim_fc)[1,]
       #tmp <- find_shift(mywindow(lim,i,j,rad,type="rect",Win=Win),mywindow(rim,i,j,120,type="rect",Win=Win))[1,]
       tmp <- find_shift(lim[max(i-Rady,1):min(i+Rady,nrow(lim)),max(j-Radx,1):min(j+Radx,ncol(lim))]*Win,
                         rim[max(i-Rady,1):min(i+Rady,nrow(lim)),max(j-Radx,1):min(j+Radx,ncol(lim))]     )[1,]
       if (tmp[1]<10 | tmp[1] > nrow(lim)-10 | r > 100)
          diff=F
       else
         r <- r+20
     }
     shift[i-Rady,j-Radx] <- tmp%*%i1
     cat(paste0("\rRow: ",i,"    Col: ",j,paste(rep(" ",3-floor(log(j,10))),collapse=""),"  Time: ",round(proc.time()[3]-t0[3],1),"s"  ))
   }
   if (i > 3)
     imageR(Re(shift)[((Rady+1):(nrow(lim)-Rady)),((Radx+1):(nrow(rim)-Radx))],col=grays2)
 }
 
 shifto <- shift
 #shift <- Re(shift)%%ncol(shift) + 1i*(Im(shift)%%nrow(shift))
 shiftc <- (((Re(shift)+ncol(Re(shift))/2) %% ncol(Re(shift)))-ncol(Re(shift))/2) + 
          1i*(((Im(shift)+nrow(Im(shift))/2) %% nrow(Im(shift)))-nrow(Im(shift))/2)
 shift3R <- Re(shiftc[c(F,F,T),c(F,F,T)])
 shift3I <- Im(shiftc[c(F,F,T),c(F,F,T)])
 shift3R[abs(shift3I)>10] <- 0
 shift3R[abs(shift3R)>30] <- 0
 imageR(shift3R,col=grays)
 shift3R[70:75,]
 imageR(lim)
 imageR(rim)
 p3 <- list(shifto,shiftc,shift3R,shift3I) # Im3, 
 
 # Need to verify that this is finding the right shift.
 i = 70; j=101
 for (j in 4*(32:38)) {
   (temp <- find_shift(mywindow(lim,i,j,20,type="rect"),rim))
   (temp <- find_shift(lim[max(i-Rady,1):min(i+Rady,nrow(lim)),max(j-Radx,1):min(j+Radx,ncol(lim))]*Win,
                      rim[max(i-Rady,1):min(i+Rady,nrow(lim)),max(j-Radx,1):min(j+Radx,ncol(lim))]     )[1,])
   (temp <- find_shift(mywindow(lim,i,j,30,type="rect"),Bf_c=rim_fc))
   (temp <- c(Im(shift)[i,j],Re(shift)[i,j]))
   Rads <- c(Rady,Radx)
   temp <- ((temp + Rads/2) %% Rads)-Rads/2
   imageR(Sub1 <- mywindow(lim,i,j,30,type="rect"))
   imageR(mywindow(rim,(i-temp[1])%%nrow(rim),(j-temp[2])%%ncol(rim),30,type="rect"))
 }
 Sub2 <- Sub1[,c(100:ncol(Sub1),1:99)]
 Sub2 <- Sub2[c(20:nrow(Sub2),1:19),]
 imageR(Sub2)
 find_shift(lim,Sub2)
 find_shift(rim,Sub2)
 
 par(mfrow=2:1,mar=rep(0,4))
 imageR(Re(shift),col=grays2)
 dev.off()
 
 dep <- ((Re(shift)+ncol(Re(shift))/2) %% ncol(Re(shift)))-ncol(Re(shift))/2
 dep <- dep[c(F,F,T),c(F,F,T)]
 imageR(dep,col=gray((1:100)/100))
 imageR(lim,col=gray((1:30)/30))
 imageR(rim,col=gray((1:30)/30))
 
 
 image(R <- Re(find_shift(lim,Sub)))
 image(R <- Re(find_shift(rim,Sub)))
 which(R > 2500,arr.ind=T)
 
 
# End script
 