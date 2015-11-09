# Experimenting with correlation matrices
 library(data.table)
 library(MASS)
 library(Matrix)
 
 corm <- diag(rep(1,5))
 corm[cbind(1:4,2:5)] <- .5
 corm[cbind(2:5,1:4)] <- .5
 corm[cbind(c(1:3,3:5),c(3:5,1:3))] <- .25
 corm[(cbind(c(1:2,4:5),c(4:5,1:2)))] <- .5
 corm[1,5] <- 1
 corm[5,1] <- 1
 
 sigdiag <- rep(1.2,5)
 sig <- sqrt(solve(diag(sigdiag)))%*%corm%*%sqrt(solve(diag(sigdiag)))
 
 ro <- .9
 nd <- 49  # An odd number
 roseq <- ro^(-abs((1:nd)-(nd+1)/2)+(nd-1)/2)
 corm <- diag(rep(1,nd))
 corm[cbind(as.vector(row(corm)),as.vector(col(corm)))] <- roseq[abs(row(corm)-col(corm))+1]
 #corm[1:6,(nd-6):(nd)]
 
 sigdiag <- rep(1.2,nd)
 sig <- sqrt(solve(diag(sigdiag)))%*%corm%*%sqrt(solve(diag(sigdiag)))
 mu <- rep(0,nd)
 draw <- mvrnorm(400,mu,sig)[,-1]
 
 col <- heat.colors(12)
 col <- terrain.colors(10)
 col <- topo.colors(20)
 map <- cbind(draw,draw,draw,draw,draw,draw,draw,draw,draw,draw,draw)
 image(t(map[nrow(map):1,]),col=col,axes=F)
 
 dev.off()
 
 # Functions
 rnormcor2 <- function(n,mu,sig,m,rho1=.9,rho2=.97) {
   roseq <- rho1^(-abs((1:m)-(m+1)/2)+(m-1)/2) #Smiling correlations (circular)
   corm <- diag(rep(1,m))
   corm[cbind(as.vector(row(corm)),as.vector(col(corm)))] <- roseq[abs(row(corm)-col(corm))+1]
   
   roseq2 <- c(rho2^(0:n)) # Decaying correlations
   corm2 <- diag(rep(1,n))
   corm2[cbind(as.vector(row(corm2)),as.vector(col(corm2)))] <- roseq2[abs(row(corm2)-col(corm2))+1]
   #corM <- kronecker(corm2,corm)
   
   #sigdiag <- diag(rep(sig,m*10))
   #Sig <- sqrt(solve(sigdiag))%*%corM%*%sqrt(solve(sigdiag))
   Sig <- corm/sig
   ch <- chol(corm2)
   
   draw <- mvrnorm(n,rep(mu,m),Sig)[,-1]
   draw2 <- t(t(draw)%*%ch)
   return(draw)
 }
 rnormcor <- function(n,mu,sig,m,rho) {
   roseq <- rho^(-abs((1:m)-(m+1)/2)+(m-1)/2) #Smiling correlations (circular)
   corm <- diag(rep(1,m))
   corm[cbind(as.vector(row(corm)),as.vector(col(corm)))] <- roseq[abs(row(corm)-col(corm))+1]
   
   # Create covariance matrix from correlation matrix
   sigdiag <- diag(rep(sig,m))
   Sig <- sqrt(solve(sigdiag))%*%corm%*%sqrt(solve(sigdiag))
   
   draw <- mvrnorm(n,rep(mu,m),Sig)[,-1]
   return(draw)
 }
 shorten <- function(base,ind,num) {
   n <- length(base)
   #cat(paste0("  Shorten ",n," ",ind," ",num,"\n"))
   if(num > n-ind)
     return(shorten(base[1:(ind-1)],1,num-(n-ind+1)))
   else
     return(base[c(0:(ind-1),(ind+num):n)])
 }
 
 lengthen <- function(base,ind,num,mu,sig) {  # Needs fixing
   n <- length(base)
   #newseg <- rnorm(num,mu,sig)
   
   newseg <- rnorm(1,base[ind],sig)
   if (num > 1)
     for (i in 2:num)
       newseg[i] <- rnorm(1,newseg[i-1],sig/4)
   
   #newseg <- sample(base,num,replace=T)
   
   #newseg <- sort(rnorm(num,mean(base),sd(base)),decreasing=ifelse(runif(1)>.5,T,F))
   return(c(base[0:(ind-1)],newseg,base[ind:n]))
 }
 
 draw3d <- function(infile=NULL,bw=NULL,col=NULL,numcols=20,repeats=10,scale=10,rho=.9,rho1=.9,rho2=.97,mu=0,
                    nd=49,sig=1.2,plot=T,outfile=NULL,clip=T) {
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
     bw <- round((1-bw)*scale)/scale
   }
   
   #draw <- rnormcor(nrow(bw),mu,sig,nd,rho)
   draw <- rnormcor2(nrow(bw),mu,sig,nd,rho1,rho2)
   
   pic <- matrix(0,nrow=nrow(bw),ncol=ncol(bw))
   pic[,1] <- draw[,1]
   for (rw in 1:nrow(draw)) {
     base <- draw[rw,]
     if(bw[rw,1] > 0) {
       base <- shorten(base,ind,round(scale*(bw[rw,1])))
     }
     pic[rw,1] <- base[1]
     ind <- 2
     for (cl in 2:ncol(pic)) {
       if(bw[rw,cl] > bw[rw,cl-1]) {
         base <- shorten(base,ind,round(scale*(bw[rw,cl] - bw[rw,cl-1])))
         if (ind > length(base))
           ind <- 1
       }
       if(bw[rw,cl] < bw[rw,cl-1]) {
         base <- lengthen(base,ind,round(scale*(bw[rw,cl-1] - bw[rw,cl])),mu[ind],sigdiag[ind])
       }
       pic[rw,cl] <- base[ind]
       ind <- (ind%%length(base))+1
       cat(paste0("\rRow: ",rw," Col: ",cl))
     }
   }
   img <- t(pic[nrow(pic):1,])
   if (clip) {
     img <- ifelse(img < mean(img)-2*sd(img),mean(img)-2*sd(img),img)
     img <- ifelse(img > mean(img)+2*sd(img),mean(img)+2*sd(img),img)
   }
   
   if(plot)
     image(img,col=col,axes=F)
   if(!is.null(outfile)){
     jpeg(outfile)
     image(img,col=col,axes=F)
     dev.off()
   }
   return(invisible(img))
 }
 dev.off()
 
 
 
 # Create black/white matrix from plot -- Possible?
 
 #bitmap("DonutBW.bmp")
 #dev.off()
 
 jpeg("B_W.jpg")
 #Plot something
 par(mai=rep(0,4))
 plot(c(1,2),rep(1,2),cex=40,lwd=60,axes=F,xlab="",ylab="",col="gray90",xlim=c(0,3))
 for(i in 1:19)
 points(c(1,2),rep(1,2),cex=40,lwd=60-3*i,col=paste0("gray",95-5*i))
 dev.off()
 # Now need to leave R, open jpg and save as bmp
 library(readbitmap)
 bw <- round((1-read.bitmap("Donut.bmp")[,,3]/255)*scale)/scale
 table(bw)
 
 map2 <- map
 block <- map2[100:300,101:120]
 map2[100:300,101:400] <- cbind(map[100:300,121:400],block)
 image(t(map2[nrow(map2):1,]),col=col,axes=F)
 
 pts <- data.table(x=runif(50),y=runif(50),z=round(runif(50)*9+1)/10)[order(z,decreasing=T)]
 jpeg("Rings.jpg",height=500,width=600)
 par(mai=rep(0,4))
 plot(pts$x,pts$y,cex=20,lwd=25,axes=F,xlab="",ylab="",col=rgb(pts$z,pts$z,pts$z))
 dev.off()
 
 bw <- round((1-read.bitmap("Rings.bmp")[,,3]/255)*scale)/scale
 img <- draw3d(infile="Rings.bmp",outfile="RingsO.jpg",plot=T,scale=20)
 img <- draw3d(infile="B_W.jpg",outfile="VolcanoO.jpg",plot=T,scale=20,rho2=1)
 
 image(img,col=col)
 
 jpeg("test.jpg",height=30,width=60)
 par(mai=rep(0,4))
 plot(1,cex=9,pch=19,xlim=1:2,ylim=1:2)
 dev.off()
 
 bw <- round((1-read.bitmap("test.bmp")[,,3]/255)*scale)/scale
 bw
 
# End script
 