# Calculate overlap of normal (or not normal) distributions 
 library(data.table)
 
 # Sorting method
 N <- 1000
 np <- NULL
 for (i in 1:N) {
   x <- rnorm(1000)
   y <- rnorm(1000)
   df <- data.table(cat=rep(1:2,each=1000),value=c(x,y))
   df[,sum(abs(diff(cat)))]
   np[i] <- df[order(value),sum(abs(diff(cat)))]
   
 }
 curve(dnorm(x),min(df$value),max(df$value))
 curve(dnorm(x,1),min(df$value),max(df$value),add=T)
 hist(np)
 
 np <- NULL
 for (i in 1:N) {
   x <- rnorm(1000)
   #y <- rnorm(1000)
   y <- rnorm(1000,,.5)
   df <- data.table(cat=rep(1:2,each=1000),value=c(x,y))
   df[,sum(abs(diff(cat)))]
   df[order(value),rank:=1:.N]
   #np[i] <- df[order(value),sum(abs(diff(abs(diff(cat)))))]
   np[i] <- df[,sum(rank),by=cat]$V1[1]
 }
 hist(np)
 
 
 multinom
 
 one <- rep(1:4,times=this[,1])
 two <- rep(1:4,times=this[,1])
 thr <- data.table(cat=rep(1:2,each=1000),value=c(one,two))
 table(thr$cat)
 
 N <- 1000
 n <- 5000
 dseq <- seq(0,20,.5)
 sseq <- exp(seq(-10,10))
 np <- matrix(0,nrow=length(dseq),ncol=length(sseq))
 avg <- function(x,y=NULL) {
   if(is.null(y))
     return((x[-1]+x[-length(x)])/2)
   else
     return((x+y)/2)
 }
 for (i in seq_along(dseq)) {
   for (j in seq_along(sseq)) {
     d <- dseq[i]
     s <- sseq[j]
     x <- rnorm(n)
     y <- rnorm(n,d,s)
     df <- data.table(cat=rep(1:2,each=1000),value=c(x,y))
     mnx <- mean(x)
     mny <- mean(y)
     sdx <- sd(x)
     sdy <- sd(y)
     myseq <- seq(min(c(x,y)),max(c(x,y)),length.out=1000)
     np[i,j] <- sum(diff(myseq)*avg(pmin(dnorm(myseq,mnx,sdx),dnorm(myseq,mny,sdy))))
     #df[,sum(abs(diff(cat)))]
     #df[order(value),rank:=1:.N]
     #np[i] <- df[order(value),sum(abs(diff(abs(diff(cat)))))]
     #np[i,j] <- df[,sum(rank),by=cat]$V1[1]
     cat(paste0("\rd=",d," s=",s))
   }
 }
 np
 
 
 # Final function
 norm.diff <- function(m1,sd1,m2,sd2) { 
   1 - integrate(function(x) {pmin(dnorm(x,m1,sd1), dnorm(x,m2,sd2))}, -Inf, Inf)$value
 }
 
 bd <- function(mu1,sd1,mu2,sd2) {log((sd1^2/sd2^2 + sd2^2/sd1^2+2)/4)/4 + ((mu2-mu1)^2/(sd1^2+sd2^2))/4}
 bc <- function(mu1,sd1,mu2,sd2) {1/exp(bd(mu1,sd1,mu2,sd2))}
 bc2 <- function(mu1,sd1,mu2,sd2) {integrate(function(x) sqrt(dnorm(x,mu1,sd1)*dnorm(x,mu2,sd2)),-Inf,Inf)$value}
 
 bc(0,1,0,1)
 bc(0,1,1,1)
 bc2(0,1,0,1)
 bc2(0,1,1,1)
 
 N <- 1000
 n <- 5000
 dseq <- seq(0,10,.05)
 sseq <- exp(seq(-10,10,.5))
 dif <- matrix(0,nrow=length(dseq),ncol=length(sseq))
 for (i in seq_along(dseq)) {
   for (j in seq_along(sseq)) {
     d <- dseq[i]
     s <- sseq[j]
     x <- rnorm(n)
     y <- rnorm(n,d,s)
     df <- data.table(cat=rep(1:2,each=1000),value=c(x,y))
     mnx <- mean(x)
     mny <- mean(y)
     sdx <- sd(x)
     sdy <- sd(y)
     myseq <- seq(min(c(x,y)),max(c(x,y)),length.out=1000)
     #dif[i,j] <- 1-norm.diff(0,1,d,s) - bc(0,1,d,s)
     dif[i,j] <- bc2(0,1,d,s) - bc(0,1,d,s)
     cat(paste0("\rd=",d," s=",s))
   }
 }
 hist(dif)
 image(dif,xlab=expression(mu[2]-mu[1]),ylab=expression(sigma[2]/sigma[1]))
 
 1-norm.diff(0,1,1,1) # matches pnorm(-.5)*2
 bc(0,1,1,1)  # Not close to theoretical value
 
 
# End script
 