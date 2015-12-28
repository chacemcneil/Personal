# Time series function
library(forecast)
library(data.table)

# This does not work for long time series (uses nxn matrices)
generateARIMA <- function(n,sig=1,d=0,phi=NULL,theta=NULL,D=0,Phi=NULL,Theta=NULL,m=12,seed=NULL,plot=T) {
  B  <- cbind(0,rbind(diag(1,n-1),0))
  PhiB <- ThetaB <- Btmp <- phiB <- thetaB <- Btmp <- Id <- diag(1,n)
  if(!is.null(Phi) | !is.null(Theta) | D > 0) {
    #seasonal=T
    Bs <- Id
    for (i in 1:m)
      Bs <- Bs%*%B
    if(length(c(Phi,Theta))>0)
      for (i in 1:max(length(Phi),length(Theta))) {
        Btmp   <- Btmp%*%Bs
        if(i <= length(Phi))
          PhiB   <- PhiB - Phi[i]*Btmp
        if(i <= length(Theta))
          ThetaB <- ThetaB + Theta[i]*Btmp
      }
  }
  else
    #seasonal=F
    
  Btmp <- Id
  if(length(c(phi,theta))>0)
    for (i in 1:max(length(phi),length(theta))) {
      Btmp   <- Btmp%*%B
      if(i <= length(phi))
        phiB   <- phiB - phi[i]*Btmp
      if(i <= length(theta))
        thetaB <- thetaB + theta[i]*Btmp
    }
  
  set.seed(seed)
  e <- rnorm(n,0,sig)
  y <- solve(phiB%*%PhiB)%*%(thetaB%*%ThetaB)%*%e
  
  dB <- DB <- Id
  if(d > 0)
    for (i in 1:d)
      dB <- dB%*%(Id-B)
  if(D > 0)
    for (i in 1:D)
      DB <- DB%*%(Id-Bs)
  
  y <- solve(dB%*%DB)%*%y
  
  if(plot)
    plot(y,type="l")
  
  return(y)
}

generateARIMA2 <- function(n,sig=1,d=0,phi=NULL,theta=NULL,D=0,Phi=NULL,Theta=NULL,m=12,seed=NULL,plot=T) {
  set.seed(seed)
  e <- rnorm(n,0,sig)
  eta <- e
  if(!is.null(Theta))
    for (i in 1:length(Theta))
      eta <- eta + Theta[i]*c(rep(0,i*m),head(e,-i*m))
  if(!is.null(theta))
    for (i in 1:length(theta))
      eta <- eta + theta[i]*c(rep(0,i),head(e,-i))
  
  
  y <- solve(phiB%*%PhiB)%*%(thetaB%*%ThetaB)%*%e
  
  dB <- DB <- Id
  if(d > 0)
    for (i in 1:d)
      dB <- dB%*%(Id-B)
  if(D > 0)
    for (i in 1:D)
      DB <- DB%*%(Id-Bs)
  
  y <- solve(dB%*%DB)%*%y
  
  if(plot)
    plot(y,type="l")
  
  return(y)
}





one <- generateARIMA(1e4,theta=.4,seed=12)
set.seed(12)
plot(two <- arima.sim(list(ma=c(.4)),400),type="l")


# Check time series
arima.sim(list(ar=c(.8,.1),ma=.5),500)


















# End script
