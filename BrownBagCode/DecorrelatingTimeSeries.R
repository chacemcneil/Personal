# Idea for Brown Bag presentation: time series analysis, decorrelating residuals
 library(forecast)
 library(data.table)
 library(ggplot2)
 library(orcutt)
 
 setwd("r:/users/cmcneil/Projects/Miscellaneous/BrownBag/DecorrelatingTimeSeries")
 
 ## Load useful functions
 #source("r:/users/cmcneil/Projects/Miscellaneous/General Code/TimeSeries.R",verbose=T)
 file.edit("r:/users/cmcneil/Projects/Miscellaneous/General Code/TimeSeries.R",verbose=T)
 
 # generateARIMA and arima.sim seem to be very similar, main difference is a burnin.
 set.seed(1003)
 x <- arima.sim(list(ar=c(0.7,.2,-.1),ma=c(0.5)),1000)
 plot(x,type="l")
 set.seed(1003)
 y <- generateARIMA(1000,phi=c(0.7,.2,-.1),theta=.5,plot=T)
 mean(x)
 
 ccf(x,y)
 head(cbind(x,y),30)
 plot(head(x,-30),tail(y,-30))
 hist(head(x,-30)-tail(y,-30))
 plot(head(x,-2),tail(y,-2))
 hist(head(x,-2)-tail(y,-2))
 #
 
 # My function for decorrelating time series
 myDecor <- function(y,X=NULL,max.p=5,max.q=5,epsilon=0.01) {
   mod <- lm(y~X)
   coef <- coefficients(mod)
   diff <- 1
   newarma <- coefficients(auto.arima(residuals(mod)))
   B <- rbind(0,cbind(diag(1,n-1),0))
   iter <- 0
   
   while(diff > epsilon) {
     newphi   <- newarma[grep("ar",names(newarma))]
     newtheta <- newarma[grep("ma",names(newarma))]
     
     PhiB <- ThetaB <- diag(1,n)
     Bpow <- B
     for (i in seq_along(newphi)) {
       PhiB <- PhiB - newphi[i]*Bpow
       Bpow <- Bpow%*%B
     }
     Bpow <- B
     for (i in seq_along(newtheta)) {
       ThetaB <- ThetaB + newtheta[i]*Bpow
       Bpow <- Bpow%*%B
     }
     A <- solve(ThetaB)%*%PhiB
     denom <- (1-sum(newphi))/(1+sum(newtheta))
     
     newy <- A%*%y
     newX <- A%*%X
     mod <- lm(newy~newX)
     newarma  <- coefficients(auto.arima(solve(A)%*%residuals(mod),max.q=max.q,max.p=max.p))
     diff <- max(abs(coef-coefficients(mod)/c(denom,rep(1,length(coef)-1))))
     coef <- coefficients(mod)/c(denom,1)
     
     #summary(mod)
     iter <- iter + 1
     cat(paste0("\rIteration: ",iter, "\tDifference: ",diff))
   }
   tab <- coefficients(summary(mod))
   tab[,1:2] <- tab[,1:2]/c(denom,1)
   return(list(coef=tab,arma=newarma,iterations=iter))
 }
 
 ## Set parameters
 phi   <- c(0.7,-0.2)
 theta <- c(.8)
 n <- 1e3
 b0 <- 3.1
 b1 <- 2.3
 
 x <- rnorm(n,1,2)
 e <- generateARIMA(n,phi=phi,theta=theta,plot=F,sig=3)
 y <- b0 + b1*x + e
 
 AIC(arima(y,order=c(2,0,0)))
 AIC(arima(y,order=c(1,0,0)))
 AIC(arima(y,order=c(2,0,1)))
 auto.arima(y)
 
 (dr <- auto.arima(y,xreg=x))
 (md <- myDecor(y,x,max.q=2))
 (co <- cochrane.orcutt(lm(y~x))$Cochran$coef)
 
 
 
 
 
# End script
 