# Kalman filtering, KFAS experimenting
 library(KFAS)
 library(MASS)
 library(ggplot2)
 library(reshape2)
 
 ## Generate time series data with known state
 generateState <- function(n,s=1,Fm=diag(1,s),R=diag(1,s),Q=diag(1,s),H=diag(1,s),B=diag(1,s),u=matrix(0,n,s)) {
   if(is.null(dim(R)))
     R <- diag(R,s)
   if(is.null(dim(Q)))
     Q <- diag(Q,s)
   x <- mvrnorm(n,mu=rep(0,s),Q)  # Noise added at state transition
   y <- mvrnorm(n,mu=rep(0,s),R)  # Noise added at measurement of state
   # st contains the true states at each time, obs contains observed values
   st <- x
   for (i in 2:nrow(x)) {
     st[i,] <- Fm%*%st[i-1,] + B%*%u[i,] + x[i,]
   }
   obs <- st + y
   return(list(state=st,observed=obs))
 }
 
 s <- 2 # Number of states
 u <- rep(0,s) # Mean noise of states
 Fm <- matrix(c(1,0,.3,1),2,2) # Transition matrix
 H  <- diag(rep(1,s))
 sigR <- 5.2
 rhoR <- .5
 R    <- sigR*rhoR^(abs(col(Fm)-row(Fm)))
 sigQ <- .5
 rhoQ <- .5
 Q    <- sigQ*rhoQ^(abs(col(Fm)-row(Fm)))
 
 ts <- generateState(1000,s=2,Fm=Fm,R=R,Q=Q)
 
 plot(ts$state[,1],type="l",lty=2)
 lines(ts$obs[,1],type="l")
 plot(ts$state[,2],type="l",lty=2)
 lines(ts$obs[,2],type="l")
 
 hist(ts$obs[,1]-ts$state[,1],breaks=20)
 hist(ts$obs[,2]-ts$state[,2],breaks=20)
 
 # Find best state estimates using Kalman filter
 fitKF <- function(obs,Fm=NULL,R,Q=NULL,H=NULL,B=NULL,u=NULL,usearima=F) {
   if(is.null(dim(obs)))
     obs <- matrix(obs)
   s <- ncol(obs) # Number of states
   n <- nrow(obs) # Number of observations
   if(is.null(B)) {
     B <- diag(1,s)
     u <- matrix(0,n,s)
   }
   if(is.null(H))
     H <- diag(1,s)
   xp <- xm <- matrix(0,n,s)
   Pp <- Pm <- array(0,dim=c(n,s,s))
   xp[1,]  <- 0
   Pp[1,,] <- cov(obs)
   K       <- Pp[1,,]%*%t(H)%*%solve(H%*%Pp[1,,]%*%t(H)+R)
   xm[1,]  <- xp[1,]+K%*%(obs[1,]-H%*%xp[1,])
   Pm[1,,] <- Pp[1,,] - K%*%H%*%Pp[1,,]
   # A kind of EM algorithm (expectation = prediction, maximization = measurement update)
   for (i in 2:n) {
     if(usearima & i>13) {
       tmp <- predict(arima(xm[1:(i-1),],order=c(1,0,0)),1)
       xp[i,]  <- tmp$pred
       Pp[i,,] <- tmp$se + var(xm[1:(i-1),])
     }
     else {
       xp[i,]  <- Fm%*%xm[i-1,] + B%*%u[i-1,]
       Pp[i,,] <- Fm%*%Pm[i-1,,]%*%t(Fm) + Q
     }
     K       <- Pp[i,,]%*%t(H)%*%solve(H%*%Pp[i,,]%*%t(H)+R)
     xm[i,]  <- xp[i,] + K%*%(obs[i,]-H%*%xp[i,])
     Pm[i,,] <- Pp[i,,] - K%*%H%*%Pp[i,,]
   }
   
   return(list(States=xm,Predictions=xp,Cov=Pm))
 }
 
 #ts <- generateState(1000,s=2,Fm=Fm,R=R,Q=Q)
 #mod1 <- fitKF(ts$obs,Fm,R,Q)
 #mod2 <- fitKF(ts$obs,Fm,R,Q)
 
 ts <- generateState(1000,s=1,Fm=.9,R=2,Q=.5)
 mod1 <- fitKF(ts$obs,.9,2,.5)     # Knowing true parameters
 mod2 <- fitKF(ts$obs,.57,2.1,.2) # Guessing true parameters
 mod3 <- arima(ts$obs,order=c(1,0,0),include.mean =F)
 mod3 <- arima(ts$obs,order=c(1,0,0),xreg=c(0,head(ts$obs,-1)))
 mod4 <- fitKF(ts$obs,.6,1.5,1.1,usearima=T)
 
 coefficients(lm(ts$obs[-1]~head(ts$obs,-1)))
 
 obserr    <- ts$obs-ts$state
 kferr1    <- mod1$States - ts$state
 kferr2    <- mod2$States - ts$state
 kferr1.2  <- mod1$Predictions - ts$state
 kferr2.2  <- mod2$Predictions - ts$state
 arimaerr  <- fitted(mod3) - ts$state              # Not the same. Not compared to states.
 arimaerr2 <- (fitted(mod3)+ts$obs)/2 - ts$state   # Not the same. Not compared to states.
 kferr4    <- mod4$States - ts$state
 
 mean(obserr^2)
 mean(kferr1^2)
 mean(kferr2^2)
 mean(kferr1.2^2)
 mean(kferr2.2^2)
 mean(arimaerr^2)
 mean(arimaerr2^2)
 mean(kferr4^2)
 
 # Plots
 df <- data.table(State=ts$state[,1],Obs=ts$observed[,1],ind=1:length(ts$state),
                  Mod1=mod1$States[,1],Mod2=mod2$States[,1],Mod3p=as.numeric(fitted(mod3)),
                  Mod1p=mod1$Predictions[,1],Mod2p=mod2$Predictions[,1],Mod3=as.numeric((fitted(mod3)+ts$obs)/2))
 ggplot(df[1:200,],aes(ind,State)) + geom_line(col="red") + geom_line(data=df[1:200,],aes(ind,Obs)) +
   geom_line(data=df[1:200,],aes(ind,Mod1),col="orange") + 
   geom_line(data=df[1:200,],aes(ind,Mod2),col="yellow") + 
   geom_line(data=df[1:200,],aes(ind,Mod3),col="green")
 
 
 
 ##
 ## Test methods by comparison of MSE
 ##
 
 gen.methods <- c("state","arima")                      # Time series generating methods
 fit.methods <- c("kf.known","kf.est","arima","naive")  # Estimation/Prediction methods
 n <- 1000                                              # Length of each time series
 N <- 100                                               # Number of time series of each type
 
 MSE1 <- MSE2 <- NULL
 dt <- data.table(Generator="",Model="",MSE1_Mean=0,MSE1_SD=0,MSE2_Mean=0,MSE2_SD=0,Time_N=0)[0]
 
 for (k in seq_along(gen.methods)) {
   for (l in seq_along(fit.methods)) { # Different fitting methods 
     # ( when k=1 and l=1, the "known" parameters may represent a random guess)
     t0 <- proc.time()
     mse1 <- NULL
     mse2 <- NULL
     for (i in 1:N) {
       Fm=runif(1,.6,1)
       Q =runif(1,.5,1.4)
       R =runif(1,.7,3)
       # Generate data
       if (k==1)
         ts <- generateState(n,Fm=Fm,Q=Q,R=R)
       else if (k==2) {
         ts <- NULL
         while(is.null(ts))
           try(ts <- list(state=NA,observed=arima.sim(model=list(ar=Fm),n=n)),silent=T)
       }
       # Fit model
       if (l==1)
         mod <- fitKF(ts$observed,Fm,R,Q)
       else if (l==2) {
         Fm.est <- median(ts$obs[-1]/head(ts$obs,-1))
         Q.est  <- var(ts$obs[-1]-Fm.est*head(ts$obs,-1))
         R.est  <- var(ts$obs)
         mod <- fitKF(ts$observed,Fm.est,R.est,Q.est)
       }
       else if (l==3) {
         mod <- auto.arima(ts$observed,seasonal=F)
         mod <- list(States=(fitted(mod)+ts$observed)/2,Predictions=fitted(mod))
       }
       else if (l==4) {
         mod <- list(States=ts$observed,Predictions=c(0,head(ts$observed,-1)))
       }
       mse1 <- c(mse1,mean((mod$States-ts$state)^2))
       mse2 <- c(mse2,mean((mod$Predictions-ts$observed)^2))
       cat(paste0("\rk: ",k,"   l: ",l,"   i: ",i,"  "))
     }
     MSE1 <- cbind(MSE1,mse1)
     MSE2 <- cbind(MSE2,mse2)
     t1 <- (proc.time() - t0)
     dt <- rbind(dt,data.table(Generator=gen.methods[k],Model=fit.methods[l],MSE1_Mean=mean(mse1),MSE1_SD=sd(mse1),MSE2_Mean=mean(mse2),MSE2_SD=sd(mse2),Time_N=t1[3]))
   }
 }
 MSE1 <- data.table(MSE1)
 MSE2 <- data.table(MSE2)
 setnames(MSE1,apply(expand.grid(fit.methods,gen.methods),1,paste,collapse="_"))
 setnames(MSE2,apply(expand.grid(fit.methods,gen.methods),1,paste,collapse="_"))
 MSETab1 <- melt(MSE1,value.name="MSE",variable.name="Combination")[,Generator:=gsub(".*_","",Combination)][,Method:=gsub("_.*","",Combination)]
 MSETab2 <- melt(MSE2,value.name="MSE",variable.name="Combination")[,Generator:=gsub(".*_","",Combination)][,Method:=gsub("_.*","",Combination)]
 
 setnames(dt,"Time_N",paste0("Time_",N))
 dt
 
 
 
 
 # Graphs
 ggplot(MSETab1,aes(MSE,fill=Method)) + geom_histogram(position="dodge") + labs(x="State Estimation MSE",y="Count",fill="")
 ggplot(MSETab1[MSE < 2],aes(MSE,fill=Method)) + geom_histogram(position="dodge") + labs(x="State Estimation MSE",y="Count",fill="")
 
 ggplot(MSETab2[Generator=="state"],aes(MSE,fill=Method)) + geom_histogram(position="dodge") + labs(x="Prediction MSE",y="Count",fill="")
 ggplot(MSETab2[Generator=="state" & MSE < 5],aes(MSE,fill=Method)) + geom_histogram(position="dodge") + labs(x="Prediction MSE",y="Count",fill="")
 
 ggplot(MSETab2[Generator=="arima"],aes(MSE,fill=Method)) + geom_histogram(position="dodge") + labs(x="Prediction MSE",y="Count",fill="")
 ggplot(MSETab2[Generator=="arima" & MSE < 2],aes(MSE,fill=Method)) + geom_histogram(position="dodge") + labs(x="Prediction MSE",y="Count",fill="")
 
 
 
 
 # Repeat including covariates (binary, continuous, both)
 
 
 
 
# End script
 