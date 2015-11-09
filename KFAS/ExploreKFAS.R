# Kalman filtering, KFAS experimenting
 library(KFAS)
 library(data.table)
 library(ggplot2)
 library(reshape2)
 library(forecast)
 library(MASS)
 
 setwd("C:/Users/cmcneil/Documents/Projects/Miscellaneous/KFAS/")
 
 dat <- data.table(read.csv("020_ksyyt_tau_102cm.csv",header=F,stringsAsFactors=F))
 setnames(dat,c("Gender","Age","Cause",1969:2013))
 dat[Gender == "Genders total",Gender:="All"]
 
 dat <- melt(dat,id.vars=c("Gender","Age","Cause"),variable.name="Year",value.name="Deaths",variable.factor=F)
 dat$Year <- as.numeric(dat$Year)
 setkeyv(dat,c("Gender","Age","Cause","Year"))
 
 tabA <- dat[Gender=="All"]
 tab1 <- dat[Gender=="Males"]
 tab2 <- dat[Gender=="Females"]
 identical(tabA[,Deaths],tab1[,Deaths]+tab2[,Deaths])
 
 genders<- c("All","Males","Females")
 years  <- 1969:2013
 causes <- unique(dat$Cause)
 ages   <- unique(dat$Age)
 ages <- ages[order(as.numeric(gsub("([[:alnum:]]*)\\s.*","\\1",ages)))]
 
 
 dat2 <- data.table(read.csv("125_vaerak_tau_106cm.csv",stringsAsFactors=F,header=F))
 setnames(dat2,c("Year",apply(expand.grid(c("Total",0:100),genders)[,2:1],1,paste,collapse=" ")))
 dat2 <- melt(dat2,id.vars="Year",variable.factor=F,value.name="Deaths")
 dat2 <- cbind(dat2[,list(Year)],t(as.data.frame(strsplit(dat2$variable," "))),dat2[,list(Deaths)])
 setnames(dat2,c("V1","V2"),c("Gender","Age"))
 setkeyv(dat2,c("Gender","Age","Year"))
 
 
 
 # Create data used in paper
 newdat <- dat[Age %in% ages[8:15] & Cause ==causes[3] & Gender=="All",list(.N,Deaths=sum(Deaths)),by=list(Age=substr(Age,1,1),Year)]
 newdat <- dat[Age %in% ages[8:15] & grepl("alcohol",Cause,ignore.case=T) & Gender=="All",list(.N,Deaths=sum(Deaths)),by=list(Age=substr(Age,1,1),Year)]
 newdat[,Age:=paste0(Age,"0 - ",Age,"9")]
 newdat
 
 ggplot(newdat,aes(Year,Deaths,col=Age)) + geom_line()
 ggplot(newdat,aes(Year,Deaths/1e5,col=Age)) + geom_line()
 
 # ... tbc
 
 
 ## Generate time series data with known state
 s <- 2 # Number of states
 mu <- rep(0,s) # Mean noise of states
 Fm <- matrix(c(1,0,.3,1),2,2) # Transition matrix
 H  <- diag(rep(1,s))
 sigR <- 5.2
 rhoR <- .5
 R    <- sigR*rhoR^(abs(col(Fm)-row(Fm)))
 sigQ <- .5
 rhoQ <- .5
 Q    <- sigQ*rhoQ^(abs(col(Fm)-row(Fm)))
 
 x <- mvrnorm(1000,mu=c(0,0),Q)  # Noise added at state transition
 y <- mvrnorm(1000,mu=c(0,0),R)  # Noise added at measurement of state
 
 st <- x
 for (i in 2:nrow(x)) {
   st[i,] <- st[i-1,]%*%t(Fm) + x[i,]
 }    # st contains the true states at each time
 
 obs <- st + y
 
 plot(st[,1],type="l",lty=2)
 lines(obs[,1],type="l")
 plot(st[,2],type="l",lty=2)
 lines(obs[,2],type="l")
   plot(st[,1],st[,2],type="l",lty=2)
   lines(obs[,1],st[,2],type="l")
 
 hist(obs[,1]-st[,1],breaks=20)
 hist(obs[,2]-st[,2],breaks=20)
 
 # Find best state estimates using Kalman filter
 myKF <- function(obs,Fm,R,Q,H=NULL,B=NULL,u=NULL) {
   if(is.null(dim(obs)))
     obs <- matrix(obs)
   if(is.null(B)) {
     B <- diag(1,s)
     u <- matrix(0,n,s)
   }
   if(is.null(H))
     H <- diag(1,s)
   s <- ncol(obs) # Number of states
   n <- nrow(obs) # Number of observations
   xp <- xm <- matrix(0,n,s)
   Pp <- Pm <- array(0,dim=c(n,s,s))
   xp[1,]  <- 0
   Pp[1,,] <- cov(obs)
   K       <- Pp[1,,]%*%t(H)%*%solve(H%*%Pp[1,,]%*%t(H)+R)
   xm[1,]  <- xp[1,]+K%*%(obs[1,]-H%*%xp[1,])
   Pm[1,,] <- Pp[1,,] - K%*%H%*%Pp[1,,]
   for (i in 2:n) {
     xp[i,]  <- Fm%*%xm[i-1,] + B%*%u[i-1,]
     Pp[i,,] <- Fm%*%Pm[i-1,,]%*%t(Fm) + Q
     K       <- Pp[i,,]%*%t(H)%*%solve(H%*%Pp[i,,]%*%t(H)+R)
     xm[i,]  <- xp[i,] + K%*%(obs[i,]-H%*%xp[i,])
     Pm[i,,] <- Pp[i,,] - K%*%H%*%Pp[i,,]
   }
   
   return(list(States=xm,Cov=Pm))
 }
 
 mod <- myKF(obs,F,R,Q)
 
 plot(obs[,1],type="l")
 plot(obs[,2],type="l")
 plot(mod$States[,1],type="l")
 hist(obs[,1]-st[,1])
 hist(mod$States[,1]-st[,1])
 hist(mod$States[,1]-obs[,1])
 hist(obs[,2]-st[,2])
 hist(mod$States[,2]-st[,2])
 hist(mod$States[,2]-obs[,2])
 
 plot(mod$Cov[,1,1],type="l")
 
 obserr <- obs-st
 kferr  <- mod$States-st
 
 mean(obserr^2)
 mean(kferr^2)
 
 
# End script
 