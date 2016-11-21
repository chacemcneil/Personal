# Modifying correlation/covariance matrix for linear regression
 library(data.table)
 library(ggplot2)
 library(mgcv)
 library(mvtnorm)
 library(nlme)
 
 n <- 200
 mu <- 2
 Mu <- rep(mu,n)
 sig2 <- 1.5
 Sig <- sig2*diag(n)
 
 # probability that two adjacent observations are correlated
 p <- .7
 sig2 <- 1.5
 rho <- .5
 #Sig[3,4] <- Sig[4,3] <- 1
 #Cor <- diag(1/sqrt(diag(Sig))) %*% Sig %*% diag(1/sqrt(diag(Sig)))
 
 Cor <- diag(n)
 rand <- sort(floor(runif(n)*50))
 Cor[cbind(1:(n-1),2:n)] <- Cor[cbind(2:n,1:(n-1))] <- rho * rbinom(n-1,1,p)
 Sig <- Cor * sig2
 matrixcalc::is.positive.definite(Sig)
 
 Sig <- do.call(Matrix::bdiag, lapply(table(rand), function(x) (sig2-rho)*diag(x)+rho ))
 
 a = 3
 b = 2
 x <- rnorm(n,3,1)
 N <- 1e3
 as <- bs <- numeric()
 for (i in 1:N) {
   y <- rmvnorm(1,a + b*x,Sig)[1,]
   tab <- summary(lm(y~x))$coef
   as[i] <- tab[1,1]
   bs[i] <- tab[2,1]
   cat(paste0("\rIter: ",i))
 }
 hist(as); mean(as); sd(as)
 hist(bs); mean(bs); sd(bs)
 
 
 for (i in 1:N) {
   y <- rnorm(n,a + b*x,sig2)
   tab <- summary(lm(y~x))$coef
   as[i] <- tab[1,1]
   bs[i] <- tab[2,1]
   cat(paste0("\rIter: ",i))
 }
 hist(as); mean(as); sd(as)
 hist(bs); mean(bs); sd(bs)
 
 
 rand <- cumsum(1-c(1,diag(Sig[-nrow(Sig),-1])/.75))
 
 
 
 
 dat <- data.table(y=y,x=x,rand=rand)
 
 n <- 300
 g <- 50
 rand <- rnorm(g,0,.6)
 a <- 3
 b <- 2
 dat <- data.table(x = rnorm(n,20,10), grp = ceiling(runif(n)*g))[,rand:=rand[grp]][,y:= a + b*x + rand + rnorm(n,0,1.3)]
 mod <- lme(fixed=y~x,random=~1|grp,data=dat,correlation=corCompSymm(form=~1|grp))
 mod <- lm(y~x,data=dat)
 summary(mod)
 
 image(extract.lme.cov(mod,data=dat))
 table(extract.lme.cov(mod,data=dat))
 
 
 table(dat$grp)
 
 
 
# End script
 