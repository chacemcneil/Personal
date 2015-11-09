# simulate data for a hierarchical bayesian analysis (eventually a structural time series as well ?)
 library(data.table)
 library(ggplot2)
 library(reshape2)
 library(nlme)
 library(MCMCpack)
 library(abind)
 
 # Parameters
 g <- 10
 n <- 200
 #membership <- rmultinom(1,n,rep(1/g,g))
 membership <- sample(1:g,200,replace=T,prob=(1:10)/sum(1:10))
 membership <- sample(1:g,200,replace=T)
 plot(table(membership))
 
 sig2e <- 1
 sig2g <- 4
 
 beta <- 1.5
 x <- rpois(n,5)
 
 # Generate mean for each group
 
 theta <- 2
 mu <- rnorm(g,theta,sqrt(sig2g))
 y <- rnorm(n,x*beta + mu[membership],sqrt(sig2e))
 
 # Useful values
 n <- length(y)
 n.mu <- table(membership)
 # Prior parameters
 m.theta <- 0; s2.theta <- 1e3
 m.beta  <- 0; s2.beta  <- 1e3
 a.e <- 1; b.e <- 1
 a.g <- 1; b.g <- 1
 # Initialize
 p.beta  <- 0
 p.sig2e <- 1
 p.sig2g <- 1
 p.theta <- 0
 p.mu    <- matrix(0,ncol=g)
 # Draws from Gibbs sampler
 Ndraws <- 1e4
 burnin <- 2000
 i <- 2
 while(i <= burnin+Ndraws) {
   # beta
   draw <- rnorm(1,
                 (p.sig2e[i-1]*m.beta + s2.beta*sum((y-p.mu[i-1,membership])*x))/(sum(x^2)*s2.beta + p.sig2e[i-1]),
                 sqrt( (s2.beta*p.sig2e[i-1])/(sum(x^2)*s2.beta + p.sig2e[i-1]) ) )
   p.beta <- c(p.beta,draw)
   # mu
   newmu <- NULL
   for (j in 1:g) {  ### possible to vectorize
     draw <- rnorm(1,
                   (p.sig2e[i-1]*p.theta[i-1] + p.sig2g[i-1]*sum(y[membership==j]-x[membership==j]*p.beta[i]))/(n.mu[j]*p.sig2g[i-1] + p.sig2e[i-1]),
                   sqrt( (p.sig2g[i-1]*p.sig2e[i-1])/(n.mu[j]*p.sig2g[i-1] + p.sig2e[i-1]) ) )
     newmu <- c(newmu,draw)
   }
   p.mu <- rbind(p.mu,newmu)
   # theta
   draw <- rnorm(1,
                 (p.sig2g[i-1]*m.theta + s2.theta*sum(p.mu[i,]))/(g*s2.theta + p.sig2g[i-1]),
                 sqrt( (s2.theta*p.sig2g[i-1])/(g*s2.theta + p.sig2g[i-1]) ) )
   p.theta <- c(p.theta,draw)
   
   # sig2e
   draw <- rinvgamma(1, n/2+a.e, b.e + sum((y - p.mu[i,membership] - x*p.beta[i])^2)/2)
   p.sig2e <- c(p.sig2e,draw)
   
   # sig2g
   draw <- rinvgamma(1, g/2+a.g, b.g + sum((p.mu[i,]-p.theta[i])^2)/2)
   p.sig2g <- c(p.sig2g,draw)
   
   # Track progress
   cat(paste0("\rIteration: ",i))
   
   i <- i+1
 }
 post <- list(mu    = p.mu[-(1:burnin),],
              beta  = p.beta[-(1:burnin)],
              theta = p.theta[-(1:burnin)],
              sig2e = p.sig2e[-(1:burnin)],
              sig2g = p.sig2g[-(1:burnin)])
 pmeans <- list(mu    = apply(post$mu,2,mean),
                beta  = mean(post$beta),
                theta = mean(post$theta),
                sig2e = mean(post$sig2e),
                sig2g = mean(post$sig2g))
 
 hist(post$beta)
 hist(post$theta)
 hist(post$sig2e)
 hist(post$sig2g)
 mean(post$beta)
 mean(post$theta)
 mean(post$sig2e)
 mean(post$sig2g)
 median(post$beta)
 median(post$theta)
 median(post$sig2e)
 median(post$sig2g)
 cbind(apply(post$mu,2,mean),mu)
 plot(apply(post$mu,2,mean),mu); abline(0,1)
 predy <- with(pmeans,mu[membership] + beta*x)
 plot(predy,y); abline(0,1)
 plot(predict(remod),y); abline(0,1)
 
 #Basic models
 lmmod <- lm(y~x)
 remod <- lme(y~x,random=~1|membership)
 summary(lmmod)
 summary(remod)
 bs <- NULL
 ints <- NULL
 for (i in 1:1e3) {
   x <- rpois(n,5)
   mu <- rnorm(g,theta,sqrt(sig2g))
   y <- rnorm(n,x*beta + mu[membership],sqrt(sig2e))
   remod <- lme(y~x,random=~1|membership)
   bs[i] <- summary(remod)$coef$fixed["x"]
   cat(paste0("\rIter: ",i))
   ints <- rbind(ints,bs[i]+summary(remod)$tTable["x","Std.Error"]*qt(c(0.025,0.975),189))
 }
 hist(bs)
 hist(ints[,1])
 
 # Intervals
 quantile(post$beta,c(0.025,0.975))
 coef(summary(remod))
 summary(remod)$coef$fixed["x"] + c(-1:1)*summary(remod)$tTable["x","Std.Error"]*qt(0.975,189)
 
 # The results from the random effects model are very similar.
 
 
 ################################################################################################
 # Implement BSTS algorithm
 ################################################################################################
 
 
 # Generate according to model
 
 lt <- 100 # length of time
 H <- 1
 Q <- 2
 alpha <- rnorm(0,sqrt(Q))
 for (i in 2:lt)
   alpha[i] <- rnorm(TT%*%alpha,sqrt(Q))
 TT <- matrix()
 Z  <- matrix()
 y <- rnorm(,H)
 
 
 # Set parameters / Generate data
 nc <- 10  # number of clinics
 nt <- 100 # number of time points (varying ?, constant for now)
 ns <- 52  # number of time periods per season
 phi <- rnorm(1,15,1)
 sig2e <- 1
 sig2c <- 2
 sig2u <- .1
 sig2v <- .01
 sig2w <- 2
 theta <- rnorm(nc,phi,sig2c)
 delta <- sapply(1:nc,function(i) cumsum(rnorm(nt,0,sig2v)))
 mu    <- sapply(1:nc,function(i) cumsum(rnorm(nt,0,sig2u)+c(0,delta[-nt,i])))
 tau   <- rnorm(ns,0,sig2w)
 circtau <- ((1:nt)-1)%%ns+1
 x <- matrix(rnorm(nt*nc),nrow=nt,ncol=nc)
 x <- apply(x,2,function(vec) ifelse(cummax(vec)==max(vec),1,0))
 Y <- matrix(theta,nt,nc,byrow=T) + mu + matrix(tau[circtau],nt,nc,byrow=F) + beta*x + matrix(rnorm(nt*nc,0,sig2e),nt,nc)
 ggplot(data.frame(melt(Y)),aes(Var1,value,col=factor(Var2),group=Var2)) + geom_line(size=1)
 rets <- lme(as.vector(Y)~as.vector(x),random=~1|rep())
 
 # Useful values
 n <- length(Y)
 circtau <- ((1:nt)-1)%%ns+1
 # Prior parameters
 m.phi   <- 5
 s2.phi  <- 100
 m.beta  <- -1
 s2.beta <- 100
 a.e <- .1; b.e <- .3
 a.c <- .1; b.c <- .3
 a.u <- .1; b.u <- .3
 a.v <- .1; b.v <- .3
 a.w <- .1; b.w <- .3
 # Initialize
 p.beta  <- 0
 p.sig2e <- 1
 p.sig2c <- 1
 p.sig2u <- 1
 p.sig2v <- 1
 p.sig2w <- 1
 p.phi   <- 0
 p.theta <- matrix(0,nrow=1,ncol=nc)
 p.mu    <- array(0,c(nt,nc,1))
 p.delta <- array(0,c(nt,nc,1))
 p.tau   <- matrix(0,nrow=1,ncol=ns)
 # Draws
 Ndraws <- 1.5e3
 burnin <- 500
 i <- 2
 while (i <= burnin + Ndraws) {
   ## beta
   draw <- rnorm(1,
                 (p.sig2e[i-1]*m.beta + s2.beta*sum((Y-matrix(p.theta[i-1,],nt,nc,T)-p.mu[,,i-1]-matrix(p.tau[i-1,circtau],nt,nc,F))*x))/(sum(x^2)*s2.beta + p.sig2e[i-1]),
                 sqrt( (s2.beta*p.sig2e[i-1])/(sum(x^2)*s2.beta + p.sig2e[i-1]) ) )
   p.beta <- c(p.beta,draw)
   
   ## theta
   #tmp <- Y - p.mu[,,i-1] - matrix(p.tau[i-1,circtau],nt,nc,F) - beta[i]*x
   #draw <- rnorm(nc,
   #              (p.sig2e[i-1]*p.phi[i-1] + p.sig2c[i-1]*apply(tmp,2,sum) )/(nt*p.sig2c[i-1] + p.sig2e[i-1]),
   #              sqrt( (p.sig2c[i-1]*p.sig2e[i-1])/(nt*p.sig2c[i-1] + p.sig2e[i-1]) ) )
   #p.theta <- rbind(p.theta,draw)
   newtheta <- NULL
   for (k in 1:nc) {  ## Could possibly be vectorized.
     draw <- rnorm(1,
                   (p.sig2e[i-1]*p.phi[i-1] + p.sig2c[i-1]*sum(Y[,k]-p.mu[,k,i-1]-p.tau[i-1,circtau]-p.beta[i]*x[,k] ) )/(nt*p.sig2c[i-1] + p.sig2e[i-1]),
                   sqrt( (p.sig2c[i-1]*p.sig2e[i-1])/(nt*p.sig2c[i-1] + p.sig2e[i-1]) ) )
     newtheta <- c(newtheta,draw)
   }
   p.theta <- rbind(p.theta,newtheta)
   
   ## phi
   draw <- rnorm(1,
                 (p.sig2c[i-1]*m.phi + s2.phi*sum(p.theta[i,]))/(nc*s2.phi + p.sig2c[i-1]),
                 sqrt( (s2.phi*p.sig2c[i-1])/(nc*s2.phi + p.sig2c[i-1]) ) )
   p.phi <- c(p.phi,draw)
   
   ## mu
   newmu <- NULL
   for (j in 1:nt) {  ### is it possible to vectorize ?
     row <- NULL
     nn <- ifelse(j==nt,1,2)
     jm <- (j-1)%%ns + 1
     for (k in 1:nc) {
       mn <- 0
       if (j<nt)
         mn <- mn + p.mu[j+1,k,i-1] - p.delta[j,k,i-1]
       if (j>1)
         mn <- mn + newmu[j-1,k] + p.delta[j-1,k,i-1]
       draw <- rnorm(1,
                     (p.sig2e[i-1]*mn + p.sig2u[i-1]*(Y[j,k]-p.theta[i,k]-p.tau[i-1,jm]-p.beta[i]*x[j,k]))/(p.sig2u[i-1] + nn*p.sig2e[i-1]),
                     sqrt( (p.sig2u[i-1]*p.sig2e[i-1])/(p.sig2u[i-1] + nn*p.sig2e[i-1]) ) )
       row <- c(row,draw)
     }
     newmu <- rbind(newmu,row)
   }
   p.mu <- abind(p.mu,newmu,along=3)
   
   ## delta
   newdelta <- NULL
   for (j in 1:nt) {  ### is it possible to vectorize ?
     row <- NULL
     nn  <- ifelse(j==nt,1,2)
     for (k in 1:nc) {
       mnd <- 0
       mnu <- 0
       if (j<nt) {
         mnd <- mnd + p.delta[j+1,k,i-1]
         mnu <- mnu + p.mu[j+1,k,i] - p.mu[j,k,i]
       }
       if (j>1)
         mnd <- mnd + newdelta[j-1,k]
       draw <- rnorm(1,
                     (p.sig2u[i-1]*mnd + p.sig2v[i-1]*mnu)/(p.sig2v[i-1] + nn*p.sig2u[i-1]),
                     sqrt( (p.sig2v[i-1]*p.sig2u[i-1])/(p.sig2v[i-1] + nn*p.sig2u[i-1]) ) )
       row <- c(row,draw)
     }
     newdelta <- rbind(newdelta,row)
   }
   p.delta <- abind(p.delta,newdelta,along=3)
   
   ## tau
   newtau <- NULL
   for (j in 1:ns) {  ## Could possibly be vectorized.
     #nn <- min(ns,nt-j+1)
     if  (j==ns)
       mnt <- -sum(newtau[1:(j-1)])
     else
       mnt <- -sum(newtau[0:(j-1)],p.tau[i-1,(j+1):ns])
     draw <- rnorm(1,
                   (p.sig2e[i-1]*mnt + p.sig2w[i-1]*sum(Y[j,]-p.mu[j,,i]-p.theta[i,]-p.beta[i]*x[j,] ) )/(nc*p.sig2w[i-1] + ns*p.sig2e[i-1]),
                   sqrt( (p.sig2w[i-1]*p.sig2e[i-1])/(nc*p.sig2w[i-1] + ns*p.sig2e[i-1]) ) )
     newtau <- c(newtau,draw)
   }
   p.tau <- rbind(p.tau,newtau)
   
   p.sig2e <- c(p.sig2e,sig2e)
   p.sig2c <- c(p.sig2c,sig2c)
   p.sig2u <- c(p.sig2u,sig2u)
   p.sig2v <- c(p.sig2v,sig2v)
   p.sig2w <- c(p.sig2w,sig2w)
#    ## sig2e
#    draw <- rinvgamma(1, n/2+a.e, b.e + sum((Y - matrix(p.theta[i,],nt,nc,T) - p.mu[,,i] - matrix(p.tau[i,circtau],nt,nc,F) - x*p.beta[i])^2)/2)
#    p.sig2e <- c(p.sig2e,draw)
#    
#    ## sig2c
#    draw <- rinvgamma(1, nc/2+a.c, b.c + sum((p.theta[i,]-p.phi[i])^2)/2)
#    p.sig2c <- c(p.sig2c,draw)
#    
#    ## sig2u
#    draw <- rinvgamma(1, n/2+a.u, b.u + sum((p.mu[,,i]-rbind(0,p.mu[-nt,,i])-rbind(0,p.delta[-nt,,i]))^2)/2)
#    p.sig2u <- c(p.sig2u,draw)
#    
#    ## sig2v
#    draw <- rinvgamma(1, n/2+a.v, b.v + sum((p.delta[,,i]-rbind(0,p.delta[-nt,,i]))^2)/2)
#    p.sig2v <- c(p.sig2v,draw)
#    
#    ## sig2w
#    #draw <- rinvgamma(1, ns/2+a.w, b.w + sum(( sapply(1:nt,function(ii) sum(tau[i,pmax(0,ii-ns):ii])) )^2)/2)
#    draw <- rinvgamma(1, ns/2+a.w, b.w + sum(( sum(p.tau[i,]) )^2)/2)
#    p.sig2w <- c(p.sig2w,draw)
   
   # Track progress
   cat(paste0("\rIteration: ",i))
   
   i <- i+1
 }
 
 p <- list(beta  = p.beta[-(1:burnin)],
           theta = p.theta[-(1:burnin),],
           phi   = p.phi[-(1:burnin)],
           mu    = p.mu[,,-(1:burnin)],
           delta = p.delta[,,-(1:burnin)],
           tau   = p.tau[-(1:burnin),],
           sig2e = p.sig2e[-(1:burnin)],
           sig2c = p.sig2c[-(1:burnin)],
           sig2u = p.sig2u[-(1:burnin)],
           sig2v = p.sig2v[-(1:burnin)],
           sig2w = p.sig2w[-(1:burnin)])
 pmeans <- with(p,list(beta  = mean(beta),
                       mu    = apply(mu,1:2,mean),
                       delta = apply(delta,1:2,mean),
                       tau   = apply(tau,2,mean),
                       theta = apply(theta,2,mean),
                       phi   = mean(phi),
                       sig2e = mean(post$sig2e),
                       sig2c = mean(post$sig2c),
                       sig2u = mean(post$sig2u),
                       sig2v = mean(post$sig2v),
                       sig2w = mean(post$sig2w)))
 
 hist(p$beta)
 hist(p$theta)
 hist(p$phi)
 hist(p$sig2e)
 hist(p$sig2c)
 hist(p$sig2u)
 hist(p$sig2v)
 hist(p$sig2w)
 
 
# End script
 