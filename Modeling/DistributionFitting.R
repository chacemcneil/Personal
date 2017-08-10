# Fit distributions to data
 library(ggplot2)
 library(data.table)
 
 
 fit.binomial <- function(vec,n,plot=T) {
   #nll.binom  <- function(p) -log(inv.logit(p))*sum(vec) - log(inv.logit(1-p))*sum(10-vec)
   nll.binom  <- function(p) -log(p)*sum(vec) - log(1-p)*sum(n-vec)
   p <- optim(0.5,nll.binom)
   if (plot) {
     hist(vec,freq=F,breaks=15,xlim=c(0,n))
     curve(dbinom(x,n,p$par),0,n,add=T)
   }
   return(p$par)
 }
 
 fit.poisson <- function(vec,plot=T) {
   nll.pois  <- function(p) p*length(vec) - log(p)*sum(vec)
   p <- nlm(nll.pois,1)
   p <- optim(0.5,nll.pois)
   if (plot) {
     hist(vec,freq=F,breaks=15,xlim=c(0,max(vec)))
     curve(dpois(x,p$par),0,max(vec),add=T)
   }
   return(p$par)
 }
 
 fit.gamma <- function(vec,plot=T) {
   nll.gamma <- function(p) {
     alpha <- p[1]
     beta  <- p[2]
     return(-(alpha-1)*sum(log(vec)) + sum(vec)/beta + length(vec)*(log(gamma(alpha))+alpha*log(beta)) )
   }
   p <- optim(rep(1,2),nll.gamma)
   if (plot) {
     hist(vec,freq=F,breaks=15)
     curve(dgamma(x,p$par[1],scale=p$par[2]),min(vec),max(vec),add=T)
   }
   return(p$par)
 }
 
 fit.exp <- function(vec,plot=T) {
   nll.exp <- function(p) length(vec)*log(p) + sum(vec)/p
   p <- optim(1,nll.exp)
   if (plot) {
     hist(vec,freq=F,breaks=15)
     curve(dexp(x,rate=1/p$par),min(vec),max(vec),add=T)
   }
   return(p$par)
 }
 
 fit.beta <- function(vec,plot=T) {
   nll.beta <- function(p) {
     alpha <- p[1]
     beta  <- p[2]
     return(length(vec)*(log(gamma(alpha))+log(gamma(beta))-log(gamma(alpha+beta))) - (alpha-1)*sum(log(vec)) - (beta-1)*sum(log(1-vec)) )
   }
   p <- optim(rep(1,2),nll.beta)
   if (plot) {
     hist(vec,freq=F,breaks=15,xlim=0:1)
     curve(dbeta(x,p$par[1],p$par[2]),0,1,add=T)
   }
   return(p$par)
 }
 
 fit.weibull <- function(vec,plot=T) {
   
 }
 
 
 
 
 
# End script
 