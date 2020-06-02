# Canonical correlation functions
 library(CCA)
 library(CCP)
 
 # !! Canonical correlation assumes multivariate normal distributions for both data sets !!
 if (F) {
   n <- 50
   p <- 3
   q <- 5
   
   # Compare cancor (stats) to cc (CCA)
   x <- matrix(rnorm(n*p), n, p)
   y <- matrix(rnorm(n*q), n, q)
   (can <- cancor(x, y, xcenter = T, ycenter = T))
   
   cca <- CCA::cc(x,y)
   
   # The coefficients for each component of one version are scaled from the coefficients from the component in the other.
   apply(can$xcoef, 2, scale)
   apply(cca$xcoef, 2, scale)
   
   # Correlations are the same
   can$cor
   cca$cor
   
   cor(x %*% can$xcoef[,1], y %*% can$ycoef[,1])  # Highest correlation
   cor(x %*% can$xcoef[,2], y %*% can$ycoef[,2])  # 2nd highest correlation
   
   cor(x %*% can$xcoef[,1], y %*% cca$ycoef[,1])  # Highest correlation
   cor(x %*% cca$xcoef[,2], y %*% cca$ycoef[,2])  # 2nd highest correlation
   
   
   # Scores from the CCA version can be obtained from components of the cancor version
   head(cca$scores$xscores)
   head(apply(x %*% can$xcoef, 2, scale))  # The cancor scores need to be scaled (and sometimes negated) to be the same as CCA
   hist(apply(x %*% can$xcoef, 2, scale) - cca$scores$xscores %*% diag(c(-1, -1, 1)))
   
   # Other scores are just correlations between the original data (X or Y) and the scores (xscores or yscores)
   cca$scores$corr.X.yscores
   cor(x, y %*% can$ycoef)
   
   
   # Calculate p-values for test of canonical dimension, using Wilke's lambda
   CCP::p.asym(can$cor, n, p, q)
   CCP::p.asym(can$cor, n, nrow(can$xcoef), nrow(can$ycoef))  # cancor does not store n
   CCP::p.asym(cca$cor, n, p, q)
   CCP::p.asym(cca$cor, nrow(cca$scores$xscores), nrow(cca$xcoef), nrow(cca$ycoef))
   
   
   # How often does this test return significance?
   N <- 1e4
   ps <- NULL
   for (i in 1:N) {
     tmpx <- matrix(rnorm(n*p), n, p)
     tmpy <- matrix(rnorm(n*q), n, q)
     temp <- cancor(tmpx, tmpy, xcenter = T, ycenter = T)
     ps <- rbind(ps, CCP::p.asym(temp$cor, n, p, q)$p.value)
   }
   hist(ps)
   mean(ps < 0.05)
   hist(apply(ps, 1, min), breaks = 20)
   mean(apply(ps, 1, min) < 0.05)
 }
 
 cancor.test <- function(ccor, nn) {
   require(CCP)
   p <- nrow(ccor$xcoef)
   q <- nrow(ccor$ycoef)
   p.asym(ccor$cor, nn, p, q, tstat = "Wilks")
 }
 
 my.cancor <- function(X, Y, standardize = T) {
   p <- ncol(X)
   q <- ncol(Y)
   minpq <- min(p, q)
   if (standardize) {
     A <- solve(cor(X)) %*% cor(X, Y) %*% solve(cor(Y)) %*% cor(Y, X)
     B <- solve(cor(Y)) %*% cor(Y, X) %*% solve(cor(X)) %*% cor(X, Y)
     
   } else {
     A <- solve(cov(Y)) %*% cov(Y, X) %*% solve(cov(X)) %*% cov(X, Y)
     B <- solve(cov(X)) %*% cov(X, Y) %*% solve(cov(Y)) %*% cov(Y, X)
   }
   
   evals <- Re(eigen(A)$values[1:minpq])
   cor <- sqrt(evals)
   
   xcoef <- Re(eigen(A)$vectors[, 1:minpq])
   ycoef <- Re(eigen(B)$vectors[, 1:minpq])
   
   list(cor = cor, xcoef = xcoef, ycoef = ycoef)
 }
 
 
# End script
 