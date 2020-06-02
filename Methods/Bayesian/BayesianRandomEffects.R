# Gibbs sampler for Bayesian random effects model
 library(data.table)
 library(MCMCpack)
 library(mvtnorm)
 library(extraDistr)
 
 # Number of individuals
 n <- 50
 # Number of observations per individual
 k <- 50
 
 # Covariance matrix for two correlated random effects per individual
 set.seed(923)
 Sig <- matrix(c(1.5, .7, .7, 1.6), 2, 2)
 tmp <- rmvnorm(n, rep(0, 2), Sig)
 # Random effects
 a1 <- tmp[,1]
 b1 <- tmp[,2]
 
 a0 <- 3
 b0 <- 7
 
 # Individual id
 ind <- rep(1:n, each = k)
 
 # Variances for y1 and y2
 sig1 <- 1.5
 sig2 <- 2.1
 y1 <- rnorm(length(ind), a0 + a1[ind], sqrt(sig1))
 y2 <- rnorm(length(ind), b0 + b1[ind], sqrt(sig2))
 
 dt <- data.table(Ind = ind, y1, y2)
 
 # Prior for a0 and b0.
 m <- 0
 s <- 100
 
 # Prior for Sig
 nu <- 2
 Psi <- diag(2)*20
 # Psi <- matrix(c(1, 1, 1, 1)/40, 2, 2)
 
 a <- 2
 b <- 5
 # curve(dinvgamma(x, a, b), 0, 10)
 
 
 # Number of draws
 N <- 2e3
 decorrelate = T
 
 # Initial draws
 a0.samp <- m
 a1.samp <- matrix(0, N, length(a1))
 a1.samp[1,] <- rep(m, length(a1))
 b0.samp <- m
 b1.samp <- matrix(0, N, length(b1))
 b1.samp[1,] <- rep(m, length(b1))
 Sig.samp <- array(0, dim = c(2, 2, N))
 Sig.samp[,,1] <- diag(2)
 UnSig.samp <- Sig.samp
 sig1.samp <- 1
 sig2.samp <- 1
 
 # Gibbs sampler
 for (i in 2:N) {
   cat(paste0("\rIteration: ", i))
   
   a0.samp[i] <- rnorm(1, (m*sig1.samp[i-1] + s*sum(y1-a1.samp[i-1,ind]))/(sig1.samp[i-1] + length(y1)*s), 
                       sqrt(sig1.samp[i-1]*s/(sig1.samp[i-1] + length(y1)*s)) )
   
   b0.samp[i] <- rnorm(1, (m*sig2.samp[i-1] + s*sum(y2-b1.samp[i-1,ind]))/(sig2.samp[i-1] + length(y2)*s), 
                       sqrt(sig2.samp[i-1]*s/(sig2.samp[i-1] + length(y2)*s)) )
   
   if(decorrelate) {
     A <- diag(c(1/sig1.samp[i-1], 1/sig2.samp[i-1]))
     B <- solve(k*A + solve(Sig.samp[,,i-1]))
     tmp <- dt[, list(Sum1 = sum(y1 - a0.samp[i]), Sum2 = sum(y2 - b0.samp[i])), by = Ind]
     tmp <- tmp[, as.list(rmvnorm(1, B%*%A%*%c(Sum1, Sum2), B)), by = Ind]
     a1.samp[i,] <- tmp$V1
     b1.samp[i,] <- tmp$V2
   } else {
     tmp <- Sig.samp[,,i-1]
     
     ma <- tmp[1,2] / tmp[2,2] * b1.samp[i-1,]
     sa <- tmp[1,1] - tmp[1,2] / tmp[2,2] * tmp[2,1]
     
     mb <- tmp[2,1] / tmp[1,1] * a1.samp[i,]
     sb <- tmp[2,2] - tmp[2,1] / tmp[1,1] * tmp[1,2]
     
     for (j in 1:ncol(a1.samp)) {
       a1.samp[i,j] <- rnorm(1, (sa*sum(y1[ind == j] - a0.samp[i]) + sig1.samp[i-1]*ma[j]) / (sig1.samp[i-1] + sa * sum(ind == j)),
                             sqrt(sig1.samp[i-1] * sa / (sig1.samp[i-1] + sa * sum(ind == j))) )
       
       b1.samp[i,j] <- rnorm(1, (sb*sum(y2[ind == j] - b0.samp[i]) + sig2.samp[i-1]*mb[j]) / (sig2.samp[i-1] + sb * sum(ind == j)),
                             sqrt(sig2.samp[i-1] * sb / (sig2.samp[i-1] + sb * sum(ind == j))) )
     }
   }
   
   
   sig1.samp[i] <- 1/rgamma(1, shape = a + length(y1)/2, rate = b + sum((y1 - a0.samp[i] - a1.samp[i, ind])^2)/2)
   sig2.samp[i] <- 1/rgamma(1, shape = a + length(y2)/2, rate = b + sum((y2 - b0.samp[i] - b1.samp[i, ind])^2)/2)
   
   X <- cbind(a1.samp[i,], b1.samp[i,])
   Sig.samp[,,i] <- solve(rwish(length(a1) + nu, solve(Psi + t(X)%*%X)))
   UnSig.samp[,,i] <- solve(sqrtm(Sig.samp[,,i]))
 }
 
 # Trace plots
 par(mfrow = c(2, 2))
 plot(a0.samp, type = "l")
 plot(b0.samp, type = "l")
 plot(a1.samp[, 14], type = "l"); abline(h = a1[14], col = "red")
 plot(b1.samp[, 10], type = "l"); abline(h = b1[10], col = "red")
 
 plot(sig1.samp, type = "l")
 plot(sig2.samp, type = "l")
 hist(sig1.samp)
 hist(sig2.samp)
 
 hist(a1)
 hist(apply(a1.samp, 2, mean))
 hist(b1)
 hist(apply(b1.samp, 2, mean))
 
 # dev.off()
 
 apply(Sig.samp, 1:2, mean)
 
 plot(a1, apply(a1.samp, 2, mean)); abline(0,1)
 plot(b1, apply(b1.samp, 2, mean)); abline(0,1)
 
 if(decorrelate)
   one <- list(a0 = a0.samp, b0 = b0.samp, a1 = a1.samp, b1 = b1.samp, Sig = Sig.samp, sig1 = sig1.samp, sog2 = sig2.samp)
 if(!decorrelate)
   two <- list(a0 = a0.samp, b0 = b0.samp, a1 = a1.samp, b1 = b1.samp, Sig = Sig.samp, sig1 = sig1.samp, sog2 = sig2.samp)
 
 indv <- 15
 plot(one$a1[, indv], one$b1[, indv], type = "l")
 plot(two$a1[, indv], two$b1[, indv], type = "l")
 
 plot(one$a1[, indv], type = "l")
 plot(two$a1[, indv], type = "l")
 
 ## Trace plot of LIKELIHOOD: sum of log(b1) over estimates of all b1 entries. Not sure about calculation..
 # this <- sapply(1:N, function(i) {
 #   # -1/2*sum((cbind(a1.samp[i,], b1.samp[i,])%*%Sig.samp[,,i]%*%rbind(a1.samp[i,], b1.samp[i,]))[cbind(1:k,1:k)])
 #   sum(sapply(1:n, function(j) -.5*c(a1.samp[i,j], b1.samp[i,j])%*%solve(Sig.samp[,,i])%*%c(a1.samp[i,j], b1.samp[i,j])))
 #   #sum(dmvnorm(cbind(a1.samp[i,1], b1.samp[i,1]), rep(0, 2), Sig.samp[,,i], log = T))
 # })
 # plot(this, type = "l")
 
 
 
# End script
 