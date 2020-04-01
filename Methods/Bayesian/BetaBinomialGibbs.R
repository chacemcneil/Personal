# Simulate data for Bayesian analysis
 library(data.table)
 
 # Hierarchical model:
 #  x_i ~ Bernoulli(p_k)
 #  p_k ~ Beta(alpha, beta)
 #  alpha ~ Gamma(a, b)
 #  beta ~ Gamma(c, d)
 
 n <- 1e6
 K <- 4e2
 truea <- 1
 trueb <- 1
 pr <- rbeta(K, truea, trueb)
 hist(pr, freq = F, xlim = 0:1)
 curve(dbeta(x, truea, trueb), 0, 1, add = T)
 grp <- rep(1:K, each = n/K)
 resp <- rbinom(n, 1, pr[grp])
 
 # Explore how the strength of a prior
 a <- 10
 b <- 2
 c <- 16
 d <- 4
 plot(curve(dgamma(x, a, b), 0, 10), type = "l")
 plot(curve(dgamma(x, c, d), 0, 10), type = "l")
 
 N <- 4e3
 burnin <- 5e2
 alpha <- beta <- numeric(N+burnin)
 p <- matrix(0, nrow = N+burnin, ncol = K)
 p[1,] <- .5
 alpha[1] <- a/b
 beta[1] <- c/d
 ns <- tapply(resp, grp, length)
 sums <- tapply(resp, grp, sum)
 for (i in 2:(burnin + N)) {
   cat("\rIteration: ", i)
   p[i, ] <- rbeta(K, alpha[i-1] + sums, beta[i-1] + ns - sums)
   lpdf_alpha <- function(x) (a-1)*log(x) - x/b + (x-1)*sum(log(p[i,])) + K*(lgamma(x + beta[i-1]) - lgamma(x))
   lpdf_beta  <- function(x) (c-1)*log(x) - x/d + (x-1)*sum(log(1-p[i,])) + K*(lgamma(x + alpha[i]) - lgamma(x))
   #alpha[i] <- mh_norm(lpdf = lpdf_alpha, alpha[i-1], sig = 1)
   #beta[i]  <- mh_norm(lpdf = lpdf_beta, beta[i-1], sig = 1)
   alpha[i] <- mh_gamma(lpdf = lpdf_alpha, alpha[i-1], sig = 1)  # This works much better than using a normal candidate
   beta[i]  <- mh_gamma(lpdf = lpdf_beta, beta[i-1], sig = 1)
 }
 # Combine results
 results <- data.table(Burned = rep(1:0, times = c(burnin, N)), alpha, beta, p)
 # Calculate acceptance rates
 1 - mean(diff(alpha[burnin:N]) == 0); 1 - mean(diff(beta[burnin:N]) == 0)
 # Trace plots
 plot(alpha[1:2e3], type = "l")
 plot(beta[1:2e3], type = "l")
 plot(alpha[1:2e3], beta[1:2e3], type = "l")
 
 # Posterior distributions for alpha and beta
 hist(alpha[burnin:N])
 hist(beta[burnin:N])
 # Posterior distribution of probabilities
 hist(pr, freq = F, xlab = paste0("Posterior: Beta(", round(mean(alpha), 1), ", ", round(mean(beta), 1), ")"), main = "Posterior Beta")
 curve(dbeta(x, truea, trueb), 0, 1, type = "l", lty = 2, add = T)
 curve(dbeta(x, mean(alpha), mean(beta)), 0, 1, type = "l", add = T)
 curve(dbeta(x, 5, 5), 0, 1, type = "l", lty=3, add = T)
 
 ###
 #  Functions
 ###
 
 # Metropolis-Hastings algorithm
 mh_gamma <- function(lpdf, currentval, sig = 10, n = 1, ...) {
   # Generate T new values, take the last one in the chain (to remove autocorrelation of draws)
   for (i in 1:n) {
     # Generate candidate
     candidate <- rgamma(1, sig, scale = currentval/sig)
     # Calculate ratio
     if(is.na(lpdf(candidate))) {
       accept = 0
     } else {
       lratio <- (lpdf(candidate) - lpdf(currentval)) - (dgamma(candidate, shape = sig, scale = currentval/sig, log = T) - 
                                                         dgamma(currentval, shape = sig, scale = candidate/sig, log = T))
       # Accept with probability = ratio
       if (exp(lratio) > runif(1)) {
         currentval = candidate
         accept = 1
       } else {
         accept = 0
       }
     }
   }
   list(currentval, accept)
   currentval
 }
 
 mh_norm <- function(lpdf, currentval, sig = 10, n = 1) {
   # Generate n new values, take the last one in the chain (to remove autocorrelation of draws)
   for (i in 1:n) {
     # Generate candidate
     candidate <- rnorm(1, mean = currentval, sd = sig)
     # Calculate ratio
     if(!is.na(lpdf(candidate))) {
       lratio <- lpdf(candidate) - lpdf(currentval) - 
                (dlnorm(candidate, mean = currentval, sd = sig) - dlnorm(currentval, mean = candidate, sd = sig))
       # Accept with probability = ratio
       if (runif(1) < exp(lratio))
         currentval = candidate
     }
   }
   currentval
 }
 
 
 
 
 
# End script
 