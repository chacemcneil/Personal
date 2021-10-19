## Gibbs Sampler
 library(data.table)
 library(ggplot2)
 library(htmlTable)
 library(MASS)
 
 
 ## Perform linear regression with three parameters (intercept, slope, variance)
 ## Priors:
 ##  Normal for intercept and slope
 ##  Inverse gamma for variance
 
 
 
 n <- 1000
 N <- 1000
 theta <- c(3, 1.5, 4)
 s2 <- 1e3
 a <- .1
 b <- .1
 if(F) {
   dt <- data.table(x = seq(-100, 100))[, dens := dnorm(x, 0, sqrt(s2))]
   ggplot(dt, aes(x, dens)) + geom_line()
   dt <- data.table(x = seq(0, 1000))[, dens := dgamma(1/x, a, b)]
   ggplot(dt, aes(x, dens)) + geom_line()
 }
 
 set.seed(745670)
 dat <- data.table(x = rnorm(n), x2 = rnorm(n), grp = rep(c("A", "B"), each = n/2))
 dat[, y := rnorm(.N, theta[1] + theta[2]*x, sqrt(theta[3]))]
 
 # htmlTable(dt[, .(x = round(x, 3), y = round(y, 3))],
 #           css.cell = padding(2), css.class = "fixedheader")
 
 mvrnorm(10, mu = rep(0, 2), Sigma = diag(2))
 
 y <- dat$y
 X <- cbind(1, dat$x)
 XX <- t(X) %*% X
 Xy <- t(X) %*% dat$y
 draws <- rbind(c(0, 0, 1))
 for(i in 1:N) {
   A <- s2*XX + draws[i, 3]*diag(2)
   betas <- mvrnorm(1, mu = s2*solve(A)%*%Xy, Sigma = solve(A)*s2*draws[i, 3])
   Xb <- X%*%betas
   sig2 <- 1/rgamma(1, a + n/2, rate = b + t(y - Xb)%*%(y - Xb)/2)
   draws <- rbind(draws, c(betas, sig2))
 }
 draws <- data.table(draws)
 setnames(draws, c("b0", "b1", "sig2"))
 
 data.table(Parameter = names(draws),
            PostMean = sapply(draws, mean),
            LowCS = sapply(draws, function(x) sort(x)[25]),
            HighCS = sapply(draws, function(x) sort(x)[975]) )
 
 ggplot(draws[-1], aes(b0, b1)) + geom_point()
 
 mod <- lm(y ~ x, data = dat)
 data.table(Est = coef(mod),
            LowCI = coef(mod) - 1.96*coef(summary(mod))[, 2],
            HighCI = coef(mod) + 1.96*coef(summary(mod))[, 2])
 
 ## Version 2
 
 y <- dat$y
 x <- dat$x
 draws2 <- rbind(c(0, 0, 1))
 for(i in 1:N) {
   beta0 <- rnorm(1, s2*sum(y + x * draws2[i, 2])/(n*s2 + draws2[i, 3]), sqrt(s2*draws2[i,3]/(n*s2 + draws2[i, 3])) )
   beta1 <- rnorm(1, s2*sum(y*x + x*beta0)/(sum(x^2)*s2 + draws2[i, 3]), sqrt(s2*draws2[i,3]/(sum(x^2)*s2 + draws2[i, 3])) )
   sig2 <- 1/rgamma(1, a + n/2, rate = b + sum((y - beta0 - x*beta1)^2)/2)
   draws2 <- rbind(draws2, c(beta0, beta1, sig2))
 }
 draws2 <- data.table(draws2)
 setnames(draws2, c("b0", "b1", "sig2"))
 
 data.table(Parameter = names(draws2),
            PostMean = sapply(draws2, mean),
            LowCS = sapply(draws2, function(x) sort(x)[25]),
            HighCS = sapply(draws2, function(x) sort(x)[975]) )
 
 
 
# End script
 