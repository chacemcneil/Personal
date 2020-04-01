# Work with lagged variables in logistic regression
 library(CMcCode)
 library(data.table)
 library(forecast)
 library(gtools)
 library(lme4)
 
 n <- 3e3
 b0 <- 3.3
 b1 <- -2.1
 b2 <- 0.4
 
 ests <- NULL
 N <- 1e3
 for (i in 1:N) {
   cat(paste0("\rIteration: ", i))
   dat <- data.table(x = rnorm(n))[, y := rbinom(n, 1, inv.logit(b0 + b1*shift(x) + b2*x))]
   smy <- summary(mod <- glm(y ~ shift(x, 5) + x, data = dat, family = "binomial"))
   ests <- rbind(ests, data.table()[,as.list(coef(mod))])
 }
 
 ests[, {hist(`(Intercept)`); abline(v = b0, col = "red", lwd = 2)}]
 ests[, {hist(`shift(x, 1)`); abline(v = b1, col = "red", lwd = 2)}]
 ests[, {hist(x); abline(v = b2, col = "red", lwd = 2)}]
 
 
 
 
 
 
 
# End script
 