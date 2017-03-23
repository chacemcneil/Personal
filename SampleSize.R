# Testing sample size calculation for logistic regression
 library(gtools)
 library(data.table)
 library(ggplot2)
 library(pwr)
 
 N <- 1e3
 n <- 200
 a <- 0
 b <- 1.2
 
 beta <- numeric()
 for (i in 1:N) {
   x <- rbinom(n,1,.5)
   y <- rbinom(n,1,inv.logit(a + b*x))
   mod <- glm(y~x,family=binomial(link="logit"))
   summary(mod)
   beta[i] <- mod$coef[2]
 }
 
 unique(beta)
 
 h <- ES.h(.35,.45)
 pwr.2p.test(h=h,,sig.level=0.05,power=.82)
 pwr.2p.test(h=h,n=391,sig.level=0.05)
 
 beta <- numeric()
 ps   <- numeric()
 x <- rep(0:1,each=391)
 for (i in 1:N) {
   y <- rbinom(length(x),1,.45 - .1*x)
   mod <- glm(y~x,family="binomial")
   beta[i] <- mod$coef[2]
   ps[i]   <- summary(mod)$coef[2,4]
   cat(paste0("\rIteration: ",i))
 }
 mean(ps < 0.05)
 
 
 
 pwr.2p.test(.2)
 
 
 one <- rbinom(1e6,10,.7)
 two <- rbinom(1e6,10,.72)
 
 t.test(one,two)
 var(one)
 7*3/10
 var(two)
 7.2*2.8/10
 
 expand <- function(binom,n) {
   rbind(binom,n-binom)
 }
 
 n <- 300
 ps <- ps2 <- NULL
 for (i in 1:N) {
   one <- rbinom(n,10,.7)
   two <- rbinom(n,10,.72)
   ps[i] <- prop.test(c(sum(one),sum(two)),c(length(one),length(two))*10)$p.value
   y <- rep(rbind(rep(1,2*n),0),times=expand(c(one,two),10))
   ps2[i] <- summary(glm(y~rep(0:1,each=n*10),family="binomial"))$coef[2,4]
   cat(paste0("\rIteration: ",i))
 }
 
 mean(ps  < 0.05)
 mean(ps2 < 0.05)
 
 pwr.2p.test(ES.h(.7,.72),n=3000)
 
 
 pwr.2p.test(ES.h(.7,.75),power=.8) # -> 1250/10 = 125 people per group for binomial model
 pwr.2p.test(ES.h(.7,.8),power=.8)  # -> 300/10 = 30 people per group for binomial model
 
 pwr.t.test(d=.5/sd(two),power=.8) # -> 125 people per group for t-test
 pwr.t.test(d=1/sd(two),power=.8)  # -> 30 people per group for t-test
 
 pwr.2p.test(ES.h(.7,.75),power=.9) # -> 1670/10 = 167 people per group for binomial model
 pwr.2p.test(ES.h(.7,.8),power=.9)  # -> 390/10 = 39 people per group for binomial model
 
 pwr.t.test(d=.5/sd(two)/2,power=.9) # -> 162 people per group for t-test
 pwr.t.test(d=1/sd(two)/2,power=.9)  # -> 41 people per group for t-test
 
 # Using a t-test has basically the same test as using a binomial test
 # If the distribution is truly binomial, 170 people per group is likely sufficient
 
 
 
 ## Adjustment to power analysis. Value in using model residuals, rather than just the response variable.
 
 N <- 1e4
 n <- 30
 a <- 5.4
 b1 <- 0.7
 b2 <- 0.5
 
 ps1 <- ps2 <- numeric(0)
 vars1 <- vars2 <- varresid <- numeric(0)
 for (i in 1:N) {
   cat(paste0("\rIteration: ", i))
   one <- data.table(x = rnorm(2*n), ind = rep(0:1, each = n))[, list(x, ind, y1 = rnorm(2*n, a + b1*ind))]
   two <- data.table(x = rnorm(2*n), ind = rep(0:1, each = n))[, list(x, ind, y2 = rnorm(2*n, a + b1*ind + b2*x, .85))]
   vars1[i] <- var(one$y1)
   vars2[i] <- var(two$y2)
   varresid[i] <- var(lm(y2 ~ x, data = two)$residuals)
   mod1 <- lm(y1 ~ ind, data = one)
   ps1[i] <- summary(mod1)$coef["ind", 4]
   mod2 <- lm(y2 ~ ind + x, data = two)
   ps2[i] <- summary(mod2)$coef["ind", 4]
 }
 hist(vars1)
 hist(vars2)
 hist(varresid)
 mean(ps1 < 0.05)
 mean(ps2 < 0.05)
 
 pwr.t.test(d = 0.7, n = n)
 pwr.t.test(d = 0.7/.85, n = n)
 
 # Conclusion: The variation of the response variable y2 is .85, but because of x appears to be close to 1. The power to detect
 #             the effect of the variable ind is higher than would be calculated using a variation of 1. 
 
 
 
 
# End script
 