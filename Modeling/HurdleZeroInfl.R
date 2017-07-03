# Exploring the hurdle model and comparing to zero-inflated model
 library(data.table)
 library(pscl)
 library(gtools)
 library(ggplot2)
 library(pROC)
 
 
 n <- 1e3
 
 # Generate via zero-inflated process
 p <- rnorm(n, 0, 2)
 grp <- rbinom(n, 1, inv.logit(2.1*p))
 x <- rnorm(n, 5)
 y <- ifelse(grp == 1, rpois(n, 1.2*x), 0)
 
 # Generate via zero hurdle process
 y <- ifelse(grp == 1, rpois(n, 1.2*x) + 1, 0)
 
 hist(y)
 
 summary(pmod <- glm(y~x, family = "poisson"))
 summary(qmod <- glm(y~x, family = "quasipoisson"))
 summary(hmod <- hurdle(y~x|p))
 summary(zmod <- zeroinfl(y~x|p))
 
 plot(predict(hmod, type = "zero"), y)
 
 # Different values predicted
 head(predict(hmod))
 head(predict(hmod, type = "count"))
 head(predict(hmod, type = "prob"))[, 1]
 1-head(predict(hmod, type = "prob"))[, 1]
 head(predict(hmod, type = "zero"))  # Probability of a non-zero result
 head(head(1-predict(hmod, type = "prob"))[, 1])/(1-ppois(0, head(predict(hmod, type = "count"))))
 
 head(y - (predict(hmod)))
 
 sqrt(mean(residuals(hmod, type = "response")^2))
 sqrt(mean(residuals(qmod, type = "response")^2))
 sqrt(mean(residuals(pmod, type = "response")^2))
 sqrt(mean(residuals(zmod, type = "response")^2))
 
 
 ### Compare bias
 
 N <- 1e3
 
 hcoefs.h <- zcoefs.h <- hcoefs.z <- zcoefs.z <- NULL
 for (i in 1:N) {
   cat("\rIteration: ", i)
   
   p <- rnorm(n, 0, 2)
   grp <- rbinom(n, 1, inv.logit(2.1*p))
   x <- rnorm(n, 5)
   
   y <- ifelse(grp == 1, rpois(n, 1.2*x), 0)
   hcoefs.z <- rbind(hcoefs.z, do.call(rbind, summary(hmod <- hurdle(y~x|p))$coefficients)[, 1])
   zcoefs.z <- rbind(zcoefs.z, do.call(rbind, summary(zmod <- zeroinfl(y~x|p))$coefficients)[, 1])
   
   y <- ifelse(grp == 1, rpois(n, 1.2*x) + 1, 0)
   hcoefs.h <- rbind(hcoefs.h, do.call(rbind, summary(hmod <- hurdle(y~x|p))$coefficients)[, 1])
   zcoefs.h <- rbind(zcoefs.h, do.call(rbind, summary(zmod <- zeroinfl(y~x|p))$coefficients)[, 1])
 }
 mean(exp(hcoefs.h[, 2])); mean(hcoefs.h[, 4])
 mean(exp(zcoefs.h[, 2])); mean(zcoefs.h[, 4])
 mean(exp(hcoefs.z[, 2])); mean(hcoefs.z[, 4])
 mean(exp(zcoefs.z[, 2])); mean(zcoefs.z[, 4])
 
 # Appears that there is less bias when the correct model is used
 
 
 
 
 
 
# End script
 