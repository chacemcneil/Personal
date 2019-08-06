## Explore gamlss package
 library(data.table)
 library(gamlss)
 library(ggplot2)
 library(gtools)
 
 N <- 1e4
 coef <- c(.5, 1.4, 2.3)
 coefbin <- c(-15, 1.4, 2.3)
 dt <- data.table(Ind = 1:N,
                  x1 = rnorm(N),
                  x2 = pmax(rnorm(N, 4, 2), 0),
                  x3 = rpois(N, 10),
                  Group = sample(LETTERS[1:4], N, replace = T) )
 dt[, y := rnorm(.N, cbind(1, x1, x3) %*% coef)]
 dt[, ybin := rbinom(.N, 1, inv.logit(cbind(1, x1, x3) %*% coefbin))]
 
 
 ## Look at distribution of y
 hd <- histDist(dt$y, nbins = 30)
 plot(hd)
 hd$mu; hd$sigma
 
 ## Gaussian regression model - underspecified
 m1 <- lm(y ~ x1, data = dt)
 gm1 <- gamlss(y ~ x1, data = dt)
 gm1b <- gamlss(y ~ x1, data = dt, family = NO2)
 plot(gm1b)
 
 ## Add over specificied model
 m2 <- lm(y ~ x1 + x2 + x3, data = dt)
 gm2 <- gamlss(y ~ x1 + x2 + x3, data = dt)
 plot(gm2)
 
 
 mb <- glm(ybin ~ x1 + x2 + x3, family = "binomial", data = dt)
 gmba <- gamlss(ybin ~ x1 + x2 + x3, family = BI, data = dt)
 gmbb <- gamlss(ybin ~ pvc(x1), family = BB, data = dt)
 d
 
 
 
# End script
 