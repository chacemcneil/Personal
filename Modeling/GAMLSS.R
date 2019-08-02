## Explore gamlss package
 library(data.table)
 library(gamlss)
 
 N <- 1e4
 coef <- c(1.4, 2.3)
 dt <- data.table(Ind = 1:N,
                  x1 = rnorm(N),
                  x2 = pmax(rnorm(N, 4, 2), 0),
                  Group = sample(LETTERS[1:4], N, replace = T) )
 dt[, y := rnorm(.N, cbind(x1, x2) %*% coef)]
 
 m1 <- lm(y ~ x1, data = dt)
 gm1 <- gamlss(y ~ x1, data = dt)
 gm1b <- gamlss(y ~ x1, data = dt, family = NO2)
 
 
 
 
# End script
 