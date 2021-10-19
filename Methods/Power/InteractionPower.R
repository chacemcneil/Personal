## Power of 2x2 with interactions
 library(data.table)
 library(Matrix)
 library(pwr)
 
 n <- 12
 d1 <- 1
 d2 <- 1.3
 d3 <- 0.8
 Iter <- 2e3
 mu <- 3
 
 design_mat <- matrix(c(1, 1, 0, 0, 1, 0, 1, 0), 4, 2)
 
 pvals <- NULL
 for (i in 1:Iter) {
   cat("\rIteration: ", i)
   dt <- data.table(kronecker(design_mat, rep(1, n)))
   dt[, y := rnorm(.N, mu + V1*d1 + V2*d2)]
   mod00 <- summary(lm(y ~ ., data = dt))$coef
   mod01 <- summary(lm(y ~ .^2, data = dt))$coef
   dt[, y := y + V1*V2*d3]
   mod10 <- summary(lm(y ~ ., data = dt))$coef
   mod11 <- summary(lm(y ~ .^2, data = dt))$coef
   pvals <- rbind(pvals,
                  data.table(Iter=i, Coef=1:2, Inxn_dat=0, Inxn_mod=0, Pval = mod00[-1, 4]),
                  data.table(Iter=i, Coef=1:3, Inxn_dat=0, Inxn_mod=1, Pval = mod01[-1, 4]),
                  data.table(Iter=i, Coef=1:2, Inxn_dat=1, Inxn_mod=0, Pval = mod10[-1, 4]),
                  data.table(Iter=i, Coef=1:3, Inxn_dat=1, Inxn_mod=1, Pval = mod11[-1, 4]) )
 }
 
 ## Find power 
 
 pvals[, .(Power = mean(Pval < .05)), keyby = .(Inxn_dat, Inxn_mod, Coef)]
 
 pwr.t.test(n = n, d = d1)$power
 pwr.t.test(n = n, d = d2)$power
 pwr.t.test(n = n, d = d3)$power
 pwr.t2n.test(n1 = n/4, n2 = 3*n/4, d = d3)$power
 
 pwr.t.test(n = 2*n, d = d1)$power
 pwr.t.test(n = 2*n, d = d2)$power
 # pwr.t.test(n = 2*n, d = d3)$power
 
 
 
# End script
 