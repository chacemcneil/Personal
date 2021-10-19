# Propensity score matching
 library(Matching)
 library(MatchIt)
 library(tableone)
 library(rcbalance)
 library(gtools)
 library(ggplot2)
 
 set.seed(7430)
 b_sel <- c(-13.4, 1.8, 2.7)
 b_eff <- c(3.1, -1.3, .8, 1.5)   # Treatment effect = 1.5
 N = 300
 
 dt <- data.table(id = 1:N, x1 = rnorm(N, 2), x2 = rnorm(N, 3, .5) )
 dt[, Treat := rbinom(.N, 1, inv.logit(b_sel %*% c(1, x1, x2))), by = id]
 dt[, y := rnorm(.N, b_eff %*% c(1, x1, x2, Treat)), by = id]
 
 ggplot(dt, aes(x1, x2, col = factor(Treat))) + geom_point(size = 2) +
   theme(text = element_text(size = 5))
 
 ## Unmatched
 print(CreateTableOne(vars = c("x1", "x2"), strata = "Treat", data = dt, test = F), smd = T)
 
 ## Match on Mahalanobis distance without caliper
 match_1 <- Match(Tr = dt$Treat, M=1, X = dt[, .(x1, x2)])
 dt_m1 <- dt[unlist(match_1[c("index.treated", "index.control")])]
 print(CreateTableOne(vars = c("x1", "x2"), strata = "Treat", data = dt_m1, test = F), smd = T)
 
 ## Match on Mahalanobis distance with caliper
 match_2 <- Match(Tr = dt$Treat, M=1, X = dt[, .(x1, x2)], caliper = .6)
 dt_m2 <- dt[unlist(match_2[c("index.treated", "index.control")])]
 print(CreateTableOne(vars = c("x1", "x2"), strata = "Treat", data = dt_m2, test = F), smd = T)
 
 ## Paired t-test
 y_trt <- dt_m2[Treat == 1, y]
 y_cnt <- dt_m2[Treat == 0, y]
 diffy <- y_trt - y_cnt
 t.test(diffy)
 
 ## Linear regression
 matched_mod <- lm(y~x1+x2+Treat, data = dt_m2)
 summary(matched_mod)
 
 ## Linear regression without matching --> less bias in treatment effect
 basic_mod <- lm(y~x1+x2+Treat, data = dt)
 summary(basic_mod)
 
 
 
# End script
 