# Explore with time-varying survival analysis models
 library(data.table)
 library(survival)
 
 
 cgd0 <- data.table(cgd0)
 
 newcgd <- tmerge(data1 = cgd0[, 1:13], data2=cgd0, id=id, tstop=futime)
 newcgd <- tmerge(newcgd, cgd0, id=id, infect = event(etime1))
 newcgd <- tmerge(newcgd, cgd0, id=id, infect = event(etime2))
 newcgd <- tmerge(newcgd, cgd0, id=id, infect = event(etime3))
 newcgd <- tmerge(newcgd, cgd0, id=id, infect = event(etime4))
 newcgd <- tmerge(newcgd, cgd0, id=id, infect = event(etime5))
 newcgd <- tmerge(newcgd, cgd0, id=id, infect = event(etime6))
 newcgd <- tmerge(newcgd, cgd0, id=id, infect = event(etime7))
 newcgd <- tmerge(newcgd, newcgd, id, enum=cumtdc(tstart))
 head(newcgd)
 attr(newcgd, "tcount")
 
 test <- tmerge(cgd0[, 1:13], cgd0, id=id, tstop=futime,
                infect = event(etime1), infect= event(etime2),
                infect = event(etime3), infect= event(etime4),
                infect = event(etime5), infect= event(etime6),
                infect = event(etime7))
 test <- tmerge(test, test, id= id, enum = cumtdc(tstart))
 all.equal(newcgd, test)
 
 
 coxph(Surv(tstart, tstop, infect) ~ treat + inherit + steroids + enum + cluster(id), newcgd)
 
 
 
 
 # Add covariates with known effects
 
 # Control probabilities, according to proportional hazards model.
 
 n <- 5000  # NUmber of individuals
 start <- 0
 stop  <- 100
 group <- round(runif(n))
 lambda <- .5
 # Create probability of event at time t for individual i --> translate to proportional hazards framework
 p <- .01
 
 i = 3
 (pexp(i) - pexp(i-1)) / (1 - pexp(i-1))
 
 dt <- data.table(group, start = start, stop = pmin(100, ceiling(rexp(n, ifelse(group == 1, lambda, lambda/2)))), event = 1)
 dt[, mean(stop), by = group]
 
 coxph(Surv(start, stop, event) ~ group, data = dt)
 
 
 
 ## Simulate data
 Tmax <- 100
 # Weibull distribution parameters
 k <- .8
 l <- 10
 # Hazard function for Weibull distribution
 h0 <- function(t) k/l*(t/l)^(k-1)
 h0t <- h0(1:Tmax)
 # Cumulative hazard function for Weibull distribution
 H0 <- function(t) (t/l)^k
 H0t <- H0(1:Tmax)
 # Assumed model coefficients
 a <- 1
 b <- 0
 c <- .4
 odds <- a + b*group
 h1 <- function(x) h0(x)*exp(odds)
 H1 <- function(x) H0(x)*exp(odds)
 prob <- outer(odds, 1:Tmax, function(x, y) exp(-H0(y-1)*exp(x + c*(y-1 >= 5))) - exp(-H0(y)*exp(x + c*(y >= 5))))
 surv <- apply(prob, 1, sum) - t(apply(cbind(0, prob[,-100]), 1, cumsum))
 cond_prob <- prob/surv
 event <- matrix(rbinom(prod(dim(cond_prob)), 1, cond_prob), nrow = nrow(cond_prob), ncol = ncol(cond_prob))
 times <- apply(event, 1, function(x) min(which(x == 1)))
 hist(times, breaks = 40, freq = F)
 curve(dweibull(x, k, l/(2.72)^(1/k)), 0, 30, add = T)
 
 # Use inverse of cdf to generate values
 times <- (-log(1-runif(n))/exp(odds))^(1/k)*l
 hist(times, breaks = 40, freq = F)
 curve(dweibull(x, k, 1/l), 0, 30, add = T)
 
 dat <- data.table(id = 1:n, tstart = 0, tstop = 100, times, group)
 dat <- tmerge(dat, dat, id = id, tstop = times, fail = event(times))
 dat <- tmerge(dat, data.table(dat[times > 5,], accel = 5), id = id, accel = tdc(accel))
 
 summary(coxph(Surv(tstart, tstop, fail)~group+accel, data = dat))
 
 
 
 ggplot() + geom_histogram(data = dat, aes(time, y = ..density.., fill = factor(group)), position = "dodge", binwidth = 1) + 
   geom_line(data = data.frame(x = (1:300)/10, y = dweibull((1:300)/10, k, l/(2.72198)^(1/k))), aes(x, y))
 
 
 
 
# End script
 