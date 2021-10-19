library(bayesm)

podwt <- structure(list(wt = c(1.76, 1.45, 1.03, 1.53, 2.34, 1.96, 1.79, 1.21, 0.49, 0.85, 1, 1.54, 1.01, 0.75, 2.11, 0.92),
                        treat = structure(c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L),
                                          .Label = c("I", "U"), class = "factor"),
                        mus = c(4.15, 2.76, 1.77, 3.11, 4.65, 3.46, 3.75, 2.04, 1.25, 2.39, 2.54, 3.41, 1.27, 1.26, 3.87, 1.01)),
                   .Names = c("wt", "treat", "mus"),
                   row.names = c(NA, -16L),
                   class = "data.frame")

# response
y1 <- podwt$wt

# First run a one-way anova

# Create the design matrix - need to insert a column of 1s
x1 <- cbind(matrix(1,nrow(podwt),1),podwt$treat)

# data for the Bayesian analysis
dt1 <- list(y=y1,X=x1)

# runiregGibbs uses a normal prior for the regression coefficients and 
# an inverse chi-squared prior for va

# mean of the normal prior. We have 2 estimates - 1 intercept 
# and 1 regression coefficient
betabar1 <- c(0,0)

# Pecision matrix for the normal prior. Again we have 2
A1 <- 0.01 * diag(2)
# note this is a very diffuse prior

# degrees of freedom for the inverse chi-square prior
n1 <- 3  

# scale parameter for the inverse chi-square prior
ssq1 <- var(y1) 

Prior1 <- list(betabar=betabar1, A=A1, nu=n1, ssq=ssq1)

# number of iterations of the Gibbs sampler
iter <- 10000  

# thinning/slicing parameter. 1 means we keep all all values
slice <- 1 

MCMC <- list(R=iter, keep=slice)

sim1 <- runiregGibbs(dt1, Prior1, MCMC)

plot(sim1$betadraw)
plot(sim1$sigmasqdraw)

summary(sim1$betadraw)
summary(sim1$sigmasqdraw)

# compare with maximum likelihood estimates:
fitpodwt <- lm(wt~treat, data=podwt)
summary(fitpodwt)
anova(fitpodwt)


# now for ordinary linear regression

x2 <- cbind(matrix(1,nrow(podwt),1),podwt$mus)

dt2 <- list(y=y1,X=x2)

sim2 <- runiregGibbs(dt2, Prior1, MCMC)

summary(sim2$betadraw)
summary(sim2$sigmasqdraw)
plot(sim2$betadraw)
plot(sim2$sigmasqdraw)

# compare with maximum likelihood estimates:
summary(lm(podwt$wt~mus,data=podwt))


# now with both variables

x3 <- cbind(matrix(1,nrow(podwt),1),podwt$treat,podwt$mus)

dt3 <- list(y=y1,X=x3)

# now we have an additional estimate so modify the prior accordingly

betabar1 <- c(0,0,0)
A1 <- 0.01 * diag(3)
Prior1 <- list(betabar=betabar1, A=A1, nu=n1, ssq=ssq1)

sim3 <- runiregGibbs(dt3, Prior1, MCMC)

plot(sim3$betadraw)
plot(sim3$sigmasqdraw)
summary(sim3$betadraw)
summary(sim3$sigmasqdraw)

# compare with maximum likelihood estimates:
summary(lm(podwt$wt~treat+mus,data=podwt))

