# Propensity score matching
 library(data.table)
 library(gtools)
 library(MatchIt)
 
 # Only sample data is used here.
 
 N <- 3e2
 coef <- c("(Intercept)" = -28, Rally = .7, Age = .5, Gender = 2, RAF = .2)
 coefnames <- factor(names(coef), levels = names(coef))
 set.seed(234941232)
 coefs <- NULL
 coefs1 <- NULL
 
 for (i in 1:N) {
   cat(paste0("\rIteration: ", i))
   n <- 1e4
   dt <- data.table(Age = round(rnorm(n, 50, 5)), Gender = sample(factor(c("M", "F")), n, replace = T), RAF = rgamma(n, 1, 1))
   dt[, Rally := rbinom(.N, 1, inv.logit(rnorm(.N, -60 + Age + 5*as.numeric(Gender), 1)/10))]
   dt[, PD := rbinom(.N, 1, inv.logit(cbind(1, Rally, Age, as.numeric(Gender), RAF)%*%coef))]
   # dt[, table(Rally)]
   # dt[, table(PD)]
   # dt[, table(Rally, PD)]
   
   # Naive model
   # summary(mod <- glm(PD ~ Rally + Age + Gender + RAF, data = dt, family = "binomial"))
   
   
   # Manually matching (mlahm)
   
   summary(premod <- glm(Rally~Age+Gender+RAF, data = dt, family = "binomial"))
   
   # Use of logodds and probabilities both supported in literature, though the logodds seem to make more sense to me.
   quant <- quantile(predict(premod, newdata = dt[Rally == 1]), c(0, seq(0.1, .9, .05), 1))
   dt[, Bucket := cut(predict(premod), quant)]
   dt[, BucketIndex := 1:.N, by = list(Bucket, Rally)]
   dt[, Probability := predict(premod, type = "response")]
   dt[, Included := rbinom(.N, 1, Probability)]
   
   #ggplot(dt, aes(Age, fill = factor(Rally))) + geom_density(alpha = .3)
   
   dt2 <- dt[Rally == 1 | BucketIndex <= 30]
   
   dt[, table(Bucket, Rally)]
   dt2[, table(Bucket, Rally)]
   
   summary(mod1 <- glm(PD ~ Rally + Age + Gender + RAF, data = dt, family = "binomial"))
   summary(mod2 <- glm(PD ~ Rally + Age + Gender + RAF, data = dt2, family = "binomial"))
   summary(mod3 <- glm(PD ~ Rally + Age + Gender + RAF, data = dt[Included == 1], family = "binomial"))
   
   coefs <- rbind(coefs, data.table(Model = "Original", Iteration = i, Coefficient = coefnames, Value = unlist(coef(mod1))))
   coefs <- rbind(coefs, data.table(Model = "Matching", Iteration = i, Coefficient = coefnames, Value = unlist(coef(mod2))))
   coefs <- rbind(coefs, data.table(Model = "MatchProb", Iteration = i, Coefficient = coefnames, Value = unlist(coef(mod3))))
   
   summary(mod1 <- glm(PD ~ Rally, data = dt, family = "binomial"))
   summary(mod2 <- glm(PD ~ Rally, data = dt2, family = "binomial"))
   summary(mod3 <- glm(PD ~ Rally, data = dt[Included == 1], family = "binomial"))
   
   coefs1 <- rbind(coefs1, data.table(Model = "Original", Iteration = i, Coefficient = c("Intercept", "Rally"), Value = unlist(coef(mod1))))
   coefs1 <- rbind(coefs1, data.table(Model = "Matching", Iteration = i, Coefficient = c("Intercept", "Rally"), Value = unlist(coef(mod2))))
   coefs1 <- rbind(coefs1, data.table(Model = "MatchProb", Iteration = i, Coefficient = c("Intercept", "Rally"), Value = unlist(coef(mod3))))
 }
 
 ggplot(coefs, aes(Value)) + facet_grid(Model ~ Coefficient, scales = "free") + geom_density()
 ggplot(coefs, aes(Value)) + facet_grid(Model ~ Coefficient, scales = "free") + geom_histogram() + geom_vline(data = data.frame(Coefficient = coefnames, Value = coef), aes(xintercept = Value), col = "blue", lwd = 2)
 
 ggplot(coefs1, aes(Value)) + facet_grid(Model ~ Coefficient, scales = "free") + geom_histogram() + geom_vline(data = data.frame(Coefficient = c("Intercept", "Rally"), Value = coef[1:2]), aes(xintercept = Value), col = "blue", lwd = 2)
 
 
 
 
 # Exploring use of MatchIt package
 library(MatchIt)
 
 matchit()
 getAnywhere(matchit2exact)
 
 n <- 1e4
 dt <- data.table(Age = round(rnorm(n, 50, 5)), Gender = sample(factor(c("M", "F")), n, replace = T), RAF = rgamma(n, 1, 1))
 dt[, Rally := rbinom(.N, 1, inv.logit(rnorm(.N, -60 + Age + 5*as.numeric(Gender), 1)/10))]
 dt[, PD := rbinom(.N, 1, inv.logit(cbind(1, Rally, Age, as.numeric(Gender), RAF)%*%coef))]
 
 summary(glm(PD ~ Rally + Age + Gender, data = dt, family = "binomial"))
 summary(glm(PD ~ Rally, data = dt, family = "binomial"))
 
 options("optmatch_max_problem_size" = Inf)
 mi <- matchit(Rally ~ Age + Gender, data = dt)
 # mi2 <- matchit(Rally ~ Age + Gender, data = dt, method = "exact")
 # mi3 <- matchit(Rally ~ Age + Gender, data = dt, method = "full")
 mi4 <- matchit(Rally ~ Age + Gender, data = dt, method = "optimal")
 # mi5 <- matchit(Rally ~ Age + Gender, data = dt, method = "subclass")
 mi6 <- matchit(Rally ~ Age + Gender, data = dt, distance = "mahalanobis", method = "optimal")
 
 dt1 <- dt[,.SD]
 # dt2 <- dt[,.SD]
 # dt3 <- dt[,.SD]
 dt4 <- dt[,.SD]
 # dt5 <- dt[,.SD]
 dt6 <- dt[,.SD]
 
 dt1[as.numeric(row.names(mi$match.matrix)), Matched := as.numeric(mi$match.matrix[,1])]
 dt1[, Included := ifelse(1:.N %in% c(row.names(mi$match.matrix), mi$match.matrix[,1]), 1, 0)]
 dt1[, table(Included, Rally)]
 dt1 <- dt1[Included == 1]
 
 dt4[as.numeric(row.names(mi4$match.matrix)), Matched := as.numeric(mi4$match.matrix[,1])]
 dt4[, Included := ifelse(1:.N %in% c(row.names(mi4$match.matrix), mi4$match.matrix[,1]), 1, 0)]
 dt4[, table(Included, Rally)]
 dt4 <- dt4[Included == 1]
 
 dt6[as.numeric(row.names(mi6$match.matrix)), Matched := as.numeric(mi6$match.matrix[,1])]
 dt6[, Included := ifelse(1:.N %in% c(row.names(mi6$match.matrix), mi6$match.matrix[,1]), 1, 0)]
 dt6[, table(Included, Rally)]
 dt6 <- dt6[Included == 1]
 
 
 summary(glm(PD ~ Rally + Age + Gender, data = dt1))
 summary(glm(PD ~ Rally, data = dt1))
 summary(glm(PD ~ Rally + Age + Gender, data = dt2))
 summary(glm(PD ~ Rally, data = dt2))
 summary(glm(PD ~ Rally + Age + Gender, data = dt3))
 summary(glm(PD ~ Rally, data = dt3))
 summary(glm(PD ~ Rally + Age + Gender, data = dt4))
 summary(glm(PD ~ Rally, data = dt4))
 summary(glm(PD ~ Rally + Age + Gender, data = dt5))
 summary(glm(PD ~ Rally, data = dt5))
 
 
 dim(dt1); dim(dt2); dim(dt3); dim(dt4); dim(dt5)
 
 
 dt[, mean(PD), by = Rally]
 dt1[, mean(PD), by = Rally]
 dt4[, mean(PD), by = Rally]
 dt6[, mean(PD), by = Rally]
 
 
 dt[, lapply(.SD, mean), .SDcols = c("PD", "Rally", "Age", "GenderNum", "RAF"), by = list(Rally)]
 dt1[, lapply(.SD, mean), .SDcols = c("PD", "Rally", "Age", "GenderNum", "RAF"), by = list(Rally)]
 dt4[, lapply(.SD, mean), .SDcols = c("PD", "Rally", "Age", "GenderNum", "RAF"), by = list(Rally)]
 dt6[, lapply(.SD, mean), .SDcols = c("PD", "Rally", "Age", "GenderNum", "RAF"), by = list(Rally)]
 
# End script
 