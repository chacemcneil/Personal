library(pwr)
#library(tidyr)
library(reshape2)
library(lme4)
library(lmerTest)

# ICC of .8 from write up  (I got an ICC of .5 from Commercial walking population)


# This creates 1 set of data
run.number <- 5000
hold.coefficients <- as.data.frame(matrix(nrow = run.number, ncol = 1))
for(q in 1:run.number){

store <- as.data.frame(matrix(nrow = 300, ncol = 33))
colnames(store)[1:3] <- c("id", "group", "Start")
store[,"id"] <- 1:300
store[1:150,"group"] <- "Control"
store[151:300, "group"] <- "Treat"

treat150 <- rnorm(150, mean = 5000, sd = 2951)
treat150[treat150 < 500] <- 500
store[1:150,"Start"] <- treat150

treat150 <- rnorm(150, mean = 6000, sd = 2951)
treat150[treat150 < 500] <- 500
store[151:300,"Start"] <- treat150

for(i in 4:33){
  for(j in 1:300){
    store[j,i] <- rnorm(1, mean = store[j,"Start"], sd = 1447)
  }
}


store[store < 0] <- 0

# now go from wide to long

store.long <- melt(store[,-3], id.vars = c("id", "group"))
colnames(store.long)[4] <- "Steps"

model <- lmer(Steps ~ group + (1|id), data = store.long)
hold.coefficients[q, 1] <- summary(model)$coefficients[2,5]

if(q%%10 == 0){
  cat("\rIteration: ", q, "\tTime elapsed: ", round(proc.time()[3] - t0), " seconds")
}
}

colnames(hold.coefficients)[1] <- "Walk"

# This creates 1 set of data
run.number <- 5000
hold.coefficients.2 <- as.data.frame(matrix(nrow = run.number, ncol = 1))
for(q in 1:run.number){
  
  store <- as.data.frame(matrix(nrow = 300, ncol = 8))
  colnames(store)[1:3] <- c("id", "group", "Start")
  store[,"id"] <- 1:300
  store[1:150,"group"] <- "Control"
  store[151:300, "group"] <- "Treat"
  
  treat150 <- rnorm(150, mean = 5000, sd = 2951)
  treat150[treat150 < 500] <- 500
  store[1:150,"Start"] <- treat150
  
  treat150 <- rnorm(150, mean = 6000, sd = 2951)
  treat150[treat150 < 500] <- 500
  store[151:300,"Start"] <- treat150
  
  for(i in 4:8){
    for(j in 1:300){
      store[j,i] <- rnorm(1, mean = store[j,"Start"], sd = 1447)
    }
  }
  
  store[store < 0] <- 0
  
  # now go from wide to long
  
  store.long <- melt(store[,-3], id.vars = c("id", "group"))
  colnames(store.long)[4] <- "Steps"
  
  model <- lmer(Steps ~ group + (1|id), data = store.long)
  hold.coefficients.2[q, 1] <- summary(model)$coefficients[2,5]
  
  if(q%%10 == 0){
    print(q)
  }
}

#colnames(hold.coefficients)[1] <- "Walk"
colnames(hold.coefficients.2)[1] <- "Sleep"

pvalues <- cbind(hold.coefficients, hold.coefficients.2)
pvalues$Reject_Walk <- 0
pvalues$Reject_Sleep <- 0

for(i in 1:1000){
  if(pvalues[i,"Walk"] < pvalues[i,"Sleep"]){
    if(pvalues[i, "Walk"] < .025){
      pvalues[i, "Reject_Walk"] <- 1
      if(pvalues[i, "Sleep"] < .05){
        pvalues[i, "Reject_Sleep"] <- 1
      }
    }
  }
  
  if(pvalues[i,"Sleep"] < pvalues[i,"Walk"]){
    if(pvalues[i, "Sleep"] < .025){
      pvalues[i, "Reject_Sleep"] <- 1
      if(pvalues[i, "Walk"] < .05){
        pvalues[i, "Reject_Walk"] <- 1
      }
    }
  }
}

sum(pvalues$Reject_Sleep)/run.number
sum(pvalues$Reject_Walk)/run.number


############################## Recreate code above ##############################

makeActiveBinding("ti", function() proc.time()[3] - t0, globalenv())

ntreat <- 150
ncont <- 150
nreps <- 30
sd.indv <- 2951
sd.day <- 1447

coefs <- NULL
pvals <- NULL
t0 <- proc.time()[3]
for (q in 1:run.number) {
  dt <- data.table(ID = 1:(ntreat + ncont), Group = rep(c("Treatment", "Control"), times = c(ntreat, ncont)))
  dt[, Start := rnorm(1:.N, 5000 + 1000*(Group == "Treatment"), sd = sd.indv)]
  dt[, Start := pmax(500, Start)]
  
  dt.long <- dt[, list(Rep = 1:nreps, Steps = rnorm(nreps, Start, sd = sd.day)), by = list(ID, Group)]
  dt.long[, Steps := pmax(0, Steps)]
  
  mod <- lmer(Steps ~ Group + (1|ID), data = dt.long, control = lmerControl(calc.derivs = F), REML = T)
  coefs <- rbind(coefs, data.table(Run = q, Coef = summary(mod)$coefficients[2, 1]))
  pvals <- rbind(pvals, data.table(Run = q, Coef = summary(mod)$coefficients[2, 5]))
  
  if(q%%10 == 0)
    cat("\rIteration: ", q, "\tTime elapsed: ", round(proc.time()[3] - t0), " seconds")
}

nreps = 8

coefs2 <- NULL
pvals2 <- NULL
t0 <- proc.time()[3]
for (q in 1:run.number) {
  dt <- data.table(ID = 1:(ntreat + ncont), Group = rep(c("Treatment", "Control"), times = c(ntreat, ncont)))
  dt[, Start := rnorm(1:.N, 5000 + 1000*(Group == "Treatment"), sd = sd.indv)]
  dt[, Start := pmax(500, Start)]
  
  dt.long <- dt[, list(Rep = 1:nreps, Steps = rnorm(nreps, Start, sd = sd.day)), by = list(ID, Group)]
  dt.long[, Steps := pmax(0, Steps)]
  
  mod <- lmer(Steps ~ Group + (1|ID), data = dt.long, control = lmerControl(calc.derivs = FALSE))
  coefs2 <- rbind(coefs2, data.table(Run = q, Coef = summary(mod)$coefficients[2, 1]))
  pvals2 <- rbind(pvals2, data.table(Run = q, PVal = summary(mod)$coefficients[2, 5]))
  
  if(q%%10 == 0)
    cat("\rIteration: ", q, "\tTime elapsed: ", round(proc.time()[3] - t0), " seconds")
}

save(coefs, pvals, coefs2, pvals2, file = "/ssdpnas_users02/Users/cmcneil/Personal/SampleSize/results.rda")
load("/ssdpnas_users02/Users/cmcneil/Personal/SampleSize/results.rda")

ps <- merge(pvals[, list(Run, PVal_Steps = PVal)], pvals2[, list(Run, PVal_Sleep = PVal)], by = "Run")

reject <- function(pvals, alpha = .05) {
  as.numeric(pvals < alpha / (length(pvals) + 1 - rank(pvals)))
}

ps[, paste0("Pval_", c("Steps", "Sleep")) := apply(.SD, 1, reject), .SDcols = c("PVal_Steps", "PVal_Sleep")]

# End script
