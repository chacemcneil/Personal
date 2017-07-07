# Preferrential voting simulations
 library(data.table)
 library(gtools)
 
 
 # Generate random voter preferences
 
 vote <- function(ncand, nvoter, nissue, cand_a = 1, cand_b = 1, voter_a = 1, voter_b = 1) {
   cand <- matrix(rbeta(ncand*nissue, cand_a, cand_b), ncand, nissue)
   voter <- matrix(rbeta(nvoter*nissue, voter_a, voter_b), nvoter, nissue)
   
   pref <- sapply(1:ncand, function(i) sapply(1:nvoter, function(j) sum((cand[i,] - voter[j,])^2) ) )
   
   ranks <- t(apply(-pref, 1, rank))
   votes <- data.table(ranks)[, .N, keyby = list(V1, V2, V3, V4)]
   
   votes
 }
 
 # Evaluate votes
 
 plural <- function(dt) {
   setnames(dt, c(paste0("V", 1:(ncol(dt)-1)), "N"))
   dt2 <- dt[, -which(colnames(dt) == "N"), with = F]
   opts <- unique(unlist(dt2))
   
   dt[, list(Votes = sum(N)), by = list(Candidate = V1)][order(-Votes)]
 }
 
 runoff <- function(dt, verbose = F) {
   setnames(dt, c(paste0("V", 1:(ncol(dt)-1)), "N"))
   dt2 <- dt[, -which(colnames(dt) == "N"), with = F]
   dt2orig <- dt2[, .SD]
   opts <- unique(unlist(dt2))
   
   results <- numeric(0)
   
   while(length(results) < length(opts)) {
     winner <- F
     curr <- dt[, list(N = sum(N)), by = V1]
     if(verbose)
       print(curr)
     if(max(prop.table(curr$N)) > .5)
       winner <- curr[, V1[which.max(prop.table(N))]]
     while(!winner) {
       loser <- curr[, V1[which.min(prop.table(N))]]
       if(length(loser) > 1)
         loser <- sample(loser, 1)
       if(verbose)
         print(paste("Candidate", loser, "eliminated."))
       dt2 <- data.table(t(apply(dt2, 1, function(x) x[-which(x == loser)])))
       dt <- cbind(dt2, N = dt$N)
       curr <- dt[, list(N = sum(N)), by = V1]
       if(verbose)
         print(curr)
       if(max(prop.table(curr$N)) > .5)
         winner <- curr[, V1[which.max(prop.table(N))]]
     }
     results <- c(results, winner)
     dt2 <- data.table(t(apply(dt2orig, 1, function(x) x[-which(x %in% results)])))
     dt <- cbind(dt2, N = dt$N)
     if(length(results) + 1 == length(opts))
       results <- c(results, setdiff(opts, results))
   }
   results
 }
 
 shulze <- function(dt, all = T) {
   setnames(dt, c(paste0("V", 1:(ncol(dt)-1)), "N"))
   dt2 <- dt[, -which(colnames(dt) == "N"), with = F]
   opts <- unique(unlist(dt2))
   nopts <- length(opts)
   
   d <- function(i, j) {
     dt[apply(dt2, 1, function(x) which(x == i)) < apply(dt2, 1, function(x) which(x == j)), sum(N)]
   }
   
   p <- matrix(0, nopts, nopts)
   for (i in 1:nopts) {
     for (j in 1:nopts) {
       if(i != j)
         if(d(i, j) > d(j, i))
           p[i,j] <- d(i, j)
         else
           p[i,j] <- 0
     }
   }
   
   for (i in 1:nopts) {
     for (j in 1:nopts) {
       if(i != j)
         for (k in 1:nopts)
           if(i != k & j != k)
             p[j,k] <- max(p[j,k], min(p[j,i], p[i,k]))
     }
   }
   
   winner <- order(apply(p, 2, function(x) sum(x != 0)))
   if(!all)
     winner <- winner[1]
   winner
 }
 
 borda <- function (dt, values = c("standard", "fraction")) {
   setnames(dt, c(paste0("V", 1:(ncol(dt)-1)), "N"))
   dt2 <- dt[, -which(colnames(dt) == "N"), with = F]
   opts <- unique(unlist(dt2))
   nopts <- length(opts)
   
   if (class(values) != "numeric") {
     values <- switch(match.arg(values),
                   standard = nopts:1 - 1,
                   fraction =  1/(1:nopts) )
   }
   
   dtlong <- melt(dt, id.vars = "N", value.name = "Candidate", variable.name = "Preference")
   dtlong[, Points := values[as.numeric(Preference)]]
   
   dtlong[, list(Score = sum(N*Points)), by = Candidate][order(-Score)]
 }
 
 set.seed(8375)
 ncand <- 4
 nvoter <- 1e3
 nissue <- 6
 votes <- vote(ncand, nvoter, nissue)
 
 plural(votes)
 runoff(votes)
 shulze(votes)
 borda(votes)
 
 N <- 1e2
 winners <- data.table(Method = "", First = 0, Second = 0, Third = 0, Fourth = 0)[0]
 winners <- NULL
 for (i in 1:N) {
   cat(paste0("\rIteration: ", i))
   votes <- vote(ncand, nvoter, nissue)
   winners <- rbind(winners, data.table(Plurality = plural(votes)$C[1],
                                        Runoff = runoff(votes)[1],
                                        Shulze = shulze(votes)[1],
                                        Borda = borda(votes)$C[1] ))
 }
 
 
 ## Methods
 
 # Pluarlity
 
 # Instant-runoff
 
 # Condorcet (multiple)
 
 # Borda count
 
 # Approval (non-pref)
 
 # Anti-pluarality
 
 # Dot voting (non-pref, cumulative)
 
 # Two-stage voting
 
 
 ## Criteria
 
 # Majority criteria (we're voting duh)
 
 # Condorcet criteria (winner should be preferred to any other candidate)
 
 # Monotonicity criteria (changes in favor of the winner should not change the outcome)
 
 # Independence of irrelevant alternatives (adding more losers shouldn't change the outcome)
 
 # Later-no-harm (giving a later preference should not cause a more preferred candidate to lose)
 
 # Consistency (sub elections achieving the same result should mean that that result will hold in a combined election)
 
 # Participation (Choosing to vote should never hurt your preferred candidate)
 
 # Resolvability (low probability of ties)
 
 # Reversal symmetry (a winning candidate should not win if all voter preferences are reversed)
 
 
 
 
 
 # Multivariate normal to multivariate beta
 
 x <- rnorm(1e3, .5, 1)
 hist(x)
 hist(inv.logit(x), breaks = 50)
 
 
 y <- rbeta(1e4, 14, 19)
 hist(y)
 hist(logit(y), breaks = 50)
 qqnorm(logit(y))
 
 
# End script
 