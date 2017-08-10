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
 
 convert2pref <- function(ranked, text = F) {
   names <- head(names(ranked), -1)
   pref <- data.table(t(apply(ranked[, -ncol(ranked), with = F], 1, order)))
   if(text)
     pref <- pref[, lapply(.SD, function(x) names[x])]
   pref[, N := ranked[, ncol(ranked), with = F]]
   colnames(pref) <- c(paste0("Pref", 1:(ncol(pref)-1)), "N")
   attr(pref, "Candidates") <- names
   pref
 }
 
 plural <- function(dt, names = NULL) {
   if(!is.null(attr(dt, "Candidates")) & is.null(names))
     names = attr(dt, "Candidates")
   dt <- dt[, .SD]
   setnames(dt, c(paste0("Pref", 1:(ncol(dt)-1)), "N"))
   dt2 <- dt[, -which(colnames(dt) == "N"), with = F]
   
   if(is.null(names))
     dt[, list(Votes = sum(N)), by = list(Candidate = Pref1)][order(-Votes)]
   else
     dt[, list(Votes = sum(N)), by = list(Candidate = names[Pref1])][order(-Votes)]
 }
 
 runoff <- function(dt, names = NULL) {
   if(!is.null(attr(dt, "Candidates")) & is.null(names))
     names = attr(dt, "Candidates")
   dt <- dt[, .SD]
   setnames(dt, c(paste0("Pref", 1:(ncol(dt)-1)), "N"))
   dt2 <- dt[, -which(colnames(dt) == "N"), with = F]
   counts <- dt[, list(Votes = sum(N)), by = list(Candidate = Pref1)][order(-Votes)]
   if(max(counts$Votes) <= .5*sum(counts$Votes)) {
     cands <- counts[order(-Votes)][1:2, Candidate]
     counts <- dt[, data.table(t(apply(cbind(.SD[, -which(colnames(dt)=="N"), with = F]), 1, intersect, cands)), N)]
     counts <- counts[, list(Votes = sum(N)), by = list(Candidate = V1)][order(-Votes)]
   }
   counts
 }
 
 irv <- function(dt, verbose = F, names = NULL) {
   if(!is.null(attr(dt, "Candidates")) & is.null(names))
     names = attr(dt, "Candidates")
   dt <- dt[, .SD]
   setnames(dt, c(paste0("Pref", 1:(ncol(dt)-1)), "N"))
   dt2 <- dt[, -which(colnames(dt) == "N"), with = F]
   dt2orig <- dt2[, .SD]
   opts <- unique(unlist(dt2))
   
   results <- numeric(0)
   
   while(length(results) < length(opts)) {
     winner <- F
     curr <- dt[, list(N = sum(N)), by = Pref1]
     if(verbose)
       print(curr)
     if(max(prop.table(curr$N)) > .5)
       winner <- curr[, Pref1[which.max(prop.table(N))]]
     while(!winner) {
       loser <- curr[, Pref1[which.min(prop.table(N))]]
       if(length(loser) > 1)
         loser <- sample(loser, 1)
       if(verbose)
         print(paste("Candidate", loser, "eliminated."))
       dt2 <- data.table(t(apply(dt2, 1, function(x) x[-which(x == loser)])))
       setnames(dt2, c(paste0("Pref", 1:ncol(dt2))))
       dt <- cbind(dt2, N = dt$N)
       curr <- dt[, list(N = sum(N)), by = Pref1]
       if(verbose)
         print(curr)
       if(max(prop.table(curr$N)) > .5)
         winner <- curr[, Pref1[which.max(prop.table(N))]]
     }
     results <- c(results, winner)
     dt2 <- data.table(t(apply(dt2orig, 1, function(x) x[-which(x %in% results)])))
     setnames(dt2, c(paste0("Pref", 1:ncol(dt2))))
     dt <- cbind(dt2, N = dt$N)
     if(length(results) + 1 == length(opts))
       results <- c(results, setdiff(opts, results))
   }
   if(is.null(names))
     results
   else
     names[results]
 }
 
 shulze <- function(dt, all = T, names = NULL, verbose = F) {
   if(!is.null(attr(dt, "Candidates")) & is.null(names))
     names = attr(dt, "Candidates")
   dt <- dt[, .SD]
   setnames(dt, c(paste0("Pref", 1:(ncol(dt)-1)), "N"))
   dt2 <- dt[, -which(colnames(dt) == "N"), with = F]
   opts <- unique(unlist(dt2[1:pmin(50, nrow(dt2))]))
   nopts <- length(opts)
   
   d <- function(i, j) {
     dt[apply(dt2, 1, function(x) which(x == i)) < apply(dt2, 1, function(x) which(x == j)), sum(N)]
   }
   
   p <- matrix(0, nopts, nopts)
   for (i in 1:nopts) {
     for (j in 1:nopts) {
       if(verbose)
         cat(paste0("\r1st loop (", nopts, "^2 iterations = ", nopts^2, "): ", i, " ", j))
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
         for (k in 1:nopts) {
           if(verbose)
             cat(paste0("\r2nd loop (", nopts, "^3 iterations = ", nopts^3, "): ", i, " ", j, " ", k))
           if(i != k & j != k)
             p[j,k] <- max(p[j,k], min(p[j,i], p[i,k]))
         }
     }
   }
   if(verbose)
     cat("\n")
   
   winner <- order(apply(p, 2, function(x) sum(x != 0)))
   if(!all)
     winner <- winner[1]
   if(is.null(names))
     winner
   else
     names[winner]
 }
 
 rpairs <- function(dt, names = NULL, verbose = F) {
   if(!is.null(attr(dt, "Candidates")) & is.null(names))
     names = attr(dt, "Candidates")
   dt <- dt[, .SD]
   setnames(dt, c(paste0("Pref", 1:(ncol(dt)-1)), "N"))
   n <- ncol(dt) - 1
   temp <- NULL
   for(i in 1:(n-1)) {
     for(j in (i+1):n) {
       if(verbose)
         cat(paste0("\rIterations: ", i, " ", j))
       temp <- rbind(temp, dt[, list(Top = get(paste0("Pref", i)), Bottom = get(paste0("Pref", j)), Count = N)])
     }
   }
   if(verbose)
     cat("\n")
   pairs <- temp[, list(Count = sum(Count)), by = list(Top, Bottom)][order(-Count)]
   A <- matrix(0, nrow = n, ncol = n)
   i = 1
   while(sum(A) < n*(n-1)/2) {
     Atmp <- A
     Atmp[pairs[i, Top], pairs[i, Bottom]] <- 1
     if(sum(eigen(Atmp)$values==0) >= n-1)
       A <- Atmp
     i <- i + 1
   }
   results <- NULL
   for (i in 1:n) {
     ind <- which(apply(A, 2, sum) == 0)
     A[ind, ] <- 0
     A[, ind] <- 1
     results[i] <- ind
   }
   if(is.null(names))
     results
   else
     names[results]
 }
 
 kyoung <- function(dt, names = NULL, verbose = F) {
   if(!is.null(attr(dt, "Candidates")) & is.null(names))
     names = attr(dt, "Candidates")
   dt <- dt[, .SD]
   setnames(dt, c(paste0("Pref", 1:(ncol(dt)-1)), "N"))
   n <- ncol(dt) - 1
   temp <- NULL
   for(i in 1:(n-1)) {
     for(j in (i+1):n) {
       if(verbose)
         cat(paste0("\rIterations: ", i, " ", j))
       temp <- rbind(temp, dt[, list(Top = get(paste0("Pref", i)), Bottom = get(paste0("Pref", j)), Count = N)])
     }
   }
   if(verbose)
     cat("\n")
   pairs <- temp[, list(Count = sum(Count)), by = list(Top, Bottom)][order(-Count)]
   mat <- pairs[, {
     mat <- matrix(0, 4, 4)
     mat[cbind(Top, Bottom)] <- Count
     mat }]
   
   currmax <- 0
   perms <- permutations(n, n)
   for (i in 1:nrow(perms)) {
     score <- sum(upper.tri(mat)*mat[perms[i,], perms[i,]])
     if(score > currmax) {
       maxi <- i
       currmax = score
     }
   }
   if(is.null(names))
     perms[maxi,]
   else
     names[perms[maxi,]]
 }
 
 borda <- function (dt, values = c("standard", "fraction"), names = NULL) {
   if(!is.null(attr(dt, "Candidates")) & is.null(names))
     names = attr(dt, "Candidates")
   dt <- dt[, .SD]
   setnames(dt, c(paste0("Pref", 1:(ncol(dt)-1)), "N"))
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
   
   if(!is.null(names))
     dt[, Candidate := names[Candidate]]
   dtlong[, list(Score = sum(N*Points)), by = Candidate][order(-Score)]
 }
 
 set.seed(8375)
 ncand <- 4
 nvoter <- 1e3
 nissue <- 6
 votes <- vote(ncand, nvoter, nissue)
 
 plural(votes)
 runoff(votes)
 irv(votes)
 shulze(votes)
 rpairs(votes)
 kyoung(votes)
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
 
 
 ## Fairness Criteria
 
 # 1. Majority criteria (we're voting duh)
 
 # 2. Condorcet criteria (winner should be preferred to any other candidate)
 
 # 3. Monotonicity criteria (changes in favor of the winner should not change the outcome)
 
 # 4. Independence of irrelevant alternatives (adding more losers shouldn't change the outcome)
 
 # Later-no-harm (giving a later preference should not cause a more preferred candidate to lose)
 
 # Consistency (sub elections achieving the same result should mean that that result will hold in a combined election)
 
 # Participation (Choosing to vote should never hurt your preferred candidate)
 
 # Resolvability (low probability of ties)
 
 # Reversal symmetry (a winning candidate should not win if all voter preferences are reversed)
 
 
 ###############################################
 ###    Brown Bag Presentation Outline
 ###########################################
 
 ## Purpose(s) of Voting (Come to a general concensus of PREFERENCE)
 # - Choose one option from many
 # - Choose multiple options from many
 # - Rank/score all options
 ## Give a survey to everyone
 
 ## Discuss decision scenarios
 # - Baisc choices: movie, game, dinner options
 # - Business decisions: who to hire, product to buy, design to implement
 # - Elected officials/committees
 # - etc.
 
 ## Criteria of voting systems
 # - ask for opinions, describe situations
 # - examples of situations that violate each criteria
 
 ## Voting systems
 # - Plurality (the usual)
 # - Instant runoff voting (IRV)
 # - Condorcet methods (multiple methods for when condorcet winner does not exist)
 
 ## The grid
 
 ## Discuss preferences (discussion of systems, vote on systems)
 
 ## Example vote calculations
 
 # Upload exported survey data
 library(openxlsx)
 survey <- read.csv("c:/users/cmcneil/downloads/Export Savvysherpa Office Preferences.csv", header = F, stringsAsFactors = F)
 
 getvotes <- function(survey) {
   if(is.character(survey))
     survey <- read.csv(survey, header = F, stringsAsFactors = F)
   ends <- which(survey[1,-1] != "" & survey[1,-ncol(survey)] == "")
   starts <- c(1, head(ends, -1) + 1)
   questions <- as.character(unlist(survey[1, starts]))
   
   survvotes <- lapply(seq_along(questions), function(i) {
     names <- as.character(unlist(survey[2, starts[i]:ends[i]]))
     mat <- as.data.table(lapply(survey[3:nrow(survey), starts[i]:ends[i]], as.numeric))
     colnames(mat) <- names
     mat[, .N, by = mat]
   })
 }
 
 
 
 # Multivariate normal to multivariate beta
 
 x <- rnorm(1e3, .5, 1)
 hist(x)
 hist(inv.logit(x), breaks = 50)
 
 
 y <- rbeta(1e4, 14, 19)
 hist(y)
 hist(logit(y), breaks = 50)
 qqnorm(logit(y))
 
 
# End script
 