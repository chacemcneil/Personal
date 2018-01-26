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
 
 rank2pref <- function(ranks) {
   names <- head(names(ranks), -1)
   question <- attr(ranks, "Question")
   ranks <- ranks[apply(ranks[, names, with = F], 1, function(x) all(!is.na(x))), .SDcols = names]
   pref <- data.table(t(apply(ranks[, -ncol(ranks), with = F], 1, order)))
   pref[, N := ranks[, ncol(ranks), with = F]]
   colnames(pref) <- c(paste0("Pref", 1:(ncol(pref)-1)), "N")
   pref <- pref[, list(N = sum(N)), by = pref[, -ncol(pref), with = F]]
   attr(pref, "Candidates") <- names
   attr(pref, "Question") <- question
   pref
 }
 
 formatpref <- function(pref, names = NULL) {
   if(!is.null(attr(pref, "Candidates")) & is.null(names))
     names = attr(pref, "Candidates")
   pref <- pref[, list(N = sum(N)), by = pref[, -ncol(pref), with = F]]
   dt <- pref[, .SD]
   setnames(dt, c(paste0("Pref", 1:(ncol(dt)-1)), "N"))
   # dt <- dt[apply(.SD, 1, function(x) all(is.na(x))), .SDcols = c(paste0("Pref", 1:(ncol(dt)-1)), "N")]
   attr(dt, "Candidates") <- names
   attr(dt, "Question") <- attr(pref, "Question")
   dt
 }
 
 plural <- function(pref, names = NULL, verbose = F, winneronly = F) {
   dt <- formatpref(pref, names = names)
   names <- attr(dt, "Candidates")
   if(nrow(dt) == 0)
     return(NA)
   dt2 <- dt[, -which(colnames(dt) == "N"), with = F]
   
   if(is.null(names))
     counts <- dt[, list(Votes = sum(N)), by = list(Candidate = Pref1)][order(-Votes)]
   else
     counts <- dt[, list(Votes = sum(N)), by = list(Candidate = names[Pref1])][order(-Votes)]
   if(winneronly)
     counts <- counts[1, Candidate]
   counts
 }
 
 runoff <- function(pref, names = NULL, verbose = F, winneronly = F) {
   dt <- formatpref(pref, names = names)
   names <- attr(dt, "Candidates")
   if(nrow(dt) == 0)
     return(NA)
   dt2 <- dt[, -which(colnames(dt) == "N"), with = F]
   counts <- dt[, list(Votes = sum(N)), by = list(Candidate = Pref1)][order(-Votes)]
   if(max(counts$Votes) <= .5*sum(counts$Votes)) {
     cands <- counts[order(-Votes)][1:2, Candidate]
     counts <- dt[, data.table(t(apply(cbind(.SD[, -which(colnames(dt)=="N"), with = F]), 1, intersect, cands)), N)]
     counts <- counts[, list(Votes = sum(N)), by = list(Candidate = V1)][order(-Votes)]
   }
   if(winneronly)
     counts <- counts[1, Candidate]
   if(!is.null(names))
     counts <- names[counts]
   counts
 }
 
 irv <- function(pref, names = NULL, verbose = F, winneronly = F) {
   dt <- formatpref(pref, names = names)
   names <- attr(dt, "Candidates")
   if(nrow(dt) == 0)
     return(NA)
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
   if(winneronly)
     results <- results[1]
   if(is.null(names))
     results
   else
     names[results]
 }
 
 shulze <- function(pref, names = NULL, verbose = F, winneronly = F) {
   dt <- formatpref(pref, names = names)
   names <- attr(dt, "Candidates")
   if(nrow(dt) == 0)
     return(NA)
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
   if(winneronly)
     winner <- winner[1]
   if(is.null(names))
     winner
   else
     names[winner]
 }
 
 rpairs <- function(pref, names = NULL, verbose = F, winneronly = F) {
   dt <- formatpref(pref, names = names)
   names <- attr(dt, "Candidates")
   if(nrow(dt) == 0)
     return(NA)
   n <- ncol(dt) - 1
   temp <- NULL
   for(i in 1:(n-1)) {
     for(j in (i+1):n) {
       if(verbose)
         cat(paste0("\rIterations: ", i, " ", j))
       temp <- rbind(temp, dt[, list(Top = get(paste0("Pref", i)), Bottom = get(paste0("Pref", j)), Count = N)])
     }
   }
   pairs <- temp[, list(Count = sum(Count)), by = list(Top, Bottom)][order(-Count)]
   if(verbose) {
     cat("\n")
     print(pairs)
   }
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
   if(winneronly)
     results <- results[1]
   if(is.null(names))
     results
   else
     names[results]
 }
 
 kyoung <- function(pref, names = NULL, verbose = F, winneronly = F) {
   dt <- formatpref(pref, names = names)
   names <- attr(dt, "Candidates")
   if(nrow(dt) == 0)
     return(NA)
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
     mat <- matrix(0, n, n)
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
   if(winneronly) {
     if(is.null(names))
       perms[maxi,1]
     else
       names[perms[maxi,1]]
   } else {
     if(is.null(names))
       perms[maxi,]
     else
       names[perms[maxi,]]
   }
 }
 
 borda <- function (pref, names = NULL, verbose = F, winneronly = F, values = c("standard", "fraction")) {
   dt <- formatpref(pref, names = names)
   names <- attr(dt, "Candidates")
   if(nrow(dt) == 0)
     return(NA)
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
     dtlong[, Candidate := names[Candidate]]
   if(winneronly)
     dtlong[, list(Score = sum(N*Points)), by = Candidate][order(-Score)][1, Candidate]
   else
     dtlong[, list(Score = sum(N*Points)), by = Candidate][order(-Score)]
 }
 
 condorcet <- function(pref, names = NULL, verbose = F, winneronly = F) {
   dt <- formatpref(pref, names = names)
   names <- attr(dt, "Candidates")
   if(nrow(dt) == 0)
     return(NA)
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
     mat <- matrix(0, n, n)
     mat[cbind(Top, Bottom)] <- Count
     mat }]
   winner <- which(apply(mat >= t(mat), 1, prod) == 1)
   if(length(winner) == 0)
     return(NA)
   
   if(winneronly)
     winner <- winner[1]
   if(is.null(names))
     winner
   else
     names[winner]
 }
 
 vote.methods <- list(Plurality = plural,
                      Runoff = runoff,
                      "Instant-Runoff" = irv,
                      Condorcet = condorcet,
                      Shulze = shulze,
                      "Ranked Pairs" = rpairs,
                      "Kemeney-Young" = kyoung,
                      "Borda Count" = borda)
 
 displaypref <- function(pref, names = NULL, verbose = F, colorcode = T, cols = NULL) {
   if(is.null(cols))
     cols <- hex(c("turquoise1", "plum", "green", "coral", "lightgoldenrod1", "dodgerblue1", "azure3", "tan3"))
   if(is.numeric(cols))
     cols <- hex(c("turquoise1", "plum", "green", "coral", "lightgoldenrod1", "dodgerblue1", "azure3", "tan3"))[cols]
   dt <- formatpref(pref, names = names)
   mat <- dt[, t(.SD), .SDcols = head(colnames(dt), -1)]
   txt <- as.matrix(apply(mat, 2, function(x) attr(dt, "Candidates")[x]))
   colnames(txt) <- dt$N
   txt <- cbind(Votes = paste("Pref", 1:nrow(txt)), txt)
   bg <- apply(mat, 2, function(x) cols[x])
   bg <- cbind("#FFFFFF", as.matrix(bg))
   if(colorcode) {
     css.cell <- "padding: 1em;"
     css.cell <- matpaste(css.cell, " background-color: ", bg, ";", sep = "")
     htmlTable(txt, align = "c", css.cell = css.cell, caption = attr(pref, "Question"), rnames = F)
   } else
     htmlTable(txt, align = "c", css.cell = padding(1), caption = attr(pref, "Question"), css.header = "font-size: 18px;", css.rgroup = "padding-right: 1em;", rnames = F)
 }
 
 displaypaired <- function(pref, names = NULL, verbose = F, colorcode = T, cols = NULL) {
   if(is.null(cols))
     cols <- c("salmon", "skyblue")
   cols <- hex(cols)
   dt <- formatpref(pref, names = names)
   names <- attr(dt, "Candidates")
   if(nrow(dt) == 0)
     return(NA)
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
     mat <- matrix(0, n, n)
     mat[cbind(Top, Bottom)] <- Count
     mat }]
   row.names(mat) <- colnames(mat) <- names
   bg <- ifelse(mat > t(mat), cols[1], cols[2])
   bg <- cbind(cols[1], bg)
   bg <- rbind(cols[2], bg)
   txt <- cbind(row.names(mat), mat)
   txt <- rbind(c("", colnames(mat)), txt)
   colnames(txt) <- NULL
   diag(bg) <- "#FFFFFF"
   css.cell <- padding(1)
   css.cell <- matpaste(css.cell, "background-color: ", bg, ";", sep = "")
   htmlTable(txt, align = "rc", css.cell = css.cell, rnames = F, caption = "Head-to-head Comparisons")
 }
 
 remove <- function(pref, cands) {
   if(is.character(cands))
     cands <- match(cands, attr(pref, "Candidates"))
   dt <- formatpref(pref, names = NULL)
   dt2 <- dt[, .SD][, N := NULL]
   newdt <- data.table(t(apply(dt2, 1, function(x) rank(x[!x %in% cands]))), N = dt$N)
   colnames(newdt) <- c(paste0("Pref", 1:(ncol(newdt) - 1)), "N")
   attr(newdt, "Candidates") <- attr(dt, "Candidates")[-cands]
   if(!is.null(attr(pref, "Question")))
     attr(newdt, "Question") <- attr(pref, "Question")
   newdt2 <- formatpref(newdt, names = NULL)
   newdt2
 }
 
 removerank <- function(ranks, cands) {
   if(is.character(cands))
     cands <- match(cands, colnames(ranks))
   dt <- ranks[, -cands, with = F][, N := NULL]
   newdt <- data.table(t(apply(dt, 1, function(x) order(x))), N = ranks$N)
   colnames(newdt) <- colnames(ranks)[-cands]
   if(!is.null(attr(ranks, "Question")))
     attr(newdt, "Question") <- attr(ranks, "Question")
   newdt
 }
 
 set.seed(8375)
 set.seed(92717)
 set.seed(84120)
 ncand <- 4
 nvoter <- 1e3
 nissue <- 6
 votes <- vote(ncand, nvoter, nissue)
 
 if(F) {
   
   plural(votes)
   runoff(votes)
   irv(votes)
   condorcet(votes)
   shulze(votes)
   rpairs(votes)
   kyoung(votes)
   borda(votes)
   
   plural(votes, winneronly = T)
   runoff(votes, winneronly = T)
   irv(votes, winneronly = T)
   condorcet(votes, winneronly = T)
   shulze(votes, winneronly = T)
   rpairs(votes, winneronly = T)
   kyoung(votes, winneronly = T)
   borda(votes, winneronly = T)
   
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
 
 
 
# End script
 