# Simulate preferential voting
 library(data.table)
 library(gtools)
 
 # 10 takes a long time, more is seemingly never-ending.
 n <- 11
 
 dim(dt <- data.table(gtools::permutations(n, n)))
 setnames(dt, paste0("Pref", 1:ncol(dt)))
 
 dt[, Votes := rmultinom(1, 1e9, rnorm(.N, 20, 2))]
 
 dt[, list(CumVotes = sum(Votes)), by = Pref1][order(CumVotes)]
 dt[, list(CumVotes = sum(Votes)), by = ifelse(Pref1 == 9, Pref2, Pref1)][order(CumVotes)]
 dt[, list(CumVotes = sum(Votes)), by = ifelse(Pref1 %in% c(5,9), ifelse(Pref2 %in% c(5,9), Pref3, Pref2), Pref1)][order(CumVotes)]
 
 
 preference <- function(dt) {
   dt <- data.table(dt)
   elim <- numeric()
   cand <- as.numeric(gsub("Pref", "", grep("Pref", colnames(dt), value = T)))
   m <- max(cand)
   winner <- 0
   switch <- function(dat, i, rem) {
     tmp <- dat[, get(paste0("Pref", i))]
     dat[, paste0("Pref", i) := ifelse(tmp %in% rem, get(paste0("Pref", i+1)), tmp)]
     dat[, paste0("Pref", i+1) := ifelse(tmp %in% rem, tmp, get(paste0("Pref", i+1)))]
     dat
   }
   while(!winner) {
     cat(paste0("\rRound: ", length(elim) + 1))
     res <- dt[, list(SumVotes = sum(Votes)), by = Pref1]
     if(any(prop.table(res$SumVotes) > .5))
       winner <- res[which(prop.table(SumVotes) > .5), Pref1]
     else {
       elim <- c(elim, res[which.min(SumVotes), Pref1])
       for (i in 1:(m-length(elim)))
         dt <- switch(dt, i, elim)
         # dt[, paste0("Pref", i) := ifelse(get(paste0("Pref", i)) %in% elim, get(paste0("Pref", i+1)), get(paste0("Pref", i)))]
       dt[, paste0("Pref", m-length(elim) + 1) := NULL]
       # dt <- cbind(t(apply(dt[, -which(colnames(dt) == "Votes"), with = F], 1, setdiff, y = elim)), dt$Votes)
     }
   }
   c(winner, rev(elim))
 }
 
 
 result <- preference(dt)
 