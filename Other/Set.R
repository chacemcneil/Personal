library(data.table)
library(ggplot2)
library(scales)

check <- function(submat, levels) {
  good = T
  for(i in 1:ncol(submat)) {
    vec <- submat[[i]]
    if(!uniqueN(vec) %in% c(1, levels)) {
      good = F
      break
    }
  }
  good
}


N <- 1e2
cards <- 12
traits <- 4
levels <- 4

poss_sets <- data.table(do.call(expand.grid, lapply(1:levels, function(x) 1:cards)))
for(i in 2:levels) {
  poss_sets <- poss_sets[poss_sets[[i-1]] < poss_sets[[i]],]
}

res <- rep(F, N)
for (i in 1:N) {
  cat("\rIteration: ", i)
  samp <- data.table(matrix(sample(1:levels, traits*cards, replace = T), cards, traits))
  setnames(samp, letters[1:levels])
  for (j in 1:nrow(poss_sets)) {
    if(check(samp[unlist(poss_sets[j,]),], levels)) {
      res[i] = T
      break
    }
  }
}

get_prob <- function(traits = 4, levels = 3, cards = 12, N = 100) {
  poss_sets <- data.table(do.call(expand.grid, lapply(1:levels, function(x) 1:cards)))
  for(i in 2:levels) {
    poss_sets <- poss_sets[poss_sets[[i-1]] < poss_sets[[i]],]
  }
  
  res <- rep(F, N)
  for (i in 1:N) {
    ## Print stats
    if(i %% 10 == 0) {
      cur <- res[1:(i-1)]
      mc <- mean(cur)
      sc <- 1.96*sqrt(mc*(1-mc)/(i-1))
      cat("\rIterations:", i, " \tMean:", sprintf("%.2f", mc), " \tCI: (",  sprintf("%.2f", mc - sc), ", ", sprintf("%.2f", mc + sc), ")     ")
    }
    else {
      cat("\rIterations:", i)
    }
    
    samp <- data.table(matrix(sample(1:levels, traits*cards, replace = T), cards, traits))
    setnames(samp, letters[1:traits])
    for (j in 1:nrow(poss_sets)) {
      if(check(samp[unlist(poss_sets[j,]),], levels)) {
        res[i] = T
        break
      }
    }
  }
  res
}

plot_results <- function(res) {
  results <- data.table(Iter = 1:length(res), Result = res)
  results[, Mean := cumsum(Result)/Iter]
  results[, STD := 1.96*sqrt(Mean * (1-Mean) / Iter)]
  results[, Low := pmin(pmax(Mean - STD, 0), 1)]
  results[, High := pmin(pmax(Mean + STD, 0), 1)]
  
  print(
    ggplot(results) +
      geom_line(aes(Iter, Mean)) +
      geom_line(aes(Iter, Low), linetype = 2) +
      geom_line(aes(Iter, High), linetype = 2) +
      geom_polygon(data = rbind(results[, .(Iter, CI = Low)],
                                results[order(-Iter), .(Iter = Iter, CI = High)]),
                   aes(Iter, CI), fill = "salmon", alpha = .3) +
      labs(x = "Iteration", y = "Estimate") +
      scale_y_continuous(labels = percent, breaks = (0:5/5), limits = 0:1)
  )
}

res44_12 <- get_prob(4, 4, 12, 200) ## 
res44_20 <- get_prob(4, 4, 20, 200) ## 0.45 (.39, .52)
res34_12 <- get_prob(3, 4, 12, 200) ## 0.38 (.31, .45)
res34_16 <- get_prob(3, 4, 16, 200) ## 
res53_12 <- get_prob(5, 3, 12, 500) ## 0.61 (0.56, 0.65)
res53_15 <- get_prob(5, 3, 15, 500) ## 0.83 (0.80, 0.87)
res53_15 <- get_prob(5, 3, 15, 200) ## 0.82 (0.77, 0.88)

plot_results(res)
plot_results(res53_12)
table(res); mean(res)

