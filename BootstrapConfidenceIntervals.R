# Bootstrapping prediction intervals
 library(data.table)
 
 set.seed(945095)
 train <- sample(1:1e3, 800)
 dt <- data.table(x = rnorm(1e3))[, y := rnorm(1e3, 1.5*x + 2)][train, Train := 1][, Train := ifelse(is.na(Train), 0, Train)]
 
 tst <- dt[Train == 0]
 tst[, Index := 1:200]
 
 modfull <- lm(y~x, data = dt[Train == 1])
 
 plot(dt$x, dt$y)
 
 xseq <- seq(min(dt$x), max(dt$x), .05)
 
 preds1 <- preds2 <- NULL
 for (i in 1:1e3) {
   cat("\rIteration: ", i)
   ii <- sample(1:800, 800, replace = T)
   mod <- lm(y~x, data = dt[Train == 1][ii])
   pred <- predict(mod, newdata = tst, interval = "prediction")
   preds1 <- rbind(preds1, data.table(Index = 1:200, data.table(pred), Iter = i))
   pred <- predict(mod, newdata = data.table(x = xseq), interval = "prediction")
   preds2 <- rbind(preds2, data.table(Index = 1:length(xseq), x = xseq, data.table(pred), Iter = i))
 }
 dim(preds1)
 dim(preds2)
 
 preds <- merge(preds, tst, by = "Index")
 preds[, InPredInt := ifelse(y > lwr & y < upr, 1, 0)]
 preds[, list(Coverage = mean(InPredInt)), by = Index]
 preds[, list(Coverage = mean(InPredInt))]
 
 preds[, list(Mean = mean(Predicted), LowCI = quantile(Predicted, 0.025), HighCI = quantile(Predicted, 0.975)), by = Index]
 this <- merge(.Last.value, tst, by = "Index")
 this[, mean(y < HighCI & y > LowCI)]
 
 
 preds[Index == 1, hist(lwr)]
 preds[Index == 1, list(mean(lwr), mean (upr))]
 preds[, list(Low = ), by = Index]
 
 predict(modfull, newdata = dt[Train == 0][1], interval = "prediction")
 
 
 lmints <- data.table(Index = 1:length(xseq), x = xseq, Type = "lm", predict(modfull, newdata = data.table(x = xseq), interval = "prediction"))
 setnames(lmints, c("lwr", "upr"), c("LowPI", "HighPI"))
 lmincs <- lmints[, list(x, Type, LowPI, HighPI, Sample = rnorm(1e4, 1.5*x + 2)), by = Index][, list(Inclusion = mean(Sample > LowPI & Sample < HighPI)), by = list(Index, x, Type)]
 ggplot(lmincs, aes(x, Inclusion)) + geom_line() + scale_y_continuous(limits = c(0.9, 1))
 
 bsints <- preds2[, list(Type = "bs", LowPI = mean(lwr), HighPI = mean(upr)), by = list(Index, x)]
 bsincs <- bsints[, list(x, Type, LowPI, HighPI, Sample = rnorm(1e4, 1.5*x + 2)), by = Index][, list(Inclusion = mean(Sample > LowPI & Sample < HighPI)), by = list(Index, x, Type)]
 ggplot(bsincs, aes(x, Inclusion)) + geom_line() + scale_y_continuous(limits = c(0.9, 1))
 
 ggplot(rbind(bsincs, lmincs), aes(x, Inclusion, col = Type)) + geom_line() + scale_y_continuous(limits = c(0.9, 1))
 
 
 preds2[, (1.5*x + 2 > quantile(fit, 0.025) & 1.5*x + 2 < quantile(fit, 0.975)), by = list(Index, x)]
 
 
 
 
# End script
 