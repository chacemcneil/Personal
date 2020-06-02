# Explore multinomial logit 
 library(mlogit)
 
 
 n <- 200
 a <- 1
 b <- 1.5
 c <- 2.5
 N <- 3e2
 
 coefs <- NULL
 for (i in 1:N) {
   cat(paste0("\rIteration: ", i))
   dt <- data.table(ID = 1:n, Numchoices = rpois(n, 15))
   dt2 <- dt[, list(Choice = 1:Numchoices, x1 = rnorm(Numchoices, 5, 2), x2 = rnorm(Numchoices, 10, 1)), by = list(ID, Numchoices)]
   dt2[, Y := rmultinom(1, 1, exp(a + b*x1 + c*x2)/sum(exp(a + b*x1 + c*x2))), by = ID]
   
   mdat <- mlogit.data(dt2, shape = "long", choice = "Y", alt.var = "Choice", chid.var = "ID")
   
   mod <- mlogit(Y ~ x1 + x2 | 0 | 0, data = mdat)
   summary(mod)
   coefs <- rbind(coefs, coefficients(mod))
 }
 
 
 preds <- predict(mod, newdata = mlogit.data(dt2, shape = "long", choice = "Y", alt.var = "Choice", chid.var = "ID"))
 dim(preds)
 preds <- ifelse(preds==0, NA, preds)
 preds <- as.vector(t(preds))
 preds <- preds[!is.na(preds)]
 dt2[, Preds := preds]
 
 head(dt2)
 
 dt2[, Value := x1 * coefs[1] + x2 * coefs[2]][1:12]
 dt2[, Prob := exp(Value)/sum(exp(Value)), by = ID][1:12]
 dt2[, hist(Prob - Preds)]
 
 mpredict <- function(model, newdata, id.var) {
   newdata <- as.data.frame(newdata)
   vals <- as.matrix(newdata[, names(coef(model))]) %*% coef(model)
   probs <- tapply(vals, newdata[,id.var], function(x) exp(x)/sum(exp(x)))
   unlist(probs)
 }
 dt2[, Prob := mpredict(mod, .SD, "ID")]
 
 
 # Make an easier way to make model
 n <- 300
 chosen <- data.table(ID = 1:n, Choice = rpois(n, 5) + 1, Y = 1)
 dat <- dt[, list(x1 = sum(...), x2 = max(...), ...), by = list(ID, Choice)]  # this likely has to be done manually each time
 dat <- merge(dat, chosen, by = c("ID", "Choice"), all.x = T)
 dat[, Y := ifelse(is.na(Y), 0, Y)]
 mdat <- mlogit.data(dat, shape = "long", choice = "Y", alt.var = "Choice", chid.var = "ID")
 mod <- mlogit(formula, data = mdat)
 
 
 
 
 
# End script
 