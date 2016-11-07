## Functions for calculating error of a predictive model
 library(data.table)
 
 ## Calculations for prediction error
 
 #  Root Mean Square Error
 rmse <- function(mod, data = NULL, na.rm = T) {
   if(is.null(data))
     data <- mod$model
   formula <- formula(mod)
   data <- data.table(data)[!is.na(eval(formula[[2]]))]
   actual <- eval(formula[[2]], envir = data)
   predicted <- predict(mod, newdata = data)
   
   sqrt(mean((actual - predicted)^2, na.rm = na.rm))
 }
 
 #  Mean Absolute Error
 mae <- function(mod, data = NULL) {
   if(is.null(data))
     data <- mod$model
   formula <- formula(mod)
   data <- data.table(data)[!is.na(eval(formula[[2]]))]
   actual <- eval(formula[[2]], envir = data)
   predicted <- predict(mod, newdata = data)
   
   mean(abs(actual - predicted))
 }
 
 #  Mean Absolute Percentage Error
 mape <- function(mod, data = NULL) {
   if(is.null(data))
     data <- mod$model
   formula <- formula(mod)
   data <- data.table(data)[!is.na(eval(formula[[2]]))]
   actual <- eval(formula[[2]], envir = data)
   predicted <- predict(mod, newdata = data)
   
   mean(abs(actual - predicted)/actual)
 }
 
 #  Weighted Mean Absolute Percentage Error
 wmape <- function(mod, data = NULL) {
   if(is.null(data))
     data <- mod$model
   formula <- formula(mod)
   data <- data.table(data)[!is.na(eval(formula[[2]]))]
   actual <- eval(formula[[2]], envir = data)
   predicted <- predict(mod, newdata = data)
   
   sum(abs(actual - predicted)) / sum(actual)
 }
 
 calcR <- function(mod, data = NULL) {
   if(is.null(data))
     data <- mod$model
   formula <- formula(mod)
   data <- data.table(data)[!is.na(eval(formula[[2]]))]
   actual <- eval(formula[[2]], envir = data)
   predicted <- predict(mod, newdata = data)
   
   cor(actual, predicted)
 }
 
 calcR2 <- function(mod, data = NULL) {
   if(is.null(data))
     data <- mod$model
   formula <- formula(mod)
   data <- data.table(data)[!is.na(eval(formula[[2]]))]
   actual <- eval(formula[[2]], envir = data)
   predicted <- predict(mod, newdata = data)
   
   cor(actual, predicted)^2
 }
 
 err.funs <- list(RMSE = rmse, MAE = mae, MAPE = mape, wMAPE = wmape)
 
 if (F) {
   
   dat <- data.table(x1 = 1:10, x2 = rnorm(10) + 10, x3 = 1:10 + rnorm(10,.01))[, y := 1.5 + 1.3 * x1 + rnorm(10)]
   mod1 <- lm(y~x1, data = dat)
   mod2 <- lm(y~x2, data = dat)
   mod3 <- lm(y~x3, data = dat)
   
   mods <- list(mod1, mod2, mod3)
   sapply(mods,function(m) c(rmse(mod), mae(mod), mape(mod), wmape(mod)))
   
   
   # paste into other code.
   rbind(data.table(),)
   
   FUNs <- list(RMSE = rmse, MAE = mae, WAPE = mape, wMAPE = wmape)
   
   do.call(rbind, lapply(mods, function(m) do.call(data.table, lapply(FUNs, function(fn) fn(m)))))
   #mapply(function(fn, m) fn(m), FUNs, mods)
 }
 
 
 
# End script
 