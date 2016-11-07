## Calculations for prediction error

#  RMSE

rmse <- function(mod, data = NULL) {
  if(is.null(data))
    data <- mod$model
  actual <- eval(mod$call$formula[[2]], envir = data)
  predicted <- predict(mod, newdata = data)
  
  sqrt(mean((actual - predicted)^2))
}

mae <- function(mod, data = NULL) {
  if(is.null(data))
    data <- mod$model
  actual <- eval(mod$call$formula[[2]], envir = data)
  predicted <- predict(mod, newdata = data)
  
  mean(abs(actual - predicted))
}

mape <- function(mod, data = NULL) {
  if(is.null(data))
    data <- mod$model
  actual <- eval(mod$call$formula[[2]], envir = data)
  predicted <- predict(mod, newdata = data)
  
  mean(abs(actual - predicted)/actual)
}

wmape <- function(mod, data = NULL) {
  if(is.null(data))
    data <- mod$model
  actual <- eval(mod$call$formula[[2]], envir = data)
  predicted <- predict(mod, newdata = data)
  
  sum(abs(actual - predicted)) / sum(actual)
}

if (F) {
  
  dat <- data.table(x1 = 1:10, x2 = rnorm(10) + 10, x3 = 1:10 + rnorm(10,.01))[, y := 1.5 + 1.3 * x1 + rnorm(10)]
  mod <- lm(y~x1, data = dat)
  mod <- lm(y~x2, data = dat)
  mod <- lm(y~x3, data = dat)
  rmse(mod); mae(mod); mape(mod); wmape(mod)
  
  mods <- list(mod1, mod2, mod3)
}

# paste into other code.
rbind(data.table(),)

FUNs <- list(RMSE = rmse, MAE = mae, WAPE = mape, wMAPE = wmape)

do.call(rbind, lapply(mods, function(m) do.call(data.table, lapply(FUNs, function(fn) fn(m)))))
mapply(function(fn, m) fn(m), FUNs, mods)


