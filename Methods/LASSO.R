## Regularization using ridge or LASSO
 library(glmnet)
 
 ### Regularization with random data
 
 ## Create objects for model using random data
 set.seed(541534)
 X <- matrix(rnorm(100*50), 100, 50)
 y <- rnorm(100)
 y <- rnorm(100, 2*X[, 31])
 
 
 # Fit LASSO regression model (default)
 mod_lasso <- cv.glmnet(X, y)
 
 # Fit ridge regression model
 mod_ridge <- cv.glmnet(X, y, alpha = 0)
 
 # Fit blended model
 mod_blend <- cv.glmnet(X, y, alpha = .2)
 
 ## Plot coefficients relatve to different lambda values
 plot(mod_lasso)
 
 coef(mod_lasso)
 
 ## Perform cross-validation on model
 mod_cv <- cv.glmnet(X, y, nlambda = 100, maxit = 1000, type.measure = "mse")
 
 ## Plot change in MSE
 plot(mod_cv)
 
 ## Find specific lambda values
 mod_cv$lambda.min
 mod_cv$lambda.1se
 
 ## Coefficients associated with selected lambda (mod_cv$lambda.1se)
 coef(mod_cv)
 
 
 
 
# End script
 