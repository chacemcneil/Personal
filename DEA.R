# Exploring DEA (Frontier Analysis) models using the 'rDEA' and 'Benchmarking' packages
 library(boot)
 library(Benchmarking)
 library(rDEA)
 
 # Both packages have a dea() function specified differently. Create new functions for clarity:
 rdea <- rDEA::dea
 bdea <- Benchmarking::dea
 
 
 ## Sample data from 'rDEA' package
 data("hospitals", package="rDEA")
 
 ## inputs and outputs for analysis
 Y = hospitals[c('inpatients')]
 Y = hospitals[c('inpatients', 'outpatients')]
 X = hospitals[c('labor')]
 X = hospitals[c('labor', 'capital')]
 W = hospitals[c('labor_price', 'capital_price')]
 
 ## Check for differences in models
 rmod <- rdea(XREF=X, YREF=Y, X=X, Y=Y, model="input", RTS="variable")
 bmod <- bdea(X=X, Y=Y, ORIENTATION="in", RTS="vrs")
 # XREF, YREF are optional in Benchmarking.
 
 # Efficiency is the same (if ORIENTATION = "out" and model= "output", then thetaOpt = 1/eff)
 hist(rmod$thetaOpt - bmod$eff)
 # Linear combination of efficient observations, also the same
 hist(bmod$lambda - rmod$lambda)
 
 ## Input options
 # Benchmarking has more options for the RTS parameter. Change from 0 to 7
 x <- matrix(c(100,200,300,500,100,200,600),ncol=1)
 y <- matrix(c(75,100,300,400,25,50,400),ncol=1)
 dea.plot.frontier(x, y, RTS = 1, txt = T)
 
 ## Plotting
 # Benchmarking has dea.plot.frontier()
 # rDEA does not have plotting functionality.
 
 firms=1:30
 # Efficiency values calculated for 'input' and 'output' are the same. Changing RTS changes efficiency values.
 # XREF and YREF used to find frontier, efficiency only calculated for observations in X and Y.
 rmod1 = rdea(XREF=X, YREF=Y, X=X[firms,], Y=Y[firms,], model="input", RTS="constant")
 rmod2 = rdea(XREF=X, YREF=Y, X=X[firms,], Y=Y[firms,], model="output", RTS="constant")
 cbind(rmod1$thetaOpt, rmod2$thetaOpt)
 rmod3 = rdea(XREF=X, YREF=Y, X=X, Y=Y, model="input", RTS="variable")
 rmod4 = rdea(XREF=X, YREF=Y, X=X, Y=Y, model="output", RTS="variable")
 cbind(rmod3$thetaOpt, rmod4$thetaOpt)
 # Using rDEA, the output (thetaOpt) is the same when RTS = "constant"
 
 rmod2_neg = rdea(XREF=-Y, YREF=-X, X=-Y[firms,], Y=-X[firms,], model="output", RTS="constant") # Not similar!
 cbind(rmod2$thetaOpt, rmod2_neg$thetaOpt)
 plot(.Last.value)
 # Negative output values does not reverse efficiency values!
 
 ## Compare dea() to basic linear programming optimization algorithm.
 # Efficiency values can be calculated manually using simplex() from the 'boot' package. (Basic linear programming solution)
 eff <- eff2 <- numeric(0)
 for (obs in 1:30) {
   smod <- simplex(a = c(unlist(Y[obs,]), rep(0, ncol(X))), max = T,
                   A1 = as.matrix(cbind(Y, -X)), b1 = rep(0, nrow(X)), 
                   A3 = as.matrix(cbind(0*Y, X))[obs,], b3 = 1)
   eff[obs] <- c(unlist(Y[obs,]), rep(0, ncol(X))) %*% smod$soln
   smod <- simplex(a = c(rep(0, ncol(Y)), unlist(X[obs,])), max = F,
                   A1 = as.matrix(cbind(Y, -X)), b1 = rep(0, nrow(X)), 
                   A3 = as.matrix(cbind(Y, 0*X))[obs,], b3 = 1)
   eff2[obs] <- 1/ (c(rep(0, ncol(Y)), unlist(X[obs,])) %*% smod$soln)
 }
 # Can maximize output or minimize input, the resulting efficiency is again the same.
 eff
 eff2
 rmod1$thetaOpt
 # Values are only the same when using RTS = "constant" in dea()!
 
 
 ### Interpretation of efficiency
 Y = hospitals[c('inpatients')]
 Y = hospitals[c('inpatients', 'outpatients')]
 X = hospitals[c('labor')]
 X = hospitals[c('labor', 'capital')]
 
 bmod <- bdea(X=X, Y=Y, ORIENTATION="in", RTS="vrs")
 ii <- which(bmod$eff == 1)
 # Duplicate efficient observations, scale inputs
 X_add <- rbind(X, X[ii, , drop = F]*seq_along(ii))
 Y_add <- rbind(Y, Y[ii, , drop = F])
 # Rerun model
 bmod_add <- bdea(X=X_add, Y=Y_add, ORIENTATION="in", RTS="vrs")
 # Efficiency is inversely related to scale of input
 tail(bmod_add$eff, length(ii))
 tail(bmod_add$eff, length(ii)) * seq_along(ii)
 
 # If output is scaled instead, the efficiency values are not scaled (unless ORIENTATION = "out")
 # Efficiency * input (or output) gives the potential input given the same output (or input)
 
 # If only one part of the input is scaled the effect is different (unpredictable)
 X_add <- rbind(X, cbind(X[ii, 1, drop = F]*seq_along(ii), X[ii, 2, drop = F]))
 Y_add <- rbind(Y, Y[ii, , drop = F])
 # Rerun model
 bmod_add <- bdea(X=X_add, Y=Y_add, ORIENTATION="in", RTS="vrs")
 # Efficiency is no longer related to scale of input
 tail(cbind(X_add, Y_add, bmod_add$eff), length(ii))
 cbind(tail(cbind(X_add, Y_add, bmod_add$eff), length(ii)), X[ii,], Y[ii,], bmod_add$eff[ii])
 
 # How do multiple inputs effect each other? Does rescaling change efficiency scores (No)?
 X2 <- cbind(X[,1,drop = F], X[,2,drop = F]*runif(1)*10)
 X2 <- cbind(X[,1,drop = F], X[,2,drop = F])
 bmod1 <- bdea(X=X, Y=Y, ORIENTATION="in", RTS=1)
 bmod2 <- bdea(X=X2, Y=Y, ORIENTATION="in", RTS=1)
 hist(bmod1$eff - bmod2$eff)
 
 
 ## How do you interpret efficiencies when there are multiple inputs (or outputs)
 
 
 ## How do we deal with categorical, dichotomous inputs/outputs ?!?!
 
 
 
 
 
# End script
 