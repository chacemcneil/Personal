# Hidden Markov Model (HMM) experimentation
 library(HMM)
 library(HiddenMarkov)
 
 
 ### Generate HMM with multinomial probabilities
 states <- c("A","B","C","D")
 Pi <- t(prop.table(rmultinom(4,4,rep(1,4)),2)) # 16 parameters
 EvPi <- matrix(.25,4,4)
 
 Dr <- prop.table(rmultinom(4,24,rep(1,6)),2)   # 24 parameters
 n <- 2000
 
 lat <- 1
 obs <- which(rmultinom(1,1,Dr[,lat[1]])==1)
 for(i in 2:n) {
   lat[i] <- which(rmultinom(1,1,Pi[lat[i-1],])==1)
   obs[i] <- which(rmultinom(1,1,Dr[,lat[i]])==1)
 }
 
 
 
 ind <- which(lat==4)
 table(lat[ind+1])
 Pi
 table(obs[ind])
 Dr
 
 
 
 
 
 means <- rnorm(4,10,5)
 sds   <- rnorm(4,1,.1)
 lat <- 1
 obs <- rnorm(1,means[lat[1]],sds[lat[1]])
 for(i in 2:n) {
   lat[i] <- which(rmultinom(1,1,Pi[lat[i-1],])==1)
   obs[i] <- rnorm(1,means[lat[i]],sds[lat[i]])
 }
 
 
 hmm <- dthmm(obs,Pi,delta=Pi[1,],distn="norm",pm=list(mean=(ms <- rnorm(4,10,3)),sd=(ss <- rnorm(4,.5,.1)) ) )
 
 hmm2 <- BaumWelch(simulate(hmm, nsim = 2500), control = bwcontrol(maxiter = 10000, tol = 1e-6))
 hmm2$Pi
 hmm2$delta
 summary(hmm2)
 
 
 # Would a Gibbs sampler work?
 L <- Pi_i^n/sqrt(2*pi*sig_i^2)*exp(-1/2/sig_i^2*(x_i-theta_i)^2)
 
 
 
 
 
 
 
# End script
 