
##-----------------------------------------
##-----------------------------------------
##-----------------------------------------
##	Monte Carlo simulation of power for RCT of CGM vs POC in cardiac surgery patients
##	PI: Mark Ott, sponsered by Savvysherpa
##
##	Updated on 2019-06-13 to account for multiple testing per Michael Lahm's 
##
##-----------------------------------------
##	hyperparameters
mc_iters <- 1e5
cores <- 4
paste0("Monte Carlo iterations: ", mc_iters)

##-----------------------------------------
##	loads packages
pkgs <- c("lme4","foreach","Matrix","lmerTest","doParallel","mgcv","gamlss")
lapply(pkgs,require,character.only=TRUE)

# ##-----------------------------------------
# ##	loads data
# directory <- "\\\\co.ihc.com\\UCR\\IM\\User\\jssorens\\Principal Investigators\\Mark Ott\\CV study of CGM and FitBit\\My work\\Power\\Athena\\"
# dfSteps <- read.csv(paste0(directory, "In\\aggregated_daily_steps_august.csv"),header=TRUE,stringsAsFactors=FALSE)
# dfSleep <- read.csv(paste0(directory, "In\\sleep_summary_daily_august.csv"),header=TRUE,stringsAsFactors=FALSE)

# ##-----------------------------------------
# ##	wrangles data
# aggregate(dfSteps[,c("savvy_id", "steps")], by=list(dfSteps$savvy_id), mean)
# aggregate(dfSteps[,c("savvy_id", "steps")], by=list(dfSteps$savvy_id), sd)
# ##	steps data
# m1 <- lmer(steps/1000 ~ 1 + (1|savvy_id), data=dfSteps)
# # m1 <- glmer(steps/1000 ~1+(1|savvy_id),family=inverse.gaussian(1/mu^2),data=dfSteps)
# mu_steps_ <- as.numeric(fixef(m1)); mu_steps_ # 6.615959
# var_steps_ <- data.frame(VarCorr(m1))[,"vcov"]; var_steps_	# 11.935821 3.053719
# icc <- var_steps_[1] / sum(var_steps_); icc # icc=var2/(var2+var1) ==> var2=icc*var1/(1-icc) # 0.7962767
# ##	sleep data
# sleepHrs <- with(subset(dfSleep, savvy_id==13174), minutes_asleep/60)
# mu_sleep_ <- mean(sleepHrs); mu_sleep_ # 5.63125
# residual_var_sleep <- var(sleepHrs); residual_var_sleep # 2.35793
# var_sleep_ <- c(icc*residual_var_sleep/(1-icc), residual_var_sleep); var_sleep_ # 9.216248 2.357930

##------------------------------------------
##	sets values in case baseline data are unavailable
mu_steps_ <- 6.616
var_steps_ <- c(11.934, 3.0437)
mu_sleep_ <- 5.6313
var_sleep_ <- c(9.216248, 2.35793)

##-----------------------------------------
##	function for MC sim
MCsim <- function(mu_steps=mu_steps_, mu_sleep=mu_sleep_, var_steps=var_steps_, var_sleep=var_sleep_){
  require(lme4)
  require(lmerTest)
  ##	Monte Carlo population values
  N <- sample(50:350, size=1); N
  days <- 7 # sample(7:30, size=1); 
  x <- rbinom(N, 1, .5) # patient-level indicator of treatment
  z <- rep(x, each=days) # longitudinal, day-level indicator of treatment
  id <- rep(1:N, each=days)
  ##	generates vector of correlated fractional effects
  rho <- runif(1) # correlation between fractional effects
  mu_theta <- qnorm(.2) # mean fractional effect on steps (theta_steps)
  steps_factor <- rnorm(1,mu_theta,.4)
  sleep_factor <- rnorm(1,mu_theta + rho*(steps_factor - mu_theta),sqrt((1-rho^2)*.4^2)) # # correlated sample from normal distribution
  theta <- pnorm(c(steps_factor, sleep_factor)) # transforms to marginal distribution: (-inf,+inf) --> (0,1)
  theta_steps <- theta[1] # fractional increase in steps
  theta_sleep <- theta[2]	# fractional increase in sleep
  ##	random effects 
  alpha_steps <- rnorm(N, 0, sqrt(var_steps[1])) # patient-level random intercepts
  alpha_sleep <- rnorm(N, 0, sqrt(var_sleep[1])) # patient-level random intercepts
  zeta_steps <- (mu_steps+alpha_steps)*(1 + x*theta_steps) # patient-level mean steps
  zeta_sleep <- (mu_sleep+alpha_sleep)*(1 + x*theta_sleep) # patient-level mean sleep
  nu_steps <- rep(zeta_steps, each=days)	# observation-level mean steps
  nu_sleep <- rep(zeta_sleep, each=days)	# observation-level mean sleep
  y_steps <- rnorm(nu_steps, mean=nu_steps, sd=sqrt(var_steps[2])) # observed steps
  y_sleep <- rnorm(nu_sleep, mean=nu_sleep, sd=sqrt(var_sleep[2])) # observed sleep
  y_steps <- ifelse(y_steps<0, 0, y_steps)
  y_sleep <- ifelse(y_sleep<0, 0, y_sleep)
  # plot(nu_steps, y_steps, col=ifelse(rep(x, each=days)==1, "forestgreen", "grey50")); lines(0:1e5, 0:1e5, lty=2, lwd=1, col="grey80")
  ## 	2-level hierarchical model
  coef_steps <- summary(lmer(y_steps ~ 1 + z + (1|id)))$coefficients
  coef_sleep <- summary(lmer(y_sleep ~ 1 + z + (1|id)))$coefficients
  muhat_steps <- coef_steps[1,1]
  muhat_sleep <- coef_sleep[1,1]
  betahat_steps <- coef_steps[2,1]
  betahat_sleep <- coef_sleep[2,1]
  ##	p-values
  p_steps <- coef_steps[2,5] 
  p_sleep <- coef_sleep[2,5]
  ##	control FWER
  p_bonf <- p.adjust(c(p_steps, p_sleep),method="bonferroni")
  p_holm <- p.adjust(c(p_steps, p_sleep),method="holm")
  ##	control FDR
  p_fdr <- p.adjust(c(p_steps, p_sleep),method="fdr")
  
  return(data.frame(N, rho, days, mu_steps, mu_sleep, muhat_steps, muhat_sleep, theta_steps, theta_sleep, betahat_steps, betahat_sleep, 
                    p_bonf_steps=p_bonf[1], p_bonf_sleep=p_bonf[2], p_holm_steps=p_holm[1], p_holm_sleep=p_holm[2], p_fdr_steps=p_fdr[1], p_fdr_sleep=p_fdr[2]))
}
MCsim()
##-----------------------------------------
##	power of CGM on Activity
t0 <- Sys.time(); t0
cl <- makeCluster(cores)
registerDoParallel(cl)
MCout <- foreach(i=seq_len(mc_iters), .combine=rbind, .errorhandling="remove") %dopar% MCsim()
stopCluster(cl)
difftime(Sys.time(), t0)
head(MCout); tail(MCout)
##	writes to .csv
write.csv(MCout, paste0(directory,"Out\\Power_CGM_on_Activity_", as.Date(Sys.time(), tz="MST"), ".csv"), na="")
##	reads from .csv
# MCout <- read.csv(paste0(directory,"Out\\","Power_CGM_on_Activity_2019-06-15.csv"),header=TRUE,stringsAsFactors=FALSE); dim(MCout)

##-----------------------------------------
##	visualizes power surface

##	processes MC output  ... Should these have .05 instead of .5?
MCout2 <- within(MCout,{
  sig_bonf_steps <- ifelse(p_bonf_steps <.5, 1, 0)
  sig_bonf_sleep <- ifelse(p_bonf_sleep <.5, 1, 0)
  sig_holm_steps <- ifelse(p_holm_steps <.5, 1, 0)
  sig_holm_sleep <- ifelse(p_holm_sleep <.5, 1, 0)
  sig_fdr_steps <- ifelse(p_fdr_steps <.5, 1, 0)
  sig_fdr_sleep <- ifelse(p_fdr_sleep <.5, 1, 0)
})
MCout2 <- subset(MCout2, theta_sleep <.3 & theta_steps <.3 & theta_sleep >.1 & theta_steps >.1); dim(MCout2)
## models power surface per GAMLSS
g2Steps_bonf <- gamlss(sig_bonf_steps ~ pvc(N)+pvc(theta_steps)+pvc(N,theta_steps)+pvc(rho)+pvc(theta_steps,rho), sigma.fo=~ pvc(N)+pvc(theta_steps)+pvc(N,theta_steps)+pvc(rho)+pvc(theta_steps,rho), data=MCout2, family=BB, control=gamlss.control(n.cyc=200))
g2Steps_holm <- gamlss(sig_holm_steps ~ pvc(N)+pvc(theta_steps)+pvc(N,theta_steps)+pvc(rho)+pvc(theta_steps,rho), sigma.fo=~ pvc(N)+pvc(theta_steps)+pvc(N,theta_steps)+pvc(rho)+pvc(theta_steps,rho), data=MCout2, family=BB, control=gamlss.control(n.cyc=200))
g2Steps_fdr <- gamlss(sig_fdr_steps ~ pvc(N)+pvc(theta_steps)+pvc(N,theta_steps)+pvc(rho)+pvc(theta_steps,rho), sigma.fo=~ pvc(N)+pvc(theta_steps)+pvc(N,theta_steps)+pvc(rho)+pvc(theta_steps,rho), data=MCout2, family=BB, control=gamlss.control(n.cyc=200))
g2Sleep_bonf <- gamlss(sig_bonf_sleep ~ pvc(N)+pvc(theta_sleep)+pvc(N,theta_sleep)+pvc(rho)+pvc(theta_sleep,rho), sigma.fo=~ pvc(N)+pvc(theta_steps)+pvc(N,theta_steps)+pvc(rho)+pvc(theta_steps,rho), data=MCout2, family=BB, control=gamlss.control(n.cyc=200))
g2Sleep_holm <- gamlss(sig_holm_sleep ~ pvc(N)+pvc(theta_sleep)+pvc(N,theta_sleep)+pvc(rho)+pvc(theta_sleep,rho), sigma.fo=~ pvc(N)+pvc(theta_steps)+pvc(N,theta_steps)+pvc(rho)+pvc(theta_steps,rho), data=MCout2, family=BB, control=gamlss.control(n.cyc=200))
g2Sleep_fdr <- gamlss(sig_fdr_sleep ~ pvc(N)+pvc(theta_sleep)+pvc(N,theta_sleep)+pvc(rho)+pvc(theta_sleep,rho), sigma.fo=~ pvc(N)+pvc(theta_steps)+pvc(N,theta_steps)+pvc(rho)+pvc(theta_steps,rho), data=MCout2, family=BB, control=gamlss.control(n.cyc=200))

##	assess fit of GAMLSS
par(mfrow=c(2,3))
wp(g2Steps_bonf, ylim.all=2)
wp(g2Steps_holm, ylim.all=2)
wp(g2Steps_fdr, ylim.all=2)
wp(g2Sleep_bonf, ylim.all=2)
wp(g2Sleep_holm, ylim.all=2)
wp(g2Sleep_fdr, ylim.all=2)

##	writes functions to generate input for contour plot of GAMLSS surface
contourMatrixSteps <- function(model, df=MCout2, correlation=.1,res=40){
  X <- with(df, expand.grid(N=seq(min(N),max(N),length.out=res),theta_steps=seq(min(theta_steps),max(theta_steps),length.out=res),rho=correlation))
  X$y <- predict(model,newdata=X,type="response")
  Z <- as.matrix(reshape(data=X[,c("N","theta_steps","y")], idvar="N",timevar="theta_steps",direction="wide")[,-1])
  L <- list(x=unique(X$N),y=unique(X$theta_steps),z=Z)
  return(L)
}
contourMatrixSleep <- function(model, df=MCout2, correlation=.1,res=40){
  X <- with(df, expand.grid(N=seq(min(N),max(N),length.out=res),theta_sleep=seq(min(theta_sleep),max(theta_sleep),length.out=res),rho=correlation))
  X$y <- predict(model,newdata=X,type="response")
  Z <- as.matrix(reshape(data=X[,c("N","theta_sleep","y")], idvar="N",timevar="theta_sleep",direction="wide")[,-1])
  L <- list(x=unique(X$N),y=unique(X$theta_sleep),z=Z)
  return(L)
}

##	visualizes GAMLSS output from simulation
pdf(paste0(directory,"Out\\Power_DailyActivity_", as.Date(Sys.time(), tz="MST"),".pdf"), height=8, width=12)
par(mfrow=c(2,3), mar=c(2,2,2,2), oma=c(6,6,6,1))
contour(contourMatrixSteps(model=g2Steps_bonf,correlation=.1),labcex=1); mtext("Assuming 7 days of data", side=2, line=2, cex=.85, font=3); mtext("Relative increase in steps", side=2, line=3, cex=1, font=2); mtext("Control FWER per Bonferroni", side=3, line=2, cex=1, font=2)
contour(contourMatrixSteps(model=g2Steps_holm,correlation=.1),labcex=1); mtext("Control FWER per Holm", side=3, line=2, cex=1, font=2)
contour(contourMatrixSteps(model=g2Steps_fdr,correlation=.1),labcex=1); mtext("Control FDR per Benjamini-Hochberg", side=3, line=2, cex=1, font=2)
contour(contourMatrixSleep(model=g2Sleep_bonf,correlation=.1),labcex=1); mtext("Assuming 7 days of data", side=2, line=2, cex=.85, font=3); mtext("Relative increase in sleep", side=2, line=3, cex=1, font=2); mtext("Prospective enrollment", side=1, line=3, cex=1, font=2)
contour(contourMatrixSleep(model=g2Sleep_holm,correlation=.1),labcex=1); mtext("Prospective enrollment", side=1, line=3, cex=1, font=2)
contour(contourMatrixSleep(model=g2Sleep_fdr,correlation=.1),labcex=1); mtext("Prospective enrollment", side=1, line=3, cex=1, font=2)
mtext("Power if corr=0.1",side=3,line=3.5,outer=TRUE,font=3,cex=2)
par(mfrow=c(2,3), mar=c(2,2,2,2), oma=c(6,6,6,1))
contour(contourMatrixSteps(model=g2Steps_bonf,correlation=.2),labcex=1); mtext("Assuming 7 days of data", side=2, line=2, cex=.85, font=3); mtext("Relative increase in steps", side=2, line=3, cex=1, font=2); mtext("Control FWER per Bonferroni", side=3, line=2, cex=1, font=2)
contour(contourMatrixSteps(model=g2Steps_holm,correlation=.2),labcex=1); mtext("Control FWER per Holm", side=3, line=2, cex=1, font=2)
contour(contourMatrixSteps(model=g2Steps_fdr,correlation=.2),labcex=1); mtext("Control FDR per Benjamini-Hochberg", side=3, line=2, cex=1, font=2)
contour(contourMatrixSleep(model=g2Sleep_bonf,correlation=.2),labcex=1); mtext("Assuming 7 days of data", side=2, line=2, cex=.85, font=3); mtext("Relative increase in sleep", side=2, line=3, cex=1, font=2); mtext("Prospective enrollment", side=1, line=3, cex=1, font=2)
contour(contourMatrixSleep(model=g2Sleep_holm,correlation=.2),labcex=1); mtext("Prospective enrollment", side=1, line=3, cex=1, font=2)
contour(contourMatrixSleep(model=g2Sleep_fdr,correlation=.2),labcex=1); mtext("Prospective enrollment", side=1, line=3, cex=1, font=2)
mtext("Power if corr=0.2",side=3,line=3.5,outer=TRUE,font=3,cex=2)
par(mfrow=c(2,3), mar=c(2,2,2,2), oma=c(6,6,6,1))
contour(contourMatrixSteps(model=g2Steps_bonf,correlation=.3),labcex=1); mtext("Assuming 7 days of data", side=2, line=2, cex=.85, font=3); mtext("Relative increase in steps", side=2, line=3, cex=1, font=2); mtext("Control FWER per Bonferroni", side=3, line=2, cex=1, font=2)
contour(contourMatrixSteps(model=g2Steps_holm,correlation=.3),labcex=1); mtext("Control FWER per Holm", side=3, line=2, cex=1, font=2)
contour(contourMatrixSteps(model=g2Steps_fdr,correlation=.3),labcex=1); mtext("Control FDR per Benjamini-Hochberg", side=3, line=2, cex=1, font=2)
contour(contourMatrixSleep(model=g2Sleep_bonf,correlation=.3),labcex=1); mtext("Assuming 7 days of data", side=2, line=2, cex=.85, font=3); mtext("Relative increase in sleep", side=2, line=3, cex=1, font=2); mtext("Prospective enrollment", side=1, line=3, cex=1, font=2)
contour(contourMatrixSleep(model=g2Sleep_holm,correlation=.3),labcex=1); mtext("Prospective enrollment", side=1, line=3, cex=1, font=2)
contour(contourMatrixSleep(model=g2Sleep_fdr,correlation=.3),labcex=1); mtext("Prospective enrollment", side=1, line=3, cex=1, font=2)
mtext("Power if corr=0.3",side=3,line=3.5,outer=TRUE,font=3,cex=2)
par(mfrow=c(2,3), mar=c(2,2,2,2), oma=c(6,6,6,1))
contour(contourMatrixSteps(model=g2Steps_bonf,correlation=.4),labcex=1); mtext("Assuming 7 days of data", side=2, line=2, cex=.85, font=3); mtext("Relative increase in steps", side=2, line=3, cex=1, font=2); mtext("Control FWER per Bonferroni", side=3, line=2, cex=1, font=2)
contour(contourMatrixSteps(model=g2Steps_holm,correlation=.4),labcex=1); mtext("Control FWER per Holm", side=3, line=2, cex=1, font=2)
contour(contourMatrixSteps(model=g2Steps_fdr,correlation=.4),labcex=1); mtext("Control FDR per Benjamini-Hochberg", side=3, line=2, cex=1, font=2)
contour(contourMatrixSleep(model=g2Sleep_bonf,correlation=.4),labcex=1); mtext("Assuming 7 days of data", side=2, line=2, cex=.85, font=3); mtext("Relative increase in sleep", side=2, line=3, cex=1, font=2); mtext("Prospective enrollment", side=1, line=3, cex=1, font=2)
contour(contourMatrixSleep(model=g2Sleep_holm,correlation=.4),labcex=1); mtext("Prospective enrollment", side=1, line=3, cex=1, font=2)
contour(contourMatrixSleep(model=g2Sleep_fdr,correlation=.4),labcex=1); mtext("Prospective enrollment", side=1, line=3, cex=1, font=2)
mtext("Power if corr=0.4",side=3,line=3.5,outer=TRUE,font=3,cex=2)
par(mfrow=c(2,3), mar=c(2,2,2,2), oma=c(6,6,6,1))
contour(contourMatrixSteps(model=g2Steps_bonf,correlation=.5),labcex=1); mtext("Assuming 7 days of data", side=2, line=2, cex=.85, font=3); mtext("Relative increase in steps", side=2, line=3, cex=1, font=2); mtext("Control FWER per Bonferroni", side=3, line=2, cex=1, font=2)
contour(contourMatrixSteps(model=g2Steps_holm,correlation=.5),labcex=1); mtext("Control FWER per Holm", side=3, line=2, cex=1, font=2)
contour(contourMatrixSteps(model=g2Steps_fdr,correlation=.5),labcex=1); mtext("Control FDR per Benjamini-Hochberg", side=3, line=2, cex=1, font=2)
contour(contourMatrixSleep(model=g2Sleep_bonf,correlation=.5),labcex=1); mtext("Assuming 7 days of data", side=2, line=2, cex=.85, font=3); mtext("Relative increase in sleep", side=2, line=3, cex=1, font=2); mtext("Prospective enrollment", side=1, line=3, cex=1, font=2)
contour(contourMatrixSleep(model=g2Sleep_holm,correlation=.5),labcex=1); mtext("Prospective enrollment", side=1, line=3, cex=1, font=2)
contour(contourMatrixSleep(model=g2Sleep_fdr,correlation=.5),labcex=1); mtext("Prospective enrollment", side=1, line=3, cex=1, font=2)
mtext("Power if corr=0.5",side=3,line=3.5,outer=TRUE,font=3,cex=2)
par(mfrow=c(2,3), mar=c(2,2,2,2), oma=c(6,6,6,1))
contour(contourMatrixSteps(model=g2Steps_bonf,correlation=.6),labcex=1); mtext("Assuming 7 days of data", side=2, line=2, cex=.85, font=3); mtext("Relative increase in steps", side=2, line=3, cex=1, font=2); mtext("Control FWER per Bonferroni", side=3, line=2, cex=1, font=2)
contour(contourMatrixSteps(model=g2Steps_holm,correlation=.6),labcex=1); mtext("Control FWER per Holm", side=3, line=2, cex=1, font=2)
contour(contourMatrixSteps(model=g2Steps_fdr,correlation=.6),labcex=1); mtext("Control FDR per Benjamini-Hochberg", side=3, line=2, cex=1, font=2)
contour(contourMatrixSleep(model=g2Sleep_bonf,correlation=.6),labcex=1); mtext("Assuming 7 days of data", side=2, line=2, cex=.85, font=3); mtext("Relative increase in sleep", side=2, line=3, cex=1, font=2); mtext("Prospective enrollment", side=1, line=3, cex=1, font=2)
contour(contourMatrixSleep(model=g2Sleep_holm,correlation=.6),labcex=1); mtext("Prospective enrollment", side=1, line=3, cex=1, font=2)
contour(contourMatrixSleep(model=g2Sleep_fdr,correlation=.6),labcex=1); mtext("Prospective enrollment", side=1, line=3, cex=1, font=2)
mtext("Power if corr=0.6",side=3,line=3.5,outer=TRUE,font=3,cex=2)
par(mfrow=c(2,3), mar=c(2,2,2,2), oma=c(6,6,6,1))
contour(contourMatrixSteps(model=g2Steps_bonf,correlation=.7),labcex=1); mtext("Assuming 7 days of data", side=2, line=2, cex=.85, font=3); mtext("Relative increase in steps", side=2, line=3, cex=1, font=2); mtext("Control FWER per Bonferroni", side=3, line=2, cex=1, font=2)
contour(contourMatrixSteps(model=g2Steps_holm,correlation=.7),labcex=1); mtext("Control FWER per Holm", side=3, line=2, cex=1, font=2)
contour(contourMatrixSteps(model=g2Steps_fdr,correlation=.7),labcex=1); mtext("Control FDR per Benjamini-Hochberg", side=3, line=2, cex=1, font=2)
contour(contourMatrixSleep(model=g2Sleep_bonf,correlation=.7),labcex=1); mtext("Assuming 7 days of data", side=2, line=2, cex=.85, font=3); mtext("Relative increase in sleep", side=2, line=3, cex=1, font=2); mtext("Prospective enrollment", side=1, line=3, cex=1, font=2)
contour(contourMatrixSleep(model=g2Sleep_holm,correlation=.7),labcex=1); mtext("Prospective enrollment", side=1, line=3, cex=1, font=2)
contour(contourMatrixSleep(model=g2Sleep_fdr,correlation=.7),labcex=1); mtext("Prospective enrollment", side=1, line=3, cex=1, font=2)
mtext("Power if corr=0.7",side=3,line=3.5,outer=TRUE,font=3,cex=2)
par(mfrow=c(2,3), mar=c(2,2,2,2), oma=c(6,6,6,1))
contour(contourMatrixSteps(model=g2Steps_bonf,correlation=.8),labcex=1); mtext("Assuming 7 days of data", side=2, line=2, cex=.85, font=3); mtext("Relative increase in steps", side=2, line=3, cex=1, font=2); mtext("Control FWER per Bonferroni", side=3, line=2, cex=1, font=2)
contour(contourMatrixSteps(model=g2Steps_holm,correlation=.8),labcex=1); mtext("Control FWER per Holm", side=3, line=2, cex=1, font=2)
contour(contourMatrixSteps(model=g2Steps_fdr,correlation=.8),labcex=1); mtext("Control FDR per Benjamini-Hochberg", side=3, line=2, cex=1, font=2)
contour(contourMatrixSleep(model=g2Sleep_bonf,correlation=.8),labcex=1); mtext("Assuming 7 days of data", side=2, line=2, cex=.85, font=3); mtext("Relative increase in sleep", side=2, line=3, cex=1, font=2); mtext("Prospective enrollment", side=1, line=3, cex=1, font=2)
contour(contourMatrixSleep(model=g2Sleep_holm,correlation=.8),labcex=1); mtext("Prospective enrollment", side=1, line=3, cex=1, font=2)
contour(contourMatrixSleep(model=g2Sleep_fdr,correlation=.8),labcex=1); mtext("Prospective enrollment", side=1, line=3, cex=1, font=2)
mtext("Power if corr=0.8",side=3,line=3.5,outer=TRUE,font=3,cex=2)
par(mfrow=c(2,3), mar=c(2,2,2,2), oma=c(6,6,6,1))
contour(contourMatrixSteps(model=g2Steps_bonf,correlation=.9),labcex=1); mtext("Assuming 7 days of data", side=2, line=2, cex=.85, font=3); mtext("Relative increase in steps", side=2, line=3, cex=1, font=2); mtext("Control FWER per Bonferroni", side=3, line=2, cex=1, font=2)
contour(contourMatrixSteps(model=g2Steps_holm,correlation=.9),labcex=1); mtext("Control FWER per Holm", side=3, line=2, cex=1, font=2)
contour(contourMatrixSteps(model=g2Steps_fdr,correlation=.9),labcex=1); mtext("Control FDR per Benjamini-Hochberg", side=3, line=2, cex=1, font=2)
contour(contourMatrixSleep(model=g2Sleep_bonf,correlation=.9),labcex=1); mtext("Assuming 7 days of data", side=2, line=2, cex=.85, font=3); mtext("Relative increase in sleep", side=2, line=3, cex=1, font=2); mtext("Prospective enrollment", side=1, line=3, cex=1, font=2)
contour(contourMatrixSleep(model=g2Sleep_holm,correlation=.9),labcex=1); mtext("Prospective enrollment", side=1, line=3, cex=1, font=2)
contour(contourMatrixSleep(model=g2Sleep_fdr,correlation=.9),labcex=1); mtext("Prospective enrollment", side=1, line=3, cex=1, font=2)
mtext("Power if corr=0.9",side=3,line=3.5,outer=TRUE,font=3,cex=2)
dev.off()

##------------------------------------------------------------
##------------------------------------------------------------
##------------------------------------------------------------