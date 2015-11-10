# Plots of

# Plots of Bias for OLS, Ridge, LASSO
par(mfrow=c(2,2),las=1,mai=c(1,1,0.5,0.5))
plot(0,0,type="n",axes=F,xlab="",ylab="")
plot(-4:4,-4:4,type="l",lty=2,xlab=expression(beta),ylab=expression(E(hat(beta))),main="Variable Subsetting")
abline(h=0)
lines(c(-4,-2,-2,0,2,2,4),c(-4,-2,0,0,0,2,4),col="slateblue",lwd=2)
plot(-4:4,-4:4,type="l",lty=2,xlab=expression(beta),ylab=expression(E(hat(beta))),main="Ridge Regression")
abline(h=0)
lines(c(-4,4),c(-2,2),col="slateblue",lwd=2)
plot(-4:4,-4:4,type="l",lty=2,xlab=expression(beta),ylab=expression(E(hat(beta))),main="LASSO Regression")
abline(h=0)
lines(c(-4,-2,0,2,4),c(-2,0,0,0,2),col="slateblue",lwd=2)

# Geometric interpretation of constraints
x1 <- rnorm(100,10,3)
x2 <- rnorm(100,10-1.2*x1,2)
y  <- 1*x1 + 5*x2 + rnorm(100)
mod <- lm(y~x1+x2)
summary(mod)
b.est <- coef(mod)[-1]
mat <- cbind(x1,x2)
b.Sig <- solve(t(mat)%*%mat)
cont <- expand.grid(x=seq(-.5,4,length.out=20),y=seq(-.5,7,length.out=20))
cont$dist <- mahalanobis(cont,b.est,b.Sig)
#Ridge Regression
ggplot() + geom_contour(data=cont,aes(x=x,y=y,z=dist),binwidth=3050) + 
  geom_hline(aes(yintercept=0)) + geom_vline(aes(xintercept=0)) + 
  geom_point(data=data.frame(x=0,y=0),aes(x,y),shape=1,cex=100) + 
  geom_point(data=data.frame(x=b.est[1],y=b.est[2]),aes(x,y),colour="indianred") +
  labs(x=expression(beta[1]),y=expression(beta[2]))
# LASSO Regression
ggplot() + geom_contour(data=cont,aes(x=x,y=y,z=dist),binwidth=3200) + 
  geom_hline(aes(yintercept=0)) + geom_vline(aes(xintercept=0)) + 
  geom_path(data=data.frame(x=c(-.5,0,2,1),y=c(1.5,2,0,-1)),aes(x=x,y=y)) + 
  geom_point(data=data.frame(x=b.est[1],y=b.est[2]),aes(x,y),colour="indianred") +
  labs(x=expression(beta[1]),y=expression(beta[2]))
ggplot() + geom_contour(data=cont,aes(x=x,y=y,z=dist),binwidth=3400) + 
  geom_hline(aes(yintercept=0)) + geom_vline(aes(xintercept=0)) + 
  geom_path(data=data.frame(x=c(-.5,0,4,3.5),y=c(3.5,4,0,-.5)),aes(x=x,y=y)) + 
  geom_point(data=data.frame(x=b.est[1],y=b.est[2]),aes(x,y),colour="indianred") +
  labs(x=expression(beta[1]),y=expression(beta[2]))
ggplot() + geom_contour(data=cont,aes(x=x,y=y,z=dist),binwidth=3200) + 
  geom_hline(aes(yintercept=0)) + geom_vline(aes(xintercept=0)) + 
  geom_path(data=data.frame(x=c(-.5,0,4),y=c(5.5,6,2)),aes(x=x,y=y)) + 
  geom_point(data=data.frame(x=b.est[1],y=b.est[2]),aes(x,y),colour="indianred") +
  labs(x=expression(beta[1]),y=expression(beta[2]))


setwd("r:/users/cmcneil/projects/optum")
load("Data/allv5.rda")               # Created in ModelGLMNET.R
load("Data/cv_scc_combined.rda")     # Created in ModelGLMNET.R
load("Data/v8scc.rda",verbose=T)     # Created in ReadLoad.R

# Example of coefficient plot
plot(cv.scc.combined$mods[[5]]$glmnet.fit)
#abline(0,1)
#abline(v=.65)

# Example of MSE/AUC cross-validation plots.
plot(allv5$mods[[5]],ylab="MSE") # Just an example
plot(cv.scc.combined$mods[[5]])


# Comparison
library(glmnet)
library(pROC)

set.seed(12)
seeds <- ceiling(runif(115)*1000)

Y <- v5scc13[,sccs]

usecols  <- c(sccs,chrs,cpts,drgs,dsdrgs,mtdrgs,labdone,lababnl)
X <- cbind(as.matrix(v5scc13[,c("Age","Gender")]),as.matrix(v5scc11[,usecols]),as.matrix(v5scc12[,usecols]))
colnames(X) <- c("Age","Gender",paste0(usecols,"_2011"),paste0(usecols,"_2012"))

col <- "SCC15"
ones  <- which(Y[,col]>0)
zeros <- which(Y[,col]==0)
train  <- c(sample(ones,floor(length(ones)/2)),sample(zeros,length(ones)))
test <- setdiff(1:nrow(Y),train)

df <- data.frame(y=Y[,col]>0,X)
mod <- glm(y~.,data=df[train,],family="binomial")
pred <- predict(mod,newdata=df[test,])
pred.q <- quantile(pred,(0:100)/100)
rc   <- roc(Y[test,col]>0,as.numeric(cut(as.vector(pred),unique(pred.q),include.lowest=T)),plot=T,print.auc=T)

vec1 <- coef(allv5$mods[col][[1]])
vec2 <- coef(mod)




# End script