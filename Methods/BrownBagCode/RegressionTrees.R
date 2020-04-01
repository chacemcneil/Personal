# Plots for Regression Trees Brown Bag presentation
 library(ggplot2)
 
 
 points <- locator(20)
 xs <- runif(1000,min=min(points$x),max=max(points$x))
 ys <- predict(loess(points$y~points$x),newdata=xs) + rnorm(length(xs),0,.1)
 
 df <- data.frame(x=xs,y=ys)
 
 ggplot(df,aes(x=x,y=y)) + geom_point(size=1)
 
 lmmod <- lm(y~x,data=df)
 ggplot(df,aes(x=x,y=y)) + geom_point(size=1) + geom_abline(intercept=lmmod$coef[1],slope=lmmod$coef[2],col="indianred",lwd=1)
 
 knots <- c(2,4.2,5.6)
 crX <- function(x) data.frame(x,I(x>knots[1]),I(x>knots[2]),I(x>knots[3]),x*I(x>knots[1]),x*I(x>knots[2]),x*I(x>knots[3]) )
 lmmod2 <- lm(df$y~as.matrix(crX(df$x)) )
 lmmod2 <- lm(y~x+x*I(x>knots[1])+x*I(x>knots[2])+x*I(x>knots[3])+x*I(x>knots[4]),data=df)
 ggplot(df,aes(x=x,y=y)) + geom_point(size=1) + geom_vline(data=data.frame(x=knots),aes(xintercept=x),col="indianred",lty=2,lwd=1) +
   #stat_function(data=df,fun=function(x) predict(lmmod2,newdata=data.frame(x,I((x-knots[1])*(x>knots[1])),I((x-knots[2])*(x>knots[2])),I((x-knots[3])*(x>knots[3])) )))
   #stat_function(data=df,fun=function(x) predict(lmmod2,newdata=data.frame(x,I(x>knots[1]),I(x>knots[2]),I(x>knots[3]),x*I(x>knots[1]),x*I(x>knots[2]),x*I(x>knots[3]) )) )
   geom_line(data=df,aes(x=x,y=predict(lmmod2,newdata=data.frame(x,x>knots[1],x>knots[2],x>knots[3],x*I(x>knots[1]),x*I(x>knots[2]),x*I(x>knots[3]) )),group=(x>knots[1])+(x>knots[2])+(x>knots[3])+(x>knots[4]) ),col="indianred",lwd=1)
 
 
 # Residual plot
 ggplot(data.frame(resid=lmmod2$resid),aes(x=seq_along(resid),y=resid)) + geom_point(col=(lmmod2$resid>0)*2 + 2,pch=19,ylab="Residuals",cex=1.1) + labs(x="Index",y="Residuals") + geom_abline(aes(intercept=0,slope=0),lty=2)
 
 
 # Cutpoint plot
 xt <- seq(0,1,.025)
 yt <- (xt-.7)^2 + .06
 ggplot(data.frame(x=xt,y=yt),aes(x=x*28,y=y)) + geom_point() + scale_y_continuous(limits=c(0,1)) + geom_abline(aes(intercept=0,slope=0)) + geom_vline(aes(xintercept=xt[which.min(yt)]*28),col="indianred",lty=2,lwd=1) + labs(x="PT",y="Total Sum of Squared Residuals")
 
 
 
 mean(lmmod$resid^2)
 mean(lmmod2$resid^2)
 
 
 # Demonstration that split points (or knots) are not chosen optimally
 
 plotknot <- function(df,n0=200,level=NA) {
   df$group <- 1
   df$pred  <- lm(y~x,data=df)$fitted.values
   knots <- numeric()
   
   while (level > 0 & max(table(df$group)) > 2*n0) {
     for (g in unique(df$group)) {
       temp <- df[df$group==g,]
       quants <- quantile(temp$x,(10:90)/100)
       Ss <- NULL
       lmod <- list()
       rmod <- list()
       for (i in seq_along(quants)) {
         lt <- which(temp$x <= quants[i])
         rt <- which(temp$x >  quants[i])
         if (length(lt) < n0 | length(rt) < n0)
           Ss[i] <- Inf
         else {
           #cat(";",length(lt),length(rt))
           lmod[[i]] <- lm(y~x,data=temp[lt,])
           rmod[[i]] <- lm(y~x,data=temp[rt,])
           Ss[i] <- sum(lmod[[i]]$resid^2,rmod[[i]]$resid^2)
         }
       }
       if (min(Ss)!=Inf) {
         ii <- which.min(Ss)
         knots <- c(knots,quants[ii])
         lg <- max(df$group) + 1
         rg <- max(df$group) + 2
         df$group[df$group==g & df$x <= quants[[ii]]] <- lg
         df$group[df$group==g & TRUE] <- rg
         df$pred[df$group==lg] <- lmod[[ii]]$fitted.values
         df$pred[df$group==rg] <- rmod[[ii]]$fitted.values
       }
     }
     level <- level - 1
   }
   print(ggplot(df,aes(x=x,y=y)) + geom_point(cex=1) + geom_line(aes(x=x,y=pred,group=group),col="indianred",lwd=1) + geom_vline(xintercept=knots,col="indianred",lty=2,lwd=1))
   return(list(df=df,knots=knots))
 }
 
 res0 <- plotknot(df,level=0)
 res1 <- plotknot(df,level=1)
 res2 <- plotknot(df,level=2)
 res3 <- plotknot(df,level=3,n0=100)
 res4 <- plotknot(df,level=4,n0=100)
 
 res <- list(res0,res1,res2,res3,res4)
 sapply(res,function(x) mean((x$df$y-x$df$pred)^2))
 
 pal <- c("indianred","slateblue","goldenrod2","seagreen","magenta")
 ps <- sapply(1:length(res),function(i) geom_line(data=res[[i]]$df,aes(x=x,y=pred,group=group),col=pal[i],lwd=1) )
 p <- ggplot(res0$df,aes(x=x,y=y)) + geom_point(cex=1)
 for (i in 1:length(ps))
   p <- p + ps[[i]]
 p + geom_vline(xintercept=res4$knots,col=pal[floor(log2(seq_along(res4$knots)))+2],lty=2,lwd=1) 
 
 ggplot(res0$df,aes(x=x,y=y)) + geom_point(cex=1) + geom_line(aes(x=x,y=pred,group=group),col="indianred",lwd=1) + geom_vline(xintercept=res0$knots,col="indianred",lty=2,lwd=1)
 
 
 
 # not working
 testtree <- splitNode(df$y,df[,c("x","x")],n0=5,family=gaussian)
 
 
 
 
 # Plot node splitting example
 plot(1:2,3:4,axes=F,xlab="",ylab="",lwd=5,type="l",cex=10,xlim=c(0,2),ylim=c(.5,4))
 points(3,cex=14,col="white",pch=19)
 points(3,cex=10,lwd=20)
 
 text(1,3,"DIS\nRAD",pos=2,offset=4,cex=2)
 
 lines((1:3)/2,c(1,3,1),lwd=5)
 points((1:3)/2,c(1,3,1),cex=14,col="white",pch=19)
 points((1:3)/2,c(1,3,1),cex=10,lwd=20)
 points(c(1,3)/2,c(1,1),cex=10,lwd=20,col="indianred")
 
 text(.25,2,expression(PT <= 19.6),cex=1.7)
 text(1.75,2,"PT > 19.6",cex=1.7)
 
 
 
 
 
 
 
 
 