# Fractals by matrices

# Rotations
Rot <- function(ang) {
	rbind(c(cos(ang),-sin(ang)),c(sin(ang),cos(ang)))
}


# Matrices
mat1 <- matrix(
c(1,0,0,0,
  0,1,0,0,
  2/3,0,1/3,0,
  0,2/3,0,1/3,
  2/3,0,1/3,0,	
  0,2/3,0,1/3,
  1/2,sqrt(3)/6,1/2,-sqrt(3)/6,
  -sqrt(3)/6,1/2,sqrt(3)/6,1/2,
  1/2,sqrt(3)/6,1/2,-sqrt(3)/6,
  -sqrt(3)/6,1/2,sqrt(3)/6,1/2,
  1/3,0,2/3,0,
  0,1/3,0,2/3,
  1/3,0,2/3,0,
  0,1/3,0,2/3,
  0,0,1,0,
  0,0,0,1
),
16,4,byrow=T)

mat2 <- matrix(
c(1,0,0,0,
  0,1,0,0,
  2/3,0,1/3,0,
  0,2/3,0,1/3,
  2/3,0,1/3,0,	
  0,2/3,0,1/3,
  1/2,sqrt(3)/6,1/2,-sqrt(3)/6,
  -sqrt(3)/6,1/2,sqrt(3)/6,1/2,
  1/2,sqrt(3)/6,1/2,-sqrt(3)/6,
  -sqrt(3)/6,1/2,sqrt(3)/6,1/2,
  1/3,0,2/3,0,
  0,1/3,0,2/3,
  1/3,0,2/3,0,
  0,1/3,0,2/3,
  2/3,0,1/3,0,	
  0,2/3,0,1/3,
  1/3,0,2/3,0,
  0,1/3,0,2/3,
  0,0,1,0,
  0,0,0,1
),
20,4,byrow=T)

mat3 <- matrix(
c(1,0,0,0,
  0,1,0,0,
  2/3,0,1/3,0,
  0,2/3,0,1/3,
  2/3,0,1/3,0,	
  0,2/3,0,1/3,
  1/2,sqrt(3)/6,1/2,-sqrt(3)/6,
  -sqrt(3)/6,1/2,sqrt(3)/6,1/2,
  1/2,sqrt(3)/6,1/2,-sqrt(3)/6,
  -sqrt(3)/6,1/2,sqrt(3)/6,1/2,
  1/3,0,2/3,0,
  0,1/3,0,2/3,
  1/3,0,2/3,0,
  0,1/3,0,2/3,
  1/2,-sqrt(3)/6,1/2,sqrt(3)/6,
  sqrt(3)/6,1/2,-sqrt(3)/6,1/2,
  1/2,-sqrt(3)/6,1/2,sqrt(3)/6,
  sqrt(3)/6,1/2,-sqrt(3)/6,1/2,
  2/3,0,1/3,0,	
  0,2/3,0,1/3,
  1/3,0,2/3,0,
  0,1/3,0,2/3,
  0,0,1,0,
  0,0,0,1
),
24,4,byrow=T)

a <- 1/5
mat4 <- matrix(
c(1,0,0,0,
  0,1,0,0,
  1/2,a,1/2,-a,
  -a,1/2,a,1/2,
  1/2,a,1/2,-a,
  -a,1/2,a,1/2,
  0,0,1,0,
  0,0,0,1,
  0,0,1,0,
  0,0,0,1,
  1/2,-a,1/2,a,
  a,1/2,-a,1/2,
  1/2,-a,1/2,a,
  a,1/2,-a,1/2,
  1,0,0,0,
  0,1,0,0
  
),
16,4,byrow=T)

a <- 1/3
mat5 <- matrix(
c(1,0,0,0,
  0,1,0,0,
  2/3,0,1/3,0,
  0,2/3,0,1/3,
  2/3,0,1/3,0,	
  0,2/3,0,1/3,
  2/3,a,1/3,-a,
  -a,2/3,a,1/3,
  2/3,a,1/3,-a,
  -a,2/3,a,1/3,
  1/3,a,2/3,-a,
  -a,1/3,a,2/3,
  1/3,a,2/3,-a,
  -a,1/3,a,2/3,
  1/3,0,2/3,0,
  0,1/3,0,2/3,
  1/3,0,2/3,0,
  0,1/3,0,2/3,
  0,0,1,0,
  0,0,0,1
),
20, 4,byrow=T)

a <- 1/3
mat6 <- matrix(
c(1,0,0,0,
  0,1,0,0,
  2/3,0,1/3,0,
  0,2/3,0,1/3,
  2/3,0,1/3,0,	
  0,2/3,0,1/3,
  2/3,-a,1/3,a,
  a,2/3,-a,1/3,
  2/3,-a,1/3,a,
  a,2/3,-a,1/3,
  1/3,-a,2/3,a,
  a,1/3,-a,2/3,
  1/3,-a,2/3,a,
  a,1/3,-a,2/3,
  1/3,0,2/3,0,
  0,1/3,0,2/3,
  1/3,0,2/3,0,
  0,1/3,0,2/3,
  0,0,1,0,
  0,0,0,1
),
20, 4,byrow=T)

a <- 1/2
mat7 <- matrix(
c(0,0,1,0,
  0,0,0,1,
  0,-a,1,a,
  a,0,-a,1,
  0,0,1,0,
  0,0,0,1,
  0,a,1,-a,
  -a,0,a,1  
),
8, 4,byrow=T)

a <- 1/2
mat8 <- matrix(
c(0,0,1,0,
  0,0,0,1,
  0,-a,1,a,
  a,0,-a,1,
  0,0,1,0,
  0,0,0,1,
  0,a,1,-a,
  -a,0,a,1,
  0,0,1,0,
  0,0,0,1,
  -a,0,1+a,0,
  0,-a,0,1+a
),
12, 4,byrow=T)



# List containing all matrices
Mats <- list(mat1,mat2,mat3,mat4,mat5,mat6,mat7,mat8) 

# Starting shapes
triang <- c(0,0,0,1,0,1,sqrt(3)/2,1/2,sqrt(3)/2,1/2,0,0) - c(1/(sqrt(3)*2),.5)
square <- c(0,0,0,1,0,1,1,1,1,1,1,0,1,0,0,0) - c(.5,.5)
diamond<- c(0,-1,-1,0,-1,0,0,1,0,1,1,0,1,0,0,-1)
hexag  <- c(0,0,-.5,sqrt(3)/2,-.5,sqrt(3)/2,0,sqrt(3),0,sqrt(3),1,sqrt(3),1,sqrt(3),1.5,sqrt(3)/2,1.5,sqrt(3)/2,1,0,1,0,0,0) - c(.5,sqrt(3)/2)
pentag <- c()
sticks <- c(0,0,0,1,0,0,0,-1)
sticks4 <- c(0,0,0,1,0,0,0,-1,0,0,-1,0,0,0,1,0)


# pts = start and end points as 4x1 vector
# ord = sequential order of matrices
# h   = level

fun <- function (pts,ord,h,meToo,col,lwd,returnpts=F) {
  retpts <- numeric()
	if(h==0 | meToo) {
		#lines(pts[c(1,3)],pts[c(2,4)],col=col,lwd=lwd)
		points(pts[c(1,3)],pts[c(2,4)],col=col,lwd=lwd,pch=19,cex=.2)
    retpts <- pts
	}
	if(h>0) {
		newpts <- Mats[[ord[1]]]%*%pts
		for ( i in 1:(length(newpts)/4)) {
			retpts <- c(retpts,fun(newpts[(4*i-3):(4*i)],c(ord[-1],ord[1]),h-1,meToo,col,lwd,returnpts=returnpts))
		}
	}
  if(returnpts)
    return(retpts)
}
start <- function(pts,ord,h,meToo=F,add=F,marg=.2,scale=1,col='black',lwd=1,returnpts=F) {
	d <- scale*(sqrt(pts[3]^2+pts[4]^2)+marg)
	if (add==F) {
		plot(c(-d,d),c(-d,d),type='n',xlab='',ylab='')
	}
  retpts <- numeric()
	for ( i in 1:(length(pts)/4)) {
		retpts <- c(retpts,fun(scale*pts[(4*i-3):(4*i)],ord,h,meToo=meToo,col=col,lwd=lwd,returnpts=returnpts))
	}
  if (returnpts)
    return(retpts)
}
start(square,c(5,6),5,meToo=T,add=F)
start(hexag,c(3,1,4,1),6,T)
pts <- start(hexag,c(2,2,1),6,returnpts=T)
for (i in 1:11)
  pts <- c(pts,start(hexag,c(2,2,1),5-floor(i/4),add=T,scale=(2/3)^i,returnpts=T))
start(square,2,9,T,F,.5,2)
start(hexag,c(6,2,5),7)

for (i in 1:6)
start(sticks4,7,6,meToo=T,add=i!=1,marg=.5,scale=2/2^i)

for (i in 1:6)
start(sticks4,8,4,meToo=T,add=i!=1,marg=.8,scale=2/2^i)


start(sticks4,8,1,marg=.8)

df <- as.data.frame(matrix(pts,ncol=4,byrow=T))
df <- rbind(df,pt)
colnames(df) <- c("x1","y1","x2","y2")
samp <- numeric()
ggplot(df[samp <- c(samp, sample(1:119400, 500),nrow(df)),],aes(x=x1,y=y1)) + geom_point(size=.0001)
ggplot(df[samp <- c(samp, sample(setdiff(1:119400,samp), 300),nrow(df)),],aes(x=x1,y=y1)) + geom_point(size=.0001)
ggplot(df[samp <- c(samp, sample(setdiff(1:119400,samp), 500),nrow(df)),],aes(x=x1,y=y1)) + geom_point(size=.0001)
ggplot(df[samp <- c(samp, sample(setdiff(1:119400,samp),1500),nrow(df)),],aes(x=x1,y=y1)) + geom_point(size=.0001)
ggplot(df[samp <- c(samp, sample(setdiff(1:119400,samp),5000),nrow(df)),],aes(x=x1,y=y1)) + geom_point(size=.0001)
ggplot(df,aes(x=x1,y=y1)) + geom_point(size=.0001)


df1 <- as.data.frame(mat6)
colnames(df1) <- c("x1","y1","x2","y2")
ggplot(df1,aes(x1,y1)) + geom_point()











pt <- c(.48,-.97,0,0)
points(pt[1],pt[2])

