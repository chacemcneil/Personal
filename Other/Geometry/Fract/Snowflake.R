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

Mats <- list(mat1,mat2)

hexag  <- c(0,0,-.5,sqrt(3)/2,-.5,sqrt(3)/2,0,sqrt(3),0,sqrt(3),1,sqrt(3),1,sqrt(3),1.5,sqrt(3)/2,1.5,sqrt(3)/2,1,0,1,0,0,0) - c(.5,sqrt(3)/2)

fun <- function (pts,ord,h,meToo,col,lwd) {
  if(h==0 | meToo) {
    lines(pts[c(1,3)],pts[c(2,4)],col=col,lwd=lwd)
  }
  if(h>0) {
    new <- Mats[[ord[1]]]%*%pts
    for ( i in 1:(length(new)/4)) {
      fun(new[(4*i-3):(4*i)],c(ord[-1],ord[1]),h-1,meToo,col,lwd)
    }
  }
}
start <- function(pts,ord,h,meToo=F,add=F,marg=.2,scale=1,col='black',lwd=1) {
  d <- scale*(sqrt(pts[3]^2+pts[4]^2)+marg)
  if (add==F) {
    plot(c(-d,d),c(-d,d),type='n',xlab='',ylab='')
  }
  for ( i in 1:(length(pts)/4)) {
    fun(scale*pts[(4*i-3):(4*i)],ord,h,meToo=meToo,col=col,lwd=lwd)
  }
}

start(hexag,c(2,2,1),5)
for (i in 1:11)
  start(hexag,c(2,2,1),5-floor(i/4),add=T,scale=(2/3)^i)
