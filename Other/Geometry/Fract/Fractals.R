# Fractals

library(RColorBrewer)

recurs <- function(one,two,h) {
	if (h==0) {
		lines(c(one[1],two[1]),c(one[2],two[2]))
	}
	else {
		vec <- (3*one+two)/4 - one
		recurs(one,one+vec,h-1)
		recurs(one+vec,one+vec+rot%*%vec,h-1)
		recurs(one+vec+rot%*%vec,two-vec+rot%*%vec,h-1)		
		recurs(two-vec+rot%*%vec,two-vec,h-1)
		recurs(two-vec,two,h-1)
	}
}
start <- function(h) {
	recurs(c(0,0),c(0,1),h)
	recurs(c(0,1),c(1,1),h)
	recurs(c(1,1),c(1,0),h)
	recurs(c(1,0),c(0,0),h)
}
rot <- cbind(c(0,-1),c(1,0))
plot(c(0,1),c(0,1),type='n') # Increase size of Quartz window
start(4)				# Adjust argument for different complexity



recurs2 <- function(one,two,h,color) {
	if (h==0) {
		lines(c(one[1],two[1]),c(one[2],two[2]),lwd=.5,col=colorRampPalette(brewer.pal(9,'GnBu'))(256)[color])
	}
	else {
		vec <- (2*one+two)/3 - one
		new <- 0*c(rnorm(1,0,sqrt(vec[1]^2+vec[2]^2)))
		recurs2(one,one+vec,h-1,color)
		recurs2(one+vec,one+vec+rot2%*%vec+new,h-1,color)
		recurs2(one+vec+rot2%*%vec+new,one+2*vec,h-1,color)
		recurs2(one+2*vec,two,h-1,color)
	}
}
start2 <- function(h,add=F,color=1,scale=1) {
	if(add==F) {
		plot(c(-.5,1),c(-.2,1.2),type='n')
	}
	one <- c(0,0)
	two <- c(0,1)
	three <- rot2%*%c(0,-1)+c(0,1)
	cent <- c(1/(2*sqrt(3)),1/2)
	recurs2(cent+scale*(one-cent),cent+scale*(two-cent),h,color)
	recurs2(cent+scale*(two-cent),cent+scale*(three-cent),h,color)
	recurs2(cent+scale*(three-cent),cent+scale*(one-cent),h,color)
}
ang <- pi/3
rot2 <- cbind(c(cos(ang),sin(ang)),c(-sin(ang),cos(ang)))
start2(4,T)				# Adjust argument for different complexity

for (i in 1:5) {
	start2(i,ifelse(i==1,F,T),col=i)
}
start2(0,T)

for (i in 1:199) {
	start2(2,ifelse(i==1,F,T),col=i,scale=2-i/100)
}

recurs12 <- function(one,two,h) {
	if (h==0) {
		lines(c(one[1],two[1]),c(one[2],two[2]))
	}
	else {
		vec <- (3*one+two)/4 - one
		recurs21(one,one+vec,h-1)
		recurs21(one+vec,one+vec+rot%*%vec,h-1)
		recurs21(one+vec+rot%*%vec,two-vec+rot%*%vec,h-1)		
		recurs21(two-vec+rot%*%vec,two-vec,h-1)
		recurs21(two-vec,two,h-1)
	}
}
recurs21 <- function(one,two,h) {
	if (h==0) {
		lines(c(one[1],two[1]),c(one[2],two[2]),lwd=.5)
	}
	else {
		vec <- (2*one+two)/3 - one
		recurs12(one,one+vec,h-1)
		recurs12(one+vec,one+vec+rot2%*%vec,h-1)
		recurs12(one+vec+rot2%*%vec,one+2*vec,h-1)
		recurs12(one+2*vec,two,h-1)
	}
}
start12 <- function(h) {
	plot(c(-.2,1.2),c(-.2,1.2),type='n')
	recurs12(c(0,0),c(0,1),h)
	recurs12(c(0,1),c(1,1),h)
	recurs12(c(1,1),c(1,0),h)
	recurs12(c(1,0),c(0,0),h)
}
start21 <- function(h) {
	plot(c(-.5,1),c(-.2,1.2),type='n')
	recurs21(c(0,0),c(0,1),h)
	recurs21(c(0,1),rot2%*%c(0,-1)+c(0,1),h)
	recurs21(rot2%*%c(0,-1)+c(0,1),c(0,0),h)
}
start12(7)
start21(4)

recurs3 <- function(one,two,h) {
	if (h==0) {
		lines(c(one[1],two[1]),c(one[2],two[2]))
	}
	else {
		vec <- (3*one+two)/4 - one
		recurs(one,one+vec,h-1)
		recurs(one+vec,one+vec+rot3%*%vec,h-1)
		recurs(one+vec+rot3%*%vec,two-vec+rot3%*%vec,h-1)		
		recurs(two-vec+rot3%*%vec,two-vec,h-1)
		recurs(two-vec,two,h-1)
	}
}
start3 <- function(h) {
	plot(c(-.5,1.5),c(-.5,1.5),type='n') 
	recurs3(c(0,0),c(0,1),h)
	recurs3(c(0,1),c(1,1),h)
	recurs3(c(1,1),c(1,0),h)
	recurs3(c(1,0),c(0,0),h)
}
rot3 <- -rot
start3(3)				# Adjust argument for different complexity



recurs123 <- function(one,two,h) {
	if (h==0) {
		lines(c(one[1],two[1]),c(one[2],two[2]))
	}
	else {
		vec <- (3*one+two)/4 - one
		recurs231(one,one+vec,h-1)
		recurs231(one+vec,one+vec+rot%*%vec,h-1)
		recurs231(one+vec+rot%*%vec,two-vec+rot%*%vec,h-1)		
		recurs231(two-vec+rot%*%vec,two-vec,h-1)
		recurs231(two-vec,two,h-1)
	}
}
recurs231 <- function(one,two,h) {
	if (h==0) {
		lines(c(one[1],two[1]),c(one[2],two[2]),lwd=.5)
	}
	else {
		vec <- (2*one+two)/3 - one
		recurs312(one,one+vec,h-1)
		recurs312(one+vec,one+vec+rot2%*%vec,h-1)
		recurs312(one+vec+rot2%*%vec,one+2*vec,h-1)
		recurs312(one+2*vec,two,h-1)
	}
}
recurs312 <- function(one,two,h) {
	if (h==0) {
		lines(c(one[1],two[1]),c(one[2],two[2]))
	}
	else {
		vec <- (3*one+two)/4 - one
		recurs123(one,one+vec,h-1)
		recurs123(one+vec,one+vec+rot3%*%vec,h-1)
		recurs123(one+vec+rot3%*%vec,two-vec+rot3%*%vec,h-1)		
		recurs123(two-vec+rot3%*%vec,two-vec,h-1)
		recurs123(two-vec,two,h-1)
	}
}
start123 <- function(h) {
	plot(c(-.2,1.2),c(-.2,1.2),type='n')
	recurs123(c(0,0),c(0,1),h)
	recurs123(c(0,1),c(1,1),h)
	recurs123(c(1,1),c(1,0),h)
	recurs123(c(1,0),c(0,0),h)
}
start231 <- function(h) {
	plot(c(-.5,1),c(-.2,1.2),type='n')
	recurs231(c(0,0),c(0,1),h)
	recurs231(c(0,1),rot2%*%c(0,-1)+c(0,1),h)
	recurs231(rot2%*%c(0,-1)+c(0,1),c(0,0),h)
}
start312 <- function(h) {
	plot(c(-.5,1.5),c(-.5,1.5),type='n') 
	recurs312(c(0,0),c(0,1),h)
	recurs312(c(0,1),c(1,1),h)
	recurs312(c(1,1),c(1,0),h)
	recurs312(c(1,0),c(0,0),h)
}
start123(5)
start231(5)
start312(6)


f1 <- function (ord,one,two,h) {
	if (h==0) {
		lines(c(one[1],two[1]),c(one[2],two[2]))
	}
	else {
		func <- fn[[1]]
		print(func)
		vec <- (3*one+two)/4 - one
		print(h)
		func(c(ord[-1],ord[1]),one,one+vec,h-1)
		func(c(ord[-1],ord[1]),one+vec,one+vec+c(-vec[2],vec[1]),h-1)
		func(c(ord[-1],ord[1]),one+vec+c(-vec[2],vec[1]),two-vec+c(-vec[2],vec[1]),h-1)		
		func(c(ord[-1],ord[1]),two-vec+c(-vec[2],vec[1]),two-vec,h-1)
		func(c(ord[-1],ord[1]),two-vec,two,h-1)
	}
}

# Could try matrices instead of different functions, then call same function

order <- c(1)
fn <- c(f1)
start <- function (ord,startx,starty,endx,endy,h) {
	plot(c(-1,1),c(0,1),type='n')
	for (i in 1:length(startx)) {
		as.function(fn[[1]])(c(ord[-1],ord[1]),c(startx[i],starty[i]),c(endx[i],endy[i]),h)
	}
}
start(1,0,0,0,1,1)
mode(as.function(fn[1]))
as.function(fn[[1]])
f1(1,c(0,0),c(0,1),2)








