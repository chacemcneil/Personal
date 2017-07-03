# Test for interior points
 library(mgcv)
 
 data(columb.polys)
 x <- seq(7.9,8.7,length=20)
 y <- seq(13.7,14.3,length=20)
 gr <- as.matrix(expand.grid(x,y))
 bnd <- columb.polys[[2]]
 inside <- in.out(bnd,gr)
 plot(gr,col=inside+1)
 lines(bnd)
 
 # in.out calculation from mgcv package
 t0 <- proc.time()
 bnd <- columb.polys[[2]]
 bnd <- bnd[1:47,]
 n <- nrow(bnd)
 #bnd <- bnd[n:1,]
 in.out(bnd,pts)
 proc.time()-t0
 
 
 # My calculation (1)
 t0 <- proc.time()
 bnd <- columb.polys[[2]]
 bnd <- bnd[1:47,]
 n <- nrow(bnd)
 bnd <- bnd[n:1,]
 
 b2 <- as.matrix(dist(bnd))[cbind(2:n,1:(n-1))]^2
 dists2 <- sapply(1:nrow(pts),function(i) apply((t(bnd)-pts[i,])^2,2,sum) ) ## Problem
 vec.x  <- sapply(1:nrow(pts),function(i) sapply(1:n, function(j) bnd[j,1]-pts[i,1]))
 vec.y  <- sapply(1:nrow(pts),function(i) sapply(1:n, function(j) bnd[j,2]-pts[i,2]))
 betas <- acos((dists2[-1,] + dists2[-n,] - b2)/(2*sqrt(dists2[-1,]*dists2[-n,])) )
 betas <- betas - 2*pi*(betas>pi)
 signs <- vec.y[-n,]*(vec.x[-1,]-vec.x[-n,]) - vec.x[-n,]*(vec.y[-1,]-vec.y[-n,])
 signs2 <- signs/abs(signs)
 betas <- betas*signs2
 sums <- zapsmall(apply(betas,2,sum ))
 table(sums)
 proc.time()-t0
 
 # My calculation (2)
 t0 <- proc.time()
 bnd <- columb.polys[[2]]
 bnd <- bnd[1:47,]
 n <- nrow(bnd)
 #bnd <- bnd[n:1,]
 #xp <- pts[,1]
 #yp <- pts[,2]
 diffx <- outer(bnd[,1],pts[,1],get("-"))
 diffy <- outer(bnd[,2],pts[,2],get("-"))
 norms <- sqrt((diffx[-n,]^2+diffy[-n,]^2)*(diffx[-1,]^2+diffy[-1,]^2))
 mats <- diffy[-n,]*diffx[-1,]-diffy[-1,]*diffx[-n,]
 matc <- ((diffx[-1,]^2+diffy[-1,]^2) + (diffx[-n,]^2+diffy[-n,]^2) - ((diffx[-1,]-diffx[-n,])^2+(diffy[-1,]-diffy[-n,])^2))/2
 matc2 <- (diffx[-1,]*diffy[-1,]+diffx[-n,]*diffy[-n,])
 #mat2 <- asin(mats/norms)
 mat2 <- acos(matc/norms)*sign(asin(mats/norms))
 sums2 <- zapsmall(apply(mat2,2,sum))
 table(sums2)
 proc.time()-t0
 
 bad <- ifelse(sums2>0 & sums2 < 6.28,1,0)
 plot(pts,col=bad+1)
 lines(bnd)
 
 # Obtuse angles are the problem
 plot(rbind(bnd[11:12,],pts[200,]),col=c("black","black","green"),pch=19,xlim=c(8.6,8.9),ylim=c(13.9,14.1))
 lines(bnd[10:13,])
 mat3[,200]
 # Look at pt 200 between vertices 11 and 12.
 x0 <- pts[200,1]
 y0 <- pts[200,2]
 x1 <- bnd[11,1]-x0
 y1 <- bnd[11,2]-y0
 x2 <- bnd[12,1]-x0
 y2 <- bnd[12,2]-y0
 
 asin((x1*y2-x2*y1)/sqrt((x1^2+y1^2)*(x2^2+y2^2)))
 asin((y1*(x2-x1) - x1*(y2-y1))/sqrt((x1^2+y1^2)*(x2^2+y2^2)))
 (x1*y2-x2*y1)/sqrt((x1^2+y1^2)*(x2^2+y2^2))
 acos(((x2^2+y2^2) + (x1^2+y1^2) - ((x2-x1)^2+(y2-y1)^2))/(2*sqrt((x1^2+y1^2)*(x2^2+y2^2))) )
 
 
# End script
 