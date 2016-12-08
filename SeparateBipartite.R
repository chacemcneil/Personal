# Finding partition in semi-bipartite graph
 library(data.table)
 library(Matrix)
 
 n <- 1e3
 
 # Create partition
 x <- rbinom(n, 1, .45) + 1
 table(x)
 
 grd <- expand.grid(x = x, y = x)
 dim(grd)
 A <- matrix(rbinom(n^2, 1, ifelse(grd$x == grd$y, ifelse(grd$x == 1, .5, .5), .4)), ncol = n, nrow = n)
 image(A)
 
 egn <- eigen(t(A)%*%A)
 plot(sort(egn$vector[, 2]), type = "l")
 
 
 mean(A[x==1, x==1]); mean(A[x==2, x==1]); mean(A[x==2, x==2])
 
 image(A[order(egn$vector[, 1]), order(egn$vector[, 1])])
 image(A[order(egn$vector[, 2]), order(egn$vector[, 2])])
 image(A[order(egn$vector[, 1] + egn$vector[, 2]), order(egn$vector[, 1] + egn$vector[, 2])])
 
 
 table(sign(egn$vector[, 2]), x)
 hist(egn$vector[, n-1])
 
 
 
 
# End script
 