# Nim 
 library(abind)
 
 n <- 10
 
 arr  <- array(0,dim=c(n+1,n+1,n+1),dimnames=list(0:n,0:n,0:n))
 sums <- array(0,dim=c(n+1,n+1,n+1),dimnames=list(0:n,0:n,0:n))
 for (i in 0:n) {
   for (j in 0:n) {
     for (k in 0:n) {
       # Find number of winning moves
       #arr[i,j,k]  <- d
       # Find Nim sum
       sums[i+1,j+1,k+1] <- nsum(i,j,k)
     }
   }
 }
 
 binarize <- function(num) {
   if(num==0)
     return(0)
   len <- (1+floor(log(num,2)))
   vec <- num %% 2^(len:1) %/% 2^((len-1):0)
   return(vec)
 }
 unbinarize <- function(vec) {
   num <- sum(2^((length(vec):1)-1)*vec)
   return(num)
 }
 binarynimadd <- function(one,two) {
   len <- max(length(one),length(two))
   one <- c(rep(0,len-length(one)),one)
   two <- c(rep(0,len-length(two)),two)
   return(as.numeric((one + two) %% 2))
 }
 nsum <- function(...) {
   nums <- unlist(list(...))
   res <- nums[1]
   if(length(nums)==1)
     return(res)
   for(i in 2:length(nums)) {
     res <- unbinarize(binarynimadd(binarize(res),binarize(nums[i])))
   }
   return(res)
 }
 
 
 
 
 
 
# End script
 