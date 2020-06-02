# Improvement on GCD and LCM calculations
 library(numbers)
 
 # Overwrite numbers::GCD
 GCD <- function (...) {
   mat <- do.call(cbind,list(...))
   if(ncol(mat)==1)
     mat <- t(mat)
   apply(mat,1,function(nums) {
     facts <- lapply(nums,primeFactors)
     primes <- sort(unique(unlist(facts)))
     mat2  <- sapply(nums,function(n) {
       table(factor(primeFactors(n),levels=primes))
     })
     if(is.null(dim(mat2)))
       mat2 <- matrix(mat2,nrow=1)
     prod(primes^apply(mat2,1,min))
   })
 }
 
 # Overwrite numbers::LCM
 LCM <- function (...) {
   mat <- do.call(cbind,list(...))
   if(ncol(mat)==1)
     mat <- t(mat)
   apply(mat,1,function(nums) {
     facts <- lapply(nums,primeFactors)
     primes <- sort(unique(unlist(facts)))
     mat2  <- sapply(nums,function(n) {
       table(factor(primeFactors(n),levels=primes))
     })
     if(is.null(dim(mat2)))
       mat2 <- matrix(mat2,nrow=1)
     prod(primes^apply(mat2,1,max))
   })
 }
 
 relPrimes <- function(n) {
   which(GCD(1:n,n)==1)
 }
 
 
 
 
 
# End script
 