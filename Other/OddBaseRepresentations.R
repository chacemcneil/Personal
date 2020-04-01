# Odd base numbers.
 library(data.table)
 
 
 basemult <- function(x,chars=c(0:9,LETTERS),bases=2:100,returnmat=F) {
   tmpx <- x
   maxx <- max(x)
   maxdigits <- 1
   while(maxx >= prod(bases[1:maxdigits])) {
     maxdigits <- maxdigits + 1
   }
   
   mat <- matrix(0,nrow=length(x),ncol=maxdigits)
   for (i in 1:ncol(mat)) {
     mat[,i] <- chars[tmpx %/% prod(head(bases,ncol(mat)-i)) + 1]
     tmpx <- tmpx %% prod(head(bases,ncol(mat)-i))
   }
   if(!returnmat)
     mat <- apply(mat,1,paste,collapse="")
   mat
 }
 
 library(numbers)  # function isPrime
 
 N <- 500
 x <- 1:N
 dt <- data.table(Num=x,
                  ContBase=basemult(x),
                  PrimeBase=basemult(x,bases=Primes(100)),
                  SquareBase=basemult(x,bases=(Primes(100)/c(1,Primes(96)))^2,returnmat=T),
                  IsPrime=isPrime(x))
 
 dt[IsPrime==T]
 
 
 ,bases=2:100                            # New digit at each factorial
 ,bases=Primes(100)                      # New digit at 2, 2*3, 2*3*5, 2*3*5*7, ...
 ,bases=(Primes(100)/c(1,Primes(96)))^2  # New digit at prime squares 2^2, 3^2, 5^2, ...
 
 
 
 
 
 
 
 
# End script
 