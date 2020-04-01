# Gate mathematical operations
 library(data.table)
 library(bitops)
 
 # Gates
 and <- get("*")
 or  <- pmax
 not <- function(x) 1-x
 
 nand <- function(x,y) (1-x)*(1-y)
 nor  <- function(x,y) pmax(1-x,1-y)
 xor <- function(x,y) as.numeric(get("!=")(x,y))
 xnor<- function(x,y) as.numeric(get("==")(x,y))
 
 ## Bit operations
 
 # Addition
 add <- data.table(expand.grid(1:0,1:0,1:0)[,3:1])
 setnames(add,c("One","Two","Carried"))
 add[,carry :=or(and(One,or(Two,Carried)),and(not(One),and(Two,Carried)))]
 add[,result:=or(and(One,xnor(Two,Carried)),and(not(One),xor(Two,Carried)))]
 add
 
 # Subtraction
 sub <- data.table(expand.grid(1:0,1:0,1:0)[,3:1])
 setnames(sub,c("One","Two","Borrowed"))
 sub[,borrow:=or(and(not(Borrowed),and(not(One),Two)),and(Borrowed,or(not(One),Two)))]
 sub[,result:=or(and(not(Borrowed),xor(One,Two)),and(Borrowed,xnor(One,Two)))]
 sub
 
 
 ## Full operations
 
 # Addition
 addBin <- function(one,two,reversed=F,print=F) {
   if(!reversed) {
     # Reverse bits so that the ones place is first
     one <- rev(one)
     two <- rev(two)
   }
   n <- max(length(one),length(two))+1
   one <- c(one,rep(0,n-length(one)))
   two <- c(two,rep(0,n-length(two)))
   car <- numeric(n) # carried vector
   res <- numeric(n) # result vector
   car[1] <- 0
   for (i in 1:(n-1)) {
     res[i]   <- or(and(one[i],xnor(two[i],car[i])),and(not(one[i]),xor(two[i],car[i])))
     car[i+1] <- or(and(one[i],or(two[i],car[i])),and(not(one[i]),and(two[i],car[i])))
   }
   res[n] <- car[n]
   if (print)
     cat(paste0(paste(rev(one),collapse=""),"\n",paste(rev(two),collapse=""),"\n",paste(rev(res),collapse=""),"\n"))
   if(res[n]==1)
     return(rev(res))
   else
     return(rev(res[-n]))
 }
 set.seed(112)
 x <- rbinom(50,1,.5)
 y <- rbinom(80,1,.5)
 identical(conv(x)+conv(y),conv(addBin(x,y,print=T))) # binary sum is correct though
 set.seed(7133)
 x <- rbinom(210,1,.5)
 y <- rbinom(180,1,.5)
 identical(conv(x)+conv(y),conv(addBin(x,y,print=T))) # binary sum is correct
 
 # Convert binary vector to number
 conv <- function (x) {
   twos <- 2^((length(x):1)-1)
   return(sum(twos*x))
 }
 
 # Greater Than
 grtBin <- function(one,two,reversed=F,print=F,orequal=T) {
   # Check that the number represented by one is greater than that represented by two 
   if(reversed) {
     # Reverse bits so that the ones place is first
     one <- rev(one)
     two <- rev(two)
   }
   n <- max(length(one),length(two))+1
   one <- c(rep(0,n-length(one)),one)
   two <- c(rep(0,n-length(two)),two)
   
 }
 
 # Subtraction
 subBin <- function(one,two,reversed=F,print=F) {
   if(!reversed) {
     # Reverse bits so that the ones place is first
     one <- rev(one)
     two <- rev(two)
   }
   n <- max(length(one),length(two))
   one <- c(one,rep(0,n-length(one)))
   two <- c(two,rep(0,n-length(two)))
   bor <- numeric(n) # borrowed vector
   res <- numeric(n) # result vector
   bor[1] <- 0
   for (i in 1:n) {
     res[i]   <- or(and(not(bor[i]),xor(one[i],two[i])),and(bor[i],xnor(one[i],two[i])))
     bor[i+1] <- or(and(not(bor[i]),and(not(one[i]),two[i])),and(bor[i],or(not(one[i]),two[i])))
   }
   if(bor[n+1]!=0)
     print("First number is smaller than second")
   if (print)
     cat(paste0(paste(rev(one),collapse=""),"\n",paste(rev(two),collapse=""),"\n",paste(rev(res),collapse=""),"\n"))
   return(rev(res))
 }
 
 set.seed(3102)
 x <- rbinom(70,1,.5)
 y <- rbinom(60,1,.5)
 identical(conv(x)-conv(y),conv(subBin(x,y,print=T))) # binary subtraction is correct though
 set.seed(381)
 x <- rbinom(210,1,.5)
 y <- rbinom(180,1,.5)
 identical(conv(x)-conv(y),conv(subBin(x,y,print=T))) # binary subtraction is correct
 
 
 # Division HARD!
 
 
 ## Creat igraph schem (can't be done easily)
 
 # division does not solve it
 # just by information, it should be doable: 1000 bits of information, there are fewer than 2^1000 possibilities
 
 
 # Test of speed for cycling through 2^1000 possibilities
 i=0
 res <- rep(0,1000)
 while(res[1]==0) {
   i <- i+1
   res <- addBin(res,1)
   cat(paste0("\rIter: ",i))
 }
 
 
 dt <- do.call(data.table,lapply(1:1000,function(n) c(rep(0,n-1),1)))
 image(as.matrix(dt)[,-1])
 
 
 
 
# End script
 