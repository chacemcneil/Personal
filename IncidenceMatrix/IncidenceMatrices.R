# Create incidence matrix for finite projection planes
 library(spam)
 library(data.table)
 library(Matrix)
 
 mat0 <- diag(0,4)
 mat1 <- diag(1,4)
 mat2 <- mat1[,c(2:4,1)]
 mat3 <- mat1[,c(3:4,1:2)]
 mat4 <- mat1[,c(4,1:3)]
 mat5 <- mat1[,c(1,4:2)]
 mat6 <- mat1[,c(2:1,4:3)]
 mat7 <- mat1[,c(3:1,4)]
 mat8 <- mat1[,c(4:1)]
 mat9 <- mat1[,c(1,3,2,4)]
 mat10<- mat1[,c(3,1,4,2)]
 mat11<- mat1[,c(4,2,3,1)]
 mat12<- mat1[,c(2,4,1,3)]
 
 line1 <- rep(c(1,0,0,0),4)
 line2 <- rep(c(0,1,0,0),each=4)
 line3 <- rep(c(0,0,1,0),each=4)
 line4 <- rep(c(0,0,0,1),each=4)
 
 line <- rep(c(1,rep(0,4)),each=4)
 
 B1 <- rbind(line1,line2,line3,line4)
 B2 <- rbind(cbind(mat1,mat1,mat1,mat1),cbind(mat1,mat8,mat6,mat3),cbind(mat1,mat6,mat3,mat8),cbind(mat1,mat3,mat8,mat6))
 
 B <- cbind(rbind(mat0,t(B1)),rbind(B1,B2))
 BB <- cbind(c(1,line),rbind(line,B))
 dimnames(BB) <- list(rep("",nrow(BB)),rep("",ncol(BB)))
 #BB
 
 apply(BB,1,sum)
 apply(BB,2,sum)
 
 ol <- matrix(0,nrow(BB),ncol(BB))
 for (i in 1:ncol(BB)) {
   for (j in setdiff(1:ncol(BB),i)) {
     ol[i,j] <- sum(BB[,i]*BB[,j])
   }
 }
 table(ol)
 #ol
 
 createIncidence <- function(p) {
   #p <- 6
   n <- p*(p-1)+1 #?!
   BB <- matrix(0,n,n)
   i <- 1
   zs <- 0
   while (i <= n) {
     j <- 1
     while (j <= n) {
       if(i==1)
         BB[i,j] <- (j<=p)
       else {
         if(j==1)
           BB[i,j] <- (i<=p)
         else
           BB[i,j] <- (sum(BB[1:(i-1),j])<p &   ## limit number of points per line
                         sum(BB[i,1:(j-1)])<p &   ## limit number of points per line
                         sum(apply(BB[1:(i-1),which(BB[i,1:(j-1)]==1),drop=F],2,function(x) {  ### Not sufficient !!
                           #if (i==12 & j==7) browser()
                           sum(x*BB[1:(i-1),j])
                         } ))==0 )
       }
       if(i>p & j>p & (i-1)%%(p-1) + (j-1)%%(p-1) == 0) {
         tmp <- BB[(i-p+2):i,(j-p+2):j]
         if(sum(tmp) < (p-1)) {
           if(sum(tmp)==0) {
             zs <- zs + 1
             cat(paste0("\rZero sum: ",zs,"\ti: ",i,"\tj: ",j))
             if((j-1)%/%(p-1) > 3)
               j <- j-(p-1)
             else if ((i-1)%/%(p-1) > 3) {
               i <- i-(p-1)
               j <- p*(p-1)+1
             }
             else {
               print("Breaking")
               cat("\014")
               Matrix(BB)
               break()
             }
             tmp <- BB[(i-p+2):i,(j-p+2):j]
           }
           ind <- which(tmp==1,arr.ind=T)
           ind <- ind[order(ind[,"row"]),,drop=F]
           i <- i-p+1+ind[nrow(ind),"row"]
           j <- j-p+1+ind[nrow(ind),"col"]
           BB[i,j] <- 0
           cat("\014")
           print(Matrix(BB))
         }
       }
       j <- j+1
     }
     i <- i+1
   }
   dimnames(BB) <- list(1:(p*(p-1)+1),1:(p*(p-1)+1))
   Matrix(BB)
   return(BB)
 }
 
 BB6 <- createIncidence(6)
 BB7 <- createIncidence(7)
 
 image(-t(BB6),ylim=c(1,0),col=gray(seq(0,1,.1)))
 abline(v=(1:31)/6+1/62,h=(1:31)/6+1/62)
 
 
 
 
 ### New process aimed at finding circular lines. (Works but is very slow)
 
 getB <- function(size,full=T,along=1) {
   require(abind)
   G <- list()
   G[[1]] <- diag(1,size+1)
   if(size + full > 1) {
     for (i in 2:(size + (full==T))) {
       G[[i]] <- G[[1]] + G[[i-1]][c(2:(size+1),1),]
     }
   }
   B <- do.call(abind,c(G,along=along))
   return(unique(B))
 }
 getupperB <- function(size,full=T) {
   G <- list()
   if(size > 0) {
     G[[1]] <- diag(1,size+1)
     if(size > 1) {
       for (i in 2:(size)) {
         G[[i]] <- ifelse((col(G[[1]]) - row(G[[1]])) > -1 & (col(G[[1]]) - row(G[[1]])) < i,1,0)[1:(size-i+2),]
       }
     }
   }
   B <- do.call(rbind,G)
   if(full)
     B <- rbind(B,1)
   return(B)
 }
 getendsB <- function(size) {
   G <- diag(1,size)
   G <- ifelse(row(G) >= col(G),1,0)
   return(rbind(cbind(G,0),cbind(0,t(G))))
 }
 getlen <- function(size) return(size*(size+1)+1)
 
 getOptions <- function(used,N,removeneg=F) {
   # Gives values not included in prior sums
   B <- getupperB(length(used)-1)
   sums <- B%*%used
   if(max(table(c(sums,N-sums)))>1 | sum(used) > N)
     return(NULL)
   if(removeneg)
     sums <- c(sums,N-sums)
   if(sum(sums>N)>0)
     print(paste("Greater than N",paste(used,collapse=" ")))
   if(sum(used)>N)
     print(paste("Used are greater than N",paste(used,collapse=" ")))
   opts <- setdiff(1:N,sums)
   return(opts)
 }
 getInevitable <- function(used,N) {
   # Gives values not included in prior sums, and restricts based on needed number of remaining terms (!?!)
   B <- getendsB(length(used)-1)
   inev <- B%*%used +N-sum(used)
   return(c(inev,N-sum(used)))
 }
 incr <- function(vars,ind,N) {
   n <- length(vars)
   if(ind==1) {
     print("Index 1 reached")
     return(NULL)
   }
   bad <- F
   opts <- getOptions(vars[0:(ind-1)],N)
   if(is.null(opts))
     bad <- T
   else {
     if(ind==n) {
       if(N-sum(vars) %in% opts)
         opts <- N-sum(vars)
       else
         opts <- numeric()
     }
     if(length(opts)==0 | sum(opts > vars[ind])==0)
       bad <- T
     else
       vars[ind] <- opts[min(which(opts > vars[ind]))] 
   }
   while(bad) {
     cat(paste("\r",paste(vars,collapse=" ")))
     vars[ind:n] <- 0
     vars <- incr(vars,ind-1,N)
     if(is.null(vars))
       return(NULL)
     opts <- getOptions(vars[0:(ind-1)],N)
     inev <- getInevitable(vars[0:(ind-1)],N)
     if(sum(! inev %in% opts)>0)
       next
     if(ind==n) {
       if((N-sum(vars)) %in% opts)
         opts <- N-sum(vars)
       else
         opts <- numeric()
       inev <- numeric()
     }
     else {
       opts <- opts[opts <= N - sum(vars)-sum(opts[1:(n-ind)])] 
     }
     if (ind==3)
       browser()
     if(length(opts)>0) {
       bad <- F
       vars[ind] <- opts[1]
     }
   }
   if(ind==n) cat(paste("\r",paste(vars,collapse=" ")))
   return(vars)
 }
 findseq <- function(size,findone=T,track=T,record=T,vars=NULL) {
   N <- size*(size+1)+1
   B <- getB(size)
   #vars <- 1:size
   #vars[size+1] <- N-sum(vars)
   if(is.null(vars)) {
     vars <- c(1,rep(0,size))
     for (i in 1:size)
       vars <- incr(vars,i+1,N)
   }
   sol <- NULL
   tries <- NULL
   errors <<- combos <<- zeros <<- 0
   seps <- sapply(sapply(1:10,rep,x=" "),paste,collapse="")
   done <- F
   #browser()
   while(!done) {
     combos <<- combos + 1
     if(length(unique(B%*%vars))==N) {
       print(paste("Solution: ",paste(vars,collapse=" ")))
       sol <- rbind(sol,vars)
       if(findone)
         done <- T
     }
     else 
       vars <- incr(vars,size+1,N)
     if(is.null(vars))
       break
     if(sum(vars) != N)
       errors <<- errors+1
     if(tail(vars,1)==0)
       zeros <<- zeros + 1
     if(track)
       cat(paste("\r",paste(vars,collapse=" "),seps[nchar(N)*size-sum(nchar(vars))],
                 "Sum: ",sum(vars),seps[nchar(N)*2-nchar(sum(vars))],
                 "\tTries: ",combos,seps[nchar(N)*3-nchar(sum(combos))],
                 "\tErrors: ",errors,seps[nchar(N)*3-nchar(sum(errors))],
                 "\tZeros: ",zeros,seps[nchar(N)*3-nchar(sum(zeros))]
                 ))
     if(record)
       tries <- rbind(tries,vars)
     if(is.null(vars)) {
       done <- T
       print("End of sequences")
     }
   }
   if(is.null(sol))
     print("No solution found")
   return(sol)
 }
 
 diffs <- findseq(7,vars=c(1,2,10,15,4,7,9,5))
 diffs <- findseq(11,vars=c(1,2,4,8,10,9,10,8,9,11,17,24))
 diffs2 <- findseq(12)
 
 getMatrix <- function(diffs) {
   # Is there a way to do this with a matrix operation?
   ind <- cumsum(diffs)
   n <- length(diffs) - 1
   N <- n*(n+1)+1
   mat <- matrix(0,N,N)
   for (i in 1:N)
     mat[i,(ind+i-2) %% N + 1] <- 1
   return(mat)
 }
 
 mat <- getMatrix(diffs[1,])
 
 testIncidence <- function(mat) {
   mat <- as.matrix(mat)
   #p <- (1+sqrt(4*nrow(mat)-3))/2
   ol <- matrix(0,nrow(mat),ncol(mat))
   for (i in 1:ncol(mat)) {
     for (j in setdiff(1:ncol(mat),i)) {
       ol[i,j] <- sum(mat[,i]*mat[,j])
     }
   }
   prod <- ol
   cat("Column inner products\n")
   print(bin <- table(prod))
   cat("Row sums")
   print(rw <- table(apply(mat,1,sum)))
   cat("Column sums")
   print(cl <- table(apply(mat,2,sum)))
   print(ifelse(bin["0"]==nrow(mat) & length(bin)==2 & length(rw)==1 & length(cl)==1,"Looks good","Problem here"))
 }
 testIncidence(mat)
 
 
 # Generating vectors of points
 vecs <- list(1,
              c(1,3),
              c(1,3,7),
              c(1,3,9,13),
              c(1,4,14,16,21),
              c(1,3,8,12,18,31),
              NA,
              c(1,3,13,32,36,43,52,57),
              c(1,3,7,15,31,36,54,63,73),
              c(1,3,9,27,49,56,61,77,81,91),
              NA,
              c(1,3,12,20,34,38,81,88,94,104,109,133))
 difs <- lapply(vecs,function(x) diff(c(0,x)))
 As <- lapply(difs[-1],makeA)
 sapply(As,function(x) max(table(x[-nrow(x),])))  # Check
 
 ## SumMatrix (Is there a metric that can be optimized (in less than N^n time))
 library(abind)
 n=8
 N <- n*(n+1)+1
 mat1 <- rbind(cbind(-1,diag(1,n)[,n:1]),0)
 S <- diag(1,n+1)[,c(2:(n+1),1)]
 pow <- function(A,n) {
   B <- diag(1,nrow(A))
   if(n>0)
     for (i in 1:n)
       B <- B%*%A
   return(B)
 }
 SS <- do.call(abind,c(lapply(0:n,pow,A=S),along=3))
 mSS <- do.call(abind,c(lapply(1:dim(SS)[3],function(i) mat1%*%SS[,,i]),along=3))
 add <- function(Arr,vec,along=1:(length(dim(Arr))-1)) {
   if (length(dim(Arr))==2)
     return(Arr%*%vec)
   else
     return(apply(Arr,MARGIN=along,function(A) A%*%vec))
 }
 # Find vector a 
 # Calculate test statistic
 stat <- matrix(0,N,n)
 stat <- array(0,c(N,n,))
 for (i in 1:N)
   for (j in 1:n) {
     tab <- table((A + i*mSS[,,j])[-(n+1),] %% N)
     tab2 <- table(tab)
     stat[i,j] <- sum((as.numeric(names(tab2))-1)^2*tab2)
     #stat[i,j] <- sum(table(A + i*mSS[,,j])-1)
   }
 image(stat)
 stat
 
 stat1 <- stat2 <- numeric(100)
 for (i in 1:100) {
   a <- sample(0:20,n+1,prob=c(21:1)^4,replace=T)
   stat1[i] <- sum(a)
   tab <- table((A + add(mSS,a))[-(n+1),] %% N)
   tab2 <- table(tab)
   stat2[i] <- sum((as.numeric(names(tab2))-1)^2*tab2)
 }
 plot(stat1,stat2)
 makeA <- function(vec) {
   n <- length(vec)-1
   B <- getB(n,along=3)
   A <- add(B,vec,along=c(1,3))
   return(t(A))
 }
 
 n=4
 N <- n*(n+1)+1
 opts <- as.matrix(expand.grid(2:(N-1),2:(N-1),2:(N-1)))
 opts <- opts[!apply(opts,1,is.unsorted,strict=T),]
 #opts <- opts[opts[,1]+opts[,2]==N-1,]
 arr <- array(0,c(nrow(opts),n+1,n+1))
 for(i in 1:nrow(opts)) {
   arr[i,,] <- makeA(c(1,diff(c(1,opts[i,],N))))
 }
 stat3 <- numeric(nrow(opts))
 for(i in 1:nrow(opts)) {
   tab <- table(arr[i,,][-(n+1),])
   tab2 <- table(tab)
   stat3[i] <- sum((as.numeric(names(tab2))-1)^2*tab2^2)
 }
 cbind(opts,stat3)
 
 
 
 
 
 ### Bumping algorithm (not finished)
 n = 8
 N <- n*(n+1)+1
 pts <- c(0:n,N)
 tab <- table(getB(n)%*%diff(pts))
 tail(which(tab==max(tab)),1)
 
 B <- getB(n)
 ends <- getEnds(n)
 
 ind <- 0
 while(!done) {
   ind <- (ind %% n) + 1
   #lens <- B%*%diff(pts)
   #tab <- table(lens)
   #lens2 <- as.character(lens[unique(which(ends==ind,arr.ind=T)[,1])])
   #unique(tab[lens2])
   
   diffs <- diff(pts)
   opts <- getOpts(diffs[c((ind+2):(n+1),1:(ind-1))])
   
 }
 
 getOpts <- function(used,N) {
   # Gives values not included in prior sums
   B <- getupperB(length(used)-1)
   opts <- setdiff(1:N,((B%*%used - 1) %% N)+1)
   tot <- sum(used)
   opts <- opts[opts < N-tot]
   return(opts)
 }
 
 getEnds <- function(n) {
   g <- c(n,1:(n-1))
   G <- matrix(0,n,n)
   cbind(g,as.vector((((row(G) - col(G)) %% n) + 1)[,c(1,rev(3:n))]))
 }
 
 
 
# End script
 
 