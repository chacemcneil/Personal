# Find new base representation when each place holder is different than an additional power
 library(data.table)
 library(ggplot2)
 library(grid)
 library(gridExtra)
 library(number)
 
 facts <- cumprod(Primes(100))
 
 newBase <- function (num,sequence = Primes(100),characters=c(0:9,LETTERS,letters)) {
   facts <- cumprod(sequence)
   maxdigits <- as.numeric(cut(max(num),facts,right=F))
   mat <- NULL
   for (i in 1:maxdigits) {
     tmp <- num %/% facts[maxdigits-i+1]
     mat <- cbind(mat,characters[tmp+1])
     num <- num - tmp * facts[maxdigits-i+1]
   }
   mat <- cbind(mat,num)
   colnames(mat) <- rep("",ncol(mat))
   apply(mat,1,paste,collapse="")
 }
 
 interpret <- function(chars,base=10,characters=c(0:9,LETTERS,letters),matrix=F) {
   if(length(base)==1)
     base <- base^(1:max(nchar(chars))-1)
   else
     base <- base[1:max(nchar(chars))]
   if(matrix)
     sapply(strsplit(chars,""),match,characters)-1
   else
     sapply(strsplit(chars,""),function(x) {
       sum(rev(base)*(match(x,characters)-1))
     })
 }
 
 N  <- facts[7]
 dt <- data.table(Number=c(1:100,(1:N)*100 + 421))
 dt <- data.table(Number=1:N)
 dt[,Prime:=isPrime(Number)]
 dt[,New:=newBase(Number)]
 
 
 
 
 grid.arrange(ggplot(dt[Number < facts[7]], aes(Number, interpret(New))) + geom_point(),
              ggplot(dt[Number < facts[6]], aes(Number, interpret(New))) + geom_point(),
              ggplot(dt[Number < facts[5]], aes(Number, interpret(New))) + geom_point(),
              ggplot(dt[Number < facts[4]], aes(Number, interpret(New))) + geom_point(),
              ggplot(dt[Number < facts[3]], aes(Number, interpret(New))) + geom_point(),
              ggplot(dt[Number < facts[2]], aes(Number, interpret(New))) + geom_point() )
 
 ggplot(dt[Number<5000],aes(Number,interpret(New)/Number)) + geom_point()
 ggplot(dt,aes(Number,interpret(New)/Number)) + geom_point()
 ggplot(dt,aes(log(Number),interpret(New)/Number)) + geom_point()
 
 
 ggplot(dt[Prime==T][1:1e3], aes(Number, interpret(substr(New,5,5)))) + geom_line()
 ggplot(dt[Prime==T][1:500], aes(Number, interpret(substr(New,6,6)))) + geom_line()
 ggplot(dt[Prime==T][1:100], aes(Number, interpret(substr(New,7,7)))) + geom_line()
 
 dt[Prime==T,pacf(interpret(substr(New,5,5)))]
 dt[Prime==T,pacf(interpret(substr(New,6,6)))]
 dt[Prime==T,pacf(interpret(substr(New,7,7)))]
 dt[Prime==T,acf(interpret(substr(New,5,5)))]
 dt[Prime==T,acf(interpret(substr(New,6,6)))]
 dt[Prime==T,acf(interpret(substr(New,7,7)))]
 
 
 # Are there any patterns in the data
 
 
 
 
 
# End script
 