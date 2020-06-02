# Compression attempt for large sankey file.
 library(data.table)
 library(ngram)
 library(tm)
 library(stylo)
 
 
 chars  <- strsplit(rawToChar(as.raw(1:255)),"")[[1]]
 chars2 <- strsplit(rawToChar(as.raw(c(33:126,128:255))),"")[[1]]
 
 
 length(chars)
 length(chars2)
 
 dat <- readLines("ProviderNetworkFull.html")
 dat2 <- paste(c(dat),collapse="\n")
 dat2 <- gsub(" ","|",dat2)
 
 dat2e <- strsplit(dat2,"")[[1]]
 unique(dat2e)
 
 chars3 <- chars2[!chars2 %in% dat2e]
 
 phrase <- paste(dat2e,collapse=" ")
 phr <- substr(phrase,1,1e5)
 
 this <- ngram(phr,2)
 
 newdat <- dat2e
 newdatstr <- dat2
 size <- 11
 i <- 1
 thresh <- 2e5
 maptab <- data.table(String="",Char="",Count=0,Order=0)[0]
 while(size > 1) {
   ng <- switch(size-1,
                table(paste0(newdat[-N],
                             newdat[-1])),
                table(paste0(head(newdat,-2),
                             newdat[-c(1,N)],
                             tail(newdat,-2))),
                table(paste0(head(newdat,-3),
                             newdat[-c(1,N-1,N)],
                             newdat[-c(1,2,N)],
                             tail(newdat,-3))),
                table(paste0(head(newdat,-4),
                             newdat[-c(1,N-2,N-1,N)],
                             newdat[-c(1,2,N-1,N)],
                             newdat[-c(1:3,N)],
                             tail(newdat,-4))),
                table(paste0(head(newdat,-5),
                             newdat[-c(1,(N-3):N)],
                             newdat[-c(1:2,(N-2):N)],
                             newdat[-c(1:3,N-1,N)],
                             newdat[-c(1:4,N)],
                             tail(newdat,-5))),
                table(paste0(head(newdat,-6),
                             newdat[-c(1,(N-4):N)],
                             newdat[-c(1:2,(N-3):N)],
                             newdat[-c(1:3,(N-2):N)],
                             newdat[-c(1:4,N-1,N)],
                             newdat[-c(1:5,N)],
                             tail(newdat,-6))),
                table(paste0(head(newdat,-7),
                             newdat[-c(1,(N-5):N)],
                             newdat[-c(1:2,(N-4):N)],
                             newdat[-c(1:3,(N-3):N)],
                             newdat[-c(1:4,(N-2):N)],
                             newdat[-c(1:5,N-1,N)],
                             newdat[-c(1:6,N)],
                             tail(newdat,-7))),
                table(paste0(head(newdat,-8),
                             newdat[-c(1,(N-6):N)],
                             newdat[-c(1:2,(N-5):N)],
                             newdat[-c(1:3,(N-4):N)],
                             newdat[-c(1:4,(N-3):N)],
                             newdat[-c(1:5,(N-2):N)],
                             newdat[-c(1:6,N-1,N)],
                             newdat[-c(1:7,N)],
                             tail(newdat,-8))),
                table(paste0(head(newdat,-9),
                             newdat[-c(1,(N-7):N)],
                             newdat[-c(1:2,(N-6):N)],
                             newdat[-c(1:3,(N-5):N)],
                             newdat[-c(1:4,(N-4):N)],
                             newdat[-c(1:5,(N-3):N)],
                             newdat[-c(1:6,(N-2):N)],
                             newdat[-c(1:7,N-1,N)],
                             newdat[-c(1:8,N)],
                             tail(newdat,-9))),
                table(paste0(head(newdat,-10),
                             newdat[-c(1,(N-8):N)],
                             newdat[-c(1:2,(N-7):N)],
                             newdat[-c(1:3,(N-6):N)],
                             newdat[-c(1:4,(N-5):N)],
                             newdat[-c(1:5,(N-4):N)],
                             newdat[-c(1:6,(N-3):N)],
                             newdat[-c(1:7,(N-2):N)],
                             newdat[-c(1:8,N-1,N)],
                             newdat[-c(1:9,N)],
                             tail(newdat,-10))))
   cat("----")
   if(max(ng) > thresh) {
     str <- tail(names(sort(ng)),1)
     newdatstr <- gsub(str,chars3[i],newdatstr)
     newdat <- strsplit(newdatstr,"")[[1]]
     maptab <- rbind(maptab,data.table(String=str,Char=chars3[i],Count=max(ng),Order=i))
     cat(paste0("Replaced ",max(ng)," occurrences of \"",str,"\" with ",chars3[i],". \n"))
     i <- i+1
   } else {
     size <- size - 1
     cat(paste0("No replacement. Only ",max(ng)," repeated ",size+1,"-grams. Reducing size to ",size,". \n"))
   }
 }
 
 
 ngrams <- list()
 N <- length(dat2e)
 n1 <- table(dat2e)
 n2 <- table(paste0(dat2e[-N],dat2e[-1]))
 n3 <- table(paste0(head(dat2e,-2),dat2e[-c(1,N)],tail(dat2e,-2)))
 n4 <- table(paste0(head(dat2e,-3),dat2e[-c(1,N-1,N)],dat2e[-c(1,2,N)],tail(dat2e,-3)))
 for (i in 2:n) {
   mat <- matrix(dat2e,nrow=length(dat2e)+1,ncol=6)
   table(paste0(mat[,1],mat[,2]))
   ngrams[[i]] <- table(apply(mat,1,paste0))
 }
 
 n1 <- table(dat2e)
 n2 <- table(paste0(dat2e))
 
 
 
 # Need some sort of clustering, need to find common patterns that are repeated frequently (more frequent the better, longer is good too)
 
 # savings equivalent to (length of segment - 1)*(number of occurrences)
 
 
 
 
 