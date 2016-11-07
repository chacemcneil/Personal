# Working with arbitrary precision Scratch code
 library(data.table)
 data.matrix(A)
 
 b <- 20
 n <- 1e4
 A <- matrix(round(runif(n*b, 0, 1e10)), n, b)
 B <- matrix(round(runif(n*b, 0, 1e10)), n, b)
 
 
 
 P <- matrix(0, nrow(A), ncol(A))
 system.time(
 for (i in 1:b) {
   for (j in 1:i) {
     P[, i] <- P[, i] + A[, j]*B[, i-j+1]
   }
 })
 #anynum <- setClass("anynum", "list", prototype = prototype(mat, base = 10, digis = 10))
 #class(A) <- "complex"
 #anynum(A, base = 10L, digits = 10L)
 #new("anynum", slots(A))
 
 
 x <- c("4567898765.000065678", "-8578.00076543", "9876")
 
 makeHP <- function(x, places = 11, digits = 10, base = 10) {
   if(is.matrix(x)) {
     mat <- x
     colnames(mat) <- c("ones", rep("", ncol(x) - 1))
     sign <- sign(mat[,1])
     mode <- ifelse(mode(mat) == "complex", "complex", "numeric")
     if (mode == "complex") {
       matR <- Re(mat)
       matI <- Im(mat)
       signR <- sign(matR[,1])
       signI <- sign(matI[,1])
     }
   } else {
     mode <- "numeric"
     x <- as.character(x)
     if(suppressWarnings (sum(is.na(as.numeric(x))))) {
       if(sum(is.na(as.complex(x))))
         stop("Must be a number. Complex numbers require the real component. (0 + 1i)")
       else
         mode <- "complex"
     }
     if(mode == "complex") {
       x <- gsub("\\s+", "", x)
       
       re <- gsub("(\\d*)?\\K(\\+|\\-)?\\d*\\.?\\d*i$", "", x, perl = T)
       re[re == ""] <- 0
       
       im <- gsub("([^i]*$|[^i]*(?=(\\+|\\-)\\d*\\.?\\d*i$))", "", x, perl = T)
       im <- gsub("^\\+", "", im)
       im <- gsub("(^|\\+|\\-)i", "\\11i", im)
       im <- gsub("i$", "", im)
       im[im == ""] <- 0
       
       signR <- sign(as.numeric(re))
       signR[signR == 0] <- 1
       ones <- abs(as.numeric(strsplit(re, "\\..*$")))
       ones[is.na(ones)] <- 0
       re <- gsub("^(-)?\\d*(\\.|$)", "", re)
       re <- paste0(re, sapply(digits * (places - 1) - nchar(re), function(n) paste0(rep("0", n), collapse = "")))
       matR <- cbind(ones, do.call(rbind, lapply(strsplit(re, "(?<=.)(?=(\\d{10})+$)", perl = T), as.numeric)))
       
       signI <- sign(as.numeric(im))
       signI[signI == 0] <- 1
       ones <- abs(as.numeric(strsplit(im, "\\..*$")))
       ones[is.na(ones)] <- 0
       im <- gsub("^(-)?\\d*(\\.|$)", "", im)
       im <- paste0(im, sapply(digits * (places - 1) - nchar(im), function(n) paste0(rep("0", n), collapse = "")))
       matI <- cbind(ones, do.call(rbind, lapply(strsplit(im, "(?<=.)(?=(\\d{10})+$)", perl = T), as.numeric)))
     } else {
       sign <- sign(as.numeric(x))
       sign[sign == 0] <- 1
       ones <- abs(as.numeric(strsplit(x, "\\..*$")))
       ones[is.na(ones)] <- 0
       x <- gsub("^(-)?\\d*(\\.|$)", "", x)
       x <- paste0(x, sapply(digits * (places - 1) - nchar(x), function(n) paste0(rep("0", n), collapse = "")))
       mat <- cbind(ones, do.call(rbind, lapply(strsplit(x, "(?<=.)(?=(\\d{10})+$)", perl = T), as.numeric)))
     }
   }
   if(mode == "complex")
     result <- list(matR = matR, matI = matI, signR = signR, signI = signI, mode = mode, base=base, digits=digits, places=places)
   else
     result <- list(mat = mat, sign = sign, mode = mode, base = base, digits = digits, places = places)
   class(result) <- "hpn"
   result
 }
 
 print.hpn <- function(obj) {
   #browser()
   if(obj$mode == "complex") {
     resr <- paste0(gsub("1", "", obj$signR), 
                    format(obj$matR[, 1], scientific = F, trim = T), ".", 
                    apply(obj$matR[, -1, drop = F], 1, function(x) {
                      x <- format(x, scientific = F, trim = T)
                      paste(substr(paste0("000000000", x), 
                                   nchar(paste0("000000000", x))-9, 
                                   nchar(paste0("000000000", x))), collapse = "" ) 
                    }) )
     resr <- gsub("(\\.)?0*$", "", resr)
     
     resi <- paste0(ifelse(obj$signI == 1, "+", "-"), 
                    format(obj$matI[, 1], scientific = F, trim = T), ".", 
                    apply(obj$matI[, -1, drop = F], 1, function(x) {
                      x <- format(x, scientific = F, trim = T)
                      paste(substr(paste0("000000000", x), 
                                   nchar(paste0("000000000", x))-9, 
                                   nchar(paste0("000000000", x))), collapse = "" ) 
                    }) )
     resi <- gsub("(\\.)?0*$", "", resi)
     resi <- paste0(resi, "i")
     
     res <- paste0(resr, resi)
   } else {
     res <- paste0(gsub("1", "", obj$sign), 
                   format(obj$mat[, 1], scientific = F, trim = T), ".", 
                   apply(obj$mat[, -1, drop = F], 1, function(x) {
                     x <- format(x, scientific = F, trim = T)
                     paste(substr(paste0("000000000", x), 
                                  nchar(paste0("000000000", x))-9, 
                                  nchar(paste0("000000000", x))), collapse = "" ) 
                   }) )
     res <- gsub("(\\.)?0*$", "", res)
   }
   print(res)
   res
 }
 
 head.hpn <- function(obj) {
   list(mat = head(obj$mat))
 }
 
 # Functions for working with objects
 
 makeComplex.hpn <- function(A) {
   if(A$mode == "numeric")
     A <- list(matR = A$mat, matI = matrix(0, nrow(A$mat), ncol(A$mat)), signR = A$sign, signI = rep(1, length(A$sign)), 
               mode = "complex", base = A$base, digits = A$digits, places = A$places)
   A
 }
 
 assign("+.hpn", function(A, B) {
   if(class(B) != "hpn")
     B <- makeHP(B)
   if(A$base != B$base | A$digits != B$digits | A$places != B$places)
     stop("Format of objects must be the same.")
   if(A$mode != B$mode) {
     A <- makeComplex.hpn(A)
     B <- makeComplex.hpn(B)
   }
   if(A$mode == "complex") {
     res <- A
     res$matR <- A$matR*A$signR + B$matR*B$signR
     res$matI <- A$matI*A$signI + B$matI*B$signI
   } else {
     res <- A
     res$mat <- A$mat*A$sign + B$mat*B$sign
   }
   carry.hpn(res)
 })
 assign("*.hpn", function(A, B) {
   if(class(B) != "hpn")
     B <- makeHP(B)
   if(A$base != B$base | A$digits != B$digits | A$places != B$places)
     stop("Format of objects must be the same.")
   if(A$mode != B$mode) {
     A <- makeComplex.hpn(A)
     B <- makeComplex.hpn(B)
   }
   if(A$mode == "complex") {
     res <- A
     res$matR <- matrix(0, nrow(A$matR), ncol(A$matR))
     res$matI <- matrix(0, nrow(A$matI), ncol(A$matI))
     for (i in 1:A$places) {
       for (j in 1:i) {
         res$matR[, i] <- res$matR[, i] + (A$matR[, j]*A$signR)*(B$matR[, i-j+1]*B$signR) - (A$matI[, j]*A$signI)*(B$matI[, i-j+1]*B$signI)
         res$matI[, i] <- res$matI[, i] + (A$matR[, j]*A$signR)*(B$matI[, i-j+1]*B$signI) + (A$matI[, j]*A$signI)*(B$matR[, i-j+1]*B$signR)
       }
     }
     
   } else {
     res <- A
     res$mat <- matrix(0, nrow(A$mat), ncol(A$mat))
     for (i in 1:A$places) {
       for (j in 1:i) {
         res$mat[, i] <- res$mat[, i] + (A$mat[, j]*A$sign)*(B$mat[, i-j+1]*B$sign)
       }
     }
   }
   carry.hpn(res)
 })
 carry.hpn <- function(A) {
   if(A$mode == "complex") {
     A$matR <- cbind(A$matR[, 1], A$matR[, -1] %% A$base^A$digits) + cbind(A$matR[, -1] %/% A$base^A$digits, 0)
     A$signR <- sign(A$matR[, 1])
     i <- 2
     while(sum(A$signR == 0)) {
       A$signR <- ifelse(A$signR == 0, sign(A$matR[, i]), A$signR)
       i <- i + 1
       if(i > ncol(A$matR))
         A$signR[A$signR == 0] = 1
     }
     A$matR <- A$matR * A$signR
     if(sum(A$matR < 0))
       A$matR <- cbind(A$matR[, 1], A$matR[, -1] %% A$base^A$digits) + cbind(A$matR[, -1] %/% A$base^A$digits, 0)
     
     A$matI <- cbind(A$matI[, 1], A$matI[, -1] %% A$base^A$digits) + cbind(A$matI[, -1] %/% A$base^A$digits, 0)
     A$signI <- sign(A$matI[, 1])
     i <- 2
     while(sum(A$signI == 0)) {
       A$signI <- ifelse(A$signI == 0, sign(A$matI[, i]), A$signI)
       i <- i + 1
       if(i > ncol(A$matI))
         A$signI[A$signI == 0] = 1
     }
     A$matI <- A$matI * A$signI
     if(sum(A$matI < 0))
       A$matI <- cbind(A$matI[, 1], A$matI[, -1] %% A$base^A$digits) + cbind(A$matI[, -1] %/% A$base^A$digits, 0)
     
   } else {
     A$mat <- cbind(A$mat[, 1], A$mat[, -1] %% A$base^A$digits) + cbind(A$mat[, -1] %/% A$base^A$digits, 0)
     A$sign <- sign(A$mat[, 1])
     i <- 2
     while(sum(A$sign == 0)) {
       A$sign <- ifelse(A$sign == 0, sign(A$mat[, i]), A$sign)
       i <- i + 1
     }
     A$mat <- A$mat * A$sign
     if(sum(A$mat < 0))
       A$mat <- cbind(A$mat[, 1], A$mat[, -1] %% A$base^A$digits) + cbind(A$mat[, -1] %/% A$base^A$digits, 0)
   }
   A
 }
 
 A <- res
 A <- makeHP(x <- c(1.00043, -2345.0001, -2345.0001, -.000345, .00012, 1 + 1i))
 B <- makeHP(y <- c(-2.2011, 5432.43204, 12.3043781, 12.00000, -431.2, 2 - 3i))
 A+B
 x+y
 A*B
 x*y
 
#  drop <- function(A, digits = 10, base = 10) {
#    interval <- base^-digits
#    round(A, digits = digits) + ((A %% (interval*base)) / interval)[, ]
#  }
#  simplify <- function(A, interval = 1e-10) {
#    # Carrying and dropping both need to be done. Drop first then carry.
#    
#  }
 
 x <- makeHP(rep(0.0000001, 1000))
 y <- makeHP(rep(0.0000001, 1000))
 
 a <- mpfr(rep(0.0000001, 1000), 20)
 b <- mpfr(rep(0.0000001, 1000), 20)
 
 system.time(
 for (i in 1:1000) {
   (0.0000001*(1:1000)) * (0.0000001*(1:1000))
 } )
 
 system.time(
 for (i in 1:1000) {
   cat("\rIteration: ", i)
   (x+(1:1000)) + (y+(1:1000))
 } )
 
 system.time(
 for (i in 1:1000) {
   cat("\rIteration: ", i)
   (a+(1:1000)) + (b+(1:1000))
 } )
 
 
 
 
 
# End script
 