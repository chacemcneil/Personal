## New number formatting technique, adding class: 
## Desired functionality:
##   Class of numbers that can be used for numerical calculations, but that format nicely when printed
##   Cooperates with other packages and does make standard code break or look awkward
##   Adds a class type to each variable. Adds attributes which define print formatting.
##   Attributes are given for:
##     Scientific notation
##     Currency
##     Rounding decimals
##     Non-breaking spaces and hyphens (for text variables, useful with HTML output)
##     Bold/italics/underline/color/size (style elements for HTML)




 library(data.table)
 library(htmlTable)
 library(ggplot2)
 
 a.n <- function(obj) {
   as.numeric(obj)
 }
 
 nicenum <- function(obj, numtype = "real", prec = NULL, ...) {
   require(CMcCode)
   
   attrs <- list(...)
   
   if(!is.numeric(obj)) {
     warning("Cannot convert non-numeric object to nucenum.")
     return(obj)
   }
   
   if(is.null(prec))
     prec <- switch(numtype, real = 2, dollar = 2, int = 0, sci = 4)
   
   if(is.null(attrs$prefix))
      attrs$prefix <- ifelse(numtype == "dollar", "$ ", "")
   if(is.null(attrs$big.mark))
     attrs$big.mark <- ","
   if(is.null(attrs$scientificf))
     attrs$scientific = numtype == "sci"
   if(is.null(attrs$nsmall))
     attrs$nsmall = prec + attrs$scientific
   if(is.null(attrs$digits))
     attrs$digits = prec + attrs$scientific
   attrs$numtype = numtype
   attrs$prec = prec
   
   attributes(obj) <- attrs
   class(obj) <- c("nicenum", class(obj))
   
   obj
 }
 
 print.nicenum <- function(x) {
   
   attr <- attributes(x)
   
   ret <- do.call(format, c(list(x = x), attr))
   ret <- paste0(attr$prefix, ret, attr$suffix)
   ret <- gsub("e", " e", ret)
   
   print(ret, quote = F)
 }
 as.character.nicenum <- function(x) {
   
   attr <- attributes(x)
   
   ret <- do.call(format, c(list(x = x), attr))
   ret <- paste0(attr$prefix, ret, attr$suffix)
   ret <- gsub("e", " e", ret)
   
   ret
 }
 
 x <- 45.54321
 y0 <- nicenum(x)
 y1 <- nicenum(x, numtype = "dollar")
 y2 <- nicenum(x, numtype = "sci")
 y3 <- nicenum(x, numtype = "int")
 y4 <- nicenum(x, numtype = "real", prec = 3)
 
 ## This part is not working ...
 x <- rnorm(5, 5000, 1e3)
 dt <- data.table(x = x,
                  y1 = nicenum(x, numtype = "dollar"),
                  y2 = nicenum(x, numtype = "sci"),
                  y3 = nicenum(x, numtype = "int"),
                  y4 = nicenum(x, numtype = "real", prec = 3))
 print(dt)
 htmlTable(dt, rnames = F)
 
# End script
 