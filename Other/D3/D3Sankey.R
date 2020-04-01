# Build basic D3 Sankey diagram
 library(data.table)
 library(igraph)
 library(rjson) # Does not work if package RJSONIO is loaded
 
 d3init <- function() 
   cat('<meta charset="utf-8"><script src="http://d3js.org/d3.v3.min.js"></script>')
 
 FUNs <- list("+"=function(lst) paste("(",lst[[1]],"+",lst[[2]],")"),
              "-"=function(lst) ifelse(length(lst)==1, paste("(-",lst[[1]],")"),paste("(",lst[[1]],"-",lst[[2]],")")),
              "*"=function(lst) paste("(",lst[[1]],"*",lst[[2]],")"),
              "/"=function(lst) paste("(",lst[[1]],"/",lst[[2]],")"),
              "^"=function(lst) paste("Math.pow(",lst[[1]],",",lst[[2]],")"),
              "sqrt"=function(lst) paste("Math.sqrt(",lst[[1]],")"),
              "log"=function(lst) paste("Math.log(",lst[[1]],")"),
              "log10"=function(lst) paste("Math.log10(",lst[[1]],")"),
              "("=function(lst) paste("(",lst[[1]],")"))
 makeD3function <- function(expr,colclasses="char") {
   jsexpr <- recurseJS(expr[[2]],colclasses)
   str <- paste("function(d) { return (",jsexpr,"); }")
   return(str)
 }
 recurseJS <- function(expr,colclasses="char") {
   #Iterate through call_tree
   if(length(expr)==0) return("")
   if(class(expr)=="name")
     return(paste0("d.",expr))
   op <- expr[[1]]
   if(class(op)=="character") return(paste0("\"",op,"\""))
   if(mode(op)=="numeric") return(op)
   if(class(op) %in% c("name","call")) {
     if(as.character(op) %in% names(FUNs))
       return(FUNs[[as.character(op)]](lapply(expr[-1],recurseJS)))
   }  
 }
 
 myFormat <- function(lst,colclasses="char") {
   lst <- lapply(lst,function(x) {
     if (class(x)=="character")
       return(paste0("\"",x,"\""))
     if (class(x)=="numeric")
       return(x)
     if (class(x)%in%c("formula","call","name"))
       return(makeD3function(x,colclasses))
   })
   return(lst)
 }
 
 mySankey <- function(nodes,links,Source="source",Target="target",ID="id",) {
   
   
   class(html) <- c("html","character")
   html
 }
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
# End script
 