# Creating a new class


vir <- setClass("binary",slots=list(fun="function"),contains="numeric")

this <- new("binary",1,fun=sum)
that <- new("binary",1)
this + 2
is(this)

envira.print <- function(vir) {
  as.numeric(vir)
}

setMethod("print","binary",function(x,...) print(as.numeric(x)))
setMethod("cat","binary",function(x,...) cat(as.numeric(x)))
this
print(this)
as.numeric(this)


add <- function(x,y=1) {
  return(x+y)
}

setMethod("mode","envira",function(x) return("numeric"))
mode(this)



# Create new operators for the class
this <- rbinom(10,1,.5)
that <- rbinom(10,1,.5)
class(this) <- c("binary","numeric")

assign("+.binary",function(x,y=NULL) {
  if(is.null(y))
    return(x)
  else
    return(as.binary(pmax(x,y)))
})
as.binary <- function(x) {
  class(x) <- c("binary","numeric")
  return(x)
}

this + that
(df <- data.frame(this,that,this+that,that+this))
sapply(df,class)


