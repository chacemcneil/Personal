# Useful things for markdown files
 library(knitr)
 
 ss_theme <- theme_bw() + theme(panel.border=element_blank(), axis.line=element_line(), panel.grid.major.y = element_line("dashed", size = .5, colour="blue"), plot.margin = unit(c(1, 2, 0.5, 0.5), "lines"))
 
 ss_themeGr <- theme_bw() + theme(panel.border=element_blank(), axis.line=element_line()) + theme(panel.grid.major.y = element_line("dashed", size = .5, colour="grey"), plot.margin = unit(c(1, 2, 0.5, 0.5), "lines"))
 options(width=200,warn=-1)
 
 ssblue   <- "#4F81BD"
 ssyellow <- "#FEBE10"
 ssgreen  <- "#9BBB59"
 ssred    <- "#C0504D"
 ssgray   <- "#A6A6A6"
 
 
 
 # Creating bulleted lists
 html_list <- function(strings,details=NULL,detailpref="- ",kind=c("ul","ol","dl"),type=c(1,"A","a","I","i")) {
   kind <- match.arg(kind)
   type <- match.arg(type)
   str <- paste("<",kind," type=\"",type,"\">\n",paste("<li>",strings,"</li>",sapply(details,function(det) paste("\n<dd>",detailpref,det,"</dd>",sep="",collapse="")),sep="",collapse="\n"),"\n</",kind,">",sep="")
   str
 }
 
 roundNumeric <- function(dt,digits=1) {
   dt <- as.data.table(lapply(dt,function(x) {
     if(class(x)=="numeric") 
       round(x,digits) 
     else 
       x
   }))
 }
 
 
 
 
 
 
 
 
 
 # End script
 