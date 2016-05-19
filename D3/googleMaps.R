# Functions for plotting data on maps using plotGoogleMaps
# Attempt to write functions to use ggplot syntax
 library(data.table)
 library(plotGoogleMaps)
 
 
 header <- function(styles=NULL) {
   html <- paste('<meta name="viewport" content="initial-scale=1.0, user-scalable=no"/>',
                 '<script type="text/javascript" src="http://maps.google.com/maps/api/js?sensor=true"></script>',
                 '<script type="text/javascript" src="http://square.github.io/crossfilter/d3.v3.min.js"></script>',
                 '<script type="text/javascript" src="http://code.jquery.com/jquery-1.8.3.min.js"></script>',sep="\n")
   if (!is.null(styles)) {
     strings <- paste(sapply(seq_along(styles),function(i) {
                        paste(sapply(seq_along(styles[[i]]),function(j) {
                                paste0(".",names(styles)[i]," ",names(styles[[i]])[[j]]," {\n",paste("  ",names(styles[[i]][[j]]),": ",styles[[i]][[j]],";",sep="",collapse="\n"),"\n}")
                              }),
                              collapse="\n\n")
                      }),
                      collapse="\n\n")
     html <- paste(html,"\n\n",strings)
   }
   html
 }
 
 body <- function() {
   
 }
 
 
 
 lst <- list(stations=list(svg=list(width="60px",height="20px"),circle=list(fill="red",stroke="black"),"circle:hover"=list(fill="blue")),marker_text=list(fill="black",visibility="hidden"))
 
 
 
 
 
# End script
 