# Create work area
 library(data.table)
 library(rjson)
 
 setwd("C:/Users/cmcneil/Documents/Projects/Miscellaneous/Personal/D3Test/Version2")
 
 rows = 30
 cols = 40
 cellwidth = cellheight = 25
 blktmp <- data.table(expand.grid(Row=0:rows,Col=0:cols))[Row %in% c(0,rows) | Col %in% c(0,cols)]
 blktmp <- blktmp[,list(x=Col*cellwidth + cellwidth/2, y=Row*cellheight + cellheight/2, xvel=0, yvel=0, direc=0,
                        grav=0, fric=0, bnce=0, mov=0, drag=0, type="rect", hw=cellwidth/2, hh=cellheight/2, r=NA, xs=NA, ys=NA,
                        color=ifelse(Row %in% c(0,rows) & Col < 12,"#FFFFDD","#DDFFFF"),
                        class=ifelse(Row %in% c(0,rows) & Col < 12,"Trmp","Block") )]
 
 blltmp <- data.table(x=(1:20)*40,y=20+5*(1:100),xvel=-1:3,yvel=0,direc=0,grav=c(-.1,.1,.1,.1),fric=0,bnce=.95,mov=0,drag=0,type=c("rect","circ"),
                      hw=cellwidth/4,hh=cellheight/4,r=cellheight/5, xs=NA, ys=NA,color="#FFEEFF",class="Bll")
 
 blltmp <- data.table(x=390,y=c(40,20,60,80),xvel=0,yvel=0,direc=0,grav=.1,fric=0,bnce=.95,mov=0,drag=0,type="circ",
                      hw=cellwidth/4,hh=cellheight/4,r=cellheight/2, xs=NA, ys=NA,color="#FFEEFF",class="Bll")
 
 rm1 <- rbind(blktmp,blltmp)
 
 write(paste0("objects = ",toJSON(rm1),";"),file="rm1.js")
 
 
 
 
 
 
 
# End script
 