# Create work area
 library(data.table)
 library(rjson)
 
 setwd("C:/Users/cmcneil/Documents/Projects/Miscellaneous/Personal/D3Test/Version2")
 
 rows = 30
 cols = 40
 cellwidth = 25
 cellheight = 25
 blktmp <- data.table(expand.grid(Row=0:rows,Col=0:cols))[Row %in% c(0,rows) | Col %in% c(0,cols)]
 blktmp <- blktmp[,list(x=Col*cellwidth + cellwidth/2, y=Row*cellheight + cellheight/2, xvel=0, yvel=0, direc=0,
                        grav=0, fric=0, bnce=0, mov=0, drag=0, type="rect", hw=cellwidth/2, hh=cellheight/2, r=NA, xs=NA, ys=NA,
                        color=ifelse((Row %in% c(0,rows) & Col < 12) | (Col == cols & Row > 20),"#FFFFDD","#CCFFFF"),
                        class=ifelse((Row %in% c(0,rows) & Col < 12) | (Col == cols & Row > 20),"Trmp","Block") )]
 
 blltmp <- data.table(x=(1:20)*40,y=20+5*(1:100),xvel=rnorm(50),yvel=0,direc=0,grav=c(-.1,.1,.1,.1),fric=0,bnce=.95,mov=0,drag=0,type=c("rect","circ"),
                      hw=cellwidth/4,hh=cellheight/4,r=cellheight/5, xs=NA, ys=NA,color="#FFEEFF",class="Bll")
 
 blltmp <- data.table(x=700 + (0:3)*20,y=c(570,520,560,580),xvel=0,yvel=10,direc=0,grav=.1,fric=0,bnce=.95,mov=0,drag=0,type=c("rect"),
                      hw=cellwidth/2,hh=cellheight/2,r=cellheight/2, xs=NA, ys=NA,color="#FFCCFF",class="Bll")
 
 write(paste0("objects = ",toJSON(rbind(blktmp,blltmp)),";"),file="rm1.js")
 
 # blltmp <- data.table(x=460,y=c(270,520),xvel=0,yvel=10,direc=0,grav=0.1,fric=0,bnce=.95,mov=0,drag=0,type=c("rect"),
 #                      hw=cellwidth/2,hh=cellheight/2,r=cellheight/2, xs=NA, ys=NA,color="#FFCCFF",class="Bll")
 
 
 
 ?repeat
 
 
# End script
 