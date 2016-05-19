# Some useful functions
 library(data.table)
 library(ggplot2)
 library(grid)
 library(gridExtra)
 
 
 upd <- function(width=260) {
   if(exists("lv"))
     rm(lv,envir=.GlobalEnv)
   if(exists("env"))
     rm(env,envir=.GlobalEnv)
   
   makeActiveBinding("env",environment,parent.env(environment()))
   makeActiveBinding("lv",function(x) .Last.value,env)
   
   options(width=width)
 }
 upd()
 
 ini <- function(print=F) {
   if(print)
     cat(paste(readLines("/home/cmcneil/.odbc.ini"),collapse="\n"))
   file.edit("/home/cmcneil/.odbc.ini")
 }
 
 ept <- function(txt) {
   # Evaluate parsed text
   env <- parent.frame()
   eval(parse(text=txt),envir=env)
 }
 
 merge.list <- function (...,priority=c("first","last")) {
   priority <- match.arg(priority)
   lst <- list(...)
   if(priority=="last")
     lst <- rev(lst)
   if (length(lst) == 1)
     newlst <- lst[[1]]
   else
     newlst <- append(lst[[1]],do.call(merge.list,lst[-1]))
   newlst[!duplicated(names(newlst))]
 }
 
 #rfiles <- dir("../Miscellaneous/",pattern=".R$",full.names=T)
 #rcode  <- lapply(rfiles,readLines)
 
 dfSummary <- function(...,table.names=NULL,track=T) {
   tabs <- list(...)
   if(is.null(names(tabs))) {
     if(is.null(table.names))
       names(tabs) <- paste0("Table",1:length(tabs))
     else
       names(tabs) <- table.names
   }
   summary <- do.call(rbind,lapply(seq_along(tabs),function(i) {
     if(track)
       cat(paste0("\rTable: ",names(tabs)[i]))
     dat <- tabs[[i]]
     column <- data.table(TableName=names(tabs)[i],
                          ColName=colnames(dat),
                          Class=sapply(dat,function(x) paste(class(x),collapse=",")),
                          Mode=sapply(dat,mode),
                          NumNA=sapply(dat,function(col) sum(is.na(col))),
                          PctNA=sapply(dat,function(col) mean(is.na(col))),
                          NumUnq=sapply(dat,function(col) length(unique(col))),
                          Length=nrow(dat),
                          NumLevels=sapply(dat,function(x) length(levels(x))))
     column[Mode=="numeric" & Class!="factor",Min                    :=sapply(dat[,ColName,with=F],min,na.rm=T)]
     column[Mode=="numeric" & Class!="factor" & Class!="Date",Qrt1   :=sapply(dat[,ColName,with=F],quantile,.25,na.rm=T)]
     column[Mode=="numeric" & Class!="factor",Median                 :=sapply(dat[,ColName,with=F],median,na.rm=T)]
     column[Mode=="numeric" & Class!="factor",Mean                   :=sapply(dat[,ColName,with=F],mean,na.rm=T)]
     column[Mode=="numeric" & Class!="factor" & Class!="Date",Qrt3   :=sapply(dat[,ColName,with=F],quantile,.75,na.rm=T)]
     column[Mode=="numeric" & Class!="factor",Max                    :=sapply(dat[,ColName,with=F],max,na.rm=T)]
     column[Mode=="numeric" & !Class %in% c("factor","Date","POSIXct,POSIXt"),Sum  :=sapply(dat[,ColName,with=F],sum,na.rm=T)]
     column[Mode=="numeric" & Class!="factor",Nonzero:=sapply(dat[,ColName,with=F],function(x) sum(x!=0,na.rm=T))]
     column[,Most:=sapply(dat,function(col) names(sort(table(col),decreasing=T))[1])]
     column[,MostCount:=sapply(dat,function(col) sort(table(col),decreasing=T)[1])]
     column[,MostUnique:=sapply(dat,function(col) diff(sort(table(col),decreasing=T)[1:2])!=0)]
     column[,Least:=sapply(dat,function(col) names(sort(table(col)))[1])]
     column[,LeastCount:=sapply(dat,function(col) sort(table(col))[1])]
     column[,LeastUnique:=sapply(dat,function(col) diff(sort(table(col))[1:2])!=0)]
     return(column)
   }))
   cat("\n")
   return(summary)
 }
 
 
 locateIndex <- function(dat,gg=F,print=T,n=1,...) {
   require(ggplot2)
   if(sum(!c("x","y") %in% colnames(dat))) {
     colnames(dat)[1:2] <- c("x","y")
   }
   if(gg)
     ggplot() + geom_point(data=dat,aes(x,y))
   else
     plot(dat$x,dat$y,...)
   
   if(gg) { # Not possible?
     gglocator(n)
   }
   else {
     coord <- data.frame(locator(n))
     ind <- NULL
     for (i in 1:nrow(coord)) {
       dist <- (dat[,1]-coord$x[i])^2 + (dat[,2]-coord$y[i])^2
       ind[i] <- which.min(dist)
     }
   }
   points(dat[ind,],col="red",pch=19)
   if(print)
     print(dat[ind,])
   return(ind)
 }
 
 #locateIndex(df <- data.frame(x=rnorm(100),y=rnorm(100)))
 
#  grid.align <- function (...,nrow=NULL,ncol=NULL, newpage=T,byrow=T,freecol=T,freerow=T) {
#    ps <- list(...)
#    n  <- length(ps)
#    if(is.null(nrow)) {
#      if(is.null(ncol)) {
#        nrow <- ceiling(sqrt(n))
#        ncol <- ceiling(n/nrow)
#      } else {
#        nrow <- ceiling(n/ncol)
#      }
#    } else {
#      if(is.null(ncol)) {
#        ncol <- ceiling(n/nrow)
#      } else {
#        if (n > ncol*nrow)
#          ps <- ps
#      }
#    }
#    if (newpage)
#      grid.newpage()
#    gs <- lapply(ps,ggplotGrob)
#    
#    # For each row of plots, standardize ylim
#    for (i in 1:nrow) {
#      if (byrow)
#        ind <- which(((1:n)-1) %/% nrow == i-1)
#      else
#        ind <- which(((1:n)-1) %% nrow == i-1)
#      lims <- do.call(cbind,lapply(ps[ind],function(pl) {
#        if (pl$scales$has_scale("y"))
#          lim <- pl$scales$get_scales("y")$limits
#        else {
#          colname <- as.character(pl$layers[[1]]$mapping$y)
#          if (length(colname)==0)
#            colname <- as.character(pl$mapping$y)
#          vals <- pl$data[[colname]]
#          lim <- range(vals[is.finite(vals)])
#        }
#        lim
#      }))
#      miny <- min(lims[1,])
#      maxy <- max(lims[2,])
#      ps[ind] <- lapply(ps[ind],function(pl) {pl <- pl + ylim(c(miny,maxy)); pl})
#    }
#    # For each column of plots, standardize xlim
#    for (i in 1:ncol) {
#      if (byrow)
#        ind <- which(((1:n)-1) %% ncol == i-1)
#      else
#        ind <- which(((1:n)-1) %/% ncol == i-1)
#      lims <- do.call(cbind,lapply(ps[ind],function(pl) {
#        if (pl$scales$has_scale("x"))
#          lim <- pl$scales$get_scales("x")$limits
#        else {
#          colname <- as.character(pl$layers[[1]]$mapping$x)
#          if (length(colname)==0)
#            colname <- as.character(pl$mapping$x)
#          vals <- pl$data[[colname]]
#          lim <- range(vals[is.finite(vals)])
#        }
#        lim
#      }))
#      minx <- min(lims[1,])
#      maxx <- max(lims[2,])
#      ps[ind] <- lapply(ps[ind],function(pl) {pl <- pl + xlim(c(minx,maxx)); pl})
#    }
#    # Standardize point size (?)
#    
#    # For each row of plots, standardize the plot height and vertical position
#    for (i in 1:nrow) {
#      if (byrow)
#        ind <- which(((1:n)-1) %/% nrow == i-1)
#      else
#        ind <- which(((1:n)-1) %% nrow == i-1)
#      maxheight <- do.call(grid::unit.pmax,lapply(ps[ind],function(pl) ggplotGrob(pl)$height[2:5]))
#      gs[ind] <- lapply(gs[ind], function(g) {g$height[2:5] <- maxheight; g})
#    }
#    # For each column of plots, standardize the plot width and horizontal powition
#    for (i in 1:ncol) {
#      if (byrow)
#        ind <- which(((1:n)-1) %% ncol == i-1)
#      else
#        ind <- which(((1:n)-1) %/% ncol == i-1)
#      maxwidth <- do.call(grid::unit.pmax,lapply(ps[ind],function(pl) ggplotGrob(pl)$width[2:5]))
#      gs[ind] <- lapply(gs[ind], function(g) {g$width[2:5] <- maxwidth; g})
#    }
#    
#    g <- do.call(arrangeGrob,c(gs,nrow=nrow,ncol=ncol))
#    grid.draw(g)
#    invisible(g)
#  }
 
 
#  getPassword <- function() {
#    require(shiny)
#    if(!file.exists("server.R")) {
#      serverscript <- "
# library(shiny)
# shinyServer(function(input, output,session) {
#     observe({
#         input$button
#         password <<- isolate(input$password)
#     })
# })
#      "
#      write(serverscript,file="server.R")
#    }
#    if(!file.exists("ui.R")) {
#      uiscript <- "
# library(shiny)
# shinyUI(
#     sidebarPanel(textInput(\"password\",\"Enter password: \"),actionButton(\"button\",\"Enter\"))
# )
#      "
#      write(uiscript,file="ui.R")
#    }
#    runApp()
#  }
 
#  getPassword <- function() {
#    require(tcltk)
#    tt <- tktoplevel() 
#    Password <- tclVar("") 
#    password <- NULL
#    tkf <- tkframe(tt,relief="ridge",borderwidth=3)
#    ## Frame options:
#    # borderwidth: width of border
#    # relief:      style of border
#    
#    entry.Password <-tkentry(tkf,width="20",textvariable=Password,show="*") 
#    tkgrid(tklabel(tkf,text="Please enter your password.")) 
#    tkgrid(entry.Password) 
#    OnOK <- function() 
#    { 
#      tkdestroy(tt)  
#      password <<- tclvalue(Password) 
#    } 
#    OK.but <-tkbutton(tt,text="   OK   ",command=OnOK) 
#    tkbind(entry.Password, "<Return>",OnOK) 
#    tkgrid(tkf,OK.but)
#    
#    while(is.null(password))
#    {}
#    tkdestroy(tt)
#    return(password)
#  }
 
 
 
 
# end script
 