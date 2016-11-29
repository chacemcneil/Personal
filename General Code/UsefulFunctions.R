# Some useful functions
 tmp <- .Last.value
 library(data.table)
 library(ggplot2)
 library(grid)
 library(gridExtra)
 library(pROC)
 library(proto)
 library(RODBC)
 
 
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
 
 rsavvy <- function() {
   read.odbc <<- function(db, dbTable = NULL, dbQuery = NULL, ...) {
     con <- odbcConnect(db)
     if(is.null(dbTable)) {
       tab <- sqlQuery(con, query = dbQuery, ...)
     } else {
       tab <- sqlQuery(con, paste("SELECT * FROM", dbTable), ...)
     }
     odbcClose(con)
     tab
   }
 }
 
 ept <- function(txt,env=NULL,drop=T) {
   # Evaluate parsed text
   # Example: ept("sum(1:10)") --> 55
   if(is.null(env))
     env <- parent.frame()
   dt <- data.table()[,lapply(txt, function(str) eval(parse(text=str),envir=env))]
   names(dt) <- txt
   if(length(txt)==1 & drop == T)
     dt <- dt[[1]]
   dt
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
 
 ssgray   <- rgb(166, 166, 166, max = 255)
 ssblue   <- rgb(79, 129, 189, max = 255)
 ssyellow <- rgb(254, 190, 1, max = 255)
 ssgreen  <- rgb(155, 187, 89, max = 255)
 ssred    <- rgb(192, 80, 77, max = 255)
 sspurple <- rgb(128, 100, 162, max = 255)
 ssteal   <- rgb(75, 172, 198, max = 255)
 sscols <- c(ssgray, ssblue, ssyellow, ssgreen, ssred, sspurple, ssteal)
 
 ss_theme <- theme_bw() + theme(panel.border = element_blank(), 
                                axis.line = element_line(ssgray),
                                panel.grid.major.y = element_line("dashed", size = .5, colour = ssgray),  
                                panel.grid.minor.y = element_line("dashed", size = .5, colour = ssgray),  
                                panel.grid.major.x = element_line(size = .5, colour = ssgray))
 
 is.color <- function (x) ## Adapted from network:is.color 
 {
   xic <- rep(FALSE, length(x))
   #xc <- sapply(x, is.character)
   x <- as.character(x)
   xic <- (x %in% colors()) | ( (nchar(x) %in% c(7, 9)) & 
                                (substr(x, 1, 1) == "#") & 
                                sapply(strsplit(substr(x,2,nchar(x)),""),function(x) all(toupper(x) %in% c(0:9,LETTERS[1:6]))) )
   xic[is.na(x)] <- NA
   xic
 }
 
 hex <- function(col)  {
   rgb <- col2rgb(col)/255
   hex <- rgb(red=rgb["red",],green=rgb["green",],blue=rgb["blue",],alpha=1)
   hex
 }
 
 darken <- function(col,factor=1/3) {
   rgb <- col2rgb(col)*(1-factor)/255
   newcol <- rgb(red=rgb["red",],green=rgb["green",],blue=rgb["blue",],alpha=1)
   newcol
 }
 
 lighten <- function(col,factor=1/3) {
   rgb <- 1- (1-col2rgb(col)/255)*(1-factor)
   newcol <- rgb(red=rgb["red",],green=rgb["green",],blue=rgb["blue",],alpha=1)
   newcol
 }
 
 choose.colors <- function (plot.it=F,locate=0)
 {
   if(!plot.it)
   {
     return(colors()) # so far, not different from colors()
   } # close on if
   else
   {
     ytop    <- rep(seq(1/26,1,by=1/26),each=26)[1:657]
     ybottom <- rep(seq(0,1-1/26,by=1/26),each=26)[1:657]
     xleft   <- rep(seq(0,1-1/26,by=1/26),times=26)[1:657]
     xright  <- rep(seq(1/26,1,by=1/26),times=26)[1:657]
     pall    <- round(col2rgb(colors())/256)
     pall    <- colSums(pall) ; pall2 <- character(0)
     pall2[pall>0]   <- "black"
     pall2[pall==0]  <- "white"
     
     par(mar=c(0,0,1,0))
     
     plot.new()
     title(main="Palette of colors()")
     rect(xleft,ybottom,xright,ytop,col=colors())
     text(x=xleft+((1/26)/2)
          ,y=ytop-((1/26)/2)
          ,labels = 1:657
          ,cex=0.55
          ,col=pall2)
     
   } # close on else
   if(locate==0) print("Palette of colors()")
   else
   {
     colmat    <- matrix(c(1:657,rep(NA,26^2-657)),byrow=T,ncol=26,nrow=26)
     cols        <- NA
     i        <- NA
     for(i in 1:locate)
     {
       h    <- locator(1)
       if(any(h$x<0,h$y<0,h$x>1,h$y>1)) stop("locator out of bounds!")
       else
       {
         cc        <- floor(h$x/(1/26))+1
         rr        <- floor(h$y/(1/26))+1            
         cols[i]    <- colors()[colmat[rr,cc]]
       } # close on else
     } # close on i
     return(cols)
   } # close on else
 } # close on else+function
 
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
     print(dat[icnd,])
   return(ind)
 }
 
 #locateIndex(df <- data.frame(x=rnorm(100),y=rnorm(100)))
 
 dcast <- function(...) {dcast.data.table(...)}
 
 roc.dt <- function (rc, cost.fn = 1, cost.fp = 1, plot = T) {
   dt <- data.table(Sens   = rc$sensitivities,
                    Spec   = rc$specificities,
                    Thresh = rc$thresholds )
   dt[,TP   := sapply(Thresh, function(th) sum(rc$cases    > th))]
   dt[,TN   := sapply(Thresh, function(th) sum(rc$controls < th))]
   dt[,FP   := sapply(Thresh, function(th) sum(rc$controls > th))]
   dt[,FN   := sapply(Thresh, function(th) sum(rc$cases    < th))]
   dt[,Cost := FP * cost.fp + FN * cost.fn]
   if (plot)
     print(ggroc(rc))
   dt
 }
 
 ggroc <- function(rc, labels = NULL, print.auc = T, mark.sens = NULL, mark.spec = NULL) {
   dat <- data.table(Sensitivity = rc$sensitivities,
                     Specificity = rc$specificities,
                     Threshold   = rc$thresholds )
   
   numcontrols <- length(rc$controls)
   numcases    <- length(rc$cases)
   
   p <- ggplot()
   if(!is.null(mark.sens))
     p <- p + geom_vline(aes(xintercept = ifelse(is.null(mark.spec), NULL, 1-mark.spec)), linetype = 2, col = "slateblue")
   if(!is.null(mark.spec))
     p <- p + geom_hline(aes(yintercept = mark.sens),linetype=2,col="slateblue")
   p <- p + 
     geom_abline(aes(intercept = 0, slope = 1), col = "gray50") + 
     geom_line(data = dat, aes(1 - Specificity, Sensitivity)) + 
     labs(title = ifelse(print.auc, paste0("AUC: ", round(rc$auc, 3)), ""), x = "\nSpecificity", y = "Sensitivity\n", yy = "Cases") +
     theme(panel.grid.minor = element_blank(), 
           axis.text = element_text(size = 15), 
           plot.title = element_text(size = 20), 
           axis.title.x = element_text(size = 20), 
           axis.title.y = element_text(size = 20))
   left <- ggplot_build(p)$panel$ranges[[1]]$x.range[1]
   bttm <- ggplot_build(p)$panel$ranges[[1]]$y.range[1]
   p <- p + 
     geom_vline(xintercept = pretty(c(0, numcontrols))/numcontrols, col = "gray70", lty = 2) +
     scale_x_continuous(breaks = c(left, (0:5)/5, left, pretty(c(0, numcontrols))/numcontrols),
                        labels = c("", paste0((5:0)/5*100, "%"),
                                   "\nControls            ", paste0("\n", scales::comma(pretty(c(0, numcontrols)))) ),
                        limits = 0:1 ) +
     geom_hline(yintercept = pretty(c(0, numcases))/numcases, col = "gray70", lty = 2) +
     scale_y_continuous(breaks = c(bttm, (0:5)/5, bttm, pretty(c(0, numcases))/numcases),
                        labels = c("", paste0((0:5)/5*100, "%"),
                                   "\nCases       ", paste0(scales::comma(pretty(c(0, numcases))), "           ") ),
                        limits = 0:1 ) + 
     geom_hline(aes(yintercept = 0), size = 1) + 
     geom_vline(aes(xintercept = 0), size = 1)
   p
 }
 
 ggroc2 <- function(..., labels = NULL, print.auc = T, mark.sens = NULL, mark.spec = NULL) {
   rcs <- list(...)
   if(is.null(labels))
     labels = 1:length(rcs)
   names(rcs) <- labels
   dat <- do.call(rbind, lapply(seq_along(rcs), function(i) data.table(Sensitivity = rcs[[i]]$sensitivities,
                                                                       Specificity = rcs[[i]]$specificities,
                                                                       Threshold   = rcs[[i]]$thresholds,
                                                                       Name        = labels[i],
                                                                       AUC         = rcs[[i]]$auc )))
   
   #numcontrols <- length(rc$controls)
   #numcases    <- length(rc$cases)
   
   p <- ggplot()
   if(!is.null(mark.sens))
     p <- p + geom_vline(aes(xintercept = ifelse(is.null(mark.spec), NULL, 1-mark.spec)), linetype = 2, col = "slateblue")
   if(!is.null(mark.spec))
     p <- p + geom_hline(aes(yintercept = mark.sens),linetype=2,col="slateblue")
   p <- p + 
     geom_abline(aes(intercept = 0, slope = 1), col = "gray50") + 
     geom_line(data = dat, aes(1 - Specificity, Sensitivity, col = paste("AUC: ", round(AUC, 3)))) + 
     labs(x = "\nSpecificity", y = "Sensitivity\n", col = "") +
     theme(panel.grid.minor = element_blank(), 
           axis.text = element_text(size=15), 
           plot.title = element_text(size = 20), 
           axis.title.x = element_text(size = 20), 
           axis.title.y = element_text(size = 20),
           legend.text = element_text(size = 15))
   left <- ggplot_build(p)$panel$ranges[[1]]$x.range[1]
   bttm <- ggplot_build(p)$panel$ranges[[1]]$y.range[1]
   p <- p + 
     geom_hline(aes(yintercept = 0), size = 1) + 
     geom_vline(aes(xintercept = 0), size = 1)
   p
 }
 
 # Adding to ggplot2
 
 ggen <- environment(ggplot)
 
 GeomViolin2 <- ggproto(GeomViolin)
 for(i in names(GeomViolin)) {
   GeomViolin2[[i]] <- GeomViolin[[i]]
 }
 class(GeomViolin2) <- c("GeomViolin2", "Geom", "ggproto")
 GeomViolin2$draw_group <- function (self, data, ..., draw_quantiles = NULL)
 {
   data <- transform(data, xminv = x - ifelse(group %%2 == 1, 1, 0)*violinwidth * (x - xmin), 
                     xmaxv = x + ifelse(group %%2 == 0, 1, 0)*violinwidth * (xmax - x),
                     group = 1)
   newdata <- rbind(plyr::arrange(transform(data, x = xminv), y),
                    plyr::arrange(transform(data, x = xmaxv), -y))
   newdata <- rbind(newdata, newdata[1, ])
   if (length(draw_quantiles) > 0 & !scales::zero_range(range(data$y))) {
     stopifnot(all(draw_quantiles >= 0), all(draw_quantiles <= 1))
     quantiles <- create_quantile_segment_frame(data, draw_quantiles)
     aesthetics <- data[rep(1, nrow(quantiles)), setdiff(names(data), c("x", "y")), drop = FALSE]
     aesthetics$alpha <- rep(1, nrow(quantiles))
     both <- cbind(quantiles, aesthetics)
     quantile_grob <- GeomPath$draw_panel(both, ...)
     ggen$ggname("geom_violin2", grobTree(GeomPolygon$draw_panel(newdata, ...), quantile_grob))
   }
   else {
     ggen$ggname("geom_violin2", GeomPolygon$draw_panel(newdata, ...))
   }
 }
 environment(GeomViolin2) <- ggen
 
 geom_violin2 <- function (mapping = NULL, data = NULL, stat = "ydensity", position = "identity", 
                           ..., draw_quantiles = NULL, trim = TRUE, scale = "area", 
                           na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) 
 {
   # This function has been manually added to be used with ggplot2.
   layer(data = data, mapping = mapping, stat = stat, geom = GeomViolin2, 
         position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
         params = list(trim = trim, scale = scale, draw_quantiles = draw_quantiles, 
                       na.rm = na.rm, ...))
 }
 environment(geom_violin2) <- ggen
 
 #ggplot(mtcars, aes(factor(cyl), mpg, fill = as.factor(vs))) + geom_violin2() #+ coord_flip() + facet_grid(.~cyl)
 #ggplot(mtcars, aes(factor(cyl), mpg, fill = "blue")) + geom_violin() #+ coord_flip() + facet_grid(.~cyl)
 
 
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
 
 invisible(tmp)
 
 
# End script
 