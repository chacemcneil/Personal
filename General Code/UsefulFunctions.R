# Some useful functions:
# upd                 Modify width of console output and initialize lv = .Last.value
# ini                 Opens ~/.odbc.ini file for editting
# rsavvy              Creates a read.odbc function mimicking the savvy function, for use in Citrix
# ept                 (Evaluate Parsed Text) Evaluates an expression in character form
# revf                reverse the levels of a factor variable, useful for ggplot bar charts
# replace.na          function for replacing NAs in a vector more easily
# merge.list          Adds or modifies elements to a list based on entry names
# backup              Creates or modifies a .backup variable to contain a copy of passed objects
# revert              Restores all objects stored in .backup
# Table               Similar to table function, but adds columns and rows for totals
# vTable              Similar to Venn diagram, counts the number of unique elements contained in each set
# ext.chisq.test      Extends the categorical version of chisq.test to more than two dimensions
# matpaste            paste function that maintains matrix form
# dfSummary           Summarizes data.table objects in another table, one row per column
# is.color            Can the input be interpretted as a color
# hex                 Convert a color to hex format, uses col2rgb and rgb
# darken              Darken a color by a given factor
# lighten             Lighten a color by a given factor
# choose.colors       Choose colors from a palette
# locateIndex         Uses locator function to identify an observation (doesn't work with ggplot)
# snippets            Prints a list of available snippets
# roc.dt              Creates a table of sensitivities, specificities and corresponding thresholds
# ggroc               Prints a roc curve using ggplot
# nps                 Calculate the Net Promoter Score (NPS)
# npsind              Convert scores to -1,0,1 indicators (for NPS)
# npsprop             Calculates the proportion of NPS indicators
# moe                 Calculates Margin of Error (for NPS)
# geom_violin2        function to work with ggplot to create asymetric violin plots

 tmp <- .Last.value
 library(abind)
 library(data.table)
 library(ggplot2)
 library(grid)
 library(gridExtra)
 library(pROC)
 library(proto)
 library(RODBC)
 
 
 upd <- function(width=260) {
   if(exists("lv"))
   rm(lv, envir=.GlobalEnv)
   if(exists("env"))
     rm(env, envir=.GlobalEnv)
   
   makeActiveBinding("env", environment, parent.env(environment()))
   makeActiveBinding("lv", function(x) .Last.value, env)
   
   options(width=width)
 }
 upd()
 
 ini <- function(print=F) {
   if(print)
     cat(paste(readLines("/home/cmcneil/.odbc.ini"), collapse="\n"))
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
 
 ept <- function(txt, env=NULL, drop=T) {
   # Evaluate parsed text
   # Example: ept("sum(1:10)") --> 55
   if(is.null(env))
     env <- parent.frame()
   dt <- data.table()[, lapply(txt, function(str) eval(parse(text=str), envir=env))]
   names(dt) <- txt
   if(length(txt)==1 & drop == T)
     dt <- dt[[1]]
   dt
 }
 
 revf <- function(vec) {
   # Reverses the levels of a factor, useful in ggplot bar charts with coord_flip()
   # Does NOT reverse the order of the vector!
   vec <- factor(vec, levels = rev(levels(vec)))
   vec
 }
 
 replace.na <- function(x, repl = NULL, ...) {
   # Creates a copy of x with NAs replaced by repl
   # If repl is not given it defaults to 0, "" or FALSE depending on mode(x)
   # repl can be given a function that takes x as an input
   isfactor = (class(x) == "factor")
   if(is.null(repl))
     repl <- switch(class(x), logical = FALSE, numeric = 0, integer = 0L, character = "", factor = stop("If x is a factor, repl must be supplied"))
   if(isfactor) {
     xlevels <- levels(x)
     x <- as.character(x)
   }
   if(class(repl) == "function")
     new <- ifelse(is.na(x), repl(x, ...), x)
   else
     new <- ifelse(is.na(x), repl, x)
   if(isfactor)
     new <- factor(new, levels = xlevels)
   new
 }
 
 count.na <- function(x) {
   # Count the number of NAs in a vector
   count <- sum(is.na(x))
   count
 }
 
 merge.list <- function (..., priority = c("first", "last")) {
   priority <- match.arg(priority)
   lst <- list(...)
   if(priority == "last")
     lst <- rev(lst)
   if (length(lst) == 1)
     newlst <- lst[[1]]
   else
     newlst <- append(lst[[1]], do.call(merge.list, lst[-1]))
   newlst[!duplicated(names(newlst))]
 }
 
 backup <- function(obj) {
   # Function for backing up objects for testing.
   # Used in conjunction with revert function
   if(!exists(".backup"))
     .backup <<- list()
   .backup[[as.character(substitute(obj))]] <<- obj
 }
 revert <- function(obj) {
   # Revert to previously backed up version of obj
   if(!exists(".backup"))
     stop("No backup has been created")
   nm <- as.character(substitute(obj))
   if(!nm %in% names(.backup))
     stop("Given object is not contained in backup")
   assign(nm, .backup[[nm]], env = parent.frame())
 }
 
 
 #rfiles <- dir("../Miscellaneous/", pattern=".R$", full.names=T)
 #rcode  <- lapply(rfiles, readLines)
 
 Table <- function(..., along = NULL, prop = FALSE) {
   require(abind)
   tab <- table(...)
   if(is.null(along))
     along <- 1:length(dim(tab))
   if(length(dim(tab)) == 1) {
     tab <- as.matrix(tab)
     colnames(tab) <- "Count"
   }
   for (i in along) {
     tab <- abind(tab, Total = apply(tab, MARGIN = setdiff(1:length(dim(tab)), i), sum), along = i)
   }
   if (prop)
     tab <- prop.table(tab) * 2^(length(dim(tab)) - (length(dim(tab))==2 & dim(tab)[2]==1))
   names <- names(list(...))
   if (is.null(names))
     names(dimnames(tab)) <- as.character(as.list(substitute(list(...)))[-1L])
   else
     names(dimnames(tab)) <- ifelse(names == "", as.character(as.list(substitute(list(...)))[-1L]), names)
   names(dimnames(tab)) <- NULL
   tab
 }
 
 vTable <- function(..., prop = FALSE, sums = FALSE) {
   # Find the number of unique values that are shared
   # Similar to values in a Venn diagram
   names <- as.character(as.list(substitute(list(...)))[-1L])
   lists <- lapply(list(...), function(x) if(class(x) == "factor") as.character(x) else x)
   tab <- data.table(Values = unique(unlist(lists)))[, (names) := lapply(lists, function(x) ifelse(Values %in% x, "In", "Out"))][, Values := NULL]
   if(sums)
     tab <- do.call(Table, tab)
   else
     tab <- do.call(table, tab)
   if (prop)
     tab <- prop.table(tab) * 2^((length(dim(tab)) - (length(dim(tab))==2 & dim(tab)[2]==1))*sums)
   tab
 }
 
 ext.cat.chisq.test <- function (arr) {
   if (length(dim(arr)) < 2)
     stop("arr must have at least two dimensions.")
   expected <- array(apply(arr, 1, sum)[slice.index(arr, 1)], dim = dim(arr))
   for (i in 2:length(dim(arr))) {
     expected <- expected * array(apply(arr, i, sum)[slice.index(arr, i)], dim = dim(arr))
   }
   expected <- expected / sum(arr)^(length(dim(arr)) - 1)
   chisq <- sum((arr - expected)^2/expected)
   df <- prod(dim(arr) - 1)
   p.value <- 1 - pchisq(chisq, df)
   cat(sprintf("Chi-squared statistic: %s\nDegrees of freedom: %s\np-value: %s\n", chisq, df, p.value))
   list(chisq = chisq, df = df, p.value = p.value)
 }
 
 bin <- function() {
   l
 }
 
 matpaste <- function(..., sep = " ", collapse = NULL, dim = NULL) {
   lst <- list(...)
   if(is.null(dim))
     dim <- dim(lst[[min(which(sapply(lst, is.matrix)))]])
   mat <- matrix(paste(..., sep = sep, collapse = collapse), nrow = dim[1], ncol = dim[2])
   mat
 }
 
 dfSummary <- function(..., table.names=NULL, track=T) {
   tabs <- list(...)
   if(is.null(names(tabs))) {
     if(is.null(table.names))
       names(tabs) <- as.list(substitute(list(...)))[-1L]
     else
       names(tabs) <- table.names
   }
   summary <- do.call(rbind, c(lapply(seq_along(tabs), function(i) {
     if(track)
       cat(paste0("\rTable: ", names(tabs)[i], "       "))
     dat <- tabs[[i]]
     column <- data.table(TableName = names(tabs)[i],
                          ColName   = colnames(dat),
                          Class     = sapply(dat, function(x) paste(class(x), collapse=",")),
                          Mode      = sapply(dat, mode),
                          NumNA     = sapply(dat, function(col) sum(is.na(col))),
                          PctNA     = sapply(dat, function(col) mean(is.na(col))),
                          NumUnq    = sapply(dat, function(col) length(unique(col[!is.na(col)]))),
                          Length    = nrow(dat),
                          NumLevels = sapply(dat, function(x) length(levels(x))))
     column[Mode == "numeric" & Class != "factor",                                 Min     := sapply(dat[, ColName, with = F], min, na.rm = T)]
     column[Mode == "numeric" & Class != "factor" & Class!="Date",                 Qrt1    := sapply(dat[, ColName, with = F], quantile, .25, na.rm = T)]
     column[Mode == "numeric" & Class != "factor",                                 Median  := sapply(dat[, ColName, with = F], median, na.rm = T)]
     column[Mode == "numeric" & Class != "factor",                                 Mean    := sapply(dat[, ColName, with = F], mean, na.rm = T)]
     column[Mode == "numeric" & Class != "factor" & Class!="Date",                 Qrt3    := sapply(dat[, ColName, with = F], quantile, .75, na.rm = T)]
     column[Mode == "numeric" & Class != "factor",                                 Max     := sapply(dat[, ColName, with = F], max, na.rm = T)]
     column[Mode == "numeric" & !Class %in% c("factor", "Date", "POSIXct,POSIXt"), Sum     := sapply(dat[, ColName, with = F], function(x) sum(as.numeric(x), na.rm = T))]
     column[Mode == "numeric" & Class != "factor",                                 Nonzero := sapply(dat[, ColName, with = F], function(x) sum(x!=0, na.rm = T))]
     column[, Most        := sapply(dat, function(col) names(sort(table(col), decreasing = T))[1])]
     column[, MostCount   := sapply(dat, function(col) sort(table(col), decreasing = T)[1])]
     column[, MostUnique  := sapply(dat, function(col) diff(sort(table(col), decreasing = T)[1:2])!=0) | NumUnq == 1]
     column[, Least       := sapply(dat, function(col) names(sort(table(col)))[1])]
     column[, LeastCount  := sapply(dat, function(col) sort(table(col))[1])]
     column[, LeastUnique := sapply(dat, function(col) diff(sort(table(col))[1:2])!=0) | NumUnq == 1]
     return(column)
   }), fill = T))
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
 sscols <- c(ssblue, ssyellow, ssgreen, ssred, sspurple, ssteal, ssgray)
 
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
                                sapply(strsplit(substr(x, 2, nchar(x)), ""), function(x) all(toupper(x) %in% c(0:9, LETTERS[1:6]))) )
   xic[is.na(x)] <- NA
   xic
 }
 
 hex <- function(col)  {
   rgb <- col2rgb(col)/255
   hex <- rgb(red=rgb["red",], green=rgb["green",], blue=rgb["blue",], alpha=1)
   hex
 }
 
 darken <- function(col, factor = 1/3) {
   rgb <- col2rgb(col)*(1-factor)/255
   newcol <- rgb(red = rgb["red",], green = rgb["green",], blue = rgb["blue",], alpha = 1)
   newcol
 }
 
 lighten <- function(col, factor = 1/3) {
   rgb <- 1- (1-col2rgb(col)/255)*(1-factor)
   newcol <- rgb(red = rgb["red",], green = rgb["green",], blue = rgb["blue",], alpha = 1)
   newcol
 }
 
 choose.colors <- function (plot.it = F, locate = 0)
 {
   if(!plot.it)
   {
     return(colors()) # so far, not different from colors()
   } # close on if
   else
   {
     ytop    <- rep(seq(1/26, 1, by=1/26), each=26)[1:657]
     ybottom <- rep(seq(0, 1-1/26, by=1/26), each=26)[1:657]
     xleft   <- rep(seq(0, 1-1/26, by=1/26), times=26)[1:657]
     xright  <- rep(seq(1/26, 1, by=1/26), times=26)[1:657]
     pall    <- round(col2rgb(colors())/256)
     pall    <- colSums(pall) ; pall2 <- character(0)
     pall2[pall>0]   <- "black"
     pall2[pall==0]  <- "white"
     
     par(mar=c(0,0,1,0))
     
     plot.new()
     title(main="Palette of colors()")
     rect(xleft, ybottom, xright, ytop, col=colors())
     text(x=xleft+((1/26)/2)
          , y=ytop-((1/26)/2)
          , labels = 1:657
          , cex=0.55
          , col=pall2)
     
   } # close on else
   if(locate==0) print("Palette of colors()")
   else
   {
     colmat    <- matrix(c(1:657, rep(NA, 26^2-657)), byrow = T, ncol = 26, nrow = 26)
     cols        <- NA
     i        <- NA
     for(i in 1:locate)
     {
       h    <- locator(1)
       if(any(h$x<0, h$y<0, h$x>1, h$y>1)) stop("locator out of bounds!")
       else
       {
         cc        <- floor(h$x/(1/26))+1
         rr        <- floor(h$y/(1/26))+1            
         cols[i]    <- colors()[colmat[rr, cc]]
       } # close on else
     } # close on i
     return(cols)
   } # close on else
 } # close on else+function
 
 locateIndex <- function(dat, gg=F, print=T, n=1, ...) {
   require(ggplot2)
   if(sum(!c("x", "y") %in% colnames(dat))) {
     colnames(dat)[1:2] <- c("x", "y")
   }
   if(gg)
     ggplot() + geom_point(data=dat, aes(x, y))
   else
     plot(dat$x, dat$y, ...)
   
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
   points(dat[ind,], col="red", pch=19)
   if(print)
     print(dat[ind,])
   return(ind)
 }
 
 plotprops <- function() {
   
 }
 
 #locateIndex(df <- data.frame(x=rnorm(100), y=rnorm(100)))
 
 ## Snippets are little keywords that can be used to write code. Example: in an R script, type ts<SHIFT-TAB>
 #  These can be customized in the r.snippets file, accessible via the following function.
 snippets <- function(type = c("r", "markdown", "css", "c_cpp", "html", "java", "javascript", "python", "sql", "stan"), edit = F) {
   type <- match.arg(type)
   filename <- paste0("~/.R/snippets/", type, ".snippets")
   if(file.exists(filename)) {
     snips <- readLines(filename)
     snips <- grep("^snippet .+$", snips, value = T)
     snips <- gsub("snippet ", "", snips)
     if(edit)
       file.edit(filename)
     return(snips)
   }
   else {
     cat(paste0("The file ", type, ".snippets has not been created.\n",
                 "Open Global Options... under the Tools menu,\n",
                 "  in the Code - Editing section select Edit Snippets...\n",
                 "  then modify and save the file of ", type, " snippets.\n",
                 "Use snippets() to get snippet names.\n",
                 "Use parameter edit=TRUE to view/modify snippets."))
     return(invisible(NULL))
   }
 }
 
 dcast <- function(...) {dcast.data.table(...)}
 
 roc.dt <- function (rc, cost.fn = 1, cost.fp = 1, plot = T) {
   dt <- data.table(Sens   = rc$sensitivities,
                    Spec   = rc$specificities,
                    Thresh = rc$thresholds )
   dt[, TP   := sapply(Thresh, function(th) sum(rc$cases    > th))]
   dt[, TN   := sapply(Thresh, function(th) sum(rc$controls < th))]
   dt[, FP   := sapply(Thresh, function(th) sum(rc$controls > th))]
   dt[, FN   := sapply(Thresh, function(th) sum(rc$cases    < th))]
   dt[, Cost := FP * cost.fp + FN * cost.fn]
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
     p <- p + geom_hline(aes(yintercept = mark.sens), linetype=2, col="slateblue")
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
     p <- p + geom_hline(aes(yintercept = mark.sens), linetype=2, col="slateblue")
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
 
 
 # NPS Functions
 npsind <- function(x, as.factor=F) {
   # Converts raw scores to -1,0,1 indicators
   x <- x[!is.na(x)]
   x <- ifelse(x > 6, ifelse(x > 8, 1, 0), -1)
   if(as.factor)
     x <- factor(x, levels=-1:1)
   x
 }
 npsprop <- function(x) {
   # Given individual raw scores, calculates the proportion of detractors (-1), passives (0), and promotors (1)
   prop.table(table(npsind(x, as.factor=T)))[c("-1", "0", "1")]
 }
 nps <- function(x) {
   # Given individual raw scores, calculates group NPS score
   npsprop(x)[["1"]] - npsprop(x)[["-1"]]
 }
 moe <- function(x) {
   # Calculates the margin of error for the NPS score
   qnorm(.975) * sd(npsind(x)) / sqrt(length(x))
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
 
 geom_prop <- function(mapping = NULL, data = NULL, stat = "identity", position = "identity", bins = 10,
                       ..., na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
   data2 <- data[, {
     lst <- list(x = quantile(eval(mapping$x), (1:bins)/bins - .5/bins),
                 y = tapply(eval(mapping$y), quantcut(eval(mapping$x), (0:bins)/bins), mean),
                 n = tapply(eval(mapping$y), quantcut(eval(mapping$x), (0:bins)/bins), length) )
     names(lst) <- as.character(map[names(lst)])
   }, by = as.character(mapping[intersect(c("colour", "group"), names(mapping))]) ]
   mapping$xlim <- substitute(xlim)
   mapping$ylim <- substitute(ylim)
   mapping <- aes(x = x, y = y, ylim = ylim, ymax = ymax)
   geom_errorbar(mapping = mapping, data = data, stat = stat, position = position, ..., na.r = na.rm, show.legend = show.legend, inherit.aes = inherit.aes)
 }
 
 #ggplot(mtcars, aes(factor(cyl), mpg, fill = as.factor(vs))) + geom_violin2() #+ coord_flip() + facet_grid(.~cyl)
 #ggplot(mtcars, aes(factor(cyl), mpg, fill = "blue")) + geom_violin() #+ coord_flip() + facet_grid(.~cyl)
 
 ggsurv <- function(s, CI = 'def', plot.cens = T, surv.col = 'gg.def',
                    cens.col = NULL, lty.est = 1, lty.ci = 2,
                    cens.shape = 3, back.white = F, xlab = 'Time',
                    ylab = 'Survival', main = ''){
   
   library(ggplot2)
   strata <- ifelse(is.null(s$strata) ==T, 1, length(s$strata))
   stopifnot(length(surv.col) == 1 | length(surv.col) == strata)
   stopifnot(length(lty.est) == 1 | length(lty.est) == strata)
   
   ggsurv.s <- function(s, CI = 'def', plot.cens = T, surv.col = 'gg.def',
                        cens.col = 'red', lty.est = 1, lty.ci = 2,
                        cens.shape = 3, back.white = F, xlab = 'Time',
                        ylab = 'Survival', main = ''){
     
     dat <- data.frame(time = c(0, s$time),
                       surv = c(1, s$surv),
                       up = c(1, s$upper),
                       low = c(1, s$lower),
                       cens = c(0, s$n.censor))
     dat.cens <- subset(dat, cens != 0)
     
     col <- ifelse(surv.col == 'gg.def', 'black', surv.col)
     
     pl <- ggplot(dat, aes(x = time, y = surv)) +
       xlab(xlab) + ylab(ylab) + ggtitle(main) +
       geom_step(col = col, lty = lty.est)
     
     pl <- if(CI == T | CI == 'def') {
       pl + geom_step(aes(y = up), color = col, lty = lty.ci) +
         geom_step(aes(y = low), color = col, lty = lty.ci)
     } else (pl)
     
     pl <- if(plot.cens == T & length(dat.cens) > 0){
       if(is.null(cens.col))
         pl + geom_point(data = dat.cens, aes(y = surv, col = group), shape = cens.shape)
       else
         pl + geom_point(data = dat.cens, aes(y = surv), shape = cens.shape, col = cens.col)
     } else if (plot.cens == T & length(dat.cens) == 0){
       stop ('There are no censored observations')
     } else(pl)
     
     pl <- if(back.white == T) {pl + theme_bw()
     } else (pl)
     pl
   }
   
   ggsurv.m <- function(s, CI = 'def', plot.cens = T, surv.col = 'gg.def',
                        cens.col = 'red', lty.est = 1, lty.ci = 2,
                        cens.shape = 3, back.white = F, xlab = 'Time',
                        ylab = 'Survival', main = '') {
     n <- s$strata
     
     groups <- factor(unlist(strsplit(names
                                      (s$strata), '='))[seq(2, 2*strata, by = 2)])
     gr.name <-  unlist(strsplit(names(s$strata), '='))[1]
     gr.df <- vector('list', strata)
     ind <- vector('list', strata)
     n.ind <- c(0, n); n.ind <- cumsum(n.ind)
     for(i in 1:strata) ind[[i]] <- (n.ind[i]+1):n.ind[i+1]
     
     for(i in 1:strata){
       gr.df[[i]] <- data.frame(
         time = c(0, s$time[ ind[[i]] ]),
         surv = c(1, s$surv[ ind[[i]] ]),
         up = c(1, s$upper[ ind[[i]] ]),
         low = c(1, s$lower[ ind[[i]] ]),
         cens = c(0, s$n.censor[ ind[[i]] ]),
         group = rep(groups[i], n[i] + 1))
     }
     
     dat <- do.call(rbind, gr.df)
     dat.cens <- subset(dat, cens != 0)
     
     pl <- ggplot(dat, aes(x = time, y = surv)) +
       xlab(xlab) + ylab(ylab) + ggtitle(main) +
       geom_step(aes(col = group))
     
     col <- if(length(surv.col == 1)){
       scale_colour_manual(name = gr.name, values = rep(surv.col, strata))
     } else{
       scale_colour_manual(name = gr.name, values = surv.col)
     }
     
     pl <- if(surv.col[1] != 'gg.def'){
       pl + col
     } else {pl + scale_colour_discrete(name = gr.name)}
     
     line <- if(length(lty.est) == 1){
       scale_linetype_manual(name = gr.name, values = rep(lty.est, strata))
     } else {scale_linetype_manual(name = gr.name, values = lty.est)}
     
     pl <- pl + line
     
     pl <- if(CI == T) {
       if(length(surv.col) > 1 && length(lty.est) > 1){
         stop('Either surv.col or lty.est should be of length 1 in order
             to plot 95% CI with multiple strata')
       }else if((length(surv.col) > 1 | surv.col == 'gg.def')[1]){
         pl + geom_step(aes(y = up, color = group), lty = lty.ci) +
           geom_step(aes(y = low, color = group), lty = lty.ci)
       } else{pl +  geom_step(aes(y = up, lty = group), col = surv.col) +
           geom_step(aes(y = low, lty = group), col = surv.col)}
     } else {pl}
     
     
     pl <- if(plot.cens == T & length(dat.cens) > 0){
       if(is.null(cens.col))
         pl + geom_point(data = dat.cens, aes(y = surv, col = group), shape = cens.shape)
       else
         pl + geom_point(data = dat.cens, aes(y = surv), shape = cens.shape, col = cens.col)
     } else if (plot.cens == T & length(dat.cens) == 0){
       stop ('There are no censored observations')
     } else(pl)
     
     pl <- if(back.white == T) {pl + theme_bw()
     } else (pl)
     pl
   }
   pl <- if(strata == 1) {ggsurv.s(s, CI , plot.cens, surv.col ,
                                   cens.col, lty.est, lty.ci,
                                   cens.shape, back.white, xlab,
                                   ylab, main)
   } else {ggsurv.m(s, CI, plot.cens, surv.col ,
                    cens.col, lty.est, lty.ci,
                    cens.shape, back.white, xlab,
                    ylab, main)}
   pl
 }
 
 
#   grid.align <- function (..., nrow=NULL, ncol=NULL, newpage=T, byrow=T, freecol=T, freerow=T) {
#     ps <- list(...)
#     n  <- length(ps)
#     if(is.null(nrow)) {
#       if(is.null(ncol)) {
#         nrow <- ceiling(sqrt(n))
#         ncol <- ceiling(n/nrow)
#       } else {
#         nrow <- ceiling(n/ncol)
#       }
#     } else {
#       if(is.null(ncol)) {
#         ncol <- ceiling(n/nrow)
#       } else {
#         if (n > ncol*nrow)
#           ps <- ps
#       }
#     }
#     if (newpage)
#       grid.newpage()
#     gs <- lapply(ps, ggplotGrob)
#     
#     # For each row of plots, standardize ylim
#     for (i in 1:nrow) {
#       if (byrow)
#         ind <- which(((1:n)-1) %/% nrow == i-1)
#       else
#         ind <- which(((1:n)-1) %% nrow == i-1)
#       lims <- do.call(cbind, lapply(ps[ind], function(pl) {
#         if (pl$scales$has_scale("y"))
#           lim <- pl$scales$get_scales("y")$limits
#         else {
#           colname <- as.character(pl$layers[[1]]$mapping$y)
#           if (length(colname)==0)
#             colname <- as.character(pl$mapping$y)
#           vals <- pl$data[[colname]]
#           lim <- range(vals[is.finite(vals)])
#         }
#         lim
#       }))
#       miny <- min(lims[1,])
#       maxy <- max(lims[2,])
#       ps[ind] <- lapply(ps[ind], function(pl) {pl <- pl + ylim(c(miny, maxy)); pl})
#     }
#     # For each column of plots, standardize xlim
#     for (i in 1:ncol) {
#       if (byrow)
#         ind <- which(((1:n)-1) %% ncol == i-1)
#       else
#         ind <- which(((1:n)-1) %/% ncol == i-1)
#       lims <- do.call(cbind, lapply(ps[ind], function(pl) {
#         if (pl$scales$has_scale("x"))
#           lim <- pl$scales$get_scales("x")$limits
#         else {
#           colname <- as.character(pl$layers[[1]]$mapping$x)
#           if (length(colname)==0)
#             colname <- as.character(pl$mapping$x)
#           vals <- pl$data[[colname]]
#           lim <- range(vals[is.finite(vals)])
#         }
#         lim
#       }))
#       minx <- min(lims[1,])
#       maxx <- max(lims[2,])
#       ps[ind] <- lapply(ps[ind], function(pl) {pl <- pl + xlim(c(minx, maxx)); pl})
#     }
#     # Standardize point size (?)
#     
#     # For each row of plots, standardize the plot height and vertical position
#     for (i in 1:nrow) {
#       if (byrow)
#         ind <- which(((1:n)-1) %/% nrow == i-1)
#       else
#         ind <- which(((1:n)-1) %% nrow == i-1)
#       maxheight <- do.call(grid::unit.pmax, lapply(ps[ind], function(pl) ggplotGrob(pl)$height[2:5]))
#       gs[ind] <- lapply(gs[ind], function(g) {g$height[2:5] <- maxheight; g})
#     }
#     # For each column of plots, standardize the plot width and horizontal powition
#     for (i in 1:ncol) {
#       if (byrow)
#         ind <- which(((1:n)-1) %% ncol == i-1)
#       else
#         ind <- which(((1:n)-1) %/% ncol == i-1)
#       maxwidth <- do.call(grid::unit.pmax, lapply(ps[ind], function(pl) ggplotGrob(pl)$width[2:5]))
#       gs[ind] <- lapply(gs[ind], function(g) {g$width[2:5] <- maxwidth; g})
#     }
#     
#     g <- do.call(arrangeGrob, c(gs, nrow=nrow, ncol=ncol))
#     grid.draw(g)
#     invisible(g)
#   }
 
 invisible(tmp)
 
 
# End script
 