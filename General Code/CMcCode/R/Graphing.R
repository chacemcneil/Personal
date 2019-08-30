### Collection of functions that create or modify ggplot2 graphs. The included functions are:
# is.color            Can the input be interpretted as a color
# hex                 Convert a color to hex format, uses col2rgb and rgb
# darken              Darken a color by a given factor
# lighten             Lighten a color by a given factor
# choose.colors       Choose colors from a palette
# locateIndex         Uses locator function to identify an observation (doesn't work with ggplot)
# roc_dt              Creates a table of sensitivities, specificities and corresponding thresholds, used in ggroc
# ggroc               Prints a roc curve using ggplot
# ggroc2              Prints a roc curve using ggplot
# geom_violin2        Function to work with ggplot to create asymetric violin plots
# ggprop              Function to work with ggplot to create logistic regression prediction diagnostic plot

#ss_themeGr <- theme_bw() + theme(panel.border = element_blank(), axis.line = element_line()) + theme(panel.grid.major.y = element_line("dashed", size = .5, colour = "grey"), plot.margin = unit(c(1, 2, 0.5, 0.5), "lines"))
#options(width = 200, warn = -1)


#' Savvysherpa Colors
#' 
#' Colors and theme matching those used in Savvysherpa reports.
#' @name ss_theme
#' @usage 
#' ssgray
#' ssblue
#' ssyellow
#' ssgreen
#' ssred
#' sspurple
#' ssteal
#' sscols
#' g + ss_theme    # g is a returned \code{ggplot2} graph.

#' @export
ssgray   <- rgb(166, 166, 166, max = 255)
#' @export
ssblue   <- rgb(79, 129, 189, max = 255)
#' @export
ssyellow <- rgb(254, 190, 1, max = 255)
#' @export
ssgreen  <- rgb(155, 187, 89, max = 255)
#' @export
ssred    <- rgb(192, 80, 77, max = 255)
#' @export
sspurple <- rgb(128, 100, 162, max = 255)
#' @export
ssteal   <- rgb(75, 172, 198, max = 255)
#' @export
sscols <- c(ssblue, ssyellow, ssgreen, ssred, sspurple, ssteal, ssgray)

#' @export
ss_theme <- (ggplot2::theme_bw() + ggplot2::theme(panel.border = ggplot2::element_blank(), 
                                                  axis.line = ggplot2::element_line(ssgray),
                                                  panel.grid.major.y = ggplot2::element_line("dashed", size = .5, colour = ssgray),  
                                                  panel.grid.minor.y = ggplot2::element_line("dashed", size = .5, colour = ssgray),  
                                                  panel.grid.major.x = ggplot2::element_line(size = .5, colour = ssgray)))

#' Functions for checking, modifying and formatting colors
#'
#' Checks whether character strings can be interpretted as colors. \code{is.color} is adapted from \code{\link[network]{is.color}}
#' \code{lighten} and \code{darken} return a color lighter/darker than the input.
#' @param x Vector of \code{character} values to check.
#' @param col Vector of colors to modify
#' @param factor Numerical value between 0 and 1 giving how much to modify. 0 is no modification.
#' @describeIn is.color Checks whether the given input can be interpreted as a color.
#' @export
#' @examples
#' # Checking and formatting colors.
#' cols <- c("red1", "red5", "#34AF0B", "#FFFFFF", "#FFFFFG", "#12345", "#123456", "#1234567", "#12345678")
#' x <- seq_along(cols)
#' plot(x, pch = 19, cex = 4, col = ifelse(is.color(cols), cols, "black"))
#' hex(cols)
#' data.table(String = cols, IsColor = is.color(cols), HexColor = hex(cols))
#' 
#' # Making colors lighter or darker.
#' x <- c("red", "green", "blue", "yellow")
#' plot(seq_along(x), rep(1, length(x)), pch = 19, cex = 4, col = darken(x), ylim = c(1, 4))
#' points(seq_along(x), rep(2, length(x)), pch = 19, cex = 4, col = x)
#' points(seq_along(x), rep(3, length(x)), pch = 19, cex = 4, col = lighten(x, factor = 2/3))
#' points(seq_along(x), rep(4, length(x)), pch = 19, cex = 4, col = lighten(x, factor = 11/12))

is.color <- function (x)
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

#' @describeIn is.color Changes color to hex format.
#' @export
hex <- function(col, alpha = 1)  {
  col <- ifelse(is.color(col), col, NA)
  rgb <- col2rgb(col)/255
  if(all(alpha == 1))
    hex <- rgb(red = rgb["red",], green = rgb["green",], blue = rgb["blue",])
  else
    hex <- rgb(red = rgb["red",], green = rgb["green",], blue = rgb["blue",], alpha = alpha)
  hex <- ifelse(is.na(col), NA, hex)
  hex
}

#' @describeIn is.color Darkens a color
#' @export
darken <- function(col, factor = 1/3) {
  rgb <- col2rgb(col)*(1-factor)/255
  newcol <- rgb(red = rgb["red",], green = rgb["green",], blue = rgb["blue",], alpha = 1)
  newcol
}

#' @describeIn is.color Lightens a color
#' @export
lighten <- function(col, factor = 1/3) {
  rgb <- 1- (1-col2rgb(col)/255)*(1-factor)
  newcol <- rgb(red = rgb["red",], green = rgb["green",], blue = rgb["blue",], alpha = 1)
  newcol
}

#' Color Choosing Function
#'
#' Displays a palette of colors allowing a specified number of them to be selected.
#' @param locate Number of colors to select from the palette
#' @param plot.it Logical. If \code{FALSE}, no palette is plotted and \code{colors}() is printed.
#' @export
#' @examples
#' x <- c(10, 40, 25, 50, 20, 15)
#' cols <- choose.colors(length(x))
#' barplot(x, col = cols)

choose.colors <- function (locate = 1, plot.it = T)
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
    print(paste0("Select ", locate, " color", ifelse(locate == 1, "", "s"), " from the palette."))
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
    cols
  }
}

#' Locate Plotted Point
#'
#' Uses \code{locator} to find the index of a plotted point. Useful for identifying outliers.
#' @param dat \code{data.frame} with plotting coordinates given in columns \code{x} and \code{y}. If none of the columns have those names, the first two columns are used.
#' @param print Whether to print the row(s) corresponding to selected points.
#' @param n How many points to select.
#' @param ... Parameters to be passed to \code{plot}.
#' @export
#' @examples
#' locateIndex(trees, n = 3, cex = 2)

locateIndex <- function(dat, print=T, n=1, ...) {
  cols <- colnames(dat)
  if(sum(!c("x", "y") %in% colnames(dat))) {
    colnames(dat)[1:2] <- c("x", "y")
  }
  plot(dat$x, dat$y, ...)
  
  print(paste0("Select ", n, " point", ifelse(n == 1, "", "s"), " from the plot."))
  coord <- data.frame(locator(n))
  ind <- NULL
  for (i in 1:nrow(coord)) {
    dist <- (dat[,1]-coord$x[i])^2 + (dat[,2]-coord$y[i])^2
    ind[i] <- which.min(dist)
  }
  
  points(dat[ind,], col="red", pch=19)
  if(print) {
    colnames(dat) <- cols
    print(dat[ind,])
  }
  ind
}

#' ROC \code{data.table} Function
#'
#' Creates a \code{data.table} with sensitivity, specificity and threshold values associated with a \code{\link[pROC]{roc}} object from package \code{\link[pROC]{pROC}}.
#' @param rc \code{\link[pROC]{roc}} object to evaluate.
#' @param cost.fn, cost.fp Costs associated with false negatives and false positives. Both default to 1.
#' @param plot If \code{TRUE} (default), the ROC curve is plotted using \code{ggroc}.
#' @export
#' @seealso \code{\link{ggroc}}
#' @examples
#' data(aSAH)
#' rc <- pROC::roc(aSAH$outcome, aSAH$s100b, levels=c("Good", "Poor"))
#' dt <- roc_dt(rc)
#' dt

roc_dt <- function (rc, cost.fn = 1, cost.fp = 1, plot = T) {
  dt <- data.table(Sens   = rc$sensitivities,
                   Spec   = rc$specificities,
                   Thresh = rc$thresholds,
                   AUC    = rc$auc)
  dt[, TP   := sapply(Thresh, function(th) sum(rc$cases    > th))]
  dt[, TN   := sapply(Thresh, function(th) sum(rc$controls < th))]
  dt[, FP   := sapply(Thresh, function(th) sum(rc$controls > th))]
  dt[, FN   := sapply(Thresh, function(th) sum(rc$cases    < th))]
  dt[, Cost := FP * cost.fp + FN * cost.fn]
  if (plot)
    print(ggroc(rc))
  setkeyv(dt, "Sens")
  dt
}

#' \code{ggplot2} ROC Curves
#'
#' Plots a ROC curve using \code{ggplot2}.
#' @param rc \code{\link[pROC]{roc}} object to evaluate.
#' @param labels 
#' @param print.auc Where to print the area under the curve (AUC). By default it is put in the title.
#' @param mark.sens, mark.spec Sensitivites and specificities to highlight with plotted lines.
#' @export
#' @seealso \code{\link{ggroc}}
#' @examples
#' data(aSAH)
#' rc <- pROC::roc(aSAH$outcome, aSAH$s100b, levels=c("Good", "Poor"))
#' ggroc(rc)
#' ggroc(rc, print.auc = "plot")

ggroc <- function(rc, labels = NULL, print.auc = c("title", "plot"), mark.sens = NULL, mark.spec = NULL, ...) {
  print.auc = match.arg(print.auc)
  dat <- data.table(Sensitivity = rc$sensitivities,
                    Specificity = rc$specificities,
                    Threshold   = rc$thresholds )
  setkeyv(dat, "Sensitivity")
  
  numcontrols <- length(rc$controls)
  numcases    <- length(rc$cases)
  
  p <- ggplot2::ggplot()
  if(!is.null(mark.sens))
    p <- p + ggplot2::geom_vline(aes(xintercept = ifelse(is.null(mark.spec), NULL, 1-mark.spec)), linetype = 2, col = "slateblue")
  if(!is.null(mark.spec))
    p <- p + ggplot2::geom_hline(aes(yintercept = mark.sens), linetype=2, col="slateblue")
  p <- p + 
    ggplot2::geom_abline(aes(intercept = 0, slope = 1), col = "gray50") + 
    ggplot2::geom_line(data = dat, aes(1 - Specificity, Sensitivity), ...) + 
    ggplot2::labs(title = ifelse(print.auc == "title", paste0("AUC: ", round(rc$auc, 3)), ""), x = "Specificity", y = "Sensitivity", yy = "Cases") +
    ggplot2::theme(panel.grid.minor = element_blank(), 
                   axis.text = element_text(size = 15), 
                   plot.title = element_text(size = 20), 
                   axis.title.x = element_text(size = 20), 
                   axis.title.y = element_text(size = 20))
  left <- ggplot2::ggplot_build(p)$panel$ranges[[1]]$x.range[1]
  bttm <- ggplot2::ggplot_build(p)$panel$ranges[[1]]$y.range[1]
  if(is.null(left)) {
    left <- ggplot2::ggplot_build(p)$layout$panel_ranges[[1]]$x.range[1]
    bttm <- ggplot2::ggplot_build(p)$layout$panel_ranges[[1]]$y.range[1]
  }
  p <- p + 
    ggplot2::geom_vline(xintercept = pretty(c(0, numcontrols))/numcontrols, col = "gray70", lty = 2) +
    ggplot2::scale_x_continuous(breaks = c(left, (0:5)/5, left, pretty(c(0, numcontrols))/numcontrols),
                                labels = c("", paste0((5:0)/5*100, "%"),
                                           "\nControls            ", paste0("\n", scales::comma(pretty(c(0, numcontrols)))) ),
                                limits = 0:1 ) +
    ggplot2::geom_hline(yintercept = pretty(c(0, numcases))/numcases, col = "gray70", lty = 2) +
    ggplot2::scale_y_continuous(breaks = c(bttm, (0:5)/5, bttm, pretty(c(0, numcases))/numcases),
                                labels = c("", paste0((0:5)/5*100, "%"),
                                           "\nCases       ", paste0(scales::comma(pretty(c(0, numcases))), "           ") ),
                                limits = 0:1 ) + 
    ggplot2::geom_hline(aes(yintercept = 0), size = 1) + 
    ggplot2::geom_vline(aes(xintercept = 0), size = 1)
  if (print.auc == "plot")
    p <- p + ggplot2::geom_text(aes(x = 0.55, y = 0.45, label = paste0("AUC: ", round(rc$auc, 3))), hjust = 0, size = 7)
  p
}

#' @describeIn ggroc Creates a ROC graph using \code{ggplot2}
#' @export

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
  setkeyv(dat, c("Name", "Sensitivity", "Specificity"))
  #numcontrols <- length(rc$controls)
  #numcases    <- length(rc$cases)
  
  p <- ggplot2::ggplot()
  if(!is.null(mark.sens))
    p <- p + ggplot2::geom_vline(aes(xintercept = ifelse(is.null(mark.spec), NULL, 1-mark.spec)), linetype = 2, col = "slateblue")
  if(!is.null(mark.spec))
    p <- p + ggplot2::geom_hline(aes(yintercept = mark.sens), linetype=2, col="slateblue")
  p <- p + 
    ggplot2::geom_abline(aes(intercept = 0, slope = 1), col = "gray50") + 
    ggplot2::geom_line(data = dat, aes(1 - Specificity, Sensitivity, col = paste(Name, "\nAUC:", round(AUC, 3)))) + 
    ggplot2::labs(x = "\nSpecificity", y = "Sensitivity\n", col = "") +
    ggplot2::theme(panel.grid.minor = element_blank(), 
                   axis.text = element_text(size=15), 
                   plot.title = element_text(size = 20), 
                   axis.title.x = element_text(size = 20), 
                   axis.title.y = element_text(size = 20),
                   legend.text = element_text(size = 15))
  left <- ggplot2::ggplot_build(p)$panel$ranges[[1]]$x.range[1]
  bttm <- ggplot2::ggplot_build(p)$panel$ranges[[1]]$y.range[1]
  p <- p + 
    ggplot2::geom_hline(aes(yintercept = 0), size = 1) + 
    ggplot2::geom_vline(aes(xintercept = 0), size = 1)
  p
}

#' Kaplan-Meyer Plot for \code{ggplot2}
#' 
#' Creates KM curves for a survival model using \code{ggplot2} functions.
#' @param s Survival model as returned from \code{survfit} in the \code{survival} package.
#' @export

ggsurv <- function(s, CI = 'def', plot.cens = T, surv.col = 'gg.def',
                   cens.col = NULL, lty.est = 1, lty.ci = 2,
                   cens.shape = 3, back.white = F, xlab = 'Time',
                   ylab = 'Survival', main = ''){
  
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
    
    pl <- ggplot2::ggplot(dat, aes(x = time, y = surv)) +
      ggplot2::xlab(xlab) + ggplot2::ylab(ylab) + ggplot2::ggtitle(main) +
      ggplot2::geom_step(col = col, lty = lty.est)
    
    pl <- if(CI == T | CI == 'def') {
      pl + ggplot2::geom_step(aes(y = up), color = col, lty = lty.ci) +
        ggplot2::geom_step(aes(y = low), color = col, lty = lty.ci)
    } else {
      pl
    }
    
    pl <- if(plot.cens == T & length(dat.cens) > 0) {
      if(is.null(cens.col))
        pl + ggplot2::geom_point(data = dat.cens, aes(y = surv, col = group), shape = cens.shape)
      else
        pl + ggplot2::geom_point(data = dat.cens, aes(y = surv), shape = cens.shape, col = cens.col)
    } else if (plot.cens == T & length(dat.cens) == 0) {
      stop ('There are no censored observations')
    } else {
      pl
    }
    
    pl <- if(back.white == T) {
      pl + theme_bw()
    } else {
      pl
    }
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
    
    pl <- ggplot2::ggplot(dat, aes(x = time, y = surv)) +
      ggplot2::xlab(xlab) + ggplot2::ylab(ylab) + ggplot2::ggtitle(main) +
      ggplot2::geom_step(aes(col = group))
    
    if(length(surv.col == 1)) {
      col <- ggplot2::scale_colour_manual(name = gr.name, values = rep(surv.col, strata))
    } else{
      col <- ggplot2::scale_colour_manual(name = gr.name, values = surv.col)
    }
    
    pl <- if(surv.col[1] != 'gg.def'){
      pl + col
    } else {pl + ggplot2::scale_colour_discrete(name = gr.name)}
    
    if(length(lty.est) == 1) {
      line <- ggplot2::scale_linetype_manual(name = gr.name, values = rep(lty.est, strata))
    } else {
      line <- ggplot2::scale_linetype_manual(name = gr.name, values = lty.est)
    }
    
    pl <- pl + line
    
    pl <- if(CI == T) {
      if(length(surv.col) > 1 && length(lty.est) > 1){
        stop('Either surv.col or lty.est should be of length 1 in order
             to plot 95% CI with multiple strata')
      } else if((length(surv.col) > 1 | surv.col == 'gg.def')[1]){
        pl + ggplot2::geom_step(aes(y = up, color = group), lty = lty.ci) +
          ggplot2::geom_step(aes(y = low, color = group), lty = lty.ci)
      } else {
        pl +  ggplot2::geom_step(aes(y = up, lty = group), col = surv.col) +
          ggplot2::geom_step(aes(y = low, lty = group), col = surv.col)
      }
    } else {
      pl
    }
    
    
    pl <- if(plot.cens == T & length(dat.cens) > 0) {
      if(is.null(cens.col))
        pl + ggplot2::geom_point(data = dat.cens, aes(y = surv, col = group), shape = cens.shape)
      else
        pl + ggplot2::geom_point(data = dat.cens, aes(y = surv), shape = cens.shape, col = cens.col)
    } else if (plot.cens == T & length(dat.cens) == 0) {
      stop ('There are no censored observations')
    } else {
      pl
    }
    
    pl <- if(back.white == T) {
      pl + ggplot2::theme_bw()
    } else {
      pl
    }
    pl
  }
  if(strata == 1) {
    pl <- ggsurv.s(s, CI , plot.cens, surv.col, cens.col, lty.est, lty.ci, cens.shape, back.white, xlab, ylab, main)
  } else {
    pl <- ggsurv.m(s, CI, plot.cens, surv.col, cens.col, lty.est, lty.ci, cens.shape, back.white, xlab, ylab, main)
  }
  pl
}

#' Scale DayYear
#'
#' Adjusts the scale of an axis representing time to show days and years. Used with \code{ggplot2}. Not currently working.
#' @export
#' @example
#' 

scale_x_dayyear <- function(p) {
  values <- with(p$data, eval(p$mapping[["x"]]))
  days <- 365.25
  # left <- ggplot2::ggplot_build(p)$panel$ranges[[1]]$x.range[1]
  left <- ggplot2::ggplot_build(p)$layout$panel_ranges[[2]]$x.range[1]
  p + ggplot2::scale_x_continuous(breaks = c(left, pretty(range(values, na.rm=T)),
                                             left, pretty(range(values/days, na.rm = T))*days ), 
                                  labels = c("Days:", pretty(range(values, na.rm = T)),
                                             "\nYears:", paste0("\n", pretty(range(values/days, na.rm = T))) ) ) + 
    ggplot2::geom_vline(data = data.frame(x = pretty(values/days)*days), aes(xintercept = x), linetype = 2, col = "seagreen", size = 1) +
    ggplot2::theme(panel.grid.minor = element_blank(),
                   axis.text.x = element_text(colour = rep(c("black", "seagreen"),
                                                           times = c(length(pretty(range(values, na.rm = T)))+1,
                                                                     length(pretty(range(values/days, na.rm = T)))+1 )), size = 15)) +
    p$layers
}


## Adding to ggplot2
# geom_violin2 creates a 2-sided violin plot
# geom_polyline creates a polynomial line

ggen <- environment(ggplot2::ggplot)

# geom_violin2

GeomViolin2 <- ggplot2::ggproto(ggplot2::GeomViolin)
for(i in names(ggplot2::GeomViolin)) {
  GeomViolin2[[i]] <- ggplot2::GeomViolin[[i]]
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

#' Modified Violin Plot
#'
#' Creates a violin plot with two different distributions on each side. Use similar to \code{geom_violin}
#' with a two-level factor variable in the \code{group} or \code{fill} arguments.
#' @export
#' @examples
#' library(MASS)
#' # geom_violin from ggplot2 package
#' ggplot(survey, aes(Fold, Wr.Hnd, fill = Sex)) + geom_violin()
#' # New geom_violin2 combined graphs
#' ggplot(survey, aes(Fold, Wr.Hnd, fill = Sex)) + geom_violin2()
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

# geom_polyline

GeomPolyline <- ggplot2::ggproto(ggplot2::GeomAbline)
for(i in names(ggplot2::GeomAbline)) {
  GeomPolyline[[i]] <- ggplot2::GeomAbline[[i]]
}
class(GeomPolyline) <- c("GeomAbline", "Geom", "ggproto")
GeomPolyline$required_aes <- paste0("c", 0)
GeomPolyline$optional_aes <- paste0("c", 1:5)
# GeomPolyline$draw_group <- GeomAbline$draw_group
GeomPolyline$draw_panel <- function (data, panel_scales, coord, precision) 
{
  ranges <- coord$range(panel_scales)
  
  #data$group <- 1:nrow(data)
  
  poly_order <- max(as.numeric(gsub("c", "", grep("^c[[:digit:]]*$", names(data), value = T, ignore.case = T))))
  for(i in 0:poly_order) {
    if(is.null(data[[paste0("c", i)]]))
      data[[paste0("c", i)]] <- 0
  }
  
  newdata <- do.call(rbind, lapply(1:nrow(data), function(i) do.call(data.frame, c(list(x = c(seq(ranges$x[1], ranges$x[2], length.out = precision), NA)), data[i,]))))
  newdata$y <- newdata$c0
  i = 1
  while(!is.null(newdata[[paste0("c", i)]])) {
    newdata$y <- newdata$y + newdata[[paste0("c", i)]] * newdata$x ^ i
    i <- i+1
  }
  # data <- data[data$x >= ranges$x[1] & data$x <= ranges$x[2] & data$y >= ranges$y[1] & data$y <= ranges$y[2],]
  with(newdata, {
    x <- ifelse(x < ranges$x[1] | x > ranges$x[2], NA, x)
    y <- ifelse(y < ranges$y[1] | y > ranges$y[2], NA, y)
  })
  
  # browser()
  
  # newdata <- merge(newdata, data, by = NULL)
  
  GeomLine$draw_panel(newdata, panel_scales, coord)
}
environment(GeomPolyline) <- ggen

#' @export
GeomPolyline

#' Plot Polynomial Curve
#'
#' Creates a polynomial line, similar to \code{geom_abline}
#' @export
#' @examples
#' dat <- data.table(x = rnorm(1e3))[, y := rnorm(1e3, x, .2)^2][, x2 := x^2]
#' coefs <- lm(y ~ ., data = dat)$coef
#' 
#' ggplot(data.frame(x = x, y = rnorm(1e3, x, .2)^2), aes(x, y)) + geom_point() + geom_polyline(aes(c0 = coefs[1], c1 = coefs[2], c2 = coefs[3]))
geom_polyline <- function (mapping = NULL, data = NULL, precision = 100, ..., c0,
                           na.rm = FALSE, show.legend = NA) 
{
  # browser()
  if (missing(mapping) && missing(c0)) {
    c0 <- 0
    c1 <- 1
    data <- data.frame(c0 = c0, c1 = c1)
  }
  # data <- data.frame(precision = precision)
  # if (!missing(slope) || !missing(intercept)) {
  #   if (missing(slope)) 
  #     slope <- 1
  #   if (missing(intercept)) 
  #     intercept <- 0
  #   data <- data.frame(intercept = intercept, slope = slope)
  #   mapping <- aes(intercept = intercept, slope = slope)
  #   show.legend <- FALSE
  # }
  layer(data = data, mapping = mapping, stat = StatIdentity, 
        geom = GeomPolyline, position = PositionIdentity, show.legend = show.legend, 
        inherit.aes = FALSE, params = list(na.rm = na.rm, precision = precision, ...))
}
environment(geom_polyline) <- ggen




#' Check Predicted Probabilities and Linearity of Means
#' 
#' Compares the proportion of success to a predicted probability using \code{ggplot2} functions. 
#' Alternatively, provides a check of linearity relative to a given covariate.
#' @param data \code{data.frame} as passed to \code{ggplot}()
#' @param mapping Aesthetic mapping using \code{aes}() as in \code{ggplot}. \code{x} and \code{y} are both required.
#' @param bins The number of quantiles to bin predicted probabilities into. Default is 10.
#' @param bound Whether to bound confidence intervals to the range [0, 1]. Default is \code{TRUE}
#' @param predicted Whether the given \code{x} represents a predicted probability. Default is \code{TRUE}.
#' @param link Provides a link function for the binned means, if \code{predicted} is \code{FALSE}.
#' @param stat, position, na.rm, show.legend, inherit.aes, ... Parameters that get passed through to \code{geom_errorbar}.
#' @param pt... \code{list} object with parameters to pass through to \code{geom_point}.
#' @export
#' @examples
#' esoph_mod <- glm(cbind(ncases, ncontrols) ~ ., data = esoph, family = "binomial")
#' esoph2 <- cbind(esoph, pred_prob = gtools::inv.logit(predict(esoph_mod)))
#' ggprop(esoph2, aes(pred_prob, cbind(ncases, ncontrols)))
#' ggprop(esoph2, aes(pred_prob, cbind(ncases, ncontrols)), predicted = F)
#' ggprop(esoph2, aes(pred_prob, cbind(ncases, ncontrols)), predicted = F, link = "identity")

ggprop <- function(data, mapping, link = binomial(), predicted = T, stat = "identity", position = "identity", bins = 10, bound = T, ..., pt... = NULL, na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  # currently only works with binary input, not two-column counts.
  x <- with(data, eval(mapping$x))
  y <- with(data, eval(mapping$y))
  data <- data.table(data)
  if(is.null(dim(y)) & length(unique(y)) > 2)
    stop("y parameter must be dichotomous.")
  if(!is.null(dim(y)))
    if(ncol(y) != 2)
      stop("y parameter must be dichotomous.")
  
  if(is.character(link))
    link = binomial(link)
  if(predicted)
    link = binomial("identity")
  
  if(!is.null(dim(y))) {
    data <- cbind(data, "_x" = x, setnames(data.table(y), c("_Pos", "_Neg")))
    data[, Index := 1:nrow(data)]
    data <- data[, list(x = `_x`, y = c(rep(1, `_Pos`), rep(0, `_Neg`))), by = setdiff(colnames(data), c("_x", "_Pos", "_Neg"))]
    data$Index <- NULL
  } else {
    if(any(! y %in% 0:1))
      y <- as.numeric(factor(y)) - 1
    data$x <- x
    data$y <- y
  }
  rm(x, y)
  
  data2 <- with(data, data.table(x = quantile(x, (1:bins)/bins - .5/bins),
                                 y = link$linkfun(tapply(y, gtools::quantcut(x, (0:bins)/bins), mean)),
                                 n = tapply(y, gtools::quantcut(x, (0:bins)/bins), length) ))
  # data2 <- data[, {
  #   lst <- list(x = quantile(x, (1:bins)/bins - .5/bins),
  #               y = link$linkfun(tapply(y, gtools::quantcut(x, (0:bins)/bins), mean)),
  #               n = tapply(y, gtools::quantcut(x, (0:bins)/bins), length) )
  #   lst
  # }, by = eval(as.character(mapping[intersect(c("colour", "group"), names(mapping))])) ]
  
  if(predicted) {
    data2[, ymin := pmax(y - 1.96 * sqrt(y*(1-y)/n), 0)]
    data2[, ymax := pmin(y + 1.96 * sqrt(y*(1-y)/n), 1)]
    mapping <- ggplot2::aes(x = x, y = y, ymin = ymin, ymax = ymax)
    p <- ggplot2::ggplot() + do.call(geom_point, c(list(data = data2, mapping = mapping), pt...)) + 
      ggplot2::geom_errorbar(data = data2, mapping = mapping, stat = stat, position = position, ..., na.rm = na.rm, show.legend = show.legend, inherit.aes = inherit.aes)
    p <- p + ggplot2::geom_line(data = data.table(x = pretty(data2$x), y = pretty(data2$x)), mapping = aes(x, y), linetype = 2)
  } else {
    mapping <- ggplot2::aes(x = x, y = y)
    p <- ggplot2::ggplot() + ggplot2::geom_point(data = data2, mapping = mapping, stat = stat, position = position, ..., na.rm = na.rm, show.legend = show.legend, inherit.aes = inherit.aes)
  }
  p
}


#' Plot Regression Lines
#' 
#' Plots response variable against all explanatory variables with estimated regression lines.
#' @param mod Linear regression model as returned by \code{lm()}.
#' @export
#' @examples
#' library(data.table)
#' n <- 1e3
#' 
#' # Three independent explanatory variables
#' coef <- c(2.1, -0.8, 1.2)
#' dt <- data.table(x1 = rnorm(n), x2 = rnorm(n, , 2), x3 = rnorm(n, 4))
#' dt[, Mean := cbind(x1, x2, x3) %*% coef]
#' dt[, y := rnorm(.N, Mean, .5)]
#' mod <- lm(y ~ x1 + x2 + x3, data = dt)
#' reglines(mod)
#' 
#' # Three correlated explanatory variables
#' coef <- c(2.1, -0.8, 1.2)
#' dt <- data.table(x1 = rnorm(n))
#' dt[, x2 := rnorm(.N, -.8 + .7*x1)]
#' dt[, x3 := rnorm(.N, 1.2 + 1.1*x1 + .5*x2)]
#' dt[, Mean := cbind(x1, x2, x3) %*% coef]
#' dt[, y := rnorm(.N, Mean, .5)]
#' mod <- lm(y ~ x1 + x2 + x3, data = dt)
#' reglines(mod)
#' 
#' # Additional interaction term
#' coef <- c(2.1, -0.8, 1.2, -0.9)
#' dt <- data.table(x1 = rnorm(n))
#' dt[, x2 := rnorm(.N, -.8 + .7*x1)]
#' dt[, x3 := rnorm(.N, 1.2 + 1.1*x1 + .5*x2)]
#' dt[, Mean := cbind(x1, x2, x3, x1*x2) %*% coef]
#' dt[, y := rnorm(.N, Mean, .5)]
#' mod <- lm(y ~ x1 + x2 + x3 + x1*x2, data = dt)
#' reglines(mod)
#' 
#' # Additional categorical variable
#' coef <- c(2.1, -0.8, 1.2, -0.9, 2)
#' dt <- data.table(x1 = rnorm(n))
#' dt[, x2 := rnorm(.N, -.8 + .7*x1)]
#' dt[, Group := letters[rbinom(.N, 2, .5) + 1]]
#' dt[, x3 := rnorm(.N, 1.2 + 1.1*x1 + .5*x2)]
#' dt[, Mean := cbind(x1, x2, x3, x1*x2, factor(Group)) %*% coef]
#' dt[, y := rnorm(.N, Mean, .5)]
#' mod <- lm(y ~ x1 + x2 + Group + x3 + x1*x2, data = dt)
#' reglines(mod)
#' 
#' # Interaction with poorly fitted model
#' coef <- c(2.1, -0.8, 1.2, -0.9, 2, -3)
#' dt[, Mean := cbind(x1, x2, x3, x1*x2, factor(Group), as.numeric(factor(Group))*x3) %*% coef]
#' dt[, y := rnorm(.N, Mean, .5)]
#' mod <- lm(y ~ x1 + x2 + Group*x3 + x3 + x1*x2, data = dt)
#' reglines(mod)
#' 
#' # Interaction with better model
#' coef <- c(2.1, -0.8, 1.2, -0.9, 2, -3)
#' dt[, Mean := cbind(x1, x2, x3, x1*x2, factor(Group), as.numeric(factor(Group))*x3) %*% coef]
#' dt[, y := rnorm(.N, Mean, .5)]
#' mod <- lm(y ~ (x1 + x2 + x3)*Group + x1*x2, data = dt)
#' reglines(mod)


reglines <- function(mod) {
  data <- data.table(mod$model)
  setnames(data, replace(names(data), 1, "y"))
  vars <- sapply(data[, -1, with = F], class)
  isfactor <- sum(c("character", "factor") %in% vars) > 0
  if(isfactor)
    factorvar <- names(data)[which(sapply(data, class) %in% c("factor", "character"))]
  vars <- names(vars)[vars %in% c("numeric", "integer")]
  p <- length(vars)
  
  coefs <- NULL
  for (i in 1:p) {
    tmpdata <- data[, .SD]
    tmpdata[, (vars[-i]) := as.list(rep(0, length(vars) - 1))]
    
    call <- mod$call
    call$data <- as.name("tmpdata")
    
    if(isfactor) {
      form <- (~ -1 + 1)[[2]]
      form[[3]] <- call$formula[[3]]
      call$formula[[3]] <- form
    }
    
    tmpmod <- eval(call)
    tmpcoef <- coef(tmpmod)
    tmpcoef <- tmpcoef[!is.na(tmpcoef)]
    
    slp <- tmpcoef[grep(paste0("(^|:)", vars[i]), names(tmpcoef))]
    slp[-1] <- slp[1] + slp[-1]
    int <- tmpcoef[-grep(paste0("(^|:)", vars[i]), names(tmpcoef))]
    
    if(isfactor)
      coefs <- rbind(coefs, data.table(Coefficient = vars[i], 
                                       Intercept = int, 
                                       Slope = slp, 
                                       Factor = names(int) ))
    else
      coefs <- rbind(coefs, data.table(Coefficient = vars[i], Intercept = tmpcoef[-which(names(tmpcoef) == vars[i])], Slope = tmpcoef[vars[i]]))
  }
  if(isfactor) {
    coefs[, Factor := gsub(paste0("^", factorvar), "", Factor)]
    setnames(coefs, "Factor", factorvar)
  }
  
  data2 <- melt(data, measure.vars = vars, variable.name = "Coefficient", value.name = "Value")
  if(isfactor)
    g <- ggplot(data2, aes(Value, y, col = get(factorvar))) + geom_point() + facet_wrap(~Coefficient, scales = "free") + 
    geom_abline(data = coefs, aes(slope = Slope, intercept = Intercept, col = get(factorvar))) + labs(y = as.character(mod$call$formula[[2]]))
  else
    g <- ggplot(data2, aes(Value, y)) + geom_point() + facet_wrap(~Coefficient, scales = "free") + geom_abline(data = coefs, aes(slope = Slope, intercept = Intercept)) + labs(y = as.character(mod$call$formula[[2]]))
  plot(g)
  
  invisible(coefs)
}

#' Plot Regression Lines and Curves
#' 
#' Expand on previous function to allow more complicated functions -- polynomials, exponentials, etc.
#' @param mod A linear regression model
#' @export
#' @examples
#' reglines2(mod)

reglines2 <- function(mod) {
  
}
