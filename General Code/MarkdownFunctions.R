# Useful things for markdown files
 library(data.table)
 library(ggplot2)
 library(knitr)
 library(pROC)
 
 #ss_theme <- theme_bw() + 
 #            theme(panel.border = element_blank(), 
 #                  axis.line = element_line(), 
 #                  panel.grid.major.y = element_line("dashed", size = .5, colour="blue"), 
 #                  plot.margin = unit(c(1, 2, 0.5, 0.5), "lines"))
 
 #ss_themeGr <- theme_bw() + theme(panel.border = element_blank(), axis.line = element_line()) + theme(panel.grid.major.y = element_line("dashed", size = .5, colour = "grey"), plot.margin = unit(c(1, 2, 0.5, 0.5), "lines"))
 #options(width = 200, warn = -1)
 
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
 
 
 
 # Creating bulleted lists
 html_list <- function(strings, details = NULL, detailpref = "- ", kind = c("ul","ol","dl"), type = c(1,"A","a","I","i")) {
   kind <- match.arg(kind)
   type <- match.arg(type)
   str <- paste("<", kind, " type = \"",type,"\">\n",
                paste("<li>", strings, "</li>",
                      sapply(details, function(det) paste("\n<dd>", detailpref, det, "</dd>", sep = "", collapse = "")),
                      sep = "", collapse = "\n" ),
                "\n</", kind, ">", sep = "" )
   str
 }
 
 roundNumeric <- function(dt, digits=1) {
   dt <- as.data.table(lapply(dt, function(x) {
     if(class(x) == "numeric") 
       round(x, digits) 
     else 
       x
   }))
 }
 
 scale_x_dayyear <- function(p) {
   values <- with(p$data, eval(p$mapping[["x"]]))
   days <- 365.25
   left <- ggplot_build(p)$panel$ranges[[1]]$x.range[1]
   p + scale_x_continuous(breaks = c(left, pretty(range(values, na.rm=T)),
                                     left, pretty(range(values/days, na.rm = T))*days ), 
                          labels = c("Days:", pretty(range(values, na.rm = T)),
                                     "\nYears:", paste0("\n", pretty(range(values/days, na.rm = T))) ) ) + 
     geom_vline(data = data.frame(x = pretty(values/days)*days), aes(xintercept = x), linetype = 2, col = "seagreen", size = 1) +
     theme(panel.grid.minor = element_blank(),
           axis.text.x = element_text(colour = rep(c("black", "seagreen"),
                                                   times = c(length(pretty(range(values, na.rm = T)))+1,
                                                             length(pretty(range(values/days, na.rm = T)))+1 )), size = 15)) +
     p$layers
 }
 
 
 
 
 
 
 
 # End script
 