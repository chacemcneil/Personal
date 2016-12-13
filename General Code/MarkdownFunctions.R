# Useful things for markdown files
 library(data.table)
 library(ggplot2)
 library(htmlTable)
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
 
 
 markdown_init <- function(filename, echo = T) {
   ## initialize functions and options to manage figure/table labels, used for html markdown and notebooks.
   #    Uses htmlTable for all tables.
   
   print(environment())
   print(parent.env(environment()))
   print(parent.frame())
   env <- parent.env(environment())
   with(env, print(current_input()))
   with(environment(), {
     ## Look through the script to find chunk labels
     rmdCon <- file(filename, open = "r")
     rmdLines <- readLines(rmdCon)
     close(rmdCon)
     rmdLines <- rmdLines[grep("^```\\{r\\s+([[:alnum:]_\\.]+)[,\\}]", rmdLines)]
     chunk_labels <- gsub("^```\\{r\\s+([[:alnum:]_\\.]+)[,\\}].*$", "\\1", rmdLines)
     if(chunk_labels[1] == "setup")
       chunk_labels <- chunk_labels[-1]
     
     ## Add automatic figure captions (can be kept separate from table captions, but requires tables to be kept track of by htmlTable)
     options(figure_counter = T, figures = chunk_labels)
     capexpr <- expression({
       lab <- opts_current$get("label")
       if(grepl("^unnamed", lab) || !options()$figure_counter) {
         if(is.null(opts_current$get("caption")))
           cap <- ""
         else
           cap <- opts_current$get("caption")
       } else {
         if(is.null(opts_current$get("caption")))
           cap <- paste("<b>Figure ", match(lab, options()$figures), "</b>")
         else
           cap <- paste("<b>Figure ", match(lab, options()$figures), ":</b> ", opts_current$get("caption"))
       }
       cap
     })
     idexpr <- expression(paste0('id="', opts_current$get("label"), '"'))
     
     ## Set expressions to be evaluated for each chunk
     # fig.cap = capexpr     sets the caption for the figure. Use caption="Plot description" in chunk header.
     # out.extra = idexpr    Puts an id attribute in the <img> tag matching the chunk label (<img id="label" ... />)
     #                       This id is then used by htmlref to create a link to the image when referenced.
     # echo                  determines whether code is shown, generally true for notebooks and false for markdown
     # fig.align = "center"  centers all figures as well as their associated captions
     opts_chunk$set(fig.cap = capexpr, out.extra = idexpr, fig.align = "center", echo = echo, fig.align = "center")
     
     ## Initialize counter and caption used by htmlTable
     options(table_counter = TRUE, table_counter_str = "<b>Table %s:</b> ")
     
     # Function that creates html hyperlinks intended for inline figure and table references. When referencing tables created
     #   by htmlTable, the strings must be created immediately after table creation, and then the strings can be put inline.
     htmlref <<- function(label, offset = 0, pattern = NULL, link = T) {
       opts_current$set(dependson = label)
       if(is.null(pattern))
         pattern <- ifelse(label %in% options()$figures, "Figure %s", "Table %s")
       str <- ifelse(label %in% options()$figures,
                     sprintf(pattern, match(label, options()$figures)),
                     sprintf(pattern, tblNoLast() + offset) )
       if(link)
         str <- paste0("<a href = \"#", label, "\">", str, "</a>")
       str
     }
     cat("<style>\ntable.gmisc_table {\n margin-left: auto;\n margin-right: auto;\n}\n</style>\n")
   })
 }
 
 
 
 
 
 # End script
 