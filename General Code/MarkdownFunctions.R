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
 sscols <- c(ssblue, ssyellow, ssgreen, ssred, sspurple, ssteal, ssgray)
 
 ss_theme <- theme_bw() + theme(panel.border = element_blank(), 
                                axis.line = element_line(ssgray),
                                panel.grid.major.y = element_line("dashed", size = .5, colour = ssgray),  
                                panel.grid.minor.y = element_line("dashed", size = .5, colour = ssgray),  
                                panel.grid.major.x = element_line(size = .5, colour = ssgray))
 
 # HTML options use at the beginning of markdown as follows
 # ---
 # title: "Title"
 # output:
 #   html_document:
 #     theme: united
 #     highlight: tango
 # ---
 html_themes <- c("default", "cerulean", "journal", "flatly", "readable", "spacelab", "united", "cosmo", "lumen",  "paper", "sandstone", "simplex", "yeti")
 html_highlights <- c("default", "tango", "pygments", "kate",  "monochrome", "espresso", "zenburn", "haddock", "textmate")
 
 
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
 
 html_summary <- function(mod, names = NULL, ...) {
   # Forms an htmlTable from a linear model
   tab <- summary(mod)$coefficients
   if(!is.null(names))
     row.names(tab) <- names
   colnames(tab)[4] <- "P-value"
   tab[,1] <- round(tab[,1], digits = 3)
   tab[,2] <- signif(tab[,2], digits = 4)
   tab[,3] <- round(tab[,3], digits = 2)
   tab[,4] <- signif(tab[,4], digits = 2)
   tab[,4] <- ifelse(tab[,4] < 0.0001, "&lt;0.0001", tab[,4])
   htmlTable(tab, ...)
 }
 
 chunkplot <- function(chunk, num) {
   require(knitr)
   include_graphics(paste0(opts_chunk$get("fig.path"), chunk, "-", num, ".png"))
 }
 
 sidebyside <- function(..., sep = 1) {
   lst <- list(...)
   sep <- paste0("<th>", paste0(rep("&nbsp;", sep), collapse = ""), "</th>")
   print("<table class='gmisc_table' style='border-collapse: collapse;'>\n<tr>")
   for (i in seq_along(lst)) {
     print("<th>")
     print(lst[[i]])
     print("</th>")
   }
   print("</tr>\n</table>")
   html <- paste0("<table class='gmisc_table' style='border-collapse: collapse;' >\n<tr>",
                  paste0("<th>", c(...), "</th>", collapse = sep),
                  "</tr>\n</table>")
   html <- c("<table class='gmisc_table' style='border-collapse: collapse;' >\n<tr>",
                  unlist(paste0("<th>", lst, "</th>")),
                  "</tr>\n</table>")
   class(html) <- c("knit_image_paths", "knit_asis")
   html
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
 