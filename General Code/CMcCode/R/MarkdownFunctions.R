### Collection of functions that create or modify ggplot2 graphs. The included functions are:
# html_list               Provides the html code for listing elements of a vector
# html_dropdown           Writes javascript functions for a dropdown menu
# html_summary            Creates an htmlTable object for model output
# fixedheader             Creates an htmlTable object for model output


# Useful things for markdown files
library(data.table)
library(ggplot2)
library(htmlTable)
library(htmltools)
library(knitr)
library(pROC)

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


#' HTML List Function
#' 
#' Creates HTML lists containing the elements of a vector.
#' @param x Vector of values to list. Will be converted to \code{character}
#' @export
#' @examples
#' x <- paste("Item", 1:8)
#' 
#' # Unordered list (default)
#' html_list(x)
#' 
#' # Ordered list
#' html_list(x, kind = "dl")

html_list <- function(strings, details = NULL, detailpref = "- ", kind = c("ul","ol","dl"), type = c(1,"A","a","I","i"), parent_element = NULL) {
  kind <- match.arg(kind)
  type <- match.arg(type)
  if(!is.null(parent_element))
    parent_element <- paste0(parent_element, "\n")
  str <- paste(parent_element, "<", kind, " type = \"",type,"\">\n",
               paste("<li>", strings, "</li>",
                     sapply(details, function(det) paste("\n<dd>", detailpref, det, "</dd>", sep = "", collapse = "")),
                     sep = "", collapse = "\n" ),
               "\n</", kind, ">", sep = "" )
  str
}

#' HTML Dropdown Function
#' 
#' Creates HTML dropdown list containing the elements of a vector.
#' @param id Character, id of the dropdown menu. Must be unique from other ids
#' @param labels Vector of values to display in dropdown list. Will be converted to \code{character}
#' @param values Values to associate with labels. If NULL (default), names of \code{labels} will be used. Can be character or numeric.
#' @export
#' @examples
#' x <- c(first = 10, second = 20, third = 30)
#' 
#' # Dropdown list with default values
#' html_dropdown("tmp", x)
#' 
#' # Dropdown list with selected values
#' html_dropdown("tmp", x, values = 1:4)

html_dropdown <- function (id, divs, labels = NULL, onchange = paste0(id, "_function(value);"), onload = "") {
  if(is.null(labels)) {
    if(any(names(divs) == ""))
      stop("Must supply labels or a named vector for divs.")
    else
      labels <- names(divs)
  }
  str <- paste("<script>\nfunction ", id, "_function (value) {\n", 
               paste(divs, ".hidden = true;", sep = "", collapse = "\n"), 
               "\nconsole.log(value);\n", "document.getElementById(value).hidden = false;\n}\n</script>", 
               "\n<select id = \"", id, "\" onchange = \"", onchange, "\" onload = \"", onload, "\">", 
               paste("\n<option value = ", divs, ">", labels, "</option>", sep = "", collapse = ""), 
               "\n</select>", "<div id = \"", id, "_disp\"></div>", sep = "")
  str
}

#' HTML Model Summary Function
#' 
#' Creates an \code{htmlTable} object for a model coefficient table.
#' @param mod Model object.
#' @param names Optional 4-element vector of column names.
#' @param caption Optional caption for the table.
#' @export
#' @examples
#' # From lm documentation:
#' ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
#' trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
#' group <- gl(2, 10, 20, labels = c("Ctl","Trt"))
#' weight <- c(ctl, trt)
#' lm.D9 <- lm(weight ~ group)
#' html_summary(lm.D9)

html_summary <- function(mod, names = NULL, caption = NULL, ...) {
  # Forms an htmlTable from a linear model
  tab <- summary(mod)$coefficients
  if(is.null(caption))
    caption = paste("<b>Response variable:</b>", as.character(mod$call[[2]])[2])
  if(!is.null(names))
    row.names(tab) <- names
  colnames(tab)[4] <- "P-value"
  tab[,1] <- round(tab[,1], digits = 3)
  tab[,2] <- signif(tab[,2], digits = 4)
  tab[,3] <- round(tab[,3], digits = 2)
  tab[,4] <- round(tab[,4], 4)
  tab[,4] <- ifelse(tab[,4] < 0.0001, "&lt;0.0001", tab[,4])
  htmlTable::htmlTable(tab, caption = caption, ...)
}

#' HTML Table with Fixed Header
#' 
#' Writes CSS and Javascript for a fixed table header for tables of a specified class.
#' @param classname Default is 'fixedheader'. Class name of tables that should have fixed headers.
#' @export
#' @examples
#' dt <- data.frame(x = 1:100, y = rnorm(100))
#' # Without fixed header
#' tab <- htmlTable(dt)
#' htmltools::tags$div(tab, style = "height: 300px; overflow-y: scroll;")
#' # With fixed header (only works when knit, otherwise the CSS is not used)
#' tab <- htmlTable(dt, css.class = "fixedheader")
#' htmltools::tags$div(tab, style = "height: 300px; overflow-y: scroll;")
fixedheaders <- function(header_background = "#447", header_color = "#FEE") {
  txt_script <- paste(
    '<script>',
    '  HTMLCollection.prototype.forEach = Array.prototype.forEach;',
    'function fixheaders(table) {',
    '  var thead = table.getElementsByTagName("thead")[0];',
    '  if(thead.getElementsByTagName("tr").length > 1) {',
    '    var th_height = 0;',
    '    for(i=0; i < thead.getElementsByTagName("tr").length; i++) {',
    '      if(i > 0) {',
    '        th_height += Number(getComputedStyle(thead.getElementsByTagName("tr")[i-1].children[0]).height.match(/[0-9\\.]+/g));',
    '      }',
    '      thead.getElementsByTagName("tr")[i].getElementsByTagName("th").forEach(function(item) {',
    '        item.style.top = th_height + "px";',
    '        if(item.innerHTML == "") {',
    '          item.style.background = "#FFF";',
    '        }',
    '      })',
    '    }',
    '  }',
    '}',
    'window.onload = function() {',
    '  document.getElementsByClassName("fixedheader").forEach(fixheaders);',
    '};',
    '</script>', sep = "\n")
  txt_style <- paste(
    '<style>',
    '  .fixedheader {',
    '    font-size: 15pt;',
    '  }',
    '.fixedheader thead tr th {',
    '  background: ', header_background, ';',
    '  color: ', header_color, ';',
    '  text-align: center;',
    '  position: sticky;',
    '  height: 20pt;',
    '  top: 0;',
    '  border-left: 3px solid #FFF;',
    '}',
    '.fixedheader tbody tr:nth-child(even) {',
    '  background: #DDD;',
    '}',
    '.fixedtotal tbody tr:nth-last-child(1) td {',
    '  background: #99B;',
    '  position: sticky;',
    '  bottom: 0;',
    '}',
    '</style>', sep = "\n" )
  txt <- paste(txt_script, txt_style, sep = "\n")
  class(txt) <- c("html", "character")
  txt
}

#' HTML Scrolling
#' 
#' Wraps HTML code in a \code{<div>} tag with style set to limit the space on the page. All overflow content is accessed by scrolling.
#' @param tab \code{htmlTable} object or other HTML code to insert in scroll window.
#' @param height Height of the \code{<div>} container on the page. Default is "300px"
#' @param width Width of the \code{<div>} container on the page. Default is "100%"
#' @export
#' @examples
#' # Create table
#' tab <- htmlTable(mtcars)
#' scroll(tab)

scroll <- function (tab, height = "300px", width = "100%") {
  tab <- paste0("<div style=\"overflow-x: scroll; width: ", width, "; overflow-y: scroll; height: ", height, "\">", tab, "</div>")
  class(tab) <- c("htmlTable", "character")
  tab
}

#' HTML Model Formula Function
#' 
#' Creates a formula for a fitted model.
#' @param mod Model object.
#' @export
#' @examples
#' # From lm documentation:
#' ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
#' trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
#' group <- gl(2, 10, 20, labels = c("Ctl","Trt"))
#' weight <- c(ctl, trt)
#' lm.D9 <- lm(weight ~ group)
#' html_summary(lm.D9)

html_model <- function(mod, caption = NULL, ...) {
  # Forms an htmlTable from a linear model
  coef <- coef(mod)
  # Not finished. Goal is to output math equation.
}

#' Chunk Plot Function
#' 
#' Returns a plot generated in a designated markdown chunk. Useful when plot needs to be outside of the chunk (e.g. next to a table).
#' @param chunk Name of the chunk where the plot is generated.
#' @param num If more than one plot are created in a chunk, \code{num} designates which plot to return
#' @export
#' @examples
#' ```{r my_chunk}
#' barplot(1:5, col = 1:5)
#' histogram(rnorm(1e3))
#' ```
#' chunkplot("my_chunk", 1)
chunkplot <- function(chunk, num = 1) {
  knitr::include_graphics(paste0(knitr::opts_chunk$get("fig.path"), chunk, "-", num, ".png"))
}

#' Side by Side Function
#' 
#' Puts plots/tables side by side. NOT WORKING!
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

#' Initialize Markdown
#' 
#' @export
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

#' Animated plot
#' 
#' Returns a sequence of plots that transition between given plots.
#' @param ... \code{ggplot2} objects to transition between. At least 2 needed.
#' @param n Number of frames between plots. Default is 20.
#' @export
#' @examples
#' 
gganimate <- function(..., n = 20) {
  lst <- list(...)
  if(any(sapply(lst, function(x) ! "gg" %in% class(x))))
    stop("Only objects of class gg are allowed.")
  if(length(lst) < 2)
    stop("At least two plots needed.")
  
  # Function not completed
  return(NULL)
}

# Use the following when using animated plots
# <script src="https://ajax.googleapis.com/ajax/libs/jquery/1.7.1/jquery.min.js"></script>
# <script src="http://vis.supstat.com/assets/themes/dinky/js/jquery.scianimator.min.js"></script>


# End script
