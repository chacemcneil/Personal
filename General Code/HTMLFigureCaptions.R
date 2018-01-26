# Code to help create captions in html markdown files.
 library(knitr)
 
 ref <- list()
 
 # A function for generating captions and cross-references
 fig <- local({
   i <- 0
   list(
     cap=function(refName, text, center=TRUE, col="darkblue", label="Figure", width=60, inline=FALSE) {
       i <<- i + 1
       ref[[refName]] <<- i
       css_ctr <- ""
       if (center) css_ctr <- paste0("text-align:center; display:inline-block; width:",width,"%;")
       cap_txt <- paste0("<span style=\"color:", col, "; ", css_ctr, "\">",label," ", i, ": ", text , "</span>")
       anchor <- paste0("<a name=\"", refName, "\"></a>")
       if (inline) {
         paste0(anchor, cap_txt)    
       } else {
         list(anchor=anchor, cap_txt=cap_txt)
       }
     },
     
     ref=function(refName, link=FALSE, checkRef=TRUE) {
       
       ## This function puts in a cross reference to a caption. You refer to the
       ## caption with the refName that was passed to fig$cap() (not the code chunk name).
       ## The cross reference can be hyperlinked.
       
       if (checkRef && any(!refName %in% names(ref))) stop(paste0("fig$ref() error: ", refName, " not found"))
       if(length(refName) == 1) {
         refs <- paste("Figure", ref[[refName]])
         if(link)
           refs <- paste0("<A HREF=\"#", refName[1], "\">", refs, "</A>")
       }
       else {
         refs <- sapply(refName, function(nm) ref[[nm]])
         if(link)
           refs <- paste("Figures", 
                         paste(paste0("<A HREF=\"#", head(refName, -1), "\">", head(refs, -1), "</A>"), collapse = ", "), "&", 
                         paste0("<A HREF=\"#", tail(refName, 1), "\">", tail(refs, 1), "</A>") )
         else
           refs <- paste("Figures", paste(head(refs, -1), collapse = ", "), "&", tail(refs, 1))
       }
       refs
     },
     
     ref_all=function(){
       ## For debugging
       ref
     })
 })
 
 ## This chunk replaces the default hook for processing plots. It achieves the purposes,
 ## of laying out auto-numbered captions, but other functionality may be gone.
 knit_hooks$set(
   plot = function(x, options) {
     sty <- ""
     if (options$fig.align == 'default') {
       sty <- ""
     } else {
       sty <- paste0(" style=\"text-align:", options$fig.align, ";\"")
     }
     
     if (is.list(options$fig.cap)) {
       ## options$fig.cap is a list returned by the function fig$cap()
       str_caption <- options$fig.cap$cap_txt
       print(str_caption)
       str_anchr <- options$fig.cap$anchor
     } else {
       ## options$fig.cap is a character object (hard coded, no anchor)
       str_caption <- options$fig.cap
       str_anchr <- ""
     }
     
     paste('<figure', sty, '>', str_anchr, '<img src="',
           opts_knit$get('base.url'), paste(x, collapse = '.'),
           '"><figcaption>', str_caption, '</figcaption></figure>',
           sep = '')
   },
   chunk = function(x, options) {
     if(options$results!='asis')
       return(x)
     
     sty <- ""
     if (options$fig.align == 'default') {
       sty <- ""
     } else {
       sty <- paste0(" style=\"text-align:", options$fig.align, ";\"")
     }
     
     if (is.list(options$fig.cap)) {
       ## options$fig.cap is a list returned by the function fig$cap()
       str_caption <- options$fig.cap$cap_txt
       str_anchr <- options$fig.cap$anchor
     } else {
       ## options$fig.cap is a character object (hard coded, no anchor)
       str_caption <- options$fig.cap
       str_anchr <- ""
     }
     
     paste('<figure', sty, '>', str_anchr, x,
           '<figcaption>', str_caption, '</figcaption></figure>',
           sep = '')
   }
 )
 
 
 ## This chunk will read through *this* Rmd file, and attempt to extract all of the 
 ## labels (not caption text) used for Figure captions. These labels are used
 ## as anchors, so scanning through the document now will allow us to create cross references
 ## before the caption actually appears. 
 
 ## Get the name of this Rmd file
 rmdFn <- knitr::current_input()
 # rmdFn <- paste(filedir,knitr::current_input(),sep="/")  # filename of input document
 
 ## Read lines and close connection
 rmdCon <- file(rmdFn, open = "r")
 rmdLines <- readLines(rmdCon)
 close(rmdCon)
 
 warning(paste0("rmdFn = ",rmdLines[15]),immediate.=T)
 
 ## Pull out all occurences of at least one back tick, followed 
 ## by any number of characters, followed by fig$cap (all on one line)
 figscap_idx <- grep("`+(.*)fig\\$cap", rmdLines)
 rmdLines <- rmdLines[figscap_idx]
 
 ## Get rid of everything up until the start of the caption label
 ## This presumes the caption label is the first argument of fig$cap()
 ## E.g., fig.cap = fig$cap("my_label", ...)
 rmdLinesSansPre <- sub("(.*)fig\\$cap(.*?)[\"']", "", rmdLines)
 
 ## Identify everything up until the first quote
 match_data <- regexpr("(.*?)[\"']", rmdLinesSansPre)
 
 ## Reduce the length by one, because we're not interested in the final quote
 attr(match_data, "match.length") <- attr(match_data, "match.length") - 1
 
 ## Extract
 fig_labels <- regmatches(rmdLinesSansPre, match_data, invert=FALSE)
 
 if (length(fig_labels) > 0) {
   
   ## Test for duplicates
   if (anyDuplicated(fig_labels) > 0) stop("Duplicate caption labels detected")
   
   ## Create a named list of Figure numbers
   ref <- as.list(1:length(fig_labels))
   names(ref) <- fig_labels
 }    
 
 
 
 
 
# End script
 