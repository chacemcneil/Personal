# Code to help create captions in html markdown files.
library(knitr)

# ref <- list()
# reftxt <- list()

# A function for generating captions and cross-references
fig <- local({
  list(
    cap = function(refName, text = NULL, center=TRUE, col="darkblue", type="Figure", width=60, inline=FALSE) {
      
      ## This function creates and formats the caption that accompanies the figure.
      
      css_ctr <- ifelse(center, paste0("text-align:center; display:inline-block; width:",width,"%;"), "")
      text <- ifelse(is.null(text), "", paste(":", text))
      
      cap_txt <- paste0("<span style=\"color:", col, "; ", css_ctr, "\">", type, " ", fig$nums[refName], text, "</span>")
      anchor <- paste0("<a name=\"", refName, "\"></a>")
      
      if (inline) {
        cap <- paste0(anchor, cap_txt)    
      } else {
        cap <- list(anchor = anchor, cap_txt = cap_txt)
      }
      cap
    },
    
    ref = function(refName, link=FALSE, checkRef=TRUE) {
      
      ## This function puts in a cross reference to a caption
      ## Refer to figure using the refName that was passed to fig$cap() (not the name of the code chunk)
      ## The cross reference can be a hyperlink to the figure by setting link = TRUE
      
      if (checkRef && any(!refName %in% fig$labels)) stop(paste0("fig$ref() error: ", refName, " not found"))
      
      if(length(refName) == 1) {
        refs <- paste(fig$types[refName], fig$nums[refName])
        if(link)
          refs <- paste0("<a href=\"#", refName[1], "\">", refs, "</a>")
      }
      else {
        unqreftxt <- unique(fig$types[refName])
        if(length(unqreftxt) > 1)
          stop("Different graphical elements must be referenced separately. (", paste(unqreftxt, collapse = ", "), ")")
        refs <- sapply(refName, function(nm) fig$nums[nm])
        if(link)
          refs <- paste(paste0(unqreftxt, "s"), 
                        paste(paste0("<a href=\"#", head(refName, -1), "\">", head(refs, -1), "</a>"), collapse = ", "), "&", 
                        paste0("<a href=\"#", tail(refName, 1), "\">", tail(refs, 1), "</a>") )
        else
          refs <- paste(paste0(unqreftxt, "s"), paste(head(refs, -1), collapse = ", "), "&", tail(refs, 1))
      }
      refs
    }
  )
})

## This chunk replaces the default hook for processing plots. It achieves the purposes,
## of laying out auto-numbered captions, but other functionality may be gone.
knit_hooks$set(
  # plot = function(x, options) {
  #   sty <- ""
  #   if (options$fig.align == 'default') {
  #     sty <- ""
  #   } else {
  #     sty <- paste0(" style=\"text-align:", options$fig.align, ";\"")
  #   }
  #   
  #   if (is.list(options$fig.cap)) {
  #     ## options$fig.cap is a list returned by the function fig$cap()
  #     str_caption <- options$fig.cap$cap_txt
  #     print(str_caption)
  #     str_anchr <- options$fig.cap$anchor
  #   } else {
  #     ## options$fig.cap is a character object (hard coded, no anchor)
  #     str_caption <- options$fig.cap
  #     str_anchr <- ""
  #   }
  #   
  #   paste('<figure', sty, '>', str_anchr, '<img src="',
  #         opts_knit$get('base.url'), paste(x, collapse = '.'),
  #         '"><figcaption>', str_caption, '</figcaption></figure>',
  #         sep = '')
  # },
  chunk = function(x, options) {
    if(options$results!='asis')
      return(x)
    
    sty <- ifelse (options$fig.align == 'default', "", paste0(" style=\"text-align:", options$fig.align, ";\""))
    
    if (is.list(options$fig.cap)) {
      ## options$fig.cap is a list returned by the function fig$cap()
      str_caption <- options$fig.cap$cap_txt
      str_anchr <- options$fig.cap$anchor
    } else {
      ## options$fig.cap is a character object (hard coded, no anchor)
      str_caption <- options$fig.cap
      str_anchr <- ""
    }
    
    paste('<figure', sty, '>', str_anchr, x, '<figcaption>', str_caption, '</figcaption></figure>', sep = '')
  }
)


## This chunk will read through *this* Rmd file, and attempt to extract all of the 
## labels (not caption text) used for Figure captions. These labels are used
## as anchors, so scanning through the document now will allow us to create cross references
## before the caption actually appears. 

## Get the name of this Rmd file, read file and find lines where fig$cap is called
rmdFn <- knitr::current_input()
rmdCon <- file(rmdFn, open = "r")
rmdLines <- readLines(rmdCon)
close(rmdCon)
figscap_idx <- grep("(.*)fig\\$cap", rmdLines)
rmdLines <- rmdLines[figscap_idx]

## Get rid of everything up until the start of the caption label
## This presumes the caption label is the first argument of fig$cap()
## E.g., fig.cap = fig$cap("my_label", ...)
fig$labels <- gsub("(.*fig\\$cap\\s?\\(\\s?[\"'])([[:alnum:]_\\.]*)([\"'].*$)", "\\2", rmdLines)
fig$types <- gsub("(.*fig\\$cap\\s?\\([^\\)]*,\\s?type\\s?=\\s?[\"'])([[:alnum:]_\\.]*)([\"'].*$)", "\\2", rmdLines)
fig$types <- ifelse(grepl("[^[:alnum:]]", fig$types), "Figure", fig$types)
tmp <- data.table(labels = fig$labels, types = fig$types)
tmp[, nums := 1:.N, by = types]
fig$nums <- tmp$nums
rm(tmp)

names(fig$types) <- fig$labels
names(fig$nums) <- fig$labels

assign("fig", fig, env = baseenv())


# End script