# Code for searching through other code files. Uses regular expressions for filtering files and for searching through lines of code files.
 
 dir = getwd()
 
 code_search <- function(pattern, filepattern = "\\.R$", removeEmpty = T, replace = NULL, directory = getwd(), value = F, ...) {
   files <- dir(directory, ...)
   files <- files[grep(filepattern, files)]
   files <- paste(directory, files, sep = "/")
   
   fls <- lapply(files, readLines, warn = F)
   results <- lapply(fls, grep, pattern = pattern, value = value)
   names(results) <- files
   if (removeEmpty)
     results <- results[sapply(results, length) > 0]
   
   if (!is.null(replace)) {
     files <- names(results)
     cat(paste(files, collapse = "\n"))
     if (readline(prompt = paste0("This will replace '", pattern, "' with '", replace, "'. Are you sure you want to modify the above files (y/n)? ")) == "y") {
       fls <- lapply(files, readLines, warn = F)
       fls <- lapply(fls, gsub, pattern = pattern, replacement = replace)
       for (i in 1:length(files))
         write(fls[[i]], file = files[i])
     }
   }
   results
 }
 
 
 #library(abind)
 results <- code_search(pattern = "(require|library)\\([[:alnum:]]+\\)", filepattern = "\\.R", directory = dir, recursive = T, value = T)
 pckgs <- as.vector(unique(gsub("^.*(library|require)\\(([[:alnum:]]+)\\).*$","\\2", do.call(c, results))))
 #code_search(pattern = "library\\(reshape\\)", filepattern = "\\.R", directory = "..", recursive = T)
 
 syspckgs <- data.table(installed.packages())$Package
 
 missing_pckgs <- setdiff(pckgs, syspckgs)
 missing_pckgs
 
 
 results <- code_search(pattern = "DEV10", filepattern = "\\.R", directory = getwd(), recursive = T, value = T)
 
 
# End script
 