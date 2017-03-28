# Using new fonts
 library(data.table)
 library(extrafont)
 library(ggplot2)
 
 # These should only need to be run once per R instance ... which is good cause it takes a while.
 if (readline(prompt="Fonts only need to be imported once. Would you like to import? (y/n): ")=="y") {
   font_import(prompt = F)                     # Imports all fonts the computer has
   font_import(paths=.libPaths(), prompt = F)  # Imports fonts from R packages
 2}
 
 
 df <- data.table(font=fonts(),x=seq_along(fonts()),y=seq_along(fonts()))
 ggplot(df,aes(x%/%50,y%%50,label=font,family=font)) + geom_text(size=10)
 
 
 
# End script
 