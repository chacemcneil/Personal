# Using new fonts
 library(data.table)
 library(extrafont)
 library(ggplot2)
 
 font_import()                   # Imports all fonts the computer has
 font_import(paths=.libPaths())  # Imports fonts from R packages
 
 
 
 df <- data.table(font=fonts(),x=seq_along(fonts()),y=seq_along(fonts()))
 ggplot(df,aes(x%/%50,y%%50,label=font,family=font)) + geom_text(size=10)
 
 
 
# End script
 