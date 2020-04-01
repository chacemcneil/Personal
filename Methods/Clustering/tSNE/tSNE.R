# t-SNE exploration
 library(animation) 
 library(ggplot2)
 library(tsne)
 
 setwd("C:/Users/cmcneil/Documents/Projects/Miscellaneous/Personal/tSNE")
 
 source('~/Projects/Miscellaneous/Personal/General Code/UsefulFunctions.R')
 
 # Iris data example
 colors = rainbow(length(unique(iris$Species)))
 names(colors) = unique(iris$Species)
 ecb = function(x,y){ plot(x,t='n'); text(x,labels=iris$Species, col=colors[iris$Species]) }
 tsne_iris = tsne(iris[,1:4], epoch_callback = ecb, perplexity=50)
 
 ## Plots
 
 animate <- function(pts, filename, color = "black", pch = 19, loop = F, navigator = F, interval = .5, epoch = 50, max_iter = 1e3, perplexity = 30, ...) {
   oldfiles <- grep(paste0("^", filename), dir("Images"), value = T)
   file.remove(paste0(filename, ".html"))
   sapply(paste0("Images/", oldfiles), file.remove)
   if(!all(is.color(color)))
     color <- rainbow(length(unique(color)))[as.numeric(factor(color))]
   if(!all(pch %in% 1:25))
     pch <- ((as.numeric(factor(pch)) - 1) %% 25) + 1
   print(length(pch)); print(length(color))
   saveHTML({tsne(pts, 
                  epoch = epoch, 
                  epoch_callback = function(x) {
                    #tmp <- data.frame(tmp)
                    print(ggplot(data.frame(x), aes(X1, X2)) + geom_point(col = color, pch = pch))
                  }, 
                  max_iter = max_iter, 
                  perplexity = perplexity )}, 
            img.name = filename, 
            htmlfile = paste0(filename, ".html"),
            navigator = navigator,
            loop = loop, 
            interval = interval, 
            global.options = ani.options(verbose = F, nmax = 50), 
            ... )
 }
 
 divec <- rep(1:2, each = 50)
 vec <- rep(1:4, each = 25)
 animate(cbind(rnorm(100,,.1) + divec, vec), vec, "Sample")
 animate(cbind(rnorm(100,,.1) + divec, rnorm(100) + vec), vec, "Sample2")
 animate(cbind(rnorm(100,,.1) + divec, divec), vec, "TwoGroups")
 animate(cbind(rnorm(100), rnorm(100)), vec, "Random")
 animate(cbind((1:100), (1:100)), vec, "String")
 
 
 
 animate(iris[, 1:4], colors[iris$Species], "Iris")
 animate(iris[, 1:4], colors[iris$Species], "Iris2")
 animate(iris[, 1:4], colors[iris$Species], "Iris3", perplexity = 50)
 animate(iris[, 1:4], colors[iris$Species], "Iris4", perplexity = 500)
 animate(iris[, 1:4], colors[iris$Species], "Iris5", perplexity = 5)
 
 
 
# End script
 