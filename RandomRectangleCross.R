# Two rectangular arrays crossed
 library(data.table)
 library(ggplot2)
 
 w <- 150
 h <- 200
 dat <- data.table(expand.grid(X = 0:w, Y = 0:h), Z = 0)
 pal <- colorRampPalette(c("lightblue", "seagreen4", "violet", "lightgoldenrod", "orange", "white", "tan3"))(w+h+1)
 dat[, Color := pal[X + Y + 1]]
 
 border <- dat[X %in% c(0, w) | Y %in% c(0, h)]
 
 ggplot(dat, aes(X, Y)) + geom_point(col = dat$Color) + guides(col = F) + 
   scale_x_continuous(limits = c(0, max(w, h))) + scale_y_continuous(limits = c(0, max(w, h)))
 
 ggplot(border, aes(X, Y)) + geom_point(size = .5) + scale_x_continuous(limits = c(0, max(w, h))) + scale_y_continuous(limits = c(0, max(w, h)))
 
 
 # Orientation
 dat[, x0 := Z + 120][, y0 := 60 + X][, z0 := 200 - Y]
 
 plot_ly(dat, x = ~x0, y = ~y0, z = ~z0, color = ~X + Y + 1, colors = pal, size = 1, sizes = 2, mode = "none") %>% add_markers() %>%
   add_trace(border, x = ~X, y = ~Y, z = 0, color = 1, colors = "black", size = 1, sizes = 1, type = "scatter3d")
 
 # Begin transformation
 
 for (i in 1:150) {
   dat[, paste0(c("x", "y", "z"), i) := {
     x <- x0
     y <- y0
     z <- z0 - i
     while(sum(z < 0) > 0) {
       xt <- ifelse(z < 0, 120+z, x)
       yt <- ifelse(z < 0, 60+x, y)
       zt <- ifelse(z < 0, 200-y-i, z)
       x <- xt
       y <- yt
       z <- zt
     }
     list(xt, yt, zt)
   }]
 }
 
 plot_ly(dat, x = ~x2, y = ~y2, z = ~z2, color = ~X + Y + 1, colors = pal, size = 1, sizes = 2, mode = "none") %>% add_markers() %>%
   add_trace(border, x = ~X, y = ~Y, z = 0, color = 1, colors = "black", size = 1, sizes = 1, type = "scatter3d")
 
 plot_ly(dat, x = ~x50, y = ~y50, z = ~z50, color = ~X + Y + 1, colors = pal, size = 1, sizes = 2, mode = "none") %>% add_markers() %>%
   add_trace(border, x = ~X, y = ~Y, z = 0, color = 1, colors = "black", size = 1, sizes = 1, type = "scatter3d")
 
 plot_ly(dat, x = ~x70, y = ~y70, z = ~z70, color = ~X + Y + 1, colors = pal, size = 1, sizes = 2, mode = "none") %>% add_markers() %>%
   add_trace(border, x = ~X, y = ~Y, z = 0, color = 1, colors = "black", size = 1, sizes = 1, type = "scatter3d")
 
 plot_ly(dat, x = ~x75, y = ~y75, z = ~z75, color = ~X + Y + 1, colors = pal, size = 1, sizes = 2, mode = "none") %>% add_markers() %>%
   add_trace(border, x = ~X, y = ~Y, z = 0, color = 1, colors = "black", size = 1, sizes = 1, type = "scatter3d")
 
 plot_ly(dat, x = ~x85, y = ~y85, z = ~z85, color = ~X + Y + 1, colors = pal, size = 1, sizes = 2, mode = "none") %>% add_markers() %>%
   add_trace(border, x = ~X, y = ~Y, z = 0, color = 1, colors = "black", size = 1, sizes = 1, type = "scatter3d")
 
 plot_ly(dat, x = ~x100, y = ~y100, z = ~z100, color = ~X + Y + 1, colors = pal, size = 1, sizes = 2, mode = "none") %>% add_markers() %>%
   add_trace(border, x = ~X, y = ~Y, z = 0, color = 1, colors = "black", size = 1, sizes = 1, type = "scatter3d")
 
 plot_ly(dat, x = ~x150, y = ~y150, z = ~z150, color = ~X + Y + 1, colors = pal, size = 1, sizes = 2, mode = "none") %>% add_markers() %>%
   add_trace(border, x = ~X, y = ~Y, z = 0, color = 1, colors = "black", size = 1, sizes = 1, type = "scatter3d")
 
 
 
 
 
 ###
 
 # Try rug
 w <- 150
 h <- 200
 rug <- data.table(expand.grid(X = 0:w, Y = 0:h), Z = 0)
 pal <- colorRampPalette(c("lightblue", "seagreen4", "violet", "lightgoldenrod", "orange", "white", "tan3"))(w+h+1)
 rug[, Color := pal[X + Y + 1]]
 
 border <- rug[X %in% c(0, w) | Y %in% c(0, h)]
 
 rug[, Orientation := "001"]
 
 