# Making waves
 library(animation)
 library(data.table)
 library(ggplot2)

 setwd("C:/Users/cmcneil/Documents/Projects/Miscellaneous/Personal/Geometry")
 
 
 incr <- .01
 dt <- data.table(expand.grid(x = seq(0, 1, incr), y = seq(0, 1, incr)))
 dt <- dt[, list(time = 1:200), by = list(x, y)]
 
 # Create sin wave.
 wv_sin <- function(dist, time, freq = 10, speed = .01, ampl = 1, damp = .995) {
   time <- time*speed
   ifelse(dist > time, 0, ampl*sin((time-dist)*freq)*damp^time)
 }
 # not used
 wv_sin_2 <- function(dist, time, freq = 5, speed = .01, ampl = 1, damp = .995, shift = 0) {
   ifelse(dist > time*speed, 0, ampl*damp^dist*sin(2*pi*(freq*(time - dist/speed) + shift)) )
 }
 
 dt[, Dist_0_0 := sqrt((x-0)^2 + (y-0)^2)]
 dt[, Dist_1_0 := sqrt((x-1)^2 + (y-0)^2)]
 dt[, Dist_1_1 := sqrt((x-1)^2 + (y-1)^2)]
 dt[, Dist_0_1 := sqrt((x-0)^2 + (y-1)^2)]
 
 # Different wave combinations
 dt[, Wave1 := wv_sin_2(Dist_0_0, time)]
 dt[, Wave2 := wv_sin_2(Dist_0_0, time, damp = .99, freq = 5) + wv_sin_2(Dist_0_1, time, damp = .99, freq = 5)]
 dt[, Wave3 := wv_sin(Dist_0_0, time, freq = 17) + wv_sin(Dist_1_1, time)]
 dt[, Wave4 := wv_sin(Dist_0_0, time, freq = 20) + wv_sin(Dist_0_0, time)]
 dt[, Wave5 := wv_sin(Dist_0_0, time, freq = 15) + wv_sin(Dist_1_1, time, freq = 15, ampl = .6)]
 dt[, Wave6 := wv_sin(Dist_0_0, time) + wv_sin(Dist_1_1, time) + wv_sin(Dist_0_1, time) + wv_sin(Dist_1_0, time)]
 dt[, Wave7 := wv_sin(Dist_55, time) + wv_sin(Dist_1_1, time) + wv_sin(Dist_1_0, time)]
 
 # Test snapshots
 ggplot(dt[time == .05], aes(x, y, fill = Wave2)) + geom_tile() + scale_fill_gradient2(limits = dt[, range(Wave1)])
 ggplot(dt[time == .3], aes(x, y, fill = Wave1)) + geom_tile() + scale_fill_gradient2(limits = dt[, range(Wave1)])
 ggplot(dt[time == 1], aes(x, y, fill = Wave2)) + geom_tile() + scale_fill_gradient2(limits = dt[, range(Wave1)])
 
 # Create animations
 waves <- grep("^Wave", colnames(dt), value = T)
 saveHTML(
   for(wv in gsub("Wave", "", waves)) {
     for (i in unique(dt$time)) {
       
       cat(paste0("\rWave: ", wv, "   Iteration: ", i, "    "))
       png(paste0("Images/wave", wv, "_", i, ".png"), width = 900, height = 700)
       print(ggplot(dt[time == i], aes(x, y, fill = get(paste0("Wave", wv)))) + geom_tile() + 
               labs(title = paste("Time:", i), fill = "Wave\nHeight") + 
               scale_fill_gradient2(limits = dt[, range(get(paste0("Wave", wv)))]))
       dev.off()
     } 
   },
   img.name = paste0("wave", wv, "_"), htmlfile = paste0("wave", wv, ".html"), opts = ani.options(interval = .02), verbose = F )
 
 for (wv in gsub("Wave", "", waves)) {
   file.remove(paste0("wave", wv, ".html"))
   saveHTML({}, img.name = paste0("wave", wv, "_"), htmlfile = paste0("wave", wv, ".html"), opts = ani.options(interval= 0.02, verbose = F, use.dev = F))
 }
 
 # In command line run (not working):
 # R CMD BATCH c:/Users/cmcneil/documents/projects/miscellaneous/personal/geometry/plotwaves2.r
 
 n <- 40
 xpos <- cos((1:n)/n*2*pi)/2 + .5
 ypos <- sin((1:n)/n*2*pi)/2 + .5
 # plot(xpos, ypos)
 
 for (i in 1:n) {
   cat(paste0("\rIteration: ", i))
   dt[, paste0("Dist_", i) := sqrt((x-xpos[i])^2 + (y-ypos[i])^2)]
   dt[, paste0("Sin_", i) := wv_sin_2(get(paste0("Dist_", i)), time, shift = i/n/4)]
 }
 dt[, paste0("Dist_", 1:n) := NULL]
 dt[, paste0("Wave", n) := apply(.SD, 1, sum), .SDcols = paste0("Sin_", 1:n)]
 dt[, paste0("Sin_", 1:n) := NULL]
 
 saveHTML(
   for(wv in gsub("Wave", "", n)) {
     for (i in unique(dt$time)) {
       
       cat(paste0("\rWave: ", wv, "   Iteration: ", i, "    "))
       png(paste0("Images/wave", wv, "_", i, ".png"), width = 900, height = 700)
       print(ggplot(dt[time == i], aes(x, y, fill = get(paste0("Wave", wv)))) + geom_tile() + 
               labs(title = paste("Time:", i), fill = "Wave\nHeight") + 
               scale_fill_gradient2(limits = dt[, range(get(paste0("Wave", wv)))]))
       dev.off()
     } 
   },
   img.name = paste0("wave", n, "_"), htmlfile = paste0("wave", wv, ".html"), opts = ani.options(interval = .02), verbose = F )
 
 
 
 
# End script
 