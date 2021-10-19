### Basic display of individual risk using normal distribution with colored regions
 library(data.table)
 library(ggplot2)
 
 ## Inputs
 med_risk <- .8
 high_risk <- .95
 quant <- .4
 
 ## Normal distribution
 dt <- data.table(xs = seq(-4, 4, .01))
 dt[, ds := dnorm(xs)]
 dt[, ps := pnorm(xs)]
 
 ## Graph
 ggplot(dt, aes(xs, ds)) +
   geom_density(data = dt, stat = "identity", size = 2) +
   geom_density(data = dt[ps <= med_risk], stat = "identity", fill = "green", col = "green", size = 0) +
   geom_density(data = dt[ps >= med_risk & ps <= high_risk], stat = "identity", fill = "yellow", col = "yellow", size = 0) +
   geom_density(data = dt[ps >= high_risk], stat = "identity", fill = "red", col = "red", size = 0) +
   theme(panel.background = element_rect(fill = "white"), axis.text = element_blank(), axis.ticks = element_blank()) +
   labs(x = "", y = "") +
   geom_vline(aes(xintercept = 0), size = 1, linetype = 2) +
   geom_vline(aes(xintercept = qnorm(quant)), col = "slateblue", size = 2) +
   geom_rect(xmin = -4, xmax = 4, ymin = -.2, max = -.005, fill = "white") +
   geom_text(aes(label = "Higher Risk", x = 2.5, y = -.025), size = 10) +
   geom_text(aes(label = "Lower Risk", x = -2.5, y = -.025), size = 10) +
   annotate("segment", x = 1, xend = 4, y = -.05, yend = -.05, size = 2, arrow = arrow(type = "closed"), col = "orange", fill = "black") +
   annotate("segment", x = -1, xend = -4, y = -.05, yend = -.05, size = 2, arrow = arrow(type = "closed"), col = "seagreen", fill = "black")
 
 
# End script
 