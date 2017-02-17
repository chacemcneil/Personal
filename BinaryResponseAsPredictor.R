# How to detect multivariate separation of categories
 library(data.table)
 library(ggplot2)
 library(grid)
 library(gridExtra)
 
 N <- 1e3
 train <- sample(1:N, .9*N)
 tab <- data.table(x1 = rnorm(N, 0, 5), x2 = rnorm(N, 0, 1), x3 = rnorm(N, 0, 5), grp = sample(factor(0:1), N, replace = T))
 tab[, x4 := ifelse(grp == 1, 10 - (x1 + x2 + x3) + rnorm(.N, 0, .1), 10 - (x1 + 2*x2 + 1.3*x3) + rnorm(.N, 0, .1))]
 
 plot(pc <- prcomp(tab[, list(x1, x2, x3, x4)]))
 pcs <- data.table(grp = tab$grp, pc$x)
 
 grid.arrange(
   ggplot(tab, aes(x1, fill = grp)) + geom_density(alpha = .5),
   ggplot(tab, aes(x2, fill = grp)) + geom_density(alpha = .5),
   ggplot(tab, aes(x3, fill = grp)) + geom_density(alpha = .5),
   ggplot(tab, aes(x4, fill = grp)) + geom_density(alpha = .5),
   ggplot(tab, aes(x1 + 2*x2 + 1.3*x3 + x4, fill = grp)) + geom_density(alpha = .5)
 )
 
 grid.arrange(
   ggplot(data.frame(pc$x), aes(PC1, PC2, col = tab$grp)) + geom_point(),
   ggplot(data.frame(pc$x), aes(PC1, PC3, col = tab$grp)) + geom_point(),
   ggplot(data.frame(pc$x), aes(PC1, PC4, col = tab$grp)) + geom_point(),
   ggplot(data.frame(pc$x), aes(PC2, PC3, col = tab$grp)) + geom_point(),
   ggplot(data.frame(pc$x), aes(PC2, PC4, col = tab$grp)) + geom_point(),
   ggplot(data.frame(pc$x), aes(PC3, PC4, col = tab$grp)) + geom_point() )
 ggplot(data.frame(pc$x), aes(PC1 + PC2 + PC3, PC4, col = tab$grp)) + geom_point()
 ggplot(tab, aes(x1 + x2 + x3, fill = grp)) + geom_density(alpha = .5)
 
 
 summary(mod <- glm(grp~., data = tab[train], family = "binomial"))
 roc(tab[-train, grp], predict(mod, newdata = tab[-train]), plot = T, print.auc = T)
 summary(mod <- glm(grp~I(x1 + 2*x2 + 1.3*x3 + x4), data = tab[train], family = "binomial"))
 roc(tab[-train, grp], predict(mod, newdata = tab[-train]), plot = T, print.auc = T)
 summary(mod <- glm(grp~., data = pcs[train], family = "binomial"))
 roc(pcs[-train, grp], predict(mod, newdata = pcs[-train]), plot = T, print.auc = T)
 
 
 #####
 # Only 3 dimensions
 N <- 1e3
 train <- sample(1:N, .9*N)
 tab3 <- data.table(x1 = rnorm(N, 0, 5), x2 = rnorm(N, 0, 1), grp = sample(factor(0:1), N, replace = T))
 tab3[, x3 := ifelse(grp == 1, 7.5 - (x1 + x2), 10/1.3 - (x1 + 2*x2)/1.3)]
 
 plot(pc3 <- prcomp(tab3[, list(x1, x2, x3)]))
 pcs3 <- data.table(grp = tab3$grp, pc3$x)
 
 grid.arrange(
   ggplot(tab3, aes(x1, fill = grp)) + geom_density(alpha = .5),
   ggplot(tab3, aes(x2, fill = grp)) + geom_density(alpha = .5),
   ggplot(tab3, aes(x3, fill = grp)) + geom_density(alpha = .5),
   ggplot(tab3, aes(x1 + 2*x2 + 1.3*x3, fill = grp)) + geom_density(alpha = .5)
 )
 
 grid.arrange(
   ggplot(data.frame(pc3$x), aes(PC1, PC2, col = tab3$grp)) + geom_point(),
   ggplot(data.frame(pc3$x), aes(PC1, PC3, col = tab3$grp)) + geom_point(),
   ggplot(data.frame(pc3$x), aes(PC2, PC3, col = tab3$grp)) + geom_point() )
 
 ggplot(tab3, aes(x1 + x2 + x3, 1.3*x1 - 1.7*x2 + .4*x3, col = tab3$grp)) + geom_point()
 
 summary(mod <- glm(grp~.^3, data = tab3[train], family = "binomial"))
 roc(tab3[-train, grp], predict(mod, newdata = tab3[-train]), plot = T, print.auc = T)
 summary(mod <- glm(grp~abs(1.3*x1 - 1.7*x2 + 0.4*x3 - 10) * abs(x1 + x2 + x3 - 7.5), data = tab3[train], family = "binomial"))
 roc(tab3[-train, grp], predict(mod, newdata = tab3[-train]), plot = T, print.auc = T)
 summary(mod <- glm(grp~., data = pcs3[train], family = "binomial"))
 roc(pcs3[-train, grp], predict(mod, newdata = pcs3[-train]), plot = T, print.auc = T)
 
 
 summary(mod2 <- lm(x3~.*grp, data = tab3))
 
 
 
 
# End script
 