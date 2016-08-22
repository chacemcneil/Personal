# Exploring NPS
 library(data.table)
 library(extrafont)
 library(ggplot2)
 library(grid)
 library(gridExtra)
 
 setwd("r:/Users/cmcneil/Projects/Miscellaneous/BrownBag/Net Promoter Score")
 load("Data.rda", verbose=T)
 
 source("r:/users/cmcneil/projects/miscellaneous/Personal/General Code/UsefulFunctions.R")
 source("r:/users/cmcneil/projects/miscellaneous/Personal/General Code/MarkdownFunctions.R")
 
 # Functions
 npsind <- function(x, as.factor=F) {
   x <- x[!is.na(x)]
   x <- ifelse(x > 6, ifelse(x > 8, 1, 0), -1)
   if(as.factor)
     x <- factor(x, levels=-1:1)
   x
 }
 npsprop <- function(x) {
   prop.table(table(npsind(x, as.factor=T)))[c("-1", "0", "1")]
 }
 nps <- function(x) {
   npsprop(x)[["1"]] - npsprop(x)[["-1"]]
 }
 moe <- function(x) {
   qnorm(.975) * sd(npsind(x)) / sqrt(length(x))
 }
 
 thm <- theme(panel.background=element_rect(fill="azure1"),   text=element_text(size=40, family="Narkisim"), 
              legend.text=element_text(size=20), legend.title=element_text(size=20), 
              panel.grid.major=element_line(color="gray85"), panel.grid.minor=element_line(color="gray93"))
 back <- ggplot() + geom_hline(aes(yint=0)) + geom_vline(aes(xint=0)) + 
   scale_y_continuous(labels = scales::percent) + scale_x_continuous(labels = scales::percent) + thm
 
 
 
 set.seed(90019)
 mu <- 6
 sc1 <- rbinom(1e3, 10, mu/10)
 table(sc1)
 mu <- 6.5
 sc2 <- rbinom(1e3, 10, mu/10)
 table(sc2)
 
 nps(sc1)
 nps(sc2)
 t.test(sc1, sc2)
 t.test(npsind(sc1), npsind(sc2))
 sd(sc1) / sqrt(1e3)
 
 # Manual calculation of std. err. and t stats
 var1 <- var(sc1)
 var2 <- var(sc2)
 t1 <- (mean(sc2) - mean(sc1))/sqrt(var1/length(sc1) + var2/length(sc2))
 varind1 <- var(npsind(sc1))
 varind2 <- var(npsind(sc2))
 t2 <- (mean(npsind(sc2)) - mean(npsind(sc1)))/sqrt(varind1/length(sc1) + varind2/length(sc2))
 
 # Manual calculation of p-values
 2*(1-pt(abs(t1), length(c(sc1, sc2))-2))
 2*(1-pt(abs(t2), length(c(sc1, sc2))-2))
 
 
 seq1 <- seq(4, 9, .1)
 seq2 <- seq(4, 9, .1)
 
 # 2D grid of group means, compare resulting t statistics
 origt <- matrix(0, length(seq1), length(seq2))
 newt  <- matrix(0, length(seq1), length(seq2))
 for (i in seq_along(seq1)) {
   for (j in seq_along(seq2)) {
     one <- rbinom(1e3, 10, seq1[i]/10)
     two <- rbinom(1e3, 10, seq2[j]/10)
     
     origt[i, j] <- t.test(one, two)$statistic
     newt[i, j]  <- t.test(npsind(one), npsind(two))$statistic
     
     cat(paste0("\rIteration: ", i, ", ", j))
   }
 }
 
 
 (1-pbinom(8, 10, .6)) - pbinom(6, 10, .6)
 
 # Compare NPS calculation based on indicators (standard calculation) to one based on estimated binomial cdf
 # (fast)
 NPS1 <- NULL
 NPS2 <- NULL
 N  <- 1e3
 n  <- 1e2
 mu <- 7
 for (i in 1:N) {
   #sc <- rbinom(n, 10, mu/10)
   sc <- pmin(round(rnorm(n, mu, 1.5)),10)
   NPS1[i] <- nps(sc)
   NPS2[i] <- (1-pbinom(8, 10, mean(sc)/10)) - pbinom(6, 10, mean(sc)/10)
   cat(paste0("\rIteration: ", i))
 }
 
 # These seem very similar -- Surprising!
 # The original scale (binomial estimator) has slightly smaller variance
 hist(NPS1); mean(NPS1); var(NPS1)
 hist(NPS2); mean(NPS2); var(NPS2)
 
 tab <- data.table(NPS=c(NPS1, NPS2), Method=rep(c("Standard", "Binomial CDF"), N))
 ggplot(tab, aes(NPS, fill=Method)) + geom_histogram(position="dodge", binwidth=.04)
 
 # How does the ratio impact the usefulness of NPS
 C   <- 100     # companies
 ps  <- seq(4, 10, length.out=C)
 p   <- (1-pbinom(8, 10, ps/10))
 d   <- pbinom(6, 10, ps/10)
 # Given customer base of 1000 people, p and d are the promoters and detractors percentages, respectively, 
 #  and each each influences an average of a and b people, respectively,
 #  what is the net increase to the customer base?
 a <- 1
 b <- 1
 dat <- data.table(NPS = p - d, Increase = p*a - d*b)
 back + geom_point(data = dat, aes(NPS, Increase))
 
 a <- .1
 b <- 1
 dat <- data.table(NPS = p - d, Increase = p*a - d*b)
 back + geom_point(data = dat, aes(NPS, Increase))
 # Nonlinear, but still a good relative statistic
 
 
 # What if the influencer ranges are off?
 top <- (1-pbinom(8, 10, ps/10))
 bot <- pbinom(6, 10, ps/10)
 p   <- (1-pbinom(9, 10, ps/10))  # Assuming promotors are still related to score but not necessarily 9 and 10
 d   <- pbinom(4, 10, ps/10)      # Assuming detractors are still related to score but not necessarily 0 through 6
 NPS <- top - bot
 a   <- 1
 b   <- 1
 dat <- data.table(NPS = top - bot, Increase = p*a - d*b)
 back + geom_point(data = dat, aes(NPS, Increase))
 # Becomes nonlinear (cubic-ish), but still monotonic
 
 # What if distribution is not binomial?
 stddev <- 2
 # Truncated normal
 p   <- (pnorm(10, ps, stddev) - pnorm(8.5, ps, stddev))/(pnorm(10, ps, stddev) - pnorm(0, ps, stddev))
 d   <- (pnorm(6.5, ps, .5) - pnorm(0, ps, stddev))/(pnorm(10, ps, stddev) - pnorm(0, ps, stddev))
 # Normal constrained at 0 and 10
 p   <- 1 - pnorm(8.5, ps, stddev)
 d   <- pnorm(6.5, ps, .5)
 a   <- .1
 b   <- .01
 dat <- data.table(NPS = p - d, Increase = p*a - d*b)
 back + geom_point(data = dat, aes(NPS, Increase))
 
 # What if the reported score is more of a probability?
 a   <- ps/10*1
 b   <- ps/10*.5
 dat <- data.table(NPS = p - d, Increase = p*a - d*b)
 back + geom_point(data = dat, aes(NPS, Increase))
 
 
 
 # Bar plots
 png("Barplots.png", width=1000, height=800)
 dat <- data.table(x=factor(sc), ind=factor(npsind(sc), labels=c("Detractors", "Passive", "Promoters")))
 grid.arrange(
   ggplot(dat, aes(x, fill=ind)) + guides(fill=F) + theme(text=element_text(size=20)) +
     geom_bar(col="white") + scale_fill_manual(values=c("indianred", "black", "slateblue")) + labs(fill="", x="", y="Count") + thm
   ,
   ggplot(dat, aes(ind, fill=ind)) + geom_bar(col="white") + scale_fill_manual(values=c("indianred", "black", "slateblue")) + 
     guides(fill=F) + labs(fill="", x="", y="Count") + theme(text=element_text(size=20)) + thm
 , nrow=1)
 dev.off()
 
 
 ## Power curves
 # Compare curves when using -1,0,1 values to those when using t.test assuming normality
 
 # (about 15 minutes run-time)
 N <- 1e3
 Ns <- (5:1)*N
 ns <- c(50, 100, 250, 500, 1000)
 p <- 6
 ds <- seq(0, 1, .05)
 dt <- data.table(Size=0, Difference=0, Power=0, Method="", Iterations=0)[0]
 for (k in seq_along(ns)[1]) {
   for (i in seq_along(ds)) {
     p1 <- p2 <- numeric()
     for (j in 1:Ns[k]) {
       sc1 <- rbinom(ns[k], 10, p/10)
       sc2 <- rbinom(ns[k], 10, (p+ds[i])/10)
       p1[j] <- t.test(sc1, sc2)$p.value
       p2[j] <- t.test(npsind(sc1), npsind(sc2))$p.value
       cat(paste0("\b\b\b\b\rDifference: ", ds[i], "    Iteration: ",  j,  "    Sample size: ", ns[k]))
     }
     dt <- rbind(dt, data.table(Size=ns[k], 
                               Difference=ds[i], 
                               Power=c(mean(p1 < 0.05), mean(p2 < 0.05)), 
                               Method=c("Original", "Indicators"), 
                               Iterations=Ns[k]))
   }
 }
 setkeyv(dt, c("Size", "Difference", "Method"))
 ggplot(dt, aes(Difference, Power, group=paste(Method, Size), color=Method, size=factor(Size), linetype=factor(Size))) + geom_line() + 
   scale_size_manual(values=c(.3, .9, 1.4, 2, 2.5)) + labs(size="Sample\nSize", linetype="Sample\nSize", color="Scale") +
   scale_x_continuous(breaks=(0:5)/5) + scale_y_continuous(breaks=(0:5)/5) + geom_vline(aes(xintercept=.5), linetype=2)
 
 dt2 <- dcast(data=dt, formula = Size+Difference+Iterations~Method, value.var="Power")
 dt2[Difference > 0.1, hist(Original-Indicators)]
 dt2[, Lift := Original - Indicators]
 
 dt2
 ggplot(dt2[Size==50 ], aes(Difference, Lift)) + geom_line()
 ggplot(dt2[Size==100], aes(Difference, Lift)) + geom_line()
 ggplot(dt2[Size==500], aes(Difference, Lift)) + geom_line()
 ggplot(dt2[Size==1e3], aes(Difference, Lift)) + geom_line()
 
 
 png("PowerCurves.png", width=1000, height=800)
 ggplot(dt, aes(Difference, Power, group=paste(Method, Size), color=Method, size=factor(Size), linetype=factor(Size))) + geom_line() + 
   scale_size_manual(values=c(.3, .9, 1.4, 2, 2.5), labels= function(x) scales::comma(as.numeric(x)) ) + 
   scale_linetype_manual(values=1:5, labels= function(x) scales::comma(as.numeric(x)) ) + 
   labs(size="Sample\nSize", linetype="Sample\nSize", color="Scale", x="Difference in Means") +
   scale_x_continuous(breaks=(0:5)/5) + scale_y_continuous(breaks=(0:5)/5, labels=scales::percent) + 
   geom_hline(aes(yintercept=.8), linetype=2) +
   scale_color_manual(values=c(ssblue, ssyellow)) + theme(text=element_text(size=20)) + thm + 
   guides(color=guide_legend(keywidth=5, override.aes=list(size=4), 
                             label.position="top", label.hjust=.5, title.position="top"),
          size=guide_legend(keywidth=3))
 dev.off()
 
 save(dt, dt2, file="Data.rda")
 
 
 ## Effects of assumptions:
 
 
 
 
 
# End script
 