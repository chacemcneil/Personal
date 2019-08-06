## Graphic example for Troy
 library(CMcCode)
 library(data.table)
 library(ggplot2)
 library(grid)
 library(gridExtra)
 
 stages <- c("All", "Contacted", "Enrolled", "Engaged", "Walked", "Completed")
 
 n <- 1e3
 dt <- data.table(Id = 1:n)
 dt[, All := 1]
 dt[, Contacted := rbinom(.N, 1, .9)]
 dt[Contacted == 1, Enrolled := rbinom(.N, 1, .7)]
 dt[Enrolled == 1, Engaged := rbinom(.N, 1, .8)]
 dt[Engaged == 1, Walked := rbinom(.N, 1, .9)]
 dt[Walked == 1, Completed := rbinom(.N, 1, .9)]
 
 # dt[, Contacted := replace.na(Contacted)]
 # dt[, Enrolled := replace.na(Enrolled)]
 # dt[, Engaged := replace.na(Engaged)]
 # dt[, Walked := replace.na(Walked)]
 # dt[, Completed := replace.na(Completed)]
 
 
 dt[, RAF := exp(rnorm(.N, -0.8 + .2*replace.na(Enrolled) + .25*replace.na(Engaged) + .1*replace.na(Walked) + .1*replace.na(Completed)))]
 dt[, PMPM := exp(rnorm(.N, 5.5 - .2*replace.na(Enrolled) - .2*replace.na(Engaged) - .1*replace.na(Walked) - .1*replace.na(Completed)))]
 dt[, Cohort := factor(replace.na(Contacted) + replace.na(Enrolled) + replace.na(Engaged) + replace.na(Walked) + replace.na(Completed), levels = 0:5, labels = stages)]
 
 dt2 <- melt(dt, id.vars = c("Id", "PMPM", "RAF", "Cohort"), variable.name = "Stage", value.name = "In")
 dt2[, Stage := factor(Stage, levels = stages)]
 # dt2 <- dt2[In == 1]
 
 ggplot(dt2[!is.na(In), list(MeanPMPM = mean(PMPM, na.rm = T), MeanRAF = mean(RAF, na.rm = T)), keyby = list(Cohort, Stage, In)], aes(as.numeric(Stage), MeanPMPM, col = Cohort)) + 
   geom_point() + geom_line() + scale_x_continuous(breaks = 0:5, labels = stages)
 
 
 dt3 <- rbind(dt[All == 1, list(MeanRAF = mean(RAF), MeanPMPM = mean(PMPM))][, Stage := "All"][, Cohort := "All"],
              dt[All == 1, list(MeanRAF = mean(RAF), MeanPMPM = mean(PMPM))][, Stage := "All"][, Cohort := "Contacted"],
              dt[All == 1, list(MeanRAF = mean(RAF), MeanPMPM = mean(PMPM))][, Stage := "All"][, Cohort := "Enrolled"],
              dt[All == 1, list(MeanRAF = mean(RAF), MeanPMPM = mean(PMPM))][, Stage := "All"][, Cohort := "Engaged"],
              dt[All == 1, list(MeanRAF = mean(RAF), MeanPMPM = mean(PMPM))][, Stage := "All"][, Cohort := "Walked"],
              dt[All == 1, list(MeanRAF = mean(RAF), MeanPMPM = mean(PMPM))][, Stage := "All"][, Cohort := "Completed"],
              dt[Contacted == 0, list(MeanRAF = mean(RAF), MeanPMPM = mean(PMPM))][, Stage := "Contacted"][, Cohort := "All"],
              dt[Contacted == 1, list(MeanRAF = mean(RAF), MeanPMPM = mean(PMPM))][, Stage := "Contacted"][, Cohort := "Contacted"],
              dt[Contacted == 1, list(MeanRAF = mean(RAF), MeanPMPM = mean(PMPM))][, Stage := "Contacted"][, Cohort := "Enrolled"],
              dt[Contacted == 1, list(MeanRAF = mean(RAF), MeanPMPM = mean(PMPM))][, Stage := "Contacted"][, Cohort := "Engaged"],
              dt[Contacted == 1, list(MeanRAF = mean(RAF), MeanPMPM = mean(PMPM))][, Stage := "Contacted"][, Cohort := "Walked"],
              dt[Contacted == 1, list(MeanRAF = mean(RAF), MeanPMPM = mean(PMPM))][, Stage := "Contacted"][, Cohort := "Completed"],
              # dt[Enrolled == 0, list(MeanRAF = mean(RAF), MeanPMPM = mean(PMPM))][, Stage := "Enrolled"][, Cohort := "All"],
              dt[Enrolled == 0, list(MeanRAF = mean(RAF), MeanPMPM = mean(PMPM))][, Stage := "Enrolled"][, Cohort := "Contacted"],
              dt[Enrolled == 1, list(MeanRAF = mean(RAF), MeanPMPM = mean(PMPM))][, Stage := "Enrolled"][, Cohort := "Enrolled"],
              dt[Enrolled == 1, list(MeanRAF = mean(RAF), MeanPMPM = mean(PMPM))][, Stage := "Enrolled"][, Cohort := "Engaged"],
              dt[Enrolled == 1, list(MeanRAF = mean(RAF), MeanPMPM = mean(PMPM))][, Stage := "Enrolled"][, Cohort := "Walked"],
              dt[Enrolled == 1, list(MeanRAF = mean(RAF), MeanPMPM = mean(PMPM))][, Stage := "Enrolled"][, Cohort := "Completed"],
              # dt[Engaged == 0, list(MeanRAF = mean(RAF), MeanPMPM = mean(PMPM))][, Stage := "Engaged"][, Cohort := "All"],
              # dt[Engaged == 0, list(MeanRAF = mean(RAF), MeanPMPM = mean(PMPM))][, Stage := "Engaged"][, Cohort := "Contacted"],
              dt[Engaged == 0, list(MeanRAF = mean(RAF), MeanPMPM = mean(PMPM))][, Stage := "Engaged"][, Cohort := "Enrolled"],
              dt[Engaged == 1, list(MeanRAF = mean(RAF), MeanPMPM = mean(PMPM))][, Stage := "Engaged"][, Cohort := "Engaged"],
              dt[Engaged == 1, list(MeanRAF = mean(RAF), MeanPMPM = mean(PMPM))][, Stage := "Engaged"][, Cohort := "Walked"],
              dt[Engaged == 1, list(MeanRAF = mean(RAF), MeanPMPM = mean(PMPM))][, Stage := "Engaged"][, Cohort := "Completed"],
              # dt[Walked == 0, list(MeanRAF = mean(RAF), MeanPMPM = mean(PMPM))][, Stage := "Walked"][, Cohort := "All"],
              # dt[Walked == 0, list(MeanRAF = mean(RAF), MeanPMPM = mean(PMPM))][, Stage := "Walked"][, Cohort := "Contacted"],
              # dt[Walked == 0, list(MeanRAF = mean(RAF), MeanPMPM = mean(PMPM))][, Stage := "Walked"][, Cohort := "Enrolled"],
              dt[Walked == 0, list(MeanRAF = mean(RAF), MeanPMPM = mean(PMPM))][, Stage := "Walked"][, Cohort := "Engaged"],
              dt[Walked == 1, list(MeanRAF = mean(RAF), MeanPMPM = mean(PMPM))][, Stage := "Walked"][, Cohort := "Walked"],
              dt[Walked == 1, list(MeanRAF = mean(RAF), MeanPMPM = mean(PMPM))][, Stage := "Walked"][, Cohort := "Completed"],
              # dt[Completed == 0, list(MeanRAF = mean(RAF), MeanPMPM = mean(PMPM))][, Stage := "Completed"][, Cohort := "All"],
              # dt[Completed == 0, list(MeanRAF = mean(RAF), MeanPMPM = mean(PMPM))][, Stage := "Completed"][, Cohort := "Contacted"],
              # dt[Completed == 0, list(MeanRAF = mean(RAF), MeanPMPM = mean(PMPM))][, Stage := "Completed"][, Cohort := "Enrolled"],
              # dt[Completed == 0, list(MeanRAF = mean(RAF), MeanPMPM = mean(PMPM))][, Stage := "Completed"][, Cohort := "Engaged"],
              dt[Completed == 0, list(MeanRAF = mean(RAF), MeanPMPM = mean(PMPM))][, Stage := "Completed"][, Cohort := "Walked"],
              dt[Completed == 1, list(MeanRAF = mean(RAF), MeanPMPM = mean(PMPM))][, Stage := "Completed"][, Cohort := "Completed"],
              NULL)
 dt3[, Stage := factor(Stage, levels = stages)]
 dt3[, Cohort := factor(Cohort, levels = stages)]
 dt3[, Text := ifelse(as.numeric(Stage) < as.numeric(Cohort), "", ifelse(Stage == Cohort, as.character(Stage), paste("Not", Stage)))]
 
 dt_cnt <- melt(dt[, lapply(.SD, sum, na.rm = T), .SDcols = stages], variable.name = "Stage", value.name = "Members")
 dt_cnt[, Stage := factor(Stage, levels = stages)]
 
 grid.arrange(
   ggplot(dt_cnt, aes(as.numeric(Stage), Members, fill = Stage)) +
     geom_bar(stat = "identity", position = position_nudge(x = .25), width = .5) + 
     scale_x_continuous(breaks = 1:6, labels = stages, limits = c(1, 7)) + scale_y_continuous(labels = comma) +
     labs(x = "Program Stage", title = "Member Waterfall") + guides(fill = FALSE),
   ggplot(dt3, aes(as.numeric(Stage), MeanPMPM, col = Cohort)) + 
     geom_point() + geom_line() + scale_x_continuous(breaks = 1:6, labels = stages, limits = c(1, 7)) + scale_y_continuous(labels = dollar) +
     geom_text(aes(as.numeric(Stage), MeanPMPM, label = Text), hjust = -.1) + guides(col = FALSE) +
     labs(x = "Program Stage", title = "PMPM Waterfall"),
   ggplot(dt3, aes(as.numeric(Stage), MeanRAF, col = Cohort)) + 
     geom_point() + geom_line() + scale_x_continuous(breaks = 1:6, labels = stages, limits = c(1, 7)) +
     geom_text(aes(as.numeric(Stage), MeanRAF, label = Text), hjust = -.1) + guides(col = FALSE) +
     labs(x = "Program Stage", title = "RAF Waterfall")
 )
 
 
 
# End script
 