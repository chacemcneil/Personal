this <- structure(list(Time = c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 
                                1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 3L, 3L, 
                                3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 4L, 4L, 4L, 4L, 4L, 4L, 
                                4L, 4L, 4L, 4L, 4L, 4L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 
                                5L, 5L, 6L, 6L, 6L, 6L, 6L, 6L, 6L, 6L, 6L, 6L, 6L, 6L, 7L, 7L, 
                                7L, 7L, 7L, 7L, 7L, 7L, 7L, 7L, 7L, 7L, 8L, 8L, 8L, 8L, 8L, 8L, 
                                8L, 8L, 8L, 8L, 8L, 8L, 9L, 9L, 9L, 9L, 9L, 9L, 9L, 9L, 9L, 9L, 
                                9L, 9L, 10L, 10L, 10L, 10L, 10L, 10L, 10L, 10L, 10L, 10L, 10L, 
                                10L), 
                       Type = c(1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 
                                1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 1L, 1L, 1L, 1L, 
                                2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 
                                3L, 3L, 3L, 3L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 
                                1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 1L, 1L, 1L, 1L, 
                                2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 
                                3L, 3L, 3L, 3L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 
                                1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L), 
                       Value = c(848565.34, 
                                 1463110.61, 626673.64, 277708.41, 841422.11, 956238.14, 461092.16, 
                                 208703.75, 800837.48, 1356164.25, 549509.34, 300241.53, 851247.9714, 
                                 1353358.318, 598536.5948, 307485.0918, 332042.2275, 666157.8721, 
                                 194981.1566, 79344.50328, 831003.6952, 1111311.517, 521632.3074, 
                                 274384.1731, 1174671.569, 1070301.745, 454876.1589, 351973.2418, 
                                 5631710.101, 279394.6061, 119034.4969, 39693.31587, 1166869.32, 
                                 1156855.09, 369816.8152, 274092.5751, 924474.1129, 975028.0207, 
                                 449213.7419, 213855.3067, 1967188.317, 178841.604, 43692.69319, 
                                 12493.90538, 835142.6168, 876273.4462, 354154.644, 182794.3813, 
                                 1158096.251, 998647.6908, 566726.9865, 195099.4295, 1798902.332, 
                                 171519.4741, 81644.02724, 12221.41779, 1301775.314, 920464.9992, 
                                 294140.4882, 175626.9677, 2179780.499, 1838687.535, 978775.2674, 
                                 366668.3462, 5385970.324, 177527.1577, 65310.32674, 5986.871716, 
                                 2250834.171, 1547858.632, 666444.2992, 251767.3006, 1786086.335, 
                                 1597055.451, 563976.9719, 309186.1626, 487105.824, 279712.1658, 
                                 86471.46603, 24434.05486, 1563940.414, 1409428.038, 531425.682, 
                                 257056.5524, 1685501.271, 1371943.438, 881348.5022, 313355.8284, 
                                 170771.9118, 155596.7479, 59881.60825, 12090.57989, 1668571.543, 
                                 1150257.058, 563054.758, 306767.0344, 2214849.859, 1724719.891, 
                                 822092.2031, 443194.4609, 8897796.235, 87491.42925, 10699.30103, 
                                 18131.89738, 2137240.993, 1476873.778, 741685.9913, 549539.9735, 
                                 1362085.657, 1266106.09, 448653.8889, 278236.8416, 1671665.39, 
                                 95239.07396, 54173.57043, 10125.82011, 1335200.152, 1167824.903, 
                                 426738.1845, 261255.2092)), 
                  .Names = c("Time", "Type", "Value"), row.names = c(NA, -120L), class = "data.frame")

class(this)
head(this)
sapply(this, class)

this$Type = factor(this$Type)

this <- data.table(this)
this[, table(Time, Type)]
this$Measurement <- rep(1:4, 30)


ggplot() + geom_bar(data = this[Measurement > 0, list(Value = sum(Value)), by = list(Time, Type)], aes(Time, Value, group = Type), position = "dodge", stat = "identity", fill = "green", col = "black") + 
  geom_bar(data = this[Measurement > 1, list(Value = sum(Value)), by = list(Time, Type)], aes(Time, Value, group = Type), position = "dodge", stat = "identity", fill = "red", col = "black") + 
  geom_bar(data = this[Measurement > 2, list(Value = sum(Value)), by = list(Time, Type)], aes(Time, Value, group = Type), position = "dodge", stat = "identity", fill = "yellow", col = "black") + 
  geom_bar(data = this[Measurement > 3, list(Value = sum(Value)), by = list(Time, Type)], aes(Time, Value, group = Type), position = "dodge", stat = "identity", fill = "blue", col = "black")


# Or alternatively ...

this2 <- this[, list(Measurement, Value = cumsum(Value)), by = list(Time, Type)]

g <- ggplot(this2, aes(factor(Time), Value, fill = factor(Measurement), group = Type)) + geom_bar(aes(y = 0), stat = "identity") + labs(x = "Time", y = "Value", fill = "")
fillcols = unique(ggplot_build(g)$data[[1]]$fill[order(ggplot_build(g)$data[[1]]$group)])
for (i in rev(seq_along(unique(this2$Measurement)))) {
  g <- g + geom_bar(data = this2[Measurement == i], fill = fillcols[i], position = "dodge", stat = "identity", col = "black")
}
g

