#
# Open graphic device
#
png(filename = "samplePlot.png", 
    width = 10, height = 5, units = "in", 
    res = 300)

par(mar = c(0, 0, 0, 0), mfrow = c(1, 2))

plot(province,
     col = colourscheme[anc1Province$class[anc1Province$year == 2015] + 1],
     border = "gray90",
     lwd = 0.5)
title(main = "At least one antenatal care visit in 2015  ", line = -1, adj = 1)

plot(province,
     col = colourscheme[anc1Province$class[anc1Province$year == 2016] + 1],
     border = "gray90",
     lwd = 0.5)
title(main = "At least one antenatal care visit in 2016  ", line = -1, adj = 1)
legend(x = "bottomright",
       inset = 0.01,
       y.intersp = 1.2,
       legend = c("0", names(print(classIntervals(anc1Province$anc1Std,
                                                  n = 5,
                                                  style = "quantile",
                                                  dataPrecision = 0), 
                                   between = "to", 
                                   cutlabels = FALSE))),
       pch = 15, pt.cex = 2, cex = 0.75,
       col = colourscheme)
#
# Close graphics device
#
dev.off()