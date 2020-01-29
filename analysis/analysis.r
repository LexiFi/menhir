#!/usr/bin/env Rscript

require(ggplot2)

# Load our data.
mydata <- read.csv("data.csv")
# Restrict ourselves to a subset of the data.
mydata <- subset(mydata, time >= 0.1)

# A function that creates a scatter plot. Data on the X and Y axes
# is determined by the argument [xy]. The scale is logarithmic.
# [lx] and [ly] are labels for the axes.
plotloglog <- function(xy, lx, ly) {
  ggplot(mydata, xy) +
    geom_point(size=3) +
    scale_x_log10() +
    scale_y_log10() +
    xlab(lx) +
    ylab(ly)
}

# Plot.
myplot <- plotloglog(aes(x=lr1states, y=time), "# LR(1) states", "time (seconds)")
ggsave("lr1states-time.pdf", myplot, width=12, height=8, units="cm")
