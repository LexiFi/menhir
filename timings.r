#!/usr/bin/env Rscript

require(ggplot2)

# Our working directory.
pwd <- "."

# Load our data.
mydata <- read.csv(paste(pwd, "timings.csv", sep="/"))
# Restrict ourselves to a subset of the data.
mydata <- subset(mydata, time >= 0.25)

# A function which saves a plot to a PDF file.
save <- function(filename) {
  ggsave(paste(pwd, filename, sep="/"), width=12, height=8, units="cm")
}

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

# Scatter plots.
plotloglog(aes(x=states, y=time), "# states", "time (seconds)")
save("states-time.pdf")
