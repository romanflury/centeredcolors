rm(list = ls())

library(colorspace)
library(fields)

hist(volcano)

# standardize volcano
scaled_volcano <- (volcano - mean(volcano))/sd(volcano)
mean(scaled_volcano)
sd(scaled_volcano)
hist(scaled_volcano, breaks = 100)

# a rudimentary approach to center colors
centercolors <- function(values) {
  values <- unique(c(values))
  valuerange <- range(values)
  colorrange <- c(-max(abs(valuerange)), max(abs(valuerange)))
  colorseq <- seq(from = colorrange[1], to = colorrange[2], by = 0.01)
  colors <- colorspace::diverge_hcl(length(colorseq), "Cork")
  ccolors <- colors[colorseq <= valuerange[2] & colorseq >= valuerange[1]]

  return(ccolors)
}

par(mfrow = c(2,2), mar = rep(3, 4))
image.plot(scaled_volcano, col = diverge_hcl(64, "Cork"), main = "standardized volcano, diverging colors")
image.plot(scaled_volcano, col = centercolors(scaled_volcano), main = "standardized volcano, diverging colors and centered at zero")
image.plot(scaled_volcano, main = "standardized volcano, rainbow color palette")

par(mfrow = c(2,1), mar = rep(3, 4))
image.plot(volcano, col = sequential_hcl(64, rev = T, "Greens"), main = "original volcano, sequential colors")
image.plot(volcano, main = "original volcano, rainbow palette")

