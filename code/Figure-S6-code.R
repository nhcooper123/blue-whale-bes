# Author: Clive Trueman
# About: script to plot kernal densities for top 10% and bottom 10% of models
# Tidied by Natalie Cooper Nov 2017

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# Load libraries
library(maps)
library(mapdata)
library(spatstat)

# Read in data
top10 <- read.csv("data/top100.csv")
bottom10 <- read.csv("data/bottom100.csv")

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# Kernal density plots

# Jitter points of lat and long to get smooth density plot
jLat <- jitter(top10$Lat, factor = 2)
jLon <- jitter(top10$Lon, factor = 1)
jLatB <- jitter(bottom10$Lat, factor = 2)
jLonB <- jitter(bottom10$Lon, factor = 1)

# Create point patterns to plot
pts <- ppp(jLon, jLat, window = owin(c(-80, 55), c(-10, 85)))
ptsB <- ppp (jLonB, jLatB, window = owin(c(-80, 55), c(-10, 85)))


png("manuscript/revision/figures/Figure-S6-kernals.png", width = 1200, height = 800)

par(mfrow = c(1, 2))
par(mar = c(2, 1, 3, 1))

# Top 10 %
plot(x = NA, y = NA, xlim = c(-80,55), ylim = c(-10, 85), 
     xlab = "", ylab = "", axes = FALSE, main = "Top 10%")
plot(density(pts), add = TRUE)
map('world', col = "grey", fill = TRUE, add = TRUE, lwd = 0.25)

# Bottom 10 %
plot(x = NA, y = NA, xlim = c(-80,55), ylim = c(-10, 85), 
     xlab = "", ylab = "", axes = FALSE, main = "Bottom 10%")
plot(density(ptsB), add = TRUE)
map('world', col = "grey", fill = TRUE, add = TRUE, lwd = 0.25)

dev.off()
