# Author: Clive Trueman
# About: script to plot track for top 10% of models
# Tidied by Natalie Cooper Nov 2017

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# Load libraries
library(maps)
library(mapdata)
library(spatstat)
library(viridis)

# Read in data
top10 <- read.csv("data/top100.csv")

# Pick colours
mycols <- c(viridis_pal()(10)[1],viridis_pal()(10)[5],viridis_pal()(10)[9])

# Jitter points of lat and long to get smooth density plot
jLat <- jitter(top10$Lat, factor = 2)
jLon <- jitter(top10$Lon, factor = 1)

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# Points plot with phases coloured
png("manuscript/revision/figures/Figure-2-points.png", width = 800, height = 600)

# Top 10 %
plot(x = NA, y = NA, xlim = c(-80, 50), ylim = c(0, 80), 
     xlab = "", ylab = "", axes = FALSE)
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], 
     col = "grey90")

# Add points coloured by phase
points(jLon, jLat, pch = 16, col = mycols[top10$phase], cex = 0.2)

# Add world map
map('world', col = "black", fill = TRUE, add = TRUE, lwd = 0.25)
# If you get this error, 
# Error in as_mapper(.f, ...) : argument ".f" is missing, with no default
# You need to detach the package purrr (detach(package:purrr)) and reload
# the maps library (library(map))

legend(x = 42, y = 28, legend = c(1, 2, 3), col = mycols, pch = 16, fill = "white",
       border = "white", title = "Phase", bg = "white", box.col = "white", xjust = 0.5,
       pt.cex = 2, cex = 2)

dev.off()
