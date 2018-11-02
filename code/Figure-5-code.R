# Author: Clive Trueman
# About: script to plot track for top 10% of models by month
# for behavioural phase 2.
# Tidied by Natalie Cooper Jan 2018

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# Load libraries
library(maps)
library(mapdata)
library(spatstat)
library(viridis)

# Read in data
mid.top10 <- read.csv("data/mid.top10percent.csv")

# Pick colours to match phases in Figure 2
mycols <- c(viridis_pal()(10)[1], viridis_pal()(10)[5], viridis_pal()(10)[9])

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# Points plot with phases coloured
png("manuscript/revision/figures/Figure-4-monthly.png", width = 800, height = 600)

# Make 12 plots
par(mfrow = c(3, 4))
par(mar = c(1, 1, 2, 1))

# Loop through months  
for (Mn in 1:12) {
  
  # Make background of plots
  plot(x = NA, y = NA, xlim = c(-80, 50), ylim = c(0, 80), 
       xlab = "", ylab = "", axes = FALSE, main = month.abb[Mn], cex.main = 2)
  rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], 
       col = "grey90")
  
  # Select month
  Month <- mid.top10[mid.top10$Month == Mn, ]
  
  # Jitter points of lat and long to get smooth density plot
  jLat <- jitter(Month$Lat, factor = 2)
  jLon <- jitter(Month$Lon, factor = 1)
  
  # Add points coloured by phase
  points(jLon, jLat, pch = 16, col = mycols[2], cex = 0.2)

  # Add world map
  map('world', col = "black", fill = TRUE, add = TRUE, lwd = 0.25)
  # If you get this error, 
  # Error in as_mapper(.f, ...) : argument ".f" is missing, with no default
  # You need to detach the package purrr (detach(package:purrr)) and reload
  # the maps library (library(map))
  
  par(new=FALSE)
}

dev.off()

