# Author: Clive Trueman
# About: script to plot track for top 10% of models by month
# for behavioural phase 2.
# Tidied by Natalie Cooper Jan 2018

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# Load libraries
try(detach(package:purrr))
library(maps)
library(mapdata)
library(spatstat)
library(viridis)
library(hdrcde)

## AJ - function to add the 2d boxplot contours

my_hdrcde.filled.contour <- function (x, y, z, 
                                      xlim = range(x, finite = TRUE), 
                                      ylim = range(y, finite = TRUE), 
                                      zlim = range(z, finite = TRUE), 
                                      levels = pretty(zlim, nlevels), 
                                      nlevels = 20, 
                                      color.palette = cm.colors, 
                                      col = color.palette(length(levels) - 
                                                                                                                                                                                                         1), plot.title, plot.axes, asp = NA, xaxs = "i", yaxs = "i", 
          las = 1, axes = TRUE, frame.plot = axes, ...) 
{
  if (any(diff(x) <= 0) || any(diff(y) <= 0)) 
    stop("increasing 'x' and 'y' values expected")
  # plot.new()
  # plot.window(xlim, ylim, "", xaxs = xaxs, yaxs = yaxs, asp = asp)
  .filled.contour(x, y, z, levels, col)
  if (missing(plot.axes)) {
    if (axes) {
      title(main = "", xlab = "", ylab = "")
      Axis(x, side = 1)
      Axis(y, side = 2)
    }
  }
  else plot.axes
  if (frame.plot) 
    box()
  if (missing(plot.title)) 
    title(...)
  else plot.title
  invisible()
}

my_plot_hdr2d <- function (x, shaded = TRUE, show.points = FALSE, 
                           outside.points = FALSE, pch = 20, 
                           shadecols = gray((length(x$alpha):1) / 
                                              (length(x$alpha) + 1)), 
                           pointcol = 1, ...) 
{
  if (shaded) 
    my_hdrcde.filled.contour(x$den$x, x$den$y, x$den$z, 
                          levels = c(x$falpha, 1e+10), col = shadecols, ...)
  else contour(x$den, levels = x$falpha, labcex = 0, add = TRUE, ...)
  if (show.points) 
    points(x$x, x$y, pch = pch, col = pointcol)
  else if (outside.points) {
    index <- (x$fxy < 0.99999 * min(x$falpha))
    points(x$x[index], x$y[index], pch = pch, col = pointcol)
  }
  points(x$mode[1], x$mode[2], pch = 19, col = "black")
  invisible(x)
}

# Read in data
mid.top10 <- read.csv("data/mid.top10percent.csv")

# Pick colours to match phases in Figure 2
mycols <- c(viridis_pal()(10)[1], viridis_pal()(10)[5], viridis_pal()(10)[9])

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# Points plot with phases coloured

#AJ commented out
#png("manuscript/revision/figures/Figure-4-monthly.png", width = 800, height = 600)

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
  
  # add the 2d boxplots using modified package:hdrcde (see funs above)
  hdrinfo <- hdrcde::hdr.2d(Month$Lon, Month$Lat)
  my_plot_hdr2d(hdrinfo, new = FALSE)
  
  # Add points coloured by phase
  points(jLon, jLat, pch = 16, col = mycols[2], cex = 0.2)
  
  # AJ add monthly means
  # points(mean(Month$Lon), mean(Month$Lat), pch = 19, cex = 1, col = 1)
         
  # Add world map
  map('world', col = "black", fill = TRUE, add = TRUE, lwd = 0.25)
  # If you get this error, 
  # Error in as_mapper(.f, ...) : argument ".f" is missing, with no default
  # You need to detach the package purrr (detach(package:purrr)) and reload
  # the maps library (library(map))
  
  par(new=FALSE)
}

#AJ commented out
# dev.off()

