# Author: Clive Trueman
# Adapted by: Andrew Jackson 19-Jun-2017
# About: script to generate figure S3
# Modified by Natalie Cooper Oct 2017

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# Load libraries
library(tidyverse)
library(raster)
library(RColorBrewer)

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# d13Carbon
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

# Read in data
carbon <- raster("data/Atl_Annual_d13C.grd")

# Extract values
val <- getValues(carbon)

# Create dataframe of values per cell
xy <- as.data.frame(xyFromCell(carbon, 1:ncell(carbon)))
xy$val <- val

# Plot map of values
p <- 
  ggplot(na.omit(xy), aes(x = x, y = y, fill = val)) + 
    geom_raster() + 
    borders("world", colour = "gray50", fill = "gray50")  +
    coord_fixed(xlim = c(-80, 50),  
                ylim = c(0, 80), 
                ratio = 1.3) + 
    scale_fill_distiller(type = "seq", palette = "YlOrRd", direction = -1,
                         name = expression(paste(delta^{13}, "C (\u2030)"))) + 
    theme_classic(base_size = 14) + 
    theme(axis.line  = element_blank(),
          axis.text  = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank())

ggsave("manuscript/revision/figures/Figure-S3-plankton-d13C-map.png", p, 
       device = png(width = 600, height = 400))

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# d15Nitrogen
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

nitrogen <- raster("data/Atl_Annual_d15N.grd")

val2 <- getValues(nitrogen)
xy2 <- as.data.frame(xyFromCell(nitrogen, 1:ncell(nitrogen)))
xy2$val2 <- val2

p2 <- 
  ggplot(na.omit(xy2), aes(x = x, y = y, fill = val2)) + 
    geom_raster() + 
    borders("world", colour = "gray50", fill = "gray50")  +
    coord_fixed(xlim = c(-80, 50),  
                ylim = c(0, 80), 
                ratio = 1.3) + 
    scale_fill_distiller(type = "seq", palette = "YlOrRd", direction = -1,
                       name = expression(paste(delta^{15}, "N (\u2030)"))) + 
    theme_classic(base_size = 14) + 
    theme(axis.line  = element_blank(),
          axis.text  = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank())

ggsave("manuscript/revision/figures/Figure-S3-plankton-d15N-map.png", p2, 
       device = png(width = 600, height = 400))
