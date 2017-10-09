# Author: Clive Trueman
# Adapted by: Andrew Jackson 19-Jun-2017
# About: script to generate figures S1 & S2
# Modified by Natalie Cooper Oct 2017

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

library(raster)
library(gstat)
library(sp)
library(maps)
library(mapdata)

library(tidyr)
library(dplyr)
library(ggplot2)
library(magrittr)
library(viridis)
library(lubridate)

library(TSA)

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# Read in raw whale isotope data
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

whale_isos <- read.csv("data/raw-whale-isotope-data.csv", header = TRUE, 
                       stringsAsFactors = FALSE)

KC7 <- filter(whale_isos, whale_ID == "KC7")

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# function to add text for the highest frequency spectral 
# densities to periodogram plots

periodogramText <- function(p, k){
  
  # extract frequencies and spectral densities from the object
  dd <- data.frame(freq = p$freq, spec = p$spec)
  
  # sort them into rank order
  ord <- dd[order(-dd$spec), ]
  
  # extract the top two densities
  top2  <-  head(ord, 2)
  
  # convert to distance along our sampling transect
  distance <- 1/top2$freq
  
  # add either the first or second
  text(0.4, max(p$spec) * 0.8, 
       paste0(sprintf("%.1f", round(distance[k], 1))," cm"), 
       cex = 2)
}

png(filename = "manuscript/figures/Figure-S1-periodograms.png", 
    width = 1000, height = 600)

par(mfrow = c(2, 2))

p1 <- periodogram(KC7$d15N, main = "A - Full d15N")
periodogramText(p1, k = 2)

p2 <- periodogram(KC7$d15N[1:60], main = "B - Oldest 60cm d15N")
periodogramText(p2, k = 1)

p3 <- periodogram(KC7$d13C, main = "C - Full d13C")
periodogramText(p3, k = 2)

p4 <- periodogram(KC7$d13C[1:60], main = "D - Oldest 60cm d13C")
periodogramText(p4, k = 2)

dev.off()

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# Figure S2 - Cross correlation

n <- nrow(KC7)

# save to file
png(filename = "manuscript/figures/Figure-S2-cross-cor.png", 
    width = 1000, height = 600)

par(mfrow = c(1,2))

ccf(KC7$d13C[1:60], KC7$d15N[1:60], lag.max = 30, 
    plot = TRUE, main = "Oldest 60cm")

ccf(KC7$d13C[61:n], KC7$d15N[61:n], lag.max = 30, 
    plot = TRUE, main = "Youngest 20cm")

# TSA::acf(cbind(KC7$d13C[1:60], KC7$d15N[1:60]), main = "Oldest 60cm")
# TSA::acf(cbind(KC7$d13C[61:n], KC7$d15N[61:n]), main = "Youngest 20cm")

dev.off()










