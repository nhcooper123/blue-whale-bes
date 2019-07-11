# Author: Clive Trueman
# Adapted by: Andrew Jackson 19-Jun-2017
# About: script to generate figures S1 & S2
# Modified by Natalie Cooper Oct 2017

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

library(tidyverse)
library(TSA)

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# Read in raw whale isotope data
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

whale_isos <- read_csv("data/raw-whale-isotope-data.csv")

KC7 <- filter(whale_isos, whale_ID == "KC7")

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# function to add text for the highest frequency spectral 
# densities to periodogram plots
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

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

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# Figure S1 - periodograms
# phase 1 = 100 - 70cm
# phase 2 = 70 - 18cm
# phase 3 = 18 - 1cm
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

png(filename = "manuscript/PeerJ/figures/Figure-S1-periodograms.png", 
    width = 1000, height = 600)

par(mfrow = c(2, 2))

p1 <- periodogram(KC7$d15N, main = "A - Full d15N")
periodogramText(p1, k = 2)

p2 <- periodogram(KC7$d15N[18:70], main = "B - Phase 2 d15N")
periodogramText(p2, k = 1)

p3 <- periodogram(KC7$d13C, main = "C - Full d13C")
periodogramText(p3, k = 2)

p4 <- periodogram(KC7$d13C[18:70], main = "D - Phase 2 d13C")
periodogramText(p4, k = 2)

dev.off()

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# Figure S2 - Cross correlation
# phase 1 = 100 - 70cm
# phase 2 = 70 - 18cm
# phase 3 = 18 - 1cm
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

n <- nrow(KC7)

# save to file
png(filename = "manuscript/PeerJ/figures/Figure-S2-cross-cor.png", 
    width = 1000, height = 600)

par(mfrow = c(1, 3))

ccf(KC7$d13C[70:n], KC7$d15N[70:n], lag.max = 30, 
    plot = TRUE, main = "Phase 1")

ccf(KC7$d13C[18:70], KC7$d15N[18:70], lag.max = 30, 
    plot = TRUE, main = "Phase 2")

ccf(KC7$d13C[1:18], KC7$d15N[1:18], lag.max = 30, 
    plot = TRUE, main = "Phase 3")

dev.off()
