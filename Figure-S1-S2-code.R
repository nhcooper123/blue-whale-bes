# Author: Clive Trueman
# Adapted by: Andrew Jackson 19-Jun-2017
# About: script to generate figure 1


# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

# setwd("/Users/trueman/Desktop/R scripts/migrate.files")


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
rm(list = ls())
graphics.off()



# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# Panel A - raw whale isotope data with two y-axes
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =


whale_isos <- read.csv("data/KC.NHM.full.csv", header = TRUE, 
                       stringsAsFactors = FALSE)

KC7 <- filter(whale_isos, Whale == "KC7")

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# convert sample number to days using following parameters
samp_resolution <- 1 # samples per centimetre

growth_rate <- 13.5  # baleen growth rate in centimetres per year

days <- KC7$Samp.No * samp_resolution * 365.25 / growth_rate

days <- days - days[1]

KC7$days <- days

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# convert to calendar date

# reverse time
rev_days <- days - days[length(days)]

last_sample <- as.Date("1891-03-01")

sample_date <- last_sample + rev_days

KC7$rev_days <- rev_days
KC7$date <- sample_date
KC7$year <- year(sample_date)

# calculate a rolling mean to identify the changes between years

idx_new_year <- c(0, diff(KC7$year))

KC7$day_new_year <- days * idx_new_year - 
  (idx_new_year * 0.5 * samp_resolution * 365.25 / growth_rate)

KC7$samp_new_year <- KC7$Samp.No * idx_new_year - 
  (idx_new_year * 0.5)

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

# function to add text for the highest frequency spectral 
# densities to periodgram plots

periodogramText <- function(p, k){
  
  # extract frequencies and spectral densities from the object
  dd <- data.frame(freq=p$freq, spec=p$spec)
  
  # sort them into rank order
  ord <- dd[order(-dd$spec),]
  
  # extract the top two densities
  top2  <-  head(ord, 2)
  
  # convert to distance along our sampling transect
  distance <- 1/top2$freq
  
  # add either the first or second
  text(0.4, max(p$spec)*0.8, 
       paste0(sprintf("%.1f", round(distance[k],1))," cm"), 
       cex = 2)
  
  #text(0.4, 3.5, paste(distance[2], "cm")
}


png(filename = "manuscript/figures/Figure-S1-periodograms.png", 
    width = 1000, height = 600)

par(mfrow = c(2,2))

p1 <- periodogram(KC7$d15N, main = "A - Full d15N")
periodogramText(p1, k = 2)

p2 <- periodogram(KC7$d15N[1:60], main = "B -Oldest 60cm d15N")
periodogramText(p2, k = 1)

p3 <- periodogram(KC7$d13C, main = "C - Full d13C")
periodogramText(p3, k = 2)

p4 <- periodogram(KC7$d13C[1:60], main = "D - Oldest 60cm d13C")
periodogramText(p4, k = 2)

dev.off()

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# Figure S2 - cross correlation

n <- nrow(KC7)

par(mfrow = c(1,2))
TSA::acf(cbind(KC7$d13C[1:60], KC7$d15N[1:60]), main = "Oldest 60cm")

TSA::acf(cbind(KC7$d13C[61:n], KC7$d15N[61:n]), main = "Youngest 20cm")

# save to file
png(filename = "manuscript/figures/Figure-S2-cross-cor.png", 
    width = 1000, height = 600)



par(mfrow = c(1,2))

ccf(KC7$d13C[1:60], KC7$d15N[1:60], lag.max=30, 
    plot=TRUE, main = "Oldest 60cm")

ccf(KC7$d13C[61:n], KC7$d15N[61:n], lag.max=30, 
    plot=TRUE, main = "Youngest 20cm")

# TSA::acf(cbind(KC7$d13C[1:60], KC7$d15N[1:60]), main = "Oldest 60cm")
# TSA::acf(cbind(KC7$d13C[61:n], KC7$d15N[61:n]), main = "Youngest 20cm")

dev.off()










