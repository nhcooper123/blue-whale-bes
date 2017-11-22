# Author: Clive Trueman
# About: script to extract top 10% and bottom 10% of models from 
# simulations and extract latitude and longitude info from them
# Tidied by Natalie Cooper Oct 2017

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# Load libraries
library(plyr)
# note if using group_by in dplyr later you'll need to detach plyr
library(maps)
library(mapdata)
library(tidyverse)

# Read in the measured whale data and select blue whale
whale_isos <- read_csv("data/raw-whale-isotope-data.csv")
blue <- filter(whale_isos, whale_ID == "KC7")

# Fix day numbers reflecting monthly samples 
# (for loess sampling - fixed to have same no.s)
predict.days <- seq(from = 30, to = (8 * 365) + 100, by = 31)
blue$Day.sim <- rev(predict.days)

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# Read in model simulations output
# Takes a bit of time
resTrack <- read_csv("data/model.sims.csv")

# Remove rep that didn't work
resTrack<-resTrack[-c(3213051:3213211),]

# Fix day numbers reflecting monthly samples (for loess sampling)
lengthS <- length(resTrack$d13C[resTrack$Rep == 1])
mo_no <- lengthS / 30 + 1
sampleD <- rnorm(mo_no, 30, 1)
sampledays <- as.integer(cumsum(sampleD))

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# Extract model values only for days of sample - 
# as defined above (could change to an average if wanted)
test <- ddply(resTrack, "Rep", function(x) {
	newSeries <- x[x$count2%in%sampledays, ]
})

test2 <- ddply(test, "Rep", function(x) {
	newSeries <- x[x$count2 < 3021, ]
})

# Add the different d13C isoscape values only for the sampled tracks
# for the six month sliding window isoscape
for(i in 1:nrow(test2)){
  mon.no <- test2$Month[i]
  test2$TL4[i] <- raster::extract(x = TL4[[mon.no]], 
                                  y = test2[i, c("Lon", "Lat")])
}

# Run loess through series and predict for same days as measured
# for the six month sliding window isoscape
test3 <- ddply(test2, "Rep", function(x){
  lo <- predict(loess(x$TL4 ~ x$count2, span = 0.05))[1:97]
})

# Transpose
test3 <- t(test3)

# Add column names
colnames(test3) <- 1:length(unique(resTrack$Rep)) - 1
test3 <- as.data.frame(test3[-1, ])

# Link to the measured data to allow regression comparison
test3$Day <- test2$count2[test2$Rep == 1][1:97]
test3$Blue <- rev(blue$d13C)

# Run regressions for each model simulation and save r square values
r2 <- vector()

for(run in 1:length(unique(resTrack$Rep))){
	testX <- lm(test3[, run] ~ test3$Blue)
	r2[run] <- summary(lm(test3[, run] ~ test3$Blue))$adj.r.squared
}

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
# extract top 10% and bottom 10% best fit models for whole series
# write out for figures 
cut <- 0.1
limit <- length(r2) - length(r2) * cut

bottom <- 0.1
limitB <- length(r2) * bottom

# Define phase of behaviour
# Phase 1 = days 1 - 999
# Phase 2 = days 1000 - 2499
# Phase 3 = days 2500 - 3019
resTrack$phase <- rep(NA, length(resTrack$Rep))
resTrack$phase[resTrack$count2 < 1000] <- 1
resTrack$phase[resTrack$count2 >= 1000 & resTrack$count2 < 2500] <- 2
resTrack$phase[resTrack$count2 > 2500] <- 3

topX <- ddply(resTrack, "Rep", function(x) {
  newSeries <- x[x$Rep%in%unique(resTrack$Rep)[order(r2)[limit:length(r2)]], ]
})

bottomY <- ddply(resTrack, "Rep", function(x) {
  newSeries <- x[x$Rep%in%unique(resTrack$Rep)[order(r2)[1:limitB]],]
})

# Write to file
write.csv(file = "data/top10.csv", topX, quote = FALSE, row.names = FALSE)
write.csv(file = "data/bottom10.csv", bottomY, quote = FALSE, row.names = FALSE)

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# Extract latitude and longitudes from top and bottom 10% of models
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# what is the max lat for each of the top X models?
max.lat <- ddply(topX, "Rep", function(x) {
	newSeries <- max(x$Lat)
})

# what is the max lat for each of the bottom Y models?
max.lat.bot <- ddply(bottomY, "Rep", function(x) {
	newSeries <- max(x$Lat)
})

# what is the std.dev of lat for each of the top X models?
sd.lat <- ddply(topX, "Rep", function(x) {
  newSeries <- sd(x$Lat)
})

# what is the std.dev of lat for each of the bottom Y models?
sd.lat.bot <- ddply(bottomY, "Rep", function(x) {
  newSeries <- sd(x$Lat)
})

# Add top and bottom groupings
max.lat$group <- rep("top", length(max.lat$Rep))
max.lat.bot$group <- rep("bottom", length(max.lat.bot$Rep))

sd.lat$group <- rep("top", length(sd.lat$Rep))
sd.lat.bot$group <- rep("bottom", length(sd.lat.bot$Rep))

# Combine
max.lat.ds <- rbind(max.lat, max.lat.bot)
sd.lat.ds <- rbind(sd.lat, sd.lat.bot)

# Write file
write.csv(file = "data/max.lat.csv", max.lat.ds, quote = FALSE)
write.csv(file = "data/sd.lat.csv", sd.lat.ds, quote = FALSE)

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
