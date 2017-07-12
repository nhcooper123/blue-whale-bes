# Author: Clive Trueman
# Adapted by: Andrew Jackson 19-Jun-2017
# About: script to generate figure 1
# Modified by NC for public engagement to omit nitrogen

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
library(raster)
library(gstat)
library(sp)
library(maps)
library(mapdata)
library(RColorBrewer)

library(tidyr)
library(dplyr)
library(ggplot2)
library(magrittr)
library(viridis)
library(lubridate)

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

new_years <- with(KC7, seq(min(year), max(year), by = 1))
new_years_chr <- paste(new_years, "01", "01", sep = "-")
new_years_date <- as.Date(new_years_chr)
new_years_day <- tail(KC7$days,1) - 
  (as.numeric(last_sample - new_years_date ))
new_years_samp <- round(new_years_day[2:length(new_years_day)] * growth_rate / 
                          (samp_resolution * 365.25))
new_years_all <- round(new_years_day * growth_rate / 
                         (samp_resolution * 365.25))


# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# plot the isotope data

point_size <- 4

isop <- ggplot(KC7, aes(x = Samp.No)) + 
  theme_classic(base_size = 16) + 
  xlab("Baleen sample (cm from youngest sample)") + 
  scale_x_continuous(breaks = seq(0,80,20), 
                     labels = seq(80,0,-20),
                     sec.axis = sec_axis(~., 
                                         breaks = new_years_all + 13.5/2,
                                         labels = new_years,
                                         name = "Year"))
  

isop <- isop + geom_line(aes(y = d13C)) + 
  geom_point(aes(y = d13C), size = point_size, shape = 21, fill = "black") +
  ylab(expression(paste(delta^{13}, "C (\u2030)"))) +
  theme(axis.text.y = element_blank())

# add the second axis and rescale it apppropriately
isop <- isop + 
  scale_y_continuous(
    sec.axis = sec_axis(~.,  name = "" )) +
    theme(axis.text.y = element_blank())

isop <- isop + geom_vline(xintercept = new_years_samp,
                          color = "grey", lty = 2)

print(isop)

ggsave("outreach/Figure-1-raw-dC-dN-data_outreach.png", isop, 
      device = png(width = 600, height = 400))
