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


# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

#----
# plot the isotope data

point_size <- 4

isop <- ggplot(KC7, aes(x = Samp.No)) + 
  theme_classic(base_size = 16) + 
  xlab("Baleen sample (cm from youngest sample)") + 
  scale_x_continuous(breaks = seq(0,80,20), 
                     labels = seq(80,0,-20))

isop <- isop + geom_line(aes(y = d13C)) + 
  geom_point(aes(y = d13C), size = point_size, shape = 21, fill = "black") +
  ylab(expression(paste(delta^{13}, "C (\u2030)")))

# add the d15N data but need to scale it to d13C
v_shift <- 27.6 #mean(KC7$d15N) - mean(KC7$d13C) 
# sd <- sd(KC7$d15N) / sd(KC7$d13C) # not currently used in rescaling

isop <- isop + geom_line(aes(y = d15N - v_shift)) + 
  geom_point(aes(y = (d15N - v_shift)), size = point_size, 
             shape = 21, fill = "grey")

# add the second axis and rescale it apppropriately
isop <- isop + 
  scale_y_continuous(
    sec.axis = sec_axis(~.+v_shift, 
                        name = expression(paste(delta^{15}, "N (\u2030)"))))

isop <- isop + geom_vline(xintercept = new_years_samp,
                          color = "grey", lty = 2)

print(isop)

ggsave("figures/Figure-1a-raw-dC-dN-data.png", isop, 
       device = png(width = 600, height = 400))

# with(KC7, day_new_year[day_new_year > 0]))

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# ***********************************************************
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# ----
# Panel B

# Map plot of all simulated tracks for the full migratory model

resTrack <- read.csv("data/record_migrate5.csv", header=TRUE)

# remove NAs from the dataset
# resTrack <- filter(resTrack, !is.na(d13C))

# create a new sequence of days on which to predict our
# loess smoothed function
days <- seq(from=1, to=(6*365+60))

# jitter the points
resTrack$jLat <- jitter(resTrack$Lat, factor = 2)
resTrack$jLon <- jitter(resTrack$Lon, factor = 1)

# im not sure what the best colormap to use is for the months / seasons
# create the colorramp
# colz<-colorRampPalette(c("light grey", "green", "pink","red"))(6)
# colz2<-colorRampPalette(c("red", "orange", "light grey"))(6)
# colzF<-c(colz,colz2)

# define seasons for each month to use to map to a palette
four.seasons <-  c(rep(1, 3),
                   rep(2, 3),
                   rep(3, 3),
                   rep(4, 3)
)

# add to resTrack df
resTrack$season <- four.seasons[resTrack$Month]

# prep mp object
mp <- NULL

# create a layer of borders
mapWorld <- borders("world",
                    colour="gray50", fill="gray50") 


# set up the plot
mp <- ggplot(resTrack, aes(Lon, Lat, group = Rep)) 

# add the world map
mp <- mp + mapWorld



# customise labels and style
mp <- mp + theme_classic(base_size = 14) #+ xlab("Longitude") + ylab("Latitude")

# zoom into Atlantic region
mp <- mp +  coord_fixed(xlim = c(-80, 50),  
                        ylim = c(0, 80), 
                        ratio = 1.3)

#Now Layer the points on top
mp2 <- mp + geom_point(aes(x=jLon, y=jLat, 
                           color = factor(season)), 
                       size=0.1) + 
  scale_color_viridis(discrete = TRUE, "Quarter") + 
  guides(colour = guide_legend(override.aes = list(size=5)))

# remove axes and ticks and labels
mp2 <- mp2 + theme(axis.line  = element_blank(),
                 axis.text  = element_blank(),
                 axis.ticks = element_blank(),
                 axis.title = element_blank())

print(mp2)

ggsave("figures/Figure-1b-migratory-model-full-map.png", mp2, 
       device = png(width = 600, height = 600, units = "px"))



# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# ***********************************************************
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# ----
# Panel C




# create a vector of dates corresponding to each day
resTrack$rev_days <- resTrack$Day.No - max(resTrack$Day.No)
resTrack$date     <- as.Date(resTrack$rev_days, origin = last_sample)

# set the constant to add as a correction to the d13C data
cnst <- 6


# a function to build the data.frame from the loess fit
# I need this here as a new "days" vector has to be 
# created from the length of the predicted loess object
tidy_loess <- function(df, span = 0.1, ...){
  Z <- predict(loess((df$d13C + cnst) ~ days, 
                     span = span))
  
  out <- data.frame(lo = Z, days = seq(1:length(Z)))
} # end function

# take resTrack and group it by replicate, then apply our 
# tidy_loess() function to generate estimated d13C for 
# each of our days
tmp <- resTrack %>% group_by(Rep) %>% 
  do(., tidy_loess(., span = 0.05))

# data required to add a vertical line indicating the start of migration
migrate <- as.Date("1889-06-01")
first_sim_day <- as.Date("1885-01-01")
migrate_day <- as.numeric(migrate - first_sim_day)

# plot the daily tracks
track_plot <- ggplot(tmp, aes(days, lo, group = Rep)) 

# add the vertical line where migration starts in the simulations
track_plot <- track_plot + geom_vline(xintercept = as.numeric(migrate_day),
                                      lty = 2,
                                      color = "black",
                                      size = 1)

# add the vertical lines for new years
# overkill code!.. the sim is in days of course, so 
# new years every 365 days. doh. Except im going
# to be a pedant and stick with it because of leap years.
new_years_sim_day <- as.numeric(new_years_date - first_sim_day)

track_plot <- track_plot + geom_vline(xintercept = new_years_sim_day,
                                      color = "grey", lty = 2)

# add the simulated d13C data
track_plot <- track_plot  + geom_line(col = viridis(3)[2], alpha = 0.25) + 
  xlab("Days") + 
  ylab(expression(paste(delta^{13}, "C (\u2030)"))) + 
  theme_classic(base_size = 14)
  

# superimpose the raw d13C data
# tdf <- 3.5 # a y-scale shift, not currently used.

x_shift <- max(tmp$days) - max(KC7$days)

y_scale <- 3

KC7$d13C_scaled <- with(KC7, ((d13C - mean(d13C)) * y_scale) + mean(d13C)) 

track_plot <- track_plot + geom_line(data = KC7, 
                                     mapping = aes(x = days + x_shift, 
                                                   y = d13C_scaled, 
                                                   group = NULL), 
                                     col = viridis::viridis(3)[1],
                                     size = 1)

track_plot <- track_plot + geom_point(data = KC7, 
                                      mapping = aes(x = days + x_shift, 
                                                   y = d13C_scaled, 
                                                   group = NULL), 
                                      col = viridis::viridis(6)[1],
                                      fill = viridis::viridis(6)[2],
                                      size = point_size,
                                      shape = 21)



print(track_plot)

ggsave("figures/Figure-1c-migratory-model-d13C.png", track_plot, 
       device = png(width = 600, height = 400))


# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# calculate the correlation between the simulated and observed data

zEmpC <- scale(KC7$d13C)
zDays <- KC7$days + x_shift # align the days between empirical and simulated


# predict the loess on the same days

# a function to build the data.frame from the loess fit
# I need this here as a new "days" vector has to be 
# created from the length of the predicted loess object
predict_loess <- function(df, span = 0.1, newdata = NULL){
  Z <- predict(loess(df$d13C ~ days, 
                     span = span), newdata = newdata)
  
  out <- data.frame(lo = Z, days = seq(1:length(Z)))
} # end function


zSimC <- resTrack %>% group_by(Rep) %>% do(., data.frame(d13C = scale(.$d13C)))

zSimMatched <- zSimC %>% group_by(Rep) %>% 
  do(., predict_loess(., span = 0.05, newdata = zDays))



corEmpSim <- zSimMatched %>% group_by(Rep) %>% 
  summarise(cor = cor(zEmpC, lo), p = cor.test(zEmpC, lo)$p.value)

hist_cor <- ggplot(corEmpSim, aes(cor)) + 
  geom_histogram(binwidth = 0.1, color = "black", fill = "grey") + 
  xlim(-0.5, 1) + theme_classic(base_size = 16) + 
  xlab("Correlation coefficient") + scale_y_continuous(expand = c(0, 0))
hist_cor


hist_cor_p <- ggplot(corEmpSim, aes(p)) + 
  geom_histogram(binwidth = 0.01, color = "black", fill = "grey") + 
   theme_classic(base_size = 16) + 
  xlab("Correlation coefficient") + scale_y_continuous(expand = c(0, 0))
hist_cor_p


# some summary statistics
corEmpSim %>% summarise(mean = mean(cor), median = median(cor), 
                        mode = hdrcde::hdr(cor)$mode, sd = sd(cor))

# number of p-values less than 0.05
p_crit <- sum(corEmpSim$p <= 0.05 / 50)






