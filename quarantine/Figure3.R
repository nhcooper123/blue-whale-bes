# Author: Clive Trueman
# Adapted by: Andrew Jackson June 2017 & Natalie Cooper Oct 2017.
# About: Script to generate figure 1

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# Load libraries

#library(raster)
#library(gstat)
#library(sp)
##library(maps)
#library(mapdata)
#library(RColorBrewer)

#library(tidyr)
#library(dplyr)
#library(ggplot2)
#library(magrittr)

library(viridis)
library(tidyverse)
library(lubridate)

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# Add raw whale isotope data
# See figure 1 code for more detail
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# read in data and extract just the blue whale
whale_isos <- read_csv("data/raw-whale-isotope-data.csv")
KC7 <- filter(whale_isos, whale_ID == "KC7")
growth_rate <- 13.5  # baleen growth rate in centimetres per year
last_sample <- ymd("1891-03-01") # final date of last keratin sample

KC7 <- 
  KC7 %>%
  mutate(days = sample_cm * 365.25 / growth_rate) %>%
  mutate(days = days - days[1]) %>%
  mutate(rev_days = days - days[length(days)]) %>%
  mutate(sample_date = last_sample + rev_days) %>%
  mutate(year = year(sample_date))

# Create a variable to label the years
year_labels <- with(KC7, seq(min(year), max(year), by = 1))
year_breaks <- which(duplicated(KC7$year) == FALSE)
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# plot the isotope data
point_size <- 4

isop <- ggplot(KC7, aes(x = rev(sample_cm))) + 
  theme_classic(base_size = 16) + 
  xlab("Baleen sample (cm from youngest sample)") + 
  scale_x_continuous(breaks = seq(0, 100, 20), 
                     labels = seq(100, 0, -20),
                     sec.axis = sec_axis(~., 
                                         breaks = rev(year_breaks),
                                         labels = rev(year_labels),
                                         name = "Year"))

isop <- isop + geom_line(aes(y = d13C)) + 
  geom_point(aes(y = d13C), size = point_size, shape = 21, fill = "black") +
  ylab(expression(paste(delta^{13}, "C (\u2030)")))

isop <- isop + geom_vline(xintercept = rev(year_breaks),
                          color = "grey", lty = 2)

print(isop)

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# Figure 3 - Map plot of all simulated tracks for the full migratory model
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

resTrack <- read_csv("data/Model.sims.csv")

resTrack <- 
  resTrack %>%
  # remove rep that didn't wrok
  slice(-c(3213051:3213211)) %>%
  # Add dates. Last date is end of time series, count2 goes from 1 -3019, where
  # first day is the last sample in time, so 3019 - count2 gives it the correct 
  # way round.
  mutate(sample_date = last_sample - (3019 - count2)) %>%
  mutate(year = year(sample_date))

# set the constant to add as a correction to the d13C data
cnst <- 6


ggplot(resTrack[1:10000, ], aes(x = sample_date, y = d13C, group = Rep)) + geom_line()

### WORKS UP TO THIS POINT

# add a vector to enable different alphas for each simulated rep
tmp$best <- tmp$Rep == -1 # -1 means they are all zero

# plot the daily tracks for simulation days >= 101 only
track_plot <- ggplot(filter(tmp, days >= 101), aes(days, lo, group = Rep, 
                                                   alpha = best )) + 
  scale_alpha_discrete(range = c(0.25, 0.9), guide = FALSE)

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



track_plot <- track_plot  + geom_line(col = viridis(3)[2]) + 
  ylab(expression(paste(delta^{13}, "C (\u2030)"))) + 
  theme_classic(base_size = 14) + 
  scale_x_continuous(name = "Time (years)", breaks = new_years_sim_day, 
                     labels = new_years)
  

# superimpose the raw d13C data
# tdf <- 3.5 # a y-scale shift, not currently used.

x_shift <- max(tmp$days) - max(KC7$days)

y_scale <- 3

KC7$d13C_scaled <- with(KC7, ((d13C - mean(d13C)) * y_scale) + mean(d13C)) 

track_plot <- track_plot + geom_line(data = KC7, 
                                     mapping = aes(x = days + x_shift, 
                                                   y = d13C_scaled, 
                                                   group = NULL,
                                                   alpha = NULL), 
                                     alpha = 1,
                                     col = viridis::viridis(3)[1],
                                     size = 1)

track_plot <- track_plot + geom_point(data = KC7, 
                                      mapping = aes(x = days + x_shift, 
                                                   y = d13C_scaled, 
                                                   group = NULL,
                                                   alpha = NULL),
                                      alpha = 1,
                                      col = viridis::viridis(6)[1],
                                      fill = viridis::viridis(6)[2],
                                      size = point_size,
                                      shape = 21)

# track_plot <- track_plot + scale_x_continuous(,
#                                               sec.axis = sec_axis(~.))


print(track_plot)

ggsave("manuscript/figures/Figure-3-migratory-model-d13C.png", track_plot, 
       device = png(width = 600, height = 400))


# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# ***********************************************************
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

# number of correlation coefficients greater than 0
count_cor <-sum(corEmpSim$cor > 0)

# number of p-values less than 0.05
p_crit <- sum(corEmpSim$p <= 0.05 / 50)

# which simulation Rep has the highest correlation coefficient?
best_sim <- which.max(corEmpSim$cor)






