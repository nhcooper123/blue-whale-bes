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
#library(viridis)

library(tidyverse)
library(lubridate)

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# Figure 2 - Map plot of all simulated tracks for the full migratory model
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

resTrack <- read_csv("data/Model.sims.csv")
res <- resTrack

# cut out last rep if it didn't work
resTrack <- slice(res, -c(3213051:3213211))



res <- filter(resTrack, Rep == 1)

















x <- 
  res %>%
  group_by(Rep) %>%
  summarise(max = max(count2)) %>%
  #mutate(days = 1:max)
  map(mutate, days = 1:max)

remove <- x$Rep[x$max < 3019]

resTrack <- slice(resTrack, -remove)



# remove NAs from the dataset
# resTrack <- filter(resTrack, !is.na(d13C))

# create a new sequence of days on which to predict our
# loess smoothed function and add it to the resTrack data.frame for 
# each Rep
days <- rep(seq(from = 1, to = 3019), length(resTrack$Rep))
# resTrack$days 
resTrack3 <- 
  resTrack %>% 
  group_by(Rep) %>% 
  data.frame(days)


# jitter the points
resTrack$jLat <- jitter(resTrack$Lat, factor = 2)
resTrack$jLon <- jitter(resTrack$Lon, factor = 1)

# data required to add a vertical line indicating the start of migration
migrate <- as.Date("1889-06-01")
first_sim_day <- as.Date("1885-01-01")
migrate_day <- as.numeric(migrate - first_sim_day)

resTrack$migrant <- as.numeric(resTrack$days >= migrate_day)

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


cust.colors <- c(brewer.pal(5, "Blues")[2:5], brewer.pal(5, "Oranges")[2:5])

# add to resTrack df
resTrack$season <- four.seasons[resTrack$Month]

# prep mp object
mp <- NULL

# create a layer of borders
mapWorld <- borders("world",
                    colour="gray50", fill="gray50") 


# set up the plot for simulation days >= 101
mp <- ggplot(filter(resTrack, days >= 101), aes(Lon, Lat, group = Rep)) 

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
                           color = days), 
                       size=0.1) + 
  scale_color_viridis(discrete = FALSE, "Day")
  # scale_color_manual(values = cust.colors) + 
  # guides(colour = guide_legend(override.aes = list(size=5)))

# remove axes and ticks and labels
mp2 <- mp2 + theme(axis.line  = element_blank(),
                 axis.text  = element_blank(),
                 axis.ticks = element_blank(),
                 axis.title = element_blank())

# add geographical locations
wexford <- data.frame(x = c(-6.3, 10), y = c(52.25, 51))

azores <- data.frame(x = c(-25.66, -35), y = c(37.75, 25.5))

mp3 <- mp2 + geom_line(data = wexford, 
                        mapping = aes(x,y, group = NULL), col = "#f03b20",
                       arrow = arrow(ends = "first", type = "closed", 
                                     angle = 20, 
                                     length = unit(0.15, "inches"))) + 
  annotate("text", x = 10, y = 51.5, label = "Wexford", 
           hjust = 0, col = "white", size = 6)

mp4 <- mp3 + geom_line(data = azores, 
                       mapping = aes(x,y, group = NULL), col = "#f03b20",
                       arrow = arrow(ends = "last", type = "closed", 
                                     angle = 20, 
                                     length = unit(0.15, "inches"))) + 
  annotate("text", x = -35, y = 24, label = "Azores", 
           hjust = 0.5, col = "black", size = 6)

print(mp4)

ggsave("manuscript/figures/Figure-2-migratory-model-full-map.png", mp4, 
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

# add the simulated d13C data

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






