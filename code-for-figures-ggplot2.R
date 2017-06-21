# Author: Clive Trueman
# Adapted by: Andrew Jackson 19-Jun-2017
# About: script to generate figures for the blue whale paper


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

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
rm(list = ls())
graphics.off()


##plot model output annual average baseline images
Atl_carbon   <- raster("data/Atl_Annual_d13C.grd")
Atl_nitrogen <- raster("data/Atl_Annual_d15N.grd")

image(Atl_carbon)
image(Atl_nitrogen)

# set the constant to add as a correction to the d13C data
cnst <- 6


##########read resident whale model files

resNor    <- read.csv("data/Norway.csv", header = TRUE)
resIre    <- read.csv("data/west.Ireland.csv", header = TRUE)
resCan    <- read.csv("data/Canaries.csv", header = TRUE)
resMidAtl <- read.csv("data/Mid_Atl.csv", header = TRUE)




#plot loess fits to the 30 resident simulated whales for each region



# setEPS()
# postscript("resident.eps", width=10, height=5)

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
#norway resident

# use piping to break up the data.frame by replicate, then 
# predict the loess
tmp <- resNor %>% group_by(Rep) %>% 
  do(., data.frame(Z = predict(loess((.$d13C + cnst) ~ .$Day.No, span = 0.1))))

# append this vector onto the original data.frame
resNor$lo <- tmp$Z
  
# plot via ggplot
nor_plot <- ggplot(resNor, aes(Day.No, lo, group = Rep)) + 
  geom_line(col = viridis(3)[2], alpha = 0.25) + 
  xlab("Time") + 
  ylab(expression(paste(delta^{13}, "C (\u2030)"))) + 
  theme_classic(base_size = 14)
print(nor_plot)

ggsave("figures/norway-resident-model-d13C.png", nor_plot, 
       device = png(width = 600, height = 400))



# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

# Using geom_smooth doesnt really improve the visualisation, 
# and certainly not for the full migration model tracks below.
# test_plot <- ggplot(resNor, aes(Day.No, d13C, group = Rep)) + 
#   geom_smooth(span = 0.1) + 
#   theme_classic(base_size = 14)
# test_plot


# A hexbin version that is pretty fugly
# nor_plot2 <- ggplot(resNor, aes(Day.No, lo, group = Rep)) + 
#   geom_hex() + 
#   xlab("Time") + 
#   ylab(expression(paste(delta^{13}, "C (\u2030)"))) + 
#   theme_classic()
# print(nor_plot2)



# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
#Ireland resident

# same as for norway example above
tmp <- resIre %>% group_by(Rep) %>% 
  do(., data.frame(Z = predict(loess((.$d13C + cnst) ~ .$Day.No, span = 0.1))))

resIre$lo <- tmp$Z

ire_plot <- ggplot(resIre, aes(Day.No, lo, group = Rep)) + 
  geom_line(col = viridis(3)[2], alpha = 0.25) + 
  xlab("Time") + 
  ylab(expression(paste(delta^{13}, "C (\u2030)"))) + 
  theme_classic(base_size = 14)
print(ire_plot)

ggsave("figures/ireland-resident-model-d13C.png", ire_plot, 
       device = png(width = 600, height = 400))



# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
#Canary resident

# NB there are some (324) NAs in this dataset that need to be removed.

resCan <- filter(resCan, !is.na(d13C))

# same as for norway example above
tmp <- resCan %>% group_by(Rep) %>% 
  do(., data.frame(Z = predict(loess((.$d13C + cnst) ~ .$Day.No, span = 0.1))))

resCan$lo <- tmp$Z

can_plot <- ggplot(resCan, aes(Day.No, lo, group = Rep)) + 
  geom_line(col = viridis(3)[2], alpha = 0.25) + 
  xlab("Time") + 
  ylab(expression(paste(delta^{13}, "C (\u2030)"))) + 
  theme_classic(base_size = 14)
print(can_plot)

ggsave("figures/canada-resident-model-d13C.png", can_plot, 
       device = png(width = 600, height = 400))



# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
#Mid Atl resident

# same as for norway example above
tmp <- resMidAtl %>% group_by(Rep) %>% 
  do(., data.frame(Z = predict(loess((.$d13C + cnst) ~ .$Day.No, span = 0.1))))

resMidAtl$lo <- tmp$Z

MidAtl_plot <- ggplot(resMidAtl, aes(Day.No, lo, group = Rep)) + 
  geom_line(col = viridis(3)[2], alpha = 0.25) + 
  xlab("Time") + 
  ylab(expression(paste(delta^{13}, "C (\u2030)"))) + 
  theme_classic(base_size = 14)
print(MidAtl_plot)

ggsave("figures/mid-atlantic-resident-model-d13C.png", MidAtl_plot, 
       device = png(width = 600, height = 400))

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

#######PLOT out the full whale simulation

resTrack <- read.csv("data/record_migrate5.csv", header=TRUE)

# remove NAs from the dataset
# resTrack <- filter(resTrack, !is.na(d13C))

# create a new sequence of days on which to predict our
# loess smoothed function
days <- seq(from=1, to=(6*365+60))

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

# plot the daily tracks
track_plot <- ggplot(tmp, aes(days, lo, group = Rep)) + 
  geom_line(col = viridis(3)[2], alpha = 0.25) + 
  xlab("Time") + 
  ylab(expression(paste(delta^{13}, "C (\u2030)"))) + 
  theme_classic(base_size = 14)
print(track_plot)

ggsave("figures/migratory-model-d13C.png", track_plot, 
       device = png(width = 600, height = 400))

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# ggplot maps

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

# a custom palette
# vv <- viridis::viridis(4)
# mypalette <- c(rep(vv[4],1),
#                rep(vv[1], 3),
#                rep(vv[2], 3),
#                rep(vv[3], 3),
#                rep(vv[4], 2)
#                )

# prep mp object
mp <- NULL

# create a layer of borders
mapWorld <- borders("world",
                    colour="gray50", fill="gray50") 


# set up the plot
mp <- ggplot(resTrack, aes(Lon, Lat, group = Rep)) 

# add the world map
mp <- mp + mapWorld

# remove axes and ticks and labels
mp <- mp + theme(axis.line  = element_blank(),
                 axis.text  = element_blank(),
                 axis.ticks = element_blank(),
                 axis.title = element_blank())

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

print(mp2)

ggsave("figures/migratory-model-full-map.png", mp2, 
       device = png(width = 600, height = 600, units = "px"))

# now a line plot (looks awful)
# mp3 <- mp + geom_line(aes(x=jLon, y=jLat, group = Rep), 
#                         color = colzF[resTrack$Month],
#                         alpha = 0.1) 
# 
# print(mp3)

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

# plot out the best fit movement model

# select the rep
do.this.rep <- 43


# take resTrack and group it by replicate, then apply our 
# tidy_loess() function to generate estimated d13C for 
# each of our days
best_rep_lo <- resTrack %>% filter(Rep == do.this.rep) %>% 
  do(., tidy_loess(., span = 0.05))

# plot the daily tracks
best_rep_plot <- ggplot(best_rep_lo, aes(days, lo)) + 
  geom_line(col = viridis(3)[2], alpha = 1) + 
  xlab("Time") + 
  ylab(expression(paste(delta^{13}, "C (\u2030)"))) + 
  theme_classic(base_size = 14)
print(best_rep_plot)

fn <- paste0("figures/migratory-model-rep-", do.this.rep,"-d13C.png")

ggsave(fn, best_rep_plot, device = png(width = 600, height = 400))


# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

# plot this best fit model on the map




mp_best <- ggplot(filter(resTrack, Rep == do.this.rep), 
                  aes(jLon, jLat, group = Rep))

# add the world map
mp_best <- mp_best + mapWorld

# apply classic theme
mp_best <- mp_best + theme_classic(base_size = 14)

# remove axes
mp_best <- mp_best + theme(axis.line  = element_blank(),
                           axis.text  = element_blank(),
                           axis.ticks = element_blank(),
                           axis.title = element_blank())

# zoom in to Atlantic region
mp_best <- mp_best + coord_fixed(xlim = c(-80, 50),  
            ylim = c(0, 80), 
            ratio = 1.3)

mp_best2 <- mp_best + geom_point(aes(color = factor(season)), 
                                  size = 0.5) + 
  scale_color_viridis(discrete = TRUE, "Quarter") + 
  guides(colour = guide_legend(override.aes = list(size=5)))

print(mp_best2)

fn <- paste0("figures/migratory-model-rep-", do.this.rep,"map.png")

ggsave(fn, mp_best2, device = png(width = 600, height = 600, units = "px"))

# ggsave(
#   "figures/ggtest.png",
#   mp_best2,
#   width = 3.25,
#   height = 3.25,
#   dpi = 1200
# )

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =


# This attempt is not working - AJ 20/06/17

# try a line plot for this single track which might look decent

# line needed to get the xpsline function to work
# plot.new()
# 
# 
# aj <- resTrack %>% filter(Rep == do.this.rep)
# 
# bb <- with(aj, smooth.spline(Lon, Lat, nknots = 30))
# 
# cc <- data.frame(Lon = bb$x, Lat = bb$y)
# 
# # spline_track <- resTrack %>% filter(Rep == do.this.rep) %>% 
# #   do(., data.frame(smooth.spline(.$Lon, .$Lat)))
# 
# 
# mp_spline <- ggplot(aj, aes(Lon, Lat)) +
#   geom_path() + 
#   geom_path(data=cc, mapping = aes(Lon, Lat, col = "red"))
# mp_spline










