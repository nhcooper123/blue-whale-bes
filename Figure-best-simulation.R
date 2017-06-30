rm(list = ls())


# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# Import and Process the empirical data
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

# Import Whale data
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
# Import and process the simulation data
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# ----

# which Rep will we plot
do_rep = 43

# Map plot of all simulated tracks for the full migratory model

resTrack <- read.csv("data/record_migrate5.csv", header=TRUE)

# remove NAs from the dataset
# resTrack <- filter(resTrack, !is.na(d13C))

# create a new sequence of days on which to predict our
# loess smoothed function and add it to the resTrack data.frame for 
# each Rep
days <- seq(from=1, to=(6*365+60))
# resTrack$days 
resTrack <- resTrack %>% filter(Rep == do_rep) %>% group_by(Rep) %>% data.frame(days)

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


# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# Plot Map of simulation data
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# prep mp object
mp <- NULL

# create a layer of borders
mapWorld <- borders("world",
                    colour="gray50", fill="gray50") 


# set up the plot for simulation days >= 101
mp <- ggplot(filter(resTrack, days >= 101), 
             aes(Lon, Lat, group = Rep)) 

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

ggsave("manuscript/figures/Figure-X-migratory-model-best-map.png", mp4, 
       device = png(width = 600, height = 600, units = "px"))


# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# Plot empirical and simulated isotope data
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# ----
# matching isotope data



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
tmp <- resTrack %>% filter(Rep == do_rep) %>% 
  do(., tidy_loess(., span = 0.05))


# plot the daily tracks for simulation days >= 101 only
point_size <- 4

track_plot <- ggplot(filter(tmp, days >= 101), aes(days, lo)) 

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
track_plot <- track_plot  + geom_line(col = viridis(3)[2], alpha = 1) + 
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

# track_plot <- track_plot + scale_x_continuous(,
#                                               sec.axis = sec_axis(~.))


print(track_plot)

ggsave("manuscript/figures/Figure-Y-migratory-model-best-sim-d13C.png", track_plot, 
       device = png(width = 600, height = 400))

