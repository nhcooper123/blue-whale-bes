# Author: Clive Trueman
# Adapted by: Andrew Jackson June 2017 & Natalie Cooper Oct 2017.
# About: Script to generate figure 1

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# Load libraries
library(tidyverse)
library(lubridate)

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# Figure 1 - raw whale isotope data with two y-axes
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# read in data and extract just the blue whale
whale_isos <- read_csv("data/raw-whale-isotope-data.csv")

KC7 <- filter(whale_isos, whale_ID == "KC7")

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# convert sample number to days based on growth rate of baleen
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

# add the d15N data but need to scale it to d13C
v_shift <- mean(KC7$d15N) - mean(KC7$d13C) 

isop <- isop + geom_line(aes(y = d15N - v_shift)) + 
  geom_point(aes(y = (d15N - v_shift)), size = point_size, 
             shape = 21, fill = "grey")

# add the second axis and rescale it appropriately
isop <- isop + 
  scale_y_continuous(
    sec.axis = sec_axis(~.+v_shift, 
                        name = expression(paste(delta^{15}, "N (\u2030)"))))

isop <- isop + geom_vline(xintercept = rev(year_breaks),
                          color = "grey", lty = 2)

print(isop)

ggsave("manuscript/revision/figures/Figure-1-raw-dC-dN-data.png", isop, 
       device = png(width = 600, height = 400))
