# Author: Natalie Cooper
# About: script to plot histogram of r2 values

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# Load libraries
library(tidyverse)

# Load data
r2 <- read.csv("data/all.r2.csv")

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# Plot

p1 <- 
  ggplot(r2, aes(x = x)) +
  geom_histogram(fill = "grey", binwidth = 0.05) +  
  theme_classic(base_size = 16) + 
  xlab(expression(paste(r^2))) +
  #xlab(expression(paste(r^2, " from models comparing real to simulated ", 
  #                delta^{13}, "C (\u2030)", " values"))) +
  expand_limits(x = c(0, 1),  y = c(0, 200))


ggsave("manuscript/revision/figures/Figure-S6-r2.png", p1, 
       device = png(width = 600, height = 400))
