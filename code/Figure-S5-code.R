# Author: Clive Trueman
# About: script to plot max and sd of latitude 
# for the top 10% and bottom 10% of models
# Modified by Natalie Cooper Nov 2017

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# Load libraries
library(tidyverse)
library(gridExtra)

# Load data
max <- read.csv("data/max.lat.csv")
sdev <- read.csv("data/sd.lat.csv")

# Relevel so top 10% is first
max$group <-relevel(max$group, ref = "top")
sdev$group <-relevel(sdev$group, ref = "top")

# Get means
mean.max <- summarise(group_by(max, group), mn = mean(V1))
mean.sd <- summarise(group_by(sdev, group), mn = mean(V1))
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# Plot

p1 <- 
  ggplot(max, aes(x = group, y = V1, fill = group)) +
  geom_boxplot() +  
  theme_classic(base_size = 16) + 
  xlab("") +
  ylab("Maximum latitude") +
  scale_x_discrete(labels = c("Top 10%", "Bottom 10%")) +
  scale_y_continuous(breaks = c(20, 40, 60, 80, 100),
                     labels = c(20, 40, 60, 80, 100)) +
  scale_fill_manual(values = c("white", "gray")) +
  theme(legend.position = "none")

p2 <- 
  ggplot(sdev, aes(x = group, y = V1, fill = group)) +
  geom_boxplot() +  
  theme_classic(base_size = 16) + 
  xlab("") +
  ylab("Standard deviation of latitude") +
  scale_x_discrete(labels = c("Top 10%", "Bottom 10%")) +
  scale_y_continuous(breaks = c(10, 20, 30, 40, 50),
                     labels = c(10, 20, 30, 40, 50)) +
  scale_fill_manual(values = c("white", "gray")) +
  theme(legend.position = "none")

boxes <- grid.arrange(p1, p2, ncol = 2)

ggsave("manuscript/revision/figures/Figure-S5-boxplots.png", boxes, 
       device = png(width = 600, height = 400))
