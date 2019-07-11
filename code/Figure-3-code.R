# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# Code for Figure 3
# Adapted from code by Clive Trueman by Natalie Cooper Nov 2017
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# Load libraries
library(tidyverse)
library(gtable)
library(grid)

# Read in the measured whale data and select blue whale
whale_isos <- read_csv("data/raw-whale-isotope-data.csv")
blue <- filter(whale_isos, whale_ID == "KC7")

# Read in top 100 models
top100 <- read.csv("data/top10smooth.csv")

# Fix day numbers to match in both
blue$Day.sim <- rev(unique(top100$count2))

# Create three plots, one for blue whale and one for simulations, and 
# a base plot with axes etc.
base_plot <- 
  ggplot(blue, aes(x = Day.sim, y = d13C)) +
  #geom_line(size = 1.5) +
  theme_classic(base_size = 16) + 
  xlab("Days") + 
  ylab(expression(paste(delta^{13}, "C (\u2030)"))) +
  scale_y_continuous(limits = c(-20, -16.5),
                     sec.axis = sec_axis(~.+4, 
                                breaks = c(-16, -15, -14, -13),
                                labels = c(-30, -25, -20, -15),
                                name = expression(paste("simulated ", 
                                                        delta^{13}, "C (\u2030)")))) +
  theme(panel.background = element_blank())

top_plot <- 
  ggplot(top100, aes(x = count2, y = d13C+2, group = Rep)) +
  geom_line(alpha = 0.2, col = "grey") +
  theme_classic(base_size = 16) + 
  xlab("") + 
  ylab("") +
  scale_y_continuous(limits = c(-30, -12.5))  +
  theme(panel.background = element_blank())

blue_plot <- 
  ggplot(blue, aes(x = Day.sim, y = d13C)) +
  geom_line(size = 1.5) +
  theme_classic(base_size = 16) + 
  xlab("Days") + 
  ylab(expression(paste(delta^{13}, "C (\u2030)"))) +
  scale_y_continuous(limits = c(-20, -16.5),
                     sec.axis = sec_axis(~.+4, 
                                         breaks = c(-16, -15, -14, -13),
                                         labels = c(-30, -25, -20, -15),
                                         name = expression(paste("simulated ", 
                                                                 delta^{13}, "C (\u2030)")))) +
  theme(panel.background = element_blank())

# Combine plots
# Create grobs
g1 <- ggplotGrob(base_plot)
g2 <- ggplotGrob(top_plot)
g3 <- ggplotGrob(blue_plot)

# Get the locations of the plot panels in g1.
pp <- c(subset(g1$layout, name == "panel", se = t:r))

# Overlap panel for second plot on that of the first plot
gall <- gtable_add_grob(g1, list(g2$grobs[[which(g2$layout$name == "panel")]], 
                                 g3$grobs[[which(g3$layout$name == "panel")]]), 
                        t = pp$t, l = pp$l, b = pp$b, r = pp$l,
                        name = 1:2)
plot(gall)

ggsave("manuscript/PeerJ/figures/Figure-3-blue-sims.png", gall, 
       device = png(width = 600, height = 400))
