# Author: Clive Trueman
# Adapted by: Andrew Jackson 19-Jun-2017
# About: script to generate figures S4a-d for the blue whale paper


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

# set the constant to add as a correction to the d13C data
cnst <- 6


##########read resident whale model files

resNor    <- read.csv("data/Norway.csv", header = TRUE)
resIre    <- read.csv("data/west.Ireland.csv", header = TRUE)
resCan    <- read.csv("data/Canaries.csv", header = TRUE)
resMidAtl <- read.csv("data/Mid_Atl.csv", header = TRUE)




#plot loess fits to the 30 resident simulated whales for each region


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
  theme_classic(base_size = 14) + ylim(-25, -14)

print(nor_plot)

# ggsave("figures/Figure-S4a-norway-resident-model-d13C.png", nor_plot, 
#        device = png(width = 600, height = 400))


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
  theme_classic(base_size = 14) + ylim(-25, -14)

print(ire_plot)

# ggsave("figures/Figure-S4b-ireland-resident-model-d13C.png", ire_plot,
#        device = png(width = 600, height = 400))



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
  theme_classic(base_size = 14) + ylim(-25, -14)

print(can_plot)

# ggsave("figures/Figure-S4c-canada-resident-model-d13C.png", can_plot, 
#        device = png(width = 600, height = 400))



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
  theme_classic(base_size = 14) + ylim(-25, -14)

print(MidAtl_plot)

# ggsave("figures/Figure-S4d-mid-atlantic-resident-model-d13C.png", MidAtl_plot, 
#        device = png(width = 600, height = 400))

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

# I started doing this with a custom function multiplot, but 
# swapped to ggplot2::facet_wrap below

# create a multiplot using code from 
# http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
# multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL, 
#                       fsave = FALSE, fn = NULL, width = NULL, height = NULL) {
#   library(grid)
#   
#   # Make a list from the ... arguments and plotlist
#   plots <- c(list(...), plotlist)
#   
#   numPlots = length(plots)
#   
#   # If layout is NULL, then use 'cols' to determine layout
#   if (is.null(layout)) {
#     # Make the panel
#     # ncol: Number of columns of plots
#     # nrow: Number of rows needed, calculated from # of cols
#     layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
#                      ncol = cols, nrow = ceiling(numPlots/cols))
#   }
#   
#   if (numPlots==1) {
#     print(plots[[1]])
#     
#   } else {
#     # Set up the page
#     grid.newpage()
#     pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
#     
#     # Make each plot, in the correct location
#     for (i in 1:numPlots) {
#       # Get the i,j matrix positions of the regions that contain this subplot
#       matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
#       
#       print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
#                                       layout.pos.col = matchidx$col))
#     }
#   }
#   
#   if (fsave) png(filename = fn, width = width, height = height)
# }
# 
# # = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# 
# nor_plot2 <- nor_plot + ggtitle("Norway")
# ire_plot2 <- ire_plot + ggtitle("Ireland")
# can_plot2 <- can_plot + ggtitle("Canaries")
# MidAtl_plot2 <- MidAtl_plot + ggtitle("Mid Atlantic")
# 
# all_models_plot <- multiplot(nor_plot2, ire_plot2, can_plot2, MidAtl_plot2, 
#                              cols = 2, fsave = TRUE, 
#                              fn = "figures/Figure-S4-all.png",
#                              width = 600, height = 400)

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# do the same but with facets

res_all <- bind_rows(list(Norway = resNor, 
                          Ireland = resIre, 
                          Canaries = resCan, 
                          "Mid-Atlantic" = resMidAtl), .id = "Region")


# same as for norway example above but we are going to plot directly from the 
# tmp object as the order gets changed due to grouping by Region.
tmp <- res_all %>% group_by(Region, Rep)  %>% 
  do(., data.frame(Z = predict(loess((.$d13C + cnst) ~ .$Day.No, span = 0.1)),
                   Day.No = .$Day.No))

# re-order the Region Factor
tmp$Region <- factor(tmp$Region, levels = c("Ireland","Norway",
                                            "Canaries","Mid-Atlantic"))

# res_all$lo <- tmp$Z

sim_facet <- ggplot(tmp, aes(Day.No, Z, group = Rep)) + 
  geom_line(col = viridis(3)[2], alpha = 0.25) + 
  xlab("Time") + 
  ylab(expression(paste(delta^{13}, "C (\u2030)"))) + 
  theme_classic(base_size = 14) + ylim(-25, -14) + 
  theme(strip.background = element_blank(),
        strip.text = element_text(size = NULL))

sim_facet <- sim_facet + facet_wrap(~Region, ncol = 2)

print(sim_facet)

ggsave("manuscript/figures/Figure-S4-facet-wrap-d13C.png", sim_facet, 
       device = png(width = 600, height = 400))

