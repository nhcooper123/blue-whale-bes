# Author: Clive Trueman
# Adapted by: Andrew Jackson 19-Jun-2017
# About: script to generate figure 3
# Modified by NC for public engagement to omit legend and change colours

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
library(raster)
library(RColorBrewer)

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# d13Carbon
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

Atl_carbon   <- raster("data/Atl_Annual_d13C.grd")

r <- Atl_carbon
# r <- reclassify(r, c(0,500,1, 500,2000,2))
val <- getValues(r)
xy <- as.data.frame(xyFromCell(r,1:ncell(r)))
xy$val <- val

p <- ggplot(na.omit(xy), aes(x=x, y=y, fill=val)) + 
  geom_raster() + 
  borders("world", colour="gray50", fill="gray50")  +
  coord_fixed(xlim = c(-80, 50),  
              ylim = c(0, 80), 
              ratio = 1.3) + 
  scale_fill_distiller(type = "seq", palette = 16, direction = -1,
                       name = expression(paste(delta^{13}, "C (\u2030)")), guide = "none") + 
  theme_classic(base_size = 14) + 
  theme(axis.line  = element_blank(),
        axis.text  = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank())

print(p)

ggsave("outreach/Figure-3-migratory-model-d13C_outreach.png", track_plot, 
       device = png(width = 600, height = 400))
