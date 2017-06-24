

library(raster)
library(RColorBrewer)


my_clrs <- brewer.pal(12, "YlOrRd")



Atl_carbon   <- raster("data/Atl_Annual_d13C.grd")
raster::image(Atl_carbon, xlab = "Long", ylab = "Lat", col = rev(my_clrs))




r <- Atl_carbon
# r <- reclassify(r, c(0,500,1, 500,2000,2))
val <- getValues(r)
xy <- as.data.frame(xyFromCell(r,1:ncell(r)))
xy <- cbind(xy,val)

p <- ggplot(na.omit(xy), aes(x=x, y=y, fill=val)) + 
  geom_raster() + 
  borders("world", colour="gray50", fill="gray50")  +
  coord_fixed(xlim = c(-80, 50),  
              ylim = c(0, 80), 
              ratio = 1.3) + 
  scale_fill_distiller(type = "seq", palette = "YlOrRd", direction = -1,
                       name = expression(paste(delta^{13}, "C (\u2030)"))) + 
  theme_classic(base_size = 14) + 
  theme(axis.line  = element_blank(),
        axis.text  = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank())

print(p)


ggsave("manuscript/figures/Figure-S3-plankton-d13C-map.png", p, 
       device = png(width = 600, height = 400))
