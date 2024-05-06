#SOURCE: https://github.com/jjakon11/30daysMapChallenge_2023/blob/main/Day30_MyFavorite/Day30_MyFavorite.Rmd
#Adapted by Antonello Salis 
#20240506

library(ggplot2)
library(raster)
library(ggridges)# install.packages("ggridges")
library(dplyr)
library(sf)
library(terra)
library(ggpubr)
library(sysfonts)
library(showtext)
font_add_google(name = 'Vollkorn SC', family = 'Vollkorn SC')
showtext_auto()

Sardegna_DEM <- raster("/home/antonello/Cartografia/SARDEGNA/DEM_TIF_Merged.tif" )
land <- aggregate(Sardegna_DEM, fact=150)
Land_Eledt <- rasterToPoints(land)
Land_Eledt2 <- as.data.frame(Land_Eledt) %>%   rename(elevation='DEM_TIF_Merged') 

summary(Land_Eledt2)

JoyP <- ggplot() +
  geom_hline(data=Land_Eledt2, 
             aes(yintercept=y+0.0005),linewidth = 0.4, 
             color="grey50")+
  geom_density_ridges(data = Land_Eledt2, 
                      aes(x = x, y = y, group = y, height = elevation),
                      stat = 'identity',
                      fill="#ffffff",
                      color="#ffffff", 
                      scale = 6,size = 0.5, rel_min_height = 0.003, 
                      alpha=0.1)+
   geom_text(mapping=aes(x=9, y=38.8, 
                         label="Sardegna:Unknown Pleasures"), 
             size=75,color="#e0e0e0" ,family="Vollkorn SC")+
   geom_text(mapping=aes(x=8.5, y=38.7, 
                         label="@ASalis"), 
             size=22,color="#e0e0e0")+
  coord_sf(xlim = c(8, 10), ylim = c(38.5, 41.2), crs=4326)+
  theme(panel.background = element_rect(fill = "#1a1a1a", color="#1a1a1a"), 
        plot.background = element_rect(fill = "#1a1a1a"),
        panel.grid = element_blank(), 
        plot.margin = margin(0, 0,-2.1,-1, "cm"),
        axis.text.x = element_text(vjust = 26, size=60, color="#e0e0e0"),
        axis.text.y = element_text(angle = 359, hjust=-2.0, vjust=-0.7, size=60, color="#e0e0e0"),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        legend.position="none")

ggexport(JoyP, filename = "/home/antonello/Sardegna.jpg", width=5300, height = 8000, res=500)
