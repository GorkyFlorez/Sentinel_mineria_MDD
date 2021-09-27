library(sf)
library(spData)
library(ggplot2)
library(cowplot)
library(rcartocolor)
library(raster)
library(RStoolbox)
library(landsat8)
library(ggspatial)

band4 <- raster("Raster/2021/2021 sentinel/GRANULE/L1C_T19LDG_A022902_20210725T145618/IMG_DATA/T19LDG_20210725T144729_B04.jp2")
band3 <- raster("Raster/2021/2021 sentinel/GRANULE/L1C_T19LDG_A022902_20210725T145618/IMG_DATA/T19LDG_20210725T144729_B03.jp2")
band2 <- raster("Raster/2021/2021 sentinel/GRANULE/L1C_T19LDG_A022902_20210725T145618/IMG_DATA/T19LDG_20210725T144729_B02.jp2")

band4_6 <- raster("Raster/2016/2016/GRANULE/L1C_T19LDG_A007758_20161216T150024/IMG_DATA/T19LDG_20161216T145722_B04.jp2")
band3_6 <- raster("Raster/2016/2016/GRANULE/L1C_T19LDG_A007758_20161216T150024/IMG_DATA/T19LDG_20161216T145722_B03.jp2")
band2_6 <- raster("Raster/2016/2016/GRANULE/L1C_T19LDG_A007758_20161216T150024/IMG_DATA/T19LDG_20161216T145722_B02.jp2")
Poligono     <- shapefile("SHP/1.shp")
Poligono2    <- shapefile("SHP/2.shp")

Poligonox  <-spTransform(Poligono, CRS=crs(band4))
PoligonoxDataFrame <- Poligonox %>% fortify
Poligono2x  <-spTransform(Poligono2, CRS=crs(band4))
Poligono2xDataFrame <- Poligono2x  %>% fortify
# Combinancion de bandas agricultura
Sentinel_Natu = stack(band4, band3, band2)
Sentinel_2016 = stack(band4_6, band3_6, band2_6)
ventana= extent(426500,433000,8598000 ,8608000)
ventana1= extent(426000,433000,8599000,8608000)



g1=ggRGB(Sentinel_Natu, r=1,g=2,b=3, stretch="lin", ext = ventana)+
  geom_polygon(col = 'gold3',
               fill = NA,
               data = PoligonoxDataFrame,
               aes(x = long, y = lat, group = group))+
  geom_polygon(col = 'gold3',
               fill = NA,
               data = Poligono2x,
               aes(x = long, y = lat, group = group))+
  coord_sf(xlim = c(426500,433000), ylim = c(8598000 ,8608000),expand = FALSE)+
  annotation_north_arrow(location="tl",which_north="true",style=north_arrow_fancy_orienteering ())+
  ggspatial::annotation_scale(location = "bl",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book")+
  theme_bw()+
  theme(panel.grid.major = element_line(color = gray(.8),linetype = "dashed", size = 0.5),
        axis.text = element_text(colour = "black"),
        # plot.background = element_rect(colour = "gray",size = 2),
        axis.text.x  = element_text(face="bold", color="black", size=8),
        axis.text.y  = element_text(angle = 90,face="bold", color="black", size=8))+
  coord_equal()+
  geom_vline(xintercept = c(426500,427000,428000,429000,430000,431000,432000,433000), color = "gray50",linetype = "dashed", size = 0.05)+ 
  geom_hline(yintercept = c(8598000 ,8599000,8600000,8601000,8602000,8603000,8604000,8605000,8606000,8607000), color = "gray50",linetype = "dashed", size = 0.05)+
  scale_x_continuous(breaks = seq(426500,433000, by = 1000))+
  scale_y_continuous(breaks = seq(8598000 ,8608000, by = 1000))+
  annotate(geom = "text", x = 429500, y = 8607800, label = "Imagen Satelital Sentinel 2  - 2021 composicion de bandas 432", 
           family="candara", color = "gold3", size = 3,fontface = "bold")+
  annotate(geom = "text", x = 427500, y = 8603000, label = "YENY \nVALDIVIA AGUILERA", 
           family="candara", color = "gold3", size = 3,fontface = "bold")+
  annotate(geom = "text", x = 431500, y = 8600000, label = "EDILBERTO ANTONIO \nAGUIRRE ALFARO", 
           family="candara", color = "gold3", size = 3,fontface = "bold")+
  labs(x = NULL, y = NULL)
 
 

g2=ggRGB(Sentinel_2016, r=1,g=2,b=3, stretch="lin", ext = ventana1)+
  geom_polygon(col = 'gold3',
               fill = NA,
               data = PoligonoxDataFrame,
               aes(x = long, y = lat, group = group))+
  geom_polygon(col = 'gold3',
               fill = NA,
               data = Poligono2x,
               aes(x = long, y = lat, group = group))+
  #coord_sf(xlim = c(425000 ,430000), ylim = c(8599000,8606000))+
  theme_bw()+
  annotation_north_arrow(location="tl",which_north="true",style=north_arrow_fancy_orienteering ())+
  ggspatial::annotation_scale(location = "bl",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book")+
  theme(panel.grid.major = element_line(color = gray(.8),linetype = "dashed", size = 0.5),
        axis.text = element_text(colour = "black"),
        # plot.background = element_rect(colour = "gray",size = 2),
        axis.text.x  = element_text(face="bold", color="black", size=8),
        axis.text.y  = element_text(angle = 90,face="bold", color="black", size=8))+
  geom_vline(xintercept = c(426000,427000,428000,429000,430000,430500), color = "gray50",linetype = "dashed", size = 0.05)+ 
  geom_hline(yintercept = c(8599000,8600000,8601000,8602000,8603000,8604000,8605000,8606000,8607000,8608000), color = "gray50",linetype = "dashed", size = 0.05)+
  scale_x_continuous(breaks = seq(426500,430000, by = 1000))+
  scale_y_continuous(breaks = seq(8599000 ,8608000, by = 1000))+
  annotate(geom = "text", x = 429500, y = 8607800, label = "Imagen Satelital Sentinel 2  - 2021 composicion de bandas 432", 
           family="candara", color = "gold3", size = 3,fontface = "bold")+
  annotate(geom = "text", x = 427500, y = 8603000, label = "YENY \nVALDIVIA AGUILERA", 
           family="candara", color = "gold3", size = 3,fontface = "bold")+
  annotate(geom = "text", x = 429000, y = 8600000, label = "EDILBERTO ANTONIO \nAGUIRRE ALFARO", 
           family="candara", color = "gold3", size = 3,fontface = "bold")+
  labs(x = NULL, y = NULL)
  

library(ggpubr)
gg_inset_map1=ggarrange(g1,g2, labels = c("A)","B)"),ncol = 2)

ggsave(plot = g1,"MAPAS/MDD_Satelital3.png",
       units = "cm", width = 21,height = 29, dpi = 900)# guardar grafico
