library(sf)
library(spData)
library(ggplot2)
library(cowplot)
library(rcartocolor)
library(raster)
library(RStoolbox)
library(landsat8)
library(ggspatial)
Peru_d       <- getData('GADM', country='Peru', level=3) %>% st_as_sf()
Dis_MDD      <- st_read ("SHP/Madre_dios_dis.shp")
Dis_MD       <- st_transform(Dis_MDD,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))
MDD          <- subset(Peru_d , NAME_1  == "Madre de Dios")
MDD_l        <- cbind(Dis_MD, st_coordinates(st_centroid(Dis_MD$geometry)))
dis_MDD_d    <- st_read ("SHP/dis_MDD.shp")
dis_MDD      <- shapefile("SHP/Dis_MDD.shp")
boxx_MDD     <- shapefile("SHP/boxx_MDD.shp")

band6 <- raster("Raster/LC08_L1TP_003069_20200420_20200508_01_T1_B6.TIF")
band5 <- raster("Raster/LC08_L1TP_003069_20200420_20200508_01_T1_B5.TIF")
band4 <- raster("Raster/LC08_L1TP_003069_20200420_20200508_01_T1_B4.TIF")
# Combinancion de bandas agricultura
landsat8_Natu = stack(band6, band5, band4)
#cortar con la zona de estudio
paute17n  <- spTransform(dis_MDD , CRS=crs(band6))
bandas1   <- crop(landsat8_Natu , extent(paute17n))
bandas    <- mask(bandas1,paute17n)

boxx  <-spTransform(boxx_MDD, CRS=crs(band6))
paute17nDataFrame <- boxx  %>% fortify
ggRGB(bandas, r=1,g=2,b=3, stretch="lin")
	
inset =ggplot()+
  geom_sf(data = Dis_MD, fill = "white")+
  geom_sf(data = dis_MDD_d, fill = NA, color = "seagreen", size = 1) +
  geom_sf(data = dis_MDD_bxx, fill = NA, color = "orangered4", size = 0.8) +
  geom_label(data = MDD_l  , aes(x= X, y=Y, label =  DI_NOCAP), size = 1.5, color="black", fontface = "bold",fontfamily = "serif") +
  annotation_north_arrow(location="tl",which_north="true",style=north_arrow_fancy_orienteering ())+
  ggspatial::annotation_scale(location = "bl",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book")+
  theme_void()

main = ggRGB(bandas, r=1,g=2,b=3, stretch="lin")+
    geom_polygon(col = 'orangered4',
                 fill = NA,
                 data = paute17nDataFrame,
                 aes(x = long, y = lat, group = group))+
    theme_void()

gg_inset_map1 = cowplot::ggdraw() +
  coord_equal(xlim = c(0, 20), ylim = c(0, 20), expand = FALSE) +
  annotation_custom(ggplotGrob(inset), xmin = 0, xmax = 10, ymin = 10, ymax = 20) +
  annotation_custom(ggplotGrob(main), xmin = 0, xmax = 20, ymin = 0, ymax = 10) +
  geom_segment(aes(x = 6.3, xend = 14.5, y = 11, yend = 9.6), color = "orangered4", size = 1.2) +
  geom_segment(aes(x = 3.2, xend = 5.5, y = 10.9, yend = 9.6), color = "orangered4", size = 1.2)+
  theme(panel.background = element_rect(fill = "seashell2"))+
  annotate(geom = "text", x = 14, y = 19, label = "Combinación de bandas para Landsat 8 \nPara el Distrito de Madre de Dios", family="candara", 
            color = "seagreen", size = 4,fontface = "bold")+
  annotate(geom = "text", x = 14.5, y = 16, label = "Está ubicado al sureste del país, en la Amazonía, limitando al norte con Ucayali y \nBrasil, al este con Bolivia, al sur con Puno y al oeste con Cuzco. \nCon 85 300 km² es el tercer departamento más extenso —por detrás de Loreto y Ucayali \ny con 1,3 hab/km², el menos densamente poblado. Fue creado el 26 de diciembre de 1912 a partir de \nterritorios de Puno y Cuzco.Recibe su nombre del río Madre de Dios, de cuya cuenca son tributarios la mayor parte de \nlos ríos de la región y sobre cuyas riberas se erige la capital departamental: Puerto Maldonado", family="candara", 
           color = "seagreen", size =2,fontface = "bold")

ggsave(plot = gg_inset_map1,"MAPAS/MDD_Satelital.png",
       units = "cm", width = 29,height = 21, dpi = 900)# guardar grafico
