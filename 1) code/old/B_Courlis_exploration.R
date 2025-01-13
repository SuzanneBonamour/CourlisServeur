# package --------------------------------------------------

library(dplyr)
library(data.table)
library(ggplot2)
library(stringi)
library(terra)
library(tmap)
library(sf)
library(spData)
library(lubridate)
library(scales)
library(ncdf4)
library(CFtime)
library(raster)
library(tinter)

###
####
# DATA to load ----------------------------------------------
####
###

# dept ---
dept <- st_read("1) data/departements.gpkg",
  layer = "contourdesdepartements"
)
dept17 <- dept[dept$code == 17, ]
dept1785 <- dept[dept$code == 17 | 
                   dept$code == 85,]

# reserve ---
reserve <- st_read("1) data/rnn/rnn/N_ENP_RNN_S_000.shp")
RMO <- reserve[reserve$NOM_SITE=="Moëze-Oléron",]
rm(reserve)
RMO_buffer <- st_read("./2) data_generated/RMO_buffer.gpkg")

# gps ---
move_point <- st_read("C:/Users/Suzanne.Bonamour/OneDrive - LPO/2) Data/4) Courlis/GPS/1) Generated_gps/move_point.gpkg")
move_linestring <- st_read("C:/Users/Suzanne.Bonamour/OneDrive - LPO/2) Data/4) Courlis/GPS/1) Generated_gps/move_linestring.gpkg")

move_point_nb <- move_point %>%
   group_by(indID) %>%
   count() 

ind_3max <- move_point_nb$indID[move_point_nb$n > 17200]
move_linestring_3max <- move_linestring[move_linestring$indID %in% ind_3max,]

# bathymétrie ---
bathy2 <- raster("1) data/environnemental/Bathymétrie/MNT_FACADE_ATLANTIQUE_HOMONIM_NM/DONNEES/MNT_ATL100m_HOMONIM_WGS84_NM_ZNEG.grd")

# restriction bathy à la zone focale
crs(bathy2)
crs(RMO_buffer)
RMO_buffer <- st_transform(RMO_buffer, 4326)
bathy_zone <- intersect(bathy2, RMO_buffer)

# bathy_crop <- crop(bathy2, RMO_buffer)
# bathy_mask <- mask(bathy2, RMO_buffer)

# corine ---
corine <- rast("1) data/environnemental/CorineLandCover/U2018_CLC2018_V2020_20u1.tif")
crs(corine)
crs(RMO_buffer)
corine <- terra::project(corine, "epsg:4326")

# restriction corine à la zone focale
corine_crop <- crop(corine, RMO_buffer)
corine_mask <- mask(corine_crop, RMO_buffer) # pour garder que dans la zone focale
corine_zone <- mask(corine_mask, dept1785) # pour enlever la partie océan

# save
writeRaster(corine_zone, "C:/Users/Suzanne.Bonamour/OneDrive - LPO/2) Data/4) Courlis/GPS/1) Generated_gps/corine_zone.tif", overwrite=TRUE)

labels_corine <- as.character(c("Continuous urban", 
                                "Discontinuous urban",
                                "Industrial & commercial",
                                "Road & rail",
                                "Port",
                                "Airports",
                                "Mineral extraction",
                                "Dump", 
                                "Construction",
                                "Green urban",
                                "Sport and leisure facilities",
                                "Non-irrigated arable land", 
                                "Permanently irrigated land", 
                                "Rice fields",
                                "Vineyards",
                                "Fruit & berry trees",
                                "Olive groves",
                                "Pastures",
                                "Annual & permanent crops",
                                "Complex cultivation patterns",
                                "Agriculture with natural vegetation",
                                "Agro-forestry areas",
                                "Broad-leaved forest",
                                "Coniferous forest",
                                "Mixed forest",
                                "Natural grasslands",
                                "Moors and heathland",
                                "Sclerophyllous vegetation",
                                "Transitional woodland-shrub",
                                "Beaches, dunes, sands",
                                "Bare rocks",
                                "Sparsely vegetated areas",
                                "Burnt areas",
                                "Glaciers and perpetual snow",
                                "Inland marshes",
                                "Peat bogs",
                                "Salt marshes",
                                "Salines",
                                "Intertidal flats",
                                "Water courses",
                                "Water bodies",
                                "Coastal lagoons",
                                "Estuaries",
                                "Sea and ocean",
                                "NODATA"))

# zostere ---

###
####
# colors ---------------------------------------------------
####
###

color_1 <- colorRampPalette(c("#1165BA","#BF69D0","#FF8D0A","#11B5E4","#F4FF52"))(19)
show_col(color_1)
color_grey <- colorRampPalette(c(darken("white",0.1),darken("white",0.3),darken("white",0.6)))(6)
show_col(color_grey)
color_bathy <- colorRampPalette(c(darken("lightblue",0.1),darken("lightblue",0.3),darken("lightblue",0.6)))(6)
show_col(color_bathy)
color_corine <- colorRampPalette(c(darken("lightgreen",0.1),"yellow", darken("lightgreen",0.4),"orange",darken("lightgreen",0.9)))(31)
show_col(color_corine)

###
####
# Move HD --------------------------------------------------
####
###

# pour garder que les ind avec un grand nombre d'observation, pour les cartes principalement

move_point_HD <- move_point %>%
  group_by(indID) %>%
  count() %>% 
  filter(n >=10000) 

move_linestring_HD <- move_point_HD %>%
  # st_as_sf(coords = c("x", "y"), crs = 25832) %>%
  group_by(indID) %>%
  dplyr::summarize(do_union=FALSE) %>%  # do_union=FALSE doesn't work as well
  st_cast("LINESTRING") 

###
####
# MAPS --------------------------------------------------
####
###

# map
tmap_mode("view")

map_1 <- tm_shape(move_linestring_HD) +
  tm_lines(col = "indID", 
           lwd = 1,
           palette = color_1) +
  tm_shape(RMO_buffer) +
  tm_polygons(col = "white", 
              alpha = 0.2) +
  tm_shape(RMO) +
  tm_polygons(col = "black", 
              alpha = 0.1); map_1


# map_2 <- tm_shape(RMO) +
#          tm_polygons(col = "black", alpha = 0.1) +
#   tm_shape(bathy_mask) +
#   tm_raster() +
#   tm_shape(move_linestring_HD) +
#   tm_lines(col = "indID", 
#            lwd = 1,
#            palette = color_1) ; map_2




map_3 <- tm_shape(RMO) +
  tm_polygons(col = "black", alpha = 0.1) +
  tm_shape(bathy_zone) +
  tm_raster(palette = color_grey) +
  tm_shape(move_linestring_HD) +
  tm_lines(col = "indID", 
           lwd = 1,
           palette = color_1) ; map_3

tmap_mode("view")
# tmap_mode("plot")



map_4 <- tm_shape(bathy_zone) +
  tm_raster(palette = color_bathy) +
  tm_shape(corine_zone) +
  tm_raster(labels = labels_corine,
            # legend.show = FALSE,
              palette = color_corine) +
  tm_shape(move_linestring_3max) +
  tm_lines(col = "indID", 
           lwd = 1,
           palette = c("yellow","orange","red")) +
  tm_shape(RMO) +
  tm_polygons(col = "black", alpha = 0.1) ; map_4



unique(corine_zone$LABEL3)







