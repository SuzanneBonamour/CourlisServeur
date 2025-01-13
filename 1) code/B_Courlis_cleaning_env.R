# packages --------------------------------------------------

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
library(cartography)
library(readxl)

###
####
# colors ---------------------------------------------------
####
###

color_1 <- colorRampPalette(c("#1165BA","#BF69D0","#FF8D0A","#11B5E4","#F4FF52"))(19)
show_col(color_1)
color_grey <- colorRampPalette(c(darken("white",0.1),darken("white",0.3),darken("white",0.6)))(6)
show_col(color_grey)
color_bathy <- colorRampPalette(c(darken("lightblue",0.6),darken("lightblue",0.3),darken("lightblue",0.1)))(5)
show_col(color_bathy)
color_corine <- colorRampPalette(c(darken("lightgreen",0.1),"yellow","orange", c(darken("orange",0.5))))(31)
show_col(color_corine)
color_maree20 <- colorRampPalette(c("lightblue","blue","darkblue"))(5)
show_col(color_maree20)
color_maree120 <- colorRampPalette(c("beige","pink","purple"))(5)
show_col(color_maree120)

###
####
# path ---------------------------------------------------
####
###

data_path <- "C:/Users/Suzanne.Bonamour/OneDrive - LPO/2) Data/4) Courlis/Data/1) data/"
data_generated_path <- "C:/Users/Suzanne.Bonamour/OneDrive - LPO/2) Data/4) Courlis/Data/2) data_generated/"

###
####
# dept ---------------------------------------------------
####
###

dept <- st_read(paste0(data_path, "departements.gpkg"),
                layer = "contourdesdepartements"
)
dept17 <- dept[dept$code == 17, ]
dept1785 <- dept[dept$code == 17 | 
                   dept$code == 85,]

# map
tmap_mode("view")

map_dept <- tm_shape(dept) +
  tm_polygons(col = "white", 
              alpha = 0.2) +
  tm_shape(dept1785) +
  tm_polygons(col = "blue", 
              alpha = 0.2) +
  tm_shape(dept17) +
  tm_polygons(col = "green", 
              alpha = 0.2) ; map_dept

###
####
# box ---------------------------------------------------
####
###

BOX <- st_as_sf(st_as_sfc(st_bbox(c(xmin = -1.6, xmax = -0.5, ymax = 45.5, ymin = 46.4), crs = st_crs(4326))))

# map
tmap_mode("view")

map_dept <- tm_shape(dept1785) +
  tm_polygons(col = "white", 
              alpha = 0.2) +
  tm_shape(BOX) +
  tm_polygons(col = "red", 
              alpha = 0.2) ; map_dept

# proj BOX
box_2154 <- st_transform(BOX, crs = 2154)
box_4326 <- st_transform(BOX, crs = 4326)
box_3035 <- st_transform(BOX, crs = 3035)

###
####
# DATA ENV to load ----------------------------------------------
####
###

## reserve --------------------------------------------------

reserve <- st_read(paste0(data_path, "Réserve_naturelle/rnn/rnn/N_ENP_RNN_S_000.shp"))
RMO <- reserve[reserve$NOM_SITE=="Moëze-Oléron",]
RMO$NOM_SITE <- "Réserve de Moëze-Oléron"
# rm(reserve)
RMO_buffer <- st_read(paste0(data_generated_path, "RMO_buffer.gpkg"))

# HASH
reserve_HASH <- hatchedLayer(x = RMO$geometry, pattern = "diamond", mode="sfc", density=0.5, lwd=0.5, color = "red") %>%
  st_as_sf() %>%
  mutate(label="Réserve Moëze-Oléron")

tmap_mode("view")

map_reserve <- tm_scale_bar() +
  tm_shape(RMO) +
  tm_text("NOM_SITE", size = 1) +
  tm_borders() +
  tm_shape(reserve_HASH) +
  tm_lines() ; map_reserve

## marais de Brouage & Oléron) ------------------------------------------------

marais <- st_read(paste0(data_path, "Marais/perim_site_n2000_brouage_3857.geojson"))
crs(marais)
box_3857 <- st_transform(BOX, crs = 3857)

marais_anti <- st_difference(box_3857, marais)

marais_HASH <- hatchedLayer(x = marais$geometry, 
                            pattern = "hexagon", mode="sfc", 
                            density=3) %>%
  st_as_sf() %>%
  mutate(legende="Marais de Brouage & Oléron nord")

# map
tmap_mode("view")
  
map_marais <- tm_scale_bar() +
  tm_shape(marais) + # marais
  tm_borders(col = "black", lwd = 0.5) +
  tm_fill(col = "black", alpha = 0.1) +
  tm_shape(marais_HASH) +
  tm_lines(col="legende", palette = "white", lwd = 1); map_marais

map_marais2 <- tm_scale_bar() +
  tm_shape(marais) + # marais
  tm_borders(col = "black", lwd = 0.5) +
  tm_shape(marais_anti) + # marais
  tm_fill(col = "white", lwd = 0.5, alpha = 0.8) ; map_marais2

## bathymétrie niv moyen -----------------------------------------------

bathy <- raster(paste0(data_path, "/Bathymétrie/MNT_FACADE_ATLANTIQUE_HOMONIM_NM/DONNEES/MNT_ATL100m_HOMONIM_WGS84_NM_ZNEG.grd"))

# restriction bathy à la zone focale autour de la reserve

bathy_crop <- crop(bathy, BOX) # pour avoir un env juste dans la zone patate
bathy_zone <- mask(bathy_crop, BOX)

# map
tmap_mode("view")

map_bathy <- tm_scale_bar() +
  tm_shape(RMO) +
  tm_polygons(col = "green", 
              alpha = 0.2) +
  tm_shape(marais) + # marais
  tm_borders(col = "black", lwd = 0.5) +
  tm_fill(col = "black", alpha = 0.1) +
  tm_shape(marais_HASH) +
  tm_lines(col="legende", palette = "white", lwd = 1) +
  tm_shape(bathy_zone) +
  tm_raster() ; map_bathy

## corine ----------------------------------------------------

corine <- rast(paste0(data_path, "CorineLandCover/U2018_CLC2018_V2020_20u1.tif"))

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

# restriction corine à la zone focale autour de la reserve
corine_crop <- crop(corine, box_3035) # pour avoir un env juste dans la zone patate
corine_zone <- mask(corine_crop, box_3035)

# map
tmap_mode("view")

map_corine_bathy <- tm_scale_bar() + 
  tm_shape(marais) + # marais
  tm_borders(col = "black", lwd = 0.5) +
  tm_fill(col = "black", alpha = 0.1) +
  tm_shape(marais_HASH) +
  tm_lines(col="legende", palette = "white", lwd = 1) +
  tm_shape(RMO) +
  tm_polygons(col = "white", 
              alpha = 0.3) +
  tm_shape(RMO) +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(corine_zone, raster.downsample = F) +
  tm_raster(labels = labels_corine,
            palette = color_corine) +
  tm_shape(bathy_zone, raster.downsample = F) +
  tm_raster(palette = color_bathy) ; map_corine_bathy

## zostere ---------------------------------------------------------

zostere <- st_read(paste0(data_path, "Zostere/bio_herbier_zostere_2022_ofb_pol_2154.shp"))

zostere_zone <- st_intersection(zostere, box_2154)

# map
tmap_mode("view")

map_zostere_corine_bathy <- tm_scale_bar() + 
  tm_shape(marais) + # marais
  tm_borders(col = "black", lwd = 0.5) +
  tm_fill(col = "black", alpha = 0.1) +
  tm_shape(marais_HASH) +
  tm_lines(col="legende", palette = "white", lwd = 1) +
  tm_shape(RMO) +
  tm_polygons(col = "white", alpha = 0.3) +
  tm_shape(RMO) +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(zostere_zone) +
  tm_fill(col = "darkgreen") ; map_zostere_corine_bathy

## tonnes de chasse ------------------------------------------------

tonnes <- st_read(paste0(data_path, "Tonnes_de_chasse/tonnes.shp"))

tonnes_zone <- st_intersection(tonnes, box_2154)

# map
map_tonnes <- tm_shape(tonnes_zone) +
  tm_dots(col = "red") ; map_tonnes

# map
tmap_mode("view")

map_zostere_corine_bathy <- tm_scale_bar() + 
  tm_shape(marais) + # marais
  tm_borders(col = "black", lwd = 0.5) +
  tm_fill(col = "black", alpha = 0.1) +
  tm_shape(marais_HASH) +
  tm_lines(col="legende", palette = "white", lwd = 1) +
  tm_shape(RMO) +
  tm_polygons(col = "white", alpha = 0.3) +
  tm_shape(RMO) +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(tonnes_zone) +
  tm_dots(col = "red"); map_zostere_corine_bathy

## reserve de chasse ------------------------------------------------

chasse <- st_read(paste0(data_path, "Reserve_de_chasse/Reserves_chasse.shp"))

chasse_zone <- st_intersection(chasse, box_2154)

chasse_zone_2 <- chasse_zone %>% 
  dplyr::select(-Commune, -Occupation)

# hash
chasse_in_one <- st_cast(chasse_zone_2$geometry, "MULTIPOLYGON")
chasse_HASH <- hatchedLayer(x = chasse_in_one, pattern = "left2right", mode="sfc", density=10) %>%
  st_as_sf() %>%
  mutate(legende="Réserve de chasse")

# map
tmap_mode("view")

map_tonnes_chasse_zostere_corine_bathy <- tm_scale_bar() + 
  tm_shape(marais) + # marais
  tm_borders(col = "black", lwd = 0.5) +
  tm_fill(col = "black", alpha = 0.1) +
  tm_shape(marais_HASH) +
  tm_lines(col="legende", palette = "white", lwd = 1) +
  tm_shape(RMO) +
  tm_polygons(col = "white", alpha = 0.3) +
  tm_shape(RMO) +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(chasse_zone_2) +
  tm_borders(col = "red", lwd = 0.5) +
  tm_shape(chasse_HASH) +
  tm_lines(col="legende", palette = "red", lwd = 0.5); map_tonnes_chasse_zostere_corine_bathy

## conchyliculture ------------------------------------------------

conch <- st_read(paste0(data_path, "Cadastre_conchylicole/L_CADASTRE_CONCH_S_017_2021_polygone.shp"))

conch_valid <- conch %>% 
  filter(sf::st_is_valid(conch) == T)

conch_zone <- st_intersection(conch_valid, box_2154)

bouchot <- st_read(paste0(data_path, "Cadastre_conchylicole/L_CADASTRE_CONCH_L_017_2021_ligne.shp"))

bouchot_zone <- st_intersection(bouchot, box_2154)

# map
tmap_mode("view")

map_conch_bouchot <- tm_scale_bar() + 
  tm_shape(marais) + # marais
  tm_borders(col = "black", lwd = 0.5) +
  tm_fill(col = "black", alpha = 0.1) +
  tm_shape(marais_HASH) +
  tm_lines(col="legende", palette = "white", lwd = 1) +
  tm_shape(RMO) +
  tm_polygons(col = "white", alpha = 0.3) +
  tm_shape(RMO) +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(conch_zone) + # conchyliculture
  tm_fill(col = "lightyellow") +
  tm_shape(bouchot_zone) + # bouchot
  tm_lines(col = "yellow"); map_conch_bouchot

## crassat ------------------------------------------------

crassat <- st_read(paste0(data_path, "Crassats/2022_i-sea_lpo_HAB-crassats_hermelles_l93.shp"))

# conch_valid <- conch %>% 
#   filter(sf::st_is_valid(conch) == T)
# 
# conch_zone <- st_intersection(conch_valid, box_2154)
# 
# bouchot <- st_read(paste0(data_path, "Cadastre_conchylicole/L_CADASTRE_CONCH_L_017_2021_ligne.shp"))
# 
# bouchot_zone <- st_intersection(bouchot, box_2154)

# map
tmap_mode("view")

map_crassat <- tm_scale_bar() + 
  tm_compass() + 
  tm_shape(marais) + # marais
  tm_borders(col = "black", lwd = 0.5) +
  tm_fill(col = "black", alpha = 0.1) +
  tm_shape(marais_HASH) +
  tm_lines(col="legende", palette = "white", lwd = 1) +
  tm_shape(RMO) +
  tm_polygons(col = "white", alpha = 0.3) +
  tm_shape(RMO) +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(conch_zone) + # conchyliculture
  tm_fill(col = "lightyellow") +
  tm_shape(bouchot_zone) + # bouchot
  tm_lines(col = "yellow") +
  tm_shape(crassat) + # rassat
  tm_fill("orange") ; map_crassat

## habitat marin EUNIS ------------------------------------------------------

seahab <- st_read(paste0(data_path, "Sea_habitat/FR004034.shp"))

# map
tmap_mode("view")

map_ques <- tm_scale_bar() + 
  tm_shape(marais) + # marais
  tm_borders(col = "black", lwd = 0.5) +
  tm_fill(col = "black", alpha = 0.1) +
  tm_shape(marais_HASH) +
  tm_lines(col="legende", palette = "white", lwd = 1) +
  tm_shape(RMO) +
  tm_polygons(col = "white", alpha = 0.3) +
  tm_shape(RMO) +
  tm_text("NOM_SITE", size = 1) +
  tm_scale_bar() + 
  tm_shape(seahab) +
  tm_polygons(col = "annexi") +
  tmap_options(check.and.fix = TRUE); map_ques

## météo -------------------------------------------------------------------

meteo <- read_excel(paste0(data_path, "Meteo/meteo_courlis_la_rochelle.xlsx"))

meteo$time <- c(1:length(meteo$date))

meteo_plot_1 <- ggplot(meteo, aes(x = time, y = tavg)) +
  geom_line(data = meteo, aes(x= time, y = tmin), shape = 21, size = 0.5, color = "blue") +
  geom_line(data = meteo, aes(x= time, y = tmax), shape = 21, size = 0.5, color = "red") +
  geom_line(shape = 21, size = 1, color = "black") +
  theme_classic() +
  labs(
    title = "",
    x = "Time (2015 > 2024)", y = "daily temperature", fill = ""
  ) ; meteo_plot_1

meteo_plot_2 <- ggplot(meteo, aes(x = time, y = wspd, color = wspd)) +
  geom_line(size = 1) +
  scale_color_gradient2(low = "green", high = "red", mid = "orange", midpoint = 25) +
  theme_classic() +
  labs(
    title = "",
    x = "Time (2015 > 2024)", y = "Wind speed", fill = ""
  ) ; meteo_plot_2

## sun light -------------------------------------------------------------------

library(suncalc)

start <- as.Date("2015-01-01")
end <- as.Date("2024-12-13")
lat_RNM <- 45.92007579251774
lon_RNM <- -1.1620299645605838

sun <- getSunlightTimes(date = seq.Date(start, end, by = 1), 
                 # keep = c("sunrise", "sunriseEnd", "sunset", "sunsetStart"), 
                 lat = lat_RNM, lon = lon_RNM, tz = "CET")

sun$time <- c(1:length(sun$date))

sun_plot_1 <- ggplot(sun, aes(x = time, y = hour(sunrise))) +
  geom_line(data = sun, aes(x= time, y = hour(sunset)), shape = 21, size = 0.5, color = "blue") +
  geom_line(shape = 21, size = 1, color = "orange") +
  theme_classic() +
  labs(
    title = "",
    x = "Time (2015 > 2024)", y = "Heure", fill = ""
  ) ; sun_plot_1

## marée -------------------------------------------------------------------

# marnage ---

maree20 <- st_read(paste0(data_path, "Maree/marnage20/INF/marnage_INF_poly_MGA.shp"))
maree120 <- st_read(paste0(data_path, "Maree/marnage120/SUP/marnage_sup_poly_MGA.shp"))

# map
tmap_mode("view")

map_maree20 <- tm_scale_bar() + 
  tm_shape(marais) + # marais
  tm_borders(col = "black", lwd = 0.5) +
  tm_fill(col = "black", alpha = 0.1) +
  tm_shape(marais_HASH) +
  tm_lines(col="legende", palette = "white", lwd = 1) +
  tm_shape(RMO) +
  tm_polygons(col = "white", alpha = 0.3) +
  tm_shape(RMO) +
  tm_text("NOM_SITE", size = 1) +
  tm_scale_bar() + 
  tm_shape(maree20) +
  tm_polygons(col = "marn_INF", alpha = 0.5, palette = color_maree20); map_maree20

map_maree120 <- tm_scale_bar() + 
  tm_shape(marais) + # marais
  tm_borders(col = "black", lwd = 0.5) +
  tm_fill(col = "black", alpha = 0.1) +
  tm_shape(marais_HASH) +
  tm_lines(col="legende", palette = "white", lwd = 1) +
  tm_shape(RMO) +
  tm_polygons(col = "white", alpha = 0.3) +
  tm_shape(RMO) +
  tm_text("NOM_SITE", size = 1) +
  tm_scale_bar() + 
  tm_shape(maree120) +
  tm_polygons(col = "marn_SUP", alpha = 0.5, palette = color_maree120); map_maree120

# maregraphie

data_path <- "C:/Users/Suzanne.Bonamour/OneDrive - LPO/2) Data/4) Courlis/Data/1) data/Maree/maregraphie/ok/"
files_maree <- paste0(data_path, list.files(path = data_path, pattern = "*.txt"))
dt_maree <- lapply(files_maree, fread, sep = ";")
maree <- rbindlist(dt_maree)

maree$time <- c(1:length(maree$Date))

maree_plot_1 <- ggplot(maree, aes(x = time, y = Valeur)) +
  geom_line(shape = 21, size = 1, color = "black") +
  theme_classic() +
  labs(
    title = "",
    x = "Time (2015 > 2024)", y = "Marée", fill = ""
  ) ; maree_plot_1

# SUMMARY maps -------------------------------------------------------------

tmap_mode("view")

## sea & land --------------------------------------------------------------

map_sea_land <- tm_scale_bar() +
  tm_shape(marais) + # marais
  tm_borders(col = "black", lwd = 0.5) +
  tm_fill(col = "black", alpha = 0.1) +
  tm_shape(marais_HASH) +
  tm_lines(col="legende", palette = "white", lwd = 1) +
  tm_shape(RMO) +
  tm_polygons(col = "white", alpha = 0.3) +
  tm_shape(RMO) +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(corine_zone, raster.downsample = F) + # corine
  tm_raster(labels = labels_corine,
            palette = color_corine) +
  tm_shape(bathy_zone, raster.downsample = F) + # bathy
  tm_raster(palette = color_bathy) +
  tm_shape(marais) + # marais
  tm_borders(col = "black", lwd = 0.5) +
  tm_fill(col = "black", alpha = 0.2) ; map_sea_land

## bouffe ------------------------------------------------------------------

map_bouffe <- tm_scale_bar() +
  tm_shape(marais) + # marais
  tm_borders(col = "black", lwd = 0.5) +
  tm_fill(col = "black", alpha = 0.1) +
  tm_shape(marais_HASH) +
  tm_lines(col="legende", palette = "white", lwd = 1) +
  tm_shape(RMO) + # reserve
  tm_polygons(col = "white", alpha = 0.3) +
  tm_shape(RMO) +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(zostere_zone) + # zostere
  tm_fill(col = "darkgreen") +
  tm_shape(conch_zone) + # conchyliculture
  tm_fill(col = "lightyellow", 
          legend.show = F) +
  tm_shape(bouchot_zone) + # bouchot
  tm_lines(col = "yellow", lwd = 1); map_bouffe

## chasse & pêche ---------------------------------------------------------

# map_chasse_peche <- tm_scale_bar() +
#   tm_shape(tonnes_zone) + # tonnes
#   tm_dots(col = "red") +
#   tm_shape(RMO) + 
#   tm_text("NOM_SITE", size = 1) +
#   tm_borders(col = "black", lwd = 0.5) +
#   tm_shape(reserve_HASH) +
#   tm_lines(col="legende", palette = "black", lwd = 0.5) +
#   tm_shape(chasse_zone_2) + # chasse
#   tm_borders(col = "red", lwd = 0.5) +
#   tm_shape(chasse_HASH) +
#   tm_lines(col="legende", palette = "red", lwd = 0.5) ; map_chasse_peche






## gps ---
move_point <- st_read("C:/Users/Suzanne.Bonamour/OneDrive - LPO/2) Data/4) Courlis/GPS/1) Generated_gps/move_point.gpkg")
move_linestring <- st_read("C:/Users/Suzanne.Bonamour/OneDrive - LPO/2) Data/4) Courlis/GPS/1) Generated_gps/move_linestring.gpkg")

move_point_nb <- move_point %>%
   group_by(indID) %>%
   count() 

ind_3max <- move_point_nb$indID[move_point_nb$n > 17200]
move_linestring_3max <- move_linestring[move_linestring$indID %in% ind_3max,]




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


# test hostpot --------------

move_point <- st_read("C:/Users/Suzanne.Bonamour/OneDrive - LPO/2) Data/4) Courlis/GPS/1) Generated_gps/move_point.gpkg")

library(sfhotspot)

# hotspot_classify
hotspot_classify_dt <- hotspot_classify(data = move_point, time = hour)
tmap_mode("view")
hotspot_classify_map <- tm_shape(RMO) +
  tm_polygons(col = "black", alpha = 0.1) +
  tm_shape(hotspot_classify_dt) +
  tm_polygons(col = "hotspot_category", alpha = 0.3); hotspot_classify_map

# hotspot_count
hotspot_count_dt <- hotspot_count(data = move_point)
tmap_mode("view")
hotspot_count_map <- tm_shape(RMO) +
  tm_polygons(col = "black", alpha = 0.1) +
  tm_shape(hotspot_count_dt) +
  tm_polygons(col = "n", alpha = 0.3); hotspot_count_map

# hotspot_kde
aa <- st_as_sf(move_point, crs = st_crs(4326))
hotspot_kde_dt <- hotspot_kde(data = aa)
tmap_mode("view")
hotspot_kde_map <- tm_shape(RMO) +
  tm_polygons(col = "black", alpha = 0.1) +
  tm_shape(hotspot_kde_dt) +
  tm_polygons(col = "hotspot_category", alpha = 0.3); hotspot_kde_map

# MEAN ENV in grid ----