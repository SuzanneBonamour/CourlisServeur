
# Run la partie "starting blok", 
# puis seulement runner la dernière partie "SAVE" avant la partie à souhaitée / en cours de travail

# STARTING BLOCK ---------------------------------------------------------------

# beep lorsqu'il y a une erreur 
options(error = function() {beep(7)})
# options(error = NULL)

# Nettoyage de l'environnement
rm(list=ls()) 

# time zone
library(lubridate)
with_tz(Sys.time(), "Europe/Paris")

## Packages --------------------------------------------------------------------

# library(dplyr)
# library(tidyr)
library(tidyverse)
library(sf)
# library(ggplot2)
# library(classInt)
# library(extrafont)
# library(ggOceanMaps)
# library(remotes)
# library(leaflet)
# library(trip)
library(adehabitatLT)
# library(extrafont)
# library(ggthemes)
library(raster)
# library(graticule)
# library(data.table)
# library(stringi)
library(terra)
library(tmap)
# library(spData)
library(adehabitatHR)
# library(rlist)
library(viridis)
library(beepr)
# library(sp)
# library(stringr)
# library(readr)
library(readxl)
library(marmap)
library(pals)
library(stars)
library(ggcorrplot)

## Functions -------------------------------------------------------------------

# crs
verif_crs <- function(objet_sf) {
  if (st_crs(objet_sf)$epsg != 4326) {
    beepr::beep(2)  # Émet un son d'alerte
    stop("Le CRS n'est pas 4326 !")
  }
}

# time zone systeme
verif_tz_sys <- function() {
  if (Sys.timezone() != "UTC") {
    beepr::beep(2)
    stop("La timezone du système n'est pas UTC !")
  }
}

# time zone
verif_tz <- function(objet, colonne) {
  if (!colonne %in% names(objet)) {
    stop(paste("La colonne", colonne, "n'existe pas dans l'objet !"))
  }
  
  tz <- attr(objet[[colonne]], "tzone")  # Récupérer la timezone
  
  if (is.null(tz) || tz != "UTC") {
    beepr::beep(2)  # Émet un son d'alerte
    stop(paste("La colonne", colonne, "n'est pas en UTC !"))
  }
}

# Fonction pour charger et fusionner les fichiers CSV d'un dossier
telecharger_donnees <- function(chemin) {
  fichiers <- list.files(path = chemin, pattern = "*.csv", full.names = TRUE)
  donnees <- lapply(fichiers, fread, sep = ",")
  return(rbindlist(donnees))
}

## Chemins de données ----------------------------------------------------------

data_path <- "D:/Projets_Suzanne/Courlis/Data/1) data/"
data_generated_path <- "D:/Projets_Suzanne/Courlis/Data/2) data_generated/"
data_image_path <- "D:/Projets_Suzanne/Courlis/Data/3) images/"
data_view_map_path <- "D:/Projets_Suzanne/Courlis/Data/4) view_map/"

###
####
## Font de carte ---------------------------------------------------------------
####
### 

# Réserve ---
reserve <- st_read(paste0(data_path, "Réserve_naturelle/rnn/rnn/N_ENP_RNN_S_000.shp")) # Lecture du fichier shapefile des réserves naturelles
RMO <- reserve[reserve$NOM_SITE == "Moëze-Oléron", ] # Filtrage pour ne garder que la réserve "Moëze-Oléron"
rm(reserve) # Suppression pour libérer de la mémoire

# Zone d'intérêt (box) ---
# box
# BOX <- st_as_sf(st_as_sfc(st_bbox(c(xmin = -1.26, xmax = -0.945, ymax = 46.01, ymin = 45.78), crs = st_crs(4326)))) # Définition d'une boîte englobante avec des coordonnées spécifiques
# st_write(BOX, paste0(data_generated_path, "BOX.gpkg"), append = FALSE) # Sauvegarde de la boîte dans un fichier GeoPackage
BOX <- st_read(paste0(data_generated_path, "BOX.gpkg")) # Lecture de la boîte depuis le fichier sauvegardé
BOX_4326 <- st_transform(BOX, crs = 4326) # Transformation de la boîte au CRS 4326 (coordonnées géographiques)
BOX_2154 <- st_transform(BOX, crs = 2154) # Transformation de la boîte au CRS 2154 (coordonnées géographiques)
# zoom
ZOOM_A <- st_transform(st_as_sf(st_as_sfc(st_bbox(c(xmin = -1.245, xmax = -1.18, ymax = 45.975, ymin = 45.825), crs = st_crs(4326)))), crs = 2154)
ZOOM_B <- st_transform(st_as_sf(st_as_sfc(st_bbox(c(xmin = -1.13, xmax = -1.06, ymax = 45.975, ymin = 45.923), crs = st_crs(4326)))), crs = 2154)
ZOOM_C <- st_transform(st_as_sf(st_as_sfc(st_bbox(c(xmin = -1.13, xmax = -1.06, ymax = 45.923, ymin = 45.865), crs = st_crs(4326)))), crs = 2154)
ZOOM_D <- st_transform(st_as_sf(st_as_sfc(st_bbox(c(xmin = -1.18, xmax = -1.08, ymax = 45.865, ymin = 45.81), crs = st_crs(4326)))), crs = 2154)
ZOOM_E <- st_transform(st_as_sf(st_as_sfc(st_bbox(c(xmin = -0.95, xmax = -1.08, ymax = 45.865, ymin = 45.795), crs = st_crs(4326)))), crs = 2154)
# 
# tmap_mode("view")
# grid_map <- tm_scalebar() +
#   tm_shape(RMO) +
#   tm_polygons(fill_alpha = 0.3, fill = "green") +
#   tm_shape(ZOOM_A) +
#   tm_polygons(fill_alpha = 0.3, fill = "red") +
#   tm_text("Zoom A", size = 1.5) +
#   tm_shape(ZOOM_B) +
#   tm_polygons(fill_alpha = 0.3, fill = "blue") +
#   tm_text("Zoom B", size = 1.5) +
#   tm_shape(ZOOM_C) +
#   tm_polygons(fill_alpha = 0.3, fill = "orange") +
#   tm_text("Zoom C", size = 1.5) +
#   tm_shape(ZOOM_D) +
#   tm_polygons(fill_alpha = 0.3, fill = "pink") +
#   tm_text("Zoom D", size = 1.5) +
#   tm_shape(ZOOM_E) +
#   tm_polygons(fill_alpha = 0.3, fill = "yellow") +
#   tm_text("Zoom E", size = 1.5) +
#   tm_shape(BOX_2154) +
#   tm_borders(col = "black") ; grid_map
# 
# st_write(ZOOM_A, paste0(data_generated_path, "ZOOM_A.gpkg"), append = FALSE) 
# st_write(ZOOM_B, paste0(data_generated_path, "ZOOM_B.gpkg"), append = FALSE) 
# st_write(ZOOM_C, paste0(data_generated_path, "ZOOM_C.gpkg"), append = FALSE) 
# st_write(ZOOM_D, paste0(data_generated_path, "ZOOM_D.gpkg"), append = FALSE)
# st_write(ZOOM_E, paste0(data_generated_path, "ZOOM_E.gpkg"), append = FALSE)

# Departement ---
dept <- st_read(paste0(data_path, "departements.gpkg"), layer = "contourdesdepartements") # Lecture du fichier des départements
dept_BOX <- st_intersection(dept, BOX_4326) # Intersection des départements avec une boîte de délimitation (BOX_4326)
rm(dept) # Suppression pour libérer de la mémoire

# Limite terre mer ---
terre_mer <- st_read(paste0(data_path, "Limite_terre_mer/Limite_terre-mer_facade_Manche_Atlantique_ligne.shp")) # Lecture du fichier shapefile des réserves naturelles
crs(terre_mer)
terre_mer <- st_transform(terre_mer, crs = 4326) # Transformation de la boîte au CRS 2154 (coordonnées géographiques)
terre_mer <- st_intersection(terre_mer, BOX_4326)

# bathymétrie
getNOAA.bathy(lon1=-1.26,lon2=-0.945,lat1=46.01,lat2=45.78, resolution=0.01) -> bathy
# plot(a)
bathy_rast <- marmap::as.raster(bathy)
bathy_stars = st_as_stars(bathy_rast)
zero_hydro = st_contour(bathy_stars, breaks = seq(-10000, 5000, by = 1000), contour_lines = TRUE)

###
####
# GPS DATA to load -------------------------------------------------------------
####
###

GPS <- st_read(file.path(data_generated_path, "GPS_clean.gpkg"))
GPS$y_m_d <- ymd(as.Date(GPS$datetime))
GPS$month_numeric <- month(as.Date(GPS$datetime))
GPS$month_label <- as.character(month(as.Date(GPS$datetime), label = TRUE, abbr = TRUE))

crs(GPS)



# # ZOOM A
# ZOOM_A_4326 <- st_transform(ZOOM_A, crs = 4326)
# GPS_ZOOM_A <- st_intersection(GPS, ZOOM_A_4326) 
# GPS_ZOOM_A <- st_write(GPS_ZOOM_A, paste0(data_generated_path, "GPS_ZOOM_A.gpkg"), append = FALSE)
# 
# # ZOOM B
# ZOOM_B_4326 <- st_transform(ZOOM_B, crs = 4326)
# GPS_ZOOM_B <- st_intersection(GPS, ZOOM_B_4326) 
# GPS_ZOOM_B <- st_write(GPS_ZOOM_B, paste0(data_generated_path, "GPS_ZOOM_B.gpkg"), append = FALSE)
# 
# # ZOOM C
# ZOOM_C_4326 <- st_transform(ZOOM_C, crs = 4326)
# GPS_ZOOM_C <- st_intersection(GPS, ZOOM_C_4326) 
# GPS_ZOOM_C <- st_write(GPS_ZOOM_C, paste0(data_generated_path, "GPS_ZOOM_C.gpkg"), append = FALSE)
# 
# # ZOOM D
# ZOOM_D_4326 <- st_transform(ZOOM_D, crs = 4326)
# GPS_ZOOM_D <- st_intersection(GPS, ZOOM_D_4326) 
# GPS_ZOOM_D <- st_write(GPS_ZOOM_D, paste0(data_generated_path, "GPS_ZOOM_D.gpkg"), append = FALSE)
# 
# table(GPS_ZOOM_A$ID)
# table(GPS_ZOOM_B$ID)
# table(GPS_ZOOM_C$ID)
# table(GPS_ZOOM_D$ID)
# 
# verif_crs(GPS_ZOOM_A)
# verif_crs(GPS_ZOOM_B)
# verif_crs(GPS_ZOOM_C)
# verif_crs(GPS_ZOOM_D)
# 
# GPS_ZOOM_A$datetime <- as.POSIXct(GPS_ZOOM_A$datetime, tz = "UTC")
# GPS_ZOOM_B$datetime <- as.POSIXct(GPS_ZOOM_B$datetime, tz = "UTC")
# GPS_ZOOM_C$datetime <- as.POSIXct(GPS_ZOOM_C$datetime, tz = "UTC")
# GPS_ZOOM_D$datetime <- as.POSIXct(GPS_ZOOM_D$datetime, tz = "UTC")
# 
# verif_tz(GPS_ZOOM_A, "datetime")
# verif_tz(GPS_ZOOM_B, "datetime")
# verif_tz(GPS_ZOOM_C, "datetime")
# verif_tz(GPS_ZOOM_D, "datetime")

###
####
# Météo ------------------------------------------------------------------------
####
###

meteo <- read_excel(paste0(data_path, "/Meteo/meteo_courlis_la_rochelle.xlsx"))

meteo_2 <- meteo %>% 
  dplyr::select(date,tavg,tmin,tmax,prcp,wdir,wspd,pres) %>% 
  rename(y_m_d = date) %>% 
  mutate(y_m_d = ymd(y_m_d))

meteo_3 <- meteo_2 %>% 
  mutate(ECE_wspd = case_when(wspd >= quantile(wspd, .95, na.rm=T) ~ "max",
                              TRUE ~ "ok"),
         ECE_pres = case_when(pres <= quantile(pres, .05, na.rm=T) ~ "min",
                              TRUE ~ "ok"),
         ECE_all = paste0(ECE_wspd,"_",ECE_pres))

meteo_3$ECE_all_2 <- meteo_3$ECE_all
meteo_3$ECE_all_2[str_count(meteo_3$ECE_all_2, "ok") <=1] <- "ECE"
meteo_3$ECE_all_2[meteo_3$ECE_all_2 != "ECE"] <- "ok"

GPS <- left_join(GPS, meteo_3)

table(GPS$ECE_all_2)

###
####
# Chasse -----------------------------------------------------------------------
####
###

chasse <- read_delim(paste0(data_path, "Chasse/2025_02_27_16h29m12_XXX_Frequentation_des_sites_Chasseurs__RNMO.csv"), 
                     delim = ";", escape_double = FALSE, trim_ws = TRUE)

###
####
# GRID -------------------------------------------------------------------------
####
###

# INPN grille ---
grid <- st_read(paste0(data_path, "INPN_grid/METROP_L932X2.shp"))
grid_crop <- st_crop(grid, BOX_2154)
# offset_point <- st_bbox(grid[grid$CD_SIG=="2kmL93E370N6526",])[c("xmin", "ymin")] - c(2000 * 0, 0) ; offset_point
offset_point <- st_bbox(grid[grid$CD_SIG=="2kmL93E370N6528",])[c("xmin", "ymin")] ; offset_point

## 100x100 m ---

# grid_100x100 <- st_make_grid(BOX_2154, cellsize = 100, offset = offset_point)
# st_write(grid_100x100, paste0(data_generated_path, "grid_100x100.gpkg"), append = FALSE)
grid_100x100 <- st_read(paste0(data_generated_path, "grid_100x100.gpkg"))

# tmap_mode("view")
# grid_map <- tm_scalebar() +
#   tm_shape(grid_100x100) +
#   tm_polygons(col = "red", fill_alpha = 0.3) +
#   tm_shape(grid_crop) +
#   tm_polygons(fill_alpha = 0.3, col = "green") +
#   tm_shape(BOX_2154) +
#   tm_borders(col = "yellow"); grid_map

raster_100x100 <- rast(grid_100x100, resolution = 100, crs="EPSG:2154")

## 10x10 m ---

resolution_ZOOM = 10

# zoom A ---
# offset_point_ZOOM_A <- st_bbox(grid[grid$CD_SIG=="2kmL93E372N6534",])[c("xmin", "ymin")] - c(1500 * 1, 0)
# grid_ZOOM_A <- st_make_grid(ZOOM_A, cellsize = resolution_ZOOM, offset = offset_point_ZOOM_A)
# st_write(grid_ZOOM_A, paste0(data_generated_path, "grid_ZOOM_A.gpkg"), append = FALSE)
grid_ZOOM_A <- st_read(paste0(data_generated_path, "grid_ZOOM_A.gpkg"))
raster_ZOOM_A <- rast(grid_ZOOM_A, resolution = resolution_ZOOM, crs="EPSG:2154")

# zoom B ---
# offset_point_ZOOM_B <- st_bbox(grid[grid$CD_SIG=="2kmL93E382N6544",])[c("xmin", "ymin")] - c(2000 * 1, 0)
# grid_ZOOM_B <- st_make_grid(ZOOM_B, cellsize = resolution_ZOOM, offset = offset_point_ZOOM_B)
# st_write(grid_ZOOM_B, paste0(data_generated_path, "grid_ZOOM_B.gpkg"), append = FALSE)
grid_ZOOM_B <- st_read(paste0(data_generated_path, "grid_ZOOM_B.gpkg"))
raster_ZOOM_B <- rast(grid_ZOOM_B, resolution = resolution_ZOOM, crs="EPSG:2154")

# zoom C ---
# offset_point_ZOOM_C <- st_bbox(grid[grid$CD_SIG=="2kmL93E380N6538",])[c("xmin", "ymin")]
# grid_ZOOM_C <- st_make_grid(ZOOM_C, cellsize = resolution_ZOOM, offset = offset_point_ZOOM_C)
# st_write(grid_ZOOM_C, paste0(data_generated_path, "grid_ZOOM_C.gpkg"), append = FALSE)
grid_ZOOM_C <- st_read(paste0(data_generated_path, "grid_ZOOM_C.gpkg"))
raster_ZOOM_C <- rast(grid_ZOOM_C, resolution = resolution_ZOOM, crs="EPSG:2154")

# zoom D ---
# offset_point_ZOOM_D <- st_bbox(grid[grid$CD_SIG=="2kmL93E376N6532",])[c("xmin", "ymin")]
# grid_ZOOM_D <- st_make_grid(ZOOM_D, cellsize = resolution_ZOOM, offset = offset_point_ZOOM_D)
# st_write(grid_ZOOM_D, paste0(data_generated_path, "grid_ZOOM_D.gpkg"), append = FALSE)
grid_ZOOM_D <- st_read(paste0(data_generated_path, "grid_ZOOM_D.gpkg"))
raster_ZOOM_D <- rast(grid_ZOOM_D, resolution = resolution_ZOOM, crs="EPSG:2154")

# zoom E ---
# offset_point_ZOOM_E <- st_bbox(grid[grid$CD_SIG=="2kmL93E376N6534",])[c("xmin", "ymin")]
# grid_ZOOM_E <- st_make_grid(ZOOM_E, cellsize = resolution_ZOOM, offset = offset_point_ZOOM_E)
# st_write(grid_ZOOM_E, paste0(data_generated_path, "grid_ZOOM_E.gpkg"), append = FALSE)
grid_ZOOM_E <- st_read(paste0(data_generated_path, "grid_ZOOM_E.gpkg"))
raster_ZOOM_E <- rast(grid_ZOOM_E, resolution = resolution_ZOOM, crs="EPSG:2154")

# tmap_mode("view")
# grid_map <- tm_scalebar() +
#   # tm_shape(grid_ZOOM_A) +
#   # tm_polygons(col = "red", fill_alpha = 0.3) +
#   # tm_shape(ZOOM_A) +
#   # tm_borders(col = "yellow") +
#   # tm_shape(grid_ZOOM_B) +
#   # tm_polygons(col = "red", fill_alpha = 0.3) +
#   # tm_shape(ZOOM_B) +
#   # tm_borders(col = "yellow") +
#   # tm_shape(grid_ZOOM_C) +
#   # tm_polygons(col = "red", fill_alpha = 0.3) +
#   # tm_shape(ZOOM_C) +
#   # tm_borders(col = "yellow") +
#   # tm_shape(grid_ZOOM_D) +
#   # tm_polygons(col = "red", fill_alpha = 0.3) +
#   # tm_shape(ZOOM_D) +
#   tm_shape(grid_ZOOM_E) +
#   tm_polygons(col = "pink", fill_alpha = 0.3) +
#   tm_shape(ZOOM_E) +
#   tm_borders(col = "yellow") +
#   tm_shape(grid_crop) +
#   tm_polygons(fill_alpha = 0.3, col = "green") ; grid_map

###
####
# ***ROOSTING*** ---------------------------------------------------------------
####
###

## GLOB -----------------------------------------------------------------------

GPS.roosting_glob <- GPS %>% 
  filter(behavior == "roosting") %>% 
  dplyr::select(lon,lat) %>% 
  st_drop_geometry() %>% 
  na.omit()

GPS.roosting_spa <- st_as_sf(GPS.roosting_glob, coords = c("lon", "lat"), crs = 4326)
GPS.roosting_spa <- st_transform(GPS.roosting_spa, crs = 32630)  
GPS.roosting_coords <- st_coordinates(GPS.roosting_spa) 

# raster/grid
crs_utm <- "EPSG:32630"
SpatRaster <- project(raster_100x100, crs_utm)
RasterLayer <- raster(SpatRaster)
SpatialPixels <- as(RasterLayer, "SpatialPixels")

# Règle de Silverman
sigma_x.roosting_glob <- sd(GPS.roosting_coords[,1]) 
sigma_y.roosting_glob <- sd(GPS.roosting_coords[,2]) 
n.roosting_glob <- nrow(GPS.roosting_glob) 
h_silverman_x.roosting_glob <- 1.06 * sigma_x.roosting_glob * n.roosting_glob^(-1/5) / 2
h_silverman_y.roosting_glob <- 1.06 * sigma_y.roosting_glob * n.roosting_glob^(-1/5) / 2
# cat("h optimal en mètres pour X:", h_silverman_x.roosting_glob, "\n")
# cat("h optimal en mètres pour Y:", h_silverman_y.roosting_glob, "\n")
locs_spa.roosting_glob <- as(GPS.roosting_spa, "Spatial")

# KernelUD
kud.roosting_glob <- kernelUD(locs_spa.roosting_glob, 
                                      grid = SpatialPixels, 
                                      h = mean(c(h_silverman_x.roosting_glob, 
                                                 h_silverman_y.roosting_glob)))

# Visualiser la densité de noyau
# par(mfrow = c(1, 1))
# image(kud_roosting_glob)

# Isoclines 
rast.roosting_glob <- rast(kud.roosting_glob)
courtour.roosting_glob <- as.contour(rast.roosting_glob)
sf.roosting_glob <- st_as_sf(courtour.roosting_glob)
results_kud.roosting_glob <- st_cast(sf.roosting_glob, "POLYGON")

# write & read
st_write(results_kud.roosting_glob, paste0(data_generated_path, "results_kud.roosting_glob.gpkg"), append = FALSE)
results_kud.roosting_glob <- st_read(file.path(data_generated_path, "results_kud.roosting_glob.gpkg"))

# plot
tmap_mode("view")
UDMap_roosting_glob <- tm_scalebar() +
  tm_shape(RMO) +
  tm_polygons() +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(results_kud.roosting_glob) +
  tm_polygons(border.col = "grey", fill = "level", fill_alpha = 0.2,
              palette = viridis::viridis(10, begin = 0, end = 1,
                                direction = 1, option = "plasma")) +
  tm_shape(rast.roosting_glob) +
  tm_raster() +
  tm_shape(terre_mer) +
  tm_lines(col = "lightblue", lwd = 0.1) + 
  tm_shape(zero_hydro) +
  tm_lines("layer", col = "darkblue", lwd = 0.5, legend.show = FALSE, 
           title.col = "Elevation"); UDMap_roosting_glob

tmap_mode("view")
UDMap_roosting_glob <- tm_scalebar() +
  tm_shape(RMO) +
  tm_polygons() +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(cast_roosting_glob) +
  tm_polygons(border.col = "grey", fill = "level", fill_alpha = 0.2,
              palette = viridis::viridis(10, begin = 0, end = 1,
                                         direction = 1, option = "plasma")) +
  tm_shape(terre_mer) +
  tm_lines(col = "lightblue", lwd = 0.1) + 
  tm_shape(zero_hydro) +
  tm_lines("layer", col = "darkblue", lwd = 0.5, legend.show = FALSE, 
           title.col = "Elevation"); UDMap_roosting_glob

## ZOOM -----------------------------------------------------------------------

resolution_ZOOM = 10

crs_utm <- "EPSG:32630"
ZOOM <- c("A","B","C","D","E")
results_kud.roosting_ZOOM = NULL

lettre = "A"

for (lettre in ZOOM){
  
  # in ZOOM
  ZOOM <- st_read(paste0(data_generated_path,"ZOOM_",lettre,".gpkg"))
  ZOOM <- st_transform(ZOOM, crs = 4326)
  GPS.ZOOM <- st_intersection(GPS, ZOOM) 
  GPS.roosting_ZOOM <- GPS.ZOOM %>% 
    filter(behavior == "roosting") %>% 
    dplyr::select(lon,lat) %>% 
    st_drop_geometry() %>% 
    na.omit()
  
  GPS_spa.roosting_ZOOM <- st_as_sf(GPS.roosting_ZOOM, coords = c("lon", "lat"), crs = 4326)
  GPS_spa.roosting_ZOOM <- st_transform(GPS_spa.roosting_ZOOM, crs = 32630)  
  GPS_coords.roosting_ZOOM <- st_coordinates(GPS_spa.roosting_ZOOM) 
  
  # raster/grid
  grid.ZOOM <- st_read(paste0(data_generated_path, "grid_ZOOM_",lettre,".gpkg"))
  raster.ZOOM <- rast(grid.ZOOM, resolution = resolution_ZOOM, crs="EPSG:2154")
  SpatRaster.ZOOM <- project(raster.ZOOM, crs_utm)
  RasterLayer.ZOOM <- raster(SpatRaster.ZOOM)
  SpatialPixels.ZOOM <- as(RasterLayer.ZOOM, "SpatialPixels") 
  
  # Règle de Silverman
  sigma_x.roosting_ZOOM <- sd(GPS_coords.roosting_ZOOM[,1]) 
  sigma_y.roosting_ZOOM <- sd(GPS_coords.roosting_ZOOM[,2]) 
  n.roosting_ZOOM <- nrow(GPS.roosting_ZOOM)
  h.silverman_x_roosting_ZOOM <- 1.06 * sigma_x.roosting_ZOOM * n.roosting_ZOOM^(-1/5) / 2
  h.silverman_y_roosting_ZOOM <- 1.06 * sigma_y.roosting_ZOOM * n.roosting_ZOOM^(-1/5) / 2
  locs_spa.roosting_ZOOM <- as(GPS_spa.roosting_ZOOM, "Spatial")
  
  # KernelUD
  kud.roosting_ZOOM <- kernelUD(locs_spa.roosting_ZOOM, 
                                grid = SpatialPixels.ZOOM, 
                                h = mean(c(h.silverman_x_roosting_ZOOM, h.silverman_y_roosting_ZOOM)))
  
  # Isoclines 
  rast.roosting_ZOOM <- rast(kud.roosting_ZOOM)
  courtour.roosting_ZOOM <- as.contour(rast.roosting_ZOOM)
  sf.roosting_ZOOM <- st_as_sf(courtour.roosting_ZOOM)
  cast.roosting_ZOOM <- st_cast(sf.roosting_ZOOM, "POLYGON")
  cast.roosting_ZOOM$ZOOM <- lettre
  results_kud.roosting_ZOOM <- rbind(results_kud.roosting_ZOOM, cast.roosting_ZOOM)

}

# write & read
st_write(results_kud.roosting_ZOOM, paste0(data_generated_path, "results_kud.roosting_ZOOM.gpkg"), append = FALSE)
results_kud.roosting_ZOOM <- st_read(file.path(data_generated_path, "results_kud.roosting_ZOOM.gpkg"))

# plot
tmap_mode("view")
UDMap_roosting_ZOOM <- tm_scalebar() +
  tm_shape(RMO) +
  tm_polygons() +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(ZOOM_A) +
  tm_polygons(fill_alpha = 0.1, fill = "grey") +
  tm_text("A", size = 1.5) +
  tm_shape(ZOOM_B) +
  tm_polygons(fill_alpha = 0.1, fill = "grey") +
  tm_text("B", size = 1.5) +
  tm_shape(ZOOM_C) +
  tm_polygons(fill_alpha = 0.1, fill = "grey") +
  tm_text("C", size = 1.5) +
  tm_shape(ZOOM_D) +
  tm_polygons(fill_alpha = 0.1, fill = "grey") +
  tm_text("D", size = 1.5) +
  tm_shape(ZOOM_E) +
  tm_polygons(fill_alpha = 0.1, fill = "grey") +
  tm_text("E", size = 1.5) +
  tm_shape(BOX_2154) +
  tm_borders(col = "black") +
  tm_shape(results_kud.roosting_ZOOM) + 
  tm_polygons(border.col = "grey", fill = "level", fill_alpha = 0.2, 
              palette = viridis::viridis(10, begin = 0, end = 1, 
                                direction = 1, option = "plasma")) +
  tm_shape(terre_mer) +
  tm_lines(col = "lightblue", lwd = 0.1) + 
  tm_shape(zero_hydro) +
  tm_lines("layer", col = "darkblue", lwd = 0.5, legend.show = FALSE, 
           title.col = "Elevation"); UDMap_roosting_ZOOM

## Month -----------------------------------------------------------------------

resolution_ZOOM = 10

crs_utm <- "EPSG:32630"
ZOOM <- c("A","B","C","D","E")
results_kud.roosting_ZOOM_month = NULL

# lettre = "B"

for (lettre in ZOOM){
  # in ZOOM
  ZOOM <- st_read(paste0(data_generated_path,"ZOOM_",lettre,".gpkg"))
  ZOOM <- st_transform(ZOOM, crs = 4326)
  GPS.ZOOM <- st_intersection(GPS, ZOOM) 
  GPS.roosting_ZOOM_month <- GPS.ZOOM %>% 
    filter(behavior == "roosting") %>% 
    dplyr::select(lon,lat,month_label) %>% 
    st_drop_geometry() %>% 
    na.omit()
  
  if (nrow(GPS.roosting_ZOOM_month) == 0) {
    next  # Passe directement à l'itération suivante
  }
  
  nb_row <- GPS.roosting_ZOOM_month %>% 
    group_by(month_label) %>%
    summarise(n = n(), .groups = "drop")
  
  if (min(nb_row$n) < 5) {
    next  # Passe directement à l'itération suivante
  }
  
    # Crée une table avec tous les mois possibles
  all_months <- tibble(
    month_label = c("janv", "févr", "mars", "avr", "mai", "juin",
                    "juil", "août", "sept", "oct", "nov", "déc")
  )
  
  # Compte les occurrences par mois dans tes données
  nb_row <- GPS.roosting_ZOOM_month %>%
    group_by(month_label) %>%
    summarise(n = n(), .groups = "drop")
  
  # Joint tous les mois et remplit avec 0 si manquant
  nb_row_complet <- all_months %>%
    left_join(nb_row, by = "month_label") %>%
    mutate(n = if_else(is.na(n), 0L, n))
  
  if (min(nb_row_complet$n) < 5) {
    next  # Passe directement à l'itération suivante
  }
  
  GPS_spa.roosting_ZOOM_month <- st_as_sf(GPS.roosting_ZOOM_month, coords = c("lon", "lat"), crs = 4326)
  GPS_spa.roosting_ZOOM_month <- st_transform(GPS_spa.roosting_ZOOM_month, crs = 32630) 
  GPS_coods.roosting_ZOOM_month <- st_coordinates(GPS_spa.roosting_ZOOM_month)
  
  # raster/grid
  grid_ZOOM <- st_read(paste0(data_generated_path, "grid_ZOOM_",lettre,".gpkg"))
  raster_ZOOM <- rast(grid_ZOOM, resolution = resolution_ZOOM, crs="EPSG:2154")
  SpatRaster_ZOOM <- project(raster_ZOOM, crs_utm)  
  RasterLayer_ZOOM <- raster(SpatRaster_ZOOM) 
  SpatialPixels_ZOOM <- as(RasterLayer_ZOOM, "SpatialPixels")
  
  # Règle de Silverman
  sigma_x.roosting_ZOOM_month <- sd(GPS_coods.roosting_ZOOM_month[,1]) 
  sigma_y.roosting_ZOOM_month <- sd(GPS_coods.roosting_ZOOM_month[,2]) 
  n.roosting_ZOOM_month <- nrow(GPS.roosting_ZOOM_month)  
  h.silverman_x_roosting_ZOOM_month <- 1.06 * sigma_x.roosting_ZOOM_month * n.roosting_ZOOM_month^(-1/5) / 2
  h_silverman_y_roosting_ZOOM_month <- 1.06 * sigma_y.roosting_ZOOM_month * n.roosting_ZOOM_month^(-1/5) / 2
  locs_spa.roosting_ZOOM_month <- as(GPS_spa.roosting_ZOOM_month, "Spatial")
  
  # KernelUD
  kud.roosting_ZOOM_month <- kernelUD(locs_spa.roosting_ZOOM_month["month_label"], 
                                    grid = SpatialPixels_ZOOM, 
                                    h = mean(c(h.silverman_x_roosting_ZOOM_month, 
                                               h_silverman_y_roosting_ZOOM_month)))
  
  kud_list.roosting_ZOOM_month <- lapply(names(kud.roosting_ZOOM_month), function(month) {
    
    print(month)
    
    # Extraire l'estimation de densité pour un ID spécifique
    kud_single.roosting_ZOOM_month <- kud.roosting_ZOOM_month[[month]]
    rast.roosting_ZOOM_month <- rast(kud_single.roosting_ZOOM_month)
    courtour.roosting_ZOOM_month <- as.contour(rast.roosting_ZOOM_month)
    sf.roosting_ZOOM_month <- st_as_sf(courtour.roosting_ZOOM_month)
    cast.roosting_ZOOM_month <- st_cast(sf.roosting_ZOOM_month, "POLYGON")
    cast.roosting_ZOOM_month$month <- month
    
    return(cast.roosting_ZOOM_month)
  })
  
  kud_all.roosting_ZOOM_month <- do.call(rbind, kud_list.roosting_ZOOM_month)
  kud_all.roosting_ZOOM_month$month <- as.factor(kud_all.roosting_ZOOM_month$month)
  kud_all.roosting_ZOOM_month$ZOOM <- lettre
  results_kud.roosting_ZOOM_month <- rbind(results_kud.roosting_ZOOM_month, kud_all.roosting_ZOOM_month)
  
}

# write
st_write(results_kud.roosting_ZOOM_month, paste0(data_generated_path, "results_kud.roosting_ZOOM_month.gpkg"), append = FALSE)
# read
results_kud.roosting_ZOOM_month <- st_read(file.path(data_generated_path, "results_kud.roosting_ZOOM_month.gpkg"))

# plot
tmap_mode("view")
UDMap_roosting_month_ZOOM <- tm_scalebar() +
  tm_shape(RMO) +
  tm_polygons() +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(ZOOM_A) +
  tm_polygons(fill_alpha = 0.1, fill = "grey") +
  tm_text("A", size = 1.5) +
  tm_shape(ZOOM_B) +
  tm_polygons(fill_alpha = 0.1, fill = "grey") +
  tm_text("B", size = 1.5) +
  tm_shape(ZOOM_C) +
  tm_polygons(fill_alpha = 0.1, fill = "grey") +
  tm_text("C", size = 1.5) +
  tm_shape(ZOOM_D) +
  tm_polygons(fill_alpha = 0.1, fill = "grey") +
  tm_text("D", size = 1.5) +
  tm_shape(ZOOM_E) +
  tm_polygons(fill_alpha = 0.1, fill = "grey") +
  tm_text("E", size = 1.5) +
  tm_shape(BOX_2154) +
  tm_borders(col = "black") +
  tm_shape(results_kud.roosting_ZOOM_month) + 
  tm_facets("month") + 
  tm_polygons(border.col = "grey", fill = "level", fill_alpha = 0.2, 
              palette = viridis::viridis(10, begin = 0, end = 1, 
                                         direction = 1, option = "plasma")) +
  tm_facets("month") +
  tm_shape(terre_mer) +
  tm_lines(col = "lightblue", lwd = 0.1) + 
  tm_shape(zero_hydro) +
  tm_lines("layer", col = "darkblue", lwd = 0.5, legend.show = FALSE, 
           title.col = "Elevation"); UDMap_roosting_month_ZOOM

###                        ###
### Repétabilité inter-month / population scale ###
###                        ###

GPS.month_repet_pop <- GPS %>% 
  filter(behavior == "roosting") %>% 
  dplyr::select(datetime,lon,lat,month_label) %>% 
  st_drop_geometry() %>% 
  na.omit()

# au moins 5 point par group
n_per_month <- GPS.month_repet_pop %>% 
  group_by(month_label) %>% 
  summarize(n = n())%>% 
  filter(n <= 5) #%>%
# mutate(ID_month = paste0(ID, "_", month_label))

# reverse of %in%  
`%ni%` <- Negate(`%in%`)

GPS.month_repet_pop <- GPS.month_repet_pop %>% 
  filter(month_label %ni% n_per_month$month_label)

# Transformer en objet spatial (EPSG:4326)
GPS_spa.month_repet_pop <- st_as_sf(GPS.month_repet_pop, coords = c("lon", "lat"), crs = 4326)
GPS_spa.month_repet_pop <- st_transform(GPS_spa.month_repet_pop, crs = 32630)

# raster/grid
crs_utm <- "EPSG:32630"
SpatRaster <- project(raster_100x100, crs_utm)
RasterLayer <- raster(SpatRaster)
SpatialPixels <- as(RasterLayer, "SpatialPixels")

# Extraire les coordonnées reprojetées
coords.month_repet_pop <- st_coordinates(GPS_spa.month_repet_pop)

# Règle de Silverman
sigma_x.roosting_month_repet_pop <- sd(coords.month_repet_pop[,1])
sigma_y_roosting_month_repet_pop <- sd(coords.month_repet_pop[,2])
n.roosting_month_repet_pop <- nrow(GPS_spa.month_repet_pop)

h.silverman_x_roosting_month_repet_pop <- 1.06 * sigma_x.roosting_month_repet_pop * n.roosting_month_repet_pop^(-1/5)
h.silverman_y_roosting_month_repet_pop <- 1.06 * sigma_y_roosting_month_repet_pop * n.roosting_month_repet_pop^(-1/5)

GPS_spa.month_repet_pop <- as(GPS_spa.month_repet_pop, "Spatial")

kud.roosting_month_repet_pop <- kernelUD(GPS_spa.month_repet["month_label"], 
                                     grid = as(SpatialPixels, "SpatialPixels"),
                                     h = mean(c(h.silverman_x_roosting_month_repet_pop,
                                                h.silverman_y_roosting_month_repet_pop)))

##                     ##
## valeur répétabilité ##
##                     ##

overlap.roosting_month_repet_pop <- kerneloverlaphr(kud.roosting_month_repet_pop, method = "BA")
mean_overlap.roosting_month_repet_pop <- mean(overlap.roosting_month_repet_pop, na.rm = T) ; mean

# overlap_matrix
min_val <- min(overlap.roosting_month_repet_pop, na.rm = TRUE)
max_val <- max(overlap.roosting_month_repet_pop, na.rm = TRUE)
ordre <- c("janv", "févr", "mars", "avr","mai","juin","juil","août","sept","oct","nov","déc")
overlap.roosting_month_repet_pop <- tt[ordre, ordre]

plot.overlapp_roosting_month_repet_pop <- ggcorrplot(overlap.roosting_month_repet_pop,
           hc.order = FALSE,
           method = "circle",
           type = "lower",
           lab = TRUE,
           digits = 1,
           colors = c("white", "yellow", "red"),
           ggtheme = theme_minimal()) +
  scale_fill_gradientn(colors = c("white", "yellow", "red"),
                       limits = c(min(tt_ordered, na.rm = TRUE), 
                                  max(tt_ordered, na.rm = TRUE))) ; plot.overlapp_roosting_month_repet_pop

##               ##
## UDMap par ind ##
##               ##

# Estimation UDmap par ind par month

# Créer une liste pour stocker les résultats
UDmaps_list.roosting_ZOOM_month <- lapply(names(kud.roosting_month_repet), function(Individu_Periode) {
  
  print(Individu_Periode)
  
  # Extraire l'estimation de densité pour un ID spécifique
  kud_single.roosting_ZOOM_month <- kud.roosting_month_repet[[Individu_Periode]]
  rast.roosting_ZOOM_month <- rast(kud_single.roosting_ZOOM_month)
  contour.roosting_ZOOM_month <- as.contour(rast.roosting_ZOOM_month)
  sf.roosting_ZOOM_month <- st_as_sf(contour.roosting_ZOOM_month)
  cast.roosting_ZOOM_month <- st_cast(sf.roosting_ZOOM_month, "POLYGON")
  cast.roosting_ZOOM_month$Individu_Periode <- Individu_Periode
  
  return(cast.roosting_ZOOM_month)
  
})

# Fusionner tous les ID dans un seul objet sf
results_kud.roosting_ZOOM_month <- do.call(rbind, UDmaps_list.roosting_ZOOM_month)
results_kud.roosting_ZOOM_month$Individu_Periode <- as.factor(results_kud.roosting_ZOOM_month$Individu_Periode)
results_kud.roosting_ZOOM_month$ID <- sub("_.*", "", results_kud.roosting_ZOOM_month$Individu_Periode)
results_kud.roosting_ZOOM_month$Individu_Periode <- droplevels(results_kud.roosting_ZOOM_month$Individu_Periode)
results_kud.roosting_ZOOM_month$Periode <- sub(".*_", "", results_kud.roosting_ZOOM_month$Individu_Periode)
results_kud.roosting_ZOOM_month$ID <- as.factor(results_kud.roosting_ZOOM_month$ID)

# plot 
tmap_mode("view")

UDMap_roosting_rep_inter_month <- tm_shape(RMO) +
  tm_polygons() +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(results_kud.roosting_ZOOM_month) + 
  tm_facets("ID") +
  tm_polygons(border.col = "grey", fill = "Periode", fillfill_alpha = 0.2) ; UDMap_roosting_rep_inter_month


###                        ###
### Repétabilité inter-month / individual scale ###
###                        ###

GPS.month_repet <- GPS %>% 
  filter(behavior == "roosting") %>% 
  dplyr::select(ID,datetime,lon,lat,month_label) %>% 
  mutate(ID_month = paste0(ID, "_", month_label)) %>% 
  st_drop_geometry() %>% 
  na.omit()

# au moins 5 point par group
n_per_month <- GPS.month_repet %>% 
  group_by(ID_month) %>% 
  summarize(n = n())%>% 
  filter(n <= 5) #%>%
  # mutate(ID_month = paste0(ID, "_", month_label))

# reverse of %in%  
`%ni%` <- Negate(`%in%`)

GPS.month_repet <- GPS.month_repet %>% 
  filter(ID_month %ni% n_per_month$ID_month)

# Transformer en objet spatial (EPSG:4326)
GPS_spa.month_repet <- st_as_sf(GPS.month_repet, coords = c("lon", "lat"), crs = 4326)
GPS_spa.month_repet <- st_transform(GPS_spa.month_repet, crs = 32630)

# raster/grid
crs_utm <- "EPSG:32630"
SpatRaster <- project(raster_100x100, crs_utm)
RasterLayer <- raster(SpatRaster)
SpatialPixels <- as(RasterLayer, "SpatialPixels")

# Extraire les coordonnées reprojetées
coords.month_repet <- st_coordinates(GPS_spa.month_repet)

# Règle de Silverman
sigma_x.roosting_month_repet <- sd(coords.month_repet[,1])
sigma_y_roosting_month_repet <- sd(coords.month_repet[,2])
n.roosting_month_repet <- nrow(GPS_spa.month_repet)

h.silverman_x_roosting_month_repet <- 1.06 * sigma_x.roosting_month_repet * n.roosting_month_repet^(-1/5)
h.silverman_y_roosting_month_repet <- 1.06 * sigma_y_roosting_month_repet * n.roosting_month_repet^(-1/5)

GPS_spa.month_repet <- as(GPS_spa.month_repet, "Spatial")

kud.roosting_month_repet <- kernelUD(GPS_spa.month_repet["ID_month"], 
                                     grid = as(SpatialPixels, "SpatialPixels"),
                                     h = mean(c(h.silverman_x_roosting_month_repet,
                                                h.silverman_y_roosting_month_repet)))

##                     ##
## valeur répétabilité ##
##                     ##

# Estimation valeur d'overlapp par ind entre chaque month

# Extraire les noms uniques des individus
individus <- unique(GPS_spa.month_repet$ID)

# Stocker les résultats
overlap_results.roosting_month_repet = NULL

# Boucle sur chaque individu
for (ind in individus) {
  
  print(ind)
  
  # Trouver les noms des périodes de cet individu dans hr_kde
  ID_periodes <- names(kud.roosting_month_repet)[grep(paste0("^", ind, "_"), names(kud.roosting_month_repet))]
  
  # Vérifier que l'individu a bien deux périodes
  # if (length(ID_periodes) == 2) {
  # Créer un estUDm valide
  hr_kde_ind.roosting_month_repet <- kud.roosting_month_repet[ID_periodes]
  class(hr_kde_ind.roosting_month_repet) <- "estUDm"  # Important pour que kerneloverlaphr() fonctionne
  
  # Calculer l'overlap entre les deux périodes
  overlap_value.roosting_month_repet <- kerneloverlaphr(hr_kde_ind.roosting_month_repet, 
                                   method = "BA")[1, 2]
  
  info_ind.roosting_month_repet <- c(ind, overlap_value.roosting_month_repet)
  
  # Stocker le résultat
  # overlap_results <- rbind(overlap_results, data.frame(Individu = ind, Overlap = overlap_value))
  overlap_results.roosting_month_repet <- rbind(overlap_results.roosting_month_repet, info_ind.roosting_month_repet)
  
  # }
}

overlap_results.roosting_month_repet <- as.data.frame(overlap_results.roosting_month_repet)

overlap_results.roosting_month_repet <- overlap_results.roosting_month_repet %>% 
  rename(ID = V1, overlap = V2)

overlap_results.roosting_month_repet$overlap <- as.numeric(overlap_results.roosting_month_repet$overlap)

mean_overlap.roosting_month_repet <- mean(overlap_results.roosting_month_repet$overlap, na.rm = T) ; mean_overlap_month_over_all_ind

# Afficher les résultats
overlap_results.roosting_month_repet <- overlap_results.roosting_month_repet[order(overlap_results.roosting_month_repet$overlap), ] ; overlap_results.roosting_month_repet

# plot
plot.roosting_month_repet <- ggplot(overlap_results.roosting_month_repet, aes(x=reorder(ID, overlap), y=overlap)) + 
  geom_point(shape = 19, size = 4) +
  theme_classic() +
  coord_flip() +
  theme(legend.position = "top") +
  scale_fill_manual() +
  labs(title="",
       x ="Individu", y = "Pourcentage d'overlap inter-mois"); plot.roosting_month_repet

##               ##
## UDMap par ind ##
##               ##

# Estimation UDmap par ind par month

# Créer une liste pour stocker les résultats
UDmaps_list.roosting_ZOOM_month <- lapply(names(kud.roosting_month_repet), function(Individu_Periode) {
  
  print(Individu_Periode)
  
  # Extraire l'estimation de densité pour un ID spécifique
  kud_single.roosting_ZOOM_month <- kud.roosting_month_repet[[Individu_Periode]]
  rast.roosting_ZOOM_month <- rast(kud_single.roosting_ZOOM_month)
  contour.roosting_ZOOM_month <- as.contour(rast.roosting_ZOOM_month)
  sf.roosting_ZOOM_month <- st_as_sf(contour.roosting_ZOOM_month)
  cast.roosting_ZOOM_month <- st_cast(sf.roosting_ZOOM_month, "POLYGON")
  cast.roosting_ZOOM_month$Individu_Periode <- Individu_Periode
  
  return(cast.roosting_ZOOM_month)
  
})

# Fusionner tous les ID dans un seul objet sf
results_kud.roosting_ZOOM_month <- do.call(rbind, UDmaps_list.roosting_ZOOM_month)
results_kud.roosting_ZOOM_month$Individu_Periode <- as.factor(results_kud.roosting_ZOOM_month$Individu_Periode)
results_kud.roosting_ZOOM_month$ID <- sub("_.*", "", results_kud.roosting_ZOOM_month$Individu_Periode)
results_kud.roosting_ZOOM_month$Individu_Periode <- droplevels(results_kud.roosting_ZOOM_month$Individu_Periode)
results_kud.roosting_ZOOM_month$Periode <- sub(".*_", "", results_kud.roosting_ZOOM_month$Individu_Periode)
results_kud.roosting_ZOOM_month$ID <- as.factor(results_kud.roosting_ZOOM_month$ID)

# plot 
tmap_mode("view")

UDMap_roosting_rep_inter_month <- tm_shape(RMO) +
  tm_polygons() +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(results_kud.roosting_ZOOM_month) + 
  tm_facets("ID") +
  tm_polygons(border.col = "grey", fill = "Periode", fillfill_alpha = 0.2) ; UDMap_roosting_rep_inter_month

## Year -----------------------------------------------------------------------

resolution_ZOOM = 10

crs_utm <- "EPSG:32630"
ZOOM <- c("A","B","C","D","E")
results_kud.roosting_ZOOM_year = NULL

# lettre = "B"

for (lettre in ZOOM){
  # in ZOOM
  ZOOM <- st_read(paste0(data_generated_path,"ZOOM_",lettre,".gpkg"))
  ZOOM <- st_transform(ZOOM, crs = 4326)
  GPS.ZOOM <- st_intersection(GPS, ZOOM) 
  GPS.roosting_ZOOM_year <- GPS.ZOOM %>% 
    filter(behavior == "roosting") %>% 
    dplyr::select(lon,lat,year) %>% 
    st_drop_geometry() %>% 
    na.omit()
  
  if (nrow(GPS.roosting_ZOOM_year) == 0) {
    next  # Passe directement à l'itération suivante
  }
  
  nb_row <- GPS.roosting_ZOOM_year %>% 
    group_by(year) %>%
    summarise(n = n(), .groups = "drop")
  
  if (min(nb_row$n) < 5) {
    next  # Passe directement à l'itération suivante
  }
  
  # Crée une table avec tous les mois possibles
  all_year <- tibble(
    year = c(2018:2024)
  )
  
  # Compte les occurrences par mois dans tes données
  nb_row <- GPS.roosting_ZOOM_year %>%
    group_by(year) %>%
    summarise(n = n(), .groups = "drop")
  
  # Joint tous les mois et remplit avec 0 si manquant
  nb_row_complet <- all_year %>%
    left_join(nb_row, by = "year") %>%
    mutate(n = if_else(is.na(n), 0L, n))
  
  if (min(nb_row_complet$n) < 5) {
    next  # Passe directement à l'itération suivante
  }
  
  GPS_spa.roosting_ZOOM_year <- st_as_sf(GPS.roosting_ZOOM_year, coords = c("lon", "lat"), crs = 4326)
  GPS_spa.roosting_ZOOM_year <- st_transform(GPS_spa.roosting_ZOOM_year, crs = 32630) 
  GPS_coods.roosting_ZOOM_year <- st_coordinates(GPS_spa.roosting_ZOOM_year)
  
  # raster/grid
  grid_ZOOM <- st_read(paste0(data_generated_path, "grid_ZOOM_",lettre,".gpkg"))
  raster_ZOOM <- rast(grid_ZOOM, resolution = resolution_ZOOM, crs="EPSG:2154")
  SpatRaster_ZOOM <- project(raster_ZOOM, crs_utm)  
  RasterLayer_ZOOM <- raster(SpatRaster_ZOOM) 
  SpatialPixels_ZOOM <- as(RasterLayer_ZOOM, "SpatialPixels")
  
  # Règle de Silverman
  sigma_x.roosting_ZOOM_year <- sd(GPS_coods.roosting_ZOOM_year[,1]) 
  sigma_y.roosting_ZOOM_year <- sd(GPS_coods.roosting_ZOOM_year[,2]) 
  n.roosting_ZOOM_year <- nrow(GPS.roosting_ZOOM_year)  
  h.silverman_x_roosting_ZOOM_year <- 1.06 * sigma_x.roosting_ZOOM_year * n.roosting_ZOOM_year^(-1/5) / 2
  h_silverman_y_roosting_ZOOM_year <- 1.06 * sigma_y.roosting_ZOOM_year * n.roosting_ZOOM_year^(-1/5) / 2
  locs_spa.roosting_ZOOM_year <- as(GPS_spa.roosting_ZOOM_year, "Spatial")
  
  # KernelUD
  kud.roosting_ZOOM_year <- kernelUD(locs_spa.roosting_ZOOM_year["year"], 
                                      grid = SpatialPixels_ZOOM, 
                                      h = mean(c(h.silverman_x_roosting_ZOOM_year, 
                                                 h_silverman_y_roosting_ZOOM_year)))
  
  kud_list.roosting_ZOOM_year <- lapply(names(kud.roosting_ZOOM_year), function(year) {
    
    print(year)
    
    # Extraire l'estimation de densité pour un ID spécifique
    kud_single.roosting_ZOOM_year <- kud.roosting_ZOOM_year[[year]]
    rast.roosting_ZOOM_year <- rast(kud_single.roosting_ZOOM_year)
    courtour.roosting_ZOOM_year <- as.contour(rast.roosting_ZOOM_year)
    sf.roosting_ZOOM_year <- st_as_sf(courtour.roosting_ZOOM_year)
    cast.roosting_ZOOM_year <- st_cast(sf.roosting_ZOOM_year, "POLYGON")
    cast.roosting_ZOOM_year$year <- year
    
    return(cast.roosting_ZOOM_year)
  })
  
  kud_all.roosting_ZOOM_year <- do.call(rbind, kud_list.roosting_ZOOM_year)
  kud_all.roosting_ZOOM_year$year <- as.factor(kud_all.roosting_ZOOM_year$year)
  kud_all.roosting_ZOOM_year$ZOOM <- lettre
  results_kud.roosting_ZOOM_year <- rbind(results_kud.roosting_ZOOM_year, kud_all.roosting_ZOOM_year)
  
}

# write
st_write(results_kud.roosting_ZOOM_year, paste0(data_generated_path, "results_kud.roosting_ZOOM_year.gpkg"), append = FALSE)
# read
results_kud.roosting_ZOOM_year <- st_read(file.path(data_generated_path, "results_kud.roosting_ZOOM_year.gpkg"))

# plot
tmap_mode("view")
UDMap_roosting_year_ZOOM <- tm_scalebar() +
  tm_shape(RMO) +
  tm_polygons() +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(ZOOM_A) +
  tm_polygons(fill_alpha = 0.1, fill = "grey") +
  tm_text("A", size = 1.5) +
  tm_shape(ZOOM_B) +
  tm_polygons(fill_alpha = 0.1, fill = "grey") +
  tm_text("B", size = 1.5) +
  tm_shape(ZOOM_C) +
  tm_polygons(fill_alpha = 0.1, fill = "grey") +
  tm_text("C", size = 1.5) +
  tm_shape(ZOOM_D) +
  tm_polygons(fill_alpha = 0.1, fill = "grey") +
  tm_text("D", size = 1.5) +
  tm_shape(ZOOM_E) +
  tm_polygons(fill_alpha = 0.1, fill = "grey") +
  tm_text("E", size = 1.5) +
  tm_shape(BOX_2154) +
  tm_borders(col = "black") +
  tm_shape(results_kud.roosting_ZOOM_year) + 
  tm_facets("year") + 
  tm_polygons(border.col = "grey", fill = "level", fill_alpha = 0.2, 
              palette = viridis::viridis(10, begin = 0, end = 1, 
                                         direction = 1, option = "plasma")) +
  tm_facets("year") +
  tm_shape(terre_mer) +
  tm_lines(col = "lightblue", lwd = 0.1) + 
  tm_shape(zero_hydro) +
  tm_lines("layer", col = "darkblue", lwd = 0.5, legend.show = FALSE, 
           title.col = "Elevation"); UDMap_roosting_year_ZOOM

###                        ###
### Repétabilité inter-year / population scale ###
###                        ###

GPS.year_repet_pop <- GPS %>% 
  filter(behavior == "roosting") %>% 
  dplyr::select(datetime,lon,lat,year) %>% 
  st_drop_geometry() %>% 
  na.omit()

# au moins 5 point par group
n_per_year <- GPS.year_repet_pop %>% 
  group_by(year) %>% 
  summarize(n = n())%>% 
  filter(n <= 5)

# reverse of %in%  
`%ni%` <- Negate(`%in%`)

GPS.year_repet_pop <- GPS.year_repet_pop %>% 
  filter(year %ni% n_per_year$year)

# Transformer en objet spatial (EPSG:4326)
GPS_spa.year_repet_pop <- st_as_sf(GPS.year_repet_pop, coords = c("lon", "lat"), crs = 4326)
GPS_spa.year_repet_pop <- st_transform(GPS_spa.year_repet_pop, crs = 32630)

# raster/grid
crs_utm <- "EPSG:32630"
SpatRaster <- project(raster_100x100, crs_utm)
RasterLayer <- raster(SpatRaster)
SpatialPixels <- as(RasterLayer, "SpatialPixels")

# Extraire les coordonnées reprojetées
coords.year_repet_pop <- st_coordinates(GPS_spa.year_repet_pop)

# Règle de Silverman
sigma_x.roosting_year_repet_pop <- sd(coords.year_repet_pop[,1])
sigma_y_roosting_year_repet_pop <- sd(coords.year_repet_pop[,2])
n.roosting_year_repet_pop <- nrow(GPS_spa.year_repet_pop)

h.silverman_x_roosting_year_repet_pop <- 1.06 * sigma_x.roosting_year_repet_pop * n.roosting_year_repet_pop^(-1/5)
h.silverman_y_roosting_year_repet_pop <- 1.06 * sigma_y_roosting_year_repet_pop * n.roosting_year_repet_pop^(-1/5)

GPS_spa.year_repet_pop <- as(GPS_spa.year_repet_pop, "Spatial")

kud.roosting_year_repet_pop <- kernelUD(GPS_spa.year_repet_pop["year"], 
                                         grid = as(SpatialPixels, "SpatialPixels"),
                                         h = mean(c(h.silverman_x_roosting_year_repet_pop,
                                                    h.silverman_y_roosting_year_repet_pop)))

##                     ##
## valeur répétabilité ##
##                     ##

overlap.roosting_year_repet_pop <- kerneloverlaphr(kud.roosting_year_repet_pop, method = "BA")
mean_overlap.roosting_year_repet_pop <- mean(overlap.roosting_year_repet_pop, na.rm = T) ; mean

# overlap_matrix
min_val <- min(overlap.roosting_year_repet_pop, na.rm = TRUE)
max_val <- max(overlap.roosting_year_repet_pop, na.rm = TRUE)
ordre <- c("2018","2019","2020","2021","2022","2023","2024")
overlap.roosting_year_repet_pop <- overlap.roosting_year_repet_pop[ordre, ordre]

plot.overlapp_roosting_year_repet_pop <- ggcorrplot(overlap.roosting_year_repet_pop,
                                                     hc.order = FALSE,
                                                     method = "circle",
                                                     type = "lower",
                                                     lab = TRUE,
                                                     digits = 1,
                                                     colors = c("white", "yellow", "red"),
                                                     ggtheme = theme_minimal()) +
  scale_fill_gradientn(colors = c("white", "yellow", "red"),
                       limits = c(min(tt_ordered, na.rm = TRUE), 
                                  max(tt_ordered, na.rm = TRUE))) ; plot.overlapp_roosting_year_repet_pop

##               ##
## UDMap par ind ##
##               ##

# Estimation UDmap par ind par year

# Créer une liste pour stocker les résultats
UDmaps_list.roosting_year_repet_pop <- lapply(names(kud.roosting_year_repet_pop), function(year) {
  
  print(year)
  
  # Extraire l'estimation de densité pour un ID spécifique
  kud_single.roosting_year_repet_pop <- kud.roosting_year_repet_pop[[year]]
  rast.roosting_year_repet_pop <- rast(kud_single.roosting_year_repet_pop)
  contour.roosting_year_repet_pop <- as.contour(rast.roosting_year_repet_pop)
  sf.roosting_year_repet_pop <- st_as_sf(contour.roosting_year_repet_pop)
  cast.roosting_year_repet_pop <- st_cast(sf.roosting_year_repet_pop, "POLYGON")
  cast.roosting_year_repet_pop$year <- year
  
  return(cast.roosting_year_repet_pop)
  
})

# Fusionner tous les ID dans un seul objet sf
results_kud.roosting_year_repet_pop <- do.call(rbind, UDmaps_list.roosting_year_repet_pop)
results_kud.roosting_year_repet_pop$year <- as.factor(results_kud.roosting_year_repet_pop$year)
# results_kud.roosting_year_repet_pop$ID <- sub("_.*", "", results_kud.roosting_year_repet_pop$year)
# results_kud.roosting_year_repet_pop$year <- droplevels(results_kud.roosting_year_repet_pop$year)
# results_kud.roosting_year_repet_pop$Periode <- sub(".*_", "", results_kud.roosting_year_repet_pop$year)
# results_kud.roosting_year_repet_pop$ID <- as.factor(results_kud.roosting_year_repet_pop$ID)

# plot 
tmap_mode("view")

UDMap.roosting_year_repet_pop <- tm_shape(RMO) +
  tm_polygons() +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(results_kud.roosting_year_repet_pop) + 
  # tm_facets("year") +
  tm_polygons(border.col = "grey", fill = "year", fill_alpha = 0.2) ; UDMap.roosting_year_repet_pop


###                        ###
### Repétabilité inter-year / individual scale ###
###                        ###

GPS.year_repet <- GPS %>% 
  filter(behavior == "roosting") %>% 
  dplyr::select(ID,datetime,lon,lat,year) %>% 
  mutate(ID_year = paste0(ID, "_", year)) %>% 
  st_drop_geometry() %>% 
  na.omit()

# au moins 5 point par group
n_per_year <- GPS.year_repet %>% 
  group_by(ID_year) %>% 
  summarize(n = n())%>% 
  filter(n <= 5)

# reverse of %in%  
`%ni%` <- Negate(`%in%`)

GPS.year_repet <- GPS.year_repet %>% 
  filter(ID_year %ni% n_per_year$ID_year)

# Transformer en objet spatial (EPSG:4326)
GPS_spa.year_repet <- st_as_sf(GPS.year_repet, coords = c("lon", "lat"), crs = 4326)
GPS_spa.year_repet <- st_transform(GPS_spa.year_repet, crs = 32630)

# raster/grid
crs_utm <- "EPSG:32630"
SpatRaster <- project(raster_100x100, crs_utm)
RasterLayer <- raster(SpatRaster)
SpatialPixels <- as(RasterLayer, "SpatialPixels")

# Extraire les coordonnées reprojetées
coords.year_repet <- st_coordinates(GPS_spa.year_repet)

# Règle de Silverman
sigma_x.roosting_year_repet <- sd(coords.year_repet[,1])
sigma_y_roosting_year_repet <- sd(coords.year_repet[,2])
n.roosting_year_repet <- nrow(GPS_spa.year_repet)

h.silverman_x_roosting_year_repet <- 1.06 * sigma_x.roosting_year_repet * n.roosting_year_repet^(-1/5)
h.silverman_y_roosting_year_repet <- 1.06 * sigma_y_roosting_year_repet * n.roosting_year_repet^(-1/5)

GPS_spa.year_repet <- as(GPS_spa.year_repet, "Spatial")

kud.roosting_year_repet <- kernelUD(GPS_spa.year_repet["ID_year"], 
                                     grid = as(SpatialPixels, "SpatialPixels"),
                                     h = mean(c(h.silverman_x_roosting_year_repet,
                                                h.silverman_y_roosting_year_repet)))

##                     ##
## valeur répétabilité ##
##                     ##

# Estimation valeur d'overlapp par ind entre chaque year

# Extraire les noms uniques des individus
individus <- unique(GPS_spa.year_repet$ID)

# Stocker les résultats
overlap_results.roosting_year_repet = NULL

# Boucle sur chaque individu
for (ind in individus) {
  
  print(ind)
  
  # Trouver les noms des périodes de cet individu dans hr_kde
  ID_periodes <- names(kud.roosting_year_repet)[grep(paste0("^", ind, "_"), names(kud.roosting_year_repet))]
  
  # Vérifier que l'individu a bien deux périodes
  if (length(ID_periodes) == 2) {
  # Créer un estUDm valide
  hr_kde_ind.roosting_year_repet <- kud.roosting_year_repet[ID_periodes]
  class(hr_kde_ind.roosting_year_repet) <- "estUDm"  # Important pour que kerneloverlaphr() fonctionne
  
  # Calculer l'overlap entre les deux périodes
  overlap_value.roosting_year_repet <- kerneloverlaphr(hr_kde_ind.roosting_year_repet, 
                                                        method = "BA")[1, 2]
  
  info_ind.roosting_year_repet <- c(ind, overlap_value.roosting_year_repet)
  
  # Stocker le résultat
  # overlap_results <- rbind(overlap_results, data.frame(Individu = ind, Overlap = overlap_value))
  overlap_results.roosting_year_repet <- rbind(overlap_results.roosting_year_repet, info_ind.roosting_year_repet)
  
  }
}

overlap_results.roosting_year_repet <- as.data.frame(overlap_results.roosting_year_repet)

overlap_results.roosting_year_repet <- overlap_results.roosting_year_repet %>% 
  rename(ID = V1, overlap = V2)

overlap_results.roosting_year_repet$overlap <- as.numeric(overlap_results.roosting_year_repet$overlap)

mean_overlap.roosting_year_repet <- mean(overlap_results.roosting_year_repet$overlap, na.rm = T) ; mean_overlap.roosting_year_repet

# Afficher les résultats
overlap_results.roosting_year_repet <- overlap_results.roosting_year_repet[order(overlap_results.roosting_year_repet$overlap), ] ; overlap_results.roosting_year_repet

# plot
plot.roosting_year_repet <- ggplot(overlap_results.roosting_year_repet, aes(x=reorder(ID, overlap), y=overlap)) + 
  geom_point(shape = 19, size = 4) +
  theme_classic() +
  coord_flip() +
  theme(legend.position = "top") +
  scale_fill_manual() +
  labs(title="",
       x ="Individu", y = "Pourcentage d'overlap inter-année"); plot.roosting_year_repet

##               ##
## UDMap par ind ##
##               ##

# Estimation UDmap par ind par year

# Créer une liste pour stocker les résultats
UDmaps_list.roosting_ZOOM_year <- lapply(names(kud.roosting_year_repet), function(Individu_Periode) {
  
  print(Individu_Periode)
  
  # Extraire l'estimation de densité pour un ID spécifique
  kud_single.roosting_ZOOM_year <- kud.roosting_year_repet[[Individu_Periode]]
  rast.roosting_ZOOM_year <- rast(kud_single.roosting_ZOOM_year)
  contour.roosting_ZOOM_year <- as.contour(rast.roosting_ZOOM_year)
  sf.roosting_ZOOM_year <- st_as_sf(contour.roosting_ZOOM_year)
  cast.roosting_ZOOM_year <- st_cast(sf.roosting_ZOOM_year, "POLYGON")
  cast.roosting_ZOOM_year$Individu_Periode <- Individu_Periode
  
  return(cast.roosting_ZOOM_year)
  
})

# Fusionner tous les ID dans un seul objet sf
results_kud.roosting_ZOOM_year <- do.call(rbind, UDmaps_list.roosting_ZOOM_year)
results_kud.roosting_ZOOM_year$Individu_Periode <- as.factor(results_kud.roosting_ZOOM_year$Individu_Periode)
results_kud.roosting_ZOOM_year$ID <- sub("_.*", "", results_kud.roosting_ZOOM_year$Individu_Periode)
results_kud.roosting_ZOOM_year$Individu_Periode <- droplevels(results_kud.roosting_ZOOM_year$Individu_Periode)
results_kud.roosting_ZOOM_year$Periode <- sub(".*_", "", results_kud.roosting_ZOOM_year$Individu_Periode)
results_kud.roosting_ZOOM_year$ID <- as.factor(results_kud.roosting_ZOOM_year$ID)

# plot 
tmap_mode("view")

UDMap_roosting_rep_inter_year <- tm_shape(RMO) +
  tm_polygons() +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(results_kud.roosting_ZOOM_year) + 
  tm_facets("ID", drop.units = TRUE) +
  tm_polygons(border.col = "grey", fill = "Periode", fill_alpha = 0.2)  +
  tm_layout(legend.outside = TRUE, legend.show = TRUE); UDMap_roosting_rep_inter_year

## Age -------------------------------------------------------------------------

### GLOB -----------------------------------------------------------------------

# GPS point roosting ---

# All zone
GPS.roosting_glob_age <- GPS %>% 
  filter(behavior == "roosting") %>% 
  dplyr::select(lon,lat,age) %>% 
  st_drop_geometry() %>% 
  na.omit()

GPS_spa.roosting_glob_age <- st_as_sf(GPS.roosting_glob_age, coords = c("lon", "lat"), crs = 4326)
GPS_spa.roosting_glob_age <- st_transform(GPS_spa.roosting_glob_age, crs = 32630) 
GPS_coords.roosting_glob_age <- st_coordinates(GPS_spa.roosting_glob_age)

# raster/grid
crs_utm <- "EPSG:32630"
SpatRaster <- project(raster_100x100, crs_utm)
RasterLayer <- raster(SpatRaster)
SpatialPixels<- as(RasterLayer, "SpatialPixels") 

# Règle de Silverman
sigma_x.roosting_glob_age <- sd(GPS_coords.roosting_glob_age[,1]) 
sigma_y.roosting_glob_age <- sd(GPS_coords.roosting_glob_age[,2]) 
n.roosting_glob_age <- nrow(GPS.roosting_glob_age) 
h.silverman_x_roosting_glob_age <- 1.06 * sigma_x.roosting_glob_age * n.roosting_glob_age^(-1/5) / 2
h.silverman_y_roosting_glob_age <- 1.06 * sigma_y.roosting_glob_age * n.roosting_glob_age^(-1/5) / 2
locs_spa.roosting_glob_age <- as(GPS_spa.roosting_glob_age, "Spatial")

# KernelUD
kud.roosting_glob_age <- kernelUD(locs_spa.roosting_glob_age["age"], 
                                      grid = SpatialPixels, 
                                      h = mean(c(h.silverman_x_roosting_glob_age, h.silverman_y_roosting_glob_age)))

kud.list_roosting_glob_age <- lapply(names(kud.roosting_glob_age), function(age) {
  
  print(age)
  
  # Extraire l'estimation de densité pour un ID spécifique
  kud_simple.roosting_glob_age <- kud.roosting_glob_age[[age]]
  rast.roosting_glob_age <- rast(kud_simple.roosting_glob_age)
  courtour.roosting_glob_age <- as.contour(rast.roosting_glob_age)
  sf.roosting_glob_age <- st_as_sf(courtour.roosting_glob_age)
  cast.roosting_glob_age <- st_cast(sf.roosting_glob_age, "POLYGON")
  cast.roosting_glob_age$age <- age
  
  return(cast.roosting_glob_age)
})

# Fusionner tous les ID dans un seul objet sf
results_kud.roosting_glob_age <- do.call(rbind, kud.list_roosting_glob_age)
results_kud.roosting_glob_age$age <- as.factor(results_kud.roosting_glob_age$age)

# write & read
st_write(results_kud.roosting_glob_age, paste0(data_generated_path, "results_kud.roosting_glob_age.gpkg"), append = FALSE)
results_kud.roosting_glob_age <- st_read(file.path(data_generated_path, "results_kud.roosting_glob_age.gpkg"))

# plot
tmap_mode("view")
UDMap_100x100_roosting_age_glob <- tm_scalebar() +
  tm_shape(RMO) +
  tm_polygons() +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(results_kud.roosting_glob_age) + 
  tm_polygons(border.col = "grey", fill = "level", fill_alpha = 0.2, 
              palette = viridis::viridis(10, begin = 0, end = 1, 
                                         direction = 1, option = "plasma")) +
  tm_facets("age") +
  tm_shape(terre_mer) +
  tm_lines(col = "lightblue", lwd = 0.1) + 
  tm_shape(zero_hydro) +
  tm_lines("layer", col = "darkblue", lwd = 0.5, legend.show = FALSE, 
           title.col = "Elevation"); UDMap_100x100_roosting_age_glob

### ZOOM -----------------------------------------------------------------------

resolution_ZOOM = 10

crs_utm <- "EPSG:32630"
ZOOM <- c("A","B","C","D","E")
results_kud.roosting_ZOOM_age = NULL

# lettre = "A"

for (lettre in ZOOM){
  # in ZOOM
  ZOOM <- st_read(paste0(data_generated_path,"ZOOM_",lettre,".gpkg"))
  ZOOM <- st_transform(ZOOM, crs = 4326)
  GPS.ZOOM <- st_intersection(GPS, ZOOM) 
  GPS.roosting_ZOOM_age <- GPS.ZOOM %>% 
    filter(behavior == "roosting") %>% 
    dplyr::select(lon,lat,age) %>% 
    st_drop_geometry() %>% 
    na.omit()
  
  if (nrow(GPS.roosting_ZOOM_age) == 0) {
    next  # Passe directement à l'itération suivante
  }
  
  GPS_spa.roosting_ZOOM_age <- st_as_sf(GPS.roosting_ZOOM_age, coords = c("lon", "lat"), crs = 4326)
  GPS_spa.roosting_ZOOM_age <- st_transform(GPS_spa.roosting_ZOOM_age, crs = 32630) 
  GPS_coods.roosting_ZOOM_age <- st_coordinates(GPS_spa.roosting_ZOOM_age)
  
  # raster/grid
  grid_ZOOM <- st_read(paste0(data_generated_path, "grid_ZOOM_",lettre,".gpkg"))
  raster_ZOOM <- rast(grid_ZOOM, resolution = resolution_ZOOM, crs="EPSG:2154")
  SpatRaster_ZOOM <- project(raster_ZOOM, crs_utm)  
  RasterLayer_ZOOM <- raster(SpatRaster_ZOOM) 
  SpatialPixels_ZOOM <- as(RasterLayer_ZOOM, "SpatialPixels")
  
  # Règle de Silverman
  sigma_x.roosting_ZOOM_age <- sd(GPS_coods.roosting_ZOOM_age[,1]) 
  sigma_y.roosting_ZOOM_age <- sd(GPS_coods.roosting_ZOOM_age[,2]) 
  n.roosting_ZOOM_age<- nrow(GPS.roosting_ZOOM_age)  
  h.silverman_x_roosting_ZOOM_age <- 1.06 * sigma_x.roosting_ZOOM_age * n.roosting_ZOOM_age^(-1/5) / 2
  h_silverman_y_roosting_ZOOM_age <- 1.06 * sigma_y.roosting_ZOOM_age * n.roosting_ZOOM_age^(-1/5) / 2
  locs_spa.roosting_ZOOM_age <- as(GPS_spa.roosting_ZOOM_age, "Spatial")
  
  # KernelUD
  kud.roosting_ZOOM_age <- kernelUD(locs_spa.roosting_ZOOM_age["age"], 
                                grid = SpatialPixels_ZOOM, 
                                h = mean(c(h.silverman_x_roosting_ZOOM_age, 
                                           h_silverman_y_roosting_ZOOM_age)))
  
  kud_list.roosting_ZOOM_age <- lapply(names(kud.roosting_ZOOM_age), function(age) {
    
    print(age)
    
    # Extraire l'estimation de densité pour un ID spécifique
    kud_single.roosting_ZOOM_age <- kud.roosting_ZOOM_age[[age]]
    rast.roosting_ZOOM_age <- rast(kud_single.roosting_ZOOM_age)
    courtour.roosting_ZOOM_age <- as.contour(rast.roosting_ZOOM_age)
    sf.roosting_ZOOM_age <- st_as_sf(courtour.roosting_ZOOM_age)
    cast.roosting_ZOOM_age <- st_cast(sf.roosting_ZOOM_age, "POLYGON")
    cast.roosting_ZOOM_age$age <- age
    
    return(cast.roosting_ZOOM_age)
  })
  
  kud_all.roosting_ZOOM_age <- do.call(rbind, kud_list.roosting_ZOOM_age)
  kud_all.roosting_ZOOM_age$age <- as.factor(kud_all.roosting_ZOOM_age$age)
  kud_all.roosting_ZOOM_age$ZOOM <- lettre
  results_kud.roosting_ZOOM_age <- rbind(results_kud.roosting_ZOOM_age, kud_all.roosting_ZOOM_age)
  
}

# write
st_write(results_kud.roosting_ZOOM_age, paste0(data_generated_path, "results_kud.roosting_ZOOM_age.gpkg"), append = FALSE)
# read
results_kud.roosting_ZOOM_age <- st_read(file.path(data_generated_path, "results_kud.roosting_ZOOM_age.gpkg"))

# plot
tmap_mode("view")
UDMap_roosting_age_ZOOM <- tm_scalebar() +
  tm_shape(RMO) +
  tm_polygons() +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(ZOOM_A) +
  tm_polygons(fill_alpha = 0.1, fill = "grey") +
  tm_text("A", size = 1.5) +
  tm_shape(ZOOM_B) +
  tm_polygons(fill_alpha = 0.1, fill = "grey") +
  tm_text("B", size = 1.5) +
  tm_shape(ZOOM_C) +
  tm_polygons(fill_alpha = 0.1, fill = "grey") +
  tm_text("C", size = 1.5) +
  tm_shape(ZOOM_D) +
  tm_polygons(fill_alpha = 0.1, fill = "grey") +
  tm_text("D", size = 1.5) +
  tm_shape(ZOOM_E) +
  tm_polygons(fill_alpha = 0.1, fill = "grey") +
  tm_text("E", size = 1.5) +
  tm_shape(BOX_2154) +
  tm_borders(col = "black") +
  tm_shape(results_kud.roosting_ZOOM_age) + 
  tm_facets("age") + 
  tm_polygons(border.col = "grey", fill = "level", fill_alpha = 0.2, 
              palette = viridis::viridis(10, begin = 0, end = 1, 
                                         direction = 1, option = "plasma")) +
  tm_facets("age") +
  tm_shape(terre_mer) +
  tm_lines(col = "lightblue", lwd = 0.1) + 
  tm_shape(zero_hydro) +
  tm_lines("layer", col = "darkblue", lwd = 0.5, legend.show = FALSE, 
           title.col = "Elevation"); UDMap_roosting_age_ZOOM

## Sexe -------------------------------------------------------------------------

### GLOB -----------------------------------------------------------------------

# GPS point roosting ---

# All zone
GPS.roosting_glob_sex <- GPS %>% 
  filter(behavior == "roosting") %>% 
  dplyr::select(lon,lat,sex) %>% 
  st_drop_geometry() %>% 
  na.omit()

GPS_spa.roosting_glob_sex <- st_as_sf(GPS.roosting_glob_sex, coords = c("lon", "lat"), crs = 4326)
GPS_spa.roosting_glob_sex <- st_transform(GPS_spa.roosting_glob_sex, crs = 32630) 
GPS_coords.roosting_glob_sex <- st_coordinates(GPS_spa.roosting_glob_sex)

# raster/grid
crs_utm <- "EPSG:32630"
SpatRaster <- project(raster_100x100, crs_utm)
RasterLayer <- raster(SpatRaster)
SpatialPixels<- as(RasterLayer, "SpatialPixels") 

# Règle de Silverman
sigma_x.roosting_glob_sex <- sd(GPS_coords.roosting_glob_sex[,1]) 
sigma_y.roosting_glob_sex <- sd(GPS_coords.roosting_glob_sex[,2]) 
n.roosting_glob_sex <- nrow(GPS.roosting_glob_sex) 
h.silverman_x_roosting_glob_sex <- 1.06 * sigma_x.roosting_glob_sex * n.roosting_glob_sex^(-1/5) / 2
h.silverman_y_roosting_glob_sex <- 1.06 * sigma_y.roosting_glob_sex * n.roosting_glob_sex^(-1/5) / 2
locs_spa.roosting_glob_sex <- as(GPS_spa.roosting_glob_sex, "Spatial")

# KernelUD
kud.roosting_glob_sex <- kernelUD(locs_spa.roosting_glob_sex["sex"], 
                                  grid = SpatialPixels, 
                                  h = mean(c(h.silverman_x_roosting_glob_sex, h.silverman_y_roosting_glob_sex)))

kud.list_roosting_glob_sex <- lapply(names(kud.roosting_glob_sex), function(sex) {
  
  print(sex)
  
  # Extraire l'estimation de densité pour un ID spécifique
  kud_simple.roosting_glob_sex <- kud.roosting_glob_sex[[sex]]
  rast.roosting_glob_sex <- rast(kud_simple.roosting_glob_sex)
  courtour.roosting_glob_sex <- as.contour(rast.roosting_glob_sex)
  sf.roosting_glob_sex <- st_as_sf(courtour.roosting_glob_sex)
  cast.roosting_glob_sex <- st_cast(sf.roosting_glob_sex, "POLYGON")
  cast.roosting_glob_sex$sex <- sex
  
  return(cast.roosting_glob_sex)
})

# Fusionner tous les ID dans un seul objet sf
results_kud.roosting_glob_sex <- do.call(rbind, kud.list_roosting_glob_sex)
results_kud.roosting_glob_sex$sex <- as.factor(results_kud.roosting_glob_sex$sex)

# write & read
st_write(results_kud.roosting_glob_sex, paste0(data_generated_path, "results_kud.roosting_glob_sex.gpkg"), append = FALSE)
results_kud.roosting_glob_sex <- st_read(file.path(data_generated_path, "results_kud.roosting_glob_sex.gpkg"))

# plot
tmap_mode("view")
UDMap.roosting_glob_sex <- tm_scalebar() +
  tm_shape(RMO) +
  tm_polygons() +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(results_kud.roosting_glob_sex) + 
  tm_polygons(border.col = "grey", fill = "level", fill_alpha = 0.2, 
              palette = viridis::viridis(10, begin = 0, end = 1, 
                                         direction = 1, option = "plasma")) +
  tm_facets("sex") +
  tm_shape(terre_mer) +
  tm_lines(col = "lightblue", lwd = 0.1) + 
  tm_shape(zero_hydro) +
  tm_lines("layer", col = "darkblue", lwd = 0.5, legend.show = FALSE, 
           title.col = "Elevation"); UDMap.roosting_glob_sex

### ZOOM -----------------------------------------------------------------------

resolution_ZOOM = 10

crs_utm <- "EPSG:32630"
ZOOM <- c("A","B","C","D","E")
results_kud.roosting_ZOOM_sex = NULL

# lettre = "E"

for (lettre in ZOOM){
  # in ZOOM
  ZOOM <- st_read(paste0(data_generated_path,"ZOOM_",lettre,".gpkg"))
  ZOOM <- st_transform(ZOOM, crs = 4326)
  GPS.ZOOM <- st_intersection(GPS, ZOOM) 
  GPS.roosting_ZOOM_sex <- GPS.ZOOM %>% 
    filter(behavior == "roosting") %>% 
    dplyr::select(lon,lat,sex) %>% 
    st_drop_geometry() %>% 
    na.omit()
  
  if (nrow(GPS.roosting_ZOOM_sex) == 0) {
    next  # Passe directement à l'itération suivante
  }
  
  GPS_spa.roosting_ZOOM_sex <- st_as_sf(GPS.roosting_ZOOM_sex, coords = c("lon", "lat"), crs = 4326)
  GPS_spa.roosting_ZOOM_sex <- st_transform(GPS_spa.roosting_ZOOM_sex, crs = 32630) 
  GPS_coods.roosting_ZOOM_sex <- st_coordinates(GPS_spa.roosting_ZOOM_sex)
  
  # raster/grid
  grid_ZOOM <- st_read(paste0(data_generated_path, "grid_ZOOM_",lettre,".gpkg"))
  raster_ZOOM <- rast(grid_ZOOM, resolution = resolution_ZOOM, crs="EPSG:2154")
  SpatRaster_ZOOM <- project(raster_ZOOM, crs_utm)  
  RasterLayer_ZOOM <- raster(SpatRaster_ZOOM) 
  SpatialPixels_ZOOM <- as(RasterLayer_ZOOM, "SpatialPixels")
  
  # Règle de Silverman
  sigma_x.roosting_ZOOM_sex <- sd(GPS_coods.roosting_ZOOM_sex[,1]) 
  sigma_y.roosting_ZOOM_sex <- sd(GPS_coods.roosting_ZOOM_sex[,2]) 
  n.roosting_ZOOM_sex<- nrow(GPS.roosting_ZOOM_sex)  
  h.silverman_x_roosting_ZOOM_sex <- 1.06 * sigma_x.roosting_ZOOM_sex * n.roosting_ZOOM_sex^(-1/5) / 2
  h_silverman_y_roosting_ZOOM_sex <- 1.06 * sigma_y.roosting_ZOOM_sex * n.roosting_ZOOM_sex^(-1/5) / 2
  locs_spa.roosting_ZOOM_sex <- as(GPS_spa.roosting_ZOOM_sex, "Spatial")
  
  # KernelUD
  kud.roosting_ZOOM_sex <- kernelUD(locs_spa.roosting_ZOOM_sex["sex"], 
                                    grid = SpatialPixels_ZOOM, 
                                    h = mean(c(h.silverman_x_roosting_ZOOM_sex, 
                                               h_silverman_y_roosting_ZOOM_sex)))
  
  kud_list.roosting_ZOOM_sex <- lapply(names(kud.roosting_ZOOM_sex), function(sex) {
    
    print(sex)
    
    # Extraire l'estimation de densité pour un ID spécifique
    kud_single.roosting_ZOOM_sex <- kud.roosting_ZOOM_sex[[sex]]
    rast.roosting_ZOOM_sex <- rast(kud_single.roosting_ZOOM_sex)
    courtour.roosting_ZOOM_sex <- as.contour(rast.roosting_ZOOM_sex)
    sf.roosting_ZOOM_sex <- st_as_sf(courtour.roosting_ZOOM_sex)
    cast.roosting_ZOOM_sex <- st_cast(sf.roosting_ZOOM_sex, "POLYGON")
    cast.roosting_ZOOM_sex$sex <- sex
    
    return(cast.roosting_ZOOM_sex)
  })
  
  kud_all.roosting_ZOOM_sex <- do.call(rbind, kud_list.roosting_ZOOM_sex)
  kud_all.roosting_ZOOM_sex$sex <- as.factor(kud_all.roosting_ZOOM_sex$sex)
  kud_all.roosting_ZOOM_sex$ZOOM <- lettre
  results_kud.roosting_ZOOM_sex <- rbind(results_kud.roosting_ZOOM_sex, kud_all.roosting_ZOOM_sex)
  
}

# write & read
st_write(results_kud.roosting_ZOOM_sex, paste0(data_generated_path, "results_kud.roosting_ZOOM_sex.gpkg"), append = FALSE)
results_kud.roosting_ZOOM_sex <- st_read(file.path(data_generated_path, "results_kud.roosting_ZOOM_sex.gpkg"))

# plot
tmap_mode("view")
UDMap_roosting_sex_ZOOM <- tm_scalebar() +
  tm_shape(RMO) +
  tm_polygons() +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(ZOOM_A) +
  tm_polygons(fill_alpha = 0.1, fill = "grey") +
  tm_text("A", size = 1.5) +
  tm_shape(ZOOM_B) +
  tm_polygons(fill_alpha = 0.1, fill = "grey") +
  tm_text("B", size = 1.5) +
  tm_shape(ZOOM_C) +
  tm_polygons(fill_alpha = 0.1, fill = "grey") +
  tm_text("C", size = 1.5) +
  tm_shape(ZOOM_D) +
  tm_polygons(fill_alpha = 0.1, fill = "grey") +
  tm_text("D", size = 1.5) +
  tm_shape(ZOOM_E) +
  tm_polygons(fill_alpha = 0.1, fill = "grey") +
  tm_text("E", size = 1.5) +
  tm_shape(BOX_2154) +
  tm_borders(col = "black") +
  tm_shape(results_kud.roosting_ZOOM_sex) + 
  tm_facets("sex") + 
  tm_polygons(border.col = "grey", fill = "level", fill_alpha = 0.2, 
              palette = viridis::viridis(10, begin = 0, end = 1, 
                                         direction = 1, option = "plasma")) +
  tm_facets("sex") +
  tm_shape(terre_mer) +
  tm_lines(col = "lightblue", lwd = 0.1) + 
  tm_shape(zero_hydro) +
  tm_lines("layer", col = "darkblue", lwd = 0.5, legend.show = FALSE, 
           title.col = "Elevation"); UDMap_roosting_sex_ZOOM

## Jour & nuit -----------------------------------------------------------------

### GLOB -----------------------------------------------------------------------

# GPS point roosting ---

# All zone
GPS.roosting_glob_jour_nuit <- GPS %>% 
  filter(behavior == "roosting") %>% 
  dplyr::select(lon,lat,jour_nuit) %>% 
  st_drop_geometry() %>% 
  na.omit()

GPS_spa.roosting_glob_jour_nuit <- st_as_sf(GPS.roosting_glob_jour_nuit, coords = c("lon", "lat"), crs = 4326)
GPS_spa.roosting_glob_jour_nuit <- st_transform(GPS_spa.roosting_glob_jour_nuit, crs = 32630) 
GPS_coords.roosting_glob_jour_nuit <- st_coordinates(GPS_spa.roosting_glob_jour_nuit)

# raster/grid
crs_utm <- "EPSG:32630"
SpatRaster <- project(raster_100x100, crs_utm)
RasterLayer <- raster(SpatRaster)
SpatialPixels<- as(RasterLayer, "SpatialPixels") 

# Règle de Silverman
sigma_x.roosting_glob_jour_nuit <- sd(GPS_coords.roosting_glob_jour_nuit[,1]) 
sigma_y.roosting_glob_jour_nuit <- sd(GPS_coords.roosting_glob_jour_nuit[,2]) 
n.roosting_glob_jour_nuit <- nrow(GPS.roosting_glob_jour_nuit) 
h.silverman_x_roosting_glob_jour_nuit <- 1.06 * sigma_x.roosting_glob_jour_nuit * n.roosting_glob_jour_nuit^(-1/5) / 2
h.silverman_y_roosting_glob_jour_nuit <- 1.06 * sigma_y.roosting_glob_jour_nuit * n.roosting_glob_jour_nuit^(-1/5) / 2
locs_spa.roosting_glob_jour_nuit <- as(GPS_spa.roosting_glob_jour_nuit, "Spatial")

# KernelUD
kud.roosting_glob_jour_nuit <- kernelUD(locs_spa.roosting_glob_jour_nuit["jour_nuit"], 
                                  grid = SpatialPixels, 
                                  h = mean(c(h.silverman_x_roosting_glob_jour_nuit, h.silverman_y_roosting_glob_jour_nuit)))

kud.list_roosting_glob_jour_nuit <- lapply(names(kud.roosting_glob_jour_nuit), function(jour_nuit) {
  
  print(jour_nuit)
  
  # Extraire l'estimation de densité pour un ID spécifique
  kud_simple.roosting_glob_jour_nuit <- kud.roosting_glob_jour_nuit[[jour_nuit]]
  rast.roosting_glob_jour_nuit <- rast(kud_simple.roosting_glob_jour_nuit)
  courtour.roosting_glob_jour_nuit <- as.contour(rast.roosting_glob_jour_nuit)
  sf.roosting_glob_jour_nuit <- st_as_sf(courtour.roosting_glob_jour_nuit)
  cast.roosting_glob_jour_nuit <- st_cast(sf.roosting_glob_jour_nuit, "POLYGON")
  cast.roosting_glob_jour_nuit$jour_nuit <- jour_nuit
  
  return(cast.roosting_glob_jour_nuit)
})

# Fusionner tous les ID dans un seul objet sf
results_kud.roosting_glob_jour_nuit <- do.call(rbind, kud.list_roosting_glob_jour_nuit)
results_kud.roosting_glob_jour_nuit$jour_nuit <- as.factor(results_kud.roosting_glob_jour_nuit$jour_nuit)

# write & read
st_write(results_kud.roosting_glob_jour_nuit, paste0(data_generated_path, "results_kud.roosting_glob_jour_nuit.gpkg"), append = FALSE)
results_kud.roosting_glob_jour_nuit <- st_read(file.path(data_generated_path, "results_kud.roosting_glob_jour_nuit.gpkg"))

# plot
tmap_mode("view")
UDMap.roosting_glob_jour_nuit <- tm_scalebar() +
  tm_shape(RMO) +
  tm_polygons() +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(results_kud.roosting_glob_jour_nuit) + 
  tm_polygons(border.col = "grey", fill = "level", fill_alpha = 0.2, 
              palette = viridis::viridis(10, begin = 0, end = 1, 
                                         direction = 1, option = "plasma")) +
  tm_facets("jour_nuit") +
  tm_shape(terre_mer) +
  tm_lines(col = "lightblue", lwd = 0.1) + 
  tm_shape(zero_hydro) +
  tm_lines("layer", col = "darkblue", lwd = 0.5, legend.show = FALSE, 
           title.col = "Elevation"); UDMap.roosting_glob_jour_nuit

### ZOOM -----------------------------------------------------------------------

resolution_ZOOM = 10

crs_utm <- "EPSG:32630"
ZOOM <- c("A","B","C","D","E")
results_kud.roosting_ZOOM_jour_nuit = NULL

# lettre = "E"

for (lettre in ZOOM){
  # in ZOOM
  ZOOM <- st_read(paste0(data_generated_path,"ZOOM_",lettre,".gpkg"))
  ZOOM <- st_transform(ZOOM, crs = 4326)
  GPS.ZOOM <- st_intersection(GPS, ZOOM) 
  GPS.roosting_ZOOM_jour_nuit <- GPS.ZOOM %>% 
    filter(behavior == "roosting") %>% 
    dplyr::select(lon,lat,jour_nuit) %>% 
    st_drop_geometry() %>% 
    na.omit()
  
  if (nrow(GPS.roosting_ZOOM_jour_nuit) == 0) {
    next  # Passe directement à l'itération suivante
  }
  
  GPS_spa.roosting_ZOOM_jour_nuit <- st_as_sf(GPS.roosting_ZOOM_jour_nuit, coords = c("lon", "lat"), crs = 4326)
  GPS_spa.roosting_ZOOM_jour_nuit <- st_transform(GPS_spa.roosting_ZOOM_jour_nuit, crs = 32630) 
  GPS_coods.roosting_ZOOM_jour_nuit <- st_coordinates(GPS_spa.roosting_ZOOM_jour_nuit)
  
  # raster/grid
  grid_ZOOM <- st_read(paste0(data_generated_path, "grid_ZOOM_",lettre,".gpkg"))
  raster_ZOOM <- rast(grid_ZOOM, resolution = resolution_ZOOM, crs="EPSG:2154")
  SpatRaster_ZOOM <- project(raster_ZOOM, crs_utm)  
  RasterLayer_ZOOM <- raster(SpatRaster_ZOOM) 
  SpatialPixels_ZOOM <- as(RasterLayer_ZOOM, "SpatialPixels")
  
  # Règle de Silverman
  sigma_x.roosting_ZOOM_jour_nuit <- sd(GPS_coods.roosting_ZOOM_jour_nuit[,1]) 
  sigma_y.roosting_ZOOM_jour_nuit <- sd(GPS_coods.roosting_ZOOM_jour_nuit[,2]) 
  n.roosting_ZOOM_jour_nuit<- nrow(GPS.roosting_ZOOM_jour_nuit)  
  h.silverman_x_roosting_ZOOM_jour_nuit <- 1.06 * sigma_x.roosting_ZOOM_jour_nuit * n.roosting_ZOOM_jour_nuit^(-1/5) / 2
  h_silverman_y_roosting_ZOOM_jour_nuit <- 1.06 * sigma_y.roosting_ZOOM_jour_nuit * n.roosting_ZOOM_jour_nuit^(-1/5) / 2
  locs_spa.roosting_ZOOM_jour_nuit <- as(GPS_spa.roosting_ZOOM_jour_nuit, "Spatial")
  
  # KernelUD
  kud.roosting_ZOOM_jour_nuit <- kernelUD(locs_spa.roosting_ZOOM_jour_nuit["jour_nuit"], 
                                    grid = SpatialPixels_ZOOM, 
                                    h = mean(c(h.silverman_x_roosting_ZOOM_jour_nuit, 
                                               h_silverman_y_roosting_ZOOM_jour_nuit)))
  
  kud_list.roosting_ZOOM_jour_nuit <- lapply(names(kud.roosting_ZOOM_jour_nuit), function(jour_nuit) {
    
    print(jour_nuit)
    
    # Extraire l'estimation de densité pour un ID spécifique
    kud_single.roosting_ZOOM_jour_nuit <- kud.roosting_ZOOM_jour_nuit[[jour_nuit]]
    rast.roosting_ZOOM_jour_nuit <- rast(kud_single.roosting_ZOOM_jour_nuit)
    courtour.roosting_ZOOM_jour_nuit <- as.contour(rast.roosting_ZOOM_jour_nuit)
    sf.roosting_ZOOM_jour_nuit <- st_as_sf(courtour.roosting_ZOOM_jour_nuit)
    cast.roosting_ZOOM_jour_nuit <- st_cast(sf.roosting_ZOOM_jour_nuit, "POLYGON")
    cast.roosting_ZOOM_jour_nuit$jour_nuit <- jour_nuit
    
    return(cast.roosting_ZOOM_jour_nuit)
  })
  
  kud_all.roosting_ZOOM_jour_nuit <- do.call(rbind, kud_list.roosting_ZOOM_jour_nuit)
  kud_all.roosting_ZOOM_jour_nuit$jour_nuit <- as.factor(kud_all.roosting_ZOOM_jour_nuit$jour_nuit)
  kud_all.roosting_ZOOM_jour_nuit$ZOOM <- lettre
  results_kud.roosting_ZOOM_jour_nuit <- rbind(results_kud.roosting_ZOOM_jour_nuit, kud_all.roosting_ZOOM_jour_nuit)
  
}

# write & read
st_write(results_kud.roosting_ZOOM_jour_nuit, paste0(data_generated_path, "results_kud.roosting_ZOOM_jour_nuit.gpkg"), append = FALSE)
results_kud.roosting_ZOOM_jour_nuit <- st_read(file.path(data_generated_path, "results_kud.roosting_ZOOM_jour_nuit.gpkg"))

# plot
tmap_mode("view")
UDMap_roosting_jour_nuit_ZOOM <- tm_scalebar() +
  tm_shape(RMO) +
  tm_polygons() +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(ZOOM_A) +
  tm_polygons(fill_alpha = 0.1, fill = "grey") +
  tm_text("A", size = 1.5) +
  tm_shape(ZOOM_B) +
  tm_polygons(fill_alpha = 0.1, fill = "grey") +
  tm_text("B", size = 1.5) +
  tm_shape(ZOOM_C) +
  tm_polygons(fill_alpha = 0.1, fill = "grey") +
  tm_text("C", size = 1.5) +
  tm_shape(ZOOM_D) +
  tm_polygons(fill_alpha = 0.1, fill = "grey") +
  tm_text("D", size = 1.5) +
  tm_shape(ZOOM_E) +
  tm_polygons(fill_alpha = 0.1, fill = "grey") +
  tm_text("E", size = 1.5) +
  tm_shape(BOX_2154) +
  tm_borders(col = "black") +
  tm_shape(results_kud.roosting_ZOOM_jour_nuit) + 
  tm_facets("jour_nuit") + 
  tm_polygons(border.col = "grey", fill = "level", fill_alpha = 0.2, 
              palette = viridis::viridis(10, begin = 0, end = 1, 
                                         direction = 1, option = "plasma")) +
  tm_facets("jour_nuit") +
  tm_shape(terre_mer) +
  tm_lines(col = "lightblue", lwd = 0.1) + 
  tm_shape(zero_hydro) +
  tm_lines("layer", col = "darkblue", lwd = 0.5, legend.show = FALSE, 
           title.col = "Elevation"); UDMap_roosting_jour_nuit_ZOOM

## Brèche ----------------------------------------------------------------------

resolution_ZOOM = 10

crs_utm <- "EPSG:32630"
ZOOM <- c("A","B","C","D","E")
results_kud.roosting_ZOOM_breche = NULL

# lettre = "E"

for (lettre in ZOOM){
  # in ZOOM
  ZOOM <- st_read(paste0(data_generated_path,"ZOOM_",lettre,".gpkg"))
  ZOOM <- st_transform(ZOOM, crs = 4326)
  GPS.ZOOM <- st_intersection(GPS, ZOOM) 
  GPS.roosting_ZOOM_breche <- GPS.ZOOM %>% 
    filter(behavior == "roosting") %>% 
    dplyr::select(lon,lat,breche) %>% 
    st_drop_geometry() %>% 
    na.omit()
  
  if (nrow(GPS.roosting_ZOOM_breche) == 0) {
    next  # Passe directement à l'itération suivante
  }
  
  GPS_spa.roosting_ZOOM_breche <- st_as_sf(GPS.roosting_ZOOM_breche, coords = c("lon", "lat"), crs = 4326)
  GPS_spa.roosting_ZOOM_breche <- st_transform(GPS_spa.roosting_ZOOM_breche, crs = 32630) 
  GPS_coods.roosting_ZOOM_breche <- st_coordinates(GPS_spa.roosting_ZOOM_breche)
  
  # raster/grid
  grid_ZOOM <- st_read(paste0(data_generated_path, "grid_ZOOM_",lettre,".gpkg"))
  raster_ZOOM <- rast(grid_ZOOM, resolution = resolution_ZOOM, crs="EPSG:2154")
  SpatRaster_ZOOM <- project(raster_ZOOM, crs_utm)  
  RasterLayer_ZOOM <- raster(SpatRaster_ZOOM) 
  SpatialPixels_ZOOM <- as(RasterLayer_ZOOM, "SpatialPixels")
  
  # Règle de Silverman
  sigma_x.roosting_ZOOM_breche <- sd(GPS_coods.roosting_ZOOM_breche[,1]) 
  sigma_y.roosting_ZOOM_breche <- sd(GPS_coods.roosting_ZOOM_breche[,2]) 
  n.roosting_ZOOM_breche<- nrow(GPS.roosting_ZOOM_breche)  
  h.silverman_x_roosting_ZOOM_breche <- 1.06 * sigma_x.roosting_ZOOM_breche * n.roosting_ZOOM_breche^(-1/5) / 2
  h_silverman_y_roosting_ZOOM_breche <- 1.06 * sigma_y.roosting_ZOOM_breche * n.roosting_ZOOM_breche^(-1/5) / 2
  locs_spa.roosting_ZOOM_breche <- as(GPS_spa.roosting_ZOOM_breche, "Spatial")
  
  # KernelUD
  kud.roosting_ZOOM_breche <- kernelUD(locs_spa.roosting_ZOOM_breche["breche"], 
                                          grid = SpatialPixels_ZOOM, 
                                          h = mean(c(h.silverman_x_roosting_ZOOM_breche, 
                                                     h_silverman_y_roosting_ZOOM_breche)))
  
  kud_list.roosting_ZOOM_breche <- lapply(names(kud.roosting_ZOOM_breche), function(breche) {
    
    print(breche)
    
    # Extraire l'estimation de densité pour un ID spécifique
    kud_single.roosting_ZOOM_breche <- kud.roosting_ZOOM_breche[[breche]]
    rast.roosting_ZOOM_breche <- rast(kud_single.roosting_ZOOM_breche)
    courtour.roosting_ZOOM_breche <- as.contour(rast.roosting_ZOOM_breche)
    sf.roosting_ZOOM_breche <- st_as_sf(courtour.roosting_ZOOM_breche)
    cast.roosting_ZOOM_breche <- st_cast(sf.roosting_ZOOM_breche, "POLYGON")
    cast.roosting_ZOOM_breche$breche <- breche
    
    return(cast.roosting_ZOOM_breche)
  })
  
  kud_all.roosting_ZOOM_breche <- do.call(rbind, kud_list.roosting_ZOOM_breche)
  kud_all.roosting_ZOOM_breche$breche <- as.factor(kud_all.roosting_ZOOM_breche$breche)
  kud_all.roosting_ZOOM_breche$ZOOM <- lettre
  results_kud.roosting_ZOOM_breche <- rbind(results_kud.roosting_ZOOM_breche, kud_all.roosting_ZOOM_breche)
  
}

# write & read
st_write(results_kud.roosting_ZOOM_breche, paste0(data_generated_path, "results_kud.roosting_ZOOM_breche.gpkg"), append = FALSE)
results_kud.roosting_ZOOM_breche <- st_read(file.path(data_generated_path, "results_kud.roosting_ZOOM_breche.gpkg"))

# plot
tmap_mode("view")
UDMap_roosting_breche_ZOOM <- tm_scalebar() +
  tm_shape(RMO) +
  tm_polygons() +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(ZOOM_A) +
  tm_polygons(fill_alpha = 0.1, fill = "grey") +
  tm_text("A", size = 1.5) +
  tm_shape(ZOOM_B) +
  tm_polygons(fill_alpha = 0.1, fill = "grey") +
  tm_text("B", size = 1.5) +
  tm_shape(ZOOM_C) +
  tm_polygons(fill_alpha = 0.1, fill = "grey") +
  tm_text("C", size = 1.5) +
  tm_shape(ZOOM_D) +
  tm_polygons(fill_alpha = 0.1, fill = "grey") +
  tm_text("D", size = 1.5) +
  tm_shape(ZOOM_E) +
  tm_polygons(fill_alpha = 0.1, fill = "grey") +
  tm_text("E", size = 1.5) +
  tm_shape(BOX_2154) +
  tm_borders(col = "black") +
  tm_shape(results_kud.roosting_ZOOM_breche) + 
  tm_facets("breche") + 
  tm_polygons(border.col = "grey", fill = "level", fill_alpha = 0.2, 
              palette = viridis::viridis(10, begin = 0, end = 1, 
                                         direction = 1, option = "plasma")) +
  tm_facets("breche") +
  tm_shape(terre_mer) +
  tm_lines(col = "lightblue", lwd = 0.1) + 
  tm_shape(zero_hydro) +
  tm_lines("layer", col = "darkblue", lwd = 0.5, legend.show = FALSE, 
           title.col = "Elevation"); UDMap_roosting_breche_ZOOM


#### (!!!!!!!!répétabilité) ----

# Charger les données en lat/lon (EPSG:4326)
coords_roosting_ID_year <- GPS %>% 
  filter(behavior == "roosting") %>% 
  dplyr::select(ID,year,lon,lat) %>% 
  mutate(ID_year = paste0(ID, "_", year)) %>% 
  st_drop_geometry() %>% 
  na.omit()

# au moins 5 point par an
n_per_year_per_ind <- coords_roosting_ID_year %>% 
  group_by(ID, year) %>% 
  summarize(n = n()) %>% 
  filter(n <=5) %>%
  mutate(ID_year = paste0(ID, "_", year))

# reverse of %in%  
`%ni%` <- Negate(`%in%`)

coords_roosting_ID_year <- coords_roosting_ID_year %>% 
  filter(ID_year %ni% n_per_year_per_ind$ID_year)

# au moins 3 années de présence sur site 
n_year <- coords_roosting_ID_year %>% 
  dplyr::select(ID, year) %>% 
  group_by(ID) %>% 
  distinct() %>% 
  # mutate(year = as.character(year)) %>% 
  summarize(n = n()) %>% 
  filter(n < 3)

coords_roosting_ID_year <- coords_roosting_ID_year %>% 
  filter(ID %ni% n_year$ID)

# Transformer en objet spatial (EPSG:4326)
locs_roosting_ID_year <- st_as_sf(coords_roosting_ID_year, coords = c("lon", "lat"), crs = 4326)

# Reprojeter en système métrique (ex. UTM zone 30N - EPSG:32630 pour la France)
locs_roosting_ID_year_32630 <- st_transform(locs_roosting_ID_year, crs = 32630)  # Adapter le CRS à votre région

# Reprojection du raster
crs_utm <- CRS("+init=epsg:32630") # Définir le CRS cible (EPSG:32630 = UTM zone 30N)
raster_100x100_32630 <- projectRaster(raster_100x100, crs = crs_utm)
crs(raster_100x100_32630)

# Extraire les coordonnées reprojetées
coords_roosting_ID_year_32630 <- st_coordinates(locs_roosting_ID_year_32630)

# Règle de Silverman
sigma_x_roosting_ID_year <- sd(coords_roosting_ID_year_32630[,1])  # Écart-type en X (mètres)
sigma_y_roosting_ID_year <- sd(coords_roosting_ID_year_32630[,2])  # Écart-type en Y (mètres)
n_roosting_ID_year <- nrow(coords_roosting_ID_year_32630)  # Nombre de points

h_silverman_x_roosting_ID_year <- 1.06 * sigma_x_roosting_ID_year * n_roosting_ID_year^(-1/5)
h_silverman_y_roosting_ID_year <- 1.06 * sigma_y_roosting_ID_year * n_roosting_ID_year^(-1/5)

cat("h optimal en mètres pour X:", h_silverman_x_roosting_ID_year, "\n")
cat("h optimal en mètres pour Y:", h_silverman_y_roosting_ID_year, "\n")

# locs_spa <- as(locs_m, "Spatial")

# locs_spa <- st_transform(locs_roosting_ID_year, crs = 32630)
locs_spa_roosting_ID_year <- as(locs_roosting_ID_year_32630, "Spatial")









locs_spa_roosting_ID_year$Periode <- ifelse(locs_spa_roosting_ID_year$year <= 2020, "Periode1", "Periode2")


# Créer une colonne combinée
locs_spa_roosting_ID_year$Individu_Periode <- paste(locs_spa_roosting_ID_year$ID, locs_spa_roosting_ID_year$Periode, sep = "_")

# Vérifier que les noms sont bien générés
unique(locs_spa_roosting_ID_year$Individu_Periode)




# Calculer les KDE en séparant par individu et période
# hr_kde <- kernelUD(locs_spa_roosting_ID_year[c("ID", "Periode")], h = "href", grid = 500)

hr_kde <- kernelUD(locs_spa_roosting_ID_year["Individu_Periode"], grid = as(raster_100x100_32630, "SpatialPixels"),
                   h = mean(c(h_silverman_x_roosting_ID_year, h_silverman_y_roosting_ID_year)))


# Extraire les noms uniques des individus
individus <- unique(locs_spa_roosting_ID_year$ID)


# Stocker les résultats
overlap_results <- data.frame(Individu = character(), Overlap = numeric())

# Boucle sur chaque individu
for (ind in individus) {
  # Trouver les noms des périodes de cet individu dans hr_kde
  ID_periodes <- names(hr_kde)[grep(paste0("^", ind, "_"), names(hr_kde))]
  
  # Vérifier que l'individu a bien deux périodes
  if (length(ID_periodes) == 2) { # pas des individus...
    # Créer un estUDm valide
    hr_kde_ind <- hr_kde[ID_periodes]
    class(hr_kde_ind) <- "estUDm"  # Important pour que kerneloverlaphr() fonctionne
    
    # Calculer l'overlap entre les deux périodes
    overlap_value <- kerneloverlaphr(hr_kde_ind, method = "BA")[1, 2]
    
    # Stocker le résultat
    overlap_results <- rbind(overlap_results, data.frame(Individu = ind, Overlap = overlap_value))
  }
}

# Afficher les résultats
print(overlap_results)


#### (!!!!!!!!similarité avant/après) ----

# Charger les données en lat/lon (EPSG:4326)
coords_roosting <- GPS %>% 
  filter(behavior == "roosting") %>% 
  dplyr::select(ID,year,lon,lat,breche) %>% 
  mutate(ID_year = paste0(ID, "_", year),
         breche = case_when(breche == "digue intacte" ~ "fermee",
                            breche == "ouverture complète" ~ "ouverte",
                            breche == "ouverture progressive" ~ "ouverte"),
         ID_breche = paste0(ID, "_", breche)) %>% 
  st_drop_geometry() %>% 
  na.omit()

# au moins 5 point avant/après breche
n_per_breche <- coords_roosting %>% 
  group_by(ID, breche) %>% 
  summarize(n = n()) %>% 
  filter(n <=5) %>%
  mutate(ID_breche = paste0(ID, "_", breche))

# reverse of %in%  
`%ni%` <- Negate(`%in%`)

coords_roosting <- coords_roosting %>% 
  filter(ID_breche %ni% n_per_breche$ID_breche)

# Transformer en objet spatial (EPSG:4326)
locs_roosting <- st_as_sf(coords_roosting, coords = c("lon", "lat"), crs = 4326)

# Reprojeter en système métrique (ex. UTM zone 30N - EPSG:32630 pour la France)
locs_roosting_32630 <- st_transform(locs_roosting, crs = 32630)  # Adapter le CRS à votre région

# Reprojection du raster
crs_utm <- CRS("+init=epsg:32630") # Définir le CRS cible (EPSG:32630 = UTM zone 30N)
raster_100x100_32630 <- projectRaster(raster_100x100, crs = crs_utm)
crs(raster_100x100_32630)

# Extraire les coordonnées reprojetées
coords_roosting_32630 <- st_coordinates(locs_roosting_32630)

# Règle de Silverman
sigma_x_roosting <- sd(coords_roosting_32630[,1])  # Écart-type en X (mètres)
sigma_y_roosting <- sd(coords_roosting_32630[,2])  # Écart-type en Y (mètres)
n_roosting <- nrow(coords_roosting_32630)  # Nombre de points

h_silverman_x_roosting <- 1.06 * sigma_x_roosting * n_roosting^(-1/5)
h_silverman_y_roosting <- 1.06 * sigma_y_roosting * n_roosting^(-1/5)

cat("h optimal en mètres pour X:", h_silverman_x_roosting, "\n")
cat("h optimal en mètres pour Y:", h_silverman_y_roosting, "\n")

locs_spa_roosting <- as(locs_roosting_32630, "Spatial")

locs_spa_roosting$Periode <- ifelse(locs_spa_roosting$breche == "fermee", "Periode1", "Periode2")

# Créer une colonne combinée
locs_spa_roosting$Individu_Periode <- paste(locs_spa_roosting$ID, locs_spa_roosting$Periode, sep = "_")

# Vérifier que les noms sont bien générés
unique(locs_spa_roosting$Individu_Periode)

# Calculer les KDE en séparant par individu et période

hr_kde <- kernelUD(locs_spa_roosting["Individu_Periode"], grid = as(raster_100x100_32630, "SpatialPixels"),
                   h = mean(c(h_silverman_x_roosting, h_silverman_y_roosting)))

# Extraire les noms uniques des individus
individus <- unique(locs_spa_roosting$ID)

# Stocker les résultats
overlap_results <- data.frame(Individu = character(), Overlap = numeric())

ind = "EC103792"

# Boucle sur chaque individu
for (ind in individus) {
  # Trouver les noms des périodes de cet individu dans hr_kde
  ID_periodes <- names(hr_kde)[grep(paste0("^", ind, "_"), names(hr_kde))]
  
  # Vérifier que l'individu a bien deux périodes
  if (length(ID_periodes) == 2) {
    # Créer un estUDm valide
    hr_kde_ind <- hr_kde[ID_periodes]
    class(hr_kde_ind) <- "estUDm"  # Important pour que kerneloverlaphr() fonctionne
    
    # Calculer l'overlap entre les deux périodes
    overlap_value <- kerneloverlaphr(hr_kde_ind, method = "BA")[1, 2]
    
    # Stocker le résultat
    overlap_results <- rbind(overlap_results, data.frame(Individu = ind, Overlap = overlap_value))
  }
}

# Afficher les résultats
overlap_results <- overlap_results[order(overlap_results$Overlap), ] ; overlap_results

# Créer une liste pour stocker les résultats
UDmaps_list_breche <- lapply(names(hr_kde), function(Individu_Periode) {
  
  print(Individu_Periode)
  
  # Extraire l'estimation de densité pour un ID spécifique
  kud_single_breche <- hr_kde[[Individu_Periode]]
  rast_breche <- rast(kud_single_breche)
  contour_breche <- as.contour(rast_breche)
  sf_breche <- st_as_sf(contour_breche)
  cast_breche <- st_cast(sf_breche, "POLYGON")
  cast_breche$Individu_Periode <- Individu_Periode
  
  return(cast_breche)
})

# Fusionner tous les ID dans un seul objet sf
UDMap_final_breche <- do.call(rbind, UDmaps_list_breche)

UDMap_final_breche$Individu_Periode <- as.factor(UDMap_final_breche$Individu_Periode)
UDMap_final_breche$ID <- sub("_.*", "", UDMap_final_breche$Individu_Periode)

# UDMap_final_breche$ID <- substring(UDMap_final_breche$Individu_Periode, first=1, last=8)
UDMap_final_breche$Individu_Periode <- droplevels(UDMap_final_breche$Individu_Periode)

UDMap_final_breche$Periode <- sub(".*_", "", UDMap_final_breche$Individu_Periode)

# UDMap_final_breche$Periode <- substring(UDMap_final_breche$Individu_Periode, first=10, last=18)
UDMap_final_breche$ID <- as.factor(UDMap_final_breche$ID)

tmap_mode("view")

UDMap_breche <- tm_shape(RMO) +
  tm_polygons() +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(UDMap_final_breche) + 
  tm_facets("ID") +
  tm_polygons(border.col = "grey", fill = "Periode", fill_fill_alpha = 0.2) ; UDMap_breche


## Type de marée haute ---------------------------------------------------------

resolution_ZOOM = 10

crs_utm <- "EPSG:32630"
ZOOM <- c("A","B","C","D","E")
results_kud.roosting_ZOOM_tides_high_type = NULL

# lettre = "E"

for (lettre in ZOOM){
  # in ZOOM
  ZOOM <- st_read(paste0(data_generated_path,"ZOOM_",lettre,".gpkg"))
  ZOOM <- st_transform(ZOOM, crs = 4326)
  GPS.ZOOM <- st_intersection(GPS, ZOOM) 
  GPS.roosting_ZOOM_tides_high_type <- GPS.ZOOM %>% 
    filter(behavior == "roosting") %>% 
    dplyr::select(lon,lat,tides_high_type) %>% 
    st_drop_geometry() %>% 
    na.omit()
  
  if (nrow(GPS.roosting_ZOOM_tides_high_type) == 0) {
    next  # Passe directement à l'itération suivante
  }
  
  GPS_spa.roosting_ZOOM_tides_high_type <- st_as_sf(GPS.roosting_ZOOM_tides_high_type, coords = c("lon", "lat"), crs = 4326)
  GPS_spa.roosting_ZOOM_tides_high_type <- st_transform(GPS_spa.roosting_ZOOM_tides_high_type, crs = 32630) 
  GPS_coods.roosting_ZOOM_tides_high_type <- st_coordinates(GPS_spa.roosting_ZOOM_tides_high_type)
  
  # raster/grid
  grid_ZOOM <- st_read(paste0(data_generated_path, "grid_ZOOM_",lettre,".gpkg"))
  raster_ZOOM <- rast(grid_ZOOM, resolution = resolution_ZOOM, crs="EPSG:2154")
  SpatRaster_ZOOM <- project(raster_ZOOM, crs_utm)  
  RasterLayer_ZOOM <- raster(SpatRaster_ZOOM) 
  SpatialPixels_ZOOM <- as(RasterLayer_ZOOM, "SpatialPixels")
  
  # Règle de Silverman
  sigma_x.roosting_ZOOM_tides_high_type <- sd(GPS_coods.roosting_ZOOM_tides_high_type[,1]) 
  sigma_y.roosting_ZOOM_tides_high_type <- sd(GPS_coods.roosting_ZOOM_tides_high_type[,2]) 
  n.roosting_ZOOM_tides_high_type<- nrow(GPS.roosting_ZOOM_tides_high_type)  
  h.silverman_x_roosting_ZOOM_tides_high_type <- 1.06 * sigma_x.roosting_ZOOM_tides_high_type * n.roosting_ZOOM_tides_high_type^(-1/5) / 2
  h_silverman_y_roosting_ZOOM_tides_high_type <- 1.06 * sigma_y.roosting_ZOOM_tides_high_type * n.roosting_ZOOM_tides_high_type^(-1/5) / 2
  locs_spa.roosting_ZOOM_tides_high_type <- as(GPS_spa.roosting_ZOOM_tides_high_type, "Spatial")
  
  # KernelUD
  kud.roosting_ZOOM_tides_high_type <- kernelUD(locs_spa.roosting_ZOOM_tides_high_type["tides_high_type"], 
                                       grid = SpatialPixels_ZOOM, 
                                       h = mean(c(h.silverman_x_roosting_ZOOM_tides_high_type, 
                                                  h_silverman_y_roosting_ZOOM_tides_high_type)))
  
  kud_list.roosting_ZOOM_tides_high_type <- lapply(names(kud.roosting_ZOOM_tides_high_type), function(tides_high_type) {
    
    print(tides_high_type)
    
    # Extraire l'estimation de densité pour un ID spécifique
    kud_single.roosting_ZOOM_tides_high_type <- kud.roosting_ZOOM_tides_high_type[[tides_high_type]]
    rast.roosting_ZOOM_tides_high_type <- rast(kud_single.roosting_ZOOM_tides_high_type)
    courtour.roosting_ZOOM_tides_high_type <- as.contour(rast.roosting_ZOOM_tides_high_type)
    sf.roosting_ZOOM_tides_high_type <- st_as_sf(courtour.roosting_ZOOM_tides_high_type)
    cast.roosting_ZOOM_tides_high_type <- st_cast(sf.roosting_ZOOM_tides_high_type, "POLYGON")
    cast.roosting_ZOOM_tides_high_type$tides_high_type <- tides_high_type
    
    return(cast.roosting_ZOOM_tides_high_type)
  })
  
  kud_all.roosting_ZOOM_tides_high_type <- do.call(rbind, kud_list.roosting_ZOOM_tides_high_type)
  kud_all.roosting_ZOOM_tides_high_type$tides_high_type <- as.factor(kud_all.roosting_ZOOM_tides_high_type$tides_high_type)
  kud_all.roosting_ZOOM_tides_high_type$ZOOM <- lettre
  results_kud.roosting_ZOOM_tides_high_type <- rbind(results_kud.roosting_ZOOM_tides_high_type, kud_all.roosting_ZOOM_tides_high_type)
  
}

# write & read
st_write(results_kud.roosting_ZOOM_tides_high_type, paste0(data_generated_path, "results_kud.roosting_ZOOM_tides_high_type.gpkg"), append = FALSE)
results_kud.roosting_ZOOM_tides_high_type <- st_read(file.path(data_generated_path, "results_kud.roosting_ZOOM_tides_high_type.gpkg"))

# plot
tmap_mode("view")
UDMap_roosting_tides_high_type_ZOOM <- tm_scalebar() +
  tm_shape(RMO) +
  tm_polygons() +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(ZOOM_A) +
  tm_polygons(fill_alpha = 0.1, fill = "grey") +
  tm_text("A", size = 1.5) +
  tm_shape(ZOOM_B) +
  tm_polygons(fill_alpha = 0.1, fill = "grey") +
  tm_text("B", size = 1.5) +
  tm_shape(ZOOM_C) +
  tm_polygons(fill_alpha = 0.1, fill = "grey") +
  tm_text("C", size = 1.5) +
  tm_shape(ZOOM_D) +
  tm_polygons(fill_alpha = 0.1, fill = "grey") +
  tm_text("D", size = 1.5) +
  tm_shape(ZOOM_E) +
  tm_polygons(fill_alpha = 0.1, fill = "grey") +
  tm_text("E", size = 1.5) +
  tm_shape(BOX_2154) +
  tm_borders(col = "black") +
  tm_shape(results_kud.roosting_ZOOM_tides_high_type) + 
  tm_facets("tides_high_type") + 
  tm_polygons(border.col = "grey", fill = "level", fill_alpha = 0.2, 
              palette = viridis::viridis(10, begin = 0, end = 1, 
                                         direction = 1, option = "plasma")) +
  tm_facets("tides_high_type") +
  tm_shape(terre_mer) +
  tm_lines(col = "lightblue", lwd = 0.1) + 
  tm_shape(zero_hydro) +
  tm_lines("layer", col = "darkblue", lwd = 0.5, legend.show = FALSE, 
           title.col = "Elevation"); UDMap_roosting_tides_high_type_ZOOM

#### (!!!!!!!!répétabilité) ----

# Charger les données en lat/lon (EPSG:4326)
coords_roosting_ID_year <- GPS %>% 
  filter(behavior == "roosting") %>% 
  dplyr::select(ID,year,lon,lat) %>% 
  mutate(ID_year = paste0(ID, "_", year)) %>% 
  st_drop_geometry() %>% 
  na.omit()

# au moins 5 point par an
n_per_year_per_ind <- coords_roosting_ID_year %>% 
  group_by(ID, year) %>% 
  summarize(n = n()) %>% 
  filter(n <=5) %>%
  mutate(ID_year = paste0(ID, "_", year))

# reverse of %in%  
`%ni%` <- Negate(`%in%`)

coords_roosting_ID_year <- coords_roosting_ID_year %>% 
  filter(ID_year %ni% n_per_year_per_ind$ID_year)

# au moins 3 années de présence sur site 
n_year <- coords_roosting_ID_year %>% 
  dplyr::select(ID, year) %>% 
  group_by(ID) %>% 
  distinct() %>% 
  # mutate(year = as.character(year)) %>% 
  summarize(n = n()) %>% 
  filter(n < 3)

coords_roosting_ID_year <- coords_roosting_ID_year %>% 
  filter(ID %ni% n_year$ID)

# Transformer en objet spatial (EPSG:4326)
locs_roosting_ID_year <- st_as_sf(coords_roosting_ID_year, coords = c("lon", "lat"), crs = 4326)

# Reprojeter en système métrique (ex. UTM zone 30N - EPSG:32630 pour la France)
locs_roosting_ID_year_32630 <- st_transform(locs_roosting_ID_year, crs = 32630)  # Adapter le CRS à votre région

# Reprojection du raster
crs_utm <- CRS("+init=epsg:32630") # Définir le CRS cible (EPSG:32630 = UTM zone 30N)
raster_100x100_32630 <- projectRaster(raster_100x100, crs = crs_utm)
crs(raster_100x100_32630)

# Extraire les coordonnées reprojetées
coords_roosting_ID_year_32630 <- st_coordinates(locs_roosting_ID_year_32630)

# Règle de Silverman
sigma_x_roosting_ID_year <- sd(coords_roosting_ID_year_32630[,1])  # Écart-type en X (mètres)
sigma_y_roosting_ID_year <- sd(coords_roosting_ID_year_32630[,2])  # Écart-type en Y (mètres)
n_roosting_ID_year <- nrow(coords_roosting_ID_year_32630)  # Nombre de points

h_silverman_x_roosting_ID_year <- 1.06 * sigma_x_roosting_ID_year * n_roosting_ID_year^(-1/5)
h_silverman_y_roosting_ID_year <- 1.06 * sigma_y_roosting_ID_year * n_roosting_ID_year^(-1/5)

cat("h optimal en mètres pour X:", h_silverman_x_roosting_ID_year, "\n")
cat("h optimal en mètres pour Y:", h_silverman_y_roosting_ID_year, "\n")

# locs_spa <- as(locs_m, "Spatial")

# locs_spa <- st_transform(locs_roosting_ID_year, crs = 32630)
locs_spa_roosting_ID_year <- as(locs_roosting_ID_year_32630, "Spatial")









locs_spa_roosting_ID_year$Periode <- ifelse(locs_spa_roosting_ID_year$year <= 2021, "Periode1", "Periode2")


# Créer une colonne combinée
locs_spa_roosting_ID_year$Individu_Periode <- paste(locs_spa_roosting_ID_year$ID, locs_spa_roosting_ID_year$Periode, sep = "_")

# Vérifier que les noms sont bien générés
unique(locs_spa_roosting_ID_year$Individu_Periode)




# Calculer les KDE en séparant par individu et période
# hr_kde <- kernelUD(locs_spa_roosting_ID_year[c("ID", "Periode")], h = "href", grid = 500)

hr_kde <- kernelUD(locs_spa_roosting_ID_year["Individu_Periode"], grid = as(raster_100x100_32630, "SpatialPixels"),
                   h = mean(c(h_silverman_x_roosting_ID_year, h_silverman_y_roosting_ID_year)))


# Extraire les noms uniques des individus
individus <- unique(locs_spa_roosting_ID_year$ID)


# Stocker les résultats
overlap_results <- data.frame(Individu = character(), Overlap = numeric())

# Boucle sur chaque individu
for (ind in individus) {
  # Trouver les noms des périodes de cet individu dans hr_kde
  ID_periodes <- names(hr_kde)[grep(paste0("^", ind, "_"), names(hr_kde))]
  
  # Vérifier que l'individu a bien deux périodes
  if (length(ID_periodes) == 2) {
    # Créer un estUDm valide
    hr_kde_ind <- hr_kde[ID_periodes]
    class(hr_kde_ind) <- "estUDm"  # Important pour que kerneloverlaphr() fonctionne
    
    # Calculer l'overlap entre les deux périodes
    overlap_value <- kerneloverlaphr(hr_kde_ind, method = "BA")[1, 2]
    
    # Stocker le résultat
    overlap_results <- rbind(overlap_results, data.frame(Individu = ind, Overlap = overlap_value))
  }
}

# Afficher les résultats
print(overlap_results)

###
####
# ***FORAGING*** ---------------------------------------------------------------
####
###

## GLOB -----------------------------------------------------------------------

GPS.foraging_glob <- GPS %>% 
  filter(behavior == "foraging") %>% 
  dplyr::select(lon,lat) %>% 
  st_drop_geometry() %>% 
  na.omit()

GPS.foraging_spa <- st_as_sf(GPS.foraging_glob, coords = c("lon", "lat"), crs = 4326)
GPS.foraging_spa <- st_transform(GPS.foraging_spa, crs = 32630)  
GPS.foraging_coords <- st_coordinates(GPS.foraging_spa) 

# raster/grid
crs_utm <- "EPSG:32630"
SpatRaster <- project(raster_100x100, crs_utm)
RasterLayer <- raster(SpatRaster)
SpatialPixels <- as(RasterLayer, "SpatialPixels")

# Règle de Silverman
sigma_x.foraging_glob <- sd(GPS.foraging_coords[,1]) 
sigma_y.foraging_glob <- sd(GPS.foraging_coords[,2]) 
n.foraging_glob <- nrow(GPS.foraging_glob) 
h_silverman_x.foraging_glob <- 1.06 * sigma_x.foraging_glob * n.foraging_glob^(-1/5) / 2
h_silverman_y.foraging_glob <- 1.06 * sigma_y.foraging_glob * n.foraging_glob^(-1/5) / 2
locs_spa.foraging_glob <- as(GPS.foraging_spa, "Spatial")

# KernelUD
kud.foraging_glob <- kernelUD(locs_spa.foraging_glob, 
                              grid = SpatialPixels, 
                              h = mean(c(h_silverman_x.foraging_glob, 
                                         h_silverman_y.foraging_glob)))

# Isoclines 
rast.foraging_glob <- rast(kud.foraging_glob)
courtour.foraging_glob <- as.contour(rast.foraging_glob)
sf.foraging_glob <- st_as_sf(courtour.foraging_glob)
results_kud.foraging_glob <- st_cast(sf.foraging_glob, "POLYGON")

# write & read
st_write(results_kud.foraging_glob, paste0(data_generated_path, "results_kud.foraging_glob.gpkg"), append = FALSE)
results_kud.foraging_glob <- st_read(file.path(data_generated_path, "results_kud.foraging_glob.gpkg"))

# plot 
tmap_mode("view")
UDMap_foraging_glob <- tm_scalebar() +
  tm_shape(RMO) +
  tm_polygons() +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(results_kud.foraging_glob) +
  tm_polygons(border.col = "grey", fill = "level", fill_alpha = 0.2,
              palette = viridis::viridis(10, begin = 0, end = 1,
                                         direction = 1, option = "plasma")) +
  tm_shape(terre_mer) +
  tm_lines(col = "lightblue", lwd = 0.1) + 
  tm_shape(zero_hydro) +
  tm_lines("layer", col = "darkblue", lwd = 0.5, legend.show = FALSE, 
           title.col = "Elevation"); UDMap_foraging_glob

## ZOOM -----------------------------------------------------------------------

resolution_ZOOM = 10

crs_utm <- "EPSG:32630"
ZOOM <- c("A","B","C","D","E")
results_kud.foraging_ZOOM = NULL

# lettre = "A"

for (lettre in ZOOM){
  
  # in ZOOM
  ZOOM <- st_read(paste0(data_generated_path,"ZOOM_",lettre,".gpkg"))
  ZOOM <- st_transform(ZOOM, crs = 4326)
  GPS.ZOOM <- st_intersection(GPS, ZOOM) 
  GPS.foraging_ZOOM <- GPS.ZOOM %>% 
    filter(behavior == "foraging") %>% 
    dplyr::select(lon,lat) %>% 
    st_drop_geometry() %>% 
    na.omit()
  
  GPS_spa.foraging_ZOOM <- st_as_sf(GPS.foraging_ZOOM, coords = c("lon", "lat"), crs = 4326)
  GPS_spa.foraging_ZOOM <- st_transform(GPS_spa.foraging_ZOOM, crs = 32630)  
  GPS_coords.foraging_ZOOM <- st_coordinates(GPS_spa.foraging_ZOOM) 
  
  # raster/grid
  grid.ZOOM <- st_read(paste0(data_generated_path, "grid_ZOOM_",lettre,".gpkg"))
  raster.ZOOM <- rast(grid.ZOOM, resolution = resolution_ZOOM, crs="EPSG:2154")
  SpatRaster.ZOOM <- project(raster.ZOOM, crs_utm)
  RasterLayer.ZOOM <- raster(SpatRaster.ZOOM)
  SpatialPixels.ZOOM <- as(RasterLayer.ZOOM, "SpatialPixels") 
  
  # Règle de Silverman
  sigma_x.foraging_ZOOM <- sd(GPS_coords.foraging_ZOOM[,1]) 
  sigma_y.foraging_ZOOM <- sd(GPS_coords.foraging_ZOOM[,2]) 
  n.foraging_ZOOM <- nrow(GPS.foraging_ZOOM)
  h.silverman_x_foraging_ZOOM <- 1.06 * sigma_x.foraging_ZOOM * n.foraging_ZOOM^(-1/5) / 2
  h.silverman_y_foraging_ZOOM <- 1.06 * sigma_y.foraging_ZOOM * n.foraging_ZOOM^(-1/5) / 2
  locs_spa.foraging_ZOOM <- as(GPS_spa.foraging_ZOOM, "Spatial")
  
  # KernelUD
  kud.foraging_ZOOM <- kernelUD(locs_spa.foraging_ZOOM, 
                                grid = SpatialPixels.ZOOM, 
                                h = mean(c(h.silverman_x_foraging_ZOOM, 
                                           h.silverman_y_foraging_ZOOM)))
  
  # Isoclines 
  rast.foraging_ZOOM <- rast(kud.foraging_ZOOM)
  courtour.foraging_ZOOM <- as.contour(rast.foraging_ZOOM)
  sf.foraging_ZOOM <- st_as_sf(courtour.foraging_ZOOM)
  cast.foraging_ZOOM <- st_cast(sf.foraging_ZOOM, "POLYGON")
  cast.foraging_ZOOM$ZOOM <- lettre
  results_kud.foraging_ZOOM <- rbind(results_kud.foraging_ZOOM, cast.foraging_ZOOM)
  
}

# write & read
st_write(results_kud.foraging_ZOOM, paste0(data_generated_path, "results_kud.foraging_ZOOM.gpkg"), append = FALSE)
results_kud.foraging_ZOOM <- st_read(file.path(data_generated_path, "results_kud.foraging_ZOOM.gpkg"))

# plot
tmap_mode("view")
UDMap_foraging_ZOOM <- tm_scalebar() +
  tm_shape(RMO) +
  tm_polygons() +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(ZOOM_A) +
  tm_polygons(fill_alpha = 0.1, fill = "grey") +
  tm_text("A", size = 1.5) +
  tm_shape(ZOOM_B) +
  tm_polygons(fill_alpha = 0.1, fill = "grey") +
  tm_text("B", size = 1.5) +
  tm_shape(ZOOM_C) +
  tm_polygons(fill_alpha = 0.1, fill = "grey") +
  tm_text("C", size = 1.5) +
  tm_shape(ZOOM_D) +
  tm_polygons(fill_alpha = 0.1, fill = "grey") +
  tm_text("D", size = 1.5) +
  tm_shape(ZOOM_E) +
  tm_polygons(fill_alpha = 0.1, fill = "grey") +
  tm_text("E", size = 1.5) +
  tm_shape(BOX_2154) +
  tm_borders(col = "black") +
  tm_shape(results_kud.foraging_ZOOM) + 
  tm_polygons(border.col = "grey", fill = "level", fill_alpha = 0.2, 
              palette = viridis::viridis(10, begin = 0, end = 1, 
                                         direction = 1, option = "plasma")) +
  tm_shape(terre_mer) +
  tm_lines(col = "lightblue", lwd = 0.1) + 
  tm_shape(zero_hydro) +
  tm_lines("layer", col = "darkblue", lwd = 0.5, legend.show = FALSE, 
           title.col = "Elevation"); UDMap_foraging_ZOOM

## Month -----------------------------------------------------------------------

resolution_ZOOM = 10

crs_utm <- "EPSG:32630"
ZOOM <- c("A","B","C","D","E")
results_kud.foraging_ZOOM_month = NULL

# lettre = "B"

for (lettre in ZOOM){
  # in ZOOM
  ZOOM <- st_read(paste0(data_generated_path,"ZOOM_",lettre,".gpkg"))
  ZOOM <- st_transform(ZOOM, crs = 4326)
  GPS.ZOOM <- st_intersection(GPS, ZOOM) 
  GPS.foraging_ZOOM_month <- GPS.ZOOM %>% 
    filter(behavior == "foraging") %>% 
    dplyr::select(lon,lat,month_label) %>% 
    st_drop_geometry() %>% 
    na.omit()
  
  if (nrow(GPS.foraging_ZOOM_month) == 0) {
    next  # Passe directement à l'itération suivante
  }
  
  nb_row <- GPS.foraging_ZOOM_month %>% 
    group_by(month_label) %>%
    summarise(n = n(), .groups = "drop")
  
  if (min(nb_row$n) < 5) {
    next  # Passe directement à l'itération suivante
  }
  
  # Crée une table avec tous les mois possibles
  all_months <- tibble(
    month_label = c("janv", "févr", "mars", "avr", "mai", "juin",
                    "juil", "août", "sept", "oct", "nov", "déc")
  )
  
  # Compte les occurrences par mois dans tes données
  nb_row <- GPS.foraging_ZOOM_month %>%
    group_by(month_label) %>%
    summarise(n = n(), .groups = "drop")
  
  # Joint tous les mois et remplit avec 0 si manquant
  nb_row_complet <- all_months %>%
    left_join(nb_row, by = "month_label") %>%
    mutate(n = if_else(is.na(n), 0L, n))
  
  if (min(nb_row_complet$n) < 5) {
    next  # Passe directement à l'itération suivante
  }
  
  GPS_spa.foraging_ZOOM_month <- st_as_sf(GPS.foraging_ZOOM_month, coords = c("lon", "lat"), crs = 4326)
  GPS_spa.foraging_ZOOM_month <- st_transform(GPS_spa.foraging_ZOOM_month, crs = 32630) 
  GPS_coods.foraging_ZOOM_month <- st_coordinates(GPS_spa.foraging_ZOOM_month)
  
  # raster/grid
  grid_ZOOM <- st_read(paste0(data_generated_path, "grid_ZOOM_",lettre,".gpkg"))
  raster_ZOOM <- rast(grid_ZOOM, resolution = resolution_ZOOM, crs="EPSG:2154")
  SpatRaster_ZOOM <- project(raster_ZOOM, crs_utm)  
  RasterLayer_ZOOM <- raster(SpatRaster_ZOOM) 
  SpatialPixels_ZOOM <- as(RasterLayer_ZOOM, "SpatialPixels")
  
  # Règle de Silverman
  sigma_x.foraging_ZOOM_month <- sd(GPS_coods.foraging_ZOOM_month[,1]) 
  sigma_y.foraging_ZOOM_month <- sd(GPS_coods.foraging_ZOOM_month[,2]) 
  n.foraging_ZOOM_month <- nrow(GPS.foraging_ZOOM_month)  
  h.silverman_x_foraging_ZOOM_month <- 1.06 * sigma_x.foraging_ZOOM_month * n.foraging_ZOOM_month^(-1/5) / 2
  h_silverman_y_foraging_ZOOM_month <- 1.06 * sigma_y.foraging_ZOOM_month * n.foraging_ZOOM_month^(-1/5) / 2
  locs_spa.foraging_ZOOM_month <- as(GPS_spa.foraging_ZOOM_month, "Spatial")
  
  # KernelUD
  kud.foraging_ZOOM_month <- kernelUD(locs_spa.foraging_ZOOM_month["month_label"], 
                                      grid = SpatialPixels_ZOOM, 
                                      h = mean(c(h.silverman_x_foraging_ZOOM_month, 
                                                 h_silverman_y_foraging_ZOOM_month)))
  
  kud_list.foraging_ZOOM_month <- lapply(names(kud.foraging_ZOOM_month), function(month) {
    
    print(month)
    
    # Extraire l'estimation de densité pour un ID spécifique
    kud_single.foraging_ZOOM_month <- kud.foraging_ZOOM_month[[month]]
    rast.foraging_ZOOM_month <- rast(kud_single.foraging_ZOOM_month)
    courtour.foraging_ZOOM_month <- as.contour(rast.foraging_ZOOM_month)
    sf.foraging_ZOOM_month <- st_as_sf(courtour.foraging_ZOOM_month)
    cast.foraging_ZOOM_month <- st_cast(sf.foraging_ZOOM_month, "POLYGON")
    cast.foraging_ZOOM_month$month <- month
    
    return(cast.foraging_ZOOM_month)
  })
  
  kud_all.foraging_ZOOM_month <- do.call(rbind, kud_list.foraging_ZOOM_month)
  kud_all.foraging_ZOOM_month$month <- as.factor(kud_all.foraging_ZOOM_month$month)
  kud_all.foraging_ZOOM_month$ZOOM <- lettre
  results_kud.foraging_ZOOM_month <- rbind(results_kud.foraging_ZOOM_month, kud_all.foraging_ZOOM_month)
  
}

# write
st_write(results_kud.foraging_ZOOM_month, paste0(data_generated_path, "results_kud.foraging_ZOOM_month.gpkg"), append = FALSE)
# read
results_kud.foraging_ZOOM_month <- st_read(file.path(data_generated_path, "results_kud.foraging_ZOOM_month.gpkg"))

# plot
tmap_mode("view")
UDMap_foraging_month_ZOOM <- tm_scalebar() +
  tm_shape(RMO) +
  tm_polygons() +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(ZOOM_A) +
  tm_polygons(fill_alpha = 0.1, fill = "grey") +
  tm_text("A", size = 1.5) +
  tm_shape(ZOOM_B) +
  tm_polygons(fill_alpha = 0.1, fill = "grey") +
  tm_text("B", size = 1.5) +
  tm_shape(ZOOM_C) +
  tm_polygons(fill_alpha = 0.1, fill = "grey") +
  tm_text("C", size = 1.5) +
  tm_shape(ZOOM_D) +
  tm_polygons(fill_alpha = 0.1, fill = "grey") +
  tm_text("D", size = 1.5) +
  tm_shape(ZOOM_E) +
  tm_polygons(fill_alpha = 0.1, fill = "grey") +
  tm_text("E", size = 1.5) +
  tm_shape(BOX_2154) +
  tm_borders(col = "black") +
  tm_shape(results_kud.foraging_ZOOM_month) + 
  tm_facets("month") + 
  tm_polygons(border.col = "grey", fill = "level", fill_alpha = 0.2, 
              palette = viridis::viridis(10, begin = 0, end = 1, 
                                         direction = 1, option = "plasma")) +
  tm_facets("month") +
  tm_shape(terre_mer) +
  tm_lines(col = "lightblue", lwd = 0.1) + 
  tm_shape(zero_hydro) +
  tm_lines("layer", col = "darkblue", lwd = 0.5, legend.show = FALSE, 
           title.col = "Elevation"); UDMap_foraging_month_ZOOM

###                        ###
### Repétabilité inter-month / population scale ###
###                        ###

GPS.month_repet_pop <- GPS %>% 
  filter(behavior == "foraging") %>% 
  dplyr::select(datetime,lon,lat,month_label) %>% 
  st_drop_geometry() %>% 
  na.omit()

# au moins 5 point par group
n_per_month <- GPS.month_repet_pop %>% 
  group_by(month_label) %>% 
  summarize(n = n())%>% 
  filter(n <= 5) #%>%
# mutate(ID_month = paste0(ID, "_", month_label))

# reverse of %in%  
`%ni%` <- Negate(`%in%`)

GPS.month_repet_pop <- GPS.month_repet_pop %>% 
  filter(month_label %ni% n_per_month$month_label)

# Transformer en objet spatial (EPSG:4326)
GPS_spa.month_repet_pop <- st_as_sf(GPS.month_repet_pop, coords = c("lon", "lat"), crs = 4326)
GPS_spa.month_repet_pop <- st_transform(GPS_spa.month_repet_pop, crs = 32630)

# raster/grid
crs_utm <- "EPSG:32630"
SpatRaster <- project(raster_100x100, crs_utm)
RasterLayer <- raster(SpatRaster)
SpatialPixels <- as(RasterLayer, "SpatialPixels")

# Extraire les coordonnées reprojetées
coords.month_repet_pop <- st_coordinates(GPS_spa.month_repet_pop)

# Règle de Silverman
sigma_x.foraging_month_repet_pop <- sd(coords.month_repet_pop[,1])
sigma_y_foraging_month_repet_pop <- sd(coords.month_repet_pop[,2])
n.foraging_month_repet_pop <- nrow(GPS_spa.month_repet_pop)

h.silverman_x_foraging_month_repet_pop <- 1.06 * sigma_x.foraging_month_repet_pop * n.foraging_month_repet_pop^(-1/5)
h.silverman_y_foraging_month_repet_pop <- 1.06 * sigma_y_foraging_month_repet_pop * n.foraging_month_repet_pop^(-1/5)

GPS_spa.month_repet_pop <- as(GPS_spa.month_repet_pop, "Spatial")

kud.foraging_month_repet_pop <- kernelUD(GPS_spa.month_repet["month_label"], 
                                         grid = as(SpatialPixels, "SpatialPixels"),
                                         h = mean(c(h.silverman_x_foraging_month_repet_pop,
                                                    h.silverman_y_foraging_month_repet_pop)))

##                     ##
## valeur répétabilité ##
##                     ##

overlap.foraging_month_repet_pop <- kerneloverlaphr(kud.foraging_month_repet_pop, method = "BA")
mean_overlap.foraging_month_repet_pop <- mean(overlap.foraging_month_repet_pop, na.rm = T) ; mean

# overlap_matrix
min_val <- min(overlap.foraging_month_repet_pop, na.rm = TRUE)
max_val <- max(overlap.foraging_month_repet_pop, na.rm = TRUE)
ordre <- c("janv", "févr", "mars", "avr","mai","juin","juil","août","sept","oct","nov","déc")
overlap.foraging_month_repet_pop <- tt[ordre, ordre]

plot.overlapp_foraging_month_repet_pop <- ggcorrplot(overlap.foraging_month_repet_pop,
                                                     hc.order = FALSE,
                                                     method = "circle",
                                                     type = "lower",
                                                     lab = TRUE,
                                                     digits = 1,
                                                     colors = c("white", "yellow", "red"),
                                                     ggtheme = theme_minimal()) +
  scale_fill_gradientn(colors = c("white", "yellow", "red"),
                       limits = c(min(tt_ordered, na.rm = TRUE), 
                                  max(tt_ordered, na.rm = TRUE))) ; plot.overlapp_foraging_month_repet_pop

##               ##
## UDMap par ind ##
##               ##

# Estimation UDmap par ind par month

# Créer une liste pour stocker les résultats
UDmaps_list.foraging_ZOOM_month <- lapply(names(kud.foraging_month_repet_pop), function(Individu_Periode) {
  
  print(Individu_Periode)
  
  # Extraire l'estimation de densité pour un ID spécifique
  kud_single.foraging_ZOOM_month <- kud.foraging_month_repet_pop[[Individu_Periode]]
  rast.foraging_ZOOM_month <- rast(kud_single.foraging_ZOOM_month)
  contour.foraging_ZOOM_month <- as.contour(rast.foraging_ZOOM_month)
  sf.foraging_ZOOM_month <- st_as_sf(contour.foraging_ZOOM_month)
  cast.foraging_ZOOM_month <- st_cast(sf.foraging_ZOOM_month, "POLYGON")
  cast.foraging_ZOOM_month$Individu_Periode <- Individu_Periode
  
  return(cast.foraging_ZOOM_month)
  
})

# Fusionner tous les ID dans un seul objet sf
results_kud.foraging_ZOOM_month <- do.call(rbind, UDmaps_list.foraging_ZOOM_month)
results_kud.foraging_ZOOM_month$Individu_Periode <- as.factor(results_kud.foraging_ZOOM_month$Individu_Periode)
results_kud.foraging_ZOOM_month$ID <- sub("_.*", "", results_kud.foraging_ZOOM_month$Individu_Periode)
results_kud.foraging_ZOOM_month$Individu_Periode <- droplevels(results_kud.foraging_ZOOM_month$Individu_Periode)
results_kud.foraging_ZOOM_month$Periode <- sub(".*_", "", results_kud.foraging_ZOOM_month$Individu_Periode)
results_kud.foraging_ZOOM_month$ID <- as.factor(results_kud.foraging_ZOOM_month$ID)

# plot 
tmap_mode("view")

UDMap_foraging_rep_inter_month <- tm_shape(RMO) +
  tm_polygons() +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(results_kud.foraging_ZOOM_month) + 
  tm_facets("ID") +
  tm_polygons(border.col = "grey", fill = "Periode", fill_alpha = 0.2) ; UDMap_foraging_rep_inter_month


###                        ###
### Repétabilité inter-month / individual scale ###
###                        ###

GPS.month_repet <- GPS %>% 
  filter(behavior == "foraging") %>% 
  dplyr::select(ID,datetime,lon,lat,month_label) %>% 
  mutate(ID_month = paste0(ID, "_", month_label)) %>% 
  st_drop_geometry() %>% 
  na.omit()

# au moins 5 point par group
n_per_month <- GPS.month_repet %>% 
  group_by(ID_month) %>% 
  summarize(n = n())%>% 
  filter(n <= 5) #%>%
# mutate(ID_month = paste0(ID, "_", month_label))

# reverse of %in%  
`%ni%` <- Negate(`%in%`)

GPS.month_repet <- GPS.month_repet %>% 
  filter(ID_month %ni% n_per_month$ID_month)

# Transformer en objet spatial (EPSG:4326)
GPS_spa.month_repet <- st_as_sf(GPS.month_repet, coords = c("lon", "lat"), crs = 4326)
GPS_spa.month_repet <- st_transform(GPS_spa.month_repet, crs = 32630)

# raster/grid
crs_utm <- "EPSG:32630"
SpatRaster <- project(raster_100x100, crs_utm)
RasterLayer <- raster(SpatRaster)
SpatialPixels <- as(RasterLayer, "SpatialPixels")

# Extraire les coordonnées reprojetées
coords.month_repet <- st_coordinates(GPS_spa.month_repet)

# Règle de Silverman
sigma_x.foraging_month_repet <- sd(coords.month_repet[,1])
sigma_y_foraging_month_repet <- sd(coords.month_repet[,2])
n.foraging_month_repet <- nrow(GPS_spa.month_repet)

h.silverman_x_foraging_month_repet <- 1.06 * sigma_x.foraging_month_repet * n.foraging_month_repet^(-1/5)
h.silverman_y_foraging_month_repet <- 1.06 * sigma_y_foraging_month_repet * n.foraging_month_repet^(-1/5)

GPS_spa.month_repet <- as(GPS_spa.month_repet, "Spatial")

kud.foraging_month_repet <- kernelUD(GPS_spa.month_repet["ID_month"], 
                                     grid = as(SpatialPixels, "SpatialPixels"),
                                     h = mean(c(h.silverman_x_foraging_month_repet,
                                                h.silverman_y_foraging_month_repet)))

##                     ##
## valeur répétabilité ##
##                     ##

# Estimation valeur d'overlapp par ind entre chaque month

# Extraire les noms uniques des individus
individus <- unique(GPS_spa.month_repet$ID)

# Stocker les résultats
overlap_results.foraging_month_repet = NULL

# Boucle sur chaque individu
for (ind in individus) {
  
  print(ind)
  
  # Trouver les noms des périodes de cet individu dans hr_kde
  ID_periodes <- names(kud.foraging_month_repet)[grep(paste0("^", ind, "_"), names(kud.foraging_month_repet))]
  
  # Vérifier que l'individu a bien deux périodes
  # if (length(ID_periodes) == 2) {
  # Créer un estUDm valide
  hr_kde_ind.foraging_month_repet <- kud.foraging_month_repet[ID_periodes]
  class(hr_kde_ind.foraging_month_repet) <- "estUDm"  # Important pour que kerneloverlaphr() fonctionne
  
  # Calculer l'overlap entre les deux périodes
  overlap_value.foraging_month_repet <- kerneloverlaphr(hr_kde_ind.foraging_month_repet, 
                                                        method = "BA")[1, 2]
  
  info_ind.foraging_month_repet <- c(ind, overlap_value.foraging_month_repet)
  
  # Stocker le résultat
  # overlap_results <- rbind(overlap_results, data.frame(Individu = ind, Overlap = overlap_value))
  overlap_results.foraging_month_repet <- rbind(overlap_results.foraging_month_repet, info_ind.foraging_month_repet)
  
  # }
}

overlap_results.foraging_month_repet <- as.data.frame(overlap_results.foraging_month_repet)

overlap_results.foraging_month_repet <- overlap_results.foraging_month_repet %>% 
  rename(ID = V1, overlap = V2)

overlap_results.foraging_month_repet$overlap <- as.numeric(overlap_results.foraging_month_repet$overlap)

mean_overlap.foraging_month_repet <- mean(overlap_results.foraging_month_repet$overlap, na.rm = T) ; mean_overlap_month_over_all_ind

# Afficher les résultats
overlap_results.foraging_month_repet <- overlap_results.foraging_month_repet[order(overlap_results.foraging_month_repet$overlap), ] ; overlap_results.foraging_month_repet

# plot
plot.foraging_month_repet <- ggplot(overlap_results.foraging_month_repet, aes(x=reorder(ID, overlap), y=overlap)) + 
  geom_point(shape = 19, size = 4) +
  theme_classic() +
  coord_flip() +
  theme(legend.position = "top") +
  scale_fill_manual() +
  labs(title="",
       x ="Individu", y = "Pourcentage d'overlap inter-mois"); plot.foraging_month_repet

##               ##
## UDMap par ind ##
##               ##

# Estimation UDmap par ind par month

# Créer une liste pour stocker les résultats
UDmaps_list.foraging_ZOOM_month <- lapply(names(kud.foraging_month_repet), function(Individu_Periode) {
  
  print(Individu_Periode)
  
  # Extraire l'estimation de densité pour un ID spécifique
  kud_single.foraging_ZOOM_month <- kud.foraging_month_repet[[Individu_Periode]]
  rast.foraging_ZOOM_month <- rast(kud_single.foraging_ZOOM_month)
  contour.foraging_ZOOM_month <- as.contour(rast.foraging_ZOOM_month)
  sf.foraging_ZOOM_month <- st_as_sf(contour.foraging_ZOOM_month)
  cast.foraging_ZOOM_month <- st_cast(sf.foraging_ZOOM_month, "POLYGON")
  cast.foraging_ZOOM_month$Individu_Periode <- Individu_Periode
  
  return(cast.foraging_ZOOM_month)
  
})

# Fusionner tous les ID dans un seul objet sf
results_kud.foraging_ZOOM_month <- do.call(rbind, UDmaps_list.foraging_ZOOM_month)
results_kud.foraging_ZOOM_month$Individu_Periode <- as.factor(results_kud.foraging_ZOOM_month$Individu_Periode)
results_kud.foraging_ZOOM_month$ID <- sub("_.*", "", results_kud.foraging_ZOOM_month$Individu_Periode)
results_kud.foraging_ZOOM_month$Individu_Periode <- droplevels(results_kud.foraging_ZOOM_month$Individu_Periode)
results_kud.foraging_ZOOM_month$Periode <- sub(".*_", "", results_kud.foraging_ZOOM_month$Individu_Periode)
results_kud.foraging_ZOOM_month$ID <- as.factor(results_kud.foraging_ZOOM_month$ID)

# plot 
tmap_mode("view")

UDMap_foraging_rep_inter_month <- tm_shape(RMO) +
  tm_polygons() +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(results_kud.foraging_ZOOM_month) + 
  tm_facets("ID") +
  tm_polygons(border.col = "grey", fill = "Periode", fill_alpha = 0.2) ; UDMap_foraging_rep_inter_month

## Year -----------------------------------------------------------------------

resolution_ZOOM = 10

crs_utm <- "EPSG:32630"
ZOOM <- c("A","B","C","D","E")
results_kud.foraging_ZOOM_year = NULL

# lettre = "B"

for (lettre in ZOOM){
  # in ZOOM
  ZOOM <- st_read(paste0(data_generated_path,"ZOOM_",lettre,".gpkg"))
  ZOOM <- st_transform(ZOOM, crs = 4326)
  GPS.ZOOM <- st_intersection(GPS, ZOOM) 
  GPS.foraging_ZOOM_year <- GPS.ZOOM %>% 
    filter(behavior == "foraging") %>% 
    dplyr::select(lon,lat,year) %>% 
    st_drop_geometry() %>% 
    na.omit()
  
  if (nrow(GPS.foraging_ZOOM_year) == 0) {
    next  # Passe directement à l'itération suivante
  }
  
  nb_row <- GPS.foraging_ZOOM_year %>% 
    group_by(year) %>%
    summarise(n = n(), .groups = "drop")
  
  if (min(nb_row$n) < 5) {
    next  # Passe directement à l'itération suivante
  }
  
  # Crée une table avec tous les mois possibles
  all_year <- tibble(
    year = c(2018:2024)
  )
  
  # Compte les occurrences par mois dans tes données
  nb_row <- GPS.foraging_ZOOM_year %>%
    group_by(year) %>%
    summarise(n = n(), .groups = "drop")
  
  # Joint tous les mois et remplit avec 0 si manquant
  nb_row_complet <- all_year %>%
    left_join(nb_row, by = "year") %>%
    mutate(n = if_else(is.na(n), 0L, n))
  
  if (min(nb_row_complet$n) < 5) {
    next  # Passe directement à l'itération suivante
  }
  
  GPS_spa.foraging_ZOOM_year <- st_as_sf(GPS.foraging_ZOOM_year, coords = c("lon", "lat"), crs = 4326)
  GPS_spa.foraging_ZOOM_year <- st_transform(GPS_spa.foraging_ZOOM_year, crs = 32630) 
  GPS_coods.foraging_ZOOM_year <- st_coordinates(GPS_spa.foraging_ZOOM_year)
  
  # raster/grid
  grid_ZOOM <- st_read(paste0(data_generated_path, "grid_ZOOM_",lettre,".gpkg"))
  raster_ZOOM <- rast(grid_ZOOM, resolution = resolution_ZOOM, crs="EPSG:2154")
  SpatRaster_ZOOM <- project(raster_ZOOM, crs_utm)  
  RasterLayer_ZOOM <- raster(SpatRaster_ZOOM) 
  SpatialPixels_ZOOM <- as(RasterLayer_ZOOM, "SpatialPixels")
  
  # Règle de Silverman
  sigma_x.foraging_ZOOM_year <- sd(GPS_coods.foraging_ZOOM_year[,1]) 
  sigma_y.foraging_ZOOM_year <- sd(GPS_coods.foraging_ZOOM_year[,2]) 
  n.foraging_ZOOM_year <- nrow(GPS.foraging_ZOOM_year)  
  h.silverman_x_foraging_ZOOM_year <- 1.06 * sigma_x.foraging_ZOOM_year * n.foraging_ZOOM_year^(-1/5) / 2
  h_silverman_y_foraging_ZOOM_year <- 1.06 * sigma_y.foraging_ZOOM_year * n.foraging_ZOOM_year^(-1/5) / 2
  locs_spa.foraging_ZOOM_year <- as(GPS_spa.foraging_ZOOM_year, "Spatial")
  
  # KernelUD
  kud.foraging_ZOOM_year <- kernelUD(locs_spa.foraging_ZOOM_year["year"], 
                                     grid = SpatialPixels_ZOOM, 
                                     h = mean(c(h.silverman_x_foraging_ZOOM_year, 
                                                h_silverman_y_foraging_ZOOM_year)))
  
  kud_list.foraging_ZOOM_year <- lapply(names(kud.foraging_ZOOM_year), function(year) {
    
    print(year)
    
    # Extraire l'estimation de densité pour un ID spécifique
    kud_single.foraging_ZOOM_year <- kud.foraging_ZOOM_year[[year]]
    rast.foraging_ZOOM_year <- rast(kud_single.foraging_ZOOM_year)
    courtour.foraging_ZOOM_year <- as.contour(rast.foraging_ZOOM_year)
    sf.foraging_ZOOM_year <- st_as_sf(courtour.foraging_ZOOM_year)
    cast.foraging_ZOOM_year <- st_cast(sf.foraging_ZOOM_year, "POLYGON")
    cast.foraging_ZOOM_year$year <- year
    
    return(cast.foraging_ZOOM_year)
  })
  
  kud_all.foraging_ZOOM_year <- do.call(rbind, kud_list.foraging_ZOOM_year)
  kud_all.foraging_ZOOM_year$year <- as.factor(kud_all.foraging_ZOOM_year$year)
  kud_all.foraging_ZOOM_year$ZOOM <- lettre
  results_kud.foraging_ZOOM_year <- rbind(results_kud.foraging_ZOOM_year, kud_all.foraging_ZOOM_year)
  
}

# write
st_write(results_kud.foraging_ZOOM_year, paste0(data_generated_path, "results_kud.foraging_ZOOM_year.gpkg"), append = FALSE)
# read
results_kud.foraging_ZOOM_year <- st_read(file.path(data_generated_path, "results_kud.foraging_ZOOM_year.gpkg"))

# plot
tmap_mode("view")
UDMap_foraging_year_ZOOM <- tm_scalebar() +
  tm_shape(RMO) +
  tm_polygons() +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(ZOOM_A) +
  tm_polygons(fill_alpha = 0.1, fill = "grey") +
  tm_text("A", size = 1.5) +
  tm_shape(ZOOM_B) +
  tm_polygons(fill_alpha = 0.1, fill = "grey") +
  tm_text("B", size = 1.5) +
  tm_shape(ZOOM_C) +
  tm_polygons(fill_alpha = 0.1, fill = "grey") +
  tm_text("C", size = 1.5) +
  tm_shape(ZOOM_D) +
  tm_polygons(fill_alpha = 0.1, fill = "grey") +
  tm_text("D", size = 1.5) +
  tm_shape(ZOOM_E) +
  tm_polygons(fill_alpha = 0.1, fill = "grey") +
  tm_text("E", size = 1.5) +
  tm_shape(BOX_2154) +
  tm_borders(col = "black") +
  tm_shape(results_kud.foraging_ZOOM_year) + 
  tm_facets("year") + 
  tm_polygons(border.col = "grey", fill = "level", fill_alpha = 0.2, 
              palette = viridis::viridis(10, begin = 0, end = 1, 
                                         direction = 1, option = "plasma")) +
  tm_facets("year") +
  tm_shape(terre_mer) +
  tm_lines(col = "lightblue", lwd = 0.1) + 
  tm_shape(zero_hydro) +
  tm_lines("layer", col = "darkblue", lwd = 0.5, legend.show = FALSE, 
           title.col = "Elevation"); UDMap_foraging_year_ZOOM

###                        ###
### Repétabilité inter-year / population scale ###
###                        ###

GPS.year_repet_pop <- GPS %>% 
  filter(behavior == "foraging") %>% 
  dplyr::select(datetime,lon,lat,year) %>% 
  st_drop_geometry() %>% 
  na.omit()

# au moins 5 point par group
n_per_year <- GPS.year_repet_pop %>% 
  group_by(year) %>% 
  summarize(n = n())%>% 
  filter(n <= 5)

# reverse of %in%  
`%ni%` <- Negate(`%in%`)

GPS.year_repet_pop <- GPS.year_repet_pop %>% 
  filter(year %ni% n_per_year$year)

# Transformer en objet spatial (EPSG:4326)
GPS_spa.year_repet_pop <- st_as_sf(GPS.year_repet_pop, coords = c("lon", "lat"), crs = 4326)
GPS_spa.year_repet_pop <- st_transform(GPS_spa.year_repet_pop, crs = 32630)

# raster/grid
crs_utm <- "EPSG:32630"
SpatRaster <- project(raster_100x100, crs_utm)
RasterLayer <- raster(SpatRaster)
SpatialPixels <- as(RasterLayer, "SpatialPixels")

# Extraire les coordonnées reprojetées
coords.year_repet_pop <- st_coordinates(GPS_spa.year_repet_pop)

# Règle de Silverman
sigma_x.foraging_year_repet_pop <- sd(coords.year_repet_pop[,1])
sigma_y_foraging_year_repet_pop <- sd(coords.year_repet_pop[,2])
n.foraging_year_repet_pop <- nrow(GPS_spa.year_repet_pop)

h.silverman_x_foraging_year_repet_pop <- 1.06 * sigma_x.foraging_year_repet_pop * n.foraging_year_repet_pop^(-1/5)
h.silverman_y_foraging_year_repet_pop <- 1.06 * sigma_y_foraging_year_repet_pop * n.foraging_year_repet_pop^(-1/5)

GPS_spa.year_repet_pop <- as(GPS_spa.year_repet_pop, "Spatial")

kud.foraging_year_repet_pop <- kernelUD(GPS_spa.year_repet_pop["year"], 
                                        grid = as(SpatialPixels, "SpatialPixels"),
                                        h = mean(c(h.silverman_x_foraging_year_repet_pop,
                                                   h.silverman_y_foraging_year_repet_pop)))

##                     ##
## valeur répétabilité ##
##                     ##

overlap.foraging_year_repet_pop <- kerneloverlaphr(kud.foraging_year_repet_pop, method = "BA")
mean_overlap.foraging_year_repet_pop <- mean(overlap.foraging_year_repet_pop, na.rm = T) ; mean

# overlap_matrix
min_val <- min(overlap.foraging_year_repet_pop, na.rm = TRUE)
max_val <- max(overlap.foraging_year_repet_pop, na.rm = TRUE)
ordre <- c("2018","2019","2020","2021","2022","2023","2024")
overlap.foraging_year_repet_pop <- overlap.foraging_year_repet_pop[ordre, ordre]

plot.overlapp_foraging_year_repet_pop <- ggcorrplot(overlap.foraging_year_repet_pop,
                                                    hc.order = FALSE,
                                                    method = "circle",
                                                    type = "lower",
                                                    lab = TRUE,
                                                    digits = 1,
                                                    colors = c("white", "yellow", "red"),
                                                    ggtheme = theme_minimal()) +
  scale_fill_gradientn(colors = c("white", "yellow", "red"),
                       limits = c(min(tt_ordered, na.rm = TRUE), 
                                  max(tt_ordered, na.rm = TRUE))) ; plot.overlapp_foraging_year_repet_pop

##               ##
## UDMap par ind ##
##               ##

# Estimation UDmap par ind par year

# Créer une liste pour stocker les résultats
UDmaps_list.foraging_year_repet_pop <- lapply(names(kud.foraging_year_repet_pop), function(year) {
  
  print(year)
  
  # Extraire l'estimation de densité pour un ID spécifique
  kud_single.foraging_year_repet_pop <- kud.foraging_year_repet_pop[[year]]
  rast.foraging_year_repet_pop <- rast(kud_single.foraging_year_repet_pop)
  contour.foraging_year_repet_pop <- as.contour(rast.foraging_year_repet_pop)
  sf.foraging_year_repet_pop <- st_as_sf(contour.foraging_year_repet_pop)
  cast.foraging_year_repet_pop <- st_cast(sf.foraging_year_repet_pop, "POLYGON")
  cast.foraging_year_repet_pop$year <- year
  
  return(cast.foraging_year_repet_pop)
  
})

# Fusionner tous les ID dans un seul objet sf
results_kud.foraging_year_repet_pop <- do.call(rbind, UDmaps_list.foraging_year_repet_pop)
results_kud.foraging_year_repet_pop$year <- as.factor(results_kud.foraging_year_repet_pop$year)
# results_kud.foraging_year_repet_pop$ID <- sub("_.*", "", results_kud.foraging_year_repet_pop$year)
# results_kud.foraging_year_repet_pop$year <- droplevels(results_kud.foraging_year_repet_pop$year)
# results_kud.foraging_year_repet_pop$Periode <- sub(".*_", "", results_kud.foraging_year_repet_pop$year)
# results_kud.foraging_year_repet_pop$ID <- as.factor(results_kud.foraging_year_repet_pop$ID)

# plot 
tmap_mode("view")

UDMap.foraging_year_repet_pop <- tm_shape(RMO) +
  tm_polygons() +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(results_kud.foraging_year_repet_pop) + 
  # tm_facets("year") +
  tm_polygons(border.col = "grey", fill = "year", fill_alpha = 0.2) ; UDMap.foraging_year_repet_pop


###                        ###
### Repétabilité inter-year / individual scale ###
###                        ###

GPS.year_repet <- GPS %>% 
  filter(behavior == "foraging") %>% 
  dplyr::select(ID,datetime,lon,lat,year) %>% 
  mutate(ID_year = paste0(ID, "_", year)) %>% 
  st_drop_geometry() %>% 
  na.omit()

# au moins 5 point par group
n_per_year <- GPS.year_repet %>% 
  group_by(ID_year) %>% 
  summarize(n = n())%>% 
  filter(n <= 5)

# reverse of %in%  
`%ni%` <- Negate(`%in%`)

GPS.year_repet <- GPS.year_repet %>% 
  filter(ID_year %ni% n_per_year$ID_year)

# Transformer en objet spatial (EPSG:4326)
GPS_spa.year_repet <- st_as_sf(GPS.year_repet, coords = c("lon", "lat"), crs = 4326)
GPS_spa.year_repet <- st_transform(GPS_spa.year_repet, crs = 32630)

# raster/grid
crs_utm <- "EPSG:32630"
SpatRaster <- project(raster_100x100, crs_utm)
RasterLayer <- raster(SpatRaster)
SpatialPixels <- as(RasterLayer, "SpatialPixels")

# Extraire les coordonnées reprojetées
coords.year_repet <- st_coordinates(GPS_spa.year_repet)

# Règle de Silverman
sigma_x.foraging_year_repet <- sd(coords.year_repet[,1])
sigma_y_foraging_year_repet <- sd(coords.year_repet[,2])
n.foraging_year_repet <- nrow(GPS_spa.year_repet)

h.silverman_x_foraging_year_repet <- 1.06 * sigma_x.foraging_year_repet * n.foraging_year_repet^(-1/5)
h.silverman_y_foraging_year_repet <- 1.06 * sigma_y_foraging_year_repet * n.foraging_year_repet^(-1/5)

GPS_spa.year_repet <- as(GPS_spa.year_repet, "Spatial")

kud.foraging_year_repet <- kernelUD(GPS_spa.year_repet["ID_year"], 
                                    grid = as(SpatialPixels, "SpatialPixels"),
                                    h = mean(c(h.silverman_x_foraging_year_repet,
                                               h.silverman_y_foraging_year_repet)))

##                     ##
## valeur répétabilité ##
##                     ##

# Estimation valeur d'overlapp par ind entre chaque year

# Extraire les noms uniques des individus
individus <- unique(GPS_spa.year_repet$ID)

# Stocker les résultats
overlap_results.foraging_year_repet = NULL

# Boucle sur chaque individu
for (ind in individus) {
  
  print(ind)
  
  # Trouver les noms des périodes de cet individu dans hr_kde
  ID_periodes <- names(kud.foraging_year_repet)[grep(paste0("^", ind, "_"), names(kud.foraging_year_repet))]
  
  # Vérifier que l'individu a bien deux périodes
  if (length(ID_periodes) == 2) {
    # Créer un estUDm valide
    hr_kde_ind.foraging_year_repet <- kud.foraging_year_repet[ID_periodes]
    class(hr_kde_ind.foraging_year_repet) <- "estUDm"  # Important pour que kerneloverlaphr() fonctionne
    
    # Calculer l'overlap entre les deux périodes
    overlap_value.foraging_year_repet <- kerneloverlaphr(hr_kde_ind.foraging_year_repet, 
                                                         method = "BA")[1, 2]
    
    info_ind.foraging_year_repet <- c(ind, overlap_value.foraging_year_repet)
    
    # Stocker le résultat
    # overlap_results <- rbind(overlap_results, data.frame(Individu = ind, Overlap = overlap_value))
    overlap_results.foraging_year_repet <- rbind(overlap_results.foraging_year_repet, info_ind.foraging_year_repet)
    
  }
}

overlap_results.foraging_year_repet <- as.data.frame(overlap_results.foraging_year_repet)

overlap_results.foraging_year_repet <- overlap_results.foraging_year_repet %>% 
  rename(ID = V1, overlap = V2)

overlap_results.foraging_year_repet$overlap <- as.numeric(overlap_results.foraging_year_repet$overlap)

mean_overlap.foraging_year_repet <- mean(overlap_results.foraging_year_repet$overlap, na.rm = T) ; mean_overlap.foraging_year_repet

# Afficher les résultats
overlap_results.foraging_year_repet <- overlap_results.foraging_year_repet[order(overlap_results.foraging_year_repet$overlap), ] ; overlap_results.foraging_year_repet

# plot
plot.foraging_year_repet <- ggplot(overlap_results.foraging_year_repet, aes(x=reorder(ID, overlap), y=overlap)) + 
  geom_point(shape = 19, size = 4) +
  theme_classic() +
  coord_flip() +
  theme(legend.position = "top") +
  scale_fill_manual() +
  labs(title="",
       x ="Individu", y = "Pourcentage d'overlap inter-année"); plot.foraging_year_repet

##               ##
## UDMap par ind ##
##               ##

# Estimation UDmap par ind par year

# Créer une liste pour stocker les résultats
UDmaps_list.foraging_ZOOM_year <- lapply(names(kud.foraging_year_repet), function(Individu_Periode) {
  
  print(Individu_Periode)
  
  # Extraire l'estimation de densité pour un ID spécifique
  kud_single.foraging_ZOOM_year <- kud.foraging_year_repet[[Individu_Periode]]
  rast.foraging_ZOOM_year <- rast(kud_single.foraging_ZOOM_year)
  contour.foraging_ZOOM_year <- as.contour(rast.foraging_ZOOM_year)
  sf.foraging_ZOOM_year <- st_as_sf(contour.foraging_ZOOM_year)
  cast.foraging_ZOOM_year <- st_cast(sf.foraging_ZOOM_year, "POLYGON")
  cast.foraging_ZOOM_year$Individu_Periode <- Individu_Periode
  
  return(cast.foraging_ZOOM_year)
  
})

# Fusionner tous les ID dans un seul objet sf
results_kud.foraging_ZOOM_year <- do.call(rbind, UDmaps_list.foraging_ZOOM_year)
results_kud.foraging_ZOOM_year$Individu_Periode <- as.factor(results_kud.foraging_ZOOM_year$Individu_Periode)
results_kud.foraging_ZOOM_year$ID <- sub("_.*", "", results_kud.foraging_ZOOM_year$Individu_Periode)
results_kud.foraging_ZOOM_year$Individu_Periode <- droplevels(results_kud.foraging_ZOOM_year$Individu_Periode)
results_kud.foraging_ZOOM_year$Periode <- sub(".*_", "", results_kud.foraging_ZOOM_year$Individu_Periode)
results_kud.foraging_ZOOM_year$ID <- as.factor(results_kud.foraging_ZOOM_year$ID)

# plot 
tmap_mode("view")

UDMap_foraging_rep_inter_year <- tm_shape(RMO) +
  tm_polygons() +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(results_kud.foraging_ZOOM_year) + 
  tm_facets("ID") +
  tm_polygons(border.col = "grey", fill = "Periode", fill_alpha = 0.2) ; UDMap_foraging_rep_inter_year

## Age -------------------------------------------------------------------------

### GLOB -----------------------------------------------------------------------

# All zone
GPS.foraging_glob_age <- GPS %>% 
  filter(behavior == "foraging") %>% 
  dplyr::select(lon,lat,age) %>% 
  st_drop_geometry() %>% 
  na.omit()

GPS_spa.foraging_glob_age <- st_as_sf(GPS.foraging_glob_age, coords = c("lon", "lat"), crs = 4326)
GPS_spa.foraging_glob_age <- st_transform(GPS_spa.foraging_glob_age, crs = 32630) 
GPS_coords.foraging_glob_age <- st_coordinates(GPS_spa.foraging_glob_age)

# raster/grid
crs_utm <- "EPSG:32630"
SpatRaster <- project(raster_100x100, crs_utm)
RasterLayer <- raster(SpatRaster)
SpatialPixels<- as(RasterLayer, "SpatialPixels") 

# Règle de Silverman
sigma_x.foraging_glob_age <- sd(GPS_coords.foraging_glob_age[,1]) 
sigma_y.foraging_glob_age <- sd(GPS_coords.foraging_glob_age[,2]) 
n.foraging_glob_age <- nrow(GPS.foraging_glob_age) 
h.silverman_x_foraging_glob_age <- 1.06 * sigma_x.foraging_glob_age * n.foraging_glob_age^(-1/5) / 2
h.silverman_y_foraging_glob_age <- 1.06 * sigma_y.foraging_glob_age * n.foraging_glob_age^(-1/5) / 2
locs_spa.foraging_glob_age <- as(GPS_spa.foraging_glob_age, "Spatial")

# KernelUD
kud.foraging_glob_age <- kernelUD(locs_spa.foraging_glob_age["age"], 
                                  grid = SpatialPixels, 
                                  h = mean(c(h.silverman_x_foraging_glob_age, 
                                             h.silverman_y_foraging_glob_age)))

kud.list_foraging_glob_age <- lapply(names(kud.foraging_glob_age), function(age) {
  
  print(age)
  
  # Extraire l'estimation de densité pour un ID spécifique
  kud_simple.foraging_glob_age <- kud.foraging_glob_age[[age]]
  rast.foraging_glob_age <- rast(kud_simple.foraging_glob_age)
  courtour.foraging_glob_age <- as.contour(rast.foraging_glob_age)
  sf.foraging_glob_age <- st_as_sf(courtour.foraging_glob_age)
  cast.foraging_glob_age <- st_cast(sf.foraging_glob_age, "POLYGON")
  cast.foraging_glob_age$age <- age
  
  return(cast.foraging_glob_age)
})

# Fusionner tous les ID dans un seul objet sf
results_kud.foraging_glob_age <- do.call(rbind, kud.list_foraging_glob_age)
results_kud.foraging_glob_age$age <- as.factor(results_kud.foraging_glob_age$age)

# write & read
st_write(results_kud.foraging_glob_age, paste0(data_generated_path, "results_kud.foraging_glob_age.gpkg"), append = FALSE)
results_kud.foraging_glob_age <- st_read(file.path(data_generated_path, "results_kud.foraging_glob_age.gpkg"))

# plot
tmap_mode("view")
UDMap_100x100_foraging_age_glob <- tm_scalebar() +
  tm_shape(RMO) +
  tm_polygons() +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(results_kud.foraging_glob_age) + 
  tm_polygons(border.col = "grey", fill = "level", fill_alpha = 0.2, 
              palette = viridis::viridis(10, begin = 0, end = 1, 
                                         direction = 1, option = "plasma")) +
  tm_facets("age") +
  tm_shape(terre_mer) +
  tm_lines(col = "lightblue", lwd = 0.1) + 
  tm_shape(zero_hydro) +
  tm_lines("layer", col = "darkblue", lwd = 0.5, 
           title.col = "Elevation") ; UDMap_100x100_foraging_age_glob

### ZOOM -----------------------------------------------------------------------

resolution_ZOOM = 10

crs_utm <- "EPSG:32630"
ZOOM <- c("A","B","C","D","E")
results_kud.foraging_ZOOM_age = NULL

# lettre = "A"

for (lettre in ZOOM){
  # in ZOOM
  ZOOM <- st_read(paste0(data_generated_path,"ZOOM_",lettre,".gpkg"))
  ZOOM <- st_transform(ZOOM, crs = 4326)
  GPS.ZOOM <- st_intersection(GPS, ZOOM) 
  GPS.foraging_ZOOM_age <- GPS.ZOOM %>% 
    filter(behavior == "foraging") %>% 
    dplyr::select(lon,lat,age) %>% 
    st_drop_geometry() %>% 
    na.omit()
  
  if (nrow(GPS.foraging_ZOOM_sex) == 0) {
    next  # Passe directement à l'itération suivante
  }
  
  GPS_spa.foraging_ZOOM_age <- st_as_sf(GPS.foraging_ZOOM_age, coords = c("lon", "lat"), crs = 4326)
  GPS_spa.foraging_ZOOM_age <- st_transform(GPS_spa.foraging_ZOOM_age, crs = 32630) 
  GPS_coods.foraging_ZOOM_age <- st_coordinates(GPS_spa.foraging_ZOOM_age)
  
  # raster/grid
  grid_ZOOM <- st_read(paste0(data_generated_path, "grid_ZOOM_",lettre,".gpkg"))
  raster_ZOOM <- rast(grid_ZOOM, resolution = resolution_ZOOM, crs="EPSG:2154")
  SpatRaster_ZOOM <- project(raster_ZOOM, crs_utm)  
  RasterLayer_ZOOM <- raster(SpatRaster_ZOOM) 
  SpatialPixels_ZOOM <- as(RasterLayer_ZOOM, "SpatialPixels")
  
  # Règle de Silverman
  sigma_x.foraging_ZOOM_age <- sd(GPS_coods.foraging_ZOOM_age[,1]) 
  sigma_y.foraging_ZOOM_age <- sd(GPS_coods.foraging_ZOOM_age[,2]) 
  n.foraging_ZOOM_age<- nrow(GPS.foraging_ZOOM_age)  
  h.silverman_x_foraging_ZOOM_age <- 1.06 * sigma_x.foraging_ZOOM_age * n.foraging_ZOOM_age^(-1/5) / 2
  h_silverman_y_foraging_ZOOM_age <- 1.06 * sigma_y.foraging_ZOOM_age * n.foraging_ZOOM_age^(-1/5) / 2
  locs_spa.foraging_ZOOM_age <- as(GPS_spa.foraging_ZOOM_age, "Spatial")
  
  # KernelUD
  kud.foraging_ZOOM_age <- kernelUD(locs_spa.foraging_ZOOM_age["age"], 
                                    grid = SpatialPixels_ZOOM, 
                                    h = mean(c(h.silverman_x_foraging_ZOOM_age, 
                                               h_silverman_y_foraging_ZOOM_age)))
  
  kud_list.foraging_ZOOM_age <- lapply(names(kud.foraging_ZOOM_age), function(age) {
    
    print(age)
    
    # Extraire l'estimation de densité pour un ID spécifique
    kud_single.foraging_ZOOM_age <- kud.foraging_ZOOM_age[[age]]
    rast.foraging_ZOOM_age <- rast(kud_single.foraging_ZOOM_age)
    courtour.foraging_ZOOM_age <- as.contour(rast.foraging_ZOOM_age)
    sf.foraging_ZOOM_age <- st_as_sf(courtour.foraging_ZOOM_age)
    cast.foraging_ZOOM_age <- st_cast(sf.foraging_ZOOM_age, "POLYGON")
    cast.foraging_ZOOM_age$age <- age
    
    return(cast.foraging_ZOOM_age)
  })
  
  kud_all.foraging_ZOOM_age <- do.call(rbind, kud_list.foraging_ZOOM_age)
  kud_all.foraging_ZOOM_age$age <- as.factor(kud_all.foraging_ZOOM_age$age)
  kud_all.foraging_ZOOM_age$ZOOM <- lettre
  results_kud.foraging_ZOOM_age <- rbind(results_kud.foraging_ZOOM_age, kud_all.foraging_ZOOM_age)
  
}

# write
st_write(results_kud.foraging_ZOOM_age, paste0(data_generated_path, "results_kud.foraging_ZOOM_age.gpkg"), append = FALSE)
# read
results_kud.foraging_ZOOM_age <- st_read(file.path(data_generated_path, "results_kud.foraging_ZOOM_age.gpkg"))

# plot
tmap_mode("view")
UDMap_foraging_age_ZOOM <- tm_scalebar() +
  tm_shape(RMO) +
  tm_polygons() +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(ZOOM_A) +
  tm_polygons(fill_alpha = 0.1, fill = "grey") +
  tm_text("A", size = 1.5) +
  tm_shape(ZOOM_B) +
  tm_polygons(fill_alpha = 0.1, fill = "grey") +
  tm_text("B", size = 1.5) +
  tm_shape(ZOOM_C) +
  tm_polygons(fill_alpha = 0.1, fill = "grey") +
  tm_text("C", size = 1.5) +
  tm_shape(ZOOM_D) +
  tm_polygons(fill_alpha = 0.1, fill = "grey") +
  tm_text("D", size = 1.5) +
  tm_shape(ZOOM_E) +
  tm_polygons(fill_alpha = 0.1, fill = "grey") +
  tm_text("E", size = 1.5) +
  tm_shape(BOX_2154) +
  tm_borders(col = "black") +
  tm_shape(results_kud.foraging_ZOOM_age) + 
  tm_facets("age") + 
  tm_polygons(border.col = "grey", fill = "level", fill_alpha = 0.2, 
              palette = viridis::viridis(10, begin = 0, end = 1, 
                                         direction = 1, option = "plasma")) +
  tm_facets("age") +
  tm_shape(terre_mer) +
  tm_lines(col = "lightblue", lwd = 0.1) + 
  tm_shape(zero_hydro) +
  tm_lines("layer", col = "darkblue", lwd = 0.5, legend.show = FALSE, 
           title.col = "Elevation"); UDMap_foraging_age_ZOOM

## Sexe -------------------------------------------------------------------------

### GLOB -----------------------------------------------------------------------

# GPS point foraging ---

# All zone
GPS.foraging_glob_sex <- GPS %>% 
  filter(behavior == "foraging") %>% 
  dplyr::select(lon,lat,sex) %>% 
  st_drop_geometry() %>% 
  na.omit()

GPS_spa.foraging_glob_sex <- st_as_sf(GPS.foraging_glob_sex, coords = c("lon", "lat"), crs = 4326)
GPS_spa.foraging_glob_sex <- st_transform(GPS_spa.foraging_glob_sex, crs = 32630) 
GPS_coords.foraging_glob_sex <- st_coordinates(GPS_spa.foraging_glob_sex)

# raster/grid
crs_utm <- "EPSG:32630"
SpatRaster <- project(raster_100x100, crs_utm)
RasterLayer <- raster(SpatRaster)
SpatialPixels<- as(RasterLayer, "SpatialPixels") 

# Règle de Silverman
sigma_x.foraging_glob_sex <- sd(GPS_coords.foraging_glob_sex[,1]) 
sigma_y.foraging_glob_sex <- sd(GPS_coords.foraging_glob_sex[,2]) 
n.foraging_glob_sex <- nrow(GPS.foraging_glob_sex) 
h.silverman_x_foraging_glob_sex <- 1.06 * sigma_x.foraging_glob_sex * n.foraging_glob_sex^(-1/5) / 2
h.silverman_y_foraging_glob_sex <- 1.06 * sigma_y.foraging_glob_sex * n.foraging_glob_sex^(-1/5) / 2
locs_spa.foraging_glob_sex <- as(GPS_spa.foraging_glob_sex, "Spatial")

# KernelUD
kud.foraging_glob_sex <- kernelUD(locs_spa.foraging_glob_sex["sex"], 
                                  grid = SpatialPixels, 
                                  h = mean(c(h.silverman_x_foraging_glob_sex, h.silverman_y_foraging_glob_sex)))

kud.list_foraging_glob_sex <- lapply(names(kud.foraging_glob_sex), function(sex) {
  
  print(sex)
  
  # Extraire l'estimation de densité pour un ID spécifique
  kud_simple.foraging_glob_sex <- kud.foraging_glob_sex[[sex]]
  rast.foraging_glob_sex <- rast(kud_simple.foraging_glob_sex)
  courtour.foraging_glob_sex <- as.contour(rast.foraging_glob_sex)
  sf.foraging_glob_sex <- st_as_sf(courtour.foraging_glob_sex)
  cast.foraging_glob_sex <- st_cast(sf.foraging_glob_sex, "POLYGON")
  cast.foraging_glob_sex$sex <- sex
  
  return(cast.foraging_glob_sex)
})

# Fusionner tous les ID dans un seul objet sf
results_kud.foraging_glob_sex <- do.call(rbind, kud.list_foraging_glob_sex)
results_kud.foraging_glob_sex$sex <- as.factor(results_kud.foraging_glob_sex$sex)

# write & read
st_write(results_kud.foraging_glob_sex, paste0(data_generated_path, "results_kud.foraging_glob_sex.gpkg"), append = FALSE)
results_kud.foraging_glob_sex <- st_read(file.path(data_generated_path, "results_kud.foraging_glob_sex.gpkg"))

# plot
tmap_mode("view")
UDMap.foraging_glob_sex <- tm_scalebar() +
  tm_shape(RMO) +
  tm_polygons() +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(results_kud.foraging_glob_sex) + 
  tm_polygons(border.col = "grey", fill = "level", fill_alpha = 0.2, 
              palette = viridis::viridis(10, begin = 0, end = 1, 
                                         direction = 1, option = "plasma")) +
  tm_facets("sex") +
  tm_shape(terre_mer) +
  tm_lines(col = "lightblue", lwd = 0.1) + 
  tm_shape(zero_hydro) +
  tm_lines("layer", col = "darkblue", lwd = 0.5, legend.show = FALSE, 
           title.col = "Elevation"); UDMap.foraging_glob_sex

### ZOOM -----------------------------------------------------------------------

resolution_ZOOM = 10

crs_utm <- "EPSG:32630"
ZOOM <- c("A","B","C","D","E")
results_kud.foraging_ZOOM_sex = NULL

# lettre = "E"

for (lettre in ZOOM){
  # in ZOOM
  ZOOM <- st_read(paste0(data_generated_path,"ZOOM_",lettre,".gpkg"))
  ZOOM <- st_transform(ZOOM, crs = 4326)
  GPS.ZOOM <- st_intersection(GPS, ZOOM) 
  GPS.foraging_ZOOM_sex <- GPS.ZOOM %>% 
    filter(behavior == "foraging") %>% 
    dplyr::select(lon,lat,sex) %>% 
    st_drop_geometry() %>% 
    na.omit()
  
  if (nrow(GPS.foraging_ZOOM_sex) == 0) {
    next  # Passe directement à l'itération suivante
  }
  
  GPS_spa.foraging_ZOOM_sex <- st_as_sf(GPS.foraging_ZOOM_sex, coords = c("lon", "lat"), crs = 4326)
  GPS_spa.foraging_ZOOM_sex <- st_transform(GPS_spa.foraging_ZOOM_sex, crs = 32630) 
  GPS_coods.foraging_ZOOM_sex <- st_coordinates(GPS_spa.foraging_ZOOM_sex)
  
  # raster/grid
  grid_ZOOM <- st_read(paste0(data_generated_path, "grid_ZOOM_",lettre,".gpkg"))
  raster_ZOOM <- rast(grid_ZOOM, resolution = resolution_ZOOM, crs="EPSG:2154")
  SpatRaster_ZOOM <- project(raster_ZOOM, crs_utm)  
  RasterLayer_ZOOM <- raster(SpatRaster_ZOOM) 
  SpatialPixels_ZOOM <- as(RasterLayer_ZOOM, "SpatialPixels")
  
  # Règle de Silverman
  sigma_x.foraging_ZOOM_sex <- sd(GPS_coods.foraging_ZOOM_sex[,1]) 
  sigma_y.foraging_ZOOM_sex <- sd(GPS_coods.foraging_ZOOM_sex[,2]) 
  n.foraging_ZOOM_sex<- nrow(GPS.foraging_ZOOM_sex)  
  h.silverman_x_foraging_ZOOM_sex <- 1.06 * sigma_x.foraging_ZOOM_sex * n.foraging_ZOOM_sex^(-1/5) / 2
  h_silverman_y_foraging_ZOOM_sex <- 1.06 * sigma_y.foraging_ZOOM_sex * n.foraging_ZOOM_sex^(-1/5) / 2
  locs_spa.foraging_ZOOM_sex <- as(GPS_spa.foraging_ZOOM_sex, "Spatial")
  
  # KernelUD
  kud.foraging_ZOOM_sex <- kernelUD(locs_spa.foraging_ZOOM_sex["sex"], 
                                    grid = SpatialPixels_ZOOM, 
                                    h = mean(c(h.silverman_x_foraging_ZOOM_sex, 
                                               h_silverman_y_foraging_ZOOM_sex)))
  
  kud_list.foraging_ZOOM_sex <- lapply(names(kud.foraging_ZOOM_sex), function(sex) {
    
    print(sex)
    
    # Extraire l'estimation de densité pour un ID spécifique
    kud_single.foraging_ZOOM_sex <- kud.foraging_ZOOM_sex[[sex]]
    rast.foraging_ZOOM_sex <- rast(kud_single.foraging_ZOOM_sex)
    courtour.foraging_ZOOM_sex <- as.contour(rast.foraging_ZOOM_sex)
    sf.foraging_ZOOM_sex <- st_as_sf(courtour.foraging_ZOOM_sex)
    cast.foraging_ZOOM_sex <- st_cast(sf.foraging_ZOOM_sex, "POLYGON")
    cast.foraging_ZOOM_sex$sex <- sex
    
    return(cast.foraging_ZOOM_sex)
  })
  
  kud_all.foraging_ZOOM_sex <- do.call(rbind, kud_list.foraging_ZOOM_sex)
  kud_all.foraging_ZOOM_sex$sex <- as.factor(kud_all.foraging_ZOOM_sex$sex)
  kud_all.foraging_ZOOM_sex$ZOOM <- lettre
  results_kud.foraging_ZOOM_sex <- rbind(results_kud.foraging_ZOOM_sex, kud_all.foraging_ZOOM_sex)
  
}

# write & read
st_write(results_kud.foraging_ZOOM_sex, paste0(data_generated_path, "results_kud.foraging_ZOOM_sex.gpkg"), append = FALSE)
results_kud.foraging_ZOOM_sex <- st_read(file.path(data_generated_path, "results_kud.foraging_ZOOM_sex.gpkg"))

# plot
tmap_mode("view")
UDMap_foraging_sex_ZOOM <- tm_scalebar() +
  tm_shape(RMO) +
  tm_polygons() +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(ZOOM_A) +
  tm_polygons(fill_alpha = 0.1, fill = "grey") +
  tm_text("A", size = 1.5) +
  tm_shape(ZOOM_B) +
  tm_polygons(fill_alpha = 0.1, fill = "grey") +
  tm_text("B", size = 1.5) +
  tm_shape(ZOOM_C) +
  tm_polygons(fill_alpha = 0.1, fill = "grey") +
  tm_text("C", size = 1.5) +
  tm_shape(ZOOM_D) +
  tm_polygons(fill_alpha = 0.1, fill = "grey") +
  tm_text("D", size = 1.5) +
  tm_shape(ZOOM_E) +
  tm_polygons(fill_alpha = 0.1, fill = "grey") +
  tm_text("E", size = 1.5) +
  tm_shape(BOX_2154) +
  tm_borders(col = "black") +
  tm_shape(results_kud.foraging_ZOOM_sex) + 
  tm_facets("sex") + 
  tm_polygons(border.col = "grey", fill = "level", fill_alpha = 0.2, 
              palette = viridis::viridis(10, begin = 0, end = 1, 
                                         direction = 1, option = "plasma")) +
  tm_facets("sex") +
  tm_shape(terre_mer) +
  tm_lines(col = "lightblue", lwd = 0.1) + 
  tm_shape(zero_hydro) +
  tm_lines("layer", col = "darkblue", lwd = 0.5, legend.show = FALSE, 
           title.col = "Elevation"); UDMap_foraging_sex_ZOOM

## Jour & nuit -----------------------------------------------------------------

resolution_ZOOM = 10

crs_utm <- "EPSG:32630"
ZOOM <- c("A","B","C","D","E")
results_kud.foraging_ZOOM_jour_nuit = NULL

# lettre = "E"

for (lettre in ZOOM){
  # in ZOOM
  ZOOM <- st_read(paste0(data_generated_path,"ZOOM_",lettre,".gpkg"))
  ZOOM <- st_transform(ZOOM, crs = 4326)
  GPS.ZOOM <- st_intersection(GPS, ZOOM) 
  GPS.foraging_ZOOM_jour_nuit <- GPS.ZOOM %>% 
    filter(behavior == "foraging") %>% 
    dplyr::select(lon,lat,jour_nuit) %>% 
    st_drop_geometry() %>% 
    na.omit()
  
  if (nrow(GPS.foraging_ZOOM_jour_nuit) == 0) {
    next  # Passe directement à l'itération suivante
  }
  
  GPS_spa.foraging_ZOOM_jour_nuit <- st_as_sf(GPS.foraging_ZOOM_jour_nuit, coords = c("lon", "lat"), crs = 4326)
  GPS_spa.foraging_ZOOM_jour_nuit <- st_transform(GPS_spa.foraging_ZOOM_jour_nuit, crs = 32630) 
  GPS_coods.foraging_ZOOM_jour_nuit <- st_coordinates(GPS_spa.foraging_ZOOM_jour_nuit)
  
  # raster/grid
  grid_ZOOM <- st_read(paste0(data_generated_path, "grid_ZOOM_",lettre,".gpkg"))
  raster_ZOOM <- rast(grid_ZOOM, resolution = resolution_ZOOM, crs="EPSG:2154")
  SpatRaster_ZOOM <- project(raster_ZOOM, crs_utm)  
  RasterLayer_ZOOM <- raster(SpatRaster_ZOOM) 
  SpatialPixels_ZOOM <- as(RasterLayer_ZOOM, "SpatialPixels")
  
  # Règle de Silverman
  sigma_x.foraging_ZOOM_jour_nuit <- sd(GPS_coods.foraging_ZOOM_jour_nuit[,1]) 
  sigma_y.foraging_ZOOM_jour_nuit <- sd(GPS_coods.foraging_ZOOM_jour_nuit[,2]) 
  n.foraging_ZOOM_jour_nuit<- nrow(GPS.foraging_ZOOM_jour_nuit)  
  h.silverman_x_foraging_ZOOM_jour_nuit <- 1.06 * sigma_x.foraging_ZOOM_jour_nuit * n.foraging_ZOOM_jour_nuit^(-1/5) / 2
  h_silverman_y_foraging_ZOOM_jour_nuit <- 1.06 * sigma_y.foraging_ZOOM_jour_nuit * n.foraging_ZOOM_jour_nuit^(-1/5) / 2
  locs_spa.foraging_ZOOM_jour_nuit <- as(GPS_spa.foraging_ZOOM_jour_nuit, "Spatial")
  
  # KernelUD
  kud.foraging_ZOOM_jour_nuit <- kernelUD(locs_spa.foraging_ZOOM_jour_nuit["jour_nuit"], 
                                    grid = SpatialPixels_ZOOM, 
                                    h = mean(c(h.silverman_x_foraging_ZOOM_jour_nuit, 
                                               h_silverman_y_foraging_ZOOM_jour_nuit)))
  
  kud_list.foraging_ZOOM_jour_nuit <- lapply(names(kud.foraging_ZOOM_jour_nuit), function(jour_nuit) {
    
    print(jour_nuit)
    
    # Extraire l'estimation de densité pour un ID spécifique
    kud_single.foraging_ZOOM_jour_nuit <- kud.foraging_ZOOM_jour_nuit[[jour_nuit]]
    rast.foraging_ZOOM_jour_nuit <- rast(kud_single.foraging_ZOOM_jour_nuit)
    courtour.foraging_ZOOM_jour_nuit <- as.contour(rast.foraging_ZOOM_jour_nuit)
    sf.foraging_ZOOM_jour_nuit <- st_as_sf(courtour.foraging_ZOOM_jour_nuit)
    cast.foraging_ZOOM_jour_nuit <- st_cast(sf.foraging_ZOOM_jour_nuit, "POLYGON")
    cast.foraging_ZOOM_jour_nuit$jour_nuit <- jour_nuit
    
    return(cast.foraging_ZOOM_jour_nuit)
  })
  
  kud_all.foraging_ZOOM_jour_nuit <- do.call(rbind, kud_list.foraging_ZOOM_jour_nuit)
  kud_all.foraging_ZOOM_jour_nuit$jour_nuit <- as.factor(kud_all.foraging_ZOOM_jour_nuit$jour_nuit)
  kud_all.foraging_ZOOM_jour_nuit$ZOOM <- lettre
  results_kud.foraging_ZOOM_jour_nuit <- rbind(results_kud.foraging_ZOOM_jour_nuit, kud_all.foraging_ZOOM_jour_nuit)
  
}

# write & read
st_write(results_kud.foraging_ZOOM_jour_nuit, paste0(data_generated_path, "results_kud.foraging_ZOOM_jour_nuit.gpkg"), append = FALSE)
results_kud.foraging_ZOOM_jour_nuit <- st_read(file.path(data_generated_path, "results_kud.foraging_ZOOM_jour_nuit.gpkg"))

# plot
tmap_mode("view")
UDMap_foraging_jour_nuit_ZOOM <- tm_scalebar() +
  tm_shape(RMO) +
  tm_polygons() +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(ZOOM_A) +
  tm_polygons(fill_alpha = 0.1, fill = "grey") +
  tm_text("A", size = 1.5) +
  tm_shape(ZOOM_B) +
  tm_polygons(fill_alpha = 0.1, fill = "grey") +
  tm_text("B", size = 1.5) +
  tm_shape(ZOOM_C) +
  tm_polygons(fill_alpha = 0.1, fill = "grey") +
  tm_text("C", size = 1.5) +
  tm_shape(ZOOM_D) +
  tm_polygons(fill_alpha = 0.1, fill = "grey") +
  tm_text("D", size = 1.5) +
  tm_shape(ZOOM_E) +
  tm_polygons(fill_alpha = 0.1, fill = "grey") +
  tm_text("E", size = 1.5) +
  tm_shape(BOX_2154) +
  tm_borders(col = "black") +
  tm_shape(results_kud.foraging_ZOOM_jour_nuit) + 
  tm_facets("jour_nuit") + 
  tm_polygons(border.col = "grey", fill = "level", fill_alpha = 0.2, 
              palette = viridis::viridis(10, begin = 0, end = 1, 
                                         direction = 1, option = "plasma")) +
  tm_facets("jour_nuit") +
  tm_shape(terre_mer) +
  tm_lines(col = "lightblue", lwd = 0.1) + 
  tm_shape(zero_hydro) +
  tm_lines("layer", col = "darkblue", lwd = 0.5, legend.show = FALSE, 
           title.col = "Elevation"); UDMap_foraging_jour_nuit_ZOOM

## Brèche ----------------------------------------------------------------------

resolution_ZOOM = 10

crs_utm <- "EPSG:32630"
ZOOM <- c("A","B","C","D","E")
results_kud.foraging_ZOOM_breche = NULL

# lettre = "E"

for (lettre in ZOOM){
  # in ZOOM
  ZOOM <- st_read(paste0(data_generated_path,"ZOOM_",lettre,".gpkg"))
  ZOOM <- st_transform(ZOOM, crs = 4326)
  GPS.ZOOM <- st_intersection(GPS, ZOOM) 
  GPS.foraging_ZOOM_breche <- GPS.ZOOM %>% 
    filter(behavior == "foraging") %>% 
    dplyr::select(lon,lat,breche) %>% 
    st_drop_geometry() %>% 
    na.omit()
  
  if (nrow(GPS.foraging_ZOOM_breche) == 0) {
    next  # Passe directement à l'itération suivante
  }
  
  nb_row <- GPS.foraging_ZOOM_breche %>% 
    group_by(breche) %>% 
    mutate(n = n())
  
  if (min(nb_row$n) < 5) {
    next  # Passe directement à l'itération suivante
  }
  
  GPS_spa.foraging_ZOOM_breche <- st_as_sf(GPS.foraging_ZOOM_breche, coords = c("lon", "lat"), crs = 4326)
  GPS_spa.foraging_ZOOM_breche <- st_transform(GPS_spa.foraging_ZOOM_breche, crs = 32630) 
  GPS_coods.foraging_ZOOM_breche <- st_coordinates(GPS_spa.foraging_ZOOM_breche)
  
  # raster/grid
  grid_ZOOM <- st_read(paste0(data_generated_path, "grid_ZOOM_",lettre,".gpkg"))
  raster_ZOOM <- rast(grid_ZOOM, resolution = resolution_ZOOM, crs="EPSG:2154")
  SpatRaster_ZOOM <- project(raster_ZOOM, crs_utm)  
  RasterLayer_ZOOM <- raster(SpatRaster_ZOOM) 
  SpatialPixels_ZOOM <- as(RasterLayer_ZOOM, "SpatialPixels")
  
  # Règle de Silverman
  sigma_x.foraging_ZOOM_breche <- sd(GPS_coods.foraging_ZOOM_breche[,1]) 
  sigma_y.foraging_ZOOM_breche <- sd(GPS_coods.foraging_ZOOM_breche[,2]) 
  n.foraging_ZOOM_breche<- nrow(GPS.foraging_ZOOM_breche)  
  h.silverman_x_foraging_ZOOM_breche <- 1.06 * sigma_x.foraging_ZOOM_breche * n.foraging_ZOOM_breche^(-1/5) / 2
  h_silverman_y_foraging_ZOOM_breche <- 1.06 * sigma_y.foraging_ZOOM_breche * n.foraging_ZOOM_breche^(-1/5) / 2
  locs_spa.foraging_ZOOM_breche <- as(GPS_spa.foraging_ZOOM_breche, "Spatial")
  
  # KernelUD
  kud.foraging_ZOOM_breche <- kernelUD(locs_spa.foraging_ZOOM_breche["breche"], 
                                          grid = SpatialPixels_ZOOM, 
                                          h = mean(c(h.silverman_x_foraging_ZOOM_breche, 
                                                     h_silverman_y_foraging_ZOOM_breche)))
  
  kud_list.foraging_ZOOM_breche <- lapply(names(kud.foraging_ZOOM_breche), function(breche) {
    
    print(breche)
    
    # Extraire l'estimation de densité pour un ID spécifique
    kud_single.foraging_ZOOM_breche <- kud.foraging_ZOOM_breche[[breche]]
    rast.foraging_ZOOM_breche <- rast(kud_single.foraging_ZOOM_breche)
    courtour.foraging_ZOOM_breche <- as.contour(rast.foraging_ZOOM_breche)
    sf.foraging_ZOOM_breche <- st_as_sf(courtour.foraging_ZOOM_breche)
    cast.foraging_ZOOM_breche <- st_cast(sf.foraging_ZOOM_breche, "POLYGON")
    cast.foraging_ZOOM_breche$breche <- breche
    
    return(cast.foraging_ZOOM_breche)
  })
  
  kud_all.foraging_ZOOM_breche <- do.call(rbind, kud_list.foraging_ZOOM_breche)
  kud_all.foraging_ZOOM_breche$breche <- as.factor(kud_all.foraging_ZOOM_breche$breche)
  kud_all.foraging_ZOOM_breche$ZOOM <- lettre
  results_kud.foraging_ZOOM_breche <- rbind(results_kud.foraging_ZOOM_breche, kud_all.foraging_ZOOM_breche)
  
}

# write & read
st_write(results_kud.foraging_ZOOM_breche, paste0(data_generated_path, "results_kud.foraging_ZOOM_breche.gpkg"), append = FALSE)
results_kud.foraging_ZOOM_breche <- st_read(file.path(data_generated_path, "results_kud.foraging_ZOOM_breche.gpkg"))

# plot
tmap_mode("view")
UDMap_foraging_breche_ZOOM <- tm_scalebar() +
  tm_shape(RMO) +
  tm_polygons() +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(ZOOM_A) +
  tm_polygons(fill_alpha = 0.1, fill = "grey") +
  tm_text("A", size = 1.5) +
  tm_shape(ZOOM_B) +
  tm_polygons(fill_alpha = 0.1, fill = "grey") +
  tm_text("B", size = 1.5) +
  tm_shape(ZOOM_C) +
  tm_polygons(fill_alpha = 0.1, fill = "grey") +
  tm_text("C", size = 1.5) +
  tm_shape(ZOOM_D) +
  tm_polygons(fill_alpha = 0.1, fill = "grey") +
  tm_text("D", size = 1.5) +
  tm_shape(ZOOM_E) +
  tm_polygons(fill_alpha = 0.1, fill = "grey") +
  tm_text("E", size = 1.5) +
  tm_shape(BOX_2154) +
  tm_borders(col = "black") +
  tm_shape(results_kud.foraging_ZOOM_breche) + 
  tm_facets("breche") + 
  tm_polygons(border.col = "grey", fill = "level", fill_alpha = 0.2, 
              palette = viridis::viridis(10, begin = 0, end = 1, 
                                         direction = 1, option = "plasma")) +
  tm_facets("breche") +
  tm_shape(terre_mer) +
  tm_lines(col = "lightblue", lwd = 0.1) + 
  tm_shape(zero_hydro) +
  tm_lines("layer", col = "darkblue", lwd = 0.5, legend.show = FALSE, 
           title.col = "Elevation"); UDMap_foraging_breche_ZOOM

# OLDOLOLDOLD ------------------------------------------------------------------
# OLDOLOLDOLD ------------------------------------------------------------------
# OLDOLOLDOLD ------------------------------------------------------------------













# Charger les données en lat/lon (EPSG:4326)
coords_roosting_ID <- GPS %>% 
  filter(behavior == "roosting") %>% 
  dplyr::select(ID,lon,lat) %>% 
  st_drop_geometry() %>% 
  na.omit()

# Transformer en objet spatial (EPSG:4326)
locs_roosting_ID <- st_as_sf(coords_roosting_ID, coords = c("lon", "lat"), crs = 4326)

# Reprojeter en système métrique (ex. UTM zone 30N - EPSG:32630 pour la France)
locs_roosting_ID_32630 <- st_transform(locs_roosting_ID, crs = 32630)  # Adapter le CRS à votre région

# Reprojection du raster
crs_utm <- CRS("+init=epsg:32630") # Définir le CRS cible (EPSG:32630 = UTM zone 30N)
raster_100x100_32630 <- projectRaster(raster_100x100, crs = crs_utm)
crs(raster_100x100_32630)

# Extraire les coordonnées reprojetées
coords_roosting_ID_32630 <- st_coordinates(locs_roosting_ID_32630)

# Règle de Silverman
sigma_x_roosting_ID <- sd(coords_roosting_ID_32630[,1])  # Écart-type en X (mètres)
sigma_y_roosting_ID <- sd(coords_roosting_ID_32630[,2])  # Écart-type en Y (mètres)
n_roosting_ID <- nrow(coords_roosting_ID_32630)  # Nombre de points

h_silverman_x_roosting_ID <- 1.06 * sigma_x_roosting_ID * n_roosting_ID^(-1/5)
h_silverman_y_roosting_ID <- 1.06 * sigma_y_roosting_ID * n_roosting_ID^(-1/5)

cat("h optimal en mètres pour X:", h_silverman_x_roosting_ID, "\n")
cat("h optimal en mètres pour Y:", h_silverman_y_roosting_ID, "\n")

# locs_spa <- as(locs_m, "Spatial")

# locs_spa <- st_transform(locs_roosting_ID, crs = 32630)
locs_spa_roosting_ID <- as(locs_roosting_ID_32630, "Spatial")

# Appliquer kernelUD avec h estimé par Silverman
kud_roosting_ID <- kernelUD(locs_spa_roosting_ID["ID"], grid = as(raster_100x100_32630, "SpatialPixels"),
                   h = mean(c(h_silverman_x_roosting_ID, h_silverman_y_roosting_ID)))

# Visualiser la densité de noyau
par(mfrow = c(1, 1))
image(kud_roosting_ID)

# Créer une liste pour stocker les résultats
UDmaps_list_roosting_ID <- lapply(names(kud_roosting_ID), function(ID) {
  
  print(ID)
  
  # Extraire l'estimation de densité pour un ID spécifique
  kud_single_roosting_ID <- kud_roosting_ID[[ID]]
  rast_roosting_ID <- rast(kud_single_roosting_ID)
  contour_roosting_ID <- as.contour(rast_roosting_ID)
  sf_roosting_ID <- st_as_sf(contour_roosting_ID)
  cast_roosting_ID <- st_cast(sf_roosting_ID, "POLYGON")
  cast_roosting_ID$ID <- ID
  
  return(cast_roosting_ID)
})

# Fusionner tous les ID dans un seul objet sf
UDMap_final_roosting_ID <- do.call(rbind, UDmaps_list_roosting_ID)

UDMap_final_roosting_ID$ID <- as.factor(UDMap_final_roosting_ID$ID)

# write
st_write(UDMap_final_roosting_ID, paste0(data_generated_path, "UDMap_final_roosting_ID.gpkg"), append = FALSE)
# read
UDMap_final_roosting_ID <- st_read(file.path(data_generated_path, "UDMap_final_roosting_ID.gpkg"))

# groupe plot
ID_list <- unique(UDMap_final_roosting_ID$ID)
ID_gp_1 <- ID_list[1:15]
ID_gp_2 <- ID_list[16:30]
ID_gp_3 <- ID_list[31:45]
ID_gp_4 <- ID_list[46:69]

UDMap_final_roosting_ID_gp1 <- UDMap_final_roosting_ID %>% 
  filter(ID %in% ID_gp_1)
# UDMap_final_roosting_ID_gp1$ID <- droplevels(UDMap_final_roosting_ID_gp1$ID)
UDMap_final_roosting_ID_gp2 <- UDMap_final_roosting_ID %>% 
  filter(ID %in% ID_gp_2)
# UDMap_final_roosting_ID_gp2$ID <- droplevels(UDMap_final_roosting_ID_gp2$ID)
UDMap_final_roosting_ID_gp3 <- UDMap_final_roosting_ID %>% 
  filter(ID %in% ID_gp_3)
# UDMap_final_roosting_ID_gp3$ID <- droplevels(UDMap_final_roosting_ID_gp3$ID)
UDMap_final_roosting_ID_gp4 <- UDMap_final_roosting_ID %>% 
  filter(ID %in% ID_gp_4)
# UDMap_final_roosting_ID_gp4$ID <- droplevels(UDMap_final_roosting_ID_gp4$ID)

# plot 
tmap_mode("view")

UDMap_roosting_ID_gp1 <- tm_shape(RMO) +
  tm_polygons() +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(UDMap_final_roosting_ID_gp1) + 
  tm_polygons(border.col = "grey", fill = "ID", fill_fill_alpha = 0.2,
              fill.legend = tm_legend(legend.outside = T, legend.stack = "horizontal", legend.outside.position = 'bottom'))

UDMap_roosting_ID_gp2 <- tm_shape(RMO) +
  tm_polygons() +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(UDMap_final_roosting_ID_gp2) + 
  tm_polygons(border.col = "grey", fill = "ID", fill_fill_alpha = 0.2,
              fill.legend = tm_legend(legend.outside = T, legend.stack = "horizontal", legend.outside.position = 'bottom'))

UDMap_roosting_ID_gp3 <- tm_shape(RMO) +
  tm_polygons() +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(UDMap_final_roosting_ID_gp3) + 
  tm_polygons(border.col = "grey", fill = "ID", fill_fill_alpha = 0.2,
              fill.legend = tm_legend(legend.outside = T, legend.stack = "horizontal", legend.outside.position = 'bottom')) 

UDMap_roosting_ID_gp4 <- tm_shape(RMO) +
  tm_polygons() +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(UDMap_final_roosting_ID_gp4) + 
  tm_polygons(border.col = "grey", fill = "ID", fill_fill_alpha = 0.2,
              fill.legend = tm_legend(legend.outside = T, legend.stack = "horizontal", legend.outside.position = 'bottom'))

UDMap_roosting_ID <- tmap_arrange(UDMap_roosting_ID_gp1, UDMap_roosting_ID_gp2, UDMap_roosting_ID_gp3, UDMap_roosting_ID_gp4) ; UDMap_roosting_ID






## ROOSTING --------------------------------------------------------------------


### ID ~ year ----

#### plot ----

# Charger les données en lat/lon (EPSG:4326)
coords_roosting_ID_year <- GPS %>% 
  filter(behavior == "roosting") %>% 
  dplyr::select(ID,year,lon,lat) %>% 
  mutate(ID_year = paste0(ID, "_", year)) %>% 
  st_drop_geometry() %>% 
  na.omit()

# au moins 5 point par an
n_per_year_per_ind <- coords_roosting_ID_year %>% 
  group_by(ID, year) %>% 
  summarize(n = n()) %>% 
  filter(n <=5) %>%
  mutate(ID_year = paste0(ID, "_", year))

# reverse of %in%  
`%ni%` <- Negate(`%in%`)

coords_roosting_ID_year <- coords_roosting_ID_year %>% 
  filter(ID_year %ni% n_per_year_per_ind$ID_year)

# au moins 3 années de présence sur site 
n_year <- coords_roosting_ID_year %>% 
  dplyr::select(ID, year) %>% 
  group_by(ID) %>% 
  distinct() %>% 
  # mutate(year = as.character(year)) %>% 
  summarize(n = n()) %>% 
  filter(n < 3)

coords_roosting_ID_year <- coords_roosting_ID_year %>% 
  filter(ID %ni% n_year$ID)

# Transformer en objet spatial (EPSG:4326)
locs_roosting_ID_year <- st_as_sf(coords_roosting_ID_year, coords = c("lon", "lat"), crs = 4326)

# Reprojeter en système métrique (ex. UTM zone 30N - EPSG:32630 pour la France)
locs_roosting_ID_year_32630 <- st_transform(locs_roosting_ID_year, crs = 32630)  # Adapter le CRS à votre région

# Reprojection du raster
crs_utm <- CRS("+init=epsg:32630") # Définir le CRS cible (EPSG:32630 = UTM zone 30N)
raster_100x100_32630 <- projectRaster(raster_100x100, crs = crs_utm)
crs(raster_100x100_32630)

# Extraire les coordonnées reprojetées
coords_roosting_ID_year_32630 <- st_coordinates(locs_roosting_ID_year_32630)

# Règle de Silverman
sigma_x_roosting_ID_year <- sd(coords_roosting_ID_year_32630[,1])  # Écart-type en X (mètres)
sigma_y_roosting_ID_year <- sd(coords_roosting_ID_year_32630[,2])  # Écart-type en Y (mètres)
n_roosting_ID_year <- nrow(coords_roosting_ID_year_32630)  # Nombre de points

h_silverman_x_roosting_ID_year <- 1.06 * sigma_x_roosting_ID_year * n_roosting_ID_year^(-1/5)
h_silverman_y_roosting_ID_year <- 1.06 * sigma_y_roosting_ID_year * n_roosting_ID_year^(-1/5)

cat("h optimal en mètres pour X:", h_silverman_x_roosting_ID_year, "\n")
cat("h optimal en mètres pour Y:", h_silverman_y_roosting_ID_year, "\n")

# locs_spa <- as(locs_m, "Spatial")

# locs_spa <- st_transform(locs_roosting_ID_year, crs = 32630)
locs_spa_roosting_ID_year <- as(locs_roosting_ID_year_32630, "Spatial")

# Appliquer kernelUD avec h estimé par Silverman

all_ID_year = NULL

for (y in unique(coords_roosting_ID_year$year)){
  
  print(y)
  
  dt_year <- locs_spa_roosting_ID_year[locs_spa_roosting_ID_year@data$year == y,]
  
  kud_roosting_ID_year <- kernelUD(dt_year["ID"], grid = as(raster_100x100_32630, "SpatialPixels"),
                                   h = mean(c(h_silverman_x_roosting_ID_year, h_silverman_y_roosting_ID_year)))
  
  # Visualiser la densité de noyau
  # par(mfrow = c(1, 1))
  # image(kud_roosting_ID_year)
  
  # Créer une liste pour stocker les résultats
  UDmaps_list_roosting_ID_year <- lapply(names(kud_roosting_ID_year), function(ID) {
    
    print(ID)
    
    # Extraire l'estimation de densité pour un ID spécifique
    kud_single_roosting_ID_year <- kud_roosting_ID_year[[ID]]
    rast_roosting_ID_year <- rast(kud_single_roosting_ID_year)
    contour_roosting_ID_year <- as.contour(rast_roosting_ID_year)
    sf_roosting_ID_year <- st_as_sf(contour_roosting_ID_year)
    cast_roosting_ID_year <- st_cast(sf_roosting_ID_year, "POLYGON")
    cast_roosting_ID_year$ID <- ID
    
    return(cast_roosting_ID_year)
  })
  
  # Fusionner tous les ID dans un seul objet sf
  UDMap_final_roosting_ID_year <- do.call(rbind, UDmaps_list_roosting_ID_year)
  
  UDMap_final_roosting_ID_year$ID <- as.factor(UDMap_final_roosting_ID_year$ID)
  UDMap_final_roosting_ID_year$year <- y
  
  all_ID_year <- rbind(all_ID_year, UDMap_final_roosting_ID_year)
  
}

# write
st_write(all_ID_year, paste0(data_generated_path, "UDMap_roosting_ID_year.gpkg"), append = FALSE)
# read
UDMap_final_roosting_ID_year <- st_read(file.path(data_generated_path, "UDMap_roosting_ID_year.gpkg"))

UDMap_final_roosting_ID_year$year <- as.character(UDMap_final_roosting_ID_year$year)
# plot 
tmap_mode("view")

UDMap_roosting_ID_year <- tm_shape(RMO) +
  tm_polygons() +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(UDMap_final_roosting_ID_year) + 
  tm_facets("ID", sync = F) +
  tm_polygons(border.col = "grey", fill = "year", fill_fill_alpha = 0.2,
              fill.legend = tm_legend(legend.outside = T, legend.stack = "horizontal", legend.outside.position = 'bottom'), 
              palette = viridis(10, begin = 0, end = 1, 
                                direction = 1, option = "plasma")) ; UDMap_roosting_ID_year

#### (répétabilité) ----

# Charger les données en lat/lon (EPSG:4326)
coords_roosting_ID_year <- GPS %>% 
  filter(behavior == "roosting") %>% 
  dplyr::select(ID,year,lon,lat) %>% 
  mutate(ID_year = paste0(ID, "_", year)) %>% 
  st_drop_geometry() %>% 
  na.omit()

# au moins 5 point par an
n_per_year_per_ind <- coords_roosting_ID_year %>% 
  group_by(ID, year) %>% 
  summarize(n = n()) %>% 
  filter(n <=5) %>%
  mutate(ID_year = paste0(ID, "_", year))

# reverse of %in%  
`%ni%` <- Negate(`%in%`)

coords_roosting_ID_year <- coords_roosting_ID_year %>% 
  filter(ID_year %ni% n_per_year_per_ind$ID_year)

# au moins 3 années de présence sur site 
n_year <- coords_roosting_ID_year %>% 
  dplyr::select(ID, year) %>% 
  group_by(ID) %>% 
  distinct() %>% 
  # mutate(year = as.character(year)) %>% 
  summarize(n = n()) %>% 
  filter(n < 3)

coords_roosting_ID_year <- coords_roosting_ID_year %>% 
  filter(ID %ni% n_year$ID)

# Transformer en objet spatial (EPSG:4326)
locs_roosting_ID_year <- st_as_sf(coords_roosting_ID_year, coords = c("lon", "lat"), crs = 4326)

# Reprojeter en système métrique (ex. UTM zone 30N - EPSG:32630 pour la France)
locs_roosting_ID_year_32630 <- st_transform(locs_roosting_ID_year, crs = 32630)  # Adapter le CRS à votre région

# Reprojection du raster
crs_utm <- CRS("+init=epsg:32630") # Définir le CRS cible (EPSG:32630 = UTM zone 30N)
raster_100x100_32630 <- projectRaster(raster_100x100, crs = crs_utm)
crs(raster_100x100_32630)

# Extraire les coordonnées reprojetées
coords_roosting_ID_year_32630 <- st_coordinates(locs_roosting_ID_year_32630)

# Règle de Silverman
sigma_x_roosting_ID_year <- sd(coords_roosting_ID_year_32630[,1])  # Écart-type en X (mètres)
sigma_y_roosting_ID_year <- sd(coords_roosting_ID_year_32630[,2])  # Écart-type en Y (mètres)
n_roosting_ID_year <- nrow(coords_roosting_ID_year_32630)  # Nombre de points

h_silverman_x_roosting_ID_year <- 1.06 * sigma_x_roosting_ID_year * n_roosting_ID_year^(-1/5)
h_silverman_y_roosting_ID_year <- 1.06 * sigma_y_roosting_ID_year * n_roosting_ID_year^(-1/5)

cat("h optimal en mètres pour X:", h_silverman_x_roosting_ID_year, "\n")
cat("h optimal en mètres pour Y:", h_silverman_y_roosting_ID_year, "\n")

# locs_spa <- as(locs_m, "Spatial")

# locs_spa <- st_transform(locs_roosting_ID_year, crs = 32630)
locs_spa_roosting_ID_year <- as(locs_roosting_ID_year_32630, "Spatial")









locs_spa_roosting_ID_year$Periode <- ifelse(locs_spa_roosting_ID_year$year <= 2021, "Periode1", "Periode2")


# Créer une colonne combinée
locs_spa_roosting_ID_year$Individu_Periode <- paste(locs_spa_roosting_ID_year$ID, locs_spa_roosting_ID_year$Periode, sep = "_")

# Vérifier que les noms sont bien générés
unique(locs_spa_roosting_ID_year$Individu_Periode)




# Calculer les KDE en séparant par individu et période
# hr_kde <- kernelUD(locs_spa_roosting_ID_year[c("ID", "Periode")], h = "href", grid = 500)

hr_kde <- kernelUD(locs_spa_roosting_ID_year["Individu_Periode"], grid = as(raster_100x100_32630, "SpatialPixels"),
                   h = mean(c(h_silverman_x_roosting_ID_year, h_silverman_y_roosting_ID_year)))


# Extraire les noms uniques des individus
individus <- unique(locs_spa_roosting_ID_year$ID)


# Stocker les résultats
overlap_results <- data.frame(Individu = character(), Overlap = numeric())

# Boucle sur chaque individu
for (ind in individus) {
  # Trouver les noms des périodes de cet individu dans hr_kde
  ID_periodes <- names(hr_kde)[grep(paste0("^", ind, "_"), names(hr_kde))]
  
  # Vérifier que l'individu a bien deux périodes
  if (length(ID_periodes) == 2) {
    # Créer un estUDm valide
    hr_kde_ind <- hr_kde[ID_periodes]
    class(hr_kde_ind) <- "estUDm"  # Important pour que kerneloverlaphr() fonctionne
    
    # Calculer l'overlap entre les deux périodes
    overlap_value <- kerneloverlaphr(hr_kde_ind, method = "BA")[1, 2]
    
    # Stocker le résultat
    overlap_results <- rbind(overlap_results, data.frame(Individu = ind, Overlap = overlap_value))
  }
}

# Afficher les résultats
print(overlap_results)











# hr_kde <- kernelUD(locs_spa_roosting_ID_year["ID"], grid = as(raster_100x100_32630, "SpatialPixels"),
#                    h = mean(c(h_silverman_x_roosting_ID_year, h_silverman_y_roosting_ID_year)))
# 
# # Calculer les home ranges à différents niveaux de densité
# hr95 <- getverticeshr(hr_kde, percent = 95) # Zone de présence à 95%
# hr50 <- getverticeshr(hr_kde, percent = 50) # Zone de forte présence (noyau)
# 
# # Visualisation
# plot(hr95, col = "blue", border = "blue", main = "Kernel Density Estimation")
# plot(hr50, col = "red", border = "red", add = TRUE)
# points(locs_spa_roosting_ID_year, col = "black", pch = 16)
# 
# # Exemple : Diviser les données en deux périodes
# periode1 <- locs_spa_roosting_ID_year[locs_spa_roosting_ID_year@data$year <= 2022, ]
# periode2 <- locs_spa_roosting_ID_year[locs_spa_roosting_ID_year@data$year > 2022, ]
# 
# # Calcul du home range pour chaque période
# hr_kde_p1 <- kernelUD(periode1["ID"], grid = as(raster_100x100_32630, "SpatialPixels"),
#                       h = mean(c(h_silverman_x_roosting_ID_year, h_silverman_y_roosting_ID_year)))
# hr_kde_p2 <- kernelUD(periode2["ID"], grid = as(raster_100x100_32630, "SpatialPixels"),
#                       h = mean(c(h_silverman_x_roosting_ID_year, h_silverman_y_roosting_ID_year)))
# 
# hr95_p1 <- getverticeshr(hr_kde_p1, percent = 95)
# hr95_p2 <- getverticeshr(hr_kde_p2, percent = 95)
# 
# # Créer une liste combinée avec les deux périodes
# hr_kde_combined <- list(periode1 = hr_kde_p1, periode2 = hr_kde_p2)
# 
# # Calculer le chevauchement avec le Bhattacharyya Index
# overlap <- kerneloverlap(hr_kde_combined, method = "BA", percent = 95)
# 
# # Afficher la matrice de chevauchement
# print(overlap)
# 
# 
# # # Calcul du recouvrement spatial entre les deux périodes
# # overlap <- kerneloverlap(hr_kde_p1, hr_kde_p2, method = "BA") # Bhattacharyya Index
# # overlap
# 
# # Visualisation
# plot(hr95_p1, col = rgb(0, 0, 1, 0.5), border = "blue", main = "Comparaison des périodes")
# plot(hr95_p2, col = rgb(1, 0, 0, 0.5), border = "red", add = TRUE)
# legend("topright", legend = c("Période 1", "Période 2"), fill = c("blue", "red"))
# 
# 
# 
# 
# 
# # Ajouter une colonne pour identifier les périodes
# locs_spa_roosting_ID_year$Periode <- ifelse(locs_spa_roosting_ID_year$year <= 2022, "Periode1", "Periode2")
# gps_data <- locs_spa_roosting_ID_year
# # Transformer en objet spatial
# # coordinates(gps_data) <- ~Longitude+Latitude
# # proj4string(gps_data) <- CRS("+proj=longlat +datum=WGS84")
# 
# # Calculer les KDE sur toutes les données en séparant par période
# hr_kde <- kernelUD(gps_data["Periode"], grid = as(raster_100x100_32630, "SpatialPixels"),
#                    h = mean(c(h_silverman_x_roosting_id_year, h_silverman_y_roosting_id_year)))
# 
# overlap_matrix <- kerneloverlaphr(hr_kde, method = "BA")
# 
# print(overlap_matrix)
# 
# 
# 
# 
# # Séparer les périodes AVANT d'appliquer kernelUD()
# periode1 <- gps_data[gps_data@data$year <= 2022, ]
# periode2 <- gps_data[gps_data@data$year > 2022, ]
# 
# # Calcul des KDE séparément pour chaque période
# hr_kde_p1 <- kernelUD(periode1["id"], h = "href", grid = 500)
# hr_kde_p2 <- kernelUD(periode2["id"], h = "href", grid = 500)
# 
# # Fusionner les distributions dans une seule matrice
# hr_kde_combined <- list(P1 = hr_kde_p1$Individu1, P2 = hr_kde_p2$Individu1)
# 
# # Calcul de l'overlap
# overlap_matrix <- kerneloverlaphr(hr_kde_combined, method = "BA")
# print(overlap_matrix)
# 
# 
# 
# 
# 
# 
# 


#ça marche poooo

# library(adehabitatHR)
# library(sp)
# 
# # Transformer en objet spatial
# coordinates(gps_data) <- ~Longitude+Latitude
# proj4string(gps_data) <- CRS("+proj=longlat +datum=WGS84")































# Vérifier les noms générés
names(hr_kde)

# Extraire la liste des individus
individus <- unique(locs_spa_roosting_ID_year$ID)

# Stocker les résultats
overlap_results <- data.frame(Individu = character(), Overlap = numeric())

ind = "EA580467"
# Boucle sur chaque individu
for (ind in individus) {
  # Vérifier si l'individu a bien deux périodes
  ID_periodes <- names(hr_kde)[grep(ind, names(hr_kde))]
  
  if (length(ID_periodes) == 2) {
    # Calculer l'overlap entre les deux périodes
    overlap_value <- kerneloverlaphr(hr_kde[ID_periodes], method = "BA")[1,2]
    
    # Stocker le résultat
    overlap_results <- rbind(overlap_results, data.frame(Individu = ind, Overlap = overlap_value))
  }
}

# Afficher les résultats
print(overlap_results)

### ID ~ week ----

# Charger les données en lat/lon (EPSG:4326)
coords_roosting_ID_week <- GPS %>% 
  filter(behavior == "roosting") %>% 
  dplyr::select(ID,datetime,year,lon,lat) %>% 
  mutate(week = week(datetime),
         week_year = paste0(week, "_", year),
         ID_week_year = paste0(ID, "_", week_year)) %>% 
  st_drop_geometry() %>% 
  na.omit()

# au moins 5 point par an
n_per_week_per_ind <- coords_roosting_ID_week %>% 
  group_by(ID, week_year) %>% 
  summarize(n = n()) %>% 
  filter(n <=5) %>%
  mutate(ID_week_year = paste0(ID, "_", week_year))

# reverse of %in%  
`%ni%` <- Negate(`%in%`)

coords_roosting_ID_week <- coords_roosting_ID_week %>% 
  filter(ID_week_year %ni% n_per_week_per_ind$ID_week_year)

# au moins 3 années de présence sur site 
n_week <- coords_roosting_ID_week %>% 
  dplyr::select(ID, week_year) %>% 
  group_by(ID) %>% 
  distinct() %>% 
  # mutate(week = as.character(week)) %>% 
  summarize(n = n()) %>% 
  filter(n < 3)

coords_roosting_ID_week <- coords_roosting_ID_week %>% 
  filter(ID %ni% n_week$ID)

# Transformer en objet spatial (EPSG:4326)
locs_roosting_ID_week <- st_as_sf(coords_roosting_ID_week, coords = c("lon", "lat"), crs = 4326)

# Reprojeter en système métrique (ex. UTM zone 30N - EPSG:32630 pour la France)
locs_roosting_ID_week_32630 <- st_transform(locs_roosting_ID_week, crs = 32630)  # Adapter le CRS à votre région

# Reprojection du raster
crs_utm <- CRS("+init=epsg:32630") # Définir le CRS cible (EPSG:32630 = UTM zone 30N)
raster_100x100_32630 <- projectRaster(raster_100x100, crs = crs_utm)
crs(raster_100x100_32630)

# Extraire les coordonnées reprojetées
coords_roosting_ID_week_32630 <- st_coordinates(locs_roosting_ID_week_32630)

# Règle de Silverman
sigma_x_roosting_ID_week <- sd(coords_roosting_ID_week_32630[,1])  # Écart-type en X (mètres)
sigma_y_roosting_ID_week <- sd(coords_roosting_ID_week_32630[,2])  # Écart-type en Y (mètres)
n_roosting_ID_week <- nrow(coords_roosting_ID_week_32630)  # Nombre de points

h_silverman_x_roosting_ID_week <- 1.06 * sigma_x_roosting_ID_week * n_roosting_ID_week^(-1/5)
h_silverman_y_roosting_ID_week <- 1.06 * sigma_y_roosting_ID_week * n_roosting_ID_week^(-1/5)

cat("h optimal en mètres pour X:", h_silverman_x_roosting_ID_week, "\n")
cat("h optimal en mètres pour Y:", h_silverman_y_roosting_ID_week, "\n")

# locs_spa <- as(locs_m, "Spatial")

# locs_spa <- st_transform(locs_roosting_ID_week, crs = 32630)
locs_spa_roosting_ID_week <- as(locs_roosting_ID_week_32630, "Spatial")

# Appliquer kernelUD avec h estimé par Silverman

all_ID_week = NULL

for (w in unique(coords_roosting_ID_week$week_year)){
  
  print(w)
  
  dt_week <- locs_spa_roosting_ID_week[locs_spa_roosting_ID_week@data$week_year == w,]
  
  kud_roosting_ID_week <- kernelUD(dt_week["ID"], grid = as(raster_100x100_32630, "SpatialPixels"),
                                   h = mean(c(h_silverman_x_roosting_ID_week, h_silverman_y_roosting_ID_week)))
  
  # Visualiser la densité de noyau
  # par(mfrow = c(1, 1))
  # image(kud_roosting_ID_week)
  
  # Créer une liste pour stocker les résultats
  UDmaps_list_roosting_ID_week <- lapply(names(kud_roosting_ID_week), function(ID) {
    
    print(ID)
    
    # Extraire l'estimation de densité pour un ID spécifique
    kud_single_roosting_ID_week <- kud_roosting_ID_week[[ID]]
    rast_roosting_ID_week <- rast(kud_single_roosting_ID_week)
    contour_roosting_ID_week <- as.contour(rast_roosting_ID_week)
    sf_roosting_ID_week <- st_as_sf(contour_roosting_ID_week)
    cast_roosting_ID_week <- st_cast(sf_roosting_ID_week, "POLYGON")
    cast_roosting_ID_week$ID <- ID
    
    return(cast_roosting_ID_week)
  })
  
  # Fusionner tous les ID dans un seul objet sf
  UDMap_final_roosting_ID_week <- do.call(rbind, UDmaps_list_roosting_ID_week)
  
  UDMap_final_roosting_ID_week$ID <- as.factor(UDMap_final_roosting_ID_week$ID)
  UDMap_final_roosting_ID_week$week_year <- w
  
  all_ID_week <- rbind(all_ID_week, UDMap_final_roosting_ID_week)
  
}

# write
st_write(all_ID_week, paste0(data_generated_path, "UDMap_roosting_ID_week.gpkg"), append = FALSE)
# read
UDMap_final_roosting_ID_week <- st_read(file.path(data_generated_path, "UDMap_roosting_ID_week.gpkg"))

# groupe plot
ID_list <- unique(UDMap_final_roosting_ID_week$ID)
ID_gp_1 <- ID_list[1:15]
ID_gp_2 <- ID_list[16:30]
ID_gp_3 <- ID_list[31:45]
ID_gp_4 <- ID_list[46:69]

UDMap_final_roosting_ID_gp1 <- UDMap_final_roosting_ID_week %>% 
  filter(ID %in% ID_gp_1)
UDMap_final_roosting_ID_gp2 <- UDMap_final_roosting_ID_week %>% 
  filter(ID %in% ID_gp_2)
UDMap_final_roosting_ID_gp3 <- UDMap_final_roosting_ID_week %>% 
  filter(ID %in% ID_gp_3)
UDMap_final_roosting_ID_gp4 <- UDMap_final_roosting_ID_week %>% 
  filter(ID %in% ID_gp_4)

# plot 
tmap_mode("view")

UDMap_roosting_ID_gp1 <- tm_shape(RMO) +
  tm_polygons() +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(UDMap_final_roosting_ID_gp1) +
  tm_facets("ID") + 
  tm_polygons(border.col = "grey", fill = "week_year", fill_fill_alpha = 0.2,
              fill.legend = tm_legend(legend.outside = T, legend.stack = "horizontal", legend.outside.position = 'bottom'))

UDMap_roosting_ID_gp2 <- tm_shape(RMO) +
  tm_polygons() +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(UDMap_final_roosting_ID_gp2) + 
  tm_polygons(border.col = "grey", fill = "ID", fill_fill_alpha = 0.2,
              fill.legend = tm_legend(legend.outside = T, legend.stack = "horizontal", legend.outside.position = 'bottom'))

UDMap_roosting_ID_gp3 <- tm_shape(RMO) +
  tm_polygons() +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(UDMap_final_roosting_ID_gp3) + 
  tm_polygons(border.col = "grey", fill = "ID", fill_fill_alpha = 0.2,
              fill.legend = tm_legend(legend.outside = T, legend.stack = "horizontal", legend.outside.position = 'bottom')) 

UDMap_roosting_ID_gp4 <- tm_shape(RMO) +
  tm_polygons() +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(UDMap_final_roosting_ID_gp4) + 
  tm_polygons(border.col = "grey", fill = "ID", fill_fill_alpha = 0.2,
              fill.legend = tm_legend(legend.outside = T, legend.stack = "horizontal", legend.outside.position = 'bottom'))

UDMap_roosting_ID <- tmap_arrange(UDMap_roosting_ID_gp1, UDMap_roosting_ID_gp2, UDMap_roosting_ID_gp3, UDMap_roosting_ID_gp4) ; UDMap_roosting_ID

# plot 
tmap_mode("view")

UDMap_roosting_ID_week <- tm_shape(RMO) +
  tm_polygons() +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(UDMap_final_roosting_ID_week) + 
  tm_facets("ID") +
  tm_polygons(border.col = "grey", fill = "week", fill_fill_alpha = 0.2,
              fill.legend = tm_legend(legend.outside = T, legend.stack = "horizontal", legend.outside.position = 'bottom'), 
              palette = viridis(10, begin = 0, end = 1, 
                                direction = 1, option = "plasma")) ; UDMap_roosting_ID_week

### ECE ------------------------------------------------

#### wspd ----------------------------------------------------------------------

# Charger les données en lat/lon (EPSG:4326)
coords_ECE_wspd <- GPS %>% 
  filter(behavior == "roosting") %>% 
  dplyr::select(lon, lat, ECE_wspd) %>% 
  st_drop_geometry() %>% 
  na.omit()

# Transformer en objet spatial (EPSG:4326)
locs_ECE_wspd <- st_as_sf(coords_ECE_wspd, coords = c("lon", "lat"), crs = 4326)

# Reprojeter en système métrique (ex. UTM zone 30N - EPSG:32630 pour la France)
locs_ECE_wspd_32630 <- st_transform(locs_ECE_wspd, crs = 32630)  # Adapter le CRS à votre région

# Reprojection du raster
crs_utm <- CRS("+init=epsg:32630") # Définir le CRS cible (EPSG:32630 = UTM zone 30N)
raster_100x100_32630 <- projectRaster(raster_100x100, crs = crs_utm)
crs(raster_100x100_32630) # Vérifier le CRS

# Extraire les coordonnées reprojetées
coords_ECE_wspd_32630 <- st_coordinates(locs_ECE_wspd_32630)

# Règle de Silverman
sigma_x_ECE_wspd <- sd(coords_ECE_wspd_32630[,1])  # Écart-type en X (mètres)
sigma_y_ECE_wspd <- sd(coords_ECE_wspd_32630[,2])  # Écart-type en Y (mètres)
n_ECE_wspd <- nrow(coords_ECE_wspd)  # Nombre de points

h_silverman_x_ECE_wspd <- 1.06 * sigma_x_ECE_wspd * n_ECE_wspd^(-1/5)
h_silverman_y_ECE_wspd <- 1.06 * sigma_y_ECE_wspd * n_ECE_wspd^(-1/5)

cat("h optimal en mètres pour X:", h_silverman_x_ECE_wspd, "\n")
cat("h optimal en mètres pour Y:", h_silverman_y_ECE_wspd, "\n")

# locs_spa <- st_transform(locs, crs = 32630)
locs_spa_ECE_wspd <- as(locs_ECE_wspd_32630, "Spatial")

# Appliquer kernelUD avec h estimé par Silverman
kud_ECE_wspd <- kernelUD(locs_spa_ECE_wspd["ECE_wspd"], grid = as(raster_100x100_32630, "SpatialPixels"),
                         h = mean(c(h_silverman_x_ECE_wspd, h_silverman_y_ECE_wspd)))

# Créer une liste pour stocker les résultats
UDmaps_list_ECE_wspd <- lapply(names(kud_ECE_wspd), function(ECE_wspd) {
  
  print(ECE_wspd)
  
  # Extraire l'estimation de densité pour un ID spécifique
  kud_single_ECE_wspd <- kud_ECE_wspd[[ECE_wspd]]
  rast_ECE_wspd <- rast(kud_single_ECE_wspd)
  contour_ECE_wspd <- as.contour(rast_ECE_wspd)
  sf_ECE_wspd <- st_as_sf(contour_ECE_wspd)
  cast_ECE_wspd <- st_cast(sf_ECE_wspd, "POLYGON")
  cast_ECE_wspd$ECE_wspd <- ECE_wspd
  
  return(cast_ECE_wspd)
})

# Fusionner tous les ID dans un seul objet sf
UDMap_final_ECE_wspd <- do.call(rbind, UDmaps_list_ECE_wspd)

UDMap_final_ECE_wspd$ECE_wspd <- as.factor(UDMap_final_ECE_wspd$ECE_wspd)

st_crs(UDMap_final_ECE_wspd) == st_crs(RMO)  # Vérifie si les projections sont identiques
UDMap_final_ECE_wspd <- st_transform(UDMap_final_ECE_wspd, st_crs(RMO))
table(is.na(UDMap_final_ECE_wspd$ECE_wspd))

# plot 
tmap_mode("view")

UDMap_final_ECE_wspd$ECE_wspd <- as.factor(UDMap_final_ECE_wspd$ECE_wspd)

UDMap_ECE_wspd <- tm_shape(RMO) +
  tm_polygons() +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(UDMap_final_ECE_wspd) + 
  tm_polygons(border.col = "grey", fill = "ECE_wspd", fill_fill_alpha = 0.2) ; UDMap_ECE_wspd

# # tmap_save(UDMap_ECE_wspd, paste0(data_image_path, "/UDMap_roosting_ECE_wspd.html"), dpi = 600)

#### pres ----------------------------------------------------------------------

# Charger les données en lat/lon (EPSG:4326)
coords_ECE_pres <- GPS %>% 
  filter(behavior == "roosting") %>% 
  dplyr::select(lon, lat, ECE_pres) %>% 
  st_drop_geometry() %>% 
  na.omit()

# Transformer en objet spatial (EPSG:4326)
locs_ECE_pres <- st_as_sf(coords_ECE_pres, coords = c("lon", "lat"), crs = 4326)

# Reprojeter en système métrique (ex. UTM zone 30N - EPSG:32630 pour la France)
locs_ECE_pres_32630 <- st_transform(locs_ECE_pres, crs = 32630)  # Adapter le CRS à votre région

# Reprojection du raster
crs_utm <- CRS("+init=epsg:32630") # Définir le CRS cible (EPSG:32630 = UTM zone 30N)
raster_100x100_32630 <- projectRaster(raster_100x100, crs = crs_utm)
crs(raster_100x100_32630) # Vérifier le CRS

# Extraire les coordonnées reprojetées
coords_ECE_pres_32630 <- st_coordinates(locs_ECE_pres_32630)

# Règle de Silverman
sigma_x_ECE_pres <- sd(coords_ECE_pres_32630[,1])  # Écart-type en X (mètres)
sigma_y_ECE_pres <- sd(coords_ECE_pres_32630[,2])  # Écart-type en Y (mètres)
n_ECE_pres <- nrow(coords_ECE_pres)  # Nombre de points

h_silverman_x_ECE_pres <- 1.06 * sigma_x_ECE_pres * n_ECE_pres^(-1/5)
h_silverman_y_ECE_pres <- 1.06 * sigma_y_ECE_pres * n_ECE_pres^(-1/5)

cat("h optimal en mètres pour X:", h_silverman_x_ECE_pres, "\n")
cat("h optimal en mètres pour Y:", h_silverman_y_ECE_pres, "\n")

# locs_spa <- st_transform(locs, crs = 32630)
locs_spa_ECE_pres <- as(locs_ECE_pres_32630, "Spatial")

# Appliquer kernelUD avec h estimé par Silverman
kud_ECE_pres <- kernelUD(locs_spa_ECE_pres["ECE_pres"], grid = as(raster_100x100_32630, "SpatialPixels"),
                         h = mean(c(h_silverman_x_ECE_pres, h_silverman_y_ECE_pres)))

# Créer une liste pour stocker les résultats
UDmaps_list_ECE_pres <- lapply(names(kud_ECE_pres), function(ECE_pres) {
  
  print(ECE_pres)
  
  # Extraire l'estimation de densité pour un ID spécifique
  kud_single_ECE_pres <- kud_ECE_pres[[ECE_pres]]
  rast_ECE_pres <- rast(kud_single_ECE_pres)
  contour_ECE_pres <- as.contour(rast_ECE_pres)
  sf_ECE_pres <- st_as_sf(contour_ECE_pres)
  cast_ECE_pres <- st_cast(sf_ECE_pres, "POLYGON")
  cast_ECE_pres$ECE_pres <- ECE_pres
  
  return(cast_ECE_pres)
})

# Fusionner tous les ID dans un seul objet sf
UDMap_final_ECE_pres <- do.call(rbind, UDmaps_list_ECE_pres)

UDMap_final_ECE_pres$ECE_pres <- as.factor(UDMap_final_ECE_pres$ECE_pres)

st_crs(UDMap_final_ECE_pres) == st_crs(RMO)  # Vérifie si les projections sont identiques
UDMap_final_ECE_pres <- st_transform(UDMap_final_ECE_pres, st_crs(RMO))
table(is.na(UDMap_final_ECE_pres$ECE_pres))

# plot 
tmap_mode("view")

UDMap_final_ECE_pres$ECE_pres <- as.factor(UDMap_final_ECE_pres$ECE_pres)

UDMap_ECE_pres <- tm_shape(RMO) +
  tm_polygons() +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(UDMap_final_ECE_pres) + 
  tm_polygons(border.col = "grey", fill = "ECE_pres", fill_fill_alpha = 0.2) ; UDMap_ECE_pres

# # tmap_save(UDMap_ECE_pres, paste0(data_image_path, "/UDMap_roosting_ECE_pres.html"), dpi = 600)

#### ECE_all_2 -----------------------------------------------------------------

# Charger les données en lat/lon (EPSG:4326)
coords_ECE_all_2 <- GPS %>% 
  filter(behavior == "roosting") %>% 
  dplyr::select(lon, lat, ECE_all_2) %>% 
  st_drop_geometry() %>% 
  na.omit()

# Transformer en objet spatial (EPSG:4326)
locs_ECE_all_2 <- st_as_sf(coords_ECE_all_2, coords = c("lon", "lat"), crs = 4326)

# Reprojeter en système métrique (ex. UTM zone 30N - EPSG:32630 pour la France)
locs_ECE_all_2_32630 <- st_transform(locs_ECE_all_2, crs = 32630)  # Adapter le CRS à votre région

# Reprojection du raster
crs_utm <- CRS("+init=epsg:32630") # Définir le CRS cible (EPSG:32630 = UTM zone 30N)
raster_100x100_32630 <- projectRaster(raster_100x100, crs = crs_utm)
crs(raster_100x100_32630) # Vérifier le CRS

# Extraire les coordonnées reprojetées
coords_ECE_all_2_32630 <- st_coordinates(locs_ECE_all_2_32630)

# Règle de Silverman
sigma_x_ECE_all_2 <- sd(coords_ECE_all_2_32630[,1])  # Écart-type en X (mètres)
sigma_y_ECE_all_2 <- sd(coords_ECE_all_2_32630[,2])  # Écart-type en Y (mètres)
n_ECE_all_2 <- nrow(coords_ECE_all_2)  # Nombre de points

h_silverman_x_ECE_all_2 <- 1.06 * sigma_x_ECE_all_2 * n_ECE_all_2^(-1/5)
h_silverman_y_ECE_all_2 <- 1.06 * sigma_y_ECE_all_2 * n_ECE_all_2^(-1/5)

cat("h optimal en mètres pour X:", h_silverman_x_ECE_all_2, "\n")
cat("h optimal en mètres pour Y:", h_silverman_y_ECE_all_2, "\n")

# locs_spa <- st_transform(locs, crs = 32630)
locs_spa_ECE_all_2 <- as(locs_ECE_all_2_32630, "Spatial")

# Appliquer kernelUD avec h estimé par Silverman
kud_ECE_all_2 <- kernelUD(locs_spa_ECE_all_2["ECE_all_2"], grid = as(raster_100x100_32630, "SpatialPixels"),
                         h = mean(c(h_silverman_x_ECE_all_2, h_silverman_y_ECE_all_2)))

# Créer une liste pour stocker les résultats
UDmaps_list_ECE_all_2 <- lapply(names(kud_ECE_all_2), function(ECE_all_2) {
  
  print(ECE_all_2)
  
  # Extraire l'estimation de densité pour un ID spécifique
  kud_single_ECE_all_2 <- kud_ECE_all_2[[ECE_all_2]]
  rast_ECE_all_2 <- rast(kud_single_ECE_all_2)
  contour_ECE_all_2 <- as.contour(rast_ECE_all_2)
  sf_ECE_all_2 <- st_as_sf(contour_ECE_all_2)
  cast_ECE_all_2 <- st_cast(sf_ECE_all_2, "POLYGON")
  cast_ECE_all_2$ECE_all_2 <- ECE_all_2
  
  return(cast_ECE_all_2)
})

# Fusionner tous les ID dans un seul objet sf
UDMap_final_ECE_all_2 <- do.call(rbind, UDmaps_list_ECE_all_2)

UDMap_final_ECE_all_2$ECE_all_2 <- as.factor(UDMap_final_ECE_all_2$ECE_all_2)

st_crs(UDMap_final_ECE_all_2) == st_crs(RMO)  # Vérifie si les projections sont identiques
UDMap_final_ECE_all_2 <- st_transform(UDMap_final_ECE_all_2, st_crs(RMO))
table(is.na(UDMap_final_ECE_all_2$ECE_all_2))

# plot 
tmap_mode("view")

UDMap_final_ECE_all_2$ECE_all_2 <- as.factor(UDMap_final_ECE_all_2$ECE_all_2)

UDMap_ECE_all_2 <- tm_shape(RMO) +
  tm_polygons() +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(UDMap_final_ECE_all_2) + 
  tm_polygons(border.col = "grey", fill = "ECE_all_2", fill_fill_alpha = 0.2) ; UDMap_ECE_all_2

# # tmap_save(UDMap_ECE_all_2, paste0(data_image_path, "/UDMap_roosting_ECE_all_2.html"), dpi = 600)


###
####
# Home Range -------------------------------------------------------------------
####
###

# Charger les données en lat/lon (EPSG:4326)
coords_HR_ID <- GPS %>% 
  # filter(behavior == "roosting") %>% 
  dplyr::select(ID,lon,lat) %>% 
  st_drop_geometry() %>% 
  na.omit()

# Transformer en objet spatial (EPSG:4326)
locs_HR_ID <- st_as_sf(coords_HR_ID, coords = c("lon", "lat"), crs = 4326)

# Reprojeter en système métrique (ex. UTM zone 30N - EPSG:32630 pour la France)
locs_HR_ID_32630 <- st_transform(locs_HR_ID, crs = 32630)  # Adapter le CRS à votre région

# Reprojection du raster
crs_utm <- CRS("+init=epsg:32630") # Définir le CRS cible (EPSG:32630 = UTM zone 30N)
raster_100x100_32630 <- projectRaster(raster_100x100, crs = crs_utm)
crs(raster_100x100_32630)

# Extraire les coordonnées reprojetées
coords_HR_ID_32630 <- st_coordinates(locs_HR_ID_32630)

# Règle de Silverman
sigma_x_HR_ID <- sd(coords_HR_ID_32630[,1])  # Écart-type en X (mètres)
sigma_y_HR_ID <- sd(coords_HR_ID_32630[,2])  # Écart-type en Y (mètres)
n_HR_ID <- nrow(coords_HR_ID_32630)  # Nombre de points

h_silverman_x_HR_ID <- 1.06 * sigma_x_HR_ID * n_HR_ID^(-1/5)
h_silverman_y_HR_ID <- 1.06 * sigma_y_HR_ID * n_HR_ID^(-1/5)

cat("h optimal en mètres pour X:", h_silverman_x_HR_ID, "\n")
cat("h optimal en mètres pour Y:", h_silverman_y_HR_ID, "\n")

# locs_spa <- as(locs_m, "Spatial")

# locs_spa <- st_transform(locs_HR_ID, crs = 32630)
locs_spa_HR_ID <- as(locs_HR_ID_32630, "Spatial")

# Appliquer kernelUD avec h estimé par Silverman
kud_HR_ID <- kernelUD(locs_spa_HR_ID["ID"], grid = as(raster_100x100_32630, "SpatialPixels"),
                            h = mean(c(h_silverman_x_HR_ID, h_silverman_y_HR_ID)))

kde_hr_95 <- getverticeshr(kud_HR_ID, 95)
kde_hr_50 <- getverticeshr(kud_HR_ID, 50)

# Conversion des home range KDE en sf
kde_hr_95_sf <- st_as_sf(kde_hr_95)
kde_hr_50_sf <- st_as_sf(kde_hr_50)

# plot(kde_hr_95, col = "blue", border = "black", lwd = 2, main = "Home range 95% et 50%")
# plot(kde_hr_50, col = "red", add = TRUE)

# Créer une liste pour stocker les résultats
UDmaps_list_HR_ID <- lapply(names(kud_HR_ID), function(ID) {

  print(ID)

  # Extraire l'estimation de densité pour un ID spécifique
  kud_single_HR_ID <- kud_HR_ID[[ID]]
  rast_HR_ID <- rast(kud_single_HR_ID)
  contour_HR_ID <- as.contour(rast_HR_ID)
  sf_HR_ID <- st_as_sf(contour_HR_ID)
  cast_HR_ID <- st_cast(sf_HR_ID, "POLYGON")
  cast_HR_ID$ID <- ID

  return(cast_HR_ID)

})

# Fusionner tous les ID dans un seul objet sf
UDMap_final_HR_ID <- do.call(rbind, UDmaps_list_HR_ID)
# UDMap_final_hr_95 <- do.call(rbind, UDmaps_list_hr_95)

UDMap_final_HR_ID$ID <- as.factor(UDMap_final_HR_ID$ID)

# groupe plot
ID_list <- unique(UDMap_final_HR_ID$ID)
ID_gp_1 <- ID_list[1:15]
ID_gp_2 <- ID_list[16:30]
ID_gp_3 <- ID_list[31:46]

kde_hr_95_sf_gp1 <- kde_hr_95_sf %>%
  filter(id %in% ID_gp_1)
kde_hr_95_sf_gp2 <- kde_hr_95_sf %>%
  filter(id %in% ID_gp_2)
kde_hr_95_sf_gp3 <- kde_hr_95_sf %>%
  filter(id %in% ID_gp_3)

kde_hr_50_sf_gp1 <- kde_hr_50_sf %>%
  filter(id %in% ID_gp_1)
kde_hr_50_sf_gp2 <- kde_hr_50_sf %>%
  filter(id %in% ID_gp_2)
kde_hr_50_sf_gp3 <- kde_hr_50_sf %>%
  filter(id %in% ID_gp_3)

# plot
tmap_mode("view")

palette_viri = viridis(10, begin = 0, end = 1, direction = 1, option = "plasma")

UDMap_HR_ID_gp1 <- tm_shape(RMO) +
  tm_polygons() +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(kde_hr_95_sf_gp1) +
  tm_lines(col = "id",
             palette = palette_viri) +
  tm_shape(kde_hr_50_sf_gp1) +
  tm_polygons(fill = "id",
              palette = palette_viri)

UDMap_HR_ID_gp2 <- tm_shape(RMO) +
  tm_polygons() +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(kde_hr_95_sf_gp2) +
  tm_lines(col = "id",
           palette = palette_viri) +
  tm_shape(kde_hr_50_sf_gp2) +
  tm_polygons(fill = "id",
              palette = palette_viri)

UDMap_HR_ID_gp3 <- tm_shape(RMO) +
  tm_polygons() +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(kde_hr_95_sf_gp3) +
  tm_lines(col = "id",
           palette = palette_viri) +
  tm_shape(kde_hr_50_sf_gp3) +
  tm_polygons(fill = "id",
              palette = palette_viri)

UDMap_HR_ID <- tmap_arrange(UDMap_HR_ID_gp1, UDMap_HR_ID_gp2, UDMap_HR_ID_gp3) ; UDMap_HR_ID

### Pourcentage dans la réserve -----

#### 95% ------

kde_hr_95_sf_2154 <- st_transform(kde_hr_95_sf, crs = 2154)

# Calculate area and tidy up
intersect_hr_95 <- st_intersection(kde_hr_95_sf_2154, RMO) %>% 
  mutate(intersect_area = st_area(.)) %>%   # create new column with shape area
  dplyr::select(id, intersect_area) %>%   # only select columns needed to merge
  st_drop_geometry()  # drop geometry as we don't need it

# Create a fresh area variable for counties
kde_hr_95_sf_2154 <- mutate(kde_hr_95_sf_2154, county_area = st_area(kde_hr_95_sf_2154))

# Merge by county name
kde_hr_95_sf_2154 <- merge(kde_hr_95_sf_2154, intersect_hr_95, by = "id", all.x = TRUE)

# Calculate coverage
kde_hr_95_sf_2154 <- kde_hr_95_sf_2154 %>% 
  mutate(coverage = as.numeric(intersect_area/county_area))

HR_95_pourc_RN <- tm_shape(RMO) +
  tm_polygons() +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(kde_hr_95_sf_2154) +  
  tm_polygons(fill = "coverage", fill_fill_alpha = 0.2,
              palette = palette_viri) ; HR_95_pourc_RN

# tmap_save(HR_95_pourc_RN, paste0(data_image_path, "/UDMap_HR_95_pourc_RN.html"), dpi = 600)

mean_hr_95_pourc_rn <- mean(kde_hr_95_sf_2154$coverage, na.rm = T)

print("pourcentage moyen des home range dans la réserve naturelle :")
mean_hr_95_pourc_rn

#### 50% ------

kde_hr_50_sf_2154 <- st_transform(kde_hr_50_sf, crs = 2154)

# Calculate area and tidy up
intersect_hr_50 <- st_intersection(kde_hr_50_sf_2154, RMO) %>% 
  mutate(intersect_area = st_area(.)) %>%   # create new column with shape area
  dplyr::select(id, intersect_area) %>%   # only select columns needed to merge
  st_drop_geometry()  # drop geometry as we don't need it

# Create a fresh area variable for counties
kde_hr_50_sf_2154 <- mutate(kde_hr_50_sf_2154, county_area = st_area(kde_hr_50_sf_2154))

# Merge by county name
kde_hr_50_sf_2154 <- merge(kde_hr_50_sf_2154, intersect_hr_50, by = "id", all.x = TRUE)

# Calculate coverage
kde_hr_50_sf_2154 <- kde_hr_50_sf_2154 %>% 
  mutate(coverage = as.numeric(intersect_area/county_area))

HR_50_pourc_RN <- tm_shape(RMO) +
  tm_polygons() +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(kde_hr_50_sf_2154) +  
  tm_polygons(fill = "coverage", fill_fill_alpha = 0.2,
              palette = palette_viri) ; HR_50_pourc_RN

# tmap_save(HR_50_pourc_RN, paste0(data_image_path, "/UDMap_HR_50_pourc_RN.html"), dpi = 600)

mean_hr_50_pourc_rn <- mean(kde_hr_50_sf_2154$coverage, na.rm = T)

print("pourcentage moyen des home range dans la réserve naturelle :")
mean_hr_50_pourc_rn

###
####
# Distance roosting - alimentation ---------------------------------------------
####
###

## roosting 50% ----------------------------------------------------------------

# Charger les données en lat/lon (EPSG:4326)
coords_HR_ID_repo <- GPS %>% 
  filter(behavior == "roosting") %>%
  dplyr::select(ID,lon,lat) %>% 
  st_drop_geometry() %>% 
  na.omit()

# Transformer en objet spatial (EPSG:4326)
locs_HR_ID_repo <- st_as_sf(coords_HR_ID_repo, coords = c("lon", "lat"), crs = 4326)

# Reprojeter en système métrique (ex. UTM zone 30N - EPSG:32630 pour la France)
locs_HR_ID_repo_32630 <- st_transform(locs_HR_ID_repo, crs = 32630)  # Adapter le CRS à votre région

# Reprojection du raster
crs_utm <- CRS("+init=epsg:32630") # Définir le CRS cible (EPSG:32630 = UTM zone 30N)
raster_100x100_32630 <- projectRaster(raster_100x100, crs = crs_utm)
crs(raster_100x100_32630)

# Extraire les coordonnées reprojetées
coords_HR_ID_repo_32630 <- st_coordinates(locs_HR_ID_repo_32630)

# Règle de Silverman
sigma_x_HR_ID_repo <- sd(coords_HR_ID_repo_32630[,1])  # Écart-type en X (mètres)
sigma_y_HR_ID_repo <- sd(coords_HR_ID_repo_32630[,2])  # Écart-type en Y (mètres)
n_HR_ID_repo <- nrow(coords_HR_ID_repo_32630)  # Nombre de points

h_silverman_x_HR_ID_repo <- 1.06 * sigma_x_HR_ID_repo * n_HR_ID_repo^(-1/5)
h_silverman_y_HR_ID_repo <- 1.06 * sigma_y_HR_ID_repo * n_HR_ID_repo^(-1/5)

cat("h optimal en mètres pour X:", h_silverman_x_HR_ID_repo, "\n")
cat("h optimal en mètres pour Y:", h_silverman_y_HR_ID_repo, "\n")

# locs_spa <- as(locs_m, "Spatial")

# locs_spa <- st_transform(locs_HR_ID_repo, crs = 32630)
locs_spa_HR_ID_repo <- as(locs_HR_ID_repo_32630, "Spatial")

# Appliquer kernelUD avec h estimé par Silverman
kud_HR_ID_repo <- kernelUD(locs_spa_HR_ID_repo["ID"], grid = as(raster_100x100_32630, "SpatialPixels"),
                      h = mean(c(h_silverman_x_HR_ID_repo, h_silverman_y_HR_ID_repo)))

kde_hr_50_ID_repo <- getverticeshr(kud_HR_ID_repo, 50)

# Conversion des home range KDE en sf
kde_hr_50_ID_repo_sf <- st_as_sf(kde_hr_50_ID_repo)

# centroID
repo_centro <- kde_hr_50_ID_repo_sf %>% 
  st_centroid()

## alimentation 50% ----------------------------------------------------------------

# Charger les données en lat/lon (EPSG:4326)
coords_HR_ID_alim <- GPS %>% 
  filter(behavior == "foraging") %>%
  dplyr::select(ID,lon,lat) %>% 
  st_drop_geometry() %>% 
  na.omit()

# Transformer en objet spatial (EPSG:4326)
locs_HR_ID_alim <- st_as_sf(coords_HR_ID_alim, coords = c("lon", "lat"), crs = 4326)

# Reprojeter en système métrique (ex. UTM zone 30N - EPSG:32630 pour la France)
locs_HR_ID_alim_32630 <- st_transform(locs_HR_ID_alim, crs = 32630)  # Adapter le CRS à votre région

# Reprojection du raster
crs_utm <- CRS("+init=epsg:32630") # Définir le CRS cible (EPSG:32630 = UTM zone 30N)
raster_100x100_32630 <- projectRaster(raster_100x100, crs = crs_utm)
crs(raster_100x100_32630)

# Extraire les coordonnées reprojetées
coords_HR_ID_alim_32630 <- st_coordinates(locs_HR_ID_alim_32630)

# Règle de Silverman
sigma_x_HR_ID_alim <- sd(coords_HR_ID_alim_32630[,1])  # Écart-type en X (mètres)
sigma_y_HR_ID_alim <- sd(coords_HR_ID_alim_32630[,2])  # Écart-type en Y (mètres)
n_HR_ID_alim <- nrow(coords_HR_ID_alim_32630)  # Nombre de points

h_silverman_x_HR_ID_alim <- 1.06 * sigma_x_HR_ID_alim * n_HR_ID_alim^(-1/5)
h_silverman_y_HR_ID_alim <- 1.06 * sigma_y_HR_ID_alim * n_HR_ID_alim^(-1/5)

cat("h optimal en mètres pour X:", h_silverman_x_HR_ID_alim, "\n")
cat("h optimal en mètres pour Y:", h_silverman_y_HR_ID_alim, "\n")

# locs_spa <- as(locs_m, "Spatial")

# locs_spa <- st_transform(locs_HR_ID_alim, crs = 32630)
locs_spa_HR_ID_alim <- as(locs_HR_ID_alim_32630, "Spatial")

# Appliquer kernelUD avec h estimé par Silverman
kud_HR_ID_alim <- kernelUD(locs_spa_HR_ID_alim["ID"], grid = as(raster_100x100_32630, "SpatialPixels"),
                           h = mean(c(h_silverman_x_HR_ID_alim, h_silverman_y_HR_ID_alim)))

kde_hr_50_ID_alim <- getverticeshr(kud_HR_ID_alim, 50)

# Conversion des home range KDE en sf
kde_hr_50_ID_alim_sf <- st_as_sf(kde_hr_50_ID_alim)

# centroid
alim_centro <- kde_hr_50_ID_alim_sf %>% 
  st_centroid()

## distance centroid ----

alim_centro_2 <- alim_centro %>%
  rename(geom_alim = geometry, area_alim = area) %>% 
  mutate(lon_alim = st_coordinates(.)[,1], lat_alim = st_coordinates(.)[,2]) %>% 
  st_drop_geometry()

repo_centro_2 <- repo_centro %>%
  rename(geom_repo = geometry, area_repo = area) %>% 
  mutate(lon_repo = st_coordinates(.)[,1], lat_repo = st_coordinates(.)[,2]) %>% 
  st_drop_geometry()

repo_alim_centro <- repo_centro_2 %>%
  left_join(alim_centro_2, by = "id")

pts_repro <- st_as_sf(repo_alim_centro, coords = c("lon_repo", "lat_repo"), crs = 32630)
pts_alim <- st_as_sf(repo_alim_centro, coords = c("lon_alim", "lat_alim"), crs = 32630)

dist_repo_alim <- st_distance(x = pts_repro$geometry, y = pts_alim$geometry, by_element = TRUE)

repo_alim_centro$dist_repo_alim <- as.numeric(dist_repo_alim)

## distance moyenne ----

print("distance moyenne entre les centroid des core home range (50%) roosting vs foraging:")
dist_mean <- mean(repo_alim_centro$dist) ; dist_mean
print("+/-")
dist_sd <- sd(repo_alim_centro$dist) ; dist_sd

## plot ----

dist_roosting_foraging_plot <- ggplot(repo_alim_centro, aes(reorder(id, dist_repo_alim), dist_repo_alim, 
                                             color = dist_repo_alim)) +
 geom_point(size = 4) +
  geom_hline(yintercept = dist_mean, color = "red", size = 1) +
  geom_hline(yintercept = dist_mean + dist_sd, color = "red", linetype = "dashed") +
  geom_hline(yintercept = dist_mean - dist_sd, color = "red", linetype = "dashed") +
  scale_color_viridis(option = "plasma") +
  coord_flip() +
  theme_classic() +
  labs(title="",
       x ="Individu", y = "Distance entre les centroids des core home 
range (50%) du foraging vs roosting", 
       fill="", 
       color = "Distance (m)") ; dist_roosting_foraging_plot

ggsave(paste0(data_image_path, "/dist_roosting_foraging_plot.png"), 
       plot = dist_roosting_foraging_plot, width = 6, height = 9, dpi = 300)

## distance ~ sexe ----

sexe_dt <- GPS %>% 
  st_drop_geometry() %>% 
  dplyr::select(ID, sex) %>% 
  na.omit() %>% 
  distinct()

repo_alim_centro_2 <- repo_alim_centro %>% 
  rename(ID = id)

dist_sexe_dt <- repo_alim_centro_2 %>% 
  left_join(sexe_dt) %>% 
  na.omit()

# test comparaison de moyenne

shapiro.test(dist_sexe_dt$dist_repo_alim[dist_sexe_dt$sex == "F"]) 
shapiro.test(dist_sexe_dt$dist_repo_alim[dist_sexe_dt$sex == "M"])
var.test(dist_sexe_dt$dist_repo_alim[dist_sexe_dt$sex == "F"], dist_sexe_dt$dist_repo_alim[dist_sexe_dt$sex == "M"])  

comp_moy_sexe = t.test(dist_sexe_dt$dist_repo_alim[dist_sexe_dt$sex == "F"], 
                       dist_sexe_dt$dist_repo_alim[dist_sexe_dt$sex == "M"], 
                       var.equal=F) ; comp_moy_sexe

summary(lm(dist_sexe_dt$dist_repo_alim ~ dist_sexe_dt$sex))

## distance ~ age ----

age_dt <- GPS %>% 
  st_drop_geometry() %>% 
  dplyr::select(ID, age) %>% 
  na.omit() %>% 
  distinct()

repo_alim_centro_2 <- repo_alim_centro %>% 
  rename(ID = id)

dist_age_dt <- repo_alim_centro_2 %>% 
  left_join(age_dt) %>% 
  na.omit()

# test comparaison de moyenne

shapiro.test(dist_age_dt$dist_repo_alim[dist_age_dt$age == "adult"]) 
shapiro.test(dist_age_dt$dist_repo_alim[dist_age_dt$age == "juv"])

comp_moy_age_juv_adult = t.test(dist_age_dt$dist_repo_alim[dist_age_dt$age == "juv"], 
                                       dist_age_dt$dist_repo_alim[dist_age_dt$age == "adult"], 
                                       var.equal=F) ; comp_moy_age_juv_adult

summary(lm(dist_age_dt$dist_repo_alim ~ dist_age_dt$age))

## distance ~ sex + age ----

sexe_age_dt <- GPS %>% 
  st_drop_geometry() %>% 
  dplyr::select(ID, sex, age) %>% 
  mutate(sex_age = paste0(sex, "_", age)) %>% 
  na.omit() %>% 
  distinct()

dist_sexe_age_dt <- repo_alim_centro_2 %>% 
  left_join(sexe_age_dt) %>% 
  na.omit()

summary(lm(dist_sexe_age_dt$dist_repo_alim ~ dist_sexe_age_dt$age*dist_sexe_age_dt$sex))

summary(lm(dist_sexe_age_dt$dist_repo_alim ~ dist_sexe_age_dt$sex_age))

###
####
# Temps passé dans la réserve --------------------------------------------------
####
###

## all point ----

GPS_2154 <- st_transform(GPS, crs = 2154)

# temps global

all_pts_everywhere_2 <- GPS_2154 %>% 
  dplyr::select(ID, datetime) %>% 
  st_drop_geometry() %>% 
  distinct()

all_pts_everywhere_3 <- all_pts_everywhere_2 %>% 
  group_by(ID) %>%
  distinct() %>% 
  summarize(n_everywhere = n()) 

all_pts_everywhere_3$tps_h_everywhere <- all_pts_everywhere_3$n_everywhere/2
all_pts_everywhere_3$tps_d_everywhere <- all_pts_everywhere_3$tps_h_everywhere/24
all_pts_everywhere_3$tps_m_everywhere <- all_pts_everywhere_3$tps_d_everywhere/30.5
all_pts_everywhere_3$tps_y_everywhere <- all_pts_everywhere_3$tps_m_everywhere/12

# temps dans la réserve

all_pts_inRMO <- st_intersection(GPS_2154, RMO)

all_pts_inRMO_2 <- all_pts_inRMO %>% 
  dplyr::select(ID, datetime) %>% 
  st_drop_geometry() %>% 
  distinct()

all_pts_inRMO_3 <- all_pts_inRMO_2 %>% 
  group_by(ID) %>%
  distinct() %>% 
  summarize(n_inRMO = n()) 

all_pts_inRMO_3$tps_h_inRMO <- all_pts_inRMO_3$n_inRMO/2
all_pts_inRMO_3$tps_d_inRMO <- all_pts_inRMO_3$tps_h_inRMO/24
all_pts_inRMO_3$tps_m_inRMO <- all_pts_inRMO_3$tps_d_inRMO/30.5
all_pts_inRMO_3$tps_y_inRMO <- all_pts_inRMO_3$tps_m_inRMO/12

# join dans la réserve et everywhere

all_pts_inRMO_everywhere <- left_join(all_pts_inRMO_3, all_pts_everywhere_3)

all_pts_inRMO_everywhere <- all_pts_inRMO_everywhere %>% 
  mutate(pourc_tps_inRMO = tps_h_inRMO/tps_h_everywhere)

mean_pourc_tps_inRMO <- mean(all_pts_inRMO_everywhere$pourc_tps_inRMO, na.rm = T)

print("Proportion du temps passé dans la réserve vs hors réserve:")
mean_pourc_tps_inRMO

## roosting ----

GPS_2154 <- st_transform(GPS, crs = 2154)

# temps global

all_pts_everywhere_roosting <- GPS_2154 %>% 
  filter(behavior=="roosting") %>% 
  dplyr::select(ID, datetime) %>% 
  st_drop_geometry() %>% 
  distinct()

all_pts_everywhere_roosting_2 <- all_pts_everywhere_roosting %>% 
  group_by(ID) %>%
  distinct() %>% 
  summarize(n_everywhere = n()) 

all_pts_everywhere_roosting_2$tps_h_everywhere <- all_pts_everywhere_roosting_2$n_everywhere/2
all_pts_everywhere_roosting_2$tps_d_everywhere <- all_pts_everywhere_roosting_2$tps_h_everywhere/24
all_pts_everywhere_roosting_2$tps_m_everywhere <- all_pts_everywhere_roosting_2$tps_d_everywhere/30.5
all_pts_everywhere_roosting_2$tps_y_everywhere <- all_pts_everywhere_roosting_2$tps_m_everywhere/12

# temps dans la réserve

GPS_2154_roosting <- GPS_2154 %>% 
  filter(behavior=="roosting")

all_pts_inRMO_roosting <- st_intersection(GPS_2154_roosting, RMO)

all_pts_inRMO_roosting_2 <- all_pts_inRMO_roosting %>% 
  dplyr::select(ID, datetime) %>% 
  st_drop_geometry() %>% 
  distinct()

all_pts_inRMO_roosting_3 <- all_pts_inRMO_roosting_2 %>% 
  group_by(ID) %>%
  distinct() %>% 
  summarize(n_inRMO = n()) 

all_pts_inRMO_roosting_3$tps_h_inRMO <- all_pts_inRMO_roosting_3$n_inRMO/2
all_pts_inRMO_roosting_3$tps_d_inRMO <- all_pts_inRMO_roosting_3$tps_h_inRMO/24
all_pts_inRMO_roosting_3$tps_m_inRMO <- all_pts_inRMO_roosting_3$tps_d_inRMO/30.5
all_pts_inRMO_roosting_3$tps_y_inRMO <- all_pts_inRMO_roosting_3$tps_m_inRMO/12

# join dans la réserve et everywhere

all_pts_inRMO_everywhere_roosting <- left_join(all_pts_inRMO_roosting_3, all_pts_everywhere_roosting_2)

all_pts_inRMO_everywhere_roosting <- all_pts_inRMO_everywhere_roosting %>% 
  mutate(pourc_tps_inRMO = tps_h_inRMO/tps_h_everywhere)

mean_pourc_tps_inRMO_roosting <- mean(all_pts_inRMO_everywhere_roosting$pourc_tps_inRMO, na.rm = T)

print("Proportion du temps passé dans la réserve vs hors réserve pour le roosting:")
mean_pourc_tps_inRMO_roosting

## foraging ----

GPS_2154 <- st_transform(GPS, crs = 2154)

# temps global

all_pts_everywhere_foraging <- GPS_2154 %>% 
  filter(behavior=="foraging") %>% 
  dplyr::select(ID, datetime) %>% 
  st_drop_geometry() %>% 
  distinct()

all_pts_everywhere_foraging_2 <- all_pts_everywhere_foraging %>% 
  group_by(ID) %>%
  distinct() %>% 
  summarize(n_everywhere = n()) 

all_pts_everywhere_foraging_2$tps_h_everywhere <- all_pts_everywhere_foraging_2$n_everywhere/2
all_pts_everywhere_foraging_2$tps_d_everywhere <- all_pts_everywhere_foraging_2$tps_h_everywhere/24
all_pts_everywhere_foraging_2$tps_m_everywhere <- all_pts_everywhere_foraging_2$tps_d_everywhere/30.5
all_pts_everywhere_foraging_2$tps_y_everywhere <- all_pts_everywhere_foraging_2$tps_m_everywhere/12

# temps dans la réserve

GPS_2154_foraging <- GPS_2154 %>% 
  filter(behavior=="foraging")

all_pts_inRMO_foraging <- st_intersection(GPS_2154_foraging, RMO)

all_pts_inRMO_foraging_2 <- all_pts_inRMO_foraging %>% 
  dplyr::select(ID, datetime) %>% 
  st_drop_geometry() %>% 
  distinct()

all_pts_inRMO_foraging_3 <- all_pts_inRMO_foraging_2 %>% 
  group_by(ID) %>%
  distinct() %>% 
  summarize(n_inRMO = n()) 

all_pts_inRMO_foraging_3$tps_h_inRMO <- all_pts_inRMO_foraging_3$n_inRMO/2
all_pts_inRMO_foraging_3$tps_d_inRMO <- all_pts_inRMO_foraging_3$tps_h_inRMO/24
all_pts_inRMO_foraging_3$tps_m_inRMO <- all_pts_inRMO_foraging_3$tps_d_inRMO/30.5
all_pts_inRMO_foraging_3$tps_y_inRMO <- all_pts_inRMO_foraging_3$tps_m_inRMO/12

# join dans la réserve et everywhere

all_pts_inRMO_everywhere_foraging <- left_join(all_pts_inRMO_foraging_3, all_pts_everywhere_foraging_2)

all_pts_inRMO_everywhere_foraging <- all_pts_inRMO_everywhere_foraging %>% 
  mutate(pourc_tps_inRMO = tps_h_inRMO/tps_h_everywhere)

mean_pourc_tps_inRMO_foraging <- mean(all_pts_inRMO_everywhere_foraging$pourc_tps_inRMO, na.rm = T)

print("Proportion du temps passé dans la réserve vs hors réserve pour le foraging:")
mean_pourc_tps_inRMO_foraging


beep()



