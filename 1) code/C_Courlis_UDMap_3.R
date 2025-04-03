
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
# ZOOM_A <- st_transform(st_as_sf(st_as_sfc(st_bbox(c(xmin = -1.245, xmax = -1.18, ymax = 45.975, ymin = 45.825), crs = st_crs(4326)))), crs = 2154) 
# ZOOM_B <- st_transform(st_as_sf(st_as_sfc(st_bbox(c(xmin = -1.13, xmax = -1.06, ymax = 45.975, ymin = 45.923), crs = st_crs(4326)))), crs = 2154) 
# ZOOM_C <- st_transform(st_as_sf(st_as_sfc(st_bbox(c(xmin = -1.13, xmax = -1.06, ymax = 45.923, ymin = 45.865), crs = st_crs(4326)))), crs = 2154) 
# ZOOM_D <- st_transform(st_as_sf(st_as_sfc(st_bbox(c(xmin = -1.18, xmax = -1.08, ymax = 45.865, ymin = 45.81), crs = st_crs(4326)))), crs = 2154) 
# ZOOM_E <- st_transform(st_as_sf(st_as_sfc(st_bbox(c(xmin = -0.95, xmax = -1.08, ymax = 45.865, ymin = 45.795), crs = st_crs(4326)))), crs = 2154) 
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

###
####
# GPS DATA to load -------------------------------------------------------------
####
###

GPS <- st_read(file.path(data_generated_path, "GPS_clean.gpkg"))
GPS$y_m_d <- ymd(as.Date(GPS$datetime))
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

# Methode de Silverman + écart interquantile (IQR)

## GLOB -----------------------------------------------------------------------

coords_roosting <- GPS %>% 
  filter(behavior == "roosting") %>% 
  dplyr::select(lon,lat) %>% 
  st_drop_geometry() %>% 
  na.omit()

locs_roosting <- st_as_sf(coords_roosting, coords = c("lon", "lat"), crs = 4326)
locs_roosting_32630 <- st_transform(locs_roosting, crs = 32630)  # Reprojeter EPSG:32630 pour la France
coords_roosting_32630 <- st_coordinates(locs_roosting_32630) # Extraire les coordonnées reprojetées

# raster/grid
crs_utm <- "EPSG:32630"
SpatRaster_32630 <- project(raster_100x100, crs_utm)  # Reprojection du raster
RasterLayer <- raster(SpatRaster_32630) # Convertir SpatRaster en RasterLayer
SpatialPixels <- as(RasterLayer, "SpatialPixels") # Convertir RasterLayer en SpatialPixels

# Règle de Silverman
sigma_x_roosting_glob <- sd(coords_roosting_32630[,1])  # Écart-type en X (mètres)
sigma_y_roosting_glob <- sd(coords_roosting_32630[,2])  # Écart-type en Y (mètres)
n_roosting_glob <- nrow(coords_roosting)  # Nombre de points
h_silverman_x_roosting_glob <- 1.06 * sigma_x_roosting_glob * n_roosting_glob^(-1/5) / 2
h_silverman_y_roosting_glob <- 1.06 * sigma_y_roosting_glob * n_roosting_glob^(-1/5) / 2
# cat("h optimal en mètres pour X:", h_silverman_x_roosting, "\n")
# cat("h optimal en mètres pour Y:", h_silverman_y_roosting, "\n")
locs_spa_roosting <- as(locs_roosting_32630, "Spatial")
# Appliquer kernelUD avec h estimé par Silverman
kud_roosting_glob <- kernelUD(locs_spa_roosting, 
                                      grid = SpatialPixels, 
                                      h = mean(c(h_silverman_x_roosting_glob, h_silverman_y_roosting_glob)))

# Visualiser la densité de noyau
# par(mfrow = c(1, 1))
# image(kud)

# Estimation des isoclines 
rast_roosting_glob <- rast(kud_roosting_glob)
courtour_roosting_glob <- as.contour(rast_roosting_glob)
sf_roosting_glob <- st_as_sf(courtour_roosting_glob)
cast_roosting_glob <- st_cast(sf_roosting_glob, "POLYGON")

# plot
tmap_mode("view")
UDMap_roosting_glob <- tm_scalebar() +
  tm_shape(RMO) +
  tm_polygons() +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(cast_roosting_glob) + 
  tm_polygons(border.col = "grey", fill = "level", fill_alpha = 0.2, 
              palette = viridis(10, begin = 0, end = 1, 
                                direction = 1, option = "plasma")) +
  tm_shape(terre_mer) +
  tm_lines(col = "darkblue", lwd = 0.5); UDMap_roosting_glob

## ZOOM -----------------------------------------------------------------------

crs_utm <- "EPSG:32630"
ZOOM <- c("A","B","C","D","E")
resolution_ZOOM = 10
lettre = "A"
cast_roosting_ZOOM_all = NULL

for (lettre in ZOOM){
  # in ZOOM
  ZOOM <- st_read(paste0(data_generated_path,"ZOOM_",lettre,".gpkg"))
  ZOOM <- st_transform(ZOOM, crs = 4326)
  GPS_ZOOM <- st_intersection(GPS, ZOOM) 
  coords_roosting_ZOOM <- GPS_ZOOM %>% 
    filter(behavior == "roosting") %>% 
    dplyr::select(lon,lat) %>% 
    st_drop_geometry() %>% 
    na.omit()
  
  locs_roosting_ZOOM <- st_as_sf(coords_roosting_ZOOM, coords = c("lon", "lat"), crs = 4326)
  locs_roosting_ZOOM <- st_transform(locs_roosting_ZOOM, crs = 32630)  # Reprojeter EPSG:32630 pour la France
  coords_roosting_ZOOM <- st_coordinates(locs_roosting_ZOOM) # Extraire les coordonnées reprojetées
  
  # raster/grid
  grid_ZOOM <- st_read(paste0(data_generated_path, "grid_ZOOM_",lettre,".gpkg"))
  raster_ZOOM <- rast(grid_ZOOM, resolution = resolution_ZOOM, crs="EPSG:2154")
  SpatRaster_ZOOM <- project(raster_ZOOM, crs_utm)  # Reprojection du raster
  RasterLayer_ZOOM <- raster(SpatRaster_ZOOM) # Convertir SpatRaster en RasterLayer
  SpatialPixels_ZOOM <- as(RasterLayer_ZOOM, "SpatialPixels") # Convertir RasterLayer en SpatialPixels
  
  # Règle de Silverman
  sigma_x <- sd(coords_roosting_ZOOM[,1])  # Écart-type en X (mètres)
  sigma_y <- sd(coords_roosting_ZOOM[,2])  # Écart-type en Y (mètres)
  n_roosting <- nrow(coords_roosting_ZOOM)  # Nombre de points
  h_silverman_x <- 1.06 * sigma_x * n_roosting^(-1/5) / 2
  h_silverman_y <- 1.06 * sigma_y * n_roosting^(-1/5) / 2
  cat("h optimal en mètres pour X:", h_silverman_x, "\n")
  cat("h optimal en mètres pour Y:", h_silverman_y, "\n")
  locs_spa_roosting_ZOOM <- as(locs_roosting_ZOOM, "Spatial")
  # Appliquer kernelUD avec h estimé par Silverman
  kud_roosting_ZOOM <- kernelUD(locs_spa_roosting_ZOOM, 
                                grid = SpatialPixels_ZOOM, 
                                h = mean(c(h_silverman_x, h_silverman_y)))
  
  # Estimation des isoclines 
  rast_roosting_ZOOM <- rast(kud_roosting_ZOOM)
  courtour_roosting_ZOOM <- as.contour(rast_roosting_ZOOM)
  sf_roosting_ZOOM <- st_as_sf(courtour_roosting_ZOOM)
  cast_roosting_ZOOM <- st_cast(sf_roosting_ZOOM, "POLYGON")
  
  cast_roosting_ZOOM$ZOOM <- lettre
  cast_roosting_ZOOM_all <- rbind(cast_roosting_ZOOM_all, cast_roosting_ZOOM)

}

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
  tm_shape(cast_roosting_ZOOM_all) + 
  tm_polygons(border.col = "grey", fill = "level", fill_alpha = 0.2, 
              palette = viridis(10, begin = 0, end = 1, 
                                direction = 1, option = "plasma")) +
  tm_shape(terre_mer) +
  tm_lines(col = "darkblue", lwd = 0.5); UDMap_roosting_ZOOM

## Age -------------------------------------------------------------------------

### GLOB -----------------------------------------------------------------------

# GPS point roosting ---

# All zone
coords_roosting_age <- GPS %>% 
  filter(behavior == "roosting") %>% 
  dplyr::select(lon,lat,age) %>% 
  st_drop_geometry() %>% 
  na.omit()

locs_roosting_age <- st_as_sf(coords_roosting_age, coords = c("lon", "lat"), crs = 4326)
locs_roosting_age_32630 <- st_transform(locs_roosting_age, crs = 32630)  # Reprojeter EPSG:32630 pour la France
coords_roosting_age_32630 <- st_coordinates(locs_roosting_age_32630) # Extraire les coordonnées reprojetées

# raster/grid
crs_utm <- "EPSG:32630"
SpatRaster_100x100_32630 <- project(raster_100x100, crs_utm)  # Reprojection du raster
RasterLayer_100x100 <- raster(SpatRaster_100x100_32630) # Convertir SpatRaster en RasterLayer
SpatialPixels_100x100 <- as(RasterLayer_100x100, "SpatialPixels") # Convertir RasterLayer en SpatialPixels

# Règle de Silverman
sigma_x_roosting_age_100x100_glob <- sd(coords_roosting_age_32630[,1])  # Écart-type en X (mètres)
sigma_y_roosting_age_100x100_glob <- sd(coords_roosting_age_32630[,2])  # Écart-type en Y (mètres)
n_roosting_age_100x100_glob <- nrow(coords_roosting_age)  # Nombre de points
h_silverman_x_roosting_age_100x100_glob <- 1.06 * sigma_x_roosting_age_100x100_glob * n_roosting_age_100x100_glob^(-1/5) / 2
h_silverman_y_roosting_age_100x100_glob <- 1.06 * sigma_y_roosting_age_100x100_glob * n_roosting_age_100x100_glob^(-1/5) / 2
locs_spa_roosting_age <- as(locs_roosting_age_32630, "Spatial")
# Appliquer kernelUD avec h estimé par Silverman
kud_roosting_age_100x100_glob <- kernelUD(locs_spa_roosting_age["age"], 
                                      grid = SpatialPixels_100x100, 
                                      h = mean(c(h_silverman_x_roosting_age_100x100_glob, h_silverman_y_roosting_age_100x100_glob)))

UDmaps_list_age <- lapply(names(kud_roosting_age_100x100_glob), function(age) {
  
  print(age)
  
  # Extraire l'estimation de densité pour un ID spécifique
  kud_roosting_age_100x100_glob_single <- kud_roosting_age_100x100_glob[[age]]
  rast_roosting_age_100x100_glob <- rast(kud_roosting_age_100x100_glob_single)
  courtour_roosting_age_100x100_glob <- as.contour(rast_roosting_age_100x100_glob)
  sf_roosting_age_100x100_glob <- st_as_sf(courtour_roosting_age_100x100_glob)
  cast_roosting_age_100x100_glob <- st_cast(sf_roosting_age_100x100_glob, "POLYGON")
  cast_roosting_age_100x100_glob$age <- age
  
  return(cast_roosting_age_100x100_glob)
})

# Fusionner tous les ID dans un seul objet sf
UDMap_final_age <- do.call(rbind, UDmaps_list_age)

UDMap_final_age$age <- as.factor(UDMap_final_age$age)

st_crs(UDMap_final_age) == st_crs(RMO)  # Vérifie si les projections sont identiques
UDMap_final_age <- st_transform(UDMap_final_age, st_crs(RMO))
table(is.na(UDMap_final_age$age))

# plot
tmap_mode("view")
UDMap_100x100_roosting_age_glob <- tm_scalebar() +
  tm_shape(RMO) +
  tm_polygons() +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(UDMap_final_age) + 
  tm_polygons(border.col = "grey", fill = "level", fill_alpha = 0.2, 
              palette = viridis(10, begin = 0, end = 1, 
                                direction = 1, option = "plasma")) +
  tm_facets("age")) +
  tm_shape(terre_mer) +
  tm_lines(col = "darkblue", lwd = 0.5); UDMap_100x100_roosting_age_glob

tmap_save(UDMap_100x100_roosting_age_glob, "map_facets_interactive.html")

### ZOOM -----------------------------------------------------------------------

crs_utm <- "EPSG:32630"
ZOOM <- c("A","B","C","D","E")
resolution_ZOOM = 10
lettre = "A"
cast_roosting_age_ZOOM_all = NULL

for (lettre in ZOOM){
  # in ZOOM
  ZOOM <- st_read(paste0(data_generated_path,"ZOOM_",lettre,".gpkg"))
  ZOOM <- st_transform(ZOOM, crs = 4326)
  GPS_ZOOM <- st_intersection(GPS, ZOOM) 
  coords_roosting_age_ZOOM <- GPS_ZOOM %>% 
    filter(behavior == "roosting") %>% 
    dplyr::select(lon,lat, age) %>% 
    st_drop_geometry() %>% 
    na.omit()
  
  locs_roosting_age_ZOOM <- st_as_sf(coords_roosting_age_ZOOM, coords = c("lon", "lat"), crs = 4326)
  locs_roosting_age_ZOOM <- st_transform(locs_roosting_age_ZOOM, crs = 32630)  # Reprojeter EPSG:32630 pour la France
  coords_roosting_age_ZOOM <- st_coordinates(locs_roosting_age_ZOOM) # Extraire les coordonnées reprojetées
  
  # raster/grid
  grid_ZOOM <- st_read(paste0(data_generated_path, "grid_ZOOM_",lettre,".gpkg"))
  raster_ZOOM <- rast(grid_ZOOM, resolution = resolution_ZOOM, crs="EPSG:2154")
  SpatRaster_ZOOM <- project(raster_ZOOM, crs_utm)  # Reprojection du raster
  RasterLayer_ZOOM <- raster(SpatRaster_ZOOM) # Convertir SpatRaster en RasterLayer
  SpatialPixels_ZOOM <- as(RasterLayer_ZOOM, "SpatialPixels") # Convertir RasterLayer en SpatialPixels
  
  # Règle de Silverman
  sigma_x <- sd(coords_roosting_age_ZOOM[,1])  # Écart-type en X (mètres)
  sigma_y <- sd(coords_roosting_age_ZOOM[,2])  # Écart-type en Y (mètres)
  n_roosting <- nrow(coords_roosting_age_ZOOM)  # Nombre de points
  h_silverman_x <- 1.06 * sigma_x * n_roosting^(-1/5) / 2
  h_silverman_y <- 1.06 * sigma_y * n_roosting^(-1/5) / 2
  cat("h optimal en mètres pour X:", h_silverman_x, "\n")
  cat("h optimal en mètres pour Y:", h_silverman_y, "\n")
  locs_spa_roosting_age_ZOOM <- as(locs_roosting_age_ZOOM, "Spatial")
  # Appliquer kernelUD avec h estimé par Silverman
  kud_roosting_age_ZOOM <- kernelUD(locs_spa_roosting_age_ZOOM["age"], 
                                grid = SpatialPixels_ZOOM, 
                                h = mean(c(h_silverman_x, h_silverman_y)))
  
  UDmaps_list_roosting_age_ZOOM <- lapply(names(kud_roosting_age_ZOOM), function(age) {
    
    print(age)
    
    # Extraire l'estimation de densité pour un ID spécifique
    kud_roosting_100x100_age_ZOOM_single <- kud_roosting_age_ZOOM[[age]]
    rast_roosting_100x100_age_ZOOM <- rast(kud_roosting_100x100_age_ZOOM_single)
    courtour_roosting_100x100_age_ZOOM <- as.contour(rast_roosting_100x100_age_ZOOM)
    sf_roosting_100x100_age_ZOOM <- st_as_sf(courtour_roosting_100x100_age_ZOOM)
    cast_roosting_age_ZOOM <- st_cast(sf_roosting_100x100_age_ZOOM, "POLYGON")
    cast_roosting_age_ZOOM$age <- age
    
    return(cast_roosting_age_ZOOM)
  })
  
  UDMap_final_roosting_age_ZOOM <- do.call(rbind, UDmaps_list_roosting_age_ZOOM)
  UDMap_final_roosting_age_ZOOM$age <- as.factor(UDMap_final_roosting_age_ZOOM$age)
  UDMap_final_roosting_age_ZOOM$ZOOM <- lettre
  cast_roosting_age_ZOOM_all <- rbind(cast_roosting_age_ZOOM_all, UDMap_final_roosting_age_ZOOM)
  
}

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
  tm_shape(cast_roosting_age_ZOOM_all) + 
  tm_facets("age") + 
  tm_polygons(border.col = "grey", fill = "level", fill_alpha = 0.2, 
              palette = viridis(10, begin = 0, end = 1, 
                                direction = 1, option = "plasma"))) +
  tm_shape(terre_mer) +
  tm_lines(col = "darkblue", lwd = 0.5); UDMap_roosting_age_ZOOM

## Sexe ------------------------------------------------------------------------

### GLOB -----------------------------------------------------------------------

# GPS point roosting ---

# All zone
coords_roosting_sex <- GPS %>% 
  filter(behavior == "roosting") %>% 
  dplyr::select(lon,lat,sex) %>% 
  st_drop_geometry() %>% 
  na.omit()

locs_roosting_sex <- st_as_sf(coords_roosting_sex, coords = c("lon", "lat"), crs = 4326)
locs_roosting_sex_32630 <- st_transform(locs_roosting_sex, crs = 32630)  # Reprojeter EPSG:32630 pour la France
coords_roosting_sex_32630 <- st_coordinates(locs_roosting_sex_32630) # Extraire les coordonnées reprojetées

# raster/grid
crs_utm <- "EPSG:32630"
SpatRaster_100x100_32630 <- project(raster_100x100, crs_utm)  # Reprojection du raster
RasterLayer_100x100 <- raster(SpatRaster_100x100_32630) # Convertir SpatRaster en RasterLayer
SpatialPixels_100x100 <- as(RasterLayer_100x100, "SpatialPixels") # Convertir RasterLayer en SpatialPixels

# Règle de Silverman
sigma_x_roosting_sex_100x100_glob <- sd(coords_roosting_sex_32630[,1])  # Écart-type en X (mètres)
sigma_y_roosting_sex_100x100_glob <- sd(coords_roosting_sex_32630[,2])  # Écart-type en Y (mètres)
n_roosting_sex_100x100_glob <- nrow(coords_roosting_sex)  # Nombre de points
h_silverman_x_roosting_sex_100x100_glob <- 1.06 * sigma_x_roosting_sex_100x100_glob * n_roosting_sex_100x100_glob^(-1/5) / 2
h_silverman_y_roosting_sex_100x100_glob <- 1.06 * sigma_y_roosting_sex_100x100_glob * n_roosting_sex_100x100_glob^(-1/5) / 2
locs_spa_roosting_sex <- as(locs_roosting_sex_32630, "Spatial")
# Appliquer kernelUD avec h estimé par Silverman
kud_roosting_sex_100x100_glob <- kernelUD(locs_spa_roosting_sex["sex"], 
                                          grid = SpatialPixels_100x100, 
                                          h = mean(c(h_silverman_x_roosting_sex_100x100_glob, h_silverman_y_roosting_sex_100x100_glob)))

UDmaps_list_sex <- lapply(names(kud_roosting_sex_100x100_glob), function(sex) {
  
  print(sex)
  
  # Extraire l'estimation de densité pour un ID spécifique
  kud_roosting_sex_100x100_glob_single <- kud_roosting_sex_100x100_glob[[sex]]
  rast_roosting_sex_100x100_glob <- rast(kud_roosting_sex_100x100_glob_single)
  courtour_roosting_sex_100x100_glob <- as.contour(rast_roosting_sex_100x100_glob)
  sf_roosting_sex_100x100_glob <- st_as_sf(courtour_roosting_sex_100x100_glob)
  cast_roosting_sex_100x100_glob <- st_cast(sf_roosting_sex_100x100_glob, "POLYGON")
  cast_roosting_sex_100x100_glob$sex <- sex
  
  return(cast_roosting_sex_100x100_glob)
})

# Fusionner tous les ID dans un seul objet sf
UDMap_final_sex <- do.call(rbind, UDmaps_list_sex)

UDMap_final_sex$sex <- as.factor(UDMap_final_sex$sex)

st_crs(UDMap_final_sex) == st_crs(RMO)  # Vérifie si les projections sont identiques
UDMap_final_sex <- st_transform(UDMap_final_sex, st_crs(RMO))
table(is.na(UDMap_final_sex$sex))

# plot
tmap_mode("view")
UDMap_100x100_roosting_sex_glob <- tm_scalebar() +
  tm_shape(RMO) +
  tm_polygons() +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(UDMap_final_sex) + 
  tm_polygons(border.col = "grey", fill = "level", fill_alpha = 0.2, 
              palette = viridis(10, begin = 0, end = 1, 
                                direction = 1, option = "plasma")) +
  tm_facets("sex")) +
  tm_shape(terre_mer) +
  tm_lines(col = "darkblue", lwd = 0.5); UDMap_100x100_roosting_sex_glob

### ZOOM -----------------------------------------------------------------------

crs_utm <- "EPSG:32630"
ZOOM <- c("A","B","C","D","E")
resolution_ZOOM = 10
lettre = "A"
cast_roosting_sex_ZOOM_all = NULL

for (lettre in ZOOM){
  # in ZOOM
  ZOOM <- st_read(paste0(data_generated_path,"ZOOM_",lettre,".gpkg"))
  ZOOM <- st_transform(ZOOM, crs = 4326)
  GPS_ZOOM <- st_intersection(GPS, ZOOM) 
  coords_roosting_sex_ZOOM <- GPS_ZOOM %>% 
    filter(behavior == "roosting") %>% 
    dplyr::select(lon,lat, sex) %>% 
    st_drop_geometry() %>% 
    na.omit()
  
  locs_roosting_sex_ZOOM <- st_as_sf(coords_roosting_sex_ZOOM, coords = c("lon", "lat"), crs = 4326)
  locs_roosting_sex_ZOOM <- st_transform(locs_roosting_sex_ZOOM, crs = 32630)  # Reprojeter EPSG:32630 pour la France
  coords_roosting_sex_ZOOM <- st_coordinates(locs_roosting_sex_ZOOM) # Extraire les coordonnées reprojetées
  
  # raster/grid
  grid_ZOOM <- st_read(paste0(data_generated_path, "grid_ZOOM_",lettre,".gpkg"))
  raster_ZOOM <- rast(grid_ZOOM, resolution = resolution_ZOOM, crs="EPSG:2154")
  SpatRaster_ZOOM <- project(raster_ZOOM, crs_utm)  # Reprojection du raster
  RasterLayer_ZOOM <- raster(SpatRaster_ZOOM) # Convertir SpatRaster en RasterLayer
  SpatialPixels_ZOOM <- as(RasterLayer_ZOOM, "SpatialPixels") # Convertir RasterLayer en SpatialPixels
  
  # Règle de Silverman
  sigma_x <- sd(coords_roosting_sex_ZOOM[,1])  # Écart-type en X (mètres)
  sigma_y <- sd(coords_roosting_sex_ZOOM[,2])  # Écart-type en Y (mètres)
  n_roosting <- nrow(coords_roosting_sex_ZOOM)  # Nombre de points
  h_silverman_x <- 1.06 * sigma_x * n_roosting^(-1/5) / 2
  h_silverman_y <- 1.06 * sigma_y * n_roosting^(-1/5) / 2
  cat("h optimal en mètres pour X:", h_silverman_x, "\n")
  cat("h optimal en mètres pour Y:", h_silverman_y, "\n")
  locs_spa_roosting_sex_ZOOM <- as(locs_roosting_sex_ZOOM, "Spatial")
  # Appliquer kernelUD avec h estimé par Silverman
  kud_roosting_sex_ZOOM <- kernelUD(locs_spa_roosting_sex_ZOOM["sex"], 
                                    grid = SpatialPixels_ZOOM, 
                                    h = mean(c(h_silverman_x, h_silverman_y)))
  
  UDmaps_list_roosting_sex_ZOOM <- lapply(names(kud_roosting_sex_ZOOM), function(sex) {
    
    print(sex)
    
    # Extraire l'estimation de densité pour un ID spécifique
    kud_roosting_100x100_sex_ZOOM_single <- kud_roosting_sex_ZOOM[[sex]]
    rast_roosting_100x100_sex_ZOOM <- rast(kud_roosting_100x100_sex_ZOOM_single)
    courtour_roosting_100x100_sex_ZOOM <- as.contour(rast_roosting_100x100_sex_ZOOM)
    sf_roosting_100x100_sex_ZOOM <- st_as_sf(courtour_roosting_100x100_sex_ZOOM)
    cast_roosting_sex_ZOOM <- st_cast(sf_roosting_100x100_sex_ZOOM, "POLYGON")
    cast_roosting_sex_ZOOM$sex <- sex
    
    return(cast_roosting_sex_ZOOM)
  })
  
  UDMap_final_roosting_sex_ZOOM <- do.call(rbind, UDmaps_list_roosting_sex_ZOOM)
  UDMap_final_roosting_sex_ZOOM$sex <- as.factor(UDMap_final_roosting_sex_ZOOM$sex)
  UDMap_final_roosting_sex_ZOOM$ZOOM <- lettre
  cast_roosting_sex_ZOOM_all <- rbind(cast_roosting_sex_ZOOM_all, UDMap_final_roosting_sex_ZOOM)
  
}

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
  tm_shape(cast_roosting_sex_ZOOM_all) + 
  tm_facets("sex") + 
  tm_polygons(border.col = "grey", fill = "level", fill_alpha = 0.2, 
              palette = viridis(10, begin = 0, end = 1, 
                                direction = 1, option = "plasma"))) +
  tm_shape(terre_mer) +
  tm_lines(col = "darkblue", lwd = 0.5); UDMap_roosting_sex_ZOOM

###
####
# ***FORAGING*** ---------------------------------------------------------------
####
###

## GLOB -----------------------------------------------------------------------

# All zone
coords_foraging <- GPS %>% 
  filter(behavior == "foraging") %>% 
  dplyr::select(lon,lat) %>% 
  st_drop_geometry() %>% 
  na.omit()

locs_foraging <- st_as_sf(coords_foraging, coords = c("lon", "lat"), crs = 4326)
locs_foraging_32630 <- st_transform(locs_foraging, crs = 32630)  # Reprojeter EPSG:32630 pour la France
coords_foraging_32630 <- st_coordinates(locs_foraging_32630) # Extraire les coordonnées reprojetées

# raster/grid
crs_utm <- "EPSG:32630"
SpatRaster_100x100_32630 <- project(raster_100x100, crs_utm)  # Reprojection du raster
RasterLayer_100x100 <- raster(SpatRaster_100x100_32630) # Convertir SpatRaster en RasterLayer
SpatialPixels_100x100 <- as(RasterLayer_100x100, "SpatialPixels") # Convertir RasterLayer en SpatialPixels

# Règle de Silverman
sigma_x_foraging_100x100_glob <- sd(coords_foraging_32630[,1])  # Écart-type en X (mètres)
sigma_y_foraging_100x100_glob <- sd(coords_foraging_32630[,2])  # Écart-type en Y (mètres)
n_foraging_100x100_glob <- nrow(coords_foraging)  # Nombre de points
h_silverman_x_foraging_100x100_glob <- 1.06 * sigma_x_foraging_100x100_glob * n_foraging_100x100_glob^(-1/5) / 2
h_silverman_y_foraging_100x100_glob <- 1.06 * sigma_y_foraging_100x100_glob * n_foraging_100x100_glob^(-1/5) / 2
# cat("h optimal en mètres pour X:", h_silverman_x_foraging, "\n")
# cat("h optimal en mètres pour Y:", h_silverman_y_foraging, "\n")
locs_spa_foraging <- as(locs_foraging_32630, "Spatial")
# Appliquer kernelUD avec h estimé par Silverman
kud_foraging_100x100_glob <- kernelUD(locs_spa_foraging, 
                                      grid = SpatialPixels_100x100, 
                                      h = mean(c(h_silverman_x_foraging_100x100_glob, h_silverman_y_foraging_100x100_glob)))

# Visualiser la densité de noyau
# par(mfrow = c(1, 1))
# image(kud)

# Estimation des isoclines 
rast_foraging_100x100_glob <- rast(kud_foraging_100x100_glob)
courtour_foraging_100x100_glob <- as.contour(rast_foraging_100x100_glob)
sf_foraging_100x100_glob <- st_as_sf(courtour_foraging_100x100_glob)
cast_foraging_100x100_glob <- st_cast(sf_foraging_100x100_glob, "POLYGON")

# plot
tmap_mode("view")
UDMap_100x100_foraging_glob <- tm_scalebar() +
  tm_shape(RMO) +
  tm_polygons() +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(cast_foraging_100x100_glob) + 
  tm_polygons(border.col = "grey", fill = "level", fill_alpha = 0.2, 
              palette = viridis(10, begin = 0, end = 1, 
                                direction = 1, option = "plasma"))) +
  tm_shape(terre_mer) +
  tm_lines(col = "darkblue", lwd = 0.5); UDMap_100x100_foraging_glob

## ZOOM -----------------------------------------------------------------------

crs_utm <- "EPSG:32630"
ZOOM <- c("A","B","C","D")
resolution_ZOOM = 10
lettre = "A"
cast_foraging_ZOOM_all = NULL

for (lettre in ZOOM){
  # in ZOOM
  ZOOM <- st_read(paste0(data_generated_path,"ZOOM_",lettre,".gpkg"))
  ZOOM <- st_transform(ZOOM, crs = 4326)
  GPS_ZOOM <- st_intersection(GPS, ZOOM) 
  coords_foraging_ZOOM <- GPS_ZOOM %>% 
    filter(behavior == "foraging") %>% 
    dplyr::select(lon,lat) %>% 
    st_drop_geometry() %>% 
    na.omit()
  
  locs_foraging_ZOOM <- st_as_sf(coords_foraging_ZOOM, coords = c("lon", "lat"), crs = 4326)
  locs_foraging_ZOOM <- st_transform(locs_foraging_ZOOM, crs = 32630)  # Reprojeter EPSG:32630 pour la France
  coords_foraging_ZOOM <- st_coordinates(locs_foraging_ZOOM) # Extraire les coordonnées reprojetées
  
  # raster/grid
  grid_ZOOM <- st_read(paste0(data_generated_path, "grid_ZOOM_",lettre,".gpkg"))
  raster_ZOOM <- rast(grid_ZOOM, resolution = resolution_ZOOM, crs="EPSG:2154")
  SpatRaster_ZOOM <- project(raster_ZOOM, crs_utm)  # Reprojection du raster
  RasterLayer_ZOOM <- raster(SpatRaster_ZOOM) # Convertir SpatRaster en RasterLayer
  SpatialPixels_ZOOM <- as(RasterLayer_ZOOM, "SpatialPixels") # Convertir RasterLayer en SpatialPixels
  
  # Règle de Silverman
  sigma_x <- sd(coords_foraging_ZOOM[,1])  # Écart-type en X (mètres)
  sigma_y <- sd(coords_foraging_ZOOM[,2])  # Écart-type en Y (mètres)
  n_foraging <- nrow(coords_foraging_ZOOM)  # Nombre de points
  h_silverman_x <- 1.06 * sigma_x * n_foraging^(-1/5) / 2
  h_silverman_y <- 1.06 * sigma_y * n_foraging^(-1/5) / 2
  cat("h optimal en mètres pour X:", h_silverman_x, "\n")
  cat("h optimal en mètres pour Y:", h_silverman_y, "\n")
  locs_spa_foraging_ZOOM <- as(locs_foraging_ZOOM, "Spatial")
  # Appliquer kernelUD avec h estimé par Silverman
  kud_foraging_ZOOM <- kernelUD(locs_spa_foraging_ZOOM, 
                                grid = SpatialPixels_ZOOM, 
                                h = mean(c(h_silverman_x, h_silverman_y)))
  
  # Estimation des isoclines 
  rast_foraging_ZOOM <- rast(kud_foraging_ZOOM)
  courtour_foraging_ZOOM <- as.contour(rast_foraging_ZOOM)
  sf_foraging_ZOOM <- st_as_sf(courtour_foraging_ZOOM)
  cast_foraging_ZOOM <- st_cast(sf_foraging_ZOOM, "POLYGON")
  
  cast_foraging_ZOOM$ZOOM <- lettre
  cast_foraging_ZOOM_all <- rbind(cast_foraging_ZOOM_all, cast_foraging_ZOOM)
  
}

# plot
tmap_mode("view")
UDMap_foraging_ZOOM <- tm_scalebar() +
  tm_shape(RMO) +
  tm_polygons() +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(ZOOM_A) +
  tm_polygons(fill_alpha = 0.1, fill = "grey") +
  tm_text("Zoom A", size = 1.5) +
  tm_shape(ZOOM_B) +
  tm_polygons(fill_alpha = 0.1, fill = "grey") +
  tm_text("Zoom B", size = 1.5) +
  tm_shape(ZOOM_C) +
  tm_polygons(fill_alpha = 0.1, fill = "grey") +
  tm_text("Zoom C", size = 1.5) +
  tm_shape(ZOOM_D) +
  tm_polygons(fill_alpha = 0.1, fill = "grey") +
  tm_text("Zoom D", size = 1.5) +
  tm_shape(BOX_2154) +
  tm_borders(col = "black") +
  tm_shape(cast_foraging_ZOOM_all) + 
  tm_polygons(border.col = "grey", fill = "level", fill_alpha = 0.2, 
              palette = viridis(10, begin = 0, end = 1, 
                                direction = 1, option = "plasma"))) +
  tm_shape(terre_mer) +
  tm_lines(col = "darkblue", lwd = 0.5); UDMap_foraging_ZOOM

## Age -------------------------------------------------------------------------

### GLOB -----------------------------------------------------------------------

# All zone
coords_foraging_age <- GPS %>% 
  filter(behavior == "foraging") %>% 
  dplyr::select(lon,lat,age) %>% 
  st_drop_geometry() %>% 
  na.omit()

locs_foraging_age <- st_as_sf(coords_foraging_age, coords = c("lon", "lat"), crs = 4326)
locs_foraging_age_32630 <- st_transform(locs_foraging_age, crs = 32630)  # Reprojeter EPSG:32630 pour la France
coords_foraging_age_32630 <- st_coordinates(locs_foraging_age_32630) # Extraire les coordonnées reprojetées

# raster/grid
crs_utm <- "EPSG:32630"
SpatRaster_100x100_32630 <- project(raster_100x100, crs_utm)  # Reprojection du raster
RasterLayer_100x100 <- raster(SpatRaster_100x100_32630) # Convertir SpatRaster en RasterLayer
SpatialPixels_100x100 <- as(RasterLayer_100x100, "SpatialPixels") # Convertir RasterLayer en SpatialPixels

# Règle de Silverman
sigma_x_foraging_age_100x100_glob <- sd(coords_foraging_age_32630[,1])  # Écart-type en X (mètres)
sigma_y_foraging_age_100x100_glob <- sd(coords_foraging_age_32630[,2])  # Écart-type en Y (mètres)
n_foraging_age_100x100_glob <- nrow(coords_foraging_age)  # Nombre de points
h_silverman_x_foraging_age_100x100_glob <- 1.06 * sigma_x_foraging_age_100x100_glob * n_foraging_age_100x100_glob^(-1/5) / 2
h_silverman_y_foraging_age_100x100_glob <- 1.06 * sigma_y_foraging_age_100x100_glob * n_foraging_age_100x100_glob^(-1/5) / 2
locs_spa_foraging_age <- as(locs_foraging_age_32630, "Spatial")
# Appliquer kernelUD avec h estimé par Silverman
kud_foraging_age_100x100_glob <- kernelUD(locs_spa_foraging_age["age"], 
                                          grid = SpatialPixels_100x100, 
                                          h = mean(c(h_silverman_x_foraging_age_100x100_glob, h_silverman_y_foraging_age_100x100_glob)))

UDmaps_list_age <- lapply(names(kud_foraging_age_100x100_glob), function(age) {
  
  print(age)
  
  # Extraire l'estimation de densité pour un ID spécifique
  kud_foraging_age_100x100_glob_single <- kud_foraging_age_100x100_glob[[age]]
  rast_foraging_age_100x100_glob <- rast(kud_foraging_age_100x100_glob_single)
  courtour_foraging_age_100x100_glob <- as.contour(rast_foraging_age_100x100_glob)
  sf_foraging_age_100x100_glob <- st_as_sf(courtour_foraging_age_100x100_glob)
  cast_foraging_age_100x100_glob <- st_cast(sf_foraging_age_100x100_glob, "POLYGON")
  cast_foraging_age_100x100_glob$age <- age
  
  return(cast_foraging_age_100x100_glob)
})

# Fusionner tous les ID dans un seul objet sf
UDMap_final_age <- do.call(rbind, UDmaps_list_age)

UDMap_final_age$age <- as.factor(UDMap_final_age$age)

st_crs(UDMap_final_age) == st_crs(RMO)  # Vérifie si les projections sont identiques
UDMap_final_age <- st_transform(UDMap_final_age, st_crs(RMO))
table(is.na(UDMap_final_age$age))

# plot
tmap_mode("view")
UDMap_100x100_foraging_age_glob <- tm_scalebar() +
  tm_shape(RMO) +
  tm_polygons() +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(UDMap_final_age) + 
  tm_polygons(border.col = "grey", fill = "level", fill_alpha = 0.2, 
              palette = viridis(10, begin = 0, end = 1, 
                                direction = 1, option = "plasma")) +
  tm_facets("age")) +
  tm_shape(terre_mer) +
  tm_lines(col = "darkblue", lwd = 0.5); UDMap_100x100_foraging_age_glob

### ZOOM -----------------------------------------------------------------------

crs_utm <- "EPSG:32630"
ZOOM <- c("A","B","C","D")
resolution_ZOOM = 10
lettre = "A"
cast_foraging_age_ZOOM_all = NULL

for (lettre in ZOOM){
  # in ZOOM
  ZOOM <- st_read(paste0(data_generated_path,"ZOOM_",lettre,".gpkg"))
  ZOOM <- st_transform(ZOOM, crs = 4326)
  GPS_ZOOM <- st_intersection(GPS, ZOOM) 
  coords_foraging_age_ZOOM <- GPS_ZOOM %>% 
    filter(behavior == "foraging") %>% 
    dplyr::select(lon,lat, age) %>% 
    st_drop_geometry() %>% 
    na.omit()
  
  locs_foraging_age_ZOOM <- st_as_sf(coords_foraging_age_ZOOM, coords = c("lon", "lat"), crs = 4326)
  locs_foraging_age_ZOOM <- st_transform(locs_foraging_age_ZOOM, crs = 32630)  # Reprojeter EPSG:32630 pour la France
  coords_foraging_age_ZOOM <- st_coordinates(locs_foraging_age_ZOOM) # Extraire les coordonnées reprojetées
  
  # raster/grid
  grid_ZOOM <- st_read(paste0(data_generated_path, "grid_ZOOM_",lettre,".gpkg"))
  raster_ZOOM <- rast(grid_ZOOM, resolution = resolution_ZOOM, crs="EPSG:2154")
  SpatRaster_ZOOM <- project(raster_ZOOM, crs_utm)  # Reprojection du raster
  RasterLayer_ZOOM <- raster(SpatRaster_ZOOM) # Convertir SpatRaster en RasterLayer
  SpatialPixels_ZOOM <- as(RasterLayer_ZOOM, "SpatialPixels") # Convertir RasterLayer en SpatialPixels
  
  # Règle de Silverman
  sigma_x <- sd(coords_foraging_age_ZOOM[,1])  # Écart-type en X (mètres)
  sigma_y <- sd(coords_foraging_age_ZOOM[,2])  # Écart-type en Y (mètres)
  n_foraging <- nrow(coords_foraging_age_ZOOM)  # Nombre de points
  h_silverman_x <- 1.06 * sigma_x * n_foraging^(-1/5) / 2
  h_silverman_y <- 1.06 * sigma_y * n_foraging^(-1/5) / 2
  cat("h optimal en mètres pour X:", h_silverman_x, "\n")
  cat("h optimal en mètres pour Y:", h_silverman_y, "\n")
  locs_spa_foraging_age_ZOOM <- as(locs_foraging_age_ZOOM, "Spatial")
  # Appliquer kernelUD avec h estimé par Silverman
  kud_foraging_age_ZOOM <- kernelUD(locs_spa_foraging_age_ZOOM["age"], 
                                    grid = SpatialPixels_ZOOM, 
                                    h = mean(c(h_silverman_x, h_silverman_y)))
  
  UDmaps_list_foraging_age_ZOOM <- lapply(names(kud_foraging_age_ZOOM), function(age) {
    
    print(age)
    
    # Extraire l'estimation de densité pour un ID spécifique
    kud_foraging_100x100_age_ZOOM_single <- kud_foraging_age_ZOOM[[age]]
    rast_foraging_100x100_age_ZOOM <- rast(kud_foraging_100x100_age_ZOOM_single)
    courtour_foraging_100x100_age_ZOOM <- as.contour(rast_foraging_100x100_age_ZOOM)
    sf_foraging_100x100_age_ZOOM <- st_as_sf(courtour_foraging_100x100_age_ZOOM)
    cast_foraging_age_ZOOM <- st_cast(sf_foraging_100x100_age_ZOOM, "POLYGON")
    cast_foraging_age_ZOOM$age <- age
    
    return(cast_foraging_age_ZOOM)
  })
  
  UDMap_final_foraging_age_ZOOM <- do.call(rbind, UDmaps_list_foraging_age_ZOOM)
  UDMap_final_foraging_age_ZOOM$age <- as.factor(UDMap_final_foraging_age_ZOOM$age)
  UDMap_final_foraging_age_ZOOM$ZOOM <- lettre
  cast_foraging_age_ZOOM_all <- rbind(cast_foraging_age_ZOOM_all, UDMap_final_foraging_age_ZOOM)
  
}

# plot
tmap_mode("view")
UDMap_foraging_age_ZOOM <- tm_scalebar() +
  tm_shape(RMO) +
  tm_polygons() +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(ZOOM_A) +
  tm_polygons(fill_alpha = 0.1, fill = "grey") +
  tm_text("Zoom A", size = 1.5) +
  tm_shape(ZOOM_B) +
  tm_polygons(fill_alpha = 0.1, fill = "grey") +
  tm_text("Zoom B", size = 1.5) +
  tm_shape(ZOOM_C) +
  tm_polygons(fill_alpha = 0.1, fill = "grey") +
  tm_text("Zoom C", size = 1.5) +
  tm_shape(ZOOM_D) +
  tm_polygons(fill_alpha = 0.1, fill = "grey") +
  tm_text("Zoom D", size = 1.5) +
  tm_shape(BOX_2154) +
  tm_borders(col = "black") +
  tm_shape(cast_foraging_age_ZOOM_all) + 
  tm_facets("age") + 
  tm_polygons(border.col = "grey", fill = "level", fill_alpha = 0.2, 
              palette = viridis(10, begin = 0, end = 1, 
                                direction = 1, option = "plasma"))) +
  tm_shape(terre_mer) +
  tm_lines(col = "darkblue", lwd = 0.5); UDMap_foraging_age_ZOOM

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






## FORAGING --------------------------------------------------------------------

### global ----

# GPS point roosting ---

# All zone
coords_foraging <- GPS %>% 
  filter(behavior == "foraging") %>% 
  dplyr::select(lon,lat) %>% 
  st_drop_geometry() %>% 
  na.omit()

locs_foraging <- st_as_sf(coords_foraging, coords = c("lon", "lat"), crs = 4326)
locs_foraging_32630 <- st_transform(locs_foraging, crs = 32630)  # Reprojeter EPSG:32630 pour la France
coords_foraging_32630 <- st_coordinates(locs_foraging_32630) # Extraire les coordonnées reprojetées

#### 100x100m ------------------------------------------------------------------

# raster/grid
crs_utm <- "EPSG:32630"
SpatRaster_100x100_32630 <- project(raster_100x100, crs_utm)  # Reprojection du raster
RasterLayer_100x100 <- raster(SpatRaster_100x100_32630) # Convertir SpatRaster en RasterLayer
SpatialPixels_100x100 <- as(RasterLayer_100x100_raster, "SpatialPixels") # Convertir RasterLayer en SpatialPixels

# Règle de Silverman
sigma_x_foraging_100x100_glob <- sd(coords_foraging_32630[,1])  # Écart-type en X (mètres)
sigma_y_foraging_100x100_glob <- sd(coords_foraging_32630[,2])  # Écart-type en Y (mètres)
n_foraging_100x100_glob <- nrow(coords_foraging)  # Nombre de points
h_silverman_x_foraging_100x100_glob <- 1.06 * sigma_x_foraging_100x100_glob * n_foraging_100x100_glob^(-1/5)
h_silverman_y_foraging_100x100_glob <- 1.06 * sigma_y_foraging_100x100_glob * n_foraging_100x100_glob^(-1/5)
cat("h optimal en mètres pour X:", h_silverman_x_foraging_100x100_glob, "\n")
cat("h optimal en mètres pour Y:", h_silverman_y_foraging_100x100_glob, "\n")
locs_spa_foraging <- as(locs_foraging_32630, "Spatial")
# Appliquer kernelUD avec h estimé par Silverman
kud_foraging_100x100_glob <- kernelUD(locs_spa_foraging, 
                                      grid = SpatialPixels_100x100, 
                                      h = mean(c(h_silverman_x_foraging_100x100_glob, h_silverman_y_foraging_100x100_glob)))

# Estimation des isoclines 
rast_foraging_100x100_glob <- rast(kud_foraging_100x100_glob)
courtour_foraging_100x100_glob <- as.contour(rast_foraging_100x100_glob)
sf_foraging_100x100_glob <- st_as_sf(courtour_foraging_100x100_glob)
cast_foraging_100x100_glob <- st_cast(sf_foraging_100x100_glob, "POLYGON")

# plot
tmap_mode("view")
UDMap_100x100_foraging_glob <- tm_scalebar() +
  tm_shape(RMO) +
  tm_polygons() +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(cast_foraging_100x100_glob) + 
  tm_polygons(border.col = "grey", fill = "level", fill_alpha = 0.2, 
              palette = viridis(10, begin = 0, end = 1, 
                                direction = 1, option = "plasma")); UDMap_100x100_foraging_glob

#### ZOOM ----------------------------------------------------------------------

crs_utm <- "EPSG:32630"
ZOOM <- c("A","B","C","D")
resolution_ZOOM = 50
lettre = "A"
cast_foraging_ZOOM_all = NULL

for (lettre in ZOOM){
  # in ZOOM
  ZOOM <- st_read(paste0(data_generated_path,"ZOOM_",lettre,".gpkg"))
  ZOOM <- st_transform(ZOOM, crs = 4326)
  GPS_ZOOM <- st_intersection(GPS, ZOOM) 
  coords_foraging_ZOOM <- GPS_ZOOM %>% 
    filter(behavior == "foraging") %>% 
    dplyr::select(lon,lat) %>% 
    st_drop_geometry() %>% 
    na.omit()
  
  locs_foraging_ZOOM <- st_as_sf(coords_foraging_ZOOM, coords = c("lon", "lat"), crs = 4326)
  locs_foraging_ZOOM <- st_transform(locs_foraging_ZOOM, crs = 32630)  # Reprojeter EPSG:32630 pour la France
  coords_foraging_ZOOM <- st_coordinates(locs_foraging_ZOOM) # Extraire les coordonnées reprojetées
  
  # raster/grid
  grid_ZOOM <- st_read(paste0(data_generated_path, "grid_ZOOM_",lettre,".gpkg"))
  raster_ZOOM <- rast(grid_ZOOM, resolution = resolution_ZOOM, crs="EPSG:2154")
  SpatRaster_ZOOM <- project(raster_ZOOM, crs_utm)  # Reprojection du raster
  RasterLayer_ZOOM <- raster(SpatRaster_ZOOM) # Convertir SpatRaster en RasterLayer
  SpatialPixels_ZOOM <- as(RasterLayer_ZOOM, "SpatialPixels") # Convertir RasterLayer en SpatialPixels
  
  # Règle de Silverman
  sigma_x <- sd(coords_foraging_ZOOM[,1])  # Écart-type en X (mètres)
  sigma_y <- sd(coords_foraging_ZOOM[,2])  # Écart-type en Y (mètres)
  n_foraging <- nrow(coords_foraging_ZOOM)  # Nombre de points
  h_silverman_x <- 1.06 * sigma_x * n_foraging^(-1/5)
  h_silverman_y <- 1.06 * sigma_y * n_foraging^(-1/5)
  cat("h optimal en mètres pour X:", h_silverman_x, "\n")
  cat("h optimal en mètres pour Y:", h_silverman_y, "\n")
  locs_spa_foraging_ZOOM <- as(locs_foraging_ZOOM, "Spatial")
  # Appliquer kernelUD avec h estimé par Silverman
  kud_foraging_ZOOM <- kernelUD(locs_spa_foraging_ZOOM, 
                                grid = SpatialPixels_ZOOM, 
                                h = mean(c(h_silverman_x, h_silverman_y)))
  
  # Estimation des isoclines 
  rast_foraging_ZOOM <- rast(kud_foraging_ZOOM)
  courtour_foraging_ZOOM <- as.contour(rast_foraging_ZOOM)
  sf_foraging_ZOOM <- st_as_sf(courtour_foraging_ZOOM)
  cast_foraging_ZOOM <- st_cast(sf_foraging_ZOOM, "POLYGON")
  
  cast_foraging_ZOOM$ZOOM <- lettre
  cast_foraging_ZOOM_all <- rbind(cast_foraging_ZOOM_all, cast_foraging_ZOOM)
  
}

# plot
tmap_mode("view")
UDMap_ZOOM <- tm_scalebar() +
  tm_shape(RMO) +
  tm_polygons() +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(ZOOM_A) +
  tm_polygons(fill_alpha = 0.1, fill = "grey") +
  tm_text("Zoom A", size = 1.5) +
  tm_shape(ZOOM_B) +
  tm_polygons(fill_alpha = 0.1, fill = "grey") +
  tm_text("Zoom B", size = 1.5) +
  tm_shape(ZOOM_C) +
  tm_polygons(fill_alpha = 0.1, fill = "grey") +
  tm_text("Zoom C", size = 1.5) +
  tm_shape(ZOOM_D) +
  tm_polygons(fill_alpha = 0.1, fill = "grey") +
  tm_text("Zoom D", size = 1.5) +
  tm_shape(BOX_2154) +
  tm_borders(col = "black") +
  tm_shape(cast_foraging_ZOOM_all) + 
  tm_polygons(border.col = "grey", fill = "level", fill_alpha = 0.2, 
              palette = viridis(10, begin = 0, end = 1, 
                                direction = 1, option = "plasma")); UDMap_ZOOM


### id ----



# # Transformer en objet spatial (EPSG:4326)
# locs_foraging_ID <- st_as_sf(coords_foraging_ID, coords = c("lon", "lat"), crs = 4326)
# 
# # Reprojeter en système métrique (ex. UTM zone 30N - EPSG:32630 pour la France)
# locs_foraging_ID_32630 <- st_transform(locs_foraging_ID, crs = 32630)  # Adapter le CRS à votre région
# 
# # Reprojection du raster
# crs_utm <- CRS("+init=epsg:32630") # Définir le CRS cible (EPSG:32630 = UTM zone 30N)
# raster_100x100_32630 <- projectRaster(raster_100x100, crs = crs_utm)
# crs(raster_100x100_32630)
# 
# # Extraire les coordonnées reprojetées
# coords_foraging_ID_32630 <- st_coordinates(locs_foraging_ID_32630)
# 
# # Règle de Silverman
# sigma_x_foraging_ID <- sd(coords_foraging_ID_32630[,1])  # Écart-type en X (mètres)
# sigma_y_foraging_ID <- sd(coords_foraging_ID_32630[,2])  # Écart-type en Y (mètres)
# n_foraging_ID <- nrow(coords_foraging_ID_32630)  # Nombre de points
# 
# h_silverman_x_foraging_ID <- 1.06 * sigma_x_foraging_ID * n_foraging_ID^(-1/5)
# h_silverman_y_foraging_ID <- 1.06 * sigma_y_foraging_ID * n_foraging_ID^(-1/5)
# 
# cat("h optimal en mètres pour X:", h_silverman_x_foraging_ID, "\n")
# cat("h optimal en mètres pour Y:", h_silverman_y_foraging_ID, "\n")
# 
# # locs_spa <- as(locs_m, "Spatial")
# 
# # locs_spa <- st_transform(locs_foraging_ID, crs = 32630)
# locs_spa_foraging_ID <- as(locs_foraging_ID_32630, "Spatial")



# GPS point foraging ---

# All zone
coords_foraging_ID <- GPS %>% 
  filter(behavior == "foraging") %>% 
  dplyr::select(ID,lon,lat) %>% 
  st_drop_geometry() %>% 
  na.omit()

locs_foraging_ID <- st_as_sf(coords_foraging_ID, coords = c("lon", "lat"), crs = 4326)
locs_foraging_ID_32630 <- st_transform(locs_foraging_ID, crs = 32630)  # Reprojeter EPSG:32630 pour la France
coords_foraging_ID_32630 <- st_coordinates(locs_foraging_ID_32630) # Extraire les coordonnées reprojetées

#### 100x100m ------------------------------------------------------------------

# raster/grid
crs_utm <- "EPSG:32630"
SpatRaster_100x100_32630 <- project(raster_100x100, crs_utm)  # Reprojection du raster
RasterLayer_100x100 <- raster(SpatRaster_100x100_32630) # Convertir SpatRaster en RasterLayer
SpatialPixels_100x100 <- as(RasterLayer_100x100_raster, "SpatialPixels") # Convertir RasterLayer en SpatialPixels

# Règle de Silverman
sigma_x_foraging_ID_100x100_glob <- sd(coords_foraging_ID_32630[,1])  # Écart-type en X (mètres)
sigma_y_foraging_ID_100x100_glob <- sd(coords_foraging_ID_32630[,2])  # Écart-type en Y (mètres)
n_foraging_ID_100x100_glob <- nrow(coords_foraging_ID)  # Nombre de points
h_silverman_x_foraging_ID_100x100_glob <- 1.06 * sigma_x_foraging_ID_100x100_glob * n_foraging_ID_100x100_glob^(-1/5)
h_silverman_y_foraging_ID_100x100_glob <- 1.06 * sigma_y_foraging_ID_100x100_glob * n_foraging_ID_100x100_glob^(-1/5)
locs_spa_foraging_ID <- as(locs_foraging_ID_32630, "Spatial")
# Appliquer kernelUD avec h estimé par Silverman
kud_foraging_ID_100x100_glob <- kernelUD(locs_spa_foraging_ID["ID"], grid = SpatialPixels_100x100,
                                         h = mean(c(h_silverman_x_foraging_ID_100x100_glob, h_silverman_y_foraging_ID_100x100_glob)))

# Créer une liste pour stocker les résultats
UDmaps_list_foraging_ID_glob <- lapply(names(kud_foraging_ID_100x100_glob), function(ID) {
  
  print(ID)
  
  # Extraire l'estimation de densité pour un ID spécifique
  kud_single_foraging_ID <- kud_foraging_ID_100x100_glob[[ID]]
  rast_foraging_ID <- rast(kud_single_foraging_ID)
  contour_foraging_ID <- as.contour(rast_foraging_ID)
  sf_foraging_ID <- st_as_sf(contour_foraging_ID)
  cast_foraging_ID <- st_cast(sf_foraging_ID, "POLYGON")
  cast_foraging_ID$ID <- ID
  
  return(cast_foraging_ID)
})

# Fusionner tous les ID dans un seul objet sf
UDMap_final_foraging_ID_glob <- do.call(rbind, UDmaps_list_foraging_ID_glob)

UDMap_final_foraging_ID_glob$ID <- as.factor(UDMap_final_foraging_ID_glob$ID)

# write
st_write(UDMap_final_foraging_ID_glob, paste0(data_generated_path, "UDMap_final_foraging_ID_glob.gpkg"), append = FALSE)
# read
UDMap_final_foraging_ID_glob <- st_read(file.path(data_generated_path, "UDMap_final_foraging_ID_glob.gpkg"))

# plot
tmap_mode("view")
UDMap_100x100_foraging_ID_glob <- tm_scalebar() +
  tm_shape(RMO) +
  tm_polygons() +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(UDMap_final_foraging_ID_glob) + 
  tm_polygons(border.col = "grey", fill = "level", fill_alpha = 0.2, 
              palette = viridis(10, begin = 0, end = 1, 
                                direction = 1, option = "plasma")); UDMap_100x100_foraging_ID_glob
























































### Répétabilité inter-month ---------------------------------------------------

# Charger les données en lat/lon (EPSG:4326)
coords_roosting_rep_inter_month <- GPS %>% 
  filter(behavior == "roosting") %>% 
  dplyr::select(ID,datetime,lon,lat) %>% 
  mutate(month = month(datetime),
         ID_month = paste0(ID, "_", month)) %>% 
  st_drop_geometry() %>% 
  na.omit()

# au moins 5 point avant/après breche
n_per_month <- coords_roosting_rep_inter_month %>% 
  group_by(ID, month) %>% 
  summarize(n = n()) %>% 
  filter(n <=5) %>%
  mutate(ID_month = paste0(ID, "_", month))

# reverse of %in%  
`%ni%` <- Negate(`%in%`)

coords_roosting_rep_inter_month <- coords_roosting_rep_inter_month %>% 
  filter(ID_month %ni% n_per_month$ID_month)

# Transformer en objet spatial (EPSG:4326)
locs_roosting_rep_inter_month <- st_as_sf(coords_roosting_rep_inter_month, coords = c("lon", "lat"), crs = 4326)

# Reprojeter en système métrique (ex. UTM zone 30N - EPSG:32630 pour la France)
locs_roosting_rep_inter_month_32630 <- st_transform(locs_roosting_rep_inter_month, crs = 32630)  # Adapter le CRS à votre région

# Reprojection du raster
crs_utm <- CRS("+init=epsg:32630") # Définir le CRS cible (EPSG:32630 = UTM zone 30N)
raster_100x100_32630 <- projectRaster(raster_100x100, crs = crs_utm)
crs(raster_100x100_32630)

# Extraire les coordonnées reprojetées
coords_roosting_rep_inter_month_32630 <- st_coordinates(locs_roosting_rep_inter_month_32630)

# Règle de Silverman
sigma_x_roosting_rep_inter_month <- sd(coords_roosting_rep_inter_month_32630[,1])  # Écart-type en X (mètres)
sigma_y_roosting_rep_inter_month <- sd(coords_roosting_rep_inter_month_32630[,2])  # Écart-type en Y (mètres)
n_roosting_rep_inter_month <- nrow(coords_roosting_rep_inter_month_32630)  # Nombre de points

h_silverman_x_roosting_rep_inter_month <- 1.06 * sigma_x_roosting_rep_inter_month * n_roosting_rep_inter_month^(-1/5)
h_silverman_y_roosting_rep_inter_month <- 1.06 * sigma_y_roosting_rep_inter_month * n_roosting_rep_inter_month^(-1/5)

cat("h optimal en mètres pour X:", h_silverman_x_roosting_rep_inter_month, "\n")
cat("h optimal en mètres pour Y:", h_silverman_y_roosting_rep_inter_month, "\n")

locs_spa_roosting_rep_inter_month <- as(locs_roosting_rep_inter_month_32630, "Spatial")

# locs_spa_roosting$Periode <- ifelse(locs_spa_roosting$breche == "fermee", "Periode1", "Periode2")

# Créer une colonne combinée
# locs_spa_roosting$Individu_Periode <- paste(locs_spa_roosting$id, locs_spa_roosting$Periode, sep = "_")

# Vérifier que les noms sont bien générés
# unique(locs_spa_roosting$Individu_Periode)

# Calculer les KDE en séparant par individu et période

hr_kde_roosting_rep_inter_month <- kernelUD(locs_spa_roosting_rep_inter_month["ID_month"], 
                                            grid = as(raster_100x100_32630, "SpatialPixels"),
                                            h = mean(c(h_silverman_x_roosting_rep_inter_month, 
                                                       h_silverman_y_roosting_rep_inter_month)))

# Extraire les noms uniques des individus
individus <- unique(locs_spa_roosting_rep_inter_month$ID)

# Stocker les résultats
# overlap_results <- data.frame(Individu = character(), Overlap = numeric())
overlap_results = NULL

# Boucle sur chaque individu
for (ind in individus) {
  
  print(ind)
  
  # Trouver les noms des périodes de cet individu dans hr_kde
  ID_periodes <- names(hr_kde_roosting_rep_inter_month)[grep(paste0("^", ind, "_"), names(hr_kde_roosting_rep_inter_month))]
  
  # Vérifier que l'individu a bien deux périodes
  # if (length(ID_periodes) == 2) {
    # Créer un estUDm valide
    hr_kde_ind <- hr_kde_roosting_rep_inter_month[ID_periodes]
    class(hr_kde_ind) <- "estUDm"  # Important pour que kerneloverlaphr() fonctionne
    
    # Calculer l'overlap entre les deux périodes
    overlap_value <- kerneloverlaphr(hr_kde_ind, method = "BA")[1, 2]
    
    info_ind <- c(ind, overlap_value)
    
    # Stocker le résultat
    # overlap_results <- rbind(overlap_results, data.frame(Individu = ind, Overlap = overlap_value))
    overlap_results <- rbind(overlap_results, info_ind)
    
  # }
}

overlap_results <- as.data.frame(overlap_results)

overlap_results <- overlap_results %>% 
  rename(ID = V1, overlap = V2)

mean_overlap_month_over_all_ind <- mean(as.numeric(overlap_results$overlap), na.rm = T) ; mean_overlap_month_over_all_ind

# Afficher les résultats
overlap_results <- overlap_results[order(overlap_results$overlap), ] ; overlap_results

# Créer une liste pour stocker les résultats
UDmaps_list_roosting_rep_inter_month <- lapply(names(hr_kde_roosting_rep_inter_month), function(Individu_Periode) {
  
  print(Individu_Periode)
  
  # Extraire l'estimation de densité pour un ID spécifique
  kud_single_roosting_rep_inter_month <- hr_kde_roosting_rep_inter_month[[Individu_Periode]]
  rast_roosting_rep_inter_month <- rast(kud_single_roosting_rep_inter_month)
  contour_roosting_rep_inter_month <- as.contour(rast_roosting_rep_inter_month)
  sf_roosting_rep_inter_month <- st_as_sf(contour_roosting_rep_inter_month)
  cast_roosting_rep_inter_month <- st_cast(sf_roosting_rep_inter_month, "POLYGON")
  cast_roosting_rep_inter_month$Individu_Periode <- Individu_Periode
  
  return(cast_roosting_rep_inter_month)
})

# Fusionner tous les ID dans un seul objet sf
UDMap_final_roosting_rep_inter_month <- do.call(rbind, UDmaps_list_roosting_rep_inter_month)

UDMap_final_roosting_rep_inter_month$Individu_Periode <- as.factor(UDMap_final_roosting_rep_inter_month$Individu_Periode)
UDMap_final_roosting_rep_inter_month$ID <- sub("_.*", "", UDMap_final_roosting_rep_inter_month$Individu_Periode)

# UDMap_final_breche$id <- substring(UDMap_final_breche$Individu_Periode, first=1, last=8)
UDMap_final_roosting_rep_inter_month$Individu_Periode <- droplevels(UDMap_final_roosting_rep_inter_month$Individu_Periode)

UDMap_final_roosting_rep_inter_month$Periode <- sub(".*_", "", UDMap_final_roosting_rep_inter_month$Individu_Periode)

# UDMap_final_breche$Periode <- substring(UDMap_final_breche$Individu_Periode, first=10, last=18)
UDMap_final_roosting_rep_inter_month$ID <- as.factor(UDMap_final_roosting_rep_inter_month$ID)

tmap_mode("view")

UDMap_roosting_rep_inter_month <- tm_shape(RMO) +
  tm_polygons() +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(UDMap_final_roosting_rep_inter_month) + 
  tm_facets("ID") +
  tm_polygons(border.col = "grey", fill = "Periode", fill_fill_alpha = 0.2) ; UDMap_roosting_rep_inter_month

### Breche ---------------------------------------------------------------------

# Charger les données en lat/lon (EPSG:4326)
coords_breche <- GPS %>% 
  filter(behavior == "roosting") %>% 
  dplyr::select(lon, lat, breche) %>% 
  st_drop_geometry() %>% 
  na.omit()

# Transformer en objet spatial (EPSG:4326)
locs_breche <- st_as_sf(coords_breche, coords = c("lon", "lat"), crs = 4326)

# Reprojeter en système métrique (ex. UTM zone 30N - EPSG:32630 pour la France)
locs_breche_32630 <- st_transform(locs_breche, crs = 32630)  # Adapter le CRS à votre région

# Reprojection du raster
crs_utm <- CRS("+init=epsg:32630") # Définir le CRS cible (EPSG:32630 = UTM zone 30N)
raster_100x100_32630 <- projectRaster(raster_100x100, crs = crs_utm)
crs(raster_100x100_32630) # Vérifier le CRS

# Extraire les coordonnées reprojetées
coords_breche_32630 <- st_coordinates(locs_breche_32630)

# Règle de Silverman
sigma_x_breche <- sd(coords_breche_32630[,1])  # Écart-type en X (mètres)
sigma_y_breche <- sd(coords_breche_32630[,2])  # Écart-type en Y (mètres)
n_breche <- nrow(coords_breche)  # Nombre de points

h_silverman_x_breche <- 1.06 * sigma_x_breche * n_breche^(-1/5)
h_silverman_y_breche <- 1.06 * sigma_y_breche * n_breche^(-1/5)

cat("h optimal en mètres pour X:", h_silverman_x_breche, "\n")
cat("h optimal en mètres pour Y:", h_silverman_y_breche, "\n")

# locs_spa <- st_transform(locs, crs = 32630)
locs_spa_breche <- as(locs_breche_32630, "Spatial")

# Appliquer kernelUD avec h estimé par Silverman
kud_breche <- kernelUD(locs_spa_breche["breche"], grid = as(raster_100x100_32630, "SpatialPixels"),
                              h = mean(c(h_silverman_x_breche, h_silverman_y_breche)))

# Visualiser la densité de noyau
# par(mfrow = c(1, 1))
# image(kud_breche)

# Créer une liste pour stocker les résultats
UDmaps_list_breche <- lapply(names(kud_breche), function(breche) {
  
  print(breche)
  
  # Extraire l'estimation de densité pour un ID spécifique
  kud_single_breche <- kud_breche[[breche]]
  rast_breche <- rast(kud_single_breche)
  contour_breche <- as.contour(rast_breche)
  sf_breche <- st_as_sf(contour_breche)
  cast_breche <- st_cast(sf_breche, "POLYGON")
  cast_breche$breche <- breche
  
  return(cast_breche)
})

# Fusionner tous les ID dans un seul objet sf
UDMap_final_breche <- do.call(rbind, UDmaps_list_breche)

UDMap_final_breche$breche <- as.factor(UDMap_final_breche$breche)

tmap_mode("view")

UDMap_breche_gp1 <- tm_shape(RMO) +
  tm_polygons() +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(UDMap_final_breche) + 
  tm_polygons(border.col = "grey", fill = "breche", fill_fill_alpha = 0.2,
              fill.legend = tm_legend(legend.outside = T, legend.stack = "horizontal", legend.outside.position = 'bottom'))

# groupe plot
UDMap_final_breche_gp1 <- UDMap_final_breche %>% 
  filter(breche == "digue intacte")
UDMap_final_breche_gp1$breche <- droplevels(UDMap_final_breche_gp1$breche)
UDMap_final_breche_gp2 <- UDMap_final_breche %>% 
  filter(breche == "ouverture progressive")
UDMap_final_breche_gp2$breche <- droplevels(UDMap_final_breche_gp2$breche)
UDMap_final_breche_gp3 <- UDMap_final_breche %>% 
  filter(breche == "ouverture complète")
UDMap_final_breche_gp3$breche <- droplevels(UDMap_final_breche_gp3$breche)

# plot 
tmap_mode("view")

UDMap_breche_gp1 <- tm_shape(RMO) +
  tm_polygons() +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(UDMap_final_breche_gp1) + 
  tm_polygons(border.col = "grey", fill = "breche", fill_fill_alpha = 0.2,
              fill.legend = tm_legend(legend.outside = T, legend.stack = "horizontal", legend.outside.position = 'bottom'))

UDMap_breche_gp2 <- tm_shape(RMO) +
  tm_polygons() +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(UDMap_final_breche_gp2) + 
  tm_polygons(border.col = "grey", fill = "breche", fill_fill_alpha = 0.2,
              fill.legend = tm_legend(legend.outside = T, legend.stack = "horizontal", legend.outside.position = 'bottom'))

UDMap_breche_gp3 <- tm_shape(RMO) +
  tm_polygons() +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(UDMap_final_breche_gp3) + 
  tm_polygons(border.col = "grey", fill = "breche", fill_fill_alpha = 0.2,
              fill.legend = tm_legend(legend.outside = T, legend.stack = "horizontal", legend.outside.position = 'bottom')) 

UDMap_breche <- tmap_arrange(UDMap_breche_gp1, UDMap_breche_gp2, UDMap_breche_gp3) ; UDMap_breche

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


#### (similarité avant/après) ----

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

### Type de marée ------------------------------------------------

# Charger les données en lat/lon (EPSG:4326)
coords_tides_high_type <- GPS %>% 
  filter(behavior == "roosting") %>% 
  dplyr::select(lon, lat, tides_high_type) %>% 
  st_drop_geometry() %>% 
  na.omit()

# Transformer en objet spatial (EPSG:4326)
locs_tides_high_type <- st_as_sf(coords_tides_high_type, coords = c("lon", "lat"), crs = 4326)

# Reprojeter en système métrique (ex. UTM zone 30N - EPSG:32630 pour la France)
locs_tides_high_type_32630 <- st_transform(locs_tides_high_type, crs = 32630)  # Adapter le CRS à votre région

# Reprojection du raster
crs_utm <- CRS("+init=epsg:32630") # Définir le CRS cible (EPSG:32630 = UTM zone 30N)
raster_100x100_32630 <- projectRaster(raster_100x100, crs = crs_utm)
raster_100x100_raster <- raster(raster_100x100_32630)

# Convertir RasterLayer en SpatialPixels
raster_100x100_pixels <- as(raster_100x100_raster, "SpatialPixels")
crs(raster_100x100_32630) # Vérifier le CRS


# Extraire les coordonnées reprojetées
coords_tides_high_type_32630 <- st_coordinates(locs_tides_high_type_32630)

# Règle de Silverman
sigma_x_tides_high_type <- sd(coords_tides_high_type_32630[,1])  # Écart-type en X (mètres)
sigma_y_tides_high_type <- sd(coords_tides_high_type_32630[,2])  # Écart-type en Y (mètres)
n_tides_high_type <- nrow(coords_tides_high_type)  # Nombre de points

h_silverman_x_tides_high_type <- 1.06 * sigma_x_tides_high_type * n_tides_high_type^(-1/5)
h_silverman_y_tides_high_type <- 1.06 * sigma_y_tides_high_type * n_tides_high_type^(-1/5)

cat("h optimal en mètres pour X:", h_silverman_x_tides_high_type, "\n")
cat("h optimal en mètres pour Y:", h_silverman_y_tides_high_type, "\n")

# locs_spa <- st_transform(locs, crs = 32630)
locs_spa_tides_high_type <- as(locs_tides_high_type_32630, "Spatial")

# Appliquer kernelUD avec h estimé par Silverman
# kud_tides_high_type <- kernelUD(locs_spa_tides_high_type["tides_high_type"], grid = as(raster_100x100_32630, "SpatialPixels"),
#                               h = mean(c(h_silverman_x_tides_high_type, h_silverman_y_tides_high_type)))
kud_tides_high_type <- kernelUD(locs_spa_tides_high_type["tides_high_type"], grid = raster_100x100_pixels, 
                         h = mean(c(h_silverman_x_roosting, h_silverman_y_roosting)))

# Visualiser la densité de noyau
par(mfrow = c(1, 1))
image(kud_tides_high_type)

# Créer une liste pour stocker les résultats
UDmaps_list_tides_high_type <- lapply(names(kud_tides_high_type), function(tides_high_type) {
  
  print(tides_high_type)
  
  # Extraire l'estimation de densité pour un ID spécifique
  kud_single_tides_high_type <- kud_tides_high_type[[tides_high_type]]
  rast_tides_high_type <- rast(kud_single_tides_high_type)
  contour_tides_high_type <- as.contour(rast_tides_high_type)
  sf_tides_high_type <- st_as_sf(contour_tides_high_type)
  cast_tides_high_type <- st_cast(sf_tides_high_type, "POLYGON")
  cast_tides_high_type$tides_high_type <- tides_high_type
  
  return(cast_tides_high_type)
})

# Fusionner tous les ID dans un seul objet sf
UDMap_final_tides_high_type <- do.call(rbind, UDmaps_list_tides_high_type)

UDMap_final_tides_high_type$tides_high_type <- as.factor(UDMap_final_tides_high_type$tides_high_type)

# plot 
tmap_mode("view")

UDMap_tides_high_type <- tm_shape(RMO) +
  tm_polygons() +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(UDMap_final_tides_high_type) + 
  tm_polygons(border.col = "grey", fill = "tides_high_type", fill_fill_alpha = 0.2,
              fill.legend = tm_legend(legend.outside = T, legend.stack = "horizontal", legend.outside.position = 'bottom')) ; UDMap_tides_high_type

# # tmap_save(UDMap_tides_high_type, paste0(data_image_path, "/UDMap_roosting_tides_high_type.html"), dpi = 600)

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

### Jour / Nuit ------------------------------------------------

# Charger les données en lat/lon (EPSG:4326)
coords_jour_nuit <- GPS %>% 
  filter(behavior == "roosting") %>% 
  dplyr::select(lon, lat, jour_nuit) %>% 
  st_drop_geometry() %>% 
  na.omit()

# Transformer en objet spatial (EPSG:4326)
locs_jour_nuit <- st_as_sf(coords_jour_nuit, coords = c("lon", "lat"), crs = 4326)

# Reprojeter en système métrique (ex. UTM zone 30N - EPSG:32630 pour la France)
locs_jour_nuit_32630 <- st_transform(locs_jour_nuit, crs = 32630)  # Adapter le CRS à votre région

# Reprojection du raster
crs_utm <- CRS("+init=epsg:32630") # Définir le CRS cible (EPSG:32630 = UTM zone 30N)
raster_100x100_32630 <- projectRaster(raster_100x100, crs = crs_utm)
crs(raster_100x100_32630) # Vérifier le CRS

# Extraire les coordonnées reprojetées
coords_jour_nuit_32630 <- st_coordinates(locs_jour_nuit_32630)

# Règle de Silverman
sigma_x_jour_nuit <- sd(coords_jour_nuit_32630[,1])  # Écart-type en X (mètres)
sigma_y_jour_nuit <- sd(coords_jour_nuit_32630[,2])  # Écart-type en Y (mètres)
n_jour_nuit <- nrow(coords_jour_nuit)  # Nombre de points

h_silverman_x_jour_nuit <- 1.06 * sigma_x_jour_nuit * n_jour_nuit^(-1/5)
h_silverman_y_jour_nuit <- 1.06 * sigma_y_jour_nuit * n_jour_nuit^(-1/5)

cat("h optimal en mètres pour X:", h_silverman_x_jour_nuit, "\n")
cat("h optimal en mètres pour Y:", h_silverman_y_jour_nuit, "\n")

# locs_spa <- st_transform(locs, crs = 32630)
locs_spa_jour_nuit <- as(locs_jour_nuit_32630, "Spatial")

# Appliquer kernelUD avec h estimé par Silverman
kud_jour_nuit <- kernelUD(locs_spa_jour_nuit["jour_nuit"], grid = as(raster_100x100_32630, "SpatialPixels"),
                           h = mean(c(h_silverman_x_jour_nuit, h_silverman_y_jour_nuit)))

# Visualiser la densité de noyau
# par(mfrow = c(1, 1))
# image(kud_jour_nuit)

# Créer une liste pour stocker les résultats
UDmaps_list_jour_nuit <- lapply(names(kud_jour_nuit), function(jour_nuit) {
  
  print(jour_nuit)
  
  # Extraire l'estimation de densité pour un ID spécifique
  kud_single_jour_nuit <- kud_jour_nuit[[jour_nuit]]
  rast_jour_nuit <- rast(kud_single_jour_nuit)
  contour_jour_nuit <- as.contour(rast_jour_nuit)
  sf_jour_nuit <- st_as_sf(contour_jour_nuit)
  cast_jour_nuit <- st_cast(sf_jour_nuit, "POLYGON")
  cast_jour_nuit$jour_nuit <- jour_nuit
  
  return(cast_jour_nuit)
})

# Fusionner tous les ID dans un seul objet sf
UDMap_final_jour_nuit <- do.call(rbind, UDmaps_list_jour_nuit)

UDMap_final_jour_nuit$jour_nuit <- as.factor(UDMap_final_jour_nuit$jour_nuit)

# plot 
tmap_mode("view")

UDMap_jour_nuit <- tm_shape(RMO) +
  tm_polygons() +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(UDMap_final_jour_nuit) + 
  tm_polygons(border.col = "grey", fill = "jour_nuit", fill_fill_alpha = 0.2,
              fill.legend = tm_legend(legend.outside = T, legend.stack = "horizontal", legend.outside.position = 'bottom')) ; UDMap_jour_nuit

# # tmap_save(UDMap_jour_nuit, paste0(data_image_path, "/UDMap_roosting_jour_nuit.html"), dpi = 600)

### Age ------------------------------------------------

# Charger les données en lat/lon (EPSG:4326)
coords_age <- GPS %>% 
  filter(behavior == "roosting") %>% 
  dplyr::select(lon, lat, age) %>% 
  st_drop_geometry() %>% 
  na.omit()

# Transformer en objet spatial (EPSG:4326)
locs_age <- st_as_sf(coords_age, coords = c("lon", "lat"), crs = 4326)

# Reprojeter en système métrique (ex. UTM zone 30N - EPSG:32630 pour la France)
locs_age_32630 <- st_transform(locs_age, crs = 32630)  # Adapter le CRS à votre région

# Reprojection du raster
crs_utm <- CRS("+init=epsg:32630") # Définir le CRS cible (EPSG:32630 = UTM zone 30N)
raster_100x100_32630 <- projectRaster(raster_100x100, crs = crs_utm)
crs(raster_100x100_32630) # Vérifier le CRS

# Extraire les coordonnées reprojetées
coords_age_32630 <- st_coordinates(locs_age_32630)

# Règle de Silverman
sigma_x_age <- sd(coords_age_32630[,1])  # Écart-type en X (mètres)
sigma_y_age <- sd(coords_age_32630[,2])  # Écart-type en Y (mètres)
n_age <- nrow(coords_age)  # Nombre de points

h_silverman_x_age <- 1.06 * sigma_x_age * n_age^(-1/5)
h_silverman_y_age <- 1.06 * sigma_y_age * n_age^(-1/5)

cat("h optimal en mètres pour X:", h_silverman_x_age, "\n")
cat("h optimal en mètres pour Y:", h_silverman_y_age, "\n")

# locs_spa <- st_transform(locs, crs = 32630)
locs_spa_age <- as(locs_age_32630, "Spatial")

# Appliquer kernelUD avec h estimé par Silverman
kud_age <- kernelUD(locs_spa_age["age"], grid = as(raster_100x100_32630, "SpatialPixels"),
                          h = mean(c(h_silverman_x_age, h_silverman_y_age)))

# Visualiser la densité de noyau
# par(mfrow = c(1, 1))
# image(kud_age)

# Créer une liste pour stocker les résultats
UDmaps_list_age <- lapply(names(kud_age), function(age) {
  
  print(age)
  
  # Extraire l'estimation de densité pour un ID spécifique
  kud_single_age <- kud_age[[age]]
  rast_age <- rast(kud_single_age)
  contour_age <- as.contour(rast_age)
  sf_age <- st_as_sf(contour_age)
  cast_age <- st_cast(sf_age, "POLYGON")
  cast_age$age <- age
  
  return(cast_age)
})

# Fusionner tous les ID dans un seul objet sf
UDMap_final_age <- do.call(rbind, UDmaps_list_age)

UDMap_final_age$age <- as.factor(UDMap_final_age$age)

st_crs(UDMap_final_age) == st_crs(RMO)  # Vérifie si les projections sont identiques
UDMap_final_age <- st_transform(UDMap_final_age, st_crs(RMO))
table(is.na(UDMap_final_age$age))

# plot 
tmap_mode("view")

UDMap_final_age$age <- as.factor(UDMap_final_age$age)

UDMap_age <- tm_shape(RMO) +
  tm_polygons() +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(UDMap_final_age) + 
  tm_polygons(border.col = "grey", fill = "age", fill_fill_alpha = 0.2) ; UDMap_age

# # tmap_save(UDMap_age, paste0(data_image_path, "/UDMap_roosting_age.html"), dpi = 600)

### Sexe ------------------------------------------------

# Charger les données en lat/lon (EPSG:4326)
coords_sex <- GPS %>% 
  filter(behavior == "roosting") %>% 
  dplyr::select(lon, lat, sex) %>% 
  st_drop_geometry() %>% 
  na.omit()

# Transformer en objet spatial (EPSG:4326)
locs_sex <- st_as_sf(coords_sex, coords = c("lon", "lat"), crs = 4326)

# Reprojeter en système métrique (ex. UTM zone 30N - EPSG:32630 pour la France)
locs_sex_32630 <- st_transform(locs_sex, crs = 32630)  # Adapter le CRS à votre région

# Reprojection du raster
crs_utm <- CRS("+init=epsg:32630") # Définir le CRS cible (EPSG:32630 = UTM zone 30N)
raster_100x100_32630 <- projectRaster(raster_100x100, crs = crs_utm)
crs(raster_100x100_32630) # Vérifier le CRS

# Extraire les coordonnées reprojetées
coords_sex_32630 <- st_coordinates(locs_sex_32630)

# Règle de Silverman
sigma_x_sex <- sd(coords_sex_32630[,1])  # Écart-type en X (mètres)
sigma_y_sex <- sd(coords_sex_32630[,2])  # Écart-type en Y (mètres)
n_sex <- nrow(coords_sex)  # Nombre de points

h_silverman_x_sex <- 1.06 * sigma_x_sex * n_sex^(-1/5)
h_silverman_y_sex <- 1.06 * sigma_y_sex * n_sex^(-1/5)

cat("h optimal en mètres pour X:", h_silverman_x_sex, "\n")
cat("h optimal en mètres pour Y:", h_silverman_y_sex, "\n")

# locs_spa <- st_transform(locs, crs = 32630)
locs_spa_sex <- as(locs_sex_32630, "Spatial")

# Appliquer kernelUD avec h estimé par Silverman
kud_sex <- kernelUD(locs_spa_sex["sex"], grid = as(raster_100x100_32630, "SpatialPixels"),
                    h = mean(c(h_silverman_x_sex, h_silverman_y_sex)))

# Visualiser la densité de noyau
par(mfrow = c(1, 1))
image(kud_sex)

# Créer une liste pour stocker les résultats
UDmaps_list_sex <- lapply(names(kud_sex), function(sex) {
  
  print(sex)
  
  # Extraire l'estimation de densité pour un ID spécifique
  kud_single_sex <- kud_sex[[sex]]
  rast_sex <- rast(kud_single_sex)
  contour_sex <- as.contour(rast_sex)
  sf_sex <- st_as_sf(contour_sex)
  cast_sex <- st_cast(sf_sex, "POLYGON")
  cast_sex$sex <- sex
  
  return(cast_sex)
})

# Fusionner tous les ID dans un seul objet sf
UDMap_final_sex <- do.call(rbind, UDmaps_list_sex)

UDMap_final_sex$sex <- as.factor(UDMap_final_sex$sex)

# plot 
tmap_mode("view")

UDMap_sex <- tm_shape(RMO) +
  tm_polygons() +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(UDMap_final_sex) + 
  tm_polygons(border.col = "grey", fill = "sex", fill_fill_alpha = 0.2,
              fill.legend = tm_legend(legend.outside = T, legend.stack = "horizontal", legend.outside.position = 'bottom')) ; UDMap_sex

# # tmap_save(UDMap_sex, paste0(data_image_path, "/UDMap_roosting_sex.html"), dpi = 600)

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

## ALIMENTATION ----------------------------------------------------------------

### global ----

# Charger les données en lat/lon (EPSG:4326)
coords_alim <- GPS %>% 
  filter(behavior == "foraging") %>% 
  dplyr::select(lon,lat) %>% 
  st_drop_geometry() %>% 
  na.omit()

# Transformer en objet spatial (EPSG:4326)
locs_alim <- st_as_sf(coords_alim, coords = c("lon", "lat"), crs = 4326)

# Reprojeter en système métrique (ex. UTM zone 30N - EPSG:32630 pour la France)
locs_alim_32630 <- st_transform(locs_alim, crs = 32630)  # Adapter le CRS à votre région

# Reprojection du raster
crs_utm <- CRS("+init=epsg:32630") # Définir le CRS cible (EPSG:32630 = UTM zone 30N)
raster_100x100_32630 <- projectRaster(raster_100x100, crs = crs_utm)
crs(raster_100x100_32630) # Vérifier le CRS

# Extraire les coordonnées reprojetées
coords_alim_32630 <- st_coordinates(locs_alim_32630)

# Règle de Silverman
sigma_x_alim <- sd(coords_alim_32630[,1])  # Écart-type en X (mètres)
sigma_y_alim <- sd(coords_alim_32630[,2])  # Écart-type en Y (mètres)
n_alim <- nrow(coords_alim)  # Nombre de points

h_silverman_x_alim <- 1.06 * sigma_x_alim * n_alim^(-1/5)
h_silverman_y_alim <- 1.06 * sigma_y_alim * n_alim^(-1/5)

cat("h optimal en mètres pour X:", h_silverman_x_alim, "\n")
cat("h optimal en mètres pour Y:", h_silverman_y_alim, "\n")

# locs_spa <- st_transform(locs, crs = 32630)
locs_spa_alim <- as(locs_alim_32630, "Spatial")

# Appliquer kernelUD avec h estimé par Silverman
kud_alim <- kernelUD(locs_spa_alim, grid = as(raster_100x100_32630, "SpatialPixels"),
                         h = mean(c(h_silverman_x_alim, h_silverman_y_alim)))

# Visualiser la densité de noyau
# par(mfrow = c(1, 1))
# image(kud)

# Estimation des isoclines 
rast_alim <- rast(kud_alim)
courtour_alim <- as.contour(rast_alim)
sf_alim <- st_as_sf(courtour_alim)
cast_alim <- st_cast(sf_alim, "POLYGON")

# plot
tmap_mode("view")
UDMap_alim <- tm_scalebar() +
  tm_shape(RMO) +
  tm_polygons() +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(cast_alim) + 
  tm_polygons(border.col = "grey", fill = "level", fill_fill_alpha = 0.2, 
              palette = viridis(10, begin = 0, end = 1, 
                                direction = 1, option = "plasma")); UDMap_alim

# # tmap_save(UDMap_alim, paste0(data_image_path, "/UDMap_alim.html"), dpi = 600)

### id ----

# Charger les données en lat/lon (EPSG:4326)
coords_alim_ID <- GPS %>% 
  filter(behavior == "foraging") %>% 
  dplyr::select(ID,lon,lat) %>% 
  st_drop_geometry() %>% 
  na.omit()

# Transformer en objet spatial (EPSG:4326)
locs_alim_ID <- st_as_sf(coords_alim_ID, coords = c("lon", "lat"), crs = 4326)

# Reprojeter en système métrique (ex. UTM zone 30N - EPSG:32630 pour la France)
locs_alim_ID_32630 <- st_transform(locs_alim_ID, crs = 32630)  # Adapter le CRS à votre région

# Reprojection du raster
crs_utm <- CRS("+init=epsg:32630") # Définir le CRS cible (EPSG:32630 = UTM zone 30N)
raster_100x100_32630 <- projectRaster(raster_100x100, crs = crs_utm)
crs(raster_100x100_32630)

# Extraire les coordonnées reprojetées
coords_alim_ID_32630 <- st_coordinates(locs_alim_ID_32630)

# Règle de Silverman
sigma_x_alim_ID <- sd(coords_alim_ID_32630[,1])  # Écart-type en X (mètres)
sigma_y_alim_ID <- sd(coords_alim_ID_32630[,2])  # Écart-type en Y (mètres)
n_alim_ID <- nrow(coords_alim_ID_32630)  # Nombre de points

h_silverman_x_alim_ID <- 1.06 * sigma_x_alim_ID * n_alim_ID^(-1/5)
h_silverman_y_alim_ID <- 1.06 * sigma_y_alim_ID * n_alim_ID^(-1/5)

cat("h optimal en mètres pour X:", h_silverman_x_alim_ID, "\n")
cat("h optimal en mètres pour Y:", h_silverman_y_alim_ID, "\n")

# locs_spa <- as(locs_m, "Spatial")

# locs_spa <- st_transform(locs_alim_ID, crs = 32630)
locs_spa_alim_ID <- as(locs_alim_ID_32630, "Spatial")

# Appliquer kernelUD avec h estimé par Silverman
kud_alim_ID <- kernelUD(locs_spa_alim_ID["ID"], grid = as(raster_100x100_32630, "SpatialPixels"),
                            h = mean(c(h_silverman_x_alim_ID, h_silverman_y_alim_ID)))

# Visualiser la densité de noyau
par(mfrow = c(1, 1))
image(kud_alim_ID)

# Créer une liste pour stocker les résultats
UDmaps_list_alim_ID <- lapply(names(kud_alim_ID), function(ID) {
  
  print(ID)
  
  # Extraire l'estimation de densité pour un ID spécifique
  kud_single_alim_ID <- kud_alim_ID[[ID]]
  rast_alim_ID <- rast(kud_single_alim_ID)
  contour_alim_ID <- as.contour(rast_alim_ID)
  sf_alim_ID <- st_as_sf(contour_alim_ID)
  cast_alim_ID <- st_cast(sf_alim_ID, "POLYGON")
  cast_alim_ID$ID <- ID
  
  return(cast_alim_ID)
})

# Fusionner tous les ID dans un seul objet sf
UDMap_final_alim_ID <- do.call(rbind, UDmaps_list_alim_ID)

UDMap_final_alim_ID$ID <- as.factor(UDMap_final_alim_ID$ID)

# write
st_write(UDMap_final_alim_ID, paste0(data_generated_path, "UDMap_final_alim_ID.gpkg"), append = FALSE)
# read
UDMap_final_alim_ID <- st_read(file.path(data_generated_path, "UDMap_final_alim_ID.gpkg"))

# groupe plot
ID_list <- unique(UDMap_final_alim_ID$ID)
ID_gp_1 <- ID_list[1:15]
ID_gp_2 <- ID_list[16:30]
ID_gp_3 <- ID_list[31:45]
ID_gp_4 <- ID_list[46:69]

UDMap_final_alim_ID_gp1 <- UDMap_final_alim_ID %>% 
  filter(ID %in% ID_gp_1)
# UDMap_final_alim_ID_gp1$ID <- droplevels(UDMap_final_alim_ID_gp1$ID)
UDMap_final_alim_ID_gp2 <- UDMap_final_alim_ID %>% 
  filter(ID %in% ID_gp_2)
# UDMap_final_alim_ID_gp2$ID <- droplevels(UDMap_final_alim_ID_gp2$ID)
UDMap_final_alim_ID_gp3 <- UDMap_final_alim_ID %>% 
  filter(ID %in% ID_gp_3)
# UDMap_final_alim_ID_gp3$ID <- droplevels(UDMap_final_alim_ID_gp3$ID)
UDMap_final_alim_ID_gp4 <- UDMap_final_alim_ID %>% 
  filter(ID %in% ID_gp_4)
# UDMap_final_alim_ID_gp4$ID <- droplevels(UDMap_final_alim_ID_gp4$ID)

# plot 
tmap_mode("view")

UDMap_alim_ID_gp1 <- tm_shape(RMO) +
  tm_polygons() +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(UDMap_final_alim_ID_gp1) + 
  tm_polygons(border.col = "grey", fill = "ID", fill_fill_alpha = 0.2,
              fill.legend = tm_legend(legend.outside = T, legend.stack = "horizontal", legend.outside.position = 'bottom'))

UDMap_alim_ID_gp2 <- tm_shape(RMO) +
  tm_polygons() +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(UDMap_final_alim_ID_gp2) + 
  tm_polygons(border.col = "grey", fill = "ID", fill_fill_alpha = 0.2,
              fill.legend = tm_legend(legend.outside = T, legend.stack = "horizontal", legend.outside.position = 'bottom'))

UDMap_alim_ID_gp3 <- tm_shape(RMO) +
  tm_polygons() +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(UDMap_final_alim_ID_gp3) + 
  tm_polygons(border.col = "grey", fill = "ID", fill_fill_alpha = 0.2,
              fill.legend = tm_legend(legend.outside = T, legend.stack = "horizontal", legend.outside.position = 'bottom')) 

UDMap_alim_ID_gp4 <- tm_shape(RMO) +
  tm_polygons() +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(UDMap_final_alim_ID_gp4) + 
  tm_polygons(border.col = "grey", fill = "ID", fill_fill_alpha = 0.2,
              fill.legend = tm_legend(legend.outside = T, legend.stack = "horizontal", legend.outside.position = 'bottom'))

# UDMap_alim_ID <- tmap_arrange(UDMap_alim_ID_gp1, UDMap_alim_ID_gp2, UDMap_alim_ID_gp3, UDMap_alim_ID_gp4) ; UDMap_alim_ID

### Breche ------------------------------------------------

# Charger les données en lat/lon (EPSG:4326)
coords_breche <- GPS %>% 
  filter(behavior == "roosting") %>% 
  dplyr::select(lon, lat, breche) %>% 
  st_drop_geometry() %>% 
  na.omit()

# Transformer en objet spatial (EPSG:4326)
locs_breche <- st_as_sf(coords_breche, coords = c("lon", "lat"), crs = 4326)

# Reprojeter en système métrique (ex. UTM zone 30N - EPSG:32630 pour la France)
locs_breche_32630 <- st_transform(locs_breche, crs = 32630)  # Adapter le CRS à votre région

# Reprojection du raster
crs_utm <- CRS("+init=epsg:32630") # Définir le CRS cible (EPSG:32630 = UTM zone 30N)
raster_100x100_32630 <- projectRaster(raster_100x100, crs = crs_utm)
crs(raster_100x100_32630) # Vérifier le CRS

# Extraire les coordonnées reprojetées
coords_breche_32630 <- st_coordinates(locs_breche_32630)

# Règle de Silverman
sigma_x_breche <- sd(coords_breche_32630[,1])  # Écart-type en X (mètres)
sigma_y_breche <- sd(coords_breche_32630[,2])  # Écart-type en Y (mètres)
n_breche <- nrow(coords_breche)  # Nombre de points

h_silverman_x_breche <- 1.06 * sigma_x_breche * n_breche^(-1/5)
h_silverman_y_breche <- 1.06 * sigma_y_breche * n_breche^(-1/5)

cat("h optimal en mètres pour X:", h_silverman_x_breche, "\n")
cat("h optimal en mètres pour Y:", h_silverman_y_breche, "\n")

# locs_spa <- st_transform(locs, crs = 32630)
locs_spa_breche <- as(locs_breche_32630, "Spatial")

# Appliquer kernelUD avec h estimé par Silverman
kud_breche <- kernelUD(locs_spa_breche["breche"], grid = as(raster_100x100_32630, "SpatialPixels"),
                               h = mean(c(h_silverman_x_breche, h_silverman_y_breche)))

# Visualiser la densité de noyau
# par(mfrow = c(1, 1))
# image(kud_breche)

# Créer une liste pour stocker les résultats
UDmaps_list_breche <- lapply(names(kud_breche), function(breche) {
  
  print(breche)
  
  # Extraire l'estimation de densité pour un ID spécifique
  kud_single_breche <- kud_breche[[breche]]
  rast_breche <- rast(kud_single_breche)
  contour_breche <- as.contour(rast_breche)
  sf_breche <- st_as_sf(contour_breche)
  cast_breche <- st_cast(sf_breche, "POLYGON")
  cast_breche$breche <- breche
  
  return(cast_breche)
})

# Fusionner tous les ID dans un seul objet sf
UDMap_final_breche <- do.call(rbind, UDmaps_list_breche)

UDMap_final_breche$breche <- as.factor(UDMap_final_breche$breche)

tmap_mode("view")

UDMap_breche_gp1 <- tm_shape(RMO) +
  tm_polygons() +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(UDMap_final_breche) + 
  tm_polygons(border.col = "grey", fill = "breche", fill_fill_alpha = 0.2,
              fill.legend = tm_legend(legend.outside = T, legend.stack = "horizontal", legend.outside.position = 'bottom'))

# groupe plot
UDMap_final_breche_gp1 <- UDMap_final_breche %>% 
  filter(breche == "digue intacte")
UDMap_final_breche_gp1$breche <- droplevels(UDMap_final_breche_gp1$breche)
UDMap_final_breche_gp2 <- UDMap_final_breche %>% 
  filter(breche == "ouverture progressive")
UDMap_final_breche_gp2$breche <- droplevels(UDMap_final_breche_gp2$breche)
UDMap_final_breche_gp3 <- UDMap_final_breche %>% 
  filter(breche == "ouverture complète")
UDMap_final_breche_gp3$breche <- droplevels(UDMap_final_breche_gp3$breche)

# plot 
tmap_mode("view")

UDMap_breche_gp1 <- tm_shape(RMO) +
  tm_polygons() +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(UDMap_final_breche_gp1) + 
  tm_polygons(border.col = "grey", fill = "breche", fill_fill_alpha = 0.2,
              fill.legend = tm_legend(legend.outside = T, legend.stack = "horizontal", legend.outside.position = 'bottom'))

UDMap_breche_gp2 <- tm_shape(RMO) +
  tm_polygons() +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(UDMap_final_breche_gp2) + 
  tm_polygons(border.col = "grey", fill = "breche", fill_fill_alpha = 0.2,
              fill.legend = tm_legend(legend.outside = T, legend.stack = "horizontal", legend.outside.position = 'bottom'))

UDMap_breche_gp3 <- tm_shape(RMO) +
  tm_polygons() +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(UDMap_final_breche_gp3) + 
  tm_polygons(border.col = "grey", fill = "breche", fill_fill_alpha = 0.2,
              fill.legend = tm_legend(legend.outside = T, legend.stack = "horizontal", legend.outside.position = 'bottom')) 

# UDMap_breche <- tmap_arrange(UDMap_breche_gp1, UDMap_breche_gp2, UDMap_breche_gp3) ; UDMap_breche

### Jour / Nuit ------------------------------------------------

# Charger les données en lat/lon (EPSG:4326)
coords_alim_jour_nuit <- GPS %>% 
  filter(behavior == "foraging") %>% 
  dplyr::select(lon, lat, jour_nuit) %>% 
  st_drop_geometry() %>% 
  na.omit()

# Transformer en objet spatial (EPSG:4326)
locs_alim_jour_nuit <- st_as_sf(coords_alim_jour_nuit, coords = c("lon", "lat"), crs = 4326)

# Reprojeter en système métrique (ex. UTM zone 30N - EPSG:32630 pour la France)
locs_alim_jour_nuit_32630 <- st_transform(locs_alim_jour_nuit, crs = 32630)  # Adapter le CRS à votre région

# Reprojection du raster
crs_utm <- CRS("+init=epsg:32630") # Définir le CRS cible (EPSG:32630 = UTM zone 30N)
raster_100x100_32630 <- projectRaster(raster_100x100, crs = crs_utm)
crs(raster_100x100_32630) # Vérifier le CRS

# Extraire les coordonnées reprojetées
coords_alim_jour_nuit_32630 <- st_coordinates(locs_alim_jour_nuit_32630)

# Règle de Silverman
sigma_x_alim_jour_nuit <- sd(coords_alim_jour_nuit_32630[,1])  # Écart-type en X (mètres)
sigma_y_alim_jour_nuit <- sd(coords_alim_jour_nuit_32630[,2])  # Écart-type en Y (mètres)
n_alim_jour_nuit <- nrow(coords_alim_jour_nuit)  # Nombre de points

h_silverman_x_alim_jour_nuit <- 1.06 * sigma_x_alim_jour_nuit * n_alim_jour_nuit^(-1/5)
h_silverman_y_alim_jour_nuit <- 1.06 * sigma_y_alim_jour_nuit * n_alim_jour_nuit^(-1/5)

cat("h optimal en mètres pour X:", h_silverman_x_alim_jour_nuit, "\n")
cat("h optimal en mètres pour Y:", h_silverman_y_alim_jour_nuit, "\n")

# locs_spa <- st_transform(locs, crs = 32630)
locs_spa_alim_jour_nuit <- as(locs_alim_jour_nuit_32630, "Spatial")

# Appliquer kernelUD avec h estimé par Silverman
kud_alim_jour_nuit <- kernelUD(locs_spa_alim_jour_nuit["jour_nuit"], grid = as(raster_100x100_32630, "SpatialPixels"),
                          h = mean(c(h_silverman_x_alim_jour_nuit, h_silverman_y_alim_jour_nuit)))

# Visualiser la densité de noyau
# par(mfrow = c(1, 1))
# image(kud_alim_jour_nuit)

# Créer une liste pour stocker les résultats
UDmaps_list_alim_jour_nuit <- lapply(names(kud_alim_jour_nuit), function(jour_nuit) {
  
  print(jour_nuit)
  
  # Extraire l'estimation de densité pour un ID spécifique
  kud_single_alim_jour_nuit <- kud_alim_jour_nuit[[jour_nuit]]
  rast_alim_jour_nuit <- rast(kud_single_alim_jour_nuit)
  contour_alim_jour_nuit <- as.contour(rast_alim_jour_nuit)
  sf_alim_jour_nuit <- st_as_sf(contour_alim_jour_nuit)
  cast_alim_jour_nuit <- st_cast(sf_alim_jour_nuit, "POLYGON")
  cast_alim_jour_nuit$jour_nuit <- jour_nuit
  
  return(cast_alim_jour_nuit)
})

# Fusionner tous les ID dans un seul objet sf
UDMap_final_alim_jour_nuit <- do.call(rbind, UDmaps_list_alim_jour_nuit)

UDMap_final_alim_jour_nuit$jour_nuit <- as.factor(UDMap_final_alim_jour_nuit$jour_nuit)

# plot 
tmap_mode("view")

UDMap_alim_jour_nuit <- tm_shape(RMO) +
  tm_polygons() +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(UDMap_final_alim_jour_nuit) + 
  tm_polygons(border.col = "grey", fill = "jour_nuit", fill_fill_alpha = 0.2,
              fill.legend = tm_legend(legend.outside = T, legend.stack = "horizontal", legend.outside.position = 'bottom'))

# # tmap_save(UDMap_alim_jour_nuit, paste0(data_image_path, "/UDMap_alim_jour_nuit.html"), dpi = 600)

### Age ------------------------------------------------

# Charger les données en lat/lon (EPSG:4326)
coords_alim_age <- GPS %>% 
  filter(behavior == "foraging") %>% 
  dplyr::select(lon, lat, age) %>% 
  st_drop_geometry() %>% 
  na.omit()

# Transformer en objet spatial (EPSG:4326)
locs_alim_age <- st_as_sf(coords_alim_age, coords = c("lon", "lat"), crs = 4326)

# Reprojeter en système métrique (ex. UTM zone 30N - EPSG:32630 pour la France)
locs_alim_age_32630 <- st_transform(locs_alim_age, crs = 32630)  # Adapter le CRS à votre région

# Reprojection du raster
crs_utm <- CRS("+init=epsg:32630") # Définir le CRS cible (EPSG:32630 = UTM zone 30N)
raster_100x100_32630 <- projectRaster(raster_100x100, crs = crs_utm)
crs(raster_100x100_32630) # Vérifier le CRS

# Extraire les coordonnées reprojetées
coords_alim_age_32630 <- st_coordinates(locs_alim_age_32630)

# Règle de Silverman
sigma_x_alim_age <- sd(coords_alim_age_32630[,1])  # Écart-type en X (mètres)
sigma_y_alim_age <- sd(coords_alim_age_32630[,2])  # Écart-type en Y (mètres)
n_alim_age <- nrow(coords_alim_age)  # Nombre de points

h_silverman_x_alim_age <- 1.06 * sigma_x_alim_age * n_alim_age^(-1/5)
h_silverman_y_alim_age <- 1.06 * sigma_y_alim_age * n_alim_age^(-1/5)

cat("h optimal en mètres pour X:", h_silverman_x_alim_age, "\n")
cat("h optimal en mètres pour Y:", h_silverman_y_alim_age, "\n")

# locs_spa <- st_transform(locs, crs = 32630)
locs_spa_alim_age <- as(locs_alim_age_32630, "Spatial")

# Appliquer kernelUD avec h estimé par Silverman
kud_alim_age <- kernelUD(locs_spa_alim_age["age"], grid = as(raster_100x100_32630, "SpatialPixels"),
                    h = mean(c(h_silverman_x_alim_age, h_silverman_y_alim_age)))

# Visualiser la densité de noyau
par(mfrow = c(1, 1))
image(kud_alim_age)

# Créer une liste pour stocker les résultats
UDmaps_list_alim_age <- lapply(names(kud_alim_age), function(age) {
  
  print(age)
  
  # Extraire l'estimation de densité pour un ID spécifique
  kud_single_alim_age <- kud_alim_age[[age]]
  rast_alim_age <- rast(kud_single_alim_age)
  contour_alim_age <- as.contour(rast_alim_age)
  sf_alim_age <- st_as_sf(contour_alim_age)
  cast_alim_age <- st_cast(sf_alim_age, "POLYGON")
  cast_alim_age$age <- age
  
  return(cast_alim_age)
})

# Fusionner tous les ID dans un seul objet sf
UDMap_final_alim_age <- do.call(rbind, UDmaps_list_alim_age)

UDMap_final_alim_age$age <- as.factor(UDMap_final_alim_age$age)

# plot 
tmap_mode("view")

UDMap_alim_age <- tm_shape(RMO) +
  tm_polygons() +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(UDMap_final_alim_age) + 
  tm_polygons(border.col = "grey", fill = "age", fill_fill_alpha = 0.2,
              fill.legend = tm_legend(legend.outside = T, legend.stack = "horizontal", legend.outside.position = 'bottom'))

# # tmap_save(UDMap_alim_age, paste0(data_image_path, "/UDMap_roosting_alim_age.html"), dpi = 600)

### Sexe ------------------------------------------------

# Charger les données en lat/lon (EPSG:4326)
coords_alim_sex <- GPS %>% 
  filter(behavior == "foraging") %>% 
  dplyr::select(lon, lat, sex) %>% 
  st_drop_geometry() %>% 
  na.omit()

# Transformer en objet spatial (EPSG:4326)
locs_alim_sex <- st_as_sf(coords_alim_sex, coords = c("lon", "lat"), crs = 4326)

# Reprojeter en système métrique (ex. UTM zone 30N - EPSG:32630 pour la France)
locs_alim_sex_32630 <- st_transform(locs_alim_sex, crs = 32630)  # Adapter le CRS à votre région

# Reprojection du raster
crs_utm <- CRS("+init=epsg:32630") # Définir le CRS cible (EPSG:32630 = UTM zone 30N)
raster_100x100_32630 <- projectRaster(raster_100x100, crs = crs_utm)
crs(raster_100x100_32630) # Vérifier le CRS

# Extraire les coordonnées reprojetées
coords_alim_sex_32630 <- st_coordinates(locs_alim_sex_32630)

# Règle de Silverman
sigma_x_alim_sex <- sd(coords_alim_sex_32630[,1])  # Écart-type en X (mètres)
sigma_y_alim_sex <- sd(coords_alim_sex_32630[,2])  # Écart-type en Y (mètres)
n_alim_sex <- nrow(coords_alim_sex)  # Nombre de points

h_silverman_x_alim_sex <- 1.06 * sigma_x_alim_sex * n_alim_sex^(-1/5)
h_silverman_y_alim_sex <- 1.06 * sigma_y_alim_sex * n_alim_sex^(-1/5)

cat("h optimal en mètres pour X:", h_silverman_x_alim_sex, "\n")
cat("h optimal en mètres pour Y:", h_silverman_y_alim_sex, "\n")

# locs_spa <- st_transform(locs, crs = 32630)
locs_spa_alim_sex <- as(locs_alim_sex_32630, "Spatial")

# Appliquer kernelUD avec h estimé par Silverman
kud_alim_sex <- kernelUD(locs_spa_alim_sex["sex"], grid = as(raster_100x100_32630, "SpatialPixels"),
                    h = mean(c(h_silverman_x_alim_sex, h_silverman_y_alim_sex)))

# Visualiser la densité de noyau
par(mfrow = c(1, 1))
image(kud_alim_sex)

# Créer une liste pour stocker les résultats
UDmaps_list_alim_sex <- lapply(names(kud_alim_sex), function(sex) {
  
  print(sex)
  
  # Extraire l'estimation de densité pour un ID spécifique
  kud_single_alim_sex <- kud_alim_sex[[sex]]
  rast_alim_sex <- rast(kud_single_alim_sex)
  contour_alim_sex <- as.contour(rast_alim_sex)
  sf_alim_sex <- st_as_sf(contour_alim_sex)
  cast_alim_sex <- st_cast(sf_alim_sex, "POLYGON")
  cast_alim_sex$sex <- sex
  
  return(cast_alim_sex)
})

# Fusionner tous les ID dans un seul objet sf
UDMap_final_alim_sex <- do.call(rbind, UDmaps_list_alim_sex)

UDMap_final_alim_sex$sex <- as.factor(UDMap_final_alim_sex$sex)

# plot 
tmap_mode("view")

UDMap_alim_sex <- tm_shape(RMO) +
  tm_polygons() +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(UDMap_final_alim_sex) + 
  tm_polygons(border.col = "grey", fill = "sex", fill_fill_alpha = 0.2,
              fill.legend = tm_legend(legend.outside = T, legend.stack = "horizontal", legend.outside.position = 'bottom'))

# # tmap_save(UDMap_alim_sex, paste0(data_image_path, "/UDMap_roosting_alim_sex.html"), dpi = 600)

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








































##############################################################################################
###CREATE UTILISATION DISTRIBUTION (UD) PER COLONY FOR ALL SEASON

# Tracks.Colony<-list()
# KUD.Colony<-list()
# stk_KUD.Colony<-list()
# sum_all_KUD.Colony<-list()
# 
# for (i in levels(TracksYS.sf$colony_name)) {
#   Tracks.Colony[[i]]<-as(TracksYS.sf[TracksYS.sf$colony_name %in% i,], "Spatial")
#   Tracks.Colony[[i]]@data<-droplevels(Tracks.Colony[[i]]@data)
#   KUD.Colony[[i]]<-kernelUD(Tracks.Colony[[i]][, c("track_id")], h=0.2, grid=as(Grid, "SpatialPixels"))
#   stk_KUD.Colony[[i]] <-stack(estUDm2spixdf(KUD.Colony[[i]]))
#   sum_all_KUD.Colony[[i]] <- overlay(stk_KUD.Colony[[i]], fun = mean)
#   sum_all_KUD.Colony[[i]] <- sum_all_KUD.Colony[[i]]/sum(getValues(sum_all_KUD.Colony[[i]]))
# }
# 
# rm(KUD.Colony,stk_KUD.Colony,Tracks.Colony)
# 
# sum(getValues(sum_all_KUD.Colony))
# 
# #WEIGTH UDs PER POP SIZE OF COLONIES
# 
# TracksYS.sf[!TracksYS.sf$colony_name %in% PopColony$colony_name,] #ok all find colony size
# 
# data.frame(TracksYS.sf) %>% group_by(colony_name) %>% summarize(NbTracks=n_distinct(track_id)) %>% data.frame()
# 
# KUD.Colony.weigh.season<-list()
# 
# for (i in levels(TracksYS.sf$colony_name)) {
#   KUD.Colony.weigh.season[[i]]<-sum_all_KUD.Colony[[i]]*(PopColony[PopColony$colony_name %in% i,]$pop_size_best/sum(PopColony$pop_size_best))
# }
# 
# KUD.Colony.weigh.season <- stack(KUD.Colony.weigh.season)
# KUD.Colony.weigh.season <- overlay(KUD.Colony.weigh.season, fun = mean)
# KUD.Colony.weigh.season <- KUD.Colony.weigh.season/sum(getValues(KUD.Colony.weigh.season))
# 
# sum(getValues(KUD.Colony.weigh.season),na.rm=TRUE)






# ################################################################################
# ### 1- Load data
# ################################################################################
# 
# XYcolonies<-read.table("C:/Users/gwenael.quaintenne/OneDrive - LPO/LIFE_PanPuffinus/DataLifePanPuffinusA3/BirdsData/ColonySizeData/XYcolonySize_YS.csv", header=TRUE, sep=";", encoding="UTF-8",stringsAsFactors = TRUE)
# 
# COUNTRY<-XYcolonies %>% group_by(Country_code,Country_name) %>% summarise(MinPop=sum(pop_size_min,na.rm = TRUE),MaxPop=sum(pop_size_max,na.rm = TRUE))
# COUNTRY$YlatDD<-c(41,37.5,42.35,39,43.5,39.5,35.7,36.3)
# COUNTRY$XlongDD<-c(16,6,5.5,25,16.5,10.6,15.4,10.2)
# COUNTRY$Country_nameFR<-as.factor(c("Albanie","Algérie","France","Grèce","Croatie","Italie","Malte","Tunisie"))
# COUNTRY$Label<-paste0(COUNTRY$Country_name," ",formatC(COUNTRY$MinPop, format="d", big.mark=" "),"-",formatC(COUNTRY$MaxPop, format="d", big.mark=" "), " p.")
# COUNTRY$LabelFR<-paste0(COUNTRY$Country_nameFR," ",formatC(COUNTRY$MinPop, format="d", big.mark=" "),"-",formatC(COUNTRY$MaxPop, format="d", big.mark=" "), " c.")
# 
# #XYcolonies<-st_as_sf(XYcolonies, coords = c("XlongDD", "YlatDD"), crs="EPSG:4326")
# 
# land<-st_read("C:/Users/gwenael.quaintenne/OneDrive - LPO/LIFE_PanPuffinus/SIG_A3LifePanPuffinus/FondCarto/Land/CNTR_RG_01M_2020_4326.shp",crs="EPSG:4326",stringsAsFactors =TRUE)
# 
# #Extend<-st_bbox(st_as_sf(XYcolonies, coords = c("XlongDD", "YlatDD"), crs="EPSG:4326"))
# #Extend[c(3,4)]<-Extend[c(3,4)]+2
# #Extend[c(1,2)]<-Extend[c(1,2)]-2
# #land<-st_crop(land, Extend)
# 
# TracksYS<-readRDS("InputR/YStracks/AllTracksYS.rds")
# 
# unique(TracksYS[!(TracksYS$colony_name %in% XYcolonies$colony_name & TracksYS$site_name %in% XYcolonies$site_name),]$site_name)
# #only Malta have tracks with no colony affiliation
# 
# NbTracks<-TracksYS %>% group_by(colony_name,site_name) %>% summarise(NbTracks=n_distinct(track_id))
# 
# XYcolonies<-left_join(XYcolonies,NbTracks,by=c("site_name","colony_name"))
# sum(XYcolonies$NbTracks,na.rm=TRUE)
# sum(NbTracks[!NbTracks$colony_name %in% c("Malta"),]$NbTracks) #ok the same

# ################################################################################
# ### 2- Map of breeding colonies at the scale of Mediterranean Sea
# ################################################################################
# 
# IntCol<-classIntervals(XYcolonies$pop_size_best, n=6, style = "jenks")
# 
# ggplot()+
#   geom_sf(data=land, fill=rgb(240, 240, 240, max=255),color=rgb(79, 79, 79, max=255))+
#   geom_point(aes(x = XlongDD, y = YlatDD), data = XYcolonies, fill=rgb(128, 128, 128,max=255), shape=3, size=0.8, col="black")+
#   geom_point(aes(x = XlongDD, y = YlatDD, size = pop_size_best), data = XYcolonies, fill=rgb(128, 128, 128,max=255), shape=19, col="#0d72a6", fill_alpha=1)+
#   scale_size(breaks=IntCol$brks[c(2:7)],guide="legend", label=IntCol$brks[c(2:7)], range=c(2,15), name="Nb. pairs")+
#   geom_point(aes(x = XlongDD, y = YlatDD, size = pop_size_best), data = XYcolonies, col=rgb(128, 128, 128,max=255), shape=1, fill_alpha=1)+
#   geom_point(aes(x = XlongDD, y = YlatDD, size = pop_size_best), data = XYcolonies[is.na(XYcolonies$NbTracks)==0,], col="#F55F19", shape=1, fill_alpha=1,show.legend=FALSE)+
#   geom_label(aes(x = XlongDD, y = YlatDD, label=Label),data=COUNTRY,colour="black",hjust = 0,family="Source Sans Pro")+
#   geom_text(aes(x = Inf, y = Inf, label=" ©LIFE PanPuffinus! (2023)"),colour="#0d72a6",hjust = 0,vjust = 1, angle = -90,size=3,family="Source Sans Pro")+
#   geom_text(aes(x = XlongDD+0.8, y = YlatDD, label=NbTracks),data=XYcolonies,colour="#F55F19",hjust = 0,family="Source Sans Pro")+
#   xlim(4,28)+
#   ylim(34,44)+
#   coord_sf()+
#   ggtitle("Yelkouan Shearwater - Location of the known breeding colonies and population size (Updated: January 2023) & tracked sample")+
#   theme(text= element_text(family="Source Sans Pro"),plot.title = element_text(family="Source Sans Pro", size=12),legend.position="bottom",legend.title = element_text(family="Source Sans Pro Semibold", size=10),legend.margin=margin(t=-15),legend.background = element_blank(),legend.key = element_blank())+
#   guides(size = guide_legend(nrow = 1))+
#   labs(tag = "Tracked sample") +
#   theme(plot.tag.position = c(0.53, 0.045),plot.tag=element_text(family="Source Sans Pro Semibold", size=10,color="#F55F19"))
# 
# ggsave("OutputR/ColonySize/YSmapcoloniessize.wmf", dpi=320, width=1050, height=609)
# 
# ###For Thierry
# ggplot()+
#   geom_sf(data=land, fill=rgb(240, 240, 240, max=255),color=rgb(79, 79, 79, max=255))+
#   geom_point(aes(x = XlongDD, y = YlatDD), data = XYcolonies, fill=rgb(128, 128, 128,max=255), shape=3, size=0.8, col="black")+
#   geom_point(aes(x = XlongDD, y = YlatDD, size = pop_size_best), data = XYcolonies, fill=rgb(128, 128, 128,max=255), shape=19, col="#0d72a6", fill_alpha=1)+
#   scale_size(breaks=IntCol$brks[c(2:7)],guide="legend", label=IntCol$brks[c(2:7)], range=c(2,15), name="Nb. couples")+
#   geom_point(aes(x = XlongDD, y = YlatDD, size = pop_size_best), data = XYcolonies, col=rgb(128, 128, 128,max=255), shape=1, fill_alpha=1)+
#   geom_label(aes(x = XlongDD, y = YlatDD, label=LabelFR),data=COUNTRY,colour="#0d72a6",hjust = 0)+
#   geom_text(aes(x = Inf, y = Inf, label="©LPO Birdlife France (2023)"),colour="#0d72a6",hjust = 0,vjust = 1, angle = -90,size=3)+
#   xlim(4,28)+
#   ylim(34,44)+
#   coord_sf()+
#   ggtitle("Puffin Yelkouan - Localisation des colonies connues et taille des pop. (Mise à jour: Décembre 2022)")+
#   theme(plot.title = element_text(family="Source Code Pro Semibold", size=10),legend.position="bottom",legend.title = element_text(family="Source Code Pro Semibold", size=10),legend.margin=margin(t=-15),legend.background = element_blank(),legend.key = element_blank())+
#   guides(size = guide_legend(nrow = 1))
# 
# ggsave("OutputR/ColonySize/YSmapcoloniessizeFR.emf", dpi=320, width=1050, height=609)
# 
# 
# #with ggOceanMaps
# 
# ggOceanMaps::basemap(limits = c(2,30, 32, 45), bathymetry = TRUE,projection.grid = TRUE,legends = FALSE,grid.col="grey70",grid.size=0.05,land.col = "#f0f0f0",land.border.col ="#4f4f4f",land.size = 0.1)+
#   geom_sf(data=land, fill="#f0f0f0",color="#4f4f4f")+
#   annotation_scale(location = "br",text_family="Source Sans Pro",bar_cols = c("#4f4f4f", "white"),text_col = "#4f4f4f",line_col = "#4f4f4f") +
#   annotation_north_arrow(location = "tr", which_north = "true",style=north_arrow_orienteering(
#     line_width = 0.1,line_col = "#4f4f4f",fill = c("white", "#4f4f4f"),text_col = "#4f4f4f",text_family = "Source Sans Pro"))+
#   geom_point(aes(x = XlongDD, y = YlatDD), data = XYcolonies, fill=rgb(128, 128, 128,max=255), shape=3, size=0.8, col="#4f4f4f")+
#   geom_point(aes(x = XlongDD, y = YlatDD, size = pop_size_best), data = XYcolonies, fill=rgb(128, 128, 128,max=255), shape=19, col="#4f4f4f", fill_alpha=1)+
#   scale_size(breaks=IntCol$brks[c(2:7)],guide="legend", label=IntCol$brks[c(2:7)], range=c(2,15), name="Nb. pairs")+
#   geom_point(aes(x = XlongDD, y = YlatDD, size = pop_size_best), data = XYcolonies, col=rgb(128, 128, 128,max=255), shape=1, fill_alpha=1)+
#   geom_label(aes(x = XlongDD, y = YlatDD, label=Label),data=COUNTRY,colour="black",hjust = 0,family="Source Sans Pro")+
#   geom_text(aes(x = Inf, y = Inf, label="©LIFE PanPuffinus! (2023)"),colour="black",hjust = 0,vjust = 1, angle = -90,size=3,family="Source Sans Pro")+
#   xlim(4,28)+
#   ylim(34,44)+
#   coord_sf()+
#   ggtitle("Yelkouan Shearwater - Location of the known breeding colonies and population size (Updated: January 2023)")+
#   theme_map(base_family = "Source Sans Pro")+
#   theme(text=element_text(family="Source Sans Pro"),plot.title = element_text(family="Source Sans Pro Semibold", size=12),legend.position="bottom",legend.title = element_text(family="Source Sans Pro Semibold", size=10),legend.margin=margin(t=-15),legend.background = element_blank(),legend.key = element_blank())+
#   guides(size = guide_legend(nrow = 1))
# 
# #with bathy
# c('#ffff80ff','#8ff041ff','#3dd13bff','#2fa190ff','#215896ff','#0d1178ff')
# 
# ggOceanMaps::basemap(limits = c(2,30, 32, 45), bathymetry = TRUE, projection.grid = TRUE,legends = FALSE,grid.col="grey70",grid.size=0.05,land.col = "#f0f0f0",land.border.col ="#4f4f4f",land.size = 0.1)+
#   annotation_scale(location = "br",text_family="Source Sans Pro",bar_cols = c("#4f4f4f", "white"),text_col = "#4f4f4f",line_col = "#4f4f4f") +
#   annotation_north_arrow(location = "tr", which_north = "true",style=north_arrow_orienteering(
#     line_width = 0.1,line_col = "#4f4f4f",fill = c("white", "#4f4f4f"),text_col = "#4f4f4f",text_family = "Source Sans Pro"))+
#   geom_point(aes(x = XlongDD, y = YlatDD), data = XYcolonies, fill=rgb(128, 128, 128,max=255), shape=3, size=0.8, col="black")+
#   geom_point(aes(x = XlongDD, y = YlatDD, size = pop_size_best), data = XYcolonies, fill=rgb(128, 128, 128,max=255), shape=19, col="black", fill_alpha=1)+
#   scale_size(breaks=IntCol$brks[c(2:7)],guide="legend", label=IntCol$brks[c(2:7)], range=c(2,15), name="Nb. pairs")+
#   geom_point(aes(x = XlongDD, y = YlatDD, size = pop_size_best), data = XYcolonies, col=rgb(128, 128, 128,max=255), shape=1, fill_alpha=1)+
#   geom_point(aes(x = XlongDD, y = YlatDD, size = pop_size_best), data = XYcolonies[is.na(XYcolonies$NbTracks)==0,], col="cyan", shape=1, fill_alpha=1,show.legend=FALSE,stroke = 1.5)+
#   geom_label(aes(x = XlongDD, y = YlatDD, label=Label),data=COUNTRY,colour="black",hjust = 0,family="Source Sans Pro")+
#   geom_text(aes(x = Inf, y = Inf, label="©LIFE PanPuffinus! (2023)"),colour="black",hjust = 0,vjust = 1, angle = -90,size=3,family="Source Sans Pro")+
#   geom_label(aes(x = XlongDD+0.6, y = YlatDD+0.4, label=NbTracks),data=XYcolonies,colour="cyan",hjust = 0,family="Source Sans Pro")+
#   xlim(4,28)+
#   ylim(34,44)+
#   coord_sf()+
#   ggtitle("Yelkouan Shearwater - Location of the known breeding colonies and population size (Updated: January 2023)")+
#   theme(text=element_text(family="Source Sans Pro"),plot.title = element_text(family="Source Sans Pro", size=12),legend.position="bottom",legend.title = element_text(family="Source Sans Pro Semibold", size=10),legend.margin=margin(t=-15),legend.background = element_blank(),legend.key = element_blank(),axis.text= element_text(family="Source Sans Pro Semibold", size=10),axis.title=element_blank())+
#   guides(size = guide_legend(nrow = 1))+
#   labs(tag = "Tracked sample") +
#   theme(plot.tag.position = c(0.82, 0.045),plot.tag=element_text(family="Source Sans Pro Semibold", size=10,color="cyan"))
# 
# rm(IntCol,COUNTRY)

# ################################################################################
# ### 3- Descriptive statistics on tracks deployment
# ################################################################################
# 
# ###Age classes deployment
# TracksYS %>% group_by(country_name,site_name,device,age) %>% summarize(MinYear=min(year(date)),MaxYear=max(year(date)),NbTracks=n_distinct(track_id)) %>% spread(key = age, value = NbTracks, fill = 0) %>% data.frame()
# 
# ###Month deployment
# TracksYS$month<-lubridate::month(TracksYS$date,label=TRUE,abbr=TRUE,locale="English_United States")
# unique(month(TracksYS$date))
# 
# TracksYS %>% group_by(country_name,site_name,device,month) %>% summarize(NbTracks=n_distinct(track_id)) %>% spread(key = month, value = NbTracks, fill = 0) %>% data.frame()
# 
# TracksYS %>% group_by(country_name,device,month) %>% summarize(NbTracks=n_distinct(track_id)) %>% spread(key = month, value = NbTracks, fill = 0) %>% data.frame()
# 
# ###Breed_stage and month
# unique(TracksYS$breed_stage)
# TracksYS$breed_stage<- ordered(sizes, levels = c("pre-egg", "incubation","brood-guard","post-guard","chick-rearing","breeding","fail (breeding season)","non-breeding","unknown"))
# 
# 
# TracksYS %>% filter(device=="GPS") %>% group_by(country_name,breed_stage,breed_status,month,device) %>% summarize(NbTracks=n_distinct(track_id)) %>% spread(key = month, value = NbTracks, fill = 0) %>% data.frame()
# 
# 
# TracksYS %>% filter(device=="GPS") %>% group_by(country_name,month,device) %>% summarize(NbTracks=n_distinct(track_id)) %>% spread(key = month, value = NbTracks, fill = 0) %>% data.frame()
# 
# ###Plot Country*Month
# 
# NbTracksYS.month.country<-TracksYS %>% filter(device=="GPS") %>% group_by(country_name,month,device) %>% summarize(NbTracks=n_distinct(track_id)) %>% data.frame()
# 
# source("C:/Users/gwenael.quaintenne/OneDrive - LPO/Enquete_AnatidesLimicolesNich/DataLIMAT2021-2022/1-CheckSamplingEffort/SourcesGraph.R",encoding ="UTF-8")
# 
# g<-c("#164D68","#777B7F","#EDEFE2","#88CCEE","#4E87B6","#B4BCCC")
# 
# ggplot(NbTracksYS.month.country, aes(x=month,y=NbTracks,fill=country_name)) + 
#   geom_bar(stat="identity")+
#   scale_fill_manual(values=g)+
#   ylab("Nb tracks") +
#   labs(fill="")+
#   theme_wsj(base_size=12,) +
#   ggtitle("GPS Month/Country's deployment")+
#   guides(fill = guide_legend(nrow = 1))
# #size output 713*445
# 
# ### Get time lag
# 
# TimeLag <- TracksYS %>% filter(device=="GPS") %>%
#   arrange(track_id, date) %>% 
#   group_by(track_id) %>%
#   mutate(time_lag = date-lag(date)) %>%
#   data.frame()
# 
# MeanTimeLag <- TimeLag %>% group_by(dataset_id,track_id) %>% Rmisc::summarySE(measurevar = "time_lag", groupvars = c("dataset_id","track_id"), na.rm=TRUE,conf.interval = 0.95) %>%
#   data.frame()
# 
# MeanTimeLag %>% Rmisc::summarySE(measurevar = "time_lag", na.rm=TRUE,conf.interval = 0.95) %>%
#   data.frame()
# 
# (3700.12-502.6121)/60 #53
# (3700.12+502.6121)/60 #70
# 3700.12/60 #62
# 
# 
# ggplot(MeanTimeLag, aes(x=time_lag/60)) + 
#   geom_histogram(fill="#4485ab")+
#   ylab("Nb tracks") +
#   theme_wsj(base_size=12) +
#   ggtitle("GPS sampling frequency (minutes)")+
#   geom_vline(aes(xintercept=3700.12/60),color="#777B7F", size=0.5)+
#   geom_vline(aes(xintercept=(3700.12-502.6121)/60),color="grey", linetype="dashed", size=0.5)+
#   geom_vline(aes(xintercept=(3700.12+502.6121)/60),color="grey", linetype="dashed", size=0.5)+
#   geom_text(aes(x=175,y=125,label="mean = 62 ±IC95% 8 min"),color="#777B7F",family="Source Sans Pro")
# 
# rm(list=ls()[! ls() %in% c("land","TracksYS","XYcolonies")])

# ################################################################################
# ### 4- Select data
# ################################################################################
# 
# #removed PTT tracks
# TracksYS<-droplevels(TracksYS[TracksYS$device=="GPS",]) # 229 402 obs.
# 
# wgs84 <- st_crs("EPSG:4326")
# 
# ##sf object
# TracksYS.sf <- st_as_sf(TracksYS, coords = c("longitude", "latitude"), crs=4326)
# 
# leaflet() %>% ## start leaflet plot
#   addProviderTiles(providers$Esri.WorldImagery, group = "World Imagery") %>% 
#   ## plot the points. Note: leaflet automatically finds lon / lat colonies
#   ## Colour accordingly.
#   addCircleMarkers(data = TracksYS.sf,
#                    radius = 3,
#                    fillColor = "cyan",
#                    fillOpacity = 0.5, stroke = F) 

################################################################################
### 5- McConnel Speedilter realistic velocity was set at 100 km/h
################################################################################

# ## create trip object (will removed duplicates)
# nrow(TracksYS[is.na(TracksYS$date),]) #one location without date???
# TracksYS<-TracksYS[!is.na(TracksYS$date),]
# 
# YStracks.trip <- TracksYS %>% 
#   group_by(track_id) %>% 
#   dplyr::select(x = longitude, 
#                 y = latitude, 
#                 datetime = date, 
#                 everything()) %>% 
#   trip()
# 
# # McConnel Speedilter
# YStracks.trip$Filter <- speedfilter(YStracks.trip, max.speed = 100)  # speed in km/h
# summary(YStracks.trip$Filter) #ok 67 locations were removed
# 
# #It remains 229 273 locations
# 
# YStracks.trip <- data.frame(YStracks.trip) %>% mutate(longitude = x,latitude = y)
# 
# ## plot McConnel Removed values
# leaflet() %>%
#   addProviderTiles(providers$Esri.WorldImagery, group = "World Imagery") %>%
#   addCircleMarkers(data = subset(YStracks.trip, YStracks.trip$Filter == T),
#                    radius = 7,
#                    fillColor = "cyan",
#                    fillOpacity = 0.5, stroke = F) %>% 
#   addCircleMarkers(data = subset(YStracks.trip, YStracks.trip$Filter == F),
#                    #label = bird_track_gaps$nlocs, 
#                    radius = 5,
#                    fillColor = "purple",
#                    fillOpacity = 0.5, stroke = F) %>% 
#   addLegend(title="McConnel Speedilter", colors = c("cyan","purple"),
#             labels = c("keeped values","Removed values"))
# 
# 
# YStracks <- YStracks.trip %>% filter(Filter==TRUE) %>% data.frame() # 229 273 obs.

################################################################################
### 6- Linear interpolation locs every 30 min
################################################################################

# ## create ltraj object
# YStracks.ltraj <- as.ltraj(xy = bind_cols(x = YStracks$x, 
#                                           y = YStracks$y),
#                            date = YStracks$datetime,
#                            id = YStracks$track_id)
# 
# ## re-sample tracks every 60 minutes (60*60 sec)
# YStracks.interp <- redisltraj(YStracks.ltraj, 60*60, type="time")
# YStracks.interp <- ld(YStracks.interp) %>% mutate(longitude = x,latitude = y)
# 
# #this results in 137 986 locations
# 
# leaflet() %>% 
#   addProviderTiles(providers$Esri.WorldImagery, group = "World Imagery") %>%
#   addCircleMarkers(data = YStracks,
#                    radius = 5,
#                    fillColor = "grey",
#                    fillOpacity = 0.5, stroke = F) %>%
#   addCircleMarkers(data = YStracks.interp,
#                    radius = 5,
#                    fillColor = "cyan",
#                    fillOpacity = 0.5, stroke = F) %>%
#   addLegend(title = "Rediscretization of trajectories",
#             colors = c("grey","cyan"),
#             labels = c("original locs","resampled locs"))
# #zoom on ok looks fine

################################################################################
### 7- Removed locations within 5km buffer of the colony
################################################################################

# YStracks.interp<-left_join(YStracks.interp,unique(YStracks[,c("track_id","site_name","colony_name","lon_colony","lat_colony","device")]),by = c("id" = "track_id"))
# 
# col<-unique(YStracks.interp[c("site_name","colony_name","lon_colony","lat_colony")])
# col$id_col<-factor(c(1:nrow(col)))
# YStracks.interp<-left_join(YStracks.interp,col)
# 
# YStracks.interp.sf<-st_as_sf(YStracks.interp, coords = c("longitude", "latitude"), crs=4326)
# YStracks.interp.sf.prj<-st_transform(YStracks.interp.sf, center=TRUE)
# 
# ## Creating a buffer around colonies
# YStracks.interp.buff <- data.frame()
# 
# st_over <- function(x, y) {
#   sapply(sf::st_intersects(x, y), function(z)
#     if (length(z) == 0) {
#       NA_integer_
#     } else {
#       z[1]
#     })
# }
# 
# for(i in levels(YStracks.interp.sf.prj$id_col)){
#   sub_col <- YStracks.interp.sf.prj[YStracks.interp.sf.prj$id_col %in% i,]
#   df_col <- data.frame(cbind(lon=sub_col$lon_colony[1], lat=sub_col$lat_colony[1]))
#   df_col <- st_as_sf(df_col, coords = c("lon", "lat"), crs=4326)
#   col_transf <- st_transform(df_col, st_crs(YStracks.interp.sf.prj))
#   buf_col <- st_buffer(col_transf, dist = 5000)
#   YStracks.interp.buff.col <- sub_col[is.na(st_over(sub_col,buf_col)),]
#   YStracks.interp.buff <- rbind(YStracks.interp.buff,data.frame(YStracks.interp.buff.col))
# }
# 
# YStracks.interp.buff$longitude<-YStracks.interp.buff$x
# YStracks.interp.buff$latitude<-YStracks.interp.buff$y
# YStracks.interp.buff.sf<-st_as_sf(YStracks.interp.buff, coords = c("longitude", "latitude"), crs=4326)
# 
# nrow(YStracks.interp)-nrow(YStracks.interp.buff) # 38 937 locations removed, 99 049 remained
# 
# rm(list=ls()[! ls() %in% c("st_over","land","YStracks","YStracks.interp.buff")])

################################################################################
### 8- Removed locations over mainland
################################################################################

# #load precise coastline
# coast<-st_read("C:/Users/gwenael.quaintenne/OneDrive - LPO/LIFE_PanPuffinus/SIG_A3LifePanPuffinus/FondCarto/Land/CoastCutMedit.shp",crs="EPSG:4326",stringsAsFactors =TRUE)
# coast<-st_make_valid(coast)
# 
# YStracks.interp.buff<-st_as_sf(YStracks.interp.buff, coords = c("longitude", "latitude"), crs=4326)
# 
# YStracks.interp.buff.land <- YStracks.interp.buff[is.na(st_over(YStracks.interp.buff,coast)),]
# 
# nrow(YStracks.interp.buff)-nrow(YStracks.interp.buff.land) #2677 locations over land removed (96 372 remained)
# 
# leaflet() %>% 
#   addProviderTiles(providers$Esri.WorldImagery, group = "World Imagery") %>%
#   addCircleMarkers(data = YStracks.interp.buff.land,
#                    radius = 5,
#                    fillColor = "cyan",
#                    fillOpacity = 0.5, stroke = F) %>%
#   addCircleMarkers(data = YStracks.interp.buff[!is.na(st_over(YStracks.interp.buff,coast)),],
#                    radius = 5,
#                    fillColor = "purple",
#                    fillOpacity = 0.5, stroke = F) %>%
#   addLegend(title = "Removed locations over land",
#             colors = c("cyan","purple"),
#             labels = c("keeped locs","removed locs"))
# 
# rm(list=ls()[! ls() %in% c("land","YStracks","YStracks.interp.buff.land")])

################################################################################
### 9- Format cleaned & resampled locations
################################################################################

#add attributes of birds

library(data.table)
BirdStat.day <- setDT(YStracks)[, .(day = seq.Date(min(date_gmt), max(date_gmt), by = 'day')), by = c("dataset_id","track_id","age","sex","breed_stage","breed_status")]
BirdStat.day<-data.frame(BirdStat.day)

BirdStat.day[BirdStat.day$track_id %in% c("PY_FT67737","PY_FT67757"),c("sex")]<-"male"

BirdStat.day<-unique(BirdStat.day[c("dataset_id","track_id","age","sex","breed_stage","breed_status","day")])

duplicates <- BirdStat.day %>% group_by(track_id,day) %>% summarise(NbId=length(sex)) %>% filter(NbId>1) %>% data.frame() %>% droplevels()

levels(duplicates$track_id) #ok no duplicates

YStracks.interp.buff.land$date2<-as.Date(YStracks.interp.buff.land$date)

YStracks.interp.buff.land<-left_join(YStracks.interp.buff.land,BirdStat.day,by=c("id"="track_id","date2"="day"))

nrow(YStracks.interp.buff.land[is.na(YStracks.interp.buff.land$dataset_id),])
###96 rows don't find event

unique(YStracks.interp.buff.land[is.na(YStracks.interp.buff.land$dataset_id),]$id) #"EE04096" 
#ok a gap in a tracks of 4 days between incubation and chick-rearing

#leaflet() %>% 
#  addProviderTiles(providers$Esri.WorldImagery, group = "World Imagery") %>%
#  addCircleMarkers(data = YStracks.interp.buff2[YStracks.interp.buff2$id=="EE04096",],
#                   radius = 5,
#                   fillColor = "cyan",
#                   fillOpacity = 0.5, stroke = F)
#ok removed it

YStracks.interp.buff.land<-YStracks.interp.buff.land[!is.na(YStracks.interp.buff.land$dataset_id),]

names(YStracks.interp.buff.land)

#format final files

YStracks.clean<-YStracks.interp.buff.land[c("dataset_id","site_name","colony_name","lon_colony" ,"lat_colony","device","id","age","sex","breed_stage","breed_status","date","x","y")]

unique(data.frame(YStracks)[c("dataset_id","scientific_name","common_name","country_code","country_name")])

YStracks.clean<-left_join(YStracks.clean, unique(data.frame(YStracks)[c("dataset_id","scientific_name","common_name","country_code","country_name")]))

YStracks.clean<-YStracks.clean[c("dataset_id","scientific_name","common_name","country_code","country_name","site_name","colony_name","lon_colony" ,"lat_colony","device","id","age","sex","breed_stage","breed_status","date","x","y")]

colnames(YStracks.clean)[colnames(YStracks.clean) == "id"] ="track_id"
colnames(YStracks.clean)[colnames(YStracks.clean) == "x"] ="longitude"
colnames(YStracks.clean)[colnames(YStracks.clean) == "y"] ="latitude"

head(YStracks.clean)
nrow(YStracks.clean)

saveRDS(YStracks.clean,"OutputR/YSCleanInterpTracks/InterpolatedGPSTracksYS.rds")

write.table(YStracks.clean,"OutputR/YSCleanInterpTracks/InterpolatedGPSTracksYS.csv",fileEncoding="UTF-8",row.names=FALSE,dec = ".",sep=";",na="")


################################################################################
### 10- Plot it = f(country_name)
################################################################################

rm(list=ls())
setwd("C:/Users/gwenael.quaintenne/OneDrive - LPO/LIFE_PanPuffinus/AnalysisA3")

TracksYS.interp<-readRDS("OutputR/YSCleanInterpTracks/InterpolatedGPSTracksYS.rds")

library(move)
library(raster)
library(sf)
library(lubridate)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(leaflet)

CountryPalette <- colorFactor(palette = c("#164D68","#777B7F","#EDEFE2","#88CCEE","#4E87B6","#B4BCCC"),levels=levels(TracksYS.interp$country_name))

TracksYS.interp %>% group_by(country_name) %>% summarise(NbTracks=n_distinct(track_id))

leaflet() %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "World Imagery") %>%
  addCircleMarkers(data = TracksYS.interp,
                   radius = 5,
                   fillColor = CountryPalette(TracksYS.interp$country_name),
                   fillOpacity = 0.8, stroke = F) %>%
  addLegend(colors = c("#164D68","#777B7F","#EDEFE2","#88CCEE","#4E87B6","#B4BCCC"),
            labels = c("Croatia n=58","France n=18","Greece n=27","Italy n=53","Malta n=264","Tunisia n=13"),
            title = "Yelkouan Shearwater GPS tracks",
            position = "bottomleft")




################################################################################
### 11- Construct kernelUD weighted per colony size
################################################################################

rm(list=ls())

library(raster)
library(sf)
library(lubridate)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(adehabitatHR)
library(viridis)

setwd("C:/Users/gwenael.quaintenne/OneDrive - LPO/LIFE_PanPuffinus/AnalysisA3")

###LOAD CLEANED TRACKS

TracksYS<-readRDS("OutputR/YSCleanInterpTracks/InterpolatedGPSTracksYS.rds") # 94 192 locs
TracksYS$track_id<-factor(TracksYS$track_id)

###CREATE GRID (same as GFW 0.1°Grid)

Grid<-raster(xmn=-5.65, xmx=43.05, ymn=30.55, ymx=47.95, resolution=0.1,crs="EPSG:4326")

#writeRaster(Grid, filename="OutputR/GFWraster/GFW_GRID.tiff", options="INTERLEAVE=BAND", overwrite=TRUE)

###CHECK MONTHLY DISTRIBUTION

TracksYS$month<-month(TracksYS$date)

source("C:/Users/gwenael.quaintenne/OneDrive - LPO/Enquete_AnatidesLimicolesNich/DataLIMAT2021-2022/1-CheckSamplingEffort/SourcesGraph.R")

ggplot(TracksYS, aes(x=month,fill=country_name)) + 
  geom_histogram(stat="count")+
  scale_fill_manual(values=c("#164D68","#777B7F","#EDEFE2","#88CCEE","#4E87B6","#B4BCCC"))+
  ylab("Nb locs") +
  scale_x_continuous(breaks=seq(2, 9, 1), labels=c("Feb","March","Apr","May","June","July","Aug","Sept"))+
  labs(fill="Month")+
  theme_wsj(base_size=12) +
  ggtitle("GPS Locs")

###REMOVED TRACKS WITH TOO FEW LOCATIONS nb locs should be >5 for Kernel, better to have >10
TracksYS<-data.frame(TracksYS)

Sel<-TracksYS %>% group_by(track_id) %>% summarise(NbLocs=length(dataset_id)) %>% filter(NbLocs<10)

TracksYS<-droplevels(TracksYS[!TracksYS$track_id %in% Sel$track_id,])

rm(Sel)

TracksYS.sf<-st_as_sf(TracksYS, coords = c("longitude", "latitude"), crs="EPSG:4326")

#GET COLONY WEIGHTS

XYcolonies<-read.table("C:/Users/gwenael.quaintenne/OneDrive - LPO/LIFE_PanPuffinus/DataLifePanPuffinusA3/BirdsData/ColonySizeData/XYcolonySize_YS.csv", header=TRUE, sep=";", encoding="UTF-8",stringsAsFactors = TRUE)

PopColony<-XYcolonies[XYcolonies$colony_name %in% levels(TracksYS.sf$colony_name),c("colony_name","pop_size_best")]
PopColony<-rbind(PopColony,data.frame(colony_name="Malta",pop_size_best=338.5))
PopColony

#load coast
land<-st_read("C:/Users/gwenael.quaintenne/OneDrive - LPO/LIFE_PanPuffinus/SIG_A3LifePanPuffinus/FondCarto/Land/CNTR_RG_01M_2020_4326.shp",crs="EPSG:4326",stringsAsFactors =TRUE)

land<-st_read("InputR/GIS/CNTR_RG_01M_2020_4326.shp",crs="EPSG:4326",stringsAsFactors =TRUE)

##############################################################################################
###CREATE UTILISATION DISTRIBUTION (UD) PER COLONY FOR ALL SEASON

Tracks.Colony<-list()
KUD.Colony<-list()
stk_KUD.Colony<-list()
sum_all_KUD.Colony<-list()

for (i in levels(TracksYS.sf$colony_name)) {
  Tracks.Colony[[i]]<-as(TracksYS.sf[TracksYS.sf$colony_name %in% i,], "Spatial")
  Tracks.Colony[[i]]@data<-droplevels(Tracks.Colony[[i]]@data)
  KUD.Colony[[i]]<-kernelUD(Tracks.Colony[[i]][, c("track_id")], h=0.2, grid=as(Grid, "SpatialPixels"))
  stk_KUD.Colony[[i]] <-stack(estUDm2spixdf(KUD.Colony[[i]]))
  sum_all_KUD.Colony[[i]] <- overlay(stk_KUD.Colony[[i]], fun = mean)
  sum_all_KUD.Colony[[i]] <- sum_all_KUD.Colony[[i]]/sum(getValues(sum_all_KUD.Colony[[i]]))
}

rm(KUD.Colony,stk_KUD.Colony,Tracks.Colony)

sum(getValues(sum_all_KUD.Colony))

#WEIGTH UDs PER POP SIZE OF COLONIES

TracksYS.sf[!TracksYS.sf$colony_name %in% PopColony$colony_name,] #ok all find colony size

data.frame(TracksYS.sf) %>% group_by(colony_name) %>% summarize(NbTracks=n_distinct(track_id)) %>% data.frame()

KUD.Colony.weigh.season<-list()

for (i in levels(TracksYS.sf$colony_name)) {
  KUD.Colony.weigh.season[[i]]<-sum_all_KUD.Colony[[i]]*(PopColony[PopColony$colony_name %in% i,]$pop_size_best/sum(PopColony$pop_size_best))
}

KUD.Colony.weigh.season <- stack(KUD.Colony.weigh.season)
KUD.Colony.weigh.season <- overlay(KUD.Colony.weigh.season, fun = mean)
KUD.Colony.weigh.season <- KUD.Colony.weigh.season/sum(getValues(KUD.Colony.weigh.season))

sum(getValues(KUD.Colony.weigh.season),na.rm=TRUE)

##############################################################################################
###CREATE UTILISATION DISTRIBUTION (UD) PER COLONY PER MONTH

###MARCH-APRIL
TracksYS.sf.march.apr<-TracksYS.sf[TracksYS.sf$month %in% c(3:4),]

Sel <- data.frame(TracksYS.sf.march.apr) %>% group_by(track_id) %>% summarise(NbLocs=length(dataset_id)) %>% filter(NbLocs<10)
TracksYS.sf.march.apr<-TracksYS.sf.march.apr[!TracksYS.sf.march.apr$track_id %in% Sel$track_id,]
TracksYS.sf.march.apr$colony_name<-droplevels(TracksYS.sf.march.apr$colony_name)
TracksYS.sf.march.apr$track_id<-droplevels(TracksYS.sf.march.apr$track_id)

Tracks.Colony<-list()
KUD.Colony<-list()
stk_KUD.Colony<-list()
sum_all_KUD.Colony<-list()

for (i in levels(TracksYS.sf.march.apr$colony_name)) {
  Tracks.Colony[[i]]<-as(TracksYS.sf.march.apr[TracksYS.sf.march.apr$colony_name %in% i,], "Spatial")
  Tracks.Colony[[i]]@data<-droplevels(Tracks.Colony[[i]]@data)
  KUD.Colony[[i]]<-kernelUD(Tracks.Colony[[i]][, c("track_id")], h=0.2, grid=as(Grid, "SpatialPixels"))
  stk_KUD.Colony[[i]] <- stack(estUDm2spixdf(KUD.Colony[[i]]))
  sum_all_KUD.Colony[[i]] <- overlay(stk_KUD.Colony[[i]], fun = mean)
  sum_all_KUD.Colony[[i]] <- sum_all_KUD.Colony[[i]]/sum(getValues(sum_all_KUD.Colony[[i]]))
}

rm(KUD.Colony,stk_KUD.Colony,Tracks.Colony)

#WEIGTH UDs PER POP SIZE OF COLONIES

TracksYS.sf.march.apr[!TracksYS.sf.march.apr$colony_name %in% PopColony$colony_name,] #ok all find colony size

data.frame(TracksYS.sf.march.apr) %>% group_by(colony_name) %>% summarize(NbTracks=n_distinct(track_id)) %>% data.frame()

KUD.Colony.weigh.march.april<-list()

for (i in levels(TracksYS.sf.march.apr$colony_name)) {
  KUD.Colony.weigh.march.april[[i]]<-sum_all_KUD.Colony[[i]]*(PopColony[PopColony$colony_name %in% i,]$pop_size_best/sum(PopColony$pop_size_best))
}

KUD.Colony.weigh.march.april <- stack(KUD.Colony.weigh.march.april)
KUD.Colony.weigh.march.april <- overlay(KUD.Colony.weigh.march.april, fun = mean)
KUD.Colony.weigh.march.april <- KUD.Colony.weigh.march.april/sum(getValues(KUD.Colony.weigh.march.april))

sum(getValues(KUD.Colony.weigh.march.april))

###MAY-JUNE
TracksYS.sf.may.june<-TracksYS.sf[TracksYS.sf$month %in% c(5:6),]

Sel <- data.frame(TracksYS.sf.may.june) %>% group_by(track_id) %>% summarise(NbLocs=length(dataset_id)) %>% filter(NbLocs<10)
TracksYS.sf.may.june<-TracksYS.sf.may.june[!TracksYS.sf.may.june$track_id %in% Sel$track_id,]
TracksYS.sf.may.june$colony_name<-droplevels(TracksYS.sf.may.june$colony_name)
TracksYS.sf.may.june$track_id<-droplevels(TracksYS.sf.may.june$track_id)

Tracks.Colony<-list()
KUD.Colony<-list()
stk_KUD.Colony<-list()
sum_all_KUD.Colony<-list()

for (i in levels(TracksYS.sf.may.june$colony_name)) {
  Tracks.Colony[[i]]<-as(TracksYS.sf.may.june[TracksYS.sf.may.june$colony_name %in% i,], "Spatial")
  Tracks.Colony[[i]]@data<-droplevels(Tracks.Colony[[i]]@data)
  KUD.Colony[[i]]<-kernelUD(Tracks.Colony[[i]][, c("track_id")], h=0.2, grid=as(Grid, "SpatialPixels"))
  stk_KUD.Colony[[i]] <- stack(estUDm2spixdf(KUD.Colony[[i]]))
  sum_all_KUD.Colony[[i]] <- overlay(stk_KUD.Colony[[i]], fun = mean)
  sum_all_KUD.Colony[[i]] <- sum_all_KUD.Colony[[i]]/sum(getValues(sum_all_KUD.Colony[[i]]))
}

rm(KUD.Colony,stk_KUD.Colony,Tracks.Colony)

#WEIGTH UDs PER POP SIZE OF COLONIES

TracksYS.sf.may.june[!TracksYS.sf.may.june$colony_name %in% PopColony$colony_name,] #ok all find colony size

data.frame(TracksYS.sf.may.june) %>% group_by(colony_name) %>% summarize(NbTracks=n_distinct(track_id)) %>% data.frame()

KUD.Colony.weigh.may.june<-list()

for (i in levels(TracksYS.sf.may.june$colony_name)) {
  KUD.Colony.weigh.may.june[[i]]<-sum_all_KUD.Colony[[i]]*(PopColony[PopColony$colony_name %in% i,]$pop_size_best/sum(PopColony$pop_size_best))
}

KUD.Colony.weigh.may.june <- stack(KUD.Colony.weigh.may.june)
KUD.Colony.weigh.may.june <- overlay(KUD.Colony.weigh.may.june, fun = mean)
KUD.Colony.weigh.may.june <- KUD.Colony.weigh.may.june/sum(getValues(KUD.Colony.weigh.may.june))

sum(getValues(KUD.Colony.weigh.may.june))

###JULY-AUGUST
TracksYS.sf.juil.aug<-TracksYS.sf[TracksYS.sf$month %in% c(7:8),]

Sel <- data.frame(TracksYS.sf.juil.aug) %>% group_by(track_id) %>% summarise(NbLocs=length(dataset_id)) %>% filter(NbLocs<10)
TracksYS.sf.juil.aug<-TracksYS.sf.juil.aug[!TracksYS.sf.juil.aug$track_id %in% Sel$track_id,]
TracksYS.sf.juil.aug$colony_name<-droplevels(TracksYS.sf.juil.aug$colony_name)
TracksYS.sf.juil.aug$track_id<-droplevels(TracksYS.sf.juil.aug$track_id)

Tracks.Colony<-list()
KUD.Colony<-list()
stk_KUD.Colony<-list()
sum_all_KUD.Colony<-list()

for (i in levels(TracksYS.sf.juil.aug$colony_name)) {
  Tracks.Colony[[i]]<-as(TracksYS.sf.juil.aug[TracksYS.sf.juil.aug$colony_name %in% i,], "Spatial")
  Tracks.Colony[[i]]@data<-droplevels(Tracks.Colony[[i]]@data)
  KUD.Colony[[i]]<-kernelUD(Tracks.Colony[[i]][, c("track_id")], h=0.2, grid=as(Grid, "SpatialPixels"))
  stk_KUD.Colony[[i]] <- stack(estUDm2spixdf(KUD.Colony[[i]]))
  sum_all_KUD.Colony[[i]] <- overlay(stk_KUD.Colony[[i]], fun = mean)
  sum_all_KUD.Colony[[i]] <- sum_all_KUD.Colony[[i]]/sum(getValues(sum_all_KUD.Colony[[i]]))
}

rm(KUD.Colony,stk_KUD.Colony,Tracks.Colony)

#WEIGTH UDs PER POP SIZE OF COLONIES

TracksYS.sf.juil.aug[!TracksYS.sf.juil.aug$colony_name %in% PopColony$colony_name,] #ok all find colony size

data.frame(TracksYS.sf.juil.aug) %>% group_by(colony_name) %>% summarize(NbTracks=n_distinct(track_id)) %>% data.frame()

KUD.Colony.weigh.juil.aug<-list()

for (i in levels(TracksYS.sf.juil.aug$colony_name)) {
  KUD.Colony.weigh.juil.aug[[i]]<-sum_all_KUD.Colony[[i]]*(PopColony[PopColony$colony_name %in% i,]$pop_size_best/sum(PopColony$pop_size_best))
}

KUD.Colony.weigh.juil.aug <- stack(KUD.Colony.weigh.juil.aug)
KUD.Colony.weigh.juil.aug <- overlay(KUD.Colony.weigh.juil.aug, fun = mean)
KUD.Colony.weigh.juil.aug <- KUD.Colony.weigh.juil.aug/sum(getValues(KUD.Colony.weigh.juil.aug))

sum(getValues(KUD.Colony.weigh.juil.aug))

################################################################################
### 13- Save UD maps
################################################################################

ls()[sapply(ls(), function(i) class(get(i))) == "RasterLayer"]

writeRaster(KUD.Colony.weigh.season, filename = "OutputR/YSWeightedUD/UDweightColonyKernel_YS_Feb2Sept.tif", format = "GTiff",overwrite=TRUE)

writeRaster(KUD.Colony.weigh.march.april, filename = "OutputR/YSWeightedUD/UDweightColonyKernel_YS_March2April.tif", format = "GTiff",overwrite=TRUE)

writeRaster(KUD.Colony.weigh.may.june, filename = "OutputR/YSWeightedUD/UDweightColonyKernel_YS_May2June.tif", format = "GTiff",overwrite=TRUE)

writeRaster(KUD.Colony.weigh.juil.aug, filename = "OutputR/YSWeightedUD/UDweightColonyKernel_YS_Jul2Aug.tif", format = "GTiff",overwrite=TRUE)


################################################################################
### 14- Construct kernelUD weighted per country size
################################################################################

rm(list=ls())

library(raster)
library(sf)
library(lubridate)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(adehabitatHR)
library(viridis)

setwd("C:/Users/gwenael.quaintenne/OneDrive - LPO/LIFE_PanPuffinus/AnalysisA3")

###LOAD CLEANED TRACKS

length(unique(TracksYS$track_id))
TracksYS<-readRDS("OutputR/YSCleanInterpTracks/InterpolatedGPSTracksYS.rds") # 96 276 locs
TracksYS$month<-month(TracksYS$date)
TracksYS$track_id<-factor(TracksYS$track_id)

###CREATE GRID (same as GFW 0.1°Grid)

Grid<-raster(xmn=-5.65, xmx=43.05, ymn=30.55, ymx=47.95, resolution=0.1,crs="EPSG:4326")

#writeRaster(Grid, filename="OutputR/GFWraster/GFW_GRID.tiff", options="INTERLEAVE=BAND", overwrite=TRUE)

###REMOVED TRACKS WITH TOO FEW LOCATIONS nb locs should be >5 for Kernel, better to have >10
TracksYS<-data.frame(TracksYS)

Sel<-TracksYS %>% group_by(track_id) %>% summarise(NbLocs=length(dataset_id)) %>% filter(NbLocs<10)

TracksYS<-droplevels(TracksYS[!TracksYS$track_id %in% Sel$track_id,])

rm(Sel)

TracksYS.sf<-st_as_sf(TracksYS, coords = c("longitude", "latitude"), crs="EPSG:4326")

TracksYS.sf$month<-month(TracksYS.sf$date)

###CREATE WEIGHT

XYcolonies<-read.table("C:/Users/gwenael.quaintenne/OneDrive - LPO/LIFE_PanPuffinus/DataLifePanPuffinusA3/BirdsData/ColonySizeData/XYcolonySize_YS.csv", header=TRUE, sep=";", encoding="UTF-8",stringsAsFactors = TRUE)
COUNTRY<-XYcolonies %>% group_by(Country_code,Country_name) %>% summarise(MinPop=sum(pop_size_min,na.rm = TRUE),MaxPop=sum(pop_size_max,na.rm = TRUE),pop_size_best=sum(pop_size_best,na.rm=TRUE)) %>% data.frame()

data.frame(TracksYS.sf) %>% group_by(country_name) %>% summarize(NbTracks=n_distinct(track_id)) %>% data.frame()

##LOAD COAST
land<-st_read("C:/Users/gwenael.quaintenne/OneDrive - LPO/LIFE_PanPuffinus/SIG_A3LifePanPuffinus/FondCarto/Land/CNTR_RG_01M_2020_4326.shp",crs="EPSG:4326",stringsAsFactors =TRUE)

##############################################################################################
###CREATE UTILISATION DISTRIBUTION (UD) PER COUNTRY FOR ALL SEASON

Tracks.Country<-list()
KUD.Country<-list()
stk_KUD.Country<-list()
sum_all_KUD.Country<-list()

for (i in levels(TracksYS.sf$country_code)) {
  Tracks.Country[[i]]<-as(TracksYS.sf[TracksYS.sf$country_code %in% i,], "Spatial")
  Tracks.Country[[i]]@data<-droplevels(Tracks.Country[[i]]@data)
  KUD.Country[[i]]<-kernelUD(Tracks.Country[[i]][, c("track_id")], h=0.2, grid=as(Grid, "SpatialPixels"))
  stk_KUD.Country[[i]] <- stack(estUDm2spixdf(KUD.Country[[i]]))
  sum_all_KUD.Country[[i]] <- overlay(stk_KUD.Country[[i]], fun = mean)
  sum_all_KUD.Country[[i]] <- sum_all_KUD.Country[[i]]/sum(getValues(sum_all_KUD.Country[[i]]))
}

rm(KUD.Country,stk_KUD.Country,Tracks.Country)

#WEIGTH UDs PER POP SIZE IN COUNTRIES

data.frame(TracksYS.sf) %>% group_by(country_code,country_name) %>% summarise(NbTracks=n_distinct(track_id))

KUD.Country.weigh.season<-list()

for (i in names(sum_all_KUD.Country)) {
  KUD.Country.weigh.season[[i]]<-sum_all_KUD.Country[[i]]*(COUNTRY[COUNTRY$Country_code %in% i,]$pop_size_best/sum(COUNTRY[COUNTRY$Country_code %in% names(sum_all_KUD.Country),]$pop_size_best))
}

KUD.Country.weigh.season <- stack(KUD.Country.weigh.season)
KUD.Country.weigh.season <- overlay(KUD.Country.weigh.season, fun = mean)
KUD.Country.weigh.season <- KUD.Country.weigh.season/sum(getValues(KUD.Country.weigh.season))

sum(getValues(KUD.Country.weigh.season))

##############################################################################################
###CREATE UTILISATION DISTRIBUTION (UD) PER COLONY PER MONTH

###MARCH-APRIL
TracksYS.sf.march.apr<-TracksYS.sf[TracksYS.sf$month %in% c(3:4),]

Sel <- data.frame(TracksYS.sf.march.apr) %>% group_by(track_id) %>% summarise(NbLocs=length(dataset_id)) %>% filter(NbLocs<10)
TracksYS.sf.march.apr<-TracksYS.sf.march.apr[!TracksYS.sf.march.apr$track_id %in% Sel$track_id,]
TracksYS.sf.march.apr$country_code<-droplevels(TracksYS.sf.march.apr$country_code)
TracksYS.sf.march.apr$track_id<-droplevels(TracksYS.sf.march.apr$track_id)

Tracks.Country<-list()
KUD.Country<-list()
stk_KUD.Country<-list()
sum_all_KUD.Country<-list()

for (i in levels(TracksYS.sf.march.apr$country_code)) {
  Tracks.Country[[i]]<-as(TracksYS.sf.march.apr[TracksYS.sf.march.apr$country_code %in% i,], "Spatial")
  Tracks.Country[[i]]@data<-droplevels(Tracks.Country[[i]]@data)
  KUD.Country[[i]]<-kernelUD(Tracks.Country[[i]][, c("track_id")], h=0.2, grid=as(Grid, "SpatialPixels"))
  stk_KUD.Country[[i]] <- stack(estUDm2spixdf(KUD.Country[[i]]))
  sum_all_KUD.Country[[i]] <- overlay(stk_KUD.Country[[i]], fun = mean)
  sum_all_KUD.Country[[i]] <- sum_all_KUD.Country[[i]]/sum(getValues(sum_all_KUD.Country[[i]]))
}

rm(KUD.Country,stk_KUD.Country,Tracks.Country)

#WEIGTH UDs 

TracksYS.sf.march.apr[!TracksYS.sf.march.apr$country_code %in% COUNTRY$Country_code,] #ok all find colony size

data.frame(TracksYS.sf.march.apr) %>% group_by(country_code,country_name) %>% summarize(NbTracks=n_distinct(track_id)) %>% data.frame()

KUD.Country.weigh.march.april<-list()

for (i in names(sum_all_KUD.Country)) {
  KUD.Country.weigh.march.april[[i]]<-sum_all_KUD.Country[[i]]*(COUNTRY[COUNTRY$Country_code %in% i,]$pop_size_best/sum(COUNTRY[COUNTRY$Country_code %in% names(sum_all_KUD.Country),]$pop_size_best))
}

KUD.Country.weigh.march.april <- stack(KUD.Country.weigh.march.april)
KUD.Country.weigh.march.april <- overlay(KUD.Country.weigh.march.april, fun = mean)
KUD.Country.weigh.march.april <- KUD.Country.weigh.march.april/sum(getValues(KUD.Country.weigh.march.april))

sum(getValues(KUD.Country.weigh.march.april))

###MAY-JUNE
TracksYS.sf.may.june<-TracksYS.sf[TracksYS.sf$month %in% c(5:6),]

Sel <- data.frame(TracksYS.sf.may.june) %>% group_by(track_id) %>% summarise(NbLocs=length(dataset_id)) %>% filter(NbLocs<10)
TracksYS.sf.may.june<-TracksYS.sf.may.june[!TracksYS.sf.may.june$track_id %in% Sel$track_id,]
TracksYS.sf.may.june$country_code<-droplevels(TracksYS.sf.may.june$country_code)
TracksYS.sf.may.june$track_id<-droplevels(TracksYS.sf.may.june$track_id)

Tracks.Country<-list()
KUD.Country<-list()
stk_KUD.Country<-list()
sum_all_KUD.Country<-list()

for (i in levels(TracksYS.sf.may.june$country_code)) {
  Tracks.Country[[i]]<-as(TracksYS.sf.may.june[TracksYS.sf.may.june$country_code %in% i,], "Spatial")
  Tracks.Country[[i]]@data<-droplevels(Tracks.Country[[i]]@data)
  KUD.Country[[i]]<-kernelUD(Tracks.Country[[i]][, c("track_id")], h=0.2, grid=as(Grid, "SpatialPixels"))
  stk_KUD.Country[[i]] <- stack(estUDm2spixdf(KUD.Country[[i]]))
  sum_all_KUD.Country[[i]] <- overlay(stk_KUD.Country[[i]], fun = mean)
  sum_all_KUD.Country[[i]] <- sum_all_KUD.Country[[i]]/sum(getValues(sum_all_KUD.Country[[i]]))
}

rm(KUD.Country,stk_KUD.Country,Tracks.Country)

#WEIGTH UDs

TracksYS.sf.may.june[!TracksYS.sf.may.june$country_code %in% COUNTRY$Country_code,] #ok all find country pop size

data.frame(TracksYS.sf.may.june) %>% group_by(country_code) %>% summarize(NbTracks=n_distinct(track_id)) %>% data.frame()

KUD.Country.weigh.may.june<-list()

for (i in names(sum_all_KUD.Country)) {
  KUD.Country.weigh.may.june[[i]]<-sum_all_KUD.Country[[i]]*(COUNTRY[COUNTRY$Country_code %in% i,]$pop_size_best/sum(COUNTRY[COUNTRY$Country_code %in% names(sum_all_KUD.Country),]$pop_size_best))
}

KUD.Country.weigh.may.june <- stack(KUD.Country.weigh.may.june)
KUD.Country.weigh.may.june <- overlay(KUD.Country.weigh.may.june, fun = mean)
KUD.Country.weigh.may.june <- KUD.Country.weigh.may.june/sum(getValues(KUD.Country.weigh.may.june))

sum(getValues(KUD.Country.weigh.may.june))

###JULY-AUGUST
TracksYS.sf.july.aug<-TracksYS.sf[TracksYS.sf$month %in% c(7:8),]

Sel <- data.frame(TracksYS.sf.july.aug) %>% group_by(track_id) %>% summarise(NbLocs=length(dataset_id)) %>% filter(NbLocs<10)
TracksYS.sf.july.aug<-TracksYS.sf.july.aug[!TracksYS.sf.july.aug$track_id %in% Sel$track_id,]
TracksYS.sf.july.aug$country_code<-droplevels(TracksYS.sf.july.aug$country_code)
TracksYS.sf.july.aug$track_id<-droplevels(TracksYS.sf.july.aug$track_id)

Tracks.Country<-list()
KUD.Country<-list()
stk_KUD.Country<-list()
sum_all_KUD.Country<-list()

for (i in levels(TracksYS.sf.july.aug$country_code)) {
  Tracks.Country[[i]]<-as(TracksYS.sf.july.aug[TracksYS.sf.july.aug$country_code %in% i,], "Spatial")
  Tracks.Country[[i]]@data<-droplevels(Tracks.Country[[i]]@data)
  KUD.Country[[i]]<-kernelUD(Tracks.Country[[i]][, c("track_id")], h=0.2, grid=as(Grid, "SpatialPixels"))
  stk_KUD.Country[[i]] <- stack(estUDm2spixdf(KUD.Country[[i]]))
  sum_all_KUD.Country[[i]] <- overlay(stk_KUD.Country[[i]], fun = mean)
  sum_all_KUD.Country[[i]] <- sum_all_KUD.Country[[i]]/sum(getValues(sum_all_KUD.Country[[i]]))
}

rm(KUD.Country,stk_KUD.Country,Tracks.Country)

#WEIGTH UDs

TracksYS.sf.july.aug[!TracksYS.sf.july.aug$country_code %in% COUNTRY$Country_code,] #ok all find country pop size

data.frame(TracksYS.sf.july.aug) %>% group_by(country_code) %>% summarize(NbTracks=n_distinct(track_id)) %>% data.frame()

KUD.Country.weigh.july.aug<-list()

for (i in names(sum_all_KUD.Country)) {
  KUD.Country.weigh.july.aug[[i]]<-sum_all_KUD.Country[[i]]*(COUNTRY[COUNTRY$Country_code %in% i,]$pop_size_best/sum(COUNTRY[COUNTRY$Country_code %in% names(sum_all_KUD.Country),]$pop_size_best))
}

KUD.Country.weigh.july.aug <- stack(KUD.Country.weigh.july.aug)
KUD.Country.weigh.july.aug <- overlay(KUD.Country.weigh.july.aug, fun = mean)
KUD.Country.weigh.july.aug <- KUD.Country.weigh.july.aug/sum(getValues(KUD.Country.weigh.july.aug))

sum(getValues(KUD.Country.weigh.july.aug))

################################################################################
### 15- Save UD maps
################################################################################

ls()[sapply(ls(), function(i) class(get(i))) == "RasterLayer"]

writeRaster(KUD.Country.weigh.season, filename = "OutputR/YSWeightedUD/UDweightCountryKernel_YS_Feb2Sept.tif", format = "GTiff",overwrite=TRUE)

writeRaster(KUD.Country.weigh.march.april, filename = "OutputR/YSWeightedUD/UDweightCountryKernel_YS_March2April.tif", format = "GTiff",overwrite=TRUE)

writeRaster(KUD.Country.weigh.may.june, filename = "OutputR/YSWeightedUD/UDweightCountryKernel_YS_May2June.tif", format = "GTiff",overwrite=TRUE)

writeRaster(KUD.Country.weigh.july.aug, filename = "OutputR/YSWeightedUD/UDweightCountryKernel_YS_Jul2Aug.tif", format = "GTiff",overwrite=TRUE)

################################################################################
### 16- Arrange map output
################################################################################

rm(list=ls())
setwd("C:/Users/gwenael.quaintenne/OneDrive - LPO/LIFE_PanPuffinus/AnalysisA3")

library(dplyr)
library(tidyr)
library(lubridate)
library(sf)
library(ggplot2)
library(classInt)
library(extrafont)
library(ggOceanMaps)
library(remotes)
library(leaflet)
library(trip)
library(adehabitatLT)
library(extrafont)
library(ggthemes)
library(raster)
library(graticule)
loadfonts(device = "win")

land<-st_read("InputR/GIS/CNTR_RG_01M_2020_4326.shp",crs="EPSG:4326",stringsAsFactors =TRUE)

bathy<-st_read("InputR/GIS/med_batim_latlong_100m.shp",crs="EPSG:4326",stringsAsFactors =TRUE)

bathy<-bathy[bathy$NAME %in% c("-5000 m","-4500 m","-4000 m","-3500 m","-3000 m","-2500 m","-2000 m","-1500 m","-1000 m","-500 m"),]

UD.season<-raster("OutputR/YSWeightedUD/UDweightCountryKernel_YS_Feb2Sept.tif")
UD.season<-aggregate(UD.season, fact=5,fun=sum)
values(UD.season)[values(UD.season)<0.0001] = NA

XYcolonies<-read.table("C:/Users/gwenael.quaintenne/OneDrive - LPO/LIFE_PanPuffinus/DataLifePanPuffinusA3/BirdsData/ColonySizeData/XYcolonySize_YS.csv", header=TRUE, sep=";", encoding="UTF-8",stringsAsFactors = TRUE)

TracksYS<-readRDS("InputR/YStracks/AllTracksYS.rds")
XYcolonies<-XYcolonies %>% mutate(TrackSample=ifelse(XYcolonies$colony_name %in% levels(TracksYS$colony_name),TRUE,FALSE)) %>% data.frame()


XYcolonies<-st_as_sf(XYcolonies, coords = c("XlongDD", "YlatDD"), crs="EPSG:4326")

my_col<-c('#ffff80ff','#8ff041ff','#3dd13bff','#2fa190ff','#215896ff','#0d1178ff')

#theme_set(theme_map() + theme(text = element_text(family = "Source Sans Pro")))
par(mar=c(5.5,2,2,2),oma=c(2,1,1,1))
grat <- graticule(lons = seq(0, 45, by = 5), lats = c(30, 35, 40, 45,50))

jpeg(file="OutputR/YSWeightedUD/UDmed_YS-Feb2Sept.jpg", res=600, width=10.917, height=7.708, units="in", pointsize=12,type="cairo")
par(mar=c(6,2,2,2),oma=c(1,1,1,1))
plot(st_geometry(bathy),col="#e2e2e2",
     main = "Yelkouan Shearwater Utilisation Distribution Febr.-Sept.",
     axes=TRUE,
     xlim = extent(rasterToPolygons(UD.season))[1:2],
     ylim = extent(rasterToPolygons(UD.season))[3:4])
plot(grat, add = TRUE, border="#e2e2e2",lwd=0.5,lty=4)
plot(UD.season,
     col=my_col,
     add=TRUE,
     axes=TRUE,
     fill_alpha=0.8,
     xlim = extent(rasterToPolygons(UD.season))[1:2],
     ylim = extent(rasterToPolygons(UD.season))[3:4],
     horizontal = TRUE)
plot(st_geometry(land),col="#e2e2e2",border="#6e6e6eff",
     add = TRUE,
     axes = TRUE,
     xlim = extent(rasterToPolygons(UD.season))[1,2], 
     ylim = extent(rasterToPolygons(UD.season))[2,4],
     extent=extent(UD.season))
plot(rasterToPolygons(UD.season),
     add=TRUE,
     border="#6e6e6eff",
     lwd=1,
     axes=TRUE,
     xlim = extent(rasterToPolygons(UD.season))[1,2], 
     ylim = extent(rasterToPolygons(UD.season))[2,4])
plot(st_geometry(XYcolonies),col=c("black","red")[factor(XYcolonies$TrackSample)],cex=c(0.5,1)[factor(XYcolonies$TrackSample)],add=TRUE,pch=19)
legend("bottomleft", legend=c("Sampled colony", "Unsampled colony"),col=c("red","black"), pt.cex=c(1,0.5),cex=0.8,pch=19,bty = "n")
box(col="black")
dev.off()

###Now loop it per for two months period

FilesList<-data.frame(Files=list.files("C:/Users/gwenael.quaintenne/OneDrive - LPO/LIFE_PanPuffinus/AnalysisA3/OutputR/YSWeightedUD",pattern=".tif",all.files=TRUE,full.names=FALSE,recursive=TRUE),ID=c(1:length(list.files("C:/Users/gwenael.quaintenne/OneDrive - LPO/LIFE_PanPuffinus/AnalysisA3/OutputR/YSWeightedUD",pattern=".tif",recursive=TRUE))))

FilesList<-FilesList[!grepl("Colony",FilesList$Files),]
FilesList$Period<-factor(c("Feb.-Sept.","July-Aug.","March-April","May-June"))
FilesList<-droplevels(FilesList[!FilesList$Period %in% c("Feb.-Sept."),])

TracksYS<-readRDS("InputR/YStracks/AllTracksYS.rds")
TracksYS<-TracksYS[TracksYS$device %in% c("GPS"),]
TracksYS$month<-month(TracksYS$date)
TracksYS$track_id<-factor(TracksYS$track_id)

st_drop_geometry(TracksYS) %>% group_by(month,country_code,country_name) %>% summarise(NbTracks=n_distinct(track_id)) %>% data.frame()

TracksYS<- TracksYS %>% mutate(Period=ifelse(month(date) %in% c(7:8),"July-Aug.",ifelse(month(date) %in% c(3:4),"March-April",ifelse(month(date) %in% c(5:6),"May-June",NA)))) 

NbTracksMonth <- st_drop_geometry(TracksYS) %>% group_by(colony_name,Period) %>% summarise(NbTracks=n_distinct(track_id)) %>% data.frame()

NbTracksMonth <- left_join(expand.grid(Period=levels(FilesList$Period),colony_name=levels(XYcolonies$colony_name)),NbTracksMonth,by=c("colony_name","Period")) %>% mutate(NbTracks = replace_na(NbTracks, 0))

NbTracksMonth<-left_join(NbTracksMonth,XYcolonies,by=c("colony_name"))  
NbTracksMonth$TrackSample<-ifelse(NbTracksMonth$NbTracks>0,TRUE,FALSE)
NbTracksMonth<-st_as_sf(NbTracksMonth, crs="EPSG:4326")
st_geometry(NbTracksMonth)[NbTracksMonth$colony_name %in% c("Malta")] <- st_point(c(14.35679,35.97265))

par(mar=c(5.5,2,2,2),oma=c(2,1,1,1))
grat <- graticule(lons = seq(0, 45, by = 5), lats = c(30, 35, 40, 45,50))

for (i in unique(NbTracksMonth$Period)) {
  UD<-raster(paste0("OutputR/YSWeightedUD/",FilesList[FilesList$Period %in% i,]$Files))
  UD<-aggregate(UD, fact=5,fun=sum)
  NbTracksMonth.i<-NbTracksMonth[NbTracksMonth$Period %in% i,]
  values(UD)[values(UD)==0] = NA
  jpeg(file=paste0("OutputR/YSWeightedUD/UDmed_YS-",i,".jpg"), res=600, width=10.917, height=7.708, units="in", pointsize=12,type="cairo")
  par(mar=c(6,2,2,2),oma=c(1,1,1,1))
  plot(st_geometry(bathy),col="#e2e2e2",
       main = paste0("Yelkouan Shearwater Utilisation Distribution ", i),
       axes=TRUE,
       xlim = extent(rasterToPolygons(UD.season))[1:2],
       ylim = extent(rasterToPolygons(UD.season))[3:4])
  plot(grat, add = TRUE, border="#e2e2e2",lwd=0.5,lty=4)
  plot(UD,
       col=my_col,
       add=TRUE,
       axes=TRUE,
       fill_alpha=0.8,
       xlim = extent(rasterToPolygons(UD.season))[1:2],
       ylim = extent(rasterToPolygons(UD.season))[3:4],
       horizontal = TRUE)
  plot(st_geometry(land),col="#e2e2e2",border="#6e6e6eff",
       add = TRUE,
       axes = TRUE,
       xlim = extent(rasterToPolygons(UD.season))[1,2], 
       ylim = extent(rasterToPolygons(UD.season))[2,4],
       extent=extent(UD.season))
  plot(rasterToPolygons(UD),
       add=TRUE,
       border="#6e6e6eff",
       lwd=1,
       axes=TRUE,
       xlim = extent(rasterToPolygons(UD.season))[1,2], 
       ylim = extent(rasterToPolygons(UD.season))[2,4])
  plot(st_geometry(NbTracksMonth.i),col=c("black","red")[factor(NbTracksMonth.i$TrackSample)],cex=c(0.5,1)[factor(NbTracksMonth.i$TrackSample)],add=TRUE,pch=19)
  legend("bottomleft", legend=c("Sampled colony", "Unsampled colony"),col=c("red","black"), pt.cex=c(1,0.5),cex=0.8,pch=19,bty = "n")
  box(col="black")
  dev.off()
}

