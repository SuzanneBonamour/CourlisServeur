
# Run la partie "starting blok", 
# puis seulement runner la dernière partie "SAVE" avant la partie à souhaitée / en cours de travail

# STARTING BLOCK ---------------------------------------------------------------

# beep lorsqu'il y a une erreur 
# options(error = function() {beep(7)})
options(error = NULL)

# Nettoyage de l'environnement
rm(list=ls()) 

# time zone
library(lubridate)
with_tz(Sys.time(), "Europe/Paris")

## Packages --------------------------------------------------------------------

library(dplyr)
library(tidyr)
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
library(data.table)
library(stringi)
library(terra)
library(tmap)
library(spData)
library(adehabitatHR)
library(rlist)
library(viridis)
library(beepr)
library(sp)

## Chemins de données ----------------------------------------------------------

data_path_serveur <- "D:/Projets_Suzanne/Courlis/Data/1) data/"
data_generated_path_serveur <- "D:/Projets_Suzanne/Courlis/Data/2) data_generated/"
data_image_path_serveur <- "D:/Projets_Suzanne/Courlis/Data/3) images/"

## Zone d'intérêt (box) --------------------------------------------------------

# BOX <- st_as_sf(st_as_sfc(st_bbox(c(xmin = -1.26, xmax = -0.945, ymax = 46.01, ymin = 45.78), crs = st_crs(4326)))) # Définition d'une boîte englobante avec des coordonnées spécifiques
# st_write(BOX, paste0(data_generated_path_serveur, "BOX.gpkg"), append = FALSE) # Sauvegarde de la boîte dans un fichier GeoPackage
BOX <- st_read(paste0(data_generated_path_serveur, "BOX.gpkg")) # Lecture de la boîte depuis le fichier sauvegardé
BOX_4326 <- st_transform(BOX, crs = 4326) # Transformation de la boîte au CRS 4326 (coordonnées géographiques)
BOX_2154 <- st_transform(BOX, crs = 2154) # Transformation de la boîte au CRS 2154 (coordonnées géographiques)

###
####
## Font de carte ---------------------------------------------------------------
####
###

# Departement ---
dept <- st_read(paste0(data_path_serveur, "departements.gpkg"), layer = "contourdesdepartements") # Lecture du fichier des départements
dept_BOX <- st_intersection(dept, BOX_4326) # Intersection des départements avec une boîte de délimitation (BOX_4326)
rm(dept) # Suppression pour libérer de la mémoire

# Réserve ---
reserve <- st_read(paste0(data_path_serveur, "Réserve_naturelle/rnn/rnn/N_ENP_RNN_S_000.shp")) # Lecture du fichier shapefile des réserves naturelles
RMO <- reserve[reserve$NOM_SITE == "Moëze-Oléron", ] # Filtrage pour ne garder que la réserve "Moëze-Oléron"
rm(reserve) # Suppression pour libérer de la mémoire

###
####
# GPS DATA to load -------------------------------------------------------------
####
###

GPS <- st_read(file.path(data_generated_path_serveur, "behaviour_24h_BOX_1000_56_sex_age_breche.gpkg"))
# all_box <- st_read(paste0(data_generated_path, "all_box.gpkg"))
# points <- st_read(paste0(data_generated_path_serveur, "all_box_1000_56.gpkg"))
# points <- st_read(paste0(data_generated_path_serveur, "behaviour_24h_box_1000_56.gpkg"))
# GPS_2 <- st_read(paste0(data_generated_path_serveur, "behaviour_24h_box_1000_56_sex_age.gpkg"))

###
####
# GRID 100x100 m ---------------------------------------------------------------
####
###

# INPN grille ---
grid <- st_read(paste0(data_path_serveur, "INPN_grid/METROP_L932X2.shp"))
grid_crop <- st_crop(grid, BOX_2154)

# 100*100m grid ---
# offset_point <- st_bbox(grid[grid$CD_SIG=="2kmL93E370N6526",])[c("xmin", "ymin")] - c(2000 * 0, 0) ; offset_point
offset_point <- st_bbox(grid[grid$CD_SIG=="2kmL93E370N6528",])[c("xmin", "ymin")] ; offset_point
grid_100x100 <- st_make_grid(BOX_2154, cellsize = 100, offset = offset_point)
# save grid de 100m x 100m, alignée dans les grilles INPN
st_write(grid_100x100, paste0(data_generated_path_serveur, "grid_100x100.gpkg"), append = FALSE)
grid_100x100 <- st_read(paste0(data_generated_path_serveur, "grid_100x100.gpkg"))

# tmap_mode("view")
# grid_map <- tm_scale_bar() +
#   tm_shape(grid_100x100) +
#   tm_polygons(col = "red", alpha = 0.3) +
#   tm_shape(grid_crop) +
#   tm_polygons(alpha = 0.3, col = "green") +
#   tm_shape(BOX_2154) +
#   tm_borders(col = "yellow"); grid_map

raster_100x100 <- raster(grid_100x100, resolution=100, crs="EPSG:2154")

###
####
# UDmaps -----------------------------------------------------------------------
####
###

# Methode de Silverman + écart interquantile (IQR)

## REPOSOIRS -------------------------------------------------------------------

### global ----

# Charger les données en lat/lon (EPSG:4326)
coords_reposoir <- GPS %>% 
  filter(behavior == "roosting") %>% 
  dplyr::select(lon,lat) %>% 
  st_drop_geometry() %>% 
  na.omit()

# Transformer en objet spatial (EPSG:4326)
locs_reposoir <- st_as_sf(coords_reposoir, coords = c("lon", "lat"), crs = 4326)

# Reprojeter en système métrique (ex. UTM zone 30N - EPSG:32630 pour la France)
locs_reposoir_32630 <- st_transform(locs_reposoir, crs = 32630)  # Adapter le CRS à votre région

# Reprojection du raster
crs_utm <- CRS("+init=epsg:32630") # Définir le CRS cible (EPSG:32630 = UTM zone 30N)
raster_100x100_32630 <- projectRaster(raster_100x100, crs = crs_utm)
crs(raster_100x100_32630) # Vérifier le CRS

# Extraire les coordonnées reprojetées
coords_reposoir_32630 <- st_coordinates(locs_reposoir_32630)

# Règle de Silverman
sigma_x_reposoir <- sd(coords_reposoir_32630[,1])  # Écart-type en X (mètres)
sigma_y_reposoir <- sd(coords_reposoir_32630[,2])  # Écart-type en Y (mètres)
n_reposoir <- nrow(coords_reposoir)  # Nombre de points

h_silverman_x_reposoir <- 1.06 * sigma_x_reposoir * n_reposoir^(-1/5)
h_silverman_y_reposoir <- 1.06 * sigma_y_reposoir * n_reposoir^(-1/5)

cat("h optimal en mètres pour X:", h_silverman_x_reposoir, "\n")
cat("h optimal en mètres pour Y:", h_silverman_y_reposoir, "\n")

# locs_spa <- st_transform(locs, crs = 32630)
locs_spa_reposoir <- as(locs_reposoir_32630, "Spatial")

# Appliquer kernelUD avec h estimé par Silverman
kud_reposoir <- kernelUD(locs_spa_reposoir, grid = as(raster_100x100_32630, "SpatialPixels"),
                h = mean(c(h_silverman_x_reposoir, h_silverman_y_reposoir)))

# Visualiser la densité de noyau
# par(mfrow = c(1, 1))
# image(kud)

# Estimation des isoclines 
rast_reposoir <- rast(kud_reposoir)
courtour_reposoir <- as.contour(rast_reposoir)
sf_reposoir <- st_as_sf(courtour_reposoir)
cast_reposoir <- st_cast(sf_reposoir, "POLYGON")

# plot
tmap_mode("view")
UDMap_reposoir <- tm_scalebar() +
  tm_shape(RMO) +
  tm_polygons() +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(cast_reposoir) + 
  tm_polygons(border.col = "grey", fill = "level", fill_alpha = 0.2, 
              palette = viridis(10, begin = 0, end = 1, 
                                direction = 1, option = "plasma")); UDMap_reposoir

tmap_save(UDMap_reposoir, paste0(data_image_path_serveur, "/UDMap_reposoir.html"), dpi = 600)

### id ----

# Charger les données en lat/lon (EPSG:4326)
coords_reposoir_id <- GPS %>% 
  filter(behavior == "roosting") %>% 
  dplyr::select(id,lon,lat) %>% 
  st_drop_geometry() %>% 
  na.omit()

# Transformer en objet spatial (EPSG:4326)
locs_reposoir_id <- st_as_sf(coords_reposoir_id, coords = c("lon", "lat"), crs = 4326)

# Reprojeter en système métrique (ex. UTM zone 30N - EPSG:32630 pour la France)
locs_reposoir_id_32630 <- st_transform(locs_reposoir_id, crs = 32630)  # Adapter le CRS à votre région

# Reprojection du raster
crs_utm <- CRS("+init=epsg:32630") # Définir le CRS cible (EPSG:32630 = UTM zone 30N)
raster_100x100_32630 <- projectRaster(raster_100x100, crs = crs_utm)
crs(raster_100x100_32630)

# Extraire les coordonnées reprojetées
coords_reposoir_id_32630 <- st_coordinates(locs_reposoir_id_32630)

# Règle de Silverman
sigma_x_reposoir_id <- sd(coords_reposoir_id_32630[,1])  # Écart-type en X (mètres)
sigma_y_reposoir_id <- sd(coords_reposoir_id_32630[,2])  # Écart-type en Y (mètres)
n_reposoir_id <- nrow(coords_reposoir_id_32630)  # Nombre de points

h_silverman_x_reposoir_id <- 1.06 * sigma_x_reposoir_id * n_reposoir_id^(-1/5)
h_silverman_y_reposoir_id <- 1.06 * sigma_y_reposoir_id * n_reposoir_id^(-1/5)

cat("h optimal en mètres pour X:", h_silverman_x_reposoir_id, "\n")
cat("h optimal en mètres pour Y:", h_silverman_y_reposoir_id, "\n")

# locs_spa <- as(locs_m, "Spatial")

# locs_spa <- st_transform(locs_reposoir_id, crs = 32630)
locs_spa_reposoir_id <- as(locs_reposoir_id_32630, "Spatial")

# Appliquer kernelUD avec h estimé par Silverman
kud_reposoir_id <- kernelUD(locs_spa_reposoir_id["id"], grid = as(raster_100x100_32630, "SpatialPixels"),
                   h = mean(c(h_silverman_x_reposoir_id, h_silverman_y_reposoir_id)))

# Visualiser la densité de noyau
par(mfrow = c(1, 1))
image(kud_reposoir_id)

# Créer une liste pour stocker les résultats
UDmaps_list_reposoir_id <- lapply(names(kud_reposoir_id), function(id) {
  
  print(id)
  
  # Extraire l'estimation de densité pour un ID spécifique
  kud_single_reposoir_id <- kud_reposoir_id[[id]]
  rast_reposoir_id <- rast(kud_single_reposoir_id)
  contour_reposoir_id <- as.contour(rast_reposoir_id)
  sf_reposoir_id <- st_as_sf(contour_reposoir_id)
  cast_reposoir_id <- st_cast(sf_reposoir_id, "POLYGON")
  cast_reposoir_id$id <- id
  
  return(cast_reposoir_id)
})

# Fusionner tous les ID dans un seul objet sf
UDMap_final_reposoir_id <- do.call(rbind, UDmaps_list_reposoir_id)

UDMap_final_reposoir_id$id <- as.factor(UDMap_final_reposoir_id$id)

# write
st_write(UDMap_final_reposoir_id, paste0(data_generated_path_serveur, "UDMap_final_reposoir_id.gpkg"), append = FALSE)
# read
UDMap_final_reposoir_id <- st_read(file.path(data_generated_path_serveur, "UDMap_final_reposoir_id.gpkg"))

# groupe plot
id_list <- unique(UDMap_final_reposoir_id$id)
id_gp_1 <- id_list[1:15]
id_gp_2 <- id_list[16:30]
id_gp_3 <- id_list[31:45]
id_gp_4 <- id_list[46:69]

UDMap_final_reposoir_id_gp1 <- UDMap_final_reposoir_id %>% 
  filter(id %in% id_gp_1)
UDMap_final_reposoir_id_gp1$id <- droplevels(UDMap_final_reposoir_id_gp1$id)
UDMap_final_reposoir_id_gp2 <- UDMap_final_reposoir_id %>% 
  filter(id %in% id_gp_2)
UDMap_final_reposoir_id_gp2$id <- droplevels(UDMap_final_reposoir_id_gp2$id)
UDMap_final_reposoir_id_gp3 <- UDMap_final_reposoir_id %>% 
  filter(id %in% id_gp_3)
UDMap_final_reposoir_id_gp3$id <- droplevels(UDMap_final_reposoir_id_gp3$id)
UDMap_final_reposoir_id_gp4 <- UDMap_final_reposoir_id %>% 
  filter(id %in% id_gp_4)
UDMap_final_reposoir_id_gp4$id <- droplevels(UDMap_final_reposoir_id_gp4$id)

# plot 
tmap_mode("view")

UDMap_reposoir_id_gp1 <- tm_shape(RMO) +
  tm_polygons() +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(UDMap_final_reposoir_id_gp1) + 
  tm_polygons(border.col = "grey", fill = "id", fill_alpha = 0.2,
              fill.legend = tm_legend(legend.outside = T, legend.stack = "horizontal", legend.outside.position = 'bottom'))

UDMap_reposoir_id_gp2 <- tm_shape(RMO) +
  tm_polygons() +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(UDMap_final_reposoir_id_gp2) + 
  tm_polygons(border.col = "grey", fill = "id", fill_alpha = 0.2,
              fill.legend = tm_legend(legend.outside = T, legend.stack = "horizontal", legend.outside.position = 'bottom'))

UDMap_reposoir_id_gp3 <- tm_shape(RMO) +
  tm_polygons() +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(UDMap_final_reposoir_id_gp3) + 
  tm_polygons(border.col = "grey", fill = "id", fill_alpha = 0.2,
              fill.legend = tm_legend(legend.outside = T, legend.stack = "horizontal", legend.outside.position = 'bottom')) 

UDMap_reposoir_id_gp4 <- tm_shape(RMO) +
  tm_polygons() +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(UDMap_final_reposoir_id_gp4) + 
  tm_polygons(border.col = "grey", fill = "id", fill_alpha = 0.2,
              fill.legend = tm_legend(legend.outside = T, legend.stack = "horizontal", legend.outside.position = 'bottom'))

UDMap_reposoir_id <- tmap_arrange(UDMap_reposoir_id_gp1, UDMap_reposoir_id_gp2, UDMap_reposoir_id_gp3, UDMap_reposoir_id_gp4) ; UDMap_reposoir_id

# tmap_save(UDMap_reposoir_id, paste0(data_image_path_serveur, "/UDMap_reposoir_id.html"))

### Breche ------------------------------------------------

#### details ----

# Charger les données en lat/lon (EPSG:4326)
coords_breche_detail <- GPS %>% 
  filter(behavior == "roosting") %>% 
  dplyr::select(lon, lat, breche_detail) %>% 
  st_drop_geometry() %>% 
  na.omit()

# Transformer en objet spatial (EPSG:4326)
locs_breche_detail <- st_as_sf(coords_breche_detail, coords = c("lon", "lat"), crs = 4326)

# Reprojeter en système métrique (ex. UTM zone 30N - EPSG:32630 pour la France)
locs_breche_detail_32630 <- st_transform(locs_breche_detail, crs = 32630)  # Adapter le CRS à votre région

# Reprojection du raster
crs_utm <- CRS("+init=epsg:32630") # Définir le CRS cible (EPSG:32630 = UTM zone 30N)
raster_100x100_32630 <- projectRaster(raster_100x100, crs = crs_utm)
crs(raster_100x100_32630) # Vérifier le CRS

# Extraire les coordonnées reprojetées
coords_breche_detail_32630 <- st_coordinates(locs_breche_detail_32630)

# Règle de Silverman
sigma_x_breche_detail <- sd(coords_breche_detail_32630[,1])  # Écart-type en X (mètres)
sigma_y_breche_detail <- sd(coords_breche_detail_32630[,2])  # Écart-type en Y (mètres)
n_breche_detail <- nrow(coords_breche_detail)  # Nombre de points

h_silverman_x_breche_detail <- 1.06 * sigma_x_breche_detail * n_breche_detail^(-1/5)
h_silverman_y_breche_detail <- 1.06 * sigma_y_breche_detail * n_breche_detail^(-1/5)

cat("h optimal en mètres pour X:", h_silverman_x_breche_detail, "\n")
cat("h optimal en mètres pour Y:", h_silverman_y_breche_detail, "\n")

# locs_spa <- st_transform(locs, crs = 32630)
locs_spa_breche_detail <- as(locs_breche_detail_32630, "Spatial")

# Appliquer kernelUD avec h estimé par Silverman
kud_breche_detail <- kernelUD(locs_spa_breche_detail["breche_detail"], grid = as(raster_100x100_32630, "SpatialPixels"),
                         h = mean(c(h_silverman_x_breche_detail, h_silverman_y_breche_detail)))

# Visualiser la densité de noyau
# par(mfrow = c(1, 1))
# image(kud_breche_detail)

# Créer une liste pour stocker les résultats
UDmaps_list_breche_detail <- lapply(names(kud_breche_detail), function(breche_detail) {
  
  print(breche_detail)
  
  # Extraire l'estimation de densité pour un ID spécifique
  kud_single_breche_detail <- kud_breche_detail[[breche_detail]]
  rast_breche_detail <- rast(kud_single_breche_detail)
  contour_breche_detail <- as.contour(rast_breche_detail)
  sf_breche_detail <- st_as_sf(contour_breche_detail)
  cast_breche_detail <- st_cast(sf_breche_detail, "POLYGON")
  cast_breche_detail$breche_detail <- breche_detail
  
  return(cast_breche_detail)
})

# Fusionner tous les ID dans un seul objet sf
UDMap_final_breche_detail <- do.call(rbind, UDmaps_list_breche_detail)

UDMap_final_breche_detail$breche_detail <- as.factor(UDMap_final_breche_detail$breche_detail)

# groupe plot
UDMap_final_breche_detail_gp1 <- UDMap_final_breche_detail %>% 
  filter(breche_detail == "digue intacte")
UDMap_final_breche_detail_gp1$breche_detail <- droplevels(UDMap_final_breche_detail_gp1$breche_detail)
UDMap_final_breche_detail_gp2 <- UDMap_final_breche_detail %>% 
  filter(breche_detail == "disparition du seuil")
UDMap_final_breche_detail_gp2$breche_detail <- droplevels(UDMap_final_breche_detail_gp2$breche_detail)
UDMap_final_breche_detail_gp3 <- UDMap_final_breche_detail %>% 
  filter(breche_detail == "ouverture progressive")
UDMap_final_breche_detail_gp3$breche_detail <- droplevels(UDMap_final_breche_detail_gp3$breche_detail)
UDMap_final_breche_detail_gp4 <- UDMap_final_breche_detail %>% 
  filter(breche_detail == "ouverture complète")
UDMap_final_breche_detail_gp4$breche_detail <- droplevels(UDMap_final_breche_detail_gp4$breche_detail)

# plot 
tmap_mode("view")

UDMap_breche_detail_gp1 <- tm_shape(RMO) +
  tm_polygons() +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(UDMap_final_breche_detail_gp1) + 
  tm_polygons(border.col = "grey", fill = "breche_detail", fill_alpha = 0.2,
              fill.legend = tm_legend(legend.outside = T, legend.stack = "horizontal", legend.outside.position = 'bottom'))

UDMap_breche_detail_gp2 <- tm_shape(RMO) +
  tm_polygons() +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(UDMap_final_breche_detail_gp2) + 
  tm_polygons(border.col = "grey", fill = "breche_detail", fill_alpha = 0.2,
              fill.legend = tm_legend(legend.outside = T, legend.stack = "horizontal", legend.outside.position = 'bottom'))

UDMap_breche_detail_gp3 <- tm_shape(RMO) +
  tm_polygons() +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(UDMap_final_breche_detail_gp3) + 
  tm_polygons(border.col = "grey", fill = "breche_detail", fill_alpha = 0.2,
              fill.legend = tm_legend(legend.outside = T, legend.stack = "horizontal", legend.outside.position = 'bottom')) 

UDMap_breche_detail_gp4 <- tm_shape(RMO) +
  tm_polygons() +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(UDMap_final_breche_detail_gp4) + 
  tm_polygons(border.col = "grey", fill = "breche_detail", fill_alpha = 0.2,
              fill.legend = tm_legend(legend.outside = T, legend.stack = "horizontal", legend.outside.position = 'bottom'))

UDMap_breche_detail <- tmap_arrange(UDMap_breche_detail_gp1, UDMap_breche_detail_gp2, UDMap_breche_detail_gp3, UDMap_breche_detail_gp4) ; UDMap_breche_detail

#### summary ----

# Charger les données en lat/lon (EPSG:4326)
coords_breche_summary <- GPS %>% 
  filter(behavior == "roosting") %>% 
  dplyr::select(lon, lat, breche_summary) %>% 
  st_drop_geometry() %>% 
  na.omit()

# Transformer en objet spatial (EPSG:4326)
locs_breche_summary <- st_as_sf(coords_breche_summary, coords = c("lon", "lat"), crs = 4326)

# Reprojeter en système métrique (ex. UTM zone 30N - EPSG:32630 pour la France)
locs_breche_summary_32630 <- st_transform(locs_breche_summary, crs = 32630)  # Adapter le CRS à votre région

# Reprojection du raster
crs_utm <- CRS("+init=epsg:32630") # Définir le CRS cible (EPSG:32630 = UTM zone 30N)
raster_100x100_32630 <- projectRaster(raster_100x100, crs = crs_utm)
crs(raster_100x100_32630) # Vérifier le CRS

# Extraire les coordonnées reprojetées
coords_breche_summary_32630 <- st_coordinates(locs_breche_summary_32630)

# Règle de Silverman
sigma_x_breche_summary <- sd(coords_breche_summary_32630[,1])  # Écart-type en X (mètres)
sigma_y_breche_summary <- sd(coords_breche_summary_32630[,2])  # Écart-type en Y (mètres)
n_breche_summary <- nrow(coords_breche_summary)  # Nombre de points

h_silverman_x_breche_summary <- 1.06 * sigma_x_breche_summary * n_breche_summary^(-1/5)
h_silverman_y_breche_summary <- 1.06 * sigma_y_breche_summary * n_breche_summary^(-1/5)

cat("h optimal en mètres pour X:", h_silverman_x_breche_summary, "\n")
cat("h optimal en mètres pour Y:", h_silverman_y_breche_summary, "\n")

# locs_spa <- st_transform(locs, crs = 32630)
locs_spa_breche_summary <- as(locs_breche_summary_32630, "Spatial")

# Appliquer kernelUD avec h estimé par Silverman
kud_breche_summary <- kernelUD(locs_spa_breche_summary["breche_summary"], grid = as(raster_100x100_32630, "SpatialPixels"),
                              h = mean(c(h_silverman_x_breche_summary, h_silverman_y_breche_summary)))

# Visualiser la densité de noyau
# par(mfrow = c(1, 1))
# image(kud_breche_summary)

# Créer une liste pour stocker les résultats
UDmaps_list_breche_summary <- lapply(names(kud_breche_summary), function(breche_summary) {
  
  print(breche_summary)
  
  # Extraire l'estimation de densité pour un ID spécifique
  kud_single_breche_summary <- kud_breche_summary[[breche_summary]]
  rast_breche_summary <- rast(kud_single_breche_summary)
  contour_breche_summary <- as.contour(rast_breche_summary)
  sf_breche_summary <- st_as_sf(contour_breche_summary)
  cast_breche_summary <- st_cast(sf_breche_summary, "POLYGON")
  cast_breche_summary$breche_summary <- breche_summary
  
  return(cast_breche_summary)
})

# Fusionner tous les ID dans un seul objet sf
UDMap_final_breche_summary <- do.call(rbind, UDmaps_list_breche_summary)

UDMap_final_breche_summary$breche_summary <- as.factor(UDMap_final_breche_summary$breche_summary)

tmap_mode("view")

UDMap_breche_summary_gp1 <- tm_shape(RMO) +
  tm_polygons() +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(UDMap_final_breche_summary) + 
  tm_polygons(border.col = "grey", fill = "breche_summary", fill_alpha = 0.2,
              fill.legend = tm_legend(legend.outside = T, legend.stack = "horizontal", legend.outside.position = 'bottom'))

# groupe plot
UDMap_final_breche_summary_gp1 <- UDMap_final_breche_summary %>% 
  filter(breche_summary == "digue intacte")
UDMap_final_breche_summary_gp1$breche_summary <- droplevels(UDMap_final_breche_summary_gp1$breche_summary)
UDMap_final_breche_summary_gp2 <- UDMap_final_breche_summary %>% 
  filter(breche_summary == "ouverture progressive")
UDMap_final_breche_summary_gp2$breche_summary <- droplevels(UDMap_final_breche_summary_gp2$breche_summary)
UDMap_final_breche_summary_gp3 <- UDMap_final_breche_summary %>% 
  filter(breche_summary == "ouverture complète")
UDMap_final_breche_summary_gp3$breche_summary <- droplevels(UDMap_final_breche_summary_gp3$breche_summary)

# plot 
tmap_mode("view")

UDMap_breche_summary_gp1 <- tm_shape(RMO) +
  tm_polygons() +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(UDMap_final_breche_summary_gp1) + 
  tm_polygons(border.col = "grey", fill = "breche_summary", fill_alpha = 0.2,
              fill.legend = tm_legend(legend.outside = T, legend.stack = "horizontal", legend.outside.position = 'bottom'))

UDMap_breche_summary_gp2 <- tm_shape(RMO) +
  tm_polygons() +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(UDMap_final_breche_summary_gp2) + 
  tm_polygons(border.col = "grey", fill = "breche_summary", fill_alpha = 0.2,
              fill.legend = tm_legend(legend.outside = T, legend.stack = "horizontal", legend.outside.position = 'bottom'))

UDMap_breche_summary_gp3 <- tm_shape(RMO) +
  tm_polygons() +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(UDMap_final_breche_summary_gp3) + 
  tm_polygons(border.col = "grey", fill = "breche_summary", fill_alpha = 0.2,
              fill.legend = tm_legend(legend.outside = T, legend.stack = "horizontal", legend.outside.position = 'bottom')) 

UDMap_breche_summary <- tmap_arrange(UDMap_breche_summary_gp1, UDMap_breche_summary_gp2, UDMap_breche_summary_gp3) ; UDMap_breche_summary

### Type de marée ------------------------------------------------

# Charger les données en lat/lon (EPSG:4326)
coords_type_maree <- GPS %>% 
  filter(behavior == "roosting") %>% 
  dplyr::select(lon, lat, type_maree) %>% 
  st_drop_geometry() %>% 
  na.omit()

# Transformer en objet spatial (EPSG:4326)
locs_type_maree <- st_as_sf(coords_type_maree, coords = c("lon", "lat"), crs = 4326)

# Reprojeter en système métrique (ex. UTM zone 30N - EPSG:32630 pour la France)
locs_type_maree_32630 <- st_transform(locs_type_maree, crs = 32630)  # Adapter le CRS à votre région

# Reprojection du raster
crs_utm <- CRS("+init=epsg:32630") # Définir le CRS cible (EPSG:32630 = UTM zone 30N)
raster_100x100_32630 <- projectRaster(raster_100x100, crs = crs_utm)
crs(raster_100x100_32630) # Vérifier le CRS

# Extraire les coordonnées reprojetées
coords_type_maree_32630 <- st_coordinates(locs_type_maree_32630)

# Règle de Silverman
sigma_x_type_maree <- sd(coords_type_maree_32630[,1])  # Écart-type en X (mètres)
sigma_y_type_maree <- sd(coords_type_maree_32630[,2])  # Écart-type en Y (mètres)
n_type_maree <- nrow(coords_type_maree)  # Nombre de points

h_silverman_x_type_maree <- 1.06 * sigma_x_type_maree * n_type_maree^(-1/5)
h_silverman_y_type_maree <- 1.06 * sigma_y_type_maree * n_type_maree^(-1/5)

cat("h optimal en mètres pour X:", h_silverman_x_type_maree, "\n")
cat("h optimal en mètres pour Y:", h_silverman_y_type_maree, "\n")

# locs_spa <- st_transform(locs, crs = 32630)
locs_spa_type_maree <- as(locs_type_maree_32630, "Spatial")

# Appliquer kernelUD avec h estimé par Silverman
kud_type_maree <- kernelUD(locs_spa_type_maree["type_maree"], grid = as(raster_100x100_32630, "SpatialPixels"),
                              h = mean(c(h_silverman_x_type_maree, h_silverman_y_type_maree)))

# Visualiser la densité de noyau
par(mfrow = c(1, 1))
image(kud_type_maree)

# Créer une liste pour stocker les résultats
UDmaps_list_type_maree <- lapply(names(kud_type_maree), function(type_maree) {
  
  print(type_maree)
  
  # Extraire l'estimation de densité pour un ID spécifique
  kud_single_type_maree <- kud_type_maree[[type_maree]]
  rast_type_maree <- rast(kud_single_type_maree)
  contour_type_maree <- as.contour(rast_type_maree)
  sf_type_maree <- st_as_sf(contour_type_maree)
  cast_type_maree <- st_cast(sf_type_maree, "POLYGON")
  cast_type_maree$type_maree <- type_maree
  
  return(cast_type_maree)
})

# Fusionner tous les ID dans un seul objet sf
UDMap_final_type_maree <- do.call(rbind, UDmaps_list_type_maree)

UDMap_final_type_maree$type_maree <- as.factor(UDMap_final_type_maree$type_maree)

# plot 
tmap_mode("view")

UDMap_type_maree <- tm_shape(RMO) +
  tm_polygons() +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(UDMap_final_type_maree) + 
  tm_polygons(border.col = "grey", fill = "type_maree", fill_alpha = 0.2,
              fill.legend = tm_legend(legend.outside = T, legend.stack = "horizontal", legend.outside.position = 'bottom'))

tmap_save(UDMap_type_maree, paste0(data_image_path_serveur, "/UDMap_reposoir_type_maree.html"), dpi = 600)

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
par(mfrow = c(1, 1))
image(kud_jour_nuit)

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
  tm_polygons(border.col = "grey", fill = "jour_nuit", fill_alpha = 0.2,
              fill.legend = tm_legend(legend.outside = T, legend.stack = "horizontal", legend.outside.position = 'bottom'))

tmap_save(UDMap_jour_nuit, paste0(data_image_path_serveur, "/UDMap_reposoir_jour_nuit.html"), dpi = 600)

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
par(mfrow = c(1, 1))
image(kud_age)

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

# plot 
tmap_mode("view")

UDMap_age <- tm_shape(RMO) +
  tm_polygons() +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(UDMap_final_age) + 
  tm_polygons(border.col = "grey", fill = "age", fill_alpha = 0.2,
              fill.legend = tm_legend(legend.outside = T, legend.stack = "horizontal", legend.outside.position = 'bottom'))

tmap_save(UDMap_age, paste0(data_image_path_serveur, "/UDMap_reposoir_age.html"), dpi = 600)

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
  tm_polygons(border.col = "grey", fill = "sex", fill_alpha = 0.2,
              fill.legend = tm_legend(legend.outside = T, legend.stack = "horizontal", legend.outside.position = 'bottom'))

tmap_save(UDMap_sex, paste0(data_image_path_serveur, "/UDMap_reposoir_sex.html"), dpi = 600)

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
  tm_polygons(border.col = "grey", fill = "level", fill_alpha = 0.2, 
              palette = viridis(10, begin = 0, end = 1, 
                                direction = 1, option = "plasma")); UDMap_alim

tmap_save(UDMap_alim, paste0(data_image_path_serveur, "/UDMap_alim.html"), dpi = 600)

### id ----

# Charger les données en lat/lon (EPSG:4326)
coords_alim_id <- GPS %>% 
  filter(behavior == "foraging") %>% 
  dplyr::select(id,lon,lat) %>% 
  st_drop_geometry() %>% 
  na.omit()

# Transformer en objet spatial (EPSG:4326)
locs_alim_id <- st_as_sf(coords_alim_id, coords = c("lon", "lat"), crs = 4326)

# Reprojeter en système métrique (ex. UTM zone 30N - EPSG:32630 pour la France)
locs_alim_id_32630 <- st_transform(locs_alim_id, crs = 32630)  # Adapter le CRS à votre région

# Reprojection du raster
crs_utm <- CRS("+init=epsg:32630") # Définir le CRS cible (EPSG:32630 = UTM zone 30N)
raster_100x100_32630 <- projectRaster(raster_100x100, crs = crs_utm)
crs(raster_100x100_32630)

# Extraire les coordonnées reprojetées
coords_alim_id_32630 <- st_coordinates(locs_alim_id_32630)

# Règle de Silverman
sigma_x_alim_id <- sd(coords_alim_id_32630[,1])  # Écart-type en X (mètres)
sigma_y_alim_id <- sd(coords_alim_id_32630[,2])  # Écart-type en Y (mètres)
n_alim_id <- nrow(coords_alim_id_32630)  # Nombre de points

h_silverman_x_alim_id <- 1.06 * sigma_x_alim_id * n_alim_id^(-1/5)
h_silverman_y_alim_id <- 1.06 * sigma_y_alim_id * n_alim_id^(-1/5)

cat("h optimal en mètres pour X:", h_silverman_x_alim_id, "\n")
cat("h optimal en mètres pour Y:", h_silverman_y_alim_id, "\n")

# locs_spa <- as(locs_m, "Spatial")

# locs_spa <- st_transform(locs_alim_id, crs = 32630)
locs_spa_alim_id <- as(locs_alim_id_32630, "Spatial")

# Appliquer kernelUD avec h estimé par Silverman
kud_alim_id <- kernelUD(locs_spa_alim_id["id"], grid = as(raster_100x100_32630, "SpatialPixels"),
                            h = mean(c(h_silverman_x_alim_id, h_silverman_y_alim_id)))

# Visualiser la densité de noyau
par(mfrow = c(1, 1))
image(kud_alim_id)

# Créer une liste pour stocker les résultats
UDmaps_list_alim_id <- lapply(names(kud_alim_id), function(id) {
  
  print(id)
  
  # Extraire l'estimation de densité pour un ID spécifique
  kud_single_alim_id <- kud_alim_id[[id]]
  rast_alim_id <- rast(kud_single_alim_id)
  contour_alim_id <- as.contour(rast_alim_id)
  sf_alim_id <- st_as_sf(contour_alim_id)
  cast_alim_id <- st_cast(sf_alim_id, "POLYGON")
  cast_alim_id$id <- id
  
  return(cast_alim_id)
})

# Fusionner tous les ID dans un seul objet sf
UDMap_final_alim_id <- do.call(rbind, UDmaps_list_alim_id)

UDMap_final_alim_id$id <- as.factor(UDMap_final_alim_id$id)

# write
st_write(UDMap_final_alim_id, paste0(data_generated_path_serveur, "UDMap_final_alim_id.gpkg"), append = FALSE)
# read
UDMap_final_alim_id <- st_read(file.path(data_generated_path_serveur, "UDMap_final_alim_id.gpkg"))

# groupe plot
id_list <- unique(UDMap_final_alim_id$id)
id_gp_1 <- id_list[1:15]
id_gp_2 <- id_list[16:30]
id_gp_3 <- id_list[31:45]
id_gp_4 <- id_list[46:69]

UDMap_final_alim_id_gp1 <- UDMap_final_alim_id %>% 
  filter(id %in% id_gp_1)
UDMap_final_alim_id_gp1$id <- droplevels(UDMap_final_alim_id_gp1$id)
UDMap_final_alim_id_gp2 <- UDMap_final_alim_id %>% 
  filter(id %in% id_gp_2)
UDMap_final_alim_id_gp2$id <- droplevels(UDMap_final_alim_id_gp2$id)
UDMap_final_alim_id_gp3 <- UDMap_final_alim_id %>% 
  filter(id %in% id_gp_3)
UDMap_final_alim_id_gp3$id <- droplevels(UDMap_final_alim_id_gp3$id)
UDMap_final_alim_id_gp4 <- UDMap_final_alim_id %>% 
  filter(id %in% id_gp_4)
UDMap_final_alim_id_gp4$id <- droplevels(UDMap_final_alim_id_gp4$id)

# plot 
tmap_mode("view")

UDMap_alim_id_gp1 <- tm_shape(RMO) +
  tm_polygons() +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(UDMap_final_alim_id_gp1) + 
  tm_polygons(border.col = "grey", fill = "id", fill_alpha = 0.2,
              fill.legend = tm_legend(legend.outside = T, legend.stack = "horizontal", legend.outside.position = 'bottom'))

UDMap_alim_id_gp2 <- tm_shape(RMO) +
  tm_polygons() +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(UDMap_final_alim_id_gp2) + 
  tm_polygons(border.col = "grey", fill = "id", fill_alpha = 0.2,
              fill.legend = tm_legend(legend.outside = T, legend.stack = "horizontal", legend.outside.position = 'bottom'))

UDMap_alim_id_gp3 <- tm_shape(RMO) +
  tm_polygons() +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(UDMap_final_alim_id_gp3) + 
  tm_polygons(border.col = "grey", fill = "id", fill_alpha = 0.2,
              fill.legend = tm_legend(legend.outside = T, legend.stack = "horizontal", legend.outside.position = 'bottom')) 

UDMap_alim_id_gp4 <- tm_shape(RMO) +
  tm_polygons() +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(UDMap_final_alim_id_gp4) + 
  tm_polygons(border.col = "grey", fill = "id", fill_alpha = 0.2,
              fill.legend = tm_legend(legend.outside = T, legend.stack = "horizontal", legend.outside.position = 'bottom'))

UDMap_alim_id <- tmap_arrange(UDMap_alim_id_gp1, UDMap_alim_id_gp2, UDMap_alim_id_gp3, UDMap_alim_id_gp4) ; UDMap_alim_id

### Breche ------------------------------------------------

#### details ----

# Charger les données en lat/lon (EPSG:4326)
coords_breche_detail <- GPS %>% 
  filter(behavior == "roosting") %>% 
  dplyr::select(lon, lat, breche_detail) %>% 
  st_drop_geometry() %>% 
  na.omit()

# Transformer en objet spatial (EPSG:4326)
locs_breche_detail <- st_as_sf(coords_breche_detail, coords = c("lon", "lat"), crs = 4326)

# Reprojeter en système métrique (ex. UTM zone 30N - EPSG:32630 pour la France)
locs_breche_detail_32630 <- st_transform(locs_breche_detail, crs = 32630)  # Adapter le CRS à votre région

# Reprojection du raster
crs_utm <- CRS("+init=epsg:32630") # Définir le CRS cible (EPSG:32630 = UTM zone 30N)
raster_100x100_32630 <- projectRaster(raster_100x100, crs = crs_utm)
crs(raster_100x100_32630) # Vérifier le CRS

# Extraire les coordonnées reprojetées
coords_breche_detail_32630 <- st_coordinates(locs_breche_detail_32630)

# Règle de Silverman
sigma_x_breche_detail <- sd(coords_breche_detail_32630[,1])  # Écart-type en X (mètres)
sigma_y_breche_detail <- sd(coords_breche_detail_32630[,2])  # Écart-type en Y (mètres)
n_breche_detail <- nrow(coords_breche_detail)  # Nombre de points

h_silverman_x_breche_detail <- 1.06 * sigma_x_breche_detail * n_breche_detail^(-1/5)
h_silverman_y_breche_detail <- 1.06 * sigma_y_breche_detail * n_breche_detail^(-1/5)

cat("h optimal en mètres pour X:", h_silverman_x_breche_detail, "\n")
cat("h optimal en mètres pour Y:", h_silverman_y_breche_detail, "\n")

# locs_spa <- st_transform(locs, crs = 32630)
locs_spa_breche_detail <- as(locs_breche_detail_32630, "Spatial")

# Appliquer kernelUD avec h estimé par Silverman
kud_breche_detail <- kernelUD(locs_spa_breche_detail["breche_detail"], grid = as(raster_100x100_32630, "SpatialPixels"),
                              h = mean(c(h_silverman_x_breche_detail, h_silverman_y_breche_detail)))

# Visualiser la densité de noyau
# par(mfrow = c(1, 1))
# image(kud_breche_detail)

# Créer une liste pour stocker les résultats
UDmaps_list_breche_detail <- lapply(names(kud_breche_detail), function(breche_detail) {
  
  print(breche_detail)
  
  # Extraire l'estimation de densité pour un ID spécifique
  kud_single_breche_detail <- kud_breche_detail[[breche_detail]]
  rast_breche_detail <- rast(kud_single_breche_detail)
  contour_breche_detail <- as.contour(rast_breche_detail)
  sf_breche_detail <- st_as_sf(contour_breche_detail)
  cast_breche_detail <- st_cast(sf_breche_detail, "POLYGON")
  cast_breche_detail$breche_detail <- breche_detail
  
  return(cast_breche_detail)
})

# Fusionner tous les ID dans un seul objet sf
UDMap_final_breche_detail <- do.call(rbind, UDmaps_list_breche_detail)

UDMap_final_breche_detail$breche_detail <- as.factor(UDMap_final_breche_detail$breche_detail)

# groupe plot
UDMap_final_breche_detail_gp1 <- UDMap_final_breche_detail %>% 
  filter(breche_detail == "digue intacte")
UDMap_final_breche_detail_gp1$breche_detail <- droplevels(UDMap_final_breche_detail_gp1$breche_detail)
UDMap_final_breche_detail_gp2 <- UDMap_final_breche_detail %>% 
  filter(breche_detail == "disparition du seuil")
UDMap_final_breche_detail_gp2$breche_detail <- droplevels(UDMap_final_breche_detail_gp2$breche_detail)
UDMap_final_breche_detail_gp3 <- UDMap_final_breche_detail %>% 
  filter(breche_detail == "ouverture progressive")
UDMap_final_breche_detail_gp3$breche_detail <- droplevels(UDMap_final_breche_detail_gp3$breche_detail)
UDMap_final_breche_detail_gp4 <- UDMap_final_breche_detail %>% 
  filter(breche_detail == "ouverture complète")
UDMap_final_breche_detail_gp4$breche_detail <- droplevels(UDMap_final_breche_detail_gp4$breche_detail)

# plot 
tmap_mode("view")

UDMap_breche_detail_gp1 <- tm_shape(RMO) +
  tm_polygons() +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(UDMap_final_breche_detail_gp1) + 
  tm_polygons(border.col = "grey", fill = "breche_detail", fill_alpha = 0.2,
              fill.legend = tm_legend(legend.outside = T, legend.stack = "horizontal", legend.outside.position = 'bottom'))

UDMap_breche_detail_gp2 <- tm_shape(RMO) +
  tm_polygons() +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(UDMap_final_breche_detail_gp2) + 
  tm_polygons(border.col = "grey", fill = "breche_detail", fill_alpha = 0.2,
              fill.legend = tm_legend(legend.outside = T, legend.stack = "horizontal", legend.outside.position = 'bottom'))

UDMap_breche_detail_gp3 <- tm_shape(RMO) +
  tm_polygons() +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(UDMap_final_breche_detail_gp3) + 
  tm_polygons(border.col = "grey", fill = "breche_detail", fill_alpha = 0.2,
              fill.legend = tm_legend(legend.outside = T, legend.stack = "horizontal", legend.outside.position = 'bottom')) 

UDMap_breche_detail_gp4 <- tm_shape(RMO) +
  tm_polygons() +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(UDMap_final_breche_detail_gp4) + 
  tm_polygons(border.col = "grey", fill = "breche_detail", fill_alpha = 0.2,
              fill.legend = tm_legend(legend.outside = T, legend.stack = "horizontal", legend.outside.position = 'bottom'))

UDMap_breche_detail <- tmap_arrange(UDMap_breche_detail_gp1, UDMap_breche_detail_gp2, UDMap_breche_detail_gp3, UDMap_breche_detail_gp4) ; UDMap_breche_detail

#### summary ----

# Charger les données en lat/lon (EPSG:4326)
coords_breche_summary <- GPS %>% 
  filter(behavior == "roosting") %>% 
  dplyr::select(lon, lat, breche_summary) %>% 
  st_drop_geometry() %>% 
  na.omit()

# Transformer en objet spatial (EPSG:4326)
locs_breche_summary <- st_as_sf(coords_breche_summary, coords = c("lon", "lat"), crs = 4326)

# Reprojeter en système métrique (ex. UTM zone 30N - EPSG:32630 pour la France)
locs_breche_summary_32630 <- st_transform(locs_breche_summary, crs = 32630)  # Adapter le CRS à votre région

# Reprojection du raster
crs_utm <- CRS("+init=epsg:32630") # Définir le CRS cible (EPSG:32630 = UTM zone 30N)
raster_100x100_32630 <- projectRaster(raster_100x100, crs = crs_utm)
crs(raster_100x100_32630) # Vérifier le CRS

# Extraire les coordonnées reprojetées
coords_breche_summary_32630 <- st_coordinates(locs_breche_summary_32630)

# Règle de Silverman
sigma_x_breche_summary <- sd(coords_breche_summary_32630[,1])  # Écart-type en X (mètres)
sigma_y_breche_summary <- sd(coords_breche_summary_32630[,2])  # Écart-type en Y (mètres)
n_breche_summary <- nrow(coords_breche_summary)  # Nombre de points

h_silverman_x_breche_summary <- 1.06 * sigma_x_breche_summary * n_breche_summary^(-1/5)
h_silverman_y_breche_summary <- 1.06 * sigma_y_breche_summary * n_breche_summary^(-1/5)

cat("h optimal en mètres pour X:", h_silverman_x_breche_summary, "\n")
cat("h optimal en mètres pour Y:", h_silverman_y_breche_summary, "\n")

# locs_spa <- st_transform(locs, crs = 32630)
locs_spa_breche_summary <- as(locs_breche_summary_32630, "Spatial")

# Appliquer kernelUD avec h estimé par Silverman
kud_breche_summary <- kernelUD(locs_spa_breche_summary["breche_summary"], grid = as(raster_100x100_32630, "SpatialPixels"),
                               h = mean(c(h_silverman_x_breche_summary, h_silverman_y_breche_summary)))

# Visualiser la densité de noyau
# par(mfrow = c(1, 1))
# image(kud_breche_summary)

# Créer une liste pour stocker les résultats
UDmaps_list_breche_summary <- lapply(names(kud_breche_summary), function(breche_summary) {
  
  print(breche_summary)
  
  # Extraire l'estimation de densité pour un ID spécifique
  kud_single_breche_summary <- kud_breche_summary[[breche_summary]]
  rast_breche_summary <- rast(kud_single_breche_summary)
  contour_breche_summary <- as.contour(rast_breche_summary)
  sf_breche_summary <- st_as_sf(contour_breche_summary)
  cast_breche_summary <- st_cast(sf_breche_summary, "POLYGON")
  cast_breche_summary$breche_summary <- breche_summary
  
  return(cast_breche_summary)
})

# Fusionner tous les ID dans un seul objet sf
UDMap_final_breche_summary <- do.call(rbind, UDmaps_list_breche_summary)

UDMap_final_breche_summary$breche_summary <- as.factor(UDMap_final_breche_summary$breche_summary)

tmap_mode("view")

UDMap_breche_summary_gp1 <- tm_shape(RMO) +
  tm_polygons() +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(UDMap_final_breche_summary) + 
  tm_polygons(border.col = "grey", fill = "breche_summary", fill_alpha = 0.2,
              fill.legend = tm_legend(legend.outside = T, legend.stack = "horizontal", legend.outside.position = 'bottom'))

# groupe plot
UDMap_final_breche_summary_gp1 <- UDMap_final_breche_summary %>% 
  filter(breche_summary == "digue intacte")
UDMap_final_breche_summary_gp1$breche_summary <- droplevels(UDMap_final_breche_summary_gp1$breche_summary)
UDMap_final_breche_summary_gp2 <- UDMap_final_breche_summary %>% 
  filter(breche_summary == "ouverture progressive")
UDMap_final_breche_summary_gp2$breche_summary <- droplevels(UDMap_final_breche_summary_gp2$breche_summary)
UDMap_final_breche_summary_gp3 <- UDMap_final_breche_summary %>% 
  filter(breche_summary == "ouverture complète")
UDMap_final_breche_summary_gp3$breche_summary <- droplevels(UDMap_final_breche_summary_gp3$breche_summary)

# plot 
tmap_mode("view")

UDMap_breche_summary_gp1 <- tm_shape(RMO) +
  tm_polygons() +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(UDMap_final_breche_summary_gp1) + 
  tm_polygons(border.col = "grey", fill = "breche_summary", fill_alpha = 0.2,
              fill.legend = tm_legend(legend.outside = T, legend.stack = "horizontal", legend.outside.position = 'bottom'))

UDMap_breche_summary_gp2 <- tm_shape(RMO) +
  tm_polygons() +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(UDMap_final_breche_summary_gp2) + 
  tm_polygons(border.col = "grey", fill = "breche_summary", fill_alpha = 0.2,
              fill.legend = tm_legend(legend.outside = T, legend.stack = "horizontal", legend.outside.position = 'bottom'))

UDMap_breche_summary_gp3 <- tm_shape(RMO) +
  tm_polygons() +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(UDMap_final_breche_summary_gp3) + 
  tm_polygons(border.col = "grey", fill = "breche_summary", fill_alpha = 0.2,
              fill.legend = tm_legend(legend.outside = T, legend.stack = "horizontal", legend.outside.position = 'bottom')) 

UDMap_breche_summary <- tmap_arrange(UDMap_breche_summary_gp1, UDMap_breche_summary_gp2, UDMap_breche_summary_gp3) ; UDMap_breche_summary

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
  tm_polygons(border.col = "grey", fill = "jour_nuit", fill_alpha = 0.2,
              fill.legend = tm_legend(legend.outside = T, legend.stack = "horizontal", legend.outside.position = 'bottom'))

tmap_save(UDMap_alim_jour_nuit, paste0(data_image_path_serveur, "/UDMap_alim_jour_nuit.html"), dpi = 600)

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
  tm_polygons(border.col = "grey", fill = "age", fill_alpha = 0.2,
              fill.legend = tm_legend(legend.outside = T, legend.stack = "horizontal", legend.outside.position = 'bottom'))

tmap_save(UDMap_alim_age, paste0(data_image_path_serveur, "/UDMap_reposoir_alim_age.html"), dpi = 600)

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
  tm_polygons(border.col = "grey", fill = "sex", fill_alpha = 0.2,
              fill.legend = tm_legend(legend.outside = T, legend.stack = "horizontal", legend.outside.position = 'bottom'))

tmap_save(UDMap_alim_sex, paste0(data_image_path_serveur, "/UDMap_reposoir_alim_sex.html"), dpi = 600)

###
####
# Home Range -------------------------------------------------------------------
####
###

# Charger les données en lat/lon (EPSG:4326)
coords_HR_id <- GPS %>% 
  # filter(behavior == "roosting") %>% 
  dplyr::select(id,lon,lat) %>% 
  st_drop_geometry() %>% 
  na.omit()

# Transformer en objet spatial (EPSG:4326)
locs_HR_id <- st_as_sf(coords_HR_id, coords = c("lon", "lat"), crs = 4326)

# Reprojeter en système métrique (ex. UTM zone 30N - EPSG:32630 pour la France)
locs_HR_id_32630 <- st_transform(locs_HR_id, crs = 32630)  # Adapter le CRS à votre région

# Reprojection du raster
crs_utm <- CRS("+init=epsg:32630") # Définir le CRS cible (EPSG:32630 = UTM zone 30N)
raster_100x100_32630 <- projectRaster(raster_100x100, crs = crs_utm)
crs(raster_100x100_32630)

# Extraire les coordonnées reprojetées
coords_HR_id_32630 <- st_coordinates(locs_HR_id_32630)

# Règle de Silverman
sigma_x_HR_id <- sd(coords_HR_id_32630[,1])  # Écart-type en X (mètres)
sigma_y_HR_id <- sd(coords_HR_id_32630[,2])  # Écart-type en Y (mètres)
n_HR_id <- nrow(coords_HR_id_32630)  # Nombre de points

h_silverman_x_HR_id <- 1.06 * sigma_x_HR_id * n_HR_id^(-1/5)
h_silverman_y_HR_id <- 1.06 * sigma_y_HR_id * n_HR_id^(-1/5)

cat("h optimal en mètres pour X:", h_silverman_x_HR_id, "\n")
cat("h optimal en mètres pour Y:", h_silverman_y_HR_id, "\n")

# locs_spa <- as(locs_m, "Spatial")

# locs_spa <- st_transform(locs_HR_id, crs = 32630)
locs_spa_HR_id <- as(locs_HR_id_32630, "Spatial")

# Appliquer kernelUD avec h estimé par Silverman
kud_HR_id <- kernelUD(locs_spa_HR_id["id"], grid = as(raster_100x100_32630, "SpatialPixels"),
                            h = mean(c(h_silverman_x_HR_id, h_silverman_y_HR_id)))

kde_hr_95 <- getverticeshr(kud_HR_id, 95)
kde_hr_50 <- getverticeshr(kud_HR_id, 50)

# Conversion des home range KDE en sf
kde_hr_95_sf <- st_as_sf(kde_hr_95)
kde_hr_50_sf <- st_as_sf(kde_hr_50)

# plot(kde_hr_95, col = "blue", border = "black", lwd = 2, main = "Home range 95% et 50%")
# plot(kde_hr_50, col = "red", add = TRUE)

# # Créer une liste pour stocker les résultats
# UDmaps_list_HR_id <- lapply(names(kud_HR_id), function(id) {
#   
#   print(id)
#   
#   # Extraire l'estimation de densité pour un ID spécifique
#   kud_single_HR_id <- kud_HR_id[[id]]
#   rast_HR_id <- rast(kud_single_HR_id)
#   contour_HR_id <- as.contour(rast_HR_id)
#   sf_HR_id <- st_as_sf(contour_HR_id)
#   cast_HR_id <- st_cast(sf_HR_id, "POLYGON")
#   cast_HR_id$id <- id
#   
#   return(cast_HR_id)
#   
# })
# 
# # Fusionner tous les ID dans un seul objet sf
# UDMap_final_HR_id <- do.call(rbind, UDmaps_list_HR_id)
# # UDMap_final_hr_95 <- do.call(rbind, UDmaps_list_hr_95)
# 
# UDMap_final_HR_id$id <- as.factor(UDMap_final_HR_id$id)

# groupe plot
id_list <- unique(UDMap_final_HR_id$id)
id_gp_1 <- id_list[1:15]
id_gp_2 <- id_list[16:30]
id_gp_3 <- id_list[31:45]
id_gp_4 <- id_list[46:69]

kde_hr_95_sf_gp1 <- kde_hr_95_sf %>% 
  filter(id %in% id_gp_1)
# kde_hr_95_sf_gp1$id <- droplevels(kde_hr_95_sf_gp1$id)

kde_hr_95_sf_gp2 <- kde_hr_95_sf %>% 
  filter(id %in% id_gp_2)
# kde_hr_95_sf_gp2$id <- droplevels(kde_hr_95_sf_gp2$id)

kde_hr_95_sf_gp3 <- kde_hr_95_sf %>% 
  filter(id %in% id_gp_1)
# kde_hr_95_sf_gp3$id <- droplevels(kde_hr_95_sf_gp3$id)

kde_hr_95_sf_gp4 <- kde_hr_95_sf %>% 
  filter(id %in% id_gp_1)
# kde_hr_95_sf_gp4$id <- droplevels(kde_hr_95_sf_gp4$id)

kde_hr_50_sf_gp1 <- kde_hr_50_sf %>% 
  filter(id %in% id_gp_1)
kde_hr_50_sf_gp2 <- kde_hr_50_sf %>% 
  filter(id %in% id_gp_2)
kde_hr_50_sf_gp3 <- kde_hr_50_sf %>% 
  filter(id %in% id_gp_1)
kde_hr_50_sf_gp4 <- kde_hr_50_sf %>% 
  filter(id %in% id_gp_1)

# plot 
tmap_mode("view")

palette_viri = viridis(10, begin = 0, end = 1, direction = 1, option = "plasma")

UDMap_HR_id_gp1 <- tm_shape(RMO) +
  tm_polygons() +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(kde_hr_95_sf_gp1) +  
  tm_lines(col = "id",
             palette = palette_viri) +
  tm_shape(kde_hr_50_sf_gp1) +
  tm_polygons(fill = "id", 
              palette = palette_viri)

UDMap_HR_id_gp2 <- tm_shape(RMO) +
  tm_polygons() +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(kde_hr_95_sf_gp2) +  
  tm_lines(col = "id",
           palette = palette_viri) +
  tm_shape(kde_hr_50_sf_gp2) +
  tm_polygons(fill = "id", 
              palette = palette_viri)

UDMap_HR_id_gp3 <- tm_shape(RMO) +
  tm_polygons() +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(kde_hr_95_sf_gp3) +  
  tm_lines(col = "id",
           palette = palette_viri) +
  tm_shape(kde_hr_50_sf_gp3) +
  tm_polygons(fill = "id", 
              palette = palette_viri)

UDMap_HR_id_gp4 <- tm_shape(RMO) +
  tm_polygons() +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(kde_hr_95_sf_gp4) +  
  tm_lines(col = "id",
           palette = palette_viri) +
  tm_shape(kde_hr_50_sf_gp1) +
  tm_polygons(fill = "id", 
              palette = palette_viri)

UDMap_HR_id <- tmap_arrange(UDMap_HR_id_gp1, UDMap_HR_id_gp2, UDMap_HR_id_gp3, UDMap_HR_id_gp4) ; UDMap_HR_id

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
  tm_polygons(fill = "coverage", fill_alpha = 0.2,
              palette = palette_viri) ; HR_pour_RN

tmap_save(HR_95_pourc_RN, paste0(data_image_path_serveur, "/UDMap_HR_95_pourc_RN.html"), dpi = 600)

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
  tm_polygons(fill = "coverage", fill_alpha = 0.2,
              palette = palette_viri) ; HR_pour_RN

tmap_save(HR_50_pourc_RN, paste0(data_image_path_serveur, "/UDMap_HR_50_pourc_RN.html"), dpi = 600)

mean_hr_50_pourc_rn <- mean(kde_hr_50_sf_2154$coverage, na.rm = T)

print("pourcentage moyen des home range dans la réserve naturelle :")
mean_hr_50_pourc_rn

###
####
# Distance reposoir - alimentation ---------------------------------------------
####
###

## reposoir 50% ----------------------------------------------------------------

# Charger les données en lat/lon (EPSG:4326)
coords_HR_id_repo <- GPS %>% 
  filter(behavior == "roosting") %>%
  dplyr::select(id,lon,lat) %>% 
  st_drop_geometry() %>% 
  na.omit()

# Transformer en objet spatial (EPSG:4326)
locs_HR_id_repo <- st_as_sf(coords_HR_id_repo, coords = c("lon", "lat"), crs = 4326)

# Reprojeter en système métrique (ex. UTM zone 30N - EPSG:32630 pour la France)
locs_HR_id_repo_32630 <- st_transform(locs_HR_id_repo, crs = 32630)  # Adapter le CRS à votre région

# Reprojection du raster
crs_utm <- CRS("+init=epsg:32630") # Définir le CRS cible (EPSG:32630 = UTM zone 30N)
raster_100x100_32630 <- projectRaster(raster_100x100, crs = crs_utm)
crs(raster_100x100_32630)

# Extraire les coordonnées reprojetées
coords_HR_id_repo_32630 <- st_coordinates(locs_HR_id_repo_32630)

# Règle de Silverman
sigma_x_HR_id_repo <- sd(coords_HR_id_repo_32630[,1])  # Écart-type en X (mètres)
sigma_y_HR_id_repo <- sd(coords_HR_id_repo_32630[,2])  # Écart-type en Y (mètres)
n_HR_id_repo <- nrow(coords_HR_id_repo_32630)  # Nombre de points

h_silverman_x_HR_id_repo <- 1.06 * sigma_x_HR_id_repo * n_HR_id_repo^(-1/5)
h_silverman_y_HR_id_repo <- 1.06 * sigma_y_HR_id_repo * n_HR_id_repo^(-1/5)

cat("h optimal en mètres pour X:", h_silverman_x_HR_id_repo, "\n")
cat("h optimal en mètres pour Y:", h_silverman_y_HR_id_repo, "\n")

# locs_spa <- as(locs_m, "Spatial")

# locs_spa <- st_transform(locs_HR_id_repo, crs = 32630)
locs_spa_HR_id_repo <- as(locs_HR_id_repo_32630, "Spatial")

# Appliquer kernelUD avec h estimé par Silverman
kud_HR_id_repo <- kernelUD(locs_spa_HR_id_repo["id"], grid = as(raster_100x100_32630, "SpatialPixels"),
                      h = mean(c(h_silverman_x_HR_id_repo, h_silverman_y_HR_id_repo)))

kde_hr_50_id_repo <- getverticeshr(kud_HR_id_repo, 50)

# Conversion des home range KDE en sf
kde_hr_50_id_repo_sf <- st_as_sf(kde_hr_50_id_repo)

# centroid
repo_centro <- kde_hr_50_id_repo_sf %>% 
  st_centroid()

## alimentation 50% ----------------------------------------------------------------

# Charger les données en lat/lon (EPSG:4326)
coords_HR_id_alim <- GPS %>% 
  filter(behavior == "foraging") %>%
  dplyr::select(id,lon,lat) %>% 
  st_drop_geometry() %>% 
  na.omit()

# Transformer en objet spatial (EPSG:4326)
locs_HR_id_alim <- st_as_sf(coords_HR_id_alim, coords = c("lon", "lat"), crs = 4326)

# Reprojeter en système métrique (ex. UTM zone 30N - EPSG:32630 pour la France)
locs_HR_id_alim_32630 <- st_transform(locs_HR_id_alim, crs = 32630)  # Adapter le CRS à votre région

# Reprojection du raster
crs_utm <- CRS("+init=epsg:32630") # Définir le CRS cible (EPSG:32630 = UTM zone 30N)
raster_100x100_32630 <- projectRaster(raster_100x100, crs = crs_utm)
crs(raster_100x100_32630)

# Extraire les coordonnées reprojetées
coords_HR_id_alim_32630 <- st_coordinates(locs_HR_id_alim_32630)

# Règle de Silverman
sigma_x_HR_id_alim <- sd(coords_HR_id_alim_32630[,1])  # Écart-type en X (mètres)
sigma_y_HR_id_alim <- sd(coords_HR_id_alim_32630[,2])  # Écart-type en Y (mètres)
n_HR_id_alim <- nrow(coords_HR_id_alim_32630)  # Nombre de points

h_silverman_x_HR_id_alim <- 1.06 * sigma_x_HR_id_alim * n_HR_id_alim^(-1/5)
h_silverman_y_HR_id_alim <- 1.06 * sigma_y_HR_id_alim * n_HR_id_alim^(-1/5)

cat("h optimal en mètres pour X:", h_silverman_x_HR_id_alim, "\n")
cat("h optimal en mètres pour Y:", h_silverman_y_HR_id_alim, "\n")

# locs_spa <- as(locs_m, "Spatial")

# locs_spa <- st_transform(locs_HR_id_alim, crs = 32630)
locs_spa_HR_id_alim <- as(locs_HR_id_alim_32630, "Spatial")

# Appliquer kernelUD avec h estimé par Silverman
kud_HR_id_alim <- kernelUD(locs_spa_HR_id_alim["id"], grid = as(raster_100x100_32630, "SpatialPixels"),
                           h = mean(c(h_silverman_x_HR_id_alim, h_silverman_y_HR_id_alim)))

kde_hr_50_id_alim <- getverticeshr(kud_HR_id_alim, 50)

# Conversion des home range KDE en sf
kde_hr_50_id_alim_sf <- st_as_sf(kde_hr_50_id_alim)

# centroid
alim_centro <- kde_hr_50_id_alim_sf %>% 
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

## distance ~ sexe ----

sexe_dt <- GPS %>% 
  st_drop_geometry() %>% 
  dplyr::select(id, sex) %>% 
  na.omit() %>% 
  distinct()

dist_sexe_dt <- repo_alim_centro %>% 
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
  dplyr::select(id, age) %>% 
  na.omit() %>% 
  distinct()

dist_age_dt <- repo_alim_centro %>% 
  left_join(age_dt) %>% 
  na.omit()

# test comparaison de moyenne

shapiro.test(dist_age_dt$dist_repo_alim[dist_age_dt$age == "adult_plus"])
shapiro.test(dist_age_dt$dist_repo_alim[dist_age_dt$age == "adult"]) 
shapiro.test(dist_age_dt$dist_repo_alim[dist_age_dt$age == "juv"])
var.test(dist_age_dt$dist_repo_alim[dist_age_dt$age == "adult_plus"], dist_age_dt$dist_repo_alim[dist_age_dt$age == "adult"])  

comp_moy_age_adult_adult_plus = t.test(dist_age_dt$dist_repo_alim[dist_age_dt$age == "adult_plus"], 
                                       dist_age_dt$dist_repo_alim[dist_age_dt$age == "adult"], 
                                       var.equal=F) ; comp_moy_age_adult_adult_plus

comp_moy_age_juv_adult = t.test(dist_age_dt$dist_repo_alim[dist_age_dt$age == "juv"], 
                                       dist_age_dt$dist_repo_alim[dist_age_dt$age == "adult"], 
                                       var.equal=F) ; comp_moy_age_juv_adult

comp_moy_age_juv_adult_plus = t.test(dist_age_dt$dist_repo_alim[dist_age_dt$age == "juv"], 
                                dist_age_dt$dist_repo_alim[dist_age_dt$age == "adult_plus"], 
                                var.equal=F) ; comp_moy_age_juv_adult_plus

summary(lm(dist_age_dt$dist_repo_alim ~ dist_age_dt$age))

## distance ~ sex + age ----

sexe_age_dt <- GPS %>% 
  st_drop_geometry() %>% 
  dplyr::select(id, sex, age) %>% 
  mutate(sex_age = paste0(sex, "_", age)) %>% 
  na.omit() %>% 
  distinct()

dist_sexe_age_dt <- repo_alim_centro %>% 
  left_join(sexe_age_dt) %>% 
  na.omit()

summary(lm(dist_sexe_age_dt$dist_repo_alim ~ dist_sexe_age_dt$age*dist_sexe_age_dt$sex))

summary(lm(dist_sexe_age_dt$dist_repo_alim ~ dist_sexe_age_dt$sex_age))





# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! ----------------------------------------------

## ~ month ---------------------------------------------------------------------

raster_100x100 <- raster(grid_100x100, resolution=100, crs="EPSG:2154")

UDMap_month = NULL
# list_UDMap_mini <- list()

month <- unique(sort(as.numeric(month(GPS$date))))

# m = 1
# tmap_mode("plot")

for(m in month){
  print(m)
  
  # data point GPS
  GPS_m <- GPS %>%
    filter(month(date)== m) 
  GPS_m_2154 <- st_transform(GPS_m, crs = 2154)
  GPS_m_2154 <- as(GPS_m_2154, "Spatial")
  # UD map
  UDMap_m <- kernelUD(GPS_m_2154, grid = as(raster_100x100, "SpatialPixels"))
  UDMap_m_rast <- rast(UDMap_m)
  UDMap_m_courtour <- as.contour(UDMap_m_rast)
  UDMap_m_sf <- st_as_sf(UDMap_m_courtour)
  UDMap_m <- st_cast(UDMap_m_sf, "POLYGON")
  UDMap_m$month = m
  # all info 
  UDMap_month <- rbind(UDMap_month, UDMap_m)
  
}

tmap_mode("plot")
grid_month_map <- tm_scale_bar() +
  tm_shape(RMO) +
  tm_polygons(border.col = "NA", col = "darkgreen", alpha = 0.3) +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(UDMap_month) + 
  tm_polygons(border.col = "grey", col = "level", alpha = 0.2, 
              palette = viridis(10, begin = 0, end = 1, 
                                direction = 1, option = "plasma")) +
  tm_facets(by = c("month")); grid_month_map

tmap_save(grid_month_map, paste0(data_image_path_serveur, "/grid_month_map.png"), dpi = 600)

tmap_mode("view")
grid_month_map <- tm_scale_bar() +
  tm_shape(RMO) +
  tm_polygons(border.col = "NA", col = "darkgreen", alpha = 0.3) +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(UDMap_month) + 
  tm_polygons(border.col = "grey", col = "level", alpha = 0.2, 
              palette = viridis(10, begin = 0, end = 1, 
                                direction = 1, option = "plasma")) +
  tm_facets(by = c("month")); grid_month_map

## ~ behavior ---------------------------------------------------------------------

### h init (~ 500) ----

raster_100x100 <- raster(grid_100x100_mini, resolution=100, crs="EPSG:2154")

UDMap = NULL

behavior <- unique(points$behavior)

# m = 1
# tmap_mode("plot")

for(b in behavior){
  print(b)
  
  # data point GPS
  GPS_b <- points %>%
    filter(behavior== b) 
  GPS_b_2154 <- st_transform(GPS_b, crs = 2154)
  GPS_b_2154 <- as(GPS_b_2154, "Spatial")
  # UD map
  UDMap_b <- kernelUD(GPS_b_2154, grid = as(raster_100x100, "SpatialPixels"))
  UDMap_b_rast <- rast(UDMap_b)
  UDMap_b_courtour <- as.contour(UDMap_b_rast)
  UDMap_b_sf <- st_as_sf(UDMap_b_courtour)
  UDMap_b <- st_cast(UDMap_b_sf, "POLYGON")
  UDMap_b$behavior = b
  # all info 
  UDMap <- rbind(UDMap, UDMap_b)
  
}

tmap_mode("plot")
grid_map <- tm_scale_bar() +
  tm_shape(RMO) +
  tm_polygons(border.col = "NA", col = "darkgreen", alpha = 0.3) +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(UDMap) + 
  tm_polygons(border.col = "grey", col = "level", alpha = 0.2, 
              palette = viridis(10, begin = 0, end = 1, 
                                direction = 1, option = "plasma")) +
  tm_facets(by = c("behavior")); grid_map

tmap_save(grid_map, paste0(data_image_path_serveur, "/grid_behav_map.png"), dpi = 600)

tmap_mode("view")
grid_map <- tm_scale_bar() +
  tm_shape(RMO) +
  tm_polygons(border.col = "NA", col = "darkgreen", alpha = 0.3) +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(UDMap) + 
  tm_polygons(border.col = "grey", col = "level", alpha = 0.2, 
              palette = viridis(10, begin = 0, end = 1, 
                                direction = 1, option = "plasma")) +
  tm_facets(by = c("behavior")); grid_map







### h = 100 ----

raster_100x100 <- raster(grid_100x100_mini, resolution=100, crs="EPSG:2154")

UDMap_behavior_h100 = NULL
# list_UDMap_mini <- list()

behavior <- unique(points$behavior)

# m = 1
# tmap_mode("plot")

for(b in behavior){
  print(b)
  
  # data point GPS
  GPS_b <- points %>%
    filter(behavior== b) 
  GPS_b_2154 <- st_transform(GPS_b, crs = 2154)
  GPS_b_2154 <- as(GPS_b_2154, "Spatial")
  # UD map
  UDMap_b <- kernelUD(GPS_b_2154, grid = as(raster_100x100, "SpatialPixels"), h = 100)
  UDMap_b_rast <- rast(UDMap_b)
  UDMap_b_courtour <- as.contour(UDMap_b_rast)
  UDMap_b_sf <- st_as_sf(UDMap_b_courtour)
  UDMap_b <- st_cast(UDMap_b_sf, "POLYGON")
  UDMap_b$behavior = b
  # all info 
  UDMap_behavior_h100 <- rbind(UDMap_behavior_h100, UDMap_b)
  
}

tmap_mode("view")
grid_behavior_h100_map <- tm_scale_bar() +
  tm_shape(RMO) +
  tm_polygons(border.col = "NA", col = "darkgreen", alpha = 0.3) +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(UDMap_behavior_h100) + 
  tm_polygons(border.col = "grey", col = "level", alpha = 0.2, 
              palette = viridis(10, begin = 0, end = 1, 
                                direction = 1, option = "plasma")) +
  tm_facets(by = c("behavior")); grid_behavior_h100_map

### h = 200 ----

raster_100x100 <- raster(grid_100x100_mini, resolution=100, crs="EPSG:2154")

UDMap_behavior_h200 = NULL
# list_UDMap_mini <- list()

behavior <- unique(points$behavior)

# m = 1
# tmap_mode("plot")

for(b in behavior){
  print(b)
  
  # data point GPS
  GPS_b <- points %>%
    filter(behavior== b) 
  GPS_b_2154 <- st_transform(GPS_b, crs = 2154)
  GPS_b_2154 <- as(GPS_b_2154, "Spatial")
  # UD map
  UDMap_b <- kernelUD(GPS_b_2154, grid = as(raster_100x100, "SpatialPixels"), h = 200)
  UDMap_b_rast <- rast(UDMap_b)
  UDMap_b_courtour <- as.contour(UDMap_b_rast)
  UDMap_b_sf <- st_as_sf(UDMap_b_courtour)
  UDMap_b <- st_cast(UDMap_b_sf, "POLYGON")
  UDMap_b$behavior = b
  # all info 
  UDMap_behavior_h200 <- rbind(UDMap_behavior_h200, UDMap_b)
  
}

tmap_mode("view")
grid_behavior_h200_map <- tm_scale_bar() +
  tm_shape(RMO) +
  tm_polygons(border.col = "NA", col = "darkgreen", alpha = 0.3) +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(UDMap_behavior_h200) + 
  tm_polygons(border.col = "grey", col = "level", alpha = 0.2, 
              palette = viridis(10, begin = 0, end = 1, 
                                direction = 1, option = "plasma")) +
  tm_facets(by = c("behavior")); grid_behavior_h200_map


### h = 300 ----

raster_100x100 <- raster(grid_100x100_mini, resolution=100, crs="EPSG:2154")

UDMap_behavior_h300 = NULL
# list_UDMap_mini <- list()

behavior <- unique(points$behavior)

# m = 1
# tmap_mode("plot")

for(b in behavior){
  print(b)
  
  # data point GPS
  GPS_b <- points %>%
    filter(behavior== b) 
  GPS_b_2154 <- st_transform(GPS_b, crs = 2154)
  GPS_b_2154 <- as(GPS_b_2154, "Spatial")
  # UD map
  UDMap_b <- kernelUD(GPS_b_2154, grid = as(raster_100x100, "SpatialPixels"), h = 300)
  UDMap_b_rast <- rast(UDMap_b)
  UDMap_b_courtour <- as.contour(UDMap_b_rast)
  UDMap_b_sf <- st_as_sf(UDMap_b_courtour)
  UDMap_b <- st_cast(UDMap_b_sf, "POLYGON")
  UDMap_b$behavior = b
  # all info 
  UDMap_behavior_h300 <- rbind(UDMap_behavior_h300, UDMap_b)
  
}

tmap_mode("view")
grid_behavior_h300_map <- tm_scale_bar() +
  tm_shape(RMO) +
  tm_polygons(border.col = "NA", col = "darkgreen", alpha = 0.3) +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(UDMap_behavior_h300) + 
  tm_polygons(border.col = "grey", col = "level", alpha = 0.2, 
              palette = viridis(10, begin = 0, end = 1, 
                                direction = 1, option = "plasma")) +
  tm_facets(by = c("behavior")); grid_behavior_h300_map

### h = 700 ----

raster_100x100 <- raster(grid_100x100_mini, resolution=100, crs="EPSG:2154")

UDMap_behavior_h700 = NULL
# list_UDMap_mini <- list()

behavior <- unique(points$behavior)

# m = 1
# tmap_mode("plot")

for(b in behavior){
  print(b)
  
  # data point GPS
  GPS_b <- points %>%
    filter(behavior== b) 
  GPS_b_2154 <- st_transform(GPS_b, crs = 2154)
  GPS_b_2154 <- as(GPS_b_2154, "Spatial")
  # UD map
  UDMap_b <- kernelUD(GPS_b_2154, grid = as(raster_100x100, "SpatialPixels"), h = 700)
  UDMap_b_rast <- rast(UDMap_b)
  UDMap_b_courtour <- as.contour(UDMap_b_rast)
  UDMap_b_sf <- st_as_sf(UDMap_b_courtour)
  UDMap_b <- st_cast(UDMap_b_sf, "POLYGON")
  UDMap_b$behavior = b
  # all info 
  UDMap_behavior_h700 <- rbind(UDMap_behavior_h700, UDMap_b)
  
}

tmap_mode("view")
grid_behavior_h700_map <- tm_scale_bar() +
  tm_shape(RMO) +
  tm_polygons(border.col = "NA", col = "darkgreen", alpha = 0.3) +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(UDMap_behavior_h700) + 
  tm_polygons(border.col = "grey", col = "level", alpha = 0.2, 
              palette = viridis(10, begin = 0, end = 1, 
                                direction = 1, option = "plasma")) +
  tm_facets(by = c("behavior")); grid_behavior_h700_map

### + sex ------------------------------------------------------------

raster_100x100 <- raster(grid_100x100_mini, resolution=100, crs="EPSG:2154")

# data
UDMap_behavior_sex = NULL
# HomeRange <- NULL

# variables
behavior <- unique(points$behavior)
GPS_sex_noNA <- GPS_2 %>% 
  na.omit(sex)
sex <- unique(GPS_sex_noNA$sex)

# plot mode
tmap_mode("plot")

# UDMap loop
for(b in behavior){
  print(b)
  
  for(s in sex){
    print(s)
    
    # data point GPS
    GPS_b_s <- GPS_2 %>%
      filter(behavior == b & sex == s) 
    GPS_b_s_2154 <- st_transform(GPS_b_s, crs = 2154)
    GPS_b_s_2154 <- as(GPS_b_s_2154, "Spatial")
    # UD map
    UDMap_b_s <- kernelUD(GPS_b_s_2154, grid = as(raster_100x100, "SpatialPixels"))
    UDMap_b_s_rast <- rast(UDMap_b_s)
    UDMap_b_s_courtour <- as.contour(UDMap_b_s_rast)
    UDMap_b_s_sf <- st_as_sf(UDMap_b_s_courtour)
    UDMap_b_s <- st_cast(UDMap_b_s_sf, "POLYGON")
    UDMap_b_s$behavior = b
    UDMap_b_s$sex = s
    
    # all info 
    UDMap_behavior_sex <- rbind(UDMap_behavior_sex, UDMap_b_s)
    
  }
  
}

tmap_mode("view")
grid_behavior_sex_map <- tm_scale_bar() +
  tm_shape(RMO) +
  tm_polygons(border.col = "NA", col = "darkgreen", alpha = 0.3) +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(UDMap_behavior_sex) + 
  tm_polygons(border.col = "grey", col = "level", alpha = 0.2, 
              palette = viridis(10, begin = 0, end = 1, 
                                direction = 1, option = "plasma")) +
  tm_facets(by = c("behavior","sex")); grid_behavior_sex_map

# h = 300

raster_100x100 <- raster(grid_100x100_mini, resolution=100, crs="EPSG:2154")

# data
UDMap_h_300_behavior_sex = NULL
# HomeRange <- NULL

# variables
behavior <- unique(points$behavior)
GPS_sex_noNA <- GPS_2 %>% 
  na.omit(sex)
sex <- unique(GPS_sex_noNA$sex)

# plot mode
tmap_mode("plot")

# UDMap loop
for(b in behavior){
  print(b)
  
  for(s in sex){
    print(s)
    
    # data point GPS
    GPS_b_s <- GPS_2 %>%
      filter(behavior == b & sex == s) 
    GPS_b_s_2154 <- st_transform(GPS_b_s, crs = 2154)
    GPS_b_s_2154 <- as(GPS_b_s_2154, "Spatial")
    # UD map
    UDMap_b_s <- kernelUD(GPS_b_s_2154, grid = as(raster_100x100, "SpatialPixels"))
    UDMap_b_s_rast <- rast(UDMap_b_s)
    UDMap_b_s_courtour <- as.contour(UDMap_b_s_rast)
    UDMap_b_s_sf <- st_as_sf(UDMap_b_s_courtour)
    UDMap_b_s <- st_cast(UDMap_b_s_sf, "POLYGON")
    UDMap_b_s$behavior = b
    UDMap_b_s$sex = s
    
    # all info 
    UDMap_h_300_behavior_sex <- rbind(UDMap_h_300_behavior_sex, UDMap_b_s)
    
  }
  
}

tmap_mode("view")
grid_h_300_behavior_sex_map <- tm_scale_bar() +
  tm_shape(RMO) +
  tm_polygons(border.col = "NA", col = "darkgreen", alpha = 0.3) +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(UDMap_h_300_behavior_sex) + 
  tm_polygons(border.col = "grey", col = "level", alpha = 0.2, 
              palette = viridis(10, begin = 0, end = 1, 
                                direction = 1, option = "plasma")) +
  tm_facets(by = c("behavior","sex")); grid_h_300_behavior_sex_map









# home range 
raster_100x100 <- raster(grid_100x100, resolution=1000, crs="EPSG:2154")

GPS_2_2154 <- st_transform(GPS_2, crs = 2154)
GPS_2_2154 <- as(GPS_2_2154, "Spatial")
UDMap_1 <- kernelUD(GPS_2_2154, grid = as(raster_100x100, "SpatialPixels"))
# crs(raster_100x100)
# crs(GPS_2_2154)
HR_1 <- getverticeshr(UDMap_1, 95)



beep()





### + age ------------------------------------------------------------

raster_100x100 <- raster(grid_100x100_mini, resolution=100, crs="EPSG:2154")

# data
UDMap_behavior_age = NULL

# variables
behavior <- unique(points$behavior)
GPS_age_noNA <- GPS_2 %>% 
  na.omit(age_baguage)
age <- unique(GPS_age_noNA$age_baguage)

# plot mode
tmap_mode("plot")

# UDMap loop
for(b in behavior){
  print(b)
  
  for(a in age){
    print(a)
    
    # data point GPS
    GPS_b_a <- GPS_2 %>%
      filter(behavior == b & age_baguage == a) 
    GPS_b_a_2154 <- st_transform(GPS_b_a, crs = 2154)
    GPS_b_a_2154 <- as(GPS_b_a_2154, "Spatial")
    # UD map
    UDMap_b_a <- kernelUD(GPS_b_a_2154, grid = as(raster_100x100, "SpatialPixels"))
    UDMap_b_a_rast <- rast(UDMap_b_a)
    UDMap_b_a_courtour <- as.contour(UDMap_b_a_rast)
    UDMap_b_a_sf <- st_as_sf(UDMap_b_a_courtour)
    UDMap_b_a <- st_cast(UDMap_b_a_sf, "POLYGON")
    UDMap_b_a$behavior = b
    UDMap_b_a$age = a
    
    # all info 
    UDMap_behavior_age <- rbind(UDMap_behavior_age, UDMap_b_a)
    
  }
  
}

tmap_mode("view")
grid_behavior_age_map <- tm_scale_bar() +
  tm_shape(RMO) +
  tm_polygons(border.col = "NA", col = "darkgreen", alpha = 0.3) +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(UDMap_behavior_age) + 
  tm_polygons(border.col = "grey", col = "level", alpha = 0.2, 
              palette = viridis(10, begin = 0, end = 1, 
                                direction = 1, option = "plasma")) +
  tm_facets(by = c("behavior","age")); grid_behavior_age_map

# h 300

raster_100x100 <- raster(grid_100x100_mini, resolution=100, crs="EPSG:2154")

# data
UDMap_h_300_behavior_age = NULL

# variables
behavior <- unique(points$behavior)
GPS_age_noNA <- GPS_2 %>% 
  na.omit(age_baguage)
age <- unique(GPS_age_noNA$age_baguage)

# plot mode
tmap_mode("plot")

# UDMap loop
for(b in behavior){
  print(b)
  
  for(a in age){
    print(a)
    
    # data point GPS
    GPS_b_a <- GPS_2 %>%
      filter(behavior == b & age_baguage == a) 
    GPS_b_a_2154 <- st_transform(GPS_b_a, crs = 2154)
    GPS_b_a_2154 <- as(GPS_b_a_2154, "Spatial")
    # UD map
    UDMap_b_a <- kernelUD(GPS_b_a_2154, grid = as(raster_100x100, "SpatialPixels"))
    UDMap_b_a_rast <- rast(UDMap_b_a)
    UDMap_b_a_courtour <- as.contour(UDMap_b_a_rast)
    UDMap_b_a_sf <- st_as_sf(UDMap_b_a_courtour)
    UDMap_b_a <- st_cast(UDMap_b_a_sf, "POLYGON")
    UDMap_b_a$behavior = b
    UDMap_b_a$age = a
    
    # all info 
    UDMap_h_300_behavior_age <- rbind(UDMap_h_300_behavior_age, UDMap_b_a)
    
  }
  
}

tmap_mode("view")
grid_h_300_behavior_age_map <- tm_scale_bar() +
  tm_shape(RMO) +
  tm_polygons(border.col = "NA", col = "darkgreen", alpha = 0.3) +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(UDMap_h_300_behavior_age) + 
  tm_polygons(border.col = "grey", col = "level", alpha = 0.2, 
              palette = viridis(10, begin = 0, end = 1, 
                                direction = 1, option = "plasma")) +
  tm_facets(by = c("behavior","age")); grid_h_300_behavior_age_map

### + year ------------------------------------------------------------

raster_100x100 <- raster(grid_100x100_mini, resolution=100, crs="EPSG:2154")

# data
UDMap_behavior_year = NULL

# variables
behavior <- unique(points$behavior)
GPS_year_noNA <- GPS_2 %>% 
  na.omit(year_baguage)
year <- unique(GPS_year_noNA$year_baguage)

# plot mode
tmap_mode("plot")

# UDMap loop
for(b in behavior){
  print(b)
  
  for(a in year){
    print(a)
    
    # data point GPS
    GPS_b_a <- GPS_2 %>%
      filter(behavior == b & year_baguage == a) 
    GPS_b_a_2154 <- st_transform(GPS_b_a, crs = 2154)
    GPS_b_a_2154 <- as(GPS_b_a_2154, "Spatial")
    # UD map
    UDMap_b_a <- kernelUD(GPS_b_a_2154, grid = as(raster_100x100, "SpatialPixels"))
    UDMap_b_a_rast <- rast(UDMap_b_a)
    UDMap_b_a_courtour <- as.contour(UDMap_b_a_rast)
    UDMap_b_a_sf <- st_as_sf(UDMap_b_a_courtour)
    UDMap_b_a <- st_cast(UDMap_b_a_sf, "POLYGON")
    UDMap_b_a$behavior = b
    UDMap_b_a$year = a
    
    # all info 
    UDMap_behavior_year <- rbind(UDMap_behavior_year, UDMap_b_a)
    
  }
  
}

tmap_mode("view")
grid_behavior_year_map <- tm_scale_bar() +
  tm_shape(RMO) +
  tm_polygons(border.col = "NA", col = "darkgreen", alpha = 0.3) +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(UDMap_behavior_year) + 
  tm_polygons(border.col = "grey", col = "level", alpha = 0.2, 
              palette = viridis(10, begin = 0, end = 1, 
                                direction = 1, option = "plasma")) +
  tm_facets(by = c("behavior","year")); grid_behavior_year_map

# h 300

raster_100x100 <- raster(grid_100x100_mini, resolution=100, crs="EPSG:2154")

# data
UDMap_h_300_behavior_year = NULL

# variables
behavior <- unique(points$behavior)
GPS_year_noNA <- GPS_2 %>% 
  na.omit(year_baguage)
year <- unique(GPS_year_noNA$year_baguage)

# plot mode
tmap_mode("plot")

# UDMap loop
for(b in behavior){
  print(b)
  
  for(a in year){
    print(a)
    
    # data point GPS
    GPS_b_a <- GPS_2 %>%
      filter(behavior == b & year_baguage == a) 
    GPS_b_a_2154 <- st_transform(GPS_b_a, crs = 2154)
    GPS_b_a_2154 <- as(GPS_b_a_2154, "Spatial")
    # UD map
    UDMap_b_a <- kernelUD(GPS_b_a_2154, grid = as(raster_100x100, "SpatialPixels"))
    UDMap_b_a_rast <- rast(UDMap_b_a)
    UDMap_b_a_courtour <- as.contour(UDMap_b_a_rast)
    UDMap_b_a_sf <- st_as_sf(UDMap_b_a_courtour)
    UDMap_b_a <- st_cast(UDMap_b_a_sf, "POLYGON")
    UDMap_b_a$behavior = b
    UDMap_b_a$year = a
    
    # all info 
    UDMap_h_300_behavior_year <- rbind(UDMap_h_300_behavior_year, UDMap_b_a)
    
  }
  
}

tmap_mode("view")
grid_h_300_behavior_year_map <- tm_scale_bar() +
  tm_shape(RMO) +
  tm_polygons(border.col = "NA", col = "darkgreen", alpha = 0.3) +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(UDMap_h_300_behavior_year) + 
  tm_polygons(border.col = "grey", col = "level", alpha = 0.2, 
              palette = viridis(10, begin = 0, end = 1, 
                                direction = 1, option = "plasma")) +
  tm_facets(by = c("behavior","year")); grid_h_300_behavior_year_map

## ~ sex ---------------------------------------------------------------------

raster_100x100 <- raster(grid_100x100_mini, resolution=100, crs="EPSG:2154")

UDMap_sex = NULL

GPS_sex_noNA <- GPS_2 %>% 
  na.omit(sex)

sex <- unique(GPS_sex_noNA$sex)

for(s in sex){
  print(s)
  
  # data point GPS
  GPS_s <- GPS_sex_noNA %>%
    filter(sex == s) 
  GPS_s_2154 <- st_transform(GPS_s, crs = 2154)
  GPS_s_2154 <- as(GPS_s_2154, "Spatial")
  # UD map
  UDMap_s <- kernelUD(GPS_s_2154, grid = as(raster_100x100, "SpatialPixels"))
  UDMap_s_rast <- rast(UDMap_s)
  UDMap_s_courtour <- as.contour(UDMap_s_rast)
  UDMap_s_sf <- st_as_sf(UDMap_s_courtour)
  UDMap_s <- st_cast(UDMap_s_sf, "POLYGON")
  UDMap_s$sex = s
  # all info 
  UDMap_sex <- rbind(UDMap_sex, UDMap_s)
  
}

tmap_mode("view")
grid_sex_map <- tm_scale_bar() +
  tm_shape(RMO) +
  tm_polygons(border.col = "NA", col = "darkgreen", alpha = 0.3) +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(UDMap_sex) + 
  tm_polygons(border.col = "grey", col = "level", alpha = 0.2, 
              palette = viridis(10, begin = 0, end = 1, 
                                direction = 1, option = "plasma")) +
  tm_facets(by = c("sex")); grid_sex_map

# h 300

raster_100x100 <- raster(grid_100x100_mini, resolution=100, crs="EPSG:2154")

UDMap_h_300_sex = NULL

GPS_sex_noNA <- GPS_2 %>% 
  na.omit(sex)

sex <- unique(GPS_sex_noNA$sex)

for(s in sex){
  print(s)
  
  # data point GPS
  GPS_s <- GPS_sex_noNA %>%
    filter(sex == s) 
  GPS_s_2154 <- st_transform(GPS_s, crs = 2154)
  GPS_s_2154 <- as(GPS_s_2154, "Spatial")
  # UD map
  UDMap_s <- kernelUD(GPS_s_2154, grid = as(raster_100x100, "SpatialPixels"))
  UDMap_s_rast <- rast(UDMap_s)
  UDMap_s_courtour <- as.contour(UDMap_s_rast)
  UDMap_s_sf <- st_as_sf(UDMap_s_courtour)
  UDMap_s <- st_cast(UDMap_s_sf, "POLYGON")
  UDMap_s$sex = s
  # all info 
  UDMap_h_300_sex <- rbind(UDMap_h_300_sex, UDMap_s)
  
}

tmap_mode("view")
grid_h_300_sex_map <- tm_scale_bar() +
  tm_shape(RMO) +
  tm_polygons(border.col = "NA", col = "darkgreen", alpha = 0.3) +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(UDMap_sex) + 
  tm_polygons(border.col = "grey", col = "level", alpha = 0.2, 
              palette = viridis(10, begin = 0, end = 1, 
                                direction = 1, option = "plasma")) +
  tm_facets(by = c("sex")); grid_h_300_sex_map

### + age ------------------------------------------------------------

raster_100x100 <- raster(grid_100x100_mini, resolution=100, crs="EPSG:2154")

# data
UDMap_sex_age = NULL

# variables
GPS_sex_noNA <- GPS_2 %>% 
  na.omit(sex)
sex <- unique(GPS_sex_noNA$sex)

GPS_age_noNA <- GPS_2 %>% 
  na.omit(age_baguage)
age <- unique(GPS_age_noNA$age_baguage)

# plot mode
tmap_mode("plot")

# UDMap loop
for(b in sex){
  print(b)
  
  for(a in age){
    print(a)
    
    # data point GPS
    GPS_b_a <- GPS_2 %>%
      filter(sex == b & age_baguage == a) 
    GPS_b_a_2154 <- st_transform(GPS_b_a, crs = 2154)
    GPS_b_a_2154 <- as(GPS_b_a_2154, "Spatial")
    # UD map
    UDMap_b_a <- kernelUD(GPS_b_a_2154, grid = as(raster_100x100, "SpatialPixels"))
    UDMap_b_a_rast <- rast(UDMap_b_a)
    UDMap_b_a_courtour <- as.contour(UDMap_b_a_rast)
    UDMap_b_a_sf <- st_as_sf(UDMap_b_a_courtour)
    UDMap_b_a <- st_cast(UDMap_b_a_sf, "POLYGON")
    UDMap_b_a$sex = b
    UDMap_b_a$age = a
    
    # all info 
    UDMap_sex_age <- rbind(UDMap_sex_age, UDMap_b_a)
    
  }
  
}

tmap_mode("view")
grid_sex_age_map <- tm_scale_bar() +
  tm_shape(RMO) +
  tm_polygons(border.col = "NA", col = "darkgreen", alpha = 0.3) +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(UDMap_sex_age) + 
  tm_polygons(border.col = "grey", col = "level", alpha = 0.2, 
              palette = viridis(10, begin = 0, end = 1, 
                                direction = 1, option = "plasma")) +
  tm_facets(by = c("sex","age")); grid_sex_age_map

## ~ age ---------------------------------------------------------------------

raster_100x100 <- raster(grid_100x100_mini, resolution=100, crs="EPSG:2154")

UDMap_age = NULL

GPS_age_noNA <- GPS_2 %>% 
  na.omit(age_baguage)

age <- unique(GPS_age_noNA$age_baguage)

for(a in age){
  print(a)
  
  # data point GPS
  GPS_a <- GPS_age_noNA %>%
    filter(age_baguage == a) 
  GPS_a_2154 <- st_transform(GPS_a, crs = 2154)
  GPS_a_2154 <- as(GPS_a_2154, "Spatial")
  # UD map
  UDMap_a <- kernelUD(GPS_a_2154, grid = as(raster_100x100, "SpatialPixels"))
  UDMap_a_rast <- rast(UDMap_a)
  UDMap_a_courtour <- as.contour(UDMap_a_rast)
  UDMap_a_sf <- st_as_sf(UDMap_a_courtour)
  UDMap_a <- st_cast(UDMap_a_sf, "POLYGON")
  UDMap_a$age_baguage = a
  # all info 
  UDMap_age <- rbind(UDMap_age, UDMap_a)
  
}

tmap_mode("view")
grid_age_map <- tm_scale_bar() +
  tm_shape(RMO) +
  tm_polygons(border.col = "NA", col = "darkgreen", alpha = 0.3) +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(UDMap_age) + 
  tm_polygons(border.col = "grey", col = "level", alpha = 0.2, 
              palette = viridis(10, begin = 0, end = 1, 
                                direction = 1, option = "plasma")) +
  tm_facets(by = c("age_baguage")); grid_age_map

## ~ year ----------------------------------------------------------------------

raster_100x100 <- raster(grid_100x100_mini, resolution=100, crs="EPSG:2154")

UDMap_year = NULL

GPS_year_noNA <- GPS_2 %>% 
  na.omit(year_baguage)

year <- unique(GPS_year_noNA$year_baguage)

for(y in year){
  print(y)
  
  # data point GPS
  GPS_y <- GPS_year_noNA %>%
    filter(year_baguage == y) 
  GPS_y_2154 <- st_transform(GPS_y, crs = 2154)
  GPS_y_2154 <- as(GPS_y_2154, "Spatial")
  # UD map
  UDMap_y <- kernelUD(GPS_y_2154, grid = as(raster_100x100, "SpatialPixels"))
  UDMap_y_rast <- rast(UDMap_y)
  UDMap_y_courtour <- as.contour(UDMap_y_rast)
  UDMap_y_sf <- st_as_sf(UDMap_y_courtour)
  UDMap_y <- st_cast(UDMap_y_sf, "POLYGON")
  UDMap_y$year_baguage = y
  # all info 
  UDMap_year <- rbind(UDMap_year, UDMap_y)
  
}

tmap_mode("view")
grid_year_map <- tm_scale_bar() +
  tm_shape(RMO) +
  tm_polygons(border.col = "NA", col = "darkgreen", alpha = 0.3) +
  tm_text("NOM_SITE", size = 1) +
  tm_shape(UDMap_year) + 
  tm_polygons(border.col = "grey", col = "level", alpha = 0.2, 
              palette = viridis(10, begin = 0, end = 1, 
                                direction = 1, option = "plasma")) +
  tm_facets(by = c("year_baguage")); grid_year_map

beep()




































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
#   geom_point(aes(x = XlongDD, y = YlatDD, size = pop_size_best), data = XYcolonies, fill=rgb(128, 128, 128,max=255), shape=19, col="#0d72a6", alpha=1)+
#   scale_size(breaks=IntCol$brks[c(2:7)],guide="legend", label=IntCol$brks[c(2:7)], range=c(2,15), name="Nb. pairs")+
#   geom_point(aes(x = XlongDD, y = YlatDD, size = pop_size_best), data = XYcolonies, col=rgb(128, 128, 128,max=255), shape=1, alpha=1)+
#   geom_point(aes(x = XlongDD, y = YlatDD, size = pop_size_best), data = XYcolonies[is.na(XYcolonies$NbTracks)==0,], col="#F55F19", shape=1, alpha=1,show.legend=FALSE)+
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
#   geom_point(aes(x = XlongDD, y = YlatDD, size = pop_size_best), data = XYcolonies, fill=rgb(128, 128, 128,max=255), shape=19, col="#0d72a6", alpha=1)+
#   scale_size(breaks=IntCol$brks[c(2:7)],guide="legend", label=IntCol$brks[c(2:7)], range=c(2,15), name="Nb. couples")+
#   geom_point(aes(x = XlongDD, y = YlatDD, size = pop_size_best), data = XYcolonies, col=rgb(128, 128, 128,max=255), shape=1, alpha=1)+
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
#   geom_point(aes(x = XlongDD, y = YlatDD, size = pop_size_best), data = XYcolonies, fill=rgb(128, 128, 128,max=255), shape=19, col="#4f4f4f", alpha=1)+
#   scale_size(breaks=IntCol$brks[c(2:7)],guide="legend", label=IntCol$brks[c(2:7)], range=c(2,15), name="Nb. pairs")+
#   geom_point(aes(x = XlongDD, y = YlatDD, size = pop_size_best), data = XYcolonies, col=rgb(128, 128, 128,max=255), shape=1, alpha=1)+
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
#   geom_point(aes(x = XlongDD, y = YlatDD, size = pop_size_best), data = XYcolonies, fill=rgb(128, 128, 128,max=255), shape=19, col="black", alpha=1)+
#   scale_size(breaks=IntCol$brks[c(2:7)],guide="legend", label=IntCol$brks[c(2:7)], range=c(2,15), name="Nb. pairs")+
#   geom_point(aes(x = XlongDD, y = YlatDD, size = pop_size_best), data = XYcolonies, col=rgb(128, 128, 128,max=255), shape=1, alpha=1)+
#   geom_point(aes(x = XlongDD, y = YlatDD, size = pop_size_best), data = XYcolonies[is.na(XYcolonies$NbTracks)==0,], col="cyan", shape=1, alpha=1,show.legend=FALSE,stroke = 1.5)+
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
#                 DateTime = date, 
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
#                            date = YStracks$DateTime,
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
     alpha=0.8,
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
       alpha=0.8,
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

