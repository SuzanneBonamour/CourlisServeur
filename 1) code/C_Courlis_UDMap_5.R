############################################################################ ---
# 1. Starting block ------------------------------------------------------------
############################################################################ ---

# --- objectif ---
# fonction qui contient tous les packages, paramètres généraux, functions créées,
# données et autre infos utile pour l'ensemble du script, à lancer en début de session :)

library(beepr)

# beep lorsqu'il y a une erreur
options(error = function() {
  beep(7)
}) # options(error = NULL), pour enlver le beep

# Nettoyage de l'environnement
rm(list = ls())

# time zone
library(lubridate)
with_tz(Sys.time(), "Europe/Paris")

options(scipen = 999)

## packages ------------------------------------------------------------------

# 1. Définir le chemin local où vous avez les droits
local_lib <- "C:/Users/Suzanne.Bonamour/Rlibs"

# 2. Créer le dossier s'il n'existe pas
dir.create(local_lib, showWarnings = FALSE, recursive = TRUE)

# 3. Ajouter ce chemin à la liste des bibliothèques
.libPaths(local_lib)

# 4. Liste des packages à installer
packages <- c(
  "tidyverse", "terra", "sf", "adehabitatLT", "raster", "tmap",
  "adehabitatHR", "viridis", "beepr", "readxl", "marmap", "pals",
  "stars", "ggcorrplot", "tibble", "paletteer", "ggeffects",
  "lmerTest", "ggthemes", "broom.mixed", "performance", "ggpubr",
  "maptiles", "ggnewscale", "tinter", "furrr", "purrr", "future.apply"
)

# 5. Identifier ceux qui ne sont pas encore installés dans local_lib
not_installed <- packages[!packages %in% installed.packages(lib.loc = local_lib)[, "Package"]]

# 6. Installer les packages manquants en version binaire
if (length(not_installed) > 0) {
  install.packages(not_installed, lib = local_lib, type = "binary")
} else {
  message("✅ Tous les packages sont déjà installés dans ", local_lib)
}

# 7. Charger les packages
invisible(lapply(packages, function(pkg) {
  library(pkg, character.only = TRUE, lib.loc = local_lib)
}))
## paramètres généraux -------------------------------------------------------

# résolution des grid pour analyses (10 m)
resolution_ZOOM <- 10

# palettes de couleurs
palette_viri <- viridis::viridis(10, begin = 0, end = 1, direction = 1, option = "plasma")

palette_grey <- paletteer_c("grDevices::Grays", 10)
palette_roosting <- paletteer_c("grDevices::Sunset", 10)
palette_foraging <- paletteer_c("grDevices::YlGnBu", 10)
nom_pal_roosting <- "grDevices::Sunset"
nom_pal_foraging <- "grDevices::YlGnBu"

couleur_roosting <- "#9A7AA0"
couleur_foraging <- "#E08E45"

couleur_roosting_param_2 <- c(lighten("#9A7AA0", 0.5), darken("#9A7AA0", 0.25))
couleur_foraging_param_2 <- c(lighten("#E08E45", 0.2), darken("#E08E45", 0.25))

# reverse of %in%
`%ni%` <- Negate(`%in%`)

# Liste des niveaux de zoom
zoom_level <- c("A", "B", "C")

## chemins -------------------------------------------------------------------

data_path <- "D:/Projets_Suzanne/Courlis/3) Data/1) data/"
data_generated_path <- "D:/Projets_Suzanne/Courlis/3) Data/2) data_generated/"
data_image_path <- "D:/Projets_Suzanne/Courlis/3) Data/3) images/"
data_view_map_path <- "D:/Projets_Suzanne/Courlis/3) Data/4) view_map/"
atlas_path <- "D:/Projets_Suzanne/Courlis/Atlas_Courlis/"

## font de carte -------------------------------------------------------------

# Réserve naturelle Moeze Oléron ---
reserve <- st_read(paste0(data_path, "Réserve_naturelle/rnn/rnn/N_ENP_RNN_S_000.shp"))
RMO <- reserve[reserve$NOM_SITE == "Moëze-Oléron", ]
rm(reserve)

# Zone d'étude globale ---
# BOX <- st_as_sf(st_as_sfc(st_bbox(c(xmin = -1.26, xmax = -0.945, ymax = 46.01, ymin = 45.78), crs = st_crs(4326))))
# st_write(BOX, paste0(data_generated_path, "BOX.gpkg"), append = FALSE)
BOX <- st_read(paste0(data_generated_path, "BOX.gpkg"))
BOX_4326 <- st_transform(BOX, crs = 4326) # Transformation de la boîte au CRS 4326 (coordonnées géographiques)
st_write(BOX_4326, paste0(data_generated_path, "BOX_4326.gpkg"), append = FALSE)
BOX_2154 <- st_transform(BOX, crs = 2154) # Transformation de la boîte au CRS 2154 (coordonnées géographiques)

area_box <- st_area(BOX)
area_box_km <- area_box / 1000000

# Zones zoom ---
ZOOM_A <- st_transform(st_as_sf(st_as_sfc(st_bbox(c(xmin = -1.245, xmax = -1.18, ymax = 45.975, ymin = 45.825), crs = st_crs(4326)))), crs = 2154)
ZOOM_B <- st_transform(st_as_sf(st_as_sfc(st_bbox(c(xmin = -1.13, xmax = -1.06, ymax = 45.975, ymin = 45.923), crs = st_crs(4326)))), crs = 2154)
ZOOM_C <- st_transform(st_as_sf(st_as_sfc(st_bbox(c(xmin = -1.15, xmax = -1.04, ymax = 45.923, ymin = 45.865), crs = st_crs(4326)))), crs = 2154)
ZOOM_D <- st_transform(st_as_sf(st_as_sfc(st_bbox(c(xmin = -1.18, xmax = -1.08, ymax = 45.865, ymin = 45.795), crs = st_crs(4326)))), crs = 2154)
ZOOM_E <- st_transform(st_as_sf(st_as_sfc(st_bbox(c(xmin = -0.95, xmax = -1.08, ymax = 45.865, ymin = 45.795), crs = st_crs(4326)))), crs = 2154)
st_write(ZOOM_A, paste0(data_generated_path, "ZOOM_A.gpkg"), append = FALSE)
st_write(ZOOM_B, paste0(data_generated_path, "ZOOM_B.gpkg"), append = FALSE)
st_write(ZOOM_C, paste0(data_generated_path, "ZOOM_C.gpkg"), append = FALSE)
st_write(ZOOM_D, paste0(data_generated_path, "ZOOM_D.gpkg"), append = FALSE)
st_write(ZOOM_E, paste0(data_generated_path, "ZOOM_E.gpkg"), append = FALSE)

ZOOM <- rbind(ZOOM_A, ZOOM_B, ZOOM_C, ZOOM_D, ZOOM_E)
ZOOM$name <- c("A", "B", "C", "D", "E")
ZOOM <- ZOOM %>%
  rename(geometry = x)

# zoom égrandi nouvelle méthode 95% 50%
ZOOM_A <- st_transform(st_as_sf(st_as_sfc(st_bbox(c(xmin = -1.26, xmax = -1.18, ymax = 46.01, ymin = 45.78), crs = st_crs(4326)))), crs = 2154)
ZOOM_B <- st_transform(st_as_sf(st_as_sfc(st_bbox(c(xmin = -1.18, xmax = -1.045, ymax = 46.01, ymin = 45.865), crs = st_crs(4326)))), crs = 2154)
ZOOM_C <- st_transform(st_as_sf(st_as_sfc(st_bbox(c(xmin = -1.18, xmax = -0.945, ymax = 45.865, ymin = 45.78), crs = st_crs(4326)))), crs = 2154)
st_write(ZOOM_A, paste0(data_generated_path, "ZOOM_A.gpkg"), append = FALSE)
st_write(ZOOM_B, paste0(data_generated_path, "ZOOM_B.gpkg"), append = FALSE)
st_write(ZOOM_C, paste0(data_generated_path, "ZOOM_C.gpkg"), append = FALSE)

ZOOM <- rbind(ZOOM_A, ZOOM_B, ZOOM_C)
ZOOM$name <- c("A", "B", "C")
ZOOM <- ZOOM %>%
  rename(geometry = x)

# limite terre mer ---
terre_mer <- st_read(paste0(data_path, "Limite_terre_mer/Limite_terre-mer_facade_Manche_Atlantique_ligne.shp"))
crs(terre_mer)
terre_mer <- st_transform(terre_mer, crs = 4326)
terre_mer <- st_intersection(terre_mer, BOX_4326)

# nom de site ---
labels_ZOOM <- data.frame(
  name = c(
    "Ors", "Pointe d'Oulme", "Pointe des Doux",
    "Arceau", "Les Palles", "Fort Vasoux",
    "Ferme aquacole", "Montportail", "Travers",
    "Grand cimétière", "Petit Matton", "Ile de Nôle",
    "Prise de l'Epée"
  ),
  x = c(
    373400, 374200, 374000,
    371145, 379600, 384500,
    380000, 384400, 384350,
    384000, 386000, 377300,
    384000
  ),
  y = c(
    6537900, 6539250, 6543200,
    6546600, 6549700, 6548800,
    6547350, 6545650, 6541650,
    6541000, 6537500, 6535500,
    6532500
  )
)

labels_ZOOM$ZOOM <- c(
  "A", "A", "A",
  "A", "B", "B",
  "B", "B", "C",
  "C", "E", "D", "E"
)

labels_ZOOM <- st_as_sf(labels_ZOOM, coords = c("x", "y"), crs = 2154)
labels_ZOOM_A <- labels_ZOOM[labels_ZOOM$ZOOM == "A", ]
labels_ZOOM_B <- labels_ZOOM[labels_ZOOM$ZOOM == "B", ]
labels_ZOOM_C <- labels_ZOOM[labels_ZOOM$ZOOM == "C", ]
labels_ZOOM_D <- labels_ZOOM[labels_ZOOM$ZOOM == "D", ]
labels_ZOOM_E <- labels_ZOOM[labels_ZOOM$ZOOM == "E", ]

labels_ZOOM_A <- labels_ZOOM[labels_ZOOM$ZOOM == "A", ]
labels_ZOOM_B <- labels_ZOOM[labels_ZOOM$ZOOM == "B", ]
labels_ZOOM_C <- labels_ZOOM[labels_ZOOM$ZOOM == "C" | labels_ZOOM$ZOOM == "D" | labels_ZOOM$ZOOM == "E", ]

# Site de baguage
site_baguage <- data.frame(
  name = c("Site de baguage"),
  lon = -1.082390, lat = 45.895373
)
site_baguage <- st_as_sf(site_baguage, coords = c("lon", "lat"), crs = 4326)
site_baguage <- st_transform(site_baguage, 2154)
site_baguage$icone <- "📍"

## données GPS ---------------------------------------------------------------

# GPS <- st_read(file.path(data_generated_path, "GPS_clean.gpkg"))
GPS <- st_read(file.path(data_generated_path, "GPS_clean_afterGwen.gpkg"))
# GPS <- st_as_sf(GPS, coords = c("lon", "lat"), crs = 4326)

# variables temporelles additionnelles
GPS$y_m_d <- ymd(as.Date(GPS$datetime))
GPS$month_numeric <- month(as.Date(GPS$datetime))
GPS$month_label <- as.character(lubridate::month(as.Date(GPS$datetime), label = TRUE, abbr = TRUE))
GPS$week <- week(as.Date(GPS$datetime))
GPS$year <- year(as.Date(GPS$datetime))

# GPS_2154 <- st_transform(GPS, crs = 2154)


## grilles -------------------------------------------------------------------

# INPN grille ---

grid <- st_read(paste0(data_path, "INPN_grid/METROP_L932X2.shp"))
# grid_crop <- st_crop(grid, BOX_2154)
# st_write(grid_crop, paste0(data_generated_path, "grid_crop.gpkg"), append = FALSE)
grid_crop <- st_read(paste0(data_generated_path, "grid_crop.gpkg"))

## 100x100 m ---

# offset_point <- st_bbox(grid[grid$CD_SIG=="2kmL93E370N6528",])[c("xmin", "ymin")] ; offset_point
# grid_100x100 <- st_make_grid(BOX_2154, cellsize = 100, offset = offset_point)
# st_write(grid_100x100, paste0(data_generated_path, "grid_100x100.gpkg"), append = FALSE)
grid_100x100 <- st_read(paste0(data_generated_path, "grid_100x100.gpkg"))
raster_100x100 <- rast(grid_100x100, resolution = 100, crs = "EPSG:2154")

# offset_point <- st_bbox(grid[grid$CD_SIG=="2kmL93E370N6528",])[c("xmin", "ymin")] ; offset_point
# grid_10x10 <- st_make_grid(BOX_2154, cellsize = 10, offset = offset_point)
# st_write(grid_10x10, paste0(data_generated_path, "grid_10x10.gpkg"), append = FALSE)
# grid_10x10 <- st_read(paste0(data_generated_path, "grid_10x10.gpkg"))
# raster_10x10 <- rast(grid_10x10, resolution = 10, crs="EPSG:2154")
# saveRDS(raster_10x10, file = paste0(data_generated_path, "raster_10x10.rds"))
raster_10x10 <- readRDS(paste0(data_generated_path, "raster_10x10.rds"))

## 10x10 m ---

# # zoom A ---
# # offset_point_ZOOM_A <- st_bbox(grid[grid$CD_SIG=="2kmL93E372N6534",])[c("xmin", "ymin")] - c(1500 * 1, 0)
# # grid_ZOOM_A <- st_make_grid(ZOOM_A, cellsize = resolution_ZOOM, offset = offset_point_ZOOM_A)
# # st_write(grid_ZOOM_A, paste0(data_generated_path, "grid_ZOOM_A.gpkg"), append = FALSE)
# # grid_ZOOM_A <- st_read(paste0(data_generated_path, "grid_ZOOM_A.gpkg"))
# # raster_ZOOM_A <- rast(grid_ZOOM_A, resolution = resolution_ZOOM, crs="EPSG:2154")
# # saveRDS(raster_ZOOM_A, file = paste0(data_generated_path, "raster_ZOOM_A.rds"))
# raster_ZOOM_A <- readRDS(paste0(data_generated_path, "raster_ZOOM_A.rds"))
# 
# # zoom B ---
# # offset_point_ZOOM_B <- st_bbox(grid[grid$CD_SIG=="2kmL93E382N6544",])[c("xmin", "ymin")] - c(2000 * 1, 0)
# # grid_ZOOM_B <- st_make_grid(ZOOM_B, cellsize = resolution_ZOOM, offset = offset_point_ZOOM_B)
# # st_write(grid_ZOOM_B, paste0(data_generated_path, "grid_ZOOM_B.gpkg"), append = FALSE)
# # grid_ZOOM_B <- st_read(paste0(data_generated_path, "grid_ZOOM_B.gpkg"))
# # raster_ZOOM_B <- rast(grid_ZOOM_B, resolution = resolution_ZOOM, crs="EPSG:2154")
# # saveRDS(raster_ZOOM_B, file = paste0(data_generated_path, "raster_ZOOM_B.rds"))
# raster_ZOOM_B <- readRDS(paste0(data_generated_path, "raster_ZOOM_B.rds"))
# 
# # zoom C ---
# # offset_point_ZOOM_C <- st_bbox(grid[grid$CD_SIG=="2kmL93E380N6538",])[c("xmin", "ymin")]
# # offset_point_ZOOM_C <- st_bbox(grid[grid$CD_SIG=="2kmL93E380N6538",])[c("xmin", "ymin")] - c(2000 * 1, 0)
# # grid_ZOOM_C <- st_make_grid(ZOOM_C, cellsize = resolution_ZOOM, offset = offset_point_ZOOM_C)
# # st_write(grid_ZOOM_C, paste0(data_generated_path, "grid_ZOOM_C.gpkg"), append = FALSE)
# # grid_ZOOM_C <- st_read(paste0(data_generated_path, "grid_ZOOM_C.gpkg"))
# # raster_ZOOM_C <- rast(grid_ZOOM_C, resolution = resolution_ZOOM, crs="EPSG:2154")
# # saveRDS(raster_ZOOM_C, file = paste0(data_generated_path, "raster_ZOOM_C.rds"))
# raster_ZOOM_C <- readRDS(paste0(data_generated_path, "raster_ZOOM_C.rds"))
# 
# # zoom D ---
# # offset_point_ZOOM_D <- st_bbox(grid[grid$CD_SIG=="2kmL93E376N6532",])[c("xmin", "ymin")]
# # grid_ZOOM_D <- st_make_grid(ZOOM_D, cellsize = resolution_ZOOM, offset = offset_point_ZOOM_D)
# # st_write(grid_ZOOM_D, paste0(data_generated_path, "grid_ZOOM_D.gpkg"), append = FALSE)
# # grid_ZOOM_D <- st_read(paste0(data_generated_path, "grid_ZOOM_D.gpkg"))
# # raster_ZOOM_D <- rast(grid_ZOOM_D, resolution = resolution_ZOOM, crs="EPSG:2154")
# # saveRDS(raster_ZOOM_D, file = paste0(data_generated_path, "raster_ZOOM_D.rds"))
# raster_ZOOM_D <- readRDS(paste0(data_generated_path, "raster_ZOOM_D.rds"))
# 
# # zoom E ---
# # offset_point_ZOOM_E <- st_bbox(grid[grid$CD_SIG=="2kmL93E384N6530",])[c("xmin", "ymin")] - c(500*1,0)
# # grid_ZOOM_E <- st_make_grid(ZOOM_E, cellsize = resolution_ZOOM, offset = offset_point_ZOOM_E)
# # st_write(grid_ZOOM_E, paste0(data_generated_path, "grid_ZOOM_E.gpkg"), append = FALSE)
# # grid_ZOOM_E <- st_read(paste0(data_generated_path, "grid_ZOOM_E.gpkg"))
# # raster_ZOOM_E <- rast(grid_ZOOM_E, resolution = resolution_ZOOM, crs="EPSG:2154")
# # saveRDS(raster_ZOOM_E, file = paste0(data_generated_path, "raster_ZOOM_E.rds"))
# raster_ZOOM_E <- readRDS(paste0(data_generated_path, "raster_ZOOM_E.rds"))

# new zone 95% 50%
# zoom A ---
offset_point_ZOOM_A <- st_bbox(grid[grid$CD_SIG=="2kmL93E370N6528",])[c("xmin", "ymin")] - c(2000 * 0.3, 0)
grid_ZOOM_A <- st_make_grid(ZOOM_A, cellsize = resolution_ZOOM, offset = offset_point_ZOOM_A)
st_write(grid_ZOOM_A, paste0(data_generated_path, "grid_ZOOM_A.gpkg"), append = FALSE)
grid_ZOOM_A <- st_read(paste0(data_generated_path, "grid_ZOOM_A.gpkg"))
raster_ZOOM_A <- rast(grid_ZOOM_A, resolution = resolution_ZOOM, crs="EPSG:2154")
saveRDS(raster_ZOOM_A, file = paste0(data_generated_path, "raster_ZOOM_A.rds"))
raster_ZOOM_A <- readRDS(paste0(data_generated_path, "raster_ZOOM_A.rds"))

# zoom B ---
offset_point_ZOOM_B <- st_bbox(grid[grid$CD_SIG=="2kmL93E380N6538",])[c("xmin", "ymin")] - c(2000 * 2, 0)
grid_ZOOM_B <- st_make_grid(ZOOM_B, cellsize = resolution_ZOOM, offset = offset_point_ZOOM_B)
st_write(grid_ZOOM_B, paste0(data_generated_path, "grid_ZOOM_B.gpkg"), append = FALSE)
grid_ZOOM_B <- st_read(paste0(data_generated_path, "grid_ZOOM_B.gpkg"))
raster_ZOOM_B <- rast(grid_ZOOM_B, resolution = resolution_ZOOM, crs="EPSG:2154")
saveRDS(raster_ZOOM_B, file = paste0(data_generated_path, "raster_ZOOM_B.rds"))
raster_ZOOM_B <- readRDS(paste0(data_generated_path, "raster_ZOOM_B.rds"))

# zoom C ---
offset_point_ZOOM_C <- st_bbox(grid[grid$CD_SIG=="2kmL93E376N6528",])[c("xmin", "ymin")] - c(2000 * 0.2, 0)
grid_ZOOM_C <- st_make_grid(ZOOM_C, cellsize = resolution_ZOOM, offset = offset_point_ZOOM_C)
st_write(grid_ZOOM_C, paste0(data_generated_path, "grid_ZOOM_C.gpkg"), append = FALSE)
grid_ZOOM_C <- st_read(paste0(data_generated_path, "grid_ZOOM_C.gpkg"))
raster_ZOOM_C <- rast(grid_ZOOM_C, resolution = resolution_ZOOM, crs="EPSG:2154")
saveRDS(raster_ZOOM_C, file = paste0(data_generated_path, "raster_ZOOM_C.rds"))
raster_ZOOM_C <- readRDS(paste0(data_generated_path, "raster_ZOOM_C.rds"))

# tmap_mode("view")
# zone_map <- tm_scalebar() +
#   tm_basemap(c("OpenStreetMap", "Esri.WorldImagery", "CartoDB.Positron")) +
#   tm_shape(terre_mer) +
#   tm_lines(col = "#32B7FF", lwd = 0.5) +
#   tm_shape(BOX_2154) +
#   tm_borders(col = "#575757") +
#   tm_shape(raster_ZOOM_C) +
#   tm_raster(col ="red") +
#   tm_shape(ZOOM) +
#   tm_polygons(fill = "#575757", alpha = 0.1, col = "#575757", lty = "dotted", size = 3) +
#   tm_labels("name", size = 1, col = "#575757", just = "center") +
#   # tm_shape(raster_ZOOM_A) +
#   # tm_raster(col ="pink") +
#   # tm_shape(raster_ZOOM_C) +
#   # tm_raster(col ="red") +
#   tm_shape(grid_crop) +
#   tm_polygons(col ="green", alpha = 0.5)
#   zone_map

## zone ------------------------------------------------------------------------

# Convertir GPS en objet sf
# GPS_sf <- st_as_sf(GPS, coords = c("lon", "lat"), crs = 4326)  # WGS84

ZOOM_A$zone <- "A"
ZOOM_B$zone <- "B"
ZOOM_C$zone <- "C"

zones_sf <- rbind(ZOOM_A, ZOOM_B, ZOOM_C)

# GPS_sf <- st_as_sf(GPS, coords = c("lon","lat"), crs = 4326)   # GPS en WGS84
GPS_sf <- st_as_sf(GPS, coords = c("lon", "lat"), crs = 4326) %>% 
  mutate(lon = st_coordinates(.)[, 1], lat = st_coordinates(.)[, 2])
GPS_sf <- st_transform(GPS_sf, crs = st_crs(zones_sf))         # transformer pour matcher les zones (EPSG:2154)

GPS_sf <- st_join(GPS_sf, zones_sf[, "zone"], left = TRUE)

table(GPS_sf$zone)

GPS <- GPS_sf

head(GPS)

## functions -----------------------------------------------------------------

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

# vérification du CRS des object spatiaux

verif_crs <- function(objet_sf) {
  if (st_crs(objet_sf)$epsg != 4326) {
    beepr::beep(2) # Émet un son d'alerte
    stop("Le CRS n'est pas 4326 !")
  }
}

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

# vérification de la time zone pour les dates et heures

# vérification pour l'orignateur, la session r
verif_tz_sys <- function() {
  if (Sys.timezone() != "UTC") {
    beepr::beep(2)
    stop("La timezone du système n'est pas UTC !")
  }
}

# vérification pour le jeu de données
verif_tz <- function(objet, colonne) {
  if (!colonne %in% names(objet)) {
    stop(paste("La colonne", colonne, "n'existe pas dans l'objet !"))
  }

  tz <- attr(objet[[colonne]], "tzone") # Récupérer la timezone

  if (is.null(tz) || tz != "UTC") {
    beepr::beep(2) # Émet un son d'alerte
    stop(paste("La colonne", colonne, "n'est pas en UTC !"))
  }
}

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

# pour charger et fusionner les fichiers CSV d'un dossier

telecharger_donnees <- function(chemin) {
  fichiers <- list.files(path = chemin, pattern = "*.csv", full.names = TRUE)
  donnees <- lapply(fichiers, fread, sep = ",")
  return(rbindlist(donnees))
}

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

# estimation des kernelUD (utilisation distribution map),
# zone A, B, C, D, E independemment

make_kud <- function(analyse, zoom_levels, comportement, GPS_sampled, data_generated_path, resolution_ZOOM, couleur) {
  message("Analyse : ", analyse, " | Zoom : ", zoom_levels)

  library(sf)
  library(dplyr)
  library(adehabitatHR)
  library(terra)
  library(tmap)
  library(raster)

  crs_utm <- "EPSG:32630"

  # nom de site ---
  labels_ZOOM <- data.frame(
    name = c(
      "Ors", "Pointe d'Oulme", "Pointe des Doux",
      "Arceau", "Les Palles", "Fort Vasoux",
      "Ferme aquacole", "Montportail", "Travers",
      "Grand cimétière", "Petit Matton", "Ile de Nôle",
      "Prise de l'Epée"
    ),
    x = c(
      373400, 374200, 374000,
      371145, 379600, 384500,
      380000, 384400, 384350,
      384000, 386000, 377300,
      384000
    ),
    y = c(
      6537900, 6539250, 6543200,
      6546600, 6549700, 6548800,
      6547350, 6545650, 6541650,
      6541000, 6537500, 6535500,
      6532500
    )
  )

  labels_ZOOM$ZOOM <- c(
    "A", "A", "A",
    "A", "B", "B",
    "B", "B", "C",
    "C", "E", "D", "E"
  )

  labels_ZOOM <- st_as_sf(labels_ZOOM, coords = c("x", "y"), crs = 2154)
  labels_ZOOM_A <- labels_ZOOM[labels_ZOOM$ZOOM == "A", ]
  labels_ZOOM_B <- labels_ZOOM[labels_ZOOM$ZOOM == "B", ]
  labels_ZOOM_C <- labels_ZOOM[labels_ZOOM$ZOOM == "C", ]
  labels_ZOOM_D <- labels_ZOOM[labels_ZOOM$ZOOM == "D", ]
  labels_ZOOM_E <- labels_ZOOM[labels_ZOOM$ZOOM == "E", ]

  labels_ZOOM_A <- labels_ZOOM[labels_ZOOM$ZOOM == "A", ]
  labels_ZOOM_B <- labels_ZOOM[labels_ZOOM$ZOOM == "B", ]
  labels_ZOOM_C <- labels_ZOOM[labels_ZOOM$ZOOM == "C" |
    labels_ZOOM$ZOOM == "D" |
    labels_ZOOM$ZOOM == "E", ]

  ### 1. Charger et filtrer les données GPS_sampled ###
  ZOOM_shape <- st_read(paste0(data_generated_path, "ZOOM_", zoom_levels, ".gpkg"), quiet = TRUE) %>%
    st_transform(crs = 4326)
  # ZOOM_shape <- st_read(paste0(data_generated_path, "ZOOM_", "A", ".gpkg"), quiet = TRUE) %>%
  #   st_transform(crs = 4326)

  GPS_sampled.ZOOM <- st_intersection(GPS_sampled, ZOOM_shape)

  GPS_sampled.behavior <- GPS_sampled.ZOOM %>%
    filter(behavior == comportement) %>%
    st_drop_geometry() %>%
    dplyr::select(lon, lat, ID, datetime) %>%
    na.omit()

  if (nrow(GPS_sampled.behavior) < 1) {
    warning("Pas assez de points pour ", zoom_levels)
    return(NULL)
  }

  GPS_sampled_spa.behavior <- GPS_sampled.behavior %>%
    st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
    st_transform(crs = crs_utm)

  GPS_sampled_coords.behavior <- st_coordinates(GPS_sampled_spa.behavior)

  ### 2. Calculer le raster de base ###
  grid <- st_read(paste0(data_generated_path, "grid_ZOOM_", zoom_levels, ".gpkg"), quiet = TRUE)
  raster_terra <- rast(grid, resolution = resolution_ZOOM, crs = "EPSG:2154")
  spatRaster <- project(raster_terra, crs_utm)
  spatialPixels <- as(raster(spatRaster), "SpatialPixels")

  ### 3. Calculer bande passante KDE ###
  nb <- nrow(GPS_sampled_coords.behavior)
  h <- mean(c(sd(GPS_sampled_coords.behavior[, 1]), sd(GPS_sampled_coords.behavior[, 2]))) * 1.06 * nb^(-1 / 5) / 2

  ### 4. KernelUD ###
  # kud <- kernelUD(as_Spatial(GPS_sampled_spa.behavior), grid = spatialPixels, h = h)
  kud <- kernelUD(as_Spatial(GPS_sampled_spa.behavior), grid = 1000, h = h)
  
  iso_list <- lapply(c(95, 50), function(p) {
    st_as_sf(getverticeshr(kud, percent = p)) %>%
      mutate(level = p)
  })

  results_kud <- do.call(rbind, iso_list) %>%
    mutate(ZOOM = zoom_levels, h = h)

  ### 5. Statistiques ###
  nb_ind_point_dt <- GPS_sampled.behavior %>%
    group_by(ID) %>%
    summarise(n = n(), .groups = "drop") %>%
    mutate(zoom = zoom_levels)

  ### 6. Carte interactive ###
  # zoom_obj <- get(paste0("ZOOM_", zoom_levels))
  zoom_obj <- st_read(paste0(data_generated_path, "ZOOM_", zoom_levels, ".gpkg"), quiet = TRUE)
  if (is.null(zoom_obj) || nrow(zoom_obj) == 0) stop("zoom_obj vide")

  Box <- st_bbox(zoom_obj)

  point_top_left <- st_sfc(st_point(c(Box["xmin"] + 1000, Box["ymax"] - 500)), crs = st_crs(zoom_obj))
  label_point <- st_sf(label = zoom_levels, geometry = point_top_left)

  nb_ind <- nrow(nb_ind_point_dt)
  nb_point <- sum(nb_ind_point_dt$n)
  info_text <- paste0(nb_point, " points / ", nb_ind, " individus / ", "h = ", h)

  point_text_info <- st_sfc(st_point(c(Box["xmin"] + 1000, Box["ymax"] - 1000)), crs = st_crs(zoom_obj))
  info_label_point <- st_sf(label = info_text, geometry = point_text_info)

  labels_zoom <- get(paste0("labels_ZOOM_", zoom_levels))
  # labels_zoom <- st_read(paste0(data_generated_path, "labels_ZOOM_", zoom_levels, ".gpkg"), quiet = TRUE)

  data_95 <- results_kud %>% filter(level == 95)
  # data_90 <- results_kud %>% filter(level == 90)
  data_50 <- results_kud %>% filter(level == 50)

  map <- tm_basemap(c("OpenStreetMap", "Esri.WorldImagery", "CartoDB.Positron")) +
    tm_shape(data_95) + tm_polygons(border.col = "white", col = couleur, alpha = 0.1) +
    # tm_shape(data_90) + tm_polygons(border.col = "white", col = couleur, alpha = 0.6) +
    tm_shape(data_50) + tm_polygons(border.col = "white", col = couleur, alpha = 0.8) +
    tm_shape(zoom_obj) + tm_borders(col = "#575757", lty = "dotted", lwd = 3) +
    tm_shape(label_point) + tm_text("label", col = "#575757", size = 3, just = c("left", "top")) +
    # tm_shape(terre_mer) + tm_lines(col = "lightblue", lwd = 0.1) +
    tm_shape(labels_zoom) + tm_text("name", size = 1, col = "#575757", fontface = "bold", just = "left") +
    tm_shape(site_baguage) + tm_text("icone", size = 1.5) +
    tm_credits(info_text,
      position = c("left", "bottom"), size = 1,
      col = "black", bg.color = "white", bg.alpha = 0.7, fontface = "bold"
    )

  tmap_save(map, paste0(atlas_path, "UDMap_", analyse, "_", comportement, "_", zoom_levels, ".html"))

  return(list(
    kud_sf = results_kud,
    stats = nb_ind_point_dt,
    map = map
  ))
  
  on.exit({
    while (!is.null(dev.list())) dev.off()
  }, add = TRUE)
  
}

# fonctionnel

# zoom_levels <- c("A", "B", "C")
# results_kud <- NULL
# nb_kud <- NULL
# analyse <- "make_kud_opti_test"
# comportement <- "roosting"
# couleur <- couleur_roosting
# 
# library(future.apply)
# 
# plan(multisession, workers = 3)
# 
# results_list <- future_lapply(
#   zoom_levels,
#   function(z) {
#     make_kud(analyse, z, comportement, GPS, data_generated_path, resolution_ZOOM, couleur)
#   },
#   future.seed = TRUE # garantit des tirages aléatoires reproductibles et indépendants
# )

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

# estimation des kernelUD (utilisation distribution map),
# zone A, B, C, D, E independemment, 
# en fonction d'un paramètre (age, sexe, marée, etc)

make_kud_param <- function(analyse, zoom_levels, comportement, GPS, data_generated_path, resolution_ZOOM, couleurs, param) {
  message("Analyse : ", analyse, " | Zoom : ", zoom_levels)
  
  library(sf)
  library(dplyr)
  library(adehabitatHR)
  library(terra)
  library(tmap)
  library(raster)
  
  crs_utm <- "EPSG:32630"
  
  # nom de site ---
  labels_ZOOM <- data.frame(
    name = c(
      "Ors", "Pointe d'Oulme", "Pointe des Doux",
      "Arceau", "Les Palles", "Fort Vasoux",
      "Ferme aquacole", "Montportail", "Travers",
      "Grand cimétière", "Petit Matton", "Ile de Nôle",
      "Prise de l'Epée"
    ),
    x = c(
      373400, 374200, 374000,
      371145, 379600, 384500,
      380000, 384400, 384350,
      384000, 386000, 377300,
      384000
    ),
    y = c(
      6537900, 6539250, 6543200,
      6546600, 6549700, 6548800,
      6547350, 6545650, 6541650,
      6541000, 6537500, 6535500,
      6532500
    )
  )
  
  labels_ZOOM$ZOOM <- c(
    "A", "A", "A",
    "A", "B", "B",
    "B", "B", "C",
    "C", "E", "D", "E"
  )
  
  labels_ZOOM <- st_as_sf(labels_ZOOM, coords = c("x", "y"), crs = 2154)
  labels_ZOOM_A <- labels_ZOOM[labels_ZOOM$ZOOM == "A", ]
  labels_ZOOM_B <- labels_ZOOM[labels_ZOOM$ZOOM == "B", ]
  labels_ZOOM_C <- labels_ZOOM[labels_ZOOM$ZOOM == "C", ]
  labels_ZOOM_D <- labels_ZOOM[labels_ZOOM$ZOOM == "D", ]
  labels_ZOOM_E <- labels_ZOOM[labels_ZOOM$ZOOM == "E", ]
  
  labels_ZOOM_A <- labels_ZOOM[labels_ZOOM$ZOOM == "A", ]
  labels_ZOOM_B <- labels_ZOOM[labels_ZOOM$ZOOM == "B", ]
  labels_ZOOM_C <- labels_ZOOM[labels_ZOOM$ZOOM == "C" |
                                  labels_ZOOM$ZOOM == "D" |
                                  labels_ZOOM$ZOOM == "E", ]
  
  ### 1. Charger et filtrer les données GPS ###
  ZOOM_shape <- st_read(paste0(data_generated_path, "ZOOM_", zoom_levels, ".gpkg"), quiet = TRUE) %>%
    st_transform(crs = 4326)
  # ZOOM_shape <- st_read(paste0(data_generated_path, "ZOOM_", "A", ".gpkg"), quiet = TRUE) %>%
  #   st_transform(crs = 4326)
  
  GPS.ZOOM <- st_intersection(GPS, ZOOM_shape)
  
  GPS.behavior <- GPS.ZOOM %>%
    filter(behavior == comportement) %>%
    st_drop_geometry() %>%
    dplyr::select(lon, lat, ID, datetime, param) %>%
    na.omit()
  
  if (nrow(GPS.behavior) < 1) {
    warning("Pas assez de points pour ", zoom_levels)
    return(NULL)
  }

  # au moins 5 point par group
  n_per <- GPS.behavior %>%
    group_by(!!sym(param)) %>%
    summarize(n = n())%>%
    filter(n <= 5)
  
  GPS.behavior <- GPS.behavior %>%
    filter(!(!!sym(param) %in% pull(n_per, !!sym(param))))
  
  if (nrow(GPS.behavior) == 0) {
    return(NULL)
  }
  
  GPS_spa <- st_as_sf(GPS.behavior, coords = c("lon", "lat"), crs = 4326)
  GPS_spa <- st_transform(GPS_spa, crs = 32630) 
  GPS_coords.behavior <- st_coordinates(GPS_spa)
  
  ### 2. Calculer le raster de base ###
  grid <- st_read(paste0(data_generated_path, "grid_ZOOM_", zoom_levels, ".gpkg"), quiet = TRUE)
  # grid <- st_read(paste0(data_generated_path, "grid_ZOOM_", "A", ".gpkg"), quiet = TRUE)
  
  raster_terra <- rast(grid, resolution = resolution_ZOOM, crs = "EPSG:2154")
  spatRaster <- project(raster_terra, crs_utm)
  spatialPixels <- as(raster(spatRaster), "SpatialPixels")
  
  ### 3. Calculer bande passante KDE ###
  nb <- nrow(GPS_coords.behavior)
  h <- mean(c(sd(GPS_coords.behavior[, 1]), sd(GPS_coords.behavior[, 2]))) * 1.06 * nb^(-1 / 5) / 2
  
  ### 4. KernelUD ###
  library(future)
  plan(sequential)
  # kud <- kernelUD(as_Spatial(GPS_spa[param]), grid = spatialPixels, h = h)
  kud <- kernelUD(as_Spatial(GPS_spa[param]), grid = 1000, h = h, same4all = TRUE)
  
  # iso_list <- lapply(c(95, 90, 50), function(p) {
  #   st_as_sf(getverticeshr(kud[[param]], percent = p)) %>%
  #     mutate(level = p)
  # })
  
  # iso_list <- lapply(c(95, 90, 50), function(p) {
  #   st_as_sf(getverticeshr(kud[[as.character(param)]], percent = p)) %>%
  #     mutate(level = p)
  # })
  
  # iso_list <- lapply(names(kud), function(id) {
  #   lapply(c(95, 90, 50), function(p) {
  #     st_as_sf(getverticeshr(kud[[id]], percent = p)) %>%
  #       mutate(level = p, id = id)
  #   }) %>% bind_rows()
  # }) %>% bind_rows()
  
  iso_list <- lapply(names(kud), function(cat_age) {
    lapply(c(95, 50), function(p) {
      st_as_sf(getverticeshr(kud[[cat_age]], percent = p)) %>%
        mutate(level = p,
               param = cat_age)  # <-- ici on crée la colonne correcte
    }) %>% bind_rows()
  }) %>% bind_rows()
  
  
  
  
  # results_kud <- do.call(rbind, iso_list) %>%
  #   mutate(ZOOM = zoom_levels, h = h)
   
  # results_kud <- do.call(rbind, iso_list) %>%
  #   mutate(ZOOM = "A", h = h)
  
  
  results_kud <- iso_list %>%
    mutate(ZOOM = zoom_levels, h = h)
  
  # results_kud <- iso_list %>%
  #   mutate(ZOOM = "A", h = h)
  
  
  results_kud$param <- as.factor(results_kud$param)

  ### 5. Statistiques ###
  # nb_ind_point_dt <- GPS.behavior %>%
  #   group_by(ID) %>%
  #   summarise(n = n(), .groups = "drop") %>%
  #   mutate(zoom = zoom_levels)
  
  
  
  # nb ind & point
  nb_ind_point_dt <- GPS.behavior %>%
    # filter(behavior == comportement) %>%
    dplyr::group_by(ID, .data[[param]]) %>%
    dplyr::select(ID, param = .data[[param]], datetime) %>%
    st_drop_geometry() %>%
    na.omit() %>%
    summarise(n = n()) %>%
    mutate(zoom = zoom_levels)

  if (nrow(nb_ind_point_dt) == 0) {
    return(NULL)
  }

  # nb ind & point
  nb_kud <- rbind(nb_kud, nb_ind_point_dt)
  nb_kud <- nb_ind_point_dt
  
  
  
  ### 6. Carte interactive ###
  # zoom_obj <- get(paste0("ZOOM_", zoom_levels))
  zoom_obj <- st_read(paste0(data_generated_path, "ZOOM_", zoom_levels, ".gpkg"), quiet = TRUE)
  # zoom_obj <- st_read(paste0(data_generated_path, "ZOOM_", "A", ".gpkg"), quiet = TRUE)
  if (is.null(zoom_obj) || nrow(zoom_obj) == 0) stop("zoom_obj vide")
  
  Box <- st_Bbox(zoom_obj)
  
  point_top_left <- st_sfc(st_point(c(Box["xmin"] + 1000, Box["ymax"] - 500)), crs = st_crs(zoom_obj))
  label_point <- st_sf(label = zoom_levels, geometry = point_top_left)
  # label_point <- st_sf(label = "A", geometry = point_top_left)
  
  nb_ind <- nrow(nb_ind_point_dt)
  nb_point <- sum(nb_ind_point_dt$n)
  nb_point_min_per_ind <- min(nb_ind_point_dt$n)
  nb_point_max_per_ind <- max(nb_ind_point_dt$n)
  info_text <- paste0(nb_point, " points", 
                      " (min = ", nb_point_min_per_ind, ", max = ", nb_point_max_per_ind, " pts par ind) / ",
                      nb_ind, " individus / ", "h = ", h)
  
  point_text_info <- st_sfc(st_point(c(Box["xmin"] + 1000, Box["ymax"] - 1000)), crs = st_crs(zoom_obj))
  info_label_point <- st_sf(label = info_text, geometry = point_text_info)
  
  labels_zoom <- get(paste0("labels_ZOOM_", zoom_levels))
  # labels_zoom <- get(paste0("labels_ZOOM_", "A"))
  
  # labels_zoom <- st_read(paste0(data_generated_path, "labels_ZOOM_", zoom_levels, ".gpkg"), quiet = TRUE)
  
  data_95 <- results_kud %>% filter(level == 95)
  # data_90 <- results_kud %>% filter(level == 90)
  data_50 <- results_kud %>% filter(level == 50)
  
  # couleurs <- c("adulte" = couleur[2], "juvénile" = couleur[1])
  # 
  # map <- tm_basemap(c("OpenStreetMap", "Esri.WorldImagery", "CartoDB.Positron")) +
  #   tm_shape(data_95) + tm_polygons(fill = "param", border.col = "white", col = couleurs, alpha = 0.3) +
  #   tm_shape(data_90) + tm_polygons(fill = "param", border.col = "white", col = couleurs, alpha = 0.6) +
  #   tm_shape(data_50) + tm_polygons(fill = "param", border.col = "white", col = couleurs, alpha = 0.95) +
  #   tm_shape(zoom_obj) + tm_borders(col = "#575757", lty = "dotted", lwd = 3) +
  #   tm_shape(label_point) + tm_text("label", col = "#575757", size = 3, just = c("left", "top")) +
  #   tm_shape(terre_mer) + tm_lines(col = "lightblue", lwd = 0.1) +
  #   tm_shape(labels_zoom) + tm_text("name", size = 1, col = "#575757", fontface = "bold", just = "left") +
  #   tm_shape(site_baguage) + tm_text("icone", size = 1.5) +
  #   tm_credits(info_text,
  #              position = c("left", "bottom"), size = 1,
  #              col = "black", bg.color = "white", bg.alpha = 0.7, fontface = "bold"
  #   )
  # 
  # # tmap_save(map, paste0(atlas_path, "UDMap_", analyse, "_", zoom_levels, ".html"))
  # tmap_save(map, paste0(atlas_path, "UDMap_", analyse, "_", "A", ".html"))
  
  
  
  
  
  
  # couleurs <- c("juvénile" = couleur[1], "adulte" = couleur[2])
  # couleurs <- c("juvénile" = "green", "adulte" = "blue")
  niveaux_param <- levels(results_kud$param)
  palette_dyn <- setNames(c("green", "blue", "red", "orange")[1:length(niveaux_param)], niveaux_param)
  
  tmap_mode("view")  # mode interactif
  
  map <- tm_basemap(c("OpenStreetMap", "Esri.WorldImagery", "CartoDB.Positron")) +
    tm_shape(data_95) + tm_polygons(fill = "param", palette = couleurs, alpha = 0.3, border.col = "white", legend.show = TRUE) +
    # tm_shape(data_90) + tm_polygons(fill = "param", palette = couleurs, alpha = 0.6, border.col = "white", legend.show = FALSE) +
    tm_shape(data_50) + tm_polygons(fill = "param", palette = couleurs, alpha = 0.95, border.col = "white", legend.show = FALSE) +
    tm_shape(zoom_obj) + tm_borders(col = "#575757", lty = "dotted", lwd = 3) +
    tm_shape(label_point) + tm_text("label", col = "#575757", size = 3, just = c("left", "top")) +
    tm_shape(terre_mer) + tm_lines(col = "lightblue", lwd = 0.1) +
    tm_shape(labels_zoom) + tm_text("name", size = 1, col = "#575757", fontface = "bold", just = "left") +
    tm_shape(site_baguage) + tm_text("icone", size = 1.5) +
    tm_credits(info_text,
               position = c("left", "bottom"), size = 1,
               col = "black", bg.color = "white", bg.alpha = 0.7, fontface = "bold")
  
  # tmap_save(map, paste0(atlas_path, "UDMap_", analyse, "_A.html"))
  tmap_save(map, paste0(atlas_path, "UDMap_",analyse,"_", param, "_", zoom_levels, ".html"))
  
  
  
  
  return(list(
    kud_sf = results_kud,
    stats = nb_ind_point_dt,
    map = map
  ))
}





# fonctionnel ???????????????

# analyse <- "roosting_age"
# results_kud <- NULL
# nb_kud <- NULL
# comportement <- "roosting"
# couleur <- nom_pal_roosting

# zoom_levels <- c("A", "B", "C")
# # zoom_levels <- c("A")
# 
# results_kud <- NULL
# nb_kud <- NULL
# analyse <- "make_kud_param_test"
# param <- "age"
# comportement <- "roosting"
# couleur <- couleur_roosting_param_2
# 
# 
# plan(multisession, workers = 2)
# 
# results_list <- future_lapply(
#   zoom_levels,
#   function(z) {
#     make_kud_param(analyse, z, comportement, GPS, data_generated_path, resolution_ZOOM, couleur, param)
#   },
#   future.seed = TRUE # garantit des tirages aléatoires reproductibles et indépendants
# )

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

sample_weighted_points <- function(data, n = 1000, param = NULL, zone = NULL, cap = Inf) {
  
  # Vérifier colonnes obligatoires
  if(!all(c("ID", "datetime") %in% names(data))) {
    stop("Les colonnes 'ID' et 'datetime' doivent exister dans les données.")
  }
  
  # Calcul du dt par individu
  data <- data %>%
    arrange(ID, datetime) %>%
    group_by(ID) %>%
    mutate(dt = as.numeric(difftime(datetime, lag(datetime), units = "secs")),
           dt = ifelse(is.na(dt), 0, dt)) %>%
    ungroup() %>%
    mutate(dt_capped = pmin(dt, cap),
           dt_capped = ifelse(dt_capped == 0, 1, dt_capped))   # éviter proba nulle
  
  # Fonction d’échantillonnage pondéré
  sample_group <- function(df) {
    df[sample(seq_len(nrow(df)), size = n, replace = FALSE, prob = df$dt_capped), ]
  }
  
  # Définir les variables de regroupement
  grouping_vars <- c("ID")
  if (!is.null(param)) grouping_vars <- c(grouping_vars, param)
  if (!is.null(zone))  grouping_vars <- c(grouping_vars, zone)
  
  # Application
  sampled <- data %>%
    group_by(across(all_of(grouping_vars))) %>%
    filter(n() >= n) %>%
    group_modify(~ sample_group(.x)) %>%
    ungroup()
  
  return(sampled)
}

# GPS_sampled <- sample_weighted_points(GPS, n = 1000, cap = 3600)
# GPS_sampled <- sample_weighted_points(GPS, n = 1000, param = "month_numeric", cap = 3600)

# table(GPS_sampled$ID)
# length(unique(GPS_sampled$ID))

############################################################################ ---
# 2. Carte de la zone d'étude --------------------------------------------------
############################################################################ ---

# --- objectif ---
# visualisation de la zone d'étude

tmap_mode("view")
zone_map <- tm_scalebar() +
  tm_basemap(c("OpenStreetMap", "Esri.WorldImagery", "CartoDB.Positron")) +
  # tm_shape(terre_mer) +
  # tm_lines(col = "#32B7FF", lwd = 0.5) +
  tm_shape(BOX_2154) +
  tm_borders(col = "#575757") +
  tm_shape(ZOOM) +
  tm_polygons(fill = "#575757", alpha = 0.1, col = "#575757", lty = "dotted", size = 3) +
  tm_labels("name", size = 1, col = "#575757", just = "center") +
  tm_shape(site_baguage) +
  tm_text("icone", size = 1.5, options = opt_tm_text(just = "center")) #+
# tm_shape(chasse2[1,1]) +
# tm_dots(shape = 13, size = 1.5, col = "red") +
# tm_shape(results_kud) +
# tm_polygons(col ="pink") #+
# tm_shape(raster_ZOOM_C) +
# tm_raster(col ="red") +
# tm_shape(grid) +
# tm_polygons(col = "green")
zone_map

tmap_save(zone_map, paste0(atlas_path, "zone_map_new.html"))

############################################################################ ---
# 3. Période d'émission des balises --------------------------------------------
############################################################################ ---

# --- objectif ---
# visualisation des périodes d'émission des balises pour chaque individu

# jeu de données
emission_dt_1 <- GPS %>%
  st_drop_geometry() %>%
  group_by(ID) %>%
  mutate(
    date = as.Date(datetime), # pas besoin de format() ici
    min_date = min(date),
    sex_age = paste0(sex, "_", age)
  ) %>%
  ungroup() %>%
  distinct(ID, date, min_date, sex_age) %>%
  mutate(
    sex_age = ifelse(sex_age %in% c(
      "NA_NA", "NA_adulte", "NA_juvénile",
      "F_NA", "M_NA"
    ), "inconnu", sex_age),
    sex_age = fct_explicit_na(as.factor(sex_age)),
    ID = fct_reorder(ID, min_date) # <- c'est ici que l’ordre est défini
  )

emission_dt_1 <- emission_dt_1 %>%
  mutate(
    min_date = as.Date(min_date),
    ID = fct_reorder(ID, min_date)
  )

table(emission_dt_1$sex_age)

emission_dt_1$sex_age <- as.character(emission_dt_1$sex_age)

table(emission_dt_1$sex_age)

emission_dt_1$sex_age[emission_dt_1$sex_age == "F_adulte"] <- "femelle adulte"
emission_dt_1$sex_age[emission_dt_1$sex_age == "F_juvénile"] <- "femelle juvénile"
emission_dt_1$sex_age[emission_dt_1$sex_age == "M_adulte"] <- "mâle adulte"
emission_dt_1$sex_age[emission_dt_1$sex_age == "M_juvénile"] <- "mâle juvénile"

table(emission_dt_1$sex_age)

# couleur par catégorie
col_sex_age <- c(
  "femelle adulte" = "purple", "femelle juvénile" = "lightpink",
  "mâle adulte" = "darkgreen", "mâle juvénile" = "lightgreen",
  "inconnu" = "grey40"
)

emission_dt_1$sex_age <- factor(emission_dt_1$sex_age, levels = names(col_sex_age))

# plot
emission_plot <- ggplot(emission_dt_1, aes(
  x = date, y = ID,
  color = sex_age
)) +
  geom_point(size = 2, shape = 15) +
  scale_color_manual(values = col_sex_age) +
  theme_classic() +
  scale_x_date(date_breaks = "2 month", date_labels = "%b %Y") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(
    title = "",
    x = "Période d'émission des balises",
    y = "Individu",
    color = "Sexe & Age"
  )
emission_plot

# save plot
ggsave(paste0(atlas_path, "/emission_plot.png"),
  plot = emission_plot, width = 12, height = 8, dpi = 1000
)

# talk ---

emission_dt_1_talk <- emission_dt_1 %>%
  mutate(
    sex_age_en = case_when(
      sex_age == "femelle adulte" ~ "adult female",
      sex_age == "inconnu" ~ "unknown",
      sex_age == "mâle adulte" ~ "adult male",
      sex_age == "femelle juvénile" ~ "juvenile female",
      sex_age == "mâle juvénile" ~ "juvenile male"
    )
  )

emission_dt_1_talk

col_sex_age_en <- c(
  "adult female" = "purple", "juvenile female" = "lightpink",
  "adult male" = "darkgreen", "juvenile male" = "lightgreen",
  "unknown" = "grey40"
)

library(scales)

emission_plot_talk <- ggplot(emission_dt_1_talk, aes(
  x = date, y = ID,
  color = sex_age_en
)) +
  geom_point(size = 2, shape = 15) +
  scale_color_manual(values = col_sex_age_en) +
  theme_classic() +
  scale_x_date(
    date_breaks = "10 month",
    labels = label_date(format = "%b %Y", locale = "en")
  )  + # theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  labs(
    title = "",
    x = "Recording periods",
    y = "Individual",
    color = "Sex & Age"
  )
emission_plot_talk

# save plot
ggsave(paste0(atlas_path, "/emission_plot_talk.png"),
  plot = emission_plot_talk, width = 15, height = 9, dpi = 1000
)

length(unique(GPS$ID)) # 80 ind
length(unique(GPS$ID[GPS$sex=="F"])) # 36 female
length(unique(GPS$ID[GPS$sex=="M"])) # 39 male
length(unique(GPS$ID[is.na(GPS$sex)])) # 7 unknown
length(unique(GPS$ID[GPS$age=="juvénile"])) # 20 juv
length(unique(GPS$ID[GPS$age=="adulte"])) # 60 ad
length(unique(GPS$ID[is.na(GPS$age)])) # 0 unknown
dim(GPS)

############################################################################ ---
# 4. Domaines vitaux -----------------------------------------------------------
############################################################################ ---

## estimation -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#----

# --- objectif ---
# estimation du domaine vital à 95 % et 50 % de chaque individu

# Préparer les coordonnées sans géométrie et sans valeurs manquantes
coords_HR_ID <- GPS %>%
  dplyr::select(ID, lon, lat) %>% # Sélectionne les colonnes d’intérêt
  st_drop_geometry() %>% # Supprime la géométrie si GPS est un sf
  na.omit() # Supprime les lignes avec des valeurs manquantes

# Convertit en objet sf avec système de coordonnées WGS84
locs_HR_ID <- st_as_sf(coords_HR_ID, coords = c("lon", "lat"), crs = 4326)

# Reprojection en UTM zone 30N (mètres)
locs_HR_ID_32630 <- st_transform(locs_HR_ID, crs = 32630)

# Préparation du fond raster (grille) pour KDE
crs_utm <- "EPSG:32630"
SpatRaster <- project(raster_100x100, crs_utm) # Reprojette le raster
RasterLayer <- raster(SpatRaster) # Convertit en RasterLayer
SpatialPixels <- as(RasterLayer, "SpatialPixels") # Convertit en pixels spatiaux pour la KDE

# Extraire les coordonnées reprojetées
coords_HR_ID_32630 <- st_coordinates(locs_HR_ID_32630)

# Calcul des largeurs de bande (bandwidths) selon la règle de Silverman
sigma_x_HR_ID <- sd(coords_HR_ID_32630[, 1]) # Écart-type en X
sigma_y_HR_ID <- sd(coords_HR_ID_32630[, 2]) # Écart-type en Y
n_HR_ID <- nrow(coords_HR_ID_32630) # Nombre d'observations

# Largeurs de bande pour X et Y (divisé par 2 : lissage plus fin)
h_silverman_x_HR_ID <- 1.06 * sigma_x_HR_ID * n_HR_ID^(-1 / 5) / 2
h_silverman_y_HR_ID <- 1.06 * sigma_y_HR_ID * n_HR_ID^(-1 / 5) / 2

# Conversion en objet Spatial pour 'adehabitatHR'
locs_spa_HR_ID <- as(locs_HR_ID_32630, "Spatial")

# Calcul de l’UD (Utilization Distribution) via KDE
kud_HR_ID <- kernelUD(
  locs_spa_HR_ID["ID"],
  grid = SpatialPixels,
  h = mean(c(h_silverman_x_HR_ID, h_silverman_y_HR_ID)) # Moyenne des bandwidths
)

# Extraction des contours 95 % et 50 % des domaines vitaux
kde_hr_95 <- getverticeshr(kud_HR_ID, 95) # Domaine vital global
kde_hr_50 <- getverticeshr(kud_HR_ID, 50) # Noyau d’activité

# Conversion en objets sf
kde_hr_95_sf <- st_as_sf(kde_hr_95)
kde_hr_50_sf <- st_as_sf(kde_hr_50)

# Création d'une liste de contours (niveaux de densité) par individu
UDmaps_list_HR_ID <- lapply(names(kud_HR_ID), function(ID) {
  print(ID) # Affiche l'ID en cours

  # Récupère l'UD pour un individu
  kud_single_HR_ID <- kud_HR_ID[[ID]]
  rast_HR_ID <- rast(kud_single_HR_ID) # Convertit en SpatRaster
  contour_HR_ID <- as.contour(rast_HR_ID) # Calcule les isovaleurs
  sf_HR_ID <- st_as_sf(contour_HR_ID) # Convertit en sf
  cast_HR_ID <- st_cast(sf_HR_ID, "POLYGON") # Assure une géométrie propre
  cast_HR_ID$ID <- ID # Ajoute l'ID

  return(cast_HR_ID)
})

# Fusionne tous les contours en un seul objet sf
UDMap_final_HR_ID <- do.call(rbind, UDmaps_list_HR_ID)

# Conversion explicite de l’ID en facteur
UDMap_final_HR_ID$ID <- as.factor(UDMap_final_HR_ID$ID)

# Sauvegarde au format GeoPackage
st_write(UDMap_final_HR_ID, paste0(data_generated_path, "UDMap_final_HR_ID.gpkg"), append = FALSE)

# Relecture du fichier sauvegardé
UDMap_final_HR_ID <- st_read(file.path(data_generated_path, "UDMap_final_HR_ID.gpkg"))

# Séparation en deux groupes d’individus (pour la carte)
ID_list <- unique(UDMap_final_HR_ID$ID)
ID_gp_1 <- ID_list[1:23]
ID_gp_2 <- ID_list[24:46]

# Filtrage des domaines vitaux 95% pour les deux groupes
kde_hr_95_sf_gp1 <- kde_hr_95_sf %>% filter(id %in% ID_gp_1)
kde_hr_95_sf_gp2 <- kde_hr_95_sf %>% filter(id %in% ID_gp_2)

# Filtrage des domaines vitaux 50% (noyaux) pour les deux groupes
kde_hr_50_sf_gp1 <- kde_hr_50_sf %>% filter(id %in% ID_gp_1)
kde_hr_50_sf_gp2 <- kde_hr_50_sf %>% filter(id %in% ID_gp_2)

# Mode interactif pour la carte
tmap_mode("view")

# Carte pour le groupe 1
UDMap_HR_ID_gp1 <- tm_scalebar() +
  tm_basemap(c("OpenStreetMap", "Esri.WorldImagery", "CartoDB.Positron")) +
  tm_shape(kde_hr_95_sf_gp1) +
  tm_lines(col = "id", palette = palette_grey) +
  tm_shape(kde_hr_50_sf_gp1) +
  tm_polygons(fill = "id", palette = palette_grey) +
  tm_shape(terre_mer) +
  tm_lines(col = "#32B7FF", lwd = 0.5) +
  tm_shape(site_baguage) +
  tm_text("icone", size = 1.5)

# Carte pour le groupe 2
UDMap_HR_ID_gp2 <- tm_scalebar() +
  tm_basemap(c("OpenStreetMap", "Esri.WorldImagery", "CartoDB.Positron")) +
  tm_shape(kde_hr_95_sf_gp2) +
  tm_lines(col = "id", palette = palette_grey) +
  tm_shape(kde_hr_50_sf_gp2) +
  tm_polygons(fill = "id", palette = palette_grey) +
  tm_shape(terre_mer) +
  tm_lines(col = "#32B7FF", lwd = 0.5) +
  tm_shape(site_baguage) +
  tm_text("icone", size = 1.5)

# Assemblage final des deux cartes
UDMap_HR_ID <- tmap_arrange(UDMap_HR_ID_gp1, UDMap_HR_ID_gp2)
UDMap_HR_ID # Affiche le résultat

## surface moyenne #-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#----

# --- objectif ---
# estimation de la surface moyenne du domaine vital à 95 % et 50 % de chaque individu

# Renomme la colonne 'area' en 'area_95' et supprime la géométrie
area_95_dt <- kde_hr_95_sf %>%
  rename(area_95 = area) %>% # Renomme la colonne area
  st_drop_geometry() # Supprime les géométries (on ne garde que les données attributaires)

# Idem pour les domaines vitaux à 50 %
area_50_dt <- kde_hr_50_sf %>%
  rename(area_50 = area) %>%
  st_drop_geometry()

# Fusionne les deux tables par identifiant ('id')
area_dt <- left_join(area_95_dt, area_50_dt)

# save & read
write.table(area_dt,
  file = paste0(data_generated_path, "area_dt.csv"),
  sep = ",", col.names = NA, qmethod = "double"
)

area_dt <- read.csv(paste0(data_generated_path, "area_dt.csv"), row.names = NULL)

# Création du graphique
area_hr_plot <- ggplot() +
  geom_hline(yintercept = mean(area_dt$area_95), col = "black") + # Ajout de la moyenne des aires à 95 %
  geom_hline(yintercept = mean(area_dt$area_95) - sd(area_dt$area_95), col = "grey", lty = "dashed") + # Lignes pointillées = moyenne ± 1 écart-type
  geom_hline(yintercept = mean(area_dt$area_95) + sd(area_dt$area_95), col = "grey", lty = "dashed") +
  geom_point(
    data = area_dt, aes(reorder(id, area_95), area_95), # Points pour chaque aire à 95 %, triés par ordre croissant
    size = 4, shape = 21, col = "white", fill = "black"
  ) +
  geom_point(
    data = area_dt, aes(reorder(id, area_95), area_50), # Points gris pour chaque aire à 50 % (noyau), même tri
    size = 4, shape = 21, col = "white", fill = "grey"
  ) +
  paletteer::scale_fill_paletteer_c("grDevices::Grays") + # !!!!!!!!!!!!!!!Palette de gris (ici inutile car pas utilisée dans les `aes`)
  theme_classic() + # Thème épuré
  theme(legend.position = c(.1, .75)) + # Positionnement de la légende
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) + # Rotation des étiquettes sur l’axe des x
  labs(
    title = "", # Titre et axes
    x = "Individu",
    y = "Aire du domaine vital à 95% (m²)",
    fill = "Aire domaine vitale à 50%"
  )
area_hr_plot

# Sauvegarde du graphique
ggsave(paste0(atlas_path, "/area_hr_plot.png"),
  plot = area_hr_plot, width = 10, height = 4, dpi = 1000
)

## surface ~ age & sex #-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#----

# --- objectif ---
# estimation de la surface moyenne du domaine vital à 95 % et 50 % en fonction de l'age

area_dt <- read.csv(paste0(data_generated_path, "area_dt.csv"), row.names = NULL)

area_dt <- area_dt %>%
  rename(ID = id)

age_sex_dt <- GPS %>%
  st_drop_geometry() %>%
  dplyr::select(ID, age, sex) %>%
  distinct()

to_keep <- as.data.frame(table(age_sex_dt$ID))
to_keep <- to_keep %>%
  filter(Freq == 1)

age_sex_dt_2 <- age_sex_dt %>%
  filter(ID %in% to_keep$Var1)

table(age_sex_dt_2$ID)

area_dt_sex_age <- area_dt %>%
  left_join(age_sex_dt_2)

# surface moyenne ~ age
mean_surface_HR_age <- area_dt_sex_age %>%
  group_by(age) %>%
  mutate(
    mean_surface_95_per_age = mean(area_95, na.rm = T),
    mean_surface_50_per_age = mean(area_50, na.rm = T),
    sd_surface_95_per_age = sd(area_95, na.rm = T),
    sd_surface_50_per_age = sd(area_50, na.rm = T)
  ) %>%
  dplyr::select(age, mean_surface_95_per_age, mean_surface_50_per_age, sd_surface_95_per_age, sd_surface_50_per_age) %>%
  distinct()

# surface moyenne ~ sexe
mean_surface_HR_sex <- area_dt_sex_age %>%
  group_by(sex) %>%
  mutate(
    mean_surface_95_per_sex = mean(area_95, na.rm = T),
    mean_surface_50_per_sex = mean(area_50, na.rm = T),
    sd_surface_95_per_sex = sd(area_95, na.rm = T),
    sd_surface_50_per_sex = sd(area_50, na.rm = T)
  ) %>%
  dplyr::select(sex, mean_surface_95_per_sex, mean_surface_50_per_sex, sd_surface_95_per_sex, sd_surface_50_per_sex) %>%
  distinct()

# test
t.test(area_dt_sex_age$area_95 ~ area_dt_sex_age$age)
t.test(area_dt_sex_age$area_50 ~ area_dt_sex_age$age)
t.test(area_dt_sex_age$area_95 ~ area_dt_sex_age$sex)
t.test(area_dt_sex_age$area_50 ~ area_dt_sex_age$sex)

wilcox.test(area_dt_sex_age$area_95 ~ area_dt_sex_age$age)
wilcox.test(area_dt_sex_age$area_50 ~ area_dt_sex_age$age)
wilcox.test(area_dt_sex_age$area_95 ~ area_dt_sex_age$sex)
wilcox.test(area_dt_sex_age$area_50 ~ area_dt_sex_age$sex)

area_dt_sex_age$sex[area_dt_sex_age$sex == "F"] <- "femelle"
area_dt_sex_age$sex[area_dt_sex_age$sex == "M"] <- "mâle"

# plot
my_comparisons <- list(c("adulte", "juvénile"))

area_dt_age_only_no_na <- area_dt_sex_age %>%
  dplyr::select(ID, area_95, area_50, age) %>%
  na.omit()

surface_95_age_plot <- ggplot(
  area_dt_age_only_no_na,
  aes(x = age, y = area_95)
) +
  geom_boxplot(col = "black", outlier.colour = "black", outlier.shape = 1, fill = "grey") +
  geom_jitter(shape = 21, size = 0.5, color = "white", alpha = 0.5, fill = "black", width = 0.3) +
  stat_summary(
    fun.ymin = function(x) mean(x) - sd(x),
    fun.ymax = function(x) mean(x) + sd(x), geom = "linerange", size = 1, color = "black"
  ) +
  stat_summary(
    fun.y = mean,
    fun.ymin = function(x) mean(x) - sd(x),
    fun.ymax = function(x) mean(x) + sd(x),
    geom = "pointrange", shape = 21, size = 1, color = "black", fill = "white"
  ) +
  theme_classic() +
  stat_compare_means(
    method = "t.test", comparisons = my_comparisons,
    label.y = c(2500), aes(label = after_stat(p.signif))
  ) +
  labs(
    title = "",
    x = "Age", y = "Aire du domaine vital à 95%", fill = ""
  )
surface_95_age_plot

surface_50_age_plot <- ggplot(
  area_dt_age_only_no_na,
  aes(x = age, y = area_50)
) +
  geom_boxplot(col = "black", outlier.colour = "black", outlier.shape = 1, fill = "grey") +
  geom_jitter(shape = 21, size = 0.5, color = "white", alpha = 0.5, fill = "black", width = 0.3) +
  stat_summary(
    fun.ymin = function(x) mean(x) - sd(x),
    fun.ymax = function(x) mean(x) + sd(x), geom = "linerange", size = 1, color = "black"
  ) +
  stat_summary(
    fun.y = mean,
    fun.ymin = function(x) mean(x) - sd(x),
    fun.ymax = function(x) mean(x) + sd(x),
    geom = "pointrange", shape = 21, size = 1, color = "black", fill = "white"
  ) +
  theme_classic() +
  stat_compare_means(
    method = "t.test", comparisons = my_comparisons,
    label.y = c(300), aes(label = after_stat(p.signif))
  ) +
  labs(
    title = "",
    x = "Age", y = "Aire du domaine vital à 50%", fill = ""
  )
surface_50_age_plot

my_comparisons <- list(c("femelle", "mâle"))

area_dt_sex_only_no_na <- area_dt_sex_age %>%
  dplyr::select(ID, area_95, area_50, sex) %>%
  na.omit()

surface_95_sex_plot <- ggplot(
  area_dt_sex_only_no_na,
  aes(x = sex, y = area_95)
) +
  geom_boxplot(col = "black", outlier.colour = "black", outlier.shape = 1, fill = "grey") +
  geom_jitter(shape = 21, size = 0.5, color = "white", alpha = 0.5, fill = "black", width = 0.3) +
  stat_summary(
    fun.ymin = function(x) mean(x) - sd(x),
    fun.ymax = function(x) mean(x) + sd(x), geom = "linerange", size = 1, color = "black"
  ) +
  stat_summary(
    fun.y = mean,
    fun.ymin = function(x) mean(x) - sd(x),
    fun.ymax = function(x) mean(x) + sd(x),
    geom = "pointrange", shape = 21, size = 1, color = "black", fill = "white"
  ) +
  theme_classic() +
  stat_compare_means(
    method = "t.test", comparisons = my_comparisons,
    label.y = c(2500), aes(label = after_stat(p.signif))
  ) +
  labs(
    title = "",
    x = "Sexe", y = "Aire du domaine vital à 95%", fill = ""
  )
surface_95_sex_plot

surface_50_sex_plot <- ggplot(
  area_dt_sex_only_no_na,
  aes(x = sex, y = area_50)
) +
  geom_boxplot(col = "black", outlier.colour = "black", outlier.shape = 1, fill = "grey") +
  geom_jitter(shape = 21, size = 0.5, color = "white", alpha = 0.5, fill = "black", width = 0.3) +
  stat_summary(
    fun.ymin = function(x) mean(x) - sd(x),
    fun.ymax = function(x) mean(x) + sd(x), geom = "linerange", size = 1, color = "black"
  ) +
  stat_summary(
    fun.y = mean,
    fun.ymin = function(x) mean(x) - sd(x),
    fun.ymax = function(x) mean(x) + sd(x),
    geom = "pointrange", shape = 21, size = 1, color = "black", fill = "white"
  ) +
  theme_classic() +
  stat_compare_means(
    method = "t.test", comparisons = my_comparisons,
    label.y = c(300), aes(label = after_stat(p.signif))
  ) +
  labs(
    title = "",
    x = "Sexe", y = "Aire du domaine vital à 50%", fill = ""
  )
surface_50_sex_plot

surface_sex_age_all_plot <- ggarrange(surface_95_age_plot,
  surface_50_age_plot,
  surface_95_sex_plot,
  surface_50_sex_plot,
  ncol = 4
)

# Sauvegarde du graphique
ggsave(paste0(atlas_path, "/surface_sex_age_all_plot.png"),
  plot = surface_sex_age_all_plot, width = 10, height = 4, dpi = 1000
)

## % dans la réserve #-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#----

# --- objectif ---
# estimation de la proportion de surface des domaines vitaux à 95 % et 50 % dans et hors réserve

### 95% du HR ---

kde_hr_95_sf_2154 <- st_transform(kde_hr_95_sf, crs = 2154)

# Calculate area and tidy up
intersect_hr_95 <- st_intersection(kde_hr_95_sf_2154, RMO) %>%
  mutate(intersect_area = st_area(.)) %>%
  dplyr::select(id, intersect_area) %>%
  st_drop_geometry()

kde_hr_95_sf_2154 <- mutate(kde_hr_95_sf_2154, county_area = st_area(kde_hr_95_sf_2154))
kde_hr_95_sf_2154 <- merge(kde_hr_95_sf_2154, intersect_hr_95, by = "id", all.x = TRUE)

mean_hr_95_pourc_rn <- mean(kde_hr_95_sf_2154$coverage, na.rm = T)

print("pourcentage moyen des home range dans la réserve naturelle :")
mean_hr_95_pourc_rn

# Calculate coverage
kde_hr_95_sf_2154 <- kde_hr_95_sf_2154 %>%
  mutate(coverage = as.numeric(intersect_area / county_area))

HR_95_pourc_RN <- tm_scalebar() +
  tm_basemap(c("OpenStreetMap", "Esri.WorldImagery", "CartoDB.Positron")) +
  tm_shape(kde_hr_95_sf_2154) +
  tm_polygons(
    fill = "coverage", alpha = 0.5,
    palette = palette_grey
  ) +
  tm_shape(terre_mer) +
  tm_lines(col = "#32B7FF", lwd = 0.5) +
  tm_credits(paste0("Pourcentage moyen des domaines vitaux à 95% dans la réserve naturelle : ", round(mean_hr_95_pourc_rn, 2) * 100, "%"))
HR_95_pourc_RN

### 50% du HR ---

kde_hr_50_sf_2154 <- st_transform(kde_hr_50_sf, crs = 2154)

# Calculate area and tidy up
intersect_hr_50 <- st_intersection(kde_hr_50_sf_2154, RMO) %>%
  mutate(intersect_area = st_area(.)) %>%
  dplyr::select(id, intersect_area) %>%
  st_drop_geometry()

kde_hr_50_sf_2154 <- mutate(kde_hr_50_sf_2154, county_area = st_area(kde_hr_50_sf_2154))
kde_hr_50_sf_2154 <- merge(kde_hr_50_sf_2154, intersect_hr_50, by = "id", all.x = TRUE)

# Calculate coverage
kde_hr_50_sf_2154 <- kde_hr_50_sf_2154 %>%
  mutate(coverage = as.numeric(intersect_area / county_area))

mean_hr_50_pourc_rn <- mean(kde_hr_50_sf_2154$coverage, na.rm = T)

print("pourcentage moyen des home range dans la réserve naturelle :")
mean_hr_50_pourc_rn

HR_50_pourc_RN <- tm_scalebar() +
  tm_basemap(c("OpenStreetMap", "Esri.WorldImagery", "CartoDB.Positron")) +
  tm_shape(kde_hr_50_sf_2154) +
  tm_polygons(
    fill = "coverage", alpha = 0.5,
    palette = palette_grey
  ) +
  tm_shape(terre_mer) +
  tm_lines(col = "#32B7FF", lwd = 0.5) +
  tm_credits(paste0("Pourcentage moyen des domaines vitaux à 50% dans la réserve naturelle : ", round(mean_hr_50_pourc_rn, 2) * 100, "%"))
HR_50_pourc_RN

HR_pourc_RN <- tmap_arrange(HR_95_pourc_RN, HR_50_pourc_RN)
HR_pourc_RN

# plot

pourc_hr_50 <- kde_hr_50_sf_2154 %>%
  st_drop_geometry() %>%
  dplyr::select(id, coverage) %>%
  rename(coverage_50 = coverage) %>%
  distinct() %>%
  left_join(area_dt)

pourc_hr <- kde_hr_95_sf_2154 %>%
  st_drop_geometry() %>%
  dplyr::select(id, coverage) %>%
  rename(coverage_95 = coverage) %>%
  distinct() %>%
  left_join(pourc_hr_50)

area_hr_plot <- ggplot() +
  geom_point(
    data = pourc_hr,
    aes(x = reorder(id, area_95), y = area_50, fill = coverage_50),
    size = 4, shape = 24, col = "white"
  ) +
  scale_fill_gradient(
    name = "Couverture 50%",
    low = "#E5E4E4", high = "#69B578"
  ) +
  new_scale_fill() +
  geom_point(
    data = pourc_hr,
    aes(x = reorder(id, area_95), y = area_95, fill = coverage_50),
    size = 4, shape = 21, col = "white"
  ) +
  scale_fill_gradient(
    name = "Couverture 95%",
    low = "#E5E4E4", high = "#F0A202"
  ) +
  geom_hline(yintercept = mean(pourc_hr$area_95), col = "#F0A202") +
  geom_hline(yintercept = mean(pourc_hr$area_95) - sd(pourc_hr$area_95), col = "#F0A202", lty = "dotted") +
  geom_hline(yintercept = mean(pourc_hr$area_95) + sd(pourc_hr$area_95), col = "#F0A202", lty = "dotted") +
  geom_hline(yintercept = mean(pourc_hr$area_50), col = "#69B578") +
  geom_hline(yintercept = mean(pourc_hr$area_50) - sd(pourc_hr$area_50), col = "#69B578", lty = "dotted") +
  geom_hline(yintercept = mean(pourc_hr$area_50) + sd(pourc_hr$area_50), col = "#69B578", lty = "dotted") +
  theme_classic() +
  theme(
    legend.position = c(.3, .75),
    legend.direction = "horizontal",
    legend.box = "horizontal",
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  ) +
  labs(
    x = "Individu",
    y = "Aire du domaine vital (m²)"
  )

ggsave(paste0(atlas_path, "/area_hr_plot.png"),
  plot = area_hr_plot, width = 10, height = 4, dpi = 1000
)

## temps dans la réserve #-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#----

# --- objectif ---
# estimation du temps passé dans la réserve pour chaque individu
# = nombre de point hors et dans la réserve

### tous comportements ---

# Temps global par individu
# On sélectionne uniquement les colonnes ID et datetime depuis les données GPS,
# on enlève la géométrie (car on ne l’utilise pas ici), puis on garde les paires uniques ID/datetime.
# Ensuite, on compte le nombre total de points (présences) par individu.
all_elsewhere <- GPS_2154 %>%
  dplyr::select(ID, datetime) %>% # Ne garder que l’identifiant et la date/heure
  st_drop_geometry() %>% # Supprimer les informations de géométrie (sf)
  distinct() %>% # Garder les paires uniques ID / datetime
  group_by(ID) %>% # Grouper par individu
  distinct() %>% # Encore une précaution pour ne pas avoir de doublons
  summarize(all_elsewhere = n()) # Compter le nombre total de points par individu

# Temps passé dans la réserve (intersection spatiale)
# On identifie les points GPS qui sont situés dans la réserve RMO.
all_inRMO <- st_intersection(GPS_2154, RMO)

# On applique le même traitement que ci-dessus : on sélectionne ID/datetime,
# on supprime la géométrie, on garde les points uniques et on compte combien chaque individu
# a de positions dans la réserve.
all_inRMO <- all_inRMO %>%
  dplyr::select(ID, datetime) %>%
  st_drop_geometry() %>%
  distinct() %>%
  group_by(ID) %>%
  distinct() %>%
  summarize(all_inRMO = n())

# Jointure et calcul de proportion
# On joint les deux tableaux (présences dans la réserve et total général),
# puis on calcule pour chaque individu la proportion du temps passé dans la réserve.
all_inRMO_elsewhere <- left_join(all_inRMO, all_elsewhere) %>%
  mutate(pourc_inRMO_all = all_inRMO / all_elsewhere)

# Moyenne de la proportion de temps passé dans la réserve
# On calcule la moyenne de cette proportion pour l’ensemble des individus.
mean_pourc_tps_inRMO <- mean(all_inRMO_elsewhere$pourc_inRMO_all, na.rm = TRUE)

# Affichage du résultat
print("Proportion du temps passé dans la réserve vs hors réserve:")
mean_pourc_tps_inRMO

### reposoir ---

# temps global
roosting_elsewhere <- GPS_2154 %>%
  filter(behavior == "roosting") %>%
  dplyr::select(ID, datetime) %>%
  st_drop_geometry() %>%
  distinct() %>%
  group_by(ID) %>%
  distinct() %>%
  summarize(roosting_elsewhere = n())

# temps dans la réserve
GPS_2154_roosting <- GPS_2154 %>%
  filter(behavior == "roosting")

roosting_inRMO <- st_intersection(GPS_2154_roosting, RMO)

roosting_inRMO <- roosting_inRMO %>%
  dplyr::select(ID, datetime) %>%
  st_drop_geometry() %>%
  distinct() %>%
  group_by(ID) %>%
  distinct() %>%
  summarize(roosting_inRMO = n())

# join dans la réserve et elsewhere
roosting_inRMO_elsewhere <- left_join(roosting_elsewhere, roosting_inRMO) %>%
  mutate(pourc_roosting_inRMO = roosting_inRMO / roosting_elsewhere)

mean_pourc_roosting_inRMO <- mean(roosting_inRMO_elsewhere$pourc_roosting_inRMO, na.rm = T)

print("Proportion du temps passé dans la réserve vs hors réserve pour le roosting:")
mean_pourc_roosting_inRMO

### alimentation ---

# temps global
foraging_everywher <- GPS_2154 %>%
  filter(behavior == "foraging") %>%
  dplyr::select(ID, datetime) %>%
  st_drop_geometry() %>%
  distinct() %>%
  group_by(ID) %>%
  distinct() %>%
  summarize(foraging_elsewhere = n())

# temps dans la réserve
GPS_2154_foraging <- GPS_2154 %>%
  filter(behavior == "foraging")

foraging_inRMO <- st_intersection(GPS_2154_foraging, RMO)

foraging_inRMO <- foraging_inRMO %>%
  dplyr::select(ID, datetime) %>%
  st_drop_geometry() %>%
  distinct() %>%
  group_by(ID) %>%
  distinct() %>%
  summarize(foraging_inRMO = n())

# join dans la réserve et elsewhere
foraging_inRMO_elsewhere <- left_join(foraging_inRMO, foraging_everywher) %>%
  mutate(pourc_foraging_inRMO = foraging_inRMO / foraging_elsewhere)

mean_pourc_inRMO_foraging <- mean(foraging_inRMO_elsewhere$pourc_foraging_inRMO, na.rm = T)

print("Proportion du temps passé dans la réserve vs hors réserve pour le foraging:")
mean_pourc_inRMO_foraging

### autres comportements ---

# ni repos, ni alimentation

# temps global
other_elsewhere <- GPS_2154 %>%
  filter(behavior == "other") %>%
  dplyr::select(ID, datetime) %>%
  st_drop_geometry() %>%
  distinct() %>%
  group_by(ID) %>%
  distinct() %>%
  summarize(other_elsewhere = n())

# temps dans la réserve
GPS_2154_other <- GPS_2154 %>%
  filter(behavior == "other")

other_inRMO <- st_intersection(GPS_2154_other, RMO)

other_inRMO <- other_inRMO %>%
  dplyr::select(ID, datetime) %>%
  st_drop_geometry() %>%
  distinct() %>%
  group_by(ID) %>%
  distinct() %>%
  summarize(other_inRMO = n())

# join dans la réserve et elsewhere
other_inRMO_elsewhere <- left_join(other_inRMO, other_elsewhere) %>%
  mutate(pourc_inRMO_other = other_inRMO / other_elsewhere)

mean_pourc_other_inRMO <- mean(other_inRMO_elsewhere$pourc_inRMO_other, na.rm = T)

print("Proportion du temps passé dans la réserve vs hors réserve pour le other:")
mean_pourc_other_inRMO

### tableau général ---

all_duree_1 <- left_join(
  other_inRMO_elsewhere,
  foraging_inRMO_elsewhere
)
all_duree_2 <- left_join(
  all_duree_1,
  roosting_inRMO_elsewhere
)
all_duree_3 <- left_join(
  all_duree_2,
  all_inRMO_elsewhere
)

all_duree_3$sum_inRMO <- all_duree_3$other_inRMO + all_duree_3$foraging_inRMO + all_duree_3$roosting_inRMO
all_duree_3$sum_elsewhere <- all_duree_3$other_elsewhere + all_duree_3$foraging_elsewhere + all_duree_3$roosting_elsewhere

all_duree_dans_reserve <- all_duree_3 %>%
  dplyr::select(ID, pourc_foraging_inRMO, pourc_roosting_inRMO, pourc_inRMO_other, pourc_inRMO_all) %>%
  dplyr::rename(
    foraging = pourc_foraging_inRMO,
    roosting = pourc_roosting_inRMO,
    autre = pourc_inRMO_other,
    total = pourc_inRMO_all
  )

all_duree_dans_reserve_long <- reshape2::melt(all_duree_dans_reserve, id = "ID")

write.table(all_duree_dans_reserve_long,
  file = paste0(data_generated_path, "all_duree_dans_reserve_long.csv"),
  sep = ",", col.names = NA, qmethod = "double"
)

### graphique ---

duree_dans_reserve_plot <- ggplot(
  all_duree_dans_reserve_long,
  aes(x = reorder(ID, value), y = value, fill = variable)
) +
  geom_hline(yintercept = mean_pourc_other_inRMO, linetype = "longdash", color = "grey") +
  geom_hline(yintercept = mean_pourc_roosting_inRMO, linetype = "longdash", color = "#CF63A6FF") +
  geom_hline(yintercept = mean_pourc_inRMO_foraging, linetype = "longdash", color = "#0095AFFF") +
  geom_hline(yintercept = mean_pourc_tps_inRMO, linetype = "longdash", color = "black") +
  geom_jitter(shape = 21, size = 4, color = "white", alpha = 0.5) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  theme(legend.position = c(0.85, 0.30)) +
  scale_fill_manual(values = c("#0095AFFF", "#CF63A6FF", "grey", "black")) +
  labs(
    title = "",
    x = "Individu", y = "Pourcentage de temps passé dans la réserve", fill = ""
  )
duree_dans_reserve_plot

ggsave(paste0(atlas_path, "/duree_dans_reserve_plot.png"),
  plot = duree_dans_reserve_plot, width = 10, height = 4, dpi = 300
)

############################################################################ ---
# 5. Zones de repos -------------------------------------------------------------
############################################################################ ---

# --- objectif ---
# localisation des principales zone de repos (durant les marées hautes)
# localisation zone par zone
# et localisation tout la zone d'étude, sous forme de point chaud avec le nombre d'individu sur chaque reposoirs

## zone A, B, C #-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#----

# # paramètres
# zoom_levels <- c("A", "B", "C")
# results_kud <- NULL
# nb_kud <- NULL
# analyse <- "roosting_95_90_50"
# comportement <- "roosting"
# couleur <- couleur_roosting
# 
# # estimer les kernelUD ---
# 
# plan(multisession, workers = 3)
# 
# results_list <- future_map(
#   zoom_levels, # vecteur : "A", "B", "C"
#   ~ estimate_kud(
#     analyse = analyse,
#     zoom_level = .x, # "A" ... puis "B" ... puis "C"
#     comportement = comportement,
#     GPS = GPS,
#     data_generated_path = data_generated_path,
#     resolution_ZOOM = resolution_ZOOM
#   ),
#   .options = furrr_options(seed = TRUE, packages = "raster")
# )
# 
# results_kud.roosting_95_50 <- do.call(rbind, results_list)
# st_write(results_kud.roosting_95_50, paste0(data_generated_path, "results_kud_", analyse, ".gpkg"), append = FALSE)
# 
# # compter les nb ind par zoom ---
# 
# nb_kud_map.roosting_95_50 <- future_map(
#   zoom_levels, # vecteur : "A", "B", "C"
#   ~ count_nb_kud(
#     zoom_level = .x, # "A" ... puis "B" ... puis "C"
#     comportement = comportement
#   ),
#   .options = furrr_options(seed = TRUE)
# )
# 
# nb.roosting_95_50 <- do.call(rbind, nb_kud_map.roosting_95_50)
# write.csv(nb.roosting_95_50, paste0(data_generated_path, "nb.", analyse, ".csv"), row.names = FALSE)
# 
# # resultats ---
# 
# results_kud.roosting_95_50_hpar4 <- st_read(file.path(data_generated_path, paste0("results_kud_", analyse, ".gpkg")))
# nb.roosting_95_50_hpar4 <- read.csv(paste0(data_generated_path, paste0("nb.", analyse, ".csv")), row.names = NULL)
# 
# maps_list.roosting <- future_map(
#   zoom_levels,
#   ~ create_map(
#     analyse = analyse,
#     zoom_level = .x,
#     couleur = couleur
#   ),
#   .options = furrr_options(
#     seed = TRUE,
#     globals = list(
#       create_map = create_map,
#       analyse = analyse,
#       couleur = couleur,
#       ZOOM_A = ZOOM_A,
#       ZOOM_B = ZOOM_B,
#       ZOOM_C = ZOOM_C,
#       labels_ZOOM_A = labels_ZOOM_A,
#       labels_ZOOM_B = labels_ZOOM_B,
#       labels_ZOOM_C = labels_ZOOM_C,
#       results_kud.roosting_95_50_hpar4 = results_kud.roosting_95_50_hpar4,
#       nb.roosting_95_50_hpar4 = nb.roosting_95_50_hpar4,
#       terre_mer = terre_mer,
#       site_baguage = site_baguage,
#       atlas_path = atlas_path
#     )
#   )
# )
# 
# 
# 
# 
# # paramètres
# # zoom_level <- c("A")
# zoom_levels <- c("A")
# results_kud <- NULL
# nb_kud <- NULL
# analyse <- "roosting_95_95_50"
# comportement <- "roosting"
# couleur <- couleur_roosting
# 
# maps_list.roosting <- future_map(
#   zoom_levels,
#   function(zl) {
#     library(sf)
#     make_kud(
#       analyse = analyse,
#       zoom_levels = zl,
#       comportement = comportement,
#       GPS = GPS,
#       data_generated_path = data_generated_path,
#       resolution_ZOOM = resolution_ZOOM,
#       couleur = couleur
#     )
#   },
#   .options = furrr_options(
#     seed = TRUE,
#     globals = list(
#       make_kud = make_kud,
#       create_map = create_map,
#       analyse = analyse,
#       couleur = couleur,
#       ZOOM_A = ZOOM_A,
#       ZOOM_B = ZOOM_B,
#       ZOOM_C = ZOOM_C,
#       labels_ZOOM_A = labels_ZOOM_A,
#       labels_ZOOM_B = labels_ZOOM_B,
#       labels_ZOOM_C = labels_ZOOM_C,
#       resolution_ZOOM = resolution_ZOOM,
#       terre_mer = terre_mer,
#       site_baguage = site_baguage,
#       atlas_path = atlas_path,
#       data_generated_path = data_generated_path,
#       GPS = GPS
#     )
#   )
# )


# 10/09/2025 ---

GPS_sampled <- sample_weighted_points(
  data = GPS,
  n = 1000,
  param = NULL,     # pas de paramètre supplémentaire
  zone = "zone",    # ta variable de zone
  cap = 3600        # plafonnement du dt si nécessaire
)

GPS_sampled <- st_as_sf(GPS_sampled, coords = c("lon", "lat"), crs = 4326) %>%
  mutate(lon = st_coordinates(.)[, 1], lat = st_coordinates(.)[, 2])

zoom_levels <- c("A", "B", "C")
results_kud <- NULL
nb_kud <- NULL
analyse <- "make_kud_sampled"
comportement <- "Roosting"
couleur <- couleur_roosting

plan(multisession, workers = 3)

results_list <- future_lapply(
  zoom_levels,
  function(z) {
    make_kud(analyse, z, comportement, GPS_sampled, data_generated_path, resolution_ZOOM, couleur)
  },
  future.seed = TRUE # garantit des tirages aléatoires reproductibles et indépendants
)

## point chaud #-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#----

# Filtrer les points GPS où le comportement est "roosting" (repos)
# Puis sélectionner les colonnes ID, longitude et latitude, enlever la géométrie, et supprimer les valeurs manquantes
coords_roosting_ID_hotspot <- GPS %>%
  filter(behavior == "roosting") %>%
  dplyr::select(ID, lon, lat) %>%
  st_drop_geometry() %>%
  na.omit()

# Convertir les coordonnées en un objet sf (simple features) avec système de coordonnées WGS84 (EPSG:4326)
locs_roosting_ID_hotspot <- st_as_sf(coords_roosting_ID_hotspot, coords = c("lon", "lat"), crs = 4326)

# Transformer les coordonnées en UTM zone 30N (EPSG:32630), utile pour les analyses spatiales métriques
locs_roosting_ID_hotspot_32630 <- st_transform(locs_roosting_ID_hotspot, crs = 32630)

# Définir la projection UTM comme chaîne EPSG
crs_utm <- "EPSG:32630"

# Reprojeter un raster de base (100x100) dans le système UTM
SpatRaster <- project(raster_100x100, crs_utm)

# Convertir l’objet SpatRaster en RasterLayer (compatible avec certaines fonctions spatiales classiques)
RasterLayer <- raster(SpatRaster)

# Convertir le RasterLayer en objet SpatialPixels (grille spatiale pour KDE)
SpatialPixels <- as(RasterLayer, "SpatialPixels")

# Extraire les coordonnées (x, y) des points GPS projetés
coords_roosting_ID_hotspot_32630 <- st_coordinates(locs_roosting_ID_hotspot_32630)

# Calcul de l'écart-type sur les axes X et Y (pour la règle de Silverman)
sigma_x_roosting_ID_hotspot <- sd(coords_roosting_ID_hotspot_32630[, 1])
sigma_y_roosting_ID_hotspot <- sd(coords_roosting_ID_hotspot_32630[, 2])

# Nombre de points
n_roosting_ID_hotspot <- nrow(coords_roosting_ID_hotspot_32630)

# Calcul de la largeur de bande (bandwidth) selon la règle de Silverman (divisée par 2 pour lisser davantage)
h_silverman_x_roosting_ID_hotspot <- 1.06 * sigma_x_roosting_ID_hotspot * n_roosting_ID_hotspot^(-1 / 5) / 2
h_silverman_y_roosting_ID_hotspot <- 1.06 * sigma_y_roosting_ID_hotspot * n_roosting_ID_hotspot^(-1 / 5) / 2

# Conversion en objet Spatial (nécessaire pour 'kernelUD' du package 'adehabitatHR')
locs_spa_roosting_ID_hotspot <- as(locs_roosting_ID_hotspot_32630, "Spatial")

# Calcul de l’estimation de la densité d’utilisation (kernel utilization distribution)
kud_roosting_ID_hotspot <- kernelUD(
  locs_spa_roosting_ID_hotspot["ID"], # Par individu
  grid = SpatialPixels, # Grille définie pour KDE
  h = mean(c(h_silverman_x_roosting_ID_hotspot, h_silverman_y_roosting_ID_hotspot)) # Largeur de bande moyenne
)

# Extraction des contours à 95% (aire d’utilisation générale) et 50% (noyau d’activité)
kde_roosting_95 <- getverticeshr(kud_roosting_ID_hotspot, 95)
kde_roosting_50 <- getverticeshr(kud_roosting_ID_hotspot, 50)

# Conversion en objets sf (pour faciliter l'affichage et les opérations spatiales ultérieures)
kde_roosting_95_sf <- st_as_sf(kde_roosting_95)
kde_roosting_50_sf <- st_as_sf(kde_roosting_50)

# Création d’une liste de cartes d’utilisation par individu (ID)
UDmaps_list_roosting_ID_hotspot <- lapply(names(kud_roosting_ID_hotspot), function(ID) {
  print(ID) # Affiche l’ID en cours de traitement

  # Extraire l’objet KDE pour cet individu
  kud_single_roosting_ID_hotspot <- kud_roosting_ID_hotspot[[ID]]

  # Convertir l’objet KDE en raster
  rast_roosting_ID_hotspot <- rast(kud_single_roosting_ID_hotspot)

  # Générer les contours (isolignes) depuis le raster
  contour_roosting_ID_hotspot <- as.contour(rast_roosting_ID_hotspot)

  # Conversion en objet sf
  sf_roosting_ID_hotspot <- st_as_sf(contour_roosting_ID_hotspot)

  # Cast en polygones (si ce sont des multilignes au départ)
  cast_roosting_ID_hotspot <- st_cast(sf_roosting_ID_hotspot, "POLYGON")

  # Ajouter l’identifiant de l’individu
  cast_roosting_ID_hotspot$ID <- ID

  return(cast_roosting_ID_hotspot) # Retourne les polygones KDE pour cet ID
})

# Fusion de toutes les cartes d’utilisation individuelles en un seul objet sf
UDMap_final_roosting_ID_hotspot <- do.call(rbind, UDmaps_list_roosting_ID_hotspot)

# Conversion de la colonne ID en facteur (utile pour le facettage ou la coloration dans les cartes)
UDMap_final_roosting_ID_hotspot$ID <- as.factor(UDMap_final_roosting_ID_hotspot$ID)

# Sauvegarder les polygones KDE fusionnés par ID dans un fichier GeoPackage
st_write(UDMap_final_roosting_ID_hotspot, paste0(data_generated_path, "UDMap_final_roosting_ID_hotspot.gpkg"), append = FALSE)

# Relire ce fichier (utile si redémarrage ou script séparé)
UDMap_final_roosting_ID_hotspot <- st_read(file.path(data_generated_path, "UDMap_final_roosting_ID_hotspot.gpkg"))

# Calculer l’aire de chaque polygone KDE (en unités de la projection UTM)
polygons_roosting_ID_hotspot <- UDMap_final_roosting_ID_hotspot %>%
  mutate(area = st_area(geom))

# Pour chaque individu (ID), conserver uniquement le polygone ayant la plus grande surface
polygons_largest_roosting_ID_hotspot <- polygons_roosting_ID_hotspot %>%
  group_by(ID) %>%
  slice_max(order_by = area, n = 1, with_ties = FALSE) %>%
  ungroup()

# Attribuer un identifiant unique à chaque polygone sélectionné
polygons_largest_roosting_ID_hotspot <- polygons_largest_roosting_ID_hotspot %>%
  mutate(id = row_number())

# Vérifier et corriger les géométries invalides
polygons_largest_roosting_ID_hotspot <- st_make_valid(polygons_largest_roosting_ID_hotspot)

# Calculer toutes les intersections entre les plus grands polygones
intersections_roosting_ID_hotspot <- st_intersection(polygons_largest_roosting_ID_hotspot)

# Ajouter une colonne 'n' représentant le nombre de polygones impliqués dans chaque intersection
intersections_roosting_ID_hotspot <- intersections_roosting_ID_hotspot %>%
  mutate(n = lengths(st_geometry(intersections_roosting_ID_hotspot)))

# On conserve ici toutes les zones superposées (aucun filtrage sur n)
zones_superposees_roosting_ID_hotspot <- intersections_roosting_ID_hotspot

# Appliquer un buffer de 10 m autour des zones superposées pour regrouper les proches
zones_buffered_roosting_ID_hotspot <- st_buffer(zones_superposees_roosting_ID_hotspot, dist = 10)

# Fusionner toutes les zones buffers en une seule géométrie (union)
zones_union_roosting_ID_hotspot <- st_union(zones_buffered_roosting_ID_hotspot)

# Reconvertir en polygones distincts (décompose les multipolygones)
zones_polygons_roosting_ID_hotspot <- st_cast(zones_union_roosting_ID_hotspot, "POLYGON")

# Convertir en objet sf
zones_grouped_roosting_ID_hotspot <- st_as_sf(zones_polygons_roosting_ID_hotspot)

# Ajouter un identifiant de groupe
zones_grouped_roosting_ID_hotspot <- zones_grouped_roosting_ID_hotspot %>%
  mutate(group_id = row_number())

# Associer chaque zone superposée à une zone fusionnée (par intersection spatiale)
join_roosting_ID_hotspot <- st_join(zones_superposees_roosting_ID_hotspot, zones_grouped_roosting_ID_hotspot, join = st_intersects)

# Regrouper par groupe fusionné et additionner le nombre de superpositions
zone_stats_roosting_ID_hotspot <- join_roosting_ID_hotspot %>%
  group_by(group_id) %>%
  summarise(total_superposed = sum(n), .groups = "drop")

# Joindre les statistiques de superposition au groupe de zones
zones_grouped_roosting_ID_hotspot <- left_join(
  zones_grouped_roosting_ID_hotspot,
  st_drop_geometry(zone_stats_roosting_ID_hotspot),
  by = "group_id"
)

# Réaliser une intersection entre les polygones d'origine et les zones superposées
zones_superposees_roosting_ID_hotspot <- st_intersection(
  polygons_largest_roosting_ID_hotspot %>% dplyr::select(ID),
  zones_superposees_roosting_ID_hotspot
)

# Associer chaque petite zone à son groupe fusionné
join_roosting_ID_hotspot <- st_join(zones_superposees_roosting_ID_hotspot, zones_grouped_roosting_ID_hotspot, join = st_intersects)

# Compter le nombre d'ID différents par groupe
zone_id_stats_roosting_ID_hotspot <- join_roosting_ID_hotspot %>%
  st_drop_geometry() %>%
  group_by(group_id) %>%
  summarise(n_ID = n_distinct(ID), .groups = "drop")

# Ajouter cette information au sf des zones fusionnées
zones_grouped_roosting_ID_hotspot <- zones_grouped_roosting_ID_hotspot %>%
  left_join(zone_id_stats_roosting_ID_hotspot, by = "group_id")

# Garder uniquement les zones ayant au moins un ID (possibilité de filtrer à >= 1)
hotspot_roosting_ID_hotspot <- zones_grouped_roosting_ID_hotspot %>%
  filter(n_ID >= 1)

# Reconvertir la variable en facteur pour la cartographie
hotspot_roosting_ID_hotspot$n_ID <- as.factor(hotspot_roosting_ID_hotspot$n_ID)

# Sauvegarder les hotspots identifiés
st_write(hotspot_roosting_ID_hotspot, paste0(data_generated_path, "hotspot_roosting_ID_hotspot.gpkg"), append = FALSE)

# Relire pour garantir la cohérence
hotspot_roosting_ID_hotspot <- st_read(file.path(data_generated_path, "hotspot_roosting_ID_hotspot.gpkg"))

# Définir l’ordre des modalités du facteur (utile pour la légende)
hotspot_roosting_ID_hotspot$n_ID <- factor(hotspot_roosting_ID_hotspot$n_ID,
  levels = c("1", "2", "3", "4", "5", "6", "27")
)

tmap_mode("view") # Mode interactif

# Création de la carte des hotspots de roosting
UDMap_roosting_hotspot <- tm_scalebar() +
  tm_basemap(c("OpenStreetMap", "Esri.WorldImagery", "CartoDB.Positron")) +
  tm_shape(hotspot_roosting_ID_hotspot) +
  tm_polygons(
    border.col = "white", fill = "n_ID", fill_alpha = 0.8,
    palette = c(
      "1" = "gray95", "2" = "gray85", "3" = "#F3E79AFF",
      "4" = "#F9B881FF", "5" = "#F28891FF", "6" = "#CF63A6FF", "27" = "#9650A6FF"
    )
  ) +
  tm_shape(site_baguage) + # Lieux de baguage (points de repère ?)
  tm_text("icone", size = 1.5) +
  tm_shape(terre_mer) + # Frontière terre-mer ?
  tm_lines(col = "lightblue", lwd = 0.1)
UDMap_roosting_hotspot

# Sauvegarder la carte au format HTML interactif
tmap_save(UDMap_roosting_hotspot, paste0(atlas_path, "UDMap_roosting_hotspot_from1id.html"))

############################################################################ ---
# 6. Zones d'alimentation -------------------------------------------------------
############################################################################ ---

# --- objectif ---
# localisation des principales d'alimentation (durant les marées hautes)
# localisation zone par zone
# et localisation tout la zone d'étude, sous forme de point chaud avec le nombre d'individu sur chaque zone d'alimentation

## zone A, B, C #-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#----

# # paramètres
# zoom_levels <- c("A", "B", "C")
# results_kud <- NULL
# nb_kud <- NULL
# analyse <- "foraging_95_50_hpar4"
# comportement <- "foraging"
# couleur <- couleur_foraging
# 
# # estimer les kernelUD ---
# 
# plan(multisession, workers = 3)
# 
# results_list <- future_map(
#   zoom_levels, # vecteur : "A", "B", "C"
#   ~ estimate_kud(
#     analyse = analyse,
#     zoom_level = .x, # "A" ... puis "B" ... puis "C"
#     comportement = comportement,
#     GPS = GPS,
#     data_generated_path = data_generated_path,
#     resolution_ZOOM = resolution_ZOOM
#   ),
#   .options = furrr_options(seed = TRUE, packages = "raster")
# )
# 
# results_kud.foraging_95_50 <- do.call(rbind, results_list)
# st_write(results_kud.foraging_95_50, paste0(data_generated_path, "results_kud_", analyse, ".gpkg"), append = FALSE)
# 
# # compter les nb ind par zoom ---
# 
# nb_kud_map.foraging_95_50 <- future_map(
#   zoom_levels, # vecteur : "A", "B", "C"
#   ~ count_nb_kud(
#     zoom_level = .x, # "A" ... puis "B" ... puis "C"
#     comportement = comportement
#   ),
#   .options = furrr_options(seed = TRUE)
# )
# 
# nb.foraging_95_50 <- do.call(rbind, nb_kud_map.foraging_95_50)
# write.csv(nb.foraging_95_50, paste0(data_generated_path, "nb.", analyse, ".csv"), row.names = FALSE)
# 
# # resultats ---
# 
# results_kud.foraging_95_50_hpar4 <- st_read(file.path(data_generated_path, paste0("results_kud_", analyse, ".gpkg")))
# nb.foraging_95_50_hpar4 <- read.csv(paste0(data_generated_path, paste0("nb.", analyse, ".csv")), row.names = NULL)
# 
# maps_list.foraging <- future_map(
#   zoom_levels,
#   ~ create_map(
#     analyse = analyse,
#     zoom_level = .x,
#     couleur = couleur
#   ),
#   .options = furrr_options(
#     seed = TRUE,
#     globals = list(
#       create_map = create_map,
#       analyse = analyse,
#       couleur = couleur,
#       ZOOM_A = ZOOM_A,
#       ZOOM_B = ZOOM_B,
#       ZOOM_C = ZOOM_C,
#       labels_ZOOM_A = labels_ZOOM_A,
#       labels_ZOOM_B = labels_ZOOM_B,
#       labels_ZOOM_C = labels_ZOOM_C,
#       results_kud.foraging_95_50_hpar4 = results_kud.foraging_95_50_hpar4,
#       nb.foraging_95_50_hpar4 = nb.foraging_95_50_hpar4,
#       terre_mer = terre_mer,
#       site_baguage = site_baguage,
#       atlas_path = atlas_path
#     )
#   )
# )

# 10/09/2025 ---

GPS_sampled <- sample_weighted_points(
  data = GPS,
  n = 1000,
  param = NULL,     # pas de paramètre supplémentaire
  zone = "zone",    # ta variable de zone
  cap = 3600        # plafonnement du dt si nécessaire
)

GPS_sampled <- st_as_sf(GPS_sampled, coords = c("lon", "lat"), crs = 4326) %>%
  mutate(lon = st_coordinates(.)[, 1], lat = st_coordinates(.)[, 2])

zoom_levels <- c("A", "B", "C")
results_kud <- NULL
nb_kud <- NULL
analyse <- "make_kud_sampled"
comportement <- "Foraging"
couleur <- couleur_foraging

plan(multisession, workers = 3)

results_list <- future_lapply(
  zoom_levels,
  function(z) {
    make_kud(analyse, z, comportement, GPS_sampled, data_generated_path, resolution_ZOOM, couleur)
  },
  future.seed = TRUE # garantit des tirages aléatoires reproductibles et indépendants
)

## point chaud #-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#----

coords_foraging_ID_hotspot <- GPS %>%
  filter(behavior == "foraging") %>%
  dplyr::select(ID, lon, lat) %>%
  st_drop_geometry() %>%
  na.omit()

locs_foraging_ID_hotspot <- st_as_sf(coords_foraging_ID_hotspot, coords = c("lon", "lat"), crs = 4326)
locs_foraging_ID_hotspot_32630 <- st_transform(locs_foraging_ID_hotspot, crs = 32630)

crs_utm <- "EPSG:32630"
SpatRaster <- project(raster_100x100, crs_utm)
RasterLayer <- raster(SpatRaster)
SpatialPixels <- as(RasterLayer, "SpatialPixels")

coords_foraging_ID_hotspot_32630 <- st_coordinates(locs_foraging_ID_hotspot_32630)
sigma_x <- sd(coords_foraging_ID_hotspot_32630[, 1])
sigma_y <- sd(coords_foraging_ID_hotspot_32630[, 2])
n <- nrow(coords_foraging_ID_hotspot_32630)
h_x <- 1.06 * sigma_x * n^(-1 / 5) / 2
h_y <- 1.06 * sigma_y * n^(-1 / 5) / 2

locs_spa <- as(locs_foraging_ID_hotspot_32630, "Spatial")
kud <- kernelUD(locs_spa["ID"], grid = SpatialPixels, h = mean(c(h_x, h_y)))
kde_95 <- getverticeshr(kud, 95)
kde_50 <- getverticeshr(kud, 50)
kde_95_sf <- st_as_sf(kde_95)
kde_50_sf <- st_as_sf(kde_50)

UDmaps_list <- lapply(names(kud), function(ID) {
  kud_single <- kud[[ID]]
  rast <- rast(kud_single)
  contour <- as.contour(rast)
  sf <- st_as_sf(contour)
  poly <- st_cast(sf, "POLYGON")
  poly$ID <- ID
  return(poly)
})

UDMap_final <- do.call(rbind, UDmaps_list)
UDMap_final$ID <- as.factor(UDMap_final$ID)

st_write(UDMap_final, paste0(data_generated_path, "UDMap_final_foraging_ID_hotspot.gpkg"), append = FALSE)
UDMap_final <- st_read(file.path(data_generated_path, "UDMap_final_foraging_ID_hotspot.gpkg"))

polygons <- UDMap_final %>%
  mutate(area = st_area(geom)) %>%
  group_by(ID) %>%
  slice_max(order_by = area, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(id = row_number())
polygons <- st_make_valid(polygons)

intersections <- st_intersection(polygons) %>%
  mutate(n = lengths(st_geometry(.)))
zones_superposees <- intersections

zones_buffered <- st_buffer(zones_superposees, dist = 100)
zones_union <- st_union(zones_buffered)
zones_polygons <- st_cast(zones_union, "POLYGON")
zones_grouped <- st_as_sf(zones_polygons) %>%
  mutate(group_id = row_number())

join1 <- st_join(zones_superposees, zones_grouped, join = st_intersects)
zone_stats <- join1 %>%
  group_by(group_id) %>%
  summarise(total_superposed = sum(n), .groups = "drop")
zones_grouped <- left_join(zones_grouped, st_drop_geometry(zone_stats), by = "group_id")

zones_superposees <- st_intersection(polygons %>% select(ID), zones_superposees)
join2 <- st_join(zones_superposees, zones_grouped, join = st_intersects)
zone_id_stats <- join2 %>%
  st_drop_geometry() %>%
  group_by(group_id) %>%
  summarise(n_ID = n_distinct(ID), .groups = "drop")
zones_grouped <- zones_grouped %>%
  left_join(zone_id_stats, by = "group_id")

hotspot <- zones_grouped %>%
  filter(n_ID >= 1)
hotspot$n_ID <- as.factor(hotspot$n_ID)

st_write(hotspot, paste0(data_generated_path, "hotspot_foraging_ID_hotspot.gpkg"), append = FALSE)
hotspot <- st_read(file.path(data_generated_path, "hotspot_foraging_ID_hotspot.gpkg"))

hotspot$n_ID <- factor(hotspot$n_ID, levels = c("1", "2", "3", "4", "34"))

tmap_mode("view")
UDMap_plot <- tm_scalebar() +
  tm_basemap(c("OpenStreetMap", "Esri.WorldImagery", "CartoDB.Positron")) +
  tm_shape(hotspot) +
  tm_polygons(
    border.col = "grey", fill = "n_ID", fill_alpha = 1,
    palette = c("1" = "gray95", "2" = "gray85", "3" = "#59C8B2FF", "4" = "#0095AFFF", "34" = "#26185FFF")
  ) +
  tm_shape(site_baguage) +
  tm_text("icone", size = 1.5) +
  tm_shape(terre_mer) +
  tm_lines(col = "lightblue", lwd = 0.1)
UDMap_plot

tmap_save(UDMap_plot, paste0(atlas_path, "UDMap_foraging_hotspot.html"))

############################################################################ ---
# 7. !!!!!!!!!!!!Stabilité inter-marée ------------------------------------------------
############################################################################ ---

# --- objectif ---
# estimation de la répétébilité/variabilité de la zone de repos ou d'alimentation entre cycle de marée
# pour chauqe individu, et chaque marée
# estimation du chevauchement des kernelUD de marée en marée



fidel_inter_maree_dt_1 <- GPS %>%
  dplyr::select(ID, behavior, datetime) %>%
  filter(behavior != "other") %>%
  distinct() %>%
  na.omit()

fidel_inter_maree_dt_2 <- fidel_inter_maree_dt_1 %>%
  st_drop_geometry() %>%
  arrange(ID, datetime) %>%
  group_by(ID) %>%
  mutate(
    time_diff = as.numeric(difftime(datetime, lag(datetime), units = "mins")),
    new_group = if_else(is.na(time_diff) | time_diff > 60 * 6, 1, 0),
    group_id = cumsum(new_group)
  ) %>%
  ungroup() %>%
  na.omit()

# GPS <- GPS %>%
#   left_join(fidel_inter_maree_dt_2) %>%
#   mutate(ID_maree = paste0(ID, "_", group_id))

GPS.maree_repet_roosting <- GPS %>%
  st_drop_geometry() %>%
  filter(behavior == "roosting") %>%
  left_join(fidel_inter_maree_dt_2) %>%
  mutate(ID_maree = paste0(ID, "_", group_id)) %>%
  dplyr::select(ID, datetime, lon, lat, ID_maree) %>%
  na.omit()

# Repétabilité / individual scale

# n_maree_per_ID <- fidel_inter_maree_dt_2 %>%
#   dplyr::select(ID, group_id) %>%
#   distinct() %>%
#   group_by(ID) %>%
#   summarize(n = n()) %>%
#   filter(n < 200)
#
# # au moins x points GPS par maree
# n_per_maree <- GPS.maree_repet %>%
#   group_by(ID_maree) %>%
#   summarize(n = n()) %>%
#   filter(n < 30)
#
# GPS.maree_repet <- GPS.maree_repet %>%
#   filter(ID_maree %ni% n_per_maree$ID_maree,
#          ID %ni% n_maree_per_ID$ID)
#
# table(GPS.maree_repet$ID)




####

# au moins x points GPS par maree pour chaque ind
# IDs_avec_30_maree <- GPS.maree_repet %>%
#   distinct(ID, ID_maree) %>%
#   count(ID, name = "n_maree") %>%
#   filter(n_maree >= 100)

# au moins x points GPS par maree pour chaque ind

table(GPS.maree_repet_roosting$ID_maree)

n_point_per_maree <- GPS.maree_repet_roosting %>%
  dplyr::select(ID, ID_maree) %>%
  group_by(ID_maree) %>%
  summarize(n = n()) %>%
  filter(n >= 100)

GPS.maree_repet_2 <- GPS.maree_repet %>%
  filter(ID_maree %in% n_point_per_maree$ID_maree)

# au moins x maree pour chaque ind
n_maree <- GPS.maree_repet_2 %>%
  dplyr::select(ID, ID_maree) %>%
  group_by(ID) %>%
  summarize(n = n()) %>%
  filter(n >= 100)

# Étape 4 : filtrer le tableau principal avec ces ID
GPS.maree_repet_3 <- GPS.maree_repet_2 %>%
  filter(ID %in% n_maree$ID)

table(GPS.maree_repet_3$ID) # nb marée par ind
table(GPS.maree_repet_3$ID_maree) # nb point par ind


####






GPS.maree_repet <- GPS.maree_repet_roosting

# Transformer en objet spatial (EPSG:4326)
GPS.maree_repet <- st_as_sf(GPS.maree_repet, coords = c("lon", "lat"), crs = 4326)
GPS.maree_repet <- st_transform(GPS.maree_repet, crs = 32630)

# raster/grid
crs_utm <- "EPSG:32630"
SpatRaster <- project(raster_100x100, crs_utm)
RasterLayer <- raster(SpatRaster)
SpatialPixels <- as(RasterLayer, "SpatialPixels")

# Extraire les coordonnées reprojetées
coords.maree_repet <- st_coordinates(GPS_spa.maree_repet)

# Règle de Silverman
sigma_x.roosting_maree_repet <- sd(coords.maree_repet[, 1])
sigma_y_roosting_maree_repet <- sd(coords.maree_repet[, 2])
n.roosting_maree_repet <- nrow(GPS_spa.maree_repet)

h.silverman_x_roosting_maree_repet <- 1.06 * sigma_x.roosting_maree_repet * n.roosting_maree_repet^(-1 / 5) / 2
h.silverman_y_roosting_maree_repet <- 1.06 * sigma_y_roosting_maree_repet * n.roosting_maree_repet^(-1 / 5) / 2

GPS_spa.maree_repet <- as(GPS_spa.maree_repet, "Spatial")

kud.roosting_maree_repet <- kernelUD(GPS_spa.maree_repet["ID_maree"],
  grid = as(SpatialPixels, "SpatialPixels"),
  h = mean(c(
    h.silverman_x_roosting_maree_repet,
    h.silverman_y_roosting_maree_repet
  ))
)

table(GPS_spa.maree_repet@data$ID_maree)

##                     ##
## valeur répétabilité ##
##                     ##

# Estimation valeur d'overlapp par ind entre chaque maree

# Extraire les noms uniques des individus
individus <- unique(GPS_spa.maree_repet$ID)

# Stocker les résultats
overlap_results.roosting_maree_repet <- NULL

ind <- "EA580462"

# Boucle sur chaque individu
for (ind in individus) {
  print(ind)

  # Trouver les noms des périodes de cet individu dans hr_kde
  ID_periodes <- names(kud.roosting_maree_repet)[grep(paste0("^", ind, "_"), names(kud.roosting_maree_repet))]

  # Vérifier que l'individu a bien deux périodes
  if (length(ID_periodes) >= 2) {
    # Créer un estUDm valide
    hr_kde_ind.roosting_maree_repet <- kud.roosting_maree_repet[ID_periodes]
    class(hr_kde_ind.roosting_maree_repet) <- "estUDm" # Important pour que kerneloverlaphr() fonctionne

    # Calculer l'overlap entre les deux périodes
    overlap_value.roosting_maree_repet <- kerneloverlaphr(hr_kde_ind.roosting_maree_repet,
      method = "BA"
    )[1, 2]

    info_ind.roosting_maree_repet <- c(ind, overlap_value.roosting_maree_repet)

    # Stocker le résultat
    # overlap_results <- rbind(overlap_results, data.frame(Individu = ind, Overlap = overlap_value))
    overlap_results.roosting_maree_repet <- rbind(overlap_results.roosting_maree_repet, info_ind.roosting_maree_repet)
  }
}

overlap_results.roosting_maree_repet <- as.data.frame(overlap_results.roosting_maree_repet)

# write & read
st_write(overlap_results.roosting_maree_repet, paste0(data_generated_path, "overlap_results.roosting_maree_repet.gpkg"), append = FALSE)
overlap_results.roosting_maree_repet <- st_read(file.path(data_generated_path, "overlap_results.roosting_maree_repet.gpkg"))

overlap_results.roosting_maree_repet <- overlap_results.roosting_maree_repet %>%
  rename(ID = V1, overlap = V2)

overlap_results.roosting_maree_repet$overlap <- as.numeric(overlap_results.roosting_maree_repet$overlap)

mean_overlap.roosting_maree_repet <- mean(overlap_results.roosting_maree_repet$overlap, na.rm = T)
mean_overlap.roosting_maree_repet

# Afficher les résultats
overlap_results.roosting_maree_repet <- overlap_results.roosting_maree_repet[order(overlap_results.roosting_maree_repet$overlap), ]
overlap_results.roosting_maree_repet

# ajout sexe et age
sex_age_dt <- GPS %>%
  st_drop_geometry() %>%
  dplyr::select(ID, age, sex) %>%
  na.omit() %>%
  distinct()

overlap_results.roosting_maree_repet <- overlap_results.roosting_maree_repet %>%
  left_join(sex_age_dt)

# plot
plot.roosting_maree_repet <- ggplot(
  overlap_results.roosting_maree_repet,
  aes(
    x = reorder(ID, overlap), y = overlap,
    fill = sex, shape = age
  )
) +
  geom_point(size = 4) +
  theme_classic() +
  theme(legend.position = c(.15, .5)) +
  scale_fill_gradientn(colors = paletteer_c("grDevices::Sunset", 10, direction = -1)) +
  # scale_fill_manual(values = c("F" = "#541388", "M" = "#FFF07C", NA = "grey")) +
  # scale_shape_manual(values = c("adult" = 21, "juv" = 17, NA = 20)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(
    title = "",
    x = "Individu", y = "Pourcentage de chevauchement moyen
de zone de reposoirs entre années"
  )
plot.roosting_maree_repet

ggsave(paste0(atlas_path, "/plot.roosting_maree_repet.png"),
  plot = plot.roosting_maree_repet, width = 8, height = 5, dpi = 1000
)

## !alimentation #-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

# old mais qui marche ???????????????????????

GPS.maree_repet <- GPS %>%
  filter(behavior == "foraging") %>%
  left_join(fidel_inter_maree_dt_2) %>%
  mutate(ID_maree = paste0(ID, "_", group_id)) %>%
  dplyr::select(ID, datetime, lon, lat, ID_maree) %>%
  st_drop_geometry() %>%
  na.omit()

# Repétabilité / individual scale

# au moins 5 point par group
n_per_maree <- GPS.maree_repet %>%
  group_by(ID_maree) %>%
  summarize(n = n()) %>%
  filter(n <= 5)

GPS.maree_repet <- GPS.maree_repet %>%
  filter(ID_maree %ni% n_per_maree$ID_maree)

# Transformer en objet spatial (EPSG:4326)
GPS_spa.maree_repet <- st_as_sf(GPS.maree_repet, coords = c("lon", "lat"), crs = 4326)
GPS_spa.maree_repet <- st_transform(GPS_spa.maree_repet, crs = 32630)

# raster/grid
crs_utm <- "EPSG:32630"
SpatRaster <- project(raster_100x100, crs_utm)
RasterLayer <- raster(SpatRaster)
SpatialPixels <- as(RasterLayer, "SpatialPixels")

# Extraire les coordonnées reprojetées
coords.maree_repet <- st_coordinates(GPS_spa.maree_repet)

# Règle de Silverman
sigma_x.foraging_maree_repet <- sd(coords.maree_repet[, 1])
sigma_y_foraging_maree_repet <- sd(coords.maree_repet[, 2])
n.foraging_maree_repet <- nrow(GPS_spa.maree_repet)

h.silverman_x_foraging_maree_repet <- 1.06 * sigma_x.foraging_maree_repet * n.foraging_maree_repet^(-1 / 5) / 2
h.silverman_y_foraging_maree_repet <- 1.06 * sigma_y_foraging_maree_repet * n.foraging_maree_repet^(-1 / 5) / 2

GPS_spa.maree_repet <- as(GPS_spa.maree_repet, "Spatial")

kud.foraging_maree_repet <- kernelUD(GPS_spa.maree_repet["ID_maree"],
  grid = as(SpatialPixels, "SpatialPixels"),
  h = mean(c(
    h.silverman_x_foraging_maree_repet,
    h.silverman_y_foraging_maree_repet
  ))
)

##                     ##
## valeur répétabilité ##
##                     ##

# Estimation valeur d'overlapp par ind entre chaque maree

# Extraire les noms uniques des individus
individus <- unique(GPS_spa.maree_repet$ID)

# Stocker les résultats
overlap_results.foraging_maree_repet <- NULL

ind <- "EA580462"

# Boucle sur chaque individu
for (ind in individus) {
  print(ind)

  # Trouver les noms des périodes de cet individu dans hr_kde
  ID_periodes <- names(kud.foraging_maree_repet)[grep(paste0("^", ind, "_"), names(kud.foraging_maree_repet))]

  # Vérifier que l'individu a bien deux périodes
  if (length(ID_periodes) >= 2) {
    # Créer un estUDm valide
    hr_kde_ind.foraging_maree_repet <- kud.foraging_maree_repet[ID_periodes]
    class(hr_kde_ind.foraging_maree_repet) <- "estUDm" # Important pour que kerneloverlaphr() fonctionne

    # Calculer l'overlap entre les deux périodes
    overlap_value.foraging_maree_repet <- kerneloverlaphr(hr_kde_ind.foraging_maree_repet,
      method = "BA"
    )[1, 2]

    info_ind.foraging_maree_repet <- c(ind, overlap_value.foraging_maree_repet)

    # Stocker le résultat
    # overlap_results <- rbind(overlap_results, data.frame(Individu = ind, Overlap = overlap_value))
    overlap_results.foraging_maree_repet <- rbind(overlap_results.foraging_maree_repet, info_ind.foraging_maree_repet)
  }
}

overlap_results.foraging_maree_repet <- as.data.frame(overlap_results.foraging_maree_repet)

# write & read
st_write(overlap_results.foraging_maree_repet, paste0(data_generated_path, "overlap_results.foraging_maree_repet.gpkg"), append = FALSE)
overlap_results.foraging_maree_repet <- st_read(file.path(data_generated_path, "overlap_results.foraging_maree_repet.gpkg"))

overlap_results.foraging_maree_repet <- overlap_results.foraging_maree_repet %>%
  rename(ID = V1, overlap = V2)

overlap_results.foraging_maree_repet$overlap <- as.numeric(overlap_results.foraging_maree_repet$overlap)

mean_overlap.foraging_maree_repet <- mean(overlap_results.foraging_maree_repet$overlap, na.rm = T)
mean_overlap.foraging_maree_repet

# Afficher les résultats
overlap_results.foraging_maree_repet <- overlap_results.foraging_maree_repet[order(overlap_results.foraging_maree_repet$overlap), ]
overlap_results.foraging_maree_repet

# ajout sexe et age
sex_age_dt <- GPS %>%
  st_drop_geometry() %>%
  dplyr::select(ID, age, sex) %>%
  na.omit() %>%
  distinct()

overlap_results.foraging_maree_repet <- overlap_results.foraging_maree_repet %>%
  left_join(sex_age_dt)

# plot
plot.foraging_maree_repet <- ggplot(
  overlap_results.foraging_maree_repet,
  aes(
    x = reorder(ID, overlap), y = overlap,
    fill = sex, shape = age
  )
) +
  geom_point(size = 4, color = "black") +
  theme_classic() +
  # theme(legend.position = c(.15, .5)) +
  scale_fill_manual(values = c("F" = "#9650A6FF", "M" = "#F9B881FF"), na.value = "grey") +
  scale_shape_manual(values = c("adult" = 21, "juv" = 24), na.value = 22) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(
    title = "",
    x = "Individu", y = "Pourcentage de chevauchement moyen
de zone de reposoirs entre années", fill = "sexe"
  )
plot.foraging_maree_repet

ggsave(paste0(atlas_path, "/plot.foraging_maree_repet.png"),
  plot = plot.foraging_maree_repet, width = 8, height = 5, dpi = 1000
)









############################################################################ ---
# 8. Mois ----------------------------------------------------------------------
############################################################################ ---

# --- objectif ---
# localisation de la zone de repos ou d'alimentation en fonction du mois de l'année
# zone par zone

## reposoir -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

zoom_level <- c("A", "B", "C", "D", "E")
analyse <- "roosting_month"
results_kud <- NULL
nb_kud <- NULL
comportement <- "roosting"
param <- "month_label"
couleur <- nom_pal_roosting

# estimer les kernelUD
map_kud.roosting_month <- Map(estimate_kud_param, zoom_level, comportement, param)
results_kud.roosting_month <- do.call(rbind, map_kud.roosting_month)
st_write(results_kud.roosting_month, paste0(data_generated_path, "results_kud.", analyse, ".gpkg"), append = FALSE)
# resultats
results_kud.roosting_month <- st_read(file.path(data_generated_path, paste0("results_kud.", analyse, ".gpkg")))
nb_kud.roosting_month <- read.csv(paste0(data_generated_path, paste0("nb_kud.", analyse, ".csv")), row.names = NULL)
maps_list.roosting_ZOOM_month <- Map(create_map_param, zoom_level, analyse, param, couleur)
# compter les nb ind par zoom
nb_kud_map.roosting_month <- Map(count_nb_kud_param, zoom_level, comportement, param)
nb_kud.roosting_month <- do.call(rbind, nb_kud_map.roosting_month)
write.csv(nb_kud.roosting_month, paste0(data_generated_path, "nb_kud.", analyse, ".csv"), row.names = FALSE)

## alimentation -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

zoom_level <- c("A", "B", "C", "D", "E")
analyse <- "foraging_month"
results_kud <- NULL
nb_kud <- NULL
comportement <- "foraging"
param <- "month_label"
couleur <- nom_pal_foraging

# estimer les kernelUD
map_kud.foraging_month <- Map(estimate_kud_param, zoom_level, comportement, param)
results_kud.foraging_month <- do.call(rbind, map_kud.foraging_month)
st_write(results_kud.foraging_month, paste0(data_generated_path, "results_kud.", analyse, ".gpkg"), append = FALSE)
# compter les nb ind par zoom
nb_kud_map.foraging_month <- Map(count_nb_kud_param, zoom_level, comportement, param)
nb_kud.foraging_month <- do.call(rbind, nb_kud_map.foraging_month)
write.csv(nb_kud.foraging_month, paste0(data_generated_path, "nb_kud.", analyse, ".csv"), row.names = FALSE)
# resultats
results_kud.foraging_month <- st_read(file.path(data_generated_path, paste0("results_kud.", analyse, ".gpkg")))
nb_kud.foraging_month <- read.csv(paste0(data_generated_path, paste0("nb_kud.", analyse, ".csv")), row.names = NULL)
maps_list.foraging_ZOOM_month <- Map(create_map_param, zoom_level, analyse, param, couleur)





# make kud test 

zoom_levels <- c("A", "B", "C")
results_kud <- NULL
nb_kud <- NULL
analyse <- "roosting_month_make_kud_test"
comportement <- "roosting"
couleur <- couleur_roosting

param <- "month_label"
couleur <- nom_pal_roosting



library(future.apply)

plan(multisession, workers = 3)

results_list <- future_lapply(
  zoom_levels,
  function(z) {
    make_kud(analyse, z, comportement, GPS, data_generated_path, resolution_ZOOM, couleur)
  },
  future.seed = TRUE # garantit des tirages aléatoires reproductibles et indépendants
)

############################################################################ ---
# 9. Jour & nuit ---------------------------------------------------------------
############################################################################ ---

# --- objectif ---
# localisation de la zone de repos ou d'alimentation en fonction du jour et de la nuit
# zone par zone

## reposoir -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

zoom_level <- c("A", "B", "C", "D", "E")
analyse <- "roosting_jour_nuit"
results_kud <- NULL
nb_kud <- NULL
comportement <- "roosting"
param <- "jour_nuit"
couleur <- nom_pal_roosting

# estimer les kernelUD
map_kud.roosting_jour_nuit <- Map(estimate_kud_param, zoom_level, comportement, param)
results_kud.roosting_jour_nuit <- do.call(rbind, map_kud.roosting_jour_nuit)
st_write(results_kud.roosting_jour_nuit, paste0(data_generated_path, "results_kud.", analyse, ".gpkg"), append = FALSE)
# compter les nb ind par zoom
nb_kud_map.roosting_jour_nuit <- Map(count_nb_kud_param, zoom_level, comportement, param)
nb_kud.roosting_jour_nuit <- do.call(rbind, nb_kud_map.roosting_jour_nuit)
write.csv(nb_kud.roosting_jour_nuit, paste0(data_generated_path, "nb_kud.", analyse, ".csv"), row.names = FALSE)
# resultats
results_kud.roosting_jour_nuit <- st_read(file.path(data_generated_path, paste0("results_kud.", analyse, ".gpkg")))
nb_kud.roosting_jour_nuit <- read.csv(paste0(data_generated_path, paste0("nb_kud.", analyse, ".csv")), row.names = NULL)
maps_list.roosting_ZOOM_jour_nuit <- Map(create_map_param, zoom_level, analyse, param, couleur)

## alimentation -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

zoom_level <- c("A", "B", "C", "D", "E")
analyse <- "foraging_jour_nuit"
results_kud <- NULL
nb_kud <- NULL
comportement <- "foraging"
param <- "jour_nuit"
couleur <- nom_pal_foraging

# estimer les kernelUD
map_kud.foraging_jour_nuit <- Map(estimate_kud_param, zoom_level, comportement, param)
results_kud.foraging_jour_nuit <- do.call(rbind, map_kud.foraging_jour_nuit)
st_write(results_kud.foraging_jour_nuit, paste0(data_generated_path, "results_kud.", analyse, ".gpkg"), append = FALSE)
# compter les nb ind par zoom
nb_kud_map.foraging_jour_nuit <- Map(count_nb_kud_param, zoom_level, comportement, param)
nb_kud.foraging_jour_nuit <- do.call(rbind, nb_kud_map.foraging_jour_nuit)
write.csv(nb_kud.foraging_jour_nuit, paste0(data_generated_path, "nb_kud.", analyse, ".csv"), row.names = FALSE)
# resultats
results_kud.foraging_jour_nuit <- st_read(file.path(data_generated_path, paste0("results_kud.", analyse, ".gpkg")))
nb_kud.foraging_jour_nuit <- read.csv(paste0(data_generated_path, paste0("nb_kud.", analyse, ".csv")), row.names = NULL)
maps_list.foraging_ZOOM_jour_nuit <- Map(create_map_param, zoom_level, analyse, param, couleur)

############################################################################ ---
# 10. Hauteur de marée ---------------------------------------------------------
############################################################################ ---

# --- objectif ---
# localisation de la zone de repos en fonction de la hauteur d'eau lors des marées hautes
# zone par zone

# paramètres
zoom_level <- c("A", "B", "C", "D", "E")
analyse <- "roosting_tides_high_type"
results_kud <- NULL
nb_kud <- NULL
comportement <- "roosting"
param <- "tides_high_type"
couleur <- nom_pal_roosting

# estimer les kernelUD
map_kud.roosting_tides_high_type <- Map(estimate_kud_param, zoom_level, comportement, param)
results_kud.roosting_tides_high_type <- do.call(rbind, map_kud.roosting_tides_high_type)
st_write(results_kud.roosting_tides_high_type, paste0(data_generated_path, "results_kud.", analyse, ".gpkg"), append = FALSE)
# compter les nb ind par zoom
nb_kud_map.roosting_tides_high_type <- Map(count_nb_kud_param, zoom_level, comportement, param)
nb_kud.roosting_tides_high_type <- do.call(rbind, nb_kud_map.roosting_tides_high_type)
write.csv(nb_kud.roosting_tides_high_type, paste0(data_generated_path, "nb_kud.", analyse, ".csv"), row.names = FALSE)
# resultats
results_kud.roosting_tides_high_type <- st_read(file.path(data_generated_path, paste0("results_kud.", analyse, ".gpkg")))
nb_kud.roosting_tides_high_type <- read.csv(paste0(data_generated_path, paste0("nb_kud.", analyse, ".csv")), row.names = NULL)
maps_list.roosting_ZOOM_tides_high_type <- Map(create_map_param, zoom_level, analyse, param, couleur)





# 10/09/2025 ---

GPS_sampled <- sample_weighted_points(
  data = GPS,
  n = 1000,
  zone = "zone",    # ta variable de zone
  param = "tide_strength",
  cap = 3600        # plafonnement du dt si nécessaire
)

GPS_sampled <- st_as_sf(GPS_sampled, coords = c("lon", "lat"), crs = 4326) %>%
  mutate(lon = st_coordinates(.)[, 1], lat = st_coordinates(.)[, 2])

zoom_levels <- c("A", "B", "C")
results_kud <- NULL
nb_kud <- NULL
analyse <- "make_kud_sampled_hauteur_maree"
comportement <- "Roosting"
couleur <- couleur_roosting
param <- "tide_strength"

plan(multisession, workers = 3)

results_list <- future_lapply(
  zoom_levels,
  function(z) {
    make_kud_param(analyse, z, comportement, GPS_sampled, data_generated_path, resolution_ZOOM, couleur, param)
  },
  future.seed = TRUE # garantit des tirages aléatoires reproductibles et indépendants
)

############################################################################ ---
# 11. Age ----------------------------------------------------------------------
############################################################################ ---

# --- objectif ---
# localisation de la zone de repos ou d'alimentation en fonction du mois de l'age des individus
# zone par zone

## reposoir -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

zoom_level <- c("A", "B", "C", "D", "E")
analyse <- "roosting_age"
results_kud <- NULL
nb_kud <- NULL
comportement <- "roosting"
param <- "age"
couleur <- nom_pal_roosting

# estimer les kernelUD
map_kud.roosting_age <- Map(estimate_kud_param, zoom_level, comportement, param)
results_kud.roosting_age <- do.call(rbind, map_kud.roosting_age)
st_write(results_kud.roosting_age, paste0(data_generated_path, "results_kud.", analyse, ".gpkg"), append = FALSE)
# compter les nb ind par zoom
nb_kud_map.roosting_age <- Map(count_nb_kud_param, zoom_level, comportement, param)
nb_kud.roosting_age <- do.call(rbind, nb_kud_map.roosting_age)
write.csv(nb_kud.roosting_age, paste0(data_generated_path, "nb_kud.", analyse, ".csv"), row.names = FALSE)
# resultats
results_kud.roosting_age <- st_read(file.path(data_generated_path, paste0("results_kud.", analyse, ".gpkg")))
nb_kud.roosting_age <- read.csv(paste0(data_generated_path, paste0("nb_kud.", analyse, ".csv")), row.names = NULL)
maps_list.roosting_ZOOM_age <- Map(create_map_param, zoom_level, analyse, param, couleur)

## alimentation -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

zoom_level <- c("A", "B", "C", "D", "E")
analyse <- "foraging_age"
results_kud <- NULL
nb_kud <- NULL
comportement <- "foraging"
param <- "age"
couleur <- nom_pal_foraging

# estimer les kernelUD
map_kud.foraging_age <- Map(estimate_kud_param, zoom_level, comportement, param)
results_kud.foraging_age <- do.call(rbind, map_kud.foraging_age)
st_write(results_kud.foraging_age, paste0(data_generated_path, "results_kud.", analyse, ".gpkg"), append = FALSE)
# compter les nb ind par zoom
nb_kud_map.foraging_age <- Map(count_nb_kud_param, zoom_level, comportement, param)
nb_kud.foraging_age <- do.call(rbind, nb_kud_map.foraging_age)
write.csv(nb_kud.foraging_age, paste0(data_generated_path, "nb_kud.", analyse, ".csv"), row.names = FALSE)
# resultats
results_kud.foraging_age <- st_read(file.path(data_generated_path, paste0("results_kud.", analyse, ".gpkg")))
nb_kud.foraging_age <- read.csv(paste0(data_generated_path, paste0("nb_kud.", analyse, ".csv")), row.names = NULL)
maps_list.foraging_ZOOM_age <- Map(create_map_param, zoom_level, analyse, param, couleur)

############################################################################ ---
# 12. Sexe ---------------------------------------------------------------------
############################################################################ ---

# --- objectif ---
# localisation de la zone de repos ou d'alimentation en fonction du sexe des individus
# zone par zone

## reposoir -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

zoom_level <- c("A", "B", "C", "D", "E")
analyse <- "roosting_sex"
results_kud <- NULL
nb_kud <- NULL
comportement <- "roosting"
param <- "sex"
couleur <- nom_pal_roosting

# estimer les kernelUD
map_kud.roosting_sex <- Map(estimate_kud_param, zoom_level, comportement, param)
results_kud.roosting_sex <- do.call(rbind, map_kud.roosting_sex)
st_write(results_kud.roosting_sex, paste0(data_generated_path, "results_kud.", analyse, ".gpkg"), append = FALSE)
# compter les nb ind par zoom
nb_kud_map.roosting_sex <- Map(count_nb_kud_param, zoom_level, comportement, param)
nb_kud.roosting_sex <- do.call(rbind, nb_kud_map.roosting_sex)
write.csv(nb_kud.roosting_sex, paste0(data_generated_path, "nb_kud.", analyse, ".csv"), row.names = FALSE)
# resultats
results_kud.roosting_sex <- st_read(file.path(data_generated_path, paste0("results_kud.", analyse, ".gpkg")))
nb_kud.roosting_sex <- read.csv(paste0(data_generated_path, paste0("nb_kud.", analyse, ".csv")), row.names = NULL)
maps_list.roosting_ZOOM_sex <- Map(create_map_param, zoom_level, analyse, param, couleur)

## alimentation -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

zoom_level <- c("A", "B", "C", "D", "E")
analyse <- "foraging_sex"
results_kud <- NULL
nb_kud <- NULL
comportement <- "foraging"
param <- "sex"
couleur <- nom_pal_foraging

# estimer les kernelUD
map_kud.foraging_sex <- Map(estimate_kud_param, zoom_level, comportement, param)
results_kud.foraging_sex <- do.call(rbind, map_kud.foraging_sex)
st_write(results_kud.foraging_sex, paste0(data_generated_path, "results_kud.", analyse, ".gpkg"), append = FALSE)
# compter les nb ind par zoom
nb_kud_map.foraging_sex <- Map(count_nb_kud_param, zoom_level, comportement, param)
nb_kud.foraging_sex <- do.call(rbind, nb_kud_map.foraging_sex)
write.csv(nb_kud.foraging_sex, paste0(data_generated_path, "nb_kud.", analyse, ".csv"), row.names = FALSE)
# resultats
results_kud.foraging_sex <- st_read(file.path(data_generated_path, paste0("results_kud.", analyse, ".gpkg")))
nb_kud.foraging_sex <- read.csv(paste0(data_generated_path, paste0("nb_kud.", analyse, ".csv")), row.names = NULL)
maps_list.foraging_ZOOM_sex <- Map(create_map_param, zoom_level, analyse, param, couleur)

############################################################################ ---
# 13. !!!!!!!!!!!!!!!!!Chasse -------------------------------------------------------------------
############################################################################ ---

## chasse à pied #-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#----

# data sets ---

chasse <- read_delim(paste0(data_path, "Chasse/2025_02_27_16h29m12_XXX_Frequentation_des_sites_Chasseurs__RNMO.csv"),
  delim = ";", escape_double = FALSE, trim_ws = TRUE
)

# chasse_date <- read_excel("D:/Projets_Suzanne/Courlis/3) Data/1) data/Chasse/date ouverture fermeture chasse.xlsx")


# effectif chasse ---

# chasse <- chasse %>%
#   mutate(
#     Saison = case_when(month(date) == 1 ~ paste0(year(date)-1,"/",year(date)),
#                        month(date) != 1 ~ paste0(year(date),"/",year(date)+1)))

# Pas de prospection = NA
chasse$effectif[chasse$effectif == -1] <- NA

hist(chasse$effectif)

# chasse$Saison <- as.character(chasse$Saison)
# chasse_date$Saison <- as.character(chasse_date$Saison)

# chasse_date <- chasse_date %>%
#   dplyr::select(Saison, `Fermeture DPM St Froult`, `Fermeture Gibier d'eau`)

chasse <- chasse %>%
  mutate(year = year(date))

# chasse <- chasse %>%
#   filter(nom_site == "DPM",
#          year >= min(GPS$year, na.rm=T)) %>%
#   dplyr::select("date", "effectif", "Saison", "longitude_centroid", "latitude_centroid")

chasse <- chasse %>%
  filter(
    nom_site == "DPM",
    year >= min(GPS$year, na.rm = T)
  ) %>%
  dplyr::select("date", "effectif", "longitude_centroid", "latitude_centroid")

# chasse_all <- chasse %>%
#   left_join(chasse_date)

# buffer ---

chasse2 <- st_as_sf(chasse, coords = c("longitude_centroid", "latitude_centroid"), crs = 4326)

chasse_buffer <- st_buffer(chasse2[1, ], 1000) %>%
  dplyr::select(geometry)

GPS_chasse <- st_intersection(GPS, chasse_buffer)

table(GPS_chasse$year)

# join GPS + chasse ---

# GPS_chasse <- GPS_chasse %>%
#   mutate(
#     Saison = case_when(month(datetime) == 1 ~ paste0(year(datetime)-1,"/",year(datetime)),
#                        month(datetime) != 1 ~ paste0(year(datetime),"/",year(datetime)+1)))

# Saison = case_when(month(date) == 1 ~ paste0(year(date)-1,"/",year(date)),
#                    month(date) != 1 ~ paste0(year(date),"/",year(date)+1))

# GPS_chasse$Saison <- as.character(GPS_chasse$Saison)
# chasse_all$Saison <- as.character(chasse_all$Saison)

# GPS_chasse <- GPS_chasse %>%
#   left_join(chasse_all)

GPS_chasse <- GPS_chasse %>%
  mutate(
    Saison = case_when(
      month(datetime) %in% c(1, 2, 3, 4, 5, 6) ~ paste0(year(datetime) - 1, "/", year(datetime)),
      month(datetime) %in% c(7, 8, 9, 10, 11, 12) ~ paste0(year(datetime), "/", year(datetime) + 1)
    )
  )

chasse <- chasse %>%
  mutate(
    Saison = case_when(
      month(date) %in% c(1, 2, 3, 4, 5, 6) ~ paste0(year(date) - 1, "/", year(date)),
      month(date) %in% c(7, 8, 9, 10, 11, 12) ~ paste0(year(date), "/", year(date) + 1)
    )
  )

# date de fermeture/ouverture periode de chasse

date_fin_chasse <- "-01-31"

chasse$ouverture_fermeture <- as.Date(paste0(as.character(year(chasse$date)), date_fin_chasse))
chasse$debut_in_chasse <- chasse$ouverture_fermeture - 15
chasse$fin_out_chasse <- chasse$ouverture_fermeture + 15

GPS_chasse <- GPS_chasse %>%
  left_join(chasse)

# que le jour
# GPS_chasse <- GPS_chasse %>%
#   filter(jour_nuit == "jour")

# grid ---

chasse_buffer <- st_transform(chasse_buffer, crs = 2154)

grid_ZOOM_C <- st_read(paste0(data_generated_path, "grid_ZOOM_C.gpkg"))

grid_chasse <- st_intersection(grid_ZOOM_C, chasse_buffer)

raster_chasse <- rast(grid_chasse, resolution = resolution_ZOOM, crs = "EPSG:2154")

tmap_mode("view")
map_chasse <- tm_scalebar() +
  tm_shape(grid_chasse) +
  tm_polygons(col = "blue") +
  tm_shape(chasse2[1, ]) + # le point DPM
  tm_dots(col = "red") +
  tm_shape(terre_mer) +
  tm_lines(col = "lightblue", lwd = 0.1)
map_chasse

#### in_out_saison -------------------------------------------------------------

# point GPS dans les 15 jours avant/après la fermeture de la période de chasse

tt <- GPS

tt$m_d <- format(tt$y_m_d, "%m-%d")

ouverture_fermeture <- as.Date("2000-01-31")
debut_in_chasse <- ouverture_fermeture - 15
fin_out_chasse <- ouverture_fermeture + 15
format(ouverture_fermeture, "%m-%d")
format(debut_in_chasse, "%m-%d")
format(fin_out_chasse, "%m-%d")

# Tes bornes de comparaison avec année fictive
ouverture_fermeture <- as.Date("2000-01-31")
debut_in_chasse <- ouverture_fermeture - 15
fin_out_chasse <- ouverture_fermeture + 15

# Conversion en "MM-DD"
debut_md <- format(debut_in_chasse, "%m-%d")
fermeture_md <- format(ouverture_fermeture, "%m-%d")
fin_md <- format(fin_out_chasse, "%m-%d")

GPS_in_out_saison_chasse <- tt %>%
  mutate(
    md = format(y_m_d, "%m-%d"), # extraire mois-jour
    in_out_saison = case_when(
      md >= debut_md & md < fermeture_md ~ "in", # saison de chasse
      md >= fermeture_md | md < fin_md ~ "out" # hors saison
    )
  )

table(GPS_in_out_saison_chasse$in_out_saison)
table(GPS_in_out_saison_chasse$month_numeric[GPS_in_out_saison_chasse$in_out_saison == "in"])

length(tt$datetime[tt$month_numeric == 11])

unique(GPS_in_out_saison_chasse$month_label[GPS_in_out_saison_chasse$in_out_saison %in% c("in", "out")])

table(tt$month_numeric)

tt$ouverture_fermeture <- as.Date(paste0(as.character(year(tt$date)), date_fin_chasse))
tt$debut_in_chasse <- tt$ouverture_fermeture - 15
tt$fin_out_chasse <- tt$ouverture_fermeture + 15

GPS_in_out_saison_chasse <- tt %>%
  mutate(in_out_saison = case_when(
    between(y_m_d, debut_in_chasse, ouverture_fermeture) ~ "in",
    between(y_m_d, ouverture_fermeture, fin_out_chasse) ~ "out"
  ))

table(GPS_in_out_saison_chasse$in_out_saison)

#### roosting ---

GPS <- GPS_in_out_saison_chasse
zoom_level <- c("C")
analyse <- "roosting_chasse_in_out_saison"
results_kud <- NULL
nb_kud <- NULL
comportement <- "roosting"
param <- "in_out_saison"
couleur <- nom_pal_roosting

# estimer les kernelUD
map_kud.roosting_chasse_in_out_saison <- Map(estimate_kud_param, zoom_level, comportement, param)
results_kud.roosting_chasse_in_out_saison <- do.call(rbind, map_kud.roosting_chasse_in_out_saison)
st_write(results_kud.roosting_chasse_in_out_saison, paste0(data_generated_path, "results_kud.", analyse, ".gpkg"), append = FALSE)
# compter les nb ind par zoom
nb_kud_map.roosting_chasse_in_out_saison <- Map(count_nb_kud_param, zoom_level, comportement, param)
nb_kud.roosting_chasse_in_out_saison <- do.call(rbind, nb_kud_map.roosting_chasse_in_out_saison)
write.csv(nb_kud.roosting_chasse_in_out_saison, paste0(data_generated_path, "nb_kud.", analyse, ".csv"), row.names = FALSE)
# resultats
results_kud.roosting_chasse_in_out_saison <- st_read(file.path(data_generated_path, paste0("results_kud.", analyse, ".gpkg")))
nb_kud.roosting_chasse_in_out_saison <- read.csv(paste0(data_generated_path, paste0("nb_kud.", analyse, ".csv")), row.names = NULL)
maps_list.roosting_ZOOM_chasse_in_out_saison <- Map(create_map_param, zoom_level, analyse, param, couleur)

#### foraging ---

GPS <- GPS_in_out_saison_chasse
zoom_level <- c("C")
analyse <- "foraging_chasse_in_out_saison"
results_kud <- NULL
nb_kud <- NULL
comportement <- "foraging"
param <- "in_out_saison"
couleur <- nom_pal_foraging

# estimer les kernelUD
map_kud.foraging_chasse_in_out_saison <- Map(estimate_kud_param, zoom_level, comportement, param)
results_kud.foraging_chasse_in_out_saison <- do.call(rbind, map_kud.foraging_chasse_in_out_saison)
st_write(results_kud.foraging_chasse_in_out_saison, paste0(data_generated_path, "results_kud.", analyse, ".gpkg"), append = FALSE)
# compter les nb ind par zoom
nb_kud_map.foraging_chasse_in_out_saison <- Map(count_nb_kud_param, zoom_level, comportement, param)
nb_kud.foraging_chasse_in_out_saison <- do.call(rbind, nb_kud_map.foraging_chasse_in_out_saison)
write.csv(nb_kud.foraging_chasse_in_out_saison, paste0(data_generated_path, "nb_kud.", analyse, ".csv"), row.names = FALSE)
# resultats
results_kud.foraging_chasse_in_out_saison <- st_read(file.path(data_generated_path, paste0("results_kud.", analyse, ".gpkg")))
nb_kud.foraging_chasse_in_out_saison <- read.csv(paste0(data_generated_path, paste0("nb_kud.", analyse, ".csv")), row.names = NULL)
maps_list.foraging_ZOOM_chasse_in_out_saison <- Map(create_map_param, zoom_level, analyse, param, couleur)

unique(GPS$month_numeric[GPS$in_out_saison == "in"])
unique(GPS$month_numeric[GPS$in_out_saison == "out"])

#### jour_de_chasse ------------------------------------------------------------

# point GPS le jour avec la présence d'un chasseur

jour_de_chasse_dt <- chasse2 %>%
  st_drop_geometry() %>%
  na.omit()

dates <- jour_de_chasse_dt %>%
  dplyr::select(date, effectif) %>%
  distinct() # si des doublons existent

dates_etendus$effectif <- as.numeric(as.character(dates_etendus$effectif))
GPS_chasse$effectif <- as.numeric(as.character(GPS_chasse$effectif))

# Joindre les dates étendues à GPS_chasse_2
GPS_jour_de_chasse <- GPS_chasse %>%
  left_join(dates) %>%
  mutate(
    jour_de_chasse = case_when(
      is.na(effectif) ~ "non",
      effectif >= 0 ~ "oui"
    )
  )

table(GPS_jour_de_chasse$jour_de_chasse)

#### roosting ---

# UDmap ---

GPS.roosting_glob_jour_de_chasse <- GPS_jour_de_chasse %>%
  filter(behavior == "roosting") %>%
  dplyr::select(lon, lat, jour_de_chasse) %>%
  st_drop_geometry() %>%
  na.omit()

GPS_spa.roosting_glob_jour_de_chasse <- st_as_sf(GPS.roosting_glob_jour_de_chasse, coords = c("lon", "lat"), crs = 4326)
GPS_spa.roosting_glob_jour_de_chasse <- st_transform(GPS_spa.roosting_glob_jour_de_chasse, crs = 32630)
GPS_coords.roosting_glob_jour_de_chasse <- st_coordinates(GPS_spa.roosting_glob_jour_de_chasse)

# raster/grid
crs_utm <- "EPSG:32630"
SpatRaster <- project(raster_chasse, crs_utm)
RasterLayer <- raster(SpatRaster)
SpatialPixels <- as(RasterLayer, "SpatialPixels")

# Règle de Silverman
sigma_x.roosting_glob_jour_de_chasse <- sd(GPS_coords.roosting_glob_jour_de_chasse[, 1])
sigma_y.roosting_glob_jour_de_chasse <- sd(GPS_coords.roosting_glob_jour_de_chasse[, 2])
n.roosting_glob_jour_de_chasse <- nrow(GPS.roosting_glob_jour_de_chasse)
h.silverman_x_roosting_glob_jour_de_chasse <- 1.06 * sigma_x.roosting_glob_jour_de_chasse * n.roosting_glob_jour_de_chasse^(-1 / 5) / 2
h.silverman_y_roosting_glob_jour_de_chasse <- 1.06 * sigma_y.roosting_glob_jour_de_chasse * n.roosting_glob_jour_de_chasse^(-1 / 5) / 2
locs_spa.roosting_glob_jour_de_chasse <- as(GPS_spa.roosting_glob_jour_de_chasse, "Spatial")

# KernelUD
kud.roosting_glob_jour_de_chasse <- kernelUD(locs_spa.roosting_glob_jour_de_chasse["jour_de_chasse"],
  grid = SpatialPixels,
  h = mean(c(h.silverman_x_roosting_glob_jour_de_chasse, h.silverman_y_roosting_glob_jour_de_chasse))
)

kud.list_roosting_glob_jour_de_chasse <- lapply(names(kud.roosting_glob_jour_de_chasse), function(jour_de_chasse) {
  print(jour_de_chasse)

  # Extraire l'estimation de densité pour un ID spécifique
  kud_simple.roosting_glob_jour_de_chasse <- kud.roosting_glob_jour_de_chasse[[jour_de_chasse]]
  rast.roosting_glob_jour_de_chasse <- rast(kud_simple.roosting_glob_jour_de_chasse)
  courtour.roosting_glob_jour_de_chasse <- as.contour(rast.roosting_glob_jour_de_chasse)
  sf.roosting_glob_jour_de_chasse <- st_as_sf(courtour.roosting_glob_jour_de_chasse)
  cast.roosting_glob_jour_de_chasse <- st_cast(sf.roosting_glob_jour_de_chasse, "POLYGON")
  cast.roosting_glob_jour_de_chasse$jour_de_chasse <- jour_de_chasse

  return(cast.roosting_glob_jour_de_chasse)
})

# Fusionner tous les ID dans un seul objet sf
results_kud.roosting_glob_jour_de_chasse <- do.call(rbind, kud.list_roosting_glob_jour_de_chasse)
results_kud.roosting_glob_jour_de_chasse$jour_de_chasse <- as.factor(results_kud.roosting_glob_jour_de_chasse$jour_de_chasse)

# write & read
st_write(results_kud.roosting_glob_jour_de_chasse, paste0(data_generated_path, "results_kud.roosting_glob_jour_de_chasse.gpkg"), append = FALSE)
results_kud.roosting_glob_jour_de_chasse <- st_read(file.path(data_generated_path, "results_kud.roosting_glob_jour_de_chasse.gpkg"))

# plot
tmap_mode("view")
UDMap_100x100_roosting_jour_de_chasse_glob <- tm_scalebar() +
  tm_basemap(c("OpenStreetMap", "Esri.WorldImagery", "CartoDB.Positron")) +
  tm_shape(results_kud.roosting_glob_jour_de_chasse) +
  tm_polygons(
    border.col = "grey", fill = "level", fill_alpha = 1,
    palette = palette_roosting
  ) +
  tm_facets("jour_de_chasse") +
  tm_shape(terre_mer) +
  tm_lines(col = "#32B7FF", lwd = 0.5)
UDMap_100x100_roosting_jour_de_chasse_glob

#### foraging ---

# UDmap ---

GPS.foraging_glob_jour_de_chasse <- GPS_jour_de_chasse %>%
  filter(behavior == "foraging") %>%
  dplyr::select(lon, lat, jour_de_chasse) %>%
  st_drop_geometry() %>%
  na.omit()

GPS_spa.foraging_glob_jour_de_chasse <- st_as_sf(GPS.foraging_glob_jour_de_chasse, coords = c("lon", "lat"), crs = 4326)
GPS_spa.foraging_glob_jour_de_chasse <- st_transform(GPS_spa.foraging_glob_jour_de_chasse, crs = 32630)
GPS_coords.foraging_glob_jour_de_chasse <- st_coordinates(GPS_spa.foraging_glob_jour_de_chasse)

# raster/grid
crs_utm <- "EPSG:32630"
SpatRaster <- project(raster_chasse, crs_utm)
RasterLayer <- raster(SpatRaster)
SpatialPixels <- as(RasterLayer, "SpatialPixels")

# Règle de Silverman
sigma_x.foraging_glob_jour_de_chasse <- sd(GPS_coords.foraging_glob_jour_de_chasse[, 1])
sigma_y.foraging_glob_jour_de_chasse <- sd(GPS_coords.foraging_glob_jour_de_chasse[, 2])
n.foraging_glob_jour_de_chasse <- nrow(GPS.foraging_glob_jour_de_chasse)
h.silverman_x_foraging_glob_jour_de_chasse <- 1.06 * sigma_x.foraging_glob_jour_de_chasse * n.foraging_glob_jour_de_chasse^(-1 / 5) / 2
h.silverman_y_foraging_glob_jour_de_chasse <- 1.06 * sigma_y.foraging_glob_jour_de_chasse * n.foraging_glob_jour_de_chasse^(-1 / 5) / 2
locs_spa.foraging_glob_jour_de_chasse <- as(GPS_spa.foraging_glob_jour_de_chasse, "Spatial")

# KernelUD
kud.foraging_glob_jour_de_chasse <- kernelUD(locs_spa.foraging_glob_jour_de_chasse["jour_de_chasse"],
  grid = SpatialPixels,
  h = mean(c(h.silverman_x_foraging_glob_jour_de_chasse, h.silverman_y_foraging_glob_jour_de_chasse))
)

kud.list_foraging_glob_jour_de_chasse <- lapply(names(kud.foraging_glob_jour_de_chasse), function(jour_de_chasse) {
  print(jour_de_chasse)

  # Extraire l'estimation de densité pour un ID spécifique
  kud_simple.foraging_glob_jour_de_chasse <- kud.foraging_glob_jour_de_chasse[[jour_de_chasse]]
  rast.foraging_glob_jour_de_chasse <- rast(kud_simple.foraging_glob_jour_de_chasse)
  courtour.foraging_glob_jour_de_chasse <- as.contour(rast.foraging_glob_jour_de_chasse)
  sf.foraging_glob_jour_de_chasse <- st_as_sf(courtour.foraging_glob_jour_de_chasse)
  cast.foraging_glob_jour_de_chasse <- st_cast(sf.foraging_glob_jour_de_chasse, "POLYGON")
  cast.foraging_glob_jour_de_chasse$jour_de_chasse <- jour_de_chasse

  return(cast.foraging_glob_jour_de_chasse)
})

# Fusionner tous les ID dans un seul objet sf
results_kud.foraging_glob_jour_de_chasse <- do.call(rbind, kud.list_foraging_glob_jour_de_chasse)
results_kud.foraging_glob_jour_de_chasse$jour_de_chasse <- as.factor(results_kud.foraging_glob_jour_de_chasse$jour_de_chasse)

# write & read
st_write(results_kud.foraging_glob_jour_de_chasse, paste0(data_generated_path, "results_kud.foraging_glob_jour_de_chasse.gpkg"), append = FALSE)
results_kud.foraging_glob_jour_de_chasse <- st_read(file.path(data_generated_path, "results_kud.foraging_glob_jour_de_chasse.gpkg"))

# plot
tmap_mode("view")
UDMap_100x100_foraging_jour_de_chasse_glob <- tm_scalebar() +
  tm_basemap(c("OpenStreetMap", "Esri.WorldImagery", "CartoDB.Positron")) +
  tm_shape(results_kud.foraging_glob_jour_de_chasse) +
  tm_polygons(
    border.col = "grey", fill = "level", fill_alpha = 1,
    palette = palette_foraging
  ) +
  tm_facets("jour_de_chasse") +
  tm_shape(terre_mer) +
  tm_lines(col = "#32B7FF", lwd = 0.5)
UDMap_100x100_foraging_jour_de_chasse_glob

#### seuil_chasse --------------------------------------------------------------

# point GPS le jour avec la présence d'un chasseur, et catégories de quantité de chasseur

jour_de_chasse_dt <- chasse2 %>%
  st_drop_geometry() %>%
  na.omit()

dates <- jour_de_chasse_dt %>%
  dplyr::select(date, effectif) %>%
  distinct() # si des doublons existent

mean <- mean(jour_de_chasse_dt$effectif)

GPS_seuil_chasse <- GPS_chasse %>%
  left_join(dates) %>%
  mutate(
    seuil_chasse = case_when(
      is.na(effectif) ~ "non",
      effectif == 0 ~ "oui_0",
      effectif < mean ~ "oui_moins_mean",
      effectif >= mean ~ "oui_plus_mean"
    )
  )

table(GPS_seuil_chasse$seuil_chasse)

#### roosting ---

# UDmap ---

GPS.roosting_glob_seuil_chasse <- GPS_seuil_chasse %>%
  filter(behavior == "roosting") %>%
  dplyr::select(lon, lat, seuil_chasse) %>%
  st_drop_geometry() %>%
  na.omit()

GPS_spa.roosting_glob_seuil_chasse <- st_as_sf(GPS.roosting_glob_seuil_chasse, coords = c("lon", "lat"), crs = 4326)
GPS_spa.roosting_glob_seuil_chasse <- st_transform(GPS_spa.roosting_glob_seuil_chasse, crs = 32630)
GPS_coords.roosting_glob_seuil_chasse <- st_coordinates(GPS_spa.roosting_glob_seuil_chasse)

# raster/grid
crs_utm <- "EPSG:32630"
SpatRaster <- project(raster_chasse, crs_utm)
RasterLayer <- raster(SpatRaster)
SpatialPixels <- as(RasterLayer, "SpatialPixels")

# Règle de Silverman
sigma_x.roosting_glob_seuil_chasse <- sd(GPS_coords.roosting_glob_seuil_chasse[, 1])
sigma_y.roosting_glob_seuil_chasse <- sd(GPS_coords.roosting_glob_seuil_chasse[, 2])
n.roosting_glob_seuil_chasse <- nrow(GPS.roosting_glob_seuil_chasse)
h.silverman_x_roosting_glob_seuil_chasse <- 1.06 * sigma_x.roosting_glob_seuil_chasse * n.roosting_glob_seuil_chasse^(-1 / 5) / 2
h.silverman_y_roosting_glob_seuil_chasse <- 1.06 * sigma_y.roosting_glob_seuil_chasse * n.roosting_glob_seuil_chasse^(-1 / 5) / 2
locs_spa.roosting_glob_seuil_chasse <- as(GPS_spa.roosting_glob_seuil_chasse, "Spatial")

# KernelUD
kud.roosting_glob_seuil_chasse <- kernelUD(locs_spa.roosting_glob_seuil_chasse["seuil_chasse"],
  grid = SpatialPixels,
  h = mean(c(h.silverman_x_roosting_glob_seuil_chasse, h.silverman_y_roosting_glob_seuil_chasse))
)

kud.list_roosting_glob_seuil_chasse <- lapply(names(kud.roosting_glob_seuil_chasse), function(seuil_chasse) {
  print(seuil_chasse)

  # Extraire l'estimation de densité pour un ID spécifique
  kud_simple.roosting_glob_seuil_chasse <- kud.roosting_glob_seuil_chasse[[seuil_chasse]]
  rast.roosting_glob_seuil_chasse <- rast(kud_simple.roosting_glob_seuil_chasse)
  courtour.roosting_glob_seuil_chasse <- as.contour(rast.roosting_glob_seuil_chasse)
  sf.roosting_glob_seuil_chasse <- st_as_sf(courtour.roosting_glob_seuil_chasse)
  cast.roosting_glob_seuil_chasse <- st_cast(sf.roosting_glob_seuil_chasse, "POLYGON")
  cast.roosting_glob_seuil_chasse$seuil_chasse <- seuil_chasse

  return(cast.roosting_glob_seuil_chasse)
})

# Fusionner tous les ID dans un seul objet sf
results_kud.roosting_glob_seuil_chasse <- do.call(rbind, kud.list_roosting_glob_seuil_chasse)
results_kud.roosting_glob_seuil_chasse$seuil_chasse <- as.factor(results_kud.roosting_glob_seuil_chasse$seuil_chasse)

# write & read
st_write(results_kud.roosting_glob_seuil_chasse, paste0(data_generated_path, "results_kud.roosting_glob_seuil_chasse.gpkg"), append = FALSE)
results_kud.roosting_glob_seuil_chasse <- st_read(file.path(data_generated_path, "results_kud.roosting_glob_seuil_chasse.gpkg"))

# plot
tmap_mode("view")
UDMap_100x100_roosting_seuil_chasse_glob <- tm_scalebar() +
  tm_basemap(c("OpenStreetMap", "Esri.WorldImagery", "CartoDB.Positron")) +
  tm_shape(results_kud.roosting_glob_seuil_chasse) +
  tm_polygons(
    border.col = "grey", fill = "level", fill_alpha = 1,
    palette = palette_roosting
  ) +
  tm_facets("seuil_chasse") +
  tm_shape(terre_mer) +
  tm_lines(col = "#32B7FF", lwd = 0.5)
UDMap_100x100_roosting_seuil_chasse_glob

#### foraging ---

# UDmap ---

GPS.foraging_glob_seuil_chasse <- GPS_seuil_chasse %>%
  filter(behavior == "foraging") %>%
  dplyr::select(lon, lat, seuil_chasse) %>%
  st_drop_geometry() %>%
  na.omit()

GPS_spa.foraging_glob_seuil_chasse <- st_as_sf(GPS.foraging_glob_seuil_chasse, coords = c("lon", "lat"), crs = 4326)
GPS_spa.foraging_glob_seuil_chasse <- st_transform(GPS_spa.foraging_glob_seuil_chasse, crs = 32630)
GPS_coords.foraging_glob_seuil_chasse <- st_coordinates(GPS_spa.foraging_glob_seuil_chasse)

# raster/grid
crs_utm <- "EPSG:32630"
SpatRaster <- project(raster_chasse, crs_utm)
RasterLayer <- raster(SpatRaster)
SpatialPixels <- as(RasterLayer, "SpatialPixels")

# Règle de Silverman
sigma_x.foraging_glob_seuil_chasse <- sd(GPS_coords.foraging_glob_seuil_chasse[, 1])
sigma_y.foraging_glob_seuil_chasse <- sd(GPS_coords.foraging_glob_seuil_chasse[, 2])
n.foraging_glob_seuil_chasse <- nrow(GPS.foraging_glob_seuil_chasse)
h.silverman_x_foraging_glob_seuil_chasse <- 1.06 * sigma_x.foraging_glob_seuil_chasse * n.foraging_glob_seuil_chasse^(-1 / 5) / 2
h.silverman_y_foraging_glob_seuil_chasse <- 1.06 * sigma_y.foraging_glob_seuil_chasse * n.foraging_glob_seuil_chasse^(-1 / 5) / 2
locs_spa.foraging_glob_seuil_chasse <- as(GPS_spa.foraging_glob_seuil_chasse, "Spatial")

# KernelUD
kud.foraging_glob_seuil_chasse <- kernelUD(locs_spa.foraging_glob_seuil_chasse["seuil_chasse"],
  grid = SpatialPixels,
  h = mean(c(h.silverman_x_foraging_glob_seuil_chasse, h.silverman_y_foraging_glob_seuil_chasse))
)

kud.list_foraging_glob_seuil_chasse <- lapply(names(kud.foraging_glob_seuil_chasse), function(seuil_chasse) {
  print(seuil_chasse)

  # Extraire l'estimation de densité pour un ID spécifique
  kud_simple.foraging_glob_seuil_chasse <- kud.foraging_glob_seuil_chasse[[seuil_chasse]]
  rast.foraging_glob_seuil_chasse <- rast(kud_simple.foraging_glob_seuil_chasse)
  courtour.foraging_glob_seuil_chasse <- as.contour(rast.foraging_glob_seuil_chasse)
  sf.foraging_glob_seuil_chasse <- st_as_sf(courtour.foraging_glob_seuil_chasse)
  cast.foraging_glob_seuil_chasse <- st_cast(sf.foraging_glob_seuil_chasse, "POLYGON")
  cast.foraging_glob_seuil_chasse$seuil_chasse <- seuil_chasse

  return(cast.foraging_glob_seuil_chasse)
})

# Fusionner tous les ID dans un seul objet sf
results_kud.foraging_glob_seuil_chasse <- do.call(rbind, kud.list_foraging_glob_seuil_chasse)
results_kud.foraging_glob_seuil_chasse$seuil_chasse <- as.factor(results_kud.foraging_glob_seuil_chasse$seuil_chasse)

# write & read
st_write(results_kud.foraging_glob_seuil_chasse, paste0(data_generated_path, "results_kud.foraging_glob_seuil_chasse.gpkg"), append = FALSE)
results_kud.foraging_glob_seuil_chasse <- st_read(file.path(data_generated_path, "results_kud.foraging_glob_seuil_chasse.gpkg"))

# plot
tmap_mode("view")
UDMap_100x100_foraging_seuil_chasse_glob <- tm_scalebar() +
  tm_basemap(c("OpenStreetMap", "Esri.WorldImagery", "CartoDB.Positron")) +
  tm_shape(results_kud.foraging_glob_seuil_chasse) +
  tm_polygons(
    border.col = "grey", fill = "level", fill_alpha = 1,
    palette = palette_foraging
  ) +
  tm_facets("seuil_chasse") +
  tm_shape(terre_mer) +
  tm_lines(col = "#32B7FF", lwd = 0.5)
UDMap_100x100_foraging_seuil_chasse_glob

## tonnes de chasse -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#----

### variables ------------------------------------------------------------------

# heure jour & nuit ---

tides <- read_csv("D:/Projets_Suzanne/Courlis/3) Data/1) data/Maree/tides_donnees_complete.csv")

jour_nuit_heure_dt <- tides %>%
  dplyr::select(y_m_d, sunset_UTC, sunrise_UTC) %>%
  mutate(
    debut_heure_chasse = sunset_UTC - 2 * 60 * 60,
    fin_heure_chasse = sunrise_UTC + 2 * 60 * 60
  ) %>%
  distinct()

# date ---

chasse_date <- read_excel("D:/Projets_Suzanne/Courlis/3) Data/1) data/Chasse/date ouverture fermeture chasse.xlsx")

tonnes_date <- chasse_date %>%
  dplyr::select(Saison, `Ouverture Gibier d'eau`, `Fermeture Gibier d'eau`)

# GPS ---

GPS_tonnes <- GPS %>%
  left_join(jour_nuit_heure_dt) %>%
  dplyr::select(
    "ID", "datetime", "sex", "age", "tides_high_type", "jour_nuit", "year",
    "y_m_d", "month_numeric", "month_label", "debut_heure_chasse", "fin_heure_chasse"
  )

# tonnes ---

tonnes <- st_read(paste0(data_path, "Tonnes_de_chasse/tonnes.shp"))

tonnes <- st_intersection(tonnes, BOX_2154)

tonnes_buffer <- tonnes %>%
  st_buffer(dist = 300)
tonnes_unioned <- st_union(tonnes_buffer)
tonnes_cut_zones <- st_intersection(tonnes_unioned, tonnes_buffer)
tonnes_zones_final <- tonnes_cut_zones %>%
  st_sf() %>%
  mutate(overlap_count = lengths(st_intersects(geometry, tonnes_buffer))) %>%
  filter(overlap_count >= 1)
tonnes_zones_grouped <- tonnes_zones_final %>%
  group_by(overlap_count) %>%
  summarise(geometry = st_union(geometry), .groups = "drop")

# histogram
hist(tonnes_zones_grouped$overlap_count)

tonnes_zones_grouped_clean <- tonnes_zones_grouped[!is.na(tonnes_zones_grouped$overlap_count), ]

# maps
tmap_mode("view")
map_tonnes <- tm_scalebar() +
  tm_shape(terre_mer) +
  tm_lines(col = "#32B7FF", lwd = 0.5) +
  tm_shape(tonnes_zones_grouped_clean) +
  tm_basemap(c("OpenStreetMap", "Esri.WorldImagery", "CartoDB.Positron")) +
  tm_polygons(
    fill = "overlap_count",
    palette = c("#FFF07C", "orange", "#D64045", "darkred"),
    style = "cont", alpha = 0.5,
    title = "Nb superposées"
  ) +
  tm_shape(tonnes) +
  tm_dots(fill = "black") +
  tm_layout(title = "Superposition des tonnes de chasse (300 m de rayon)") +
  tm_shape(site_baguage) +
  tm_text("icone", size = 1.5)
map_tonnes

tmap_save(map_tonnes, paste0(atlas_path, "map_tonnes.html"))

#### saison de chasse (sept-fev) -----------------------------------------------

open <- format(as.POSIXct(tonnes_date$`Ouverture Gibier d'eau`[1], format = "%Y-%m-%d UTC", tz = "UTC"), "%m-%d")
close <- format(as.POSIXct(tonnes_date$`Fermeture Gibier d'eau`[1], format = "%Y-%m-%d UTC", tz = "UTC"), "%m-%d")

GPS_saison_chasse_restrited <- GPS_tonnes %>%
  mutate(
    open_tonnes = ymd(paste0(year, "-", open)),
    close_tonnes = ymd(paste0(year + 1, "-", close)),
    saison_chasse_restrited = ifelse(between(y_m_d, open_tonnes, close_tonnes), "saison chasse : ouverte", "saison chasse : fermée")
  ) %>%
  dplyr::select(
    "ID", "datetime", "sex", "age", "tides_high_type", "jour_nuit", "year",
    "y_m_d", "month_numeric", "month_label", "saison_chasse_restrited"
  ) %>%
  filter(month_numeric %in% c(9, 10, 11, 12, 1, 2)) %>%
  distinct()

table(GPS_saison_chasse_restrited$saison_chasse)

#### saison + heure de chasse ------------------------------------------------------------

GPS_heure_chasse <- GPS_tonnes %>%
  mutate(
    open_tonnes = ymd(paste0(year, "-", open)),
    close_tonnes = ymd(paste0(year + 1, "-", close)),
    saison_chasse = ifelse(between(y_m_d, open_tonnes, close_tonnes), "saison chasse : ouverte", "saison chasse : fermée")
  ) %>%
  filter(saison_chasse == "saison chasse : ouverte") %>%
  mutate(
    open_tonnes = ymd(paste0(year, "-", open)),
    close_tonnes = ymd(paste0(year + 1, "-", close)),
    heure_chasse = ifelse(between(datetime, fin_heure_chasse, debut_heure_chasse), "heure chasse : fermée", "heure chasse : ouverte")
  ) %>%
  dplyr::select(
    "ID", "datetime", "sex", "age", "tides_high_type", "jour_nuit",
    "y_m_d", "month_numeric", "month_label", "saison_chasse", "heure_chasse"
  ) %>%
  distinct()

table(GPS_heure_chasse$heure_chasse)

#### periode d'étude de chasse (15j av/ap) -------------------------------------

# periode d'étude 15 jour avant/après fermeture de la chasse
open_etude <- format(as.POSIXct(tonnes_date$`Fermeture Gibier d'eau`[1] - 15 * 60 * 60 * 24, format = "%Y-%m-%d UTC", tz = "UTC"), "%m-%d")
close_etude <- format(as.POSIXct(tonnes_date$`Fermeture Gibier d'eau`[1] + 15 * 60 * 60 * 24, format = "%Y-%m-%d UTC", tz = "UTC"), "%m-%d")

GPS_periode_etude_chasse <- GPS_tonnes %>%
  mutate(
    open_etude_tonnes = ymd(paste0(year, "-", open_etude)),
    close_etude_tonnes = ymd(paste0(year, "-", close_etude)),
    close_tonnes = ymd(paste0(year, "-", close))
  ) %>%
  filter(between(y_m_d, open_etude_tonnes, close_etude_tonnes)) %>%
  mutate(periode_etude_chasse = ifelse(between(datetime, open_etude_tonnes, close_tonnes), "periode étude chasse : ouverte", "periode étude chasse : fermée")) %>%
  dplyr::select(
    "ID", "datetime", "sex", "age", "tides_high_type", "jour_nuit",
    "y_m_d", "month_numeric", "month_label", "periode_etude_chasse"
  ) %>%
  distinct()

table(GPS_periode_etude_chasse$periode_etude_chasse)
table(GPS_periode_etude_chasse$month_label)

### carte zones proxi vs danger ---------------------------------------------

# créer zone de danger de 300m
# créer zone de proximité de 1 km
# selectionner les point GPS dans les zones de danger et proximité
# garder les ind que avec assez de point dans les deux zones
# proportion danger/proximité ~ week par ind
# voir si plus bas pendant la période de chasse

tonnes_danger <- tonnes %>%
  st_buffer(dist = 300)
tonnes_proxi <- tonnes %>%
  st_buffer(dist = 1500)

tonnes_danger_unioned <- st_union(tonnes_danger)
tonnes_proxi_unioned <- st_union(tonnes_proxi)

area_danger <- as.numeric(st_area(tonnes_danger_unioned)) / 1000000
area_proxi <- as.numeric(st_area(tonnes_proxi_unioned)) / 1000000

# maps
tmap_mode("view")
map_tonnes_v2 <- tm_scalebar() +
  tm_basemap(c("OpenStreetMap", "Esri.WorldImagery", "CartoDB.Positron")) +
  tm_shape(terre_mer) +
  tm_lines(col = "#32B7FF", lwd = 0.5) +
  tm_shape(tonnes_proxi_unioned) +
  tm_polygons(fill = "#FFF07C", alpha = 0.5) +
  tm_shape(tonnes_danger_unioned) +
  tm_polygons(fill = "#541388", alpha = 0.7) +
  tm_shape(tonnes) +
  tm_dots(fill = "black") +
  tm_layout(title = "Tonne de chasse, zone de danger (300 m), zone de proximité (1500 m)") +
  tm_shape(site_baguage) +
  tm_text("icone", size = 1.5)
map_tonnes_v2

tmap_save(map_tonnes_v2, paste0(atlas_path, "map_tonnes_v2.html"))

tonnes_proxi_unioned <- st_as_sf(tonnes_proxi_unioned)
tonnes_proxi_unioned <- st_transform(tonnes_proxi_unioned, st_crs(GPS_tonnes))
tonnes_proxi_unioned <- st_make_valid(tonnes_proxi_unioned)

tonnes_danger_unioned <- st_as_sf(tonnes_danger_unioned)
tonnes_danger_unioned <- st_transform(tonnes_danger_unioned, st_crs(GPS_tonnes))
tonnes_danger_unioned <- st_make_valid(tonnes_danger_unioned)

### analyses -------------------------------------------------------------------

#### saison de chasse (sept-fev) -----------------------------------------------

points_dans_proxi_saison_chasse_restrited <- GPS_saison_chasse_restrited %>%
  dplyr::select(ID, saison_chasse_restrited, y_m_d, year) %>%
  st_filter(tonnes_proxi_unioned)

table(points_dans_proxi_saison_chasse_restrited$ID)

# au moins x point par ID
n_per_ID_dans_proxi_saison_chasse_restrited <- points_dans_proxi_saison_chasse_restrited %>%
  group_by(ID) %>%
  summarize(n = n()) %>%
  filter(n < 1)

points_dans_proxi_saison_chasse_restrited <- points_dans_proxi_saison_chasse_restrited %>%
  filter(ID %ni% n_per_ID_dans_proxi_saison_chasse_restrited$ID)

table(points_dans_proxi_saison_chasse_restrited$ID)

points_dans_proxi_saison_chasse_restrited <- points_dans_proxi_saison_chasse_restrited %>%
  mutate(
    zone = ifelse(
      st_within(points_dans_proxi_saison_chasse_restrited, tonnes_danger_unioned, sparse = FALSE)[, 1],
      "zone de danger",
      "zone marginale"
    )
  )

nb_point_id_tonnes_saison_chasse_restrited_v2 <- points_dans_proxi_saison_chasse_restrited %>%
  st_drop_geometry() %>%
  group_by(ID, saison_chasse_restrited, zone) %>%
  summarize(nb_point = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = zone,
    values_from = nb_point,
    values_fill = 0 # remplit les NA par 0 si un ID/week n’a pas de points dans une zone
  )

prop_id_tonnes_saison_chasse_restrited_v2 <- nb_point_id_tonnes_saison_chasse_restrited_v2 %>%
  st_drop_geometry() %>%
  group_by(ID, saison_chasse_restrited) %>%
  mutate(prop_danger_proxi = `zone de danger` / `zone marginale` * 100) %>%
  dplyr::select(ID, saison_chasse_restrited, prop_danger_proxi) %>%
  distinct()

dt_saison_chasse_restrited <- points_dans_proxi_saison_chasse_restrited %>%
  st_drop_geometry() %>%
  group_by(ID, saison_chasse_restrited, zone) %>%
  summarize(nb_point = n(), .groups = "drop")

dt_saison_chasse_restrited <- dt_saison_chasse_restrited %>%
  mutate(nb_area = case_when(
    zone == "zone de danger" ~ (nb_point / area_danger),
    TRUE ~ (nb_point / area_proxi)
  ))

dt_saison_chasse_restrited$saison_chasse_restrited <- as.factor(dt_saison_chasse_restrited$saison_chasse_restrited)
dt_saison_chasse_restrited$saison_chasse_restrited <- factor(dt_saison_chasse_restrited$saison_chasse_restrited, levels = c("saison chasse : fermée", "saison chasse : ouverte"))
dt_saison_chasse_restrited$zone <- as.factor(dt_saison_chasse_restrited$zone)
dt_saison_chasse_restrited$zone <- factor(dt_saison_chasse_restrited$zone, levels = c("zone marginale", "zone de danger"))

# Réestimer le modèle
lmer_model_saison_chasse_restrited <- lmer(nb_area ~ zone * saison_chasse_restrited + (1 | ID), data = dt_saison_chasse_restrited)

# Résumé avec p-values
summary(lmer_model_saison_chasse_restrited)

# Résultats au format tidy
fixed_saison_chasse_restrited <- tidy(lmer_model_saison_chasse_restrited, effects = "fixed", conf.int = TRUE)

# Ajouter les étoiles
fixed_saison_chasse_restrited$signif <- cut(fixed_saison_chasse_restrited$p.value,
  breaks = c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf),
  labels = c("***", "**", "*", ".", "")
)

# R²
r2_saison_chasse_restrited <- as.data.frame(r2(lmer_model_saison_chasse_restrited))

# Variance des effets aléatoires
random_saison_chasse_restrited <- as.data.frame(VarCorr(lmer_model_saison_chasse_restrited))
random_saison_chasse_restrited <- random_saison_chasse_restrited[, c("grp", "vcov", "sdcor")]
colnames(random_saison_chasse_restrited) <- c("Effet", "Variance", "Écart-type")

# Sauvegarder tout
saveRDS(
  list(fixed = fixed_saison_chasse_restrited, r2 = r2_saison_chasse_restrited, random = random_saison_chasse_restrited),
  paste0(atlas_path, "resultats_modeles_saison_chasse_restrited.rds")
)

# Effets moyens pour interaction zone * tonnes_period
preds_saison_chasse_restrited <- ggpredict(lmer_model_saison_chasse_restrited, terms = c("zone", "saison_chasse_restrited"))

# plot
preds_saison_chasse_restrited_plot <- ggplot(preds_saison_chasse_restrited, aes(x = group, y = predicted, color = x, group = x)) +
  geom_point(size = 4) +
  geom_line(size = 2) +
  geom_riBon(aes(ymin = conf.low, ymax = conf.high, fill = x),
    alpha = 0.2, color = NA
  ) +
  scale_color_manual(values = c("zone de danger" = "#541388", "zone marginale" = "#FFF07C")) +
  scale_fill_manual(values = c("zone de danger" = "#541388", "zone marginale" = "#FFF07C")) +
  labs(
    x = "Saison de chasse", y = "Nombre de point GPS / surface de la zone",
    color = "Zone", fill = "Zone"
  ) +
  theme_hc() +
  theme(legend.position = c(0.8, 0.9))
preds_saison_chasse_restrited_plot

ggsave(paste0(atlas_path, "/pred_saison_chasse_restrited_plot.png"),
  plot = preds_saison_chasse_restrited_plot, width = 5, height = 5, dpi = 1000
)

##
##
##
# diag et meilleur modèle ---
##
##
##

hist(dt_saison_chasse_restrited$nb_area)

lmer_model_saison_chasse_restrited <- lmer(nb_area ~ zone * saison_chasse_restrited + (1 | ID), data = dt_saison_chasse_restrited)

summary(lmer_model_saison_chasse_restrited)

library(glmmTMB)

# Modèle gaussien
mod_gauss_saison_chasse_restrited <- lm(nb_area ~ 1,
  data = dt_saison_chasse_restrited
)

mod_gamma_saison_chasse_restrited <- glmmTMB(nb_area ~ 1,
  data = dt_saison_chasse_restrited,
  family = Gamma(link = "log")
)

AIC(mod_gauss_saison_chasse_restrited, mod_gamma_saison_chasse_restrited)
BIC(mod_gauss_saison_chasse_restrited, mod_gamma_saison_chasse_restrited)
compare_performance(mod_gauss_saison_chasse_restrited, mod_gamma_saison_chasse_restrited)

mod_gamma_saison_chasse_restrited <- glmmTMB(nb_area ~ zone * saison_chasse_restrited + (1 | ID),
  data = dt_saison_chasse_restrited,
  family = Gamma(link = "log")
)

summary(mod_gamma_saison_chasse_restrited)

preds_saison_chasse_restrited <- ggpredict(mod_gamma_saison_chasse_restrited, terms = c("zone", "saison_chasse_restrited"), bias_correction = TRUE)

# plot
preds_saison_chasse_restrited_plot <- ggplot(preds_saison_chasse_restrited, aes(x = group, y = predicted, color = x, group = x)) +
  geom_point(size = 4) +
  geom_line(size = 2) +
  geom_riBon(aes(ymin = conf.low, ymax = conf.high, fill = x),
    alpha = 0.2, color = NA
  ) +
  scale_color_manual(values = c("zone de danger" = "#541388", "zone marginale" = "#FFF07C")) +
  scale_fill_manual(values = c("zone de danger" = "#541388", "zone marginale" = "#FFF07C")) +
  labs(
    x = "Saison de chasse", y = "Nombre de point GPS / surface de la zone",
    color = "Zone", fill = "Zone"
  ) +
  theme_hc() +
  theme(legend.position = c(0.8, 0.9))
preds_saison_chasse_restrited_plot

# diag

library(DHARMa)
install.packages("effects") # Une seule fois si pas déjà installé
library(effects)

simulationOutput_saison_chasse_restrited <- simulateResiduals(fittedModel = mod_gamma_saison_chasse_restrited, plot = F)
# residuals(simulationOutput_saison_chasse_restrited)
# residuals(simulationOutput_saison_chasse_restrited, quantileFunction = qnorm, outlierValues = c(-7,7))
residuals_saison_chasse_restrited <- plot(simulationOutput_saison_chasse_restrited)
allEffects_saison_chasse_restrited <- plot(allEffects(mod_gamma_saison_chasse_restrited), type = "response")

# 1.Test de dispersion
testDispersion(simulationOutput_saison_chasse_restrited)

# 3.Outliers
testOutliers(simulationOutput_saison_chasse_restrited)


#### saison + heure de chasse ------------------------------------------------------------

points_dans_proxi_heure_chasse <- GPS_heure_chasse %>%
  dplyr::select(ID, heure_chasse, y_m_d) %>%
  st_filter(tonnes_proxi_unioned)

table(points_dans_proxi_heure_chasse$ID)

# au moins x point par ID
n_per_ID_dans_proxi_heure_chasse <- points_dans_proxi_heure_chasse %>%
  group_by(ID) %>%
  summarize(n = n()) %>%
  filter(n < 1)

points_dans_proxi_heure_chasse <- points_dans_proxi_heure_chasse %>%
  filter(ID %ni% n_per_ID_dans_proxi_heure_chasse$ID)

table(points_dans_proxi_heure_chasse$ID)

points_dans_proxi_heure_chasse <- points_dans_proxi_heure_chasse %>%
  mutate(
    zone = ifelse(
      st_within(points_dans_proxi_heure_chasse, tonnes_danger_unioned, sparse = FALSE)[, 1],
      "zone de danger",
      "zone marginale"
    )
  )

nb_point_id_tonnes_heure_chasse_v2 <- points_dans_proxi_heure_chasse %>%
  st_drop_geometry() %>%
  group_by(ID, heure_chasse, zone) %>%
  summarize(nb_point = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = zone,
    values_from = nb_point,
    values_fill = 0 # remplit les NA par 0 si un ID/week n’a pas de points dans une zone
  )

prop_id_tonnes_heure_chasse_v2 <- nb_point_id_tonnes_heure_chasse_v2 %>%
  st_drop_geometry() %>%
  group_by(ID, heure_chasse) %>%
  mutate(prop_danger_proxi = `zone de danger` / `zone marginale` * 100) %>%
  dplyr::select(ID, heure_chasse, prop_danger_proxi) %>%
  distinct()

dt_heure_chasse <- points_dans_proxi_heure_chasse %>%
  st_drop_geometry() %>%
  group_by(ID, heure_chasse, zone) %>%
  summarize(nb_point = n(), .groups = "drop")

dt_heure_chasse <- dt_heure_chasse %>%
  mutate(nb_area = case_when(
    zone == "zone de danger" ~ (nb_point / area_danger),
    TRUE ~ (nb_point / area_proxi)
  ))

dt_heure_chasse$heure_chasse <- as.factor(dt_heure_chasse$heure_chasse)
dt_heure_chasse$heure_chasse <- factor(dt_heure_chasse$heure_chasse, levels = c("heure chasse : fermée", "heure chasse : ouverte"))
dt_heure_chasse$zone <- as.factor(dt_heure_chasse$zone)
dt_heure_chasse$zone <- factor(dt_heure_chasse$zone, levels = c("zone marginale", "zone de danger"))

# Réestimer le modèle
lmer_model_heure_chasse <- lmer(nb_area ~ zone * heure_chasse + (1 | ID), data = dt_heure_chasse)

# Résumé avec p-values
summary(lmer_model_heure_chasse)

# Résultats au format tidy
fixed_heure_chasse <- tidy(lmer_model_heure_chasse, effects = "fixed", conf.int = TRUE)

# Ajouter les étoiles
fixed_heure_chasse$signif <- cut(fixed_heure_chasse$p.value,
  breaks = c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf),
  labels = c("***", "**", "*", ".", "")
)

# R²
r2_heure_chasse <- as.data.frame(r2(lmer_model_heure_chasse))

# Variance des effets aléatoires
random_heure_chasse <- as.data.frame(VarCorr(lmer_model_heure_chasse))
random_heure_chasse <- random_heure_chasse[, c("grp", "vcov", "sdcor")]
colnames(random_heure_chasse) <- c("Effet", "Variance", "Écart-type")

# Sauvegarder tout
saveRDS(
  list(fixed = fixed_heure_chasse, r2 = r2_heure_chasse, random = random_heure_chasse),
  paste0(atlas_path, "resultats_modeles_heure_chasse.rds")
)

# Effets moyens pour interaction zone * tonnes_period
preds_heure_chasse <- ggpredict(lmer_model_heure_chasse, terms = c("zone", "heure_chasse"))

# plot
preds_heure_chasse_plot <- ggplot(preds_heure_chasse, aes(x = group, y = predicted, color = x, group = x)) +
  geom_point(size = 4) +
  geom_line(size = 2) +
  geom_riBon(aes(ymin = conf.low, ymax = conf.high, fill = x),
    alpha = 0.2, color = NA
  ) +
  scale_color_manual(values = c("zone de danger" = "#541388", "zone marginale" = "#FFF07C")) +
  scale_fill_manual(values = c("zone de danger" = "#541388", "zone marginale" = "#FFF07C")) +
  labs(
    x = "heure de chasse", y = "Nombre de point GPS / surface de la zone",
    color = "Zone", fill = "Zone"
  ) +
  theme_hc() +
  theme(legend.position = c(0.8, 0.9))
preds_heure_chasse_plot

ggsave(paste0(atlas_path, "/pred_heure_chasse_plot.png"),
  plot = preds_heure_chasse_plot, width = 5, height = 5, dpi = 1000
)




##
##
##
# diag et meilleur modèle ---
##
##
##

hist(dt_heure_chasse$nb_area)

# Réestimer le modèle
lmer_model_heure_chasse <- lmer(nb_area ~ zone * heure_chasse + (1 | ID), data = dt_heure_chasse)

# Résumé avec p-values
summary(lmer_model_heure_chasse)

library(glmmTMB)

# Modèle gaussien
mod_gauss_heure_chasse <- lm(nb_area ~ 1,
  data = dt_heure_chasse
)

mod_gamma_heure_chasse <- glmmTMB(nb_area ~ 1,
  data = dt_heure_chasse,
  family = Gamma(link = "log")
)

AIC(mod_gauss_heure_chasse, mod_gamma_heure_chasse)
BIC(mod_gauss_heure_chasse, mod_gamma_heure_chasse)
compare_performance(mod_gauss_heure_chasse, mod_gamma_heure_chasse)

mod_gamma_heure_chasse <- glmmTMB(nb_area ~ zone * heure_chasse + (1 | ID),
  data = dt_heure_chasse,
  family = Gamma(link = "log")
)

summary(mod_gamma_heure_chasse)

# Effets moyens pour interaction zone * tonnes_period
preds_heure_chasse <- ggpredict(mod_gamma_heure_chasse, terms = c("zone", "heure_chasse"), bias_correction = TRUE)

# plot
preds_heure_chasse_plot <- ggplot(preds_heure_chasse, aes(x = group, y = predicted, color = x, group = x)) +
  geom_point(size = 4) +
  geom_line(size = 2) +
  geom_riBon(aes(ymin = conf.low, ymax = conf.high, fill = x),
    alpha = 0.2, color = NA
  ) +
  scale_color_manual(values = c("zone de danger" = "#541388", "zone marginale" = "#FFF07C")) +
  scale_fill_manual(values = c("zone de danger" = "#541388", "zone marginale" = "#FFF07C")) +
  labs(
    x = "heure de chasse", y = "Nombre de point GPS / surface de la zone",
    color = "Zone", fill = "Zone"
  ) +
  theme_hc() +
  theme(legend.position = c(0.8, 0.9))
preds_heure_chasse_plot

# diag

library(DHARMa)
library(effects)

simulationOutput_heure_chasse <- simulateResiduals(fittedModel = mod_gamma_heure_chasse, plot = F)
# residuals(simulationOutput_heure_chasse)
# residuals(simulationOutput_heure_chasse, quantileFunction = qnorm, outlierValues = c(-7,7))
residuals_heure_chasse <- plot(simulationOutput_heure_chasse)
allEffects_heure_chasse <- plot(allEffects(mod_gamma_heure_chasse), type = "response")

# 1.Test de dispersion
testDispersion(simulationOutput_heure_chasse)

# 3.Outliers
testOutliers(simulationOutput_heure_chasse)



#### periode étude chasse (15j av/ap) ------------------------------------------

points_dans_proxi_periode_etude_chasse <- GPS_periode_etude_chasse %>%
  dplyr::select(ID, periode_etude_chasse, y_m_d) %>%
  st_filter(tonnes_proxi_unioned)

table(points_dans_proxi_periode_etude_chasse$ID)

# au moins x point par ID
n_per_ID_dans_proxi_periode_etude_chasse <- points_dans_proxi_periode_etude_chasse %>%
  group_by(ID) %>%
  summarize(n = n()) %>%
  filter(n < 1)

points_dans_proxi_periode_etude_chasse <- points_dans_proxi_periode_etude_chasse %>%
  filter(ID %ni% n_per_ID_dans_proxi_periode_etude_chasse$ID)

table(points_dans_proxi_periode_etude_chasse$ID)

points_dans_proxi_periode_etude_chasse <- points_dans_proxi_periode_etude_chasse %>%
  mutate(
    zone = ifelse(
      st_within(points_dans_proxi_periode_etude_chasse, tonnes_danger_unioned, sparse = FALSE)[, 1],
      "zone de danger",
      "zone marginale"
    )
  )

nb_point_id_tonnes_periode_etude_chasse_v2 <- points_dans_proxi_periode_etude_chasse %>%
  st_drop_geometry() %>%
  group_by(ID, periode_etude_chasse, zone) %>%
  summarize(nb_point = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = zone,
    values_from = nb_point,
    values_fill = 0 # remplit les NA par 0 si un ID/week n’a pas de points dans une zone
  )

prop_id_tonnes_periode_etude_chasse_v2 <- nb_point_id_tonnes_periode_etude_chasse_v2 %>%
  st_drop_geometry() %>%
  group_by(ID, periode_etude_chasse) %>%
  mutate(prop_danger_proxi = `zone de danger` / `zone marginale` * 100) %>%
  dplyr::select(ID, periode_etude_chasse, prop_danger_proxi) %>%
  distinct()

dt_periode_etude_chasse <- points_dans_proxi_periode_etude_chasse %>%
  st_drop_geometry() %>%
  group_by(ID, periode_etude_chasse, zone) %>%
  summarize(nb_point = n(), .groups = "drop")

dt_periode_etude_chasse <- dt_periode_etude_chasse %>%
  mutate(nb_area = case_when(
    zone == "zone de danger" ~ (nb_point / area_danger),
    TRUE ~ (nb_point / area_proxi)
  ))

dt_periode_etude_chasse$periode_etude_chasse <- as.factor(dt_periode_etude_chasse$periode_etude_chasse)
dt_periode_etude_chasse$periode_etude_chasse <- factor(dt_periode_etude_chasse$periode_etude_chasse, levels = c("periode étude chasse : fermée", "periode étude chasse : ouverte"))
dt_periode_etude_chasse$zone <- as.factor(dt_periode_etude_chasse$zone)
dt_periode_etude_chasse$zone <- factor(dt_periode_etude_chasse$zone, levels = c("zone marginale", "zone de danger"))

# Réestimer le modèle
lmer_model_periode_etude_chasse <- lmer(nb_area ~ zone * periode_etude_chasse + (1 | ID), data = dt_periode_etude_chasse)

# Résumé avec p-values
summary(lmer_model_periode_etude_chasse)

# Résultats au format tidy
fixed_periode_etude_chasse <- tidy(lmer_model_periode_etude_chasse, effects = "fixed", conf.int = TRUE)

# Ajouter les étoiles
fixed_periode_etude_chasse$signif <- cut(fixed_periode_etude_chasse$p.value,
  breaks = c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf),
  labels = c("***", "**", "*", ".", "")
)

# R²
r2_periode_etude_chasse <- as.data.frame(r2(lmer_model_periode_etude_chasse))

# Variance des effets aléatoires
random_periode_etude_chasse <- as.data.frame(VarCorr(lmer_model_periode_etude_chasse))
random_periode_etude_chasse <- random_periode_etude_chasse[, c("grp", "vcov", "sdcor")]
colnames(random_periode_etude_chasse) <- c("Effet", "Variance", "Écart-type")

# Sauvegarder tout
saveRDS(
  list(fixed = fixed_periode_etude_chasse, r2 = r2_periode_etude_chasse, random = random_periode_etude_chasse),
  paste0(atlas_path, "resultats_modeles_periode_etude_chasse.rds")
)

# Effets moyens pour interaction zone * tonnes_period
preds_periode_etude_chasse <- ggpredict(lmer_model_periode_etude_chasse, terms = c("zone", "periode_etude_chasse"))

# plot
preds_periode_etude_chasse_plot <- ggplot(preds_periode_etude_chasse, aes(x = group, y = predicted, color = x, group = x)) +
  geom_point(size = 4) +
  geom_line(size = 2) +
  geom_riBon(aes(ymin = conf.low, ymax = conf.high, fill = x),
    alpha = 0.2, color = NA
  ) +
  scale_color_manual(values = c("zone de danger" = "#541388", "zone marginale" = "#FFF07C")) +
  scale_fill_manual(values = c("zone de danger" = "#541388", "zone marginale" = "#FFF07C")) +
  labs(
    x = "heure de chasse", y = "Nombre de point GPS / surface de la zone",
    color = "Zone", fill = "Zone"
  ) +
  theme_hc() +
  theme(legend.position = c(0.8, 0.9))
preds_periode_etude_chasse_plot

ggsave(paste0(atlas_path, "/pred_periode_etude_chasse_plot.png"),
  plot = preds_periode_etude_chasse_plot, width = 5, height = 5, dpi = 1000
)



##
##
##
# diag et meilleur modèle ---
##
##
##

hist(dt_periode_etude_chasse$nb_area)

# Réestimer le modèle
lmer_model_periode_etude_chasse <- lmer(nb_area ~ zone * periode_etude_chasse + (1 | ID), data = dt_periode_etude_chasse)

# Résumé avec p-values
summary(lmer_model_periode_etude_chasse)

library(glmmTMB)

# Modèle gaussien
mod_gauss_periode_etude_chasse <- lm(nb_area ~ 1,
  data = dt_periode_etude_chasse
)

mod_gamma_periode_etude_chasse <- glmmTMB(nb_area ~ 1,
  data = dt_periode_etude_chasse,
  family = Gamma(link = "log")
)

mod_tweedie_periode_etude_chasse <- glmmTMB(nb_area ~ 1,
  data = dt_periode_etude_chasse,
  family = tweedie(link = "log")
)

AIC(mod_gauss_periode_etude_chasse, mod_gamma_periode_etude_chasse, mod_tweedie_periode_etude_chasse)
BIC(mod_gauss_periode_etude_chasse, mod_gamma_periode_etude_chasse, mod_tweedie_periode_etude_chasse)
compare_performance(mod_gauss_periode_etude_chasse, mod_gamma_periode_etude_chasse, mod_tweedie_periode_etude_chasse)

mod_gamma_periode_etude_chasse <- glmmTMB(nb_area ~ zone * periode_etude_chasse + (1 | ID),
  data = dt_periode_etude_chasse,
  family = Gamma(link = "log")
)

summary(mod_gamma_periode_etude_chasse)

# Effets moyens pour interaction zone * tonnes_period
preds_periode_etude_chasse <- ggpredict(mod_gamma_periode_etude_chasse, terms = c("zone", "periode_etude_chasse"), bias_correction = TRUE)

# plot
preds_periode_etude_chasse_plot <- ggplot(preds_periode_etude_chasse, aes(x = group, y = predicted, color = x, group = x)) +
  geom_point(size = 4) +
  geom_line(size = 2) +
  geom_riBon(aes(ymin = conf.low, ymax = conf.high, fill = x),
    alpha = 0.2, color = NA
  ) +
  scale_color_manual(values = c("zone de danger" = "#541388", "zone marginale" = "#FFF07C")) +
  scale_fill_manual(values = c("zone de danger" = "#541388", "zone marginale" = "#FFF07C")) +
  labs(
    x = "heure de chasse", y = "Nombre de point GPS / surface de la zone",
    color = "Zone", fill = "Zone"
  ) +
  theme_hc() +
  theme(legend.position = c(0.8, 0.9))
preds_periode_etude_chasse_plot

# diag

library(DHARMa)
library(effects)

simulationOutput_periode_etude_chasse <- simulateResiduals(fittedModel = mod_gamma_periode_etude_chasse, plot = F)
# residuals(simulationOutput_periode_etude_chasse)
# residuals(simulationOutput_periode_etude_chasse, quantileFunction = qnorm, outlierValues = c(-7,7))
residuals_periode_etude_chasse <- plot(simulationOutput_periode_etude_chasse)
allEffects_periode_etude_chasse <- plot(allEffects(mod_gamma_periode_etude_chasse), type = "response")

# 1.Test de dispersion
testDispersion(simulationOutput_periode_etude_chasse)

# 3.Outliers
testOutliers(simulationOutput_periode_etude_chasse)

############################################################################ ---
# 14. Distance reposoir - alimentation ------------------------------------------
############################################################################ ---

# --- objectif ---
# estimation de la distance entre reposoir et alimentation
# estimation pour chauqe individu, de jour en jour, à chaque cycle de marée
# en moyenne, et en fonction de paramètres (sexe, age, chasse, ...)
# = estimation des distances inter-centroïdes entre comportements consécutifs (par exemple, de "foraging" à "roosting")

## estimation distance de jour en jour #-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#----

## Estimation de la distance entre centroïdes de comportements différents (hors "other")

# Filtrage des données pertinentes (hors comportement "other")
distance_dt_1 <- GPS %>%
  dplyr::select(ID, behavior, datetime) %>% # On garde uniquement les colonnes utiles
  filter(behavior != "other") %>% # On exclut les comportements "other"
  distinct() %>% # On retire les doublons éventuels
  na.omit() # On retire les lignes avec NA

# Création de groupes temporels (sessions d'activité séparées par > 6h)
distance_dt_2 <- distance_dt_1 %>%
  arrange(ID, datetime) %>% # Tri chronologique par individu
  group_by(ID) %>%
  mutate(
    time_diff = as.numeric(difftime(datetime, lag(datetime), units = "mins")), # Temps écoulé entre deux points
    new_group = if_else(is.na(time_diff) | time_diff > 60 * 6, 1, 0), # Nouveau groupe si > 6h entre points
    group_id = cumsum(new_group) # Attribution d’un ID de groupe
  ) %>%
  ungroup() %>%
  na.omit() # Nettoyage des NA éventuels

# Calcul du centroïde pour chaque groupe (zone d'activité temporelle)
distance_dt_3 <- distance_dt_2 %>%
  group_by(ID, group_id) %>%
  mutate(centroid = st_centroid(st_union(geom))) %>% # Centroïde des points du groupe
  dplyr::select(ID, behavior, group_id, datetime, centroid) %>%
  st_drop_geometry() # Suppression de la géométrie d'origine

# Conversion en objet sf avec la colonne "centroid"
centroid_sf <- distance_dt_3 %>%
  st_as_sf(crs = 4326) %>% # Conversion en sf si les centroïdes sont bien géométriques
  arrange(ID, datetime) # Tri temporel par individu

# Reconversion au cas où le st_as_sf du dessus ne fonctionne pas
centroid_sf <- st_as_sf(distance_dt_3)

# Appariement des centroïdes consécutifs (changement de comportement et < 12h d’écart)
paired_centroids <- centroid_sf %>%
  group_by(ID) %>%
  arrange(datetime) %>%
  mutate(
    behavior_next = lead(behavior), # Comportement suivant
    datetime_next = lead(datetime), # Timestamp suivant
    geom_next = lead(centroid) # Centroïde suivant
  ) %>%
  filter(
    !is.na(datetime_next), # On garde les lignes complètes
    abs(difftime(datetime_next, datetime, units = "hours")) <= 12, # Max 12h d'écart
    behavior != behavior_next # Changement de comportement
  ) %>%
  mutate(
    distance_m = st_distance(centroid, geom_next, by_element = TRUE) # Distance entre centroïdes
  ) %>%
  ungroup()

# Conversion des distances en numérique (pour l’export/stats)
paired_centroids$distance_m <- as.numeric(paired_centroids$distance_m)

# Calcul de la distance moyenne (et SD) par individu
paired_centroids_mean_dt <- paired_centroids %>%
  st_drop_geometry() %>%
  filter(distance_m > 0) %>% # On garde uniquement les vraies distances (> 0)
  group_by(ID) %>%
  summarise(
    mean_dist = mean(distance_m), # Moyenne des distances
    sd_dist = sd(distance_m) # Écart-type des distances
  )

# Moyennes globales (tous individus confondus)
mean_dist <- mean(paired_centroids_mean_dt$mean_dist) # Moyenne globale
sd_dist <- sd(paired_centroids_mean_dt$mean_dist) # Écart-type global

## ~ sexe -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#----

# Récupération du sexe des individus (à partir de la table GPS, sans la géométrie)
sexe_dt <- GPS %>%
  st_drop_geometry() %>%
  dplyr::select(ID, sex) %>% # On sélectionne uniquement ID et sexe
  na.omit() %>% # Suppression des lignes avec NA (individus sans info de sexe)
  distinct() # On garde une seule ligne par ID

# Jointure entre les distances calculées et les sexes des individus
paired_centroids_sex_dt <- paired_centroids %>%
  left_join(sexe_dt) %>% # Ajout de la colonne "sex" par jointure sur ID
  na.omit() # On supprime les lignes avec NA (par exemple, si le sexe est inconnu)

# Nettoyage des doublons et distances nulles
paired_centroids_sex_dt_2 <- paired_centroids_sex_dt %>%
  st_drop_geometry() %>%
  dplyr::select(ID, distance_m, sex) %>% # On garde les infos pertinentes
  filter(distance_m > 0) %>% # On enlève les distances nulles
  distinct() # On enlève les doublons éventuels

# TESTS STATISTIQUES DE COMPARAISON DES DISTANCES ENTRE LES SEXES

# Test de normalité de Shapiro-Wilk pour les femelles
shapiro.test(paired_centroids_sex_dt_2$distance_m[paired_centroids_sex_dt_2$sex == "F"])

# Test de normalité pour les mâles
shapiro.test(paired_centroids_sex_dt_2$distance_m[paired_centroids_sex_dt_2$sex == "M"])

# Test de comparaison des variances (F-test) entre les deux sexes
var.test(
  paired_centroids_sex_dt_2$distance_m[paired_centroids_sex_dt_2$sex == "F"],
  paired_centroids_sex_dt_2$distance_m[paired_centroids_sex_dt_2$sex == "M"]
)

# Test t de Student (avec variances inégales) pour comparer les moyennes de distance entre sexes
comp_moy_sexe <- t.test(
  paired_centroids_sex_dt_2$distance_m[paired_centroids_sex_dt_2$sex == "F"],
  paired_centroids_sex_dt_2$distance_m[paired_centroids_sex_dt_2$sex == "M"],
  var.equal = FALSE
)
comp_moy_sexe # Affichage du résultat

# Modèle linéaire pour tester l'effet du sexe sur la distance
summary(lm(paired_centroids_sex_dt_2$distance_m ~ paired_centroids_sex_dt_2$sex))

## ~ age #-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#----

age_dt <- GPS %>%
  st_drop_geometry() %>%
  dplyr::select(ID, age) %>%
  na.omit() %>%
  distinct()

paired_centroids_age_dt <- paired_centroids %>%
  left_join(age_dt) %>%
  na.omit()

paired_centroids_age_dt_2 <- paired_centroids_age_dt %>%
  st_drop_geometry() %>%
  dplyr::select(ID, distance_m, age) %>%
  filter(distance_m > 0) %>%
  distinct()

# test comparaison de moyenne

shapiro.test(paired_centroids_age_dt_2$distance_m[paired_centroids_age_dt_2$age == "adulte"])
shapiro.test(paired_centroids_age_dt_2$distance_m[paired_centroids_age_dt_2$age == "juvénile"])
var.test(
  paired_centroids_age_dt_2$distance_m[paired_centroids_age_dt_2$age == "adulte"],
  paired_centroids_age_dt_2$distance_m[paired_centroids_age_dt_2$age == "juvénile"]
)

comp_moy_age <- t.test(paired_centroids_age_dt_2$distance_m[paired_centroids_age_dt_2$age == "adulte"],
  paired_centroids_age_dt_2$distance_m[paired_centroids_age_dt_2$age == "juvénile"],
  var.equal = F
)
comp_moy_age

summary(lm(paired_centroids_age_dt_2$distance_m ~ paired_centroids_age_dt_2$age))

## ~ sex + age #-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#----

age_sex_dt <- GPS %>%
  st_drop_geometry() %>%
  dplyr::select(ID, age, sex) %>%
  na.omit() %>%
  distinct()

paired_centroids_age_sex_dt <- paired_centroids %>%
  left_join(age_sex_dt) %>%
  na.omit()

paired_centroids_age_sex_dt_2 <- paired_centroids_age_sex_dt %>%
  st_drop_geometry() %>%
  dplyr::select(ID, distance_m, age, sex) %>%
  filter(distance_m > 0) %>%
  distinct()

summary(lm(paired_centroids_age_sex_dt_2$distance_m ~ paired_centroids_age_sex_dt_2$age * paired_centroids_age_sex_dt_2$sex))
summary(lm(paired_centroids_age_sex_dt_2$distance_m ~ paired_centroids_age_sex_dt_2$age + paired_centroids_age_sex_dt_2$sex))

## ~ chasse -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#----

GPS_dist_chasse <- GPS %>%
  mutate(
    Saison = case_when(
      month(datetime) == 1 ~ paste0(year(datetime) - 1, "/", year(datetime)),
      month(datetime) != 1 ~ paste0(year(datetime), "/", year(datetime) + 1)
    )
  )

GPS_dist_chasse$Saison <- as.character(GPS_dist_chasse$Saison)

chasse_date <- read_excel("D:/Projets_Suzanne/Courlis/3) Data/1) data/Chasse/date ouverture fermeture chasse.xlsx")

GPS_dist_chasse <- GPS_dist_chasse %>%
  left_join(chasse_date)

GPS_dist_chasse <- GPS_dist_chasse %>%
  mutate(in_out_saison = case_when(
    !between(y_m_d, `Ouverture DPM St Froult`, `Fermeture DPM St Froult`) ~ "out",
    between(y_m_d, `Ouverture DPM St Froult`, `Fermeture DPM St Froult`) ~ "in"
  )) %>%
  filter(month_numeric %in% c(7, 8, 9, 10, 11, 12, 1))

table(GPS_dist_chasse$in_out_saison)
table(GPS_dist_chasse$month_numeric)
table(GPS_dist_chasse$month_numeric[GPS_dist_chasse$in_out_saison == "in"])
table(GPS_dist_chasse$month_numeric[GPS_dist_chasse$in_out_saison == "out"])

# test

# chasse_dt <- GPS_dist_chasse %>%
#   st_drop_geometry() %>%
#   dplyr::select(ID, in_out_saison) %>%
#   na.omit() %>%
#   distinct()

paired_centroids_chasse_dt <- paired_centroids %>%
  st_drop_geometry() %>%
  left_join(GPS_dist_chasse) %>%
  na.omit()

table(paired_centroids_chasse_dt$month_numeric)

paired_centroids_chasse_dt_2 <- paired_centroids_chasse_dt %>%
  st_drop_geometry() %>%
  dplyr::select(ID, distance_m, in_out_saison) %>%
  filter(distance_m > 0) %>%
  distinct()

# test comparaison de moyenne

shapiro.test(paired_centroids_chasse_dt_2$distance_m[paired_centroids_chasse_dt_2$in_out_saison == "in"])
shapiro.test(paired_centroids_chasse_dt_2$distance_m[paired_centroids_chasse_dt_2$in_out_saison == "out"])
var.test(
  paired_centroids_chasse_dt_2$distance_m[paired_centroids_chasse_dt_2$in_out_saison == "in"],
  paired_centroids_chasse_dt_2$distance_m[paired_centroids_chasse_dt_2$in_out_saison == "out"]
)

comp_moy_chasse <- t.test(paired_centroids_chasse_dt_2$distance_m[paired_centroids_chasse_dt_2$in_out_saison == "in"],
  paired_centroids_chasse_dt_2$distance_m[paired_centroids_chasse_dt_2$in_out_saison == "out"],
  var.equal = F
)
comp_moy_chasse

summary(lm(paired_centroids_chasse_dt_2$distance_m ~ paired_centroids_chasse_dt_2$in_out_saison))

## ~ tides_high_type #-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#----

tides_high_type_dt <- GPS %>%
  st_drop_geometry() %>%
  dplyr::select(ID, datetime, tides_high_type) %>%
  na.omit() %>%
  distinct()

paired_centroids_tides_high_type_dt <- paired_centroids %>%
  left_join(tides_high_type_dt) %>%
  na.omit()

paired_centroids_tides_high_type_dt_2 <- paired_centroids_tides_high_type_dt %>%
  st_drop_geometry() %>%
  dplyr::select(ID, distance_m, tides_high_type) %>%
  filter(distance_m > 0) %>%
  distinct()

# test comparaison de moyenne

shapiro.test(paired_centroids_tides_high_type_dt_2$distance_m[paired_centroids_tides_high_type_dt_2$tides_high_type == "vives_eaux"])
shapiro.test(paired_centroids_tides_high_type_dt_2$distance_m[paired_centroids_tides_high_type_dt_2$tides_high_type == "mortes_eaux"])
shapiro.test(paired_centroids_tides_high_type_dt_2$distance_m[paired_centroids_tides_high_type_dt_2$tides_high_type == "submersion"])

var.test(
  paired_centroids_tides_high_type_dt_2$distance_m[paired_centroids_tides_high_type_dt_2$tides_high_type == "vives_eaux"],
  paired_centroids_tides_high_type_dt_2$distance_m[paired_centroids_tides_high_type_dt_2$tides_high_type == "mortes_eaux"]
)

var.test(
  paired_centroids_tides_high_type_dt_2$distance_m[paired_centroids_tides_high_type_dt_2$tides_high_type == "vives_eaux"],
  paired_centroids_tides_high_type_dt_2$distance_m[paired_centroids_tides_high_type_dt_2$tides_high_type == "submersion"]
)

var.test(
  paired_centroids_tides_high_type_dt_2$distance_m[paired_centroids_tides_high_type_dt_2$tides_high_type == "mortes_eaux"],
  paired_centroids_tides_high_type_dt_2$distance_m[paired_centroids_tides_high_type_dt_2$tides_high_type == "submersion"]
)

summary(lm(paired_centroids_tides_high_type_dt_2$distance_m ~ paired_centroids_tides_high_type_dt_2$tides_high_type))

## graphique #-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#----

my_comparisons <- list(c("F", "M"))

distance_roost_forag_sex_plot <- ggplot(
  paired_centroids_age_sex_dt_2,
  aes(x = sex, y = distance_m)
) +
  geom_boxplot(col = "black", outlier.colour = "black", outlier.shape = 1, fill = "grey") +
  geom_jitter(shape = 21, size = 0.5, color = "white", alpha = 0.5, fill = "black", width = 0.3) +
  stat_summary(
    fun.ymin = function(x) mean(x) - sd(x),
    fun.ymax = function(x) mean(x) + sd(x), geom = "linerange", size = 1, color = "black"
  ) +
  stat_summary(
    fun.y = mean,
    fun.ymin = function(x) mean(x) - sd(x),
    fun.ymax = function(x) mean(x) + sd(x),
    geom = "pointrange", shape = 21, size = 1, color = "black", fill = "white"
  ) +
  stat_compare_means(
    method = "t.test", comparisons = my_comparisons,
    label.y = c(12500), aes(label = after_stat(p.signif))
  ) +
  theme_classic() +
  labs(
    title = "",
    x = "Sexe", y = "Distance moyenne (m) entre les zones individuelles
journalière d'alimentation et de repos", fill = ""
  )
distance_roost_forag_sex_plot

my_comparisons <- list(c("adulte", "juvénile"))

distance_roost_forag_age_plot <- ggplot(
  paired_centroids_age_sex_dt_2,
  aes(x = age, y = distance_m)
) +
  geom_boxplot(col = "black", outlier.colour = "black", outlier.shape = 1, fill = "grey") +
  geom_jitter(shape = 21, size = 0.5, color = "white", alpha = 0.5, fill = "black", width = 0.3) +
  stat_summary(
    fun.ymin = function(x) mean(x) - sd(x),
    fun.ymax = function(x) mean(x) + sd(x), geom = "linerange", size = 1, color = "black"
  ) +
  stat_summary(
    fun.y = mean,
    fun.ymin = function(x) mean(x) - sd(x),
    fun.ymax = function(x) mean(x) + sd(x),
    geom = "pointrange", shape = 21, size = 1, color = "black", fill = "white"
  ) +
  theme_classic() +
  stat_compare_means(
    method = "t.test", comparisons = my_comparisons,
    label.y = c(12500), aes(label = after_stat(p.signif))
  ) +
  labs(
    title = "",
    x = "Age", y = "Distance moyenne (m) entre les zones individuelles
journalière d'alimentation et de repos", fill = ""
  )
distance_roost_forag_age_plot

my_comparisons <- list(c("in", "out"))

distance_roost_forag_chasse_plot <- ggplot(
  paired_centroids_chasse_dt_2,
  aes(x = in_out_saison, y = distance_m)
) +
  geom_boxplot(col = "black", outlier.colour = "black", outlier.shape = 1, fill = "grey") +
  geom_jitter(shape = 21, size = 0.5, color = "white", alpha = 0.5, fill = "black", width = 0.3) +
  stat_summary(
    fun.ymin = function(x) mean(x) - sd(x),
    fun.ymax = function(x) mean(x) + sd(x), geom = "linerange", size = 1, color = "black"
  ) +
  stat_summary(
    fun.y = mean,
    fun.ymin = function(x) mean(x) - sd(x),
    fun.ymax = function(x) mean(x) + sd(x),
    geom = "pointrange", shape = 21, size = 1, color = "black", fill = "white"
  ) +
  theme_classic() +
  stat_compare_means(
    method = "t.test", comparisons = my_comparisons,
    label.y = c(12500), aes(label = after_stat(p.signif))
  ) +
  labs(
    title = "",
    x = "Periode de chasse", y = "Distance moyenne (m) entre les zones individuelles
journalière d'alimentation et de repos", fill = ""
  )
distance_roost_forag_chasse_plot

my_comparisons <- list(c("vives_eaux", "mortes_eaux", "submersion"))

paired_centroids_tides_high_type_dt_2$tides_high_type <- factor(
  paired_centroids_tides_high_type_dt_2$tides_high_type,
  levels = c("mortes_eaux", "vives_eaux", "submersion")
)

distance_roost_forag_tides_high_type_plot <- ggplot(
  paired_centroids_tides_high_type_dt_2,
  aes(x = tides_high_type, y = distance_m)
) +
  geom_boxplot(col = "black", outlier.colour = "black", outlier.shape = 1, fill = "grey") +
  geom_jitter(shape = 21, size = 0.5, color = "white", alpha = 0.5, fill = "black", width = 0.3) +
  stat_summary(
    fun.ymin = function(x) mean(x) - sd(x),
    fun.ymax = function(x) mean(x) + sd(x), geom = "linerange", size = 1, color = "black"
  ) +
  stat_summary(
    fun.y = mean,
    fun.ymin = function(x) mean(x) - sd(x),
    fun.ymax = function(x) mean(x) + sd(x),
    geom = "pointrange", shape = 21, size = 1, color = "black", fill = "white"
  ) +
  stat_compare_means(
    method = "t.test", comparisons = my_comparisons,
    label.y = c(12500), aes(label = after_stat(p.signif))
  ) +
  theme_classic() +
  labs(
    title = "",
    x = "Hauteur d'eau", y = "Distance moyenne (m) entre les zones individuelles
journalière d'alimentation et de repos", fill = ""
  )
distance_roost_forag_tides_high_type_plot

distance_roost_forag_allvar_plot <- ggarrange(distance_roost_forag_sex_plot,
  distance_roost_forag_age_plot,
  distance_roost_forag_chasse_plot,
  distance_roost_forag_tides_high_type_plot,
  ncol = 4
)

ggsave(paste0(atlas_path, "/distance_roost_forag_allvar_plot.png"),
  plot = distance_roost_forag_allvar_plot, width = 13, height = 4, dpi = 300
)

# mean individuelle
distance_roost_forag_plot <- ggplot(
  paired_centroids_mean_dt,
  aes(x = reorder(ID, mean_dist), y = mean_dist)
) +
  geom_hline(yintercept = mean_dist, color = "black") +
  geom_hline(yintercept = mean_dist + sd_dist, linetype = "longdash", color = "grey") +
  geom_hline(yintercept = mean_dist - sd_dist, linetype = "longdash", color = "grey") +
  geom_errorbar(aes(ymin = mean_dist - sd_dist, ymax = mean_dist + sd_dist), width = 0, color = "grey") +
  geom_point(shape = 21, size = 4, color = "black", fill = "grey") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(
    title = "",
    x = "Individu", y = "Distance moyenne (+/- écart-type) entre
les zones d'alimentation et de repos (m)", fill = ""
  )
distance_roost_forag_plot

ggsave(paste0(atlas_path, "/distance_roost_forag_plot.png"),
  plot = distance_roost_forag_plot, width = 10, height = 4, dpi = 300
)

summary(lm(paired_centroids_mean_dt$mean_dist ~ paired_centroids_mean_dt$sd_dist))

# graphique talk ---

distance_roost_forag_plot_talk <- ggplot(
  paired_centroids_mean_dt,
  aes(x = reorder(ID, mean_dist), y = mean_dist)
) +
  geom_hline(yintercept = mean_dist, color = "black") +
  geom_hline(yintercept = mean_dist + sd_dist, linetype = "longdash", color = "grey") +
  geom_hline(yintercept = mean_dist - sd_dist, linetype = "longdash", color = "grey") +
  geom_errorbar(aes(ymin = mean_dist - sd_dist, ymax = mean_dist + sd_dist), width = 0, color = "grey") +
  geom_point(shape = 21, size = 4, color = "black", fill = "grey") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 7)) +
  labs(
    title = "",
    x = "Individual", y = "Distance between individual
roosting and foragin areas (m)", fill = ""
  )
distance_roost_forag_plot

ggsave(paste0(atlas_path, "/distance_roost_forag_plot_talk.png"),
  plot = distance_roost_forag_plot_talk, width = 8, height = 4, dpi = 300
)

# graphique talk ---

col_sex_age <- c(
  "femelle adulte" = "purple", "femelle juvénile" = "lightpink",
  "mâle adulte" = "darkgreen", "mâle juvénile" = "lightgreen",
  "inconnu" = "grey40"
)

emission_dt_1$sex_age <- factor(emission_dt_1$sex_age, levels = names(col_sex_age))

# plot

dt_distance_talk <- paired_centroids_age_sex_dt_2 %>%
  mutate(
    sex_en = case_when(
      sex == "F" ~ "female",
      sex == "M" ~ "male"
    ),
    age_en = case_when(
      age == "adulte" ~ "adult",
      age == "juvénile" ~ "juvenile"
    )
  )

my_comparisons <- list(c("female", "male"))

distance_roost_forag_sex_plot <- ggplot(
  dt_distance_talk,
  aes(x = sex_en, y = distance_m, fill = sex_en)
) +
  scale_fill_manual(values = c("male" = "darkgreen", "female" = "purple")) +
  geom_boxplot(outlier.colour = "grey", outlier.shape = 1) +
  geom_jitter(shape = 21, size = 0.5, color = "white", alpha = 0.5, fill = "black", width = 0.3) +
  stat_summary(
    fun.ymin = function(x) mean(x) - sd(x),
    fun.ymax = function(x) mean(x) + sd(x), geom = "linerange", size = 1, color = "black"
  ) +
  stat_summary(
    fun.y = mean,
    fun.ymin = function(x) mean(x) - sd(x),
    fun.ymax = function(x) mean(x) + sd(x),
    geom = "pointrange", shape = 21, size = 1, color = "black", fill = "white"
  ) +
  stat_compare_means(
    method = "t.test", comparisons = my_comparisons,
    label.y = c(12500), aes(label = after_stat(p.signif))
  ) +
  theme_classic() +
  theme(legend.position = "none") +
  labs(
    title = "",
    x = "Sex", y = "Distance between individual
roosting and foragin areas (m)", fill = ""
  )
distance_roost_forag_sex_plot

my_comparisons <- list(c("adult", "juvenile"))

distance_roost_forag_age_plot <- ggplot(
  dt_distance_talk,
  aes(x = age_en, y = distance_m, fill = age_en)
) +
  geom_boxplot(outlier.colour = "grey", outlier.shape = 1) +
  scale_fill_manual(
    values = c("adult" = "#D47545", "juvenile" = "#D2AB99"),
    name = "Age"
  ) +
  geom_jitter(shape = 21, size = 0.5, color = "white", alpha = 0.5, fill = "black", width = 0.3) +
  stat_summary(
    fun.ymin = function(x) mean(x) - sd(x),
    fun.ymax = function(x) mean(x) + sd(x), geom = "linerange", size = 1, color = "black"
  ) +
  stat_summary(
    fun.y = mean,
    fun.ymin = function(x) mean(x) - sd(x),
    fun.ymax = function(x) mean(x) + sd(x),
    geom = "pointrange", shape = 21, size = 1, color = "black", fill = "white"
  ) +
  theme_classic() +
  stat_compare_means(
    method = "t.test", comparisons = my_comparisons,
    label.y = c(12500), aes(label = after_stat(p.signif))
  ) +
  theme(legend.position = "none") +
  labs(
    title = "",
    x = "Age", y = "Distance between individual
roosting and foragin areas (m)", fill = ""
  )
distance_roost_forag_age_plot


dt_distance_talk_tide <- paired_centroids_tides_high_type_dt_2 %>%
  mutate(
    tides_high_type_en = case_when(
      tides_high_type == "vives_eaux" ~ "high",
      tides_high_type == "submersion" ~ "flooding",
      tides_high_type == "mortes_eaux" ~ "low",
    )
  )

my_comparisons <- list(c("low", "high", "flooding"))

dt_distance_talk_tide$tides_high_type_en <- factor(dt_distance_talk_tide$tides_high_type_en,
  levels = c("low", "high", "flooding")
)

distance_roost_forag_tides_high_type_plot <- ggplot(
  dt_distance_talk_tide,
  aes(x = tides_high_type_en, y = distance_m, fill = tides_high_type_en)
) +
  geom_boxplot(col = "black", outlier.colour = "grey", outlier.shape = 1) +
  scale_fill_manual(values = c("low" = "#65B4E5", "high" = "#2083C1", "flooding" = "#00426C")) +
  geom_jitter(shape = 21, size = 0.5, color = "white", alpha = 0.5, fill = "black", width = 0.3) +
  stat_summary(
    fun.ymin = function(x) mean(x) - sd(x),
    fun.ymax = function(x) mean(x) + sd(x), geom = "linerange", size = 1, color = "black"
  ) +
  stat_summary(
    fun.y = mean,
    fun.ymin = function(x) mean(x) - sd(x),
    fun.ymax = function(x) mean(x) + sd(x),
    geom = "pointrange", shape = 21, size = 1, color = "black", fill = "white"
  ) +
  stat_compare_means(
    method = "t.test", comparisons = my_comparisons,
    label.y = c(12500), aes(label = after_stat(p.signif))
  ) +
  theme_classic() +
  theme(legend.position = "none") +
  labs(
    title = "",
    x = "Tidal range", y = "Distance between individual
roosting and foragin areas (m)", fill = ""
  )
distance_roost_forag_tides_high_type_plot

distance_roost_forag_allvar_plot <- ggarrange(distance_roost_forag_sex_plot,
  distance_roost_forag_age_plot,
  distance_roost_forag_tides_high_type_plot,
  ncol = 3
)

ggsave(paste0(atlas_path, "/distance_roost_forag_allvar_plot_talk.png"),
  plot = distance_roost_forag_allvar_plot, width = 8, height = 4, dpi = 300
)

############################################################################ ---
# 15. Submersion ---------------------------------------------------------------
############################################################################ ---

## date de submersion -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#----

behavior_dt_final <- st_read(file.path(data_generated_path_serveur, "behavior_dt_final.gpkg"))

sub_dt_1 <- behavior_dt_final %>%
  st_drop_geometry() %>%
  dplyr::select(date, height) %>%
  distinct()

sub_seuil <- 6.4

sub_dt_2 <- sub_dt_1 %>%
  filter(height >= sub_seuil) %>%
  mutate(date = format(date, "%y-%m-%d")) %>%
  distinct()

write.csv(sub_dt_2, paste0(data_generated_path, "submersion_date", ".csv"), row.names = FALSE)

## déplacement lors des submersions -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#----

sub_seuil <- 6.4

# Extraire les dates de submersion
submersion_dates_raw <- GPS$datetime[GPS$tide_height >= sub_seuil]
submersion_dates <- unique(as.Date(submersion_dates_raw))
submersion_dates <- submersion_dates[!is.na(submersion_dates)]

# Créer une séquence de ±7 jours autour de chaque date de submersion
window_dates <- unique(do.call(c, lapply(submersion_dates, function(date) {
  seq(from = as.Date(date) - 3, to = as.Date(date) + 1, by = "day")
})))

# Vérifier la classe
class(window_dates) # Doit retourner "Date"

# Filtrer la base GPS
GPS_filtered <- GPS[as.Date(GPS$datetime) %in% window_dates, ]

# Ajouter la colonne periode
GPS_filtered$periode_sub <- sapply(as.Date(GPS_filtered$datetime), function(date) {
  # Chercher les dates de submersion dans les ±3 jours
  date_submersion_proche <- submersion_dates[abs(difftime(date, submersion_dates, units = "days")) <= 3]

  if (length(date_submersion_proche) == 0) {
    return(NA) # En dehors de la fenêtre
  }

  # Prendre la date de submersion la plus proche
  closest <- date_submersion_proche[which.min(abs(difftime(date, date_submersion_proche, units = "days")))]

  # Comparer
  if (date < closest) {
    return("avant")
  } else if (date > closest) {
    return("après")
  } else {
    return("submersion") # Facultatif, pour les jours de submersion exacts
  }
})

# Vérification
table(GPS_filtered$periode_sub)

# Behaviors
GPS_filtered_roosting <- GPS_filtered %>%
  filter(behavior == "roosting")

GPS_filtered_foraging <- GPS_filtered %>%
  filter(behavior == "foraging")

### roosting #-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

zoom_level <- c("A", "B", "C", "D", "E")
analyse <- "roosting_periode_sub"
results_kud <- NULL
nb_kud <- NULL
comportement <- "roosting"
param <- "periode_sub"
couleur <- nom_pal_roosting
GPS <- GPS_filtered_roosting

# estimer les kernelUD
map_kud.roosting_periode_sub <- Map(estimate_kud_param, zoom_level, comportement, param)
results_kud.roosting_periode_sub <- do.call(rbind, map_kud.roosting_periode_sub)
st_write(results_kud.roosting_periode_sub, paste0(data_generated_path, "results_kud.", analyse, ".gpkg"), append = FALSE)
# compter les nb ind par zoom
nb_kud_map.roosting_periode_sub <- Map(count_nb_kud_param, zoom_level, comportement, param)
nb_kud.roosting_periode_sub <- do.call(rbind, nb_kud_map.roosting_periode_sub)
write.csv(nb_kud.roosting_periode_sub, paste0(data_generated_path, "nb_kud.", analyse, ".csv"), row.names = FALSE)
# resultats
results_kud.roosting_periode_sub <- st_read(file.path(data_generated_path, paste0("results_kud.", analyse, ".gpkg")))
nb_kud.roosting_periode_sub <- read.csv(paste0(data_generated_path, paste0("nb_kud.", analyse, ".csv")), row.names = NULL)
maps_list.roosting_ZOOM_periode_sub <- Map(create_map_param, zoom_level, analyse, param, couleur)

### foraging #-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

zoom_level <- c("A", "B", "C", "D", "E")
analyse <- "foraging_periode_sub"
results_kud <- NULL
nb_kud <- NULL
comportement <- "foraging"
param <- "periode_sub"
couleur <- nom_pal_foraging
GPS <- GPS_filtered_foraging

# estimer les kernelUD
map_kud.foraging_periode_sub <- Map(estimate_kud_param, zoom_level, comportement, param)
results_kud.foraging_periode_sub <- do.call(rbind, map_kud.foraging_periode_sub)
st_write(results_kud.foraging_periode_sub, paste0(data_generated_path, "results_kud.", analyse, ".gpkg"), append = FALSE)
# compter les nb ind par zoom
nb_kud_map.foraging_periode_sub <- Map(count_nb_kud_param, zoom_level, comportement, param)
nb_kud.foraging_periode_sub <- do.call(rbind, nb_kud_map.foraging_periode_sub)
write.csv(nb_kud.foraging_periode_sub, paste0(data_generated_path, "nb_kud.", analyse, ".csv"), row.names = FALSE)
# resultats
results_kud.foraging_periode_sub <- st_read(file.path(data_generated_path, paste0("results_kud.", analyse, ".gpkg")))
nb_kud.foraging_periode_sub <- read.csv(paste0(data_generated_path, paste0("nb_kud.", analyse, ".csv")), row.names = NULL)
maps_list.foraging_ZOOM_periode_sub <- Map(create_map_param, zoom_level, analyse, param, couleur)

############################################################################ ---
# 16. Evènements climatiques extrêmes ------------------------------------------
############################################################################ ---

## données météo #-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#----

# Lecture du fichier Excel contenant les données météo de La Rochelle
meteo <- read_excel(paste0(data_path, "/Meteo/meteo_courlis_la_rochelle.xlsx"))

# Sélection des variables d'intérêt et conversion de la colonne 'date' en format Date
meteo_2 <- meteo %>%
  dplyr::select(date, tavg, tmin, tmax, prcp, wdir, wspd, pres) %>% # températures, précipitations, vent, pression
  rename(y_m_d = date) %>% # renommage de la colonne date
  mutate(y_m_d = ymd(y_m_d)) # conversion en format Date

## vent fort #-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#----

# Création d'une variable catégorielle 'ECE_wspd' qui identifie les jours avec vent ≥ 95e percentile
meteo_ECE_wspd <- meteo_2 %>%
  mutate(
    ECE_wspd = case_when(
      wspd >= quantile(wspd, .95, na.rm = TRUE) ~ "ECE95%", # jour avec événement de vent fort
      TRUE ~ "RAS" # sinon : rien à signaler
    )
  )

# Extraction des dates des événements ECE de vent fort
ECE_dates_wspd <- meteo_ECE_wspd %>%
  filter(ECE_wspd != "RAS") %>% # on garde uniquement les ECE
  pull(y_m_d) # on extrait les dates

# Création d’un vecteur de dates élargi : ajout des jours -7 pour chaque ECE (comparaison sans ECE)
dates_autour_ECE_wspd <- unique(c(ECE_dates_wspd, ECE_dates_wspd - days(7)))

# Filtrage du jeu de données météo pour ne garder que les dates ECE et leurs jours de comparaison
meteo_filtre_ECE_wspd <- meteo_ECE_wspd %>%
  filter(y_m_d %in% dates_autour_ECE_wspd) %>%
  dplyr::select(y_m_d, ECE_wspd)

# Vérification de la distribution des étiquettes (ECE ou RAS)
table(meteo_filtre_ECE_wspd$ECE_wspd)

# Jointure entre les données GPS et les jours avec ou sans ECE, on retire les NA sur ECE_wspd
GPS_ECE_wspd <- left_join(GPS, meteo_filtre_ECE_wspd) %>%
  na.omit(ECE_wspd) # ne garder que les lignes avec un label ECE ou RAS

# Mise à jour de l'objet GPS avec l’info ECE même si NA (utile pour analyses ultérieures)
GPS <- left_join(GPS, meteo_filtre_ECE_wspd)

### reposoir #-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

zoom_level <- c("A", "B", "C", "D", "E")
analyse <- "roosting_ECE_wspd_restricted"
results_kud <- NULL
nb_kud <- NULL
comportement <- "roosting"
param <- "ECE_wspd"
couleur <- nom_pal_roosting

# estimer les kernelUD
map_kud.roosting_ECE_wspd_restricted <- Map(estimate_kud_param, zoom_level, comportement, param)
results_kud.roosting_ECE_wspd_restricted <- do.call(rbind, map_kud.roosting_ECE_wspd_restricted)
st_write(results_kud.roosting_ECE_wspd_restricted, paste0(data_generated_path, "results_kud.", analyse, ".gpkg"), append = FALSE)
# compter les nb ind par zoom
nb_kud_map.roosting_ECE_wspd_restricted <- Map(count_nb_kud_param, zoom_level, comportement, param)
nb_kud.roosting_ECE_wspd_restricted <- do.call(rbind, nb_kud_map.roosting_ECE_wspd_restricted)
write.csv(nb_kud.roosting_ECE_wspd_restricted, paste0(data_generated_path, "nb_kud.", analyse, ".csv"), row.names = FALSE)
# resultats
results_kud.roosting_ECE_wspd_restricted <- st_read(file.path(data_generated_path, paste0("results_kud.", analyse, ".gpkg")))
nb_kud.roosting_ECE_wspd_restricted <- read.csv(paste0(data_generated_path, paste0("nb_kud.", analyse, ".csv")), row.names = NULL)
maps_list.roosting_ZOOM_ECE_wspd_restricted <- Map(create_map_param, zoom_level, analyse, param, couleur)

### alimentation #-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

zoom_level <- c("A", "B", "C", "D", "E")
analyse <- "foraging_ECE_wspd_restricted"
results_kud <- NULL
nb_kud <- NULL
comportement <- "foraging"
param <- "ECE_wspd"
couleur <- nom_pal_foraging

# estimer les kernelUD
map_kud.foraging_ECE_wspd_restricted <- Map(estimate_kud_param, zoom_level, comportement, param)
results_kud.foraging_ECE_wspd_restricted <- do.call(rbind, map_kud.foraging_ECE_wspd_restricted)
st_write(results_kud.foraging_ECE_wspd_restricted, paste0(data_generated_path, "results_kud.", analyse, ".gpkg"), append = FALSE)
# compter les nb ind par zoom
nb_kud_map.foraging_ECE_wspd_restricted <- Map(count_nb_kud_param, zoom_level, comportement, param)
nb_kud.foraging_ECE_wspd_restricted <- do.call(rbind, nb_kud_map.foraging_ECE_wspd_restricted)
write.csv(nb_kud.foraging_ECE_wspd_restricted, paste0(data_generated_path, "nb_kud.", analyse, ".csv"), row.names = FALSE)
# resultats
results_kud.foraging_ECE_wspd_restricted <- st_read(file.path(data_generated_path, paste0("results_kud.", analyse, ".gpkg")))
nb_kud.foraging_ECE_wspd_restricted <- read.csv(paste0(data_generated_path, paste0("nb_kud.", analyse, ".csv")), row.names = NULL)
maps_list.foraging_ZOOM_ECE_wspd_restricted <- Map(create_map_param, zoom_level, analyse, param, couleur)

## vent de Nord-Ouest -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#----

# Création d’une variable 'ECE_wNO' pour détecter les jours où le vent vient du Nord-Ouest (≥ 270°)
meteo_ECE_wNO <- meteo_2 %>%
  mutate(
    ECE_wNO = case_when(
      between(wdir, 270, max(meteo$wdir, na.rm = TRUE)) ~ "ECE Nord-Ouest", # si direction du vent ≥ 270°
      TRUE ~ "RAS" # sinon : rien à signaler
    )
  )

# Extraction des dates où un vent de Nord-Ouest a été détecté (événement ECE)
ECE_dates_wNO <- meteo_ECE_wNO %>%
  filter(ECE_wNO != "RAS") %>% # on garde uniquement les jours avec vent de NO
  pull(y_m_d) # extraction des dates

# Création d’un vecteur de dates élargi : ajout des jours -7 pour chaque ECE (comparaison sans ECE)
dates_autour_ECE_wNO <- unique(c(ECE_dates_wNO, ECE_dates_wNO - days(7)))

# Filtrage du jeu de données météo pour ne garder que les dates ECE et leurs jours de comparaison
meteo_filtre_ECE_wNO <- meteo_ECE_wNO %>%
  filter(y_m_d %in% dates_autour_ECE_wNO) %>%
  dplyr::select(y_m_d, ECE_wNO)

# Vérification du nombre de jours avec ECE ou RAS
table(meteo_filtre_ECE_wNO$ECE_wNO)

# Jointure des données GPS avec l'information sur le vent de Nord-Ouest, et suppression des lignes sans étiquette
GPS_ECE_wNO <- left_join(GPS, meteo_filtre_ECE_wNO) %>%
  na.omit(ECE_wNO) # on garde uniquement les données GPS associées à un jour étiqueté

# Mise à jour de l'objet GPS avec l'information ECE wNO (même si certains jours n'ont pas d'étiquette)
GPS <- left_join(GPS, meteo_filtre_ECE_wNO)

### reposoir #-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

zoom_level <- c("A", "B", "C", "D", "E")
analyse <- "roosting_ECE_wNO_restricted"
results_kud <- NULL
nb_kud <- NULL
comportement <- "roosting"
param <- "ECE_wNO"
couleur <- nom_pal_roosting

# estimer les kernelUD
map_kud.roosting_ECE_wNO_restricted <- Map(estimate_kud_param, zoom_level, comportement, param)
results_kud.roosting_ECE_wNO_restricted <- do.call(rbind, map_kud.roosting_ECE_wNO_restricted)
st_write(results_kud.roosting_ECE_wNO_restricted, paste0(data_generated_path, "results_kud.", analyse, ".gpkg"), append = FALSE)
# compter les nb ind par zoom
nb_kud_map.roosting_ECE_wNO_restricted <- Map(count_nb_kud_param, zoom_level, comportement, param)
nb_kud.roosting_ECE_wNO_restricted <- do.call(rbind, nb_kud_map.roosting_ECE_wNO_restricted)
write.csv(nb_kud.roosting_ECE_wNO_restricted, paste0(data_generated_path, "nb_kud.", analyse, ".csv"), row.names = FALSE)
# resultats
results_kud.roosting_ECE_wNO_restricted <- st_read(file.path(data_generated_path, paste0("results_kud.", analyse, ".gpkg")))
nb_kud.roosting_ECE_wNO_restricted <- read.csv(paste0(data_generated_path, paste0("nb_kud.", analyse, ".csv")), row.names = NULL)
maps_list.roosting_ZOOM_ECE_wNO_restricted <- Map(create_map_param, zoom_level, analyse, param, couleur)

### alimentation #-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

zoom_level <- c("A", "B", "C", "D", "E")
analyse <- "foraging_ECE_wNO_restricted"
results_kud <- NULL
nb_kud <- NULL
comportement <- "foraging"
param <- "ECE_wNO"
couleur <- nom_pal_foraging

results_kud.foraging_ECE_wNO_restricted <- st_read(file.path(data_generated_path, paste0("results_kud.", analyse, ".gpkg")))
nb_kud.foraging_ECE_wNO_restricted <- read.csv(paste0(data_generated_path, paste0("nb_kud.", analyse, ".csv")), row.names = NULL)
# Générer les maps pour chaque zoom
maps_list.foraging_ZOOM_ECE_wNO_restricted <- Map(create_map_param, zoom_level, analyse, param, couleur)

map_kud.foraging_ECE_wNO_restricted <- Map(estimate_kud_param, zoom_level, comportement, param)
results_kud.foraging_ECE_wNO_restricted <- do.call(rbind, map_kud.foraging_ECE_wNO_restricted)
st_write(results_kud.foraging_ECE_wNO_restricted, paste0(data_generated_path, "results_kud.", analyse, ".gpkg"), append = FALSE)
# compter les nb ind par zoom
nb_kud_map.foraging_ECE_wNO_restricted <- Map(count_nb_kud_param, zoom_level, comportement, param)
nb_kud.foraging_ECE_wNO_restricted <- do.call(rbind, nb_kud_map.foraging_ECE_wNO_restricted)
write.csv(nb_kud.foraging_ECE_wNO_restricted, paste0(data_generated_path, "nb_kud.", analyse, ".csv"), row.names = FALSE)

## vent de Nord-Ouest & vent fort -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#----

# Création de trois colonnes :
# - 'ECE_wspd' : vent fort (≥ 95e percentile)
# - 'ECE_wNO' : vent de Nord-Ouest (direction ≥ 270°)
# - 'ECE_wNO_wspd95' : les deux conditions réunies

meteo_ECE_wNO_wspd95 <- meteo_2 %>%
  mutate(
    # Identification des jours avec vent fort
    ECE_wspd = case_when(
      wspd >= quantile(wspd, .95, na.rm = TRUE) ~ "ECE95%",
      TRUE ~ "RAS"
    ),
    # Identification des jours avec vent de Nord-Ouest
    ECE_wNO = case_when(
      between(wdir, 270, max(meteo$wdir, na.rm = TRUE)) ~ "ECE Nord-Ouest",
      TRUE ~ "RAS"
    ),
    # Événement combiné : vent fort ET de Nord-Ouest
    ECE_wNO_wspd95 = case_when(
      wspd >= quantile(wspd, .95, na.rm = TRUE) & ECE_wNO == "ECE Nord-Ouest" ~ "ECE95% & Nord-Ouest",
      TRUE ~ "RAS"
    )
  )

# Extraction des dates où les deux conditions sont réunies
ECE_dates_wNO_wspd95 <- meteo_ECE_wNO_wspd95 %>%
  filter(ECE_wNO_wspd95 != "RAS") %>% # on garde uniquement les jours avec ECE combiné
  pull(y_m_d) # extraction des dates

# Ajout de jours de comparaison (7 jours avant chaque ECE)
dates_autour_ECE_wNO_wspd95 <- unique(c(ECE_dates_wNO_wspd95, ECE_dates_wNO_wspd95 - days(7)))

# Filtrage du jeu de données météo pour ne garder que les dates ECE combinées et les jours de comparaison
meteo_filtre_ECE_wNO_wspd95 <- meteo_ECE_wNO_wspd95 %>%
  filter(y_m_d %in% dates_autour_ECE_wNO_wspd95) %>%
  dplyr::select(y_m_d, ECE_wNO_wspd95)

# Vérification du nombre d’événements combinés vs RAS
table(meteo_filtre_ECE_wNO_wspd95$ECE_wNO_wspd95)

# Jointure entre les données GPS et les étiquettes d'événement combiné, suppression des lignes sans étiquette
GPS_ECE_wNO_wspd95 <- left_join(GPS, meteo_filtre_ECE_wNO_wspd95) %>%
  na.omit(ECE_wNO_wspd95) # on garde uniquement les données avec une étiquette valide

# Mise à jour de l’objet GPS avec l’info d’ECE combiné (même si certains jours n’ont pas d’étiquette)
GPS <- left_join(GPS, meteo_filtre_ECE_wNO_wspd95)

### reposoir #-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

zoom_level <- c("A", "B", "C", "D", "E")
analyse <- "roosting_ECE_wNO_wspd95_restricted"
results_kud <- NULL
nb_kud <- NULL
comportement <- "roosting"
param <- "ECE_wNO_wspd95"
couleur <- nom_pal_roosting

# estimer les kernelUD
map_kud.roosting_ECE_wNO_wspd95_restricted <- Map(estimate_kud_param, zoom_level, comportement, param)
results_kud.roosting_ECE_wNO_wspd95_restricted <- do.call(rbind, map_kud.roosting_ECE_wNO_wspd95_restricted)
st_write(results_kud.roosting_ECE_wNO_wspd95_restricted, paste0(data_generated_path, "results_kud.", analyse, ".gpkg"), append = FALSE)
# compter les nb ind par zoom
nb_kud_map.roosting_ECE_wNO_wspd95_restricted <- Map(count_nb_kud_param, zoom_level, comportement, param)
nb_kud.roosting_ECE_wNO_wspd95_restricted <- do.call(rbind, nb_kud_map.roosting_ECE_wNO_wspd95_restricted)
write.csv(nb_kud.roosting_ECE_wNO_wspd95_restricted, paste0(data_generated_path, "nb_kud.", analyse, ".csv"), row.names = FALSE)
# resultats
results_kud.roosting_ECE_wNO_wspd95_restricted <- st_read(file.path(data_generated_path, paste0("results_kud.", analyse, ".gpkg")))
nb_kud.roosting_ECE_wNO_wspd95_restricted <- read.csv(paste0(data_generated_path, paste0("nb_kud.", analyse, ".csv")), row.names = NULL)
maps_list.roosting_ZOOM_ECE_wNO_wspd95_restricted <- Map(create_map_param, zoom_level, analyse, param, couleur)

### alimentation #-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

zoom_level <- c("A", "B", "C", "D", "E")
analyse <- "foraging_ECE_wNO_wspd95_restricted"
results_kud <- NULL
nb_kud <- NULL
comportement <- "foraging"
param <- "ECE_wNO_wspd95"
couleur <- nom_pal_foraging

# estimer les kernelUD
map_kud.foraging_ECE_wNO_wspd95_restricted <- Map(estimate_kud_param, zoom_level, comportement, param)
results_kud.foraging_ECE_wNO_wspd95_restricted <- do.call(rbind, map_kud.foraging_ECE_wNO_wspd95_restricted)
st_write(results_kud.foraging_ECE_wNO_wspd95_restricted, paste0(data_generated_path, "results_kud.", analyse, ".gpkg"), append = FALSE)
# compter les nb ind par zoom
nb_kud_map.foraging_ECE_wNO_wspd95_restricted <- Map(count_nb_kud_param, zoom_level, comportement, param)
nb_kud.foraging_ECE_wNO_wspd95_restricted <- do.call(rbind, nb_kud_map.foraging_ECE_wNO_wspd95_restricted)
write.csv(nb_kud.foraging_ECE_wNO_wspd95_restricted, paste0(data_generated_path, "nb_kud.", analyse, ".csv"), row.names = FALSE)
# resultats
results_kud.foraging_ECE_wNO_wspd95_restricted <- st_read(file.path(data_generated_path, paste0("results_kud.", analyse, ".gpkg")))
nb_kud.foraging_ECE_wNO_wspd95_restricted <- read.csv(paste0(data_generated_path, paste0("nb_kud.", analyse, ".csv")), row.names = NULL)
maps_list.foraging_ZOOM_ECE_wNO_wspd95_restricted <- Map(create_map_param, zoom_level, analyse, param, couleur)

# 17. Zone critique ------------------------------------------------------------

hotspot_roosting_ID_hotspot <- st_read(file.path(data_generated_path, "hotspot_roosting_ID_hotspot.gpkg"))
hotspot_foraging_ID_hotspot <- st_read(file.path(data_generated_path, "hotspot_foraging_ID_hotspot.gpkg"))

hotspot_roosting_ID_hotspot$n_ID <- factor(hotspot_roosting_ID_hotspot$n_ID, levels = c("1", "2", "3", "4", "5", "6", "27"))
hotspot_foraging_ID_hotspot$n_ID <- factor(hotspot_foraging_ID_hotspot$n_ID, levels = c("1", "2", "3", "4", "34"))

# Ajout type
hotspot_roosting_ID_hotspot$type <- "Roosting"
hotspot_foraging_ID_hotspot$type <- "Foraging"

# Colonnes communes
common_cols <- intersect(names(hotspot_roosting_ID_hotspot), names(hotspot_foraging_ID_hotspot))

# Fusion
hotspots_all <- rbind(
  hotspot_roosting_ID_hotspot[, common_cols],
  hotspot_foraging_ID_hotspot[, common_cols]
)

# Nettoyage et colonnes supplémentaires
hotspots_all <- hotspots_all %>%
  mutate(
    n_ID = as.numeric(as.character(n_ID)),
    border_color = ifelse(type == "Roosting", "#9650A6FF", "#0095AFFF")
  )

# Palette de fill en nuances de noir selon n_ID
n_ids <- sort(unique(hotspots_all$n_ID))
amounts <- seq(0.1, 0.6, length.out = length(n_ids))

fill_colors <- sapply(amounts, function(x) lighten("black", amount = x))
fill_palette <- setNames(fill_colors, as.character(n_ids))

# Couleurs spécifiques
fill_palette["27"] <- "black"
fill_palette["34"] <- "black"

# tmap interactive
tmap_mode("view")
zone_critique_hotspot_map_final_1 <- tm_basemap(c("OpenStreetMap", "Esri.WorldImagery", "CartoDB.Positron")) +
  tm_scalebar() +
  tm_shape(hotspots_all) +
  tm_polygons(
    fill = "n_ID", # remplit selon n_ID
    palette = fill_palette, # nuances de noir
    border.col = "border_color", # bordure couleur selon type
    fill_alpha = 0.5,
    lwd = 2,
    title = "Hotspot ID"
  ) +
  tm_shape(site_baguage) +
  tm_text("icone", size = 1.5)
zone_critique_hotspot_map_final_1

tmap_save(zone_critique_hotspot_map_final_1, paste0(atlas_path, "zone_critique_hotspot_map_final_1.html"))

tmap_mode("view")
zone_critique_hotspot_map_final_2 <- tm_basemap(c("OpenStreetMap", "Esri.WorldImagery", "CartoDB.Positron")) +
  tm_scalebar() +
  tm_shape(hotspots_all) +
  tm_polygons(
    fill = "n_ID", # remplit selon n_ID
    palette = fill_palette, # nuances de noir
    border.col = "border_color", # bordure couleur selon type
    fill_alpha = 0.5,
    lwd = 2,
    title = "Hotspot ID"
  ) +
  tm_shape(site_baguage) +
  tm_text("icone", size = 1.5) +
  tm_shape(tonnes_zones_grouped_clean) +
  tm_basemap(c("OpenStreetMap", "Esri.WorldImagery", "CartoDB.Positron")) +
  tm_polygons(
    fill = "#D64045",
    style = "cont", alpha = 0.5,
    title = "Nb superposées"
  ) +
  tm_shape(tonnes) +
  tm_dots(fill = "black")
zone_critique_hotspot_map_final_2

tmap_save(zone_critique_hotspot_map_final_2, paste0(atlas_path, "zone_critique_hotspot_map_final_2.html"))

# 18. Trajet quotidien ---------------------------------------------------------

library(tmap)
library(sf)
library(dplyr)

# Exemple fictif : df = données GPS
# Colonnes : id (oiseau), date (jour), lon, lat
# Convertir en sf
gps_sf <- GPS %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
  filter(ID %in% c("EA580464", "EA581523", "EC103787")) %>%
  filter(week == 31)

trajets_jour <- gps_sf %>%
  mutate(round_time = round(datetime + 3600/2 - !(as.numeric(datetime) %% 3600), "hours")) %>%
  arrange(ID, round_time) %>%
  group_by(ID, round_time) %>%
  filter(n() > 1) %>%   # au moins 2 points pour faire une ligne
  summarise(
    round_time = first(round_time),                 # conserve la date
    geometry = st_combine(geometry) |> st_cast("LINESTRING"),
    .groups = "drop"
  )

tmap_mode("view")

tm_shape(trajets_jour) +
  tm_lines(col = "ID", lwd = 2) +
  tm_facets(pages = "round_time")





library(dplyr)
library(sf)
library(tmap)
library(lubridate)

# convertir en sf et filtrer
gps_sf <- GPS %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
  filter(ID %in% c("EA580464", "EA581523", "EC103787"),
         week == 31)

# arrondir l'heure au plus proche
trajets_jour <- gps_sf %>%
  mutate(round_time = floor_date(datetime, unit = "hour")) %>%  # lubridate floor_date
  arrange(ID, round_time) %>%
  group_by(ID, round_time) %>%
  filter(n() > 1) %>%  # au moins 2 points pour faire une ligne
  summarise(
    geometry = st_cast(st_combine(geometry), "LINESTRING"),
    .groups = "drop"
  )

trajets_jour <- trajets_jour %>%
  mutate(round_time_label = format(round_time, "%Y-%m-%d %H:%M"))  # transforme en caractère lisible

trajets_jour <- trajets_jour %>% 
  filter(round_time_label)

tmap_mode("view")

tm_shape(trajets_jour) +
  tm_lines(col = "round_time_label", lwd = 2) +
  tm_facets(by = "ID") + 
  tm_layout(legend.show = FALSE)








# gganimate

library(dplyr)
library(sf)
library(ggplot2)
library(gganimate)
library(lubridate)

# Filtrer et arrondir à l'heure
gps_sf <- GPS %>%
  filter(ID %in% c("EA580464"),
         week == 31) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  mutate(round_time = floor_date(datetime, "hour")) %>%
  arrange(ID, round_time)

# Créer les lignes heure par heure
trajets_jour <- gps_sf %>%
  group_by(ID, round_time) %>%
  filter(n() > 1) %>%
  summarise(geometry = st_cast(st_combine(geometry), "LINESTRING"), .groups = "drop")

# Convertir en dataframe pour ggplot
coords <- st_coordinates(trajets_jour)
trajets_df <- as.data.frame(coords)

# Lier les IDs et round_time via la colonne L1 générée par st_coordinates()
trajets_df$ID <- trajets_jour$ID[trajets_df$L1]
trajets_df$round_time <- trajets_jour$round_time[trajets_df$L1]

# Créer l'animation
p <- ggplot(trajets_df, aes(X, Y, color = ID, group = interaction(ID, round_time))) +
  geom_path(size = 1.2) +
  theme_minimal() +
  labs(title = "Déplacement des oiseaux : {frame_time}", x = "Longitude", y = "Latitude") +
  transition_time(round_time) +
  ease_aes('linear')

animate(p, nframes = length(unique(trajets_df$round_time)), fps = 2)

library(gganimate)

# Supposons que p est ton ggplot animé
anim <- animate(p, nframes = length(unique(trajets_df$round_time)), fps = 2, width = 800, height = 600)

# Sauvegarder en GIF
library(gganimate)
library(gifski)

# Générer l'animation et forcer le renderer GIF
anim <- animate(
  p,
  nframes = length(unique(trajets_df$round_time)),
  fps = 2,
  width = 800,
  height = 600,
  renderer = gifski_renderer()
)

# Sauvegarder
anim_save(paste0(atlas_path, "deplacement_oiseaux.gif"), animation = anim)



