
# Run la partie "starting blok", 
# puis seulement runner la dernière partie "SAVE" avant la partie à souhaitée / en cours de travail

# STARTING BLOCK ---------------------------------------------------------------
library(beepr)

# beep lorsqu'il y a une erreur 
options(error = function() {beep(7)})
# options(error = NULL)

# Nettoyage de l'environnement
rm(list=ls()) 

# time zone
library(lubridate)
with_tz(Sys.time(), "Europe/Paris")

## Packages --------------------------------------------------------------------
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
  "maptiles", "ggnewscale"
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


























# # old 
# 
# 
# 
# dir.create("C:/Users/Suzanne.Bonamour/Rlibs", showWarnings = FALSE, recursive = TRUE)
# .libPaths("C:/Users/Suzanne.Bonamour/Rlibs")
# install.packages("stringi", lib = "C:/Users/Suzanne.Bonamour/Rlibs", type = "binary")
# 
# install.packages("marmap", lib = "C:/Users/Suzanne.Bonamour/Rlibs", dependencies = TRUE)
# 
# update.packages(ask = FALSE)
# 
# # dir.create("C:/Users/Suzanne.Bonamour/R_lib_temp", showWarnings = FALSE)
# # install.packages("terra", lib = "C:/Users/Suzanne.Bonamour/R_lib_temp", type = "binary")
# # library(terra, lib.loc = "C:/Users/Suzanne.Bonamour/R_lib_temp")
# library(tidyverse)
# library(terra)
# library(sf)
# library(adehabitatLT)
# library(raster)
# library(tmap)
# library(adehabitatHR)
# library(viridis)
# library(beepr)
# library(readxl)
# library(marmap)
# library(pals)
# library(stars)
# library(ggcorrplot)
# library(tibble)
# library(paletteer)
# library(ggeffects)
# library(lmerTest)
# library(ggthemes)
# library(broom.mixed)
# library(performance)
# library(ggpubr)
# library(maptiles)
# library(ggnewscale)

## Paramètres généraux ---------------------------------------------------------

resolution_ZOOM = 10

palette_viri = viridis::viridis(10, begin = 0, end = 1, direction = 1, option = "plasma")

palette_grey <- paletteer_c("grDevices::Grays", 10) 
palette_roosting <- paletteer_c("grDevices::Sunset", 10)  
palette_foraging <- paletteer_c("grDevices::YlGnBu", 10) 
nom_pal_roosting <- "grDevices::Sunset"  
nom_pal_foraging <- "grDevices::YlGnBu" 

# reverse of %in%  
`%ni%` <- Negate(`%in%`)

# Liste des niveaux de zoom
zoom_level <- c("A", "B", "C", "D", "E")

## Functions -------------------------------------------------------------------

# ---
# crs ---
# ---

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

estimate_kud <- function(zoom_level, analyse, comportement) {
  
  crs_utm <- "EPSG:32630"
  
  # in ZOOM
  ZOOM_shape <- st_read(paste0(data_generated_path,"ZOOM_",zoom_level,".gpkg"))
  ZOOM_shape <- st_transform(ZOOM_shape, crs = 4326)
  GPS.ZOOM <- st_intersection(GPS, ZOOM_shape) 
  
  # données pour kernel
  GPS.behavior <- GPS.ZOOM %>% 
    filter(behavior == comportement) %>% 
    dplyr::select(lon,lat) %>% 
    st_drop_geometry() %>% 
    na.omit()
  
  GPS_spa.behavior <- st_as_sf(GPS.behavior, coords = c("lon", "lat"), crs = 4326)
  GPS_spa.behavior <- st_transform(GPS_spa.behavior, crs = 32630)  
  GPS_coords.behavior <- st_coordinates(GPS_spa.behavior) 
  
  # raster/grid
  grid <- st_read(paste0(data_generated_path, "grid_ZOOM_",zoom_level,".gpkg"))
  raster <- rast(grid, resolution = resolution_ZOOM, crs="EPSG:2154")
  spatRaster <- project(raster, crs_utm)
  rasterLayer <- raster(spatRaster)
  spatialPixels <- as(rasterLayer, "SpatialPixels") 
  
  # Règle de Silverman
  sigma_x <- sd(GPS_coords.behavior[,1]) 
  sigma_y <- sd(GPS_coords.behavior[,2]) 
  nb <- nrow(GPS.behavior)
  h.silverman_x <- 1.06 * sigma_x * nb^(-1/5) / 2
  h.silverman_y <- 1.06 * sigma_y * nb^(-1/5) / 2
  locs_spa <- as(GPS_spa.behavior, "Spatial")
  
  # KernelUD
  kud <- kernelUD(locs_spa, 
                  grid = spatialPixels, 
                  h = mean(c(h.silverman_x, h.silverman_y)))
  
  # Isoclines 
  rast <- rast(kud)
  courtour <- as.contour(rast)
  sf <- st_as_sf(courtour)
  cast <- st_cast(sf, "POLYGON")
  cast$ZOOM <- zoom_level
  # results_kud <- rbind(results_kud, cast)
  results_kud <- cast
  
}

estimate_kud_param <- function(zoom_level, comportement, param) {
  
  crs_utm <- "EPSG:32630"
  
  ZOOM <- st_read(paste0(data_generated_path,"ZOOM_",zoom_level,".gpkg"))
  ZOOM <- st_transform(ZOOM, crs = 4326)
  GPS.ZOOM <- st_intersection(GPS, ZOOM) 
  
  GPS.behavior.param <- GPS.ZOOM %>% 
    filter(behavior == comportement) %>% 
    dplyr::select(lon,lat,param) %>% 
    st_drop_geometry() %>% 
    na.omit()
  
  if (nrow(GPS.behavior.param) == 0) {
    return(NULL)
  }
  
  GPS_spa <- st_as_sf(GPS.behavior.param, coords = c("lon", "lat"), crs = 4326)
  GPS_spa <- st_transform(GPS_spa, crs = 32630) 
  GPS_coods <- st_coordinates(GPS_spa)
  
  # raster/grid
  grid <- st_read(paste0(data_generated_path, "grid_ZOOM_",zoom_level,".gpkg"))
  raster <- rast(grid, resolution = resolution_ZOOM, crs="EPSG:2154")
  spatRaster <- project(raster, crs_utm)
  rasterLayer <- raster(spatRaster)
  spatialPixels <- as(rasterLayer, "SpatialPixels") 
  
  # Règle de Silverman
  sigma_x <- sd(GPS_coods[,1]) 
  sigma_y <- sd(GPS_coods[,2]) 
  nb <- nrow(GPS.behavior.param)  
  h.silverman_x <- 1.06 * sigma_x * nb^(-1/5) / 2
  h_silverman_y <- 1.06 * sigma_y * nb^(-1/5) / 2
  locs_spa <- as(GPS_spa, "Spatial")
  
  # KernelUD
  kud <- kernelUD(locs_spa[param], 
                  grid = spatialPixels, 
                  h = mean(c(h.silverman_x, h_silverman_y)))
  
  kud_list <- lapply(names(kud), function(param) {
    
    print(param)
    
    # Extraire l'estimation de densité pour un ID spécifique
    kud_single <- kud[[param]]
    rast <- rast(kud_single)
    courtour <- as.contour(rast)
    sf <- st_as_sf(courtour)
    cast <- st_cast(sf, "POLYGON")
    cast$param <- param
    
    return(cast)
    
  })
  
  kud_all <- do.call(rbind, kud_list)
  kud_all$param <- as.factor(kud_all$param)
  kud_all$ZOOM <- zoom_level
  results_kud <- kud_all
  
  # kud_all$param <- as.factor(kud_all$param)
  # kud_all$ZOOM <- zoom_level
  # 
  # return(kud_all)
  # 
}

count_nb_kud <- function(zoom_level, comportement) {

  # in ZOOM
  ZOOM_shape <- st_read(paste0(data_generated_path,"ZOOM_",zoom_level,".gpkg"))
  ZOOM_shape <- st_transform(ZOOM_shape, crs = 4326)
  GPS.ZOOM <- st_intersection(GPS, ZOOM_shape) 
  
  # nb ind & point 
  nb_ind_point_dt <- GPS.ZOOM %>% 
    filter(behavior == comportement) %>%
    group_by(ID) %>% 
    dplyr::select(ID, datetime) %>% 
    st_drop_geometry() %>% 
    na.omit() %>% 
    summarise(n = n()) %>% 
    mutate(zoom = zoom_level)

  # nb ind & point
  nb_kud <- rbind(nb_kud, nb_ind_point_dt)
  nb_kud <- nb_ind_point_dt
  
}

count_nb_kud_param <- function(zoom_level, comportement, param) {
  
  # in ZOOM
  ZOOM <- st_read(paste0(data_generated_path,"ZOOM_",zoom_level,".gpkg"))
  ZOOM <- st_transform(ZOOM, crs = 4326)
  GPS.ZOOM <- st_intersection(GPS, ZOOM) 
  
  # nb ind & point 
  nb_ind_point_dt <- GPS.ZOOM %>% 
    filter(behavior == comportement) %>%
    dplyr::group_by(ID, .data[[param]]) %>% 
    dplyr::select(ID, param = .data[[param]], datetime) %>% 
    st_drop_geometry() %>% 
    na.omit() %>% 
    summarise(n = n()) %>% 
    mutate(zoom = zoom_level)
  
  if (nrow(nb_ind_point_dt) == 0) {
    return(NULL)
  }
  
  # nb ind & point
  nb_kud <- rbind(nb_kud, nb_ind_point_dt)
  nb_kud <- nb_ind_point_dt
  
}

create_map <- function(zoom_level, analyse, couleur) {
  
  # Récupérer l'objet ZOOM correspondant
  zoom_obj <- get(paste0("ZOOM_", zoom_level))
  
  # Récupérer le bbox du zoom
  bbox <- st_bbox(zoom_obj)
  
  # Créer un point en haut à gauche avec un décalage
  point_top_left <- st_sfc(st_point(c(bbox["xmin"] + 1000, bbox["ymax"] - 500)), crs = st_crs(zoom_obj))
  
  # Créer l'objet sf pour le label
  label_point <- st_sf(label = zoom_level, geometry = point_top_left)
  
  # Get
  labels_zoom <- get(paste0("labels_ZOOM_", zoom_level))
  nb_kud <- get(paste0("nb.",analyse))
  results_kud <- get(paste0("results_kud.",analyse))
  
  # nb ind et point 
  stats_row <- nb_kud[nb_kud$zoom == zoom_level, ]
  nb_ind <- length(stats_row$ID)
  nb_point <- sum(stats_row$n)
  info_text <- paste0(nb_point, " points / ", nb_ind, " individus" )
  point_text_info <- st_sfc(st_point(c(bbox["xmin"] + 1000, bbox["ymax"] - 1000)), crs = st_crs(zoom_obj))
  info_label_point <- st_sf(label = info_text, geometry = point_text_info)
  
  # Construire la carte tmap
  tmap_mode("view")
  map <- tm_scalebar() +
    tm_basemap(c("OpenStreetMap", "Esri.WorldImagery", "CartoDB.Positron")) +
    tm_shape(results_kud[results_kud$ZOOM == zoom_level,]) + 
    tm_polygons(border.col = "grey", fill = "level", fill_alpha = 1, 
                palette = paletteer_c(couleur, 10)) +
    tm_shape(zoom_obj) +
    tm_borders(col = "#575757", lty = "dotted", size = 3) +
    tm_shape(label_point) +
    tm_text("label", col = "#575757", size = 3, just = c("left", "top")) + 
    tm_shape(terre_mer) +
    tm_lines(col = "lightblue", lwd = 0.1) +
    tm_shape(labels_zoom) +
    tm_text("name", size = 1, col = "#575757", 
            fontface = "bold", just = "left") +
    tm_credits(info_text, 
               position = c("left", "bottom"), 
               size = 1, 
               col = "black", 
               bg.color = "white", 
               bg.alpha = 0.7,
               fontface = "bold")
  
  # Sauvegarder la carte
  tmap_save(map, paste0(atlas_path, "UDMap_",analyse,"_", zoom_level, ".html"))
  
  return(map)
}

create_map_param <- function(zoom_level, analyse, param, couleur) {
  
  # Récupérer l'objet ZOOM correspondant
  zoom_obj <- get(paste0("ZOOM_", zoom_level))
  
  # Récupérer le bbox du zoom
  bbox <- st_bbox(zoom_obj)
  
  # Créer un point en haut à gauche avec un décalage
  point_top_left <- st_sfc(st_point(c(bbox["xmin"] + 1000, bbox["ymax"] - 500)), crs = st_crs(zoom_obj))
  
  # Créer l'objet sf pour le label
  label_point <- st_sf(label = zoom_level, geometry = point_top_left)
  
  # Get
  labels_zoom <- get(paste0("labels_ZOOM_", zoom_level))
  nb_kud <- get(paste0("nb_kud.",analyse))
  results_kud <- get(paste0("results_kud.", analyse))
  
  names(results_kud)[2] <- param
  
  # nb ind et point # nb inparamd et point 
  stats_row <- nb_kud[nb_kud$zoom == zoom_level, ]
  nb_ind <- length(stats_row$ID)
  nb_point <- sum(stats_row$n)
  info_text <- paste0(nb_point, " points / ", nb_ind, " individus" )
  point_text_info <- st_sfc(st_point(c(bbox["xmin"] + 1000, bbox["ymax"] - 1000)), crs = st_crs(zoom_obj))
  info_label_point <- st_sf(label = info_text, geometry = point_text_info)
  
  if (nrow(stats_row) == 0) {
    message("Pas de données !")
    return(invisible(NULL))
  }
  
  # Construire la carte tmap
  tmap_mode("view")
  map <- tm_scalebar() +
    tm_basemap(c("OpenStreetMap", "Esri.WorldImagery", "CartoDB.Positron")) +
    tm_shape(results_kud[results_kud$ZOOM == zoom_level,]) + 
    tm_polygons(border.col = "grey", fill = param, 
                title = param,
                fill_alpha = 0.8, 
                palette = paletteer::paletteer_c(couleur, 3)
                ) +
    tm_shape(zoom_obj) +
    tm_borders(col = "#575757", lty = "dotted", size = 3) +
    tm_shape(label_point) +
    tm_text("label", col = "#575757", size = 3, just = c("left", "top")) + 
    tm_shape(terre_mer) +
    tm_lines(col = "lightblue", lwd = 0.1) +
    tm_shape(labels_zoom) +
    tm_text("name", size = 1, col = "#575757", 
            fontface = "bold", just = "left") +
    tm_credits(info_text, 
               position = c("left", "bottom"), 
               size = 1, 
               col = "black", 
               bg.color = "white", 
               bg.alpha = 0.7,
               fontface = "bold")
  
  # Sauvegarder la carte
  tmap_save(map, paste0(atlas_path, "UDMap_",analyse,"_", zoom_level, ".html"))
  
  return(map)
}

## Chemins de données ----------------------------------------------------------

data_path <- "D:/Projets_Suzanne/Courlis/3) Data/1) data/"
data_generated_path <- "D:/Projets_Suzanne/Courlis/3) Data/2) data_generated/"
data_image_path <- "D:/Projets_Suzanne/Courlis/3) Data/3) images/"
data_view_map_path <- "D:/Projets_Suzanne/Courlis/3) Data/4) view_map/"
atlas_path <- "D:/Projets_Suzanne/Courlis/Atlas_Courlis/"


## Font de carte ---------------------------------------------------------------

# Réserve naturelle Moeze Oléron ---
reserve <- st_read(paste0(data_path, "Réserve_naturelle/rnn/rnn/N_ENP_RNN_S_000.shp")) 
RMO <- reserve[reserve$NOM_SITE == "Moëze-Oléron", ]
rm(reserve) 

# Zone d'intérêt (box) ---
# BOX <- st_as_sf(st_as_sfc(st_bbox(c(xmin = -1.26, xmax = -0.945, ymax = 46.01, ymin = 45.78), crs = st_crs(4326)))) 
# st_write(BOX, paste0(data_generated_path, "BOX.gpkg"), append = FALSE) 
BOX <- st_read(paste0(data_generated_path, "BOX.gpkg")) 
BOX_4326 <- st_transform(BOX, crs = 4326) # Transformation de la boîte au CRS 4326 (coordonnées géographiques)
BOX_2154 <- st_transform(BOX, crs = 2154) # Transformation de la boîte au CRS 2154 (coordonnées géographiques)

area_box <- st_area(BOX)
area_box_km <- area_box / 1000000

# Zoom ---
ZOOM_A <- st_transform(st_as_sf(st_as_sfc(st_bbox(c(xmin = -1.245, xmax = -1.18, ymax = 45.975, ymin = 45.825), crs = st_crs(4326)))), crs = 2154)
ZOOM_B <- st_transform(st_as_sf(st_as_sfc(st_bbox(c(xmin = -1.13, xmax = -1.06, ymax = 45.975, ymin = 45.923), crs = st_crs(4326)))), crs = 2154)
ZOOM_C <- st_transform(st_as_sf(st_as_sfc(st_bbox(c(xmin = -1.13, xmax = -1.06, ymax = 45.923, ymin = 45.865), crs = st_crs(4326)))), crs = 2154)
ZOOM_D <- st_transform(st_as_sf(st_as_sfc(st_bbox(c(xmin = -1.18, xmax = -1.08, ymax = 45.865, ymin = 45.81), crs = st_crs(4326)))), crs = 2154)
ZOOM_E <- st_transform(st_as_sf(st_as_sfc(st_bbox(c(xmin = -0.95, xmax = -1.08, ymax = 45.865, ymin = 45.795), crs = st_crs(4326)))), crs = 2154)
# st_write(ZOOM_A, paste0(data_generated_path, "ZOOM_A.gpkg"), append = FALSE) 
# st_write(ZOOM_B, paste0(data_generated_path, "ZOOM_B.gpkg"), append = FALSE) 
# st_write(ZOOM_C, paste0(data_generated_path, "ZOOM_C.gpkg"), append = FALSE) 
# st_write(ZOOM_D, paste0(data_generated_path, "ZOOM_D.gpkg"), append = FALSE)
# st_write(ZOOM_E, paste0(data_generated_path, "ZOOM_E.gpkg"), append = FALSE)

ZOOM <- rbind(ZOOM_A, ZOOM_B, ZOOM_C, ZOOM_D, ZOOM_E)
ZOOM$name <- c("A","B","C","D","E")
ZOOM <- ZOOM %>%
  rename(geometry = x)

# Departement ---
dept <- st_read(paste0(data_path, "departements.gpkg"), layer = "contourdesdepartements")
dept_BOX <- st_intersection(dept, BOX_4326)
rm(dept) 

# Limite terre mer ---
terre_mer <- st_read(paste0(data_path, "Limite_terre_mer/Limite_terre-mer_facade_Manche_Atlantique_ligne.shp")) 
crs(terre_mer)
terre_mer <- st_transform(terre_mer, crs = 4326) 
terre_mer <- st_intersection(terre_mer, BOX_4326)

# bathymétrie 20m grain fin ---
bathy <- raster(paste0(data_path, "/Bathymétrie/MNT_PC20m_HOMONIM_WGS84_NM_ZNEG_V2.0.grd"))
crs(bathy) <- "+proj=longlat +datum=WGS84"
bathy_crop <- crop(bathy, BOX) # pour avoir un env juste dans la zone patate
bathy_zone <- mask(bathy_crop, BOX)

# nom de site ---
# Exemple de dataframe avec les coordonnées et les noms
labels_ZOOM <- data.frame(
  name = c("Ors", "Pointe d'Oulme", "Pointe des Doux", 
           "Arceau", "Les Palles", "Fort Vasoux", 
           "Ferme aquacole", "Montportail", "Travers",
           "Grand cimétière", "Petit Matton", "Ile de Nôle", 
           "Prise de l'Epée"),
  x = c(373400, 374200, 374000, 
        371145, 379600, 384500,
        380000, 384400, 384350,
        384000, 386000, 377300, 
        384000),
  y = c(6537900, 6539250, 6543200, 
        6546600, 6549700, 6548800, 
        6547350, 6545650, 6541650,
        6541000, 6537500, 6535500, 
        6532500)
)

labels_ZOOM$ZOOM <- c("A","A","A",
                    "A","B","B",
                    "B","B","C",
                    "C","E","D","E")

labels_ZOOM <- st_as_sf(labels_ZOOM, coords = c("x", "y"), crs = 2154)
labels_ZOOM_A <- labels_ZOOM[labels_ZOOM$ZOOM=="A",]
labels_ZOOM_B <- labels_ZOOM[labels_ZOOM$ZOOM=="B",]
labels_ZOOM_C <- labels_ZOOM[labels_ZOOM$ZOOM=="C",]
labels_ZOOM_D <- labels_ZOOM[labels_ZOOM$ZOOM=="D",]
labels_ZOOM_E <- labels_ZOOM[labels_ZOOM$ZOOM=="E",]

# plot zoom ---
tmap_mode("view")
zone_map <- tm_scalebar() +
  tm_basemap(c("OpenStreetMap", "Esri.WorldImagery", "CartoDB.Positron")) +
  tm_shape(terre_mer) +
  tm_lines(col = "#32B7FF", lwd = 0.5) + 
  tm_shape(BOX_2154) +
  tm_borders(col = "#575757") +
  tm_shape(ZOOM) +
  tm_polygons(fill = "#575757", fill_alpha = 0.1, col = "#575757", lty = "dotted", size = 3) +
  tm_labels("name", size = 1, col = "#575757", just = "center"); zone_map

tmap_save(zone_map, paste0(atlas_path,"zone_map.html"))

## GPS -------------------------------------------------------------------------

GPS <- st_read(file.path(data_generated_path, "GPS_clean.gpkg"))

# variables temporelles additionnelles
GPS$y_m_d <- ymd(as.Date(GPS$datetime))
GPS$month_numeric <- month(as.Date(GPS$datetime))
GPS$month_label <- as.character(lubridate::month(as.Date(GPS$datetime), label = TRUE, abbr = TRUE))
GPS$week <-  week(as.Date(GPS$datetime))

GPS_2154 <- st_transform(GPS, crs = 2154)

## Grid ------------------------------------------------------------------------

# INPN grille ---
grid <- st_read(paste0(data_path, "INPN_grid/METROP_L932X2.shp"))
# grid_crop <- st_crop(grid, BOX_2154)
# st_write(grid_crop, paste0(data_generated_path, "grid_crop.gpkg"), append = FALSE)
grid_crop <- st_read(paste0(data_generated_path, "grid_crop.gpkg"))

## 100x100 m ---
offset_point <- st_bbox(grid[grid$CD_SIG=="2kmL93E370N6528",])[c("xmin", "ymin")] ; offset_point
# grid_100x100 <- st_make_grid(BOX_2154, cellsize = 100, offset = offset_point)
# st_write(grid_100x100, paste0(data_generated_path, "grid_100x100.gpkg"), append = FALSE)
grid_100x100 <- st_read(paste0(data_generated_path, "grid_100x100.gpkg"))
raster_100x100 <- rast(grid_100x100, resolution = 100, crs="EPSG:2154")

offset_point <- st_bbox(grid[grid$CD_SIG=="2kmL93E370N6528",])[c("xmin", "ymin")] ; offset_point
# grid_10x10 <- st_make_grid(BOX_2154, cellsize = 10, offset = offset_point)
# st_write(grid_10x10, paste0(data_generated_path, "grid_10x10.gpkg"), append = FALSE)
grid_10x10 <- st_read(paste0(data_generated_path, "grid_10x10.gpkg"))
raster_10x10 <- rast(grid_10x10, resolution = 10, crs="EPSG:2154")

# tmap_mode("view")
# grid_map <- tm_scalebar() +
#   tm_shape(grid_100x100) +
#   tm_polygons(col = "red", fill_alpha = 0.3) +
#   tm_shape(grid_crop) +
#   tm_polygons(fill_alpha = 0.3, col = "green") +
#   tm_shape(BOX_2154) +
#   tm_borders(col = "yellow"); grid_map

## 10x10 m ---

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
# offset_point_ZOOM_E <- st_bbox(grid[grid$CD_SIG=="2kmL93E384N6530",])[c("xmin", "ymin")] - c(500*1,0)
# grid_ZOOM_E <- st_make_grid(ZOOM_E, cellsize = resolution_ZOOM, offset = offset_point_ZOOM_E)
# st_write(grid_ZOOM_E, paste0(data_generated_path, "grid_ZOOM_E.gpkg"), append = FALSE)
grid_ZOOM_E <- st_read(paste0(data_generated_path, "grid_ZOOM_E.gpkg"))
raster_ZOOM_E <- rast(grid_ZOOM_E, resolution = resolution_ZOOM, crs="EPSG:2154")

# tmap_mode("view")
# grid_map <- tm_scalebar() +
# tm_shape(grid_ZOOM_A) +
# tm_polygons(col = "red", fill_alpha = 0.3) +
# tm_shape(ZOOM_A) +
# tm_borders(col = "yellow") +
# tm_shape(grid_ZOOM_B) +
# tm_polygons(col = "red", fill_alpha = 0.3) +
# tm_shape(ZOOM_B) +
# tm_borders(col = "yellow") +
# tm_shape(grid_ZOOM_C) +
# tm_polygons(col = "red", fill_alpha = 0.3) +
# tm_shape(ZOOM_C) +
# tm_borders(col = "yellow") +
# tm_shape(grid_ZOOM_D) +
# tm_polygons(col = "red", fill_alpha = 0.3) +
# tm_shape(ZOOM_D) +
# tm_shape(grid_ZOOM_E) +
# tm_polygons(col = "pink", fill_alpha = 0.3) +
# tm_shape(ZOOM_E) +
# tm_borders(col = "yellow") +
# tm_shape(grid_crop) +
# tm_polygons(fill_alpha = 0.3, col = "green") ; grid_map

################## ---
# *Home Range       ------------------------------------------------------------
################## ---

## ## ## ## ## ## ## ## ## ---
## *estimation individuelle ----------------------------------------------------
## ## ## ## ## ## ## ## ## ---

coords_HR_ID <- GPS %>% 
  dplyr::select(ID,lon,lat) %>% 
  st_drop_geometry() %>% 
  na.omit()

locs_HR_ID <- st_as_sf(coords_HR_ID, coords = c("lon", "lat"), crs = 4326)
locs_HR_ID_32630 <- st_transform(locs_HR_ID, crs = 32630)

crs_utm <- "EPSG:32630"
SpatRaster <- project(raster_100x100, crs_utm)
RasterLayer <- raster(SpatRaster)
SpatialPixels <- as(RasterLayer, "SpatialPixels")

coords_HR_ID_32630 <- st_coordinates(locs_HR_ID_32630)

# Règle de Silverman
sigma_x_HR_ID <- sd(coords_HR_ID_32630[,1]) 
sigma_y_HR_ID <- sd(coords_HR_ID_32630[,2])
n_HR_ID <- nrow(coords_HR_ID_32630)

h_silverman_x_HR_ID <- 1.06 * sigma_x_HR_ID * n_HR_ID^(-1/5) / 2
h_silverman_y_HR_ID <- 1.06 * sigma_y_HR_ID * n_HR_ID^(-1/5) / 2

locs_spa_HR_ID <- as(locs_HR_ID_32630, "Spatial")

kud_HR_ID <- kernelUD(locs_spa_HR_ID["ID"], grid = SpatialPixels,
                      h = mean(c(h_silverman_x_HR_ID, h_silverman_y_HR_ID)))

kde_hr_95 <- getverticeshr(kud_HR_ID, 95)
kde_hr_50 <- getverticeshr(kud_HR_ID, 50)

kde_hr_95_sf <- st_as_sf(kde_hr_95)
kde_hr_50_sf <- st_as_sf(kde_hr_50)

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

UDMap_final_HR_ID <- do.call(rbind, UDmaps_list_HR_ID)

UDMap_final_HR_ID$ID <- as.factor(UDMap_final_HR_ID$ID)

# write & read
st_write(UDMap_final_HR_ID, paste0(data_generated_path, "UDMap_final_HR_ID.gpkg"), append = FALSE)
UDMap_final_HR_ID <- st_read(file.path(data_generated_path, "UDMap_final_HR_ID.gpkg"))

ID_list <- unique(UDMap_final_HR_ID$ID)
ID_gp_1 <- ID_list[1:23]
ID_gp_2 <- ID_list[24:46]

kde_hr_95_sf_gp1 <- kde_hr_95_sf %>%
  filter(id %in% ID_gp_1)
kde_hr_95_sf_gp2 <- kde_hr_95_sf %>%
  filter(id %in% ID_gp_2)

kde_hr_50_sf_gp1 <- kde_hr_50_sf %>%
  filter(id %in% ID_gp_1)
kde_hr_50_sf_gp2 <- kde_hr_50_sf %>%
  filter(id %in% ID_gp_2) 

# plot
tmap_mode("view")

UDMap_HR_ID_gp1 <- tm_scalebar() +
  tm_basemap(c("OpenStreetMap", "Esri.WorldImagery", "CartoDB.Positron")) +
  tm_shape(kde_hr_95_sf_gp1) +
  tm_lines(col = "id",
           palette = palette_grey) +
  tm_shape(kde_hr_50_sf_gp1) +
  tm_polygons(fill = "id",
              palette = palette_grey)  + 
  tm_shape(RMO) +
  tm_borders(col = "white", lwd = 3, lty = "dashed") +
  tm_shape(terre_mer) +
  tm_lines(col = "#32B7FF", lwd = 0.5) 

UDMap_HR_ID_gp2 <- tm_scalebar() +
  tm_basemap(c("OpenStreetMap", "Esri.WorldImagery", "CartoDB.Positron")) +
  tm_shape(kde_hr_95_sf_gp2) +
  tm_lines(col = "id",
           palette = palette_grey) +
  tm_shape(kde_hr_50_sf_gp2) +
  tm_polygons(fill = "id",
              palette = palette_grey) + 
  tm_shape(RMO) +
  tm_borders(col = "white", lwd = 3, lty = "dashed") +
  tm_shape(terre_mer) +
  tm_lines(col = "#32B7FF", lwd = 0.5)

UDMap_HR_ID <- tmap_arrange(UDMap_HR_ID_gp1, UDMap_HR_ID_gp2) ; UDMap_HR_ID

## ## ## ## ## ##  ---
## *surface moyenne ------------------------------------------------------------
## ## ## ## ## ##  ---

area_95_dt <- kde_hr_95_sf %>% 
  rename(area_95 = area) %>% 
  st_drop_geometry()

area_50_dt <- kde_hr_50_sf %>% 
  rename(area_50 = area) %>% 
  st_drop_geometry()

area_dt <- left_join(area_95_dt, area_50_dt)

# plot 
area_hr_plot <- ggplot() +
  geom_hline(yintercept = mean(area_dt$area_95), col = "black") + 
  geom_hline(yintercept = mean(area_dt$area_95) - sd(area_dt$area_95), col = "grey", lty = "dashed") +
  geom_hline(yintercept = mean(area_dt$area_95) + sd(area_dt$area_95), col = "grey", lty = "dashed") + 
  geom_point(data = area_dt, aes(reorder(id, area_95), area_95),
             size = 4, shape = 21, col = "white", fill = "black") +
  geom_point(data = area_dt, aes(reorder(id, area_95), area_50),
             size = 4, shape = 21, col = "white", fill = "grey") +
  paletteer::scale_fill_paletteer_c("grDevices::Grays") +
  theme_classic() +
  theme(legend.position = c(.1, .75)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  labs(title="",
       x ="Individu", y = "Aire du domaine vital à 95% (m²)", fill="Aire domaine 
vitale à 50%"); area_hr_plot

ggsave(paste0(atlas_path, "/area_hr_plot.png"), 
       plot = area_hr_plot, width = 10, height = 4, dpi = 1000)

## ## ## ## ## ## ## ## ## ## ## ---
## *pourcentage dans la réserve   ----------------------------------------------
## ## ## ## ## ## ## ## ## ## ## ---

### 95% ---

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
  mutate(coverage = as.numeric(intersect_area/county_area))

HR_95_pourc_RN <- tm_scalebar() +
  tm_basemap(c("OpenStreetMap", "Esri.WorldImagery", "CartoDB.Positron")) +
  tm_shape(kde_hr_95_sf_2154) +  
  tm_polygons(fill = "coverage", alpha = 0.5,
              palette = palette_grey) +
  tm_shape(RMO) +
  tm_borders(col = "white", lwd = 3, lty = "dashed") +
  tm_shape(terre_mer) +
  tm_lines(col = "#32B7FF", lwd = 0.5) +
  tm_credits(paste0("Pourcentage moyen des domaines vitaux à 95% dans la réserve naturelle : ", round(mean_hr_95_pourc_rn, 2)*100, "%")); HR_95_pourc_RN

### 50% ---

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
  mutate(coverage = as.numeric(intersect_area/county_area))

mean_hr_50_pourc_rn <- mean(kde_hr_50_sf_2154$coverage, na.rm = T)

print("pourcentage moyen des home range dans la réserve naturelle :")
mean_hr_50_pourc_rn

HR_50_pourc_RN <- tm_scalebar() +
  tm_basemap(c("OpenStreetMap", "Esri.WorldImagery", "CartoDB.Positron")) +
  tm_shape(kde_hr_50_sf_2154) +  
  tm_polygons(fill = "coverage", alpha = 0.5,
              palette = palette_grey) +
  tm_shape(RMO) +
  tm_borders(col = "white", lwd = 3, lty = "dashed") +
  tm_shape(terre_mer) +
  tm_lines(col = "#32B7FF", lwd = 0.5) +
  tm_credits(paste0("Pourcentage moyen des domaines vitaux à 50% dans la réserve naturelle : ", round(mean_hr_50_pourc_rn, 2)*100, "%")); HR_50_pourc_RN

HR_pourc_RN <- tmap_arrange(HR_95_pourc_RN, HR_50_pourc_RN) ; HR_pourc_RN

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
  geom_point(data = pourc_hr, 
             aes(x = reorder(id, area_95), y = area_50, fill = coverage_50),
             size = 4, shape = 24, col = "white") +
  scale_fill_gradient(name = "Couverture 50%",
                      low = "#E5E4E4", high = "#69B578") +
  new_scale_fill() +
  geom_point(data = pourc_hr, 
             aes(x = reorder(id, area_95), y = area_95, fill = coverage_50),
             size = 4, shape = 21, col = "white") +
  scale_fill_gradient(name = "Couverture 95%",
                      low = "#E5E4E4", high = "#F0A202") +
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
  labs(x = "Individu", 
       y = "Aire du domaine vital (m²)")

ggsave(paste0(atlas_path, "/area_hr_plot.png"), 
       plot = area_hr_plot, width = 10, height = 4, dpi = 1000)

########################## ---
# *Temps dans la réserve    ----------------------------------------------------
########################## ---

## # # # # # --- 
## *all point ------------------------------------------------------------------
## # # # # # ---

# temps global
all_elsewhere <- GPS_2154 %>% 
  dplyr::select(ID, datetime) %>%
  st_drop_geometry() %>% 
  distinct() %>% 
  group_by(ID) %>%
  distinct() %>% 
  summarize(all_elsewhere = n()) 

# temps dans la réserve
all_inRMO <- st_intersection(GPS_2154, RMO)

all_inRMO <- all_inRMO %>% 
  dplyr::select(ID, datetime) %>% 
  st_drop_geometry() %>% 
  distinct() %>% 
  group_by(ID) %>%
  distinct() %>% 
  summarize(all_inRMO = n()) 

# join dans la réserve et elsewhere
all_inRMO_elsewhere <- left_join(all_inRMO, all_elsewhere) %>% 
  mutate(pourc_inRMO_all = all_inRMO/all_elsewhere)

mean_pourc_tps_inRMO <- mean(all_inRMO_elsewhere$pourc_inRMO_all, na.rm = T)

print("Proportion du temps passé dans la réserve vs hors réserve:")
mean_pourc_tps_inRMO

## # # # # # --- 
## *reposoir -------------------------------------------------------------------
## # # # # # ---

# temps global
roosting_elsewhere <- GPS_2154 %>% 
  filter(behavior=="roosting") %>% 
  dplyr::select(ID, datetime) %>%
  st_drop_geometry() %>% 
  distinct() %>% 
  group_by(ID) %>%
  distinct() %>% 
  summarize(roosting_elsewhere = n())

# temps dans la réserve
GPS_2154_roosting <- GPS_2154 %>% 
  filter(behavior=="roosting")

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
  mutate(pourc_roosting_inRMO = roosting_inRMO/roosting_elsewhere)

mean_pourc_roosting_inRMO <- mean(roosting_inRMO_elsewhere$pourc_roosting_inRMO, na.rm = T)

print("Proportion du temps passé dans la réserve vs hors réserve pour le roosting:")
mean_pourc_roosting_inRMO

## # # # # # --- 
## *alimentation ---------------------------------------------------------------
## # # # # # ---

# temps global
foraging_everywher <- GPS_2154 %>% 
  filter(behavior=="foraging") %>% 
  dplyr::select(ID, datetime) %>% 
  st_drop_geometry() %>% 
  distinct() %>% 
  group_by(ID) %>%
  distinct() %>% 
  summarize(foraging_elsewhere = n()) 

# temps dans la réserve
GPS_2154_foraging <- GPS_2154 %>% 
  filter(behavior=="foraging")

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
  mutate(pourc_foraging_inRMO = foraging_inRMO/foraging_elsewhere)

mean_pourc_inRMO_foraging <- mean(foraging_inRMO_elsewhere$pourc_foraging_inRMO, na.rm = T)

print("Proportion du temps passé dans la réserve vs hors réserve pour le foraging:")
mean_pourc_inRMO_foraging

## # # # # # --- 
## *other ----------------------------------------------------------------------
## # # # # # ---

# temps global
other_elsewhere <- GPS_2154 %>% 
  filter(behavior=="other") %>% 
  dplyr::select(ID, datetime) %>% 
  st_drop_geometry() %>% 
  distinct() %>% 
  group_by(ID) %>%
  distinct() %>% 
  summarize(other_elsewhere = n()) 

# temps dans la réserve
GPS_2154_other <- GPS_2154 %>% 
  filter(behavior=="other")

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
  mutate(pourc_inRMO_other = other_inRMO/other_elsewhere)

mean_pourc_other_inRMO <- mean(other_inRMO_elsewhere$pourc_inRMO_other, na.rm = T)

print("Proportion du temps passé dans la réserve vs hors réserve pour le other:")
mean_pourc_other_inRMO

# dans une seul tableau

all_duree_1 <- left_join(other_inRMO_elsewhere, 
                         foraging_inRMO_elsewhere)

all_duree_2 <- left_join(all_duree_1, 
                         roosting_inRMO_elsewhere)

all_duree_3 <- left_join(all_duree_2, 
                         all_inRMO_elsewhere)

all_duree_3$sum_inRMO <- all_duree_3$other_inRMO + all_duree_3$foraging_inRMO + all_duree_3$roosting_inRMO
all_duree_3$sum_elsewhere <- all_duree_3$other_elsewhere + all_duree_3$foraging_elsewhere + all_duree_3$roosting_elsewhere

## *table -----

all_duree_dans_reserve <- all_duree_3 %>% 
  dplyr::select(ID, pourc_foraging_inRMO, pourc_roosting_inRMO, pourc_inRMO_other, pourc_inRMO_all) %>% 
  dplyr::rename(foraging = pourc_foraging_inRMO, 
                roosting = pourc_roosting_inRMO, 
                autre = pourc_inRMO_other, 
                total = pourc_inRMO_all)

all_duree_dans_reserve_long <- reshape2::melt(all_duree_dans_reserve, id = 'ID')

write.table(all_duree_dans_reserve_long, file = paste0(data_generated_path, "all_duree_dans_reserve_long.csv"), 
            sep = ",", col.names = NA, qmethod = "double")

## *plot ----

duree_dans_reserve_plot <- ggplot(all_duree_dans_reserve_long, 
                                  aes(x = reorder(ID, value), y = value, fill = variable)) + 
  geom_hline(yintercept=mean_pourc_other_inRMO, linetype="longdash", color = "grey") +
  geom_hline(yintercept=mean_pourc_roosting_inRMO, linetype="longdash", color = "#CF63A6FF") +
  geom_hline(yintercept=mean_pourc_inRMO_foraging, linetype="longdash", color = "#0095AFFF") +
  geom_hline(yintercept=mean_pourc_tps_inRMO, linetype="longdash", color = "black") +
  geom_jitter(shape = 21, size = 4, color = "white", alpha = 0.5) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  theme(legend.position = c(0.85,0.30)) +
  scale_fill_manual(values=c("#0095AFFF", "#CF63A6FF", "grey", "black")) +
  labs(title="",
       x ="Individu", y = "Pourcentage de temps passé dans la réserve", fill="") ; duree_dans_reserve_plot

ggsave(paste0(atlas_path, "/duree_dans_reserve_plot.png"), 
       plot = duree_dans_reserve_plot, width = 10, height = 4, dpi = 300)

################## ---
# *Zone de reposoir ------------------------------------------------------------
################## ---

## *zoom  ----------------------------------------------------------------------

zoom_level <- c("A", "B", "C", "D", "E")
results_kud = NULL
nb_kud = NULL
analyse <- "roosting"
comportement <- "roosting"
# estimer les kernelUD
kud_map.roosting <- Map(estimate_kud, zoom_level, analyse, comportement)
results_kud.roosting <- do.call(rbind, kud_map.roosting)
st_write(results_kud.roosting, paste0(data_generated_path, "results_kud_", analyse, ".gpkg"), append = FALSE)
results_kud.roosting <- st_read(file.path(data_generated_path, paste0("results_kud_", analyse,".gpkg")))
# compter les nb ind par zoom
nb_kud_map.roosting <- Map(count_nb_kud, zoom_level, comportement)
nb.roosting <- do.call(rbind, nb_kud_map.roosting)
write.csv(nb.roosting, paste0(data_generated_path, "nb.", analyse, ".csv"), row.names = FALSE)
nb.roosting <- read.csv(paste0(data_generated_path, paste0("nb.", analyse, ".csv")), row.names = NULL)
# Générer les maps pour chaque zoom
couleur = nom_pal_roosting
maps_list.roosting <- Map(create_map, zoom_level, analyse, couleur)

## *hotspot --------------------------------------------------------------------

coords_roosting_ID_hotspot <- GPS %>% 
  filter(behavior == "roosting") %>% 
  dplyr::select(ID,lon,lat) %>%
  st_drop_geometry() %>% 
  na.omit()

locs_roosting_ID_hotspot <- st_as_sf(coords_roosting_ID_hotspot, coords = c("lon", "lat"), crs = 4326)
locs_roosting_ID_hotspot_32630 <- st_transform(locs_roosting_ID_hotspot, crs = 32630)

crs_utm <- "EPSG:32630"
SpatRaster <- project(raster_100x100, crs_utm)
RasterLayer <- raster(SpatRaster)
SpatialPixels <- as(RasterLayer, "SpatialPixels")

coords_roosting_ID_hotspot_32630 <- st_coordinates(locs_roosting_ID_hotspot_32630)

# Règle de Silverman
sigma_x_roosting_ID_hotspot <- sd(coords_roosting_ID_hotspot_32630[,1]) 
sigma_y_roosting_ID_hotspot <- sd(coords_roosting_ID_hotspot_32630[,2])
n_roosting_ID_hotspot <- nrow(coords_roosting_ID_hotspot_32630)

h_silverman_x_roosting_ID_hotspot <- 1.06 * sigma_x_roosting_ID_hotspot * n_roosting_ID_hotspot^(-1/5) / 2
h_silverman_y_roosting_ID_hotspot <- 1.06 * sigma_y_roosting_ID_hotspot * n_roosting_ID_hotspot^(-1/5) / 2

locs_spa_roosting_ID_hotspot <- as(locs_roosting_ID_hotspot_32630, "Spatial")

kud_roosting_ID_hotspot <- kernelUD(locs_spa_roosting_ID_hotspot["ID"], grid = SpatialPixels,
                      h = mean(c(h_silverman_x_roosting_ID_hotspot, h_silverman_y_roosting_ID_hotspot)))

kde_roosting_95 <- getverticeshr(kud_roosting_ID_hotspot, 95)
kde_roosting_50 <- getverticeshr(kud_roosting_ID_hotspot, 50)

kde_roosting_95_sf <- st_as_sf(kde_roosting_95)
kde_roosting_50_sf <- st_as_sf(kde_roosting_50)

UDmaps_list_roosting_ID_hotspot <- lapply(names(kud_roosting_ID_hotspot), function(ID) {
  
  print(ID)
  
  # Extraire l'estimation de densité pour un ID spécifique
  kud_single_roosting_ID_hotspot <- kud_roosting_ID_hotspot[[ID]]
  rast_roosting_ID_hotspot <- rast(kud_single_roosting_ID_hotspot)
  contour_roosting_ID_hotspot <- as.contour(rast_roosting_ID_hotspot)
  sf_roosting_ID_hotspot <- st_as_sf(contour_roosting_ID_hotspot)
  cast_roosting_ID_hotspot <- st_cast(sf_roosting_ID_hotspot, "POLYGON")
  cast_roosting_ID_hotspot$ID <- ID
  
  return(cast_roosting_ID_hotspot)
  
})

UDMap_final_roosting_ID_hotspot <- do.call(rbind, UDmaps_list_roosting_ID_hotspot)

UDMap_final_roosting_ID_hotspot$ID <- as.factor(UDMap_final_roosting_ID_hotspot$ID)

# write & read
st_write(UDMap_final_roosting_ID_hotspot, paste0(data_generated_path, "UDMap_final_roosting_ID_hotspot.gpkg"), append = FALSE)
UDMap_final_roosting_ID_hotspot <- st_read(file.path(data_generated_path, "UDMap_final_roosting_ID_hotspot.gpkg"))

# Calculer l'aire de chaque polygone
polygons_roosting_ID_hotspot <- UDMap_final_roosting_ID_hotspot %>%
  mutate(area = st_area(geom))

# Garder le plus grand polygone pour chaque 'ind'
polygons_largest_roosting_ID_hotspot <- polygons_roosting_ID_hotspot %>%
  group_by(ID) %>%
  slice_max(order_by = area, n = 1, with_ties = FALSE) %>%
  ungroup()

# Ajouter un ID unique
polygons_largest_roosting_ID_hotspot <- polygons_largest_roosting_ID_hotspot %>%
  mutate(id = row_number())

polygons_largest_roosting_ID_hotspot <- st_make_valid(polygons_largest_roosting_ID_hotspot)

# Faire toutes les intersections
intersections_roosting_ID_hotspot <- st_intersection(polygons_largest_roosting_ID_hotspot)

# La colonne 'id' contiendra une liste des identifiants des polygones qui se superposent
# On compte combien d'IDs sont impliqués dans chaque géométrie
intersections_roosting_ID_hotspot <- intersections_roosting_ID_hotspot %>%
  mutate(n = lengths(st_geometry(intersections_roosting_ID_hotspot)))

# Filtrer pour garder seulement les zones avec 3 superpositions ou plus
zones_superposees_roosting_ID_hotspot <- intersections_roosting_ID_hotspot

# Buffer de 10 mètres pour relier les zones proches
zones_buffered_roosting_ID_hotspot <- st_buffer(zones_superposees_roosting_ID_hotspot, dist = 10)

# Fusionner les géométries avec st_union (résultat = sfc multipolygon)
zones_union_roosting_ID_hotspot <- st_union(zones_buffered_roosting_ID_hotspot)

# Revenir à des polygones séparés
zones_polygons_roosting_ID_hotspot <- st_cast(zones_union_roosting_ID_hotspot, "POLYGON")

# Créer un sf à partir du résultat
zones_grouped_roosting_ID_hotspot <- st_as_sf(zones_polygons_roosting_ID_hotspot)

# Donner un identifiant à chaque zone fusionnée
zones_grouped_roosting_ID_hotspot <- zones_grouped_roosting_ID_hotspot %>%
  mutate(group_id = row_number())

# Associer les polygones sources (zones_superposees) aux zones fusionnées
join_roosting_ID_hotspot <- st_join(zones_superposees_roosting_ID_hotspot, zones_grouped_roosting_ID_hotspot, join = st_intersects)

# Regrouper par groupe fusionné et agréger le total des superpositions
zone_stats_roosting_ID_hotspot <- join_roosting_ID_hotspot %>%
  group_by(group_id) %>%
  summarise(total_superposed = sum(n), .groups = "drop")

zones_grouped_roosting_ID_hotspot <- left_join(
  zones_grouped_roosting_ID_hotspot,
  st_drop_geometry(zone_stats_roosting_ID_hotspot),  # enlève la géométrie pour éviter le conflit
  by = "group_id"
)

# Rejoindre les zones superposées avec leurs IDs d'origine
zones_superposees_roosting_ID_hotspot <- st_intersection(
  polygons_largest_roosting_ID_hotspot %>% dplyr::select(ID),
  zones_superposees_roosting_ID_hotspot
)

# Associer chaque petite zone superposée avec sa zone fusionnée
join_roosting_ID_hotspot <- st_join(zones_superposees_roosting_ID_hotspot, zones_grouped_roosting_ID_hotspot, join = st_intersects)

# Regrouper par group_id, et compter les ID uniques
zone_id_stats_roosting_ID_hotspot <- join_roosting_ID_hotspot %>%
  st_drop_geometry() %>%
  group_by(group_id) %>%
  summarise(n_ID = n_distinct(ID), .groups = "drop")

zones_grouped_roosting_ID_hotspot <- zones_grouped_roosting_ID_hotspot %>%
  left_join(zone_id_stats_roosting_ID_hotspot, by = "group_id")

hotspot_roosting_ID_hotspot <- zones_grouped_roosting_ID_hotspot %>% 
  filter(n_ID >= 1)

hotspot_roosting_ID_hotspot$n_ID <- as.factor(hotspot_roosting_ID_hotspot$n_ID)

# write & read
st_write(hotspot_roosting_ID_hotspot, paste0(data_generated_path, "hotspot_roosting_ID_hotspot.gpkg"), append = FALSE)
hotspot_roosting_ID_hotspot <- st_read(file.path(data_generated_path, "hotspot_roosting_ID_hotspot.gpkg"))

# plot
tmap_mode("view")
UDMap_roosting_hotspot <- tm_scalebar() +
  tm_basemap(c("OpenStreetMap", "Esri.WorldImagery", "CartoDB.Positron")) +
  tm_shape(hotspot_roosting_ID_hotspot) +
  tm_polygons(border.col = "white", fill = "n_ID", fill_alpha = 0.8,
              palette = c("1" = "gray95", "2" = "gray85", 
                          "3" = "#F3E79AFF", "4" = "#F9B881FF", "5" = "#F28891FF", 
                          "6" = "#CF63A6FF", "27" = "#9650A6FF")) + # " palette_roosting"
  tm_shape(RMO) +
  tm_borders(col = "white", lwd = 3, lty = "dashed") +
  tm_shape(terre_mer) +
  tm_lines(col = "lightblue", lwd = 0.1); UDMap_roosting_hotspot

tmap_save(UDMap_roosting_hotspot, paste0(atlas_path,"UDMap_roosting_hotspot_from1id.html"))

## *tides_high_type ------------------------------------------------------------------------

# estimation 
crs_utm <- "EPSG:32630"
ZOOM <- c("A","B","C","D","E")
results_kud.roosting_ZOOM_tides_high_type = NULL
nb_kud.roosting_ZOOM_tides_high_type = NULL

for (lettre in ZOOM){
  
  # in ZOOM
  ZOOM <- st_read(paste0(data_generated_path,"ZOOM_",lettre,".gpkg"))
  ZOOM <- st_transform(ZOOM, crs = 4326)
  GPS.ZOOM <- st_intersection(GPS, ZOOM) 
  
  # nb ind & point 
  nb_ind_point_dt <- GPS.ZOOM %>% 
    filter(behavior == "roosting") %>%
    group_by(ID) %>% 
    dplyr::select(ID, datetime) %>% 
    st_drop_geometry() %>% 
    na.omit() %>% 
    summarise(n = n()) %>% 
    mutate(zoom = lettre)
  
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
  
    # nb ind & point
  nb_kud.roosting_ZOOM_tides_high_type <- rbind(nb_kud.roosting_ZOOM_tides_high_type, nb_ind_point_dt)
  
}

# write & read
st_write(results_kud.roosting_ZOOM_tides_high_type, paste0(data_generated_path, "results_kud.roosting_ZOOM_tides_high_type.gpkg"), append = FALSE)
write.csv(nb_kud.roosting_ZOOM_tides_high_type, paste0(data_generated_path, "nb_kud.roosting_ZOOM_tides_high_type.csv"), row.names = FALSE)
results_kud.roosting_ZOOM_tides_high_type <- st_read(file.path(data_generated_path, "results_kud.roosting_ZOOM_tides_high_type.gpkg"))
nb_kud.roosting_ZOOM_tides_high_type <- read.csv(paste0(data_generated_path, "nb_kud.roosting_ZOOM_tides_high_type.csv"), row.names = NULL)

# Générer les maps pour chaque zoom
analyse <- "roosting_ZOOM_tides_high_type"
param = "tides_high_type"
couleur = nom_pal_roosting
maps_list.roosting_ZOOM_tides_high_type <- Map(create_map_param,
                                               zoom_level, analyse, param, couleur)

##################### ---
# *Zone d'alimentation ---------------------------------------------------------
##################### ---

## *zoom -----------------------------------------------------------------------

zoom_level <- c("A", "B", "C", "D", "E")
results_kud = NULL
nb_kud = NULL
analyse <- "foraging"
comportement <- "foraging"
# estimer les kernelUD
kud_map.foraging <- Map(estimate_kud, zoom_level, analyse, comportement)
results_kud.foraging <- do.call(rbind, kud_map.foraging)
st_write(results_kud.foraging, paste0(data_generated_path, "results_kud_", analyse, ".gpkg"), append = FALSE)
results_kud.foraging <- st_read(file.path(data_generated_path, paste0("results_kud_", analyse,".gpkg")))
# compter les nb ind par zoom
nb_kud_map.foraging <- Map(count_nb_kud, zoom_level, comportement)
nb.foraging <- do.call(rbind, nb_kud_map.foraging)
write.csv(nb.foraging, paste0(data_generated_path, "nb.", analyse, ".csv"), row.names = FALSE)
nb.foraging <- read.csv(paste0(data_generated_path, paste0("nb.", analyse, ".csv")), row.names = NULL)
# Générer les maps pour chaque zoom
couleur = nom_pal_foraging
maps_list.foraging <- Map(create_map, zoom_level, analyse, couleur)

## ## ## ## ## ## ## ## ## ---
## *hotspot --------------------------------------------------------------------
## ## ## ## ## ## ## ## ## ---

coords_foraging_ID_hotspot <- GPS %>% 
  filter(behavior == "foraging") %>% 
  dplyr::select(ID,lon,lat) %>% 
  st_drop_geometry() %>% 
  na.omit()

locs_foraging_ID_hotspot <- st_as_sf(coords_foraging_ID_hotspot, coords = c("lon", "lat"), crs = 4326)
locs_foraging_ID_hotspot_32630 <- st_transform(locs_foraging_ID_hotspot, crs = 32630)

crs_utm <- "EPSG:32630"
SpatRaster <- project(raster_100x100, crs_utm)
RasterLayer <- raster(SpatRaster)
SpatialPixels <- as(RasterLayer, "SpatialPixels")

coords_foraging_ID_hotspot_32630 <- st_coordinates(locs_foraging_ID_hotspot_32630)

# Règle de Silverman
sigma_x_foraging_ID_hotspot <- sd(coords_foraging_ID_hotspot_32630[,1]) 
sigma_y_foraging_ID_hotspot <- sd(coords_foraging_ID_hotspot_32630[,2])
n_foraging_ID_hotspot <- nrow(coords_foraging_ID_hotspot_32630)

h_silverman_x_foraging_ID_hotspot <- 1.06 * sigma_x_foraging_ID_hotspot * n_foraging_ID_hotspot^(-1/5) / 2
h_silverman_y_foraging_ID_hotspot <- 1.06 * sigma_y_foraging_ID_hotspot * n_foraging_ID_hotspot^(-1/5) / 2

locs_spa_foraging_ID_hotspot <- as(locs_foraging_ID_hotspot_32630, "Spatial")

kud_foraging_ID_hotspot <- kernelUD(locs_spa_foraging_ID_hotspot["ID"], grid = SpatialPixels,
                                    h = mean(c(h_silverman_x_foraging_ID_hotspot, h_silverman_y_foraging_ID_hotspot)))

kde_foraging_95 <- getverticeshr(kud_foraging_ID_hotspot, 95)
kde_foraging_50 <- getverticeshr(kud_foraging_ID_hotspot, 50)

kde_foraging_95_sf <- st_as_sf(kde_foraging_95)
kde_foraging_50_sf <- st_as_sf(kde_foraging_50)

UDmaps_list_foraging_ID_hotspot <- lapply(names(kud_foraging_ID_hotspot), function(ID) {
  
  print(ID)
  
  # Extraire l'estimation de densité pour un ID spécifique
  kud_single_foraging_ID_hotspot <- kud_foraging_ID_hotspot[[ID]]
  rast_foraging_ID_hotspot <- rast(kud_single_foraging_ID_hotspot)
  contour_foraging_ID_hotspot <- as.contour(rast_foraging_ID_hotspot)
  sf_foraging_ID_hotspot <- st_as_sf(contour_foraging_ID_hotspot)
  cast_foraging_ID_hotspot <- st_cast(sf_foraging_ID_hotspot, "POLYGON")
  cast_foraging_ID_hotspot$ID <- ID
  
  return(cast_foraging_ID_hotspot)
  
})

UDMap_final_foraging_ID_hotspot <- do.call(rbind, UDmaps_list_foraging_ID_hotspot)

UDMap_final_foraging_ID_hotspot$ID <- as.factor(UDMap_final_foraging_ID_hotspot$ID)

# write & read
st_write(UDMap_final_foraging_ID_hotspot, paste0(data_generated_path, "UDMap_final_foraging_ID_hotspot.gpkg"), append = FALSE)
UDMap_final_foraging_ID_hotspot <- st_read(file.path(data_generated_path, "UDMap_final_foraging_ID_hotspot.gpkg"))

# Calculer l'aire de chaque polygone
polygons_foraging_ID_hotspot <- UDMap_final_foraging_ID_hotspot %>%
  mutate(area = st_area(geom))

# Garder le plus grand polygone pour chaque 'ind'
polygons_largest_foraging_ID_hotspot <- polygons_foraging_ID_hotspot %>%
  group_by(ID) %>%
  slice_max(order_by = area, n = 1, with_ties = FALSE) %>%
  ungroup()

# Ajouter un ID unique
polygons_largest_foraging_ID_hotspot <- polygons_largest_foraging_ID_hotspot %>%
  mutate(id = row_number())

polygons_largest_foraging_ID_hotspot <- st_make_valid(polygons_largest_foraging_ID_hotspot)

# Faire toutes les intersections
intersections_foraging_ID_hotspot <- st_intersection(polygons_largest_foraging_ID_hotspot)

# La colonne 'id' contiendra une liste des identifiants des polygones qui se superposent
# On compte combien d'IDs sont impliqués dans chaque géométrie
intersections_foraging_ID_hotspot <- intersections_foraging_ID_hotspot %>%
  mutate(n = lengths(st_geometry(intersections_foraging_ID_hotspot)))

# Filtrer pour garder seulement les zones avec 3 superpositions ou plus
zones_superposees_foraging_ID_hotspot <- intersections_foraging_ID_hotspot

# Buffer de 10 mètres pour relier les zones proches
zones_buffered_foraging_ID_hotspot <- st_buffer(zones_superposees_foraging_ID_hotspot, dist = 100)

# Fusionner les géométries avec st_union (résultat = sfc multipolygon)
zones_union_foraging_ID_hotspot <- st_union(zones_buffered_foraging_ID_hotspot)

# Revenir à des polygones séparés
zones_polygons_foraging_ID_hotspot <- st_cast(zones_union_foraging_ID_hotspot, "POLYGON")

# Créer un sf à partir du résultat
zones_grouped_foraging_ID_hotspot <- st_as_sf(zones_polygons_foraging_ID_hotspot)

# Donner un identifiant à chaque zone fusionnée
zones_grouped_foraging_ID_hotspot <- zones_grouped_foraging_ID_hotspot %>%
  mutate(group_id = row_number())

# Associer les polygones sources (zones_superposees) aux zones fusionnées
join_foraging_ID_hotspot <- st_join(zones_superposees_foraging_ID_hotspot, zones_grouped_foraging_ID_hotspot, join = st_intersects)

# Regrouper par groupe fusionné et agréger le total des superpositions
zone_stats_foraging_ID_hotspot <- join_foraging_ID_hotspot %>%
  group_by(group_id) %>%
  summarise(total_superposed = sum(n), .groups = "drop")

zones_grouped_foraging_ID_hotspot <- left_join(
  zones_grouped_foraging_ID_hotspot,
  st_drop_geometry(zone_stats_foraging_ID_hotspot),  # enlève la géométrie pour éviter le conflit
  by = "group_id"
)

# Rejoindre les zones superposées avec leurs IDs d'origine
zones_superposees_foraging_ID_hotspot <- st_intersection(
  polygons_largest_foraging_ID_hotspot %>% dplyr::select(ID),
  zones_superposees_foraging_ID_hotspot
)

# Associer chaque petite zone superposée avec sa zone fusionnée
join_foraging_ID_hotspot <- st_join(zones_superposees_foraging_ID_hotspot, zones_grouped_foraging_ID_hotspot, join = st_intersects)

# Regrouper par group_id, et compter les ID uniques
zone_id_stats_foraging_ID_hotspot <- join_foraging_ID_hotspot %>%
  st_drop_geometry() %>%
  group_by(group_id) %>%
  summarise(n_ID = n_distinct(ID), .groups = "drop")

zones_grouped_foraging_ID_hotspot <- zones_grouped_foraging_ID_hotspot %>%
  left_join(zone_id_stats_foraging_ID_hotspot, by = "group_id")

hotspot_foraging_ID_hotspot <- zones_grouped_foraging_ID_hotspot %>% 
  filter(n_ID >=3)

hotspot_foraging_ID_hotspot$n_ID <- as.factor(hotspot_foraging_ID_hotspot$n_ID)

# write & read
st_write(hotspot_foraging_ID_hotspot, paste0(data_generated_path, "hotspot_foraging_ID_hotspot.gpkg"), append = FALSE)
hotspot_foraging_ID_hotspot <- st_read(file.path(data_generated_path, "hotspot_foraging_ID_hotspot.gpkg"))

# plot
tmap_mode("view")
UDMap_foraging_hotspot <- tm_scalebar() +
  tm_basemap(c("OpenStreetMap", "Esri.WorldImagery", "CartoDB.Positron"))  + 
  tm_shape(hotspot_foraging_ID_hotspot) +
  tm_polygons(border.col = "grey", fill = "n_ID", fill_alpha = 1,
              palette = c("#26185FFF", "#0095AFFF", "#59C8B2FF")) + # " palette_foraging"
  tm_shape(RMO) +
  tm_borders(col = "white", lwd = 3, lty = "dashed") +
  tm_shape(terre_mer) +
  tm_lines(col = "lightblue", lwd = 0.1); UDMap_foraging_hotspot

tmap_save(UDMap_foraging_hotspot, paste0(atlas_path,"UDMap_foraging_hotspot.html"))

################################## ---
# *Distance reposoir - alimentation ---------------------------------------------
################################## ---

# distance de jour en jour

distance_dt_1 <- GPS %>% 
  dplyr::select(ID, behavior, datetime) %>% 
  filter(behavior !="other") %>% 
  distinct() %>% 
  na.omit()

distance_dt_2 <- distance_dt_1 %>%
  arrange(ID, datetime) %>%
  group_by(ID) %>%
  mutate(
    time_diff = as.numeric(difftime(datetime, lag(datetime), units = "mins")),
    new_group = if_else(is.na(time_diff) | time_diff > 60*6, 1, 0),
    group_id = cumsum(new_group)
  ) %>%
  ungroup() %>% 
  na.omit()

distance_dt_3 <- distance_dt_2 %>% 
  group_by(ID, group_id) %>% 
  mutate(centroid = st_centroid(st_union(geom))) %>% 
  dplyr::select(ID, behavior, group_id, datetime, centroid) %>% 
  st_drop_geometry()

centroid_sf <- distance_dt_3 %>%
  st_as_sf(crs = 4326) %>%  # si centroid est en texte WKT, sinon adapter
  arrange(ID, datetime)

centroid_sf <- st_as_sf(distance_dt_3)

# Ajouter les lignes suivantes dans un mutate par groupe ID
paired_centroids <- centroid_sf %>%
  group_by(ID) %>%
  arrange(datetime) %>%
  mutate(
    behavior_next = lead(behavior),
    datetime_next = lead(datetime),
    geom_next = lead(centroid)
  ) %>%
  filter(
    !is.na(datetime_next),
    abs(difftime(datetime_next, datetime, units = "hours")) <= 12,
    behavior != behavior_next
  ) %>%
  mutate(
    distance_m = st_distance(centroid, geom_next, by_element = TRUE)
  ) %>%
  ungroup()

paired_centroids$distance_m <- as.numeric(paired_centroids$distance_m)

paired_centroids_mean_dt <- paired_centroids %>% 
  st_drop_geometry() %>% 
  filter(distance_m > 0) %>% 
  group_by(ID) %>% 
  summarise(mean_dist = mean(distance_m),
            sd_dist = sd(distance_m))

mean_dist <- mean(paired_centroids_mean_dt$mean_dist)
sd_dist <- sd(paired_centroids_mean_dt$mean_dist)

###  #   #   #   #  --- 
### ~ sexe    -----------
###  #   #   #   #  --- 

sexe_dt <- GPS %>% 
  st_drop_geometry() %>% 
  dplyr::select(ID, sex) %>% 
  na.omit() %>% 
  distinct()

paired_centroids_sex_dt <- paired_centroids %>% 
  left_join(sexe_dt) %>% 
  na.omit()

paired_centroids_sex_dt_2 <- paired_centroids_sex_dt %>% 
  st_drop_geometry() %>% 
  dplyr::select(ID, distance_m, sex) %>% 
  filter(distance_m > 0) %>% 
  distinct()

# test comparaison de moyenne

shapiro.test(paired_centroids_sex_dt_2$distance_m[paired_centroids_sex_dt_2$sex == "F"]) 
shapiro.test(paired_centroids_sex_dt_2$distance_m[paired_centroids_sex_dt_2$sex == "M"])
var.test(paired_centroids_sex_dt_2$distance_m[paired_centroids_sex_dt_2$sex == "F"], 
         paired_centroids_sex_dt_2$distance_m[paired_centroids_sex_dt_2$sex == "M"])  

comp_moy_sexe = t.test(paired_centroids_sex_dt_2$distance_m[paired_centroids_sex_dt_2$sex == "F"], 
                       paired_centroids_sex_dt_2$distance_m[paired_centroids_sex_dt_2$sex == "M"], 
                       var.equal=F) ; comp_moy_sexe

summary(lm(paired_centroids_sex_dt_2$distance_m ~ paired_centroids_sex_dt_2$sex))

###  #   #   #   #  --- 
### ~ age    -----------
###  #   #   #   #  --- 

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

shapiro.test(paired_centroids_age_dt_2$distance_m[paired_centroids_age_dt_2$age == "adult"]) 
shapiro.test(paired_centroids_age_dt_2$distance_m[paired_centroids_age_dt_2$age == "juv"])
var.test(paired_centroids_age_dt_2$distance_m[paired_centroids_age_dt_2$age == "adult"], 
         paired_centroids_age_dt_2$distance_m[paired_centroids_age_dt_2$age == "juv"])  

comp_moy_age = t.test(paired_centroids_age_dt_2$distance_m[paired_centroids_age_dt_2$age == "adult"], 
                       paired_centroids_age_dt_2$distance_m[paired_centroids_age_dt_2$age == "juv"], 
                       var.equal=F) ; comp_moy_age

summary(lm(paired_centroids_age_dt_2$distance_m ~ paired_centroids_age_dt_2$age))

###  #   #   #   #  --- 
### ~ sex + age    -----------
###  #   #   #   #  --- 

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

summary(lm(paired_centroids_age_sex_dt_2$distance_m ~ paired_centroids_age_sex_dt_2$age*paired_centroids_age_sex_dt_2$sex))
summary(lm(paired_centroids_age_sex_dt_2$distance_m ~ paired_centroids_age_sex_dt_2$age + paired_centroids_age_sex_dt_2$sex))

###  #   #   #   #  --- 
### ~ chasse    -------
###  #   #   #   #  --- 

GPS_dist_chasse <- GPS %>% 
  mutate(
    Saison = case_when(month(datetime) == 1 ~ paste0(year(datetime)-1,"/",year(datetime)),
                       month(datetime) != 1 ~ paste0(year(datetime),"/",year(datetime)+1)))

GPS_dist_chasse$Saison <- as.character(GPS_dist_chasse$Saison)

chasse_date <- read_excel("D:/Projets_Suzanne/Courlis/3) Data/1) data/Chasse/date ouverture fermeture chasse.xlsx")

GPS_dist_chasse <- GPS_dist_chasse %>% 
  left_join(chasse_date)

GPS_dist_chasse <- GPS_dist_chasse %>% 
  mutate(in_out_saison = case_when(!between(y_m_d, `Ouverture DPM St Froult`, `Fermeture DPM St Froult`) ~ "out",
                                   between(y_m_d, `Ouverture DPM St Froult`, `Fermeture DPM St Froult`) ~ "in")) %>% 
  filter(month_numeric %in% c(7,8,9,10,11,12,1))

table(GPS_dist_chasse$in_out_saison)
table(GPS_dist_chasse$month_numeric)
table(GPS_dist_chasse$month_numeric[GPS_dist_chasse$in_out_saison=="in"])
table(GPS_dist_chasse$month_numeric[GPS_dist_chasse$in_out_saison=="out"])

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
var.test(paired_centroids_chasse_dt_2$distance_m[paired_centroids_chasse_dt_2$in_out_saison == "in"], 
         paired_centroids_chasse_dt_2$distance_m[paired_centroids_chasse_dt_2$in_out_saison == "out"])  

comp_moy_chasse = t.test(paired_centroids_chasse_dt_2$distance_m[paired_centroids_chasse_dt_2$in_out_saison == "in"], 
                       paired_centroids_chasse_dt_2$distance_m[paired_centroids_chasse_dt_2$in_out_saison == "out"], 
                       var.equal=F) ; comp_moy_chasse

summary(lm(paired_centroids_chasse_dt_2$distance_m ~ paired_centroids_chasse_dt_2$in_out_saison))

### plot ----

my_comparisons <- list( c("F", "M"))

distance_roost_forag_sex_plot <- ggplot(paired_centroids_age_sex_dt_2, 
                                    aes(x = sex, y = distance_m)) + 
  geom_boxplot(col = "black", outlier.colour = "black", outlier.shape = 1, fill = "grey") +
  geom_jitter(shape = 21, size = 0.5, color = "white", alpha = 0.5, fill = "black", width = 0.3) +
  stat_summary(fun.ymin = function(x) mean(x) - sd(x),
               fun.ymax = function(x) mean(x) + sd(x), geom="linerange", size=1, color="black") + 
  stat_summary(fun.y = mean,
               fun.ymin = function(x) mean(x) - sd(x),
               fun.ymax = function(x) mean(x) + sd(x),
               geom = "pointrange", shape=21, size=1, color="black", fill="white") +
  stat_compare_means(method = "t.test", comparisons = my_comparisons, 
                     label.y = c(6000), aes(label = after_stat(p.signif))) +
  theme_classic() +
  labs(title="",
       x ="Sexe", y = "Distance moyenne (m) entre les zones individuelles
journalière d'alimentation et de repos", fill="") ; distance_roost_forag_sex_plot

my_comparisons <- list( c("adult", "juv"))

distance_roost_forag_age_plot <- ggplot(paired_centroids_age_sex_dt_2, 
                                        aes(x = age, y = distance_m)) + 
  geom_boxplot(col = "black", outlier.colour = "black", outlier.shape = 1, fill = "grey") +
  geom_jitter(shape = 21, size = 0.5, color = "white", alpha = 0.5, fill = "black", width = 0.3) +
  stat_summary(fun.ymin = function(x) mean(x) - sd(x),
               fun.ymax = function(x) mean(x) + sd(x), geom="linerange", size=1, color="black") + 
  stat_summary(fun.y = mean,
               fun.ymin = function(x) mean(x) - sd(x),
               fun.ymax = function(x) mean(x) + sd(x),
               geom = "pointrange", shape=21, size=1, color="black", fill="white") +
  theme_classic() +
  stat_compare_means(method = "t.test", comparisons = my_comparisons, 
                     label.y = c(6000), aes(label = after_stat(p.signif))) +
  labs(title="",
       x ="Age", y = "Distance moyenne (m) entre les zones individuelles
journalière d'alimentation et de repos", fill="") ; distance_roost_forag_age_plot

my_comparisons <- list( c("in", "out"))

distance_roost_forag_chasse_plot <- ggplot(paired_centroids_chasse_dt_2, 
                                        aes(x = in_out_saison, y = distance_m)) + 
  geom_boxplot(col = "black", outlier.colour = "black", outlier.shape = 1, fill = "grey") +
  geom_jitter(shape = 21, size = 0.5, color = "white", alpha = 0.5, fill = "black", width = 0.3) +
  stat_summary(fun.ymin = function(x) mean(x) - sd(x),
               fun.ymax = function(x) mean(x) + sd(x), geom="linerange", size=1, color="black") + 
  stat_summary(fun.y = mean,
               fun.ymin = function(x) mean(x) - sd(x),
               fun.ymax = function(x) mean(x) + sd(x),
               geom = "pointrange", shape=21, size=1, color="black", fill="white") +
  theme_classic() +
  stat_compare_means(method = "t.test", comparisons = my_comparisons, 
                     label.y = c(6000), aes(label = after_stat(p.signif))) +
  labs(title="",
       x ="Periode de chasse", y = "Distance moyenne (m) entre les zones individuelles
journalière d'alimentation et de repos", fill="") ; distance_roost_forag_chasse_plot

distance_roost_forag_age_sex_chasse_plot <- ggarrange(distance_roost_forag_sex_plot, 
                                                      distance_roost_forag_age_plot, 
                                                      distance_roost_forag_chasse_plot, 
                                                      ncol = 3)

ggsave(paste0(atlas_path, "/distance_roost_forag_age_sex_plot.png"), 
       plot = distance_roost_forag_age_sex_chasse_plot, width = 10, height = 4, dpi = 300)

# mean individuelle
distance_roost_forag_plot <- ggplot(paired_centroids_mean_dt, 
                                  aes(x = reorder(ID, mean_dist), y = mean_dist)) + 
  geom_hline(yintercept=mean_dist, color = "black") +
  geom_hline(yintercept=mean_dist + sd_dist, linetype="longdash", color = "grey") +
  geom_hline(yintercept=mean_dist - sd_dist, linetype="longdash", color = "grey") +
  geom_errorbar(aes(ymin= mean_dist - sd_dist, ymax= mean_dist + sd_dist), width=0, color="grey") +
  geom_point(shape = 21, size = 4, color = "black", fill = "grey") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  labs(title="",
       x ="Individu", y = "Distance moyenne (+/- écart-type) entre 
les zones d'alimentation et de repos (m)", fill="") ; distance_roost_forag_plot

ggsave(paste0(atlas_path, "/distance_roost_forag_plot.png"), 
       plot = distance_roost_forag_plot, width = 10, height = 4, dpi = 300)

summary(lm(paired_centroids_mean_dt$mean_dist ~ paired_centroids_mean_dt$sd_dist))

# ########################## ---
# Variation inter-annuelle ----------------------------------------------------
# ########################## ---
# 
# ## # # # # # --- 
## reposoir  ------------------------------------------------------------------
# ## # # # # # --- 
# 
# crs_utm <- "EPSG:32630"
# ZOOM <- c("A","B","C","D","E")
# results_kud.roosting_ZOOM_year = NULL
# 
# for (lettre in ZOOM){
#   # in ZOOM
#   ZOOM <- st_read(paste0(data_generated_path,"ZOOM_",lettre,".gpkg"))
#   ZOOM <- st_transform(ZOOM, crs = 4326)
#   GPS.ZOOM <- st_intersection(GPS, ZOOM) 
#   GPS.roosting_ZOOM_year <- GPS.ZOOM %>% 
#     filter(behavior == "roosting") %>% 
#     dplyr::select(lon,lat,year) %>% 
#     st_drop_geometry() %>% 
#     na.omit()
#   
#   if (nrow(GPS.roosting_ZOOM_year) == 0) {
#     next  # Passe directement à l'itération suivante
#   }
#   
#   nb_row <- GPS.roosting_ZOOM_year %>% 
#     group_by(year) %>%
#     summarise(n = n(), .groups = "drop")
#   
#   if (min(nb_row$n) < 5) {
#     next  # Passe directement à l'itération suivante
#   }
#   
#   # Crée une table avec tous les mois possibles
#   all_year <- tibble(
#     year = c(2018:2024)
#   )
#   
#   # Compte les occurrences par mois dans tes données
#   nb_row <- GPS.roosting_ZOOM_year %>%
#     group_by(year) %>%
#     summarise(n = n(), .groups = "drop")
#   
#   # Joint tous les mois et remplit avec 0 si manquant
#   nb_row_complet <- all_year %>%
#     left_join(nb_row, by = "year") %>%
#     mutate(n = if_else(is.na(n), 0L, n))
#   
#   if (min(nb_row_complet$n) < 5) {
#     next  # Passe directement à l'itération suivante
#   }
#   
#   GPS_spa.roosting_ZOOM_year <- st_as_sf(GPS.roosting_ZOOM_year, coords = c("lon", "lat"), crs = 4326)
#   GPS_spa.roosting_ZOOM_year <- st_transform(GPS_spa.roosting_ZOOM_year, crs = 32630) 
#   GPS_coods.roosting_ZOOM_year <- st_coordinates(GPS_spa.roosting_ZOOM_year)
#   
#   # raster/grid
#   grid_ZOOM <- st_read(paste0(data_generated_path, "grid_ZOOM_",lettre,".gpkg"))
#   raster_ZOOM <- rast(grid_ZOOM, resolution = resolution_ZOOM, crs="EPSG:2154")
#   SpatRaster_ZOOM <- project(raster_ZOOM, crs_utm)  
#   RasterLayer_ZOOM <- raster(SpatRaster_ZOOM) 
#   SpatialPixels_ZOOM <- as(RasterLayer_ZOOM, "SpatialPixels")
#   
#   # Règle de Silverman
#   sigma_x.roosting_ZOOM_year <- sd(GPS_coods.roosting_ZOOM_year[,1]) 
#   sigma_y.roosting_ZOOM_year <- sd(GPS_coods.roosting_ZOOM_year[,2]) 
#   n.roosting_ZOOM_year <- nrow(GPS.roosting_ZOOM_year)  
#   h.silverman_x_roosting_ZOOM_year <- 1.06 * sigma_x.roosting_ZOOM_year * n.roosting_ZOOM_year^(-1/5) / 2
#   h_silverman_y_roosting_ZOOM_year <- 1.06 * sigma_y.roosting_ZOOM_year * n.roosting_ZOOM_year^(-1/5) / 2
#   locs_spa.roosting_ZOOM_year <- as(GPS_spa.roosting_ZOOM_year, "Spatial")
#   
#   # KernelUD
#   kud.roosting_ZOOM_year <- kernelUD(locs_spa.roosting_ZOOM_year["year"], 
#                                      grid = SpatialPixels_ZOOM, 
#                                      h = mean(c(h.silverman_x_roosting_ZOOM_year, 
#                                                 h_silverman_y_roosting_ZOOM_year)))
#   
#   kud_list.roosting_ZOOM_year <- lapply(names(kud.roosting_ZOOM_year), function(year) {
#     
#     print(year)
#     
#     # Extraire l'estimation de densité pour un ID spécifique
#     kud_single.roosting_ZOOM_year <- kud.roosting_ZOOM_year[[year]]
#     rast.roosting_ZOOM_year <- rast(kud_single.roosting_ZOOM_year)
#     courtour.roosting_ZOOM_year <- as.contour(rast.roosting_ZOOM_year)
#     sf.roosting_ZOOM_year <- st_as_sf(courtour.roosting_ZOOM_year)
#     cast.roosting_ZOOM_year <- st_cast(sf.roosting_ZOOM_year, "POLYGON")
#     cast.roosting_ZOOM_year$year <- year
#     
#     return(cast.roosting_ZOOM_year)
#   })
#   
#   kud_all.roosting_ZOOM_year <- do.call(rbind, kud_list.roosting_ZOOM_year)
#   kud_all.roosting_ZOOM_year$year <- as.factor(kud_all.roosting_ZOOM_year$year)
#   kud_all.roosting_ZOOM_year$ZOOM <- lettre
#   results_kud.roosting_ZOOM_year <- rbind(results_kud.roosting_ZOOM_year, kud_all.roosting_ZOOM_year)
#   
# }
# 
# # write & read
# st_write(results_kud.roosting_ZOOM_year, paste0(data_generated_path, "results_kud.roosting_ZOOM_year.gpkg"), append = FALSE)
# results_kud.roosting_ZOOM_year <- st_read(file.path(data_generated_path, "results_kud.roosting_ZOOM_year.gpkg"))
# 
# # plot
# # tmap_mode("view")
# # UDMap_roosting_year_ZOOM <- tm_scalebar() +   
# #   tm_basemap(c("OpenStreetMap", "Esri.WorldImagery", "CartoDB.Positron")) +
# #   tm_shape(results_kud.roosting_ZOOM_year) + 
# #   tm_facets("year") + 
# #   tm_polygons(border.col = "grey", fill = "level", fill_alpha = 0.5, 
# #               palette = palette_roosting) +
# #   tm_shape(RMO) +
# #   tm_borders(col = "white", lwd = 3, lty = "dashed") +
# #   tm_shape(terre_mer) +
# #   tm_lines(col = "lightblue", lwd = 0.1); UDMap_roosting_year_ZOOM
# 
# ###                        ###
# ### Repétabilité inter-year / population scale ###
# ###                        ###
# 
# GPS.year_repet_pop <- GPS %>% 
#   filter(behavior == "roosting") %>% 
#   dplyr::select(datetime,lon,lat,year) %>% 
#   st_drop_geometry() %>% 
#   na.omit()
# 
# # au moins 5 point par group
# n_per_year <- GPS.year_repet_pop %>% 
#   group_by(year) %>% 
#   summarize(n = n())%>% 
#   filter(n <= 5)
# 
# GPS.year_repet_pop <- GPS.year_repet_pop %>% 
#   filter(year %ni% n_per_year$year)
# 
# # Transformer en objet spatial (EPSG:4326)
# GPS_spa.year_repet_pop <- st_as_sf(GPS.year_repet_pop, coords = c("lon", "lat"), crs = 4326)
# GPS_spa.year_repet_pop <- st_transform(GPS_spa.year_repet_pop, crs = 32630)
# 
# # raster/grid
# crs_utm <- "EPSG:32630"
# SpatRaster <- project(raster_100x100, crs_utm)
# RasterLayer <- raster(SpatRaster)
# SpatialPixels <- as(RasterLayer, "SpatialPixels")
# 
# # Extraire les coordonnées reprojetées
# coords.year_repet_pop <- st_coordinates(GPS_spa.year_repet_pop)
# 
# # Règle de Silverman
# sigma_x.roosting_year_repet_pop <- sd(coords.year_repet_pop[,1])
# sigma_y_roosting_year_repet_pop <- sd(coords.year_repet_pop[,2])
# n.roosting_year_repet_pop <- nrow(GPS_spa.year_repet_pop)
# 
# h.silverman_x_roosting_year_repet_pop <- 1.06 * sigma_x.roosting_year_repet_pop * n.roosting_year_repet_pop^(-1/5) / 2
# h.silverman_y_roosting_year_repet_pop <- 1.06 * sigma_y_roosting_year_repet_pop * n.roosting_year_repet_pop^(-1/5) / 2 
# 
# GPS_spa.year_repet_pop <- as(GPS_spa.year_repet_pop, "Spatial")
# 
# kud.roosting_year_repet_pop <- kernelUD(GPS_spa.year_repet_pop["year"], 
#                                         grid = as(SpatialPixels, "SpatialPixels"),
#                                         h = mean(c(h.silverman_x_roosting_year_repet_pop,
#                                                    h.silverman_y_roosting_year_repet_pop)))
# 
# ##                     ##
# ## valeur répétabilité ##
# ##                     ##
# 
# overlap.roosting_year_repet_pop <- kerneloverlaphr(kud.roosting_year_repet_pop, method = "BA")
# mean_overlap.roosting_year_repet_pop <- mean(overlap.roosting_year_repet_pop, na.rm = T) ; mean
# 
# # overlap_matrix
# min_val <- min(overlap.roosting_year_repet_pop, na.rm = TRUE)
# max_val <- max(overlap.roosting_year_repet_pop, na.rm = TRUE)
# ordre <- c("2018","2019","2020","2021","2022","2023","2024")
# overlap.roosting_year_repet_pop <- overlap.roosting_year_repet_pop[ordre, ordre]
# 
# overlap.roosting_year_repet_pop <- as.data.frame(overlap.roosting_year_repet_pop)
# 
# plot.overlapp_roosting_year_repet_pop <- ggcorrplot(overlap.roosting_year_repet_pop,
#                                                     outline.col = "white",
#                                                     hc.order = FALSE,
#                                                     type = "lower",
#                                                     lab = TRUE,
#                                                     digits = 1) +
#   scale_x_continuous(breaks=seq(2000, 2030, 1)) +
#   scale_y_continuous(breaks=seq(2000, 2030, 1)) +
#   scale_fill_gradientn(colors = paletteer_c("grDevices::Sunset", 10, direction = -1),
#                        limits = c(min(min_val, na.rm = TRUE)-0.05, 
#                                   max(max_val, na.rm = TRUE)+0.05)) ; plot.overlapp_roosting_year_repet_pop
# 
# ggsave(paste0(atlas_path, "/plot.overlapp_roosting_year_repet_pop.png"), 
#        plot = plot.overlapp_roosting_year_repet_pop, width = 10, height = 5, dpi = 1000)
# 
# ##               ##
# ## UDMap par ind ##
# ##               ##
# 
# # Estimation UDmap par ind par year
# 
# # Créer une liste pour stocker les résultats
# UDmaps_list.roosting_year_repet_pop <- lapply(names(kud.roosting_year_repet_pop), function(year) {
#   
#   print(year)
#   
#   # Extraire l'estimation de densité pour un ID spécifique
#   kud_single.roosting_year_repet_pop <- kud.roosting_year_repet_pop[[year]]
#   rast.roosting_year_repet_pop <- rast(kud_single.roosting_year_repet_pop)
#   contour.roosting_year_repet_pop <- as.contour(rast.roosting_year_repet_pop)
#   sf.roosting_year_repet_pop <- st_as_sf(contour.roosting_year_repet_pop)
#   cast.roosting_year_repet_pop <- st_cast(sf.roosting_year_repet_pop, "POLYGON")
#   cast.roosting_year_repet_pop$year <- year
#   
#   return(cast.roosting_year_repet_pop)
#   
# })
# 
# # Fusionner tous les ID dans un seul objet sf
# results_kud.roosting_year_repet_pop <- do.call(rbind, UDmaps_list.roosting_year_repet_pop)
# results_kud.roosting_year_repet_pop$year <- as.factor(results_kud.roosting_year_repet_pop$year)
# # results_kud.roosting_year_repet_pop$ID <- sub("_.*", "", results_kud.roosting_year_repet_pop$year)
# # results_kud.roosting_year_repet_pop$year <- droplevels(results_kud.roosting_year_repet_pop$year)
# # results_kud.roosting_year_repet_pop$Periode <- sub(".*_", "", results_kud.roosting_year_repet_pop$year)
# # results_kud.roosting_year_repet_pop$ID <- as.factor(results_kud.roosting_year_repet_pop$ID)
# 
# # plot 
# tmap_mode("view")
# UDMap.roosting_year_repet_pop <- tm_scalebar() +   
#   tm_basemap(c("OpenStreetMap", "Esri.WorldImagery", "CartoDB.Positron")) +
#   tm_shape(results_kud.roosting_year_repet_pop) + 
#   tm_polygons(border.col = "grey", fill = "year", fill_alpha = 0.8, 
#               palette = palette_roosting) +
#   tm_shape(RMO) +
#   tm_borders(col = "white", lwd = 3, lty = "dashed") +
#   tm_shape(terre_mer) +
#   tm_lines(col = "lightblue", lwd = 0.1); UDMap.roosting_year_repet_pop
# 
# tmap_save(UDMap.roosting_year_repet_pop, paste0(atlas_path,"UDMap.roosting_year_repet_pop.html"))
# 
# ###                        ###
# ### Repétabilité inter-year / individual scale ###
# ###                        ###
# 
# GPS.year_repet <- GPS %>% 
#   filter(behavior == "roosting") %>% 
#   dplyr::select(ID,datetime,lon,lat,year) %>% 
#   mutate(ID_year = paste0(ID, "_", year)) %>% 
#   st_drop_geometry() %>% 
#   na.omit()
# 
# # au moins 5 point par group
# n_per_year <- GPS.year_repet %>% 
#   group_by(ID_year) %>% 
#   summarize(n = n())%>% 
#   filter(n <= 5)
# 
# GPS.year_repet <- GPS.year_repet %>% 
#   filter(ID_year %ni% n_per_year$ID_year)
# 
# # Transformer en objet spatial (EPSG:4326)
# GPS_spa.year_repet <- st_as_sf(GPS.year_repet, coords = c("lon", "lat"), crs = 4326)
# GPS_spa.year_repet <- st_transform(GPS_spa.year_repet, crs = 32630)
# 
# # raster/grid
# crs_utm <- "EPSG:32630"
# SpatRaster <- project(raster_100x100, crs_utm)
# RasterLayer <- raster(SpatRaster)
# SpatialPixels <- as(RasterLayer, "SpatialPixels")
# 
# # Extraire les coordonnées reprojetées
# coords.year_repet <- st_coordinates(GPS_spa.year_repet)
# 
# # Règle de Silverman
# sigma_x.roosting_year_repet <- sd(coords.year_repet[,1])
# sigma_y_roosting_year_repet <- sd(coords.year_repet[,2])
# n.roosting_year_repet <- nrow(GPS_spa.year_repet)
# 
# h.silverman_x_roosting_year_repet <- 1.06 * sigma_x.roosting_year_repet * n.roosting_year_repet^(-1/5) / 2
# h.silverman_y_roosting_year_repet <- 1.06 * sigma_y_roosting_year_repet * n.roosting_year_repet^(-1/5) / 2 
# 
# GPS_spa.year_repet <- as(GPS_spa.year_repet, "Spatial")
# 
# kud.roosting_year_repet <- kernelUD(GPS_spa.year_repet["ID_year"], 
#                                     grid = as(SpatialPixels, "SpatialPixels"),
#                                     h = mean(c(h.silverman_x_roosting_year_repet,
#                                                h.silverman_y_roosting_year_repet)))
# 
# ##                     ##
# ## valeur répétabilité ##
# ##                     ##
# 
# # Estimation valeur d'overlapp par ind entre chaque year
# 
# # Extraire les noms uniques des individus
# individus <- unique(GPS_spa.year_repet$ID)
# 
# # Stocker les résultats
# overlap_results.roosting_year_repet = NULL
# 
# # Boucle sur chaque individu
# for (ind in individus) {
#   
#   print(ind)
#   
#   # Trouver les noms des périodes de cet individu dans hr_kde
#   ID_periodes <- names(kud.roosting_year_repet)[grep(paste0("^", ind, "_"), names(kud.roosting_year_repet))]
#   
#   # Vérifier que l'individu a bien deux périodes
#   if (length(ID_periodes) >= 2) {
#     # Créer un estUDm valide
#     hr_kde_ind.roosting_year_repet <- kud.roosting_year_repet[ID_periodes]
#     class(hr_kde_ind.roosting_year_repet) <- "estUDm"  # Important pour que kerneloverlaphr() fonctionne
#     
#     # Calculer l'overlap entre les deux périodes
#     overlap_value.roosting_year_repet <- kerneloverlaphr(hr_kde_ind.roosting_year_repet, 
#                                                          method = "BA")[1, 2]
#     
#     info_ind.roosting_year_repet <- c(ind, overlap_value.roosting_year_repet)
#     
#     # Stocker le résultat
#     # overlap_results <- rbind(overlap_results, data.frame(Individu = ind, Overlap = overlap_value))
#     overlap_results.roosting_year_repet <- rbind(overlap_results.roosting_year_repet, info_ind.roosting_year_repet)
#     
#   }
# }
# 
# overlap_results.roosting_year_repet <- as.data.frame(overlap_results.roosting_year_repet)
# 
# overlap_results.roosting_year_repet <- overlap_results.roosting_year_repet %>% 
#   rename(ID = V1, overlap = V2)
# 
# overlap_results.roosting_year_repet$overlap <- as.numeric(overlap_results.roosting_year_repet$overlap)
# 
# mean_overlap.roosting_year_repet <- mean(overlap_results.roosting_year_repet$overlap, na.rm = T) ; mean_overlap.roosting_year_repet
# 
# # Afficher les résultats
# overlap_results.roosting_year_repet <- overlap_results.roosting_year_repet[order(overlap_results.roosting_year_repet$overlap), ] ; overlap_results.roosting_year_repet
# 
# # plot
# plot.roosting_year_repet <- ggplot(overlap_results.roosting_year_repet, aes(x=reorder(ID, overlap), y=overlap, fill = overlap)) + 
#   geom_point(shape = 21, size = 4) +
#   theme_classic() +
#   theme(legend.position = c(.75, .3)) +
#   scale_fill_gradientn(colors = paletteer_c("grDevices::Sunset", 10, direction = -1)) +
#   theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
#   # scale_fill_manual() +
#   labs(title="",
#        x ="Individu", y = "Pourcentage de chevauchement moyen 
# de zone de reposoirs entre années") ; plot.roosting_year_repet
# 
# ggsave(paste0(atlas_path, "/plot.roosting_year_repet.png"), 
#        plot = plot.roosting_year_repet, width = 8, height = 5, dpi = 1000)
# 
# ##               ##
# ## UDMap par ind ##
# ##               ##
# 
# # Estimation UDmap par ind par year
# 
# # Créer une liste pour stocker les résultats
# UDmaps_list.roosting_ZOOM_year <- lapply(names(kud.roosting_year_repet), function(Individu_Periode) {
#   
#   print(Individu_Periode)
#   
#   # Extraire l'estimation de densité pour un ID spécifique
#   kud_single.roosting_ZOOM_year <- kud.roosting_year_repet[[Individu_Periode]]
#   rast.roosting_ZOOM_year <- rast(kud_single.roosting_ZOOM_year)
#   contour.roosting_ZOOM_year <- as.contour(rast.roosting_ZOOM_year)
#   sf.roosting_ZOOM_year <- st_as_sf(contour.roosting_ZOOM_year)
#   cast.roosting_ZOOM_year <- st_cast(sf.roosting_ZOOM_year, "POLYGON")
#   cast.roosting_ZOOM_year$Individu_Periode <- Individu_Periode
#   
#   return(cast.roosting_ZOOM_year)
#   
# })
# 
# # Fusionner tous les ID dans un seul objet sf
# results_kud.roosting_ZOOM_year <- do.call(rbind, UDmaps_list.roosting_ZOOM_year)
# results_kud.roosting_ZOOM_year$Individu_Periode <- as.factor(results_kud.roosting_ZOOM_year$Individu_Periode)
# results_kud.roosting_ZOOM_year$ID <- sub("_.*", "", results_kud.roosting_ZOOM_year$Individu_Periode)
# results_kud.roosting_ZOOM_year$Individu_Periode <- droplevels(results_kud.roosting_ZOOM_year$Individu_Periode)
# results_kud.roosting_ZOOM_year$Periode <- sub(".*_", "", results_kud.roosting_ZOOM_year$Individu_Periode)
# results_kud.roosting_ZOOM_year$ID <- as.factor(results_kud.roosting_ZOOM_year$ID)
# 
# # plot 
# tmap_mode("view")
# UDMap_roosting_rep_inter_year <- tm_scalebar() +   
#   tm_basemap(c("OpenStreetMap", "Esri.WorldImagery", "CartoDB.Positron")) +
#   tm_shape(results_kud.roosting_ZOOM_year) + 
#   tm_facets("ID", drop.units = TRUE) +
#   tm_polygons(border.col = "grey", fill = "Periode", fill_alpha = 0.2,
#               palette = palette_roosting) +
#   tm_shape(RMO) +
#   tm_borders(col = "white", lwd = 3, lty = "dashed") +
#   tm_shape(terre_mer) +
#   tm_lines(col = "lightblue", lwd = 0.1); UDMap_roosting_rep_inter_year
# 
# tmap_save(UDMap_roosting_rep_inter_year, paste0(atlas_path,"UDMap_roosting_rep_inter_year.html"))
# 
# ## # # # # # --- 
## alimentation  ---------------------------------------------------------------
# ## # # # # # ---
# 
# crs_utm <- "EPSG:32630"
# ZOOM <- c("A","B","C","D","E")
# results_kud.foraging_ZOOM_year = NULL
# 
# # lettre = "B"
# 
# for (lettre in ZOOM){
#   # in ZOOM
#   ZOOM <- st_read(paste0(data_generated_path,"ZOOM_",lettre,".gpkg"))
#   ZOOM <- st_transform(ZOOM, crs = 4326)
#   GPS.ZOOM <- st_intersection(GPS, ZOOM) 
#   GPS.foraging_ZOOM_year <- GPS.ZOOM %>% 
#     filter(behavior == "foraging") %>% 
#     dplyr::select(lon,lat,year) %>% 
#     st_drop_geometry() %>% 
#     na.omit()
#   
#   if (nrow(GPS.foraging_ZOOM_year) == 0) {
#     next  # Passe directement à l'itération suivante
#   }
#   
#   nb_row <- GPS.foraging_ZOOM_year %>% 
#     group_by(year) %>%
#     summarise(n = n(), .groups = "drop")
#   
#   if (min(nb_row$n) < 5) {
#     next  # Passe directement à l'itération suivante
#   }
#   
#   # Crée une table avec tous les mois possibles
#   all_year <- tibble(
#     year = c(2018:2024)
#   )
#   
#   # Compte les occurrences par mois dans tes données
#   nb_row <- GPS.foraging_ZOOM_year %>%
#     group_by(year) %>%
#     summarise(n = n(), .groups = "drop")
#   
#   # Joint tous les mois et remplit avec 0 si manquant
#   nb_row_complet <- all_year %>%
#     left_join(nb_row, by = "year") %>%
#     mutate(n = if_else(is.na(n), 0L, n))
#   
#   if (min(nb_row_complet$n) < 5) {
#     next  # Passe directement à l'itération suivante
#   }
#   
#   GPS_spa.foraging_ZOOM_year <- st_as_sf(GPS.foraging_ZOOM_year, coords = c("lon", "lat"), crs = 4326)
#   GPS_spa.foraging_ZOOM_year <- st_transform(GPS_spa.foraging_ZOOM_year, crs = 32630) 
#   GPS_coods.foraging_ZOOM_year <- st_coordinates(GPS_spa.foraging_ZOOM_year)
#   
#   # raster/grid
#   grid_ZOOM <- st_read(paste0(data_generated_path, "grid_ZOOM_",lettre,".gpkg"))
#   raster_ZOOM <- rast(grid_ZOOM, resolution = resolution_ZOOM, crs="EPSG:2154")
#   SpatRaster_ZOOM <- project(raster_ZOOM, crs_utm)  
#   RasterLayer_ZOOM <- raster(SpatRaster_ZOOM) 
#   SpatialPixels_ZOOM <- as(RasterLayer_ZOOM, "SpatialPixels")
#   
#   # Règle de Silverman
#   sigma_x.foraging_ZOOM_year <- sd(GPS_coods.foraging_ZOOM_year[,1]) 
#   sigma_y.foraging_ZOOM_year <- sd(GPS_coods.foraging_ZOOM_year[,2]) 
#   n.foraging_ZOOM_year <- nrow(GPS.foraging_ZOOM_year)  
#   h.silverman_x_foraging_ZOOM_year <- 1.06 * sigma_x.foraging_ZOOM_year * n.foraging_ZOOM_year^(-1/5) / 2
#   h_silverman_y_foraging_ZOOM_year <- 1.06 * sigma_y.foraging_ZOOM_year * n.foraging_ZOOM_year^(-1/5) / 2
#   locs_spa.foraging_ZOOM_year <- as(GPS_spa.foraging_ZOOM_year, "Spatial")
#   
#   # KernelUD
#   kud.foraging_ZOOM_year <- kernelUD(locs_spa.foraging_ZOOM_year["year"], 
#                                      grid = SpatialPixels_ZOOM, 
#                                      h = mean(c(h.silverman_x_foraging_ZOOM_year, 
#                                                 h_silverman_y_foraging_ZOOM_year)))
#   
#   kud_list.foraging_ZOOM_year <- lapply(names(kud.foraging_ZOOM_year), function(year) {
#     
#     print(year)
#     
#     # Extraire l'estimation de densité pour un ID spécifique
#     kud_single.foraging_ZOOM_year <- kud.foraging_ZOOM_year[[year]]
#     rast.foraging_ZOOM_year <- rast(kud_single.foraging_ZOOM_year)
#     courtour.foraging_ZOOM_year <- as.contour(rast.foraging_ZOOM_year)
#     sf.foraging_ZOOM_year <- st_as_sf(courtour.foraging_ZOOM_year)
#     cast.foraging_ZOOM_year <- st_cast(sf.foraging_ZOOM_year, "POLYGON")
#     cast.foraging_ZOOM_year$year <- year
#     
#     return(cast.foraging_ZOOM_year)
#   })
#   
#   kud_all.foraging_ZOOM_year <- do.call(rbind, kud_list.foraging_ZOOM_year)
#   kud_all.foraging_ZOOM_year$year <- as.factor(kud_all.foraging_ZOOM_year$year)
#   kud_all.foraging_ZOOM_year$ZOOM <- lettre
#   results_kud.foraging_ZOOM_year <- rbind(results_kud.foraging_ZOOM_year, kud_all.foraging_ZOOM_year)
#   
# }
# 
# # write
# st_write(results_kud.foraging_ZOOM_year, paste0(data_generated_path, "results_kud.foraging_ZOOM_year.gpkg"), append = FALSE)
# # read
# results_kud.foraging_ZOOM_year <- st_read(file.path(data_generated_path, "results_kud.foraging_ZOOM_year.gpkg"))
# 
# # # plot
# # tmap_mode("view")
# # UDMap_foraging_year_ZOOM <- tm_scalebar() +   
# #   tm_basemap(c("OpenStreetMap", "Esri.WorldImagery", "CartoDB.Positron")) +
# #   tm_shape(results_kud.foraging_ZOOM_year) + 
# #   tm_facets("year") + 
# #   tm_polygons(border.col = "grey", fill = "level", fill_alpha = 0.5, 
# #               palette = viridis::viridis(10, begin = 0, end = 1, 
# #                                          direction = 1, option = "plasma")) +  tm_facets("year") +
# #   tm_shape(terre_mer) +
# #   tm_lines(col = "lightblue", lwd = 0.1); UDMap_foraging_year_ZOOM
# 
# ###                        ###
# ### Repétabilité inter-year / population scale ###
# ###                        ###
# 
# GPS.year_repet_pop <- GPS %>% 
#   filter(behavior == "foraging") %>% 
#   dplyr::select(datetime,lon,lat,year) %>% 
#   st_drop_geometry() %>% 
#   na.omit()
# 
# # au moins 5 point par group
# n_per_year <- GPS.year_repet_pop %>% 
#   group_by(year) %>% 
#   summarize(n = n())%>% 
#   filter(n <= 5)
# 
# GPS.year_repet_pop <- GPS.year_repet_pop %>% 
#   filter(year %ni% n_per_year$year)
# 
# # Transformer en objet spatial (EPSG:4326)
# GPS_spa.year_repet_pop <- st_as_sf(GPS.year_repet_pop, coords = c("lon", "lat"), crs = 4326)
# GPS_spa.year_repet_pop <- st_transform(GPS_spa.year_repet_pop, crs = 32630)
# 
# # raster/grid
# crs_utm <- "EPSG:32630"
# SpatRaster <- project(raster_100x100, crs_utm)
# RasterLayer <- raster(SpatRaster)
# SpatialPixels <- as(RasterLayer, "SpatialPixels")
# 
# # Extraire les coordonnées reprojetées
# coords.year_repet_pop <- st_coordinates(GPS_spa.year_repet_pop)
# 
# # Règle de Silverman
# sigma_x.foraging_year_repet_pop <- sd(coords.year_repet_pop[,1])
# sigma_y_foraging_year_repet_pop <- sd(coords.year_repet_pop[,2])
# n.foraging_year_repet_pop <- nrow(GPS_spa.year_repet_pop)
# 
# h.silverman_x_foraging_year_repet_pop <- 1.06 * sigma_x.foraging_year_repet_pop * n.foraging_year_repet_pop^(-1/5) / 2 
# h.silverman_y_foraging_year_repet_pop <- 1.06 * sigma_y_foraging_year_repet_pop * n.foraging_year_repet_pop^(-1/5) / 2 
# 
# GPS_spa.year_repet_pop <- as(GPS_spa.year_repet_pop, "Spatial")
# 
# kud.foraging_year_repet_pop <- kernelUD(GPS_spa.year_repet_pop["year"], 
#                                         grid = as(SpatialPixels, "SpatialPixels"),
#                                         h = mean(c(h.silverman_x_foraging_year_repet_pop,
#                                                    h.silverman_y_foraging_year_repet_pop)))
# 
# ##                     ##
# ## valeur répétabilité ##
# ##                     ##
# 
# overlap.foraging_year_repet_pop <- kerneloverlaphr(kud.foraging_year_repet_pop, method = "BA")
# mean_overlap.foraging_year_repet_pop <- mean(overlap.foraging_year_repet_pop, na.rm = T) ; mean
# 
# # overlap_matrix
# min_val <- min(overlap.foraging_year_repet_pop, na.rm = TRUE)
# max_val <- max(overlap.foraging_year_repet_pop, na.rm = TRUE)
# ordre <- c("2018","2019","2020","2021","2022","2023","2024")
# overlap.foraging_year_repet_pop <- overlap.foraging_year_repet_pop[ordre, ordre]
# 
# plot.overlapp_foraging_year_repet_pop <- ggcorrplot(overlap.foraging_year_repet_pop,
#                                                     hc.order = FALSE,
#                                                     type = "lower",
#                                                     lab = TRUE,
#                                                     digits = 1,
#                                                     colors = c("#C6EDC8FF", "#00B1AEFF", "#00468BFF"),
#                                                     ggtheme = theme_minimal()) +
#   scale_fill_gradientn(colors = c("#C6EDC8FF", "#00B1AEFF", "#00468BFF"),
#                        limits = c(min_val - 0.1, 
#                                   max_val + 0.1)) ; plot.overlapp_foraging_year_repet_pop
# 
# ggsave(paste0(atlas_path, "/plot.overlapp_foraging_year_repet_pop.png"), 
#        plot = plot.overlapp_foraging_year_repet_pop, width = 10, height = 5, dpi = 1000)
# 
# ##               ##
# ## UDMap par ind ##
# ##               ##
# 
# # Estimation UDmap par ind par year
# 
# # Créer une liste pour stocker les résultats
# UDmaps_list.foraging_year_repet_pop <- lapply(names(kud.foraging_year_repet_pop), function(year) {
#   
#   print(year)
#   
#   # Extraire l'estimation de densité pour un ID spécifique
#   kud_single.foraging_year_repet_pop <- kud.foraging_year_repet_pop[[year]]
#   rast.foraging_year_repet_pop <- rast(kud_single.foraging_year_repet_pop)
#   contour.foraging_year_repet_pop <- as.contour(rast.foraging_year_repet_pop)
#   sf.foraging_year_repet_pop <- st_as_sf(contour.foraging_year_repet_pop)
#   cast.foraging_year_repet_pop <- st_cast(sf.foraging_year_repet_pop, "POLYGON")
#   cast.foraging_year_repet_pop$year <- year
#   
#   return(cast.foraging_year_repet_pop)
#   
# })
# 
# # Fusionner tous les ID dans un seul objet sf
# results_kud.foraging_year_repet_pop <- do.call(rbind, UDmaps_list.foraging_year_repet_pop)
# results_kud.foraging_year_repet_pop$year <- as.factor(results_kud.foraging_year_repet_pop$year)
# # results_kud.foraging_year_repet_pop$ID <- sub("_.*", "", results_kud.foraging_year_repet_pop$year)
# # results_kud.foraging_year_repet_pop$year <- droplevels(results_kud.foraging_year_repet_pop$year)
# # results_kud.foraging_year_repet_pop$Periode <- sub(".*_", "", results_kud.foraging_year_repet_pop$year)
# # results_kud.foraging_year_repet_pop$ID <- as.factor(results_kud.foraging_year_repet_pop$ID)
# 
# # plot 
# tmap_mode("view")
# UDMap.foraging_year_repet_pop <- tm_scalebar() +   
#   tm_basemap(c("OpenStreetMap", "Esri.WorldImagery", "CartoDB.Positron")) +
#   tm_shape(results_kud.foraging_year_repet_pop) + 
#   tm_polygons(border.col = "grey", fill = "year", fill_alpha = 0.8, 
#               palette = palette_foraging) +
#   tm_shape(RMO) +
#   tm_borders(col = "white", lwd = 3, lty = "dashed") +
#   tm_shape(terre_mer) +
#   tm_lines(col = "lightblue", lwd = 0.1); UDMap.foraging_year_repet_pop
# 
# tmap_save(UDMap.foraging_year_repet_pop, paste0(atlas_path,"UDMap.foraging_year_repet_pop.html"))
# 
# ###                        ###
# ### Repétabilité inter-year / individual scale ###
# ###                        ###
# 
# GPS.year_repet <- GPS %>% 
#   filter(behavior == "foraging") %>% 
#   dplyr::select(ID,datetime,lon,lat,year) %>% 
#   mutate(ID_year = paste0(ID, "_", year)) %>% 
#   st_drop_geometry() %>% 
#   na.omit()
# 
# # au moins 5 point par group
# n_per_year <- GPS.year_repet %>% 
#   group_by(ID_year) %>% 
#   summarize(n = n())%>% 
#   filter(n <= 5)
# 
# GPS.year_repet <- GPS.year_repet %>% 
#   filter(ID_year %ni% n_per_year$ID_year)
# 
# # Transformer en objet spatial (EPSG:4326)
# GPS_spa.year_repet <- st_as_sf(GPS.year_repet, coords = c("lon", "lat"), crs = 4326)
# GPS_spa.year_repet <- st_transform(GPS_spa.year_repet, crs = 32630)
# 
# # raster/grid
# crs_utm <- "EPSG:32630"
# SpatRaster <- project(raster_100x100, crs_utm)
# RasterLayer <- raster(SpatRaster)
# SpatialPixels <- as(RasterLayer, "SpatialPixels")
# 
# # Extraire les coordonnées reprojetées
# coords.year_repet <- st_coordinates(GPS_spa.year_repet)
# 
# # Règle de Silverman
# sigma_x.foraging_year_repet <- sd(coords.year_repet[,1])
# sigma_y_foraging_year_repet <- sd(coords.year_repet[,2])
# n.foraging_year_repet <- nrow(GPS_spa.year_repet)
# 
# h.silverman_x_foraging_year_repet <- 1.06 * sigma_x.foraging_year_repet * n.foraging_year_repet^(-1/5) / 2 
# h.silverman_y_foraging_year_repet <- 1.06 * sigma_y_foraging_year_repet * n.foraging_year_repet^(-1/5) / 2 
# 
# GPS_spa.year_repet <- as(GPS_spa.year_repet, "Spatial")
# 
# kud.foraging_year_repet <- kernelUD(GPS_spa.year_repet["ID_year"], 
#                                     grid = as(SpatialPixels, "SpatialPixels"),
#                                     h = mean(c(h.silverman_x_foraging_year_repet,
#                                                h.silverman_y_foraging_year_repet)))
# 
# ##                     ##
# ## valeur répétabilité ##
# ##                     ##
# 
# # Estimation valeur d'overlapp par ind entre chaque year
# 
# # Extraire les noms uniques des individus
# individus <- unique(GPS_spa.year_repet$ID)
# 
# # Stocker les résultats
# overlap_results.foraging_year_repet = NULL
# 
# # Boucle sur chaque individu
# for (ind in individus) {
#   
#   print(ind)
#   
#   # Trouver les noms des périodes de cet individu dans hr_kde
#   ID_periodes <- names(kud.foraging_year_repet)[grep(paste0("^", ind, "_"), names(kud.foraging_year_repet))]
#   
#   # Vérifier que l'individu a bien deux périodes
#   if (length(ID_periodes) >= 2) {
#     # Créer un estUDm valide
#     hr_kde_ind.foraging_year_repet <- kud.foraging_year_repet[ID_periodes]
#     class(hr_kde_ind.foraging_year_repet) <- "estUDm"  # Important pour que kerneloverlaphr() fonctionne
#     
#     # Calculer l'overlap entre les deux périodes
#     overlap_value.foraging_year_repet <- kerneloverlaphr(hr_kde_ind.foraging_year_repet, 
#                                                          method = "BA")[1, 2]
#     
#     info_ind.foraging_year_repet <- c(ind, overlap_value.foraging_year_repet)
#     
#     # Stocker le résultat
#     # overlap_results <- rbind(overlap_results, data.frame(Individu = ind, Overlap = overlap_value))
#     overlap_results.foraging_year_repet <- rbind(overlap_results.foraging_year_repet, info_ind.foraging_year_repet)
#     
#   }
# }
# 
# overlap_results.foraging_year_repet <- as.data.frame(overlap_results.foraging_year_repet)
# 
# overlap_results.foraging_year_repet <- overlap_results.foraging_year_repet %>% 
#   rename(ID = V1, overlap = V2)
# 
# overlap_results.foraging_year_repet$overlap <- as.numeric(overlap_results.foraging_year_repet$overlap)
# 
# mean_overlap.foraging_year_repet <- mean(overlap_results.foraging_year_repet$overlap, na.rm = T) ; mean_overlap.foraging_year_repet
# 
# # Afficher les résultats
# overlap_results.foraging_year_repet <- overlap_results.foraging_year_repet[order(overlap_results.foraging_year_repet$overlap), ] ; overlap_results.foraging_year_repet
# 
# # plot
# plot.foraging_year_repet <- ggplot(overlap_results.foraging_year_repet, aes(x=reorder(ID, overlap), y=overlap, fill = overlap)) + 
#   geom_point(shape = 21, size = 4) +
#   theme_classic() +
#   theme(legend.position = c(.85, .3)) +
#   scale_fill_gradientn(colors = paletteer_c("grDevices::YlGnBu", 10, direction = -1)) +
#   theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
#   # scale_fill_manual() +
#   labs(title="",
#        x ="Individu", y = "Pourcentage de chevauchement moyen 
# de zone d'alimentation entre années") ; plot.foraging_year_repet
# 
# ggsave(paste0(atlas_path, "/plot.foraging_year_repet.png"), 
#        plot = plot.foraging_year_repet, width = 8, height = 5, dpi = 1000)
# 
# ##               ##
# ## UDMap par ind ##
# ##               ##
# 
# # Estimation UDmap par ind par year
# 
# # Créer une liste pour stocker les résultats
# UDmaps_list.foraging_ZOOM_year <- lapply(names(kud.foraging_year_repet), function(Individu_Periode) {
#   
#   print(Individu_Periode)
#   
#   # Extraire l'estimation de densité pour un ID spécifique
#   kud_single.foraging_ZOOM_year <- kud.foraging_year_repet[[Individu_Periode]]
#   rast.foraging_ZOOM_year <- rast(kud_single.foraging_ZOOM_year)
#   contour.foraging_ZOOM_year <- as.contour(rast.foraging_ZOOM_year)
#   sf.foraging_ZOOM_year <- st_as_sf(contour.foraging_ZOOM_year)
#   cast.foraging_ZOOM_year <- st_cast(sf.foraging_ZOOM_year, "POLYGON")
#   cast.foraging_ZOOM_year$Individu_Periode <- Individu_Periode
#   
#   return(cast.foraging_ZOOM_year)
#   
# })
# 
# # Fusionner tous les ID dans un seul objet sf
# results_kud.foraging_ZOOM_year <- do.call(rbind, UDmaps_list.foraging_ZOOM_year)
# results_kud.foraging_ZOOM_year$Individu_Periode <- as.factor(results_kud.foraging_ZOOM_year$Individu_Periode)
# results_kud.foraging_ZOOM_year$ID <- sub("_.*", "", results_kud.foraging_ZOOM_year$Individu_Periode)
# results_kud.foraging_ZOOM_year$Individu_Periode <- droplevels(results_kud.foraging_ZOOM_year$Individu_Periode)
# results_kud.foraging_ZOOM_year$Periode <- sub(".*_", "", results_kud.foraging_ZOOM_year$Individu_Periode)
# results_kud.foraging_ZOOM_year$ID <- as.factor(results_kud.foraging_ZOOM_year$ID)
# 
# 
# # plot 
# tmap_mode("view")
# UDMap_foraging_rep_inter_year <- tm_scalebar() +   
#   tm_basemap(c("OpenStreetMap", "Esri.WorldImagery", "CartoDB.Positron")) +
#   tm_shape(results_kud.foraging_ZOOM_year) + 
#   tm_facets("ID", drop.units = TRUE) +
#   tm_polygons(border.col = "grey", fill = "Periode", fill_alpha = 0.2,
#               palette = palette_foraging) +
#   tm_shape(RMO) +
#   tm_borders(col = "white", lwd = 3, lty = "dashed") +
#   tm_shape(terre_mer) +
#   tm_lines(col = "lightblue", lwd = 0.1) ; UDMap_foraging_rep_inter_year
# 
# # tmap_save(UDMap_foraging_rep_inter_year, paste0(atlas_path,"UDMap_foraging_rep_inter_year.html"))
# 
### correlation ------
# 
# overlap_results.roosting_year_repet <- overlap_results.roosting_year_repet %>% 
#   rename(overlap_roosting = overlap)
# 
# overlap_results.foraging_year_repet <- overlap_results.foraging_year_repet %>% 
#   rename(overlap_foraging = overlap)
# 
# correlation_fidelite_alim_repos_dt <- overlap_results.roosting_year_repet %>% 
#   left_join(overlap_results.foraging_year_repet)
# 
# summary(lm(correlation_fidelite_alim_repos_dt$overlap_roosting ~ correlation_fidelite_alim_repos_dt$overlap_foraging))
# 
# # plot
# correlation_fidelite_plot <- ggplot(correlation_fidelite_alim_repos_dt, aes(x=overlap_roosting, y=overlap_foraging)) + 
#   geom_smooth(method = lm, formula = y ~ x, se = T, col = "black") + 
#   geom_point(shape = 21, size = 4, col = "black", fill = "grey") +
#   theme_classic() +
#  labs(title="",
#        x ="Pourcentage de chevauchement 
# inter-annuelle de zone de repos", y = "Pourcentage de chevauchement 
# inter-annuelle de zone d'alimentation") ; correlation_fidelite_plot
# 
# ggsave(paste0(atlas_path, "/correlation_fidelite_plot.png"), 
#        plot = correlation_fidelite_plot, width = 4, height = 4, dpi = 1000)
# 
# ########################## ---
# Variation inter-mensuelle ----------------------------------------------------
# ########################## ---
#   
# ## # # # # # --- 
## reposoir  -------------------------------------------------------------------
# ## # # # # # --- 
# 
# crs_utm <- "EPSG:32630"
# ZOOM <- c("A","B","C","D","E")
# results_kud.roosting_ZOOM_month = NULL
# 
# # lettre = "B"
# 
# for (lettre in ZOOM){
#   # in ZOOM
#   ZOOM <- st_read(paste0(data_generated_path,"ZOOM_",lettre,".gpkg"))
#   ZOOM <- st_transform(ZOOM, crs = 4326)
#   GPS.ZOOM <- st_intersection(GPS, ZOOM) 
#   GPS.roosting_ZOOM_month <- GPS.ZOOM %>% 
#     filter(behavior == "roosting") %>% 
#     dplyr::select(lon,lat,month_label) %>% 
#     st_drop_geometry() %>% 
#     na.omit()
#   
#   if (nrow(GPS.roosting_ZOOM_month) == 0) {
#     next  # Passe directement à l'itération suivante
#   }
#   
#   nb_row <- GPS.roosting_ZOOM_month %>% 
#     group_by(month_label) %>%
#     summarise(n = n(), .groups = "drop")
#   
#   if (min(nb_row$n) < 5) {
#     next  # Passe directement à l'itération suivante
#   }
#   
#   # Crée une table avec tous les mois possibles
#   all_months <- tibble(
#     month_label = c("janv", "févr", "mars", "avr", "mai", "juin",
#                     "juil", "août", "sept", "oct", "nov", "déc")
#   )
#   
#   # Compte les occurrences par mois dans tes données
#   nb_row <- GPS.roosting_ZOOM_month %>%
#     group_by(month_label) %>%
#     summarise(n = n(), .groups = "drop")
#   
#   # Joint tous les mois et remplit avec 0 si manquant
#   nb_row_complet <- all_months %>%
#     left_join(nb_row, by = "month_label") %>%
#     mutate(n = if_else(is.na(n), 0L, n))
#   
#   if (min(nb_row_complet$n) < 5) {
#     next  # Passe directement à l'itération suivante
#   }
#   
#   GPS_spa.roosting_ZOOM_month <- st_as_sf(GPS.roosting_ZOOM_month, coords = c("lon", "lat"), crs = 4326)
#   GPS_spa.roosting_ZOOM_month <- st_transform(GPS_spa.roosting_ZOOM_month, crs = 32630) 
#   GPS_coods.roosting_ZOOM_month <- st_coordinates(GPS_spa.roosting_ZOOM_month)
#   
#   # raster/grid
#   grid_ZOOM <- st_read(paste0(data_generated_path, "grid_ZOOM_",lettre,".gpkg"))
#   raster_ZOOM <- rast(grid_ZOOM, resolution = resolution_ZOOM, crs="EPSG:2154")
#   SpatRaster_ZOOM <- project(raster_ZOOM, crs_utm)  
#   RasterLayer_ZOOM <- raster(SpatRaster_ZOOM) 
#   SpatialPixels_ZOOM <- as(RasterLayer_ZOOM, "SpatialPixels")
#   
#   # Règle de Silverman
#   sigma_x.roosting_ZOOM_month <- sd(GPS_coods.roosting_ZOOM_month[,1]) 
#   sigma_y.roosting_ZOOM_month <- sd(GPS_coods.roosting_ZOOM_month[,2]) 
#   n.roosting_ZOOM_month <- nrow(GPS.roosting_ZOOM_month)  
#   h.silverman_x_roosting_ZOOM_month <- 1.06 * sigma_x.roosting_ZOOM_month * n.roosting_ZOOM_month^(-1/5) / 2
#   h_silverman_y_roosting_ZOOM_month <- 1.06 * sigma_y.roosting_ZOOM_month * n.roosting_ZOOM_month^(-1/5) / 2
#   locs_spa.roosting_ZOOM_month <- as(GPS_spa.roosting_ZOOM_month, "Spatial")
#   
#   # KernelUD
#   kud.roosting_ZOOM_month <- kernelUD(locs_spa.roosting_ZOOM_month["month_label"], 
#                                       grid = SpatialPixels_ZOOM, 
#                                       h = mean(c(h.silverman_x_roosting_ZOOM_month, 
#                                                  h_silverman_y_roosting_ZOOM_month)))
#   
#   kud_list.roosting_ZOOM_month <- lapply(names(kud.roosting_ZOOM_month), function(month) {
#     
#     print(month)
#     
#     # Extraire l'estimation de densité pour un ID spécifique
#     kud_single.roosting_ZOOM_month <- kud.roosting_ZOOM_month[[month]]
#     rast.roosting_ZOOM_month <- rast(kud_single.roosting_ZOOM_month)
#     courtour.roosting_ZOOM_month <- as.contour(rast.roosting_ZOOM_month)
#     sf.roosting_ZOOM_month <- st_as_sf(courtour.roosting_ZOOM_month)
#     cast.roosting_ZOOM_month <- st_cast(sf.roosting_ZOOM_month, "POLYGON")
#     cast.roosting_ZOOM_month$month <- month
#     
#     return(cast.roosting_ZOOM_month)
#   })
#   
#   kud_all.roosting_ZOOM_month <- do.call(rbind, kud_list.roosting_ZOOM_month)
#   kud_all.roosting_ZOOM_month$month <- as.factor(kud_all.roosting_ZOOM_month$month)
#   kud_all.roosting_ZOOM_month$ZOOM <- lettre
#   results_kud.roosting_ZOOM_month <- rbind(results_kud.roosting_ZOOM_month, kud_all.roosting_ZOOM_month)
#   
# }
# 
# # write
# st_write(results_kud.roosting_ZOOM_month, paste0(data_generated_path, "results_kud.roosting_ZOOM_month.gpkg"), append = FALSE)
# # read
# results_kud.roosting_ZOOM_month <- st_read(file.path(data_generated_path, "results_kud.roosting_ZOOM_month.gpkg"))
# 
# # plot
# tmap_mode("view")
# UDMap_roosting_month_ZOOM <- tm_scalebar() +   tm_basemap(c("OpenStreetMap", "Esri.WorldImagery", "CartoDB.Positron")) +
#   tm_shape(RMO) +
#   tm_polygons() +
#   tm_text("NOM_SITE", size = 1) +
#   tm_shape(ZOOM_A) +
#   tm_polygons(fill_alpha = 0.1, fill = "grey") +
#   tm_text("A", size = 1.5) +
#   tm_shape(ZOOM_B) +
#   tm_polygons(fill_alpha = 0.1, fill = "grey") +
#   tm_text("B", size = 1.5) +
#   tm_shape(ZOOM_C) +
#   tm_polygons(fill_alpha = 0.1, fill = "grey") +
#   tm_text("C", size = 1.5) +
#   tm_shape(ZOOM_D) +
#   tm_polygons(fill_alpha = 0.1, fill = "grey") +
#   tm_text("D", size = 1.5) +
#   tm_shape(ZOOM_E) +
#   tm_polygons(fill_alpha = 0.1, fill = "grey") +
#   tm_text("E", size = 1.5) +
#   tm_shape(BOX_2154) +
#   tm_borders(col = "black") +
#   tm_shape(results_kud.roosting_ZOOM_month) + 
#   tm_facets("month") + 
#   tm_polygons(border.col = "grey", fill = "level", fill_alpha = 0.2, 
#               palette = viridis::viridis(10, begin = 0, end = 1, 
#                                          direction = 1, option = "plasma")) +
#   tm_facets("month") +
#   tm_shape(terre_mer) +
#   tm_lines(col = "lightblue", lwd = 0.1); UDMap_roosting_month_ZOOM
# 
# ###                        ###
# ### Repétabilité inter-month / population scale ###
# ###                        ###
# 
# GPS.month_repet_pop <- GPS %>% 
#   filter(behavior == "roosting") %>% 
#   dplyr::select(datetime,lon,lat,month_label) %>% 
#   st_drop_geometry() %>% 
#   na.omit()
# 
# # au moins 5 point par group
# n_per_month <- GPS.month_repet_pop %>% 
#   group_by(month_label) %>% 
#   summarize(n = n())%>% 
#   filter(n <= 5) #%>%
# # mutate(ID_month = paste0(ID, "_", month_label))
# 
# GPS.month_repet_pop <- GPS.month_repet_pop %>% 
#   filter(month_label %ni% n_per_month$month_label)
# 
# # Transformer en objet spatial (EPSG:4326)
# GPS_spa.month_repet_pop <- st_as_sf(GPS.month_repet_pop, coords = c("lon", "lat"), crs = 4326)
# GPS_spa.month_repet_pop <- st_transform(GPS_spa.month_repet_pop, crs = 32630)
# 
# # raster/grid
# crs_utm <- "EPSG:32630"
# SpatRaster <- project(raster_100x100, crs_utm)
# RasterLayer <- raster(SpatRaster)
# SpatialPixels <- as(RasterLayer, "SpatialPixels")
# 
# # Extraire les coordonnées reprojetées
# coords.month_repet_pop <- st_coordinates(GPS_spa.month_repet_pop)
# 
# # Règle de Silverman
# sigma_x.roosting_month_repet_pop <- sd(coords.month_repet_pop[,1])
# sigma_y_roosting_month_repet_pop <- sd(coords.month_repet_pop[,2])
# n.roosting_month_repet_pop <- nrow(GPS_spa.month_repet_pop)
# 
# h.silverman_x_roosting_month_repet_pop <- 1.06 * sigma_x.roosting_month_repet_pop * n.roosting_month_repet_pop^(-1/5) / 2 
# h.silverman_y_roosting_month_repet_pop <- 1.06 * sigma_y_roosting_month_repet_pop * n.roosting_month_repet_pop^(-1/5) / 2 
# 
# GPS_spa.month_repet_pop <- as(GPS_spa.month_repet_pop, "Spatial")
# 
# kud.roosting_month_repet_pop <- kernelUD(GPS_spa.month_repet_pop["month_label"], 
#                                          grid = as(SpatialPixels, "SpatialPixels"),
#                                          h = mean(c(h.silverman_x_roosting_month_repet_pop,
#                                                     h.silverman_y_roosting_month_repet_pop)))
# 
# ##                     ##
# ## valeur répétabilité ##
# ##                     ##
# 
# overlap.roosting_month_repet_pop <- kerneloverlaphr(kud.roosting_month_repet_pop, method = "BA")
# mean_overlap.roosting_month_repet_pop <- mean(overlap.roosting_month_repet_pop, na.rm = T) ; mean
# 
# # overlap_matrix
# min_val <- min(overlap.roosting_month_repet_pop, na.rm = TRUE)
# max_val <- max(overlap.roosting_month_repet_pop, na.rm = TRUE)
# ordre <- c("janv", "févr", "mars", "avr","mai","juin","juil","août","sept","oct","nov","déc")
# overlap.roosting_month_repet_pop <- overlap.roosting_month_repet_pop[ordre, ordre]
# 
# plot.overlapp_roosting_month_repet_pop <- ggcorrplot(overlap.roosting_month_repet_pop,
#                                                      hc.order = FALSE,
#                                                      method = "circle",
#                                                      type = "lower",
#                                                      lab = TRUE,
#                                                      digits = 1,
#                                                      colors = c("white", "yellow", "red"),
#                                                      ggtheme = theme_minimal()) +
#   scale_fill_gradientn(colors = c("white", "yellow", "red"),
#                        limits = c(min_val, 
#                                   max_val)) ; plot.overlapp_roosting_month_repet_pop
# 
# ##               ##
# ## UDMap par ind ##
# ##               ##
# 
# # Estimation UDmap par ind par month
# 
# # Créer une liste pour stocker les résultats
# UDmaps_list.roosting_ZOOM_month <- lapply(names(kud.roosting_month_repet_pop), function(Individu_Periode) {
#   
#   print(Individu_Periode)
#   
#   # Extraire l'estimation de densité pour un ID spécifique
#   kud_single.roosting_ZOOM_month <- kud.roosting_month_repet_pop[[Individu_Periode]]
#   rast.roosting_ZOOM_month <- rast(kud_single.roosting_ZOOM_month)
#   contour.roosting_ZOOM_month <- as.contour(rast.roosting_ZOOM_month)
#   sf.roosting_ZOOM_month <- st_as_sf(contour.roosting_ZOOM_month)
#   cast.roosting_ZOOM_month <- st_cast(sf.roosting_ZOOM_month, "POLYGON")
#   cast.roosting_ZOOM_month$Individu_Periode <- Individu_Periode
#   
#   return(cast.roosting_ZOOM_month)
#   
# })
# 
# # Fusionner tous les ID dans un seul objet sf
# results_kud.roosting_ZOOM_month <- do.call(rbind, UDmaps_list.roosting_ZOOM_month)
# results_kud.roosting_ZOOM_month$Individu_Periode <- as.factor(results_kud.roosting_ZOOM_month$Individu_Periode)
# results_kud.roosting_ZOOM_month$ID <- sub("_.*", "", results_kud.roosting_ZOOM_month$Individu_Periode)
# results_kud.roosting_ZOOM_month$Individu_Periode <- droplevels(results_kud.roosting_ZOOM_month$Individu_Periode)
# results_kud.roosting_ZOOM_month$Periode <- sub(".*_", "", results_kud.roosting_ZOOM_month$Individu_Periode)
# results_kud.roosting_ZOOM_month$ID <- as.factor(results_kud.roosting_ZOOM_month$ID)
# 
# # plot 
# tmap_mode("view")
# 
# UDMap_roosting_rep_inter_month <- tm_shape(RMO) +
#   tm_polygons() +
#   tm_text("NOM_SITE", size = 1) +
#   tm_shape(results_kud.roosting_ZOOM_month) + 
#   tm_facets("ID") +
#   tm_polygons(border.col = "grey", fill = "Periode", fillfill_alpha = 0.2) ; UDMap_roosting_rep_inter_month
# 
# 
# ###                        ###
# ### Repétabilité inter-month / individual scale ###
# ###                        ###
# 
# GPS.month_repet <- GPS %>% 
#   filter(behavior == "roosting") %>% 
#   dplyr::select(ID,datetime,lon,lat,month_label) %>% 
#   mutate(ID_month = paste0(ID, "_", month_label)) %>% 
#   st_drop_geometry() %>% 
#   na.omit()
# 
# # au moins 5 point par group
# n_per_month <- GPS.month_repet %>% 
#   group_by(ID_month) %>% 
#   summarize(n = n())%>% 
#   filter(n <= 5) #%>%
# # mutate(ID_month = paste0(ID, "_", month_label))
# 
# GPS.month_repet <- GPS.month_repet %>% 
#   filter(ID_month %ni% n_per_month$ID_month)
# 
# # Transformer en objet spatial (EPSG:4326)
# GPS_spa.month_repet <- st_as_sf(GPS.month_repet, coords = c("lon", "lat"), crs = 4326)
# GPS_spa.month_repet <- st_transform(GPS_spa.month_repet, crs = 32630)
# 
# # raster/grid
# crs_utm <- "EPSG:32630"
# SpatRaster <- project(raster_100x100, crs_utm)
# RasterLayer <- raster(SpatRaster)
# SpatialPixels <- as(RasterLayer, "SpatialPixels")
# 
# # Extraire les coordonnées reprojetées
# coords.month_repet <- st_coordinates(GPS_spa.month_repet)
# 
# # Règle de Silverman
# sigma_x.roosting_month_repet <- sd(coords.month_repet[,1])
# sigma_y_roosting_month_repet <- sd(coords.month_repet[,2])
# n.roosting_month_repet <- nrow(GPS_spa.month_repet)
# 
# h.silverman_x_roosting_month_repet <- 1.06 * sigma_x.roosting_month_repet * n.roosting_month_repet^(-1/5) / 2 
# h.silverman_y_roosting_month_repet <- 1.06 * sigma_y_roosting_month_repet * n.roosting_month_repet^(-1/5) / 2 
# 
# GPS_spa.month_repet <- as(GPS_spa.month_repet, "Spatial")
# 
# kud.roosting_month_repet <- kernelUD(GPS_spa.month_repet["ID_month"], 
#                                      grid = as(SpatialPixels, "SpatialPixels"),
#                                      h = mean(c(h.silverman_x_roosting_month_repet,
#                                                 h.silverman_y_roosting_month_repet)))
# 
# ##                     ##
# ## valeur répétabilité ##
# ##                     ##
# 
# # Estimation valeur d'overlapp par ind entre chaque month
# 
# # Extraire les noms uniques des individus
# individus <- unique(GPS_spa.month_repet$ID)
# 
# # Stocker les résultats
# overlap_results.roosting_month_repet = NULL
# 
# # Boucle sur chaque individu
# for (ind in individus) {
#   
#   print(ind)
#   
#   # Trouver les noms des périodes de cet individu dans hr_kde
#   ID_periodes <- names(kud.roosting_month_repet)[grep(paste0("^", ind, "_"), names(kud.roosting_month_repet))]
#   
#   # Vérifier que l'individu a bien deux périodes
#   # if (length(ID_periodes) >= 2) {
#   # Créer un estUDm valide
#   hr_kde_ind.roosting_month_repet <- kud.roosting_month_repet[ID_periodes]
#   class(hr_kde_ind.roosting_month_repet) <- "estUDm"  # Important pour que kerneloverlaphr() fonctionne
#   
#   # Calculer l'overlap entre les deux périodes
#   overlap_value.roosting_month_repet <- kerneloverlaphr(hr_kde_ind.roosting_month_repet, 
#                                                         method = "BA")[1, 2]
#   
#   info_ind.roosting_month_repet <- c(ind, overlap_value.roosting_month_repet)
#   
#   # Stocker le résultat
#   # overlap_results <- rbind(overlap_results, data.frame(Individu = ind, Overlap = overlap_value))
#   overlap_results.roosting_month_repet <- rbind(overlap_results.roosting_month_repet, info_ind.roosting_month_repet)
#   
#   # }
# }
# 
# overlap_results.roosting_month_repet <- as.data.frame(overlap_results.roosting_month_repet)
# 
# overlap_results.roosting_month_repet <- overlap_results.roosting_month_repet %>% 
#   rename(ID = V1, overlap = V2)
# 
# overlap_results.roosting_month_repet$overlap <- as.numeric(overlap_results.roosting_month_repet$overlap)
# 
# mean_overlap.roosting_month_repet <- mean(overlap_results.roosting_month_repet$overlap, na.rm = T) ; mean_overlap.roosting_month_repet
# 
# # Afficher les résultats
# overlap_results.roosting_month_repet <- overlap_results.roosting_month_repet[order(overlap_results.roosting_month_repet$overlap), ] ; overlap_results.roosting_month_repet
# 
# # plot
# plot.roosting_month_repet <- ggplot(overlap_results.roosting_month_repet, aes(x=reorder(ID, overlap), y=overlap)) + 
#   geom_point(shape = 19, size = 4) +
#   theme_classic() +
#   coord_flip() +
#   theme(legend.position = "top") +
#   scale_fill_manual() +
#   labs(title="",
#        x ="Individu", y = "Pourcentage d'overlap inter-mois"); plot.roosting_month_repet
# 
# ##               ##
# ## UDMap par ind ##
# ##               ##
# 
# # Estimation UDmap par ind par month
# 
# # Créer une liste pour stocker les résultats
# UDmaps_list.roosting_ZOOM_month <- lapply(names(kud.roosting_month_repet), function(Individu_Periode) {
#   
#   print(Individu_Periode)
#   
#   # Extraire l'estimation de densité pour un ID spécifique
#   kud_single.roosting_ZOOM_month <- kud.roosting_month_repet[[Individu_Periode]]
#   rast.roosting_ZOOM_month <- rast(kud_single.roosting_ZOOM_month)
#   contour.roosting_ZOOM_month <- as.contour(rast.roosting_ZOOM_month)
#   sf.roosting_ZOOM_month <- st_as_sf(contour.roosting_ZOOM_month)
#   cast.roosting_ZOOM_month <- st_cast(sf.roosting_ZOOM_month, "POLYGON")
#   cast.roosting_ZOOM_month$Individu_Periode <- Individu_Periode
#   
#   return(cast.roosting_ZOOM_month)
#   
# })
# 
# # Fusionner tous les ID dans un seul objet sf
# results_kud.roosting_ZOOM_month <- do.call(rbind, UDmaps_list.roosting_ZOOM_month)
# results_kud.roosting_ZOOM_month$Individu_Periode <- as.factor(results_kud.roosting_ZOOM_month$Individu_Periode)
# results_kud.roosting_ZOOM_month$ID <- sub("_.*", "", results_kud.roosting_ZOOM_month$Individu_Periode)
# results_kud.roosting_ZOOM_month$Individu_Periode <- droplevels(results_kud.roosting_ZOOM_month$Individu_Periode)
# results_kud.roosting_ZOOM_month$Periode <- sub(".*_", "", results_kud.roosting_ZOOM_month$Individu_Periode)
# results_kud.roosting_ZOOM_month$ID <- as.factor(results_kud.roosting_ZOOM_month$ID)
# 
# # plot 
# tmap_mode("view")
# 
# UDMap_roosting_rep_inter_month <- tm_shape(RMO) +
#   tm_polygons() +
#   tm_text("NOM_SITE", size = 1) +
#   tm_shape(results_kud.roosting_ZOOM_month) + 
#   tm_facets("ID") +
#   tm_polygons(border.col = "grey", fill = "Periode", fillfill_alpha = 0.2) ; UDMap_roosting_rep_inter_month
# 
# 
# tm_layout(legend.outside = TRUE, legend.show = TRUE); UDMap_roosting_rep_inter_year
# 
# ## # # # # # --- 
## alimentation  ---------------------------------------------------------------
# ## # # # # # --- 
# 
# 
# 
# crs_utm <- "EPSG:32630"
# ZOOM <- c("A","B","C","D","E")
# results_kud.foraging_ZOOM_month = NULL
# 
# # lettre = "B"
# 
# for (lettre in ZOOM){
#   # in ZOOM
#   ZOOM <- st_read(paste0(data_generated_path,"ZOOM_",lettre,".gpkg"))
#   ZOOM <- st_transform(ZOOM, crs = 4326)
#   GPS.ZOOM <- st_intersection(GPS, ZOOM) 
#   GPS.foraging_ZOOM_month <- GPS.ZOOM %>% 
#     filter(behavior == "foraging") %>% 
#     dplyr::select(lon,lat,month_label) %>% 
#     st_drop_geometry() %>% 
#     na.omit()
#   
#   if (nrow(GPS.foraging_ZOOM_month) == 0) {
#     next  # Passe directement à l'itération suivante
#   }
#   
#   nb_row <- GPS.foraging_ZOOM_month %>% 
#     group_by(month_label) %>%
#     summarise(n = n(), .groups = "drop")
#   
#   if (min(nb_row$n) < 5) {
#     next  # Passe directement à l'itération suivante
#   }
#   
#   # Crée une table avec tous les mois possibles
#   all_months <- tibble(
#     month_label = c("janv", "févr", "mars", "avr", "mai", "juin",
#                     "juil", "août", "sept", "oct", "nov", "déc")
#   )
#   
#   # Compte les occurrences par mois dans tes données
#   nb_row <- GPS.foraging_ZOOM_month %>%
#     group_by(month_label) %>%
#     summarise(n = n(), .groups = "drop")
#   
#   # Joint tous les mois et remplit avec 0 si manquant
#   nb_row_complet <- all_months %>%
#     left_join(nb_row, by = "month_label") %>%
#     mutate(n = if_else(is.na(n), 0L, n))
#   
#   if (min(nb_row_complet$n) < 5) {
#     next  # Passe directement à l'itération suivante
#   }
#   
#   GPS_spa.foraging_ZOOM_month <- st_as_sf(GPS.foraging_ZOOM_month, coords = c("lon", "lat"), crs = 4326)
#   GPS_spa.foraging_ZOOM_month <- st_transform(GPS_spa.foraging_ZOOM_month, crs = 32630) 
#   GPS_coods.foraging_ZOOM_month <- st_coordinates(GPS_spa.foraging_ZOOM_month)
#   
#   # raster/grid
#   grid_ZOOM <- st_read(paste0(data_generated_path, "grid_ZOOM_",lettre,".gpkg"))
#   raster_ZOOM <- rast(grid_ZOOM, resolution = resolution_ZOOM, crs="EPSG:2154")
#   SpatRaster_ZOOM <- project(raster_ZOOM, crs_utm)  
#   RasterLayer_ZOOM <- raster(SpatRaster_ZOOM) 
#   SpatialPixels_ZOOM <- as(RasterLayer_ZOOM, "SpatialPixels")
#   
#   # Règle de Silverman
#   sigma_x.foraging_ZOOM_month <- sd(GPS_coods.foraging_ZOOM_month[,1]) 
#   sigma_y.foraging_ZOOM_month <- sd(GPS_coods.foraging_ZOOM_month[,2]) 
#   n.foraging_ZOOM_month <- nrow(GPS.foraging_ZOOM_month)  
#   h.silverman_x_foraging_ZOOM_month <- 1.06 * sigma_x.foraging_ZOOM_month * n.foraging_ZOOM_month^(-1/5) / 2
#   h_silverman_y_foraging_ZOOM_month <- 1.06 * sigma_y.foraging_ZOOM_month * n.foraging_ZOOM_month^(-1/5) / 2
#   locs_spa.foraging_ZOOM_month <- as(GPS_spa.foraging_ZOOM_month, "Spatial")
#   
#   # KernelUD
#   kud.foraging_ZOOM_month <- kernelUD(locs_spa.foraging_ZOOM_month["month_label"], 
#                                       grid = SpatialPixels_ZOOM, 
#                                       h = mean(c(h.silverman_x_foraging_ZOOM_month, 
#                                                  h_silverman_y_foraging_ZOOM_month)))
#   
#   kud_list.foraging_ZOOM_month <- lapply(names(kud.foraging_ZOOM_month), function(month) {
#     
#     print(month)
#     
#     # Extraire l'estimation de densité pour un ID spécifique
#     kud_single.foraging_ZOOM_month <- kud.foraging_ZOOM_month[[month]]
#     rast.foraging_ZOOM_month <- rast(kud_single.foraging_ZOOM_month)
#     courtour.foraging_ZOOM_month <- as.contour(rast.foraging_ZOOM_month)
#     sf.foraging_ZOOM_month <- st_as_sf(courtour.foraging_ZOOM_month)
#     cast.foraging_ZOOM_month <- st_cast(sf.foraging_ZOOM_month, "POLYGON")
#     cast.foraging_ZOOM_month$month <- month
#     
#     return(cast.foraging_ZOOM_month)
#   })
#   
#   kud_all.foraging_ZOOM_month <- do.call(rbind, kud_list.foraging_ZOOM_month)
#   kud_all.foraging_ZOOM_month$month <- as.factor(kud_all.foraging_ZOOM_month$month)
#   kud_all.foraging_ZOOM_month$ZOOM <- lettre
#   results_kud.foraging_ZOOM_month <- rbind(results_kud.foraging_ZOOM_month, kud_all.foraging_ZOOM_month)
#   
# }
# 
# # write
# st_write(results_kud.foraging_ZOOM_month, paste0(data_generated_path, "results_kud.foraging_ZOOM_month.gpkg"), append = FALSE)
# # read
# results_kud.foraging_ZOOM_month <- st_read(file.path(data_generated_path, "results_kud.foraging_ZOOM_month.gpkg"))
# 
# # plot
# tmap_mode("view")
# UDMap_foraging_month_ZOOM <- tm_scalebar() +   tm_basemap(c("OpenStreetMap", "Esri.WorldImagery", "CartoDB.Positron")) +
#   tm_shape(RMO) +
#   tm_polygons() +
#   tm_text("NOM_SITE", size = 1) +
#   tm_shape(ZOOM_A) +
#   tm_polygons(fill_alpha = 0.1, fill = "grey") +
#   tm_text("A", size = 1.5) +
#   tm_shape(ZOOM_B) +
#   tm_polygons(fill_alpha = 0.1, fill = "grey") +
#   tm_text("B", size = 1.5) +
#   tm_shape(ZOOM_C) +
#   tm_polygons(fill_alpha = 0.1, fill = "grey") +
#   tm_text("C", size = 1.5) +
#   tm_shape(ZOOM_D) +
#   tm_polygons(fill_alpha = 0.1, fill = "grey") +
#   tm_text("D", size = 1.5) +
#   tm_shape(ZOOM_E) +
#   tm_polygons(fill_alpha = 0.1, fill = "grey") +
#   tm_text("E", size = 1.5) +
#   tm_shape(BOX_2154) +
#   tm_borders(col = "black") +
#   tm_shape(results_kud.foraging_ZOOM_month) + 
#   tm_facets("month") + 
#   tm_polygons(border.col = "grey", fill = "level", fill_alpha = 0.2, 
#               palette = viridis::viridis(10, begin = 0, end = 1, 
#                                          direction = 1, option = "plasma")) +
#   tm_facets("month") +
#   tm_shape(terre_mer) +
#   tm_lines(col = "lightblue", lwd = 0.1); UDMap_foraging_month_ZOOM
# 
# ###                        ###
# ### Repétabilité inter-month / population scale ###
# ###                        ###
# 
# GPS.month_repet_pop <- GPS %>% 
#   filter(behavior == "foraging") %>% 
#   dplyr::select(datetime,lon,lat,month_label) %>% 
#   st_drop_geometry() %>% 
#   na.omit()
# 
# # au moins 5 point par group
# n_per_month <- GPS.month_repet_pop %>% 
#   group_by(month_label) %>% 
#   summarize(n = n())%>% 
#   filter(n <= 5) #%>%
# # mutate(ID_month = paste0(ID, "_", month_label))
# 
# GPS.month_repet_pop <- GPS.month_repet_pop %>% 
#   filter(month_label %ni% n_per_month$month_label)
# 
# # Transformer en objet spatial (EPSG:4326)
# GPS_spa.month_repet_pop <- st_as_sf(GPS.month_repet_pop, coords = c("lon", "lat"), crs = 4326)
# GPS_spa.month_repet_pop <- st_transform(GPS_spa.month_repet_pop, crs = 32630)
# 
# # raster/grid
# crs_utm <- "EPSG:32630"
# SpatRaster <- project(raster_100x100, crs_utm)
# RasterLayer <- raster(SpatRaster)
# SpatialPixels <- as(RasterLayer, "SpatialPixels")
# 
# # Extraire les coordonnées reprojetées
# coords.month_repet_pop <- st_coordinates(GPS_spa.month_repet_pop)
# 
# # Règle de Silverman
# sigma_x.foraging_month_repet_pop <- sd(coords.month_repet_pop[,1])
# sigma_y_foraging_month_repet_pop <- sd(coords.month_repet_pop[,2])
# n.foraging_month_repet_pop <- nrow(GPS_spa.month_repet_pop)
# 
# h.silverman_x_foraging_month_repet_pop <- 1.06 * sigma_x.foraging_month_repet_pop * n.foraging_month_repet_pop^(-1/5) / 2 
# h.silverman_y_foraging_month_repet_pop <- 1.06 * sigma_y_foraging_month_repet_pop * n.foraging_month_repet_pop^(-1/5) / 2 
# 
# GPS_spa.month_repet_pop <- as(GPS_spa.month_repet_pop, "Spatial")
# 
# kud.foraging_month_repet_pop <- kernelUD(GPS_spa.month_repet["month_label"], 
#                                          grid = as(SpatialPixels, "SpatialPixels"),
#                                          h = mean(c(h.silverman_x_foraging_month_repet_pop,
#                                                     h.silverman_y_foraging_month_repet_pop)))
# 
# ##                     ##
# ## valeur répétabilité ##
# ##                     ##
# 
# overlap.foraging_month_repet_pop <- kerneloverlaphr(kud.foraging_month_repet_pop, method = "BA")
# mean_overlap.foraging_month_repet_pop <- mean(overlap.foraging_month_repet_pop, na.rm = T) ; mean
# 
# # overlap_matrix
# min_val <- min(overlap.foraging_month_repet_pop, na.rm = TRUE)
# max_val <- max(overlap.foraging_month_repet_pop, na.rm = TRUE)
# ordre <- c("janv", "févr", "mars", "avr","mai","juin","juil","août","sept","oct","nov","déc")
# overlap.foraging_month_repet_pop <- overlap.foraging_month_repet_pop[ordre, ordre]
# 
# plot.overlapp_foraging_month_repet_pop <- ggcorrplot(overlap.foraging_month_repet_pop,
#                                                      hc.order = FALSE,
#                                                      method = "circle",
#                                                      type = "lower",
#                                                      lab = TRUE,
#                                                      digits = 1,
#                                                      colors = c("white", "yellow", "red"),
#                                                      ggtheme = theme_minimal()) +
#   scale_fill_gradientn(colors = c("white", "yellow", "red"),
#                        limits = c(min_val, 
#                                   max_val)) ; plot.overlapp_foraging_month_repet_pop
# 
# ##               ##
# ## UDMap par ind ##
# ##               ##
# 
# # Estimation UDmap par ind par month
# 
# # Créer une liste pour stocker les résultats
# UDmaps_list.foraging_ZOOM_month <- lapply(names(kud.foraging_month_repet_pop), function(Individu_Periode) {
#   
#   print(Individu_Periode)
#   
#   # Extraire l'estimation de densité pour un ID spécifique
#   kud_single.foraging_ZOOM_month <- kud.foraging_month_repet_pop[[Individu_Periode]]
#   rast.foraging_ZOOM_month <- rast(kud_single.foraging_ZOOM_month)
#   contour.foraging_ZOOM_month <- as.contour(rast.foraging_ZOOM_month)
#   sf.foraging_ZOOM_month <- st_as_sf(contour.foraging_ZOOM_month)
#   cast.foraging_ZOOM_month <- st_cast(sf.foraging_ZOOM_month, "POLYGON")
#   cast.foraging_ZOOM_month$Individu_Periode <- Individu_Periode
#   
#   return(cast.foraging_ZOOM_month)
#   
# })
# 
# # Fusionner tous les ID dans un seul objet sf
# results_kud.foraging_ZOOM_month <- do.call(rbind, UDmaps_list.foraging_ZOOM_month)
# results_kud.foraging_ZOOM_month$Individu_Periode <- as.factor(results_kud.foraging_ZOOM_month$Individu_Periode)
# results_kud.foraging_ZOOM_month$ID <- sub("_.*", "", results_kud.foraging_ZOOM_month$Individu_Periode)
# results_kud.foraging_ZOOM_month$Individu_Periode <- droplevels(results_kud.foraging_ZOOM_month$Individu_Periode)
# results_kud.foraging_ZOOM_month$Periode <- sub(".*_", "", results_kud.foraging_ZOOM_month$Individu_Periode)
# results_kud.foraging_ZOOM_month$ID <- as.factor(results_kud.foraging_ZOOM_month$ID)
# 
# # plot 
# tmap_mode("view")
# 
# UDMap_foraging_rep_inter_month <- tm_shape(RMO) +
#   tm_polygons() +
#   tm_text("NOM_SITE", size = 1) +
#   tm_shape(results_kud.foraging_ZOOM_month) + 
#   tm_facets("ID") +
#   tm_polygons(border.col = "grey", fill = "Periode", fill_alpha = 0.2) ; UDMap_foraging_rep_inter_month
# 
# 
# ###                        ###
# ### Repétabilité inter-month / individual scale ###
# ###                        ###
# 
# GPS.month_repet <- GPS %>% 
#   filter(behavior == "foraging") %>% 
#   dplyr::select(ID,datetime,lon,lat,month_label) %>% 
#   mutate(ID_month = paste0(ID, "_", month_label)) %>% 
#   st_drop_geometry() %>% 
#   na.omit()
# 
# # au moins 5 point par group
# n_per_month <- GPS.month_repet %>% 
#   group_by(ID_month) %>% 
#   summarize(n = n())%>% 
#   filter(n <= 5) #%>%
# # mutate(ID_month = paste0(ID, "_", month_label))
# 
# GPS.month_repet <- GPS.month_repet %>% 
#   filter(ID_month %ni% n_per_month$ID_month)
# 
# # Transformer en objet spatial (EPSG:4326)
# GPS_spa.month_repet <- st_as_sf(GPS.month_repet, coords = c("lon", "lat"), crs = 4326)
# GPS_spa.month_repet <- st_transform(GPS_spa.month_repet, crs = 32630)
# 
# # raster/grid
# crs_utm <- "EPSG:32630"
# SpatRaster <- project(raster_100x100, crs_utm)
# RasterLayer <- raster(SpatRaster)
# SpatialPixels <- as(RasterLayer, "SpatialPixels")
# 
# # Extraire les coordonnées reprojetées
# coords.month_repet <- st_coordinates(GPS_spa.month_repet)
# 
# # Règle de Silverman
# sigma_x.foraging_month_repet <- sd(coords.month_repet[,1])
# sigma_y_foraging_month_repet <- sd(coords.month_repet[,2])
# n.foraging_month_repet <- nrow(GPS_spa.month_repet)
# 
# h.silverman_x_foraging_month_repet <- 1.06 * sigma_x.foraging_month_repet * n.foraging_month_repet^(-1/5) / 2 
# h.silverman_y_foraging_month_repet <- 1.06 * sigma_y_foraging_month_repet * n.foraging_month_repet^(-1/5) / 2 
# 
# GPS_spa.month_repet <- as(GPS_spa.month_repet, "Spatial")
# 
# kud.foraging_month_repet <- kernelUD(GPS_spa.month_repet["ID_month"], 
#                                      grid = as(SpatialPixels, "SpatialPixels"),
#                                      h = mean(c(h.silverman_x_foraging_month_repet,
#                                                 h.silverman_y_foraging_month_repet)))
# 
# ##                     ##
# ## valeur répétabilité ##
# ##                     ##
# 
# # Estimation valeur d'overlapp par ind entre chaque month
# 
# # Extraire les noms uniques des individus
# individus <- unique(GPS_spa.month_repet$ID)
# 
# # Stocker les résultats
# overlap_results.foraging_month_repet = NULL
# 
# # Boucle sur chaque individu
# for (ind in individus) {
#   
#   print(ind)
#   
#   # Trouver les noms des périodes de cet individu dans hr_kde
#   ID_periodes <- names(kud.foraging_month_repet)[grep(paste0("^", ind, "_"), names(kud.foraging_month_repet))]
#   
#   # Vérifier que l'individu a bien deux périodes
#   # if (length(ID_periodes) >= 2) {
#   # Créer un estUDm valide
#   hr_kde_ind.foraging_month_repet <- kud.foraging_month_repet[ID_periodes]
#   class(hr_kde_ind.foraging_month_repet) <- "estUDm"  # Important pour que kerneloverlaphr() fonctionne
#   
#   # Calculer l'overlap entre les deux périodes
#   overlap_value.foraging_month_repet <- kerneloverlaphr(hr_kde_ind.foraging_month_repet, 
#                                                         method = "BA")[1, 2]
#   
#   info_ind.foraging_month_repet <- c(ind, overlap_value.foraging_month_repet)
#   
#   # Stocker le résultat
#   # overlap_results <- rbind(overlap_results, data.frame(Individu = ind, Overlap = overlap_value))
#   overlap_results.foraging_month_repet <- rbind(overlap_results.foraging_month_repet, info_ind.foraging_month_repet)
#   
#   # }
# }
# 
# overlap_results.foraging_month_repet <- as.data.frame(overlap_results.foraging_month_repet)
# 
# overlap_results.foraging_month_repet <- overlap_results.foraging_month_repet %>% 
#   rename(ID = V1, overlap = V2)
# 
# overlap_results.foraging_month_repet$overlap <- as.numeric(overlap_results.foraging_month_repet$overlap)
# 
# mean_overlap.foraging_month_repet <- mean(overlap_results.foraging_month_repet$overlap, na.rm = T) ; mean_overlap.foraging_month_repet
# 
# # Afficher les résultats
# overlap_results.foraging_month_repet <- overlap_results.foraging_month_repet[order(overlap_results.foraging_month_repet$overlap), ] ; overlap_results.foraging_month_repet
# 
# # plot
# plot.foraging_month_repet <- ggplot(overlap_results.foraging_month_repet, aes(x=reorder(ID, overlap), y=overlap)) + 
#   geom_point(shape = 19, size = 4) +
#   theme_classic() +
#   coord_flip() +
#   theme(legend.position = "top") +
#   scale_fill_manual() +
#   labs(title="",
#        x ="Individu", y = "Pourcentage d'overlap inter-mois"); plot.foraging_month_repet
# 
# ##               ##
# ## UDMap par ind ##
# ##               ##
# 
# # Estimation UDmap par ind par month
# 
# # Créer une liste pour stocker les résultats
# UDmaps_list.foraging_ZOOM_month <- lapply(names(kud.foraging_month_repet), function(Individu_Periode) {
#   
#   print(Individu_Periode)
#   
#   # Extraire l'estimation de densité pour un ID spécifique
#   kud_single.foraging_ZOOM_month <- kud.foraging_month_repet[[Individu_Periode]]
#   rast.foraging_ZOOM_month <- rast(kud_single.foraging_ZOOM_month)
#   contour.foraging_ZOOM_month <- as.contour(rast.foraging_ZOOM_month)
#   sf.foraging_ZOOM_month <- st_as_sf(contour.foraging_ZOOM_month)
#   cast.foraging_ZOOM_month <- st_cast(sf.foraging_ZOOM_month, "POLYGON")
#   cast.foraging_ZOOM_month$Individu_Periode <- Individu_Periode
#   
#   return(cast.foraging_ZOOM_month)
#   
# })
# 
# # Fusionner tous les ID dans un seul objet sf
# results_kud.foraging_ZOOM_month <- do.call(rbind, UDmaps_list.foraging_ZOOM_month)
# results_kud.foraging_ZOOM_month$Individu_Periode <- as.factor(results_kud.foraging_ZOOM_month$Individu_Periode)
# results_kud.foraging_ZOOM_month$ID <- sub("_.*", "", results_kud.foraging_ZOOM_month$Individu_Periode)
# results_kud.foraging_ZOOM_month$Individu_Periode <- droplevels(results_kud.foraging_ZOOM_month$Individu_Periode)
# results_kud.foraging_ZOOM_month$Periode <- sub(".*_", "", results_kud.foraging_ZOOM_month$Individu_Periode)
# results_kud.foraging_ZOOM_month$ID <- as.factor(results_kud.foraging_ZOOM_month$ID)
# 
# # plot 
# tmap_mode("view")
# 
# UDMap_foraging_rep_inter_month <- tm_shape(RMO) +
#   tm_polygons() +
#   tm_text("NOM_SITE", size = 1) +
#   tm_shape(results_kud.foraging_ZOOM_month) + 
#   tm_facets("ID") +
#   tm_polygons(border.col = "grey", fill = "Periode", fill_alpha = 0.2) ; UDMap_foraging_rep_inter_month
# 
# ######################### ---
# Variabilité inter-hebdo -------------------------------------------------------
# ######################### ---
# 
# # ## # # # # # --- 
## reposoir  -------------------------------------------------------------------
# # ## # # # # # --- 
# # 
# # 
# # 
# # crs_utm <- "EPSG:32630"
# # ZOOM <- c("A","B","C","D","E")
# # results_kud.roosting_ZOOM_week = NULL
# # 
# # lettre = "B"
# # 
# # for (lettre in ZOOM){
# #   # in ZOOM
# #   ZOOM <- st_read(paste0(data_generated_path,"ZOOM_",lettre,".gpkg"))
# #   ZOOM <- st_transform(ZOOM, crs = 4326)
# #   GPS.ZOOM <- st_intersection(GPS, ZOOM) 
# #   GPS.roosting_ZOOM_week <- GPS.ZOOM %>% 
# #     filter(behavior == "roosting") %>% 
# #     dplyr::select(lon,lat,week) %>% 
# #     st_drop_geometry() %>% 
# #     na.omit()
# #   
# #   if (nrow(GPS.roosting_ZOOM_week) == 0) {
# #     next  # Passe directement à l'itération suivante
# #   }
# #   
# #   nb_row <- GPS.roosting_ZOOM_week %>% 
# #     group_by(week) %>%
# #     summarise(n = n(), .groups = "drop")
# #   
# #   if (min(nb_row$n) < 5) {
# #     next  # Passe directement à l'itération suivante
# #   }
# #   
# #   # Crée une table avec tous les mois possibles
# #   all_weeks <- tibble(
# #     week = c(1:56)
# #   )
# #   
# #   all_weeks$week <- as.double(all_weeks$week)
# #   
# #   # Compte les occurrences par mois dans tes données
# #   nb_row <- GPS.roosting_ZOOM_week %>%
# #     group_by(week) %>%
# #     summarise(n = n(), .groups = "drop")
# #   
# #   # Joint tous les mois et remplit avec 0 si manquant
# #   nb_row_complet <- all_weeks %>%
# #     left_join(nb_row, by = "week") %>%
# #     mutate(n = if_else(is.na(n), 0L, n))
# #   
# #   if (min(nb_row_complet$n) < 5) {
# #     next  # Passe directement à l'itération suivante
# #   }
# #   
# #   GPS_spa.roosting_ZOOM_week <- st_as_sf(GPS.roosting_ZOOM_week, coords = c("lon", "lat"), crs = 4326)
# #   GPS_spa.roosting_ZOOM_week <- st_transform(GPS_spa.roosting_ZOOM_week, crs = 32630) 
# #   GPS_coods.roosting_ZOOM_week <- st_coordinates(GPS_spa.roosting_ZOOM_week)
# #   
# #   # raster/grid
# #   grid_ZOOM <- st_read(paste0(data_generated_path, "grid_ZOOM_",lettre,".gpkg"))
# #   raster_ZOOM <- rast(grid_ZOOM, resolution = resolution_ZOOM, crs="EPSG:2154")
# #   SpatRaster_ZOOM <- project(raster_ZOOM, crs_utm)  
# #   RasterLayer_ZOOM <- raster(SpatRaster_ZOOM) 
# #   SpatialPixels_ZOOM <- as(RasterLayer_ZOOM, "SpatialPixels")
# #   
# #   # Règle de Silverman
# #   sigma_x.roosting_ZOOM_week <- sd(GPS_coods.roosting_ZOOM_week[,1]) 
# #   sigma_y.roosting_ZOOM_week <- sd(GPS_coods.roosting_ZOOM_week[,2]) 
# #   n.roosting_ZOOM_week <- nrow(GPS.roosting_ZOOM_week)  
# #   h.silverman_x_roosting_ZOOM_week <- 1.06 * sigma_x.roosting_ZOOM_week * n.roosting_ZOOM_week^(-1/5) / 2
# #   h_silverman_y_roosting_ZOOM_week <- 1.06 * sigma_y.roosting_ZOOM_week * n.roosting_ZOOM_week^(-1/5) / 2
# #   locs_spa.roosting_ZOOM_week <- as(GPS_spa.roosting_ZOOM_week, "Spatial")
# #   
# #   # KernelUD
# #   kud.roosting_ZOOM_week <- kernelUD(locs_spa.roosting_ZOOM_week["week"], 
# #                                       grid = SpatialPixels_ZOOM, 
# #                                       h = mean(c(h.silverman_x_roosting_ZOOM_week, 
# #                                                  h_silverman_y_roosting_ZOOM_week)))
# #   
# #   kud_list.roosting_ZOOM_week <- lapply(names(kud.roosting_ZOOM_week), function(week) {
# #     
# #     print(week)
# #     
# #     # Extraire l'estimation de densité pour un ID spécifique
# #     kud_single.roosting_ZOOM_week <- kud.roosting_ZOOM_week[[week]]
# #     rast.roosting_ZOOM_week <- rast(kud_single.roosting_ZOOM_week)
# #     courtour.roosting_ZOOM_week <- as.contour(rast.roosting_ZOOM_week)
# #     sf.roosting_ZOOM_week <- st_as_sf(courtour.roosting_ZOOM_week)
# #     cast.roosting_ZOOM_week <- st_cast(sf.roosting_ZOOM_week, "POLYGON")
# #     cast.roosting_ZOOM_week$week <- week
# #     
# #     return(cast.roosting_ZOOM_week)
# #   })
# #   
# #   kud_all.roosting_ZOOM_week <- do.call(rbind, kud_list.roosting_ZOOM_week)
# #   kud_all.roosting_ZOOM_week$week <- as.factor(kud_all.roosting_ZOOM_week$week)
# #   kud_all.roosting_ZOOM_week$ZOOM <- lettre
# #   results_kud.roosting_ZOOM_week <- rbind(results_kud.roosting_ZOOM_week, kud_all.roosting_ZOOM_week)
# #   
# # }
# # 
# # # write
# # st_write(results_kud.roosting_ZOOM_week, paste0(data_generated_path, "results_kud.roosting_ZOOM_week.gpkg"), append = FALSE)
# # # read
# # results_kud.roosting_ZOOM_week <- st_read(file.path(data_generated_path, "results_kud.roosting_ZOOM_week.gpkg"))
# # 
# # # plot
# # tmap_mode("view")
# # UDMap_roosting_week_ZOOM <- tm_scalebar() +   tm_basemap(c("OpenStreetMap", "Esri.WorldImagery", "CartoDB.Positron")) +
# #   tm_shape(RMO) +
# #   tm_polygons() +
# #   tm_text("NOM_SITE", size = 1) +
# #   tm_shape(ZOOM_A) +
# #   tm_polygons(fill_alpha = 0.1, fill = "grey") +
# #   tm_text("A", size = 1.5) +
# #   tm_shape(ZOOM_B) +
# #   tm_polygons(fill_alpha = 0.1, fill = "grey") +
# #   tm_text("B", size = 1.5) +
# #   tm_shape(ZOOM_C) +
# #   tm_polygons(fill_alpha = 0.1, fill = "grey") +
# #   tm_text("C", size = 1.5) +
# #   tm_shape(ZOOM_D) +
# #   tm_polygons(fill_alpha = 0.1, fill = "grey") +
# #   tm_text("D", size = 1.5) +
# #   tm_shape(ZOOM_E) +
# #   tm_polygons(fill_alpha = 0.1, fill = "grey") +
# #   tm_text("E", size = 1.5) +
# #   tm_shape(BOX_2154) +
# #   tm_borders(col = "black") +
# #   tm_shape(results_kud.roosting_ZOOM_week) + 
# #   tm_polygons(border.col = "grey", fill = "level", fill_alpha = 0.2, 
# #               palette = viridis::viridis(10, begin = 0, end = 1, 
# #                                          direction = 1, option = "plasma")) +
# #   tm_facets("week") +
# #   tm_shape(terre_mer) +
# #   tm_lines(col = "lightblue", lwd = 0.1); UDMap_roosting_week_ZOOM
# # 
# # ###                        ###
# # ### Repétabilité inter-week / population scale ###
# # ###                        ###
# # 
# # GPS.week_repet_pop <- GPS %>% 
# #   filter(behavior == "roosting") %>% 
# #   dplyr::select(datetime,lon,lat,week) %>% 
# #   st_drop_geometry() %>% 
# #   na.omit()
# # 
# # # au moins 5 point par group
# # n_per_week <- GPS.week_repet_pop %>% 
# #   group_by(week) %>% 
# #   summarize(n = n())%>% 
# #   filter(n <= 5) #%>%
# # # mutate(ID_week = paste0(ID, "_", week))
# # 
# # 
# # GPS.week_repet_pop <- GPS.week_repet_pop %>% 
# #   filter(week %ni% n_per_week$week)
# # 
# # # Transformer en objet spatial (EPSG:4326)
# # GPS_spa.week_repet_pop <- st_as_sf(GPS.week_repet_pop, coords = c("lon", "lat"), crs = 4326)
# # GPS_spa.week_repet_pop <- st_transform(GPS_spa.week_repet_pop, crs = 32630)
# # 
# # # raster/grid
# # crs_utm <- "EPSG:32630"
# # SpatRaster <- project(raster_100x100, crs_utm)
# # RasterLayer <- raster(SpatRaster)
# # SpatialPixels <- as(RasterLayer, "SpatialPixels")
# # 
# # # Extraire les coordonnées reprojetées
# # coords.week_repet_pop <- st_coordinates(GPS_spa.week_repet_pop)
# # 
# # # Règle de Silverman
# # sigma_x.roosting_week_repet_pop <- sd(coords.week_repet_pop[,1])
# # sigma_y_roosting_week_repet_pop <- sd(coords.week_repet_pop[,2])
# # n.roosting_week_repet_pop <- nrow(GPS_spa.week_repet_pop)
# # 
# # h.silverman_x_roosting_week_repet_pop <- 1.06 * sigma_x.roosting_week_repet_pop * n.roosting_week_repet_pop^(-1/5) / 2 
# # h.silverman_y_roosting_week_repet_pop <- 1.06 * sigma_y_roosting_week_repet_pop * n.roosting_week_repet_pop^(-1/5) / 2 
# # 
# # GPS_spa.week_repet_pop <- as(GPS_spa.week_repet_pop, "Spatial")
# # 
# # kud.roosting_week_repet_pop <- kernelUD(GPS_spa.week_repet_pop["week"], 
# #                                          grid = as(SpatialPixels, "SpatialPixels"),
# #                                          h = mean(c(h.silverman_x_roosting_week_repet_pop,
# #                                                     h.silverman_y_roosting_week_repet_pop)))
# # 
# # ##                     ##
# # ## valeur répétabilité ##
# # ##                     ##
# # 
# # overlap.roosting_week_repet_pop <- kerneloverlaphr(kud.roosting_week_repet_pop, method = "BA")
# # mean_overlap.roosting_week_repet_pop <- mean(overlap.roosting_week_repet_pop, na.rm = T) ; mean
# # 
# # # overlap_matrix
# # min_val <- min(overlap.roosting_week_repet_pop, na.rm = TRUE)
# # max_val <- max(overlap.roosting_week_repet_pop, na.rm = TRUE)
# # ordre <- c("janv", "févr", "mars", "avr","mai","juin","juil","août","sept","oct","nov","déc")
# # overlap.roosting_week_repet_pop <- overlap.roosting_week_repet_pop[ordre, ordre]
# # 
# # plot.overlapp_roosting_week_repet_pop <- ggcorrplot(overlap.roosting_week_repet_pop,
# #                                                      hc.order = FALSE,
# #                                                      method = "circle",
# #                                                      type = "lower",
# #                                                      lab = TRUE,
# #                                                      digits = 1,
# #                                                      colors = c("white", "yellow", "red"),
# #                                                      ggtheme = theme_minimal()) +
# #   scale_fill_gradientn(colors = c("white", "yellow", "red"),
# #                        limits = c(min_val, 
# #                                   max_val)) ; plot.overlapp_roosting_week_repet_pop
# # 
# # ##               ##
# # ## UDMap par ind ##
# # ##               ##
# # 
# # # Estimation UDmap par ind par week
# # 
# # # Créer une liste pour stocker les résultats
# # UDmaps_list.roosting_ZOOM_week <- lapply(names(kud.roosting_week_repet_pop), function(Individu_Periode) {
# #   
# #   print(Individu_Periode)
# #   
# #   # Extraire l'estimation de densité pour un ID spécifique
# #   kud_single.roosting_ZOOM_week <- kud.roosting_week_repet_pop[[Individu_Periode]]
# #   rast.roosting_ZOOM_week <- rast(kud_single.roosting_ZOOM_week)
# #   contour.roosting_ZOOM_week <- as.contour(rast.roosting_ZOOM_week)
# #   sf.roosting_ZOOM_week <- st_as_sf(contour.roosting_ZOOM_week)
# #   cast.roosting_ZOOM_week <- st_cast(sf.roosting_ZOOM_week, "POLYGON")
# #   cast.roosting_ZOOM_week$Individu_Periode <- Individu_Periode
# #   
# #   return(cast.roosting_ZOOM_week)
# #   
# # })
# # 
# # # Fusionner tous les ID dans un seul objet sf
# # results_kud.roosting_ZOOM_week <- do.call(rbind, UDmaps_list.roosting_ZOOM_week)
# # results_kud.roosting_ZOOM_week$Individu_Periode <- as.factor(results_kud.roosting_ZOOM_week$Individu_Periode)
# # results_kud.roosting_ZOOM_week$ID <- sub("_.*", "", results_kud.roosting_ZOOM_week$Individu_Periode)
# # results_kud.roosting_ZOOM_week$Individu_Periode <- droplevels(results_kud.roosting_ZOOM_week$Individu_Periode)
# # results_kud.roosting_ZOOM_week$Periode <- sub(".*_", "", results_kud.roosting_ZOOM_week$Individu_Periode)
# # results_kud.roosting_ZOOM_week$ID <- as.factor(results_kud.roosting_ZOOM_week$ID)
# # 
# # # plot 
# # tmap_mode("view")
# # 
# # UDMap_roosting_rep_inter_week <- tm_shape(RMO) +
# #   tm_polygons() +
# #   tm_text("NOM_SITE", size = 1) +
# #   tm_shape(results_kud.roosting_ZOOM_week) + 
# #   tm_facets("ID") +
# #   tm_polygons(border.col = "grey", fill = "Periode", fillfill_alpha = 0.2) ; UDMap_roosting_rep_inter_week
# # 
# # 
# # ###                        ###
# # ### Repétabilité inter-week / individual scale ###
# # ###                        ###
# # 
# # GPS.week_repet <- GPS %>% 
# #   filter(behavior == "roosting") %>% 
# #   dplyr::select(ID,datetime,lon,lat,week) %>% 
# #   mutate(ID_week = paste0(ID, "_", week)) %>% 
# #   st_drop_geometry() %>% 
# #   na.omit()
# # 
# # # au moins 5 point par group
# # n_per_week <- GPS.week_repet %>% 
# #   group_by(ID_week) %>% 
# #   summarize(n = n())%>% 
# #   filter(n <= 5) #%>%
# # # mutate(ID_week = paste0(ID, "_", week))
# # 
# # 
# # GPS.week_repet <- GPS.week_repet %>% 
# #   filter(ID_week %ni% n_per_week$ID_week)
# # 
# # # Transformer en objet spatial (EPSG:4326)
# # GPS_spa.week_repet <- st_as_sf(GPS.week_repet, coords = c("lon", "lat"), crs = 4326)
# # GPS_spa.week_repet <- st_transform(GPS_spa.week_repet, crs = 32630)
# # 
# # # raster/grid
# # crs_utm <- "EPSG:32630"
# # SpatRaster <- project(raster_100x100, crs_utm)
# # RasterLayer <- raster(SpatRaster)
# # SpatialPixels <- as(RasterLayer, "SpatialPixels")
# # 
# # # Extraire les coordonnées reprojetées
# # coords.week_repet <- st_coordinates(GPS_spa.week_repet)
# # 
# # # Règle de Silverman
# # sigma_x.roosting_week_repet <- sd(coords.week_repet[,1])
# # sigma_y_roosting_week_repet <- sd(coords.week_repet[,2])
# # n.roosting_week_repet <- nrow(GPS_spa.week_repet)
# # 
# # h.silverman_x_roosting_week_repet <- 1.06 * sigma_x.roosting_week_repet * n.roosting_week_repet^(-1/5) / 2 
# # h.silverman_y_roosting_week_repet <- 1.06 * sigma_y_roosting_week_repet * n.roosting_week_repet^(-1/5) / 2 
# # 
# # GPS_spa.week_repet <- as(GPS_spa.week_repet, "Spatial")
# # 
# # kud.roosting_week_repet <- kernelUD(GPS_spa.week_repet["ID_week"], 
# #                                      grid = as(SpatialPixels, "SpatialPixels"),
# #                                      h = mean(c(h.silverman_x_roosting_week_repet,
# #                                                 h.silverman_y_roosting_week_repet)))
# # 
# # ##                     ##
# # ## valeur répétabilité ##
# # ##                     ##
# # 
# # # Estimation valeur d'overlapp par ind entre chaque week
# # 
# # # Extraire les noms uniques des individus
# # individus <- unique(GPS_spa.week_repet$ID)
# # 
# # # Stocker les résultats
# # overlap_results.roosting_week_repet = NULL
# # 
# # # Boucle sur chaque individu
# # for (ind in individus) {
# #   
# #   print(ind)
# #   
# #   # Trouver les noms des périodes de cet individu dans hr_kde
# #   ID_periodes <- names(kud.roosting_week_repet)[grep(paste0("^", ind, "_"), names(kud.roosting_week_repet))]
# #   
# #   # Vérifier que l'individu a bien deux périodes
# #   # if (length(ID_periodes) >= 2) {
# #   # Créer un estUDm valide
# #   hr_kde_ind.roosting_week_repet <- kud.roosting_week_repet[ID_periodes]
# #   class(hr_kde_ind.roosting_week_repet) <- "estUDm"  # Important pour que kerneloverlaphr() fonctionne
# #   
# #   # Calculer l'overlap entre les deux périodes
# #   overlap_value.roosting_week_repet <- kerneloverlaphr(hr_kde_ind.roosting_week_repet, 
# #                                                         method = "BA")[1, 2]
# #   
# #   info_ind.roosting_week_repet <- c(ind, overlap_value.roosting_week_repet)
# #   
# #   # Stocker le résultat
# #   # overlap_results <- rbind(overlap_results, data.frame(Individu = ind, Overlap = overlap_value))
# #   overlap_results.roosting_week_repet <- rbind(overlap_results.roosting_week_repet, info_ind.roosting_week_repet)
# #   
# #   # }
# # }
# # 
# # overlap_results.roosting_week_repet <- as.data.frame(overlap_results.roosting_week_repet)
# # 
# # overlap_results.roosting_week_repet <- overlap_results.roosting_week_repet %>% 
# #   rename(ID = V1, overlap = V2)
# # 
# # overlap_results.roosting_week_repet$overlap <- as.numeric(overlap_results.roosting_week_repet$overlap)
# # 
# # mean_overlap.roosting_week_repet <- mean(overlap_results.roosting_week_repet$overlap, na.rm = T) ; mean_overlap.roosting_week_repet
# # 
# # # Afficher les résultats
# # overlap_results.roosting_week_repet <- overlap_results.roosting_week_repet[order(overlap_results.roosting_week_repet$overlap), ] ; overlap_results.roosting_week_repet
# # 
# # # plot
# # plot.roosting_week_repet <- ggplot(overlap_results.roosting_week_repet, aes(x=reorder(ID, overlap), y=overlap)) + 
# #   geom_point(shape = 19, size = 4) +
# #   theme_classic() +
# #   coord_flip() +
# #   theme(legend.position = "top") +
# #   scale_fill_manual() +
# #   labs(title="",
# #        x ="Individu", y = "Pourcentage d'overlap inter-mois"); plot.roosting_week_repet
# # 
# # ##               ##
# # ## UDMap par ind ##
# # ##               ##
# # 
# # # Estimation UDmap par ind par week
# # 
# # # Créer une liste pour stocker les résultats
# # UDmaps_list.roosting_ZOOM_week <- lapply(names(kud.roosting_week_repet), function(Individu_Periode) {
# #   
# #   print(Individu_Periode)
# #   
# #   # Extraire l'estimation de densité pour un ID spécifique
# #   kud_single.roosting_ZOOM_week <- kud.roosting_week_repet[[Individu_Periode]]
# #   rast.roosting_ZOOM_week <- rast(kud_single.roosting_ZOOM_week)
# #   contour.roosting_ZOOM_week <- as.contour(rast.roosting_ZOOM_week)
# #   sf.roosting_ZOOM_week <- st_as_sf(contour.roosting_ZOOM_week)
# #   cast.roosting_ZOOM_week <- st_cast(sf.roosting_ZOOM_week, "POLYGON")
# #   cast.roosting_ZOOM_week$Individu_Periode <- Individu_Periode
# #   
# #   return(cast.roosting_ZOOM_week)
# #   
# # })
# # 
# # # Fusionner tous les ID dans un seul objet sf
# # results_kud.roosting_ZOOM_week <- do.call(rbind, UDmaps_list.roosting_ZOOM_week)
# # results_kud.roosting_ZOOM_week$Individu_Periode <- as.factor(results_kud.roosting_ZOOM_week$Individu_Periode)
# # results_kud.roosting_ZOOM_week$ID <- sub("_.*", "", results_kud.roosting_ZOOM_week$Individu_Periode)
# # results_kud.roosting_ZOOM_week$Individu_Periode <- droplevels(results_kud.roosting_ZOOM_week$Individu_Periode)
# # results_kud.roosting_ZOOM_week$Periode <- sub(".*_", "", results_kud.roosting_ZOOM_week$Individu_Periode)
# # results_kud.roosting_ZOOM_week$ID <- as.factor(results_kud.roosting_ZOOM_week$ID)
# # 
# # # plot 
# # tmap_mode("view")
# # 
# # UDMap_roosting_rep_inter_week <- tm_shape(RMO) +
# #   tm_polygons() +
# #   tm_text("NOM_SITE", size = 1) +
# #   tm_shape(results_kud.roosting_ZOOM_week) + 
# #   tm_facets("ID") +
# #   tm_polygons(border.col = "grey", fill = "Periode", fillfill_alpha = 0.2) ; UDMap_roosting_rep_inter_week
# # 
# # 
# # tm_layout(legend.outside = TRUE, legend.show = TRUE); UDMap_roosting_rep_inter_year
# # 
# # ## # # # # # --- 
## alimentation  ---------------------------------------------------------------
# # ## # # # # # --- 
# # 
# # 
# # 
# # crs_utm <- "EPSG:32630"
# # ZOOM <- c("A","B","C","D","E")
# # results_kud.foraging_ZOOM_week = NULL
# # 
# # # lettre = "B"
# # 
# # for (lettre in ZOOM){
# #   # in ZOOM
# #   ZOOM <- st_read(paste0(data_generated_path,"ZOOM_",lettre,".gpkg"))
# #   ZOOM <- st_transform(ZOOM, crs = 4326)
# #   GPS.ZOOM <- st_intersection(GPS, ZOOM) 
# #   GPS.foraging_ZOOM_week <- GPS.ZOOM %>% 
# #     filter(behavior == "foraging") %>% 
# #     dplyr::select(lon,lat,week) %>% 
# #     st_drop_geometry() %>% 
# #     na.omit()
# #   
# #   if (nrow(GPS.foraging_ZOOM_week) == 0) {
# #     next  # Passe directement à l'itération suivante
# #   }
# #   
# #   nb_row <- GPS.foraging_ZOOM_week %>% 
# #     group_by(week) %>%
# #     summarise(n = n(), .groups = "drop")
# #   
# #   if (min(nb_row$n) < 5) {
# #     next  # Passe directement à l'itération suivante
# #   }
# #   
# #   # Crée une table avec tous les mois possibles
# #   all_weeks <- tibble(
# #     week = c("janv", "févr", "mars", "avr", "mai", "juin",
# #                     "juil", "août", "sept", "oct", "nov", "déc")
# #   )
# #   
# #   # Compte les occurrences par mois dans tes données
# #   nb_row <- GPS.foraging_ZOOM_week %>%
# #     group_by(week) %>%
# #     summarise(n = n(), .groups = "drop")
# #   
# #   # Joint tous les mois et remplit avec 0 si manquant
# #   nb_row_complet <- all_weeks %>%
# #     left_join(nb_row, by = "week") %>%
# #     mutate(n = if_else(is.na(n), 0L, n))
# #   
# #   if (min(nb_row_complet$n) < 5) {
# #     next  # Passe directement à l'itération suivante
# #   }
# #   
# #   GPS_spa.foraging_ZOOM_week <- st_as_sf(GPS.foraging_ZOOM_week, coords = c("lon", "lat"), crs = 4326)
# #   GPS_spa.foraging_ZOOM_week <- st_transform(GPS_spa.foraging_ZOOM_week, crs = 32630) 
# #   GPS_coods.foraging_ZOOM_week <- st_coordinates(GPS_spa.foraging_ZOOM_week)
# #   
# #   # raster/grid
# #   grid_ZOOM <- st_read(paste0(data_generated_path, "grid_ZOOM_",lettre,".gpkg"))
# #   raster_ZOOM <- rast(grid_ZOOM, resolution = resolution_ZOOM, crs="EPSG:2154")
# #   SpatRaster_ZOOM <- project(raster_ZOOM, crs_utm)  
# #   RasterLayer_ZOOM <- raster(SpatRaster_ZOOM) 
# #   SpatialPixels_ZOOM <- as(RasterLayer_ZOOM, "SpatialPixels")
# #   
# #   # Règle de Silverman
# #   sigma_x.foraging_ZOOM_week <- sd(GPS_coods.foraging_ZOOM_week[,1]) 
# #   sigma_y.foraging_ZOOM_week <- sd(GPS_coods.foraging_ZOOM_week[,2]) 
# #   n.foraging_ZOOM_week <- nrow(GPS.foraging_ZOOM_week)  
# #   h.silverman_x_foraging_ZOOM_week <- 1.06 * sigma_x.foraging_ZOOM_week * n.foraging_ZOOM_week^(-1/5) / 2
# #   h_silverman_y_foraging_ZOOM_week <- 1.06 * sigma_y.foraging_ZOOM_week * n.foraging_ZOOM_week^(-1/5) / 2
# #   locs_spa.foraging_ZOOM_week <- as(GPS_spa.foraging_ZOOM_week, "Spatial")
# #   
# #   # KernelUD
# #   kud.foraging_ZOOM_week <- kernelUD(locs_spa.foraging_ZOOM_week["week"], 
# #                                       grid = SpatialPixels_ZOOM, 
# #                                       h = mean(c(h.silverman_x_foraging_ZOOM_week, 
# #                                                  h_silverman_y_foraging_ZOOM_week)))
# #   
# #   kud_list.foraging_ZOOM_week <- lapply(names(kud.foraging_ZOOM_week), function(week) {
# #     
# #     print(week)
# #     
# #     # Extraire l'estimation de densité pour un ID spécifique
# #     kud_single.foraging_ZOOM_week <- kud.foraging_ZOOM_week[[week]]
# #     rast.foraging_ZOOM_week <- rast(kud_single.foraging_ZOOM_week)
# #     courtour.foraging_ZOOM_week <- as.contour(rast.foraging_ZOOM_week)
# #     sf.foraging_ZOOM_week <- st_as_sf(courtour.foraging_ZOOM_week)
# #     cast.foraging_ZOOM_week <- st_cast(sf.foraging_ZOOM_week, "POLYGON")
# #     cast.foraging_ZOOM_week$week <- week
# #     
# #     return(cast.foraging_ZOOM_week)
# #   })
# #   
# #   kud_all.foraging_ZOOM_week <- do.call(rbind, kud_list.foraging_ZOOM_week)
# #   kud_all.foraging_ZOOM_week$week <- as.factor(kud_all.foraging_ZOOM_week$week)
# #   kud_all.foraging_ZOOM_week$ZOOM <- lettre
# #   results_kud.foraging_ZOOM_week <- rbind(results_kud.foraging_ZOOM_week, kud_all.foraging_ZOOM_week)
# #   
# # }
# # 
# # # write
# # st_write(results_kud.foraging_ZOOM_week, paste0(data_generated_path, "results_kud.foraging_ZOOM_week.gpkg"), append = FALSE)
# # # read
# # results_kud.foraging_ZOOM_week <- st_read(file.path(data_generated_path, "results_kud.foraging_ZOOM_week.gpkg"))
# # 
# # # plot
# # tmap_mode("view")
# # UDMap_foraging_week_ZOOM <- tm_scalebar() +   tm_basemap(c("OpenStreetMap", "Esri.WorldImagery", "CartoDB.Positron")) +
# #   tm_shape(RMO) +
# #   tm_polygons() +
# #   tm_text("NOM_SITE", size = 1) +
# #   tm_shape(ZOOM_A) +
# #   tm_polygons(fill_alpha = 0.1, fill = "grey") +
# #   tm_text("A", size = 1.5) +
# #   tm_shape(ZOOM_B) +
# #   tm_polygons(fill_alpha = 0.1, fill = "grey") +
# #   tm_text("B", size = 1.5) +
# #   tm_shape(ZOOM_C) +
# #   tm_polygons(fill_alpha = 0.1, fill = "grey") +
# #   tm_text("C", size = 1.5) +
# #   tm_shape(ZOOM_D) +
# #   tm_polygons(fill_alpha = 0.1, fill = "grey") +
# #   tm_text("D", size = 1.5) +
# #   tm_shape(ZOOM_E) +
# #   tm_polygons(fill_alpha = 0.1, fill = "grey") +
# #   tm_text("E", size = 1.5) +
# #   tm_shape(BOX_2154) +
# #   tm_borders(col = "black") +
# #   tm_shape(results_kud.foraging_ZOOM_week) + 
# #   tm_facets("week") + 
# #   tm_polygons(border.col = "grey", fill = "level", fill_alpha = 0.2, 
# #               palette = viridis::viridis(10, begin = 0, end = 1, 
# #                                          direction = 1, option = "plasma")) +
# #   tm_facets("week") +
# #   tm_shape(terre_mer) +
# #   tm_lines(col = "lightblue", lwd = 0.1) ; UDMap_foraging_week_ZOOM
# # 
# # ###                        ###
# # ### Repétabilité inter-week / population scale ###
# # ###                        ###
# # 
# # GPS.week_repet_pop <- GPS %>% 
# #   filter(behavior == "foraging") %>% 
# #   dplyr::select(datetime,lon,lat,week) %>% 
# #   st_drop_geometry() %>% 
# #   na.omit()
# # 
# # # au moins 5 point par group
# # n_per_week <- GPS.week_repet_pop %>% 
# #   group_by(week) %>% 
# #   summarize(n = n())%>% 
# #   filter(n <= 5) #%>%
# # # mutate(ID_week = paste0(ID, "_", week))
# # 
# # 
# # GPS.week_repet_pop <- GPS.week_repet_pop %>% 
# #   filter(week %ni% n_per_week$week)
# # 
# # # Transformer en objet spatial (EPSG:4326)
# # GPS_spa.week_repet_pop <- st_as_sf(GPS.week_repet_pop, coords = c("lon", "lat"), crs = 4326)
# # GPS_spa.week_repet_pop <- st_transform(GPS_spa.week_repet_pop, crs = 32630)
# # 
# # # raster/grid
# # crs_utm <- "EPSG:32630"
# # SpatRaster <- project(raster_100x100, crs_utm)
# # RasterLayer <- raster(SpatRaster)
# # SpatialPixels <- as(RasterLayer, "SpatialPixels")
# # 
# # # Extraire les coordonnées reprojetées
# # coords.week_repet_pop <- st_coordinates(GPS_spa.week_repet_pop)
# # 
# # # Règle de Silverman
# # sigma_x.foraging_week_repet_pop <- sd(coords.week_repet_pop[,1])
# # sigma_y_foraging_week_repet_pop <- sd(coords.week_repet_pop[,2])
# # n.foraging_month_repet_pop <- nrow(GPS_spa.month_repet_pop)
# # 
# # h.silverman_x_foraging_month_repet_pop <- 1.06 * sigma_x.foraging_month_repet_pop * n.foraging_month_repet_pop^(-1/5) / 2
# # h.silverman_y_foraging_month_repet_pop <- 1.06 * sigma_y_foraging_month_repet_pop * n.foraging_month_repet_pop^(-1/5) / 2
# # 
# # GPS_spa.week_repet_pop <- as(GPS_spa.week_repet_pop, "Spatial")
# # 
# # kud.foraging_week_repet_pop <- kernelUD(GPS_spa.week_repet["week"], 
# #                                          grid = as(SpatialPixels, "SpatialPixels"),
# #                                          h = mean(c(h.silverman_x_foraging_week_repet_pop,
# #                                                     h.silverman_y_foraging_week_repet_pop)))
# # 
# # ##                     ##
# # ## valeur répétabilité ##
# # ##                     ##
# # 
# # overlap.foraging_week_repet_pop <- kerneloverlaphr(kud.foraging_week_repet_pop, method = "BA")
# # mean_overlap.foraging_week_repet_pop <- mean(overlap.foraging_week_repet_pop, na.rm = T) ; mean
# # 
# # # overlap_matrix
# # min_val <- min(overlap.foraging_week_repet_pop, na.rm = TRUE)
# # max_val <- max(overlap.foraging_week_repet_pop, na.rm = TRUE)
# # ordre <- c("janv", "févr", "mars", "avr","mai","juin","juil","août","sept","oct","nov","déc")
# # overlap.foraging_week_repet_pop <- overlap.foraging_week_repet_pop[ordre, ordre]
# # 
# # plot.overlapp_foraging_week_repet_pop <- ggcorrplot(overlap.foraging_week_repet_pop,
# #                                                      hc.order = FALSE,
# #                                                      method = "circle",
# #                                                      type = "lower",
# #                                                      lab = TRUE,
# #                                                      digits = 1,
# #                                                      colors = c("white", "yellow", "red"),
# #                                                      ggtheme = theme_minimal()) +
# #   scale_fill_gradientn(colors = c("white", "yellow", "red"),
# #                        limits = c(min_val, 
# #                                   max_val)) ; plot.overlapp_foraging_week_repet_pop
# # 
# # ##               ##
# # ## UDMap par ind ##
# # ##               ##
# # 
# # # Estimation UDmap par ind par week
# # 
# # # Créer une liste pour stocker les résultats
# # UDmaps_list.foraging_ZOOM_week <- lapply(names(kud.foraging_week_repet_pop), function(Individu_Periode) {
# #   
# #   print(Individu_Periode)
# #   
# #   # Extraire l'estimation de densité pour un ID spécifique
# #   kud_single.foraging_ZOOM_week <- kud.foraging_week_repet_pop[[Individu_Periode]]
# #   rast.foraging_ZOOM_week <- rast(kud_single.foraging_ZOOM_week)
# #   contour.foraging_ZOOM_week <- as.contour(rast.foraging_ZOOM_week)
# #   sf.foraging_ZOOM_week <- st_as_sf(contour.foraging_ZOOM_week)
# #   cast.foraging_ZOOM_week <- st_cast(sf.foraging_ZOOM_week, "POLYGON")
# #   cast.foraging_ZOOM_week$Individu_Periode <- Individu_Periode
# #   
# #   return(cast.foraging_ZOOM_week)
# #   
# # })
# # 
# # # Fusionner tous les ID dans un seul objet sf
# # results_kud.foraging_ZOOM_week <- do.call(rbind, UDmaps_list.foraging_ZOOM_week)
# # results_kud.foraging_ZOOM_week$Individu_Periode <- as.factor(results_kud.foraging_ZOOM_week$Individu_Periode)
# # results_kud.foraging_ZOOM_week$ID <- sub("_.*", "", results_kud.foraging_ZOOM_week$Individu_Periode)
# # results_kud.foraging_ZOOM_week$Individu_Periode <- droplevels(results_kud.foraging_ZOOM_week$Individu_Periode)
# # results_kud.foraging_ZOOM_week$Periode <- sub(".*_", "", results_kud.foraging_ZOOM_week$Individu_Periode)
# # results_kud.foraging_ZOOM_week$ID <- as.factor(results_kud.foraging_ZOOM_week$ID)
# # 
# # # plot 
# # tmap_mode("view")
# # 
# # UDMap_foraging_rep_inter_week <- tm_shape(RMO) +
# #   tm_polygons() +
# #   tm_text("NOM_SITE", size = 1) +
# #   tm_shape(results_kud.foraging_ZOOM_week) + 
# #   tm_facets("ID") +
# #   tm_polygons(border.col = "grey", fill = "Periode", fill_alpha = 0.2) ; UDMap_foraging_rep_inter_week
# # 
# # 
# # ###                        ###
# # ### Repétabilité inter-week / individual scale ###
# # ###                        ###
# # 
# # GPS.week_repet <- GPS %>% 
# #   filter(behavior == "foraging") %>% 
# #   dplyr::select(ID,datetime,lon,lat,week) %>% 
# #   mutate(ID_week = paste0(ID, "_", week)) %>% 
# #   st_drop_geometry() %>% 
# #   na.omit()
# # 
# # # au moins 5 point par group
# # n_per_week <- GPS.week_repet %>% 
# #   group_by(ID_week) %>% 
# #   summarize(n = n())%>% 
# #   filter(n <= 5) #%>%
# # # mutate(ID_week = paste0(ID, "_", week))
# # 
# # 
# # GPS.week_repet <- GPS.week_repet %>% 
# #   filter(ID_week %ni% n_per_week$ID_week)
# # 
# # # Transformer en objet spatial (EPSG:4326)
# # GPS_spa.week_repet <- st_as_sf(GPS.week_repet, coords = c("lon", "lat"), crs = 4326)
# # GPS_spa.week_repet <- st_transform(GPS_spa.week_repet, crs = 32630)
# # 
# # # raster/grid
# # crs_utm <- "EPSG:32630"
# # SpatRaster <- project(raster_100x100, crs_utm)
# # RasterLayer <- raster(SpatRaster)
# # SpatialPixels <- as(RasterLayer, "SpatialPixels")
# # 
# # # Extraire les coordonnées reprojetées
# # coords.week_repet <- st_coordinates(GPS_spa.week_repet)
# # 
# # # Règle de Silverman
# # sigma_x.foraging_week_repet <- sd(coords.week_repet[,1])
# # sigma_y_foraging_week_repet <- sd(coords.week_repet[,2])
# # n.foraging_week_repet <- nrow(GPS_spa.week_repet)
# # 
# # h.silverman_x_foraging_week_repet <- 1.06 * sigma_x.foraging_week_repet * n.foraging_week_repet^(-1/5) / 2
# # h.silverman_y_foraging_week_repet <- 1.06 * sigma_y_foraging_week_repet * n.foraging_week_repet^(-1/5) / 2
# # 
# # GPS_spa.week_repet <- as(GPS_spa.week_repet, "Spatial")
# # 
# # kud.foraging_week_repet <- kernelUD(GPS_spa.week_repet["ID_week"], 
# #                                      grid = as(SpatialPixels, "SpatialPixels"),
# #                                      h = mean(c(h.silverman_x_foraging_week_repet,
# #                                                 h.silverman_y_foraging_week_repet)))
# # 
# # ##                     ##
# # ## valeur répétabilité ##
# # ##                     ##
# # 
# # # Estimation valeur d'overlapp par ind entre chaque week
# # 
# # # Extraire les noms uniques des individus
# # individus <- unique(GPS_spa.week_repet$ID)
# # 
# # # Stocker les résultats
# # overlap_results.foraging_week_repet = NULL
# # 
# # # Boucle sur chaque individu
# # for (ind in individus) {
# #   
# #   print(ind)
# #   
# #   # Trouver les noms des périodes de cet individu dans hr_kde
# #   ID_periodes <- names(kud.foraging_week_repet)[grep(paste0("^", ind, "_"), names(kud.foraging_week_repet))]
# #   
# #   # Vérifier que l'individu a bien deux périodes
# #   # if (length(ID_periodes) >= 2) {
# #   # Créer un estUDm valide
# #   hr_kde_ind.foraging_week_repet <- kud.foraging_week_repet[ID_periodes]
# #   class(hr_kde_ind.foraging_week_repet) <- "estUDm"  # Important pour que kerneloverlaphr() fonctionne
# #   
# #   # Calculer l'overlap entre les deux périodes
# #   overlap_value.foraging_week_repet <- kerneloverlaphr(hr_kde_ind.foraging_week_repet, 
# #                                                         method = "BA")[1, 2]
# #   
# #   info_ind.foraging_week_repet <- c(ind, overlap_value.foraging_week_repet)
# #   
# #   # Stocker le résultat
# #   # overlap_results <- rbind(overlap_results, data.frame(Individu = ind, Overlap = overlap_value))
# #   overlap_results.foraging_week_repet <- rbind(overlap_results.foraging_week_repet, info_ind.foraging_week_repet)
# #   
# #   # }
# # }
# # 
# # overlap_results.foraging_week_repet <- as.data.frame(overlap_results.foraging_week_repet)
# # 
# # overlap_results.foraging_week_repet <- overlap_results.foraging_week_repet %>% 
# #   rename(ID = V1, overlap = V2)
# # 
# # overlap_results.foraging_week_repet$overlap <- as.numeric(overlap_results.foraging_week_repet$overlap)
# # 
# # mean_overlap.foraging_week_repet <- mean(overlap_results.foraging_week_repet$overlap, na.rm = T) ; mean_overlap.foraging_week_repet
# # 
# # # Afficher les résultats
# # overlap_results.foraging_week_repet <- overlap_results.foraging_week_repet[order(overlap_results.foraging_week_repet$overlap), ] ; overlap_results.foraging_week_repet
# # 
# # # plot
# # plot.foraging_week_repet <- ggplot(overlap_results.foraging_week_repet, aes(x=reorder(ID, overlap), y=overlap)) + 
# #   geom_point(shape = 19, size = 4) +
# #   theme_classic() +
# #   coord_flip() +
# #   theme(legend.position = "top") +
# #   scale_fill_manual() +
# #   labs(title="",
# #        x ="Individu", y = "Pourcentage d'overlap inter-mois"); plot.foraging_week_repet
# # 
# # ##               ##
# # ## UDMap par ind ##
# # ##               ##
# # 
# # # Estimation UDmap par ind par week
# # 
# # # Créer une liste pour stocker les résultats
# # UDmaps_list.foraging_ZOOM_week <- lapply(names(kud.foraging_week_repet), function(Individu_Periode) {
# #   
# #   print(Individu_Periode)
# #   
# #   # Extraire l'estimation de densité pour un ID spécifique
# #   kud_single.foraging_ZOOM_week <- kud.foraging_week_repet[[Individu_Periode]]
# #   rast.foraging_ZOOM_week <- rast(kud_single.foraging_ZOOM_week)
# #   contour.foraging_ZOOM_week <- as.contour(rast.foraging_ZOOM_week)
# #   sf.foraging_ZOOM_week <- st_as_sf(contour.foraging_ZOOM_week)
# #   cast.foraging_ZOOM_week <- st_cast(sf.foraging_ZOOM_week, "POLYGON")
# #   cast.foraging_ZOOM_week$Individu_Periode <- Individu_Periode
# #   
# #   return(cast.foraging_ZOOM_week)
# #   
# # })
# # 
# # # Fusionner tous les ID dans un seul objet sf
# # results_kud.foraging_ZOOM_week <- do.call(rbind, UDmaps_list.foraging_ZOOM_week)
# # results_kud.foraging_ZOOM_week$Individu_Periode <- as.factor(results_kud.foraging_ZOOM_week$Individu_Periode)
# # results_kud.foraging_ZOOM_week$ID <- sub("_.*", "", results_kud.foraging_ZOOM_week$Individu_Periode)
# # results_kud.foraging_ZOOM_week$Individu_Periode <- droplevels(results_kud.foraging_ZOOM_week$Individu_Periode)
# # results_kud.foraging_ZOOM_week$Periode <- sub(".*_", "", results_kud.foraging_ZOOM_week$Individu_Periode)
# # results_kud.foraging_ZOOM_week$ID <- as.factor(results_kud.foraging_ZOOM_week$ID)
# # 
# # # plot 
# # tmap_mode("view")
# # 
# # UDMap_foraging_rep_inter_week <- tm_shape(RMO) +
# #   tm_polygons() +
# #   tm_text("NOM_SITE", size = 1) +
# #   tm_shape(results_kud.foraging_ZOOM_week) + 
# #   tm_facets("ID") +
# #   tm_polygons(border.col = "grey", fill = "Periode", fill_alpha = 0.2) ; UDMap_foraging_rep_inter_week

# Variabilité cycle de marais --------------------------------------------------

fidel_inter_maree_dt_1 <- GPS %>% 
  dplyr::select(ID, behavior, datetime) %>% 
  filter(behavior !="other") %>% 
  distinct() %>% 
  na.omit()

fidel_inter_maree_dt_2 <- fidel_inter_maree_dt_1 %>%
  st_drop_geometry() %>% 
  arrange(ID, datetime) %>%
  group_by(ID) %>%
  mutate(
    time_diff = as.numeric(difftime(datetime, lag(datetime), units = "mins")),
    new_group = if_else(is.na(time_diff) | time_diff > 60*6, 1, 0),
    group_id = cumsum(new_group)
  ) %>%
  ungroup() %>% 
  na.omit()

## roosting -----

# Repétabilité / individual scale

GPS.maree_repet <- GPS %>% 
  filter(behavior == "roosting") %>% 
  left_join(fidel_inter_maree_dt_2) %>% 
  mutate(ID_maree = paste0(ID, "_", group_id)) %>% 
  dplyr::select(ID,datetime,lon,lat, ID_maree) %>%
  st_drop_geometry() %>% 
  na.omit()

# au moins 5 point par group
n_per_maree <- GPS.maree_repet %>% 
  group_by(ID_maree) %>% 
  summarize(n = n())%>% 
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
sigma_x.roosting_maree_repet <- sd(coords.maree_repet[,1])
sigma_y_roosting_maree_repet <- sd(coords.maree_repet[,2])
n.roosting_maree_repet <- nrow(GPS_spa.maree_repet)

h.silverman_x_roosting_maree_repet <- 1.06 * sigma_x.roosting_maree_repet * n.roosting_maree_repet^(-1/5) / 2
h.silverman_y_roosting_maree_repet <- 1.06 * sigma_y_roosting_maree_repet * n.roosting_maree_repet^(-1/5) / 2 

GPS_spa.maree_repet <- as(GPS_spa.maree_repet, "Spatial")

kud.roosting_maree_repet <- kernelUD(GPS_spa.maree_repet["ID_maree"], 
                                    grid = as(SpatialPixels, "SpatialPixels"),
                                    h = mean(c(h.silverman_x_roosting_maree_repet,
                                               h.silverman_y_roosting_maree_repet)))

##                     ##
## valeur répétabilité ##
##                     ##

# Estimation valeur d'overlapp par ind entre chaque maree

# Extraire les noms uniques des individus
individus <- unique(GPS_spa.maree_repet$ID)

# Stocker les résultats
overlap_results.roosting_maree_repet = NULL

# ind = "EA580462"

# Boucle sur chaque individu
for (ind in individus) {
  
  print(ind)
  
  # Trouver les noms des périodes de cet individu dans hr_kde
  ID_periodes <- names(kud.roosting_maree_repet)[grep(paste0("^", ind, "_"), names(kud.roosting_maree_repet))]
  
  # Vérifier que l'individu a bien deux périodes
  if (length(ID_periodes) >= 2) {
    # Créer un estUDm valide
    hr_kde_ind.roosting_maree_repet <- kud.roosting_maree_repet[ID_periodes]
    class(hr_kde_ind.roosting_maree_repet) <- "estUDm"  # Important pour que kerneloverlaphr() fonctionne
    
    # Calculer l'overlap entre les deux périodes
    overlap_value.roosting_maree_repet <- kerneloverlaphr(hr_kde_ind.roosting_maree_repet, 
                                                         method = "BA")[1, 2]
    
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

mean_overlap.roosting_maree_repet <- mean(overlap_results.roosting_maree_repet$overlap, na.rm = T) ; mean_overlap.roosting_maree_repet

# Afficher les résultats
overlap_results.roosting_maree_repet <- overlap_results.roosting_maree_repet[order(overlap_results.roosting_maree_repet$overlap), ] ; overlap_results.roosting_maree_repet

# plot
plot.roosting_maree_repet <- ggplot(overlap_results.roosting_maree_repet, aes(x=reorder(ID, overlap), y=overlap, fill = overlap)) + 
  geom_point(shape = 21, size = 4) +
  theme_classic() +
  theme(legend.position = c(.75, .3)) +
  scale_fill_gradientn(colors = paletteer_c("grDevices::Sunset", 10, direction = -1)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  # scale_fill_manual() +
  labs(title="",
       x ="Individu", y = "Pourcentage de chevauchement moyen 
de zone de reposoirs entre années") ; plot.roosting_maree_repet

ggsave(paste0(atlas_path, "/plot.roosting_maree_repet.png"), 
       plot = plot.roosting_maree_repet, width = 8, height = 5, dpi = 1000)

##               ##
## UDMap par ind ##
##               ##

# # Estimation UDmap par ind par maree
# 
# # Créer une liste pour stocker les résultats
# UDmaps_list.roosting_ZOOM_maree <- lapply(names(kud.roosting_maree_repet), function(Individu_Periode) {
#   
#   print(Individu_Periode)
#   
#   # Extraire l'estimation de densité pour un ID spécifique
#   kud_single.roosting_ZOOM_maree <- kud.roosting_maree_repet[[Individu_Periode]]
#   rast.roosting_ZOOM_maree <- rast(kud_single.roosting_ZOOM_maree)
#   contour.roosting_ZOOM_maree <- as.contour(rast.roosting_ZOOM_maree)
#   sf.roosting_ZOOM_maree <- st_as_sf(contour.roosting_ZOOM_maree)
#   cast.roosting_ZOOM_maree <- st_cast(sf.roosting_ZOOM_maree, "POLYGON")
#   cast.roosting_ZOOM_maree$Individu_Periode <- Individu_Periode
#   
#   return(cast.roosting_ZOOM_maree)
#   
# })
# 
# # Fusionner tous les ID dans un seul objet sf
# results_kud.roosting_ZOOM_maree <- do.call(rbind, UDmaps_list.roosting_ZOOM_maree)
# results_kud.roosting_ZOOM_maree$Individu_Periode <- as.factor(results_kud.roosting_ZOOM_maree$Individu_Periode)
# results_kud.roosting_ZOOM_maree$ID <- sub("_.*", "", results_kud.roosting_ZOOM_maree$Individu_Periode)
# results_kud.roosting_ZOOM_maree$Individu_Periode <- droplevels(results_kud.roosting_ZOOM_maree$Individu_Periode)
# results_kud.roosting_ZOOM_maree$Periode <- sub(".*_", "", results_kud.roosting_ZOOM_maree$Individu_Periode)
# results_kud.roosting_ZOOM_maree$ID <- as.factor(results_kud.roosting_ZOOM_maree$ID)
# 
# # plot 
# tmap_mode("view")
# UDMap_roosting_rep_inter_maree <- tm_scalebar() +   
#   tm_basemap(c("OpenStreetMap", "Esri.WorldImagery", "CartoDB.Positron")) +
#   tm_shape(results_kud.roosting_ZOOM_maree) + 
#   tm_facets("ID", drop.units = TRUE) +
#   tm_polygons(border.col = "grey", fill = "Periode", fill_alpha = 0.2,
#               palette = palette_roosting) +
#   tm_shape(RMO) +
#   tm_borders(col = "white", lwd = 3, lty = "dashed") +
#   tm_shape(terre_mer) +
#   tm_lines(col = "lightblue", lwd = 0.1); UDMap_roosting_rep_inter_maree
# 

## alimentation -----

# Repétabilité / individual scale

GPS.maree_repet <- GPS %>% 
  filter(behavior == "foraging") %>% 
  left_join(fidel_inter_maree_dt_2) %>% 
  mutate(ID_maree = paste0(ID, "_", group_id)) %>% 
  dplyr::select(ID,datetime,lon,lat, ID_maree) %>%
  st_drop_geometry() %>% 
  na.omit()

# au moins 5 point par group
n_per_maree <- GPS.maree_repet %>% 
  group_by(ID_maree) %>% 
  summarize(n = n())%>% 
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
sigma_x.foraging_maree_repet <- sd(coords.maree_repet[,1])
sigma_y_foraging_maree_repet <- sd(coords.maree_repet[,2])
n.foraging_maree_repet <- nrow(GPS_spa.maree_repet)

h.silverman_x_foraging_maree_repet <- 1.06 * sigma_x.foraging_maree_repet * n.foraging_maree_repet^(-1/5) / 2
h.silverman_y_foraging_maree_repet <- 1.06 * sigma_y_foraging_maree_repet * n.foraging_maree_repet^(-1/5) / 2 

GPS_spa.maree_repet <- as(GPS_spa.maree_repet, "Spatial")

kud.foraging_maree_repet <- kernelUD(GPS_spa.maree_repet["ID_maree"], 
                                     grid = as(SpatialPixels, "SpatialPixels"),
                                     h = mean(c(h.silverman_x_foraging_maree_repet,
                                                h.silverman_y_foraging_maree_repet)))

##                     ##
## valeur répétabilité ##
##                     ##

# Estimation valeur d'overlapp par ind entre chaque maree

# Extraire les noms uniques des individus
individus <- unique(GPS_spa.maree_repet$ID)

# Stocker les résultats
overlap_results.foraging_maree_repet = NULL

# ind = "EA580462"

# Boucle sur chaque individu
for (ind in individus) {
  
  print(ind)
  
  # Trouver les noms des périodes de cet individu dans hr_kde
  ID_periodes <- names(kud.foraging_maree_repet)[grep(paste0("^", ind, "_"), names(kud.foraging_maree_repet))]
  
  # Vérifier que l'individu a bien deux périodes
  if (length(ID_periodes) >= 2) {
    # Créer un estUDm valide
    hr_kde_ind.foraging_maree_repet <- kud.foraging_maree_repet[ID_periodes]
    class(hr_kde_ind.foraging_maree_repet) <- "estUDm"  # Important pour que kerneloverlaphr() fonctionne
    
    # Calculer l'overlap entre les deux périodes
    overlap_value.foraging_maree_repet <- kerneloverlaphr(hr_kde_ind.foraging_maree_repet, 
                                                          method = "BA")[1, 2]
    
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

mean_overlap.foraging_maree_repet <- mean(overlap_results.foraging_maree_repet$overlap, na.rm = T) ; mean_overlap.foraging_maree_repet

# Afficher les résultats
overlap_results.foraging_maree_repet <- overlap_results.foraging_maree_repet[order(overlap_results.foraging_maree_repet$overlap), ] ; overlap_results.foraging_maree_repet

# plot
plot.foraging_maree_repet <- ggplot(overlap_results.foraging_maree_repet, aes(x=reorder(ID, overlap), y=overlap, fill = overlap)) + 
  geom_point(shape = 21, size = 4) +
  theme_classic() +
  theme(legend.position = c(.75, .3)) +
  scale_fill_gradientn(colors = paletteer_c("grDevices::YlGnBu", 10, direction = -1)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  # scale_fill_manual() +
  labs(title="",
       x ="Individu", y = "Pourcentage de chevauchement moyen 
de zone de reposoirs entre années") ; plot.foraging_maree_repet

ggsave(paste0(atlas_path, "/plot.foraging_maree_repet.png"), 
       plot = plot.roosting_maree_repet, width = 8, height = 5, dpi = 1000)

##               ##
## UDMap par ind ##
##               ##

# # Estimation UDmap par ind par maree
# 
# # Créer une liste pour stocker les résultats
# UDmaps_list.roosting_ZOOM_maree <- lapply(names(kud.roosting_maree_repet), function(Individu_Periode) {
#   
#   print(Individu_Periode)
#   
#   # Extraire l'estimation de densité pour un ID spécifique
#   kud_single.roosting_ZOOM_maree <- kud.roosting_maree_repet[[Individu_Periode]]
#   rast.roosting_ZOOM_maree <- rast(kud_single.roosting_ZOOM_maree)
#   contour.roosting_ZOOM_maree <- as.contour(rast.roosting_ZOOM_maree)
#   sf.roosting_ZOOM_maree <- st_as_sf(contour.roosting_ZOOM_maree)
#   cast.roosting_ZOOM_maree <- st_cast(sf.roosting_ZOOM_maree, "POLYGON")
#   cast.roosting_ZOOM_maree$Individu_Periode <- Individu_Periode
#   
#   return(cast.roosting_ZOOM_maree)
#   
# })
# 
# # Fusionner tous les ID dans un seul objet sf
# results_kud.roosting_ZOOM_maree <- do.call(rbind, UDmaps_list.roosting_ZOOM_maree)
# results_kud.roosting_ZOOM_maree$Individu_Periode <- as.factor(results_kud.roosting_ZOOM_maree$Individu_Periode)
# results_kud.roosting_ZOOM_maree$ID <- sub("_.*", "", results_kud.roosting_ZOOM_maree$Individu_Periode)
# results_kud.roosting_ZOOM_maree$Individu_Periode <- droplevels(results_kud.roosting_ZOOM_maree$Individu_Periode)
# results_kud.roosting_ZOOM_maree$Periode <- sub(".*_", "", results_kud.roosting_ZOOM_maree$Individu_Periode)
# results_kud.roosting_ZOOM_maree$ID <- as.factor(results_kud.roosting_ZOOM_maree$ID)
# 
# # plot 
# tmap_mode("view")
# UDMap_roosting_rep_inter_maree <- tm_scalebar() +   
#   tm_basemap(c("OpenStreetMap", "Esri.WorldImagery", "CartoDB.Positron")) +
#   tm_shape(results_kud.roosting_ZOOM_maree) + 
#   tm_facets("ID", drop.units = TRUE) +
#   tm_polygons(border.col = "grey", fill = "Periode", fill_alpha = 0.2,
#               palette = palette_roosting) +
#   tm_shape(RMO) +
#   tm_borders(col = "white", lwd = 3, lty = "dashed") +
#   tm_shape(terre_mer) +
#   tm_lines(col = "lightblue", lwd = 0.1); UDMap_roosting_rep_inter_maree
# 














































# fidel_inter_marais_dt_3 <- fidel_inter_marais_dt_2 %>% 
#   group_by(ID, group_id) %>% 
#   mutate(centroid = st_centroid(st_union(geom))) %>% 
#   dplyr::select(ID, behavior, group_id, datetime, centroid) %>% 
#   st_drop_geometry()
# 
# centroid_sf <- distance_dt_3 %>%
#   st_as_sf(crs = 4326) %>%  # si centroid est en texte WKT, sinon adapter
#   arrange(ID, datetime)
# 
# centroid_sf <- st_as_sf(distance_dt_3)
# 
# # Ajouter les lignes suivantes dans un mutate par groupe ID
# paired_centroids <- centroid_sf %>%
#   group_by(ID) %>%
#   arrange(datetime) %>%
#   mutate(
#     behavior_next = lead(behavior),
#     datetime_next = lead(datetime),
#     geom_next = lead(centroid)
#   ) %>%
#   filter(
#     !is.na(datetime_next),
#     abs(difftime(datetime_next, datetime, units = "hours")) <= 12,
#     behavior != behavior_next
#   ) %>%
#   mutate(
#     distance_m = st_distance(centroid, geom_next, by_element = TRUE)
#   ) %>%
#   ungroup()
# 
# paired_centroids$distance_m <- as.numeric(paired_centroids$distance_m)
# 
# paired_centroids_mean_dt <- paired_centroids %>% 
#   st_drop_geometry() %>% 
#   filter(distance_m > 0) %>% 
#   group_by(ID) %>% 
#   summarise(mean_dist = mean(distance_m),
#             sd_dist = sd(distance_m))
# 
# mean_dist <- mean(paired_centroids_mean_dt$mean_dist)
# sd_dist <- sd(paired_centroids_mean_dt$mean_dist)

########################## ---
# *Age ----------------------------------------------------------------------
########################## ---
  
## # # # # # --- 
## *reposoir  ------------------------------------------------------------------
## # # # # # ---

zoom_level <- c("A","B","C","D","E")
analyse = "roosting_age"
results_kud = NULL
nb_kud = NULL
comportement = "roosting"
param <- "age"
couleur = nom_pal_roosting
map_kud.roosting_age <- Map(estimate_kud_param, zoom_level, comportement, param)
results_kud.roosting_age <- do.call(rbind, map_kud.roosting_age)
st_write(results_kud.roosting_age, paste0(data_generated_path, "results_kud.", analyse, ".gpkg"), append = FALSE)
results_kud.roosting <- st_read(file.path(data_generated_path, paste0("results_kud.", analyse,".gpkg")))
# compter les nb ind par zoom
nb_kud_map.roosting_age <- Map(count_nb_kud_param, zoom_level, comportement, param)
nb_kud.roosting_age <- do.call(rbind, nb_kud_map.roosting_age)
write.csv(nb_kud.roosting_age, paste0(data_generated_path, "nb_kud.", analyse, ".csv"), row.names = FALSE)
nb_kud.roosting_age <- read.csv(paste0(data_generated_path, paste0("nb_kud.", analyse, ".csv")), row.names = NULL)
# Générer les maps pour chaque zoom
maps_list.roosting_ZOOM_age<- Map(create_map_param, zoom_level, analyse, param, couleur)

## # # # # # --- 
## *alimentation  ---------------------------------------------------------------
## # # # # # ---

zoom_level <- c("A","B","C","D","E")
analyse = "foraging_age"
results_kud = NULL
nb_kud = NULL
comportement = "foraging"
param <- "age"
couleur = nom_pal_foraging
map_kud.foraging_age <- Map(estimate_kud_param, zoom_level, comportement, param)
results_kud.foraging_age <- do.call(rbind, map_kud.foraging_age)
st_write(results_kud.foraging_age, paste0(data_generated_path, "results_kud.", analyse, ".gpkg"), append = FALSE)
results_kud.foraging <- st_read(file.path(data_generated_path, paste0("results_kud.", analyse,".gpkg")))
# compter les nb ind par zoom
nb_kud_map.foraging_age <- Map(count_nb_kud_param, zoom_level, comportement, param)
nb_kud.foraging_age <- do.call(rbind, nb_kud_map.foraging_age)
write.csv(nb_kud.foraging_age, paste0(data_generated_path, "nb_kud.", analyse, ".csv"), row.names = FALSE)
nb_kud.foraging_age <- read.csv(paste0(data_generated_path, paste0("nb_kud.", analyse, ".csv")), row.names = NULL)
# Générer les maps pour chaque zoom
maps_list.foraging_ZOOM_age<- Map(create_map_param, zoom_level, analyse, param, couleur)

########################## ---
# *Sexe ---------------------------------------------------------------------
########################## ---

## # # # # # --- 
## *reposoir  ------------------------------------------------------------------
## # # # # # ---

zoom_level <- c("A","B","C","D","E")
analyse = "roosting_sex"
results_kud = NULL
nb_kud = NULL
comportement = "roosting"
param <- "sex"
couleur = nom_pal_roosting
map_kud.roosting_sex <- Map(estimate_kud_param, zoom_level, comportement, param)
results_kud.roosting_sex <- do.call(rbind, map_kud.roosting_sex)
st_write(results_kud.roosting_sex, paste0(data_generated_path, "results_kud.", analyse, ".gpkg"), append = FALSE)
results_kud.roosting <- st_read(file.path(data_generated_path, paste0("results_kud.", analyse,".gpkg")))
# compter les nb ind par zoom
nb_kud_map.roosting_sex <- Map(count_nb_kud_param, zoom_level, comportement, param)
nb_kud.roosting_sex <- do.call(rbind, nb_kud_map.roosting_sex)
write.csv(nb_kud.roosting_sex, paste0(data_generated_path, "nb_kud.", analyse, ".csv"), row.names = FALSE)
nb_kud.roosting_sex <- read.csv(paste0(data_generated_path, paste0("nb_kud.", analyse, ".csv")), row.names = NULL)
# Générer les maps pour chaque zoom
maps_list.roosting_ZOOM_sex <- Map(create_map_param, zoom_level, analyse, param, couleur)
beep(3)

## # # # # # --- 
## *alimentation  ---------------------------------------------------------------
## # # # # # ---

zoom_level <- c("A","B","C","D","E")
analyse = "foraging_sex"
results_kud = NULL
nb_kud = NULL
comportement = "foraging"
param <- "sex"
couleur = nom_pal_foraging
map_kud.foraging_sex <- Map(estimate_kud_param, zoom_level, comportement, param)
results_kud.foraging_sex <- do.call(rbind, map_kud.foraging_sex)
st_write(results_kud.foraging_sex, paste0(data_generated_path, "results_kud.", analyse, ".gpkg"), append = FALSE)
results_kud.foraging <- st_read(file.path(data_generated_path, paste0("results_kud.", analyse,".gpkg")))
# compter les nb ind par zoom
nb_kud_map.foraging_sex <- Map(count_nb_kud_param, zoom_level, comportement, param)
nb_kud.foraging_sex <- do.call(rbind, nb_kud_map.foraging_sex)
write.csv(nb_kud.foraging_sex, paste0(data_generated_path, "nb_kud.", analyse, ".csv"), row.names = FALSE)
nb_kud.foraging_sex <- read.csv(paste0(data_generated_path, paste0("nb_kud.", analyse, ".csv")), row.names = NULL)
# Générer les maps pour chaque zoom
maps_list.foraging_ZOOM_sex <- Map(create_map_param, zoom_level, analyse, param, couleur)
beep(3)

########################## ---
# *Jour & nuit --------------------------------------------------------------
########################## ---

## # # # # # --- 
## *reposoir  ------------------------------------------------------------------
## # # # # # ---

zoom_level <- c("A","B","C","D","E")
analyse = "roosting_jour_nuit"
results_kud = NULL
nb_kud = NULL
comportement = "roosting"
param <- "jour_nuit"
couleur = nom_pal_roosting
map_kud.roosting_jour_nuit <- Map(estimate_kud_param, zoom_level, comportement, param)
results_kud.roosting_jour_nuit <- do.call(rbind, map_kud.roosting_jour_nuit)
st_write(results_kud.roosting_jour_nuit, paste0(data_generated_path, "results_kud.", analyse, ".gpkg"), append = FALSE)
results_kud.roosting <- st_read(file.path(data_generated_path, paste0("results_kud.", analyse,".gpkg")))
# compter les nb ind par zoom
nb_kud_map.roosting_jour_nuit <- Map(count_nb_kud_param, zoom_level, comportement, param)
nb_kud.roosting_jour_nuit <- do.call(rbind, nb_kud_map.roosting_jour_nuit)
write.csv(nb_kud.roosting_jour_nuit, paste0(data_generated_path, "nb_kud.", analyse, ".csv"), row.names = FALSE)
nb_kud.roosting_jour_nuit <- read.csv(paste0(data_generated_path, paste0("nb_kud.", analyse, ".csv")), row.names = NULL)
# Générer les maps pour chaque zoom
maps_list.roosting_ZOOM_jour_nuit <- Map(create_map_param, zoom_level, analyse, param, couleur)
beep(3)

## # # # # # --- 
## *alimentation  ---------------------------------------------------------------
## # # # # # ---

zoom_level <- c("A","B","C","D","E")
analyse = "foraging_jour_nuit"
results_kud = NULL
nb_kud = NULL
comportement = "foraging"
param <- "jour_nuit"
couleur = nom_pal_foraging
map_kud.foraging_jour_nuit <- Map(estimate_kud_param, zoom_level, comportement, param)
results_kud.foraging_jour_nuit <- do.call(rbind, map_kud.foraging_jour_nuit)
st_write(results_kud.foraging_jour_nuit, paste0(data_generated_path, "results_kud.", analyse, ".gpkg"), append = FALSE)
results_kud.foraging <- st_read(file.path(data_generated_path, paste0("results_kud.", analyse,".gpkg")))
# compter les nb ind par zoom
nb_kud_map.foraging_jour_nuit <- Map(count_nb_kud_param, zoom_level, comportement, param)
nb_kud.foraging_jour_nuit <- do.call(rbind, nb_kud_map.foraging_jour_nuit)
write.csv(nb_kud.foraging_jour_nuit, paste0(data_generated_path, "nb_kud.", analyse, ".csv"), row.names = FALSE)
nb_kud.foraging_jour_nuit <- read.csv(paste0(data_generated_path, paste0("nb_kud.", analyse, ".csv")), row.names = NULL)
# Générer les maps pour chaque zoom
maps_list.foraging_ZOOM_jour_nuit <- Map(create_map_param, zoom_level, analyse, param, couleur)
beep(3)

########################## ---
# Brèche -------------------------------------------------------------------
########################## ---

table(GPS$breche)

## # # # # # --- 
## reposoir  ---------------------------------------------------------------
## # # # # # ---

crs_utm <- "EPSG:32630"
ZOOM <- c("A","B","C","D","E")
results_kud.roosting_ZOOM_breche = NULL

lettre = "E"

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
UDMap_roosting_breche_ZOOM <- tm_scalebar() +   tm_basemap(c("OpenStreetMap", "Esri.WorldImagery", "CartoDB.Positron")) +
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
  tm_lines(col = "lightblue", lwd = 0.1) ; UDMap_roosting_breche_ZOOM

### similarité avant/après ---

GPS.roosting_breche_repet_pop <- GPS %>% 
  filter(behavior == "roosting") %>% 
  dplyr::select(datetime,lon,lat,breche) %>% 
  st_drop_geometry() %>% 
  na.omit()

# au moins 5 point par group
n_per_roosting_breche <- GPS.roosting_breche_repet_pop %>% 
  group_by(breche) %>% 
  summarize(n = n())%>% 
  filter(n <= 5)

GPS.roosting_breche_repet_pop <- GPS.roosting_breche_repet_pop %>% 
  filter(breche %ni% n_per_roosting_breche$breche)

# Transformer en objet spatial (EPSG:4326)
GPS_spa.roosting_breche_repet_pop <- st_as_sf(GPS.roosting_breche_repet_pop, coords = c("lon", "lat"), crs = 4326)
GPS_spa.roosting_breche_repet_pop <- st_transform(GPS_spa.roosting_breche_repet_pop, crs = 32630)

# raster/grid
crs_utm <- "EPSG:32630"
SpatRaster <- project(raster_100x100, crs_utm)
RasterLayer <- raster(SpatRaster)
SpatialPixels <- as(RasterLayer, "SpatialPixels")

# Extraire les coordonnées reprojetées
coords.roosting_breche_repet_pop <- st_coordinates(GPS_spa.roosting_breche_repet_pop)

# Règle de Silverman
sigma_x.roosting_breche_repet_pop <- sd(coords.roosting_breche_repet_pop[,1])
sigma_y_roosting_breche_repet_pop <- sd(coords.roosting_breche_repet_pop[,2])
n.roosting_breche_repet_pop <- nrow(GPS_spa.roosting_breche_repet_pop)

h.silverman_x_roosting_breche_repet_pop <- 1.06 * sigma_x.roosting_breche_repet_pop * n.roosting_breche_repet_pop^(-1/5) / 2 
h.silverman_y_roosting_breche_repet_pop <- 1.06 * sigma_y_roosting_breche_repet_pop * n.roosting_breche_repet_pop^(-1/5) / 2 

GPS_spa.roosting_breche_repet_pop <- as(GPS_spa.roosting_breche_repet_pop, "Spatial")

kud.roosting_breche_repet_pop <- kernelUD(GPS_spa.roosting_breche_repet_pop["breche"], 
                                         grid = as(SpatialPixels, "SpatialPixels"),
                                         h = mean(c(h.silverman_x_roosting_breche_repet_pop,
                                                    h.silverman_y_roosting_breche_repet_pop)))

##                     ##
## valeur répétabilité ##
##                     ##

overlap.roosting_breche_repet_pop <- kerneloverlaphr(kud.roosting_breche_repet_pop, method = "BA")
mean_overlap.roosting_breche_repet_pop <- mean(overlap.roosting_breche_repet_pop, na.rm = T) ; mean

# overlap_matrix
min_val <- min(overlap.roosting_breche_repet_pop, na.rm = TRUE)
max_val <- max(overlap.roosting_breche_repet_pop, na.rm = TRUE)
ordre <- c("ouverture progressive","ouverture complète")
overlap.roosting_breche_repet_pop <- overlap.roosting_breche_repet_pop[ordre, ordre]

plot.overlapp_roosting_breche_repet_pop <- ggcorrplot(overlap.roosting_breche_repet_pop,
                                                     hc.order = FALSE,
                                                     method = "circle",
                                                     type = "lower",
                                                     lab = TRUE,
                                                     digits = 1,
                                                     colors = c("white", "yellow", "red"),
                                                     ggtheme = theme_minimal()) +
  scale_fill_gradientn(colors = c("white", "yellow", "red"),
                       limits = c(min_val, 
                                  max_val)) ; plot.overlapp_roosting_breche_repet_pop

## # # # # # --- 
## alimentation  ---------------------------------------------------------------
## # # # # # ---

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
UDMap_foraging_breche_ZOOM <- tm_scalebar() +   tm_basemap(c("OpenStreetMap", "Esri.WorldImagery", "CartoDB.Positron")) +
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
  tm_lines(col = "lightblue", lwd = 0.1) ; UDMap_foraging_breche_ZOOM


### similarité avant/après ---

GPS.foraging_breche_repet_pop <- GPS %>% 
  filter(behavior == "foraging") %>% 
  dplyr::select(datetime,lon,lat,breche) %>% 
  st_drop_geometry() %>% 
  na.omit()

# au moins 5 point par group
n_per_foraging_breche <- GPS.foraging_breche_repet_pop %>% 
  group_by(breche) %>% 
  summarize(n = n())%>% 
  filter(n <= 5)

GPS.foraging_breche_repet_pop <- GPS.foraging_breche_repet_pop %>% 
  filter(breche %ni% n_per_foraging_breche$breche)

# Transformer en objet spatial (EPSG:4326)
GPS_spa.foraging_breche_repet_pop <- st_as_sf(GPS.foraging_breche_repet_pop, coords = c("lon", "lat"), crs = 4326)
GPS_spa.foraging_breche_repet_pop <- st_transform(GPS_spa.foraging_breche_repet_pop, crs = 32630)

# raster/grid
crs_utm <- "EPSG:32630"
SpatRaster <- project(raster_100x100, crs_utm)
RasterLayer <- raster(SpatRaster)
SpatialPixels <- as(RasterLayer, "SpatialPixels")

# Extraire les coordonnées reprojetées
coords.foraging_breche_repet_pop <- st_coordinates(GPS_spa.foraging_breche_repet_pop)

# Règle de Silverman
sigma_x.foraging_breche_repet_pop <- sd(coords.foraging_breche_repet_pop[,1])
sigma_y.foraging_breche_repet_pop <- sd(coords.foraging_breche_repet_pop[,2])
n.foraging_breche_repet_pop <- nrow(GPS_spa.foraging_breche_repet_pop)

h.silverman_x_foraging_breche_repet_pop <- 1.06 * sigma_x.foraging_breche_repet_pop * n.foraging_breche_repet_pop^(-1/5) / 2 
h.silverman_y_foraging_breche_repet_pop <- 1.06 * sigma_y.foraging_breche_repet_pop * n.foraging_breche_repet_pop^(-1/5) / 2 

GPS_spa.foraging_breche_repet_pop <- as(GPS_spa.foraging_breche_repet_pop, "Spatial")

kud.foraging_breche_repet_pop <- kernelUD(GPS_spa.foraging_breche_repet_pop["breche"], 
                                                   grid = as(SpatialPixels, "SpatialPixels"),
                                                   h = mean(c(h.silverman_x_foraging_breche_repet_pop,
                                                              h.silverman_y_foraging_breche_repet_pop)))
##                     ##
## valeur répétabilité ##
##                     ##

overlap.foraging_breche_repet_pop <- kerneloverlaphr(kud.foraging_breche_repet_pop, method = "BA")
mean_overlap.foraging_breche_repet_pop <- mean(overlap.foraging_breche_repet_pop, na.rm = T) ; mean

# overlap_matrix
min_val <- min(overlap.foraging_breche_repet_pop, na.rm = TRUE)
max_val <- max(overlap.foraging_breche_repet_pop, na.rm = TRUE)
ordre <- c("ouverture progressive","ouverture complète")
overlap.foraging_breche_repet_pop <- overlap.foraging_breche_repet_pop[ordre, ordre]

plot.overlapp_foraging_breche_repet_pop <- ggcorrplot(overlap.foraging_breche_repet_pop,
                                                               hc.order = FALSE,
                                                               method = "circle",
                                                               type = "lower",
                                                               lab = TRUE,
                                                               digits = 1,
                                                               colors = c("white", "yellow", "red"),
                                                               ggtheme = theme_minimal()) +
  scale_fill_gradientn(colors = c("white", "yellow", "red"),
                       limits = c(min_val, 
                                  max_val)) ; plot.overlapp_foraging_breche_repet_pop

########################## ---
# Chasse -----------------------------------------------------------------------
########################## ---

# data sets ---

chasse <- read_delim(paste0(data_path, "Chasse/2025_02_27_16h29m12_XXX_Frequentation_des_sites_Chasseurs__RNMO.csv"), 
                     delim = ";", escape_double = FALSE, trim_ws = TRUE)

# chasse_date <- read_excel("D:/Projets_Suzanne/Courlis/3) Data/1) data/Chasse/date ouverture fermeture chasse.xlsx")


# effectif chasse ---

# chasse <- chasse %>% 
#   mutate(
#     Saison = case_when(month(date) == 1 ~ paste0(year(date)-1,"/",year(date)),
#                        month(date) != 1 ~ paste0(year(date),"/",year(date)+1)))

# Pas de prospection = NA
chasse$effectif[chasse$effectif==-1] <- NA

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
  filter(nom_site == "DPM",
         year >= min(GPS$year, na.rm=T)) %>% 
  dplyr::select("date", "effectif", "longitude_centroid", "latitude_centroid")

# chasse_all <- chasse %>% 
#   left_join(chasse_date)

# buffer ---

chasse2 <- st_as_sf(chasse, coords = c("longitude_centroid", "latitude_centroid"), crs = 4326)

chasse_buffer <- st_buffer(chasse2[1,], 1000) %>% 
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
    Saison = case_when(month(datetime) %in% c(1,2,3,4,5,6) ~ paste0(year(datetime)-1,"/",year(datetime)),
                       month(datetime) %in% c(7,8,9,10,11,12) ~ paste0(year(datetime),"/",year(datetime)+1)))

chasse <- chasse %>%
  mutate(
    Saison = case_when(month(date) %in% c(1,2,3,4,5,6) ~ paste0(year(date)-1,"/",year(date)),
                       month(date) %in% c(7,8,9,10,11,12) ~ paste0(year(date),"/",year(date)+1)))

# date de fermeture/ouverture periode de chasse 

date_fin_chasse = "-01-31"

chasse$ouverture_fermeture <- as.Date(paste0(as.character(year(chasse$date)), date_fin_chasse))
chasse$debut_in_chasse <- chasse$ouverture_fermeture - 15
chasse$fin_out_chasse <- chasse$ouverture_fermeture + 15

GPS_chasse <- GPS_chasse %>% 
  left_join(chasse)

# que le jour 
GPS_chasse <- GPS_chasse %>% 
  filter(jour_nuit == "jour")

# grid ---

chasse_buffer <- st_transform(chasse_buffer, crs = 2154)

grid_ZOOM_C <- st_read(paste0(data_generated_path, "grid_ZOOM_C.gpkg"))

grid_chasse <- st_intersection(grid_ZOOM_C, chasse_buffer)

raster_chasse <- rast(grid_chasse, resolution = resolution_ZOOM, crs="EPSG:2154")

tmap_mode("view")
map_chasse <- tm_scalebar() +
  tm_shape(RMO) +
  tm_borders(col = "white", lwd = 3, lty = "dashed") +
  tm_shape(grid_chasse) +
  tm_polygons(col = "blue") +
  tm_shape(chasse2[1,]) + # le point DPM
  tm_dots(col = "red") +
  tm_shape(terre_mer) +
  tm_lines(col = "lightblue", lwd = 0.1) ; map_chasse

### variables ------------------------------------------------------------------

# in_out_saison :
# point GPS dans ou hors période de chasse de l'année/saison


# GPS_chasse <- GPS_chasse %>%
#   mutate(
#     Saison = case_when(month(datetime) %in% c(1,2,3,4,5,6) ~ paste0(year(datetime)-1,"/",year(datetime)),
#                        month(datetime) %in% c(7,8,9,10,11,12) ~ paste0(year(datetime),"/",year(datetime)+1)))
# 
# chasse <- chasse %>%
#   mutate(
#     Saison = case_when(month(date) %in% c(1,2,3,4,5,6) ~ paste0(year(date)-1,"/",year(date)),
#                        month(date) %in% c(7,8,9,10,11,12) ~ paste0(year(date),"/",year(date)+1)))

# date de fermeture/ouverture periode de chasse

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
    md = format(y_m_d, "%m-%d"),  # extraire mois-jour
    in_out_saison = case_when(
      md >= debut_md & md < fermeture_md ~ "in",     # saison de chasse
      md >= fermeture_md | md < fin_md ~ "out"     # hors saison
    )
  )

table(GPS_in_out_saison_chasse$in_out_saison)
table(GPS_in_out_saison_chasse$month_numeric[GPS_in_out_saison_chasse$in_out_saison=="in"])

length(tt$datetime[tt$month_numeric==11])

table(tt$month_numeric)
# GPS_in_out_saison_chasse <- tt %>%
#   mutate(in_out_saison = case_when(between(y_m_d, debut_in_chasse, ouverture_fermeture) ~ "in",
#                                    between(y_m_d, ouverture_fermeture, fin_out_chasse) ~ "out"))




tt$ouverture_fermeture <- as.Date(paste0(as.character(year(tt$date)), date_fin_chasse))
tt$debut_in_chasse <- tt$ouverture_fermeture - 15
tt$fin_out_chasse <- tt$ouverture_fermeture + 15











# GPS_in_out_saison_chasse <- GPS_chasse %>% 
#   mutate(in_out_saison = case_when(!between(y_m_d, `Ouverture DPM St Froult`, `Fermeture DPM St Froult`) ~ "out",
#                                     between(y_m_d, `Ouverture DPM St Froult`, `Fermeture DPM St Froult`) ~ "in")) %>% 
#   filter(month_numeric %in% c(7,8,9,10,11,12,1))

# table(GPS_chasse$month_label)
# 
# tt <- GPS_chasse %>%
#   filter(month_label == "févr")
# #   
# 
# GPS_in_out_saison_chasse <- GPS_chasse %>% 
#   mutate(debut_periode_in_chasse = `Fermeture DPM St Froult` - 1296000,
#          fin_periode_out_chasse = `Fermeture DPM St Froult` + 1296000)
# 
# GPS_in_out_saison_chasse$`Fermeture DPM St Froult` <- as.POSIXct(GPS_in_out_saison_chasse$`Fermeture DPM St Froult`)
# GPS_in_out_saison_chasse$fin_periode_out_chasse <- as.POSIXct(GPS_in_out_saison_chasse$fin_periode_out_chasse)
# GPS_in_out_saison_chasse$debut_periode_in_chasse <- as.POSIXct(GPS_in_out_saison_chasse$debut_periode_in_chasse)
# GPS_in_out_saison_chasse$y_m_d <- as.POSIXct(GPS_in_out_saison_chasse$y_m_d)

# GPS_in_out_saison_chasse_2 <- GPS_in_out_saison_chasse %>% 
#   mutate(in_out_saison = case_when(between(y_m_d, `Fermeture DPM St Froult`, fin_periode_out_chasse) ~ "out",
#                                    between(y_m_d, debut_periode_in_chasse, `Fermeture DPM St Froult`) ~ "in"))


GPS_in_out_saison_chasse <- tt %>%
  mutate(in_out_saison = case_when(between(y_m_d, debut_in_chasse, ouverture_fermeture) ~ "in",
                                   between(y_m_d, ouverture_fermeture, fin_out_chasse) ~ "out"))

table(GPS_in_out_saison_chasse$in_out_saison)
table(GPS_in_out_saison_chasse$month_numeric[GPS_in_out_saison_chasse$in_out_saison=="in"])







# jour_de_chasse :
# point GPS le jour ou dans les 2 jours après la présence d'un chasseur

jour_de_chasse_dt <- chasse2 %>% 
  st_drop_geometry() %>% 
  na.omit()

dates_etendus <- jour_de_chasse_dt %>%
  # bind_rows(
    # mutate(jour_de_chasse_dt, date = date + 1),
    # mutate(jour_de_chasse_dt, date = date + 2)
  # ) %>%
  dplyr::select(date, effectif) %>%
  distinct()  # si des doublons existent

dates_etendus$effectif <- as.numeric(as.character(dates_etendus$effectif))
GPS_chasse$effectif <- as.numeric(as.character(GPS_chasse$effectif))

# Joindre les dates étendues à GPS_chasse_2
GPS_jour_de_chasse <- GPS_chasse %>%
  left_join(dates_etendus) %>%
  mutate(
    jour_de_chasse = case_when(
      is.na(effectif)            ~ "non",
      effectif == 0              ~ "oui_0",
      effectif > 0               ~ "oui_plus"
    )
  ) %>% 
  filter(jour_de_chasse %in% c("oui_0", "oui_plus"))

table(GPS_jour_de_chasse$jour_de_chasse)

# jour_de_chasse + effectif quantile : 
# point GPS le jour ou dans les 2 jours après la présence d'un chasseur, et catégories de quantité de chasseur

jour_de_chasse_dt <- chasse2 %>% 
  st_drop_geometry() %>% 
  na.omit()

dates_etendus <- jour_de_chasse_dt %>%
  # bind_rows(
    # mutate(jour_de_chasse_dt, date = date + 1),
    # mutate(jour_de_chasse_dt, date = date + 2)
  # ) %>%
  dplyr::select(date, effectif) %>%
  distinct()  # si des doublons existent

mean <- mean(jour_de_chasse_dt$effectif)

GPS_seuil_chasse <- GPS_chasse %>%
  left_join(dates_etendus) %>%
  mutate(
    seuil_chasse = case_when(
      is.na(effectif) ~ "non",
      effectif == 0 ~ "oui_0",
      effectif < mean ~ "oui_moins_mean",
      effectif >= mean ~ "oui_plus_mean"
    )
  ) %>% 
  filter(seuil_chasse %in% c("oui_0", "oui_moins_mean", "oui_plus_mean"))

table(GPS_seuil_chasse$seuil_chasse)

### in_out_saison --------------------------------------------------------------

#### roosting ------------------------------------------------------------------

# UDmap ---

GPS.roosting_glob_in_out_saison <- GPS_in_out_saison_chasse %>% 
  filter(behavior == "roosting") %>% 
  dplyr::select(lon,lat,in_out_saison) %>% 
  st_drop_geometry() %>% 
  na.omit()

GPS_spa.roosting_glob_in_out_saison <- st_as_sf(GPS.roosting_glob_in_out_saison, coords = c("lon", "lat"), crs = 4326)
GPS_spa.roosting_glob_in_out_saison <- st_transform(GPS_spa.roosting_glob_in_out_saison, crs = 32630) 
GPS_coords.roosting_glob_in_out_saison <- st_coordinates(GPS_spa.roosting_glob_in_out_saison)

# raster/grid
crs_utm <- "EPSG:32630"
SpatRaster <- project(raster_chasse, crs_utm)
RasterLayer <- raster(SpatRaster)
SpatialPixels<- as(RasterLayer, "SpatialPixels") 

# Règle de Silverman
sigma_x.roosting_glob_in_out_saison <- sd(GPS_coords.roosting_glob_in_out_saison[,1]) 
sigma_y.roosting_glob_in_out_saison <- sd(GPS_coords.roosting_glob_in_out_saison[,2]) 
n.roosting_glob_in_out_saison <- nrow(GPS.roosting_glob_in_out_saison) 
h.silverman_x_roosting_glob_in_out_saison <- 1.06 * sigma_x.roosting_glob_in_out_saison * n.roosting_glob_in_out_saison^(-1/5) / 2
h.silverman_y_roosting_glob_in_out_saison <- 1.06 * sigma_y.roosting_glob_in_out_saison * n.roosting_glob_in_out_saison^(-1/5) / 2
locs_spa.roosting_glob_in_out_saison <- as(GPS_spa.roosting_glob_in_out_saison, "Spatial")

# KernelUD
kud.roosting_glob_in_out_saison <- kernelUD(locs_spa.roosting_glob_in_out_saison["in_out_saison"], 
                                  grid = SpatialPixels, 
                                  h = mean(c(h.silverman_x_roosting_glob_in_out_saison, h.silverman_y_roosting_glob_in_out_saison)))

kud.list_roosting_glob_in_out_saison <- lapply(names(kud.roosting_glob_in_out_saison), function(in_out_saison) {
  
  print(in_out_saison)
  
  # Extraire l'estimation de densité pour un ID spécifique
  kud_simple.roosting_glob_in_out_saison <- kud.roosting_glob_in_out_saison[[in_out_saison]]
  rast.roosting_glob_in_out_saison <- rast(kud_simple.roosting_glob_in_out_saison)
  courtour.roosting_glob_in_out_saison <- as.contour(rast.roosting_glob_in_out_saison)
  sf.roosting_glob_in_out_saison <- st_as_sf(courtour.roosting_glob_in_out_saison)
  cast.roosting_glob_in_out_saison <- st_cast(sf.roosting_glob_in_out_saison, "POLYGON")
  cast.roosting_glob_in_out_saison$in_out_saison <- in_out_saison
  
  return(cast.roosting_glob_in_out_saison)
})

# Fusionner tous les ID dans un seul objet sf
results_kud.roosting_glob_in_out_saison <- do.call(rbind, kud.list_roosting_glob_in_out_saison)
results_kud.roosting_glob_in_out_saison$in_out_saison <- as.factor(results_kud.roosting_glob_in_out_saison$in_out_saison)

# write & read
st_write(results_kud.roosting_glob_in_out_saison, paste0(data_generated_path, "results_kud.roosting_glob_in_out_saison.gpkg"), append = FALSE)
results_kud.roosting_glob_in_out_saison <- st_read(file.path(data_generated_path, "results_kud.roosting_glob_in_out_saison.gpkg"))

# plot
tmap_mode("view")
UDMap_100x100_roosting_in_out_saison_glob <- tm_scalebar() +   
  tm_basemap(c("OpenStreetMap", "Esri.WorldImagery", "CartoDB.Positron")) +
  tm_shape(results_kud.roosting_glob_in_out_saison) + 
  tm_polygons(border.col = "grey", fill = "level", fill_alpha = 1, 
              palette = palette_roosting) +
  tm_facets("in_out_saison") + 
  tm_shape(RMO) +
  tm_borders(col = "white", lwd = 3, lty = "dashed") +
  tm_shape(terre_mer) +
  tm_lines(col = "#32B7FF", lwd = 0.5) ; UDMap_100x100_roosting_in_out_saison_glob

#### foraging ------------------------------------------------------------------

# UDmap ---

GPS.foraging_glob_in_out_saison <- GPS_in_out_saison_chasse %>% 
  filter(behavior == "foraging") %>% 
  dplyr::select(lon,lat,in_out_saison) %>% 
  st_drop_geometry() %>% 
  na.omit()

GPS_spa.foraging_glob_in_out_saison <- st_as_sf(GPS.foraging_glob_in_out_saison, coords = c("lon", "lat"), crs = 4326)
GPS_spa.foraging_glob_in_out_saison <- st_transform(GPS_spa.foraging_glob_in_out_saison, crs = 32630) 
GPS_coords.foraging_glob_in_out_saison <- st_coordinates(GPS_spa.foraging_glob_in_out_saison)

# raster/grid
crs_utm <- "EPSG:32630"
SpatRaster <- project(raster_chasse, crs_utm)
RasterLayer <- raster(SpatRaster)
SpatialPixels<- as(RasterLayer, "SpatialPixels") 

# Règle de Silverman
sigma_x.foraging_glob_in_out_saison <- sd(GPS_coords.foraging_glob_in_out_saison[,1]) 
sigma_y.foraging_glob_in_out_saison <- sd(GPS_coords.foraging_glob_in_out_saison[,2]) 
n.foraging_glob_in_out_saison <- nrow(GPS.foraging_glob_in_out_saison) 
h.silverman_x_foraging_glob_in_out_saison <- 1.06 * sigma_x.foraging_glob_in_out_saison * n.foraging_glob_in_out_saison^(-1/5) / 2
h.silverman_y_foraging_glob_in_out_saison <- 1.06 * sigma_y.foraging_glob_in_out_saison * n.foraging_glob_in_out_saison^(-1/5) / 2
locs_spa.foraging_glob_in_out_saison <- as(GPS_spa.foraging_glob_in_out_saison, "Spatial")

# KernelUD
kud.foraging_glob_in_out_saison <- kernelUD(locs_spa.foraging_glob_in_out_saison["in_out_saison"], 
                                            grid = SpatialPixels, 
                                            h = mean(c(h.silverman_x_foraging_glob_in_out_saison, h.silverman_y_foraging_glob_in_out_saison)))

kud.list_foraging_glob_in_out_saison <- lapply(names(kud.foraging_glob_in_out_saison), function(in_out_saison) {
  
  print(in_out_saison)
  
  # Extraire l'estimation de densité pour un ID spécifique
  kud_simple.foraging_glob_in_out_saison <- kud.foraging_glob_in_out_saison[[in_out_saison]]
  rast.foraging_glob_in_out_saison <- rast(kud_simple.foraging_glob_in_out_saison)
  courtour.foraging_glob_in_out_saison <- as.contour(rast.foraging_glob_in_out_saison)
  sf.foraging_glob_in_out_saison <- st_as_sf(courtour.foraging_glob_in_out_saison)
  cast.foraging_glob_in_out_saison <- st_cast(sf.foraging_glob_in_out_saison, "POLYGON")
  cast.foraging_glob_in_out_saison$in_out_saison <- in_out_saison
  
  return(cast.foraging_glob_in_out_saison)
})

# Fusionner tous les ID dans un seul objet sf
results_kud.foraging_glob_in_out_saison <- do.call(rbind, kud.list_foraging_glob_in_out_saison)
results_kud.foraging_glob_in_out_saison$in_out_saison <- as.factor(results_kud.foraging_glob_in_out_saison$in_out_saison)

# write & read
st_write(results_kud.foraging_glob_in_out_saison, paste0(data_generated_path, "results_kud.foraging_glob_in_out_saison.gpkg"), append = FALSE)
results_kud.foraging_glob_in_out_saison <- st_read(file.path(data_generated_path, "results_kud.foraging_glob_in_out_saison.gpkg"))

# plot
tmap_mode("view")
UDMap_100x100_foraging_in_out_saison_glob <- tm_scalebar() +   
  tm_basemap(c("OpenStreetMap", "Esri.WorldImagery", "CartoDB.Positron")) +
  tm_shape(results_kud.foraging_glob_in_out_saison) + 
  tm_polygons(border.col = "grey", fill = "level", fill_alpha = 1, 
              palette = palette_foraging) +
  tm_facets("in_out_saison") + 
  tm_shape(RMO) +
  tm_borders(col = "white", lwd = 3, lty = "dashed") +
  tm_shape(terre_mer) +
  tm_lines(col = "#32B7FF", lwd = 0.5) ; UDMap_100x100_foraging_in_out_saison_glob

### jour_de_chasse -------------------------------------------------------------

#### roosting ------------------------------------------------------------------

# UDmap ---

GPS.roosting_glob_jour_de_chasse <- GPS_jour_de_chasse %>% 
  filter(behavior == "roosting") %>% 
  dplyr::select(lon,lat,jour_de_chasse) %>% 
  st_drop_geometry() %>% 
  na.omit()

GPS_spa.roosting_glob_jour_de_chasse <- st_as_sf(GPS.roosting_glob_jour_de_chasse, coords = c("lon", "lat"), crs = 4326)
GPS_spa.roosting_glob_jour_de_chasse <- st_transform(GPS_spa.roosting_glob_jour_de_chasse, crs = 32630) 
GPS_coords.roosting_glob_jour_de_chasse <- st_coordinates(GPS_spa.roosting_glob_jour_de_chasse)

# raster/grid
crs_utm <- "EPSG:32630"
SpatRaster <- project(raster_chasse, crs_utm)
RasterLayer <- raster(SpatRaster)
SpatialPixels<- as(RasterLayer, "SpatialPixels") 

# Règle de Silverman
sigma_x.roosting_glob_jour_de_chasse <- sd(GPS_coords.roosting_glob_jour_de_chasse[,1]) 
sigma_y.roosting_glob_jour_de_chasse <- sd(GPS_coords.roosting_glob_jour_de_chasse[,2]) 
n.roosting_glob_jour_de_chasse <- nrow(GPS.roosting_glob_jour_de_chasse) 
h.silverman_x_roosting_glob_jour_de_chasse <- 1.06 * sigma_x.roosting_glob_jour_de_chasse * n.roosting_glob_jour_de_chasse^(-1/5) / 2
h.silverman_y_roosting_glob_jour_de_chasse <- 1.06 * sigma_y.roosting_glob_jour_de_chasse * n.roosting_glob_jour_de_chasse^(-1/5) / 2
locs_spa.roosting_glob_jour_de_chasse <- as(GPS_spa.roosting_glob_jour_de_chasse, "Spatial")

# KernelUD
kud.roosting_glob_jour_de_chasse <- kernelUD(locs_spa.roosting_glob_jour_de_chasse["jour_de_chasse"], 
                                            grid = SpatialPixels, 
                                            h = mean(c(h.silverman_x_roosting_glob_jour_de_chasse, h.silverman_y_roosting_glob_jour_de_chasse)))

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
  tm_polygons(border.col = "grey", fill = "level", fill_alpha = 1, 
              palette = palette_roosting) +
  tm_facets("jour_de_chasse") + 
  tm_shape(RMO) +
  tm_borders(col = "white", lwd = 3, lty = "dashed") +
  tm_shape(terre_mer) +
  tm_lines(col = "#32B7FF", lwd = 0.5) ; UDMap_100x100_roosting_jour_de_chasse_glob

#### foraging ------------------------------------------------------------------

# UDmap ---

GPS.foraging_glob_jour_de_chasse <- GPS_jour_de_chasse %>% 
  filter(behavior == "foraging") %>% 
  dplyr::select(lon,lat,jour_de_chasse) %>% 
  st_drop_geometry() %>% 
  na.omit()

GPS_spa.foraging_glob_jour_de_chasse <- st_as_sf(GPS.foraging_glob_jour_de_chasse, coords = c("lon", "lat"), crs = 4326)
GPS_spa.foraging_glob_jour_de_chasse <- st_transform(GPS_spa.foraging_glob_jour_de_chasse, crs = 32630) 
GPS_coords.foraging_glob_jour_de_chasse <- st_coordinates(GPS_spa.foraging_glob_jour_de_chasse)

# raster/grid
crs_utm <- "EPSG:32630"
SpatRaster <- project(raster_chasse, crs_utm)
RasterLayer <- raster(SpatRaster)
SpatialPixels<- as(RasterLayer, "SpatialPixels") 

# Règle de Silverman
sigma_x.foraging_glob_jour_de_chasse <- sd(GPS_coords.foraging_glob_jour_de_chasse[,1]) 
sigma_y.foraging_glob_jour_de_chasse <- sd(GPS_coords.foraging_glob_jour_de_chasse[,2]) 
n.foraging_glob_jour_de_chasse <- nrow(GPS.foraging_glob_jour_de_chasse) 
h.silverman_x_foraging_glob_jour_de_chasse <- 1.06 * sigma_x.foraging_glob_jour_de_chasse * n.foraging_glob_jour_de_chasse^(-1/5) / 2
h.silverman_y_foraging_glob_jour_de_chasse <- 1.06 * sigma_y.foraging_glob_jour_de_chasse * n.foraging_glob_jour_de_chasse^(-1/5) / 2
locs_spa.foraging_glob_jour_de_chasse <- as(GPS_spa.foraging_glob_jour_de_chasse, "Spatial")

# KernelUD
kud.foraging_glob_jour_de_chasse <- kernelUD(locs_spa.foraging_glob_jour_de_chasse["jour_de_chasse"], 
                                             grid = SpatialPixels, 
                                             h = mean(c(h.silverman_x_foraging_glob_jour_de_chasse, h.silverman_y_foraging_glob_jour_de_chasse)))

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
  tm_polygons(border.col = "grey", fill = "level", fill_alpha = 1, 
              palette = palette_foraging) +
  tm_facets("jour_de_chasse") + 
  tm_shape(RMO) +
  tm_borders(col = "white", lwd = 3, lty = "dashed") +
  tm_shape(terre_mer) +
  tm_lines(col = "#32B7FF", lwd = 0.5) ; UDMap_100x100_foraging_jour_de_chasse_glob

### seuil_chasse ---------------------------------------------------------------

#### roosting ------------------------------------------------------------------

# UDmap ---

GPS.roosting_glob_seuil_chasse <- GPS_seuil_chasse %>% 
  filter(behavior == "roosting") %>% 
  dplyr::select(lon,lat,seuil_chasse) %>% 
  st_drop_geometry() %>% 
  na.omit()

GPS_spa.roosting_glob_seuil_chasse <- st_as_sf(GPS.roosting_glob_seuil_chasse, coords = c("lon", "lat"), crs = 4326)
GPS_spa.roosting_glob_seuil_chasse <- st_transform(GPS_spa.roosting_glob_seuil_chasse, crs = 32630) 
GPS_coords.roosting_glob_seuil_chasse <- st_coordinates(GPS_spa.roosting_glob_seuil_chasse)

# raster/grid
crs_utm <- "EPSG:32630"
SpatRaster <- project(raster_chasse, crs_utm)
RasterLayer <- raster(SpatRaster)
SpatialPixels<- as(RasterLayer, "SpatialPixels") 

# Règle de Silverman
sigma_x.roosting_glob_seuil_chasse <- sd(GPS_coords.roosting_glob_seuil_chasse[,1]) 
sigma_y.roosting_glob_seuil_chasse <- sd(GPS_coords.roosting_glob_seuil_chasse[,2]) 
n.roosting_glob_seuil_chasse <- nrow(GPS.roosting_glob_seuil_chasse) 
h.silverman_x_roosting_glob_seuil_chasse <- 1.06 * sigma_x.roosting_glob_seuil_chasse * n.roosting_glob_seuil_chasse^(-1/5) / 2
h.silverman_y_roosting_glob_seuil_chasse <- 1.06 * sigma_y.roosting_glob_seuil_chasse * n.roosting_glob_seuil_chasse^(-1/5) / 2
locs_spa.roosting_glob_seuil_chasse <- as(GPS_spa.roosting_glob_seuil_chasse, "Spatial")

# KernelUD
kud.roosting_glob_seuil_chasse <- kernelUD(locs_spa.roosting_glob_seuil_chasse["seuil_chasse"], 
                                             grid = SpatialPixels, 
                                             h = mean(c(h.silverman_x_roosting_glob_seuil_chasse, h.silverman_y_roosting_glob_seuil_chasse)))

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
  tm_polygons(border.col = "grey", fill = "level", fill_alpha = 1, 
              palette = palette_roosting) +
  tm_facets("seuil_chasse") + 
  tm_shape(RMO) +
  tm_borders(col = "white", lwd = 3, lty = "dashed") +
  tm_shape(terre_mer) +
  tm_lines(col = "#32B7FF", lwd = 0.5) ; UDMap_100x100_roosting_seuil_chasse_glob

#### foraging ------------------------------------------------------------------

# UDmap ---

GPS.foraging_glob_seuil_chasse <- GPS_seuil_chasse %>% 
  filter(behavior == "foraging") %>% 
  dplyr::select(lon,lat,seuil_chasse) %>% 
  st_drop_geometry() %>% 
  na.omit()

GPS_spa.foraging_glob_seuil_chasse <- st_as_sf(GPS.foraging_glob_seuil_chasse, coords = c("lon", "lat"), crs = 4326)
GPS_spa.foraging_glob_seuil_chasse <- st_transform(GPS_spa.foraging_glob_seuil_chasse, crs = 32630) 
GPS_coords.foraging_glob_seuil_chasse <- st_coordinates(GPS_spa.foraging_glob_seuil_chasse)

# raster/grid
crs_utm <- "EPSG:32630"
SpatRaster <- project(raster_chasse, crs_utm)
RasterLayer <- raster(SpatRaster)
SpatialPixels<- as(RasterLayer, "SpatialPixels") 

# Règle de Silverman
sigma_x.foraging_glob_seuil_chasse <- sd(GPS_coords.foraging_glob_seuil_chasse[,1]) 
sigma_y.foraging_glob_seuil_chasse <- sd(GPS_coords.foraging_glob_seuil_chasse[,2]) 
n.foraging_glob_seuil_chasse <- nrow(GPS.foraging_glob_seuil_chasse) 
h.silverman_x_foraging_glob_seuil_chasse <- 1.06 * sigma_x.foraging_glob_seuil_chasse * n.foraging_glob_seuil_chasse^(-1/5) / 2
h.silverman_y_foraging_glob_seuil_chasse <- 1.06 * sigma_y.foraging_glob_seuil_chasse * n.foraging_glob_seuil_chasse^(-1/5) / 2
locs_spa.foraging_glob_seuil_chasse <- as(GPS_spa.foraging_glob_seuil_chasse, "Spatial")

# KernelUD
kud.foraging_glob_seuil_chasse <- kernelUD(locs_spa.foraging_glob_seuil_chasse["seuil_chasse"], 
                                           grid = SpatialPixels, 
                                           h = mean(c(h.silverman_x_foraging_glob_seuil_chasse, h.silverman_y_foraging_glob_seuil_chasse)))

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
  tm_polygons(border.col = "grey", fill = "level", fill_alpha = 1, 
              palette = palette_foraging) +
  tm_facets("seuil_chasse") + 
  tm_shape(RMO) +
  tm_borders(col = "white", lwd = 3, lty = "dashed") +
  tm_shape(terre_mer) +
  tm_lines(col = "#32B7FF", lwd = 0.5) ; UDMap_100x100_foraging_seuil_chasse_glob

########################## ---
# Tonnes de chasse -------------------------------------------------------------
########################## ---

## data ------------------------------------------------------------------------

# tonne 

tonnes <- st_read(paste0(data_path, "Tonnes_de_chasse/tonnes.shp"))

tonnes <- st_intersection(tonnes, BOX_2154)

# 1. Créer les buffers
tonnes_buffer <- tonnes %>%
  st_buffer(dist = 300)
# 3. Fusionner tous les buffers en un seul objet
tonnes_unioned <- st_union(tonnes_buffer)
# 4. Intersecter ce polygone fusionné avec tous les buffers originaux
# Cela va forcer un vrai découpage des zones de recouvrement
tonnes_cut_zones <- st_intersection(tonnes_unioned, tonnes_buffer)
# 5. Calculer le nombre de buffers qui recouvrent chaque morceau
tonnes_zones_final <- tonnes_cut_zones %>%
  st_sf() %>%
  mutate(overlap_count = lengths(st_intersects(geometry, tonnes_buffer))) %>%
  filter(overlap_count >= 1) # garder uniquement les zones couvertes
# 6. Fusionner les zones ayant le même niveau de recouvrement (facultatif mais utile)
tonnes_zones_grouped <- tonnes_zones_final %>%
  group_by(overlap_count) %>%
  summarise(geometry = st_union(geometry), .groups = "drop")

# histogram
hist(tonnes_zones_grouped$overlap_count)

tonnes_zones_grouped_clean <- tonnes_zones_grouped[!is.na(tonnes_zones_grouped$overlap_count), ]

# maps
tmap_mode("view")
map_tonnes <- tm_scalebar() +   
  tm_shape(RMO) +
  tm_borders(col = "white", lwd = 3, lty = "dashed") +
  tm_shape(terre_mer) +
  tm_lines(col = "#32B7FF", lwd = 0.5) +
  tm_shape(tonnes_zones_grouped_clean) +
  tm_basemap(c("OpenStreetMap", "Esri.WorldImagery", "CartoDB.Positron")) + 
  tm_polygons(fill = "overlap_count",
              palette = c("#FFF07C", "orange", "#D64045", "darkred"), 
              style = "cont", alpha = 0.5,
              title = "Nb superposées") +
  tm_shape(tonnes) +
  tm_dots(fill = "black") +
  tm_layout(title = "Superposition des tonnes de chasse (300 m de rayon)") ; map_tonnes

tmap_save(map_tonnes, paste0(atlas_path,"map_tonnes.html"))

# date 
chasse_date <- read_excel("D:/Projets_Suzanne/Courlis/3) Data/1) data/Chasse/date ouverture fermeture chasse.xlsx")

tonnes_date <- chasse_date %>% 
  dplyr::select(Saison, `Ouverture Gibier d'eau`, `Fermeture Gibier d'eau`)

# GPS 

open = format(as.POSIXct(tonnes_date$`Ouverture Gibier d'eau`[1], format = "%Y-%m-%d UTC", tz = "UTC"), "%m-%d")
close = format(as.POSIXct(tonnes_date$`Fermeture Gibier d'eau`[1], format = "%Y-%m-%d UTC", tz = "UTC"), "%m-%d")

GPS_tonnes <- GPS %>%
  filter(jour_nuit=="nuit") %>% 
  mutate(open_tonnes = ymd(paste0(year,"-", open)),
         close_tonnes = ymd(paste0(year+1,"-", close)),
         tonnes_period = ifelse(between(y_m_d, open_tonnes, close_tonnes), "chasse ouverte", "pas de chasse"))

## temps dans les zones de danger v1 ----------------------------------------------

# selectionner que les ind avec des points GPS dans les zones de danger (au moins 100 points)
# regarde le nombre de point dans les zones de danger en fonction de la date, et de la periode de chasse
# faire un graph avec nb point par semaine ~ semaine (+ bar periode en vertical) => voir si ça diminue

tonnes_zones_grouped_clean <- st_transform(tonnes_zones_grouped_clean, st_crs(GPS_tonnes))

tonnes_zones_grouped_clean <- st_make_valid(tonnes_zones_grouped_clean)

points_dans_danger <- GPS_tonnes %>%
  dplyr::select(ID, tonnes_period, y_m_d, week) %>% 
  st_join(tonnes_zones_grouped_clean, join = st_within) %>%
  filter(!is.na(overlap_count))

# au moins 1000 point par ID
n_per_ID_dans_danger <- points_dans_danger %>% 
  group_by(ID) %>% 
  summarize(n = n())%>% 
  filter(n < 100)

points_dans_danger_1000 <- points_dans_danger %>% 
  filter(ID %ni% n_per_ID_dans_danger$ID)

nb_point_id_tonnes <- points_dans_danger_1000 %>% 
  st_drop_geometry() %>% 
  group_by(ID, week, tonnes_period) %>% 
  summarize(nb_point = n())

nb_point_moy_id_tonnes <- nb_point_id_tonnes %>% 
  st_drop_geometry() %>% 
  group_by(week) %>% 
  mutate(nb_point_mean = mean(nb_point),
         nb_point_sd = sd(nb_point)) %>% 
  dplyr::select(week, nb_point_mean, nb_point_sd) %>% 
  distinct()

open_plot <- as.character(week(tonnes_date$`Ouverture Gibier d'eau`[1]))
close_plot <- as.character(week(tonnes_date$`Fermeture Gibier d'eau`[1]))

# Redéfinir l'ordre de l'axe x : 10 à 50, puis 1 à 9
custom_order <- c(open_plot:max(nb_point_id_tonnes$week), 1:open_plot-1)
nb_point_id_tonnes$week_factor <- factor(nb_point_id_tonnes$week, levels = custom_order)

custom_order <- c(open_plot:max(nb_point_moy_id_tonnes$week), 1:open_plot-1)
nb_point_moy_id_tonnes$week_factor <- factor(nb_point_moy_id_tonnes$week, levels = custom_order)

# plot
point_tonnes_plot <- ggplot() + 
  geom_line(data = nb_point_id_tonnes, aes(x=week_factor, y=nb_point, color = ID, group = ID, fill = ID),
            size = 1) + 
  geom_point(data = nb_point_moy_id_tonnes, aes(x=week_factor, y=nb_point_mean),
            size = 3, color = "yellow") + 
  geom_vline(xintercept = open_plot,  
             color = "black", size=1.5) +
  geom_vline(xintercept = close_plot,
             color = "black", size=1.5) +
  theme_classic() +
  labs(title="",
       x ="Individu", y = "Nombre de point GPS dans une zone de danger",
       color = "individu") ; point_tonnes_plot

ggsave(paste0(atlas_path, "/point_tonnes_plot.png"), 
       plot = point_tonnes_plot, width = 10, height = 5, dpi = 1000)

## temps dans les zones de danger v2 ----------------------------------------------

# créer zone de danger de 300m
# créer zone de proximité de 1 km
# selectionner les point GPS dans les zones de danger et proximité 
# garder les ind que avec assez de point dans les deux zones
# proportion danger/proximité ~ week par ind 
# voir si plus bas pendant la période de chasse


tonnes_danger300 <- tonnes %>%
  st_buffer(dist = 300)
tonnes_proxi1000 <- tonnes %>%
  st_buffer(dist = 1500)

tonnes_danger300_unioned <- st_union(tonnes_danger300)
tonnes_proxi1000_unioned <- st_union(tonnes_proxi1000)

area_danger <- as.numeric(st_area(tonnes_danger300_unioned)) / 1000000
area_proxi <- as.numeric(st_area(tonnes_proxi1000_unioned)) / 1000000

# maps
tmap_mode("view")
map_tonnes_v2 <- tm_scalebar() +  
  tm_basemap(c("OpenStreetMap", "Esri.WorldImagery", "CartoDB.Positron")) + 
  tm_shape(RMO) +
  tm_borders(col = "white", lwd = 3, lty = "dashed") +
  tm_shape(terre_mer) +
  tm_lines(col = "#32B7FF", lwd = 0.5) +
  tm_shape(tonnes_proxi1000_unioned) +
  tm_polygons(fill = "#FFF07C", alpha = 0.7) +
  tm_shape(tonnes_danger300_unioned) +
  tm_polygons(fill = "#D64045", alpha = 1) +
  tm_shape(tonnes) +
  tm_dots(fill = "black") +
  tm_layout(title = "Tonne de chasse, zone de danger (300 m), zone de proximité (1500 m)") ; map_tonnes_v2

tmap_save(map_tonnes_v2, paste0(atlas_path,"map_tonnes_v2.html"))


tonnes_proxi1000_unioned <- st_as_sf(tonnes_proxi1000_unioned)
tonnes_proxi1000_unioned <- st_transform(tonnes_proxi1000_unioned, st_crs(GPS_tonnes))
tonnes_proxi1000_unioned <- st_make_valid(tonnes_proxi1000_unioned)

tonnes_danger300_unioned <- st_as_sf(tonnes_danger300_unioned)
tonnes_danger300_unioned <- st_transform(tonnes_danger300_unioned, st_crs(GPS_tonnes))
tonnes_danger300_unioned <- st_make_valid(tonnes_danger300_unioned)

points_dans_proxi <- GPS_tonnes %>%
  dplyr::select(ID, tonnes_period, y_m_d, week, jour_nuit) %>%
  st_filter(tonnes_proxi1000_unioned)

table(points_dans_proxi$ID)

# au moins 1000 point par ID
n_per_ID_dans_proxi <- points_dans_proxi %>% 
  group_by(ID) %>% 
  summarize(n = n())%>% 
  filter(n < 1)

points_dans_proxi <- points_dans_proxi %>% 
  filter(ID %ni% n_per_ID_dans_proxi$ID)

table(points_dans_proxi$ID)

points_dans_proxi <- points_dans_proxi %>%
  mutate(
    zone = ifelse(
      st_within(points_dans_proxi, tonnes_danger300_unioned, sparse = FALSE)[, 1],
      "zone de danger",
      "zone marginale"
    )
  )

nb_point_id_tonnes_v2 <- points_dans_proxi %>% 
  st_drop_geometry() %>% 
  group_by(ID, week, tonnes_period, zone, jour_nuit) %>% 
  summarize(nb_point = n(), .groups = "drop") %>% 
  pivot_wider(
    names_from = zone,
    values_from = nb_point,
    values_fill = 0  # remplit les NA par 0 si un ID/week n’a pas de points dans une zone
  )


prop_id_tonnes_v2 <- nb_point_id_tonnes_v2 %>% 
  st_drop_geometry() %>% 
  group_by(ID, week, tonnes_period) %>% 
  mutate(prop_danger_proxi = `zone de danger`/`zone marginale`) %>% 
  dplyr::select(ID, week, tonnes_period, prop_danger_proxi, jour_nuit) %>% 
  distinct()

open_plot <- as.character(week(tonnes_date$`Ouverture Gibier d'eau`[1]))
close_plot <- as.character(week(tonnes_date$`Fermeture Gibier d'eau`[1]))

# Redéfinir l'ordre de l'axe x : 10 à 50, puis 1 à 9
custom_order <- c(open_plot:max(prop_id_tonnes_v2$week), 1:open_plot-1)
prop_id_tonnes_v2$week_factor <- factor(prop_id_tonnes_v2$week, levels = custom_order)

prop_id_tonnes_v2 <- prop_id_tonnes_v2[prop_id_tonnes_v2$prop_danger_proxi!="Inf",]

# prop_id_tonnes_v2 <- prop_id_tonnes_v2[prop_id_tonnes_v2$ID!="EA635103",]
# prop_id_tonnes_v2 <- prop_id_tonnes_v2[prop_id_tonnes_v2$ID!="EA580462",]

# plot
point_tonnes_v2_plot <- ggplot(data = prop_id_tonnes_v2, aes(x=week_factor, y=prop_danger_proxi, 
                                                             color = ID, group = ID, fill = ID)) + 
  geom_line(size = 0.5) +
  geom_point(size = 2, shape = 21, fill = "white") +
  geom_vline(xintercept = open_plot,  
             color = "#D64045", size=1) +
  geom_vline(xintercept = close_plot,
             color = "#D64045", size=1) +
  stat_summary(aes(group = 1), 
               fun = mean, 
               fun.min = function(x) mean(x) - sd(x), 
               fun.max = function(x) mean(x) + sd(x),
               geom = "pointrange", 
               color = "black", size = 0.5) +
  scale_color_grey() +  
  theme_classic() +
  theme(legend.position='none') +
  labs(title="",
       x ="Semaine", y = "Ratio zone de danger / zone de proximité",
       color = "individu") ; point_tonnes_v2_plot

ggsave(paste0(atlas_path, "/point_tonnes_v2_plot.png"),
       plot = point_tonnes_v2_plot, width = 8, height = 5, dpi = 1000)

tt <- points_dans_proxi %>%
  st_drop_geometry() %>%
  group_by(ID, week, tonnes_period, zone) %>%
  summarize(nb_point = n(), .groups = "drop")

tt <- tt %>%
  mutate(nb_area = case_when(zone=="zone de danger" ~ (nb_point/area_danger),
                             TRUE ~ (nb_point/area_proxi)))

tt$tonnes_period <- as.factor(tt$tonnes_period)
tt$tonnes_period <- factor(tt$tonnes_period, levels = c("pas de chasse", "chasse ouverte"))
tt$zone <- as.factor(tt$zone)
tt$zone <- factor(tt$zone, levels = c("zone marginale", "zone de danger"))

# Réestimer le modèle
lmer_model <- lmer(nb_area ~ zone * tonnes_period + (1 | ID), data = tt)

# Résumé avec p-values
summary(lmer_model)

# Résultats au format tidy
fixed <- tidy(lmer_model, effects = "fixed", conf.int = TRUE)

# Ajouter les étoiles
fixed$signif <- cut(fixed$p.value,
                    breaks = c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf),
                    labels = c("***", "**", "*", ".", ""))

# R²
r2 <- as.data.frame(r2(lmer_model))

# Variance des effets aléatoires
random <- as.data.frame(VarCorr(lmer_model))
random <- random[, c("grp", "vcov", "sdcor")]
colnames(random) <- c("Effet", "Variance", "Écart-type")

# Sauvegarder tout
saveRDS(list(fixed = fixed, r2 = r2, random = random), paste0(atlas_path,"resultats_modeles.rds"))

lmer_model_jour <- readRDS(paste0(atlas_path,"resultats_modeles.rds"))

# Effets moyens pour interaction zone * tonnes_period
preds <- ggpredict(lmer_model, terms = c("zone", "tonnes_period"))

# Plot interactif
plot(preds) + theme_minimal()

preds_tonnes_chasses_plot <- ggplot(preds, aes(x = group, y = predicted, color = x, group = x)) +
  geom_point(size = 4) +
  geom_line(size = 2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = x),
              alpha = 0.2, color = NA) +
  scale_color_manual(values = c("zone de danger" = "#D64045", "zone marginale" = "#FFF07C")) +
  scale_fill_manual(values = c("zone de danger" = "#D64045", "zone marginale" = "#FFF07C")) +
  labs(x = "Période de chasse", y = "Nombre de point GPS / surface de la zone", 
       color = "Zone", fill = "Zone") +
  theme_hc() +
  theme(legend.position = c(0.8,0.9)); preds_tonnes_chasses_plot

ggsave(paste0(atlas_path, "/preds_tonnes_chasses_plot.png"),
       plot = preds_tonnes_chasses_plot, width = 5, height = 5, dpi = 1000)

########################## ---
# *ECE -------------------------------------------------------------------------
########################## ---

## Météo ------------------------------------------------------------------------

meteo <- read_excel(paste0(data_path, "/Meteo/meteo_courlis_la_rochelle.xlsx"))

meteo_2 <- meteo %>% 
  dplyr::select(date,tavg,tmin,tmax,prcp,wdir,wspd,pres) %>% 
  rename(y_m_d = date) %>% 
  mutate(y_m_d = ymd(y_m_d))

meteo_3 <- meteo_2 %>% 
  mutate(ECE_wspd = case_when(wspd >= quantile(wspd, .95, na.rm=T) ~ "ECE95%",
                              TRUE ~ "RAS"),
         ECE_wNO = case_when(between(wdir, 270,max(meteo$wdir, na.rm = T)) ~ "ECE Nord-Ouest",
                             TRUE ~ "RAS"),
         ECE_wNO_wspd80 = case_when(wspd >= quantile(wspd, .80, na.rm=T) &
                                      ECE_wNO == "ECE Nord-Ouest" ~ "ECE80% & Nord-Ouest",
                                    TRUE ~ "RAS"),
         ECE_wNO_wspd95 = case_when(wspd >= quantile(wspd, .95, na.rm=T) &
                                      ECE_wNO == "ECE Nord-Ouest" ~ "ECE95% & Nord-Ouest",
                                    TRUE ~ "RAS"))

table(meteo_3$ECE_wspd)
table(meteo_3$ECE_wNO)
table(meteo_3$ECE_wNO_wspd80)
table(meteo_3$ECE_wNO_wspd95)

GPS <- left_join(GPS, meteo_3)

## # # # # # --- 
## *wspd ----------------------------------------------------------------------
## # # # # # ---

## # # # # # --- 
### *reposoir  ------------------------------------------------------------------
## # # # # # ---

zoom_level <- c("A","B","C","D","E")
analyse = "roosting_ECE_wspd"
results_kud = NULL
nb_kud = NULL
comportement = "roosting"
param <- "ECE_wspd"
couleur = nom_pal_roosting
map_kud.roosting_ECE_wspd <- Map(estimate_kud_param, zoom_level, comportement, param)
results_kud.roosting_ECE_wspd <- do.call(rbind, map_kud.roosting_ECE_wspd)
st_write(results_kud.roosting_ECE_wspd, paste0(data_generated_path, "results_kud.", analyse, ".gpkg"), append = FALSE)
results_kud.roosting <- st_read(file.path(data_generated_path, paste0("results_kud.", analyse,".gpkg")))
# compter les nb ind par zoom
nb_kud_map.roosting_ECE_wspd <- Map(count_nb_kud_param, zoom_level, comportement, param)
nb_kud.roosting_ECE_wspd <- do.call(rbind, nb_kud_map.roosting_ECE_wspd)
write.csv(nb_kud.roosting_ECE_wspd, paste0(data_generated_path, "nb_kud.", analyse, ".csv"), row.names = FALSE)
nb_kud.roosting_ECE_wspd <- read.csv(paste0(data_generated_path, paste0("nb_kud.", analyse, ".csv")), row.names = NULL)
# Générer les maps pour chaque zoom
maps_list.roosting_ZOOM_ECE_wspd <- Map(create_map_param, zoom_level, analyse, param, couleur)
beep(3)

## # # # # # --- 
### *alimentation  ---------------------------------------------------------------
## # # # # # ---

zoom_level <- c("A","B","C","D","E")
analyse = "foraging_ECE_wspd"
results_kud = NULL
nb_kud = NULL
comportement = "foraging"
param <- "ECE_wspd"
couleur = nom_pal_foraging
map_kud.foraging_ECE_wspd <- Map(estimate_kud_param, zoom_level, comportement, param)
results_kud.foraging_ECE_wspd <- do.call(rbind, map_kud.foraging_ECE_wspd)
st_write(results_kud.foraging_ECE_wspd, paste0(data_generated_path, "results_kud.", analyse, ".gpkg"), append = FALSE)
results_kud.foraging <- st_read(file.path(data_generated_path, paste0("results_kud.", analyse,".gpkg")))
# compter les nb ind par zoom
nb_kud_map.foraging_ECE_wspd <- Map(count_nb_kud_param, zoom_level, comportement, param)
nb_kud.foraging_ECE_wspd <- do.call(rbind, nb_kud_map.foraging_ECE_wspd)
write.csv(nb_kud.foraging_ECE_wspd, paste0(data_generated_path, "nb_kud.", analyse, ".csv"), row.names = FALSE)
nb_kud.foraging_ECE_wspd <- read.csv(paste0(data_generated_path, paste0("nb_kud.", analyse, ".csv")), row.names = NULL)
# Générer les maps pour chaque zoom
maps_list.foraging_ZOOM_ECE_wspd <- Map(create_map_param, zoom_level, analyse, param, couleur)
beep(3)

## # # # # # --- 
## *Nord-Ouest --------------------------------------------------------------
## # # # # # ---

## # # # # # --- 
### *reposoir  ------------------------------------------------------------------
## # # # # # ---

zoom_level <- c("A","B","C","D","E")
analyse = "roosting_ECE_wNO"
results_kud = NULL
nb_kud = NULL
comportement = "roosting"
param <- "ECE_wNO"
couleur = nom_pal_roosting
map_kud.roosting_ECE_wNO <- Map(estimate_kud_param, zoom_level, comportement, param)
results_kud.roosting_ECE_wNO <- do.call(rbind, map_kud.roosting_ECE_wNO)
st_write(results_kud.roosting_ECE_wNO, paste0(data_generated_path, "results_kud.", analyse, ".gpkg"), append = FALSE)
results_kud.roosting <- st_read(file.path(data_generated_path, paste0("results_kud.", analyse,".gpkg")))
# compter les nb ind par zoom
nb_kud_map.roosting_ECE_wNO <- Map(count_nb_kud_param, zoom_level, comportement, param)
nb_kud.roosting_ECE_wNO <- do.call(rbind, nb_kud_map.roosting_ECE_wNO)
write.csv(nb_kud.roosting_ECE_wNO, paste0(data_generated_path, "nb_kud.", analyse, ".csv"), row.names = FALSE)
nb_kud.roosting_ECE_wNO <- read.csv(paste0(data_generated_path, paste0("nb_kud.", analyse, ".csv")), row.names = NULL)
# Générer les maps pour chaque zoom
maps_list.roosting_ZOOM_ECE_wNO <- Map(create_map_param, zoom_level, analyse, param, couleur)
beep(3)

## # # # # # --- 
### *alimentation  ---------------------------------------------------------------
## # # # # # ---

zoom_level <- c("A","B","C","D","E")
analyse = "foraging_ECE_wNO"
results_kud = NULL
nb_kud = NULL
comportement = "foraging"
param <- "ECE_wNO"
couleur = nom_pal_foraging
map_kud.foraging_ECE_wNO <- Map(estimate_kud_param, zoom_level, comportement, param)
results_kud.foraging_ECE_wNO <- do.call(rbind, map_kud.foraging_ECE_wNO)
st_write(results_kud.foraging_ECE_wNO, paste0(data_generated_path, "results_kud.", analyse, ".gpkg"), append = FALSE)
results_kud.foraging <- st_read(file.path(data_generated_path, paste0("results_kud.", analyse,".gpkg")))
# compter les nb ind par zoom
nb_kud_map.foraging_ECE_wNO <- Map(count_nb_kud_param, zoom_level, comportement, param)
nb_kud.foraging_ECE_wNO <- do.call(rbind, nb_kud_map.foraging_ECE_wNO)
write.csv(nb_kud.foraging_ECE_wNO, paste0(data_generated_path, "nb_kud.", analyse, ".csv"), row.names = FALSE)
nb_kud.foraging_ECE_wNO <- read.csv(paste0(data_generated_path, paste0("nb_kud.", analyse, ".csv")), row.names = NULL)
# Générer les maps pour chaque zoom
maps_list.foraging_ZOOM_ECE_wNO <- Map(create_map_param, zoom_level, analyse, param, couleur)
beep(3)

## # # # # # --- 
## *Nord-Ouest + vent fort -----------------------------------------------------
## # # # # # ---

### *80% ------

## # # # # # --- 
#### *reposoir  ------------------------------------------------------------------
## # # # # # ---


zoom_level <- c("A","B","C","D","E")
analyse = "roosting_ECE_wNO_wspd80"
results_kud = NULL
nb_kud = NULL
comportement = "roosting"
param <- "ECE_wNO_wspd80"
couleur = nom_pal_roosting
map_kud.roosting_ECE_wNO_wspd80 <- Map(estimate_kud_param, zoom_level, comportement, param)
results_kud.roosting_ECE_wNO_wspd80 <- do.call(rbind, map_kud.roosting_ECE_wNO_wspd80)
st_write(results_kud.roosting_ECE_wNO_wspd80, paste0(data_generated_path, "results_kud.", analyse, ".gpkg"), append = FALSE)
results_kud.roosting <- st_read(file.path(data_generated_path, paste0("results_kud.", analyse,".gpkg")))
# compter les nb ind par zoom
nb_kud_map.roosting_ECE_wNO_wspd80 <- Map(count_nb_kud_param, zoom_level, comportement, param)
nb_kud.roosting_ECE_wNO_wspd80 <- do.call(rbind, nb_kud_map.roosting_ECE_wNO_wspd80)
write.csv(nb_kud.roosting_ECE_wNO_wspd80, paste0(data_generated_path, "nb_kud.", analyse, ".csv"), row.names = FALSE)
nb_kud.roosting_ECE_wNO_wspd80 <- read.csv(paste0(data_generated_path, paste0("nb_kud.", analyse, ".csv")), row.names = NULL)
# Générer les maps pour chaque zoom
maps_list.roosting_ZOOM_ECE_wNO_wspd80 <- Map(create_map_param, zoom_level, analyse, param, couleur)
beep(3)

## # # # # # --- 
#### *alimentation  ---------------------------------------------------------------
## # # # # # ---

zoom_level <- c("A","B","C","D","E")
analyse = "foraging_ECE_wNO_wspd80"
results_kud = NULL
nb_kud = NULL
comportement = "foraging"
param <- "ECE_wNO_wspd80"
couleur = nom_pal_foraging
map_kud.foraging_ECE_wNO_wspd80 <- Map(estimate_kud_param, zoom_level, comportement, param)
results_kud.foraging_ECE_wNO_wspd80 <- do.call(rbind, map_kud.foraging_ECE_wNO_wspd80)
st_write(results_kud.foraging_ECE_wNO_wspd80, paste0(data_generated_path, "results_kud.", analyse, ".gpkg"), append = FALSE)
results_kud.foraging <- st_read(file.path(data_generated_path, paste0("results_kud.", analyse,".gpkg")))
# compter les nb ind par zoom
nb_kud_map.foraging_ECE_wNO_wspd80 <- Map(count_nb_kud_param, zoom_level, comportement, param)
nb_kud.foraging_ECE_wNO_wspd80 <- do.call(rbind, nb_kud_map.foraging_ECE_wNO_wspd80)
write.csv(nb_kud.foraging_ECE_wNO_wspd80, paste0(data_generated_path, "nb_kud.", analyse, ".csv"), row.names = FALSE)
nb_kud.foraging_ECE_wNO_wspd80 <- read.csv(paste0(data_generated_path, paste0("nb_kud.", analyse, ".csv")), row.names = NULL)
# Générer les maps pour chaque zoom
maps_list.foraging_ZOOM_ECE_wNO_wspd80 <- Map(create_map_param, zoom_level, analyse, param, couleur)
beep(3)

### *95% ------

## # # # # # --- 
#### *reposoir  ----------------------------------------------------------------
## # # # # # ---

zoom_level <- c("A","B","C","D","E")
analyse = "roosting_ECE_wNO_wspd95"
results_kud = NULL
nb_kud = NULL
comportement = "roosting"
param <- "ECE_wNO_wspd95"
couleur = nom_pal_roosting
map_kud.roosting_ECE_wNO_wspd95 <- Map(estimate_kud_param, zoom_level, comportement, param)
results_kud.roosting_ECE_wNO_wspd95 <- do.call(rbind, map_kud.roosting_ECE_wNO_wspd95)
st_write(results_kud.roosting_ECE_wNO_wspd95, paste0(data_generated_path, "results_kud.", analyse, ".gpkg"), append = FALSE)
results_kud.roosting <- st_read(file.path(data_generated_path, paste0("results_kud.", analyse,".gpkg")))
# compter les nb ind par zoom
nb_kud_map.roosting_ECE_wNO_wspd95 <- Map(count_nb_kud_param, zoom_level, comportement, param)
nb_kud.roosting_ECE_wNO_wspd95 <- do.call(rbind, nb_kud_map.roosting_ECE_wNO_wspd95)
write.csv(nb_kud.roosting_ECE_wNO_wspd95, paste0(data_generated_path, "nb_kud.", analyse, ".csv"), row.names = FALSE)
nb_kud.roosting_ECE_wNO_wspd95 <- read.csv(paste0(data_generated_path, paste0("nb_kud.", analyse, ".csv")), row.names = NULL)
# Générer les maps pour chaque zoom
maps_list.roosting_ZOOM_ECE_wNO_wspd95 <- Map(create_map_param, zoom_level, analyse, param, couleur)
beep(3)

## # # # # # --- 
#### *alimentation  ---------------------------------------------------------------
## # # # # # ---

zoom_level <- c("A","B","C","D","E")
analyse = "foraging_ECE_wNO_wspd95"
results_kud = NULL
nb_kud = NULL
comportement = "foraging"
param <- "ECE_wNO_wspd95"
couleur = nom_pal_foraging
map_kud.foraging_ECE_wNO_wspd95 <- Map(estimate_kud_param, zoom_level, comportement, param)
results_kud.foraging_ECE_wNO_wspd95 <- do.call(rbind, map_kud.foraging_ECE_wNO_wspd95)
st_write(results_kud.foraging_ECE_wNO_wspd95, paste0(data_generated_path, "results_kud.", analyse, ".gpkg"), append = FALSE)
results_kud.foraging <- st_read(file.path(data_generated_path, paste0("results_kud.", analyse,".gpkg")))
# compter les nb ind par zoom
nb_kud_map.foraging_ECE_wNO_wspd95 <- Map(count_nb_kud_param, zoom_level, comportement, param)
nb_kud.foraging_ECE_wNO_wspd95 <- do.call(rbind, nb_kud_map.foraging_ECE_wNO_wspd95)
write.csv(nb_kud.foraging_ECE_wNO_wspd95, paste0(data_generated_path, "nb_kud.", analyse, ".csv"), row.names = FALSE)
nb_kud.foraging_ECE_wNO_wspd95 <- read.csv(paste0(data_generated_path, paste0("nb_kud.", analyse, ".csv")), row.names = NULL)
# Générer les maps pour chaque zoom
maps_list.foraging_ZOOM_ECE_wNO_wspd95 <- Map(create_map_param, zoom_level, analyse, param, couleur)
beep(3)
