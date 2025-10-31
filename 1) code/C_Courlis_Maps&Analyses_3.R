# _____________________________________________________________________________________________________________________________________
# _____________________________________________________________________________________________________________________________________
# 1. Starting block ----
# _____________________________________________________________________________________________________________________________________
# _____________________________________________________________________________________________________________________________________

# --- objectif ---
# a lancer au début, contient les library, fonction, données et autres paramètres à définir

# beep lorsqu'il y a une erreur
library(beepr)
options(error = function() {
  beep(7)
}) # options(error = NULL), pour enlver le beep

# Nettoyage de l'environnement
rm(list = ls())

# time zone
library(lubridate)
with_tz(Sys.time(), "Europe/Paris")

options(scipen = 999)

# packages______________________________________________________________________

local_lib <- "C:/Users/Suzanne.Bonamour/Rlibs" # Définir le chemin local où vous avez les droits
dir.create(local_lib, showWarnings = FALSE, recursive = TRUE) # Créer le dossier s'il n'existe pas
.libPaths(local_lib) # Ajouter ce chemin à la liste des bibliothèques

# Liste des packages à installer
packages <- c(
  "tidyverse", "terra", "sf", "adehabitatLT", "raster", "tmap",
  "adehabitatHR", "viridis", "beepr", "readxl", "marmap", "pals",
  "stars", "ggcorrplot", "tibble", "paletteer", "ggeffects",
  "lmerTest", "ggthemes", "broom.mixed", "performance", "ggpubr",
  "maptiles", "ggnewscale", "tinter", "furrr", "purrr", "future.apply",
  "DHARMa", "effects", "glmmTMB", "scales", "ggspatial", "prettymapr",
  "rosm", "gridExtra", "lme4", "betareg"
)

not_installed <- packages[!packages %in% installed.packages(lib.loc = local_lib)[, "Package"]] # Identifier ceux qui ne sont pas encore installés dans local_lib

if (length(not_installed) > 0) { # Installer les packages manquants en version binaire
  install.packages(not_installed, lib = local_lib, type = "binary")
} else {
  message("✅ Tous les packages sont déjà installés dans ", local_lib)
}

invisible(lapply(packages, function(pkg) { # 7. Charger les packages
  library(pkg, character.only = TRUE, lib.loc = local_lib)
}))

# a définir_____________________________________________________________________

zoom_level <- c("A", "B", "C") # Liste des niveaux de zoom
resolution_ZOOM <- 10 # résolution des grid pour analyses (10 m)

# couleurs
couleur_roosting <- "#FF00E6"
couleur_foraging <- "#49B6FF"
couleur_roosting_variable_2 <- c(lighten("#FF00E6", 0.1), darken("#FF00E6", 0.25))
couleur_foraging_variable_2 <- c(lighten("#49B6FF", 0.2), darken("#49B6FF", 0.25))

# chemins_______________________________________________________________________

data_path <- "D:/Projets_Suzanne/Courlis/3) Data/1) data/"
data_generated_path <- "D:/Projets_Suzanne/Courlis/3) Data/2) data_generated/"
atlas_path <- "D:/Projets_Suzanne/Courlis/Atlas_Courlis/"

# Réserve naturelle Moeze Oléron________________________________________________

reserve <- st_read(paste0(data_path, "Réserve_naturelle/rnn/rnn/N_ENP_RNN_S_000.shp"))
RMO <- reserve[reserve$NOM_SITE == "Moëze-Oléron", ]
rm(reserve)

# Zone d'étude globale__________________________________________________________

# BOX <- st_as_sf(st_as_sfc(st_bbox(c(xmin = -1.26, xmax = -0.945, ymax = 46.01, ymin = 45.78), crs = st_crs(4326))))
# st_write(BOX, paste0(data_generated_path, "BOX.gpkg"), append = FALSE)
BOX <- st_read(paste0(data_generated_path, "BOX.gpkg"))
BOX_4326 <- st_transform(BOX, crs = 4326) # Transformation de la boîte au CRS 4326 (coordonnées géographiques)
# st_write(BOX_4326, paste0(data_generated_path, "BOX_4326.gpkg"), append = FALSE)
BOX_2154 <- st_transform(BOX, crs = 2154) # Transformation de la boîte au CRS 2154 (coordonnées géographiques)

# Aire de la zone d'étude
area_box <- st_area(BOX)
area_box_km <- area_box / 1000000

# zoom__________________________________________________________________________

ZOOM_B <- st_transform(st_as_sf(st_as_sfc(st_bbox(c(xmin = -1.26, xmax = -1.18, ymax = 46.01, ymin = 45.78), crs = st_crs(4326)))))
ZOOM_A <- st_transform(st_as_sf(st_as_sfc(st_bbox(c(xmin = -1.18, xmax = -1.045, ymax = 46.01, ymin = 45.865), crs = st_crs(4326)))))
ZOOM_C <- st_transform(st_as_sf(st_as_sfc(st_bbox(c(xmin = -1.18, xmax = -0.945, ymax = 45.865, ymin = 45.78), crs = st_crs(4326)))))
st_write(ZOOM_B, paste0(data_generated_path, "ZOOM_B.gpkg"), append = FALSE)
st_write(ZOOM_A, paste0(data_generated_path, "ZOOM_A.gpkg"), append = FALSE)
st_write(ZOOM_C, paste0(data_generated_path, "ZOOM_C.gpkg"), append = FALSE)

ZOOM <- rbind(ZOOM_A, ZOOM_B, ZOOM_C)
ZOOM$name <- c("A", "B", "C")
ZOOM <- ZOOM %>%
  rename(geometry = x)

# Site de baguage_______________________________________________________________

site_baguage <- data.frame(
  name = c("Site de baguage"),
  lon = -1.082390, lat = 45.895373
)
site_baguage <- st_as_sf(site_baguage, coords = c("lon", "lat"), crs = 4326)
site_baguage <- st_transform(site_baguage, 2154)
site_baguage$icone <- "📍"

# grilles_______________________________________________________________________

# INPN grille
# grid <- st_read(paste0(data_path, "INPN_grid/METROP_L932X2.shp"))
# grid_crop <- st_crop(grid, BOX_2154)
# st_write(grid_crop, paste0(data_generated_path, "grid_crop.gpkg"), append = FALSE)
grid_crop <- st_read(paste0(data_generated_path, "grid_crop.gpkg"))

# 100x100 m
# offset_point <- st_bbox(grid[grid$CD_SIG=="2kmL93E370N6528",])[c("xmin", "ymin")] ; offset_point
# grid_100x100 <- st_make_grid(BOX_2154, cellsize = 100, offset = offset_point)
# st_write(grid_100x100, paste0(data_generated_path, "grid_100x100.gpkg"), append = FALSE)
grid_100x100 <- st_read(paste0(data_generated_path, "grid_100x100.gpkg"))
raster_100x100 <- rast(grid_100x100, resolution = 100, crs = "EPSG:2154")

# 10x10 m
# offset_point <- st_bbox(grid[grid$CD_SIG=="2kmL93E370N6528",])[c("xmin", "ymin")] ; offset_point
# grid_10x10 <- st_make_grid(BOX_2154, cellsize = 10, offset = offset_point)
# st_write(grid_10x10, paste0(data_generated_path, "grid_10x10.gpkg"), append = FALSE)
# grid_10x10 <- st_read(paste0(data_generated_path, "grid_10x10.gpkg"))
# raster_10x10 <- rast(grid_10x10, resolution = 10, crs="EPSG:2154")
# saveRDS(raster_10x10, file = paste0(data_generated_path, "raster_10x10.rds"))
raster_10x10 <- readRDS(paste0(data_generated_path, "raster_10x10.rds"))

# zoom A
# offset_point_ZOOM_A <- st_bbox(grid[grid$CD_SIG=="2kmL93E380N6538", ])[c("xmin", "ymin")] - c(2000 * 2, 0) # Point d’origine de la grille
# ZOOM_A_l93 <- st_transform(ZOOM_A, st_crs(grid)) # ZOOM_A doit être dans le même CRS que 'grid'
# grid_ZOOM_A <- st_make_grid(ZOOM_A_l93, cellsize = resolution_ZOOM, offset = offset_point_ZOOM_A) # Création de la grille
# st_write(st_sf(geometry = grid_ZOOM_A), paste0(data_generated_path, "grid_ZOOM_A.gpkg"), append = FALSE) # Sauvegarde en GeoPackage (ok)
# grid_ZOOM_A <- st_read(paste0(data_generated_path, "grid_ZOOM_A.gpkg"))
# # Étape 1 : créer un raster vide à partir de l’emprise de la grille
# raster_ZOOM_A <- rast(ext = ext(st_bbox(grid_ZOOM_A)), resolution = resolution_ZOOM, crs = "EPSG:2154") # Conversion en raster
# grid_ZOOM_A$cell_id <- 1:nrow(grid_ZOOM_A) # un identifiant par cellule
# raster_ZOOM_A <- rasterize(vect(grid_ZOOM_A), raster_ZOOM_A, field = "cell_id")
# saveRDS(raster_ZOOM_A, file = paste0(data_generated_path, "raster_ZOOM_A.rds"))
raster_ZOOM_A <- readRDS(paste0(data_generated_path, "raster_ZOOM_A.rds"))

# zoom B
# offset_point_ZOOM_B <- st_bbox(grid[grid$CD_SIG=="2kmL93E370N6528", ])[c("xmin", "ymin")] - c(2000 * 0.3, 0) # Point d’origine de la grille
# ZOOM_B_l93 <- st_transform(ZOOM_B, st_crs(grid)) # ZOOM_B doit être dans le même CRS que 'grid' 
# grid_ZOOM_B <- st_make_grid(ZOOM_B_l93, cellsize = resolution_ZOOM, offset = offset_point_ZOOM_B) # Création de la grille
# st_write(st_sf(geometry = grid_ZOOM_B), paste0(data_generated_path, "grid_ZOOM_B.gpkg"), append = FALSE) # Sauvegarde en GeoPackage
# grid_ZOOM_B <- st_read(paste0(data_generated_path, "grid_ZOOM_B.gpkg"))
# raster_ZOOM_B <- rast(ext = ext(st_bbox(grid_ZOOM_B)), resolution = resolution_ZOOM, crs = "EPSG:2154") # créer un raster vide à partir de l’emprise de la grille
# grid_ZOOM_B$cell_id <- 1:nrow(grid_ZOOM_B) # un identifiant par cellule
# raster_ZOOM_B <- rasterize(vect(grid_ZOOM_B), raster_ZOOM_B, field = "cell_id") # Conversion en raster
# saveRDS(raster_ZOOM_B, file = paste0(data_generated_path, "raster_ZOOM_B.rds"))
raster_ZOOM_B <- readRDS(paste0(data_generated_path, "raster_ZOOM_B.rds"))

# zoom C
# offset_point_ZOOM_C <- st_bbox(grid[grid$CD_SIG=="2kmL93E376N6528",])[c("xmin", "ymin")] - c(2000 * 0.2, 0) # Point d’origine de la grille
# ZOOM_C_l93 <- st_transform(ZOOM_C, st_crs(grid)) # ZOOM_C doit être dans le même CRS que 'grid' 
# grid_ZOOM_C <- st_make_grid(ZOOM_C_l93, cellsize = resolution_ZOOM, offset = offset_point_ZOOM_C) # Création de la grille
# st_write(st_sf(geometry = grid_ZOOM_C), paste0(data_generated_path, "grid_ZOOM_C.gpkg"), append = FALSE) # Sauvegarde en GeoPackage
# grid_ZOOM_C <- st_read(paste0(data_generated_path, "grid_ZOOM_C.gpkg"))
# raster_ZOOM_C <- rast(ext = ext(st_bbox(grid_ZOOM_C)), resolution = resolution_ZOOM, crs = "EPSG:2154") # créer un raster vide à partir de l’emprise de la grille
# grid_ZOOM_C$cell_id <- 1:nrow(grid_ZOOM_C) # un identifiant par cellule
# raster_ZOOM_C <- rasterize(vect(grid_ZOOM_C), raster_ZOOM_C, field = "cell_id") # Conversion en raster
# saveRDS(raster_ZOOM_C, file = paste0(data_generated_path, "raster_ZOOM_C.rds"))
raster_ZOOM_C <- readRDS(paste0(data_generated_path, "raster_ZOOM_C.rds"))

# tmap_mode("view")
  # zone_map <- tm_scalebar() +
  # tm_basemap(c("Esri.WorldImagery", "OpenStreetMap", "CartoDB.Positron")) +
  # tm_shape(BOX_2154) +
  # tm_borders(col = "#575757") +
  # tm_shape(raster_ZOOM_A) +
  # tm_raster(col ="red") +
  # tm_shape(ZOOM) +
  # tm_polygons(fill = "#575757", alpha = 0.1, col = "#575757", lty = "dotted", size = 3) +
  # tm_labels("name", size = 1, col = "#575757", just = "center")
  # tm_shape(raster_ZOOM_B) +
  # tm_raster(col ="pink") +
  # tm_shape(raster_ZOOM_C) +
  # tm_raster(col ="red") +
  # tm_shape(grid_crop) +
  # tm_polygons(col ="green", alpha = 0.5)
  # zone_map

# données GPS___________________________________________________________________

GPS <- st_read(file.path(data_generated_path, "GPS_clean_afterGwen.gpkg"))

# variables temporelles additionnelles
GPS$y_m_d <- ymd(as.Date(GPS$datetime))
GPS$month_numeric <- month(as.Date(GPS$datetime))
GPS$month_label <- as.character(lubridate::month(as.Date(GPS$datetime), label = TRUE, abbr = TRUE))
GPS$week <- week(as.Date(GPS$datetime))
GPS$year <- year(as.Date(GPS$datetime))

# Taille du jeu de données
length(unique(GPS$ID)) # 80 ind
length(unique(GPS$ID[GPS$sex == "F"])) # 36 female
length(unique(GPS$ID[GPS$sex == "M"])) # 39 male
length(unique(GPS$ID[is.na(GPS$sex)])) # 7 unknown
length(unique(GPS$ID[GPS$age == "juvénile"])) # 20 juv
length(unique(GPS$ID[GPS$age == "adulte"])) # 60 ad
length(unique(GPS$ID[is.na(GPS$age)])) # 0 unknown
dim(GPS)

# Formatage variables 
GPS$sub <- GPS$tide_strength
GPS$sub[GPS$sub == "spring_tide"] <- "no submersion"
GPS$sub[GPS$sub == "neap_tide"] <- "no submersion"

GPS$behavior[GPS$behavior == "Roosting"] <- "roosting"
GPS$behavior[GPS$behavior == "Foraging"] <- "foraging"

GPS$timeofday[GPS$timeofday == "day"] <- "jour"
GPS$timeofday[GPS$timeofday == "night"] <- "nuit"

GPS$sex[GPS$sex == "F"] <- "femelle"
GPS$sex[GPS$sex == "M"] <- "mâle"

GPS$tide_strength[GPS$tide_strength == "spring_tide"] <- "marée de vives eaux"
GPS$tide_strength[GPS$tide_strength == "neap_tide"] <- "marée de mortes eaux"

# nom des Ind___________________________________________________________________

ids_uniques <- unique(GPS$ID) # Extraire les ID uniques existants
length(unique(GPS$ID))

lettres <- LETTERS[1:10] # Créer 80 nouveaux ID sous la forme lettre + chiffre (A1 à H10)
chiffres <- 1:9
nouveaux_ids <- paste0(
  rep(lettres, each = 9),
  chiffres
)

length(nouveaux_ids)

table_correspondance <- data.frame( # Créer la table de correspondance
  bague = ids_uniques,
  ID = nouveaux_ids[seq_along(ids_uniques)]
)

table_correspondance <- table_correspondance %>% 
  distinct()

# save ---
write.csv(table_correspondance, paste0(atlas_path, "table_correspondance", ".csv"), row.names = FALSE)
table_correspondance <- read.csv(paste0(atlas_path, paste0("table_correspondance", ".csv")), row.names = NULL)

GPS$ID <- table_correspondance$ID[ # Appliquer la correspondance au tableau GPS
  match(GPS$ID, table_correspondance$bague)
]

head(GPS$ID) # Vérifier le résultat
length(unique(GPS$ID))
table(GPS$ID, useNA = "always")

# zone__________________________________________________________________________

ZOOM_A$zone <- "A"
ZOOM_B$zone <- "B"
ZOOM_C$zone <- "C"
zones_sf <- rbind(ZOOM_B, ZOOM_A, ZOOM_C)

GPS_sf <- st_as_sf(GPS, coords = c("lon", "lat"), crs = 4326) %>%
  mutate(lon = st_coordinates(.)[, 1], lat = st_coordinates(.)[, 2])
GPS_sf <- st_transform(GPS_sf, crs = st_crs(zones_sf)) # transformer pour matcher les zones (EPSG:2154)
GPS_sf <- st_join(GPS_sf, zones_sf[, "zone"], left = TRUE)

table(GPS_sf$zone)

GPS <- GPS_sf

# roosting & foraging datasets__________________________________________________

GPS_roosting <- GPS %>%
  filter(behavior == "roosting")

GPS_foraging <- GPS %>%
  filter(behavior == "foraging")

# functions_____________________________________________________________________

`%ni%` <- Negate(`%in%`) # reverse of %in%

verif_crs <- function(objet_sf) { # vérification du CRS des object spatiaux
  if (st_crs(objet_sf)$epsg != 4326) {
    beepr::beep(2) 
    stop("Le CRS n'est pas 4326 !")
  }
}

verif_tz_sys <- function() { # vérification de la time zone pour les dates et heures de l'ordinateur, la session r 
  if (Sys.timezone() != "UTC") {
    beepr::beep(2)
    stop("La timezone du système n'est pas UTC !")
  }
}

verif_tz <- function(objet, colonne) { # vérification pour le jeu de données
  if (!colonne %in% names(objet)) {
    stop(paste("La colonne", colonne, "n'existe pas dans l'objet !"))
  }

  tz <- attr(objet[[colonne]], "tzone") 

  if (is.null(tz) || tz != "UTC") {
    beepr::beep(2) 
    stop(paste("La colonne", colonne, "n'est pas en UTC !"))
  }
}

telecharger_donnees <- function(chemin) { # pour charger et fusionner les fichiers CSV d'un dossier
  fichiers <- list.files(path = chemin, pattern = "*.csv", full.names = TRUE)
  donnees <- lapply(fichiers, fread, sep = ",")
  return(rbindlist(donnees))
}

make_kud <- function(analyse, zoom_levels, comportement, GPS_sampled, data_generated_path, resolution_ZOOM, couleur) { # analyses spatiales et cartes
  
  # message 
  message("Analyse : ", analyse, " | Zoom : ", zoom_levels)
  
  # packages 
  library(sf)
  library(dplyr)
  library(adehabitatHR)
  library(terra)
  library(tmap)
  library(raster)

  # crs
  crs_utm <- "EPSG:32630"

  # nom de site
  labels_zoom <- data.frame(
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
      6541000, 6537500, 6535480,
      6532500
    )
  )
  
  labels_zoom <- st_as_sf(labels_zoom, coords = c("x", "y"), crs = 2154)
  
  # zoom
  ZOOM_shape <- st_read(paste0(data_generated_path, "ZOOM_", zoom_levels, ".gpkg"), quiet = TRUE) %>%
    st_transform(crs = 4326)

  # point GPS dans le zone zoom
  GPS_sampled.ZOOM <- st_intersection(GPS_sampled, ZOOM_shape)

  # point GPS pour le comportement 
  GPS_sampled.behavior <- GPS_sampled.ZOOM %>%
    filter(behavior == comportement) %>%
    st_drop_geometry() %>%
    dplyr::select(lon, lat, ID, datetime) %>%
    na.omit()

  GPS_sampled_spa.behavior <- GPS_sampled.behavior %>%
    st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
    st_transform(crs = crs_utm)

  GPS_sampled_coords.behavior <- st_coordinates(GPS_sampled_spa.behavior)
  
  # vérif assez de points GPS
  if (nrow(GPS_sampled.behavior) < 1) {
    warning("Pas assez de points pour ", zoom_levels)
    return(NULL)
  }
  
  # raster pour la zone zoom 
  grid <- st_read(paste0(data_generated_path, "grid_ZOOM_", zoom_levels, ".gpkg"), quiet = TRUE)
  raster_terra <- rast(grid, resolution = resolution_ZOOM, crs = "EPSG:2154")
  spatRaster <- project(raster_terra, crs_utm)
  spatialPixels <- as(raster(spatRaster), "SpatialPixels")

  # Calcul de h
  nb <- nrow(GPS_sampled_coords.behavior)
  h <- mean(c(sd(GPS_sampled_coords.behavior[, 1]), sd(GPS_sampled_coords.behavior[, 2]))) * 1.06 * nb^(-1 / 5) / 2

  # analyse KernelUD
  kud <- kernelUD(as_Spatial(GPS_sampled_spa.behavior), grid = 1000, h = h)

  iso_list <- lapply(c(95, 50), function(p) {
    st_as_sf(getverticeshr(kud, percent = p)) %>%
      mutate(level = p)
  })

  results_kud <- do.call(rbind, iso_list) %>%
    mutate(ZOOM = zoom_levels, h = h)

  # taille jeu de données
  nb_ind_point_dt <- GPS_sampled.behavior %>%
    group_by(ID) %>%
    summarise(n = n(), .groups = "drop") %>%
    mutate(zoom = zoom_levels)

  # creation carte
  zoom_obj <- st_read(paste0(data_generated_path, "ZOOM_", zoom_levels, ".gpkg"), quiet = TRUE)
  
  if (is.null(zoom_obj) || nrow(zoom_obj) == 0) stop("zoom_obj vide")

  Box <- st_bbox(zoom_obj)

  point_top_left <- st_sfc(st_point(c(Box["xmin"] + 1000, Box["ymax"] - 500)), crs = st_crs(zoom_obj))
  label_point <- st_sf(label = zoom_levels, geometry = point_top_left)

  nb_ind <- nrow(nb_ind_point_dt)
  nb_point <- sum(nb_ind_point_dt$n)
  info_text <- paste0(nb_point, " points / ", nb_ind, " individus / ", "h = ", round(h))

  point_text_info <- st_sfc(st_point(c(Box["xmin"] + 1000, Box["ymax"] - 1000)), crs = st_crs(zoom_obj))
  info_label_point <- st_sf(label = info_text, geometry = point_text_info)

  labels_zoom <- get("labels_zoom")

  data_95 <- results_kud %>% filter(level == 95)
  data_50 <- results_kud %>% filter(level == 50)

  map <- tm_scalebar() +
    tm_basemap(c("Esri.WorldImagery", "OpenStreetMap", "CartoDB.Positron")) +
    tm_shape(RMO) + tm_polygons(col = "darkgreen", fill_alpha = 0, col_Blpha = 1, lwd = 2) +
    tm_shape(zoom_obj) + tm_borders(col = "lightgrey", lty = "dotted", size = 3) +
    tm_shape(data_95) + tm_polygons(border.col = NULL, col = couleur, alpha = 0.5, legend.alpha = 1) +
    tm_shape(data_50) + tm_polygons(border.col = "white", col = couleur, alpha = 0.9, legend.alpha = 1) +
    tm_shape(label_point) + tm_text("label", col = "lightgrey", size = 3, just = c("left", "top")) +
    tm_shape(labels_zoom) + tm_text("name", size = 1, col = "lightgrey", fontface = "bold", just = "left") +
    tm_shape(site_baguage) + tm_text("icone", size = 1.5) +
    tm_credits(info_text,
      position = c("left", "bottom"), size = 1,
      col = "black", bg.color = "white", bg.alpha = 0.7, fontface = "bold"
    )

  # sauvegarde resultats analyses 
  st_write(data_95, paste0(data_generated_path, "UDMap_data_95_", analyse, "_", comportement, "_", zoom_levels, ".gpkg"), append = FALSE)
  st_write(data_50, paste0(data_generated_path, "UDMap_data_50_", analyse, "_", comportement, "_", zoom_levels, ".gpkg"), append = FALSE)
  
  # sauvegarde carte 
  tmap_save(map, paste0(atlas_path, "UDMap_", analyse, "_", comportement, "_", zoom_levels, ".html"))

  return(list(
    kud_sf = results_kud,
    stats = nb_ind_point_dt,
    map = map
  ))

  on.exit(
    {
      while (!is.null(dev.list())) dev.off()
    },
    add = TRUE
  )
}

make_kud_variable <- function(analyse, zoom_levels, comportement, GPS, data_generated_path, resolution_ZOOM, couleurs, variable) { # analyses spatiales et cartes avec une co-variable
  message("Analyse : ", analyse, " | Zoom : ", zoom_levels)

  # package 
  library(sf)
  library(dplyr)
  library(adehabitatHR)
  library(terra)
  library(tmap)
  library(raster)
  library(future)
  
  # crs
  crs_utm <- "EPSG:32630"

  # nom de site
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

  labels_zoom <- st_as_sf(labels_zoom, coords = c("x", "y"), crs = 2154)
  
  # zoom
  ZOOM_shape <- st_read(paste0(data_generated_path, "ZOOM_", zoom_levels, ".gpkg"), quiet = TRUE) %>%
    st_transform(crs = 4326)

  # point GPS dans la zone zoom
  GPS.ZOOM <- st_intersection(GPS, ZOOM_shape)

  # point GPS avec le bon comportement 
  GPS.behavior <- GPS.ZOOM %>%
    filter(behavior == comportement) %>%
    st_drop_geometry() %>%
    dplyr::select(lon, lat, ID, datetime, variable) %>%
    na.omit()

  if (nrow(GPS.behavior) < 1) {
    warning("Pas assez de points pour ", zoom_levels)
    return(NULL)
  }

  # au moins 5 point par group
  n_per <- GPS.behavior %>%
    group_by(!!sym(variable)) %>%
    summarize(n = n()) %>%
    filter(n <= 5)

  GPS.behavior <- GPS.behavior %>%
    filter(!(!!sym(variable) %in% pull(n_per, !!sym(variable))))

  if (nrow(GPS.behavior) == 0) {
    return(NULL)
  }

  GPS_spa <- st_as_sf(GPS.behavior, coords = c("lon", "lat"), crs = 4326)
  GPS_spa <- st_transform(GPS_spa, crs = 32630)
  GPS_coords.behavior <- st_coordinates(GPS_spa)

  # raster
  grid <- st_read(paste0(data_generated_path, "grid_ZOOM_", zoom_levels, ".gpkg"), quiet = TRUE)
  raster_terra <- rast(grid, resolution = resolution_ZOOM, crs = "EPSG:2154")
  spatRaster <- project(raster_terra, crs_utm)
  spatialPixels <- as(raster(spatRaster), "SpatialPixels")

  # calcul h
  nb <- nrow(GPS_coords.behavior)
  h <- mean(c(sd(GPS_coords.behavior[, 1]), sd(GPS_coords.behavior[, 2]))) * 1.06 * nb^(-1 / 5) / 2

  # analyse KernelUD
  plan(sequential)
  kud <- kernelUD(as_Spatial(GPS_spa[variable]), grid = 1000, h = h, same4all = TRUE)

  iso_list <- lapply(names(kud), function(variable) {
    lapply(c(95, 50), function(p) {
      st_as_sf(getverticeshr(kud[[variable]], percent = p)) %>%
        mutate(
          level = p,
          variable = variable
        ) 
    }) %>% bind_rows()
  }) %>% bind_rows()

  results_kud <- iso_list %>%
    mutate(ZOOM = zoom_levels, h = h)

  results_kud$variable <- as.factor(results_kud$variable)

  # taille jeu de données
  nb_ind_point_dt <- GPS.behavior %>%
    dplyr::group_by(ID, .data[[variable]]) %>%
    dplyr::select(ID, variable = .data[[variable]], datetime) %>%
    st_drop_geometry() %>%
    na.omit() %>%
    summarise(n = n()) %>%
    mutate(zoom = zoom_levels)

  if (nrow(nb_ind_point_dt) == 0) {
    return(NULL)
  }

  nb_kud <- rbind(nb_kud, nb_ind_point_dt)
  nb_kud <- nb_ind_point_dt

  # creation carte
  zoom_obj <- st_read(paste0(data_generated_path, "ZOOM_", zoom_levels, ".gpkg"), quiet = TRUE)
  
  if (is.null(zoom_obj) || nrow(zoom_obj) == 0) stop("zoom_obj vide")

  Box <- st_bbox(zoom_obj)

  point_top_left <- st_sfc(st_point(c(Box["xmin"] + 1000, Box["ymax"] - 500)), crs = st_crs(zoom_obj))
  label_point <- st_sf(label = zoom_levels, geometry = point_top_left)

  nb_ind <- length(unique(GPS.behavior$ID))
  nb_point <- sum(nb_ind_point_dt$n)
  nb_point_min_per_ind <- min(nb_ind_point_dt$n)
  nb_point_max_per_ind <- max(nb_ind_point_dt$n)
  info_text <- paste0(nb_point, " points / ", nb_ind, " individus / ", "h = ", round(h))

  point_text_info <- st_sfc(st_point(c(Box["xmin"] + 1000, Box["ymax"] - 1000)), crs = st_crs(zoom_obj))
  info_label_point <- st_sf(label = info_text, geometry = point_text_info)

  labels_zoom <- get(paste0("labels_ZOOM_", zoom_levels))

  data_95 <- results_kud %>% filter(level == 95)
  data_50 <- results_kud %>% filter(level == 50)

  niveaux_variable <- levels(results_kud$variable)
  palette_dyn <- setNames(c(couleurs)[1:length(niveaux_variable)], niveaux_variable)

  tmap_mode("view") 
  map <- tm_scalebar() +
    tm_basemap(c("Esri.WorldImagery", "OpenStreetMap", "CartoDB.Positron")) +
    tm_shape(RMO) + tm_polygons(col = "darkgreen", fill_alpha = 0, col_Blpha = 1, lwd = 2) +
    tm_shape(zoom_obj) + tm_borders(col = "lightgrey", lty = "dotted", size = 3) +
    tm_shape(data_95) + tm_polygons(fill = "variable", palette = couleurs, fill_alpha = 0.5, border.col = NULL, legend.show = TRUE) +
    tm_shape(data_50) + tm_polygons(fill = "variable", palette = couleurs, fill_alpha = 0.9, border.col = "white", legend.show = FALSE) +
    tm_shape(label_point) + tm_text("label", col = "lightgrey", size = 3, just = c("left", "top")) +
    tm_shape(labels_zoom) + tm_text("name", size = 1, col = "lightgrey", fontface = "bold", just = "left") +
    tm_shape(site_baguage) + tm_text("icone", size = 1.5) +
    tm_credits(info_text,
      position = c("left", "bottom"), size = 1,
      col = "lightgrey", bg.color = "white", bg.alpha = 0.7, fontface = "bold"
    ) +
    tm_layout(legend.show = TRUE, legend.alpha = 1)

  tmap_save(map, paste0(atlas_path, "UDMap_", analyse, "_", comportement, "_", variable, "_", zoom_levels, ".html"))

  return(list(
    kud_sf = results_kud,
    stats = nb_ind_point_dt,
    map = map
  ))
}

sample_weighted_points <- function(data, n_sample = 1000, variable = NULL, zone = NULL, cap = Inf) {
 
   if (!all(c("ID", "datetime") %in% names(data))) { # Vérifier colonnes obligatoires
    stop("Les colonnes 'ID' et 'datetime' doivent exister dans les données.")
  }
 
   if (!is.null(variable)) { # Supprimer les NA dans la colonne spécifiée comme variable (si elle existe)
    if (!variable %in% names(data)) stop(paste("La colonne", variable, "n'existe pas dans les données."))
    data <- data[!is.na(data[[variable]]), ]
  }

  data <- data %>% # Calcul du dt par individu
    arrange(ID, datetime) %>%
    group_by(ID) %>%
    mutate(
      dt = as.numeric(difftime(datetime, lag(datetime), units = "secs")),
      dt = ifelse(is.na(dt), 0, dt)
    ) %>%
    ungroup() %>%
    mutate(
      dt_capped = pmin(dt, cap),
      dt_capped = ifelse(dt_capped == 0, 1, dt_capped)
    ) 

  sample_group <- function(df) { # Fonction d’échantillonnage pondéré
    if (nrow(df) == 0) { # éviter erreur si groupe vide
      return(df)
    } 
    size <- min(n_sample, nrow(df)) # si groupe trop petit, on prend tout
    df[sample(seq_len(nrow(df)), size = size, replace = FALSE, prob = df$dt_capped), ]
  }

  grouping_vars <- c("ID") # Définir les variables de regroupement
  if (!is.null(variable)) grouping_vars <- c(grouping_vars, variable)
  if (!is.null(zone)) grouping_vars <- c(grouping_vars, zone)

  sampled <- data %>%
    group_by(across(all_of(grouping_vars))) %>%
    group_modify(~ sample_group(.x)) %>%
    ungroup()

  return(sampled)
}

generate_color_gradient <- function(base_color = "#9A7AA0", n_total = 12, light_max = 1, dark_max = 1) {
  if (n_total < 3) stop("Le nombre total de couleurs doit être au moins 3.")
  n_light <- ceiling(n_total / 2)
  n_dark <- floor(n_total / 2)
  light_Bmounts <- seq(0.1, light_max, length.out = n_light)
  dark_Bmounts <- seq(0.1, dark_max, length.out = n_dark)
  light_colors <- sapply(light_Bmounts, function(x) lighten(base_color, x))
  dark_colors <- sapply(dark_Bmounts, function(x) darken(base_color, x))
  gradient <- c(light_colors, dark_colors)
  rgb_matrix <- col2rgb(gradient)
  luminance <- apply(rgb_matrix, 2, function(rgb) {
    0.299 * rgb[1] + 0.587 * rgb[2] + 0.114 * rgb[3]
  })
  gradient <- gradient[order(-luminance)]
  return(gradient)
}

generate_five_gradient <- function(color1 = "#9A7AA0", color2 = "#A9C5A0", color3 = "#7A9AA0", color4 = "black", color5 = "grey", n_total = 12) {
  if (n_total < 5) stop("Le nombre total de couleurs doit être au moins 3.")
  n_6 <- ceiling(n_total / 5) # Nombre de points intermédiaires entre chaque paire
  grad1 <- colorRampPalette(c(color1, color2))(n_total - n_6*3 + 1)
  grad2 <- colorRampPalette(c(color2, color3))(n_total - n_6*3)
  grad3 <- colorRampPalette(c(color3, color4))(n_total - n_6*3)
  grad4 <- colorRampPalette(c(color4, color5))(n_total - n_6*3 + 2)
  gradient <- unique(c(grad1, grad2, grad3, grad4)) # Fusionner les deux dégradés (en supprimant le doublon central color2)
  return(gradient)
}

richesse <- function(x) { # richesse brute
  length(unique(x))
}

shannon <- function(x) { # indice de Shannon
  p <- table(x) / length(x)
  -sum(p * log(p))
}

variation_taux <- function(x) { # taux de variation
  sum(x[-1] != x[-length(x)]) / (length(x) - 1)
}

equitabilite <- function(x) { # équitabilité 
  H <- shannon(x)
  R <- richesse(x)
  if (R > 1) H / log(R) else 0 # si 1 seul comportement → équitabilité = 0
}

## STOP ##
stop()
## STOP ##

# _____________________________________________________________________________________________________________________________________
# _____________________________________________________________________________________________________________________________________
# 2. Carte de la zone d'étude ----
# _____________________________________________________________________________________________________________________________________
# _____________________________________________________________________________________________________________________________________

# --- objectif ---
# visualisation de la zone d'étude et des 3 zones A, B, et C

labels_zoom <- data.frame(
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
    384000, 386000, 376000,
    384000
  ),
  y = c(
    6537900, 6539250, 6543200,
    6546600, 6549700, 6548800,
    6547350, 6545650, 6541650,
    6541000, 6537500, 6535480,
    6532500
  )
)

labels_zoom <- st_as_sf(labels_zoom, coords = c("x", "y"), crs = 2154)

tmap_mode("view")
zone_map <- tm_scalebar() +
  tm_basemap(c("Esri.WorldImagery", "OpenStreetMap", "CartoDB.Positron")) +
  tm_shape(RMO) + tm_polygons(col = "darkgreen", fill = "darkgreen", fill_alpha = 0.1, col_Blpha = 1, lwd = 2) +
  tm_shape(BOX_2154) +
  tm_borders(col = "lightgrey") +
  tm_shape(ZOOM) +
  tm_borders(col = "lightgrey", lty = "dotted", size = 3) +
  tm_labels("name", size = 1, col = "lightgrey", just = "center") +
  tm_shape(site_baguage) +
  tm_text("icone", size = 1.5, options = opt_tm_text(just = "center")) +
  tm_shape(labels_zoom) + tm_text("name", size = 1, col = "lightgrey", fontface = "bold", just = "left")
zone_map

tmap_save(zone_map, paste0(atlas_path, "zone_map_new.html"))

# _____________________________________________________________________________________________________________________________________
# _____________________________________________________________________________________________________________________________________
# 3. Période d'émission des balises ----
# _____________________________________________________________________________________________________________________________________
# _____________________________________________________________________________________________________________________________________

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

emission_dt_1$sex_age[emission_dt_1$sex_age == "femelle_adulte"] <- "femelle adulte"
emission_dt_1$sex_age[emission_dt_1$sex_age == "femelle_juvénile"] <- "femelle juvénile"
emission_dt_1$sex_age[emission_dt_1$sex_age == "mâle_adulte"] <- "mâle adulte"
emission_dt_1$sex_age[emission_dt_1$sex_age == "mâle_juvénile"] <- "mâle juvénile"

table(emission_dt_1$sex_age)

# couleur par catégorie
col_sex_age <- c(
  "femelle adulte" = "#FF00E6", "femelle juvénile" = "pink",
  "mâle adulte" = "#49B6FF", "mâle juvénile" = "lightblue",
  "inconnu" = "grey40"
)

emission_dt_1$sex_age <- factor(emission_dt_1$sex_age, levels = names(col_sex_age))

# plot
emission_plot <- ggplot(emission_dt_1, aes(
  x = ID, y = date,
  color = sex_age
)) +
  geom_point(size = 2, shape = 15) +
  scale_color_manual(values = col_sex_age) +
  theme_classic() +
  scale_y_date(date_breaks = "2 month", date_labels = "%b %Y") +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1),
    legend.position = c(.15, .82),
  ) +
  labs(
    title = "",
    x = "Individu",
    y = "Temps",
    color = "Sexe & Age"
  )
emission_plot

# save plot
ggsave(paste0(atlas_path, "/emission_plot.png"),
  plot = emission_plot, width = 10, height = 7, dpi = 300
)

# talk ---

# emission_dt_1_talk <- emission_dt_1 %>%
#   mutate(
#     sex_age_en = case_when(
#       sex_age == "femelle adulte" ~ "adult female",
#       sex_age == "inconnu" ~ "unknown",
#       sex_age == "mâle adulte" ~ "adult male",
#       sex_age == "femelle juvénile" ~ "juvenile female",
#       sex_age == "mâle juvénile" ~ "juvenile male"
#     )
#   )
#
# emission_dt_1_talk
#
# col_sex_age_en <- c(
#   "adult female" = "purple", "juvenile female" = "lightpink",
#   "adult male" = "darkgreen", "juvenile male" = "lightgreen",
#   "unknown" = "grey40"
# )
#
# emission_plot_talk <- ggplot(emission_dt_1_talk, aes(
#   x = date, y = ID,
#   color = sex_age_en
# )) +
#   geom_point(size = 2, shape = 15) +
#   scale_color_manual(values = col_sex_age_en) +
#   theme_classic() +
#   scale_x_date(
#     date_breaks = "10 month",
#     labels = label_date(format = "%b %Y", locale = "en")
#   )  + # theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
#   labs(
#     title = "",
#     x = "Recording periods",
#     y = "Individual",
#     color = "Sex & Age"
#   )
# emission_plot_talk
#
# # save plot
# ggsave(paste0(atlas_path, "/emission_plot_talk.png"),
#   plot = emission_plot_talk, width = 15, height = 9, dpi = 300
# )

# _____________________________________________________________________________________________________________________________________
# _____________________________________________________________________________________________________________________________________
# 4. Domaines vitaux ----
# _____________________________________________________________________________________________________________________________________
# _____________________________________________________________________________________________________________________________________

# --- objectif ---
# estimation et visualisation des domaines vitaux

# estimation____________________________________________________________________

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
labels_ZOOM_B <- labels_ZOOM[labels_ZOOM$ZOOM == "A", ]
labels_ZOOM_A <- labels_ZOOM[labels_ZOOM$ZOOM == "B", ]
labels_ZOOM_C <- labels_ZOOM[labels_ZOOM$ZOOM == "C", ]
labels_ZOOM_D <- labels_ZOOM[labels_ZOOM$ZOOM == "D", ]
labels_ZOOM_E <- labels_ZOOM[labels_ZOOM$ZOOM == "E", ]

labels_ZOOM_B <- labels_ZOOM[labels_ZOOM$ZOOM == "A", ]
labels_ZOOM_A <- labels_ZOOM[labels_ZOOM$ZOOM == "B", ]
labels_ZOOM_C <- labels_ZOOM[labels_ZOOM$ZOOM == "C" |
  labels_ZOOM$ZOOM == "D" |
  labels_ZOOM$ZOOM == "E", ]

### 1. Charger et filtrer les données GPS ###
ZOOM_shape <- st_read(paste0(data_generated_path, "grid_100x100", ".gpkg"), quiet = TRUE) %>%
  st_transform(crs = 4326)

# GPS.ZOOM <- st_intersection(GPS, ZOOM_shape)
GPS.ZOOM <- GPS

GPS.behavior <- GPS.ZOOM %>%
  st_drop_geometry() %>%
  dplyr::select(lon, lat, ID, datetime) %>%
  na.omit()

GPS_spa <- st_as_sf(GPS.behavior, coords = c("lon", "lat"), crs = 4326)
GPS_spa <- st_transform(GPS_spa, crs = 32630)
GPS_coords.behavior <- st_coordinates(GPS_spa)

### 2. Calculer le raster de base ###
grid <- st_read(paste0(data_generated_path, "grid_100x100", ".gpkg"), quiet = TRUE)
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
# kud <- kernelUD(as_Spatial(GPS_spa[variable]), grid = spatialPixels, h = h)
kud <- kernelUD(as_Spatial(GPS_spa["ID"]), grid = 1000, h = h, same4all = TRUE)

iso_list <- lapply(names(kud), function(ID) {
  lapply(c(95, 50), function(p) {
    st_as_sf(getverticeshr(kud[[ID]], percent = p)) %>%
      mutate(
        level = p,
        variable = ID
      )
  }) %>% bind_rows()
}) %>% bind_rows()

results_kud_HR <- iso_list # %>%

results_kud_HR$variable <- as.factor(results_kud_HR$variable)

# write & read
st_write(results_kud, paste0(data_generated_path, "results_kud_HR.gpkg"), append = FALSE)
results_kud_HR <- st_read(file.path(data_generated_path, "results_kud_HR.gpkg"))

results_kud_HR <- results_kud_HR %>% 
  rename(individu = param)

ID_list <- unique(results_kud_HR$individu)
ID_gp_1 <- ID_list[1:20]
ID_gp_2 <- ID_list[21:40]
ID_gp_3 <- ID_list[41:60]
ID_gp_4 <- ID_list[61:80]

data_95_gp_1 <- results_kud_HR %>%
  filter(
    level == 95,
    individu %in% ID_gp_1
  )
data_50_gp_1 <- results_kud_HR %>%
  filter(
    level == 50,
    individu %in% ID_gp_1
  )

data_95_gp_2 <- results_kud_HR %>%
  filter(
    level == 95,
    individu %in% ID_gp_2
  )
data_50_gp_2 <- results_kud_HR %>%
  filter(
    level == 50,
    individu %in% ID_gp_2
  )

data_95_gp_3 <- results_kud_HR %>%
  filter(
    level == 95,
    individu %in% ID_gp_3
  )
data_50_gp_3 <- results_kud_HR %>%
  filter(
    level == 50,
    individu %in% ID_gp_3
  )

data_95_gp_4 <- results_kud_HR %>%
  filter(
    level == 95,
    individu %in% ID_gp_4
  )
data_50_gp_4 <- results_kud_HR %>%
  filter(
    level == 50,
    individu %in% ID_gp_4
  )

couleurs <- "black"
info_text <- ""

couleurs_Aase <- c(
  "#FF00E6", # roosting
  "#49B6FF",
  "lightgrey", # foraging
  "yellow",
  "red"
)

# Création d'une fonction de palette
palette_gradient <- colorRampPalette(couleurs_Aase)

# Exemple : générer 100 couleurs dans le gradient
couleurs_gradient <- palette_gradient(20)
scales::show_col(couleurs_gradient)

tmap_mode("view")
map_gp_1 <- tm_scalebar() +
  tm_basemap(c("Esri.WorldImagery", "OpenStreetMap", "CartoDB.Positron")) +
  tm_shape(RMO) + tm_polygons(col = "darkgreen", fill_alpha = 0, col_Blpha = 1, lwd = 2) +
  tm_shape(data_95_gp_1) + tm_polygons(fill = "individu", palette = couleurs_gradient, fill_alpha = 0.5, border.col = NULL, legend.show = TRUE) +
  tm_shape(data_50_gp_1) + tm_polygons(fill = "individu", palette = couleurs_gradient, fill_alpha = 0.9, border.col = "white", legend.show = FALSE) +
  tm_shape(site_baguage) + tm_text("icone", size = 1.5) +
  tm_credits(info_text,
    position = c("left", "bottom"), size = 1,
    col = "lightgrey", bg.color = "white", bg.alpha = 0.7, fontface = "bold"
  ) +
  tm_layout(legend.show = TRUE, legend.alpha = 1)
map_gp_1

tmap_mode("view")
map_gp_2 <- tm_scalebar() +
  tm_basemap(c("Esri.WorldImagery", "OpenStreetMap", "CartoDB.Positron")) +
  tm_shape(RMO) + tm_polygons(col = "darkgreen", fill_alpha = 0, col_Blpha = 1, lwd = 2) +
  tm_shape(data_95_gp_2) + tm_polygons(fill = "individu", palette = couleurs_gradient, fill_alpha = 0.5, border.col = NULL, legend.show = TRUE) +
  tm_shape(data_50_gp_2) + tm_polygons(fill = "individu", palette = couleurs_gradient, fill_alpha = 0.9, border.col = "white", legend.show = FALSE) +
  tm_shape(site_baguage) + tm_text("icone", size = 1.5) +
  tm_credits(info_text,
    position = c("left", "bottom"), size = 1,
    col = "lightgrey", bg.color = "white", bg.alpha = 0.7, fontface = "bold"
  ) +
  tm_layout(legend.show = TRUE, legend.alpha = 1)
map_gp_2

tmap_mode("view")
map_gp_3 <- tm_scalebar() +
  tm_basemap(c("Esri.WorldImagery", "OpenStreetMap", "CartoDB.Positron")) +
  tm_shape(RMO) + tm_polygons(col = "darkgreen", fill_alpha = 0, col_Blpha = 1, lwd = 2) +
  tm_shape(data_95_gp_3) + tm_polygons(fill = "individu", palette = couleurs_gradient, fill_alpha = 0.5, border.col = NULL, legend.show = TRUE) +
  tm_shape(data_50_gp_3) + tm_polygons(fill = "individu", palette = couleurs_gradient, fill_alpha = 0.9, border.col = "white", legend.show = FALSE) +
  tm_shape(site_baguage) + tm_text("icone", size = 1.5) +
  tm_credits(info_text,
    position = c("left", "bottom"), size = 1,
    col = "lightgrey", bg.color = "white", bg.alpha = 0.7, fontface = "bold"
  ) +
  tm_layout(legend.show = TRUE, legend.alpha = 1)
map_gp_3

tmap_mode("view")
map_gp_4 <- tm_scalebar() +
  tm_basemap(c("Esri.WorldImagery", "OpenStreetMap", "CartoDB.Positron")) +
  tm_shape(RMO) + tm_polygons(col = "darkgreen", fill_alpha = 0, col_Blpha = 1, lwd = 2) +
  tm_shape(data_95_gp_4) + tm_polygons(fill = "individu", palette = couleurs_gradient, fill_alpha = 0.5, border.col = NULL, legend.show = TRUE) +
  tm_shape(data_50_gp_4) + tm_polygons(fill = "individu", palette = couleurs_gradient, fill_alpha = 0.9, border.col = "white", legend.show = FALSE) +
  tm_shape(site_baguage) + tm_text("icone", size = 1.5) +
  tm_credits(info_text,
    position = c("left", "bottom"), size = 1,
    col = "lightgrey", bg.color = "white", bg.alpha = 0.7, fontface = "bold"
  ) +
  tm_layout(legend.show = TRUE, legend.alpha = 1)
map_gp_4

tmap_save(map_gp_1, paste0(atlas_path, "UDMap_HR_gp_1", ".html"))
tmap_save(map_gp_2, paste0(atlas_path, "UDMap_HR_gp_2", ".html"))
tmap_save(map_gp_3, paste0(atlas_path, "UDMap_HR_gp_3", ".html"))
tmap_save(map_gp_4, paste0(atlas_path, "UDMap_HR_gp_4", ".html"))

# surface moyenne_______________________________________________________________

# --- objectif ---
# estimation de la surface moyenne du domaine vital à 95 % et 50 % de chaque individu

age_sex_dt <- GPS %>%
  st_drop_geometry() %>%
  dplyr::select(ID, age, sex) %>%
  distinct()

to_keep <- as.data.frame(table(age_sex_dt$ID))
to_keep <- to_keep %>%
  filter(Freq == 1)

age_sex_dt_2 <- age_sex_dt %>%
  filter(ID %in% to_keep$Var1)

results_kud_HR <- st_read(file.path(data_generated_path, "results_kud_HR.gpkg"))

# Renomme la colonne 'area' en 'area_95' et supprime la géométrie
results_kud_HR_dt <- results_kud_HR %>%
  rename(ID = param) %>% # Renomme la colonne area
  st_drop_geometry() # Supprime les géométries (on ne garde que les données attributaires)

# save & read
write.table(results_kud_HR_dt,
  file = paste0(data_generated_path, "results_kud_HR_dt.csv"),
  sep = ",", col.names = NA, qmethod = "double"
)

results_kud_HR_dt <- read.csv(paste0(data_generated_path, "results_kud_HR_dt.csv"), row.names = NULL)

results_kud_HR_dt$level <- as.character(results_kud_HR_dt$level)
results_kud_HR_dt$level[results_kud_HR_dt$level == "50"] <- "vertice 50%"
results_kud_HR_dt$level[results_kud_HR_dt$level == "95"] <- "vertice 95%"

results_kud_HR_dt <- results_kud_HR_dt %>%
  group_by(ID) %>%
  mutate(mean_Bire50_95 = mean(area)) %>%
  left_join(age_sex_dt_2)

results_kud_HR_dt$sex_age <- paste0(results_kud_HR_dt$sex, "_", results_kud_HR_dt$age)

results_kud_HR_dt$level <- as.factor(results_kud_HR_dt$level)

results_kud_HR_dt$sex_age[results_kud_HR_dt$sex_age == "femelle_adulte"] <- "femelle adulte"
results_kud_HR_dt$sex_age[results_kud_HR_dt$sex_age == "femelle_juvénile"] <- "femelle juvénile"
results_kud_HR_dt$sex_age[results_kud_HR_dt$sex_age == "mâle_adulte"] <- "mâle adulte"
results_kud_HR_dt$sex_age[results_kud_HR_dt$sex_age == "mâle_juvénile"] <- "mâle juvénile"

# surface ~ age & sex___________________________________________________________

# --- objectif ---
# estimation de la surface moyenne du domaine vital à 95 % et 50 % en fonction de l'age et du sexe

results_kud_HR <- st_read(file.path(data_generated_path, "results_kud_HR.gpkg"))

area_dt <- read.csv(paste0(data_generated_path, "results_kud_HR_dt.csv"), row.names = NULL)

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

hist(area_dt_sex_age$area)
hist(area_dt_sex_age$area[area_dt_sex_age$level == 95])
hist(area_dt_sex_age$area[area_dt_sex_age$level == 50])

m0_level <- lm(area ~ 1, data = area_dt_sex_age)
m0_gamma_level <- glm(area ~ 1, data = area_dt_sex_age, family = Gamma(link = "log"))

AIC(m0_level, m0_gamma_level)

m1_gamma_level <- glm(area ~ level * sex * age, data = area_dt_sex_age, family = Gamma(link = "log"))
m2_gamma_level <- glm(area ~ level + sex * age, data = area_dt_sex_age, family = Gamma(link = "log"))
m3_gamma_level <- glm(area ~ level + sex, data = area_dt_sex_age, family = Gamma(link = "log"))
m4_gamma_level <- glm(area ~ level + age, data = area_dt_sex_age, family = Gamma(link = "log"))

AIC(m1_gamma_level, m2_gamma_level, m3_gamma_level, m4_gamma_level)

summary(m3_gamma_level)

# diag
sim <- simulateResiduals(fittedModel = m3_gamma_level, plot = F)
# residuals(sim)
# residuals(sim, quantileFunction = qnorm, outlierValues = c(-7,7))
residuals_2 <- plot(sim)
testDispersion(sim)
testOutliers(sim)

# 95%
m0_95 <- lm(area ~ 1, data = area_dt_sex_age[area_dt_sex_age$level == 95, ])
m0_gamma_95 <- glm(area ~ 1, data = area_dt_sex_age[area_dt_sex_age$level == 95, ], family = Gamma(link = "log"))

AIC(m0_95, m0_gamma_95)

m1_gamma_95 <- glm(area ~ sex * age, data = area_dt_sex_age[area_dt_sex_age$level == 95, ], family = Gamma(link = "log"))
m2_gamma_95 <- glm(area ~ sex + age, data = area_dt_sex_age[area_dt_sex_age$level == 95, ], family = Gamma(link = "log"))
m3_gamma_95 <- glm(area ~ sex, data = area_dt_sex_age[area_dt_sex_age$level == 95, ], family = Gamma(link = "log"))
m4_gamma_95 <- glm(area ~ age, data = area_dt_sex_age[area_dt_sex_age$level == 95, ], family = Gamma(link = "log"))

AIC(m1_gamma_95, m2_gamma_95, m3_gamma_95, m4_gamma_95)

summary(m3_gamma_95)

# diag
sim <- simulateResiduals(fittedModel = m3_gamma_95, plot = F)
# residuals(sim)
# residuals(sim, quantileFunction = qnorm, outlierValues = c(-7,7))
residuals_2 <- plot(sim)
testDispersion(sim)
testOutliers(sim)

# 50%
m0_50 <- lm(area ~ 1, data = area_dt_sex_age[area_dt_sex_age$level == 50, ])
m0_gamma_50 <- glm(area ~ 1, data = area_dt_sex_age[area_dt_sex_age$level == 50, ], family = Gamma(link = "log"))

AIC(m0_50, m0_gamma_50)

m1_gamma_50 <- glm(area ~ sex * age, data = area_dt_sex_age[area_dt_sex_age$level == 50, ], family = Gamma(link = "log"))
m2_gamma_50 <- glm(area ~ sex + age, data = area_dt_sex_age[area_dt_sex_age$level == 50, ], family = Gamma(link = "log"))
m3_gamma_50 <- glm(area ~ sex, data = area_dt_sex_age[area_dt_sex_age$level == 50, ], family = Gamma(link = "log"))
m4_gamma_50 <- glm(area ~ age, data = area_dt_sex_age[area_dt_sex_age$level == 50, ], family = Gamma(link = "log"))

AIC(m1_gamma_50, m2_gamma_50, m3_gamma_50, m4_gamma_50)

summary(m3_gamma_50)

# diag
sim <- simulateResiduals(fittedModel = m3_gamma_50, plot = F)
# residuals(sim)
# residuals(sim, quantileFunction = qnorm, outlierValues = c(-7,7))
residuals_2 <- plot(sim)
testDispersion(sim)
testOutliers(sim)

# % dans la réserve_____________________________________________________________

results_kud_HR <- st_read(file.path(data_generated_path, "results_kud_HR.gpkg"))

# Unifier CRS et géométrie
RMO_transformed <- st_transform(RMO, st_crs(results_kud_HR))
RMO_geom <- st_union(RMO_transformed) # fusionner en un seul polygone
results_kud_HR <- st_transform(results_kud_HR, st_crs(RMO_geom))

results_kud_HR <- st_transform(results_kud_HR, st_crs(RMO_geom))

danslareserve <- results_kud_HR %>%
  rowwise() %>%
  mutate(
    # intersection (vide si aucune)
    geom_intersection = list({
      inter <- st_intersection(geom, RMO_geom)
      if (length(inter) == 0 || st_is_empty(inter)) {
        st_sfc(st_geometrycollection(), crs = st_crs(RMO_geom))
      } else {
        inter
      }
    }),
    # recalculer les aires directement depuis les géométries EN M2
    area_geom = as.numeric(st_area(geom)), # aire originale du polygone (m²)
    area_in_RMO = as.numeric(st_area(geom_intersection)), # aire dans la réserve (m²)
    # pourcentage cohérent
    pct_in_RMO = (area_in_RMO / area_geom) * 100
  ) %>%
  ungroup() %>%
  distinct()

danslareserve <- danslareserve %>%
  st_drop_geometry() %>%
  dplyr::select(-id) %>%
  rename(ID = param) %>%
  dplyr::select(ID, level, pct_in_RMO)

results_kud_HR_dt <- read.csv(paste0(data_generated_path, "results_kud_HR_dt.csv"), row.names = NULL)

results_kud_HR_dt <- results_kud_HR_dt %>%
  dplyr::select(-id, -X)

results_kud_HR_dt$level <- as.character(results_kud_HR_dt$level)
danslareserve$level <- as.character(danslareserve$level)

danslareserve$geom_intersection <- NULL

results_kud_HR_dt <- results_kud_HR_dt %>%
  st_drop_geometry() %>%
  group_by(ID) %>%
  mutate(mean_Bire50_95 = mean(area)) %>%
  left_join(age_sex_dt_2) %>%
  left_join(danslareserve)

results_kud_HR_dt$sex_age <- paste0(results_kud_HR_dt$sex, "_", results_kud_HR_dt$age)

results_kud_HR_dt$level <- as.factor(results_kud_HR_dt$level)

results_kud_HR_dt$sex_age[results_kud_HR_dt$sex_age == "femelle_adulte"] <- "femelle adulte"
results_kud_HR_dt$sex_age[results_kud_HR_dt$sex_age == "femelle_juvénile"] <- "femelle juvénile"
results_kud_HR_dt$sex_age[results_kud_HR_dt$sex_age == "mâle_adulte"] <- "mâle adulte"
results_kud_HR_dt$sex_age[results_kud_HR_dt$sex_age == "mâle_juvénile"] <- "mâle juvénile"

results_kud_HR_dt$level <- as.character(results_kud_HR_dt$level)
results_kud_HR_dt$level[results_kud_HR_dt$level == "50"] <- "Domaine vital principal"
results_kud_HR_dt$level[results_kud_HR_dt$level == "95"] <- "Domaine vital étendu"

results_kud_HR_dt <- results_kud_HR_dt %>%
  na.omit()

# Graphique_____________________________________________________________________

hr_plot <- ggplot() +
  geom_point(
    data = results_kud_HR_dt, aes(reorder(ID, mean_Bire50_95), area,
      shape = sex_age, col = pct_in_RMO
    ), size = 4
  ) +
  facet_wrap(level ~ ., scales = "free", ncol = 1) +
  scale_color_gradient2(low = "lightgrey", mid = "#49B6FF", high = "black", , midpoint = 50) +
  scale_shape_manual(values = c(17, 2, 16, 1)) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    legend.position = c(.3, .3),
    legend.direction = "horizontal"
  ) +
  labs(
    title = "", # Titre et axes
    x = "Individu",
    y = "Aire du domaine vital (m²)",
    col = "% d'utilisation de la RNNMO",
    shape = "Sexe & age"
  ) ; hr_plot

# Calculer la moyenne par niveau
moyennes_par_niveau <- results_kud_HR_dt %>%
  group_by(level) %>%
  summarise(moyenne = mean(pct_in_RMO, na.rm = TRUE))

# Graphique
hist_danslareserve <- ggplot(results_kud_HR_dt, aes(x = pct_in_RMO)) +
  geom_histogram(
    bins = 30,
    fill = "#49B6FF",
    color = "white",
    alpha = 0.8
  ) +
  geom_vline(
    data = moyennes_par_niveau,
    aes(xintercept = moyenne),
    color = "black",
    size = 1
  ) +
  theme_classic() +
  labs(
    title = "",
    x = "Pourcentage d'utilisation\nde la RNNMO",
    y = "Nombre d'individus"
  ) +
  facet_wrap(~level, scales = "free", ncol = 2) ; hist_danslareserve

# Sauvegarde du NULL# Sauvegarde du graphique
ggsave(paste0(atlas_path, "/hr_plot.png"),
  plot = hr_plot, width = 12, height = 8, dpi = 300
)

# Sauvegarde du graphique
ggsave(paste0(atlas_path, "/hist_danslareserve.png"),
       plot = hist_danslareserve, width = 8, height = 4, dpi = 300
)

# _____________________________________________________________________________________________________________________________________
# _____________________________________________________________________________________________________________________________________
# 5. Zones de repos ----
# _____________________________________________________________________________________________________________________________________
# _____________________________________________________________________________________________________________________________________

# --- objectif ---
# localisation des principales zone de repos

GPS_sampled <- sample_weighted_points(
  data = GPS_roosting,
  n = 1000,
  variable = NULL,
  zone = "zone",
  cap = 3600
)

GPS_sampled <- st_as_sf(GPS_sampled, coords = c("lon", "lat"), crs = 4326) %>%
  mutate(lon = st_coordinates(.)[, 1], lat = st_coordinates(.)[, 2])

zoom_levels <- c("A", "B", "C")
results_kud <- NULL
nb_kud <- NULL
analyse <- "kud"
comportement <- "roosting"
couleur <- couleur_roosting

plan(multisession, workers = 3)

results_list <- future_lapply(
  zoom_levels,
  function(z) {
    make_kud(analyse, z, comportement, GPS_sampled, data_generated_path, resolution_ZOOM, couleur)
  },
  future.seed = TRUE
)

# enregistrement roosting_poly _________________________________________________

# Extraire les objets sf pour chaque zoom
kud_sf_B <- results_list[[1]]$kud_sf
kud_sf_A <- results_list[[2]]$kud_sf
kud_sf_c <- results_list[[3]]$kud_sf

# Rassembler les trois objets en un seul
sf_long <- bind_rows(kud_sf_B, kud_sf_A, kud_sf_c)

# Éclater les MultiPolygon en Polygons
roosting_poly <- st_cast(sf_long, "POLYGON")

# Ajouter un ID_roosting unique par zoom
roosting_poly <- roosting_poly %>%
  group_by(ZOOM, level) %>%
  mutate(ID_roosting = paste0(ZOOM, "_", level, "_", row_number())) %>%
  ungroup()

# write & read
st_write(roosting_poly, paste0(data_generated_path, "roosting_poly.gpkg"), append = FALSE)
roosting_poly <- st_read(file.path(data_generated_path, "roosting_poly.gpkg"))

# _____________________________________________________________________________________________________________________________________
# _____________________________________________________________________________________________________________________________________
# 6. Zones d'alimentation ----
# _____________________________________________________________________________________________________________________________________
# _____________________________________________________________________________________________________________________________________

# --- objectif ---
# localisation des principales d'alimentation

GPS_sampled <- sample_weighted_points(
  data = GPS_foraging,
  n = 1000,
  variable = NULL,
  zone = "zone",
  cap = 3600
)

GPS_sampled <- st_as_sf(GPS_sampled, coords = c("lon", "lat"), crs = 4326) %>%
  mutate(lon = st_coordinates(.)[, 1], lat = st_coordinates(.)[, 2])

zoom_levels <- c("A", "B", "C")
results_kud <- NULL
nb_kud <- NULL
analyse <- "kud"
comportement <- "foraging"
couleur <- couleur_foraging

plan(multisession, workers = 3)

results_list <- future_lapply(
  zoom_levels,
  function(z) {
    make_kud(analyse, z, comportement, GPS_sampled, data_generated_path, resolution_ZOOM, couleur)
  },
  future.seed = TRUE # garantit des tirages aléatoires reproductibles et indépendants
)

# _____________________________________________________________________________________________________________________________________
# _____________________________________________________________________________________________________________________________________
# 7. Mois ----------------------------------------------------------------------
# _____________________________________________________________________________________________________________________________________
# _____________________________________________________________________________________________________________________________________

# --- objectif ---
# localisation de la zone de repos ou d'alimentation en fonction du mois de l'année

# reposoir______________________________________________________________________

GPS_sampled <- sample_weighted_points(
  data = GPS_roosting,
  n = 1000,
  variable = "month_label",
  zone = "zone",
  cap = 3600
)

GPS_sampled <- st_as_sf(GPS_sampled, coords = c("lon", "lat"), crs = 4326) %>%
  mutate(lon = st_coordinates(.)[, 1], lat = st_coordinates(.)[, 2])

table(GPS_sampled$ID)

zoom_levels <- c("A", "B", "C")

results_kud <- NULL
nb_kud <- NULL
analyse <- "kud"
variable <- "month_label"
comportement <- "roosting"
couleur <- generate_color_gradient("#9A7AA0", light_max = 1, dark_max = 1, n_total = 12)
scales::show_col(couleur)
couleur <- generate_five_gradient("red", "yellow", "darkgrey", darken("#FF00E6", 0.9), "#FF00E6", n_total = 12)
scales::show_col(couleur)

plan(multisession, workers = 3)

results_list <- future_lapply(
  zoom_levels,
  function(z) {
    make_kud_variable(analyse, z, comportement, GPS_sampled, data_generated_path, resolution_ZOOM, couleur, variable)
  },
  future.seed = TRUE
)

# alimentation__________________________________________________________________

GPS_sampled <- sample_weighted_points(
  data = GPS_foraging,
  n = 1000,
  variable = "month_label",
  zone = "zone",
  cap = 3600
)

GPS_sampled <- st_as_sf(GPS_sampled, coords = c("lon", "lat"), crs = 4326) %>%
  mutate(lon = st_coordinates(.)[, 1], lat = st_coordinates(.)[, 2])

zoom_levels <- c("A", "B", "C")

results_kud <- NULL
nb_kud <- NULL
analyse <- "kud"
variable <- "month_label"
comportement <- "foraging"
couleur <- generate_five_gradient("red", "yellow","darkgrey",darken("#49B6FF", 0.9),"#49B6FF", n_total = 12)
scales::show_col(couleur)

plan(multisession, workers = 3)

results_list <- future_lapply(
  zoom_levels,
  function(z) {
    make_kud_variable(analyse, z, comportement, GPS_sampled, data_generated_path, resolution_ZOOM, couleur, variable)
  },
  future.seed = TRUE
)

# _____________________________________________________________________________________________________________________________________
# _____________________________________________________________________________________________________________________________________
# 8. Jour & nuit ---------------------------------------------------------------
# _____________________________________________________________________________________________________________________________________
# _____________________________________________________________________________________________________________________________________

# --- objectif ---
# localisation de la zone de repos ou d'alimentation en fonction du jour et de la nuit

# reposoir______________________________________________________________________

GPS_sampled <- sample_weighted_points(
  data = GPS_roosting,
  n = 1000,
  variable = "timeofday",
  zone = "zone",
  cap = 3600
)

GPS_sampled <- st_as_sf(GPS_sampled, coords = c("lon", "lat"), crs = 4326) %>%
  mutate(lon = st_coordinates(.)[, 1], lat = st_coordinates(.)[, 2])

table(GPS_sampled$ID)

zoom_levels <- c("A", "B", "C")

results_kud <- NULL
nb_kud <- NULL
analyse <- "kud"
variable <- "timeofday"
comportement <- "roosting"
couleur <- c("#FF00E6", "yellow")
scales::show_col(couleur)

plan(multisession, workers = 3)

results_list <- future_lapply(
  zoom_levels,
  function(z) {
    make_kud_variable(analyse, z, comportement, GPS_sampled, data_generated_path, resolution_ZOOM, couleur, variable)
  },
  future.seed = TRUE
)

# alimentation__________________________________________________________________

GPS_sampled <- sample_weighted_points(
  data = GPS_foraging,
  n = 1000,
  variable = "timeofday",
  zone = "zone",
  cap = 3600
)

GPS_sampled <- st_as_sf(GPS_sampled, coords = c("lon", "lat"), crs = 4326) %>%
  mutate(lon = st_coordinates(.)[, 1], lat = st_coordinates(.)[, 2])

zoom_levels <- c("A", "B", "C")

results_kud <- NULL
nb_kud <- NULL
analyse <- "kud"
variable <- "timeofday"
comportement <- "foraging"
couleur <- c("#49B6FF", "yellow")
scales::show_col(couleur)

plan(multisession, workers = 3)

results_list <- future_lapply(
  zoom_levels,
  function(z) {
    make_kud_variable(analyse, z, comportement, GPS_sampled, data_generated_path, resolution_ZOOM, couleur, variable)
  },
  future.seed = TRUE
)

# _____________________________________________________________________________________________________________________________________
# _____________________________________________________________________________________________________________________________________
# 9. Neap, spring, submersion --------------------------------------------------
# _____________________________________________________________________________________________________________________________________
# _____________________________________________________________________________________________________________________________________

# --- objectif ---
# localisation de la zone de repos en fonction de la hauteur d'eau lors des marées hautes

# reposoir______________________________________________________________________

GPS_sampled <- sample_weighted_points(
  data = GPS_roosting,
  n = 1000,
  variable = "tide_strength",
  zone = "zone",
  cap = 3600
)

table(GPS_sampled$tide_strength, useNA = "always")

GPS_sampled <- st_as_sf(GPS_sampled, coords = c("lon", "lat"), crs = 4326) %>%
  mutate(lon = st_coordinates(.)[, 1], lat = st_coordinates(.)[, 2])


zoom_levels <- c("A", "B", "C")

results_kud <- NULL
nb_kud <- NULL
analyse <- "kud"
variable <- "tide_strength"
comportement <- "roosting"
couleur <- c("#FF00E6", "yellow", "red")
scales::show_col(couleur)

plan(multisession, workers = 3)

results_list <- future_lapply(
  zoom_levels,
  function(z) {
    make_kud_variable(analyse, z, comportement, GPS_sampled, data_generated_path, resolution_ZOOM, couleur, variable)
  },
  future.seed = TRUE
)

# alimentation__________________________________________________________________

GPS_sampled <- sample_weighted_points(
  data = GPS_foraging,
  n = 1000,
  variable = "tide_strength",
  zone = "zone",
  cap = 3600
)

GPS_sampled <- st_as_sf(GPS_sampled, coords = c("lon", "lat"), crs = 4326) %>%
  mutate(lon = st_coordinates(.)[, 1], lat = st_coordinates(.)[, 2])

zoom_levels <- c("A", "B", "C")

results_kud <- NULL
nb_kud <- NULL
analyse <- "kud"
variable <- "tide_strength"
comportement <- "foraging"
couleur <- c("#49B6FF", "yellow", "red")
scales::show_col(couleur)

plan(multisession, workers = 3)

results_list <- future_lapply(
  zoom_levels,
  function(z) {
    make_kud_variable(analyse, z, comportement, GPS_sampled, data_generated_path, resolution_ZOOM, couleur, variable)
  },
  future.seed = TRUE
)

kud_sf <- results_list[[2]]$kud_sf

# Recalcule des surfaces par polygone (en m²)
kud_sf <- kud_sf %>%
  mutate(surface_m2 = as.numeric(st_area(geometry)))

# Somme des surfaces par variable (type de marée) ET level (50 ou 95)
surface_par_zone_level <- kud_sf %>%
  group_by(variable, level) %>%
  summarise(surface_totale_m2 = sum(surface_m2), .groups = "drop") %>%
  mutate(surface_ha = surface_totale_m2 / 10000)

surface_par_zone_level

# _____________________________________________________________________________________________________________________________________
# _____________________________________________________________________________________________________________________________________
# 10. Age ----------------------------------------------------------------------
# _____________________________________________________________________________________________________________________________________
# _____________________________________________________________________________________________________________________________________

# --- objectif ---
# localisation de la zone de repos ou d'alimentation en fonction du mois de l'age des individus

# reposoir______________________________________________________________________

GPS_sampled <- sample_weighted_points(
  data = GPS_roosting,
  n = 1000,
  variable = "age",
  zone = "zone",
  cap = 3600
)

GPS_sampled <- st_as_sf(GPS_sampled, coords = c("lon", "lat"), crs = 4326) %>%
  mutate(lon = st_coordinates(.)[, 1], lat = st_coordinates(.)[, 2])

table(GPS_sampled$ID)

zoom_levels <- c("A", "B", "C")

results_kud <- NULL
nb_kud <- NULL
analyse <- "kud"
variable <- "age"
comportement <- "roosting"
couleur <- c("#FF00E6", "yellow")
scales::show_col(couleur)

plan(multisession, workers = 3)

results_list <- future_lapply(
  zoom_levels,
  function(z) {
    make_kud_variable(analyse, z, comportement, GPS_sampled, data_generated_path, resolution_ZOOM, couleur, variable)
  },
  future.seed = TRUE
)

# alimentation__________________________________________________________________

GPS_sampled <- sample_weighted_points(
  data = GPS_foraging,
  n = 1000,
  variable = "age",
  zone = "zone",
  cap = 3600
)

GPS_sampled <- st_as_sf(GPS_sampled, coords = c("lon", "lat"), crs = 4326) %>%
  mutate(lon = st_coordinates(.)[, 1], lat = st_coordinates(.)[, 2])

zoom_levels <- c("A", "B", "C")

results_kud <- NULL
nb_kud <- NULL
analyse <- "kud"
variable <- "age"
comportement <- "foraging"
couleur <- c("#49B6FF", "yellow")
scales::show_col(couleur)

plan(multisession, workers = 3)

results_list <- future_lapply(
  zoom_levels,
  function(z) {
    make_kud_variable(analyse, z, comportement, GPS_sampled, data_generated_path, resolution_ZOOM, couleur, variable)
  },
  future.seed = TRUE
)

# _____________________________________________________________________________________________________________________________________
# _____________________________________________________________________________________________________________________________________
# 11. Sexe ---------------------------------------------------------------------
# _____________________________________________________________________________________________________________________________________
# _____________________________________________________________________________________________________________________________________

# --- objectif ---
# localisation de la zone de repos ou d'alimentation en fonction du sexe des individus

# reposoir______________________________________________________________________

GPS_sampled <- sample_weighted_points(
  data = GPS_roosting,
  n = 1000,
  variable = "sex",
  zone = "zone",
  cap = 3600
)

GPS_sampled <- st_as_sf(GPS_sampled, coords = c("lon", "lat"), crs = 4326) %>%
  mutate(lon = st_coordinates(.)[, 1], lat = st_coordinates(.)[, 2])

table(GPS_sampled$ID)

zoom_levels <- c("A", "B", "C")

results_kud <- NULL
nb_kud <- NULL
analyse <- "kud"
variable <- "sex"
comportement <- "roosting"
couleur <- c("#FF00E6", "yellow")
scales::show_col(couleur)

plan(multisession, workers = 3)

results_list <- future_lapply(
  zoom_levels,
  function(z) {
    make_kud_variable(analyse, z, comportement, GPS_sampled, data_generated_path, resolution_ZOOM, couleur, variable)
  },
  future.seed = TRUE
)

# alimentation__________________________________________________________________

GPS_sampled <- sample_weighted_points(
  data = GPS_foraging,
  n = 1000,
  variable = "sex",
  zone = "zone",
  cap = 3600
)

GPS_sampled <- st_as_sf(GPS_sampled, coords = c("lon", "lat"), crs = 4326) %>%
  mutate(lon = st_coordinates(.)[, 1], lat = st_coordinates(.)[, 2])

zoom_levels <- c("A", "B", "C")

results_kud <- NULL
nb_kud <- NULL
analyse <- "kud"
variable <- "sex"
comportement <- "foraging"
couleur <- c("#49B6FF", "yellow")
scales::show_col(couleur)

plan(multisession, workers = 3)

results_list <- future_lapply(
  zoom_levels,
  function(z) {
    make_kud_variable(analyse, z, comportement, GPS_sampled, data_generated_path, resolution_ZOOM, couleur, variable)
  },
  future.seed = TRUE
)

# _____________________________________________________________________________________________________________________________________
# _____________________________________________________________________________________________________________________________________
# 12. Fidélité aux reposoirs ---------------------------------------------------
# _____________________________________________________________________________________________________________________________________
# _____________________________________________________________________________________________________________________________________

# identification des reposoirs__________________________________________________

# Charger le polygone
roosting_poly <- st_read(file.path(data_generated_path, "roosting_poly.gpkg"))

# roosting_poly <- roosting_poly %>%
#   filter(level =="95")

# Filtrer les points 'roosting'
GPS_roosting_where <- GPS %>%
  filter(behavior == "roosting")

# S'assurer que les CRS correspondent
GPS_roosting_where <- st_transform(GPS_roosting_where, st_crs(roosting_poly))

# ---- Version rapide avec st_join ---
# Effectue une jointure spatiale : chaque point reçoit l'attribut du polygone dans lequel il tombe
GPS_roosting_where <- st_join(
  GPS_roosting_where,
  roosting_poly %>% dplyr::select(ID_roosting, level),
  join = st_intersects,
  left = TRUE
)

# Renommer la colonne si tu veux garder la logique originale
GPS_roosting_where <- GPS_roosting_where %>%
  rename(where = ID_roosting)

# write & read
st_write(GPS_roosting_where, paste0(data_generated_path, "GPS_roosting_where.gpkg"), append = FALSE)
GPS_roosting_where <- st_read(file.path(data_generated_path, "GPS_roosting_where.gpkg"))

# calcul fidélité reposoirs_____________________________________________________

GPS_roosting_where <- GPS_roosting_where[!is.na(GPS_roosting_where$where), ]

div_roosting <- GPS_roosting_where %>%
  st_drop_geometry() %>%
  group_by(ID, sex, age, level, timeofday, tide_strength) %>%
  summarise(
    richesse_st = n_distinct(where) / length(where) * 2, # + = moins de fidélité
    shannon = shannon(where), # + shannon est grand = moins de fidélité = plus la variété des reposoirs est importante
    variation_taux = variation_taux(where), # + = moins de fidélité = Plus la valeur est grande, plus la séquence est instable (changements fréquents de reposoirs)
    equitabilite = equitabilite(where)
  ) %>% # si certains oiseaux “se concentrent” sur une ou plusieurs zones : → Équitabilité = 1 → utilisation équilibrée de plusieurs zones, → Équitabilité faible → utilisation concentrée (forte fidélité à une seule zone)
  na.omit()

## reposoirs 50%________________________________________________________________

div_roosting_50 <- div_roosting %>%
  filter(level == "50")

div_roosting_50$tide_strength[div_roosting_50$tide_strength == "marée de mortes eaux"] <- "mortes eaux"
div_roosting_50$tide_strength[div_roosting_50$tide_strength == "marée de vives eaux"] <- "vives eaux"

# richesse_st__________

hist(div_roosting_50$richesse_st)

# Modèle linéaire pour tester l'effet du age sur la distance
richesse_gaussien <- lm(richesse_st ~ 1, data = div_roosting_50)
richesse_gamma <- glm(richesse_st ~ 1, data = div_roosting_50, family = Gamma(link = "log"))
richesse_log <- lm(log(richesse_st) ~ 1, data = div_roosting_50)

AIC(richesse_gaussien, richesse_gamma, richesse_log)

# diag
sim <- simulateResiduals(fittedModel = richesse_gamma, plot = F)
# residuals(sim)
# residuals(sim, quantileFunction = qnorm, outlierValues = c(-7,7))
residuals_2 <- plot(sim)
testDispersion(sim)
testOutliers(sim)

summary(richesse_gamma)

# selection de modèle
richesse_m1_50 <- glmer(richesse_st ~ sex + age + timeofday + tide_strength + (1 | ID), data = div_roosting_50, family = Gamma(link = "log"))
richesse_m2_50 <- glmer(richesse_st ~ sex + age + timeofday + (1 | ID), data = div_roosting_50, family = Gamma(link = "log"))
richesse_m3_50 <- glmer(richesse_st ~ sex + age + (1 | ID), data = div_roosting_50, family = Gamma(link = "log"))
richesse_m4_50 <- glmer(richesse_st ~ sex + (1 | ID), data = div_roosting_50, family = Gamma(link = "log"))
richesse_m5_50 <- glmer(richesse_st ~ age + (1 | ID), data = div_roosting_50, family = Gamma(link = "log"))
richesse_m6_50 <- glmer(richesse_st ~ sex * age + (1 | ID), data = div_roosting_50, family = Gamma(link = "log"))
richesse_m7_50 <- glmer(richesse_st ~ sex * tide_strength + age * tide_strength + (1 | ID), data = div_roosting_50, family = Gamma(link = "log"))
richesse_m8_50 <- glmer(richesse_st ~ sex * tide_strength + age + (1 | ID), data = div_roosting_50, family = Gamma(link = "log"))
richesse_m9_50 <- glmer(richesse_st ~ sex + age * tide_strength + (1 | ID), data = div_roosting_50, family = Gamma(link = "log"))

AIC(richesse_m1_50, richesse_m2_50, richesse_m4_50, richesse_m5_50, richesse_m6_50, richesse_m7_50, richesse_m8_50, richesse_m9_50)

summary(richesse_m7_50)

# predictions plot

# Créer un jeu de données pour les prédictions
new_data_richesse_50 <- expand.grid(
  sex = c("femelle", "mâle"),
  tide_strength = c("mortes eaux", "vives eaux", "submersion"),
  age = c("adulte", "juvénile")
)

# S'assurer que les niveaux correspondent à ceux du modèle
new_data_richesse_50$sex <- factor(new_data_richesse_50$sex, levels = c("femelle", "mâle"))
new_data_richesse_50$tide_strength <- factor(new_data_richesse_50$tide_strength, levels = c("mortes eaux", "vives eaux", "submersion"))
new_data_richesse_50$age <- factor(new_data_richesse_50$age, levels = c("adulte", "juvénile"))

# Ajouter une colonne ID fictive (nécessaire pour le modèle)
new_data_richesse_50$ID <- "prediction"

# Calculer les prédictions et les erreurs standards
predictions <- predict(richesse_m7_50, newdata = new_data_richesse_50, re.form = NA, type = "response", se.fit = TRUE)
new_data_richesse_50$pred <- predictions$fit
new_data_richesse_50$se <- predictions$se.fit

# Calculer les intervalles de confiance à 95%
new_data_richesse_50$lower <- new_data_richesse_50$pred - 1.96 * new_data_richesse_50$se
new_data_richesse_50$upper <- new_data_richesse_50$pred + 1.96 * new_data_richesse_50$se

pred_richesse_50_plot <- ggplot(new_data_richesse_50, aes(x = tide_strength, y = pred, color = sex, shape = age)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0, position = position_dodge(width = 0.5)) +
  labs(
    title = "a)",
    x = "Intensité de marée",
    y = "Prédiction de richesse\nen reposoirs principaux",
    color = "Sexe",
    shape = "Âge"
  ) +
  geom_jitter(size = 3, position = position_dodge(width = 0.5), fill = "white") +
  scale_color_manual(values = c("#FF00E6", "#49B6FF")) +
  scale_shape_manual(values = c(19, 21)) +
  theme_classic()
pred_richesse_50_plot

# shannon__________

hist(div_roosting_50$shannon)

# Modèle linéaire pour tester l'effet du age sur la distance
shannon_gaussien <- lm(shannon ~ 1, data = div_roosting_50)

# diag
sim <- simulateResiduals(fittedModel = shannon_gaussien, plot = F)
# residuals(sim)
# residuals(sim, quantileFunction = qnorm, outlierValues = c(-7,7))
residuals_2 <- plot(sim)
testDispersion(sim)
testOutliers(sim)

summary(shannon_gaussien)

# selection de modèle
shannon_m1_50 <- lmer(shannon ~ sex + age + timeofday + tide_strength + (1 | ID), data = div_roosting_50)
shannon_m2_50 <- lmer(shannon ~ sex + age + timeofday + (1 | ID), data = div_roosting_50)
shannon_m3_50 <- lmer(shannon ~ sex + age + (1 | ID), data = div_roosting_50)
shannon_m4_50 <- lmer(shannon ~ sex + (1 | ID), data = div_roosting_50)
shannon_m5_50 <- lmer(shannon ~ age + (1 | ID), data = div_roosting_50)
shannon_m6_50 <- lmer(shannon ~ sex * age + (1 | ID), data = div_roosting_50)
shannon_m7_50 <- lmer(shannon ~ sex * tide_strength + age * tide_strength + (1 | ID), data = div_roosting_50)
shannon_m8_50 <- lmer(shannon ~ sex * tide_strength + age + (1 | ID), data = div_roosting_50)
shannon_m9_50 <- lmer(shannon ~ sex + age * tide_strength + (1 | ID), data = div_roosting_50)

AIC(shannon_m1_50, shannon_m2_50, shannon_m3_50, shannon_m4_50, shannon_m5_50, shannon_m6_50, shannon_m7_50, shannon_m8_50, shannon_m9_50)

summary(shannon_m9_50)

# predictions plot

# Créer un jeu de données pour les prédictions
new_data_shannon_50 <- expand.grid(
  sex = c("femelle", "mâle"),
  tide_strength = c("mortes eaux", "vives eaux", "submersion"),
  age = c("adulte", "juvénile")
)

# S'assurer que les niveaux correspondent à ceux du modèle
new_data_shannon_50$sex <- factor(new_data_shannon_50$sex, levels = c("femelle", "mâle"))
new_data_shannon_50$tide_strength <- factor(new_data_shannon_50$tide_strength, levels = c("mortes eaux", "vives eaux", "submersion"))
new_data_shannon_50$age <- factor(new_data_shannon_50$age, levels = c("adulte", "juvénile"))

# Ajouter une colonne ID fictive (nécessaire pour le modèle)
new_data_shannon_50$ID <- "prediction"

# Calculer les prédictions et les erreurs standards
predictions <- predict(shannon_m9_50, newdata = new_data_shannon_50, re.form = NA, type = "response", se.fit = TRUE)
new_data_shannon_50$pred <- predictions$fit
new_data_shannon_50$se <- predictions$se.fit

# Calculer les intervalles de confiance à 95%
new_data_shannon_50$lower <- new_data_shannon_50$pred - 1.96 * new_data_shannon_50$se
new_data_shannon_50$upper <- new_data_shannon_50$pred + 1.96 * new_data_shannon_50$se

pred_shannon_50_plot <- ggplot(new_data_shannon_50, aes(x = tide_strength, y = pred, shape = age, color = sex)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0, position = position_dodge(width = 0.5)) +
  labs(
    title = "b)",
    x = "Intensité de marée",
    y = "Prédiction indice de Shannon\nen reposoirs principaux",
    color = "Sexe",
    shape = "Âge"
  ) +
  geom_jitter(size = 3, position = position_dodge(width = 0.5), fill = "white") +
  scale_color_manual(values = c("#FF00E6", "#49B6FF")) +
  scale_shape_manual(values = c(19, 21)) +
  theme_classic()
pred_shannon_50_plot

# variation_taux__________

hist(div_roosting_50$variation_taux)

# Modèle linéaire pour tester l'effet du age sur la distance
variation_taux_gaussien <- lm(variation_taux ~ 1, data = div_roosting_50)
variation_taux_Aetareg <- betareg::betareg(variation_taux ~ 1, data = div_roosting_50, link = "logit")

AIC(variation_taux_gaussien, variation_taux_Aetareg)

# library(statmod)

# diag
# sim <- simulateResiduals(fittedModel = shannon_Aetareg, plot = F)
# residuals(sim)
# residuals(sim, quantileFunction = qnorm, outlierValues = c(-7,7))
# residuals_2 <- plot(sim)
# testDispersion(sim)
# testOutliers(sim)

# selection de modèle
variation_taux_m1_50 <- lmer(variation_taux ~ sex + age + timeofday + tide_strength + (1 | ID), data = div_roosting_50)
variation_taux_m2_50 <- lmer(variation_taux ~ sex + age + timeofday + (1 | ID), data = div_roosting_50)
variation_taux_m3_50 <- lmer(variation_taux ~ sex + age + (1 | ID), data = div_roosting_50)
variation_taux_m4_50 <- lmer(variation_taux ~ sex + (1 | ID), data = div_roosting_50)
variation_taux_m5_50 <- lmer(variation_taux ~ age + (1 | ID), data = div_roosting_50)
variation_taux_m6_50 <- lmer(variation_taux ~ sex * age + (1 | ID), data = div_roosting_50)
variation_taux_m7_50 <- lmer(variation_taux ~ sex * tide_strength + age * tide_strength + (1 | ID), data = div_roosting_50)
variation_taux_m8_50 <- lmer(variation_taux ~ sex * tide_strength + age + (1 | ID), data = div_roosting_50)
variation_taux_m9_50 <- lmer(variation_taux ~ sex + age * tide_strength + (1 | ID), data = div_roosting_50)

AIC(
  variation_taux_m1_50, variation_taux_m2_50, variation_taux_m3_50, variation_taux_m4_50, variation_taux_m5_50,
  variation_taux_m6_50, variation_taux_m7_50, variation_taux_m8_50, variation_taux_m9_50
)

summary(variation_taux_m5_50)

# predictions plot

# Créer un jeu de données pour les prédictions
new_data_variation_taux_50 <- expand.grid(
  # sex = c("femelle", "mâle"),
  # tide_strength = c("marée de mortes eaux","marée de vives eaux", "submersion"),
  age = c("adulte", "juvénile")
)

# S'assurer que les niveaux correspondent à ceux du modèle
# new_data_variation_taux_50$sex <- factor(new_data_variation_taux_50$sex, levels = c("femelle", "mâle"))
# new_data_variation_taux_50$tide_strength <- factor(new_data_variation_taux_50$tide_strength, levels = c("marée de mortes eaux","marée de vives eaux", "submersion"))
new_data_variation_taux_50$age <- factor(new_data_variation_taux_50$age, levels = c("adulte", "juvénile"))

# Ajouter une colonne ID fictive (nécessaire pour le modèle)
new_data_variation_taux_50$ID <- "prediction"

# Calculer les prédictions et les erreurs standards
predictions <- predict(variation_taux_m5_50, newdata = new_data_variation_taux_50, re.form = NA, type = "response", se.fit = TRUE)
new_data_variation_taux_50$pred <- predictions$fit
new_data_variation_taux_50$se <- predictions$se.fit

# Calculer les intervalles de confiance à 95%
new_data_variation_taux_50$lower <- new_data_variation_taux_50$pred - 1.96 * new_data_variation_taux_50$se
new_data_variation_taux_50$upper <- new_data_variation_taux_50$pred + 1.96 * new_data_variation_taux_50$se

pred_variation_taux_50_plot <- ggplot(new_data_variation_taux_50, aes(x = age, y = pred, shape = age)) +
  geom_jitter(size = 3, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0, position = position_dodge(width = 0.5)) +
  labs(
    title = "b)",
    x = "Age",
    y = "Prédiction du taux de variation \nen reposoirs principaux",
    shape = "Âge",
  ) +
  geom_jitter(size = 3, position = position_dodge(width = 0.5), fill = "white") +
  scale_shape_manual(values = c(19, 21)) +
  theme_classic() +
  theme(legend.position = "none")
pred_variation_taux_50_plot

# equitabilité__________

hist(div_roosting_50$equitabilite)

# Modèle linéaire pour tester l'effet du age sur la distance
equitabilite_gaussien <- lm(equitabilite ~ 1, data = div_roosting_50)
# equitabilite_gamma <- glm(equitabilite ~ 1, data = div_roosting_50, family = Gamma(link = "log"))
# equitabilite_log <- lm(log(equitabilite) ~ 1, data = div_roosting_50)

# AIC(equitabilite_gaussien, equitabilite_gamma, equitabilite_log)

# diag
sim <- simulateResiduals(fittedModel = equitabilite_gaussien, plot = F)
# residuals(sim)
# residuals(sim, quantileFunction = qnorm, outlierValues = c(-7,7))
residuals_2 <- plot(sim)
testDispersion(sim)
testOutliers(sim)

summary(equitabilite_gaussien)

# selection de modèle
equitabilite_m1_50 <- lmer(equitabilite ~ sex + age + timeofday + tide_strength + (1 | ID), data = div_roosting_50)
equitabilite_m2_50 <- lmer(equitabilite ~ sex + age + timeofday + (1 | ID), data = div_roosting_50)
equitabilite_m3_50 <- lmer(equitabilite ~ sex + age + (1 | ID), data = div_roosting_50)
equitabilite_m4_50 <- lmer(equitabilite ~ sex + (1 | ID), data = div_roosting_50)
equitabilite_m5_50 <- lmer(equitabilite ~ age + (1 | ID), data = div_roosting_50)
equitabilite_m6_50 <- lmer(equitabilite ~ sex * age + (1 | ID), data = div_roosting_50)
equitabilite_m7_50 <- lmer(equitabilite ~ sex * tide_strength + age * tide_strength + (1 | ID), data = div_roosting_50)
equitabilite_m8_50 <- lmer(equitabilite ~ sex * tide_strength + age + (1 | ID), data = div_roosting_50)
equitabilite_m9_50 <- lmer(equitabilite ~ sex + age * tide_strength + (1 | ID), data = div_roosting_50)

AIC(equitabilite_m1_50, equitabilite_m2_50, equitabilite_m4_50, equitabilite_m5_50, equitabilite_m6_50, equitabilite_m7_50, equitabilite_m8_50, equitabilite_m9_50)

summary(equitabilite_m7_50)

# predictions plot

# Créer un jeu de données pour les prédictions
new_data_equitabilite_50 <- expand.grid(
  sex = c("femelle", "mâle"),
  tide_strength = c("mortes eaux", "vives eaux", "submersion"),
  age = c("adulte", "juvénile")
)

# S'assurer que les niveaux correspondent à ceux du modèle
new_data_equitabilite_50$sex <- factor(new_data_equitabilite_50$sex, levels = c("femelle", "mâle"))
new_data_equitabilite_50$tide_strength <- factor(new_data_equitabilite_50$tide_strength, levels = c("mortes eaux", "vives eaux", "submersion"))
new_data_equitabilite_50$age <- factor(new_data_equitabilite_50$age, levels = c("adulte", "juvénile"))

# Ajouter une colonne ID fictive (nécessaire pour le modèle)
new_data_equitabilite_50$ID <- "prediction"

# Calculer les prédictions et les erreurs standards
predictions <- predict(equitabilite_m7_50, newdata = new_data_equitabilite_50, re.form = NA, type = "response", se.fit = TRUE)
new_data_equitabilite_50$pred <- predictions$fit
new_data_equitabilite_50$se <- predictions$se.fit

# Calculer les intervalles de confiance à 95%
new_data_equitabilite_50$lower <- new_data_equitabilite_50$pred - 1.96 * new_data_equitabilite_50$se
new_data_equitabilite_50$upper <- new_data_equitabilite_50$pred + 1.96 * new_data_equitabilite_50$se

pred_equitabilite_50_plot <- ggplot(new_data_equitabilite_50, aes(x = tide_strength, y = pred, color = sex, shape = age)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0, position = position_dodge(width = 0.5)) +
  labs(
    title = "a)",
    x = "Intensité de marée",
    y = "Prédiction de équitabilité\nen reposoirs principaux",
    color = "Sexe",
    shape = "Âge"
  ) +
  geom_jitter(size = 3, position = position_dodge(width = 0.5), fill = "white") +
  scale_color_manual(values = c("#FF00E6", "#49B6FF")) +
  scale_shape_manual(values = c(19, 21)) +
  theme_classic()
pred_equitabilite_50_plot

## reposoir 95%_________________________________________________________________

div_roosting_95 <- div_roosting %>%
  filter(level == "95")

div_roosting_95$tide_strength[div_roosting_95$tide_strength == "marée de mortes eaux"] <- "mortes eaux"
div_roosting_95$tide_strength[div_roosting_95$tide_strength == "marée de vives eaux"] <- "vives eaux"

# richesse_st__________

hist(div_roosting_95$richesse_st)

# Modèle linéaire pour tester l'effet du age sur la distance
richesse_gaussien <- lm(richesse_st ~ 1, data = div_roosting_95)
richesse_gamma <- glm(richesse_st ~ 1, data = div_roosting_95, family = Gamma(link = "log"))
richesse_log <- lm(log(richesse_st) ~ 1, data = div_roosting_95)

AIC(richesse_gaussien, richesse_gamma, richesse_log)

# diag
sim <- simulateResiduals(fittedModel = richesse_gamma, plot = F)
# residuals(sim)
# residuals(sim, quantileFunction = qnorm, outlierValues = c(-7,7))
residuals_2 <- plot(sim)
testDispersion(sim)
testOutliers(sim)

summary(richesse_gamma)

# selection de modèle
richesse_m1_95 <- glmer(richesse_st ~ sex + age + timeofday + tide_strength + (1 | ID), data = div_roosting_95, family = Gamma(link = "log"))
richesse_m2_95 <- glmer(richesse_st ~ sex + age + timeofday + (1 | ID), data = div_roosting_95, family = Gamma(link = "log"))
richesse_m3_95 <- glmer(richesse_st ~ sex + age + (1 | ID), data = div_roosting_95, family = Gamma(link = "log"))
richesse_m4_95 <- glmer(richesse_st ~ sex + (1 | ID), data = div_roosting_95, family = Gamma(link = "log"))
richesse_m5_95 <- glmer(richesse_st ~ age + (1 | ID), data = div_roosting_95, family = Gamma(link = "log"))
richesse_m6_95 <- glmer(richesse_st ~ sex * age + (1 | ID), data = div_roosting_95, family = Gamma(link = "log"))
richesse_m7_95 <- glmer(richesse_st ~ sex * tide_strength + age * tide_strength + (1 | ID), data = div_roosting_95, family = Gamma(link = "log"))
richesse_m8_95 <- glmer(richesse_st ~ sex * tide_strength + age + (1 | ID), data = div_roosting_95, family = Gamma(link = "log"))
richesse_m9_95 <- glmer(richesse_st ~ sex + age * tide_strength + (1 | ID), data = div_roosting_95, family = Gamma(link = "log"))

AIC(richesse_m1_95, richesse_m2_95, richesse_m4_95, richesse_m5_95, richesse_m6_95, richesse_m7_95, richesse_m8_95, richesse_m9_95)

summary(richesse_m7_95)

# predictions plot

# Créer un jeu de données pour les prédictions
new_data_richesse_95 <- expand.grid(
  sex = c("femelle", "mâle"),
  tide_strength = c("mortes eaux", "vives eaux", "submersion"),
  age = c("adulte", "juvénile")
)

# S'assurer que les niveaux correspondent à ceux du modèle
new_data_richesse_95$sex <- factor(new_data_richesse_95$sex, levels = c("femelle", "mâle"))
new_data_richesse_95$tide_strength <- factor(new_data_richesse_95$tide_strength, levels = c("mortes eaux", "vives eaux", "submersion"))
new_data_richesse_95$age <- factor(new_data_richesse_95$age, levels = c("adulte", "juvénile"))

# Ajouter une colonne ID fictive (nécessaire pour le modèle)
new_data_richesse_95$ID <- "prediction"

# Calculer les prédictions et les erreurs standards
predictions <- predict(richesse_m7_95, newdata = new_data_richesse_95, re.form = NA, type = "response", se.fit = TRUE)
new_data_richesse_95$pred <- predictions$fit
new_data_richesse_95$se <- predictions$se.fit

# Calculer les intervalles de confiance à 95%
new_data_richesse_95$lower <- new_data_richesse_95$pred - 1.96 * new_data_richesse_95$se
new_data_richesse_95$upper <- new_data_richesse_95$pred + 1.96 * new_data_richesse_95$se

pred_richesse_95_plot <- ggplot(new_data_richesse_95, aes(x = tide_strength, y = pred, shape = age, color = sex)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0, position = position_dodge(width = 0.5)) +
  labs(
    title = "d)",
    x = "Intensité de marée",
    y = "Prédiction de richesse\nen reposoirs secondaires",
    color = "Sexe",
    shape = "Âge"
  ) +
  geom_jitter(size = 3, position = position_dodge(width = 0.5), fill = "white") +
  scale_color_manual(values = c("#FF00E6", "#49B6FF")) +
  scale_shape_manual(values = c(19, 21)) +
  theme_classic()
pred_richesse_95_plot

# shannon__________

hist(div_roosting_95$shannon)

# Modèle linéaire pour tester l'effet du age sur la distance
shannon_gaussien <- lm(shannon ~ 1, data = div_roosting_95)

# diag
sim <- simulateResiduals(fittedModel = shannon_gaussien, plot = F)
# residuals(sim)
# residuals(sim, quantileFunction = qnorm, outlierValues = c(-7,7))
residuals_2 <- plot(sim)
testDispersion(sim)
testOutliers(sim)

summary(shannon_gaussien)

# selection de modèle
shannon_m1_95 <- lmer(shannon ~ sex + age + timeofday + tide_strength + (1 | ID), data = div_roosting_95)
shannon_m2_95 <- lmer(shannon ~ sex + age + timeofday + (1 | ID), data = div_roosting_95)
shannon_m3_95 <- lmer(shannon ~ sex + age + (1 | ID), data = div_roosting_95)
shannon_m4_95 <- lmer(shannon ~ sex + (1 | ID), data = div_roosting_95)
shannon_m5_95 <- lmer(shannon ~ age + (1 | ID), data = div_roosting_95)
shannon_m6_95 <- lmer(shannon ~ sex * age + (1 | ID), data = div_roosting_95)
shannon_m7_95 <- lmer(shannon ~ sex * tide_strength + age * tide_strength + (1 | ID), data = div_roosting_95)
shannon_m8_95 <- lmer(shannon ~ sex * tide_strength + age + (1 | ID), data = div_roosting_95)
shannon_m9_95 <- lmer(shannon ~ sex + age * tide_strength + (1 | ID), data = div_roosting_95)

AIC(shannon_m1_95, shannon_m2_95, shannon_m3_95, shannon_m4_95, shannon_m5_95, shannon_m6_95, shannon_m7_95, shannon_m8_95, shannon_m9_95)

summary(shannon_m8_95)

# predictions plot

# Créer un jeu de données pour les prédictions
new_data_shannon_95 <- expand.grid(
  sex = c("femelle", "mâle"),
  tide_strength = c("mortes eaux", "vives eaux", "submersion"),
  age = c("adulte", "juvénile")
)

# S'assurer que les niveaux correspondent à ceux du modèle
new_data_shannon_95$sex <- factor(new_data_shannon_95$sex, levels = c("femelle", "mâle"))
new_data_shannon_95$tide_strength <- factor(new_data_shannon_95$tide_strength, levels = c("mortes eaux", "vives eaux", "submersion"))
new_data_shannon_95$age <- factor(new_data_shannon_95$age, levels = c("adulte", "juvénile"))

# Ajouter une colonne ID fictive (nécessaire pour le modèle)
new_data_shannon_95$ID <- "prediction"

# Calculer les prédictions et les erreurs standards
predictions <- predict(shannon_m8_95, newdata = new_data_shannon_95, re.form = NA, type = "response", se.fit = TRUE)
new_data_shannon_95$pred <- predictions$fit
new_data_shannon_95$se <- predictions$se.fit

# Calculer les intervalles de confiance à 95%
new_data_shannon_95$lower <- new_data_shannon_95$pred - 1.96 * new_data_shannon_95$se
new_data_shannon_95$upper <- new_data_shannon_95$pred + 1.96 * new_data_shannon_95$se

pred_shannon_95_plot <- ggplot(new_data_shannon_95, aes(x = tide_strength, y = pred, shape = age, color = sex)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0, position = position_dodge(width = 0.5)) +
  labs(
    title = "e)",
    x = "Intensité de marée",
    y = "Prédiction indice de Shannon\nen reposoirs secondaires",
    color = "Sexe",
    shape = "Âge"
  ) +
  geom_jitter(size = 3, position = position_dodge(width = 0.5), fill = "white") +
  scale_color_manual(values = c("#FF00E6", "#49B6FF")) +
  scale_shape_manual(values = c(19, 21)) +
  theme_classic()
pred_shannon_95_plot

# variation_taux__________

hist(div_roosting_95$variation_taux)

# Modèle linéaire pour tester l'effet du age sur la distance
variation_taux_gaussien <- lm(variation_taux ~ 1, data = div_roosting_95)
variation_taux_Aetareg <- betareg::betareg(variation_taux ~ 1, data = div_roosting_95, link = "logit")

AIC(variation_taux_gaussien, variation_taux_Aetareg)

# library(statmod)

# diag
# sim <- simulateResiduals(fittedModel = shannon_Aetareg, plot = F)
# residuals(sim)
# residuals(sim, quantileFunction = qnorm, outlierValues = c(-7,7))
# residuals_2 <- plot(sim)
# testDispersion(sim)
# testOutliers(sim)

# selection de modèle
variation_taux_m1_95 <- lmer(variation_taux ~ sex + age + timeofday + tide_strength + (1 | ID), data = div_roosting_95)
variation_taux_m2_95 <- lmer(variation_taux ~ sex + age + timeofday + (1 | ID), data = div_roosting_95)
variation_taux_m3_95 <- lmer(variation_taux ~ sex + age + (1 | ID), data = div_roosting_95)
variation_taux_m4_95 <- lmer(variation_taux ~ sex + (1 | ID), data = div_roosting_95)
variation_taux_m5_95 <- lmer(variation_taux ~ age + (1 | ID), data = div_roosting_95)
variation_taux_m6_95 <- lmer(variation_taux ~ sex * age + (1 | ID), data = div_roosting_95)
variation_taux_m7_95 <- lmer(variation_taux ~ sex * tide_strength + age * tide_strength + (1 | ID), data = div_roosting_95)
variation_taux_m8_95 <- lmer(variation_taux ~ sex * tide_strength + age + (1 | ID), data = div_roosting_95)
variation_taux_m9_95 <- lmer(variation_taux ~ sex + age * tide_strength + (1 | ID), data = div_roosting_95)

AIC(
  variation_taux_m1_95, variation_taux_m2_95, variation_taux_m3_95, variation_taux_m4_95, variation_taux_m5_95,
  variation_taux_m6_95, variation_taux_m7_95, variation_taux_m8_95, variation_taux_m9_95
)

summary(variation_taux_m5_95)

# predictions plot

# Créer un jeu de données pour les prédictions
new_data_variation_taux_95 <- expand.grid(
  # sex = c("femelle", "mâle"),
  # tide_strength = c("marée de mortes eaux","marée de vives eaux", "submersion"),
  age = c("adulte", "juvénile")
)

# S'assurer que les niveaux correspondent à ceux du modèle
# new_data_variation_taux_95$sex <- factor(new_data_variation_taux_95$sex, levels = c("femelle", "mâle"))
# new_data_variation_taux_95$tide_strength <- factor(new_data_variation_taux_95$tide_strength, levels = c("marée de mortes eaux","marée de vives eaux", "submersion"))
new_data_variation_taux_95$age <- factor(new_data_variation_taux_95$age, levels = c("adulte", "juvénile"))

# Ajouter une colonne ID fictive (nécessaire pour le modèle)
new_data_variation_taux_95$ID <- "prediction"

# Calculer les prédictions et les erreurs standards
predictions <- predict(variation_taux_m5_95, newdata = new_data_variation_taux_95, re.form = NA, type = "response", se.fit = TRUE)
new_data_variation_taux_95$pred <- predictions$fit
new_data_variation_taux_95$se <- predictions$se.fit

# Calculer les intervalles de confiance à 95%
new_data_variation_taux_95$lower <- new_data_variation_taux_95$pred - 1.96 * new_data_variation_taux_95$se
new_data_variation_taux_95$upper <- new_data_variation_taux_95$pred + 1.96 * new_data_variation_taux_95$se

pred_variation_taux_95_plot <- ggplot(new_data_variation_taux_95, aes(x = age, y = pred, shape = age)) +
  geom_jitter(size = 3, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0, position = position_dodge(width = 0.5)) +
  labs(
    title = "f)",
    x = "Intensité de marée",
    y = "Prédiction du taux de variation \nen reposoirs secondaires",
    shape = "Âge",
  ) +
  geom_jitter(size = 3, position = position_dodge(width = 0.5), fill = "white") +
  scale_shape_manual(values = c(19, 21)) +
  theme_classic() +
  theme(legend.position = "none")
pred_variation_taux_95_plot

new_data_variation_taux_95$level <- "vertice 95%"
new_data_variation_taux_50$level <- "vertice 50%"

new_data_variation_taux_50_95 <- rbind(new_data_variation_taux_95, new_data_variation_taux_50)

pred_variation_taux_50_95_plot <- ggplot(new_data_variation_taux_50_95, aes(x = age, y = pred, shape = age)) +
  geom_jitter(size = 3, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0, position = position_dodge(width = 0.5)) +
  labs(
    title = "b)",
    x = "Intensité de marée",
    y = "Prédiction du taux de variation en reposoirs",
    shape = "Âge",
  ) +
  facet_grid(. ~ level) +
  geom_jitter(size = 3, position = position_dodge(width = 0.5), fill = "white") +
  scale_shape_manual(values = c(19, 21)) +
  theme_classic() +
  theme(legend.position = "none")
pred_variation_taux_50_95_plot

# equitabilité__________

hist(div_roosting_95$equitabilite)

# Modèle linéaire pour tester l'effet du age sur la distance
equitabilite_gaussien <- lm(equitabilite ~ 1, data = div_roosting_95)
# equitabilite_gamma <- glm(equitabilite ~ 1, data = div_roosting_95, family = Gamma(link = "log"))
# equitabilite_log <- lm(log(equitabilite) ~ 1, data = div_roosting_95)

# AIC(equitabilite_gaussien, equitabilite_gamma, equitabilite_log)

# diag
sim <- simulateResiduals(fittedModel = equitabilite_gaussien, plot = F)
# residuals(sim)
# residuals(sim, quantileFunction = qnorm, outlierValues = c(-7,7))
residuals_2 <- plot(sim)
testDispersion(sim)
testOutliers(sim)

summary(equitabilite_gaussien)

# selection de modèle
equitabilite_m1_95 <- lmer(equitabilite ~ sex + age + timeofday + tide_strength + (1 | ID), data = div_roosting_95)
equitabilite_m2_95 <- lmer(equitabilite ~ sex + age + timeofday + (1 | ID), data = div_roosting_95)
equitabilite_m3_95 <- lmer(equitabilite ~ sex + age + (1 | ID), data = div_roosting_95)
equitabilite_m4_95 <- lmer(equitabilite ~ sex + (1 | ID), data = div_roosting_95)
equitabilite_m5_95 <- lmer(equitabilite ~ age + (1 | ID), data = div_roosting_95)
equitabilite_m6_95 <- lmer(equitabilite ~ sex * age + (1 | ID), data = div_roosting_95)
equitabilite_m7_95 <- lmer(equitabilite ~ sex * tide_strength + age * tide_strength + (1 | ID), data = div_roosting_95)
equitabilite_m8_95 <- lmer(equitabilite ~ sex * tide_strength + age + (1 | ID), data = div_roosting_95)
equitabilite_m9_95 <- lmer(equitabilite ~ sex + age * tide_strength + (1 | ID), data = div_roosting_95)

AIC(equitabilite_m1_95, equitabilite_m2_95, equitabilite_m3_95, equitabilite_m4_95, equitabilite_m5_95, equitabilite_m6_95, equitabilite_m7_95, equitabilite_m8_95, equitabilite_m9_95)

summary(equitabilite_m4_95)

# predictions plot

# Créer un jeu de données pour les prédictions
new_data_equitabilite_95 <- expand.grid(
  sex = c("femelle", "mâle"),
  tide_strength = c("mortes eaux", "vives eaux", "submersion"),
  age = c("adulte", "juvénile")
)

# S'assurer que les niveaux correspondent à ceux du modèle
new_data_equitabilite_95$sex <- factor(new_data_equitabilite_95$sex, levels = c("femelle", "mâle"))
new_data_equitabilite_95$tide_strength <- factor(new_data_equitabilite_95$tide_strength, levels = c("mortes eaux", "vives eaux", "submersion"))
new_data_equitabilite_95$age <- factor(new_data_equitabilite_95$age, levels = c("adulte", "juvénile"))

# Ajouter une colonne ID fictive (nécessaire pour le modèle)
new_data_equitabilite_95$ID <- "prediction"

# Calculer les prédictions et les erreurs standards
predictions <- predict(equitabilite_m4_95, newdata = new_data_equitabilite_95, re.form = NA, type = "response", se.fit = TRUE)
new_data_equitabilite_95$pred <- predictions$fit
new_data_equitabilite_95$se <- predictions$se.fit

# Calculer les intervalles de confiance à 95%
new_data_equitabilite_95$lower <- new_data_equitabilite_95$pred - 1.96 * new_data_equitabilite_95$se
new_data_equitabilite_95$upper <- new_data_equitabilite_95$pred + 1.96 * new_data_equitabilite_95$se

pred_equitabilite_95_plot <- ggplot(new_data_equitabilite_95, aes(x = tide_strength, y = pred, color = sex, shape = age)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0, position = position_dodge(width = 0.5)) +
  labs(
    title = "a)",
    x = "Intensité de marée",
    y = "Prédiction de equitabilite\nen reposoirs secondaires",
    color = "Sexe",
    shape = "Âge"
  ) +
  geom_jitter(size = 3, position = position_dodge(width = 0.5), fill = "white") +
  scale_color_manual(values = c("#FF00E6", "#49B6FF")) +
  scale_shape_manual(values = c(19, 21)) +
  theme_classic()
pred_equitabilite_95_plot

new_data_equitabilite_95$level <- "vertice 95%"
new_data_equitabilite_50$level <- "vertice 50%"

new_data_equitabilite_50_95 <- rbind(new_data_equitabilite_95, new_data_equitabilite_50)

pred_equitabilite_50_95_plot <- ggplot(new_data_equitabilite_50_95, aes(x = tide_strength, y = pred, color = sex, shape = age)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0, position = position_dodge(width = 0.5)) +
  labs(
    title = "a)",
    x = "Intensité de marée",
    y = "Prédiction d'équitabilité en reposoirs",
    color = "Sexe",
    shape = "Âge"
  ) +
  facet_wrap(. ~ level) +
  geom_jitter(size = 3, position = position_dodge(width = 0.5), fill = "white") +
  scale_color_manual(values = c("#FF00E6", "#49B6FF")) +
  scale_shape_manual(values = c(19, 21)) +
  theme_classic() +
  theme(legend.position = c(.85, .24))
pred_equitabilite_50_95_plot

# save ---
pred_50_95_plot <- grid.arrange(pred_equitabilite_50_plot, pred_variation_taux_50_plot, nrow = 1)

ggsave(paste0(atlas_path, "/pred_50_95_plot.png"),
  plot = pred_50_95_plot, width = 10, height = 5, dpi = 300
)

# _____________________________________________________________________________________________________________________________________
# _____________________________________________________________________________________________________________________________________
# 13. Connectivité des reposoirs -----------------------------------------------
# _____________________________________________________________________________________________________________________________________
# _____________________________________________________________________________________________________________________________________

GPS_roosting_where <- st_read(file.path(data_generated_path, "GPS_roosting_where.gpkg"))

roosting_poly <- st_read(file.path(data_generated_path, "roosting_poly.gpkg"))

# reposoirs 50%_________________________________________________________________ 

roosting_poly_50 <- roosting_poly %>%
  filter(level == "50", ) %>%
  rename(where = ID_roosting) %>%
  dplyr::select(where, ZOOM, level)

roosting_centroid_50 <- roosting_poly_50 %>%
  mutate(centroid = st_centroid(geom))

network_dt_50 <- GPS_roosting_where %>%
  st_drop_geometry() %>%
  dplyr::select(ID, datetime, where) %>%
  left_join(roosting_centroid_50) %>%
  na.omit()

connections_50 <- network_dt_50 %>%
  group_by(where) %>%
  summarise(count = n()) %>%
  merge(., ., by = NULL, all = TRUE) %>%
  filter(where.x != where.y) %>%
  mutate(weight = count.x * count.y) %>%
  mutate(weight_st = (weight - min(weight, na.rm = TRUE)) /
    (max(weight, na.rm = TRUE) - min(weight, na.rm = TRUE)))

# Convertir les polygones
network_sf_50 <- st_as_sf(network_dt_50)

# Convertir les centroïdes
centroids_sf_50 <- st_as_sf(network_dt_50$centroid)

centroids_coords_50 <- as.data.frame(st_coordinates(centroids_sf_50))
colnames(centroids_coords_50) <- c("x", "y")
centroids_coords_50$where <- network_dt_50$where
centroids_coords_50 <- centroids_coords_50 %>%
  distinct()

centroids_coords_pour_plot_50 <- st_as_sf(centroids_coords_50, coords = c("x", "y"), crs = 4326)

# Fusionner avec les coordonnées des centroïdes
connections2_50 <- connections_50 %>%
  left_join(centroids_coords_50, by = c("where.x" = "where")) %>%
  rename(x_start = x, y_start = y) %>%
  na.omit()

connections3_50 <- connections2_50 %>%
  left_join(centroids_coords_50, by = c("where.y" = "where")) %>%
  rename(x_end = x, y_end = y) %>%
  na.omit()

connections4_50 <- connections3_50 %>%
  filter(weight_st >= quantile(weight_st, 0))

# Déterminer l'emprise géographique à partir de ton polygone
bbox <- st_bbox(roosting_poly)

gc()

# Télécharger le fond de carte satellite Esri (World Imagery)
esri_sat <- get_tiles(
  roosting_poly, # zone d'étude
  provider = "Esri.WorldImagery", # fond satellite
  zoom = 12 # ajuste selon la taille de ta zone
)

network_plot_1_50 <- ggplot() +
  layer_spatial(esri_sat) +
  geom_sf(data = RMO, color = "darkgreen", fill = "darkgreen", size = 0, alpha = 0.5) +
  geom_sf(data = roosting_poly_50, fill = "black", alpha = 1) +
  geom_sf(data = centroids_coords_pour_plot_50, color = "black", size = 10) +
  geom_segment(
    data = connections4_50,
    aes(
      x = x_start, y = y_start,
      xend = x_end, yend = y_end,
      size = weight_st,
      color = weight_st
    ),
    arrow = arrow(length = unit(0.3, "cm")),
    alpha = 0.5
  ) +
  scale_size(range = c(0.05, 2)) +
  scale_color_gradient2(low = "white", mid = "#49B6FF", high = "#FF00E6", midpoint = 0.5) +
  theme_minimal() +
  theme(
    legend.position = c(0.16, 0.34),
    legend.background = element_rect(fill = "white", color = "white")
  ) +
  labs(
    title = "",
    x = "Longitude", y = "Latitude",
    size = "Connexion", color = "Connexion"
  )

ggsave(paste0(atlas_path, "/network_plot_50.png"), plot = network_plot_1_50, width = 7, height = 7, dpi = 300)

# all reposoirs_________________________________________________________________

roosting_poly_all_quantile <- roosting_poly %>%
  rename(where = ID_roosting) %>%
  dplyr::select(where, ZOOM, level)

roosting_poly_all_quantile$level[roosting_poly_all_quantile$level == 95] <- "reposoirs secondaires (95%)"
roosting_poly_all_quantile$level[roosting_poly_all_quantile$level == 50] <- "reposoirs principaux (50%)"

roosting_centroid_all_quantile <- roosting_poly_all_quantile %>%
  mutate(centroid = st_centroid(geom))

network_dt_all_quantile <- GPS_roosting_where %>%
  st_drop_geometry() %>%
  dplyr::select(ID, datetime, where) %>%
  left_join(roosting_centroid_all_quantile) %>%
  na.omit()

connections_all_quantile <- network_dt_all_quantile %>%
  group_by(where) %>%
  summarise(count = n()) %>%
  merge(., ., by = NULL, all = TRUE) %>%
  filter(where.x != where.y) %>%
  mutate(weight = count.x * count.y) %>%
  mutate(weight_st = (weight - min(weight, na.rm = TRUE)) /
    (max(weight, na.rm = TRUE) - min(weight, na.rm = TRUE)))

# Convertir les polygones
network_sf_all_quantile <- st_as_sf(network_dt_all_quantile)

# Convertir les centroïdes
centroids_sf_all_quantile <- st_as_sf(network_dt_all_quantile$centroid)

centroids_coords_all_quantile <- as.data.frame(st_coordinates(centroids_sf_all_quantile))
colnames(centroids_coords_all_quantile) <- c("x", "y")
centroids_coords_all_quantile$where <- network_dt_all_quantile$where
centroids_coords_all_quantile <- centroids_coords_all_quantile %>%
  distinct()

centroids_coords_pour_plot_all_quantile <- st_as_sf(centroids_coords_all_quantile, coords = c("x", "y"), crs = 4326)

# Fusionner avec les coordonnées des centroïdes
connections2_all_quantile <- connections_all_quantile %>%
  left_join(centroids_coords_all_quantile, by = c("where.x" = "where")) %>%
  rename(x_start = x, y_start = y) %>%
  na.omit()

connections3_all_quantile <- connections2_all_quantile %>%
  left_join(centroids_coords_all_quantile, by = c("where.y" = "where")) %>%
  rename(x_end = x, y_end = y) %>%
  na.omit()

connections4_all_quantile <- connections3_all_quantile %>%
  filter(weight_st >= quantile(weight_st, 0.95))

# Déterminer l'emprise géographique à partir de ton polygone
bbox <- st_bbox(roosting_poly)

# Télécharger le fond de carte satellite Esri (World Imagery)
esri_sat <- get_tiles(
  roosting_poly, # zone d'étude
  provider = "Esri.WorldImagery", # fond satellite
  zoom = 12 # ajuste selon la taille de ta zone
)

# Créer ton graphique
network_plot_1_all_quantile <- ggplot() +
  # Ajouter le fond satellite
  layer_spatial(esri_sat) +
  # RMO
  geom_sf(data = RMO, color = "darkgreen", fill = "darkgreen", size = 0, alpha = 0.5) +
  # Polygones et centroïdes
  geom_sf(data = roosting_poly_all_quantile, aes(fill = as.factor(level)), alpha = 1) +
  geom_sf(data = centroids_coords_pour_plot_all_quantile, color = "black", size = 10) +
  # Liens entre centroïdes
  geom_segment(
    data = connections4_all_quantile,
    aes(
      x = x_start, y = y_start,
      xend = x_end, yend = y_end,
      size = weight_st,
      color = weight_st,
    ),
    arrow = arrow(length = unit(0.3, "cm")),
    alpha = 0.5
  ) +
  # Thème et échelles
  scale_size(range = c(0.05, 2)) +
  scale_fill_manual(values = c("black", "grey")) +
  scale_color_gradient2(low = "white", mid = "#49B6FF", high = "#FF00E6", midpoint = 0.5) +
  theme_minimal() +
  theme(
    legend.position = c(0.25, 0.4),
    legend.background = element_rect(fill = "white", color = "white")
  ) +
  labs(
    title = "",
    x = "Longitude", y = "Latitude",
    size = "Connexion", color = "Connexion", fill = "Reposoirs"
  )

ggsave(paste0(atlas_path, "/network_plot_all_quantile.png"), plot = network_plot_1_all_quantile, width = 7, height = 7, dpi = 300)

# _____________________________________________________________________________________________________________________________________
# _____________________________________________________________________________________________________________________________________
# 14. Chasse à pied ----
# _____________________________________________________________________________________________________________________________________
# _____________________________________________________________________________________________________________________________________

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

GPS <- st_transform(GPS, crs = 4326)

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

grid_ZOOM_A <- st_read(paste0(data_generated_path, "grid_ZOOM_A.gpkg"))

grid_chasse <- st_intersection(grid_ZOOM_A, chasse_buffer)

raster_chasse <- rast(grid_chasse, resolution = resolution_ZOOM, crs = "EPSG:2154")

# tmap_mode("view")
# map_chasse <- tm_scalebar() +
#   tm_shape(grid_chasse) +
#   tm_polygons(col = "blue") +
#   tm_shape(chasse2[1, ]) + # le point DPM
#   tm_dots(col = "red") ;
#   # tm_shape(terre_mer) +
#   # tm_lines(col = "lightblue", lwd = 0.1)
#   map_chasse

# période ---

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

GPS_période_chasse <- tt %>%
  mutate(
    md = format(y_m_d, "%m-%d"), # extraire mois-jour
    période = case_when(
      md >= debut_md & md < fermeture_md ~ "saison de chasse", # saison de chasse
      md >= fermeture_md | md < fin_md ~ "hors saison de chasse" # hors saison
    )
  )

table(GPS_période_chasse$période)
table(GPS_période_chasse$month_numeric[GPS_période_chasse$période == "saison de chasse"])

# length(tt$datetime[tt$month_numeric == 11])
#
# unique(GPS_période_chasse$month_label[GPS_période_chasse$période %in% c("in", "out")])
#
# table(tt$month_numeric)

tt$ouverture_fermeture <- as.Date(paste0(as.character(year(tt$date)), date_fin_chasse))
tt$debut_in_chasse <- tt$ouverture_fermeture - 15
tt$fin_out_chasse <- tt$ouverture_fermeture + 15

GPS_période_chasse <- tt %>%
  mutate(période = case_when(
    between(y_m_d, debut_in_chasse, ouverture_fermeture) ~ "saison de chasse",
    between(y_m_d, ouverture_fermeture, fin_out_chasse) ~ "hors saison de chasse"
  ))

table(GPS_période_chasse$période, useNA = "always")

GPS_période_chasse <- GPS_période_chasse %>%
  filter(!is.na(période))

#### roosting ---

# 15/09/2025 ---

GPS_période_chasse_roosting <- GPS_période_chasse %>%
  filter(behavior == "roosting")

GPS_période_chasse_foraging <- GPS_période_chasse %>%
  filter(behavior == "foraging")

GPS_sampled <- sample_weighted_points(
  data = GPS_période_chasse_roosting,
  n = 1000,
  variable = "période",
  zone = "zone",
  cap = 3600
)

# GPS_sampled %>%
#   group_by(ID, zone, période) %>%
#   summarise(n_points = n(), .groups = "drop") %>%
#   arrange(ID, zone, période)

GPS_sampled <- st_as_sf(GPS_sampled, coords = c("lon", "lat"), crs = 4326) %>%
  mutate(lon = st_coordinates(.)[, 1], lat = st_coordinates(.)[, 2])

zoom_levels <- c("B")
results_kud <- NULL
nb_kud <- NULL
analyse <- "kud"
comportement <- "roosting"
couleurs <- c("yellow", couleur_roosting)
variable <- "période"

plan(multisession, workers = 1)

results_list <- future_lapply(
  zoom_levels,
  function(z) {
    make_kud_variable(analyse, z, comportement, GPS_sampled, data_generated_path, resolution_ZOOM, couleurs, variable)
  },
  future.seed = TRUE # garantit des tirages aléatoires reproductibles et indépendants
)

#### foraging ---

GPS_sampled <- sample_weighted_points(
  data = GPS_période_chasse_foraging,
  n = 1000,
  variable = "période",
  zone = "zone",
  cap = 3600
)

GPS_sampled <- st_as_sf(GPS_sampled, coords = c("lon", "lat"), crs = 4326) %>%
  mutate(lon = st_coordinates(.)[, 1], lat = st_coordinates(.)[, 2])

zoom_levels <- c("B")
results_kud <- NULL
nb_kud <- NULL
analyse <- "kud"
comportement <- "foraging"
couleurs <- c("yellow", couleur_foraging)
variable <- "période"

plan(multisession, workers = 1)

results_list <- future_lapply(
  zoom_levels,
  function(z) {
    make_kud_variable(analyse, z, comportement, GPS_sampled, data_generated_path, resolution_ZOOM, couleurs, variable)
  },
  future.seed = TRUE # garantit des tirages aléatoires reproductibles et indépendants
)

# _____________________________________________________________________________________________________________________________________
# _____________________________________________________________________________________________________________________________________
# 15. Tonnes de chasse ----
# _____________________________________________________________________________________________________________________________________
# _____________________________________________________________________________________________________________________________________

tonnes <- st_read(paste0(data_path, "Tonnes_de_chasse/tonnes.shp"))

tonnes <- st_intersection(tonnes, BOX_2154)

# créer zone de danger de 300m
# créer zone de proximité de 1 km
# selectionner les point GPS dans les zones de danger et proximité
# garder les ind que avec assez de point dans les deux zones
# proportion danger/proximité ~ week par ind
# voir si plus bas pendant la période de chasse

tonnes_danger <- tonnes %>%
  st_buffer(dist = 300)
tonnes_500 <- tonnes %>%
  st_buffer(dist = 500)
tonnes_1000 <- tonnes %>%
  st_buffer(dist = 1000)
tonnes_proxi <- tonnes %>%
  st_buffer(dist = 1500)

tonnes_danger_unioned <- st_union(tonnes_danger)
tonnes_500_unioned <- st_union(tonnes_500)
tonnes_1000_unioned <- st_union(tonnes_1000)
tonnes_proxi_unioned <- st_union(tonnes_proxi)

# area_danger <- as.numeric(st_area(tonnes_danger_unioned)) / 1000000
# area_proxi <- as.numeric(st_area(tonnes_proxi_unioned)) / 1000000

# maps
tmap_mode("view")
map_tonnes_v0 <- tm_scalebar() +
  tm_basemap(c("Esri.WorldImagery", "OpenStreetMap", "CartoDB.Positron")) +
  # tm_shape(terre_mer) +
  # tm_lines(col = "#32B7FF", lwd = 0.5) +
  # tm_shape(tonnes_proxi_unioned) +
  # tm_polygons(fill = "#FFF07C", alpha = 0.5) +
  # tm_shape(tonnes_danger_unioned) +
  # tm_polygons(fill = "darkred", alpha = 0.7) +
  tm_shape(tonnes) +
  tm_dots(fill = "black") +
  tm_layout(title = "") +
  tm_shape(site_baguage) +
  tm_text("icone", size = 1.5)
map_tonnes_v0

tmap_save(map_tonnes_v0, paste0(atlas_path, "map_tonnes_v0.html"))


# superposition____________________

# tonnes ---

tonnes <- st_read(paste0(data_path, "Tonnes_de_chasse/tonnes.shp"))

tonnes <- st_intersection(tonnes, BOX_2154)

tonnes_Auffer <- tonnes %>%
  st_buffer(dist = 300)
tonnes_unioned <- st_union(tonnes_Auffer)
tonnes_cut_zones <- st_intersection(tonnes_unioned, tonnes_Auffer)
tonnes_zones_final <- tonnes_cut_zones %>%
  st_sf() %>%
  mutate(overlap_count = lengths(st_intersects(geometry, tonnes_Auffer))) %>%
  filter(overlap_count >= 1)
tonnes_zones_grouped <- tonnes_zones_final %>%
  group_by(overlap_count) %>%
  summarise(geometry = st_union(geometry), .groups = "drop")

# histogram
hist(tonnes_zones_grouped$overlap_count)

tonnes_zones_grouped_clean <- tonnes_zones_grouped[!is.na(tonnes_zones_grouped$overlap_count), ]

# maps
tmap_mode("view")
map_tonnes_superposition <- tm_scalebar() +
  # tm_shape(terre_mer) +
  # tm_lines(col = "#32B7FF", lwd = 0.5) +
  tm_shape(tonnes_zones_grouped_clean) +
  tm_basemap(c("Esri.WorldImagery", "OpenStreetMap", "CartoDB.Positron")) +
  tm_polygons(
    fill = "overlap_count",
    palette = c("white", "yellow", "#FF00E6", "red"),
    style = "cont", alpha = 0.5,
    title = "Nb superposées"
  ) +
  tm_shape(RMO) +
  tm_polygons(col = "darkgreen", fill_alpha = 0, col_Blpha = 1, lwd = 2)  +
  tm_shape(tonnes) +
  tm_dots(fill = "black") +
  tm_layout(title = "Superposition des tonnes de chasse (300 m de rayon)") +
  tm_shape(site_baguage) +
  tm_text("icone", size = 1.5) ; map_tonnes_superposition

tmap_save(map_tonnes_superposition, paste0(atlas_path, "map_tonnes_superposition.html"))

# intersection avec la réserve ---

# Buffers 300 m
buf_300 <- tonnes %>%
  st_buffer(dist = 300) %>%
  mutate(intersect_reserve = lengths(st_intersects(., RMO)) > 0) %>%
  summarise(
    geometry = st_union(geometry),
    intersect_reserve = any(intersect_reserve),
    .groups = "drop"
  ) %>%
  mutate(couleur = ifelse(intersect_reserve, "300 m (intersecte)", NA))

# Buffers 500 m
buf_500 <- tonnes %>%
  st_buffer(dist = 500) %>%
  mutate(intersect_reserve = lengths(st_intersects(., RMO)) > 0) %>%
  summarise(
    geometry = st_union(geometry),
    intersect_reserve = any(intersect_reserve),
    .groups = "drop"
  ) %>%
  mutate(couleur = ifelse(intersect_reserve, "500 m (intersecte)", NA))

# Buffers 1000 m
buf_1000 <- tonnes %>%
  st_buffer(dist = 1000) %>%
  mutate(intersect_reserve = lengths(st_intersects(., RMO)) > 0) %>%
  summarise(
    geometry = st_union(geometry),
    intersect_reserve = any(intersect_reserve),
    .groups = "drop"
  ) %>%
  mutate(couleur = ifelse(intersect_reserve, "1000 m (intersecte)", NA))

# Points tonnes, taille uniforme et couleur selon intersection
tonnes_colores <- tonnes %>%
  mutate(
    rayon_intersect = sapply(1:nrow(tonnes), function(i) {
      intersect_r <- sapply(c(300, 500, 1000), function(r) {
        buf <- tonnes[i, ] %>% st_buffer(dist = r)
        any(st_intersects(buf, RMO, sparse = FALSE))
      })
      if (any(intersect_r)) c(300, 500, 1000)[which(intersect_r)[1]] else NA
    }),
    couleur = factor(case_when(
      rayon_intersect == 300 ~ "300 m",
      rayon_intersect == 500 ~ "500 m",
      rayon_intersect == 1000 ~ "1000 m",
      TRUE ~ "Pas d'intersection"
    ), levels = c("300 m", "500 m", "1000 m", "Pas d'intersection"))
  )

# Carte interactive
# D'abord, créez une variable binaire pour la taille
tonnes_colores <- tonnes_colores %>%
  mutate(taille_cat = if_else(couleur == "Pas d'intersection",
    "Pas d'intersection",
    "Intersection"
  ))

# Puis dans tmap :
tmap_mode("view")
intersection_tonne_map <- tm_basemap(c("Esri.WorldImagery", "OpenStreetMap", "CartoDB.Positron")) +
  # Chaque buffer séparé avec alpha sur les bordures
  tm_shape(buf_1000) +
  tm_polygons(col = NULL, fill = "darkgrey", fill_alpha = 0.4, col_Blpha = 0.7, lwd = 2) +
  tm_shape(buf_500) +
  tm_polygons(col = NULL, fill = "yellow", fill_alpha = 0.3, col_Blpha = 0.7, lwd = 2) +
  tm_shape(buf_300) +
  tm_polygons(col = NULL, fill = "#FF00E6", fill_alpha = 0.3, col_Blpha = 0.7, lwd = 2) +
  # Points tonnes
  tm_shape(tonnes_colores) +
  tm_dots(
    size = "taille_cat",
    size.scale = tm_scale_categorical(
      values = c("Intersection" = 0.5, "Pas d'intersection" = 0.1),
      labels = c(
        "Intersection" = "Grand (avec intersection)",
        "Pas d'intersection" = "Petit (sans intersection)"
      )
    ),
    size.legend = tm_legend(title = "Taille des points"),
    fill = "couleur",
    fill.scale = tm_scale_categorical(
      values = c(
        "300 m" = "#FF00E6", "500 m" = "yellow",
        "1000 m" = "darkgrey", "Pas d'intersection" = "lightgrey"
      ),
      labels = c(
        "300 m" = "< 300 m",
        "500 m" = "300-500 m",
        "1000 m" = "500-1000 m",
        "Pas d'intersection" = "A distance de la RNN"
      )
    ),
    fill.legend = tm_legend(title = "Distance à la tonne")
  ) +
  # Réserve en contour noir (sans transparence)
  tm_shape(RMO) +
  tm_polygons(col = "darkgreen", fill_alpha = 0, col_Blpha = 1, lwd = 2)  +
  tm_shape(site_baguage) +
  tm_text("icone", size = 1.5) ; intersection_tonne_map

tmap_save(intersection_tonne_map, paste0(atlas_path, "intersection_tonne_map", ".html"))

# _____________________________________________________________________________________________________________________________________
# _____________________________________________________________________________________________________________________________________
# 16. Distance reposoir - alimentation ------------------------------------------
# _____________________________________________________________________________________________________________________________________
# _____________________________________________________________________________________________________________________________________

# --- objectif ---
# estimation de la distance entre reposoir et alimentation
# estimation pour chauqe individu, de jour en jour, à chaque cycle de marée
# en moyenne, et en fonction de variableètres (sexe, age, chasse, ...)
# = estimation des distances inter-centroïdes entre comportements consécutifs (par exemple, de "foraging" à "roosting")

# estimation distance de jour en jour___________________________________________

# Filtrage des données pertinentes (hors comportement "other")
distance_dt_1 <- GPS %>%
  dplyr::select(ID, behavior, datetime, tide_strength, timeofday, month_numeric) %>% # On garde uniquement les colonnes utiles
  filter(behavior != "other") %>% # On exclut les comportements "other"
  distinct() %>% # On retire les doublons éventuels
  na.omit() # On retire les lignes avec NA

distance_dt_3 <- distance_dt_1 %>%
  dplyr::select(ID, behavior, datetime) %>%
  filter(behavior %in% c("foraging", "roosting")) %>%
  arrange(ID, datetime) %>%
  group_by(ID) %>%
  mutate(
    dt_diff = as.numeric(difftime(datetime, lag(datetime), units = "hours")),
    new_run = (behavior != lag(behavior)) | (dt_diff > 6) | is.na(lag(behavior)),
    behavior_run = cumsum(new_run)
  ) %>%
  ungroup()

# Calcul du centroïde pour comportement
distance_dt_4 <- distance_dt_3 %>%
  group_by(behavior_run) %>%
  mutate(centroid = st_centroid(st_union(geometry))) %>% # Centroïde des points du groupe
  dplyr::select(-dt_diff, -new_run) %>%
  st_drop_geometry() %>% # Suppression de la géométrie d'origine
  distinct()

distance_dt_4$ID_run <- paste(distance_dt_4$ID, "_", distance_dt_4$behavior_run)

Freq_distance_dt_4 <- as.data.frame(table(distance_dt_4$ID_run)) %>%
  filter(Freq > 1)

distance_dt_5 <- distance_dt_4 %>%
  arrange(ID, datetime) %>%
  group_by(ID_run) %>%
  mutate(
    mean_date = mean(datetime)
  ) %>%
  dplyr::select(-datetime) %>%
  distinct()

Freq_distance_dt_5 <- as.data.frame(table(distance_dt_5$ID_run)) %>%
  filter(Freq > 1)

pairs_dist <- distance_dt_5 %>%
  arrange(ID, mean_date) %>%
  group_by(ID) %>%
  mutate(
    next_Aehavior   = lead(behavior),
    next_centroid   = lead(centroid),
    next_date       = lead(mean_date)
  ) %>%
  filter(
    # garder seulement les transitions roosting <-> foraging
    (behavior == "roosting" & next_Aehavior == "foraging") |
      (behavior == "foraging" & next_Aehavior == "roosting")
  ) %>%
  mutate(
    time_diff_h = as.numeric(difftime(next_date, mean_date, units = "hours")),
    distance_m  = st_distance(centroid, next_centroid, by_element = TRUE)
  ) %>%
  filter(between(time_diff_h, 4, 8))

mean_dist_ID <- pairs_dist %>%
  group_by(ID) %>%
  summarise(
    mean_dist = mean(distance_m),
    sd_dist = sd(distance_m)
  )

# save ---
write.csv(mean_dist_ID, paste0(data_generated_path, "mean_dist_ID", ".csv"), row.names = FALSE)
mean_dist_ID <- read.csv(paste0(data_generated_path, paste0("mean_dist_ID", ".csv")), row.names = NULL)

# Moyennes globales (tous individus confondus)
mean_dist <- mean(pairs_dist$distance_m) # Moyenne globale
sd_dist <- sd(pairs_dist$distance_m) # Écart-type global

# ~ sexe________________________________________________________________________

# Récupération du sexe des individus (à partir de la table GPS, sans la géométrie)
sexe_dt <- GPS %>%
  st_drop_geometry() %>%
  dplyr::select(ID, sex) %>% # On sélectionne uniquement ID et sexe
  na.omit() %>% # Suppression des lignes avec NA (individus sans info de sexe)
  distinct() # On garde une seule ligne par ID

# Jointure entre les distances calculées et les sexes des individus
paired_centroids_sex_dt <- mean_dist_ID %>%
  left_join(sexe_dt) %>% # Ajout de la colonne "sex" par jointure sur ID
  na.omit() # On supprime les lignes avec NA (par exemple, si le sexe est inconnu)

paired_centroids_sex_dt$mean_dist <- as.numeric(as.character(paired_centroids_sex_dt$mean_dist))

hist(paired_centroids_sex_dt$mean_dist)

# save ---
write.csv(paired_centroids_sex_dt, paste0(data_generated_path, "paired_centroids_sex_dt", ".csv"), row.names = FALSE)
paired_centroids_sex_dt <- read.csv(paste0(data_generated_path, paste0("paired_centroids_sex_dt", ".csv")), row.names = NULL)

# Modèle linéaire pour tester l'effet du sexe sur la distance
m_sex_gaussien <- lm(mean_dist ~ sex, data = paired_centroids_sex_dt)
m_sex_gamma <- glm(mean_dist ~ sex, data = paired_centroids_sex_dt, family = Gamma(link = "log"))

AIC(m_sex_gaussien, m_sex_gamma)
summary(m_sex_gamma)

# diag
sim <- simulateResiduals(fittedModel = m_sex_gamma, plot = F)
# residuals(sim)
# residuals(sim, quantileFunction = qnorm, outlierValues = c(-7,7))
residuals_2 <- plot(sim)
testDispersion(sim)
testOutliers(sim)

boxplot(mean_dist ~ sex,
  data = paired_centroids_sex_dt,
  col = c("lightblue", "lightpink"),
  ylab = "Distance moyenne",
  xlab = "Sexe"
)

# ~ age_________________________________________________________________________

# Récupération du sexe des individus (à partir de la table GPS, sans la géométrie)
age_dt <- GPS %>%
  st_drop_geometry() %>%
  dplyr::select(ID, age) %>% # On sélectionne uniquement ID et age
  na.omit() %>% # Suppression des lignes avec NA (individus sans info de age)
  distinct() # On garde une seule ligne par ID

# Jointure entre les distances calculées et les ages des individus
paired_centroids_age_dt <- mean_dist_ID %>%
  left_join(age_dt) %>% # Ajout de la colonne "age" par jointure sur ID
  na.omit() # On supprime les lignes avec NA (par exemple, si le age est inconnu)

paired_centroids_age_dt$mean_dist <- as.numeric(as.character(paired_centroids_age_dt$mean_dist))

hist(paired_centroids_age_dt$mean_dist)

# save ---
write.csv(paired_centroids_age_dt, paste0(data_generated_path, "paired_centroids_age_dt", ".csv"), row.names = FALSE)
paired_centroids_age_dt <- read.csv(paste0(data_generated_path, paste0("paired_centroids_age_dt", ".csv")), row.names = NULL)

# Modèle linéaire pour tester l'effet du age sur la distance
m_age_gaussien <- lm(mean_dist ~ age, data = paired_centroids_age_dt)
m_age_gamma <- glm(mean_dist ~ age, data = paired_centroids_age_dt, family = Gamma(link = "log"))

AIC(m_age_gaussien, m_age_gamma)
summary(m_age_gamma)

# diag
sim <- simulateResiduals(fittedModel = m_age_gamma, plot = F)
# residuals(sim)
# residuals(sim, quantileFunction = qnorm, outlierValues = c(-7,7))
residuals_2 <- plot(sim)
testDispersion(sim)
testOutliers(sim)

boxplot(mean_dist ~ age,
  data = paired_centroids_age_dt,
  col = c("lightblue", "lightpink"),
  ylab = "Distance moyenne",
  xlab = "age"
)

# ~ sex + age___________________________________________________________________

# Récupération du sexe des individus (à partir de la table GPS, sans la géométrie)
sex_age_dt <- GPS %>%
  st_drop_geometry() %>%
  dplyr::select(ID, sex, age) %>% # On sélectionne uniquement ID et age
  na.omit() %>% # Suppression des lignes avec NA (individus sans info de sex_age)
  distinct() # On garde une seule ligne par ID

# Jointure entre les distances calculées et les sex_ages des individus
paired_centroids_sex_age_dt <- mean_dist_ID %>%
  left_join(sex_age_dt) %>% # Ajout de la colonne "sex_age" par jointure sur ID
  na.omit() # On supprime les lignes avec NA (par exemple, si le sex_age est inconnu)

paired_centroids_sex_age_dt$mean_dist <- as.numeric(as.character(paired_centroids_sex_age_dt$mean_dist))

hist(paired_centroids_sex_age_dt$mean_dist)

# save ---
write.csv(paired_centroids_sex_age_dt, paste0(data_generated_path, "paired_centroids_sex_age_dt", ".csv"), row.names = FALSE)
paired_centroids_sex_age_dt <- read.csv(paste0(data_generated_path, paste0("paired_centroids_sex_age_dt", ".csv")), row.names = NULL)

# Modèle linéaire pour tester l'effet du sex_age sur la distance
m_sex_age_gaussien <- lm(mean_dist ~ sex * age, data = paired_centroids_sex_age_dt)
m_sex_age_gamma <- glm(mean_dist ~ sex * age, data = paired_centroids_sex_age_dt, family = Gamma(link = "log"))

AIC(m_sex_age_gaussien, m_sex_age_gamma)
summary(m_sex_age_gamma)

# diag
sim <- simulateResiduals(fittedModel = m_sex_age_gamma, plot = F)
# residuals(sim)
# residuals(sim, quantileFunction = qnorm, outlierValues = c(-7,7))
residuals_2 <- plot(sim)
testDispersion(sim)
testOutliers(sim)

# ~ tides_high_type_____________________________________________________________

# Filtrage des données pertinentes (hors comportement "other")
distance_tide_dt_1 <- GPS %>%
  dplyr::select(ID, behavior, datetime, tide_strength, timeofday, month_numeric) %>% # On garde uniquement les colonnes utiles
  filter(behavior != "other") %>% # On exclut les comportements "other"
  distinct() %>% # On retire les doublons éventuels
  na.omit() # On retire les lignes avec NA

distance_tide_dt_3 <- distance_tide_dt_1 %>%
  dplyr::select(ID, behavior, datetime, tide_strength) %>%
  filter(behavior %in% c("foraging", "roosting")) %>%
  arrange(ID, datetime) %>%
  group_by(ID) %>%
  mutate(
    dt_diff = as.numeric(difftime(datetime, lag(datetime), units = "hours")),
    new_run = (behavior != lag(behavior)) | (dt_diff > 6) | is.na(lag(behavior)),
    behavior_run = cumsum(new_run)
  ) %>%
  ungroup()

# Calcul du centroïde pour comportement
distance_tide_dt_4 <- distance_tide_dt_3 %>%
  group_by(behavior_run, tide_strength) %>%
  mutate(centroid = st_centroid(st_union(geometry))) %>% # Centroïde des points du groupe
  dplyr::select(-dt_diff, -new_run) %>%
  st_drop_geometry() %>% # Suppression de la géométrie d'origine
  distinct()

distance_tide_dt_4$ID_run <- paste0(distance_tide_dt_4$ID, "_", distance_tide_dt_4$behavior_run)
distance_tide_dt_4$ID_run_tide <- paste0(distance_tide_dt_4$ID, "_", distance_tide_dt_4$behavior_run, "_", distance_tide_dt_4$tide_strength)

Freq_distance_tide_dt_4 <- as.data.frame(table(distance_tide_dt_4$ID_run_tide)) %>%
  filter(Freq > 1)

distance_tide_dt_5 <- distance_tide_dt_4 %>%
  arrange(ID, datetime) %>%
  group_by(ID_run_tide) %>%
  mutate(
    mean_date = mean(datetime)
  ) %>%
  dplyr::select(-datetime) %>%
  distinct()

Freq_distance_tide_dt_5 <- as.data.frame(table(distance_tide_dt_5$ID_run)) %>%
  filter(Freq > 1)

pairs_dist <- distance_tide_dt_5 %>%
  arrange(ID, mean_date) %>%
  group_by(ID) %>%
  mutate(
    next_Aehavior   = lead(behavior),
    next_centroid   = lead(centroid),
    next_date       = lead(mean_date)
  ) %>%
  filter(
    # garder seulement les transitions roosting <-> foraging
    (behavior == "roosting" & next_Aehavior == "foraging") |
      (behavior == "foraging" & next_Aehavior == "roosting")
  ) %>%
  mutate(
    time_diff_h = as.numeric(difftime(next_date, mean_date, units = "hours")),
    distance_m  = st_distance(centroid, next_centroid, by_element = TRUE)
  ) %>%
  filter(between(time_diff_h, 4, 8))

mean_dist_ID <- pairs_dist %>%
  group_by(ID, tide_strength) %>%
  summarise(
    mean_dist = mean(distance_m),
    sd_dist = sd(distance_m)
  )

paired_centroids_tide_dt <- mean_dist_ID

paired_centroids_tide_dt$mean_dist <- as.numeric(as.character(paired_centroids_tide_dt$mean_dist))

hist(paired_centroids_tide_dt$mean_dist)

# save ---
write.csv(paired_centroids_tide_dt, paste0(data_generated_path, "paired_centroids_tide_dt", ".csv"), row.names = FALSE)
paired_centroids_tide_dt <- read.csv(paste0(data_generated_path, paste0("paired_centroids_tide_dt", ".csv")), row.names = NULL)

# Modèle linéaire pour tester l'effet du tide sur la distance
m_tide_gaussien <- lm(mean_dist ~ tide_strength, data = paired_centroids_tide_dt)
m_tide_gamma <- glm(mean_dist ~ tide_strength, data = paired_centroids_tide_dt, family = Gamma(link = "log"))

AIC(m_tide_gaussien, m_tide_gamma)
summary(m_tide_gamma)

# diag
sim <- simulateResiduals(fittedModel = m_tide_gamma, plot = F)
# residuals(sim)
# residuals(sim, quantileFunction = qnorm, outlierValues = c(-7,7))
residuals_2 <- plot(sim)
testDispersion(sim)
testOutliers(sim)

boxplot(mean_dist ~ tide_strength,
  data = paired_centroids_tide_dt,
  col = c("lightblue", "lightpink", "red"),
  ylab = "Distance moyenne",
  xlab = "tide"
)

# ~ chasse______________________________________________________________________

distance_chasse_dt_5 <- distance_dt_5 %>%
  mutate(month = month(mean_date)) %>%
  filter(month %in% c(1, 2))

pairs_dist_chasse <- distance_chasse_dt_5 %>%
  arrange(ID, mean_date) %>%
  group_by(ID) %>%
  mutate(
    next_Aehavior   = lead(behavior),
    next_centroid   = lead(centroid),
    next_date       = lead(mean_date)
  ) %>%
  filter(
    # garder seulement les transitions roosting <-> foraging
    (behavior == "roosting" & next_Aehavior == "foraging") |
      (behavior == "foraging" & next_Aehavior == "roosting")
  ) %>%
  mutate(
    time_diff_h = as.numeric(difftime(next_date, mean_date, units = "hours")),
    distance_m  = st_distance(centroid, next_centroid, by_element = TRUE)
  ) %>%
  filter(between(time_diff_h, 4, 8))

mean_dist_ID <- pairs_dist_chasse %>%
  group_by(ID, month) %>%
  summarise(
    mean_dist = mean(distance_m),
    sd_dist = sd(distance_m)
  )

paired_centroids_chasse_dt <- mean_dist_ID

paired_centroids_chasse_dt$mean_dist <- as.numeric(as.character(paired_centroids_chasse_dt$mean_dist))

hist(paired_centroids_chasse_dt$mean_dist)

# save ---
write.csv(paired_centroids_chasse_dt, paste0(data_generated_path, "paired_centroids_chasse_dt", ".csv"), row.names = FALSE)
paired_centroids_chasse_dt <- read.csv(paste0(data_generated_path, paste0("paired_centroids_chasse_dt", ".csv")), row.names = NULL)

# Modèle linéaire pour tester l'effet du chasse sur la distance
m_chasse_gaussien <- lm(mean_dist ~ month, data = paired_centroids_chasse_dt)
m_chasse_gamma <- glm(mean_dist ~ month, data = paired_centroids_chasse_dt, family = Gamma(link = "log"))

AIC(m_chasse_gaussien, m_chasse_gamma)
summary(m_chasse_gamma)

# diag
sim <- simulateResiduals(fittedModel = m_chasse_gamma, plot = F)
# residuals(sim)
# residuals(sim, quantileFunction = qnorm, outlierValues = c(-7,7))
residuals_2 <- plot(sim)
testDispersion(sim)
testOutliers(sim)

boxplot(mean_dist ~ month,
  data = paired_centroids_chasse_dt,
  col = c("lightblue", "lightpink", "red"),
  ylab = "Distance moyenne",
  xlab = "chasse en janvier (mois)"
)

# ~ all_________________________________________________________________________

# Filtrage des données pertinentes (hors comportement "other")
distance_all_dt_1 <- GPS %>%
  dplyr::select(ID, behavior, datetime, tide_strength, timeofday, month_numeric) %>% # On garde uniquement les colonnes utiles
  filter(behavior != "other") %>% # On exclut les comportements "other"
  distinct() %>% # On retire les doublons éventuels
  na.omit() # On retire les lignes avec NA

distance_all_dt_3 <- distance_all_dt_1 %>%
  dplyr::select(ID, behavior, datetime, tide_strength, timeofday, month_numeric) %>%
  filter(behavior %in% c("foraging", "roosting")) %>%
  arrange(ID, datetime) %>%
  group_by(ID) %>%
  mutate(
    dt_diff = as.numeric(difftime(datetime, lag(datetime), units = "hours")),
    new_run = (behavior != lag(behavior)) | (dt_diff > 6) | is.na(lag(behavior)),
    behavior_run = cumsum(new_run)
  ) %>%
  ungroup()

# Calcul du centroïde pour comportement
distance_all_dt_4 <- distance_all_dt_3 %>%
  group_by(behavior_run, tide_strength, timeofday, month_numeric) %>%
  mutate(centroid = st_centroid(st_union(geometry))) %>% # Centroïde des points du groupe
  dplyr::select(-dt_diff, -new_run) %>%
  st_drop_geometry() %>% # Suppression de la géométrie d'origine
  distinct()

distance_all_dt_4$ID_run <- paste0(distance_all_dt_4$ID, "_", distance_all_dt_4$behavior_run)
distance_all_dt_4$ID_run_all <- paste0(
  distance_all_dt_4$ID, "_", distance_all_dt_4$behavior_run, "_", distance_all_dt_4$tide_strength,
  "_", distance_all_dt_4$timeofday, "_", distance_all_dt_4$month_numeric
)

Freq_distance_all_dt_4 <- as.data.frame(table(distance_all_dt_4$ID_run_all)) %>%
  filter(Freq > 1)

distance_all_dt_5 <- distance_all_dt_4 %>%
  arrange(ID, datetime) %>%
  group_by(ID_run_all) %>%
  mutate(
    mean_date = mean(datetime)
  ) %>%
  dplyr::select(-datetime) %>%
  distinct()

Freq_distance_all_dt_5 <- as.data.frame(table(distance_all_dt_5$ID_run)) %>%
  filter(Freq > 1)

pairs_dist <- distance_all_dt_5 %>%
  arrange(ID, mean_date) %>%
  group_by(ID) %>%
  mutate(
    next_Aehavior   = lead(behavior),
    next_centroid   = lead(centroid),
    next_date       = lead(mean_date)
  ) %>%
  filter(
    # garder seulement les transitions roosting <-> foraging
    (behavior == "roosting" & next_Aehavior == "foraging") |
      (behavior == "foraging" & next_Aehavior == "roosting")
  ) %>%
  mutate(
    time_diff_h = as.numeric(difftime(next_date, mean_date, units = "hours")),
    distance_m  = st_distance(centroid, next_centroid, by_element = TRUE)
  ) %>%
  filter(between(time_diff_h, 4, 8))

mean_dist_ID <- pairs_dist %>%
  group_by(ID, tide_strength, timeofday, month_numeric) %>%
  summarise(
    mean_dist = mean(distance_m),
    sd_dist = sd(distance_m)
  )

# Jointure entre les distances calculées et les sex_ages des individus
paired_centroids_all_dt <- mean_dist_ID %>%
  left_join(sex_age_dt) %>% # Ajout de la colonne "sex_age" par jointure sur ID
  na.omit() # On supprime les lignes avec NA (par exemple, si le all est inconnu)

paired_centroids_all_dt$mean_dist <- as.numeric(as.character(paired_centroids_all_dt$mean_dist))

hist(paired_centroids_all_dt$mean_dist)

# save ---
write.csv(paired_centroids_all_dt, paste0(data_generated_path, "paired_centroids_all_dt", ".csv"), row.names = FALSE)
paired_centroids_all_dt <- read.csv(paste0(data_generated_path, paste0("paired_centroids_all_dt", ".csv")), row.names = NULL)

# Modèle linéaire pour tester l'effet du all sur la distance
m_all_gaussien <- lm(mean_dist ~ sex + age + tide_strength + timeofday + month_numeric, data = paired_centroids_all_dt)
m_all_gamma <- glm(mean_dist ~ sex + age + tide_strength + timeofday + month_numeric, data = paired_centroids_all_dt, family = Gamma(link = "log"))
m_all_gamma2 <- glm(mean_dist ~ sex + age + tide_strength, data = paired_centroids_all_dt, family = Gamma(link = "log"))
m_all_gamma4 <- glm(mean_dist ~ sex * age + tide_strength, data = paired_centroids_all_dt, family = Gamma(link = "log"))
m_all_gamma3 <- glm(mean_dist ~ sex + age + tide_strength + timeofday, data = paired_centroids_all_dt, family = Gamma(link = "log"))
m_all_gamma5 <- glm(mean_dist ~ sex * age + tide_strength * sex, data = paired_centroids_all_dt, family = Gamma(link = "log"))
m_all_gamma6 <- glm(mean_dist ~ sex * age + tide_strength * age, data = paired_centroids_all_dt, family = Gamma(link = "log"))
m_all_gamma7 <- glm(mean_dist ~ sex * age + tide_strength * sex + tide_strength * age, data = paired_centroids_all_dt, family = Gamma(link = "log"))
m_all_gamma8 <- glm(mean_dist ~ sex * age + tide_strength * sex + tide_strength * age + timeofday, data = paired_centroids_all_dt, family = Gamma(link = "log"))
m_all_gamma9 <- glm(mean_dist ~ sex * age + tide_strength * age, data = paired_centroids_all_dt, family = Gamma(link = "log"))
m_all_gamma10 <- glm(mean_dist ~ sex + tide_strength * age, data = paired_centroids_all_dt, family = Gamma(link = "log"))

AIC(m_all_gaussien, m_all_gamma, m_all_gamma2, m_all_gamma3, m_all_gamma4, m_all_gamma5, 
    m_all_gamma6, m_all_gamma7, m_all_gamma8, m_all_gamma9, m_all_gamma10)

# talk talk talk
summary(m_all_gamma7)
summary(m_all_gamma9)

# diag
sim <- simulateResiduals(fittedModel = m_all_gamma7, plot = F)
# residuals(sim)
# residuals(sim, quantileFunction = qnorm, outlierValues = c(-7,7))
residuals_2 <- plot(sim)
testDispersion(sim)
testOutliers(sim)

# resultats en plot :

# 1. Créer une grille avec toutes les combinaisons des variables
newdat <- expand.grid(
  sex = unique(paired_centroids_all_dt$sex),
  age = unique(paired_centroids_all_dt$age),
  tide_strength = unique(paired_centroids_all_dt$tide_strength)
)

# 2. Prédictions avec IC
pred <- predict(m_all_gamma7, newdata = newdat, type = "link", se.fit = TRUE)

# Transformer en réponse (échelle originale de mean_dist)
newdat$fit <- exp(pred$fit) # car lien = log
newdat$se <- pred$se.fit
newdat$lwr <- exp(pred$fit - 1.96 * pred$se)
newdat$upr <- exp(pred$fit + 1.96 * pred$se)

# 3. Visualiser avec ggplot
ggplot(newdat, aes(x = age, y = fit, color = sex, group = sex)) +
  geom_point(position = position_dodge(width = 0.3), size = 3) +
  geom_errorbar(aes(ymin = lwr, ymax = upr),
    position = position_dodge(width = 0.3), width = 0.2
  ) +
  geom_line(position = position_dodge(width = 0.3)) +
  facet_wrap(~tide_strength) +
  labs(
    y = "Mean predicted roosting-foraging distance", x = "Age",
    title = ""
  ) +
  theme_classic()

levels(newdat$age)[levels(newdat$age) == "juvénile"] <- "Juvenile"
levels(newdat$age)[levels(newdat$age) == "adulte"] <- "Adult"

levels(newdat$tide_strength)[levels(newdat$tide_strength) == "neap_tide"] <- "Neap tide"
levels(newdat$tide_strength)[levels(newdat$tide_strength) == "spring_tide"] <- "Spring tide"

pred_all_plot <- ggplot(newdat, aes(x = age, y = fit, color = sex, group = sex)) +
  geom_errorbar(aes(ymin = lwr, ymax = upr),
    position = position_dodge(width = 0.3), width = 0, alpha = 0.5
  ) +
  geom_line(position = position_dodge(width = 0.3)) +
  geom_point(size = 3, position = position_dodge(width = 0.3)) +
  facet_wrap(~tide_strength) +
  scale_color_manual(
    values = c(
      "F" = "purple",
      "M" = "darkgreen"
    )
  ) +
  labs(
    y = "Mean predicted roosting-foraging distance", x = "Age", color = "Sex",
    title = ""
  ) +
  theme_classic()
pred_all_plot

ggsave(paste0(atlas_path, "/pred_all_plot.png"),
  plot = pred_all_plot, width = 4, height = 4, dpi = 300
)

# ~ all & chasse________________________________________________________________

# Filtrage des données pertinentes (hors comportement "other")
distance_all_chasse_dt_1 <- GPS %>%
  dplyr::select(ID, behavior, datetime, tide_strength, timeofday, month_numeric) %>% # On garde uniquement les colonnes utiles
  filter(behavior != "other") %>% # On exclut les comportements "other"
  filter(month_numeric %in% c(1, 2)) %>%
  distinct() %>% # On retire les doublons éventuels
  na.omit() # On retire les lignes avec NA

distance_all_chasse_dt_3 <- distance_all_chasse_dt_1 %>%
  dplyr::select(ID, behavior, datetime, tide_strength, timeofday, month_numeric) %>%
  filter(behavior %in% c("foraging", "roosting")) %>%
  arrange(ID, datetime) %>%
  group_by(ID) %>%
  mutate(
    dt_diff = as.numeric(difftime(datetime, lag(datetime), units = "hours")),
    new_run = (behavior != lag(behavior)) | (dt_diff > 6) | is.na(lag(behavior)),
    behavior_run = cumsum(new_run)
  ) %>%
  ungroup()

# Calcul du centroïde pour comportement
distance_all_chasse_dt_4 <- distance_all_chasse_dt_3 %>%
  group_by(behavior_run, tide_strength, timeofday, month_numeric) %>%
  mutate(centroid = st_centroid(st_union(geometry))) %>% # Centroïde des points du groupe
  dplyr::select(-dt_diff, -new_run) %>%
  st_drop_geometry() %>% # Suppression de la géométrie d'origine
  distinct()

distance_all_chasse_dt_4$ID_run <- paste0(distance_all_chasse_dt_4$ID, "_", distance_all_chasse_dt_4$behavior_run)
distance_all_chasse_dt_4$ID_run_all_chasse <- paste0(
  distance_all_chasse_dt_4$ID, "_", distance_all_chasse_dt_4$behavior_run, "_", distance_all_chasse_dt_4$tide_strength,
  "_", distance_all_chasse_dt_4$timeofday, "_", distance_all_chasse_dt_4$month_numeric
)

Freq_distance_all_chasse_dt_4 <- as.data.frame(table(distance_all_chasse_dt_4$ID_run_all_chasse)) %>%
  filter(Freq > 1)

distance_all_chasse_dt_5 <- distance_all_chasse_dt_4 %>%
  arrange(ID, datetime) %>%
  group_by(ID_run_all_chasse) %>%
  mutate(
    mean_date = mean(datetime)
  ) %>%
  dplyr::select(-datetime) %>%
  distinct()

Freq_distance_all_chasse_dt_5 <- as.data.frame(table(distance_all_chasse_dt_5$ID_run)) %>%
  filter(Freq > 1)

pairs_dist <- distance_all_chasse_dt_5 %>%
  arrange(ID, mean_date) %>%
  group_by(ID) %>%
  mutate(
    next_Aehavior   = lead(behavior),
    next_centroid   = lead(centroid),
    next_date       = lead(mean_date)
  ) %>%
  filter(
    # garder seulement les transitions roosting <-> foraging
    (behavior == "roosting" & next_Aehavior == "foraging") |
      (behavior == "foraging" & next_Aehavior == "roosting")
  ) %>%
  mutate(
    time_diff_h = as.numeric(difftime(next_date, mean_date, units = "hours")),
    distance_m  = st_distance(centroid, next_centroid, by_element = TRUE)
  ) %>%
  filter(between(time_diff_h, 4, 8))

mean_dist_ID <- pairs_dist %>%
  group_by(ID, tide_strength, timeofday, month_numeric) %>%
  summarise(
    mean_dist = mean(distance_m),
    sd_dist = sd(distance_m)
  )

# Jointure entre les distances calculées et les sex_ages des individus
paired_centroids_all_chasse_dt <- mean_dist_ID %>%
  left_join(sex_age_dt) %>% # Ajout de la colonne "sex_age" par jointure sur ID
  na.omit() # On supprime les lignes avec NA (par exemple, si le all est inconnu)

paired_centroids_all_chasse_dt$mean_dist <- as.numeric(as.character(paired_centroids_all_chasse_dt$mean_dist))

hist(paired_centroids_all_chasse_dt$mean_dist)

# save ---
write.csv(paired_centroids_all_chasse_dt, paste0(data_generated_path, "paired_centroids_all_chasse_dt", ".csv"), row.names = FALSE)
paired_centroids_all_chasse_dt <- read.csv(paste0(data_generated_path, paste0("paired_centroids_all_chasse_dt", ".csv")), row.names = NULL)

# Modèle linéaire pour tester l'effet du all sur la distance
m_all_chasse_gaussien <- lm(mean_dist ~ 1, data = paired_centroids_all_chasse_dt)

m_all_chasse_gamma <- glm(mean_dist ~ 1, data = paired_centroids_all_chasse_dt, family = Gamma(link = "log"))

AIC(m_all_chasse_gaussien, m_all_chasse_gamma)

paired_centroids_all_chasse_dt$month_numeric <- as.factor(paired_centroids_all_chasse_dt$month_numeric)

m_all_chasse_gamma2 <- glm(mean_dist ~ sex + age + tide_strength + timeofday + month_numeric, data = paired_centroids_all_chasse_dt, family = Gamma(link = "log"))
m_all_chasse_gamma3 <- glm(mean_dist ~ sex * age + tide_strength * sex + tide_strength * age + month_numeric, data = paired_centroids_all_chasse_dt, family = Gamma(link = "log"))
m_all_chasse_gamma4 <- glm(mean_dist ~ sex * age + tide_strength * sex + tide_strength + month_numeric, data = paired_centroids_all_chasse_dt, family = Gamma(link = "log"))
m_all_chasse_gamma5 <- glm(mean_dist ~ sex * age + tide_strength + month_numeric, data = paired_centroids_all_chasse_dt, family = Gamma(link = "log"))
m_all_chasse_gamma6 <- glm(mean_dist ~ sex + age + tide_strength + month_numeric, data = paired_centroids_all_chasse_dt, family = Gamma(link = "log"))
m_all_chasse_gamma7 <- glm(mean_dist ~ age + tide_strength + sex * month_numeric, data = paired_centroids_all_chasse_dt, family = Gamma(link = "log"))
m_all_chasse_gamma8 <- glm(mean_dist ~ sex + tide_strength + age * month_numeric, data = paired_centroids_all_chasse_dt, family = Gamma(link = "log"))
m_all_chasse_gamma9 <- glm(mean_dist ~ tide_strength + age * month_numeric + sex * month_numeric, data = paired_centroids_all_chasse_dt, family = Gamma(link = "log"))
m_all_chasse_gamma10 <- glm(mean_dist ~ sex + age + month_numeric, data = paired_centroids_all_chasse_dt, family = Gamma(link = "log"))
m_all_chasse_gamma11 <- glm(mean_dist ~ sex * age + month_numeric, data = paired_centroids_all_chasse_dt, family = Gamma(link = "log"))
m_all_chasse_gamma12 <- glm(mean_dist ~ sex + age * month_numeric, data = paired_centroids_all_chasse_dt, family = Gamma(link = "log"))
m_all_chasse_gamma13 <- glm(mean_dist ~ age + sex * month_numeric, data = paired_centroids_all_chasse_dt, family = Gamma(link = "log"))
m_all_chasse_gamma14 <- glm(mean_dist ~ sex + month_numeric, data = paired_centroids_all_chasse_dt, family = Gamma(link = "log"))
m_all_chasse_gamma15 <- glm(mean_dist ~ age + month_numeric, data = paired_centroids_all_chasse_dt, family = Gamma(link = "log"))

AIC(
  m_all_chasse_gamma, m_all_chasse_gamma2, m_all_chasse_gamma3,
  m_all_chasse_gamma4, m_all_chasse_gamma5, m_all_chasse_gamma6,
  m_all_chasse_gamma7, m_all_chasse_gamma8, m_all_chasse_gamma9,
  m_all_chasse_gamma10, m_all_chasse_gamma11, m_all_chasse_gamma12,
  m_all_chasse_gamma13, m_all_chasse_gamma14, m_all_chasse_gamma15
)

# talk talk talk
summary(m_all_chasse_gamma14)

# diag
sim <- simulateResiduals(fittedModel = m_all_chasse_gamma14, plot = F)
# residuals(sim)
# residuals(sim, quantileFunction = qnorm, outlierValues = c(-7,7))
residuals_2 <- plot(sim)
testDispersion(sim)
testOutliers(sim)

# resultats en plot :

# 1. Créer une grille
newdat2 <- expand.grid(
  sex = unique(paired_centroids_all_chasse_dt$sex),
  month_numeric = unique(paired_centroids_all_chasse_dt$month_numeric)
)

# 2. Prédictions
pred2 <- predict(m_all_chasse_gamma14, newdata = newdat2, type = "link", se.fit = TRUE)

newdat2$fit <- exp(pred2$fit)
newdat2$se <- pred2$se.fit
newdat2$lwr <- exp(pred2$fit - 1.96 * pred2$se)
newdat2$upr <- exp(pred2$fit + 1.96 * pred2$se)

levels(newdat2$month_numeric)[levels(newdat2$month_numeric) == "1"] <- "Allowed"
levels(newdat2$month_numeric)[levels(newdat2$month_numeric) == "2"] <- "Forbidden"

# 3. Visualisation
pred_all_chasse_plot <- ggplot(newdat2, aes(x = factor(month_numeric), y = fit, color = sex, group = sex)) +
  geom_point(size = 3, position = position_dodge(width = 0.3)) +
  geom_errorbar(aes(ymin = lwr, ymax = upr),
    position = position_dodge(width = 0.3), width = 0, alpha = 0.5
  ) +
  geom_line(position = position_dodge(width = 0.3)) +
  scale_color_manual(values = c("F" = "purple", "M" = "darkgreen")) +
  labs(
    y = "Mean predicted roosting-foraging distance",
    x = "Hunting",
    title = "",
    color = "Sex"
  ) +
  theme_classic()
pred_all_chasse_plot

ggsave(paste0(atlas_path, "/pred_all_chasse_plot.png"),
  plot = pred_all_chasse_plot, width = 4, height = 4, dpi = 300
)

# graphique_____________________________________________________________________

col_sex_age <- c(
  "femelle adulte" = "purple", "femelle juvénile" = "lightpink",
  "mâle adulte" = "darkgreen", "mâle juvénile" = "lightgreen",
  "inconnu" = "grey40"
)

graph_dist_dt <- pairs_dist %>%
  left_join(sex_age_dt) %>% # Ajout de la colonne "sex_age" par jointure sur ID
  na.omit()

dt_distance_talk$distance_m <- as.numeric(as.character(dt_distance_talk$distance_m))

emission_dt_1$sex_age <- factor(emission_dt_1$sex_age, levels = names(col_sex_age))

# plot

dt_distance_talk <- graph_dist_dt %>%
  mutate(
    sex = case_when(
      sex == "F" ~ "femelle",
      sex == "M" ~ "mâle"
    )
  )

dt_distance_talk$distance_m <- as.numeric(as.character(dt_distance_talk$distance_m))

my_comparisons <- list(c("femelle", "mâle"))

distance_roost_forag_sex_plot <- ggplot(
  dt_distance_talk,
  aes(x = sex, y = distance_m, fill = sex)
) +
  scale_fill_manual(values = c("mâle" = "darkgreen", "femelle" = "purple")) +
  geom_Aoxplot(outlier.colour = "grey", outlier.shape = 1) +
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
  scale_y_continuous(limits = c(0, 13500), breaks = seq(0, 12500, by = 2500)) +
  theme(legend.position = "none") +
  labs(
    title = "",
    x = "Sexe", y = expression(atop(
      "Distance entre les aires individuelles",
      "de repos et de alimentation (m)"
    )),
    fill = ""
  )
distance_roost_forag_sex_plot

my_comparisons <- list(c("adulte", "juvénile"))

distance_roost_forag_age_plot <- ggplot(
  dt_distance_talk,
  aes(x = age, y = distance_m, fill = age)
) +
  geom_Aoxplot(outlier.colour = "grey", outlier.shape = 1) +
  scale_fill_manual(
    values = c("adulte" = "#D47545", "juvénile" = "#D2AB99"),
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
  scale_y_continuous(limits = c(0, 13500), breaks = seq(0, 12500, by = 2500)) +
  labs(
    title = "",
    x = "Age", y = expression(atop(
      "Distance entre les aires individuelles",
      "de repos et de alimentation (m)"
    )),
    fill = ""
  )
distance_roost_forag_age_plot

dt_distance_talk_tide <- graph_dist_dt %>%
  mutate(
    tide_strength = case_when(
      tide_strength == "spring_tide" ~ "vives eaux",
      tide_strength == "submersion" ~ "submersion",
      tide_strength == "neap_tide" ~ "mortes eaux",
    )
  )

dt_distance_talk_tide$distance_m <- as.numeric(as.character(dt_distance_talk_tide$distance_m))

my_comparisons <- list(c("mortes eaux", "vives eaux", "submersion"))

dt_distance_talk_tide$tide_strength <- factor(dt_distance_talk_tide$tide_strength,
  levels = c("mortes eaux", "vives eaux", "submersion")
)

distance_roost_forag_tides_high_type_plot <- ggplot(
  dt_distance_talk_tide,
  aes(x = tide_strength, y = distance_m, fill = tide_strength)
) +
  geom_Aoxplot(col = "black", outlier.colour = "grey", outlier.shape = 1) +
  scale_fill_manual(values = c("mortes eaux" = "#65B4E5", "vives eaux" = "#2083C1", "submersion" = "#00426C")) +
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
  scale_y_continuous(limits = c(0, 13500), breaks = seq(0, 12500, by = 2500)) +
  theme(legend.position = "none") +
  labs(
    title = "",
    x = "Marée", y = expression(atop(
      "Distance entre les aires individuelles",
      "de repos et de alimentation (m)"
    )),
    fill = ""
  )
distance_roost_forag_tides_high_type_plot

distance_roost_forag_allvar_plot <- ggarrange(distance_roost_forag_sex_plot,
  distance_roost_forag_age_plot,
  distance_roost_forag_tides_high_type_plot,
  ncol = 3
)

distance_roost_forag_allvar_plot

ggsave(paste0(atlas_path, "/distance_roost_forag_allvar_plot_talk.png"),
  plot = distance_roost_forag_allvar_plot, width = 10, height = 4, dpi = 300
)

# _____________________________________________________________________________________________________________________________________
# _____________________________________________________________________________________________________________________________________
# 17. Evènements climatiques extrêmes ------------------------------------------
# _____________________________________________________________________________________________________________________________________
# _____________________________________________________________________________________________________________________________________

# données météo_________________________________________________________________

# Lecture du fichier Excel contenant les données météo de La Rochelle
meteo <- read_excel(paste0(data_path, "/Meteo/meteo_courlis_la_rochelle.xlsx"))

# Sélection des variables d'intérêt et conversion de la colonne 'date' en format Date
meteo_2 <- meteo %>%
  dplyr::select(date, tavg, tmin, tmax, prcp, wdir, wspd, pres) %>% # températures, précipitations, vent, pression
  rename(y_m_d = date) %>% # renommage de la colonne date
  mutate(y_m_d = ymd(y_m_d)) # conversion en format Date

# vent fort_____________________________________________________________________

# Création d'une variable catégorielle 'ECE_wspd' qui identifie les jours avec vent ≥ 95e percentile
meteo_ECE_wspd <- meteo_2 %>%
  mutate(
    vent = case_when(
      wspd >= quantile(wspd, .95, na.rm = TRUE) ~ "vent fort", # jour avec événement de vent fort
      TRUE ~ "jour de référence" # sinon : rien à signaler
    )
  )

# Extraction des dates des événements ECE de vent fort
ECE_dates_wspd <- meteo_ECE_wspd %>%
  filter(vent != "jour de référence") %>% # on garde uniquement les ECE
  pull(y_m_d) # on extrait les dates

# Création d’un vecteur de dates élargi : ajout des jours -7 pour chaque ECE (comparaison sans ECE)
dates_Butour_ECE_wspd <- unique(c(ECE_dates_wspd, ECE_dates_wspd - days(7)))

# Filtrage du jeu de données météo pour ne garder que les dates ECE et leurs jours de comparaison
meteo_filtre_ECE_wspd <- meteo_ECE_wspd %>%
  filter(y_m_d %in% dates_Butour_ECE_wspd) %>%
  dplyr::select(y_m_d, vent)

# Vérification de la distribution des étiquettes (ECE ou RAS)
table(meteo_filtre_ECE_wspd$vent)

# Jointure entre les données GPS et les jours avec ou sans ECE, on retire les NA sur ECE_wspd
GPS_ECE_wspd <- left_join(GPS, meteo_filtre_ECE_wspd) %>%
  na.omit(vent) # ne garder que les lignes avec un label ECE ou RAS

# Mise à jour de l'objet GPS avec l’info ECE même si NA (utile pour analyses ultérieures)
GPS <- left_join(GPS, meteo_filtre_ECE_wspd)

GPS_roosting <- GPS %>%
  filter(behavior == "roosting")

GPS_foraging <- GPS %>%
  filter(behavior == "foraging")

# reposoir_________

GPS_sampled <- sample_weighted_points(
  data = GPS_roosting,
  n = 1000,
  variable = "vent",
  zone = "zone",
  cap = 3600
)

table(GPS_sampled$vent, useNA = "always")

GPS_sampled <- st_as_sf(GPS_sampled, coords = c("lon", "lat"), crs = 4326) %>%
  mutate(lon = st_coordinates(.)[, 1], lat = st_coordinates(.)[, 2])

zoom_levels <- c("A", "B", "C")

results_kud <- NULL
nb_kud <- NULL
analyse <- "kud"
variable <- "vent"
comportement <- "roosting"
couleur <- c("yellow", "#FF00E6")
scales::show_col(couleur)

plan(multisession, workers = 3)

results_list <- future_lapply(
  zoom_levels,
  function(z) {
    make_kud_variable(analyse, z, comportement, GPS_sampled, data_generated_path, resolution_ZOOM, couleur, variable)
  },
  future.seed = TRUE
)

# alimentation___________

GPS_sampled <- sample_weighted_points(
  data = GPS_foraging,
  n = 1000,
  variable = "vent",
  zone = "zone",
  cap = 3600
)

GPS_sampled <- st_as_sf(GPS_sampled, coords = c("lon", "lat"), crs = 4326) %>%
  mutate(lon = st_coordinates(.)[, 1], lat = st_coordinates(.)[, 2])

zoom_levels <- c("A", "B", "C")

results_kud <- NULL
nb_kud <- NULL
analyse <- "kud"
variable <- "vent"
comportement <- "foraging"
couleur <- c("yellow", "#49B6FF")
scales::show_col(couleur)

plan(multisession, workers = 3)

results_list <- future_lapply(
  zoom_levels,
  function(z) {
    make_kud_variable(analyse, z, comportement, GPS_sampled, data_generated_path, resolution_ZOOM, couleur, variable)
  },
  future.seed = TRUE
)

# vent de Nord-Ouest____________________________________________________________

# Création d’une variable 'ECE_wNO' pour détecter les jours où le vent vient du Nord-Ouest (≥ 270°)
meteo_ECE_wNO <- meteo_2 %>%
  mutate(
    orientation = case_when(
      between(wdir, 270, max(meteo$wdir, na.rm = TRUE)) ~ "vent de Nord-Ouest", # si direction du vent ≥ 270°
      TRUE ~ "jour de référence" # sinon : rien à signaler
    )
  )

# Extraction des dates où un vent de Nord-Ouest a été détecté (événement ECE)
ECE_dates_wNO <- meteo_ECE_wNO %>%
  filter(orientation != "jour de référence") %>% # on garde uniquement les jours avec vent de NO
  pull(y_m_d) # extraction des dates

# Création d’un vecteur de dates élargi : ajout des jours -7 pour chaque ECE (comparaison sans ECE)
dates_Butour_ECE_wNO <- unique(c(ECE_dates_wNO, ECE_dates_wNO - days(7)))

# Filtrage du jeu de données météo pour ne garder que les dates ECE et leurs jours de comparaison
meteo_filtre_ECE_wNO <- meteo_ECE_wNO %>%
  filter(y_m_d %in% dates_Butour_ECE_wNO) %>%
  dplyr::select(y_m_d, orientation)

# Vérification du nombre de jours avec ECE ou RAS
table(meteo_filtre_ECE_wNO$orientation)

# Jointure des données GPS avec l'information sur le vent de Nord-Ouest, et suppression des lignes sans étiquette
GPS_ECE_wNO <- left_join(GPS, meteo_filtre_ECE_wNO) %>%
  na.omit(orientation) # on garde uniquement les données GPS associées à un jour étiqueté

# Mise à jour de l'objet GPS avec l'information ECE wNO (même si certains jours n'ont pas d'étiquette)
GPS <- left_join(GPS, meteo_filtre_ECE_wNO)

GPS_roosting <- GPS %>%
  filter(behavior == "roosting")

GPS_foraging <- GPS %>%
  filter(behavior == "foraging")

# reposoir______________

GPS_sampled <- sample_weighted_points(
  data = GPS_roosting,
  n = 1000,
  variable = "orientation",
  zone = "zone",
  cap = 3600
)

GPS_sampled <- st_as_sf(GPS_sampled, coords = c("lon", "lat"), crs = 4326) %>%
  mutate(lon = st_coordinates(.)[, 1], lat = st_coordinates(.)[, 2])

table(GPS_sampled$zone, useNA = "always")

zoom_levels <- c("A", "B", "C")

results_kud <- NULL
nb_kud <- NULL
analyse <- "kud"
variable <- "orientation"
comportement <- "roosting"
couleur <- c("yellow", "#FF00E6")
scales::show_col(couleur)

plan(multisession, workers = 3)

results_list <- future_lapply(
  zoom_levels,
  function(z) {
    make_kud_variable(analyse, z, comportement, GPS_sampled, data_generated_path, resolution_ZOOM, couleur, variable)
  },
  future.seed = TRUE
)

# alimentation______________

GPS_sampled <- sample_weighted_points(
  data = GPS_foraging,
  n = 1000,
  variable = "orientation",
  zone = "zone",
  cap = 3600
)

GPS_sampled <- st_as_sf(GPS_sampled, coords = c("lon", "lat"), crs = 4326) %>%
  mutate(lon = st_coordinates(.)[, 1], lat = st_coordinates(.)[, 2])

zoom_levels <- c("A", "B", "C")

results_kud <- NULL
nb_kud <- NULL
analyse <- "kud"
variable <- "orientation"
comportement <- "foraging"
couleur <- c("yellow", "#49B6FF")
scales::show_col(couleur)

plan(multisession, workers = 3)

results_list <- future_lapply(
  zoom_levels,
  function(z) {
    make_kud_variable(analyse, z, comportement, GPS_sampled, data_generated_path, resolution_ZOOM, couleur, variable)
  },
  future.seed = TRUE
)

# vent de Nord-Ouest & vent fort________________________________________________

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
      wspd >= quantile(wspd, .95, na.rm = TRUE) & ECE_wNO == "ECE Nord-Ouest" ~ "vent fort de Nord-Ouest",
      TRUE ~ "jour de référence"
    )
  )

# Extraction des dates où les deux conditions sont réunies
ECE_dates_wNO_wspd95 <- meteo_ECE_wNO_wspd95 %>%
  filter(ECE_wNO_wspd95 != "jour de référence") %>% # on garde uniquement les jours avec ECE combiné
  pull(y_m_d) # extraction des dates

# Ajout de jours de comparaison (7 jours avant chaque ECE)
dates_Butour_ECE_wNO_wspd95 <- unique(c(ECE_dates_wNO_wspd95, ECE_dates_wNO_wspd95 - days(7)))

# Filtrage du jeu de données météo pour ne garder que les dates ECE combinées et les jours de comparaison
meteo_filtre_ECE_wNO_wspd95 <- meteo_ECE_wNO_wspd95 %>%
  filter(y_m_d %in% dates_Butour_ECE_wNO_wspd95) %>%
  dplyr::select(y_m_d, ECE_wNO_wspd95)

# Vérification du nombre d’événements combinés vs RAS
table(meteo_filtre_ECE_wNO_wspd95$ECE_wNO_wspd95)

# Jointure entre les données GPS et les étiquettes d'événement combiné, suppression des lignes sans étiquette
GPS_ECE_wNO_wspd95 <- left_join(GPS, meteo_filtre_ECE_wNO_wspd95) %>%
  na.omit(ECE_wNO_wspd95) # on garde uniquement les données avec une étiquette valide

# Mise à jour de l’objet GPS avec l’info d’ECE combiné (même si certains jours n’ont pas d’étiquette)
GPS <- left_join(GPS, meteo_filtre_ECE_wNO_wspd95)

GPS_roosting <- GPS %>%
  filter(behavior == "roosting")

GPS_foraging <- GPS %>%
  filter(behavior == "foraging")

# reposoir_______________

GPS_sampled <- sample_weighted_points(
  data = GPS_roosting,
  n = 1000,
  variable = "ECE_wNO_wspd95",
  zone = "zone",
  cap = 3600
)

GPS_sampled <- st_as_sf(GPS_sampled, coords = c("lon", "lat"), crs = 4326) %>%
  mutate(lon = st_coordinates(.)[, 1], lat = st_coordinates(.)[, 2])

table(GPS_sampled$ID)

zoom_levels <- c("A", "B", "C")

results_kud <- NULL
nb_kud <- NULL
analyse <- "kud"
variable <- "ECE_wNO_wspd95"
comportement <- "roosting"
couleur <- c("yellow", "#FF00E6")
scales::show_col(couleur)

plan(multisession, workers = 3)

results_list <- future_lapply(
  zoom_levels,
  function(z) {
    make_kud_variable(analyse, z, comportement, GPS_sampled, data_generated_path, resolution_ZOOM, couleur, variable)
  },
  future.seed = TRUE
)

# alimentation________________

GPS_sampled <- sample_weighted_points(
  data = GPS_foraging,
  n = 1000,
  variable = "ECE_wNO_wspd95",
  zone = "zone",
  cap = 3600
)

GPS_sampled <- st_as_sf(GPS_sampled, coords = c("lon", "lat"), crs = 4326) %>%
  mutate(lon = st_coordinates(.)[, 1], lat = st_coordinates(.)[, 2])

zoom_levels <- c("A", "B", "C")

results_kud <- NULL
nb_kud <- NULL
analyse <- "kud"
variable <- "ECE_wNO_wspd95"
comportement <- "foraging"
couleur <- c("yellow", "#49B6FF")
scales::show_col(couleur)

plan(multisession, workers = 3)

results_list <- future_lapply(
  zoom_levels,
  function(z) {
    make_kud_variable(analyse, z, comportement, GPS_sampled, data_generated_path, resolution_ZOOM, couleur, variable)
  },
  future.seed = TRUE
)

# _____________________________________________________________________________________________________________________________________
# _____________________________________________________________________________________________________________________________________
# 18. Zone critique ------------------------------------------------------------
# _____________________________________________________________________________________________________________________________________
# _____________________________________________________________________________________________________________________________________

# roosting
UDMap_data_95_kud_roosting_B <- st_read(file.path(data_generated_path, "UDMap_data_95_kud_roosting_B.gpkg"))
UDMap_data_50_kud_roosting_B <- st_read(file.path(data_generated_path, "UDMap_data_50_kud_roosting_B.gpkg"))
UDMap_data_95_kud_roosting_A <- st_read(file.path(data_generated_path, "UDMap_data_95_kud_roosting_A.gpkg"))
UDMap_data_50_kud_roosting_A <- st_read(file.path(data_generated_path, "UDMap_data_50_kud_roosting_A.gpkg"))
UDMap_data_95_kud_roosting_C <- st_read(file.path(data_generated_path, "UDMap_data_95_kud_roosting_C.gpkg"))
UDMap_data_50_kud_roosting_C <- st_read(file.path(data_generated_path, "UDMap_data_50_kud_roosting_C.gpkg"))
# foraging
UDMap_data_95_kud_foraging_B <- st_read(file.path(data_generated_path, "UDMap_data_95_kud_foraging_B.gpkg"))
UDMap_data_50_kud_foraging_B <- st_read(file.path(data_generated_path, "UDMap_data_50_kud_foraging_B.gpkg"))
UDMap_data_95_kud_foraging_A <- st_read(file.path(data_generated_path, "UDMap_data_95_kud_foraging_A.gpkg"))
UDMap_data_50_kud_foraging_A <- st_read(file.path(data_generated_path, "UDMap_data_50_kud_foraging_A.gpkg"))
UDMap_data_95_kud_foraging_C <- st_read(file.path(data_generated_path, "UDMap_data_95_kud_foraging_C.gpkg"))
UDMap_data_50_kud_foraging_C <- st_read(file.path(data_generated_path, "UDMap_data_50_kud_foraging_C.gpkg"))
# tonnes
tonnes <- st_read(paste0(data_path, "Tonnes_de_chasse/tonnes.shp"))

couleur_roosting <- "#FF00E6"
couleur_foraging <- "#49B6FF"

# 1. Combiner tous les polygones KUD pour roosting et foraging
# 50% KUD
data_50_kud_roosting <- rbind(
  UDMap_data_50_kud_roosting_B,
  UDMap_data_50_kud_roosting_A,
  UDMap_data_50_kud_roosting_C
)
data_50_kud_foraging <- rbind(
  UDMap_data_50_kud_foraging_B,
  UDMap_data_50_kud_foraging_A,
  UDMap_data_50_kud_foraging_C
)

# 95% KUD
data_95_kud_roosting <- rbind(
  UDMap_data_95_kud_roosting_B,
  UDMap_data_95_kud_roosting_A,
  UDMap_data_95_kud_roosting_C
)
data_95_kud_foraging <- rbind(
  UDMap_data_95_kud_foraging_B,
  UDMap_data_95_kud_foraging_A,
  UDMap_data_95_kud_foraging_C
)

# 2. Harmoniser le CRS
crs_reference <- st_crs(data_50_kud_roosting)
data_50_kud_foraging <- st_transform(data_50_kud_foraging, crs_reference)
data_95_kud_roosting <- st_transform(data_95_kud_roosting, crs_reference)
data_95_kud_foraging <- st_transform(data_95_kud_foraging, crs_reference)
RMO <- st_transform(RMO, crs_reference)
tonnes <- st_transform(tonnes, crs_reference)

# 3. Calculer les intersections roosting-foraging
intersection_50_roosting_foraging <- st_intersection(
  st_union(data_50_kud_roosting),
  st_union(data_50_kud_foraging)
)

intersection_95_roosting_foraging <- st_intersection(
  st_union(data_95_kud_roosting),
  st_union(data_95_kud_foraging)
)

# 3bis. Supprimer les parties d'intersection à l'intérieur de la réserve RMO
intersection_50_roosting_foraging_hors_RMO <- st_difference(intersection_50_roosting_foraging, st_union(RMO))
intersection_95_roosting_foraging_hors_RMO <- st_difference(intersection_95_roosting_foraging, st_union(RMO))

# 4. Identifier les tonnes selon les zones
tonnes$couleur <- "black"
tonnes$taille <- 0.2

indices_50 <- lengths(st_intersects(tonnes, st_union(data_50_kud_foraging, data_50_kud_roosting))) > 0
indices_95 <- lengths(st_intersects(tonnes, st_union(data_95_kud_foraging, data_95_kud_roosting))) > 0

tonnes$couleur[indices_95] <- "yellow"
tonnes$taille[indices_95] <- 1.2
tonnes$couleur[indices_50] <- "red"
tonnes$taille[indices_50] <- 1.5

print(paste("Nombre de tonnes rouges (50%):", sum(tonnes$couleur == "red")))
print(paste("Nombre de tonnes jaunes (95%):", sum(tonnes$couleur == "yellow")))
print(paste("Nombre de tonnes noires:", sum(tonnes$couleur == "black")))

# 5. Créer la carte
zone_critique_map <- tm_basemap(c("Esri.WorldImagery", "OpenStreetMap", "CartoDB.Positron")) +
  # 50% KUD
  tm_shape(UDMap_data_50_kud_foraging_B) + tm_polygons(border.col = "white", col = couleur_foraging, fill_alpha = 0.8) +
  tm_shape(UDMap_data_50_kud_foraging_A) + tm_polygons(border.col = "white", col = couleur_foraging, fill_alpha = 0.8) +
  tm_shape(UDMap_data_50_kud_foraging_C) + tm_polygons(border.col = "white", col = couleur_foraging, fill_alpha = 0.8) +
  tm_shape(UDMap_data_50_kud_roosting_B) + tm_polygons(border.col = "white", col = couleur_roosting, fill_alpha = 0.8) +
  tm_shape(UDMap_data_50_kud_roosting_A) + tm_polygons(border.col = "white", col = couleur_roosting, fill_alpha = 0.8) +
  tm_shape(UDMap_data_50_kud_roosting_C) + tm_polygons(border.col = "white", col = couleur_roosting, fill_alpha = 0.8) +
  # 95% KUD
  tm_shape(UDMap_data_95_kud_foraging_B) + tm_polygons(border.col = NULL, col = couleur_foraging, fill_alpha = 0.3) +
  tm_shape(UDMap_data_95_kud_foraging_A) + tm_polygons(border.col = NULL, col = couleur_foraging, fill_alpha = 0.3) +
  tm_shape(UDMap_data_95_kud_foraging_C) + tm_polygons(border.col = NULL, col = couleur_foraging, fill_alpha = 0.3) +
  tm_shape(UDMap_data_95_kud_roosting_B) + tm_polygons(border.col = NULL, col = couleur_roosting, fill_alpha = 0.3) +
  tm_shape(UDMap_data_95_kud_roosting_A) + tm_polygons(border.col = NULL, col = couleur_roosting, fill_alpha = 0.3) +
  tm_shape(UDMap_data_95_kud_roosting_C) + tm_polygons(border.col = NULL, col = couleur_roosting, fill_alpha = 0.3) +
  # RMO
  tm_shape(RMO) + tm_polygons(col = "darkgreen", fill_alpha = 0, col_Blpha = 1, lwd = 2) +
  # INTERSECTIONS HORS RMO
  tm_shape(intersection_95_roosting_foraging_hors_RMO) + tm_polygons(col = "yellow", fill_alpha = 0.7, border.col = NULL, lwd = 2) +
  tm_shape(intersection_50_roosting_foraging_hors_RMO) + tm_polygons(col = "red", fill_alpha = 0.7, border.col = NULL, lwd = 2) +
  # TONNES
  tm_shape(tonnes) + tm_dots(col = "couleur", size = "taille", palette = c("black" = "black", "yellow" = "yellow", "red" = "red")) +
  # Site de baguage
  tm_shape(site_baguage) + tm_text("icone", size = 1.5) +
  tm_layout(legend.show = FALSE)

zone_critique_map

tmap_save(zone_critique_map, paste0(atlas_path, "zone_critique_map.html"))

# 6. Statistiques
cat("\n=== STATISTIQUES ===\n")
cat("Surface d'intersection 50% roosting-foraging (m²):", sum(st_area(intersection_50_roosting_foraging)), "\n")
cat("Surface d'intersection 95% roosting-foraging (m²):", sum(st_area(intersection_95_roosting_foraging)), "\n")
cat("Nombre de tonnes dans zones 50%:", sum(tonnes$couleur == "red"), "\n")
cat("Nombre de tonnes dans zones 95% uniquement:", sum(tonnes$couleur == "yellow"), "\n")
