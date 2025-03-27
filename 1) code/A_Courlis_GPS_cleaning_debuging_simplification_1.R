
# Run la partie "starting blok", 
# puis seulement runner la dernière partie "SAVE" avant la partie à souhaitée / en cours de travail

# STARTING BLOCK ---------------------------------------------------------------

# beep lorsqu'il y a une erreur 
options(error = function() {beep(7)})

# Nettoyage de l'environnement
rm(list=ls()) 

# time zone
# with_tz(Sys.time(), "Europe/Paris") # ça sert à rien, juste changement d'affichage 
# Sys.setenv(TZ = "UTC")  # Définit la timezone pour R, pour changer les tz en UTC partout pour les ojects crée après 

## Packages --------------------------------------------------------------------

library(tidyr)
library(lubridate)
library(sf)
library(ggplot2)
library(classInt)
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
library(gridExtra)
library(readxl)
library(ggalt)
library(tidyverse)
library(beepr)
library(readr)
library(dplyr)

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

## Chemins de données -------------------------------------------

data_path_serveur <- "D:/Projets_Suzanne/Courlis/Data/1) data/"
data_generated_path_serveur <- "D:/Projets_Suzanne/Courlis/Data/2) data_generated/"
data_image_path_serveur <- "D:/Projets_Suzanne/Courlis/Data/3) images/"
time_lag_path <- "D:/Projets_Suzanne/Courlis/Data/2) data_generated/time_lag/"

## Zone d'intérêt (BOX) -----------------------------------------

BOX <- st_as_sf(st_as_sfc(st_bbox(c(xmin = -1.26, xmax = -0.945, ymax = 46.01, ymin = 45.78), crs = st_crs(4326)))) # Définition d'une boîte englobante avec des coordonnées spécifiques
st_write(BOX, paste0(data_generated_path_serveur, "BOX.gpkg"), append = FALSE) # Sauvegarde de la boîte dans un fichier GeoPackage
BOX <- st_read(paste0(data_generated_path_serveur, "BOX.gpkg")) # Lecture de la boîte depuis le fichier sauvegardé
BOX_4326 <- st_transform(BOX, crs = 4326) # Transformation de la boîte au CRS 4326 (coordonnées géographiques)
verif_crs(BOX_4326)

###
####
## Font de carte ---------------------------------------------------------------
####
###

# Departements ---
dept <- st_read(paste0(data_path_serveur, "departements.gpkg"), layer = "contourdesdepartements") # Lecture du fichier des départements
verif_crs(dept)
dept_BOX <- st_intersection(dept, BOX_4326) # Intersection des départements avec une boîte de délimitation (BOX_4326)
rm(dept) # Suppression pour libérer de la mémoire
verif_crs(dept_BOX)

# Réserve ---
reserve <- st_read(paste0(data_path_serveur, "Réserve_naturelle/rnn/rnn/N_ENP_RNN_S_000.shp")) # Lecture du fichier shapefile des réserves naturelles
RMO <- reserve[reserve$NOM_SITE == "Moëze-Oléron", ] # Filtrage pour ne garder que la réserve "Moëze-Oléron"
rm(reserve) # Suppression pour libérer de la mémoire
RMO_4326 <- st_transform(RMO, crs = 4326) # Transformation de la boîte au CRS 4326 (coordonnées géographiques)
verif_crs(RMO_4326)

###
####
## Cartographie de la zone ------------------------------------------------------
####
###

# tmap_mode("view") # Activation du mode interactif pour tmap
# 
# # Création et affichage d'une carte avec tmap
# map <- tm_scalebar() +                 
#   tm_shape(BOX_4326) +                        
#   tm_polygons(col = "green", fill_alpha = 0.5) +  
#   tm_shape(RMO_4326) +                        
#   tm_polygons(col = "red", fill_alpha = 0.5) ; map

###
####
# GPS DATA to load -------------------------------------------------------------
####
###

# Chargement de toutes les donnees CSV en une seule fois

chemins_gps <- c(
  "C:/Users/Suzanne.Bonamour/Documents/Courlis/GPS/0) Original_gps/Data_brute_GPS/Extraction_Courlis-cendre_29_08_2024/MOVEBANK_limitrack/",
  "C:/Users/Suzanne.Bonamour/Documents/Courlis/GPS/0) Original_gps/Data_brute_GPS/Extraction_Courlis-cendre_29_08_2024/MOVEBANK_pp1083/"
)

donnees_gps <- lapply(chemins_gps, telecharger_donnees) # applique telecharger_donnees à chaque chemin et renvoie une liste contenant les données de chaque dossier

# Harmonisation et fusion des jeux de donnees
donnees_gps[[1]] <- donnees_gps[[1]] %>% dplyr::select(-comments)
all_gps <- rbindlist(donnees_gps)
rm(donnees_gps)  # Liberation de memoire

# Selection et renommage des colonnes
colonnes_utiles <- c(
  "event-id", "timestamp", "location-long", "location-lat", "individual-local-identifier"
)

noms_colonnes <- c(
  eventID = "event-id", time = "timestamp", lon = "location-long",
  lat = "location-lat", indID = "individual-local-identifier"
)

all_gps <- all_gps %>% dplyr::select(all_of(colonnes_utiles)) %>% dplyr::rename(!!!noms_colonnes)

# Nettoyage des identifiants individuels
all_gps$indID <- stri_extract(all_gps$indID, regex = "(?<=\\[).*?(?=\\])")
all_gps$indID[all_gps$indID == " FRP_ EC103792"] <- "FRP_EC103792"

# Suppression des lignes sans coordonnees GPS
all_gps <- all_gps[!is.na(all_gps$lon) & !is.na(all_gps$lat), ]

# Extraction de l'ID unique des bagues
all_gps$indID <- substring(all_gps$indID, first=5, last=12)

# Suppression des " sur les event ID
all_gps$eventID <- substring(all_gps$eventID, first=2, last=25)

# Suppression d'une point abérant (baro +++ et lon lat = 0)
all_gps <- all_gps %>% 
  filter(lon != 0)

verif_tz(all_gps, "time")

# *** SAMPLE *** 
# *** SAMPLE *** ---------------------------------------------------------------
# *** SAMPLE ***  

# sample <- unique(all_gps$indID)
# sample <- sample[1:15]
# 
# all_gps <- all_gps[all_gps$indID %in% sample,]
# 
# table(all_gps$indID)
# 
# verif_tz(all_gps, "time")

###
####
# SPATIAL DATA -----------------------------------------------------------------
####
###

# Conversion en objet sf avec projection WGS84 (EPSG:4326)
all_gps_spa <- st_as_sf(all_gps, coords = c("lon", "lat"), crs = 4326)

# Restauration explicite des colonnes longitude et latitude (inutile si elles existent déjà)
all_gps_spa$lon <- all_gps$lon
all_gps_spa$lat <- all_gps$lat

table(all_gps_spa$indID)

verif_crs(all_gps_spa)
verif_tz(all_gps_spa, "time")

# tmap_mode("plot")
# map <- tm_scalebar() +
#   tm_shape(world[world$continent=="Europe",]) +
#   tm_polygons() +
#   tm_shape(all_gps_spa) +
#   tm_dots(fill_alpha = 0.5) +
#   tm_shape(RMO_4326) +
#   tm_borders(col = "red") +
#   tm_crs("auto") ; map

###
####
# DANS LA BOX ------------------------------------------------------
####
###

# Filtrage des points à l'intérieur de la boîte définie (opération coûteuse en temps)
all_gps_spa_BOX <- st_intersection(all_gps_spa, BOX_4326) 

table(all_gps_spa_BOX$indID)

verif_crs(all_gps_spa_BOX)
verif_tz(all_gps_spa_BOX, "time")

# tmap_mode("plot")
# map <- tm_scalebar() +
#   tm_shape(dept_BOX) +
#   tm_polygons() +
#   tm_shape(all_gps_spa_BOX) +
#   tm_dots(fill_alpha = 0.5) +
#   tm_shape(RMO_4326) +
#   tm_borders(col = "red"); map

###
####
# TRIP -------------------------------------------------------------------------
####
###

# Vérification et suppression des valeurs manquantes dans la colonne 'time'
# all_gps_spa <- all_gps_spa[!is.na(all_gps_spa$time),]
# verif_crs(all_gps_spa)
# verif_tz(all_gps_spa, "time")

# changed 
all_gps_spa <- all_gps_spa_BOX[!is.na(all_gps_spa_BOX$time),]
verif_crs(all_gps_spa)
verif_tz(all_gps_spa, "time")

# Conversion en data frame et suppression de la colonne géométrique
all_gps_dt <- all_gps_spa %>%
  as.data.frame() %>%
  dplyr::mutate(ID = indID) %>%
  dplyr::select(-geometry) %>% 
  na.omit()

# Création des trajets (trip) par individu avec une structure temporelle ordonnée
all_gps_dt_2 <- data.frame(
  x = all_gps_dt$lon,          # Longitudes
  y = all_gps_dt$lat,            # Latitudes
  DateTime = as.POSIXct(all_gps_dt$time, format = "%Y-%m-%d %H:%M:%S"),
  ID = all_gps_dt$ID)  # Date-heure

verif_tz(all_gps_dt, "time")
verif_tz(all_gps_dt_2, "DateTime")

sum(is.na(all_gps_dt_2$DateTime))
all_gps_dt_2 <- na.omit(all_gps_dt_2)

# !!!!!!!!!!!!! faire comme dis par GPT pour la fonction rtip sans warning ...

all_trip <- all_gps_dt_2 %>% 
  group_by(ID) %>% 
  trip()

table(all_trip$ID)

verif_tz(all_trip, "DateTime")

###
####
# STATIONARY -------------------------------------------------------------------
####
###

# Filtrage des points stationnaires avec une vitesse maximale
all_trip$stationary <- speedfilter(all_trip, max.speed = 0.5)  # vitesse en km/h
summary(all_trip$stationary) # Vérification des points supprimés

verif_tz(all_trip, "DateTime")

# Conversion en objet sf
all_trip_stationary_sf <- st_as_sf(all_trip)
all_trip_stationary_sf <- st_transform(all_trip_stationary_sf, crs = 4326)

verif_tz(all_trip_stationary_sf, "DateTime")
verif_crs(all_trip_stationary_sf)

# Sélection des points valides avec une vitesse inférieure ou égale à la vitesse maximale km/h
all_trip_stationary_sf <- all_trip_stationary_sf %>% 
  filter(stationary == TRUE) %>% 
  select(-stationary)

# Extraction des coordonnées longitude et latitude
all_trip_stationary_sf <- all_trip_stationary_sf %>%
  mutate(lon = st_coordinates(.)[,1], lat = st_coordinates(.)[,2])

verif_tz(all_trip_stationary_sf, "DateTime")
verif_crs(all_trip_stationary_sf)

###
####
# INTERPOLATION ----------------------------------------------------------------
####
###

# #again 2
# # Installer et charger le package redisltraj si nécessaire
# # install.packages("redisltraj")
# # library(redisltraj)
# 
# # Exemple de données
# # orig_x <- as.POSIXct(c("2025-03-21 08:00", "2025-03-21 08:45", "2025-03-21 09:30"), format="%Y-%m-%d %H:%M")
# # orig_y <- c(10, 20, 30)
# 
# # Cibles d'interpolation
# # x_target <- as.POSIXct(c("2025-03-21 08:15", "2025-03-21 08:30", "2025-03-21 08:50", "2025-03-21 09:00"), format="%Y-%m-%d %H:%M")
# 
# # Calcul de l'écart entre les points originaux
# gaps <- diff(as.numeric(all_trip_stationary_sf$DateTime))
# 
# # Identifier les indices valides en fonction du gap maximum de 30 minutes
# max_gap <- 30 * 60  # 30 minutes en secondes
# valid_indices <- which(gaps <= max_gap)
# 
# # Filtrer les points originaux pour ne garder que les intervalles valides
# filtered_x <- orig_x[valid_indices + 1]  # Ajout de l'indice pour les gaps
# filtered_y <- orig_y[valid_indices + 1]
# 
# # Utiliser la fonction redisltraj pour effectuer l'interpolation
# interpolated_values <- redisltraj(filtered_x, filtered_y, x_target)
# 
# # Résultat de l'interpolation
# interpolated_values


# all_trip_stationary_sf <- na.omit(all_trip_stationary_sf)
# all_trip_stationary_sf$DateTime <- as.POSIXct(all_trip_stationary_sf$DateTime)
# all_trip_stationary_sf$ID <- as.factor(all_trip_stationary_sf$ID)
# 
# # Créer un data frame propre pour les coordonnées
# coords <- data.frame(x = all_trip_stationary_sf$lon, y = all_trip_stationary_sf$lat)
# 
# # Vérifier si des valeurs manquent
# if (any(is.na(coords$x) | is.na(coords$y) | is.na(all_trip_stationary_sf$DateTime))) {
#   stop("Attention : certaines valeurs sont NA dans les données d'entrée.")
# }
# 
# # Création de l'objet ltraj
# all_stationary.ltraj <- as.ltraj(
#   xy = coords,
#   date = all_trip_stationary_sf$DateTime,
#   id = all_trip_stationary_sf$ID
# )
# 
# # Vérification
# print(all_stationary.ltraj)
# 
# traj_filtered <- cutltraj(all_stationary.ltraj, f = function(dt) dt <= 30 * 60, criterion = "time")
# 
# 
# 
# ##### again
# ls()
# rm(coin)
# 
# str(all_stationary.ltraj)
# 
# str(all_trip_stationary_sf)
# 
# # Extraire les coordonnées (lon, lat) de l'objet sf
# coords <- st_coordinates(all_trip_stationary_sf)
# 
# # Vérifier la structure des coordonnées
# head(coords)
# 
# all_stationary.ltraj <- as.ltraj(
#   xy = coords,  # Utilise les coordonnées extraites
#   date = all_trip_stationary_sf$DateTime,
#   id = all_trip_stationary_sf$ID
# )
# 
# # Vérifier la structure de all_stationary.ltraj
# str(all_stationary.ltraj)
# 
# traj_filtered <- cutltraj(all_stationary.ltraj, f = function(dt) dt <= 30 * 60, criterion = "time")
# 
# # Vérifier le résultat filtré
# print(traj_filtered)
# 
# 
# # Tester sur un sous-ensemble plus petit de données
# subset_data <- all_stationary.ltraj[1:10]
# traj_filtered_subset <- cutltraj(subset_data, 
#                                  f = function(dt) dt <= 30 * 60, 
#                                  criterion = "time")
# print(traj_filtered_subset)
# 
# 
# rm(all_stationary.ltraj)
# rm(cutltraj)
# rm(as.ltraj)
# 
# 
# 
# 
# # Création de l'objet ltraj pour stocker les trajectoires des animaux
# all_stationary.ltraj <- as.ltraj(
#   xy = bind_cols(x = all_trip_stationary_sf$lon, y = all_trip_stationary_sf$lat),
#   date = all_trip_stationary_sf$DateTime,
#   id = all_trip_stationary_sf$ID
# )
# 
# 
# coords <- data.frame(x = all_trip_stationary_sf$lon, y = all_trip_stationary_sf$lat)
# 
# all_stationary.ltraj <- as.ltraj(
#   xy = coords,
#   date = all_trip_stationary_sf$DateTime,
#   id = all_trip_stationary_sf$ID
# )
# 
# all_trip_stationary_sf$DateTime <- as.POSIXct(all_trip_stationary_sf$DateTime)
# all_trip_stationary_sf$ID <- as.factor(all_trip_stationary_sf$ID)
# 
# str(all_stationary.ltraj)
# 
# 
# 
# traj_filtered <- cutltraj(all_stationary.ltraj, f = function(dt) dt <= 30 * 60, criterion = "time")
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 

# # Exemple de données de trajectoire avec des informations temporelles
# traj <- data.frame(
#   id = rep(1, 10),
#   time = c(1, 2, 3, 4, 5, 7, 8, 9, 10, 12),  # Temps variés
#   x = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
#   y = c(1, 2, 3, 4, 5, 6, 5, 4, 3, 2)
# )
# 
# # Convertir time en POSIXct (en supposant que le temps commence à une date spécifique)
# traj$time <- as.POSIXct(traj$time, origin = "2025-01-01", tz = "UTC")
# 
# 
# # Convertir les données en objet ltraj
# # library(trajectory)
# 
# traj_ltraj <- as.ltraj(xy = traj[, c("x", "y")], date = traj$time, id = traj$id)
# 
# # Exemple de découpage en fonction d'un seuil temporel
# seuil_temps <- 2  # seuil pour l'intervalle de temps
# # Découper la trajectoire en fonction d'un seuil temporel
# traj_cut <- cutltraj(traj_ltraj, timethresh = seuil_temps, criterion = "time")
# 
# # Affichage du résultat
# print(traj_cut)











# time_filter <- function(dt) {
#   return(dt > 30 * 60)  # 30 minutes en secondes
# }
# 
# time_filter(31 * 60)  # Doit retourner TRUE
# time_filter(15 * 60)  # Doit retourner FALSE
# 
# print(time_filter)
# 
# # Appliquer la coupure de la trajectoire
# # traj_filtre <- cutltraj(all_stationary.ltraj, time_filter, "time")
# traj_filtre <- cutltraj(all_stationary.ltraj, function(dt) dt > 30 * 60, "time")
# 
# # Visualisation de la trajectoire après découpage
# plot(traj_filtre)
# 
# summary(all_stationary.ltraj)
# 
# # Voir les premières valeurs de l'objet
# head(all_stationary.ltraj[[1]])
# 
# difftime(all_stationary.ltraj[[1]]$date[-1], all_stationary.ltraj[[1]]$date[-length(all_stationary.ltraj[[1]]$date)], units = "mins")
# 
# traj_filtre <- cutltraj(all_stationary.ltraj, function(dt) { dt > 30 * 60 }, "dt")
# 
# head(all_stationary.ltraj[[1]]$dt)
# 
# head(all_stationary.ltraj[[1]])
# 
# 
# 
# traj_filtre <- cutltraj(all_stationary.ltraj, function(dt) { dt > 30 * 60 }, "dt")
# 
# traj_test <- cutltraj(all_stationary.ltraj[[1]], function(dt) dt > 1800, "dt")
# 
# class(all_stationary.ltraj[[1]])
# 
# library(adehabitatLT)
# 
# # Supposons que tes données initiales soient dans un data.frame "your_data"
# # all_stationary.ltraj <- as.ltraj(
# #   xy = all_trip_stationary_sf[, c("lon", "lat")], 
# #   date = as.POSIXct(all_trip_stationary_sf$DateTime, tz = "UTC"), 
# #   id = all_trip_stationary_sf$ID
# # )
# 
# traj_filtre <- cutltraj(all_stationary.ltraj, time_filter(all_stationary.ltraj$dt), "dt")
# 
# 
# class(all_stationary.ltraj)
# 
# 
# traj_filtered <- cutltraj(
#   all_stationary.ltraj, 
#   f = function(dt) { 
#     if (is.null(dt) || length(dt) == 0) return(FALSE)  # Évite les erreurs
#     return(!is.na(dt) & dt <= (30 * 60))  # Vérifie et applique la condition
#   }, 
#   criterion = "time"
# )
# 
# ?cutltraj
# 
# 
# foo <- function(date) {
#   da <- as.POSIXlt(date, "UTC")
#   ho <- da$hour + da$min/60
#   return(ho>21&ho<22)
# }
# 
# ## We cut the trajectory into bursts after the relocation taken at 21H30:
# 
# bea1 <- cutltraj(all_stationary.ltraj, "foo(date)", nextr = TRUE)
# bea1
# 
# 
# 
# 
# 
# 
# traj_filtered <- cutltraj(all_stationary.ltraj, criterion = "time")
# 
# 
# 
# # traj_filtered <- cutltraj(all_stationary.ltraj, f = function(dt) { dt <= (30 * 60) }, criterion = "time")
# 
# traj_filtered <- cutltraj(all_stationary.ltraj, f = function(dt) dt <= 30 * 60, criterion = "time")
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 


# ok old 
all_stationary.ltraj <- as.ltraj(
  xy = bind_cols(x = all_trip_stationary_sf$lon, y = all_trip_stationary_sf$lat),
  date = all_trip_stationary_sf$DateTime,
  id = all_trip_stationary_sf$ID
)

# Re-échantillonnage des trajectoires
all_stationary.interp <- redisltraj(all_stationary.ltraj, 60*5, type = "time")

# rm(all_stationary.ltraj)

# Conversion en data frame avec renommer des colonnes pour clarté
all_stationary.interp <- ld(all_stationary.interp) %>% 
  rename(longitude = x, latitude = y)

# Conversion en objet sf (Spatial Feature)
inter_sf <- st_as_sf(all_stationary.interp, coords = c("longitude", "latitude"), crs = 4326)

inter_sf <- inter_sf %>%
  mutate(lon = st_coordinates(.)[,1], lat = st_coordinates(.)[,2]) %>% 
  select(date, id, pkey, geometry, lon, lat)

verif_tz(inter_sf, "date")
verif_crs(inter_sf)

###
####
# (TIME LAG 30 min max) --------------------------------------------------------
####
###

# # pour identifier les trous de plus de 30 min dans les enregistrements GPS
# 
# # all_trip_stationary_sf <- st_read(file.path(data_generated_path_serveur, "all_trip_stationary_sf.gpkg"))
# 
# time_lag_path <- "D:/Projets_Suzanne/Courlis/Data/2) data_generated/time_lag/"
# 
# # Paramètres
# max_time_lag <- 30
# 
# # Calcul des intervalles de temps
# all_trip_stationary_sf_timeLag <- all_trip_stationary_sf %>%
#   group_by(ID) %>%
#   arrange(ID, DateTime) %>%
#   mutate(timeLag = as.numeric(difftime(DateTime, lag(DateTime), units = "mins")))
# 
# # Identification des gaps temporels
# filtered_time_lags <- all_trip_stationary_sf_timeLag %>%
#   st_drop_geometry() %>%
#   arrange(ID, DateTime) %>%
#   group_by(ID) %>%
#   mutate(Date_before_timeLag = lag(DateTime),
#          Date_after_timeLag = lead(DateTime),
#          diff_before_after = as.numeric(difftime(Date_after_timeLag, Date_before_timeLag, units = "mins"))) %>%
#   # filter(timeLag > max_time_lag) %>%
#   dplyr::select(ID, DateTime, Date_before_timeLag, Date_after_timeLag, diff_before_after) %>%
#   distinct()
# 
# # Vérification des chevauchements
# overlap_check <- filtered_time_lags %>%
#   arrange(ID, DateTime) %>%
#   group_by(ID) %>%
#   mutate(overlap = int_overlaps(interval(Date_before_timeLag, Date_after_timeLag),
#                                 interval(lag(Date_before_timeLag), lag(Date_after_timeLag)))) %>%
#   na.omit()
# 
# # permet de regrouper les intervalles consécutifs non chevauchants sous un même identifiant de groupe
# overlap_check$group <- cumsum(!overlap_check$overlap) + 1
# 
# # Séparation des gaps non chevauchants
# gaps_non_overlapping <- overlap_check %>% filter(overlap == FALSE) %>%
#   rename(starting_gap = Date_before_timeLag, ending_gap = Date_after_timeLag) %>%
#   dplyr::select(ID, starting_gap, ending_gap)
# 
# # Fusion des gaps chevauchants
# gaps_overlapping <- overlap_check %>%
#   filter(overlap == TRUE) %>%
#   group_by(ID, group) %>%
#   summarise(starting_gap = min(Date_before_timeLag), ending_gap = max(Date_after_timeLag), .groups = "drop") %>%
#   dplyr::select(ID, starting_gap, ending_gap)
# 
# # Union des gaps
# all_gaps <- bind_rows(gaps_non_overlapping, gaps_overlapping)
# all_gaps$n <- seq_len(nrow(all_gaps))
# 
# # Suppression des objets inutiles
# rm(all_trip_stationary_sf, all_trip_stationary_sf_timeLag)
# 
# verif_tz(all_gaps, "starting_gap")
# verif_tz(all_gaps, "ending_gap")
# verif_tz(inter_sf, "date")
# verif_crs(inter_sf)
# 
# # Suppression des points interpolés ---
# 
# inter_remove <- inter_sf %>%
#   st_drop_geometry()
# 
# remove_i_all <- list()
# 
# for (ind_i in unique(all_gaps$ID)) {
#   cat("Processing:", ind_i, "\n")
# 
#   ind_i_data <- all_gaps %>% filter(ID == ind_i)
# 
#   remove_i_list <- map(ind_i_data$n, function(n) {
#     start_i_n <- ind_i_data$starting_gap[ind_i_data$n == n]
#     end_i_n <- ind_i_data$ending_gap[ind_i_data$n == n]
#     inter_remove %>% filter(id == ind_i & between(date, start_i_n, end_i_n))
#   })
# 
#   remove_i_dt <- bind_rows(remove_i_list)
#   write.table(remove_i_dt, file = paste0(time_lag_path, "inter_remove_ind_", ind_i, ".csv"),
#               sep = ";", row.names = FALSE)
# }
# 
# # Agrégation des résultats
# files_time_lag <- list.files(path = time_lag_path, pattern = "*.csv", full.names = TRUE)
# dt_time_lag <- lapply(files_time_lag, fread, sep = ";")
# all_time_lag_remove <- rbindlist(dt_time_lag, fill = TRUE)
# 
# verif_tz(all_time_lag_remove, "date")
# 
# # Suppression des points dans le dataset GPS
# dt_base <- inter_sf %>%
#   st_drop_geometry()
# 
# rr <- as.data.frame(all_time_lag_remove$pkey) %>%
#   rename(pkey = `all_time_lag_remove$pkey`)
# 
# df_diff <- anti_join(dt_base, rr)
# 
# # Vérification des longueurs
# cat("Nombre total de points à supprimer:", length(all_time_lag_remove$pkey), "\n")
# cat("Nombre total de points initiaux:", length(dt_base$pkey), "\n")
# cat("Nombre total de points restants:", length(df_diff$pkey), "\n")
# 
# if (length(dt_base$pkey) - length(all_time_lag_remove$pkey) != length(df_diff$pkey)) {
#   beep(2) ; beep(7)
# }
# 
# 
# 
# 
# # Finalisation
# point_no_gap <- left_join(df_diff, inter_sf)
# 
# verif_tz(point_no_gap, "date")

###
####
# (TIME LAG MAX v2) --------------------------------------------------------------
####
###

# # pour identifier les trous de plus de 30 min dans les enregistrements GPS
# 
# # all_trip_stationary_sf <- st_read(file.path(data_generated_path_serveur, "all_trip_stationary_sf.gpkg"))
# 
# time_lag_path <- "D:/Projets_Suzanne/Courlis/Data/2) data_generated/time_lag/"
# 
# # Paramètres
# max_time_lag <- 30
# 
# # Calcul des intervalles de temps
# all_trip_stationary_sf_timeLag <- all_trip_stationary_sf %>%
#   group_by(ID) %>%
#   arrange(ID, DateTime) %>%
#   mutate(timeLag = as.numeric(difftime(DateTime, lag(DateTime), units = "mins")))
# 
# # Identification des gaps temporels
# filtered_time_lags <- all_trip_stationary_sf_timeLag %>%
#   st_drop_geometry() %>%
#   group_by(ID) %>%
#   arrange(ID, DateTime) %>%
#   mutate(Date_t = DateTime,
#          Date_t1 = lead(DateTime),
#          timeLag = as.numeric(difftime(Date_t1, Date_t, units = "mins"))) %>%
#   filter(timeLag > max_time_lag) %>%
#   dplyr::select(ID, DateTime, Date_t, Date_t1, timeLag) %>%
#   distinct()
# 
# table(filtered_time_lags$ID)
# 
# # # Vérification des chevauchements
# # overlap_check <- filtered_time_lags %>%
# #   arrange(ID, DateTime) %>%
# #   group_by(ID) %>%
# #   mutate(overlap = int_overlaps(interval(Date_before_timeLag, Date_after_timeLag),
# #                                 interval(lag(Date_before_timeLag), lag(Date_after_timeLag)))) %>%
# #   na.omit()
# # 
# # # permet de regrouper les intervalles consécutifs non chevauchants sous un même identifiant de groupe
# # overlap_check$group <- cumsum(!overlap_check$overlap) + 1
# # 
# # # Séparation des gaps non chevauchants
# # gaps_non_overlapping <- overlap_check %>% filter(overlap == FALSE) %>%
# #   rename(starting_gap = Date_before_timeLag, ending_gap = Date_after_timeLag) %>%
# #   dplyr::select(ID, starting_gap, ending_gap)
# # 
# # # Fusion des gaps chevauchants
# # gaps_overlapping <- overlap_check %>%
# #   filter(overlap == TRUE) %>%
# #   group_by(ID, group) %>%
# #   summarise(starting_gap = min(Date_before_timeLag), ending_gap = max(Date_after_timeLag), .groups = "drop") %>%
# #   dplyr::select(ID, starting_gap, ending_gap)
# # 
# # # Union des gaps
# # all_gaps <- bind_rows(gaps_non_overlapping, gaps_overlapping)
# # all_gaps$n <- seq_len(nrow(all_gaps))
# # 
# # # Suppression des objets inutiles
# # rm(all_trip_stationary_sf, all_trip_stationary_sf_timeLag)
# 
# verif_tz(filtered_time_lags, "Date_t")
# verif_tz(filtered_time_lags, "Date_t1")
# verif_tz(inter_sf, "date")
# verif_crs(inter_sf)
# 
# 
# 
# 
# # Fonction pour fusionner les intervalles de dates en conservant les IDs
# fusionner_intervalles <- function(df) {
#   # Trier les données par date de début
#   df <- df %>% arrange(Date_t)
#   
#   # Initialiser la liste des intervalles fusionnés
#   result <- data.frame(start = as.POSIXct(character()), 
#                        end = as.POSIXct(character()), 
#                        ids = character(), 
#                        stringsAsFactors = FALSE)
#   
#   # Initialiser le premier intervalle
#   current_start <- df$Date_t[1]
#   current_end <- df$Date_t1[1]
#   current_ids <- as.character(df$ID[1])  # Stocker les IDs
#   
#   for (i in 2:nrow(df)) {
#     next_start <- df$Date_t[i]
#     next_end <- df$Date_t1[i]
#     next_id <- as.character(df$ID[i])
#     
#     # Vérifier si les intervalles se chevauchent ou sont contigus (écart ≤ 1 min)
#     if (next_start <= (current_end + 60)) {
#       # Fusionner les intervalles et ajouter l'ID
#       current_end <- max(current_end, next_end)
#       current_ids <- paste(current_ids, next_id, sep = ",")
#     } else {
#       # Ajouter l'intervalle fusionné et passer au suivant
#       result <- rbind(result, data.frame(start = current_start, end = current_end, ids = current_ids))
#       current_start <- next_start
#       current_end <- next_end
#       current_ids <- next_id
#     }
#   }
#   
#   # Ajouter le dernier intervalle fusionné
#   result <- rbind(result, data.frame(start = current_start, end = current_end, ids = current_ids))
#   
#   return(result)
# }
# 
# # Exemple de données avec des chevauchements et des écarts de 1 min
# # df <- data.frame(
# #   id = c(1, 2, 3, 4),
# #   start = as.POSIXct(c("2024-03-21 08:00:00", "2024-03-21 08:45:00", "2024-03-21 09:00:00", "2024-03-21 09:31:00")),
# #   end = as.POSIXct(c("2024-03-21 08:45:00", "2024-03-21 09:00:00", "2024-03-21 09:30:00", "2024-03-21 10:00:00"))
# # )
# 
# # Appliquer la fonction
# fusionner_intervalles(filtered_time_lags)
# 
# 
# 
# 
# 
# 
# # Suppression des points interpolés ---
# 
# filtered_time_lags$n <- seq_len(nrow(filtered_time_lags))
# 
# 
# inter_remove <- inter_sf %>%
#   st_drop_geometry()
# 
# remove_i_all <- list()
# 
# ind_i = "EA542017"
# # n = 7793
# 
# for (ind_i in unique(filtered_time_lags$ID)) {
#   cat("Processing:", ind_i, "\n")
# 
#   ind_i_data <- filtered_time_lags %>% filter(ID == ind_i)
#   
#   for (n in unique(ind_i_data$n)){
#     
#     remove_i_list <- map(ind_i_data$n, function(n) {
#       start_i_n <- ind_i_data$Date_t[ind_i_data$n == n]
#       end_i_n <- ind_i_data$Date_t1[ind_i_data$n == n]
#       inter_remove %>% filter(id == ind_i & !between(date, start_i_n, end_i_n))
#     })
#     
#   }
#   
#   remove_i_dt <- bind_rows(remove_i_list)
#   # remove_i_dt <- remove_i_dt %>% 
#   #   distinct()
#   # write.table(remove_i_dt, file = paste0(time_lag_path, "inter_remove_ind_", ind_i, ".csv"),
#   #             sep = ";", row.names = FALSE)
# }
# 
# length(ind_i_data$DateTime)
# length(inter_remove$date)
# length(inter_sf$date[inter_sf$id==ind_i])
# 
# 
# # Agrégation des résultats
# files_time_lag <- list.files(path = time_lag_path, pattern = "*.csv", full.names = TRUE)
# dt_time_lag <- lapply(files_time_lag, fread, sep = ";")
# all_time_lag_remove <- rbindlist(dt_time_lag, fill = TRUE)
# 
# verif_tz(all_time_lag_remove, "date")
# 
# # Suppression des points dans le dataset GPS
# dt_base <- inter_sf %>%
#   st_drop_geometry()
# 
# rr <- as.data.frame(all_time_lag_remove$pkey) %>%
#   rename(pkey = `all_time_lag_remove$pkey`)
# 
# df_diff <- anti_join(dt_base, rr)
# 
# # Vérification des longueurs
# cat("Nombre total de points à supprimer:", length(all_time_lag_remove$pkey), "\n")
# cat("Nombre total de points initiaux:", length(dt_base$pkey), "\n")
# cat("Nombre total de points restants:", length(df_diff$pkey), "\n")
# 
# if (length(dt_base$pkey) - length(all_time_lag_remove$pkey) != length(df_diff$pkey)) {
#   beep(2) ; beep(7)
# }
# 
# 
# 
# 
# # Finalisation
# point_no_gap <- left_join(df_diff, inter_sf)
# 
# verif_tz(point_no_gap, "date")




# (OKEYYYYYYYYYYYYY) -------------------------------------------------------------

# library(adehabitatLT)
# 
# # Simuler une trajectoire avec des timestamps
# # set.seed(123)
# # time_stamps <- as.POSIXct(c("2024-03-20 12:00:00", 
# #                             "2024-03-20 12:15:00", 
# #                             "2024-03-20 13:00:00",  # Trop éloigné (45 min)
# #                             "2024-03-20 13:10:00",
# #                             "2024-03-20 14:00:00")) # Trop éloigné (50 min)
# # 
# # coords <- data.frame(x = c(1, 2, 5, 6, 10), y = c(1, 2, 5, 6, 10))
# # 
# # # Créer une trajectoire
# # traj <- as.ltraj(xy = coords, date = time_stamps, id = "Animal1")
# 
# # Filtrer les segments avec un écart temporel <= 30 min
# traj_filtered <- cutltraj(all_stationary.ltraj, f = function(dt) dt <= 30 * 60, criterion = "time")
# 
# # Rediscrétiser la trajectoire à un pas de temps régulier (ex: toutes les 10 minutes)
# traj_redisc <- redisltraj(traj_filtered, 10 * 60, type = "time")
# 
# # Afficher la trajectoire modifiée
# plot(traj_redisc)

###
####
# TIME LAG max v3 --------------------------------------------------------------
####
###

# pour identifier les trous de plus de 30 min dans les enregistrements GPS

# all_trip_stationary_sf <- st_read(file.path(data_generated_path_serveur, "all_trip_stationary_sf.gpkg"))

# Paramètres
max_time_lag <- 5

# Calcul des intervalles de temps
all_trip_stationary_sf_timeLag <- all_trip_stationary_sf %>%
  group_by(ID) %>%
  arrange(ID, DateTime) %>%
  mutate(timeLag = as.numeric(difftime(DateTime, lag(DateTime), units = "mins")))

# Identification des gaps temporels
filtered_time_lags <- all_trip_stationary_sf %>%
  st_drop_geometry() %>%
  arrange(ID, DateTime) %>%
  group_by(ID) %>%
  mutate(Date_before_timeLag = DateTime,
         Date_after_timeLag = lead(DateTime),
         diff_before_after = as.numeric(difftime(Date_after_timeLag, Date_before_timeLag, units = "mins"))) %>%
  filter(diff_before_after > max_time_lag) %>%
  dplyr::select(ID, DateTime, Date_before_timeLag, Date_after_timeLag, diff_before_after) %>%
  distinct()



#######################
#########################

# # library(intervals)
# # 
# # date_ranges <- data.frame(
# #   start = as.Date(c("2024-01-01", "2024-01-05", "2024-01-10", "2024-01-15", "2024-01-20")),
# #   end = as.Date(c("2024-01-06", "2024-01-12", "2024-01-18", "2024-01-22", "2024-01-25"))
# # )
# # 
# # merged_dates <- filtered_time_lags %>% 
# #   arrange(ID, DateTime) %>%
# #   group_by(ID) %>%
# #   interval_union(start_col = "Date_before_timeLag", end_col = "Date_after_timeLag")
# # 
# # print(merged_dates)
# # 
# # library(dplyr)
# # library(fuzzyjoin)
# # 
# # library(dplyr)
# # library(fuzzyjoin)
# # library(purrr)
# # 
# # library(dplyr)
# # library(IRanges)
# # library(purrr)
# # 
# # 
# # 
# # install.packages("BiocManager")  # Installer Bioconductor si ce n'est pas fait
# # BiocManager::install("IRanges")  # Installer IRanges
# # library(IRanges)
# # 
# # 
# # # Fonction pour fusionner les intervalles d'un individu
# # merge_intervals <- function(df) {
# #   ranges <- IRanges(start = as.integer(df$Date_before_timeLag), 
# #                     end = as.integer(df$Date_after_timeLag))
# #   merged_ranges <- reduce(ranges)
# #   return(data.frame(
# #     ID = unique(df$ID),  # Garder l'ID de l'individu
# #     start = as.Date(start(merged_ranges), origin = "1970-01-01"),
# #     end = as.Date(end(merged_ranges), origin = "1970-01-01")
# #   ))
# # }
# # 
# # # Appliquer la fonction séparément pour chaque individu
# # merged_dates <- filtered_time_lags %>%
# #   arrange(ID, Date_before_timeLag) %>%
# #   group_split(ID) %>%              # Séparer par ID
# #   map_df(merge_intervals)          # Appliquer la fusion à chaque groupe
# # 
# # # Affichage du résultat
# # print(merged_dates)
# 
# 
# collapse_date_ranges <- function(w, min.gapwidth = 1L) {
#   library(data.table)
#   library(magrittr)
#   IRanges::IRanges(start = as.integer(as.Date(w$Date_before_timeLag, "%y-%m-%d %h:%m:%s")), 
#                    end = as.integer(as.Date(w$Date_after_timeLag, "%y-%m-%d %h:%m:%s"))) %>% 
#     IRanges::reduce(min.gapwidth = min.gapwidth) %>% 
#     as.data.table() %>% 
#     .[, lapply(.SD, lubridate::as_date),
#       .SDcols = c("start", "end")]
# }
# 
# tt <- filtered_time_lags %>% 
#   na.omit()
# aa <- collapse_date_ranges(tt, 0L)

############################ pas interessant 
###########################

# Vérification des chevauchements
# overlap_check <- filtered_time_lags %>%
#   arrange(ID, DateTime) %>%
#   group_by(ID) %>%
#   mutate(overlap = int_overlaps(interval(Date_before_timeLag, Date_after_timeLag),
#                                 interval(lead(Date_before_timeLag), lead(Date_after_timeLag)))) %>%
#   na.omit()


######################
#######################

# overlap_check$line <- c(1:length(overlap_check$ID))
# 
# overlap_check <- overlap_check %>% 
#   arrange(ID, DateTime) %>%
#   group_by(line) %>%
#   mutate(
#     group_1 = sample(1:100000000000, 1))
# 
# overlap_check$group_2 <- cumsum(c(1, diff(overlap_check$overlap) != 0))
# 
# overlap_check <- overlap_check %>% 
#   arrange(ID, DateTime) %>%
#   mutate(group = case_when(overlap == FALSE ~ group_1,
#                            overlap == TRUE ~ group_2))
# 
# 
# 
# 
# overlap_check <- overlap_check %>% 
#   arrange(ID, DateTime) %>%
#   mutate(group_2 = )
# 
# max(table(overlap_check$group))
# 
# group <- as.data.frame(table(overlap_check$group))




#####################
######################

# permet de regrouper les intervalles consécutifs non chevauchants sous un même identifiant de groupe
# overlap_check$group <- cumsum(!overlap_check$overlap) + 1
# 
# # Séparation des gaps non chevauchants
# gaps_non_overlapping <- overlap_check %>% filter(overlap == FALSE) %>%
#   rename(starting_gap = Date_before_timeLag, ending_gap = Date_after_timeLag) %>%
#   dplyr::select(ID, starting_gap, ending_gap)
# 
# # Fusion des gaps chevauchants
# gaps_overlapping <- overlap_check %>%
#   filter(overlap == TRUE) %>%
#   group_by(ID, group) %>%
#   summarise(starting_gap = min(Date_before_timeLag), ending_gap = max(Date_after_timeLag), .groups = "drop") %>%
#   dplyr::select(ID, starting_gap, ending_gap)
# 
# # Union des gaps
# all_gaps <- bind_rows(gaps_non_overlapping, gaps_overlapping)
# all_gaps$n <- seq_len(nrow(all_gaps))
# 
# # Suppression des objets inutiles
# rm(all_trip_stationary_sf, all_trip_stationary_sf_timeLag)

verif_tz(filtered_time_lags, "Date_before_timeLag")
verif_tz(filtered_time_lags, "Date_after_timeLag")
verif_tz(inter_sf, "date")
verif_crs(inter_sf)

# Suppression des points interpolés ---

inter_remove <- inter_sf %>%
  st_drop_geometry()
# 
# remove_i_all <- list()
# 
# filtered_time_lags$n <- seq_len(nrow(filtered_time_lags))
# ind_i = "4025065"
# n = 1



# # Pré-filtrer les données pour éviter des appels répétitifs
# filtered_time_lags_split <- split(filtered_time_lags, filtered_time_lags$ID)
# 
# # Initialisation d'une liste pour stocker les résultats
# ind_i_point_all_list <- list()
# 
# # Boucle optimisée
# for (ind_i in names(filtered_time_lags_split)) {
#   
#   cat("Processing:", ind_i, "\n")
#   
#   # Extraire les données de l'ID courant
#   ind_i_dt <- filtered_time_lags_split[[ind_i]]
#   
#   # Appliquer un filtrage plus performant pour chaque gap unique
#   for (gap in unique(ind_i_dt$n)) {
#     
#     # Déterminer les dates de début et de fin pour le gap
#     start_i_n <- ind_i_dt$Date_before_timeLag[ind_i_dt$n == gap]
#     end_i_n <- ind_i_dt$Date_after_timeLag[ind_i_dt$n == gap]
#     
#     # Filtrage dans inter_remove
#     ind_i_point <- inter_remove %>%
#       filter(id == ind_i & !between(date, start_i_n, end_i_n))
#     
#     # Ajouter les résultats dans la liste
#     ind_i_point_all_list[[length(ind_i_point_all_list) + 1]] <- ind_i_point
#   }
# }
# 
# # Combiner les résultats en un seul data frame
# ind_i_point_all <- bind_rows(ind_i_point_all_list)
# 
# ind_i_point_all_2 <- ind_i_point_all %>% 
#   distinct
# 
# # # Écrire le fichier de sortie
# # write.table(ind_i_point_all, file = paste0(time_lag_path, "to_keep_ind", ind_i, ".csv"),
# #             sep = ";", row.names = FALSE)
# 
# 
# 
















# ind_i = "EA580421"
# 
# 
# 
# # Pré-filtrer les données pour éviter des appels répétitifs
# filtered_time_lags_split <- split(filtered_time_lags, filtered_time_lags$ID)
# 
# # Initialisation d'une liste pour stocker les résultats
# ind_i_point_all_list <- list()
# 
# # Boucle optimisée
# for (ind_i in names(filtered_time_lags_split)) {
#   
#   cat("Processing:", ind_i, "\n")
#   
#   # Extraire les données de l'ID courant
#   ind_i_dt <- filtered_time_lags_split[[ind_i]]
#   
#   # Créer une liste des intervalles de dates pour chaque gap
#   intervals <- lapply(unique(ind_i_dt$n), function(gap) {
#     list(
#       start = ind_i_dt$Date_before_timeLag[ind_i_dt$n == gap],
#       end = ind_i_dt$Date_after_timeLag[ind_i_dt$n == gap]
#     )
#   })
#   
#   # Appliquer le filtre pour exclure les dates qui tombent dans les intervalles
#   ind_i_point <- inter_remove %>%
#     filter(id == ind_i & 
#              !sapply(1:length(intervals), function(i) {
#                between(date, ymd_hms(intervals[[i]]$start), ymd_hms(intervals[[i]]$end))
#              }) %>% 
#              rowSums() == 0)  # Assurez-vous que la date ne tombe dans aucun intervalle
#   
#   # Ajouter les résultats dans la liste
#   ind_i_point_all_list[[length(ind_i_point_all_list) + 1]] <- ind_i_point
# }
# 
# # Combiner les résultats en un seul data frame
# ind_i_point_all <- bind_rows(ind_i_point_all_list)
# 
# length(inter_remove$date)
# length(ind_i_point_all$date)
# 
# (length(inter_remove$date) - length(ind_i_point_all$date))/length(inter_remove$date)*100
# 
# 
# 
# 
# beep(2)


################## ça marche ???????????????????? 24/03/2025
################## ça marche ???????????????????? 24/03/2025
################## ça marche ???????????????????? 24/03/2025

# start.time <- Sys.time()
# 
# library(dplyr)
# library(lubridate)
# library(purrr)
# 
# # Pré-filtrer les données pour éviter des appels répétitifs
# filtered_time_lags_split <- split(filtered_time_lags, filtered_time_lags$ID)
# 
# # Initialisation d'une liste pour stocker les résultats
# ind_i_point_all_list <- list()
# 
# # ind_i = "EA580421"
# 
# # Boucle principale
# for (ind_i in names(filtered_time_lags_split)) {
#   
#   cat("Processing:", ind_i, "\n")
#   
#   # Extraire les données de l'ID courant
#   ind_i_dt <- filtered_time_lags_split[[ind_i]]
#   
#   # Créer une liste des intervalles de dates
#   intervals <- ind_i_dt %>%
#     select(Date_before_timeLag, Date_after_timeLag) %>%
#     distinct() %>%
#     mutate(start = ymd_hms(Date_before_timeLag), end = ymd_hms(Date_after_timeLag)) %>%
#     select(start, end) %>%
#     split(1:nrow(.))  # Convertir en liste de paires start-end
#   
#   # Filtrage pour exclure toutes les dates qui tombent dans au moins un intervalle
#   ind_i_point <- inter_remove %>%
#     filter(id == ind_i) %>%
#     filter(!map_lgl(date, function(d) {
#       any(map_lgl(intervals, ~ between(d, .x$start, .x$end)))
#     }))
#   
#   # Ajouter les résultats dans la liste
#   ind_i_point_all_list[[length(ind_i_point_all_list) + 1]] <- ind_i_point
# }
# 
# # Combiner les résultats en un seul data frame
# ind_i_point_all_not_opti <- bind_rows(ind_i_point_all_list)
# 
# end.time <- Sys.time()
# time.taken <- end.time - start.time
# time.taken

################## ça marche ???????????????????? 24/03/2025
################## ça marche ???????????????????? 24/03/2025
################## ça marche ???????????????????? 24/03/2025


############################### version optimisée
############################### version optimisée
############################### version optimisée

# `%ni%` <- Negate(`%in%`)

# start.time <- Sys.time()
# 
# library(data.table)
# library(lubridate)
# 
# # ✅ 1. Convertir en data.table
# setDT(filtered_time_lags)
# setDT(inter_remove)
# 
# # ✅ 2. Vérifier et renommer les colonnes si nécessaire
# if (!"ID" %in% colnames(filtered_time_lags)) setnames(filtered_time_lags, old = names(filtered_time_lags)[grepl("id", names(filtered_time_lags), ignore.case = TRUE)], new = "ID")
# if (!"ID" %in% colnames(inter_remove)) setnames(inter_remove, old = names(inter_remove)[grepl("id", names(inter_remove), ignore.case = TRUE)], new = "ID")
# 
# # ✅ 3. Conversion des dates
# filtered_time_lags[, Date_before_timeLag := ymd_hms(Date_before_timeLag, quiet = TRUE)]
# filtered_time_lags[, Date_after_timeLag := ymd_hms(Date_after_timeLag, quiet = TRUE)]
# inter_remove[, date := ymd_hms(date, quiet = TRUE)]
# 
# # ✅ 4. Supprimer les valeurs NA
# filtered_time_lags <- filtered_time_lags[!is.na(Date_before_timeLag) & !is.na(Date_after_timeLag)]
# inter_remove <- inter_remove[!is.na(date)]
# 
# # ✅ 5. Créer la table des intervalles avec clé pour foverlaps()
# intervals <- unique(filtered_time_lags[, .(ID, start = Date_before_timeLag, end = Date_after_timeLag)])
# setkey(intervals, ID, start, end)
# 
# # ✅ 6. Préparer les points avec un "intervalle" de 0 seconde
# inter_remove[, `:=`(start = date, end = date)]
# setkey(inter_remove, ID, start, end)
# 
# # ✅ 7. Appliquer foverlaps() pour trouver les dates à exclure
# overlapped <- foverlaps(inter_remove, intervals, by.x = c("ID", "start", "end"), 
#                         by.y = c("ID", "start", "end"), nomatch = 0L)
# 
# # ✅ 8. Exclure les points tombant dans un intervalle
# # inter_remove_filtered <- inter_remove[!inter_remove$start %in% overlapped$start]
# inter_remove_filtered <- anti_join(inter_remove, overlapped)
# 
# # ✅ 9. Résultat final propre
# ind_i_point_all <- inter_remove_filtered[, .(ID, date)]
# 
# 
# 
# end.time <- Sys.time()
# time.taken <- end.time - start.time
# time.taken

############################### version optimisée
############################### version optimisée
############################### version optimisée
# verif <- inter_remove_filtered %>% 
#   filter(ID == "EA580424" &
#            between(date, ymd_hms("2016-11-07 21:42:00"), ymd_hms("2016-11-08 00:13:00")))
# 
# verif



################## optimisé à la main
################## optimisé à la main
################## optimisé à la main

start.time <- Sys.time()

library(data.table)
library(lubridate)
library(dplyr)
library(lubridate)
library(purrr)

# ✅ 1. Convertir en data.table
setDT(filtered_time_lags)
setDT(inter_remove)

# # Pré-filtrer les données pour éviter des appels répétitifs
# filtered_time_lags_split <- split(filtered_time_lags, filtered_time_lags$ID)
# 
# # Initialisation d'une liste pour stocker les résultats
# ind_i_point_all_list <- list()

# ind_i = "EA580421"

# ✅ 5. Créer la table des intervalles avec clé pour foverlaps()
intervals <- unique(filtered_time_lags[, .(ID, start = Date_before_timeLag, end = Date_after_timeLag)])
setkey(intervals, ID, start, end)

# ✅ 6. Préparer les points avec un "intervalle" de 0 seconde
inter_remove <- inter_remove %>% 
  rename(ID = id)
points <- unique(inter_remove[, .(ID, start = date, end = date)])
setkey(points, ID, start, end)

# ✅ 7. Appliquer foverlaps() pour trouver les dates à exclure
overlapped <- foverlaps(intervals, points,
                        by.x = c("ID", "start", "end"), 
                        by.y = c("ID", "start", "end"))

# ✅ 8. Exclure les points tombant dans un intervalle
# inter_remove_filtered <- inter_remove[!inter_remove$start %in% overlapped$start]
# inter_remove_filtered <- anti_join(inter_remove, overlapped)
point_to_remove <- overlapped %>% 
  select(ID, start) %>% 
  distinct()

point_filtered <- anti_join(points, point_to_remove)

# ✅ 9. Résultat final propre
ind_i_point_all <- point_filtered %>% 
  select(ID, start) %>% 
  rename(ID = ID, date = start)

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken


# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# # Boucle principale
# for (ind_i in names(filtered_time_lags_split)) {
#   
#   cat("Processing:", ind_i, "\n")
#   
#   # Extraire les données de l'ID courant
#   ind_i_dt <- filtered_time_lags_split[[ind_i]]
#   
#   # Créer une liste des intervalles de dates
#   intervals <- ind_i_dt %>%
#     select(Date_before_timeLag, Date_after_timeLag) %>%
#     distinct() %>%
#     mutate(start = ymd_hms(Date_before_timeLag), end = ymd_hms(Date_after_timeLag)) %>%
#     select(start, end) %>%
#     split(1:nrow(.))  # Convertir en liste de paires start-end
#   
#   # Filtrage pour exclure toutes les dates qui tombent dans au moins un intervalle
#   ind_i_point <- inter_remove %>%
#     filter(id == ind_i) %>%
#     filter(!map_lgl(date, function(d) {
#       any(map_lgl(intervals, ~ between(d, .x$start, .x$end)))
#     }))
#   
#   # Ajouter les résultats dans la liste
#   ind_i_point_all_list[[length(ind_i_point_all_list) + 1]] <- ind_i_point
# }
# 
# # Combiner les résultats en un seul data frame
# ind_i_point_all_not_opti <- bind_rows(ind_i_point_all_list)

# end.time <- Sys.time()
# time.taken <- end.time - start.time
# time.taken

################## optimisé à la main
################## optimisé à la main
################## optimisé à la main



verif <- ind_i_point_all %>% 
  filter(ID == "EA580424" &
           between(date, ymd_hms("2016-11-07 21:42:00"), ymd_hms("2016-11-08 00:13:00")))

verif


tt <- ind_i_point_all %>% 
  rename(id = ID)


ttt <- left_join(tt, inter_sf) %>% 
  na.omit()

table(ttt$id)


# x = data.table(start=c(5,31,22,16), end=c(8,50,25,18), val2 = 7:10)
# y = data.table(start=c(10, 20, 30), end=c(15, 35, 45), val1 = 1:3)
# setkey(y, start, end)
# foverlaps(x, y, type="any", which=TRUE) ## return overlap indices
# foverlaps(x, y, type="any") ## return overlap join
# foverlaps(x, y, type="any", mult="first") ## returns only first match
# foverlaps(x, y, type="within") ## matches iff 'x' is within 'y'

































# test 

# table(filtered_time_lags$ID)
# table(ind_i_point_all$ID)

#
# tt <- inter_remove %>%
#   filter(id == "EA580421")
# #
# #
# ttt <- inter_remove %>%
#   filter(id == "EA580421" &
#            !between(date, ymd_hms("2016-08-31 10:06:00"), ymd_hms("2016-08-31 10:40:00")))





##################### old pas ok (????)
######################

# # permet de regrouper les intervalles consécutifs non chevauchants sous un même identifiant de groupe
# overlap_check$group <- cumsum(!overlap_check$overlap) + 1
# 
# # Séparation des gaps non chevauchants
# gaps_non_overlapping <- overlap_check %>% filter(overlap == FALSE) %>%
#   rename(starting_gap = Date_before_timeLag, ending_gap = Date_after_timeLag) %>%
#   dplyr::select(ID, starting_gap, ending_gap)
# 
# # Fusion des gaps chevauchants
# gaps_overlapping <- overlap_check %>%
#   filter(overlap == TRUE) %>%
#   group_by(ID, group) %>%
#   summarise(starting_gap = min(Date_before_timeLag), ending_gap = max(Date_after_timeLag), .groups = "drop") %>%
#   dplyr::select(ID, starting_gap, ending_gap)
# 
# # Union des gaps
# all_gaps <- bind_rows(gaps_non_overlapping, gaps_overlapping)
# all_gaps$n <- seq_len(nrow(all_gaps))
# 
# # Suppression des objets inutiles
# rm(all_trip_stationary_sf, all_trip_stationary_sf_timeLag)
# 
# verif_tz(all_gaps, "starting_gap")
# verif_tz(all_gaps, "ending_gap")
# verif_tz(inter_sf, "date")
# verif_crs(inter_sf)
# 
# # Suppression des points interpolés ---
# 
# inter_remove <- inter_sf %>%
#   st_drop_geometry()
# 
# remove_i_all <- list()
# 
# for (ind_i in unique(all_gaps$ID)) {
#   cat("Processing:", ind_i, "\n")
# 
#   ind_i_data <- all_gaps %>% filter(ID == ind_i)
# 
#   remove_i_list <- map(ind_i_data$n, function(n) {
#     start_i_n <- ind_i_data$starting_gap[ind_i_data$n == n]
#     end_i_n <- ind_i_data$ending_gap[ind_i_data$n == n]
#     inter_remove %>% filter(id == ind_i & between(date, start_i_n, end_i_n))
#   })
# 
#   remove_i_dt <- bind_rows(remove_i_list)
#   write.table(remove_i_dt, file = paste0(time_lag_path, "inter_remove_ind_", ind_i, ".csv"),
#               sep = ";", row.names = FALSE)
# }
# 
# # Agrégation des résultats
# files_time_lag <- list.files(path = time_lag_path, pattern = "*.csv", full.names = TRUE)
# dt_time_lag <- lapply(files_time_lag, fread, sep = ";")
# all_time_lag_remove <- rbindlist(dt_time_lag, fill = TRUE)
# 
# verif_tz(all_time_lag_remove, "date")
# 
# # Suppression des points dans le dataset GPS
# dt_base <- inter_sf %>%
#   st_drop_geometry()
# 
# rr <- as.data.frame(all_time_lag_remove$pkey) %>%
#   rename(pkey = `all_time_lag_remove$pkey`)
# 
# df_diff <- anti_join(dt_base, rr)
# 
# # Vérification des longueurs
# cat("Nombre total de points à supprimer:", length(all_time_lag_remove$pkey), "\n")
# cat("Nombre total de points initiaux:", length(dt_base$pkey), "\n")
# cat("Nombre total de points restants:", length(df_diff$pkey), "\n")
# 
# if (length(dt_base$pkey) - length(all_time_lag_remove$pkey) != length(df_diff$pkey)) {
#   beep(2) ; beep(7)
# }
# 
# 
# 
# 
# # Finalisation
# point_no_gap <- left_join(df_diff, inter_sf)
# 
# verif_tz(point_no_gap, "date")
# 



























# test !!!!!!!!!!!!!!!! dernier test vendredi, à voir si ok
# a modifier lundi si non pas ok !
# 9A MARHCE PAS OUF;;;; time lag toujours à faire fonctionner correctement !
# !!!!!!!!!!!!!!!!!!!

# tt <- inter_sf %>% 
#   st_drop_geometry()
# 
# aa <- anti_join(tt, df_diff)
# 
# point_no_gap <- aa













###
####
# MAREE ------------------------------------------------------------------------
####
###

## Prédites ----

# Chargement des données marées
tides <- read_csv("~/Courlis/Data/1) data/Maree/tides.csv")
tides$DateTime <- paste0(tides$y_m_d, " ", tides$time)

tides$sunrise_UTC <- ymd_hms(paste0(tides$y_m_d, tides$sunrise), tz = "Europe/Paris")
tides$sunrise_UTC <- with_tz(tides$sunrise_UTC, "UTC")
tides$sunset_UTC <- ymd_hms(paste0(tides$y_m_d, tides$sunset), tz = "Europe/Paris")
tides$sunset_UTC <- with_tz(tides$sunset_UTC, "UTC")

tides <- tides %>% 
  na.omit() %>% 
  distinct()

tides <- tides %>%
  mutate(date_UTC = as.character(DateTime),  # Convertir si besoin
         date_UTC = if_else(grepl("^\\d{2}-\\d{2}-\\d{4}", date_UTC), 
                            dmy_hms(date_UTC),  # Format DD-MM-YYYY
                            ymd_hms(date_UTC)),
         date_UTC = with_tz(date_UTC, "UTC")) # Format YYYY-MM-DD

tides <- tides %>% 
  mutate(date_rounded = round_date(ymd_hms(date_UTC), "30 mins"))

## Observées ----

# prendre en priorité #3 "validé temps différé", 
# puis #2 "brute temps différé", 
# puis #1 "brute hautes fréquences"  

# remplissage des na dans le marégraphe de l'ile d'aix avec les infos des corrélations 
# en priorité de la rochelle, puis de la cotinière

# Chargement des données marées

# Ile d'aix
maree_path_aix <- paste0(data_path_serveur, "Maree/maregraphie/Ile_d_aix/ok/")
files_maree_aix <- paste0(maree_path_aix, list.files(path = maree_path_aix, pattern = "*.txt"))
dt_maree_aix <- lapply(files_maree_aix, fread, sep = ";")
maree_aix <- rbindlist(dt_maree_aix)
maree_aix$maregraphe <- "aix"

maree_aix_2 <- maree_aix %>% 
  filter(Source !=4) %>% 
  pivot_wider(names_from = Source, values_from = Valeur) %>% 
  rename("valide_temps_diff" = "3", "brute_temps_diff" = "2", "brute_haute_freq" = "1") %>% 
  mutate(hauteur_eau = coalesce(valide_temps_diff, brute_temps_diff, brute_haute_freq)) %>% 
  select(Date, maregraphe, hauteur_eau) %>% 
  distinct()

# La cotiniere
maree_path_coti <- paste0(data_path_serveur, "Maree/maregraphie/La_cotiniere/ok/")
files_maree_coti <- paste0(maree_path_coti, list.files(path = maree_path_coti, pattern = "*.txt"))
dt_maree_coti <- lapply(files_maree_coti, fread, sep = ";")
maree_coti <- rbindlist(dt_maree_coti)
maree_coti$maregraphe <- "cotiniere"

maree_coti_2 <- maree_coti %>% 
  rename("brute_haute_freq" = "Source", "hauteur_eau" = "Valeur") %>% 
  select(Date, maregraphe, hauteur_eau) %>% 
  distinct()

# La Rochelle
maree_path_roch <- paste0(data_path_serveur, "Maree/maregraphie/La_rochelle/ok/")
files_maree_roch <- paste0(maree_path_roch, list.files(path = maree_path_roch, pattern = "*.txt"))
dt_maree_roch <- lapply(files_maree_roch, fread, sep = ";")
maree_roch <- rbindlist(dt_maree_roch)
maree_roch$maregraphe <- "rochelle"

maree_roch_2 <- maree_roch %>% 
  filter(Source !=4) %>% 
  pivot_wider(names_from = Source, values_from = Valeur) %>% 
  rename("valide_temps_diff" = "3", "brute_temps_diff" = "2", "brute_haute_freq" = "1") %>% 
  mutate(hauteur_eau = coalesce(valide_temps_diff, brute_temps_diff, brute_haute_freq)) %>% 
  select(Date, maregraphe, hauteur_eau) %>% 
  distinct()

# all maregraphes
maree <- rbind(maree_aix_2, maree_coti_2, maree_roch_2) %>% 
  na.omit()

maree2 <- maree %>% 
  mutate(date_2 = gsub("/", "-", Date))

maree3 <- maree2 %>%
  mutate(date_UTC = as.character(date_2),  # Convertir si besoin
         date_UTC = if_else(grepl("^\\d{2}-\\d{2}-\\d{4}", date_UTC), 
                            dmy_hms(date_UTC),  # Format DD-MM-YYYY
                            ymd_hms(date_UTC)),
         date_UTC = with_tz(date_UTC, "UTC")) # Format YYYY-MM-DD

str(maree3$date_UTC)  # Vérifie le type
tz(maree3$date_UTC)

maree4 <- maree3 %>% 
  mutate(date_rounded = round_date(date_UTC, "30 mins"))

# comparaison entre marégraphe
maree_sum <- maree4 %>%  
  group_by(maregraphe, date_rounded) %>% 
  summarise(mean_hauteur_eau = mean(hauteur_eau))

maree_spread <- maree_sum %>% 
  pivot_wider(names_from = maregraphe, values_from = c(mean_hauteur_eau))

model_aix_roch <- lm(maree_spread$aix ~ maree_spread$rochelle)
model_aix_coti <- lm(maree_spread$aix ~ maree_spread$cotiniere)

# estimation des prédictions selon la corrélation aix ~ rochelle et aix ~ cotiniere
maree_spread <- maree_spread %>% 
  mutate(pred_aix_roch = aix*model_aix_roch$coefficients[2] + model_aix_roch$coefficients[1],
         pred_aix_coti = aix*model_aix_coti$coefficients[2] + model_aix_coti$coefficients[1])

# selection de la valeur observé, puis la prédiction via la rochelle, puis via cotiniere
maree_correl <- maree_spread %>% 
  mutate(hauteur_eau_and_pred = coalesce(aix, pred_aix_roch, pred_aix_coti)) %>% 
  select(date_rounded, hauteur_eau_and_pred) %>% 
  distinct()

## Prédites et observées ---- 

tides <- left_join(tides, maree_correl)

tides <- tides %>% 
  dplyr::rename(mean_height_obs = hauteur_eau_and_pred)

tides <- tides %>%
  mutate(high_type = case_when(
    type == "High" & mean_height_obs <= 5 ~ "mortes_eaux",
    type == "High" & between(mean_height_obs, 5, 6.3) ~ "vives_eaux",
    type == "High" & mean_height_obs >= 6.3 ~ "submersion" # 6.9
  ))

table(tides$high_type)

###
####
# BEHAVIORS --------------------------------------------------------------------
####
###
point_no_gap <- ttt

# point_no_gap <- ind_i_point_all

# point_no_gap <- inter_sf

# point_no_gap <- read.table(paste0(data_generated_path_serveur, "point_no_gap.txt"),
#                            header = TRUE, sep = ";")

# point_no_gap <- point_no_gap %>%
#   dplyr::select(-geometry, -pkey)

# point_no_gap <- point_no_gap %>%
#   dplyr::select(-geometry)

# foraging : 2h avant-après la marée base 
# roosting : 2h avant-après la marée haute 

# Coordinated Universal Time

# changed
# verif_tz(point_no_gap$date)

point_no_gap$date_UTC <- point_no_gap$date
# !!!!!!!!!!!!!!!!!! date dajà en time zone UTC, donc pas besoin de la mettre en UE + UTC ???
# point_no_gap$date_UTC <- ymd_hms(point_no_gap$date, tz = "Europe/Paris")
# point_no_gap$date_UTC <- with_tz(point_no_gap$date_UTC, "UTC")
# tz(point_no_gap$date)
# tz(point_no_gap$date_UTC)

# Attribution d'un index unique aux dates
tides <- tides %>% 
  filter(y_m_d >= min(point_no_gap$date)) %>%
  distinct() %>% 
  mutate(t = dense_rank(y_m_d)) # attribue une valeur croissante aux dates

behaviour_dt_1 <- NULL

max_i <- max(tides$t) ; max_i

i = 1 ; n = 1

# Boucle sur chaque date unique des marées
for (i in unique(tides$t)) {
  
  print(i)
  
  # Pour la marée i
  dt_i <- filter(tides, t == i)
  
  # Séparation en marée basse et haute
  dt_i_low <- dt_i %>% filter(type == "Low") %>% mutate(n = row_number())
  dt_i_high <- dt_i %>% filter(type == "High") %>% mutate(n = row_number())
  
  # Traitement des marées basses
  for (n in unique(dt_i_low$n)) {
    time_i_n <- ymd_hms(dt_i_low$date_UTC[dt_i_low$n == n]) # time
    foraging_period <- time_i_n + c(-2, 2) * 3600 # period +/- 2h
    height_low_i_n <- dt_i_low$mean_height_obs[dt_i_low$n == n] # hauteur d'eau
    type_maree_low_i_n <- "basse" # type de marée
    
    all_info_low <- point_no_gap %>%
      filter(between(date_UTC, foraging_period[1], foraging_period[2])) %>%
      mutate(behavior = "foraging",
             height_obs = height_low_i_n,
             type_maree = type_maree_low_i_n) #%>%

    if (nrow(all_info_low) > 0) {
      behaviour_dt_1 <- bind_rows(behaviour_dt_1, mutate(all_info_low, i = i, n = n))
    } else {
      print("No Data Available")
    }
  }
  
  # Traitement des marées hautes
  for (n in unique(dt_i_high$n)) {
    time_i_n <- ymd_hms(dt_i_high$date_UTC[dt_i_high$n == n])
    roosting_period <- time_i_n + c(-2, 2) * 3600
    height_high_i_n <- dt_i_high$mean_height_obs[dt_i_high$n == n]
    type_maree_high_i_n <- dt_i_high$high_type[dt_i_high$n == n]
    
    all_info_high <- point_no_gap %>%
      filter(between(date_UTC, roosting_period[1], roosting_period[2])) %>%
      mutate(behavior = "roosting",
             height_obs = height_high_i_n,
             type_maree = type_maree_high_i_n)
    
    if (nrow(all_info_high) > 0) {
      behaviour_dt_1 <- bind_rows(behaviour_dt_1, mutate(all_info_high, i = i, n = n))
    } else {
      print("No Data Available")
    }
  }
}

# Sauvegarde des résultats
behaviour_dt_1 <- behaviour_dt_1 %>% 
  arrange(date_UTC)

# Conversion en objet sf avec projection WGS84 (EPSG:4326)
behaviour_dt_1_spa <- st_as_sf(behaviour_dt_1, coords = c("lon", "lat"), crs = 4326)

# Restauration explicite des colonnes longitude et latitude
behaviour_dt_1_spa$lon <- behaviour_dt_1$lon
behaviour_dt_1_spa$lat <- behaviour_dt_1$lat

length(behaviour_dt_1_spa$lat)

###
####
# 1000 POINTS & 56 JOURS -----------------------------------------------------
####
###

behaviour_jour_nuit <- behaviour_dt_1_spa

# Filtrage basé sur le nombre de points et la durée minimale de suivi
behaviour_24h_BOX_1000_56 <- behaviour_jour_nuit %>%
  group_by(id) %>%
  mutate(
    nb_point = n(),
    nb_days = as.numeric(difftime(max(date), min(date), units = "days"))
  ) %>%
  filter(nb_point >= 1000, nb_days >= 56)

# Nombre d'individus restant après filtrage
behaviour_24h_nb_ind_1000_56 <- n_distinct(behaviour_24h_BOX_1000_56$id)
print(behaviour_24h_nb_ind_1000_56)

###
####
# VISUALISATION ----------------------------------------------------------------
####
###

crs(behaviour_24h_BOX_1000_56)
crs(dept_BOX)
crs(RMO_4326)

behaviour_24h_BOX_1000_56_sex_age <- behaviour_24h_BOX_1000_56

RMO_4326 <- st_transform(RMO, crs = 4326)

# Conversion en objet sf avec projection WGS84 (EPSG:4326)
behaviour_24h_BOX_1000_56_sex_age <- st_as_sf(behaviour_24h_BOX_1000_56_sex_age, coords = c("lon", "lat"), crs = 4326)
crs(behaviour_24h_BOX_1000_56_sex_age)

table(behaviour_24h_BOX_1000_56_sex_age$id)
length(table(behaviour_24h_BOX_1000_56_sex_age$id))

tmap_mode("plot")
tmap_plot_behavior <- tm_scalebar() +
  tm_shape(dept_BOX) +
  tm_polygons() +
  tm_shape(behaviour_24h_BOX_1000_56_sex_age) +
  # tm_dots(col = 'id', fill_alpha = 0.5) +
  tm_dots(fill_alpha = 0.5) +
  tm_facets(by = "behavior", free.coords = FALSE) +
  # tmap_options(max.categories = 70) +
  tm_shape(RMO_4326) +
  tm_borders(col = "black") ; tmap_plot_behavior

beep(3)

