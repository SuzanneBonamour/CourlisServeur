
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
library(purrr)

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

data_path_serveur <- "D:/Projets_Suzanne/Courlis/3) Data/1) data/"
data_generated_path_serveur <- "D:/Projets_Suzanne/Courlis/3) Data/2) data_generated/"
data_image_path_serveur <- "D:/Projets_Suzanne/Courlis/3) Data/3) images/"
time_lag_path <- "D:/Projets_Suzanne/Courlis/3) Data/2) data_generated/time_lag/"

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

# /!\ /!\ /!\ SAVE /!\ /!\ /!\ 
# /!\ /!\ /!\ SAVE /!\ /!\ /!\ -------------------------------------------------
# /!\ /!\ /!\ SAVE /!\ /!\ /!\ 

write.csv(all_gps, file = paste0(data_generated_path_serveur, "all_gps.csv"))
all_gps <- read.csv(file = paste0(data_generated_path_serveur, "all_gps.csv"))

all_gps$time <- as.POSIXct(all_gps$time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
verif_tz(all_gps, "time")

###
####
# NB ---------------------------------------------------------------------------
####
###

# nb point GPS
length(unique(all_gps$eventID))

# Nb ind
length(unique(all_gps$indID))

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

all_gps$time <- as.POSIXct(all_gps$time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
verif_tz(all_gps, "time")

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

all_gps_spa_BOX$time <- as.POSIXct(all_gps_spa_BOX$time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
verif_tz(all_gps_spa_BOX, "time")

# tmap_mode("plot")
# map <- tm_scalebar() +
#   tm_shape(dept_BOX) +
#   tm_polygons() +
#   tm_shape(all_gps_spa_BOX) +
#   tm_dots(fill_alpha = 0.5) +
#   tm_shape(RMO_4326) +
#   tm_borders(col = "red"); map

# /!\ /!\ /!\ SAVE /!\ /!\ /!\ 
# /!\ /!\ /!\ SAVE /!\ /!\ /!\ -------------------------------------------------
# /!\ /!\ /!\ SAVE /!\ /!\ /!\  

# write
st_write(all_gps_spa_BOX, paste0(data_generated_path_serveur, "all_gps_spa_BOX.gpkg"), append = FALSE)
# read
all_gps_spa_BOX <- st_read(file.path(data_generated_path_serveur, "all_gps_spa_BOX.gpkg"))

verif_tz(all_gps_spa_BOX, "time")
all_gps_spa_BOX$time <- as.POSIXct(all_gps_spa_BOX$time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
verif_tz(all_gps_spa_BOX, "time")

###
####
# NB ---------------------------------------------------------------------------
####
###

# nb point GPS
length(unique(all_gps_spa_BOX$eventID))

# Nb ind
length(unique(all_gps_spa_BOX$indID))

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
tz(all_gps_spa$time)
all_gps_spa$time <- as.POSIXct(all_gps_spa$time, tz = "UTC")
verif_tz(all_gps_spa, "time")

# Conversion en data frame et suppression de la colonne géométrique
all_gps_dt <- all_gps_spa %>%
  as.data.frame() %>%
  dplyr::mutate(ID = indID) %>%
  dplyr::select(-geom) %>% 
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

all_trip <- all_gps_dt_2 %>% 
  group_by(ID) %>% 
  trip()

table(all_trip$ID)

verif_tz(all_trip, "DateTime")

all_trip$DateTime

# jusqu'ici good 
# jusqu'ici good 
# jusqu'ici good 
# jusqu'ici good 
# jusqu'ici good 
# jusqu'ici good 

###
####
# STATIONARY -------------------------------------------------------------------
####
###

# Filtrage des points stationnaires avec une vitesse maximale
all_trip$stationary <- speedfilter(all_trip, max.speed = 1)  # vitesse en km/h
all_trip$DateTime
summary(all_trip$stationary) # Vérification des points supprimés
verif_tz(all_trip, "DateTime")

# small <- all_trip[c(1:100000),]
# 
# # Filtrage des points stationnaires avec une vitesse maximale
# small$stationary <- speedfilter(small, max.speed = 0.5)  # vitesse en km/h
# summary(small$stationary) # Vérification des points supprimés
# small$DateTime
# verif_tz(small, "DateTime")
# 
# all_trip <- small

# Conversion en objet sf
all_trip_stationary_sf <- st_as_sf(all_trip)
all_trip_stationary_sf <- st_transform(all_trip_stationary_sf, crs = 4326)

verif_tz(all_trip_stationary_sf, "DateTime")
verif_crs(all_trip_stationary_sf)

# Sélection des points valides avec une vitesse inférieure ou égale à la vitesse maximale km/h
all_trip_stationary_sf <- all_trip_stationary_sf %>% 
  filter(stationary == TRUE) %>% 
  dplyr::select(-stationary)

# Extraction des coordonnées longitude et latitude
all_trip_stationary_sf <- all_trip_stationary_sf %>%
  mutate(lon = st_coordinates(.)[,1], lat = st_coordinates(.)[,2])

verif_crs(all_trip_stationary_sf)

verif_tz(all_trip_stationary_sf, "DateTime")
# all_trip_stationary_sf$DateTime <- as.POSIXct(all_trip_stationary_sf$DateTime, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# verif_tz(all_trip_stationary_sf, "DateTime")

# /!\ /!\ /!\ SAVE /!\ /!\ /!\ 
# /!\ /!\ /!\ SAVE /!\ /!\ /!\ -------------------------------------------------
# /!\ /!\ /!\ SAVE /!\ /!\ /!\  

# write
st_write(all_trip_stationary_sf, paste0(data_generated_path_serveur, "all_trip_stationary_sf.gpkg"), append = FALSE)
# read
all_trip_stationary_sf <- st_read(file.path(data_generated_path_serveur, "all_trip_stationary_sf.gpkg"))

verif_tz(all_trip_stationary_sf, "DateTime")
all_trip_stationary_sf$DateTime <- as.POSIXct(all_trip_stationary_sf$DateTime, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
verif_tz(all_trip_stationary_sf, "DateTime")

# all good
# all good
# all good
# all good

###
####
# INTERPOLATION ----------------------------------------------------------------
####
###

all_stationary.ltraj <- as.ltraj(
  xy = bind_cols(x = all_trip_stationary_sf$lon, y = all_trip_stationary_sf$lat),
  date = all_trip_stationary_sf$DateTime,
  id = all_trip_stationary_sf$ID
)

# Re-échantillonnage des trajectoires
all_stationary.interp <- redisltraj(all_stationary.ltraj, 60*5, type = "time")

# Conversion en data frame avec renommer des colonnes pour clarté
all_stationary.interp <- ld(all_stationary.interp) %>% 
  rename(longitude = x, latitude = y)

# Conversion en objet sf (Spatial Feature)
inter_sf <- st_as_sf(all_stationary.interp, coords = c("longitude", "latitude"), crs = 4326)

inter_sf <- inter_sf %>%
  mutate(lon = st_coordinates(.)[,1], lat = st_coordinates(.)[,2]) %>% 
  dplyr::select(date, id, pkey, geometry, lon, lat)

verif_crs(inter_sf)

verif_tz(inter_sf, "date")
inter_sf$date <- as.POSIXct(inter_sf$date, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
verif_tz(inter_sf, "date")

# /!\ /!\ /!\ SAVE /!\ /!\ /!\ 
# /!\ /!\ /!\ SAVE /!\ /!\ /!\ -------------------------------------------------
# /!\ /!\ /!\ SAVE /!\ /!\ /!\  

# write
st_write(inter_sf, paste0(data_generated_path_serveur, "inter_sf.gpkg"), append = FALSE)
# read
inter_sf <- st_read(file.path(data_generated_path_serveur, "inter_sf.gpkg"))

verif_tz(inter_sf, "date")
inter_sf$date <- as.POSIXct(inter_sf$date, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
verif_tz(inter_sf, "date")

###
####
# TIME GAP ---------------------------------------------------------------------
####
###

# pour identifier les trous de plus de x min dans les enregistrements GPS

# Paramètres
max_time_lag <- 5

# Calcul des intervalles de temps
all_trip_stationary_sf_timeLag <- all_trip_stationary_sf %>%
  group_by(ID) %>%
  arrange(ID, DateTime) %>%
  mutate(timeLag = as.numeric(difftime(DateTime, lag(DateTime), units = "mins")))

verif_tz(all_trip_stationary_sf, "DateTime")

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

verif_crs(inter_sf)
verif_tz(filtered_time_lags, "Date_before_timeLag")
# filtered_time_lags$Date_before_timeLag <- as.POSIXct(filtered_time_lags$Date_before_timeLag, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
verif_tz(filtered_time_lags, "Date_before_timeLag")

# filtered_time_lags$Date_after_timeLag <- as.POSIXct(filtered_time_lags$Date_after_timeLag, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
verif_tz(filtered_time_lags, "Date_after_timeLag")

# Suppression des points interpolés ---

inter_remove <- inter_sf %>%
  st_drop_geometry()

# ✅ 1. Convertir en data.table
setDT(filtered_time_lags)
setDT(inter_remove)

# ✅ 5. Créer la table des intervalles avec clé pour foverlaps()
intervals <- unique(filtered_time_lags[, .(ID, start = Date_before_timeLag, end = Date_after_timeLag)])
setkey(intervals, ID, start, end)

# ✅ 6. Préparer les points avec un "intervalle" de 0 seconde
inter_remove <- inter_remove %>% 
  rename(ID = id)

points <- unique(inter_remove[, .(ID, start = date, end = date)])
setkey(points, ID, start, end)

# ✅ 7. Appliquer foverlaps() pour trouver les dates à exclure
verif_tz(points, "start")
verif_tz(intervals, "start")

overlapped <- foverlaps(intervals, points,
                        by.x = c("ID","start","end"), 
                        by.y = c("ID","start","end"))

# ✅ 8. Exclure les points tombant dans un intervalle
point_to_remove <- overlapped %>% 
  mutate(to_do = "remove") %>% 
  dplyr::select(ID, start, to_do) %>% 
  distinct()

point_filtered <- left_join(points, point_to_remove)

table(point_filtered$to_do, useNA = "always")

# ✅ 9. Résultat final propre
ind_i_point_all <- point_filtered %>% 
  dplyr::select(ID, start, to_do) %>% 
  rename(ID = ID, date = start) %>% 
  filter(is.na(to_do))

# verif <- ind_i_point_all %>% 
#   filter(ID == "EC103792" &
#            between(date, ymd_hms("2019-10-18 11:13:33"), ymd_hms("2019-10-18 11:23:00")))
# 
# verif

tt <- ind_i_point_all %>% 
  rename(id = ID)

ttt <- left_join(tt, inter_sf) #%>% 
  # na.omit()

table(ttt$id)

point_no_gap <- ttt

# point_no_gap$date <- as.POSIXct(point_no_gap$date, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
verif_tz(point_no_gap, "date")


# #################
# 
# max_time_lag <- 5 # minutes
# 
# # Trouver les intervalles avec trop de temps entre deux points
# gap_intervals <- all_trip_stationary_sf %>%
#   st_drop_geometry() %>%  # 🔹 dès le début
#   arrange(ID, DateTime) %>%
#   group_by(ID) %>%
#   mutate(
#     next_time = lead(DateTime),
#     time_diff = as.numeric(difftime(next_time, DateTime, units = "mins"))
#   ) %>%
#   filter(time_diff > max_time_lag) %>%
#   transmute(
#     ID,
#     start = DateTime,
#     end = next_time
#   ) %>%
#   drop_na()
# 
# # Enlever la géométrie si besoin
# inter_remove <- inter_sf %>%
#   st_drop_geometry() %>%
#   rename(ID = id) # harmoniser les noms
# 
# # Convertir en data.table si ce n'est pas déjà fait
# setDT(gap_intervals)
# setDT(points)
# 
# # Forcer timezone UTC sur dates
# gap_intervals[, start := as.POSIXct(start, tz = "UTC")]
# gap_intervals[, end := as.POSIXct(end, tz = "UTC")]
# points[, start := as.POSIXct(start, tz = "UTC")]
# points[, end := as.POSIXct(end, tz = "UTC")]
# 
# attr(points$start, "tzone")         # devrait afficher "UTC"
# attr(gap_intervals$start, "tzone") # idem
# 
# # Puis faire le setkey
# setkey(gap_intervals, ID, start, end)
# setkey(points, ID, start, end)
# 
# # Trouver les points dans les intervalles problématiques
# overlapping_points <- foverlaps(points, gap_intervals, type = "within", nomatch = 0L)
# 
# # Supprimer les points concernés
# points_to_keep <- anti_join(points, overlapping_points, by = c("ID", "start" = "start"))
# 
# # Si tu veux garder que les bons points avec géométrie
# inter_filtered <- inter_sf %>%
#   rename(ID = id) %>%
#   inner_join(points_to_keep, by = c("ID", "date" = "start"))
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
# 
# 
# ##################
# 
# 
# 
# 
# 
# max_time_lag <- 5 # minutes
# 
# # 1. Trouver les intervalles à gap > max_time_lag
# gap_intervals <- all_trip_stationary_sf %>%
#   st_drop_geometry() %>%
#   arrange(ID, DateTime) %>%
#   group_by(ID) %>%
#   mutate(
#     next_time = lead(DateTime),
#     time_diff = as.numeric(difftime(next_time, DateTime, units = "mins"))
#   ) %>%
#   filter(time_diff > max_time_lag) %>%
#   transmute(ID, start = DateTime, end = next_time) %>%
#   drop_na()
# 
# verif_tz(gap_intervals, "DateTime")
# gap_intervals$DateTime <- as.POSIXct(gap_intervals$DateTime, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# verif_tz(gap_intervals, "DateTime")
# 
# # 2. Préparer points pour foverlaps
# inter_remove <- inter_sf %>%
#   st_drop_geometry() %>%
#   rename(ID = id) %>%
#   mutate(start = date, end = date)
# 
# # Renommer ici lors de la préparation
# point_dt <- inter_remove %>%
#   mutate(start = date, end = date) %>%
#   as.data.table()
# 
# # Puis tu peux continuer avec :
# setkey(point_dt, ID, start, end)
# 
# # Et pour foverlaps
# overlapping_points <- foverlaps(point_dt, gap_intervals, type = "within", nomatch = 0L)
# points_to_remove <- overlapping_points[, .(ID, start)]
# points_to_keep <- point_dt[!points_to_remove, on = .(ID, start)]
# 
# 
# 
# 
# 
# 
# verif_tz(points, "start")
# # points$start <- as.POSIXct(points$start, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# # verif_tz(points, "start")
# 
# # 5. Convertir en data.table
# setDT(gap_intervals)
# setDT(points)
# 
# # 3. Convertir ID en caractère
# gap_intervals <- gap_intervals %>%
#   mutate(ID = as.character(ID))
# points[, ID := as.character(ID)]
# 
# # 4. Forcer dates en POSIXct UTC (à adapter selon ton cas)
# # gap_intervals[, start := as.POSIXct(start, tz = "UTC")]
# # gap_intervals[, end := as.POSIXct(end, tz = "UTC")]
# # points[, start := as.POSIXct(start, tz = "UTC")]
# # points[, end := as.POSIXct(end, tz = "UTC")]
# 
# # 6. Définir clé
# setkey(gap_intervals, ID, start, end)
# setkey(points, ID, start, end)
# 
# # 7. Trouver points dans gaps
# # Résultat de foverlaps : tous les points tombant dans des intervalles à exclure
# overlapping_points <- foverlaps(points, gap_intervals, type = "within", nomatch = 0L)
# 
# # Étape 1 : on extrait uniquement ID + start des points à retirer
# points_to_remove <- overlapping_points[, .(ID, start)]
# 
# # Étape 2 : on filtre les points d’origine
# points_to_keep <- points[!points_to_remove, on = .(ID, start)]
# 
# 
# 
# 
# 
# 
# verif_tz(overlapping_points, "start")
# 
# common_cols <- intersect(colnames(points), colnames(overlapping_points))
# points_sub <- points[, ..common_cols]
# overlapping_points_sub <- overlapping_points[, ..common_cols]
# 
# # 8. Exclure points dans gaps
# points_to_keep <- fsetdiff(points_sub, overlapping_points_sub, all=TRUE)
# 
# verif_tz(points_to_keep, "start")
# 
# # 9. Rejoindre avec géométrie
# inter_filtered <- inter_sf %>%
#   mutate(ID = as.character(id)) %>%
#   inner_join(points_to_keep[, .(ID, start)], by = c("ID", "date" = "start"))
# 
# verif_tz(inter_filtered, "date")
# 
# ############################
# #############################
# 
# library(dplyr)
# library(data.table)
# library(sf)
# 
# # ---- Paramètre ----
# max_time_lag <- 5 # minutes
# 
# # ---- 1. Détection des intervalles à faible densité temporelle ----
# gap_intervals <- all_trip_stationary_sf %>%
#   st_drop_geometry() %>%
#   arrange(ID, DateTime) %>%
#   group_by(ID) %>%
#   mutate(
#     next_time = lead(DateTime),
#     time_diff = as.numeric(difftime(next_time, DateTime, units = "mins"))
#   ) %>%
#   filter(time_diff > max_time_lag) %>%
#   transmute(
#     ID,
#     start = as.POSIXct(DateTime, tz = "UTC"),
#     end   = as.POSIXct(next_time, tz = "UTC")
#   ) %>%
#   drop_na()
# 
# # ---- 2. Préparation des points ----
# point_dt <- inter_sf %>%
#   st_drop_geometry() %>%
#   rename(ID = id) %>%
#   mutate(
#     start = as.POSIXct(date, tz = "UTC"),
#     end   = as.POSIXct(date, tz = "UTC")
#   ) %>%
#   as.data.table()
# 
# # ---- 3. Conversion & clé pour data.table ----
# setDT(gap_intervals)
# setDT(point_dt)
# 
# gap_intervals[, ID := as.character(ID)]
# point_dt[, ID := as.character(ID)]
# 
# setkey(gap_intervals, ID, start, end)
# setkey(point_dt, ID, start, end)
# 
# # ---- 4. Overlap : trouver les points dans les "gaps" ----
# overlapping_points <- foverlaps(point_dt, gap_intervals, type = "within", nomatch = 0L)
# 
# # ---- 5. Filtrer les points : on garde ceux hors des gaps ----
# # points_to_remove <- overlapping_points[, .(ID, start)]
# points_to_remove <- overlapping_points[, .(ID, i.start)]
# points_to_remove <- points_to_remove %>% 
#   rename(start = i.start)
# points_to_keep   <- point_dt[!points_to_remove, on = .(ID, start)]
# 
# # ---- 6. Joindre pour retrouver la géométrie ----
# inter_filtered <- inter_sf %>%
#   rename(ID = id) %>%
#   mutate(date = as.POSIXct(date, tz = "UTC")) %>%
#   inner_join(points_to_keep, by = c("ID", "date" = "start"))
# 
# # ---- 7. Vérifications optionnelles ----
# table(format(points_to_remove$start, "%H:%M:%S"))  # Pour voir si toutes à minuit
# attr(inter_filtered$date, "tzone")  # Doit être UTC
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
# ##########################
# #######################
# 
# # 1. gap_intervals
# gap_intervals <- all_trip_stationary_sf %>%
#   st_drop_geometry() %>%
#   arrange(ID, DateTime) %>%
#   group_by(ID) %>%
#   mutate(
#     next_time = lead(DateTime),
#     time_diff = as.numeric(difftime(next_time, DateTime, units = "mins"))
#   ) %>%
#   filter(time_diff > max_time_lag) %>%
#   transmute(
#     ID,
#     interval_start = as.POSIXct(DateTime, tz = "UTC"),
#     interval_end   = as.POSIXct(next_time, tz = "UTC")
#   ) %>%
#   drop_na()
# 
# # 2. points avec row_id
# inter_sf <- inter_sf %>%
#   mutate(row_id = row_number())
# 
# point_dt <- inter_sf %>%
#   st_drop_geometry() %>%
#   rename(ID = id) %>%
#   mutate(
#     point_time = as.POSIXct(date, tz = "UTC")
#   ) %>%
#   transmute(ID, point_start = point_time, point_end = point_time, row_id)
# 
# # 3. Convert to data.table
# setDT(gap_intervals)
# setDT(point_dt)
# 
# # 4. Renommer les colonnes pour éviter conflits
# setnames(gap_intervals, c("interval_start", "interval_end"), c("start", "end"))
# setnames(point_dt, c("point_start", "point_end"), c("start", "end"))
# 
# # 5. Set keys
# gap_intervals[, ID := as.character(ID)]
# point_dt[, ID := as.character(ID)]
# 
# setkey(gap_intervals, ID, start, end)
# setkey(point_dt, ID, start, end)
# 
# # 6. foverlaps en toute sécurité
# overlapping_points <- foverlaps(point_dt, gap_intervals, type = "within", nomatch = 0L)
# 
# # 7. Extraire les points à retirer
# rows_to_remove <- overlapping_points$row_id
# 
# # 8. Filtrage final
# inter_filtered <- inter_sf %>%
#   filter(!row_id %in% rows_to_remove)
# 
# 
# # devraient être exclus
# # 1er gap
# # 4025065 2016-07-11 00:00:01 2016-08-02 00:02:06
# # point qui ne doit pas être la 
# # 2016-07-11 00:05:00 4025065 4025065.2016-07-11 02:05:00
# 
# 
# head(point_dt$start)
# head(points_to_remove$start)
# 
# # Format précis
# unique(format(point_dt$start, "%Y-%m-%d %H:%M:%S"))
# unique(format(points_to_remove$start, "%Y-%m-%d %H:%M:%S"))
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
# 
# table(overlapping_points$ID)
# table(inter_sf$id)
# table(gap_intervals$ID)

# /!\ /!\ /!\ SAVE /!\ /!\ /!\ 
# /!\ /!\ /!\ SAVE /!\ /!\ /!\ -------------------------------------------------
# /!\ /!\ /!\ SAVE /!\ /!\ /!\  

# write
st_write(point_no_gap, paste0(data_generated_path_serveur, "point_no_gap.gpkg"), append = FALSE)
# read
point_no_gap <- st_read(file.path(data_generated_path_serveur, "point_no_gap.gpkg"))

verif_tz(point_no_gap, "date")
point_no_gap$date <- as.POSIXct(point_no_gap$date, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
verif_tz(point_no_gap, "date")

###
####
# BEHAVIORS --------------------------------------------------------------------
####
###

# les points GPS ---
point_no_gap$date_UTC <- point_no_gap$date
verif_tz(point_no_gap, "date_UTC")
 
# les infos de maree ---
tides <- read_csv("D:/Projets_Suzanne/Courlis/3) Data/1) data/Maree/tides_donnees_complete.csv")

# timer
start.time <- Sys.time()

# ✅ 1. Convertir en data.table
setDT(tides)
setDT(point_no_gap)

# ✅ 5. Créer la table des intervalles avec clé pour foverlaps()
deux_heures <- 3600*2
intervals_tides <- unique(tides[, .(type, height, start = DateTime-deux_heures, end = DateTime+deux_heures)])
setkey(intervals_tides, type, start, end)
ID_list <- unique(point_no_gap$id)

# Répéter le dataframe pour chaque ID de la liste
intervals_tides <- map_dfr(ID_list, ~ intervals_tides %>% 
                           mutate(ID = .x))
intervals_tides <- intervals_tides %>% 
  dplyr::select(ID, type, height, start, end)

# ✅ 6. Préparer les points avec un "intervalle" de 0 seconde
point_dt <- point_no_gap %>% 
  rename(ID = id)
point_dt <- unique(point_dt[, .(ID, start = date_UTC, end = date_UTC)])
setkey(point_dt, ID, start, end)

# ✅ 7. Appliquer foverlaps() pour trouver les dates à exclure
overlapped_tides_points <- foverlaps(intervals_tides, point_dt,
                                     by.x = c("ID","start", "end"), 
                                     by.y = c("ID","start", "end"))
overlapped_tides_points_2 <- overlapped_tides_points %>% 
  na.omit()

table(overlapped_tides_points_2$ID)
table(point_dt$ID)

# ✅ 8. Essocier le bon comportement à tous les points 
overlapped_tides_points_2 <- overlapped_tides_points_2 %>%
  dplyr::select(ID, type, height, start)

overlapped_tides_points_3 <- left_join(point_dt, overlapped_tides_points_2)

behavior_dt <- overlapped_tides_points_3 %>% 
  mutate(behavior = case_when(type == "Low" ~ "foraging",
                              type == "High" ~ "roosting",
                              is.na(type) ~ "other")) 

# ✅ 8 On recolle toutes les autres infos 

behavior_dt <- behavior_dt %>% 
  rename(date = start)

all_other_info <- inter_sf %>% 
  rename(ID = id)

behavior_dt_final <- left_join(behavior_dt, all_other_info)

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

# /!\ /!\ /!\ SAVE /!\ /!\ /!\ 
# /!\ /!\ /!\ SAVE /!\ /!\ /!\ -------------------------------------------------
# /!\ /!\ /!\ SAVE /!\ /!\ /!\  

# write
st_write(behavior_dt_final, paste0(data_generated_path_serveur, "behavior_dt_final.gpkg"), append = FALSE)
# read
behavior_dt_final <- st_read(file.path(data_generated_path_serveur, "behavior_dt_final.gpkg"))

table(behavior_dt_final$behavior)

###
####
# 1000 POINTS & 56 JOURS -----------------------------------------------------
####
###

behaviour_jour_nuit <- behavior_dt_final
# behaviour_jour_nuit <- behaviour_dt_1_spa

# Filtrage basé sur le nombre de points et la durée minimale de suivi
behaviour_24h_BOX_1000_56 <- behaviour_jour_nuit %>%
  group_by(ID) %>%
  mutate(
    nb_point = n(),
    nb_days = as.numeric(difftime(max(date), min(date), units = "days"))
  ) %>%
  filter(nb_point >= 100, nb_days >= 10) # 1000 & 56

# Nombre d'individus restant après filtrage
behaviour_24h_nb_ind_1000_56 <- n_distinct(behaviour_24h_BOX_1000_56$ID)
print(behaviour_24h_nb_ind_1000_56)

# SEX --------------------------------------------------------------------------

# Importation des données sexe et âge
DATA_LIMI <- read_excel(file.path(data_path_serveur, "Age_Sex/DATA_LIMI.xlsx"))
bague <- all_gps %>% distinct(indID)

# Traitement du sexe
sex_1 <- DATA_LIMI %>% 
  filter(ACTION == "B", BAGUE %in% bague$indID) %>%
  dplyr::select(BAGUE, SEXE, sexe, SEXE.2)

# Remplacement des '?' par NA
sex_1 <- sex_1 %>% 
  mutate(across(everything(), ~ replace(.x, .x == "?", NA))) %>%
  mutate_all(~ str_replace_all(., "F\\?", "F")) %>%
  mutate_all(~ str_replace_all(., "M\\?", "M")) %>%
  mutate(across(everything(), ~ na_if(., "NA")))


# Suppression des doublons et remplissage des valeurs manquantes
sex_2 <- sex_1 %>% distinct()
sex_3 <- sex_2 %>%
  group_by(BAGUE) %>%
  fill(SEXE, sexe, SEXE.2, .direction = "downup") %>%
  ungroup()

# Extraction de la première lettre de SEXE
sex_3 <- sex_3 %>% mutate(SEXE = substr(SEXE, 1, 1))

# Fusion des informations des différentes colonnes
sex_3 <- sex_3 %>% 
  mutate(sex_ok = coalesce(SEXE.2, sexe, SEXE)) %>%
  dplyr::select(BAGUE, sex_ok) %>% 
  distinct() %>%
  drop_na() %>% 
  rename(indID = BAGUE, sex = sex_ok)

# Ajout des informations de sexe
sex_data <- sex_3 %>% rename(ID = indID)

behaviour_24h_BOX_1000_56_sex <- left_join(behaviour_24h_BOX_1000_56, sex_data, by = "ID")

# AGE --------------------------------------------------------------------------

## Age au baguage --------------------------------------------------------------

age_1 <- DATA_LIMI %>% 
  filter(ACTION == "B", BAGUE %in% bague$indID) %>% 
  dplyr::select(BAGUE, Year, AGE)

# Remplacement des "NA" (chaînes) par de véritables valeurs manquantes
age_1 <- age_1 %>% mutate(AGE = na_if(AGE, "NA"))

# Suppression des doublons et remplissage des valeurs manquantes
age_2 <- age_1 %>% distinct()
age_3 <- age_2 %>% 
  group_by(BAGUE) %>% 
  fill(AGE, .direction = "downup") %>%
  ungroup()

# Correction des incohérences
age_3 <- age_3 %>% mutate(AGE = replace(AGE, BAGUE == "EA580488", "JUV"))

age_data <- age_3 %>%
  dplyr::rename(ID = BAGUE, year_baguage = Year, age_baguage = AGE)

behaviour_24h_BOX_1000_56_sex_age <- left_join(behaviour_24h_BOX_1000_56_sex, age_data)

## Age chronologique -----------------------------------------------------------

behaviour_24h_BOX_1000_56_sex_age <- behaviour_24h_BOX_1000_56_sex_age %>% 
  mutate(year = year(date),
         age = case_when(age_baguage == "JUV" & year_baguage == year ~ "juv",
                         age_baguage == "AD" & year_baguage == year ~ "adult",
                         age_baguage == "JUV" & year_baguage == year_baguage + 1 ~ "adult",
                         age_baguage == "JUV" & year_baguage + 1 < year ~ "adult",
                         age_baguage == "AD" & year_baguage < year ~ "adult",
                         is.na(age_baguage) ~ NA,
                         is.na(year_baguage) ~ NA))


table(behaviour_24h_BOX_1000_56_sex_age$age)

#from gpt : vérif si ça marche !
# behaviour_24h_BOX_1000_56_sex_age <- behaviour_24h_BOX_1000_56_sex_age %>%
#   mutate(date = as.Date(date),  # s'assurer que 'date' est au format Date
#          passage_adulte = make_date(year = year_baguage + 1, month = 9, day = 1),
#          age = case_when(
#            age_baguage == "AD" ~ "adult",  # toujours adulte
#            age_baguage == "JUV" & date < passage_adulte ~ "juv",
#            age_baguage == "JUV" & date >= passage_adulte ~ "adult",
#            TRUE ~ NA_character_
#          )) %>%
#   select(-passage_adulte)  # optionnel : enlever la colonne temporaire

###
####
# JOUR & NUIT ------------------------------------------------------------------
####
###

# behaviour_dt_1$date_2 <- gsub("/", "-", behaviour_dt_1$date)

# behaviour_dt_1 <- behaviour_dt_1 %>% 
#   mutate(y_m_d =  as_date(date_2))

behaviour_24h_BOX_1000_56_sex_age$time <- substring(behaviour_24h_BOX_1000_56_sex_age$date, 12)

jour_nuit_dt <- tides %>% 
  dplyr::select(y_m_d, sunrise_UTC, sunset_UTC) %>% 
  mutate(y_m_d =  as_date(y_m_d)) %>% 
  distinct()

behaviour_24h_BOX_1000_56_sex_age <- behaviour_24h_BOX_1000_56_sex_age %>% 
  mutate(y_m_d =  ymd(as_date(date)))

behaviour_24h_BOX_1000_56_sex_age <- left_join(behaviour_24h_BOX_1000_56_sex_age, jour_nuit_dt)

behaviour_24h_BOX_1000_56_sex_age <- behaviour_24h_BOX_1000_56_sex_age %>% 
  mutate(jour_nuit = case_when(between(ymd_hms(date), ymd_hms(sunrise_UTC), ymd_hms(sunset_UTC)) ~ "jour",
                               !between(ymd_hms(date), ymd_hms(sunrise_UTC), ymd_hms(sunset_UTC)) ~ "nuit"))

###
####
# TYPE de MAREE ----------------------------------------------------------------
####
###

behaviour_24h_BOX_1000_56_sex_age <- behaviour_24h_BOX_1000_56_sex_age %>%
  mutate(high_type = case_when(
    behavior=="roosting" & height <= 4.8 ~ "mortes_eaux",
    behavior=="roosting" & between(height, 4.8, 6.4) ~ "vives_eaux",
    behavior=="roosting" & height >= 6.4 ~ "submersion"
  ))

# behaviour_24h_BOX_1000_56_sex_age <- behaviour_24h_BOX_1000_56_sex_age %>%
#   mutate(high_type = case_when(
#     height <= 4.8 ~ "mortes_eaux",
#     between(height, 4.8, 6.4) ~ "vives_eaux",
#     height >= 6.4 ~ "submersion"
#   ))
 
table(behaviour_24h_BOX_1000_56_sex_age$high_type)

###
####
# BRECHE -----------------------------------------------------------------------
####
###

# Les dates clés à retenir et à intégrer dans l’analyse de l’utilisation des reposoirs sur la partie continentale : 
# o	Avant le 01/01/2018 : gestion fine des niveaux d’eau, « toujours » favorables 
# o	01/01/2018 – 01/10/2020 : ouverture progressive de la brèche avec entrées d’eau, 
# mais maintien d’un seuil qui limite les entrées et l’évacuation de l’eau de mer 
# o	01/10/2020 – 01/07/2021 : disparition du seuil et ouverture progressive d’un chenal, 
# l’eau rentre mais ne ressort pas complétement, les coursives se comblent de sédiments 
# (dans un premier temps tu peux regrouper cette période avec celle précédente, phase « transitoire ») 
# o	01/07/2021 à maintenant : ouverture complète d’un chenal à travers les coursives et la brèche, l’eau rentre et se vide à chaque grande marée

behaviour_24h_BOX_1000_56_sex_age$date_no_time <- ymd(as_date(behaviour_24h_BOX_1000_56_sex_age$date))

behaviour_24h_BOX_1000_56_sex_age_breche <- behaviour_24h_BOX_1000_56_sex_age %>%
  mutate(
    breche = case_when(
      date_no_time < ymd("2018-01-01") ~ "digue intacte",
      between(date_no_time, ymd("2018-01-01"), ymd("2021-07-01")) ~ "ouverture progressive",
      date_no_time >= ymd("2021-07-01") ~ "ouverture complète")
  )

# /!\ /!\ /!\ SAVE /!\ /!\ /!\ 
# /!\ /!\ /!\ SAVE /!\ /!\ /!\ -------------------------------------------------
# /!\ /!\ /!\ SAVE /!\ /!\ /!\  

GPS_clean <- behaviour_24h_BOX_1000_56_sex_age_breche %>% 
  dplyr::select(ID,date,type,height,behavior,lon,lat,geometry,sex,
  year_baguage,age_baguage,year,age,high_type,breche,jour_nuit) %>% 
  dplyr::rename(ID = ID, datetime = date, tide_low_high = type, tide_height = height,
         behavior = behavior, lon = lon, lat = lat,
         sex = sex, year_baguage = year_baguage, age_baguage = age_baguage,
         year = year, age = age, tides_high_type = high_type, breche = breche)

# write
st_write(GPS_clean, paste0(data_generated_path_serveur, "GPS_clean.gpkg"), append = FALSE)
# read
GPS_clean <- st_read(file.path(data_generated_path_serveur, "GPS_clean.gpkg"))

###
####
# NB ---------------------------------------------------------------------------
####
###

# nb point GPS
length(GPS_clean$ID)

# Nb ind
length(unique(GPS_clean$ID))

# Nb sex
length(unique(GPS_clean$ID[GPS_clean$sex=="F"]))
length(unique(GPS_clean$ID[GPS_clean$sex=="M"]))

# Nb age
length(unique(GPS_clean$ID[GPS_clean$age=="adult"]))
length(unique(GPS_clean$ID[GPS_clean$age=="juv"]))

###
####
# VISUALISATION ----------------------------------------------------------------
####
###

crs(GPS_clean)
crs(dept_BOX)
crs(RMO_4326)

# Conversion en objet sf avec projection WGS84 (EPSG:4326)
GPS_clean <- st_as_sf(GPS_clean, coords = c("lon", "lat"), crs = 4326)
verif_crs(GPS_clean)

# nb individual and nb point per individuals
table(GPS_clean$ID)
length(table(GPS_clean$ID))

tmap_mode("plot")
tmap_plot_behavior <- tm_scalebar() +
  tm_shape(dept_BOX) +
  tm_polygons() +
  tm_shape(GPS_clean) +
  tm_dots(fill_alpha = 0.5) +
  tm_facets(by = "behavior", free.coords = FALSE) +
  tm_shape(RMO_4326) +
  tm_borders(col = "black") ; tmap_plot_behavior

beep(3)

tmap_save(tmap_plot_behavior, paste0(data_image_path_serveur, "/tmap_plot_behavior.png"), dpi = 1000)



tmap_mode("plot")
tmap_plot_behavior <- tm_scalebar() +
  tm_shape(dept_BOX) +
  tm_polygons() +
  tm_shape(GPS_clean) +
  tm_dots(fill_alpha = 0.5) +
  tm_facets(by = "tides_high_type", free.coords = FALSE) +
  tm_shape(RMO_4326) +
  tm_borders(col = "black") ; tmap_plot_behavior

