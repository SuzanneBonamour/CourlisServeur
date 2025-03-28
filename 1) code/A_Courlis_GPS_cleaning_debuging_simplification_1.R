
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
  select(date, id, pkey, geometry, lon, lat)

verif_tz(inter_sf, "date")
verif_crs(inter_sf)

###
####
# TIME GAP ---------------------------------------------------------------------
####
###

# pour identifier les trous de plus de x min dans les enregistrements GPS

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

verif_tz(filtered_time_lags, "Date_before_timeLag")
verif_tz(filtered_time_lags, "Date_after_timeLag")
verif_tz(inter_sf, "date")
verif_crs(inter_sf)

# Suppression des points interpolés ---

inter_remove <- inter_sf %>%
  st_drop_geometry()

# timer
start.time <- Sys.time()

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
overlapped <- foverlaps(intervals, points,
                        by.x = c("ID","start", "end"), 
                        by.y = c("ID", "start", "end"))

# ✅ 8. Exclure les points tombant dans un intervalle
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

verif <- ind_i_point_all %>% 
  filter(ID == "EC103792" &
           between(date, ymd_hms("2019-10-18 11:13:33"), ymd_hms("2019-10-18 11:23:00")))

verif

tt <- ind_i_point_all %>% 
  rename(id = ID)

ttt <- left_join(tt, inter_sf) %>% 
  na.omit()

table(ttt$id)

###
####
# BEHAVIORS roosting & foraging only -------------------------------------------
####
###
# point_no_gap <- ttt
# 
# # point_no_gap <- ind_i_point_all
# 
# # point_no_gap <- inter_sf
# 
# # point_no_gap <- read.table(paste0(data_generated_path_serveur, "point_no_gap.txt"),
# #                            header = TRUE, sep = ";")
# 
# # point_no_gap <- point_no_gap %>%
# #   dplyr::select(-geometry, -pkey)
# 
# # point_no_gap <- point_no_gap %>%
# #   dplyr::select(-geometry)
# 
# # foraging : 2h avant-après la marée base 
# # roosting : 2h avant-après la marée haute 
# 
# # Coordinated Universal Time
# 
# # changed
# # verif_tz(point_no_gap$date)
# 
# point_no_gap$date_UTC <- point_no_gap$date
# # !!!!!!!!!!!!!!!!!! date dajà en time zone UTC, donc pas besoin de la mettre en UE + UTC ???
# # point_no_gap$date_UTC <- ymd_hms(point_no_gap$date, tz = "Europe/Paris")
# # point_no_gap$date_UTC <- with_tz(point_no_gap$date_UTC, "UTC")
# # tz(point_no_gap$date)
# # tz(point_no_gap$date_UTC)
# 
# # données de maree 
# # where <- paste0("D:/Projets_Suzanne/Courlis/Data/1) data/Maree/", "tides_donnees_complete.csv")
# # write.csv(x=tides, file=where)
# 
# tides <- read_csv("D:/Projets_Suzanne/Courlis/Data/1) data/Maree/tides_donnees_complete.csv")
# 
# # Attribution d'un index unique aux dates
# one_day <- 86400
# 
# tides <- tides %>% 
#   filter(between(y_m_d, min_date-one_day, max_date+one_day)) %>% # on réduit le jeu de données de marée à la période étudiée
#   distinct() %>% 
#   mutate(t = dense_rank(y_m_d)) # attribue une valeur croissante aux dates
# 
# length(unique(tides$t))
# 
# behaviour_dt_1 <- NULL
# 
# max_i <- max(tides$t) ; max_i
# 
# # i = 1510 ; n = 1
# 
# # Boucle sur chaque date unique des marées
# for (i in unique(tides$t)) {
#   
#   print(i)
#   
#   # Pour la marée i
#   dt_i <- filter(tides, t == i)
#   
#   # Séparation en marée basse et haute
#   dt_i_low <- dt_i %>% filter(type == "Low") %>% mutate(n = row_number())
#   dt_i_high <- dt_i %>% filter(type == "High") %>% mutate(n = row_number())
#   
#   # Traitement des marées basses
#   for (n in unique(dt_i_low$n)) {
#     time_i_n <- ymd_hms(dt_i_low$date_UTC[dt_i_low$n == n]) # time
#     foraging_period <- time_i_n + c(-2, 2) * 3600 # period +/- 2h
#     height_low_i_n <- dt_i_low$mean_height_obs[dt_i_low$n == n] # hauteur d'eau
#     type_maree_low_i_n <- "basse" # type de marée
#     
#     all_info_low <- point_no_gap %>%
#       filter(between(date_UTC, foraging_period[1], foraging_period[2])) %>%
#       mutate(behavior = "foraging",
#              height_obs = height_low_i_n,
#              type_maree = type_maree_low_i_n) #%>%
# 
#     if (nrow(all_info_low) > 0) {
#       behaviour_dt_1 <- bind_rows(behaviour_dt_1, mutate(all_info_low, i = i, n = n))
#     } else {
#       print("No Data Available")
#     }
#   }
#   
#   # Traitement des marées hautes
#   for (n in unique(dt_i_high$n)) {
#     time_i_n <- ymd_hms(dt_i_high$date_UTC[dt_i_high$n == n])
#     roosting_period <- time_i_n + c(-2, 2) * 3600
#     height_high_i_n <- dt_i_high$mean_height_obs[dt_i_high$n == n]
#     type_maree_high_i_n <- dt_i_high$high_type[dt_i_high$n == n]
#     
#     all_info_high <- point_no_gap %>%
#       filter(between(date_UTC, roosting_period[1], roosting_period[2])) %>%
#       mutate(behavior = "roosting",
#              height_obs = height_high_i_n,
#              type_maree = type_maree_high_i_n)
#     
#     if (nrow(all_info_high) > 0) {
#       behaviour_dt_1 <- bind_rows(behaviour_dt_1, mutate(all_info_high, i = i, n = n))
#     } else {
#       print("No Data Available")
#     }
#   }
# }
# 
# # Sauvegarde des résultats
# behaviour_dt_1 <- behaviour_dt_1 %>% 
#   arrange(date_UTC)
# 
# # Conversion en objet sf avec projection WGS84 (EPSG:4326)
# behaviour_dt_1_spa <- st_as_sf(behaviour_dt_1, coords = c("lon", "lat"), crs = 4326)
# 
# # Restauration explicite des colonnes longitude et latitude
# behaviour_dt_1_spa$lon <- behaviour_dt_1$lon
# behaviour_dt_1_spa$lat <- behaviour_dt_1$lat
# 
# length(behaviour_dt_1_spa$lat)

###
####
# BEHAVIORS all points ---------------------------------------------------------
####
###

# les points GPS ---
point_no_gap <- ttt
point_no_gap$date_UTC <- point_no_gap$date
verif_tz(point_no_gap, "date_UTC")
# 
# les infos de maree ---
tides <- read_csv("D:/Projets_Suzanne/Courlis/Data/1) data/Maree/tides_donnees_complete.csv")
# 
# one_day <- 86400
# 
# tides <- tides %>% 
#   # filter(between(y_m_d, min_date-one_day, max_date+one_day)) %>% # on réduit le jeu de données de marée à la période étudiée
#   distinct() %>% 
#   mutate(t = dense_rank(y_m_d)) # attribue une valeur croissante aux dates
# 
# # tides_high <- tides %>% 
# #   filter(type=="High")
# # tides_high <- tides %>% 
# #   filter(type=="High")
# 
# # max_i <- max(tides$t) ; max_i
# # 
# # length(unique(tides$t))
# 
# # gpt 
# 
# library(data.table)
# 
# # Exemple de tableau tides avec des types (foraging ou roosting)
# # tides <- data.table(
# #   DateTime = as.POSIXct(c("2024-03-27 10:00:00", "2024-03-27 15:00:00")),
# #   type = c("foraging", "roosting") # Type de comportement associé à chaque marée
# # )
# 
# setDT(tides)
# setDT(point_no_gap)
# 
# library(data.table)
# 
# # Extraire les colonnes de date_UTC et DateTime
# dates_point_no_gap <- point_no_gap$date_UTC
# dates_tides <- tides$DateTime
# 
# # Effectuer la jointure cartésienne entre les dates
# merged <- CJ(dates_point_no_gap, dates_tides, unique = TRUE, sorted = FALSE)
# setnames(merged, c("date_UTC", "DateTime"))
# 
# # Convertir les dates en POSIXct si nécessaire (important pour la différence de temps)
# merged <- data.table(merged)
# merged[, date_UTC := as.POSIXct(date_UTC, tz = "UTC")]
# merged[, DateTime := as.POSIXct(DateTime, tz = "UTC")]
# 
# # Vérifier les doublons dans point_no_gap
# table(duplicated(point_no_gap$date_UTC))
# 
# # Vérifier les doublons dans tides
# table(duplicated(tides$DateTime))
# 
# 
# 
# 
# # Ajouter les colonnes de point_no_gap et tides
# # Effectuer la jointure avec allow.cartesian = TRUE
# merged <- merge(merged, point_no_gap, by.x = "date_UTC", by.y = "date_UTC", all.x = TRUE, allow.cartesian = TRUE)
# merged <- merge(merged, tides, by.x = "DateTime", by.y = "DateTime", all.x = TRUE)
# 
# # Calculer la différence de temps en secondes
# merged[, time_diff := abs(as.numeric(difftime(date_UTC, DateTime, units = "secs")))]
# 
# # Filtrer les lignes avec une différence de temps inférieure ou égale à 2 heures (7200 secondes)
# interval <- 2 * 60 * 60  # 2 heures en secondes
# merged <- merged[time_diff <= interval]
# 
# # Assigner le comportement basé sur le type de marée
# merged[, behavior := fifelse(type == "low", "foraging",
#                              fifelse(type == "high", "roosting", "other"))]
# 
# # Garder le comportement associé à chaque point dans 'point_no_gap'
# result <- merged[, .(behavior = behavior[1]), by = Date_UTC]
# 
# # Mettre à jour le comportement dans point_no_gap
# point_no_gap[result, behavior := i.behavior, on = .(Date_UTC)]
# 
# 
# 
# 
# # Calculer les différences de temps pour chaque groupe
# point_no_gap[, behavior := {
#   # Calculer la différence de temps et assigner le comportement
#   temp <- tides[abs(date_UTC - DateTime) <= 2 * 60 * 60]  # Filtrer les marées proches
#   if (nrow(temp) > 0) {
#     if (temp$type[1] == "low") {
#       "foraging"
#     } else if (temp$type[1] == "high") {
#       "roosting"
#     } else {
#       "other"
#     }
#   } else {
#     "other"
#   }
# }, by = date_UTC]
# 
# 
# 
# 
# table(point_no_gap$behavior)
# 
# 
# # old à la main #################
# # old à la main #################
# # old à la main #################
# 
# 
# 
# 
# # assignation de chaque point à un behavior ou à rien ---
# 
# behaviour_dt_all_points <- NULL
# 
# i = 100 ; n = 1
# 
# # Boucle sur chaque date unique des marées
# for (i in unique(tides$t)) {
#   
#   print(i)
#   
#   # Pour la marée i
#   dt_i <- filter(tides, t == i)
#   
#   # Séparation en marée basse et haute
#   dt_i_low <- dt_i %>% filter(type == "Low") %>% mutate(n = row_number())
#   dt_i_high <- dt_i %>% filter(type == "High") %>% mutate(n = row_number())
#   
#   # Traitement des marées basses
#   for (n in unique(dt_i_low$n)) {
#     # periode de temps autour des marée
#     time_i_n <- ymd_hms(dt_i_low$date_UTC[dt_i_low$n == n]) # time
#     foraging_period <- time_i_n + c(-2, 2) * 3600 # period +/- 2h
#     # hauteur d'eau
#     height_low_i_n <- dt_i_low$mean_height_obs[dt_i_low$n == n]
#     # type de marée
#     type_maree_low_i_n <- "basse" 
#     
#     behav_dt <- point_no_gap %>% 
#       mutate(behavior = case_when(between(date_UTC, foraging_period[1], foraging_period[2]) ~ "foraging",
#                                   !between(date_UTC, foraging_period[1], foraging_period[2]) ~ "other"))
#     
#     # all_info_low <- point_no_gap %>%
#     #   filter(between(date_UTC, foraging_period[1], foraging_period[2])) %>%
#     #   mutate(behavior = "foraging",
#     #          height_obs = height_low_i_n,
#     #          type_maree = type_maree_low_i_n) #%>%
#     
#     if (nrow(behav_dt) > 0) {
#       behaviour_dt_1 <- bind_rows(behaviour_dt_1, mutate(behav_dt, i = i, n = n))
#     } else {
#       print("No Data Available")
#     }
#   }
#   
#   # Traitement des marées hautes
#   for (n in unique(dt_i_high$n)) {
#     time_i_n <- ymd_hms(dt_i_high$date_UTC[dt_i_high$n == n])
#     roosting_period <- time_i_n + c(-2, 2) * 3600
#     height_high_i_n <- dt_i_high$mean_height_obs[dt_i_high$n == n]
#     type_maree_high_i_n <- dt_i_high$high_type[dt_i_high$n == n]
#     
#     all_info_high <- point_no_gap %>%
#       filter(between(date_UTC, roosting_period[1], roosting_period[2])) %>%
#       mutate(behavior = "roosting",
#              height_obs = height_high_i_n,
#              type_maree = type_maree_high_i_n)
#     
#     if (nrow(all_info_high) > 0) {
#       behaviour_dt_all_points <- bind_rows(behaviour_dt_all_points, mutate(all_info_high, i = i, n = n))
#     } else {
#       print("No Data Available")
#     }
#   }
# }
# 
# # Sauvegarde des résultats
# behaviour_dt_all_points <- behaviour_dt_all_points %>% 
#   arrange(date_UTC)
# 
# # Conversion en objet sf avec projection WGS84 (EPSG:4326)
# behaviour_dt_all_points_spa <- st_as_sf(behaviour_dt_all_points, coords = c("lon", "lat"), crs = 4326)
# 
# # Restauration explicite des colonnes longitude et latitude
# behaviour_dt_all_points_spa$lon <- behaviour_dt_all_points$lon
# behaviour_dt_all_points_spa$lat <- behaviour_dt_all_points$lat
# 
# length(behaviour_dt_all_points_spa$lat)


# avec le code time gap ##################

# timer
start.time <- Sys.time()

# ✅ 1. Convertir en data.table
setDT(tides)
setDT(point_no_gap)

# ✅ 5. Créer la table des intervalles avec clé pour foverlaps()
deux_heures <- 3600*2
intervals_tides <- unique(tides[, .(type, start = DateTime-deux_heures, end = DateTime+deux_heures)])
setkey(intervals_tides, type, start, end)
ID_list <- unique(point_no_gap$id)
library(purrr)
# Répéter le dataframe pour chaque ID de la liste
intervals_tides <- map_dfr(ID_list, ~ intervals_tides %>% 
                           mutate(ID = .x))
intervals_tides <- intervals_tides %>% 
  select(ID, type, start, end)

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
  select(ID, type, start)

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
  filter(nb_point >= 1000, nb_days >= 56)

# Nombre d'individus restant après filtrage
behaviour_24h_nb_ind_1000_56 <- n_distinct(behaviour_24h_BOX_1000_56$ID)
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

table(behaviour_24h_BOX_1000_56_sex_age$ID)
length(table(behaviour_24h_BOX_1000_56_sex_age$ID))

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
