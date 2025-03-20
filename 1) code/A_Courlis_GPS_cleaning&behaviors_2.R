
# Run la partie "starting blok", 
# puis seulement runner la dernière partie "SAVE" avant la partie à souhaitée / en cours de travail

# STARTING BLOCK ---------------------------------------------------------------

# beep lorsqu'il y a une erreur 
options(error = function() {beep(7)})

# Nettoyage de l'environnement
rm(list=ls()) 

# time zone
with_tz(Sys.time(), "Europe/Paris")

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

## Chemins de données -------------------------------------------

data_path_serveur <- "D:/Projets_Suzanne/Courlis/Data/1) data/"
data_generated_path_serveur <- "D:/Projets_Suzanne/Courlis/Data/2) data_generated/"
data_image_path_serveur <- "D:/Projets_Suzanne/Courlis/Data/3) images/"

## Zone d'intérêt (BOX) -----------------------------------------

# BOX <- st_as_sf(st_as_sfc(st_bbox(c(xmin = -1.26, xmax = -0.945, ymax = 46.01, ymin = 45.78), crs = st_crs(4326)))) # Définition d'une boîte englobante avec des coordonnées spécifiques
# st_write(BOX, paste0(data_generated_path_serveur, "BOX.gpkg"), append = FALSE) # Sauvegarde de la boîte dans un fichier GeoPackage
BOX <- st_read(paste0(data_generated_path_serveur, "BOX.gpkg")) # Lecture de la boîte depuis le fichier sauvegardé
BOX_4326 <- st_transform(BOX, crs = 4326) # Transformation de la boîte au CRS 4326 (coordonnées géographiques)
BOX_2154 <- st_transform(BOX_4326, crs = 2154)

###
####
## Font de carte ---------------------------------------------------------------
####
###

# Departements ---
dept <- st_read(paste0(data_path_serveur, "departements.gpkg"), layer = "contourdesdepartements") # Lecture du fichier des départements
dept_BOX <- st_intersection(dept, BOX_4326) # Intersection des départements avec une boîte de délimitation (BOX_4326)
rm(dept) # Suppression pour libérer de la mémoire

# Réserve ---
reserve <- st_read(paste0(data_path_serveur, "Réserve_naturelle/rnn/rnn/N_ENP_RNN_S_000.shp")) # Lecture du fichier shapefile des réserves naturelles
RMO <- reserve[reserve$NOM_SITE == "Moëze-Oléron", ] # Filtrage pour ne garder que la réserve "Moëze-Oléron"
rm(reserve) # Suppression pour libérer de la mémoire

###
####
## Cartographie de la zone ------------------------------------------------------
####
###

# tmap_mode("view") # Activation du mode interactif pour tmap
# 
# # Création et affichage d'une carte avec tmap
# map <- tm_scale_bar() +                 
#   tm_shape(BOX) +                        
#   tm_polygons(col = "green", alpha = 0.5) +  
#   tm_shape(RMO) +                        
#   tm_polygons(col = "red", alpha = 0.5) ; map

###
####
# GPS DATA to load -------------------------------------------------------------
####
###

# Chargement de toutes les donnees CSV en une seule fois

# Fonction pour charger et fusionner les fichiers CSV d'un dossier
telecharger_donnees <- function(chemin) {
  fichiers <- list.files(path = chemin, pattern = "*.csv", full.names = TRUE)
  donnees <- lapply(fichiers, fread, sep = ",")
  return(rbindlist(donnees))
}

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

# /!\ /!\ /!\ SAVE /!\ /!\ /!\ 
# /!\ /!\ /!\ SAVE /!\ /!\ /!\ -------------------------------------------------
# /!\ /!\ /!\ SAVE /!\ /!\ /!\  

# write
write.table(all_gps, paste0(data_generated_path_serveur, "all_gps.txt"),
            append = FALSE, sep = ";", dec = ".", col.names = TRUE)

# read
all_gps <- read.table(paste0(data_generated_path_serveur, "all_gps.txt"),
                           header = TRUE, sep = ";")

table(all_gps$indID)

# *** SAMPLE *** 
# *** SAMPLE *** ---------------------------------------------------------------
# *** SAMPLE *** 

# all_gps <- all_gps %>% 
#   group_by(indID) %>% 
#   filter(n() >= 5000)
# 
# table(all_gps$indID)
# 
# sample <- unique(all_gps$indID)
# sample <- sample[1:25]
# 
# all_gps <- all_gps[all_gps$indID %in% sample,]
# 
# table(all_gps$indID)

# sample <- c("EA580466", "EA635103", "EC103787")
# # sample <- c("EA580466", "EA635103", "EC103787", "EA581515", "EA635142")
# 
# 
# all_gps <- all_gps[all_gps$indID %in% sample,]
# 
# table(all_gps$indID)

###
####
# SPATIAL DATA -----------------------------------------------------------------
####
###

# Conversion en objet sf avec projection WGS84 (EPSG:4326)
all_gps_spa <- st_as_sf(all_gps, coords = c("lon", "lat"), crs = 4326)
all_gps_spa <- st_transform(all_gps_spa, crs = 2154)

crs(all_gps_spa)

# Restauration explicite des colonnes longitude et latitude (inutile si elles existent déjà)
all_gps_spa$lon <- all_gps$lon
all_gps_spa$lat <- all_gps$lat

###
####
# DANS LA BOX ------------------------------------------------------
####
###

# Filtrage des points à l'intérieur de la boîte définie (opération coûteuse en temps)
# all_gps_spa_BOX <- st_intersection(all_gps_spa, BOX_4326) 
all_gps_spa_BOX <- st_intersection(all_gps_spa, BOX_2154) 


# /!\ /!\ /!\ SAVE /!\ /!\ /!\ 
# /!\ /!\ /!\ SAVE /!\ /!\ /!\ -------------------------------------------------
# /!\ /!\ /!\ SAVE /!\ /!\ /!\  

# write
st_write(all_gps_spa_BOX, paste0(data_generated_path_serveur, "all_gps_spa_BOX.gpkg"), append = FALSE)
# read
all_gps_spa <- st_read(file.path(data_generated_path_serveur, "all_gps_spa_BOX.gpkg"))

###
####
# TRIP -------------------------------------------------------------------------
####
###

# Vérification et suppression des valeurs manquantes dans la colonne 'time'
all_gps_spa <- all_gps_spa[!is.na(all_gps_spa$time),]

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

str(all_gps_dt_2$DateTime)

sum(is.na(all_gps_dt_2$DateTime))
all_gps_dt_2 <- na.omit(all_gps_dt_2)

crs(all_gps_spa)
crs(all_gps_dt_2)


# all_gps_dt_2_4326 <- st_as_sf(all_gps_dt_2, coords = c("x", "y"), crs=4326)


all_trip <- all_gps_dt_2 %>% 
  group_by(ID) %>% 
  trip()

# line(all_trip)

###
####
# STATIONARY -------------------------------------------------------------------
####
###

# crs(all_gps_dt_2)
# crs(all_trip)
# 
# library(sf)
# st_crs(all_trip) # Doit afficher EPSG:4326
# 
# # Convertir en sf
# all_trip_sf <- st_as_sf(all_trip)
# 
# # Reprojeter en EPSG:2154
# all_trip_sf_m <- st_transform(all_trip_sf, crs = 2154)
# 
# # Convertir en objet Spatial (sp) car trip utilise {sp}
# # Convertir en objet sp (SpatialPointsDataFrame) car `trip` utilise {sp}
# all_trip_sp_m <- as(all_trip_sf_m, "Spatial")
# 
# # Vérifier que l'objet contient bien un timestamp et un ID
# timestamp_col <- "DateTime"  # Remplace par le vrai nom de la colonne temporelle
# id_col <- "ID"  # Remplace par l'ID de l'animal ou du trajet
# 
# # Vérifier que ces colonnes existent
# if (!(timestamp_col %in% names(all_trip_sp_m))) stop("Colonne timestamp manquante !")
# if (!(id_col %in% names(all_trip_sp_m))) stop("Colonne ID manquante !")
# 
# # Créer l'objet trip
# all_trip_m <- trip(all_trip_sp_m, c(timestamp_col, id_col))
# 
# library(amt)
# 
# all_trip_m$stationary <- speedfilter(all_trip_m, max.speed = 0.5/3.6)  # vitesse en km/h
# 
# summary(all_trip_m$stationary) # Vérification des points supprimés

# all_trip_stationary_sf <- all_trip_m

# 
# 
# kh <- 0.5 # 0.5 km/h
# max_speed_m_s <- 0.5 / 3.6  # Résultat : 13.89 m/s
# 
# # Filtrage des points stationnaires avec une vitesse maximale de 0.5 km/h
# all_trip$stationary <- speedfilter(all_trip, max.speed = max_speed_m_s)  # vitesse en km/h
# summary(all_trip$stationary) # Vérification des points supprimés
# 
# Conversion en objet sf
# all_trip_stationary_sf <- st_as_sf(all_trip)
# 
# # Sélection des points valides avec une vitesse inférieure ou égale à 100 km/h
# all_trip_stationary_sf <- all_trip_stationary_sf %>%
#   filter(stationary == TRUE) %>%
#   select(-stationary)
# 
# # Extraction des coordonnées longitude et latitude
# all_trip_stationary_sf <- all_trip_stationary_sf %>%
#   mutate(lon = st_coordinates(.)[,1], lat = st_coordinates(.)[,2])

###
# old ---
###

# Filtrage des points stationnaires avec une vitesse maximale de 27 km/h
all_trip$stationary <- speedfilter(all_trip, max.speed = 27)  # vitesse en km/h
summary(all_trip$stationary) # Vérification des points supprimés

# Conversion en objet sf
all_trip_stationary_sf <- st_as_sf(all_trip)

# Sélection des points valides avec une vitesse inférieure ou égale à 100 km/h
all_trip_stationary_sf <- all_trip_stationary_sf %>% 
  filter(stationary == TRUE) %>% 
  select(-stationary)

# Extraction des coordonnées longitude et latitude
all_trip_stationary_sf <- all_trip_stationary_sf %>%
  mutate(lon = st_coordinates(.)[,1], lat = st_coordinates(.)[,2])

# /!\ /!\ /!\ SAVE /!\ /!\ /!\ 
# /!\ /!\ /!\ SAVE /!\ /!\ /!\ -------------------------------------------------
# /!\ /!\ /!\ SAVE /!\ /!\ /!\  

# write
# st_write(all_trip_stationary_sf, paste0(data_generated_path_serveur, "all_trip_stationary_sf.gpkg"), append = FALSE)
# read
# all_trip_stationary_sf <- st_read(file.path(data_generated_path_serveur, "all_trip_stationary_sf.gpkg"))


# all_trip_stationary_sf <- all_trip_stationary_sf %>% 
#   dplyr::rename(ID = indID, DateTime = Time)

table(all_trip_stationary_sf$ID)


###
####
# INTERPOLATION ----------------------------------------------------------------
####
###

# Création de l'objet ltraj pour stocker les trajectoires des animaux
all_stationary.ltraj <- as.ltraj(
  xy = bind_cols(x = all_trip_stationary_sf$lon, y = all_trip_stationary_sf$lat),
  date = all_trip_stationary_sf$DateTime,
  id = all_trip_stationary_sf$ID
)

# Re-échantillonnage des trajectoires toutes les 5 minutes (300 secondes)
# 5 min = 300 sec
# 30 min = 1800 sec

all_stationary.interp <- redisltraj(all_stationary.ltraj, 1800, type = "time")

rm(all_stationary.ltraj)

# Conversion en data frame avec renommer des colonnes pour clarté
all_stationary.interp <- ld(all_stationary.interp) %>% 
  rename(longitude = x, latitude = y)

# Conversion en objet sf (Spatial Feature)
inter_sf <- st_as_sf(all_stationary.interp, coords = c("longitude", "latitude"), crs = 4326)

inter_sf <- inter_sf %>%
  mutate(lon = st_coordinates(.)[,1], lat = st_coordinates(.)[,2]) %>% 
  select(date, id, pkey, geometry, lon, lat)

# /!\ /!\ /!\ SAVE /!\ /!\ /!\ 
# /!\ /!\ /!\ SAVE /!\ /!\ /!\ -------------------------------------------------
# /!\ /!\ /!\ SAVE /!\ /!\ /!\  

# write
# st_write(inter_sf, paste0(data_generated_path_serveur, "inter_sf.gpkg"), append = FALSE)
# read
# inter_sf <- st_read(file.path(data_generated_path_serveur, "inter_sf.gpkg"))

table(inter_sf$id)

# *** SAMPLE *** 
# *** SAMPLE *** ---------------------------------------------------------------
# *** SAMPLE *** 

# inter_sf <- inter_sf %>%
#   group_by(id) %>%
#   filter(n() >= 5000)
# 
# table(inter_sf$id)
# 
# sample <- unique(inter_sf$id)
# sample <- sample[1:10]
# sample <- c("EA580466", "EA635103", "EC103787", "EA581515", "EA635142")
# 
# inter_sf <- inter_sf[inter_sf$id %in% sample,]
# 
# table(inter_sf$id)

###
####
# TIME LAG ---------------------------------------------------------------------
####
###

# pour identifier les trous de plus de 30 min dans les enregistrements GPS

# all_trip_stationary_sf <- st_read(file.path(data_generated_path_serveur, "all_trip_stationary_sf.gpkg"))

time_lag_path <- "D:/Projets_Suzanne/Courlis/Data/2) data_generated/time_lag/"

# Paramètres
max_time_lag <- 30

# Calcul des intervalles de temps
all_trip_stationary_sf_timeLag <- all_trip_stationary_sf %>%
  arrange(ID, DateTime) %>%
  group_by(ID) %>%
  mutate(timeLag = as.numeric(difftime(DateTime, lag(DateTime), units = "mins")))

# Identification des gaps temporels
filtered_time_lags <- all_trip_stationary_sf_timeLag %>%
  st_drop_geometry() %>%
  arrange(ID, DateTime) %>%
  group_by(ID) %>%
  mutate(Date_before_timeLag = lag(DateTime),
         Date_after_timeLag = lead(DateTime),
         diff_before_after = as.numeric(difftime(Date_after_timeLag, Date_before_timeLag, units = "mins"))) %>%
  filter(timeLag > max_time_lag) %>%
  dplyr::select(ID, DateTime, Date_before_timeLag, Date_after_timeLag, diff_before_after) %>%
  distinct()

# Vérification des chevauchements
overlap_check <- filtered_time_lags %>%
  arrange(ID, DateTime) %>%
  group_by(ID) %>%
  mutate(overlap = int_overlaps(interval(Date_before_timeLag, Date_after_timeLag),
                                interval(lag(Date_before_timeLag), lag(Date_after_timeLag)))) %>%
  na.omit()

# permet de regrouper les intervalles consécutifs non chevauchants sous un même identifiant de groupe
overlap_check$group <- cumsum(!overlap_check$overlap) + 1

# Séparation des gaps non chevauchants
gaps_non_overlapping <- overlap_check %>% filter(overlap == FALSE) %>%
  rename(starting_gap = Date_before_timeLag, ending_gap = Date_after_timeLag) %>%
  dplyr::select(ID, starting_gap, ending_gap)

# Fusion des gaps chevauchants
gaps_overlapping <- overlap_check %>%
  filter(overlap == TRUE) %>%
  group_by(ID, group) %>%
  summarise(starting_gap = min(Date_before_timeLag), ending_gap = max(Date_after_timeLag), .groups = "drop") %>%
  dplyr::select(ID, starting_gap, ending_gap)

# Union des gaps
all_gaps <- bind_rows(gaps_non_overlapping, gaps_overlapping)
all_gaps$n <- seq_len(nrow(all_gaps))

# Suppression des objets inutiles
rm(all_trip_stationary_sf, all_trip_stationary_sf_timeLag)

# Suppression des points interpolés ---

inter_remove <- inter_sf %>%
  st_drop_geometry()

remove_i_all <- list()

for (ind_i in unique(all_gaps$ID)) {
  cat("Processing:", ind_i, "\n")

  ind_i_data <- all_gaps %>% filter(ID == ind_i)

  remove_i_list <- map(ind_i_data$n, function(n) {
    start_i_n <- ind_i_data$starting_gap[ind_i_data$n == n]
    end_i_n <- ind_i_data$ending_gap[ind_i_data$n == n]
    inter_remove %>% filter(id == ind_i & between(date, start_i_n, end_i_n))
  })

  remove_i_dt <- bind_rows(remove_i_list)
  write.table(remove_i_dt, file = paste0(time_lag_path, "inter_remove_ind_", ind_i, ".csv"),
              sep = ";", row.names = FALSE)
}

# Agrégation des résultats
files_time_lag <- list.files(path = time_lag_path, pattern = "*.csv", full.names = TRUE)
dt_time_lag <- lapply(files_time_lag, fread, sep = ";")
all_time_lag_remove <- rbindlist(dt_time_lag, fill = TRUE)

# Suppression des points dans le dataset GPS
dt_base <- inter_sf %>%
  st_drop_geometry()

rr <- as.data.frame(all_time_lag_remove$pkey) %>%
  rename(pkey = `all_time_lag_remove$pkey`)

df_diff <- anti_join(dt_base, rr)

# Vérification des longueurs
cat("Nombre total de points à supprimer:", length(all_time_lag_remove$pkey), "\n")
cat("Nombre total de points initiaux:", length(dt_base$pkey), "\n")
cat("Nombre total de points restants:", length(df_diff$pkey), "\n")

# Finalisation
point_no_gap <- left_join(df_diff, inter_sf)

# /!\ /!\ /!\ SAVE /!\ /!\ /!\ 
# /!\ /!\ /!\ SAVE /!\ /!\ /!\ -------------------------------------------------
# /!\ /!\ /!\ SAVE /!\ /!\ /!\  

# # write
# write.table(point_no_gap, paste0(data_generated_path_serveur, "point_no_gap.txt"),
#             append = FALSE, sep = ";", dec = ".", col.names = TRUE)
# # read
# point_no_gap <- read.table(paste0(data_generated_path_serveur, "point_no_gap.txt"),
#                            header = TRUE, sep = ";")
# 
# table(point_no_gap$id)
# 
# beep(2)

###
####
# MAREE ------------------------------------------------------------------------
####
###

## Prédites ----

# # Chargement des données marées
# tides <- read_csv("~/Courlis/Data/1) data/Maree/tides.csv")
# tides$DateTime <- paste0(tides$y_m_d, " ", tides$time)
# 
# tides$sunrise_UTC <- ymd_hms(paste0(tides$y_m_d, tides$sunrise), tz = "Europe/Paris")
# tides$sunrise_UTC <- with_tz(tides$sunrise_UTC, "UTC")
# tides$sunset_UTC <- ymd_hms(paste0(tides$y_m_d, tides$sunset), tz = "Europe/Paris")
# tides$sunset_UTC <- with_tz(tides$sunset_UTC, "UTC")
# 
# tides <- tides %>% 
#   na.omit() %>% 
#   distinct()
# 
# tides <- tides %>%
#   mutate(date_UTC = as.character(DateTime),  # Convertir si besoin
#          date_UTC = if_else(grepl("^\\d{2}-\\d{2}-\\d{4}", date_UTC), 
#                             dmy_hms(date_UTC),  # Format DD-MM-YYYY
#                             ymd_hms(date_UTC)),
#          date_UTC = with_tz(date_UTC, "UTC")) # Format YYYY-MM-DD
# 
# tides <- tides %>% 
#   mutate(date_rounded = round_date(ymd_hms(date_UTC), "30 mins"))

## Observées ----

# # prendre en priorité #3 "validé temps différé", 
# # puis #2 "brute temps différé", 
# # puis #1 "brute hautes fréquences"  
# 
# # remplissage des na dans le marégraphe de l'ile d'aix avec les infos des corrélations 
# # en priorité de la rochelle, puis de la cotinière
# 
# # Chargement des données marées
# 
# # Ile d'aix
# maree_path_aix <- paste0(data_path_serveur, "Maree/maregraphie/Ile_d_aix/ok/")
# files_maree_aix <- paste0(maree_path_aix, list.files(path = maree_path_aix, pattern = "*.txt"))
# dt_maree_aix <- lapply(files_maree_aix, fread, sep = ";")
# maree_aix <- rbindlist(dt_maree_aix)
# maree_aix$maregraphe <- "aix"
# 
# maree_aix_2 <- maree_aix %>% 
#   filter(Source !=4) %>% 
#   pivot_wider(names_from = Source, values_from = Valeur) %>% 
#   rename("valide_temps_diff" = "3", "brute_temps_diff" = "2", "brute_haute_freq" = "1") %>% 
#   mutate(hauteur_eau = coalesce(valide_temps_diff, brute_temps_diff, brute_haute_freq)) %>% 
#   select(Date, maregraphe, hauteur_eau) %>% 
#   distinct()
# 
# # La cotiniere
# maree_path_coti <- paste0(data_path_serveur, "Maree/maregraphie/La_cotiniere/ok/")
# files_maree_coti <- paste0(maree_path_coti, list.files(path = maree_path_coti, pattern = "*.txt"))
# dt_maree_coti <- lapply(files_maree_coti, fread, sep = ";")
# maree_coti <- rbindlist(dt_maree_coti)
# maree_coti$maregraphe <- "cotiniere"
# 
# maree_coti_2 <- maree_coti %>% 
#   rename("brute_haute_freq" = "Source", "hauteur_eau" = "Valeur") %>% 
#   select(Date, maregraphe, hauteur_eau) %>% 
#   distinct()
# 
# # La Rochelle
# maree_path_roch <- paste0(data_path_serveur, "Maree/maregraphie/La_rochelle/ok/")
# files_maree_roch <- paste0(maree_path_roch, list.files(path = maree_path_roch, pattern = "*.txt"))
# dt_maree_roch <- lapply(files_maree_roch, fread, sep = ";")
# maree_roch <- rbindlist(dt_maree_roch)
# maree_roch$maregraphe <- "rochelle"
# 
# maree_roch_2 <- maree_roch %>% 
#   filter(Source !=4) %>% 
#   pivot_wider(names_from = Source, values_from = Valeur) %>% 
#   rename("valide_temps_diff" = "3", "brute_temps_diff" = "2", "brute_haute_freq" = "1") %>% 
#   mutate(hauteur_eau = coalesce(valide_temps_diff, brute_temps_diff, brute_haute_freq)) %>% 
#   select(Date, maregraphe, hauteur_eau) %>% 
#   distinct()
# 
# # all maregraphes
# maree <- rbind(maree_aix_2, maree_coti_2, maree_roch_2) %>% 
#   na.omit()
# 
# maree2 <- maree %>% 
#   mutate(date_2 = gsub("/", "-", Date))
# 
# maree3 <- maree2 %>%
#   mutate(date_UTC = as.character(date_2),  # Convertir si besoin
#          date_UTC = if_else(grepl("^\\d{2}-\\d{2}-\\d{4}", date_UTC), 
#                             dmy_hms(date_UTC),  # Format DD-MM-YYYY
#                             ymd_hms(date_UTC)),
#          date_UTC = with_tz(date_UTC, "UTC")) # Format YYYY-MM-DD
# 
# str(maree3$date_UTC)  # Vérifie le type
# 
# maree4 <- maree3 %>% 
#   mutate(date_rounded = round_date(date_UTC, "30 mins"))
# 
# # comparaison entre marégraphe
# maree_sum <- maree4 %>%  
#   group_by(maregraphe, date_rounded) %>% 
#   summarise(mean_hauteur_eau = mean(hauteur_eau))
# 
# maree_spread <- maree_sum %>% 
#   pivot_wider(names_from = maregraphe, values_from = c(mean_hauteur_eau))
# 
# model_aix_roch <- lm(maree_spread$aix ~ maree_spread$rochelle)
# model_aix_coti <- lm(maree_spread$aix ~ maree_spread$cotiniere)
# 
# # estimation des prédictions selon la corrélation aix ~ rochelle et aix ~ cotiniere
# maree_spread <- maree_spread %>% 
#   mutate(pred_aix_roch = aix*model_aix_roch$coefficients[2] + model_aix_roch$coefficients[1],
#          pred_aix_coti = aix*model_aix_coti$coefficients[2] + model_aix_coti$coefficients[1])
# 
# # selection de la valeur observé, puis la prédiction via la rochelle, puis via cotiniere
# maree_correl <- maree_spread %>% 
#   mutate(hauteur_eau_and_pred = coalesce(aix, pred_aix_roch, pred_aix_coti)) %>% 
#   select(date_rounded, hauteur_eau_and_pred) %>% 
#   distinct()

## Prédites et observées ---- 

# tides <- left_join(tides, maree_correl)
# 
# tides <- tides %>% 
#   dplyr::rename(mean_height_obs = hauteur_eau_and_pred)
# 
# tides <- tides %>%
#   mutate(high_type = case_when(
#     type == "High" & mean_height_obs <= 5 ~ "mortes_eaux",
#     type == "High" & between(mean_height_obs, 5, 6.3) ~ "vives_eaux",
#     type == "High" & mean_height_obs >= 6.3 ~ "submersion" # 6.9
#   ))
# 
# table(tides$high_type)

# /!\ /!\ /!\ SAVE /!\ /!\ /!\ 
# /!\ /!\ /!\ SAVE /!\ /!\ /!\ -------------------------------------------------
# /!\ /!\ /!\ SAVE /!\ /!\ /!\  

# write
# write.table(tides, paste0(data_generated_path_serveur, "tides.txt"),
#             append = FALSE, sep = ";", dec = ".", col.names = TRUE)
# read
tides <- read.table(paste0(data_generated_path_serveur, "tides.txt"),
                           header = TRUE, sep = ";")

###
####
# BEHAVIORS --------------------------------------------------------------------
####
###

point_no_gap <- inter_sf

# point_no_gap <- read.table(paste0(data_generated_path_serveur, "point_no_gap.txt"),
#                            header = TRUE, sep = ";")

point_no_gap <- point_no_gap %>% 
  dplyr::select(-geometry, -pkey)

# foraging : 2h avant-après la marée base 
# roosting : 2h avant-après la marée haute 

# Coordinated Universal Time
point_no_gap$date_UTC <- ymd_hms(point_no_gap$date, tz = "Europe/Paris")
point_no_gap$date_UTC <- with_tz(point_no_gap$date_UTC, "UTC")

# Attribution d'un index unique aux dates
tides <- tides %>% 
  filter(y_m_d >= min(point_no_gap$date)) %>%
  distinct() %>% 
  mutate(t = dense_rank(y_m_d)) # attribue une valeur croissante aux dates

behaviour_dt_1 <- NULL

max_i <- max(tides$t) ; max_i

# i = 1 ; n = 1

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
    height_low_i_n <- dt_i_low$mean_height_obs[dt_i_low$n == n] # hauter d'eau
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

beep(2)

# /!\ /!\ /!\ SAVE /!\ /!\ /!\ 
# /!\ /!\ /!\ SAVE /!\ /!\ /!\ -------------------------------------------------
# /!\ /!\ /!\ SAVE /!\ /!\ /!\  

# write
# st_write(behaviour_dt_1_spa, paste0(data_generated_path_serveur, "behaviour_dt_1.gpkg"), append = FALSE)
# read
# behaviour_dt_1 <- st_read(file.path(data_generated_path_serveur, "behaviour_dt_1.gpkg"))


###
####
# VISUALISATION ----------------------------------------------------------------
####
###

tmap_mode("plot")
tmap_plot_behavior <- tm_scale_bar() +
  tm_shape(dept_BOX) +
  tm_polygons() +
  tm_shape(behaviour_dt_1_spa) +
  tm_dots(col = 'id', fill_alpha = 0.5) +
  tm_facets(by = "behavior", free.coords = FALSE) +
  # tmap_options(max.categories = 70) +
  tm_shape(RMO) +
  tm_borders(col = "black") ; tmap_plot_behavior

beep(3)


table(behaviour_24h_BOX_1000_56_sex_age$id)


