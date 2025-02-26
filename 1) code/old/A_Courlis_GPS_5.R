
# ls()
# object.size("Object_name")
# rm("Object_name")
# sizes <- sapply( ls(), function(name) { object.size(get(name)) } )
# sort(sizes, decreasing=T)[1:10]
# rm()

# Run la partie "starting blok", 
# puis seulement runner la dernière partie "SAVE" avant la partie à souhaitée / en cours de travail

# STARTING BLOCK ---------------------------------------------------------------

# beep error
options(error = function() {beep(7)})

# beep when done
addTaskCallback(function(...) { system("say done"); TRUE }, name = "announce when done")

# Nettoyage de l'environnement
rm(list=ls()) 
# gc()

## Chargement des bibliothèques nécessaires -------------------------------------

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
library(lubridate)
library(beepr)
library(readr)
library(dplyr)

## Définition des chemins de données -------------------------------------------

# data_path_serveur <- "C:/Users/Suzanne.Bonamour/Documents/Courlis/Data/1) data/"
# data_generated_path_serveur <- "C:/Users/Suzanne.Bonamour/Documents/Courlis/Data/2) data_generated/"
# data_image_path_serveur <- "C:/Users/Suzanne.Bonamour/Documents/Courlis/Data/3) images/"

data_path_serveur <- "D:/Projets_Suzanne/Courlis/Data/1) data/"
data_generated_path_serveur <- "D:/Projets_Suzanne/Courlis/Data/2) data_generated/"
data_image_path_serveur <- "D:/Projets_Suzanne/Courlis/Data/3) images/"




## Création d'une zone d'intérêt (BOX) -----------------------------------------

# BOX <- st_as_sf(st_as_sfc(st_bbox(c(xmin = -1.26, xmax = -0.945, ymax = 46.01, ymin = 45.78), crs = st_crs(4326)))) # Définition d'une boîte englobante avec des coordonnées spécifiques
# st_write(BOX, paste0(data_generated_path_serveur, "BOX.gpkg"), append = FALSE) # Sauvegarde de la boîte dans un fichier GeoPackage
BOX <- st_read(paste0(data_generated_path_serveur, "BOX.gpkg")) # Lecture de la boîte depuis le fichier sauvegardé
BOX_4326 <- st_transform(BOX, crs = 4326) # Transformation de la boîte au CRS 4326 (coordonnées géographiques)

###
####
## MAPs background --------------------------------------------------------------
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
## Cartographie de la zone ------------------------------------------------------
####
###

tmap_mode("view") # Activation du mode interactif pour tmap

# Création et affichage d'une carte avec tmap
map <- tm_scale_bar() +                 # Ajout d'une barre d'échelle
  tm_shape(BOX) +                        # Ajout de la boîte englobante
  tm_polygons(col = "green", alpha = 0.5) +  # Polygone vert semi-transparent
  tm_shape(RMO) +                        # Ajout d'une autre couche (RMO)
  tm_polygons(col = "red", alpha = 0.5)    # Polygone rouge semi-transparent

map

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

# Chargement des donnees GPS MOVEBANK
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

# # write
# write.table(all_gps, paste0(data_generated_path_serveur, "all_gps.txt"),
#             append = FALSE, sep = ";", dec = ".", col.names = TRUE)
# 
# # read
# all_gps <- read.table(paste0(data_generated_path_serveur, "all_gps.txt"),
#                            header = TRUE, sep = ";")


# *** SAMPLE *** 
# *** SAMPLE *** ---------------------------------------------------------------
# *** SAMPLE ***  

sample <- unique(all_gps$indID)
sample <- sample[1:30]

all_gps <- all_gps[all_gps$indID %in% sample,]

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

###
####
# DANS LA BOX ------------------------------------------------------
####
###

# Filtrage des points à l'intérieur de la boîte définie (opération coûteuse en temps)
all_gps_spa_BOX <- st_intersection(all_gps_spa, BOX_4326) 

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

# all_gps_spa <- all_gps_spa[all_gps_spa$indID %in% c("EC103792", "EA580470", "EA580498"),]

# Vérification et suppression des valeurs manquantes dans la colonne 'time'
all_gps_spa <- all_gps_spa[!is.na(all_gps_spa$time),]

# Conversion en data frame et suppression de la colonne géométrique
all_gps_dt <- all_gps_spa %>%
  as.data.frame() %>%
  dplyr::mutate(ID = indID) %>%
  dplyr::select(-geom) %>% 
  na.omit()

# all_gps_dt$time <- as.POSIXct(all_gps_dt$time, format = "%Y-%m-%d %H:%M:%S")

# str(all_gps_dt$time)

# Création des trajets (trip) par individu avec une structure temporelle ordonnée
all_gps_dt_2 <- data.frame(
  x = all_gps_dt$lon,          # Longitudes
  y = all_gps_dt$lat,            # Latitudes
  DateTime = as.POSIXct(all_gps_dt$time, format = "%Y-%m-%d %H:%M:%S"),
  ID = all_gps_dt$ID)  # Date-heure

str(all_gps_dt_2$DateTime)

sum(is.na(all_gps_dt_2$DateTime))
all_gps_dt_2 <- na.omit(all_gps_dt_2)

all_trip <- all_gps_dt_2 %>% 
  group_by(ID) %>% 
  trip()

###
####
# STATIONARY 27 km/h -----------------------------------------------------------
####
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
st_write(all_trip_stationary_sf, paste0(data_generated_path_serveur, "all_trip_stationary_sf.gpkg"), append = FALSE)
# read
all_trip_stationary_sf <- st_read(file.path(data_generated_path_serveur, "all_trip_stationary_sf.gpkg"))

###
####
# INTERPOLATION toutes les 30 minutes ------------------------------------------
####
###

# all_stationary <- all_trip_100maxi
# all_stationary <- all_trip_stationary_sf

# Création de l'objet ltraj pour stocker les trajectoires des animaux
all_stationary.ltraj <- as.ltraj(
  xy = bind_cols(x = all_trip_stationary_sf$lon, y = all_trip_stationary_sf$lat),
  date = all_trip_stationary_sf$DateTime,
  id = all_trip_stationary_sf$ID
)

# Re-échantillonnage des trajectoires toutes les 30 minutes (1800 secondes)
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
st_write(inter_sf, paste0(data_generated_path_serveur, "inter_sf.gpkg"), append = FALSE)
# read
inter_sf <- st_read(file.path(data_generated_path_serveur, "inter_sf.gpkg"))

###
####
# TIME LAG 30 min max ----------------------------------------------------------
####
###

# pour trouver où il y a des trous dans le jeu de données --- 

all_trip_stationary_sf <- st_read(file.path(data_generated_path_serveur, "all_trip_stationary_sf.gpkg"))

time_lag_path <- "C:/Users/Suzanne.Bonamour/Documents/Courlis/Data/2) data_generated/time_lag/"

# Paramètres
max_time_lag <- 30

# Chargement des données
# data_path <- paste0(data_generated_path_serveur, "inter_sf.gpkg")
# inter_sf <- st_read(data_path)
# all_trip_path <- paste0(data_generated_path_serveur, "all_trip_stationary_sf.gpkg")
# all_trip_stationary_sf <- st_read(all_trip_path)

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

overlap_check$group <- cumsum(!overlap_check$overlap) + 1 # permet de regrouper les intervalles consécutifs non chevauchants sous un même identifiant de groupe

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
# inter_sf <- st_read(data_path)
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

# write
write.table(point_no_gap, paste0(data_generated_path_serveur, "point_no_gap.txt"),
            append = FALSE, sep = ";", dec = ".", col.names = TRUE)
# read
point_no_gap <- read.table(paste0(data_generated_path_serveur, "point_no_gap.txt"),
                           header = TRUE, sep = ";")
###
####
# MAREE ------------------------------------------------------------------------
####
###

## Prédites ----

# Chargement des données marées
tides <- read_csv("~/Courlis/Data/1) data/Maree/tides.csv")
tides$DateTime <- paste0(tides$y_m_d, " ", tides$time)

tides <- tides %>% 
  na.omit() %>% 
  distinct()

tides <- tides %>% 
  mutate(date_rounded = round_date(ymd_hms(DateTime), "30 mins"))

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

maree <- maree %>% 
  mutate(date_2 = gsub("/", "-", maree$Date)) %>% 
  mutate(date_rounded = round_date(dmy_hms(date_2), "30 mins")) %>% 
  select(- Date, -date_2)

maree_2 <- maree %>%  
  group_by(maregraphe, date_rounded) %>% 
  summarise(mean_hauteur_eau = mean(hauteur_eau))

maree_spread <- maree_2 %>% 
  pivot_wider(names_from = maregraphe, values_from = c(mean_hauteur_eau))

model_aix_roch <- lm(maree_spread$aix ~ maree_spread$rochelle)
model_aix_coti <- lm(maree_spread$aix ~ maree_spread$cotiniere)

maree_spread <- maree_spread %>% 
  mutate(pred_aix_roch = aix*model_aix_roch$coefficients[2] + model_aix_roch$coefficients[1],
         pred_aix_coti = aix*model_aix_coti$coefficients[2] + model_aix_coti$coefficients[1])

maree_correl <- maree_spread %>% 
  mutate(hauteur_eau_and_pred = coalesce(aix, pred_aix_roch, pred_aix_coti)) %>% 
  select(date_rounded, hauteur_eau_and_pred) %>% 
  distinct()

## Prédites et observées ---- 

# maree_round_dt <- maree_correl %>% 
#   group_by(date_rounded) %>% 
#   mutate(mean_height_obs = mean(hauteur_eau_and_pred, na.rm = TRUE)) %>% 
#   select(date_rounded, mean_height_obs) %>% 
#   distinct() %>% 
#   rename()
# 
# maree_round_dt <- maree %>% 
#   group_by(date_rounded, maregraphe) %>% 
#   summarise(mean_height_obs = mean(hauteur_eau, na.rm = T)) %>% 
#   rename()

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

# /!\ /!\ /!\ SAVE /!\ /!\ /!\ 
# /!\ /!\ /!\ SAVE /!\ /!\ /!\ -------------------------------------------------
# /!\ /!\ /!\ SAVE /!\ /!\ /!\  

# write
write.table(tides, paste0(data_generated_path_serveur, "tides.txt"),
            append = FALSE, sep = ";", dec = ".", col.names = TRUE)
# read
tides <- read.table(paste0(data_generated_path_serveur, "tides.txt"),
                           header = TRUE, sep = ";")

###
####
# BEHAVIORS --------------------------------------------------------------------
####
###

# jusqu'à i = 3449

point_no_gap <- read.table(paste0(data_generated_path_serveur, "point_no_gap.txt"),
                           header = TRUE, sep = ";")

# foraging : 2h avant-après la marée base 
# roosting : 2h avant-après la marée haute 
# + hauteur d'eau
# (+ jour / nuit)

# Coordinated Universal Time

library(lubridate)
with_tz(Sys.time(), "Europe/Paris")

point_no_gap$date_UTC <- ymd_hms(point_no_gap$date, tz = "Europe/Paris")
point_no_gap$date_UTC <- with_tz(point_no_gap$date_UTC, "UTC")

# Mise en forme des données des points de suivi
# point_no_gap <- point_no_gap %>%
#   arrange(date_UTC) %>%
#   mutate(date_POS = as.POSIXct(date, tz = "UTC")) %>% 
#   mutate(date_tzone = format(date_ymd_hms, "%Y-%m-%d %H:%M:%S", tz = "Europe/Paris"))

tides$date_UTC <- ymd_hms(tides$DateTime, tz = "Europe/Paris")
tides$date_UTC <- with_tz(tides$date_UTC, "UTC")

# tides <- tides %>% 
#   filter(y_m_d > "2015-10-12") %>%  # Filtrage des dates pertinentes
#   mutate(DateTime = lubridate::with_tz(DateTime, tzone = "Europe/Paris"))

# Attribution d'un index unique aux dates
tides <- tides %>% 
  mutate(t = dense_rank(y_m_d_UTC)) # attribue une valeur croissante aux dates

# Fusion des données
# tides <- tides %>% 
#   left_join(dplyr::select(tides, y_m_d), by = "y_m_d") %>% 
#   mutate(DateTime = as.POSIXct(DateTime)) %>% 
#   distinct()

behaviour_dt_1 <- NULL

max_i <- max(tides$t)

i = 2992
n = 1

# Boucle sur chaque date unique des marées
for (i in unique(tides$t)) {
  if (i == max_i) break  # Évite d'inclure la dernière marée sans info suivante
  
  print(i)
  dt_i <- filter(tides, t == i)
  
  # Séparation en marée basse et haute
  dt_i_low <- dt_i %>% filter(type == "Low") %>% mutate(n = row_number())
  dt_i_high <- dt_i %>% filter(type == "High") %>% mutate(n = row_number())
  
  # Traitement des marées basses
  for (n in unique(dt_i_low$n)) {
    time_i_n <- dt_i_low$DateTime[dt_i_low$n == n]
    foraging_period <- time_i_n + c(-2, 2) * 3600
    height_low_i_n <- dt_i_low$mean_height_obs[dt_i_low$n == n]
    type_maree_low_i_n <- "basse"
    
    all_info_low <- point_no_gap %>%
      mutate(behavior = case_when(between(date, foraging_period[1], foraging_period[2]) ~ "foraging")) %>%
      filter(!is.na(behavior)) %>%
      # dplyr::select(id, date, behavior, pkey) %>%
      mutate(height_obs = height_low_i_n,
             type_maree = type_maree_low_i_n) %>%
      st_drop_geometry()
    
    if (nrow(all_info_low) > 0) {
      behaviour_dt_1 <- bind_rows(behaviour_dt_1, mutate(all_info_low, i = i, n = n))
    } else {
      print(paste(i, "No Data Available"))
    }
  }
  
  # Traitement des marées hautes
  for (n in unique(dt_i_high$n)) {
    time_i_n <- dt_i_high$DateTime[dt_i_high$n == n]
    
    # if (dt_i_high$mean_height_obs[dt_i_high$n == n] < 0) next
    
    roosting_period <- time_i_n + c(-2, 2) * 3600
    height_high_i_n <- dt_i_high$mean_height_obs[dt_i_high$n == n]
    type_maree_high_i_n <- dt_i_high$high_type[dt_i_high$n == n]  
    
    all_info_high <- point_no_gap %>%
      mutate(behavior = case_when(between(date, roosting_period[1], roosting_period[2]) ~ "roosting")) %>%
      filter(!is.na(behavior)) %>%
      # dplyr::select(id, date, behavior, pkey) %>%
      mutate(height_obs = height_high_i_n,
             type_maree = type_maree_high_i_n) %>%
      st_drop_geometry()
    
    if (nrow(all_info_high) > 0) {
      behaviour_dt_1 <- bind_rows(behaviour_dt_1, mutate(all_info_high, i = i, n = n))
    } else {
      print(paste(i, "No Data Available"))
    }
  }
}

# Organisation et sauvegarde des résultats
behaviour_dt_1 <- behaviour_dt_1 %>% 
  arrange(date)

# Conversion en objet sf avec projection WGS84 (EPSG:4326)
behaviour_dt_1_spa <- st_as_sf(behaviour_dt_1, coords = c("lon", "lat"), crs = 4326)

# Restauration explicite des colonnes longitude et latitude
behaviour_dt_1_spa$lon <- behaviour_dt_1$lon
behaviour_dt_1_spa$lat <- behaviour_dt_1$lat

# /!\ /!\ /!\ SAVE /!\ /!\ /!\ 
# /!\ /!\ /!\ SAVE /!\ /!\ /!\ -------------------------------------------------
# /!\ /!\ /!\ SAVE /!\ /!\ /!\  

# write
st_write(behaviour_dt_1_spa, paste0(data_generated_path_serveur, "behaviour_dt_1.gpkg"), append = FALSE)
# read
behaviour_dt_1 <- st_read(file.path(data_generated_path_serveur, "behaviour_dt_1.gpkg"))

###
####
# JOUR & NUIT ------------------------------------------------------------------
####
###

behaviour_dt_1$date_2 <- gsub("/", "-", behaviour_dt_1$date)

behaviour_dt_1 <- behaviour_dt_1 %>% 
  mutate(y_m_d =  as_date(date_2))

behaviour_dt_1$time <- substring(behaviour_dt_1$date, 12)

jour_nuit_dt <- tides %>% 
  dplyr::select(y_m_d, sunrise, sunset) %>% 
  mutate(y_m_d =  as_date(y_m_d)) %>% 
  distinct()

behaviour_dt_2 <- left_join(behaviour_dt_1, jour_nuit_dt)

behaviour_dt_2$time <- hms(behaviour_dt_2$time)
behaviour_dt_2$sunrise <- hms(behaviour_dt_2$sunrise)
behaviour_dt_2$sunset <- hms(behaviour_dt_2$sunset)

behaviour_dt_2 <- behaviour_dt_2 %>% 
  mutate(jour_nuit = case_when(between(time, sunrise, sunset) ~ "jour",
                               !between(time, sunrise, sunset) ~ "nuit"))

# /!\ /!\ /!\ SAVE /!\ /!\ /!\ 
# /!\ /!\ /!\ SAVE /!\ /!\ /!\ -------------------------------------------------
# /!\ /!\ /!\ SAVE /!\ /!\ /!\  

# write
st_write(behaviour_dt_2, paste0(data_generated_path_serveur, "behaviour_jour_nuit.gpkg"), append = FALSE)
# read
behaviour_jour_nuit <- st_read(file.path(data_generated_path_serveur, "behaviour_jour_nuit.gpkg"))

###
####
# 1000 POINTS & 56 JOURS -----------------------------------------------------
####
###

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

# /!\ /!\ /!\ SAVE /!\ /!\ /!\ 
# /!\ /!\ /!\ SAVE /!\ /!\ /!\ -------------------------------------------------
# /!\ /!\ /!\ SAVE /!\ /!\ /!\  

# write
st_write(behaviour_24h_BOX_1000_56, paste0(data_generated_path_serveur, "behaviour_24h_BOX_1000_56.gpkg"), append = FALSE)
# read
behaviour_24h_BOX_1000_56 <- st_read(file.path(data_generated_path_serveur, "behaviour_24h_BOX_1000_56.gpkg"))

# SEX --------------------------------------------------------------------------

# Importation des données sexe et âge
DATA_LIMI <- read_excel(file.path(data_path_serveur, "Age_Sex/DATA_LIMI.xlsx"))
bague <- all_gps %>% distinct(indID)

# Traitement du sexe
sex_1 <- DATA_LIMI %>% 
  filter(ACTION == "B", BAGUE %in% bague$indID) %>%
  dplyr::select(BAGUE, SEXE, sexe, SEXE.2)

# Remplacement des '?' par NA
sex_1 <- sex_1 %>% mutate(across(everything(), ~ replace(.x, .x == "?", NA)))

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
sex_data <- sex_3 %>% rename(id = indID)

behaviour_24h_BOX_1000_56_sex <- left_join(behaviour_24h_BOX_1000_56, sex_data, by = "id")

# AGE --------------------------------------------------------------------------

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

# # Nettoyage final
# tt <- all_gps %>%
#   left_join(age_3 %>% rename(indID = BAGUE, year_baguage = Year, age_baguage = AGE), by = "indID")
# 
# # Calcul de l'âge par rapport aux données GPS
# tt <- tt %>% mutate(year = year(time), baguage_gps = year_baguage - year)
# 
# # Filtrage des individus avec des incohérences temporelles
# tt2 <- tt %>%
#   dplyr::select(indID, year, year_baguage, baguage_gps) %>%
#   distinct() %>%
#   filter(baguage_gps > 0)

# Ajout des informations d'âge
age_data <- age_3 %>% rename(id = BAGUE)

behaviour_24h_BOX_1000_56_sex_age <- left_join(behaviour_24h_BOX_1000_56_sex, age_data, by = "id")

# /!\ /!\ /!\ SAVE /!\ /!\ /!\ 
# /!\ /!\ /!\ SAVE /!\ /!\ /!\ -------------------------------------------------
# /!\ /!\ /!\ SAVE /!\ /!\ /!\  

# write
st_write(behaviour_24h_BOX_1000_56_sex_age, paste0(data_generated_path_serveur, "behaviour_24h_BOX_1000_56_sex_age.gpkg"), append = FALSE)
# read
behaviour_24h_BOX_1000_56_sex_age <- st_read(file.path(data_generated_path_serveur, "behaviour_24h_BOX_1000_56_sex_age.gpkg"))

###
####
# TYPE de MAREE ----------------------------------------------------------------
####
###


behaviour_24h_BOX_1000_56_sex_age <- behaviour_24h_BOX_1000_56_sex_age %>%
  mutate(high_type = case_when(
    behavior=="roosting" & height_obs <= 3.57 ~ "mortes_eaux",
    behavior=="roosting" & between(height_obs, 3.57, 6.3) ~ "vives_eaux",
    behavior=="roosting" & height_obs >= 6.3 ~ "submersion" # 6.9
  ))

table(behaviour_24h_BOX_1000_56_sex_age$high_type)

###
####
# VISUALISATION ----------------------------------------------------------------
####
###

## Behavior ----

# Mode de visualisation statique
tmap_mode("plot")
tmap_plot_behavior <- tm_scale_bar() +
  tm_shape(dept_BOX) +
  tm_polygons() +
  tm_shape(behaviour_24h_BOX_1000_56_sex_age) +
  tm_dots(col = 'id', alpha = 0.5) +
  tm_facets(by = "behavior", free.coords = FALSE) +
  tmap_options(max.categories = 70) +
  tm_shape(RMO) +
  tm_borders(col = "black") ; tmap_plot_behavior

# Sauvegarde de la carte statique
tmap_save(tmap_plot_behavior, 
          paste0(data_image_path_serveur, "/tmap_plot_behavior.png"), dpi = 600)

# Mode de visualisation interactive
tmap_mode("view")
tmap_view_behavior <- tm_scale_bar() +
  tm_shape(behaviour_24h_BOX_1000_56_sex_age) +
  tm_dots(col = 'id', alpha = 0.5) +
  tm_facets(by = "behavior", free.coords = FALSE) +
  tmap_options(max.categories = 70) +
  tm_shape(RMO) +
  tm_borders(col = "black") ; tmap_view_behavior

## Type de marée ----

type_maree_map <- behaviour_24h_BOX_1000_56_sex_age[behaviour_24h_BOX_1000_56_sex_age$behavior=="roosting",]

# Mode de visualisation statique
tmap_mode("plot")
tmap_plot_maree <- tm_scale_bar() +
  tm_shape(dept_BOX) +
  tm_polygons() +
  tm_shape(type_maree_map) +
  tm_dots(col = 'id', alpha = 0.5) +
  tm_facets(by = c("type_maree"), free.coords = FALSE) +
  tmap_options(max.categories = 70) +
  tm_shape(RMO) +
  tm_borders(col = "black") ; tmap_plot_maree

# Sauvegarde de la carte statique
tmap_save(tmap_plot_maree, 
          paste0(data_image_path_serveur, "/tmap_plot_maree.png"), dpi = 600)

# Mode de visualisation interactive
tmap_mode("view")
tmap_view_maree <- tm_scale_bar() +
  tm_shape(type_maree_map) +
  tm_dots(col = 'id', alpha = 0.5) +
  tm_facets(by = c("type_maree"), free.coords = FALSE) +
  tmap_options(max.categories = 70) +
  tm_shape(RMO) +
  tm_borders(col = "black") ; tmap_view_maree

## Sexe & Behavior ----

# Mode de visualisation statique
tmap_mode("plot")
tmap_plot_behav_sex <- tm_scale_bar() +
  tm_shape(dept_BOX) +
  tm_polygons() +
  tm_shape(behaviour_24h_BOX_1000_56_sex_age) +
  tm_dots(col = 'id', alpha = 0.5) +
  tm_facets(by = c("behavior", "sex"), free.coords = FALSE) +
  tmap_options(max.categories = 70) +
  tm_shape(RMO) +
  tm_borders(col = "black") ; tmap_plot_behav_sex

# Sauvegarde de la carte statique
tmap_save(tmap_plot_behav_sex, 
          paste0(data_image_path_serveur, "/tmap_plot_behav_sex.png"), dpi = 600)

# Mode de visualisation interactive
tmap_mode("view")
tmap_view_behav_sex <- tm_scale_bar() +
  tm_shape(behaviour_24h_BOX_1000_56_sex_age) +
  tm_dots(col = 'id', alpha = 0.5) +
  tm_facets(by = c("behavior", "sex"), free.coords = FALSE) +
  tmap_options(max.categories = 70) +
  tm_shape(RMO) +
  tm_borders(col = "black") ; tmap_view_behav_sex

## Age & Behavior ----

# Mode de visualisation statique
tmap_mode("plot")
tmap_plot_behav_age <- tm_scale_bar() +
  tm_shape(dept_BOX) +
  tm_polygons() +
  tm_shape(behaviour_24h_BOX_1000_56_sex_age) +
  tm_dots(col = 'id', alpha = 0.5) +
  tm_facets(by = c("behavior", "AGE"), free.coords = FALSE) +
  tmap_options(max.categories = 70) +
  tm_shape(RMO) +
  tm_borders(col = "black") ; tmap_plot_behav_age

# Sauvegarde de la carte statique
tmap_save(tmap_plot_behav_age, 
          paste0(data_image_path_serveur, "/tmap_plot_behav_age.png"), dpi = 600)

# Mode de visualisation interactive
tmap_mode("view")
tmap_view_behav_age <- tm_scale_bar() +
  tm_shape(behaviour_24h_BOX_1000_56_sex_age) +
  tm_dots(col = 'id', alpha = 0.5) +
  tm_facets(by = c("behavior", "AGE"), free.coords = FALSE) +
  tmap_options(max.categories = 70) +
  tm_shape(RMO) +
  tm_borders(col = "black") ; tmap_view_behav_age

# Signal sonore à la fin du script
beep(3)


# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


###
####
# > DESCRIPTION DES TRACKS -------------------------------------------------------
####
###

## Période de suivi ------------------------------------------------------------

tracking_period <- sort(unique(lubridate::year(all_gps$time))) 
print(tracking_period)

first_track_day <- min(all_gps$time) 
print(first_track_day)

last_track_day <- max(all_gps$time) 
print(last_track_day)

## Nombre d'individus suivis ---------------------------------------------------

nb_ind <- length(unique(all_gps$indID)) 
print(nb_ind)

## Statistiques par individu ---------------------------------------------------

# Conversion de 'indID' en facteur pour garantir un bon groupement
all_gps$indID <- as.factor(all_gps$indID)

# Regroupement des données par individu et calcul des statistiques
descript_dt_1 <- all_gps %>% 
  group_by(indID) %>% 
  summarise(
    nb_event_per_ind = n(),  # Nombre d'événements par individu
    recorded_period_per_ind = difftime(max(time), min(time), units = "days"),  # Durée totale de suivi
    
    # Statistiques sur la vitesse
    speed_mean = mean(speed, na.rm = TRUE),
    speed_min = min(speed, na.rm = TRUE),
    speed_max = max(speed, na.rm = TRUE),
    
    # Statistiques sur l'accélération dans les axes X, Y et Z
    acc_x_mean = mean(acc_x, na.rm = TRUE),
    acc_x_min = min(acc_x, na.rm = TRUE),
    acc_x_max = max(acc_x, na.rm = TRUE),
    
    acc_y_mean = mean(acc_y, na.rm = TRUE),
    acc_y_min = min(acc_y, na.rm = TRUE),
    acc_y_max = max(acc_y, na.rm = TRUE),
    
    acc_z_mean = mean(acc_z, na.rm = TRUE),
    acc_z_min = min(acc_z, na.rm = TRUE),
    acc_z_max = max(acc_z, na.rm = TRUE),
    
    # Statistiques sur la température
    temp_mean = mean(temp, na.rm = TRUE),
    temp_min = min(temp, na.rm = TRUE),
    temp_max = max(temp, na.rm = TRUE)
  )

# Affichage du tableau récapitulatif
print(descript_dt_1)

## Décalage temporel entre les points GPS --------------------------------------
time_lag_ind_dt <- all_gps %>%
  arrange(indID, time) %>%
  group_by(indID) %>%
  mutate(diff_time = time - lag(time)) %>%
  summarize(
    diff_time_max = as.numeric(max(diff_time, na.rm = TRUE), units = "mins"),
    diff_time_min = as.numeric(min(diff_time, na.rm = TRUE), units = "mins"),
    diff_time_mean = as.numeric(mean(diff_time, na.rm = TRUE), units = "mins"),
    diff_time_med = as.numeric(median(diff_time, na.rm = TRUE), units = "mins")
  )

## Mois ------------------------------------------------------------------------

# Extraire le mois depuis la colonne 'time', avec étiquettes abrégées en anglais
all_gps$month <- lubridate::month(all_gps$time, label = TRUE, abbr = TRUE, locale = "English_United States")

as_tibble(all_gps)

# Afficher les mois uniques présents dans le dataset
unique(all_gps$month)

# Nombre total de points par mois
month_tracks_dt <- all_gps %>%
  group_by(month) %>%
  summarize(n = n())  # n() est plus efficace que length(eventID)

# Nombre de points par mois et par individu
month_ind_tracks_dt <- all_gps %>%
  group_by(indID, month) %>%
  summarize(n = n())  # n() pour compter les lignes par groupe
