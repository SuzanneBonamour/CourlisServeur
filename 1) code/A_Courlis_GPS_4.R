# Nettoyage de l'environnement
rm(list=ls()) 

# Chargement des bibliothèques nécessaires -----------------------------------

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


# Définition des chemins de données -------------------------------------------

data_path_serveur <- "C:/Users/Suzanne.Bonamour/Documents/Courlis/Data/1) data/"
data_generated_path_serveur <- "C:/Users/Suzanne.Bonamour/Documents/Courlis/Data/2) data_generated/"
data_image_path_serveur <- "C:/Users/Suzanne.Bonamour/Documents/Courlis/Data/3) images/"

# Création d'une zone d'intérêt (BOX) -----------------------------------------

# Définition d'une boîte englobante avec des coordonnées spécifiques
# BOX <- st_as_sf(st_as_sfc(st_bbox(c(xmin = -1.26, xmax = -0.945, ymax = 46.01, ymin = 45.78), crs = st_crs(4326))))

# Sauvegarde de la boîte dans un fichier GeoPackage
# st_write(BOX, paste0(data_generated_path_serveur, "BOX.gpkg"), append = FALSE)

# Lecture de la boîte depuis le fichier sauvegardé
BOX <- st_read(paste0(data_generated_path_serveur, "BOX.gpkg"))

# Transformation de la boîte au CRS 4326 (coordonnées géographiques)
BOX_4326 <- st_transform(BOX, crs = 4326)

###
####
# MAPs background --------------------------------------------------------------
####
###

# Chargement des contours des départements

# Lecture du fichier des départements
dept <- st_read(paste0(data_path_serveur, "departements.gpkg"),
                layer = "contourdesdepartements")

# Filtrage pour le département 17 (Charente-Maritime)
dept17 <- dept[dept$code == 17, ]

# Intersection des départements avec une boîte de délimitation (BOX_4326)
dept_BOX <- st_intersection(dept, BOX_4326)

# Écriture du résultat dans un fichier geopackage (écrasement activé)
# st_write(dept_BOX, paste0(data_generated_path_serveur, "dept_BOX.gpkg"), append = FALSE)

# Chargement et filtrage des réserves naturelles
# Lecture du fichier shapefile des réserves naturelles
reserve <- st_read(paste0(data_path_serveur, "Réserve_naturelle/rnn/rnn/N_ENP_RNN_S_000.shp"))

# Filtrage pour ne garder que la réserve "Moëze-Oléron"
RMO <- reserve[reserve$NOM_SITE == "Moëze-Oléron", ]

# Suppression de l'objet `reserve` pour libérer de la mémoire
rm(reserve)

###
####
# Cartographie de la zone ------------------------------------------------------
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
  "event-id", "timestamp", "location-long", "location-lat",
  "acceleration-raw-x", "acceleration-raw-y", "acceleration-raw-z",
  "bar:barometric-height", "battery-charge-percent", "external-temperature",
  "ground-speed", "gls:light-level", "individual-local-identifier"
)

noms_colonnes <- c(
  eventID = "event-id", time = "timestamp", lon = "location-long",
  lat = "location-lat", acc_x = "acceleration-raw-x", acc_y = "acceleration-raw-y",
  acc_z = "acceleration-raw-z", baro = "bar:barometric-height", battery = "battery-charge-percent",
  temp = "external-temperature", speed = "ground-speed", light = "gls:light-level",
  indID = "individual-local-identifier"
)

all_gps <- all_gps %>% dplyr::select(all_of(colonnes_utiles)) %>% dplyr::rename(!!!noms_colonnes)

# Nettoyage des identifiants individuels
all_gps$indID <- stri_extract(all_gps$indID, regex = "(?<=\\[).*?(?=\\])")
all_gps$indID[all_gps$indID == " FRP_ EC103792"] <- "FRP_EC103792"

# Suppression des lignes sans coordonnees GPS
all_gps <- all_gps[!is.na(all_gps$lon) & !is.na(all_gps$lat), ]

# Extraction de l'ID unique des bagues
all_gps$indID <- substring(all_gps$indID, first=5, last=12)

###
####
# DESCRIPTION DES TRACKS -------------------------------------------------------
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

## Sexe des inds ---------------------------------------------------------------

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

# Jointure avec les données GPS
all_gps <- left_join(all_gps, sex_3, by = "indID")

## Age des inds ----------------------------------------------------------------
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

# Nettoyage final
tt <- all_gps %>%
  left_join(age_3 %>% rename(indID = BAGUE, year_baguage = Year, age_baguage = AGE), by = "indID")

# Calcul de l'âge par rapport aux données GPS
tt <- tt %>% mutate(year = year(time), baguage_gps = year_baguage - year)

# Filtrage des individus avec des incohérences temporelles
tt2 <- tt %>%
  dplyr::select(indID, year, year_baguage, baguage_gps) %>%
  distinct() %>%
  filter(baguage_gps > 0)

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
# TRIP -------------------------------------------------------------------------
####
###

# Vérification et suppression des valeurs manquantes dans la colonne 'time'
all_gps_spa <- all_gps_spa[!is.na(all_gps_spa$time),]

# Conversion en data frame et suppression de la colonne géométrique
all_gps_dt <- all_gps_spa %>%
  as.data.frame() %>%
  dplyr::mutate(ID = indID) %>%
  dplyr::select(-geometry)

# Création des trajets (trip) par individu avec une structure temporelle ordonnée
all_trip <- all_gps_dt %>%
  group_by(ID) %>%
  dplyr::select(x = lon, y = lat, DateTime = time, everything()) %>%
  trip()

###
####
# SPEED LIMIT ERROR ------------------------------------------------------------
####
###

# Filtrage des vitesses supérieures à 100 km/h avec la méthode Speedfilter
all_trip$filter <- speedfilter(all_trip, max.speed = 100)

# Conversion en objet sf
all_trip_sf <- st_as_sf(all_trip)

# récupération lon/lat 
# all_trip_sf_2 <- left_join(all_trip_sf, all_gps_dt)

# Sélection des points valides avec une vitesse inférieure ou égale à 100 km/h
all_trip_100maxi <- all_trip_sf %>% filter(filter == TRUE)

# Extraction des coordonnées longitude et latitude
all_trip_100maxi <- all_trip_100maxi %>%
  mutate(lon = st_coordinates(.)[,1], lat = st_coordinates(.)[,2])

# Sauvegarde du fichier au format GeoPackage
# st_write(all_trip_100maxi, file.path(data_generated_path_serveur, "all_trip_100maxi.gpkg"), append = FALSE)

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
all_trip_stationary_sf <- all_trip_stationary_sf %>% filter(filter == TRUE)

# Extraction des coordonnées longitude et latitude
all_trip_stationary_sf <- all_trip_stationary_sf %>%
  mutate(lon = st_coordinates(.)[,1], lat = st_coordinates(.)[,2])

# Sauvegarde du fichier au format GeoPackage
# st_write(all_trip_stationary_sf, file.path(data_generated_path_serveur, "all_trip_stationary_sf.gpkg"), append = FALSE)

###
####
# INTERPOLATION toutes les 30 minutes ------------------------------------------
####
###

# Chargement des données
# all_trip_stationary_sf <- st_read(file.path(data_generated_path_serveur, "all_trip_stationary_sf.gpkg"))

# all_stationary <- all_trip_100maxi
all_stationary <- all_trip_stationary_sf


# Création de l'objet ltraj pour stocker les trajectoires des animaux
all_stationary.ltraj <- as.ltraj(
  xy = bind_cols(x = all_stationary$lon, y = all_stationary$lat),
  date = all_stationary$DateTime,
  id = all_stationary$ID
)

# Re-échantillonnage des trajectoires toutes les 30 minutes (1800 secondes)
all_stationary.interp <- redisltraj(all_stationary.ltraj, 1800, type = "time")

# Conversion en data frame avec renommer des colonnes pour clarté
all_stationary.interp <- ld(all_stationary.interp) %>% 
  rename(longitude = x, latitude = y)

# Conversion en objet sf (Spatial Feature)
inter_sf <- st_as_sf(all_stationary.interp, coords = c("longitude", "latitude"), crs = 4326)

inter_sf <- inter_sf %>%
  mutate(lon = st_coordinates(.)[,1], lat = st_coordinates(.)[,2])

# Sauvegarde de l'objet interpolé
# st_write(inter_sf, file.path(data_generated_path_serveur, "inter_sf.gpkg"), append = FALSE)

###
####
# TIME LAG 30 min max ----------------------------------------------------------
####
###

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
  arrange(indID, DateTime) %>%
  group_by(indID) %>%
  mutate(timeLag = as.numeric(difftime(DateTime, lag(DateTime), units = "mins")))

# Identification des gaps temporels
filtered_time_lags <- all_trip_stationary_sf_timeLag %>% 
  st_drop_geometry() %>% 
  arrange(indID, DateTime) %>%
  group_by(indID) %>%
  mutate(Date_before_timeLag = lag(DateTime),
         Date_after_timeLag = lead(DateTime),
         diff_before_after = as.numeric(difftime(Date_after_timeLag, Date_before_timeLag, units = "mins"))) %>% 
  filter(timeLag > max_time_lag) %>% 
  dplyr::select(indID, DateTime, Date_before_timeLag, Date_after_timeLag, diff_before_after) %>% 
  distinct()

# Vérification des chevauchements
overlap_check <- filtered_time_lags %>%
  arrange(indID, DateTime) %>%
  group_by(indID) %>%
  mutate(overlap = int_overlaps(interval(Date_before_timeLag, Date_after_timeLag),
                                interval(lag(Date_before_timeLag), lag(Date_after_timeLag)))) %>% 
  na.omit()

overlap_check$group <- cumsum(!overlap_check$overlap) + 1 # permet de regrouper les intervalles consécutifs non chevauchants sous un même identifiant de groupe

# Séparation des gaps non chevauchants
gaps_non_overlapping <- overlap_check %>% filter(overlap == FALSE) %>% 
  rename(starting_gap = Date_before_timeLag, ending_gap = Date_after_timeLag) %>% 
  dplyr::select(indID, starting_gap, ending_gap)

# Fusion des gaps chevauchants
gaps_overlapping <- overlap_check %>% filter(overlap == TRUE) %>% 
  group_by(indID, group) %>%
  summarise(starting_gap = min(Date_before_timeLag), ending_gap = max(Date_after_timeLag), .groups = "drop") %>% 
  dplyr::select(indID, starting_gap, ending_gap)

# Union des gaps
all_gaps <- bind_rows(gaps_non_overlapping, gaps_overlapping)
all_gaps$n <- seq_len(nrow(all_gaps))

# Suppression des objets inutiles
rm(all_trip_stationary_sf, all_trip_stationary_sf_timeLag)
gc()

# Suppression des points interpolés
inter_remove <- inter_sf %>% st_drop_geometry()
remove_i_all <- list()

for (ind_i in unique(all_gaps$indID)) {
  cat("Processing:", ind_i, "\n")
  
  ind_i_data <- all_gaps %>% filter(indID == ind_i)
  
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
write.table(all_time_lag_remove, file = paste0(time_lag_path, "all_time_lag_remove.csv"),
            sep = ";", row.names = FALSE)

# Suppression des points dans le dataset GPS
# inter_sf <- st_read(data_path)
dt_base <- inter_sf %>% st_drop_geometry()

rr <- as.data.frame(all_time_lag_remove$pkey) %>% 
  rename(pkey = `all_time_lag_remove$pkey`)
df_diff <- anti_join(dt_base, rr)

# Vérification des longueurs
cat("Nombre total de points à supprimer:", length(all_time_lag_remove$pkey), "\n")
cat("Nombre total de points initiaux:", length(dt_base$pkey), "\n")
cat("Nombre total de points restants:", length(df_diff$pkey), "\n")

# Finalisation
point_no_gap <- left_join(df_diff, inter_sf)

###
####
# MAREE ------------------------------------------------------------------------
####
###

# Chargement des données spatiales
# behaviour_24h_data <- st_read(paste0(data_generated_path_serveur, "behaviour_24h_test_cleaning_BOX_1000_56_sex_age_no_gap.gpkg"))
# behaviour_24h_data <- point_no_gap 

## Prédites ----

# Chargement des données marées
tides <- read_csv("~/Courlis/Data/1) data/Maree/tides.csv")
tides$DateTime <- paste0(tides$y_m_d, " ", tides$time)

tides <- tides %>% 
  na.omit() %>% 
  distinct()

tides <- tides %>% 
  mutate(date_rounded = round_date(ymd_hms(DateTime), "30 mins"))

# # type de marée haute (morte, vives eaux, submersion)
# 
# # adaptée
# tides <- tides %>% 
#   mutate(high_type = case_when(
#     type == "High" & height <= 5 ~ "mortes_eaux",
#     type == "High" & between(height, 5, 6) ~ "vives_eaux",
#     type == "High" & height >= 6 ~ "submersion"
#   ))

# hist(tides$height[tides$type=="High"])


## Observées ----

# prendre en priorité #3 "validé temps différé", 
# puis #2 "brute temps différé", 
# puis #1 "brute hautes fréquences"  

# Chargement des données marées
maree_path <- paste0(data_path_serveur, "Maree/maregraphie/ok/")
files_maree <- paste0(maree_path, list.files(path = maree_path, pattern = "*.txt"))
dt_maree <- lapply(files_maree, fread, sep = ";")
maree <- rbindlist(dt_maree)

maree <- maree %>% 
  filter(Source !=4) %>% 
  pivot_wider(names_from = Source, values_from = Valeur) %>% 
  rename("valide_temps_diff" = "3", "brute_temps_diff" = "2", "brute_haute_freq" = "1") %>% 
  mutate(hauteur_eau = coalesce(valide_temps_diff, brute_temps_diff, brute_haute_freq)) %>% 
  select(Date, hauteur_eau) %>% 
  distinct()


# rounded time 

# behaviour_24h_data <- behaviour_24h_data %>% 
#   mutate(date_rounded = round_date(ymd_hms(date), "30 mins"))

maree$date_2 <- gsub("/", "-", maree$Date)

maree <- maree %>% 
  mutate(date_rounded = round_date(dmy_hms(date_2), "30 mins"))

maree_round_dt <- maree %>% 
  group_by(date_rounded) %>% 
  summarise(mean_height_obs = mean(hauteur_eau, nr.rm = T)) %>% 
  rename()

# observée et prédites together

tides <- left_join(tides, maree_round_dt)




# old (mean sur les différentes estimation d'hauteur d'eau)  
# # rounded time 
# 
# # behaviour_24h_data <- behaviour_24h_data %>% 
# #   mutate(date_rounded = round_date(ymd_hms(date), "30 mins"))
# 
# maree$date_2 <- gsub("/", "-", maree$Date)
# 
# maree <- maree %>% 
#   mutate(date_rounded = round_date(dmy_hms(date_2), "30 mins"))
# 
# maree_round_dt <- maree %>% 
#   group_by(date_rounded) %>% 
#   summarise(mean_height_obs = mean(Valeur, nr.rm = T)) %>% 
#   rename()
# 
# # observée et prédites together
# 
# tides <- left_join(tides, maree_round_dt)







# behaviour_24h_data_2 <- left_join(behaviour_24h_data, maree_round_dt)

# hist(behaviour_24h_data_2$mean_height)

# # creation de la variable "high_type"
# behaviour_24h_data_2 <- behaviour_24h_data_2 %>% 
#   mutate(high_type = case_when(
#     mean_height <= 3.57 ~ "mortes_eaux",
#     between(mean_height, 3.57, 6.9) ~ "vives_eaux",
#     mean_height >= 6.9 ~ "submersion"
#   ))
# 
# table(behaviour_24h_data_2$high_type)

###
####
# BEHAVIORS --------------------------------------------------------------------
####
###

point_no_gap <- left_join(df_diff, inter_sf)

# foraging : 2h avant-après la marée base 
# roosting : 2h avant-après la marée haute 
# + hauteur d'eau
# (+ jour / nuit)

# Mise en forme des données des points de suivi
point_no_gap <- point_no_gap %>%
  arrange(date) %>%
  mutate(date = lubridate::with_tz(date, tzone = "Europe/Paris"))

tides <- tides %>% 
  filter(y_m_d > "2015-10-12") %>%  # Filtrage des dates pertinentes
  mutate(DateTime = lubridate::with_tz(DateTime, tzone = "Europe/Paris"))

# Attribution d'un index unique aux dates
tides <- tides %>% 
  mutate(t = dense_rank(y_m_d)) # attribue une valeur croissante aux dates

# Fusion des données
tides <- tides %>% 
  left_join(dplyr::select(tides, y_m_d), by = "y_m_d") %>% 
  mutate(DateTime = as.POSIXct(DateTime)) %>% 
  distinct() #%>% 
  # slice_head(n = 100)

behaviour_dt_1 <- NULL
max_i <- max(tides$t)

# i = 1

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
    
    all_info_low <- point_no_gap %>%
      mutate(behavior = case_when(between(date, foraging_period[1], foraging_period[2]) ~ "foraging")) %>%
      filter(!is.na(behavior)) %>%
      # dplyr::select(id, date, behavior, pkey) %>%
      mutate(height_obs = height_low_i_n) %>%
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
    
    all_info_high <- point_no_gap %>%
      mutate(behavior = case_when(between(date, roosting_period[1], roosting_period[2]) ~ "roosting")) %>%
      filter(!is.na(behavior)) %>%
      # dplyr::select(id, date, behavior, pkey) %>%
      mutate(height_obs = height_high_i_n) %>%
      st_drop_geometry()
    
    if (nrow(all_info_high) > 0) {
      behaviour_dt_1 <- bind_rows(behaviour_dt_1, mutate(all_info_high, i = i, n = n))
    } else {
      print(paste(i, "No Data Available"))
    }
  }
}

# Organisation et sauvegarde des résultats
behaviour_dt_1 <- behaviour_dt_1 %>% arrange(date)

write.table(behaviour_dt_1, paste0(data_generated_path_serveur, "behaviour_24h_height_maree_obs.txt"),
            append = FALSE, sep = ";", dec = ".", col.names = TRUE)

###
####
# DANS LA BOX ------------------------------------------------------
####
###

# Chargement des données de comportement
# behaviour_24h <- read.table(paste0(data_generated_path_serveur, "behaviour_24h_test_cleaning.txt"),
#                              header = TRUE, sep = ";")
behaviour_24h <- read.table(paste0(data_generated_path_serveur, "behaviour_24h_height_maree_obs.txt"),
                            header = TRUE, sep = ";")

# behaviour_24h <- behaviour_dt_1
behaviour_24h <- na.omit(behaviour_24h)

# #récupération des lon/lat
# 
# behaviour_24h <- behaviour_24h %>%
#   mutate(lon = st_coordinates(.)[,1], lat = st_coordinates(.)[,2])

# Conversion en objet spatial (sf)
behaviour_24h_sf <- st_as_sf(behaviour_24h, coords = c("x", "y"), crs = 4326)
behaviour_24h_sf$lon <- behaviour_24h$x
behaviour_24h_sf$lat <- behaviour_24h$y

crs(behaviour_24h_sf)
crs(BOX_4326)

# Filtrage des points à l'intérieur de la boîte définie (opération coûteuse en temps)
behaviour_24h_BOX <- st_intersection(behaviour_24h_sf, BOX_4326) 

# Sauvegarde des données filtrées
# st_write(behaviour_24h_BOX, paste0(data_generated_path_serveur, "behaviour_24h_test_cleaning_BOX_no_gap.gpkg"), append = FALSE)

###
####
# 1000 POINTS & 56 JOURS ----------------------------------------------
###

# Chargement des données filtrées
# behaviour_24h_BOX <- st_read(paste0(data_generated_path_serveur, "behaviour_24h_test_cleaning_BOX_no_gap.gpkg"))

# Filtrage basé sur le nombre de points et la durée minimale de suivi
behaviour_24h_BOX_1000_56 <- behaviour_24h_BOX %>%
  group_by(id) %>%
  mutate(
    nb_point = n(),
    nb_days = as.numeric(difftime(max(date), min(date), units = "days"))
  ) %>%
  filter(nb_point >= 1000, nb_days >= 56)

# Nombre d'individus restant après filtrage
behaviour_24h_nb_ind_1000_56 <- n_distinct(behaviour_24h_BOX_1000_56$id)
print(behaviour_24h_nb_ind_1000_56)

# Sauvegarde des données filtrées
# st_write(behaviour_24h_BOX_1000_56, paste0(data_generated_path_serveur, "behaviour_24h_test_cleaning_BOX_1000_56_no_gap.gpkg"), append = FALSE)

###
####
# AJOUT SEXE ET AGE ------------------------------------------------------------
####
###

# Chargement des données filtrées
# behaviour_24h_BOX_1000_56 <- st_read(paste0(data_generated_path_serveur, "behaviour_24h_test_cleaning_BOX_1000_56_no_gap.gpkg"))

behaviour_24h_BOX_1000_56 <- 
# Ajout des informations de sexe
sex_data <- sex_3 %>% rename(id = indID)
behaviour_24h_BOX_1000_56_sex <- left_join(behaviour_24h_BOX_1000_56, sex_data, by = "id")

# Ajout des informations d'âge
age_data <- age_3 %>% rename(id = BAGUE)
behaviour_24h_BOX_1000_56_sex_age <- left_join(behaviour_24h_BOX_1000_56_sex, age_data, by = "id")

# Sauvegarde des données enrichies
# st_write(behaviour_24h_BOX_1000_56_sex_age, paste0(data_generated_path_serveur, "behaviour_24h_test_cleaning_BOX_1000_56_sex_age_no_gap.gpkg"), append = FALSE)

###
####
# VISUALISATION ----------------------------------------------------------------
####
###

# behaviour_24h_data <- behaviour_24h_BOX_1000_56_sex_age
  
# Création d'un groupe d'individus pour les visualisations
behaviour_24h_gp_ind <- behaviour_24h_data %>% 
  st_drop_geometry() %>% 
  dplyr::select(id, nb_point) %>% 
  distinct() %>% 
  arrange(nb_point) %>% 
  mutate(group = rep(1:6, length.out = n()))

# Fusion des données avec les groupes définis
dt_map_group_behaviour_24h <- left_join(behaviour_24h_data, behaviour_24h_gp_ind, by = "id")

# Mode de visualisation statique
tmap_mode("plot")
behaviour_24h_BOX_maps_static <- tm_scale_bar() +
  tm_shape(dept_BOX) +
  tm_polygons() +
  tm_shape(dt_map_group_behaviour_24h) +
  tm_dots(col = 'id', alpha = 0.5) +
  tm_facets(by = "behavior", free.coords = FALSE) +
  tmap_options(max.categories = 70) +
  tm_shape(RMO) +
  tm_borders(col = "black") ; behaviour_24h_BOX_maps_static

# Sauvegarde de la carte statique
tmap_save(behaviour_24h_BOX_maps_static, 
          paste0(data_image_path_serveur, "/behaviour_test_clean.png"), dpi = 600)

# Mode de visualisation interactive
tmap_mode("view")
behaviour_24h_BOX_maps_interactive <- tm_scale_bar() +
  tm_shape(dt_map_group_behaviour_24h) +
  tm_dots(col = 'id', alpha = 0.5) +
  tm_facets(by = c("group", "behavior"), free.coords = FALSE) +
  tmap_options(max.categories = 70) +
  tm_shape(RMO) +
  tm_borders(col = "black") ; behaviour_24h_BOX_maps_interactive

# Mode de visualisation interactive
tmap_mode("view")
behaviour_24h_BOX_maps_interactive <- tm_scale_bar() +
  tm_shape(dt_map_group_behaviour_24h) +
  tm_dots(col = 'id', alpha = 0.5) +
  tm_facets(by = "behavior", free.coords = FALSE) +
  tmap_options(max.categories = 70) +
  tm_shape(RMO) +
  tm_borders(col = "black") ; behaviour_24h_BOX_maps_interactive

# Mode de visualisation interactive
tt <- dt_map_group_behaviour_24h[dt_map_group_behaviour_24h$behavior=="roosting",]
table(tt$high_type)

table1tmap_mode("view")
behaviour_24h_BOX_maps_interactive_2 <- tm_scale_bar() +
  tm_shape(tt) +
  tm_dots(col = 'id', alpha = 0.5) +
  tm_facets(by = "high_type", free.coords = FALSE) +
  tmap_options(max.categories = 70) +
  tm_shape(RMO) +
  tm_borders(col = "black") ; behaviour_24h_BOX_maps_interactive_2

tmap_mode("plot")
behaviour_24h_BOX_maps_interactive_2 <- tm_scale_bar() +
  tm_shape(tt) +
  tm_dots(col = 'id', alpha = 0.5) +
  tm_facets(by = "high_type", free.coords = FALSE) +
  tmap_options(max.categories = 70) +
  tm_shape(RMO) +
  tm_borders(col = "black") ; behaviour_24h_BOX_maps_interactive_2

# Signal sonore à la fin du script
beep(3)


###
####
# !!!!!!!!!!!!!!!!!!!! TO CLEAN ----------------------------------------------------------------------
####
###





###
####
# path -------------------------------------------------------------------------
####
###

# data_path <- "C:/Users/Suzanne.Bonamour/OneDrive - LPO/2) Data/4) Courlis/Data/1) data/"
# data_generated_path <- "C:/Users/Suzanne.Bonamour/OneDrive - LPO/2) Data/4) Courlis/Data/2) data_generated/"
# data_image_path <- "C:/Users/Suzanne.Bonamour/OneDrive - LPO/2) Data/4) Courlis/Data/3) images/"

data_path_serveur <- "C:/Users/Suzanne.Bonamour/Documents/Courlis/Data/1) data/"
data_generated_path_serveur <- "C:/Users/Suzanne.Bonamour/Documents/Courlis/Data/2) data_generated/"
data_image_path_serveur <- "C:/Users/Suzanne.Bonamour/Documents/Courlis/Data/3) images/"

###
####
# BOX --------------------------------------------------------------------------
####
###

BOX <- st_as_sf(st_as_sfc(st_bbox(c(xmin = -1.26, xmax = -0.945, ymax = 46.01, ymin = 45.78), crs = st_crs(4326))))
st_write(BOX, paste0(data_generated_path_serveur, "BOX.gpkg"), append = FALSE)
BOX <- st_read(paste0(data_generated_path_serveur, "BOX.gpkg"))
BOX_4326 <- st_transform(BOX, crs = 4326)

# map
tmap_mode("view")
map <- tm_scale_bar() +
  tm_shape(BOX) +
  tm_polygons(col = "green", alpha = 0.5) +
  tm_shape(RMO) +
  tm_polygons(col = "red", alpha = 0.5); map

###
####
# MAPs background --------------------------------------------------------------
####
###

# dept ---
dept <- st_read(paste0(data_path_serveur, "departements.gpkg"),
                layer = "contourdesdepartements"
)
dept17 <- dept[dept$code == 17, ]

dept_box <- st_intersection(dept, BOX_4326)
st_write(dept_box, paste0(data_generated_path_serveur, "dept_box.gpkg"), append = FALSE)

dept_BOX <- st_intersection(dept, BOX_4326)
st_write(dept_BOX, paste0(data_generated_path_serveur, "dept_BOX.gpkg"), append = FALSE)

# reserve ---
reserve <- st_read(paste0(data_path_serveur, "Réserve_naturelle/rnn/rnn/N_ENP_RNN_S_000.shp"))
RMO <- reserve[reserve$NOM_SITE=="Moëze-Oléron",]
rm(reserve)

###
####
# GPS DATA to load -----------------------------------------------------------------
####
###

# all csv at the same time
# data_path <- "C:/Users/Suzanne.Bonamour/OneDrive - LPO/2) Data/4) Courlis/GPS/0) Original_gps/Data_brute_GPS/Extraction_Courlis-cendre_29_08_2024/MOVEBANK_limitrack/"
data_path_gps <- "C:/Users/Suzanne.Bonamour/Documents/Courlis/GPS/0) Original_gps/Data_brute_GPS/Extraction_Courlis-cendre_29_08_2024/MOVEBANK_limitrack/"
files_limitrak <- paste0(data_path_gps, list.files(path = data_path_gps, pattern = "*.csv"))
dt_limitrack <- lapply(files_limitrak, fread, sep = ",")
all_limitrack <- rbindlist(dt_limitrack)

# data_path <- "C:/Users/Suzanne.Bonamour/OneDrive - LPO/2) Data/4) Courlis/GPS/0) Original_gps/Data_brute_GPS/Extraction_Courlis-cendre_29_08_2024/MOVEBANK_pp1083/"
data_path_gps <- "C:/Users/Suzanne.Bonamour/Documents/Courlis/GPS/0) Original_gps/Data_brute_GPS/Extraction_Courlis-cendre_29_08_2024/MOVEBANK_pp1083/"
files_pp1083 <- paste0(data_path_gps, list.files(path = data_path_gps, pattern = "*.csv"))
dt_pp1083 <- lapply(files_pp1083, fread, sep = ",")
all_pp1083 <- rbindlist(dt_pp1083)

# on garde les mêmes colonnes pour les deux data frame
all_limitrack_2 <- all_limitrack %>%
  dplyr::select(-comments)

# toute les données gps pour tous les ind
all_gps <- rbind(all_limitrack_2, all_pp1083)

# on libére un peu d'espace dans l'env
rm(all_limitrack_2)
rm(all_limitrack)
rm(all_pp1083)
rm(dt_limitrack)
rm(dt_pp1083)

# changement des noms de colonnes et selection des colonnes utiles
all_gps <- all_gps %>%
  dplyr::select(
    "event-id", "timestamp", "location-long", "location-lat",
    "acceleration-raw-x", "acceleration-raw-y", "acceleration-raw-z",
    "bar:barometric-height", "battery-charge-percent", "external-temperature",
    "ground-speed", "gls:light-level", "individual-local-identifier"
  ) %>%
  dplyr::rename(
    eventID = "event-id", time = "timestamp", lon = "location-long",
    lat = "location-lat", acc_x = "acceleration-raw-x", acc_y = "acceleration-raw-y",
    acc_z = "acceleration-raw-z", baro = "bar:barometric-height", battery = "battery-charge-percent", temp = "external-temperature",
    speed = "ground-speed", light = "gls:light-level", indID = "individual-local-identifier"
  )

# mise au propre des données
all_gps$indID[all_gps$indID == "\"\"Courlis cendré [FRP_EA635104]\"\""] <- "\"\"[FRP_EA635104]\"\""
all_gps$eventID <- substr(all_gps$eventID, 2, 20)
all_gps$indID <- stri_extract(all_gps$indID, regex = "(?<=\\[).*?(?=\\])")
all_gps$indID[all_gps$indID == " FRP_ EC103792"] <- "FRP_EC103792"
all_gps$indID[all_gps$indID == " FRP_ EC103792"] <- "FRP_EC103792"

# on garde que les lignes avec des données lon/lat
all_gps <- all_gps[!is.na(all_gps$lon), ]
all_gps <- all_gps[!is.na(all_gps$lat), ]

# bague 
all_gps$indID <- substring(all_gps$indID, first=5, last=12)


###
####
# DESCRIPTION tracks -----------------------------------------------------------
####
###

## tracking period -------------------------------------------------------------

tracking_period <- sort(as.numeric(as.character(unique(year(all_gps$time))))) ; tracking_period
first_track_day <- min(all_gps$time) ; first_track_day
last_track_day <- max(all_gps$time) ; last_track_day

## nb ind ----------------------------------------------------------------------

nb_ind <- length(unique(all_gps$indID)) ; nb_ind

## mean & min & max ~ ind --------------------------------------------------------------

all_gps$indID <- as.factor(all_gps$indID)

descript_dt_1 <- all_gps %>% 
  group_by(indID) %>% 
  summarise(nb_event_per_ind = length(eventID),
            recorded_period_per_ind = difftime(max(time), min(time), units = "days"),
            speed_mean = mean(speed, na.rm=T),
            speed_min = min(speed, na.rm=T),
            speed_max = max(speed, na.rm=T),
            acc_x_min = min(acc_x, na.rm=T),
            acc_x_max = max(acc_x, na.rm=T),
            acc_y_min = min(acc_x, na.rm=T),
            acc_y_max = max(acc_x, na.rm=T),
            acc_z_min = min(acc_x, na.rm=T),
            acc_z_max = max(acc_x, na.rm=T),
            temp_mean = mean(temp, na.rm=T),
            temp_min = min(temp, na.rm=T),
            temp_max = max(temp, na.rm=T))

## time lag between points -------------------------------------------------

time_lag_ind_dt <- all_gps %>%
  arrange(indID, time) %>%
  group_by(indID) %>%
  mutate(diff_time_max = as.numeric(max(time - lag(time), na.rm = T), units = 'mins'),
         diff_time_min = as.numeric(min(time - lag(time), na.rm = T), units = 'mins'),
         diff_time_mean = as.numeric(mean(time - lag(time), na.rm = T), units = 'mins'),
         diff_time_med = as.numeric(median(time - lag(time), na.rm = T), units = 'mins')) %>% 
  dplyr::select(indID, diff_time_max, diff_time_min, diff_time_mean, diff_time_med) %>% 
  distinct()

## sex -------------------------------------------------------------------------

# jeu de données avec le sex et l'age des ind
DATA_LIMI <- read_excel(paste0(data_path_serveur, "Age_Sex/DATA_LIMI.xlsx"))

# infos a récupérer pour ces ind
bague <- all_gps %>% 
  distinct(indID)

# pour garder seulement les ind qui nous interesse 
sex_1 <- DATA_LIMI %>% 
  filter(ACTION == "B") %>% 
  filter(BAGUE %in% bague$indID) %>% 
  dplyr::select(BAGUE, ACTION, SEXE, sexe, SEXE.2)

# on change tout le ? pour des NA pour comparer les colonnes
sex_1$SEXE[sex_1$SEXE=="?"] <- "NA"
sex_1$sexe[sex_1$sexe=="?"] <- "NA"
sex_1$SEXE.2[sex_1$SEXE.2=="?"] <- "NA"

# on garde qu'une ligne par ind si info identifque entre les 3 colonnes
sex_2 <- sex_1 %>% 
  distinct()

# on change les "NA" en NA (missing value)
sex_2[sex_2=="NA"] <- NA

# on rempli les cellule NA avec les infos existante 
library(stringi)
sex_3 <- sex_2 %>%
  group_by(BAGUE) %>%
  fill(SEXE, .direction = "downup") %>%
  fill(sexe, .direction = "downup") %>%
  fill(SEXE.2, .direction = "downup") %>% 
  distinct()

doublons <- as.data.frame(table(sex_3$BAGUE))
max(doublons$Freq)  # pas de doublon

# on considère que les incertitude sont certaines ^^
sex_3$SEXE <- substring(sex_3$SEXE, first=1, last=1)

# on mix les infos de toutes les colonnes 
sex_3$sex_ok <- sex_3$SEXE.2

sex_3 <- sex_3 %>% 
  mutate(sex_ok = ifelse(is.na(SEXE.2), sexe, SEXE.2)) %>% 
  mutate(sex_ok = ifelse(is.na(sex_ok), SEXE, SEXE.2))

sex_4 <- sex_3 %>% 
  dplyr::select(BAGUE, sex_ok) %>% 
  distinct() %>% 
  na.omit() %>% 
  rename(indID = BAGUE, sex = sex_ok)

# 49 ind dont on connait le sexe

table(sex_4$sex)
# 27 males, 22 femelles

all_gps <- left_join(all_gps, sex_4)

## age -------------------------------------------------------------------------

# pour garder seulement les ind qui nous interesse 
age_1 <- DATA_LIMI %>% 
  filter(ACTION == "B") %>% 
  filter(BAGUE %in% bague$indID) %>% 
  dplyr::select(BAGUE, Year, AGE)

# on change les "NA" en des NA pour comparer les colonnes
age_1$AGE[age_1$AGE=="NA"] <- NA

# on garde qu'une ligne par ind si info identifque entre les 3 colonnes
age_2 <- age_1 %>% 
  distinct()

# on rempli les cellule NA avec les infos existante 
age_3 <- age_2 %>%
  group_by(BAGUE) %>%
  fill(AGE, .direction = "downup") %>%
  distinct()

doublons <- as.data.frame(table(age_3$BAGUE))
max(doublons$Freq)  # des incongruences !

age_4 <- age_3
age_4$AGE[age_4$BAGUE=="EA580488"] <- "JUV"

age_5 <- age_4 %>% 
  distinct() %>% 
  na.omit() %>% 
  rename(indID = BAGUE, year_baguage = Year, age_baguage = AGE)

# 62 ind dont on connait l'age

table(age_5$age_baguage)
# 45 ad, 17 juv

all_gps <- left_join(all_gps, age_5)

tt <- all_gps
tt$year <- year(tt$time)
tt$baguage_gps <- as.numeric(tt$year_baguage - tt$year)

tt2 <- tt %>% 
  dplyr::select(indID, year, year_baguage, baguage_gps) %>% 
  distinct() %>% 
  filter(baguage_gps > 0)

table(tt$baguage_gps)

## month -----------------------------------------------------------------------

all_gps$month <- lubridate::month(all_gps$time, label=TRUE, abbr=TRUE, locale="English_United States")

unique(all_gps$month)

month_tracks_dt <- all_gps %>% # nb point par mois
  group_by(month) %>% 
  summarize(n = length(eventID))

month_ind_tracks_dt <- all_gps %>% # nb point par mois par ind 
  group_by(indID, month) %>% 
  summarize(n = length(eventID))

###
####
# SPATIAL data -----------------------------------------------------------------
####
###

# spatialisation in a sf object
all_gps_spa <- st_as_sf(all_gps, coords = c("lon", "lat"), crs = 4326)

all_gps_spa$lon <- all_gps$lon
all_gps_spa$lat <- all_gps$lat

###
####
# TRIP -------------------------------------------------------------------------
####
###

nrow(all_gps_spa[is.na(all_gps_spa$time),]) # ok, no na in time
all_gps_spa <- all_gps_spa[!is.na(all_gps_spa$time),]

# create a "trip" for each ind, by specifying the data columns that define the "TimeOrdered" quality of the records
all_gps_dt <- as.data.frame(all_gps_spa) # as data frame
all_gps_dt$ID <- all_gps_dt$indID
all_gps_dt <- all_gps_dt %>%
  dplyr::select(-geometry) # remove geom

all_trip <- all_gps_dt %>%
  group_by(ID) %>%
  dplyr::select(x = lon,
                y = lat,
                DateTime = time,
                # ID = ID,
                everything()) %>%
  trip() # need a data frame (sans geom)

###
####
# SPEED limit error ------------------------------------------------------------
####
###

# McConnel Speedilter realistic velocity was set at 100 km/h
all_trip$filter <- speedfilter(all_trip, max.speed = 100)  # speed in km/h
summary(all_trip$filter) # ok 225 locations were removed

all_trip_sf <- st_as_sf(all_trip)

# on garde que les point avec une vitesse de moins de 100 km/h
all_trip_sf_TRUE <- all_trip_sf %>%
  filter(filter == TRUE)

# recup des lon & lat en colonnes
all_trip_sf_TRUE$lon <- st_coordinates(all_trip_sf_TRUE)[,1]
all_trip_sf_TRUE$lat <- st_coordinates(all_trip_sf_TRUE)[,2]

# save
st_write(all_trip_sf_TRUE, paste0(data_generated_path_serveur, "all_trip_sf_TRUE.gpkg"), append = FALSE)

###
####
# STATIONARY 27 km/h -----------------------------------------------------------
####
###

# McConnel Speedilter realistic velocity was set at 27 km/h
all_trip$stationary <- speedfilter(all_trip, max.speed = 27)  # speed in km/h
summary(all_trip$stationary) # ok 225 locations were removed

all_trip_stationary_sf <- st_as_sf(all_trip)

# save
st_write(all_trip_stationary_sf, paste0(data_generated_path_serveur, "all_trip_stationary_sf.gpkg"), append = FALSE)

###
####
# V1 - TIME LAG 30 min minimum ------------------------------------------------------
####
###

# remove all point after one time lag > 30 min

all_trip_stationary_sf <- st_read(paste0(data_generated_path_serveur, "all_trip_stationary_sf.gpkg"))

max_time_lag = 30

all_trip_stationary_sf_timeLag <- all_trip_stationary_sf %>% 
  arrange(indID, DateTime) %>%
  group_by(indID) %>%
  mutate(timeLag = as.numeric(DateTime - lag(DateTime), units = 'mins'))

date_timeLag_dt <- all_trip_stationary_sf_timeLag %>% 
  arrange(indID, DateTime) %>%
  group_by(indID) %>%
  filter(timeLag >= max_time_lag) %>% 
  mutate(Date_timeLag = min(DateTime)) %>% 
  dplyr::select(indID, Date_timeLag) %>% 
  st_drop_geometry() %>% 
  distinct()

all_with_date_timeLag <- left_join(all_trip_stationary_sf_timeLag, date_timeLag_dt)

all_with_date_timeLag_2 <- all_with_date_timeLag %>% 
  arrange(indID, DateTime) %>%
  group_by(indID) %>%
  filter(DateTime < Date_timeLag)

tt <- as.data.frame(table(all_with_date_timeLag_2$indID))











p <- ggplot(all_trip_stationary_sf_timeLag, aes(DateTime, timeLag, group = ID, color = ID)) +
  geom_line() +
  geom_hline(yintercept=30, color = "red", size = 3) +
  geom_hline(yintercept=60, color = "orange", size = 3); p




# save
st_write(all_trip_stationary_sf, paste0(data_generated_path_serveur, "all_trip_stationary_sf.gpkg"), append = FALSE)



###
####
# INTERPOLATION every 30 min ---------------------------------------------------
####
###

all_trip_stationary_sf <- st_read(paste0(data_generated_path_serveur, "all_trip_stationary_sf.gpkg"))

all_stationary <- all_trip_sf_TRUE

## create ltraj object, to store trajectories of animals
all_stationary.ltraj <- as.ltraj(xy = bind_cols(x = all_stationary$lon, 
                                                y = all_stationary$lat),
                                 date = all_stationary$DateTime,
                                 id = all_stationary$ID)

## re-sample tracks every 30 minutes (60*30 sec)
all_stationary.interp <- redisltraj(all_stationary.ltraj, 60*30, type="time")
all_stationary.interp <- ld(all_stationary.interp) %>% 
  mutate(longitude = x,latitude = y) # convert objects of class ltraj from and to dataframes

# sf
inter_sf <- st_as_sf(all_stationary.interp, coords = c("longitude", "latitude"), crs=4326)

# save
st_write(inter_sf, paste0(data_generated_path_serveur, "inter_sf.gpkg"), append = FALSE)

###
####
# V2 - TIME LAG 30 min minimum ------------------------------------------------------
####
###

####
# remove interpolated points between date with a time lag > 30 min

inter_sf <- st_read(paste0(data_generated_path_serveur, "inter_sf.gpkg"))

# identifying date intervals

all_trip_stationary_sf <- st_read(paste0(data_generated_path_serveur, "all_trip_stationary_sf.gpkg"))

max_time_lag = 30

all_trip_stationary_sf_timeLag <- all_trip_stationary_sf %>% 
  arrange(indID, DateTime) %>%
  group_by(indID) %>%
  mutate(timeLag = as.numeric(DateTime - lag(DateTime), units = 'mins'))

date_timeLag_dt <- all_trip_stationary_sf_timeLag %>% 
  st_drop_geometry() %>% 
  arrange(indID, DateTime) %>%
  group_by(indID) %>%
  mutate(Date_before_timeLag = lag(DateTime),
         Date_after_timeLag = lead(DateTime),
         diff_before_after = as.numeric(Date_after_timeLag - Date_before_timeLag, units = 'mins')) %>% 
  filter(timeLag > max_time_lag) %>% 
  dplyr::select(indID, DateTime, Date_before_timeLag, Date_after_timeLag, diff_before_after) %>% 
  distinct()

# overlaping date intervals

aa <- date_timeLag_dt %>%
  arrange(indID, DateTime) %>%
  group_by(indID) %>%
  mutate(overlap = int_overlaps(interval(Date_before_timeLag, Date_after_timeLag),
                                interval(lag(Date_before_timeLag), lag(Date_after_timeLag)))) %>% 
  na.omit()


aa$group <- cumsum(!aa$overlap) + 1

aa_FALSE <- aa %>%
  arrange(indID, DateTime) %>%
  group_by(indID) %>% 
  filter(overlap==FALSE)

aa_FALSE_2 <- aa_FALSE %>% 
  dplyr::rename(starting_gap = Date_before_timeLag, ending_gap = Date_after_timeLag) %>% 
  dplyr::select(indID, starting_gap, ending_gap)
  
aa_TRUE <- aa %>%
  arrange(indID, DateTime) %>%
  group_by(indID) %>% 
  filter(overlap==TRUE) %>% 
  # dplyr::select(-DateTime) %>% 
  distinct()

aa_TRUE_2 <- aa_TRUE %>% 
  arrange(indID, DateTime) %>%
  group_by(indID, group) %>%
  mutate(starting_gap = min(Date_before_timeLag),
         ending_gap = max(Date_after_timeLag)) %>% 
  ungroup() %>% 
  arrange(indID, DateTime) %>%
  # mutate(n = 1:length(indID)) %>%
  dplyr::select(indID, starting_gap, ending_gap) %>% 
  distinct()

aa_all <- rbind(aa_FALSE_2, aa_TRUE_2)

aa_all$n <- 1:length(aa_all$indID)

tt <- as.data.frame(table(aa_all$indID))

# verif qu'il n'y a plus d'overlap 
verif <- aa_all %>%
  arrange(n) %>%
  group_by(indID) %>%
  mutate(overlap = int_overlaps(interval(starting_gap, ending_gap),
                                interval(lag(starting_gap), lag(ending_gap)))) %>% 
  na.omit()

as.data.frame(table(verif$overlap))

all_gap_ok <- verif %>% 
  filter(overlap == FALSE) %>% 
  dplyr::select(indID, starting_gap, ending_gap)
  
all_gap_ok$n <- 1:length(all_gap_ok$indID)

all_gap_ok_2 <- all_gap_ok 


# changement all_gap_part_1, 2, 3, 4 pour recoller le tout ensuite car trop gros
#done
all_gap_part_1 <- all_gap_ok_2 %>% 
filter(between(n, 1, 20000))

all_gap_part_2 <- all_gap_ok_2 %>%
  filter(between(n, 20001, 30000))

# all_gap_part_3 <- all_gap_ok_2 %>%
#   filter(between(n, 40001, 60000))
# 
# all_gap_part_4 <- all_gap_ok_2 %>%
#   filter(between(n, 60001, max(n)))

inter_remove <- inter_sf

inter_remove$to_remove <- NA

inter_remove_all <- list()

# i = 1

for (i in unique(all_gap_ok_2$n)){
  
  print(i)
  
  ind_i = all_gap_ok_2$indID[all_gap_ok_2$n==i]
  start_i = all_gap_ok_2$starting_gap[all_gap_ok_2$n==i]
  end_i = all_gap_ok_2$ending_gap[all_gap_ok_2$n==i]
  
  inter_remove_i <- inter_remove %>%
    filter(id %in% ind_i) %>% 
    mutate(to_remove = case_when(to_remove == "remove" ~ to_remove,
                                 between(date, start_i, end_i) == TRUE ~ "remove",
                                 between(date, start_i, end_i) == FALSE ~ "keep"))
  
  # inter_remove_all <- rbind(inter_remove_all, inter_remove_i)
  inter_remove_all[[length(inter_remove_all) + 1]] <- inter_remove_i
}

# Combiner les résultats de la liste en une seule table
# inter_remove_all_part_1 <- bind_rows(inter_remove_all)
inter_remove_all_part_1 <- rbindlist(inter_remove_all_part_1)
inter_remove_all_part_2 <- rbindlist(inter_remove_all_part_2)
inter_remove_all_part_3 <- rbindlist(inter_remove_all_part_3)
inter_remove_all_part_4 <- rbindlist(inter_remove_all_part_4)

as.data.frame(table(inter_remove_all_part_1$to_remove))

# save
st_write(inter_remove_all_part_1, paste0(data_generated_path_serveur, "inter_remove_all_part_1.gpkg"), append = FALSE)
st_write(inter_remove_all_part_2, paste0(data_generated_path_serveur, "inter_remove_all_part_2.gpkg"), append = FALSE)
st_write(inter_remove_all_part_3, paste0(data_generated_path_serveur, "inter_remove_all_part_3.gpkg"), append = FALSE)
st_write(inter_remove_all_part_4, paste0(data_generated_path_serveur, "inter_remove_all_part_4.gpkg"), append = FALSE)

beep(3)

rm(inter_remove_all_part_1)
rm(inter_remove_all_part_2)
rm(inter_remove_all_part_3)
rm(inter_remove_all_part_4)

###
####
# V3 - TIME LAG 30 min minimum ------------------------------------------------------
####
###

####
# remove interpolated points between date with a time lag > 30 min

inter_sf <- st_read(paste0(data_generated_path_serveur, "inter_sf.gpkg"))

# identifying date intervals

all_trip_stationary_sf <- st_read(paste0(data_generated_path_serveur, "all_trip_stationary_sf.gpkg"))

max_time_lag = 30

all_trip_stationary_sf_timeLag <- all_trip_stationary_sf %>% 
  arrange(indID, DateTime) %>%
  group_by(indID) %>%
  mutate(timeLag = as.numeric(DateTime - lag(DateTime), units = 'mins'))

date_timeLag_dt <- all_trip_stationary_sf_timeLag %>% 
  st_drop_geometry() %>% 
  arrange(indID, DateTime) %>%
  group_by(indID) %>%
  mutate(Date_before_timeLag = lag(DateTime),
         Date_after_timeLag = lead(DateTime),
         diff_before_after = as.numeric(Date_after_timeLag - Date_before_timeLag, units = 'mins')) %>% 
  filter(timeLag > max_time_lag) %>% 
  dplyr::select(indID, DateTime, Date_before_timeLag, Date_after_timeLag, diff_before_after) %>% 
  distinct()

# overlaping date intervals

aa <- date_timeLag_dt %>%
  arrange(indID, DateTime) %>%
  group_by(indID) %>%
  mutate(overlap = int_overlaps(interval(Date_before_timeLag, Date_after_timeLag),
                                interval(lag(Date_before_timeLag), lag(Date_after_timeLag)))) %>% 
  na.omit()


aa$group <- cumsum(!aa$overlap) + 1

aa_FALSE <- aa %>%
  arrange(indID, DateTime) %>%
  group_by(indID) %>% 
  filter(overlap==FALSE)

aa_FALSE_2 <- aa_FALSE %>% 
  dplyr::rename(starting_gap = Date_before_timeLag, ending_gap = Date_after_timeLag) %>% 
  dplyr::select(indID, starting_gap, ending_gap)

aa_TRUE <- aa %>%
  arrange(indID, DateTime) %>%
  group_by(indID) %>% 
  filter(overlap==TRUE) %>% 
  # dplyr::select(-DateTime) %>% 
  distinct()

aa_TRUE_2 <- aa_TRUE %>% 
  arrange(indID, DateTime) %>%
  group_by(indID, group) %>%
  mutate(starting_gap = min(Date_before_timeLag),
         ending_gap = max(Date_after_timeLag)) %>% 
  ungroup() %>% 
  arrange(indID, DateTime) %>%
  # mutate(n = 1:length(indID)) %>%
  dplyr::select(indID, starting_gap, ending_gap) %>% 
  distinct()

aa_all <- rbind(aa_FALSE_2, aa_TRUE_2)

aa_all$n <- 1:length(aa_all$indID)

tt <- as.data.frame(table(aa_all$indID))

# verif qu'il n'y a plus d'overlap 
verif <- aa_all %>%
  arrange(n) %>%
  group_by(indID) %>%
  mutate(overlap = int_overlaps(interval(starting_gap, ending_gap),
                                interval(lag(starting_gap), lag(ending_gap)))) %>% 
  na.omit()

as.data.frame(table(verif$overlap))

all_gap_ok <- verif %>% 
  filter(overlap == FALSE) %>% 
  dplyr::select(indID, starting_gap, ending_gap)

all_gap_ok$n <- 1:length(all_gap_ok$indID)

all_gap_ok_2 <- all_gap_ok %>%
  filter(between(n, 1, 100))

inter_remove <- inter_sf

# ChatGPT ###

# Initialisation
inter_remove$to_remove <- NA
inter_remove_all <- list() 

# Diviser le travail par groupes de 'n'
unique_n <- unique(all_gap_ok_2$n)
batch_size <- 10  # Nombre de groupes à traiter dans chaque lot

# Traiter par lots
for (start in seq(1, length(unique_n), by = batch_size)) {
  end <- min(start + batch_size - 1, length(unique_n))  # S'assurer que le dernier lot ne dépasse pas la taille
  batch_n <- unique_n[start:end]
  
  # Filtrer et traiter uniquement les groupes du lot actuel
  for (i in batch_n) {
    print(paste("Traitement de n =", i))
    
    # Extraire les indices, start et end pour ce groupe
    ind_i <- all_gap_ok_2$indID[all_gap_ok_2$n == i]
    start_i <- all_gap_ok_2$starting_gap[all_gap_ok_2$n == i]
    end_i <- all_gap_ok_2$ending_gap[all_gap_ok_2$n == i]
    
    # Filtrer et muter la colonne 'to_remove'
    inter_remove_i <- inter_remove %>%
      filter(id %in% ind_i) %>%  
      mutate(to_remove = case_when(
        to_remove == "remove" ~ to_remove,
        between(date, start_i, end_i) ~ "remove",
        TRUE ~ "keep"
      ))
    
    # Ajouter à la liste
    inter_remove_all[[length(inter_remove_all) + 1]] <- inter_remove_i
  }
  
  # Combiner les résultats pour ce lot
  inter_remove_batch <- bind_rows(inter_remove_all)
  
  # Réinitialiser la liste pour le prochain lot
  inter_remove_all <- list()  
  
  # Optionnel : Sauvegarder temporairement les résultats du lot si nécessaire
  write.table(inter_remove_batch, 
            paste0("C:/Users/Suzanne.Bonamour/Documents/Courlis/Data/2) data_generated/time_lag/inter_remove_batch_", start, "_", end, ".csv"),
            sep = ";" , row.names = F)
}

# Combiner tous les résultats
# inter_remove_all_final <- bind_rows(inter_remove_all)

time_lag_path <- "C:/Users/Suzanne.Bonamour/Documents/Courlis/Data/2) data_generated/time_lag/"
files_time_lag <- paste0(time_lag_path, list.files(path = time_lag_path, pattern = "*.csv"))
dt_time_lag <- lapply(files_time_lag, fread, sep = ";")
all_time_lag <- rbindlist(dt_time_lag)





# # Liste des fichiers CSV à charger
# file_list <- list.files(path = "C:/Users/Suzanne.Bonamour/Documents/Courlis/Data/2) data_generated/time_lag", pattern = "*.csv", full.names = TRUE)
# 
# # Lire les fichiers CSV et s'assurer que les colonnes sont identiques
# csv_data_list <- lapply(file_list, function(file) {
#   # Lire chaque fichier CSV
#   data <- read.csv(file)
#   
#   # Vérifier que les colonnes sont les mêmes dans tous les fichiers
#   if (!identical(names(data), names(csv_data_list[[1]]))) {
#     stop("Les colonnes du fichier ", file, " ne correspondent pas aux autres fichiers.")
#   }
#   
#   return(data)
# })
# 
# # Fusionner les fichiers
# inter_remove_all_final <- bind_rows(csv_data_list)


all_time_lag_2 <- all_time_lag %>% 
  distinct()



















# code perso























all_gap_part_2 <- all_gap_ok_2 %>%
  filter(between(n, 20001, 30000))

# all_gap_part_3 <- all_gap_ok_2 %>%
#   filter(between(n, 40001, 60000))
# 
# all_gap_part_4 <- all_gap_ok_2 %>%
#   filter(between(n, 60001, max(n)))

inter_remove <- inter_sf

inter_remove$to_remove <- NA

inter_remove_all <- list()

# i = 1

for (i in unique(all_gap_part_1$n)){
  
  print(i)
  
  ind_i = all_gap_part_1$indID[all_gap_part_1$n==i]
  start_i = all_gap_part_1$starting_gap[all_gap_part_1$n==i]
  end_i = all_gap_part_1$ending_gap[all_gap_part_1$n==i]
  
  inter_remove_i <- inter_remove %>%
    filter(id %in% ind_i) %>% 
    mutate(to_remove = case_when(to_remove == "remove" ~ to_remove,
                                 between(date, start_i, end_i) == TRUE ~ "remove",
                                 between(date, start_i, end_i) == FALSE ~ "keep"))
  
  # inter_remove_all <- rbind(inter_remove_all, inter_remove_i)
  inter_remove_all[[length(inter_remove_all) + 1]] <- inter_remove_i
}

# Combiner les résultats de la liste en une seule table
# inter_remove_all_part_1 <- bind_rows(inter_remove_all)
inter_remove_all_part_1 <- rbindlist(inter_remove_all_part_1)
inter_remove_all_part_2 <- rbindlist(inter_remove_all_part_2)
inter_remove_all_part_3 <- rbindlist(inter_remove_all_part_3)
inter_remove_all_part_4 <- rbindlist(inter_remove_all_part_4)

as.data.frame(table(inter_remove_all_part_1$to_remove))

# save
st_write(inter_remove_all_part_1, paste0(data_generated_path_serveur, "inter_remove_all_part_1.gpkg"), append = FALSE)
st_write(inter_remove_all_part_2, paste0(data_generated_path_serveur, "inter_remove_all_part_2.gpkg"), append = FALSE)
st_write(inter_remove_all_part_3, paste0(data_generated_path_serveur, "inter_remove_all_part_3.gpkg"), append = FALSE)
st_write(inter_remove_all_part_4, paste0(data_generated_path_serveur, "inter_remove_all_part_4.gpkg"), append = FALSE)

beep(3)

rm(inter_remove_all_part_1)
rm(inter_remove_all_part_2)
rm(inter_remove_all_part_3)
rm(inter_remove_all_part_4)

###
####
# V4 - TIME LAG 30 min minimum ------------------------------------------------------
####
###

####
# remove interpolated points between date with a time lag > 30 min

inter_sf <- st_read(paste0(data_generated_path_serveur, "inter_sf.gpkg"))

# identifying date intervals

all_trip_stationary_sf <- st_read(paste0(data_generated_path_serveur, "all_trip_stationary_sf.gpkg"))

max_time_lag = 30

all_trip_stationary_sf_timeLag <- all_trip_stationary_sf %>% 
  arrange(indID, DateTime) %>%
  group_by(indID) %>%
  mutate(timeLag = as.numeric(DateTime - lag(DateTime), units = 'mins'))

date_timeLag_dt <- all_trip_stationary_sf_timeLag %>% 
  st_drop_geometry() %>% 
  arrange(indID, DateTime) %>%
  group_by(indID) %>%
  mutate(Date_before_timeLag = lag(DateTime),
         Date_after_timeLag = lead(DateTime),
         diff_before_after = as.numeric(Date_after_timeLag - Date_before_timeLag, units = 'mins')) %>% 
  filter(timeLag > max_time_lag) %>% 
  dplyr::select(indID, DateTime, Date_before_timeLag, Date_after_timeLag, diff_before_after) %>% 
  distinct()

# overlaping date intervals

aa <- date_timeLag_dt %>%
  arrange(indID, DateTime) %>%
  group_by(indID) %>%
  mutate(overlap = int_overlaps(interval(Date_before_timeLag, Date_after_timeLag),
                                interval(lag(Date_before_timeLag), lag(Date_after_timeLag)))) %>% 
  na.omit()


aa$group <- cumsum(!aa$overlap) + 1

aa_FALSE <- aa %>%
  arrange(indID, DateTime) %>%
  group_by(indID) %>% 
  filter(overlap==FALSE)

aa_FALSE_2 <- aa_FALSE %>% 
  dplyr::rename(starting_gap = Date_before_timeLag, ending_gap = Date_after_timeLag) %>% 
  dplyr::select(indID, starting_gap, ending_gap)

aa_TRUE <- aa %>%
  arrange(indID, DateTime) %>%
  group_by(indID) %>% 
  filter(overlap==TRUE) %>% 
  # dplyr::select(-DateTime) %>% 
  distinct()

aa_TRUE_2 <- aa_TRUE %>% 
  arrange(indID, DateTime) %>%
  group_by(indID, group) %>%
  mutate(starting_gap = min(Date_before_timeLag),
         ending_gap = max(Date_after_timeLag)) %>% 
  ungroup() %>% 
  arrange(indID, DateTime) %>%
  # mutate(n = 1:length(indID)) %>%
  dplyr::select(indID, starting_gap, ending_gap) %>% 
  distinct()

aa_all <- rbind(aa_FALSE_2, aa_TRUE_2)

aa_all$n <- 1:length(aa_all$indID)

tt <- as.data.frame(table(aa_all$indID))

# verif qu'il n'y a plus d'overlap 
verif <- aa_all %>%
  arrange(n) %>%
  group_by(indID) %>%
  mutate(overlap = int_overlaps(interval(starting_gap, ending_gap),
                                interval(lag(starting_gap), lag(ending_gap)))) %>% 
  na.omit()

as.data.frame(table(verif$overlap))

all_gap_ok <- verif %>% 
  filter(overlap == FALSE) %>% 
  dplyr::select(indID, starting_gap, ending_gap)

all_gap_ok$n <- 1:length(all_gap_ok$indID)

all_gap_ok_2 <- all_gap_ok %>% 
  filter(between(n, 1, 500)) 

# inspi ChatGPT ###
# Initialisation

inter_remove <- inter_sf %>% 
  head(20000)
inter_remove$to_remove <- NA
inter_remove_all <- list()

# Diviser les données en blocs de taille de lignes
block_size <- 100  # Taille du bloc (en nombre de lignes)
total_rows <- nrow(inter_remove)
num_blocks <- ceiling(total_rows / block_size)

# Traiter chaque bloc
for (block in 1:num_blocks) {
  print(paste("Traitement du bloc", block))
  
  # Déterminer les indices du bloc
  start_row <- (block - 1) * block_size + 1
  end_row <- min(block * block_size, total_rows)
  
  # Extraire les lignes du bloc
  inter_remove_block <- inter_remove[start_row:end_row, ]
  
for (i in unique(all_gap_ok_2$n)){
  
  print(i)
  
  ind_i = all_gap_ok_2$indID[all_gap_ok_2$n==i]
  start_i = all_gap_ok_2$starting_gap[all_gap_ok_2$n==i]
  end_i = all_gap_ok_2$ending_gap[all_gap_ok_2$n==i]
  
  inter_remove_i <- inter_remove_block %>%
    filter(id %in% ind_i) %>% 
    mutate(to_remove = case_when(to_remove == "remove" ~ to_remove,
                                 between(date, start_i, end_i) == TRUE ~ "remove",
                                 between(date, start_i, end_i) == FALSE ~ "keep"))
  
  # inter_remove_all <- rbind(inter_remove_all, inter_remove_i)
  inter_remove_all[[length(inter_remove_all) + 1]] <- inter_remove_i
  
  
}
  
  
  # Combiner les résultats pour ce lot
  inter_remove_batch <- bind_rows(inter_remove_all)
  
  # Réinitialiser la liste pour le prochain lot
  # inter_remove_all <- list()
  
  # Optionnel : Sauvegarder temporairement les résultats du lot si nécessaire
  write.table(inter_remove_batch, 
              paste0("C:/Users/Suzanne.Bonamour/Documents/Courlis/Data/2) data_generated/time_lag/inter_remove_batch_", start, "_", end, ".csv"),
              sep = ";" , row.names = F)
  
}

# Combiner tous les résultats
# inter_remove_all_final <- bind_rows(inter_remove_all)

# time_lag_path <- "C:/Users/Suzanne.Bonamour/Documents/Courlis/Data/2) data_generated/time_lag/"
# files_time_lag <- paste0(time_lag_path, list.files(path = time_lag_path, pattern = "*.csv"))
# dt_time_lag <- lapply(files_time_lag, fread, sep = ";")
# all_time_lag <- rbindlist(dt_time_lag)
















# Combiner les résultats de la liste en une seule table
# inter_remove_all_part_1 <- bind_rows(inter_remove_all)
inter_remove_all_part_1 <- rbindlist(inter_remove_all_part_1)
inter_remove_all_part_2 <- rbindlist(inter_remove_all_part_2)
inter_remove_all_part_3 <- rbindlist(inter_remove_all_part_3)
inter_remove_all_part_4 <- rbindlist(inter_remove_all_part_4)

as.data.frame(table(inter_remove_all_part_1$to_remove))

# save
st_write(inter_remove_all_part_1, paste0(data_generated_path_serveur, "inter_remove_all_part_1.gpkg"), append = FALSE)
st_write(inter_remove_all_part_2, paste0(data_generated_path_serveur, "inter_remove_all_part_2.gpkg"), append = FALSE)
st_write(inter_remove_all_part_3, paste0(data_generated_path_serveur, "inter_remove_all_part_3.gpkg"), append = FALSE)
st_write(inter_remove_all_part_4, paste0(data_generated_path_serveur, "inter_remove_all_part_4.gpkg"), append = FALSE)

beep(3)

rm(inter_remove_all_part_1)
rm(inter_remove_all_part_2)
rm(inter_remove_all_part_3)
rm(inter_remove_all_part_4)









# ChatGPT
# en parallèle ?????









library(dplyr)
library(furrr)
library(data.table)

all_gap_ok_2 <- all_gap_ok #%>% 
  # filter(between(n, 1, 500)) 

inter_remove <- inter_sf #%>% 
  # head(20000)

# inspi ChatGPT ###
# Initialisation

# Convertir les dataframes en data.table pour de meilleures performances
setDT(inter_remove)
setDT(all_gap_ok_2)

# Initialiser la colonne 'to_remove'
inter_remove$to_remove <- NA

# Planifier le parallélisme (4 cœurs ici, ajuste selon ta machine)
plan(multisession, workers = 4)

# Fonction pour traiter chaque groupe de n
process_group <- function(i) {
  print(paste("Traitement de n =", i))
  
  # Extraire les indices, start et end pour ce groupe
  ind_i <- all_gap_ok_2[n == i, indID]
  start_i <- all_gap_ok_2[n == i, starting_gap]
  end_i <- all_gap_ok_2[n == i, ending_gap]
  
  # Filtrer et muter la colonne 'to_remove' avec case_when
  inter_remove_i <- inter_remove[id %in% ind_i]  # Filtrer par id
  inter_remove_i[, to_remove := case_when(
    to_remove == "remove" ~ to_remove,  # Si déjà "remove", garder "remove"
    between(date, start_i, end_i) ~ "remove",  # Si date est entre start_i et end_i, mettre "remove"
    TRUE ~ "keep"  # Sinon, mettre "keep"
  )]
  
  return(inter_remove_i)
}

# Diviser les groupes de n en lots (en fonction de la taille)
unique_n <- unique(all_gap_ok_2$n)
batch_size <- 10000  # Taille des lots (ajuste en fonction de la mémoire et des données)

# Diviser les groupes en lots
batches <- split(unique_n, ceiling(seq_along(unique_n) / batch_size))

# Traiter les lots en parallèle
inter_remove_all_final <- future_map_dfr(batches, function(batch) {
  lapply(batch, process_group) %>%
    bind_rows()
})

inter_remove_all_final_2 <- inter_remove_all_final %>% 
  distinct()

# Désactiver le parallélisme après l'exécution
plan(sequential)

# Optionnel : Sauvegarder temporairement les résultats du lot si nécessaire
write.table(inter_remove_all_final_2, "C:/Users/Suzanne.Bonamour/Documents/Courlis/Data/2) data_generated/time_lag/inter_remove_all_final_2_V4.csv",
            sep = ";" , row.names = F)

###
####
# V5 - TIME LAG 30 min minimum ------------------------------------------------------
####
###

####
# remove interpolated points between date with a time lag > 30 min

inter_sf <- st_read(paste0(data_generated_path_serveur, "inter_sf.gpkg"))

# identifying date intervals

all_trip_stationary_sf <- st_read(paste0(data_generated_path_serveur, "all_trip_stationary_sf.gpkg"))

max_time_lag = 30

all_trip_stationary_sf_timeLag <- all_trip_stationary_sf %>% 
  arrange(indID, DateTime) %>%
  group_by(indID) %>%
  mutate(timeLag = as.numeric(DateTime - lag(DateTime), units = 'mins'))

date_timeLag_dt <- all_trip_stationary_sf_timeLag %>% 
  st_drop_geometry() %>% 
  arrange(indID, DateTime) %>%
  group_by(indID) %>%
  mutate(Date_before_timeLag = lag(DateTime),
         Date_after_timeLag = lead(DateTime),
         diff_before_after = as.numeric(Date_after_timeLag - Date_before_timeLag, units = 'mins')) %>% 
  filter(timeLag > max_time_lag) %>% 
  dplyr::select(indID, DateTime, Date_before_timeLag, Date_after_timeLag, diff_before_after) %>% 
  distinct()

# overlaping date intervals

aa <- date_timeLag_dt %>%
  arrange(indID, DateTime) %>%
  group_by(indID) %>%
  mutate(overlap = int_overlaps(interval(Date_before_timeLag, Date_after_timeLag),
                                interval(lag(Date_before_timeLag), lag(Date_after_timeLag)))) %>% 
  na.omit()


aa$group <- cumsum(!aa$overlap) + 1

aa_FALSE <- aa %>%
  arrange(indID, DateTime) %>%
  group_by(indID) %>% 
  filter(overlap==FALSE)

aa_FALSE_2 <- aa_FALSE %>% 
  dplyr::rename(starting_gap = Date_before_timeLag, ending_gap = Date_after_timeLag) %>% 
  dplyr::select(indID, starting_gap, ending_gap)

aa_TRUE <- aa %>%
  arrange(indID, DateTime) %>%
  group_by(indID) %>% 
  filter(overlap==TRUE) %>% 
  # dplyr::select(-DateTime) %>% 
  distinct()

aa_TRUE_2 <- aa_TRUE %>% 
  arrange(indID, DateTime) %>%
  group_by(indID, group) %>%
  mutate(starting_gap = min(Date_before_timeLag),
         ending_gap = max(Date_after_timeLag)) %>% 
  ungroup() %>% 
  arrange(indID, DateTime) %>%
  # mutate(n = 1:length(indID)) %>%
  dplyr::select(indID, starting_gap, ending_gap) %>% 
  distinct()

aa_all <- rbind(aa_FALSE_2, aa_TRUE_2)

aa_all$n <- 1:length(aa_all$indID)

tt <- as.data.frame(table(aa_all$indID))

# verif qu'il n'y a plus d'overlap 
verif <- aa_all %>%
  arrange(n) %>%
  group_by(indID) %>%
  mutate(overlap = int_overlaps(interval(starting_gap, ending_gap),
                                interval(lag(starting_gap), lag(ending_gap)))) %>% 
  na.omit()

as.data.frame(table(verif$overlap))

all_gap_ok <- verif %>% 
  filter(overlap == FALSE) %>% 
  dplyr::select(indID, starting_gap, ending_gap)

all_gap_ok$n <- 1:length(all_gap_ok$indID)

# making space :
# making space :
rm(all_trip_stationary_sf)
rm(all_trip_stationary_sf_timeLag)
gc()
# making space :
# making space :


# for the loops ###

all_gap_ok_2 <- all_gap_ok 

# all_gap_ok_2 <- all_gap_ok_2 #%>% 
  # filter(between(n, 1350, 1450))

inter_remove <- inter_sf

# inter_remove$to_remove <- NA

inter_remove <- inter_remove %>% 
  st_drop_geometry()

# inter_remove_all <- list()
remove_i_all <- list()

# n = 1

# ind_i = "4025065"




for (ind_i in unique(all_gap_ok_2$indID)){
  
  print(ind_i)
  
  # ind_i table
  ind_i_dt <- all_gap_ok_2 %>% 
    filter(indID == ind_i)
  
  for (n in unique(ind_i_dt$n)){
    
    # print(n)
    
    start_i_n <- ind_i_dt$starting_gap[ind_i_dt$n == n]
    end_i_n <- ind_i_dt$ending_gap[ind_i_dt$n == n]
    
    remove_i_n <- inter_remove %>%
      filter(id == ind_i & between(date, start_i_n, end_i_n))
    
    remove_i_all[[length(remove_i_all) + 1]] <- remove_i_n
    
  }
  
  remove_i_all_dt <- bind_rows(remove_i_all)
  
  remove_i_all <- list()
  
  # sauvegarde pour chaque ind
  write.table(remove_i_all_dt, 
              paste0("C:/Users/Suzanne.Bonamour/Documents/Courlis/Data/2) data_generated/time_lag/inter_remove_ind_", ind_i, ".csv"),
              sep = ";" , row.names = F)
  
  rm(remove_i_all_dt)
  
}


# all csv at the same time
time_lag_path <- "C:/Users/Suzanne.Bonamour/Documents/Courlis/Data/2) data_generated/time_lag/"
files_time_lag <- paste0(time_lag_path, list.files(path = time_lag_path, pattern = "*.csv"))
dt_time_lag <- lapply(files_time_lag, fread, sep = ";")
all_time_lag_remove <- rbindlist(dt_time_lag)

# sauvegarde pour chaque ind
write.table(all_time_lag_remove, 
            "C:/Users/Suzanne.Bonamour/Documents/Courlis/Data/2) data_generated/all_time_lag_remove.csv",
            sep = ";", row.names = F)



# remove les points du jeu de données GPS de base

inter_sf <- st_read(paste0(data_generated_path_serveur, "inter_sf.gpkg"))

dt_base <- inter_sf %>% 
  st_drop_geometry() #%>% 
  # dplyr::select(x, y , id, date, pkey)

rr <- as.data.frame(all_time_lag_remove$pkey)
names(rr) <- "pkey"

df_diff <- anti_join(dt_base, rr)

length(all_time_lag_remove$pkey)
length(dt_base$pkey)

length(df_diff$pkey)

length(dt_base$pkey) - length(all_time_lag_remove$pkey)


point_no_gap <- left_join(df_diff, inter_sf)


beep()

###
####
# MAREE ------------------------------------------------------------------------
####
###

tides <- read_csv("~/Courlis/Data/1) data/Maree/tides.csv")
tides$DateTime <- paste0(tides$y_m_d, " ", tides$time)

tides <- tides %>% 
  na.omit() %>% 
  distinct()

###
####
# 24h - BEHAVIORS --------------------------------------------------------------------
####
###

# inter_sf <- st_read(paste0(data_generated_path_serveur, "inter_sf.gpkg"))

# foraging : 2h avant-après la marée base 
# roosting : 2h avant-après la marée haute 
  # + hauteur d'eau min > à mean(tides$height[tides$type=="High"]) = 5.5m

aa <- point_no_gap %>%
  arrange(date)

# same time zone
aa$date <- lubridate::with_tz(aa$date, tzone = "Europe/Paris")
tides$DateTime <- lubridate::with_tz(tides$DateTime, tzone = "Europe/Paris")

tides <- tides %>% 
  filter(y_m_d > "2015-10-12")

unique_date <- tides %>% 
  dplyr::select(y_m_d) %>% 
  distinct() %>% 
  arrange(y_m_d) %>%
  mutate(i = 1:length(y_m_d))

bb <- left_join(tides, unique_date) #%>% 
  # filter(between(i, 210, 220))

bb$DateTime <- as.POSIXct(bb$DateTime)

behaviour_dt_1 = NULL

# i = 210

max_i = max(bb$i)

### pour chaque point maree
for (i in unique(bb$i)) {
  
  # pour stopper à l'avant dernière marée (car pas d'info sur la marée d'après pour time_i1)
  if (i == max_i)
    break;

  print(i)
  
  # data of the date i
  dt_i <- bb[bb$i == i,]
  
  # data of the low of the date i
  dt_i_low <- dt_i[dt_i$type=="Low",]
  dt_i_low$n <- c(1:length(dt_i_low$ID))
  # data of the high of the date i
  dt_i_high <- dt_i[dt_i$type=="High",]
  dt_i_high$n <- c(1:length(dt_i_high$ID))
  
  ### pour chaque maree low du jour i
      for (n in unique(dt_i_low$n)){
        
          time_i_n = dt_i_low$DateTime[dt_i_low$n == n]
          
          # period limit
          foraging_low_i_n = time_i_n - (3600*2)
          foraging_up_i_n = time_i_n + (3600*2)
          
          height_low_i_n = dt_i_low$height[dt_i_low$n == n]
          
          info_low <- c(i, as.character(time_i_n), as.character(foraging_low_i_n), 
                        as.character(foraging_up_i_n), "Low", height_low_i_n)
          
          # assignation des behaviours dans info
          all_info_low <- aa %>%
            mutate(behavior = case_when(between(date, info_low[3], info_low[4]) ~ "foraging")) %>%
            filter(behavior == "foraging" | behavior == "roosting") %>%
            dplyr::select(id, date, behavior, x, y) %>%
            mutate(height = height_low_i_n) %>% 
            st_drop_geometry()
        
          if(nrow(all_info_low) == 0){
            print(i) ; print("No Data Available")
          } else {
            # save
            all_info_low_2 <- cbind(all_info_low, i, n)
            behaviour_dt_1 <- rbind(all_info_low_2, behaviour_dt_1)
          }
    }
  
  for (n in unique(dt_i_high$n)){
    
    time_i_n = dt_i_high$DateTime[dt_i_high$n == n]
    
    # period limit
    roosting_low_i_n = time_i_n - (3600*2)
    roosting_up_i_n = time_i_n + (3600*2)
    
    height_high_i_n = dt_i_high$height[dt_i_high$n == n]
    
    if (height_high_i_n < mean(tides$height[tides$type=="High"]))
      next;
    
    info_high <- c(i, as.character(time_i_n), as.character(roosting_low_i_n), 
                  as.character(roosting_up_i_n), "High", height_high_i_n)
    
    # assignation des behaviours
    all_info_high <- aa %>%
      mutate(behavior = case_when(between(date, info_high[3], info_high[4]) ~ "roosting")) %>%
      filter(behavior == "foraging" | behavior == "roosting") %>%
      dplyr::select(id, date, behavior, x, y) %>%
      mutate(height = height_high_i_n) %>% 
      st_drop_geometry()
    
    if(nrow(all_info_high) == 0){
      print(i) ; print("No Data Available")
    } else {
      # save
      all_info_high_2 <- cbind(all_info_high, i, n)
      behaviour_dt_1 <- rbind(all_info_high_2, behaviour_dt_1)
    }
  }
}

behaviour_dt_1 <- as.data.frame(behaviour_dt_1)

behaviour_dt_1 <- behaviour_dt_1 %>% 
  arrange(date) 

head(behaviour_dt_1) ; tail(behaviour_dt_1)

table(behaviour_dt_1$behavior)

# save
write.table(behaviour_dt_1, paste0(data_generated_path_serveur, "behaviour_24h_after_erreur_no_gap.txt"),
            append = FALSE, sep = ";", dec = ".", col.names = TRUE)

beep(3)










###
####
# INSIDE the BOX ---------------------------------------------------------------
####
###

behaviour_24h <- read.table(paste0(data_generated_path_serveur, "behaviour_24h_after_erreur_no_gap.txt"), 
                             header = T, sep = ";")
behaviour_24h_spa <- st_as_sf(behaviour_24h, coords = c("x", "y"), crs = 4326)
behaviour_24h_spa$lon <- behaviour_24h$x
behaviour_24h_spa$lat <- behaviour_24h$y

# inside the box
behaviour_24h_BOX <- st_intersection(behaviour_24h_spa, BOX_4326) # time consuming...
st_write(behaviour_24h_BOX, paste0(data_generated_path_serveur, "behaviour_24h_BOX_no_gap.gpkg"), append = FALSE)

###
####
# 1000 POINTS & 56 DAYS ---------------------------------------------------------
###

behaviour_24h_BOX <- st_read(paste0(data_generated_path_serveur, "behaviour_24h_BOX_no_gap.gpkg"))

behaviour_24h_BOX_1000_56 <- behaviour_24h_BOX %>%
  group_by(id) %>%
  mutate(nb_point = n()) %>%
  mutate(nb_days = difftime(max(date), min(date), units = "days")) %>%
  filter(nb_point >= 1000) %>%
  filter(nb_days >= 28*2)

behaviour_24h_nb_ind_1000_56 <- length(unique(behaviour_24h_BOX_1000_56$id)) ; behaviour_24h_nb_ind_1000_56

st_write(behaviour_24h_BOX_1000_56, paste0(data_generated_path_serveur, "behaviour_24h_BOX_1000_56_no_gap.gpkg"), append = FALSE)

# add sex

behaviour_24h_BOX_1000_56 <- st_read(paste0(data_generated_path_serveur, "behaviour_24h_BOX_1000_56_no_gap.gpkg"))

sex_dt <- sex_4 %>% 
  rename(id = indID)

behaviour_24h_BOX_1000_56_sex <- left_join(behaviour_24h_BOX_1000_56, sex_dt)

# add age 

age_dt <- age_5 %>% 
  rename(id = indID)

behaviour_24h_BOX_1000_56_sex_age <- left_join(behaviour_24h_BOX_1000_56_sex, age_dt)

st_write(behaviour_24h_BOX_1000_56_sex_age, paste0(data_generated_path_serveur, "behaviour_24h_BOX_1000_56_sex_age_no_gap.gpkg"), append = FALSE)

###
####
# JOUR & NUIT ------------------------------------------------------------------
####
###

# behaviour_24h_BOX_1000_56_sex_age <- st_read(paste0(data_generated_path_serveur, "behaviour_24h_BOX_1000_56_sex_age.gpkg"))
# 
# 
# tides <- read_csv("~/Courlis/Data/1) data/Maree/tides.csv")
# 
# tides$DateTime <- paste0(tides$y_m_d, " ", tides$hour)
# 
# tides_low <- tides %>% 
#   filter(type == "Low")


###
####
# VISUALISATION ----------------------------------------------------------------
####
###

behaviour_24h_BOX_1000_56 <- st_read(paste0(data_generated_path_serveur, "behaviour_24h_BOX_1000_56_no_gap.gpkg"))

# group of individual for plots 
behaviour_24h_gp_ind <- behaviour_24h_BOX_1000_56 %>% 
  group_by(id) %>% 
  st_drop_geometry() %>% 
  dplyr::select(id, nb_point) %>% 
  arrange(nb_point) %>%
  distinct() 

behaviour_24h_gp_ind_2 <- behaviour_24h_gp_ind %>% 
  bind_cols(group = rep(1:6, length.out = nrow(behaviour_24h_gp_ind)))

dt_map_group_behaviour_24h <- left_join(behaviour_24h_BOX_1000_56, behaviour_24h_gp_ind_2)

# map

tmap_mode("plot")
behaviour_24h_BOX_maps_2 <- tm_scale_bar() +
  tm_shape(dept_BOX) +
  tm_polygons() +
  tm_shape(dt_map_group_behaviour_24h) +
  tm_dots(col = 'id', alpha = 0.5) +
  tm_facets(by = c("behavior"), free.coords = FALSE) +
  tmap_options(max.categories = 70) +
  tm_shape(RMO) +
  tm_borders(col = "black"); behaviour_24h_BOX_maps_2

tmap_save(behaviour_24h_BOX_maps_2, paste0(data_image_path_serveur, "/behaviour_24h_BOX_2_no_gap.png"), dpi = 600)

tmap_mode("view")
behaviour_24h_BOX_maps_1 <- tm_scale_bar() +
  tm_shape(dt_map_group_behaviour_24h) +
  tm_dots(col = 'id', alpha = 0.5) +
  tm_facets(by = c("group", "behavior"), free.coords = FALSE) +
  tmap_options(max.categories = 70) +
  tm_shape(RMO) +
  tm_borders(col = "black"); behaviour_24h_BOX_maps_1

beep(3)

