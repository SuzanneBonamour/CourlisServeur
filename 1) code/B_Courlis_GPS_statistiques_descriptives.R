
# Run la partie "starting blok", 
# puis seulement runner la dernière partie "SAVE" avant la partie à souhaitée / en cours de travail

# STARTING BLOCK ---------------------------------------------------------------

# beep lorsqu'il y a une erreur 
options(error = function() {beep(7)})

# Nettoyage de l'environnement
rm(list=ls()) 

# time zone
with_tz(Sys.time(), "Europe/Paris")

## Chargement des bibliothèques nécessaires -------------------------------------

library(tidyr)
# library(lubridate)
library(sf)
library(ggplot2)
# library(classInt)
# library(ggOceanMaps)
# library(remotes)
# library(leaflet)
# library(trip)
# library(adehabitatLT)
# library(extrafont)
# library(ggthemes)
# library(raster)
# library(graticule)
# library(data.table)
# library(stringi)
# library(terra)
# library(tmap)
# library(spData)
# library(gridExtra)
# library(readxl)
# library(ggalt)
# library(tidyverse)
library(beepr)
# library(readr)
library(dplyr)
library(ggbreak)

## Chemins de données ----------------------------------------------------------

data_path_serveur <- "D:/Projets_Suzanne/Courlis/Data/1) data/"
data_generated_path_serveur <- "D:/Projets_Suzanne/Courlis/Data/2) data_generated/"
data_image_path_serveur <- "D:/Projets_Suzanne/Courlis/Data/3) images/"

## Zone d'intérêt (box) --------------------------------------------------------

# BOX <- st_as_sf(st_as_sfc(st_bbox(c(xmin = -1.26, xmax = -0.945, ymax = 46.01, ymin = 45.78), crs = st_crs(4326)))) # Définition d'une boîte englobante avec des coordonnées spécifiques
# st_write(BOX, paste0(data_generated_path_serveur, "BOX.gpkg"), append = FALSE) # Sauvegarde de la boîte dans un fichier GeoPackage
BOX <- st_read(paste0(data_generated_path_serveur, "BOX.gpkg")) # Lecture de la boîte depuis le fichier sauvegardé
BOX_4326 <- st_transform(BOX, crs = 4326) # Transformation de la boîte au CRS 4326 (coordonnées géographiques)

###
####
## Font de carte ---------------------------------------------------------------
####
###

# Monde ---
data("World")

# Europe ---
Europe <- world[world$subregion=="Eastern Europe" |
                  world$subregion=="Northern Europe" |
                  world$subregion=="Western Europe",]

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

tmap_mode("view") 
map <- tm_scale_bar() +                 
  tm_shape(BOX) +                        
  tm_polygons(col = "green", alpha = 0.5) +  
  tm_shape(RMO) +                       
  tm_polygons(col = "red", alpha = 0.5) ; map

###
####
# GPS DATA to load -------------------------------------------------------------
####
###

# brute 
GPS_raw <- read.table(paste0(data_generated_path_serveur, "all_gps.txt"), header = TRUE, sep = ";")
GPS_raw <- st_as_sf(GPS_raw, coords = c("lon", "lat"), crs = 4326) # Conversion en objet sf avec projection WGS84 (EPSG:4326)

# brute dans la box 
GPS_box <- st_read(file.path(data_generated_path_serveur, "all_gps_spa_BOX.gpkg"))

# cleaned
GPS <- st_read(file.path(data_generated_path_serveur, "behaviour_24h_BOX_1000_56_sex_age.gpkg"))

###
####
# DESCRIPTION ------------------------------------------------------------------
####
###

## Jeu de données brut ---------------------------------------------------------

### Période de suivi -----------------------------------------------------------

tracking_period <- sort(unique(lubridate::year(GPS_raw$time))) 
print(tracking_period)

first_track_day <- min(GPS_raw$time) 
print(first_track_day)

last_track_day <- max(GPS_raw$time) 
print(last_track_day)

### Nombre d'individus suivis --------------------------------------------------

nb_ind <- length(unique(GPS_raw$indID)) 
print(nb_ind)

### Décalage temporel entre les points GPS -------------------------------------

time_lag_ind_dt <- GPS_raw %>%
  arrange(indID, time) %>%
  group_by(indID) %>%
  mutate(diff_time = ymd_hms(time) - lag(ymd_hms(time))) %>%
  summarize(
    diff_time_max = as.numeric(max(diff_time, na.rm = TRUE), units = "mins"),
    diff_time_min = as.numeric(min(diff_time, na.rm = TRUE), units = "mins"),
    diff_time_mean = as.numeric(mean(diff_time, na.rm = TRUE), units = "mins"),
    diff_time_med = as.numeric(median(diff_time, na.rm = TRUE), units = "mins"),
    diff_time_sd = as.numeric(sd(diff_time, na.rm = TRUE), units = "mins")
  )

time_lag_ind_dt

# plot
library(ggbreak)
time_lag_plot <- ggplot(time_lag_ind_dt, aes(reorder(indID, diff_time_mean), diff_time_mean, 
                                             color = diff_time_max)) +
  geom_errorbar(aes(ymin=diff_time_mean-diff_time_sd, ymax=diff_time_mean+diff_time_sd),
                width=0, color="grey") +
  geom_point(size = 4) +
  geom_point(aes(reorder(indID, diff_time_mean), diff_time_med), 
                 size = 1, color = "red") +
  coord_flip() +
  scale_y_break(c(200, 1000000), scales = 0.2) +
  scale_y_break(c(-900000, -10), scales = 5) +
    # scale_color_manual(values=cols) +
  theme_classic() +
  # theme(legend.position = "top") +
  # annotate("text", label = "time lag median", x = 2, y = 40, size = 8, colour = "red") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(title="time lag entre deux points gps enregistrés",
       x ="Ind", y = "time lag (+/- écart-type)", 
       fill="", 
       color = "time lag max") ; time_lag_plot

### Carte ----------------------------------------------------------------------

# box ---

box_europe <- st_as_sf(st_as_sfc(st_bbox(c(xmin = -30, ymin = 15, xmax = 65, ymax = 72), crs = st_crs(4326)))) # Définition d'une boîte englobante avec des coordonnées spécifiques
# box_europe <- st_transform(box_europe, crs = 4326) # Transformation de la boîte au CRS 4326 (coordonnées géographiques)

World_box <- st_intersection(World, box_europe)

# groupe d'ind ---

ind_dt <- as.data.frame(unique(GPS_raw$indID))

ind_dt <- ind_dt %>% 
  rename(indID = `unique(GPS_raw$indID)`) %>% 
  mutate(gp = rep(1:6, length.out = length(ind_dt[,1])))

GPS_raw <- left_join(GPS_raw, ind_dt)

# maps ---

tmap_mode("plot")
GPS_raw_plotgp_1 <- tm_scale_bar() +
  tm_shape(World_box) +
  tm_polygons() +
  tm_shape(GPS_raw[GPS_raw$gp == 1,]) +
  tm_dots(col = 'indID', alpha = 0.5) +
  tmap_options(max.categories = 70) +
  tm_shape(RMO) +
  tm_borders(col = "black") ; GPS_raw_plotgp_1

GPS_raw_plotgp_2 <- tm_scale_bar() +
  tm_shape(World_box) +
  tm_polygons() +
  tm_shape(GPS_raw[GPS_raw$gp == 2,]) +
  tm_dots(col = 'indID', alpha = 0.5) +
  tmap_options(max.categories = 70) +
  tm_shape(RMO) +
  tm_borders(col = "black") ; GPS_raw_plotgp_2

GPS_raw_plotgp_3 <- tm_scale_bar() +
  tm_shape(World_box) +
  tm_polygons() +
  tm_shape(GPS_raw[GPS_raw$gp == 3,]) +
  tm_dots(col = 'indID', alpha = 0.5) +
  tmap_options(max.categories = 70) +
  tm_shape(RMO) +
  tm_borders(col = "black") ; GPS_raw_plotgp_3

GPS_raw_plotgp_4 <- tm_scale_bar() +
  tm_shape(World_box) +
  tm_polygons() +
  tm_shape(GPS_raw[GPS_raw$gp == 4,]) +
  tm_dots(col = 'indID', alpha = 0.5) +
  tmap_options(max.categories = 70) +
  tm_shape(RMO) +
  tm_borders(col = "black") ; GPS_raw_plotgp_4

GPS_raw_plotgp_5 <- tm_scale_bar() +
  tm_shape(World_box) +
  tm_polygons() +
  tm_shape(GPS_raw[GPS_raw$gp == 5,]) +
  tm_dots(col = 'indID', alpha = 0.5) +
  tmap_options(max.categories = 70) +
  tm_shape(RMO) +
  tm_borders(col = "black") ; GPS_raw_plotgp_5

GPS_raw_plotgp_6 <- tm_scale_bar() +
  tm_shape(World_box) +
  tm_polygons() +
  tm_shape(GPS_raw[GPS_raw$gp == 6,]) +
  tm_dots(col = 'indID', alpha = 0.5) +
  tmap_options(max.categories = 70) +
  tm_shape(RMO) +
  tm_borders(col = "black") ; GPS_raw_plotgp_6

GPS_raw_plot <- tmap_arrange(GPS_raw_plotgp_1, GPS_raw_plotgp_2,
                             GPS_raw_plotgp_3, GPS_raw_plotgp_4,
                             GPS_raw_plotgp_5, GPS_raw_plotgp_6)

tmap_save(GPS_raw_plot, paste0(data_image_path_serveur, "/GPS_raw_plot.png"), dpi = 600)

## Jeu de données cleaned ------------------------------------------------------


### Periode de présence --------------------------------------------------------

GPS$month <- lubridate::month(GPS$date_UTC, label = TRUE, abbr = TRUE)
GPS$year <- lubridate::year(GPS$date_UTC)

as_tibble(GPS)

# Afficher les mois uniques présents dans le dataset
unique(GPS$month)

# Nombre total de points par mois
month_tracks_dt <- GPS %>%
  group_by(month) %>%
  summarize(n = n()) # n() pour compter les lignes par groupe

# Nombre de points par mois et par individu
period_ind_tracks_dt <- GPS %>%
  st_drop_geometry() %>% 
  group_by(id) %>%
  mutate(month_min = min(month),
         month_max = max(month),
         year_min = min(year),
         year_max = max(year)) %>% 
  dplyr::select(id, year, month_min, month_max, year_min, year_max) %>% 
  distinct()

mig_ind_tracks_dt <- GPS %>%
  st_drop_geometry() %>% 
  group_by(id, year) %>%
  mutate(month_min = min(month),
         month_max = max(month),
         period = difftime(max(date_UTC), min(date_UTC), units = "days")) %>% 
  dplyr::select(id, year, month_min, month_max, period) %>% 
  distinct()

month_ind_tracks_dt <- GPS %>%
  group_by(id, month) %>%
  summarize(n = n())  # n() pour compter les lignes par groupe

# année de présence pour chaque ind ---

year_ind_plot <- ggplot(period_ind_tracks_dt) +
  geom_segment(aes(x=reorder(id, year_min), xend=reorder(id, year_min), 
                   y=year_min, yend=year_max, size = 2), 
               color="black") +
  geom_point(aes(x=reorder(id, year_min), y=year_min), 
             size=3, shape = 21, fill = "white") +
  geom_point(aes(x=reorder(id, year_min), y=year_max), 
             size=3, shape = 21, fill = "white") +
  geom_point(aes(x=reorder(id, year_min), y=year), 
             size=2, shape = 21, fill = "white") +
  coord_flip() +
  theme_classic() +
  theme(
    legend.position = "none",
  ) +
  xlab("") +
  ylab("Année de présence dans la zone d'étude") ; year_ind_plot

# nombre de point GPS enregistré chaque mois ---

month_plot <- ggplot(GPS, aes(month)) +
  geom_bar() +
  theme_classic() +
  labs(title="",
       x ="MOis", y = "Nombre de points GPS dans la zone d'étude") ; month_plot

# periode de suivi (mois) de présence pour chauqe ind ---

mig_ind_plot <- ggplot(mig_ind_tracks_dt) +
  geom_segment(aes(x=reorder(id, -period), xend=reorder(id, -period), 
                   y=month_min, yend=month_max, 
                   alpha = 0.2, size = 2), 
               color="black") +
  geom_point(aes(x=reorder(id, -period), y=month_min), 
             size=3, shape = 21, fill = "white") +
  geom_point(aes(x=reorder(id, -period), y=month_max), 
             size=3, shape = 21, fill = "white") +
  coord_flip() +
  theme_classic() +
  theme(
    legend.position = "none",
  ) +
  xlab("") +
  ylab("Période de présence dans la zone d'étude (nuance de gris : années)") ; mig_ind_plot

# nb d'année d'enregistrement pour chaque ind ---

nb_anne <- GPS %>% 
  st_drop_geometry() %>% 
  group_by(id) %>% 
  mutate(nb_year = max(year) - min(year) + 1) %>% 
  dplyr::select(id, nb_year) %>% 
  distinct()

hist(nb_anne$nb_year)

### Sexe -----------------------------------------------------------------------

# nombre de point GPS enregistré pour chaque sexe

sex_dt <- GPS %>% 
  st_drop_geometry() %>% 
  group_by(sex) %>% 
  summarise(n = n())

### Age au baguage -------------------------------------------------------------

# nombre de point GPS enregistré pour chaque age au baguage

age_baguage_dt <- GPS %>% 
  st_drop_geometry() %>% 
  group_by(age_baguage) %>% 
  summarise(n = n())

### Sexe + Age au baguage ------------------------------------------------------

# nombre de point GPS enregistré pour chaque sexe et age au baguage

sexe_age_baguage_dt <- GPS %>% 
  st_drop_geometry() %>% 
  group_by(sex, age_baguage) %>% 
  summarise(n = n())

### Jour/Nuit ------------------------------------------------------------------

# nombre de point GPS enregistré en jour ou nuit

jour_nuit_dt <- GPS %>% 
  st_drop_geometry() %>% 
  group_by(jour_nuit) %>% 
  summarise(n = n())

### Type de marée --------------------------------------------------------------

# nombre de point GPS enregistré en jour ou nuit

maree_dt <- GPS %>% 
  st_drop_geometry() %>% 
  group_by(type_maree) %>% 
  summarise(n = n())

















###
####
# VISUALISATION point GPS ------------------------------------------------------
####
###

## Behavior ---------------------------------

tmap_mode("plot")
tmap_plot_behavior <- tm_scale_bar() +
  tm_shape(dept_BOX) +
  tm_polygons() +
  tm_shape(GPS) +
  tm_dots(col = 'id', alpha = 0.5) +
  tm_facets(by = "behavior", free.coords = FALSE) +
  tmap_options(max.categories = 70) +
  tm_shape(RMO) +
  tm_borders(col = "black") ; tmap_plot_behavior

tmap_save(tmap_plot_behavior, paste0(data_image_path_serveur, "/GPS_behavior.png"), dpi = 600)

tmap_mode("view")
tmap_view_behavior <- tm_scale_bar() +
  tm_shape(GPS) +
  tm_dots(col = 'id', alpha = 0.5) +
  tm_facets(by = "behavior", free.coords = FALSE) +
  tmap_options(max.categories = 70) +
  tm_shape(RMO) +
  tm_borders(col = "black") ; tmap_view_behavior

## Type de marée ----------------------------

type_maree_map <- GPS[GPS$behavior=="roosting",]

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

tmap_save(tmap_plot_maree, paste0(data_image_path_serveur, "/GPS_maree.png"), dpi = 600)

tmap_mode("view")
tmap_view_maree <- tm_scale_bar() +
  tm_shape(type_maree_map) +
  tm_dots(col = 'id', alpha = 0.5) +
  tm_facets(by = c("type_maree"), free.coords = FALSE) +
  tmap_options(max.categories = 70) +
  tm_shape(RMO) +
  tm_borders(col = "black") ; tmap_view_maree

## Sexe & Behavior --------------------------

tmap_mode("plot")
tmap_plot_behav_sex <- tm_scale_bar() +
  tm_shape(dept_BOX) +
  tm_polygons() +
  tm_shape(GPS) +
  tm_dots(col = 'id', alpha = 0.5) +
  tm_facets(by = c("behavior", "sex"), free.coords = FALSE) +
  tmap_options(max.categories = 70) +
  tm_shape(RMO) +
  tm_borders(col = "black") ; tmap_plot_behav_sex

tmap_save(tmap_plot_behav_sex, paste0(data_image_path_serveur, "/GPS_behav_sex.png"), dpi = 600)

tmap_mode("view")
tmap_view_behav_sex <- tm_scale_bar() +
  tm_shape(GPS) +
  tm_dots(col = 'id', alpha = 0.5) +
  tm_facets(by = c("behavior", "sex"), free.coords = FALSE) +
  tmap_options(max.categories = 70) +
  tm_shape(RMO) +
  tm_borders(col = "black") ; tmap_view_behav_sex

## Age & Behavior ---------------------------

tmap_mode("plot")
tmap_plot_behav_age <- tm_scale_bar() +
  tm_shape(dept_BOX) +
  tm_polygons() +
  tm_shape(GPS) +
  tm_dots(col = 'id', alpha = 0.5) +
  tm_facets(by = c("behavior", "AGE"), free.coords = FALSE) +
  tmap_options(max.categories = 70) +
  tm_shape(RMO) +
  tm_borders(col = "black") ; tmap_plot_behav_age

# Sauvegarde de la carte statique
tmap_save(tmap_plot_behav_age, paste0(data_image_path_serveur, "/GPS_behav_age.png"), dpi = 600)

tmap_mode("view")
tmap_view_behav_age <- tm_scale_bar() +
  tm_shape(GPS) +
  tm_dots(col = 'id', alpha = 0.5) +
  tm_facets(by = c("behavior", "AGE"), free.coords = FALSE) +
  tmap_options(max.categories = 70) +
  tm_shape(RMO) +
  tm_borders(col = "black") ; tmap_view_behav_age

# Signal sonore à la fin du script
beep(3)

# OLDOLDOLDOLD ------------------------------------------------------------------
### Statistiques par individu --------------------------------------------------

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

### Décalage temporel entre les points GPS --------------------------------------
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

