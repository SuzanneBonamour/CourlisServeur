
# Run la partie "starting blok", 
# puis seulement runner la dernière partie "SAVE" avant la partie à souhaitée / en cours de travail

# STARTING BLOCK ---------------------------------------------------------------

library(beepr)

# beep lorsqu'il y a une erreur 
# options(error = function() {beep(7)})
options(error = NULL)

# Nettoyage de l'environnement
rm(list=ls()) 

# time zone
library(lubrIDate)
with_tz(Sys.time(), "Europe/Paris")

## Chargement des bibliothèques nécessaires -------------------------------------

library(tIDyr)
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
library(tmap)
library(spData)
# library(grIDExtra)
# library(readxl)
# library(ggalt)
# library(tIDyverse)
# library(readr)
library(dplyr)
library(ggbreak)
library(stringr)

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
map <- tm_scalebar() +                 
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
# GPS_raw <- read.table(paste0(data_generated_path_serveur, "all_gps.txt"), header = TRUE, sep = ";")
GPS_raw <- read.csv(file = paste0(data_generated_path_serveur, "all_gps.csv"))
GPS_raw <- st_as_sf(GPS_raw, coords = c("lon", "lat"), crs = 4326) # Conversion en objet sf avec projection WGS84 (EPSG:4326)

# brute dans la box 
GPS_box <- st_read(file.path(data_generated_path_serveur, "all_gps_spa_BOX.gpkg"))

# cleaned
# GPS <- st_read(file.path(data_generated_path_serveur, "behaviour_24h_BOX_1000_56_sex_age_breche.gpkg"))
GPS <- st_read(file.path(data_generated_path_serveur, "GPS_clean.gpkg"))

###
####
# DESCRIPTION ------------------------------------------------------------------
####
###

## Jeu de données brut ---------------------------------------------------------

### Période de suivi -----------------------------------------------------------

tracking_period <- sort(unique(lubrIDate::year(GPS_raw$time))) 
print(tracking_period)

first_track_day <- min(GPS_raw$time) 
print(first_track_day)

last_track_day <- max(GPS_raw$time) 
print(last_track_day)

### Nombre d'indivIDus suivis --------------------------------------------------

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
time_lag_plot <- ggplot(time_lag_ind_dt, aes(reorder(indID, diff_time_mean), diff_time_mean, 
                                             color = diff_time_max)) +
  geom_errorbar(aes(ymin=diff_time_mean-diff_time_sd, ymax=diff_time_mean+diff_time_sd),
                wIDth=0, color="grey") +
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

ggsave(paste0(data_image_path_serveur, "/time_lag_plot.png"), 
       plot = time_lag_plot, wIDth = 6, height = 9, dpi = 300)

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
GPS_raw_plotgp_1 <- tm_scalebar() +
  tm_shape(World_box) +
  tm_polygons() +
  tm_shape(GPS_raw[GPS_raw$gp == 1,]) +
  tm_dots(col = 'indID', alpha = 0.5) +
  # tmap_options(max.categories = 70) +
  tm_shape(RMO) +
  tm_borders(col = "black") ; GPS_raw_plotgp_1

GPS_raw_plotgp_2 <- tm_scalebar() +
  tm_shape(World_box) +
  tm_polygons() +
  tm_shape(GPS_raw[GPS_raw$gp == 2,]) +
  tm_dots(col = 'indID', alpha = 0.5) +
  # tmap_options(max.categories = 70) +
  tm_shape(RMO) +
  tm_borders(col = "black") ; GPS_raw_plotgp_2

GPS_raw_plotgp_3 <- tm_scalebar() +
  tm_shape(World_box) +
  tm_polygons() +
  tm_shape(GPS_raw[GPS_raw$gp == 3,]) +
  tm_dots(col = 'indID', alpha = 0.5) +
  # tmap_options(max.categories = 70) +
  tm_shape(RMO) +
  tm_borders(col = "black") ; GPS_raw_plotgp_3

GPS_raw_plotgp_4 <- tm_scalebar() +
  tm_shape(World_box) +
  tm_polygons() +
  tm_shape(GPS_raw[GPS_raw$gp == 4,]) +
  tm_dots(col = 'indID', alpha = 0.5) +
  # tmap_options(max.categories = 70) +
  tm_shape(RMO) +
  tm_borders(col = "black") ; GPS_raw_plotgp_4

GPS_raw_plotgp_5 <- tm_scalebar() +
  tm_shape(World_box) +
  tm_polygons() +
  tm_shape(GPS_raw[GPS_raw$gp == 5,]) +
  tm_dots(col = 'indID', alpha = 0.5) +
  # tmap_options(max.categories = 70) +
  tm_shape(RMO) +
  tm_borders(col = "black") ; GPS_raw_plotgp_5

GPS_raw_plotgp_6 <- tm_scalebar() +
  tm_shape(World_box) +
  tm_polygons() +
  tm_shape(GPS_raw[GPS_raw$gp == 6,]) +
  tm_dots(col = 'indID', alpha = 0.5) +
  # tmap_options(max.categories = 70) +
  tm_shape(RMO) +
  tm_borders(col = "black") ; GPS_raw_plotgp_6

GPS_raw_plot <- tmap_arrange(GPS_raw_plotgp_1, GPS_raw_plotgp_2,
                             GPS_raw_plotgp_3, GPS_raw_plotgp_4,
                             GPS_raw_plotgp_5, GPS_raw_plotgp_6)

tmap_save(GPS_raw_plot, paste0(data_image_path_serveur, "/GPS_raw_plot.png"), dpi = 600)

## Jeu de données propre -------------------------------------------------------

### % de point dans la zone ----------------------------------------------------

# brute 

hiver = c("11","12","01","02")

# Extraction des coordonnées longitude et latitude
GPS_raw <- GPS_raw %>%
  mutate(lon = st_coordinates(.)[,1], lat = st_coordinates(.)[,2])

GPS_raw_hiver <- GPS_raw %>% 
  st_drop_geometry() %>% 
  distinct() %>% 
  mutate(month = as.character(substring(time, first=6, last=7))) %>% 
  filter(month %in% hiver)

table(GPS_raw_hiver$month)

nb_point_raw_hiver <- length(GPS_raw_hiver$eventID) ; nb_point_raw_hiver

# dans la box

GPS_raw_hiver_spa <- st_as_sf(GPS_raw_hiver, coords = c("lon", "lat"), crs = 4326) # Conversion en objet sf avec projection WGS84 (EPSG:4326)

GPS_hiver_within_box <- st_intersection(GPS_raw_hiver_spa, BOX_4326)

table(GPS_hiver_within_box$month)

nb_point_hiver_within_box <- length(GPS_hiver_within_box$eventID) ; nb_point_hiver_within_box

# % de point dans la zone vs ailleurs 

pourc_within_box <- nb_point_hiver_within_box / nb_point_raw_hiver * 100 ; pourc_within_box

### Periode de présence --------------------------------------------------------

GPS$month <- lubrIDate::month(GPS$datetime, label = TRUE, abbr = TRUE)
GPS$year <- lubrIDate::year(GPS$datetime)

as_tibble(GPS)

# Afficher les mois uniques présents dans le dataset
unique(GPS$month)

# Nombre total de points par mois
month_tracks_dt <- GPS %>%
  group_by(month) %>%
  summarize(n = n()) # n() pour compter les lignes par groupe

# Nombre de points par mois et par indivIDu
period_ind_tracks_dt <- GPS %>%
  st_drop_geometry() %>% 
  group_by(ID) %>%
  mutate(month_min = min(month),
         month_max = max(month),
         year_min = min(year),
         year_max = max(year)) %>% 
  dplyr::select(ID, sex, age, year, month_min, month_max, year_min, year_max) %>% 
  distinct()

mig_ind_tracks_dt <- GPS %>%
  st_drop_geometry() %>% 
  group_by(ID, year) %>%
  mutate(month_min = min(month),
         month_max = max(month),
         period = difftime(max(datetime), min(datetime), units = "days")) %>% 
  dplyr::select(ID, sex, age, year, month_min, month_max, period) %>% 
  distinct()

month_ind_tracks_dt <- GPS %>%
  group_by(ID, month) %>%
  summarize(n = n())  # n() pour compter les lignes par groupe

# année de présence pour chaque ind ---

year_ind_plot <- ggplot(period_ind_tracks_dt) +
  geom_segment(aes(x=reorder(ID, year_min), xend=reorder(ID, year_min), 
                   y=year_min, yend=year_max, size = 2), 
               color="black") +
  geom_point(aes(x=reorder(ID, year_min), y=year_min), 
             size=3, shape = 21, fill = "white") +
  geom_point(aes(x=reorder(ID, year_min), y=year_max), 
             size=3, shape = 21, fill = "white") +
  geom_point(aes(x=reorder(ID, year_min), y=year), 
             size=2, shape = 21, fill = "white") +
  coord_flip() +
  theme_classic() +
  theme(
    legend.position = "none",
  ) +
  xlab("") +
  ylab("Année de présence dans la zone d'étude") ; year_ind_plot

ggsave(paste0(data_image_path_serveur, "/year_ind_plot.png"), 
       plot = year_ind_plot, wIDth = 6, height = 9, dpi = 300)

# nombre de point GPS enregistré chaque mois ---

month_plot <- ggplot(GPS, aes(month)) +
  geom_bar() +
  theme_classic() +
  labs(title="",
       x ="Mois", y = "Nombre de points GPS dans la zone d'étude") ; month_plot

ggsave(paste0(data_image_path_serveur, "/month_plot.png"), 
       plot = month_plot, width = 6, height = 6, dpi = 300)

# periode de suivi (mois) de présence pour chauqe ind ---

mig_ind_tracks_dt$month_min <- str_replace_all(mig_ind_tracks_dt$month_min, "janv","1")
mig_ind_tracks_dt$month_min <- str_replace_all(mig_ind_tracks_dt$month_min, "févr","2")  
mig_ind_tracks_dt$month_min <- str_replace_all(mig_ind_tracks_dt$month_min, "mars","3")
mig_ind_tracks_dt$month_min <- str_replace_all(mig_ind_tracks_dt$month_min, "avr","4") 
mig_ind_tracks_dt$month_min <- str_replace_all(mig_ind_tracks_dt$month_min, "mai","5") 
mig_ind_tracks_dt$month_min <- str_replace_all(mig_ind_tracks_dt$month_min, "juin","6") 
mig_ind_tracks_dt$month_min <- str_replace_all(mig_ind_tracks_dt$month_min, "juil","7") 
mig_ind_tracks_dt$month_min <- str_replace_all(mig_ind_tracks_dt$month_min, "août","8") 
mig_ind_tracks_dt$month_min <- str_replace_all(mig_ind_tracks_dt$month_min, "sept","9") 
mig_ind_tracks_dt$month_min <- str_replace_all(mig_ind_tracks_dt$month_min, "oct","10") 
mig_ind_tracks_dt$month_min <- str_replace_all(mig_ind_tracks_dt$month_min, "nov","11")
mig_ind_tracks_dt$month_min <- str_replace_all(mig_ind_tracks_dt$month_min, "déc","12")

mig_ind_tracks_dt$month_max <- str_replace_all(mig_ind_tracks_dt$month_max, "janv","1")
mig_ind_tracks_dt$month_max <- str_replace_all(mig_ind_tracks_dt$month_max, "févr","2")  
mig_ind_tracks_dt$month_max <- str_replace_all(mig_ind_tracks_dt$month_max, "mars","3")
mig_ind_tracks_dt$month_max <- str_replace_all(mig_ind_tracks_dt$month_max, "avr","4") 
mig_ind_tracks_dt$month_max <- str_replace_all(mig_ind_tracks_dt$month_max, "mai","5") 
mig_ind_tracks_dt$month_max <- str_replace_all(mig_ind_tracks_dt$month_max, "juin","6") 
mig_ind_tracks_dt$month_max <- str_replace_all(mig_ind_tracks_dt$month_max, "juil","7") 
mig_ind_tracks_dt$month_max <- str_replace_all(mig_ind_tracks_dt$month_max, "août","8") 
mig_ind_tracks_dt$month_max <- str_replace_all(mig_ind_tracks_dt$month_max, "sept","9") 
mig_ind_tracks_dt$month_max <- str_replace_all(mig_ind_tracks_dt$month_max, "oct","10") 
mig_ind_tracks_dt$month_max <- str_replace_all(mig_ind_tracks_dt$month_max, "nov","11")
mig_ind_tracks_dt$month_max <- str_replace_all(mig_ind_tracks_dt$month_max, "déc","12")  
  
mig_ind_tracks_dt$month_min <- as.numeric(mig_ind_tracks_dt$month_min)
mig_ind_tracks_dt$month_max <- as.numeric(mig_ind_tracks_dt$month_max)

mig_ind_plot <- ggplot(mig_ind_tracks_dt) +
  geom_segment(aes(x=reorder(ID, -period), xend=reorder(ID, -period), 
                   y=month_min, yend=month_max, 
                   alpha = 0.2, size = 2), 
               color="black") +
  geom_point(aes(x=reorder(ID, -period), y=month_min), 
             size=3, shape = 21, fill = "white") +
  geom_point(aes(x=reorder(ID, -period), y=month_max), 
             size=3, shape = 21, fill = "white") +
  coord_flip() +
  theme_classic() +
  theme(
    legend.position = "none",
  ) +
  xlab("") +
  ylab("Période de présence dans la zone d'étude (nuance de gris : années)") ; mig_ind_plot

ggsave(paste0(data_image_path_serveur, "/mig_ind_plot.png"), 
       plot = mig_ind_plot, width = 6, height = 9, dpi = 300)

tt <- mig_ind_tracks_dt %>% 
  na.omit(age)

tt$age_f = factor(tt$age, levels=c('juv','adult'))

mig_age_ind_plot <- ggplot(tt) +
  geom_segment(aes(x=reorder(ID, -period), xend=reorder(ID, -period), 
                   y=month_min, yend=month_max, 
                   alpha = 0.2, size = 2), 
               color="black") +
  geom_point(aes(x=reorder(ID, -period), y=month_min), 
             size=3, shape = 21, fill = "white") +
  geom_point(aes(x=reorder(ID, -period), y=month_max), 
             size=3, shape = 21, fill = "white") +
  coord_flip() +
  facet_grid(.~ age_f) +
  theme_classic() +
  theme(
    legend.position = "none",
  ) +
  xlab("") +
  ylab("Période de présence dans la zone d'étude (nuance de gris : années)") ; mig_age_ind_plot

ggsave(paste0(data_image_path_serveur, "/mig_age_ind_plot.png"), 
       plot = mig_age_ind_plot, width = 6, height = 9, dpi = 300)

# nb d'année d'enregistrement pour chaque ind ---

nb_anne <- GPS %>% 
  st_drop_geometry() %>% 
  group_by(ID) %>% 
  mutate(nb_year = max(year) - min(year) + 1) %>% 
  dplyr::select(ID, nb_year) %>% 
  distinct()

png(paste0(data_image_path_serveur, "/hist_nb_year.png"))
hist_year <- hist(nb_anne$nb_year)
dev.off()

# ggsave(paste0(data_image_path_serveur, "/hist_nb_year.png"), 
#        plot = hist_year, wIDth = 6, height = 9, dpi = 300)

### Sexe -----------------------------------------------------------------------

# nombre de point GPS enregistré pour chaque sexe

sex_dt <- GPS %>% 
  st_drop_geometry() %>% 
  group_by(sex) %>% 
  summarise(n = n())

### Age ------------------------------------------------------------------------

#### Age au baguage ------------------------------------------------------------

# nombre de point GPS enregistré pour chaque age au baguage

age_baguage_dt <- GPS %>% 
  st_drop_geometry() %>% 
  group_by(age_baguage) %>% 
  summarise(n = n())

#### Age chronologique ---------------------------------------------------------

age_chrono_dt <- GPS %>% 
  st_drop_geometry() %>% 
  group_by(age) %>% 
  summarise(n = n())

#### Sexe + Age au baguage ------------------------------------------------------

# nombre de point GPS enregistré pour chaque sexe et age au baguage

sexe_age_baguage_dt <- GPS %>% 
  st_drop_geometry() %>% 
  group_by(sex, age_baguage) %>% 
  summarise(n = n())

#### Sexe + Age chronologique --------------------------------------------------

# nombre de point GPS enregistré pour chaque sexe et age

sexe_age_chrono_dt <- GPS %>% 
  st_drop_geometry() %>% 
  group_by(sex, age) %>% 
  summarise(n = n())

### Jour/Nuit ------------------------------------------------------------------

# nombre de point GPS enregistré en jour ou nuit

jour_nuit_dt <- GPS %>% 
  st_drop_geometry() %>% 
  group_by(jour_nuit) %>% 
  summarise(n = n()) ; jour_nuit_dt

### Type de marée --------------------------------------------------------------

maree_dt <- GPS %>% 
  st_drop_geometry() %>% 
  group_by(tides_high_type) %>% 
  summarise(n = n()) ; maree_dt

### Brèche ---------------------------------------------------------------------

breche_dt <- GPS %>% 
  st_drop_geometry() %>% 
  group_by(breche) %>% 
  summarise(n = n()) ; breche_dt

###
####
# VISUALISATION point GPS ------------------------------------------------------
####
###

## Behavior ---------------------------------

tmap_mode("plot")
tmap_plot_behavior <- tm_scalebar() +
  tm_shape(dept_BOX) +
  tm_polygons() +
  tm_shape(GPS) +
  tm_dots(fill = 'ID') +
  tm_facets(by = "behavior", free.coords = FALSE) +
  # tmap_options(max.categories = 70) +
  tm_shape(RMO) +
  tm_borders(col = "black") ; tmap_plot_behavior

tmap_save(tmap_plot_behavior, paste0(data_image_path_serveur, "/GPS_behavior.png"), dpi = 600)

GPS$behavior <- as.factor(GPS$behavior)

tmap_mode("plot")
tmap_plot_behavior_v2 <- tm_scalebar() +
  tm_shape(dept_BOX) +
  tm_polygons() +
  tm_shape(GPS) +
  tm_symbols(shape = 20, size = 1, fill_alpha = 0.1, fill = 'behavior') +
  # tmap_options(max.categories = 70) +
  tm_shape(RMO) +
  tm_borders(col = "black") ; tmap_plot_behavior_v2

tmap_save(tmap_plot_behavior_v2, paste0(data_image_path_serveur, "/GPS_behavior_v2.png"), dpi = 600)

tmap_mode("view")
tmap_view_behavior <- tm_scalebar() +
  tm_shape(GPS) +
  tm_dots(col = 'ID', fill_alpha = 0.5) +
  tm_facets(by = "behavior", free.coords = FALSE) +
  # tmap_options(max.categories = 70) +
  tm_shape(RMO) +
  tm_borders(col = "black") ; tmap_view_behavior

tmap_save(tmap_view_behavior, paste0(data_image_path_serveur, "/tmap_view_behavior.html"))

tmap_mode("view")
tmap_plot_behavior_v2 <- tm_scalebar() +
  tm_shape(dept_BOX) +
  tm_polygons() +
  tm_shape(GPS) +
  tm_symbols(shape = 20, size = 1, fill_alpha = 0.1, fill = 'behavior') +
  # tmap_options(max.categories = 70) +
  tm_shape(RMO) +
  tm_borders(col = "black") ; tmap_plot_behavior_v2

tmap_save(tmap_plot_behavior_v2, paste0(data_image_path_serveur, "/GPS_behavior_v2.html"), dpi = 600)


# ggplot 

# library(ggplot2)
# library(sf)
# library(dplyr)
# 
# # Assurez-vous que vos données sont bien au format sf
# dept_BOX_sf <- st_as_sf(dept_BOX)
# GPS_sf <- st_as_sf(GPS)
# RMO_sf <- st_as_sf(RMO)
# 
# # Création du plot
# ggplot() +
#   geom_sf(data = dept_BOX_sf, fill = "gray80", color = "black", alpha = 0.2) +  # Polygones des départements
#   geom_sf(data = GPS_sf, aes(color = ID), shape = 21, fill = NA) +  # Points GPS avec couleur par ID
#   geom_sf(data = RMO_sf, color = "black", linetype = "solid") +  # Bordures RMO
#   facet_wrap(~ behavior) +  # Facettes sur "behavior"
#   theme_minimal() +
#   theme(legend.position = "none") +
#   coord_sf()  # Ajustement des coordonnées

## Type de marée ----------------------------

type_maree_map <- GPS[GPS$behavior=="roosting",]

tmap_mode("plot")
tmap_plot_maree <- tm_scalebar() +
  tm_shape(dept_BOX) +
  tm_polygons() +
  tm_shape(type_maree_map) +
  tm_dots(fill_alpha = 0.5) +
  tm_facets(by = c("tides_high_type"), free.coords = FALSE) +
  tm_shape(RMO) +
  tm_borders(col = "black") ; tmap_plot_maree

tmap_save(tmap_plot_maree, paste0(data_image_path_serveur, "/GPS_maree.png"), dpi = 600)

tmap_mode("view")
tmap_view_maree <- tm_scalebar() +
  tm_shape(type_maree_map) +
  tm_dots(col = 'ID', fill_alpha = 0.5) +
  tm_facets(by = c("tides_high_type"), free.coords = FALSE) +
  tm_shape(RMO) +
  tm_borders(col = "black") ; tmap_view_maree

## Sexe & Behavior --------------------------

GPS_no_other <- GPS %>% 
  filter(behavior != "other")

GPS_no_other$behavior_sex <- paste(GPS_no_other$behavior, GPS_no_other$sex, sep = "_")

tmap_mode("plot")
tmap_plot_behav_sex <- tm_scalebar() +
  tm_shape(dept_BOX) +
  tm_polygons() +
  tm_shape(GPS_no_other) +
  tm_dots(fill = 'ID', 
          fill_alpha = 0.5) +
  tm_facets(by = c("behavior_sex")) +
  tm_shape(RMO) +
  tm_borders(col = "black") ; tmap_plot_behav_sex

tmap_save(tmap_plot_behav_sex, paste0(data_image_path_serveur, "/GPS_behav_sex.png"), dpi = 600)

tmap_mode("view")
tmap_view_behav_sex <- tm_scalebar() +
  tm_shape(GPS_no_other) +
  tm_dots(fill = 'ID', fill_alpha = 0.5) +
  tm_facets(by = c("behavior_sex"), free.coords = FALSE) +
  tm_shape(RMO) +
  tm_borders(col = "black") ; tmap_view_behav_sex

## Age au baguage & Behavior ---------------------------

GPS_no_other$behavior_age_baguage <- paste(GPS_no_other$behavior, GPS_no_other$age_baguage, sep = "_")

tmap_mode("plot")
tmap_plot_behav_age_baguage <- tm_scalebar() +
  tm_shape(dept_BOX) +
  tm_polygons() +
  tm_shape(GPS_no_other) +
  tm_dots(col = 'ID', fill_alpha = 0.5) +
  tm_facets(by = c("behavior_age_baguage"), free.coords = FALSE) +
  # tmap_options(max.categories = 70) +
  tm_shape(RMO) +
  tm_borders(col = "black") ; tmap_plot_behav_age_baguage

# Sauvegarde de la carte statique
tmap_save(tmap_plot_behav_age_baguage, paste0(data_image_path_serveur, "/GPS_behav_age_baguage.png"), dpi = 600)

tmap_mode("view")
tmap_view_behav_age_baguage <- tm_scalebar() +
  tm_shape(GPS) +
  tm_dots(col = 'ID', fill_alpha = 0.5) +
  tm_facets(by = c("behavior_age_baguage"), free.coords = FALSE) +
  tm_shape(RMO) +
  tm_borders(col = "black") ; tmap_view_behav_age_baguage

## Age chrono & Behavior ---------------------------

GPS_no_other$behavior_age <- paste(GPS_no_other$behavior, GPS_no_other$age, sep = "_")

tmap_mode("plot")
tmap_plot_behav_age <- tm_scalebar() +
  tm_shape(dept_BOX) +
  tm_polygons() +
  tm_shape(GPS) +
  tm_dots(col = 'ID', fill_alpha = 0.5) +
  tm_facets(by = c("behavior", "age"), free.coords = FALSE) +
  # tmap_options(max.categories = 70) +
  tm_shape(RMO) +
  tm_borders(col = "black") ; tmap_plot_behav_age

# Sauvegarde de la carte statique
tmap_save(tmap_plot_behav_age, paste0(data_image_path_serveur, "/GPS_behav_age.png"), dpi = 600)

tmap_mode("view")
tmap_view_behav_age <- tm_scalebar() +
  tm_shape(GPS) +
  tm_dots(col = 'ID', fill_alpha = 0.5) +
  tm_facets(by = c("behavior", "age"), free.coords = FALSE) +
  # tmap_options(max.categories = 70) +
  tm_shape(RMO) +
  tm_borders(col = "black") ; tmap_view_behav_age

# Signal sonore à la fin du script
beep(3)
