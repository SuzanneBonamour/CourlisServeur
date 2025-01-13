# package --------------------------------------------------


###
####
# path ---------------------------------------------------
####
###

data_path <- "C:/Users/Suzanne.Bonamour/OneDrive - LPO/2) Data/4) Courlis/Data/1) data/"
data_generated_path <- "C:/Users/Suzanne.Bonamour/OneDrive - LPO/2) Data/4) Courlis/Data/2) data_generated/"

###
####
# DATA to load ----------------------------------------------
####
###

# dept ---
dept <- st_read(paste0(data_path, "departements.gpkg"),
  layer = "contourdesdepartements"
)
dept17 <- dept[dept$code == 17, ]

# world ---
world <- st_read(system.file("shapes/world.gpkg", package = "spData"))

# reserve ---
reserve <- st_read(paste0(data_path, "Réserve_naturelle/rnn/rnn/N_ENP_RNN_S_000.shp"))
RMO <- reserve[reserve$NOM_SITE=="Moëze-Oléron",]
rm(reserve)

# gps ---
# all csv at the same time
data_path <- "C:/Users/Suzanne.Bonamour/OneDrive - LPO/2) Data/4) Courlis/GPS/0) Original_gps/Data_brute_GPS/Extraction_Courlis-cendre_29_08_2024/MOVEBANK_limitrack/"
files_limitrak <- paste0(data_path, list.files(path = data_path, pattern = "*.csv"))
dt_limitrack <- lapply(files_limitrak, fread, sep = ",")
all_limitrack <- rbindlist(dt_limitrack)

data_path <- "C:/Users/Suzanne.Bonamour/OneDrive - LPO/2) Data/4) Courlis/GPS/0) Original_gps/Data_brute_GPS/Extraction_Courlis-cendre_29_08_2024/MOVEBANK_pp1083/"
files_pp1083 <- paste0(data_path, list.files(path = data_path, pattern = "*.csv"))
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

# rm(all_gps)

# DESCRIPTION tracks -----------------------------------------------------------

## tracking period -------------------------------------------------------------

tracking_period <- sort(as.numeric(as.character(unique(year(all_gps$time))))) ; tracking_period

first_track_day <- min(all_gps$time) ; first_track_day

last_track_day <- max(all_gps$time) ; last_track_day

## nb ind ----------------------------------------------------------------------

nb_ind <- length(unique(all_gps$indID)) ; nb_ind

## nb point / ind --------------------------------------------------------------

all_gps$indID <- as.factor(all_gps$indID)

descript_dt_1 <- all_gps %>% 
  group_by(indID) %>% 
  summarise(nb_event_per_ind = length(eventID),
            recorded_period_per_ind = difftime(max(time), min(time), units = "days"),
            speed_mean = mean(speed, na.rm= T),
            speed_min = min(speed, na.rm= T),
            speed_max = max(speed, na.rm= T),
            acc_x_min = min(acc_x, na.rm= T),
            acc_x_max = max(acc_x, na.rm= T),
            acc_y_min = min(acc_x, na.rm= T),
            acc_y_max = max(acc_x, na.rm= T),
            acc_z_min = min(acc_x, na.rm= T),
            acc_z_max = max(acc_x, na.rm= T),
            temp_mean = mean(temp, na.rm= T),
            temp_min = min(temp, na.rm= T),
            temp_max = max(temp, na.rm= T),
            diff_time_mean = mean(c(0, difftime(dt$time[2:nrow(dt)], dt$time[1:(nrow(dt)-1)], units="min"))))







dt <- all_gps
dt$diff_time <- c(0, difftime(dt$time[2:nrow(dt)], dt$time[1:(nrow(dt)-1)], units="min"))



# faire un table/graph du nombre de point par ind et pas period (genre mois, ou semaine)

# CLEAN ------------------------------------------------------------------------

## 56 days --------------------------

## durée suivi gps pour chaque ind
duree_suivi_par_ind_dt <- all_gps %>%
  dplyr::select(indID, time) %>%
  group_by(indID) %>%
  mutate(duree = as.numeric(difftime(max(time), min(time), units = "days"))) %>%
  dplyr::select(indID, duree) %>%
  distinct()

# hist
hist(duree_suivi_par_ind_dt$duree)

# plot
duree_plot <- ggplot(
  duree_suivi_par_ind_dt,
  aes(reorder(indID, duree), duree)
) +
  geom_point(size = 3, shape = 21, fill = "beige") +
  geom_hline(yintercept = 56, linetype = "dashed", color = "red") +
  # coord_flip() +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(
    title = "",
    x = "individu", y = "durée de suivi gps", fill = ""
  ) ; duree_plot

# pour garder que les individus suivi assez longtemps
# au moins 2 cycle linaire/grande marée = 2*28 jours = 56 jours
length(duree_suivi_par_ind_dt$indID[duree_suivi_par_ind_dt$duree<=56]) # on retire 10 individu

all_gps_2 <- all_gps %>%
  group_by(indID) %>%
  mutate(duree = as.numeric(difftime(max(time), min(time), units = "days"))) %>%
  filter(duree >= 56)

# length(unique(all_gps$indID))
# length(unique(all_gps_2$indID))

## spatialisation -------------------------------

all_gps_spa <- st_as_sf(all_gps_2, coords = c("lon", "lat"))
st_crs(all_gps_spa) <- 4326
crs(dept)
crs(all_gps_spa)

## hivernage ---------------------------------------------------------

all_gps_spa$month <- month(all_gps_spa$time)

all_gps_spa_hiv <- all_gps_spa %>% 
  filter(month %in% c(9,10,11,12,1,2,3)) # en dehors de la période de repro

# map
tmap_mode("view")
move_hiv <- tm_shape(dept17) +
  tm_polygons(col = "#F5F5F5") +
  tm_shape(RMO_buffer) +
  tm_polygons(col = "grey", alpha = 0.2) +
  tm_shape(RMO) +
  tm_polygons(col = "red", alpha = 0.2) +  
  tm_shape(all_gps_spa_hiv) +
  tm_dots() ; move_hiv

## buffer 50 km around reserve ---------------------------------------

# estimation du buffer 
RMO_buffer <- st_buffer(RMO, 50000)
st_write(RMO_buffer, paste0(data_generated_path, "RMO_buffer.gpkg"), append = FALSE)
RMO_buffer <- st_read(paste0(data_generated_path, "RMO_buffer.gpkg"))

# map
tmap_mode("view")
move_HD_map <- tm_shape(dept17) +
  tm_polygons(col = "#F5F5F5") +
  # tm_shape(move_HD) +
  # tm_lines(col = "indID") +
  tm_shape(RMO_buffer) +
  tm_polygons(col = "grey", alpha = 0.2) +
  tm_shape(RMO) +
  tm_polygons(col = "red", alpha = 0.2); move_HD_map

# restriction des points gps à ceux dans le buffer de 50 km autour de la réserve
crs(all_gps_spa)
crs(RMO_buffer)
RMO_buffer <- st_transform(RMO_buffer, 4326)
all_gps_RMO <- st_intersection(all_gps_spa, RMO_buffer)
# save
st_write(all_gps_RMO, "C:/Users/Suzanne.Bonamour/OneDrive - LPO/2) Data/4) Courlis/GPS/1) Generated_gps/all_gps_RMO.gpkg", append = FALSE)
# read
all_gps_RMO <- st_read("C:/Users/Suzanne.Bonamour/OneDrive - LPO/2) Data/4) Courlis/GPS/1) Generated_gps/all_gps_RMO.gpkg")

## centroid / h ----------------------------------

# autour de la réserve seulement ---

# arrondir le time par heure
all_gps_RMO$hour <- round_date(all_gps_RMO$time, "hour")

# estimation des centroid par ind et hour, pour diminuer homogénéiser le nombre point par ind/time
centro_hour_RMO <- all_gps_RMO %>%
  group_by(indID, hour) %>%
  summarize(geometry = st_union(geometry)) %>%
  st_centroid()

centro_hour_RMO <- centro_hour_RMO %>% # pas de doublon
  distinct()

centro_hour_RMO <- centro_hour_RMO %>% # pas de NA
  na.omit()

# hivernage seulement ---

# arrondir le time par heure
all_gps_spa_hiv$hour <- round_date(all_gps_spa_hiv$time, "hour")

# estimation des centroid par ind et hour, pour diminuer homogénéiser le nombre point par ind/time
# centro_hour_hiv <- all_gps_spa_hiv %>%
#   group_by(indID, hour) %>%
#   summarize(geometry = st_union(geometry)) %>%
#   st_centroid()
# 
# centro_hour_hiv <- centro_hour_hiv %>% # pas de doublon
#   distinct()
# 
# centro_hour_hiv <- centro_hour_hiv %>% # pas de NA
#   na.omit()

# save
# st_write(centro_hour_hiv, "C:/Users/Suzanne.Bonamour/OneDrive - LPO/2) Data/4) Courlis/GPS/1) Generated_gps/centro_hour_hiv.gpkg", append = FALSE)
# read
centro_hour_hiv <- st_read("C:/Users/Suzanne.Bonamour/OneDrive - LPO/2) Data/4) Courlis/GPS/1) Generated_gps/centro_hour_hiv.gpkg")

# map
tmap_mode("view")
hiv_pts_map <- tm_shape(dept17) +
  tm_polygons(col = "#F5F5F5") +
  tm_shape(RMO_buffer) +
  tm_polygons(col = "grey", alpha = 0.2) +
  tm_shape(RMO) +
  tm_polygons(col = "red", alpha = 0.2) +
  tm_shape(centro_hour_hiv) +
  tm_dots(); hiv_pts_map

## 100 hours --------------------------------------------

# pour garder que les ind avec au moins 100 points sur 100 heures différentes

# hist(indLess_100H$n, breaks = 30)
# 
# indLess_100H <- centro_hour_RMO %>%
#   group_by(indID) %>%
#   count() %>%
#   filter(n <= 100)
#   
# h100_centro_hour_RMO <- centro_hour_RMO %>%
#   filter(!indID %in% indLess_100H$indID)

# tous les ind ont plus de 100h now

###
####
# SAVE point --------------------------------------------
####
###

st_write(centro_hour_RMO, "C:/Users/Suzanne.Bonamour/OneDrive - LPO/2) Data/4) Courlis/GPS/1) Generated_gps/move_point.gpkg", append = FALSE)

###
####
# MOVE linestring --------------------------------------------
####
###

move_linestring <- centro_hour_RMO %>%
  # st_as_sf(coords = c("x", "y"), crs = 25832) %>%
  group_by(indID) %>%
  dplyr::summarize(do_union=FALSE) %>%  # do_union=FALSE doesn't work as well
  st_cast("LINESTRING") 

###
####
# SAVE linestring --------------------------------------------
####
###

st_write(move_linestring, "C:/Users/Suzanne.Bonamour/OneDrive - LPO/2) Data/4) Courlis/GPS/1) Generated_gps/move_linestring.gpkg", append = FALSE)

