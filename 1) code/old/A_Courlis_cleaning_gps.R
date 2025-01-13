# package --------------------------------------------------

library(dplyr)
library(data.table)
library(ggplot2)
library(stringi)
library(terra)
library(tmap)
library(sf)
library(spData)
library(lubridate)

###
####
# DATA to load ----------------------------------------------
####
###

## dept -----------------------------------------------------

dept <- st_read("1) data/departements.gpkg",
  layer = "contourdesdepartements"
)

dept17 <- dept[dept$code == 17, ]

## world -----------------------------------------------------

world <- st_read(system.file("shapes/world.gpkg", package = "spData"))

## Reserve ---------------------------------------------------

reserve <- st_read("1) data/rnn/rnn/N_ENP_RNN_S_000.shp")
RMO <- reserve[reserve$NOM_SITE=="Moëze-Oléron",]
rm(reserve)

## gps -------------------------------------------------------

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
  select(-comments)

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

## 56 days --------------------------

## durée suivi gps pour chaque ind
duree_suivi_par_ind_dt <- all_gps %>%
  select(indID, time) %>%
  group_by(indID) %>%
  mutate(duree = as.numeric(difftime(max(time), min(time), units = "days"))) %>%
select(indID, duree) %>%
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

## dept17 ---------------------------------------

# restriction des points gps à ceux dans le département charente maritime
# all_gps_dept17 <- st_intersection(all_gps_spa, dept17)

# save
# st_write(all_gps_dept17, "C:/Users/Suzanne.Bonamour/OneDrive - LPO/2) Data/4) Courlis/GPS/1) Generated_gps/all_gps_dept17.gpkg", append = FALSE)

# rm(all_gps_dept17)

###
####
# READ pré-clean 1 --------------------------------------
####
###

GPS <- st_read("C:/Users/Suzanne.Bonamour/OneDrive - LPO/2) Data/4) Courlis/GPS/1) Generated_gps/all_gps_dept17.gpkg")

###
####
# CENTROID moyen ----------------------------------------
####
###

# centroid des points pour chaque individu,
# soit sur l'ensemble des points gps,
# soit seulement pour les points gps dans le département charente maritime

## world
centro_world <- all_gps_spa %>%
  group_by(indID) %>%
  summarize(geometry = st_union(geometry)) %>%
  st_centroid()

## Charente maritime
centro_dept17 <- GPS %>%
  group_by(indID) %>%
  summarize(geometry = st_union(geom)) %>%
  st_centroid()

## maps ------------------------------------------------

# recentrage carte pays sur la zone avec des points centroids

country_centro_world <- st_intersection(world, centro_world)
list_country_world <- unique(country_centro_world$name_long)
country_world <- world[world$name_long %in% list_country_world, ]

# pour enlever la guyane

country_world <- st_cast(country_world, "POLYGON")
country_world <- country_world[c(2:length(country_world$iso_a2)), ]

# carte des centroids

## world

tmap_mode("plot")

centro_world_map <- tm_shape(country_world) +
  tm_polygons(col = "#F5F5F5") +
  tm_shape(centro_world) +
  tm_dots(
    size = 0.2,
    col = "green"
  )
centro_world_map

tmap_mode("view")

centro_world_map <- tm_shape(centro_world) +
  tm_dots(
    size = 0.2,
    col = "green"
  )
centro_world_map

## Charente maritime

tmap_mode("plot")

centro_dept17_map <- tm_shape(dept17) +
  tm_polygons(col = "#F5F5F5") +
  tm_shape(centro_dept17) +
  tm_dots(
    size = 0.2,
    col = "blue"
  )
centro_dept17_map

tmap_mode("view")

centro_dept17_map <- tm_shape(centro_dept17) +
  tm_dots(
    size = 0.2,
    col = "blue"
  )
centro_dept17_map

###
####
# CENTROID par heure ----------------------------------
####
###

GPS$hour <- round_date(GPS$time, "hour")

## Charente maritime
# centro_hour <- GPS %>%
#   group_by(indID, hour) %>%
#   summarize(geometry = st_union(geom)) %>%
#   st_centroid()
# 
# centro_hour_2 <- centro_hour %>% # pas de doublon
#   distinct()
# 
# centro_hour_3 <- centro_hour_2 %>% # pas de NA
#   na.omit()

# save
# st_write(centro_hour_3, "C:/Users/Suzanne.Bonamour/OneDrive - LPO/2) Data/4) Courlis/GPS/1) Generated_gps/centro_hour_3.gpkg", append = FALSE)

###
####
# READ pré-clean 2 --------------------------------------
####
###

GPS_Hpts <- st_read("C:/Users/Suzanne.Bonamour/OneDrive - LPO/2) Data/4) Courlis/GPS/1) Generated_gps/centro_hour_3.gpkg")

rm(centro_hour)
rm(centro_hour_2)

## 100 hours --------------------------------------------

# pour garder que les ind avec au moins 100 points sur 100 heures différentes

# indLess_100H <- GPS_Hpts %>%
#   group_by(indID) %>%
#   count() %>%
#   filter(n <= 100)
# 
# GPS_Hpts_2 <- GPS_Hpts %>%
#   filter(!indID %in% indLess_100H$indID)

# save
st_write(GPS_Hpts_2, "C:/Users/Suzanne.Bonamour/OneDrive - LPO/2) Data/4) Courlis/GPS/1) Generated_gps/GPS_Hpts_2.gpkg", append = FALSE)

###
####
# READ pré-clean 2 --------------------------------------
####
###

GPS_Hpts_2 <- st_read("C:/Users/Suzanne.Bonamour/OneDrive - LPO/2) Data/4) Courlis/GPS/1) Generated_gps/GPS_Hpts_2.gpkg")

rm(GPS_Hpts)

# MOVE lines --------------------------------------------

move <- GPS_Hpts_2 %>%
  # st_as_sf(coords = c("x", "y"), crs = 25832) %>%
  group_by(indID) %>%
  dplyr::summarize(do_union=FALSE) %>%  # do_union=FALSE doesn't work as well
  st_cast("LINESTRING") 

GPS_Hpts_2_HD <- GPS_Hpts_2 %>%
  group_by(indID) %>%
  count() %>% 
  filter(n >=2500) 

move_HD <- GPS_Hpts_2_HD %>%
  # st_as_sf(coords = c("x", "y"), crs = 25832) %>%
  group_by(indID) %>%
  dplyr::summarize(do_union=FALSE) %>%  # do_union=FALSE doesn't work as well
  st_cast("LINESTRING") 


# map
tmap_mode("view")

move_HD_map <- #tm_shape(dept17) +
  #tm_polygons(col = "#F5F5F5") +
  tm_shape(move_HD) +
  tm_lines(col = "indID") +
  tm_shape(RMO) +
  tm_polygons(col = "grey", alpha = 0.2) ; move_HD_map

move_map <- #tm_shape(dept17) +
  #tm_polygons(col = "#F5F5F5") +
  tm_shape(RMO) +
  tm_polygons(col = "lightblue", alpha = 0.2) +
  tm_shape(move) +
  tm_lines(col = "indID") ; move_map

