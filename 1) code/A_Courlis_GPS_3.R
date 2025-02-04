rm(list=ls())

# package ----------------------------------------------------------------------

library(dplyr)
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

# BOX
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
  dplyr::select(indID, Date_before_timeLag, Date_after_timeLag, diff_before_after) %>% 
  distinct()

tt <- as.data.frame(table(date_timeLag_dt$indID))













date_timeLag_dt_2 <- date_timeLag_dt %>% 
  arrange(indID, DateTime) %>%
  group_by(indID) %>%
  mutate(overlap = foverlaps(Date_before_timeLag, Date_after_timeLag))

date_timeLag_dt_2 <- date_timeLag_dt %>% 
  arrange(indID, DateTime) %>%
  group_by(indID) %>%
  mutate(same_period = Date_before_timeLag == lag(Date_after_timeLag))

tt <- as.data.frame(table(date_timeLag_dt_2$same_period))


# %>% 
  # dplyr::select(indID, Date_timeLag) %>% 
  # st_drop_geometry() %>% 
  # distinct()

all_with_date_timeLag <- left_join(all_trip_stationary_sf_timeLag, date_timeLag_dt)

all_with_date_timeLag_2 <- all_with_date_timeLag %>% 
  arrange(indID, DateTime) %>%
  group_by(indID) %>%
  filter(DateTime < Date_timeLag)

tt <- as.data.frame(table(all_with_date_timeLag_2$indID))




















inter_sf <- st_read(paste0(data_generated_path_serveur, "inter_sf.gpkg"))

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

inter_sf <- st_read(paste0(data_generated_path_serveur, "inter_sf.gpkg"))

# foraging : 2h avant-après la marée base 
# roosting : 2h avant-après la marée haute 
  # + hauteur d'eau min > à mean(tides$height[tides$type=="High"]) = 5.5m

aa <- inter_sf %>%
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
                        as.character(foraging_up_i_n), "Low", height)
          
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
                  as.character(roosting_up_i_n), "High", height)
    
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
write.table(behaviour_dt_1, paste0(data_generated_path_serveur, "behaviour_24h_after_erreur.txt"),
            append = FALSE, sep = ";", dec = ".", col.names = TRUE)

beep()










###
####
# INSIDE the BOX ---------------------------------------------------------------
####
###

behaviour_24h <- read.table(paste0(data_generated_path_serveur, "behaviour_24h_after_erreur.txt"), 
                             header = T, sep = ";")
behaviour_24h_spa <- st_as_sf(behaviour_24h, coords = c("x", "y"), crs = 4326)
behaviour_24h_spa$lon <- behaviour_24h$x
behaviour_24h_spa$lat <- behaviour_24h$y

# inside the box
behaviour_24h_BOX <- st_intersection(behaviour_24h_spa, BOX_4326) # time consuming...
st_write(behaviour_24h_BOX, paste0(data_generated_path_serveur, "behaviour_24h_BOX.gpkg"), append = FALSE)

###
####
# 1000 POINTS & 56 DAYS ---------------------------------------------------------
###

behaviour_24h_BOX <- st_read(paste0(data_generated_path_serveur, "behaviour_24h_BOX.gpkg"))

behaviour_24h_BOX_1000_56 <- behaviour_24h_BOX %>%
  group_by(id) %>%
  mutate(nb_point = n()) %>%
  mutate(nb_days = difftime(max(date), min(date), units = "days")) %>%
  filter(nb_point >= 1000) %>%
  filter(nb_days >= 28*2)

behaviour_24h_nb_ind_1000_56 <- length(unique(behaviour_24h_BOX_1000_56$id)) ; behaviour_24h_nb_ind_1000_56

st_write(behaviour_24h_BOX_1000_56, paste0(data_generated_path_serveur, "behaviour_24h_BOX_1000_56.gpkg"), append = FALSE)

# add sex

behaviour_24h_box_1000_56 <- st_read(paste0(data_generated_path_serveur, "behaviour_24h_box_1000_56.gpkg"))

sex_dt <- sex_4 %>% 
  rename(id = indID)

behaviour_24h_box_1000_56_sex <- left_join(behaviour_24h_box_1000_56, sex_dt)

# add age 

age_dt <- age_5 %>% 
  rename(id = indID)

behaviour_24h_box_1000_56_sex_age <- left_join(behaviour_24h_box_1000_56_sex, age_dt)

st_write(behaviour_24h_box_1000_56_sex_age, paste0(data_generated_path_serveur, "behaviour_24h_box_1000_56_sex_age.gpkg"), append = FALSE)

###
####
# JOUR & NUIT ------------------------------------------------------------------
####
###

# behaviour_24h_box_1000_56_sex_age <- st_read(paste0(data_generated_path_serveur, "behaviour_24h_box_1000_56_sex_age.gpkg"))
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

behaviour_24h_BOX_1000_56 <- st_read(paste0(data_generated_path_serveur, "behaviour_24h_BOX_1000_56.gpkg"))

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

tmap_save(behaviour_24h_BOX_maps_2, paste0(data_image_path_serveur, "/behaviour_24h_BOX_2.png"), dpi = 600)

tmap_mode("view")
behaviour_24h_BOX_maps_1 <- tm_scale_bar() +
  tm_shape(dt_map_group_behaviour_24h) +
  tm_dots(col = 'id', alpha = 0.5) +
  tm_facets(by = c("group", "behavior"), free.coords = FALSE) +
  tmap_options(max.categories = 70) +
  tm_shape(RMO) +
  tm_borders(col = "black"); behaviour_24h_BOX_maps_1

beep()

