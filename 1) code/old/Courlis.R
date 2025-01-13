# package --------------------------------------------------

library(dplyr)
library(data.table)
library(ggplot2)
library(stringi)
library(terra)
library(tmap)
library(sf)

###
####
# DATA -----------------------------------------------------
####
###

## dept -----------------------------------------------------

dept <- st_read("1) data/departements.gpkg",
                layer = "contourdesdepartements"
)

dept_zoom <- dept[dept$code==17,] 

## gps ------------------------------------------------------

# FRP_EC103792_182044 <- fread("1) data/Data_brute_GPS/Extraction_Courlis-cendre_29_08_2024/MOVEBANK_limitrack/FRP_EA542017-MOZ14.csv")

# all csv at the same time
data_path <- "1) data/Data_brute_GPS/Extraction_Courlis-cendre_29_08_2024/MOVEBANK_limitrack/"
files_limitrak <- paste0(data_path, list.files(path = data_path, pattern = "*.csv"))
dt_limitrack <- lapply(files_limitrak, fread, sep = ",")
all_limitrack <- rbindlist(dt_limitrack)

data_path <- "1) data/Data_brute_GPS/Extraction_Courlis-cendre_29_08_2024/MOVEBANK_pp1083/"
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
all_gps <- all_gps[!is.na(all_gps$lon),]
all_gps <- all_gps[!is.na(all_gps$lat),]

# spatialisation
all_gps_spa <- st_as_sf(all_gps, coords = c("lon", "lat"))
st_crs(all_gps_spa) <- 4326 
crs(dept)
crs(all_gps_spa)

# on garde que les points gps dans le département 17
all_gps_dept17 <- st_intersection(all_gps_spa, dept_zoom)

###
####
# EXPLORATION ----------------------------------------------
####
###

## durée suivi gps pour chaque ind --------------------------

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
  geom_hline(yintercept = 30, linetype = "dashed", color = "red") +
  # coord_flip() +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(
    title = "",
    x = "individu", y = "durée de suivi gps", fill = ""
  )
duree_plot

## centroid -------------------------------------------

### world 

ind_centroid_dt <- all_gps_spa %>% 
  group_by(indID) %>% 
  summarize(geometry = st_union(geometry)) %>%
  st_centroid() 

# crs(ind_centroid_dt)
# crs(dept)
# crs(dept_zoom)
# 
# dept_2154 <- st_transform(dept, 2154)
# dept_zoom_2154 <- st_transform(dept_zoom, 2154)
# ind_centroid_dt_2154 <- st_transform(ind_centroid_dt, 2154)





# # tmap_mode("view")
# tmap_mode("plot")
# 
# all_pts_map <- tm_shape(ind_centroid_dt_2154) +
#   tm_dots(
#     size = 3,
#     col = "red"
#     ) +
#   tm_legend(position = c(0, 0)) ; all_pts_map
# 
# crs(ind_centroid_dt)
# crs(dept)
# crs(dept_zoom)
# crs(dept_2)
# crs(dept_zoom_2)

tmap_mode("view")
# tmap_mode("plot")



# !!!!!!!!!!
all_pts_map <- 
  tm_shape(ind_centroid_dt) +
  tm_dots(
    # col = "ATLAS_text",
    # title.col = "Code atlas",
    size = 0.2,
    col = "red"
    # palette = c(
    #   "white",
    #   darken("#8D6B94", amount = 0.3),
    #   "#8D6B94",
    #   lighten("#8D6B94", amount = 0.3)
    # )
  ) ; all_pts_map






# tmap_mode("view")
# # tmap_mode("plot")
# 
# all_pts_map <- 
#   tm_shape(ind_centroid_dt_2154) +
#   tm_dots(
#     # col = "ATLAS_text",
#     # title.col = "Code atlas",
#     size = 3,
#     col = "blue"
#     # palette = c(
#     #   "white",
#     #   darken("#8D6B94", amount = 0.3),
#     #   "#8D6B94",
#     #   lighten("#8D6B94", amount = 0.3)
#     # )
#   ) ; all_pts_map

### Charente maritime 

centro_dept17 <- all_gps_dept17 %>% 
  group_by(indID) %>% 
  summarize(geometry = st_union(geometry)) %>%
  st_centroid() 

crs(centro_dept17)
crs(dept)
crs(dept_zoom)

# dept_2154 <- st_transform(dept, 2154)
# dept_zoom_2154 <- st_transform(dept_zoom, 2154)
# ind_centroid_dt_2154 <- st_transform(ind_centroid_dt, 2154)





# tmap_mode("view")
tmap_mode("plot")

all_pts_map <- tm_shape(centro_dept17) +
  tm_dots(
    size = 3,
    col = "red"
  ) +
  tm_legend(position = c(0, 0)) ; all_pts_map

crs(ind_centroid_dt)
crs(dept)
crs(dept_zoom)
crs(dept_2)
crs(dept_zoom_2)

tmap_mode("view")
# tmap_mode("plot")

all_pts_map <- tm_shape(dept) +
  tm_polygons(col="#F5F5F5", alpha = 0.1)+
  tm_shape(centro_dept17) +
  tm_dots(
    # col = "ATLAS_text",
    # title.col = "Code atlas",
    size = 3,
    col = "red"
    # palette = c(
    #   "white",
    #   darken("#8D6B94", amount = 0.3),
    #   "#8D6B94",
    #   lighten("#8D6B94", amount = 0.3)
    # )
  ) ; all_pts_map


tmap_mode("plot")
centro17_map <- tm_shape(dept_zoom) +
  tm_polygons(col="#F5F5F5", alpha = 0.1)+
  tm_shape(centro_dept17) +
  tm_dots(
    # col = "indID",
    # title.col = "indID",
    size = .2,
    col = "red"
    # palette = c(
    #   "white",
    #   darken("#8D6B94", amount = 0.3),
    #   "#8D6B94",
    #   lighten("#8D6B94", amount = 0.3)
    # )
  ) ; centro17_map


# faire centroid par ind par heure, regarder les habitat pour chaque centroid