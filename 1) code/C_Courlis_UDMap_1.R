# package ----------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(lubridate)
library(sf)
library(ggplot2)
library(classInt)
library(extrafont)
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
library(adehabitatHR)

###
####
# path -------------------------------------------------------------------------
####
###

data_path <- "C:/Users/Suzanne.Bonamour/OneDrive - LPO/2) Data/4) Courlis/Data/1) data/"
data_generated_path <- "C:/Users/Suzanne.Bonamour/OneDrive - LPO/2) Data/4) Courlis/Data/2) data_generated/"
data_image_path <- "C:/Users/Suzanne.Bonamour/OneDrive - LPO/2) Data/4) Courlis/Data/3) images/"

###
####
# DATA to load -----------------------------------------------------------------
####
###

# all_box <- st_read(paste0(data_generated_path, "all_box.gpkg"))
points <- st_read(paste0(data_generated_path, "all_box_1000_56.gpkg"))


###
####
# BOX area ---------------------------------------------------------------------
####
###

BOX <- st_read(paste0(data_generated_path, "BOX_mini.gpkg"))

# map
tmap_mode("view")
map_box <- tm_shape(BOX) +
  tm_polygons(col = "blue", 
              alpha = 0.2); map_box

box_4326 <- st_transform(BOX, crs = 4326)
box_3035 <- st_transform(BOX, crs = 3035)
box_2154 <- st_transform(BOX, crs = 2154)

# MAP background ---------------------------------------------------------------

# dept ---
dept <- st_read(paste0(data_path, "departements.gpkg"),
                layer = "contourdesdepartements"
)
dept17 <- dept[dept$code == 17,]
dept_box <- st_read(paste0(data_generated_path, "dept_box_mini.gpkg"))

# reserve ---
reserve <- st_read(paste0(data_path, "Réserve_naturelle/rnn/rnn/N_ENP_RNN_S_000.shp"))
RMO <- reserve[reserve$NOM_SITE=="Moëze-Oléron",]
rm(reserve)

# INPN grille ---
grid <- st_read(paste0(data_path, "INPN_grid/METROP_L932X2.shp"))
grid_crop <- st_crop(grid, box_2154)

# 100*100m grid ---
offset_point <- st_bbox(grid[grid$CD_SIG=="2kmL93E370N6526",])[c("xmin", "ymin")] - c(2000 * 6, 1) ; offset_point
grid_100x100 <- st_make_grid(box_2154, cellsize = 100, offset = offset_point)
# save grid de 100m x 100m, alignée dans les grilles INPN
st_write(grid_100x100, paste0(data_generated_path, "grid_100x100.gpkg"), append = FALSE)
grid_100x100 <- st_read(paste0(data_generated_path, "grid_100x100.gpkg"))

tmap_mode("view")
grid_map <- tm_scale_bar() +
  tm_shape(grid_100x100) +
  tm_polygons(col = "red", alpha = 0.3) +
  tm_shape(grid_crop) +
  tm_polygons(alpha = 0.3, col = "green") ; grid_map

###
####
# UDmaps -----------------------------------------------------------------
####
###

## UDMaps sur grille 100*100 INPN ------

library(rlist)

raster_100x100 <- raster(grid_100x100, resolution=100, crs="EPSG:2154")

table(month(points$date))

list_UDMap <- list()

month <- unique(sort(as.numeric(month(points$date))))

m = 1
tmap_mode("plot")

for(m in month){
  print(m)

  points_m <- points %>%
    filter(month(date)== m) 
  points_m_2154 <- st_transform(points_m, crs = 2154)
  points_m_2154 <- as(points_m_2154, "Spatial")
  UDMap_m <- kernelUD(points_m_2154, grid = as(raster_100x100, "SpatialPixels"))
  plot_UDMap_m <- tm_scale_bar() +
    tm_shape(UDMap_m) +
    tm_raster() +
    tm_shape(dept_box) +
    tm_polygons(alpha = 0) +
    tm_shape(RMO) +
    tm_polygons(border.col = "green", col = "white", alpha = 0)
  
  list_UDMap[[paste0("month", m)]] <- plot_UDMap_m

}

all_month_UDMap <- tmap_arrange(list_UDMap) ; all_month_UDMap

## sur mini box ------

BOX_mini <- st_as_sf(st_as_sfc(st_bbox(c(xmin = -1.26, xmax = -1.05, ymax = 45.93, ymin = 45.83), crs = st_crs(4326))))
BOX_mini_2154 <- st_transform(BOX_mini, crs = 2154)

# 100*100m grid sur box_mini
offset_point_mini <- st_bbox(grid[grid$CD_SIG=="2kmL93E380N6534",])[c("xmin", "ymin")] - c(2000 * 5, 1) ; offset_point_mini
grid_100x100_mini <- st_make_grid(BOX_mini_2154, cellsize = 100, offset = offset_point_mini)
st_write(grid_100x100_mini, paste0(data_generated_path, "grid_100x100_mini.gpkg"), append = FALSE)
grid_100x100_mini <- st_read(paste0(data_generated_path, "grid_100x100_mini.gpkg"))

tmap_mode("view")
grid_map <- tm_scale_bar() +
  tm_shape(grid_100x100_mini) +
  tm_polygons(col = "red", alpha = 0.3) +
  tm_shape(BOX_mini_2154) +
  tm_polygons(col = "blue", alpha = 0, border.col = "blue") +
  tm_shape(grid_crop) +
  tm_polygons(alpha = 0.3, col = "green") ; grid_map






raster_100x100_mini <- raster(grid_100x100_mini, resolution=100, crs="EPSG:2154")

list_UDMap_mini <- list()

month <- unique(sort(as.numeric(month(points$date))))

m = 1
tmap_mode("plot")

for(m in month){
  print(m)
  
  points_m <- points %>%
    filter(month(date)== m) 
  points_m_2154 <- st_transform(points_m, crs = 2154)
  points_m_2154 <- as(points_m_2154, "Spatial")
  UDMap_m <- kernelUD(points_m_2154, grid = as(raster_100x100_mini, "SpatialPixels"))
  plot_UDMap_m <- tm_scale_bar() +
    tm_shape(UDMap_m) +
    tm_raster() +
    tm_shape(dept_box) +
    tm_polygons(alpha = 0) +
    tm_shape(RMO) +
    tm_polygons(border.col = "green", col = "white", alpha = 0)
  
  list_UDMap_mini[[paste0("month", m)]] <- plot_UDMap_m
  
}

all_month_UDMap_mini <- tmap_arrange(list_UDMap_mini) ; all_month_UDMap_mini







































# map
tmap_mode("view")
map_dept <- tm_shape(dept_box) +
  tm_polygons(col = "white", 
              alpha = 0.2) +
  tm_shape(BOX_mini) +
  tm_polygons(col = "red", 
              alpha = 0.2) ; map_dept

# proj BOX
box_2154 <- st_transform(BOX, crs = 2154)
box_4326 <- st_transform(BOX, crs = 4326)
box_3035 <- st_transform(BOX, crs = 3035)










# decembre
points_jan <- points %>%
  filter(month(date)=="1") 
points_jan_2154 <- st_transform(points_jan, crs = 2154)
points_jan_2154 <- as(points_jan_2154, "Spatial")
UDMap_jan <- kernelUD(points_jan_2154, grid = as(raster_100x100, "SpatialPixels"))
image_UDMap_jan <- image((UDMap_jan))
tmap_mode("plot")
plot_UDMap_jan <- tm_scale_bar() +
  tm_shape(UDMap_jan) +
  tm_raster() +
  tm_shape(dept_box) +
  tm_polygons(alpha = 0) +
  tm_shape(RMO) +
  tm_polygons(border.col = "green", col = "white", alpha = 0) ; plot_UDMap_jan

# Mai
points_mai <- points %>%
  filter(month(date)=="5") 
points_mai_2154 <- st_transform(points_mai, crs = 2154)
points_mai_2154 <- as(points_mai_2154, "Spatial")
UDMap_mai <- kernelUD(points_mai_2154, grid = as(raster_100x100, "SpatialPixels"))
image_UDMap_mai <- image((UDMap_mai))
tmap_mode("plot")
plot_UDMap_mai <- tm_scale_bar() +
  tm_shape(UDMap_mai) +
  tm_raster() +
  tm_shape(dept_box) +
  tm_polygons(alpha = 0) +
  tm_shape(RMO) +
  tm_polygons(border.col = "green", col = "white", alpha = 0) ; plot_UDMap_mai















UD_dt_hour <- points %>% 
  mutate(hour = as.numeric(hour(date))) %>% 
  dplyr::select(x, y, id, date, hour)

UD_dt_morning <- UD_dt_hour %>% 
  dplyr::filter(hour %in% (5:8)) %>% 
  dplyr::select(x, y, id, date, hour)

UD_dt_night <- UD_dt_hour %>% 
  dplyr::filter(hour %in% (18:22)) %>% 
  dplyr::select(x, y, id, date, hour)

UD_dt_morning_sfc_1 = st_sfc(UD_dt_morning$geom, crs = 4326)
UD_dt_morning_spa_1 <- as(UD_dt_morning_sfc_1, "Spatial")
UD_dt_morning_1 <- kernelUD(UD_dt_morning_spa_1, grid = 1000)

UD_dt_night_sfc_1 = st_sfc(UD_dt_night$geom, crs= 4326)
UD_dt_night_spa_1 <- as(UD_dt_night_sfc_1, "Spatial")
UD_dt_night_1 <- kernelUD(UD_dt_night_spa_1, grid = 500)

tmap_mode("view")
map_UD_1 <- tm_scale_bar() +
  tm_shape(dept_box) +
  tm_polygons(alpha = 0) +
  tm_shape(UD_dt_morning_1) +
  tm_raster() +
  tm_shape(BOX) +
  tm_polygons(col = "white", alpha = 0) +
  tm_shape(RMO) +
  tm_polygons(col = "green", 
              alpha = 0.2) ; map_UD_1

tmap_mode("view")
map_UD_night <- tm_scale_bar() + 
  tm_shape(dept_box) +
  tm_polygons(alpha = 0) +
  tm_shape(UD_dt_night_1) +
  tm_raster() +
  tm_shape(BOX) +
  tm_polygons(col = "white", alpha = 0) +
  tm_shape(RMO) +
  tm_polygons(col = "green", 
              alpha = 0.2) ; map_UD_night

all_point_facet <- tmap_arrange(map_UD_1, map_UD_night, 
                                nrow = 2)

tmap_save(all_point_facet, paste0(data_image_path, "/UDtest.png"), dpi = 600)





###
####
# UDmaps test param -----------------------------------------------------------------
####
###

UD_dt_test_grid300_h0002 <- kernelUD(UD_dt_morning_spa_1, h=0.01, grid = 300)
UD_dt_test_grid300_h08 <- kernelUD(UD_dt_morning_spa_1, h=0.8, grid = 300)
UD_dt_test_grid300_hdefault <- kernelUD(UD_dt_morning_spa_1, grid = 300)
UD_dt_test_grid300_hdefault <- kernelUD(UD_dt_morning_spa_1, grid = 5000)

image(UD_dt_test_grid300_h0002)
UD_dt_test_grid300_h02@h
UD_dt_test_grid300_h02@proj4string
area <- getverticeshr(UD_dt_test_grid300_h02, percent = 95) # pour avoir l'air du home range
area

# !!!!!!!!!!!!!!!!!!!!!!!!!

# test with id as a factor/character?
UD_dt_morning$id <- as.factor(UD_dt_morning$id)
# UD_dt_morning <- UD_dt_morning[!is.na(UD_dt_morning$id),]
# UD_dt_sfc_test_1 = st_as_sfc(UD_dt_morning, crs = 4326)
# UD_dt_test_spa_1 <- as(UD_dt_sfc_test_1, "Spatial")
# UD_test_1 <- kernelUD(UD_dt_test_spa_1, grid = 300)
# image(UD_test_1)


# kernelUD(UD_dt_test_spa_1[[UD_dt_test_spa_1, grid=as(Grid, "SpatialPixels"))

# Grid<-raster(xmn=-5.65, xmx=43.05, ymn=30.55, ymx=47.95, resolution=0.1,crs="EPSG:4326")
# Grid <- raster(xmn = -1.45, xmx = -0.95, ymn = 45.75, ymx = 46.07, resolution=200, crs="EPSG:4326")
Grid <- raster(xmn = -13732602.5, xmx = -789180.5, ymn = 10501897, ymx = 24789320, resolution=200, crs="EPSG:2154")

# grid_lamb <- projectRaster(Grid, crs="EPSG:2154", res = 100)
# bbox(grid_lamb)

UD_dt_morning <- st_transform(UD_dt_morning, crs="EPSG:2154")
# BOX <- st_as_sf(st_as_sfc(st_bbox(c(xmin = -1.45, xmax = -0.95, ymax = 45.75, ymin = 46.07), crs = st_crs(4326))))

# seulement les 5 premiers ind car sinon trop long/lourd 

UD_dt_morning <- UD_dt_hour %>% 
  dplyr::filter(hour %in% (5:8)) %>% 
  dplyr::select(x, y, id, date, hour)

dt_nb <- as.data.frame(table(UD_dt_morning$id))
id_3000 <- dt_nb %>% 
  filter(Freq > 3900)

UD_dt_morning <- UD_dt_morning[UD_dt_morning$id %in% id_3000$Var1,]

table(UD_dt_morning$id)

UD_dt_morning$id <- as.factor(UD_dt_morning$id)

Tracks.Colony <- list()
KUD.Colony <- list()
stk_KUD.Colony <- list()
sum_all_KUD.Colony <- list()

for (i in levels(UD_dt_morning$id)) {
  Tracks.Colony[[i]] <- as(UD_dt_morning[UD_dt_morning$id %in% i,], "Spatial")
  Tracks.Colony[[i]]@data <- droplevels(Tracks.Colony[[i]]@data)
  KUD.Colony[[i]] <- kernelUD(Tracks.Colony[[i]], grid=as(Grid, "SpatialPixels")) # [, c("id")]
  stk_KUD.Colony[[i]] <- stack(estUDm2spixdf(KUD.Colony[[i]]))
  sum_all_KUD.Colony[[i]] <- overlay(stk_KUD.Colony[[i]], fun = mean)
  # sum_all_KUD.Colony[[i]] <- sum_all_KUD.Colony[[i]]/sum(getValues(sum_all_KUD.Colony[[i]]))
}

image(sum_all_KUD.Colony[[1]])

################################################################################
### 13- Save UD maps
################################################################################

ls()[sapply(ls(), function(i) class(get(i))) == "RasterLayer"]

writeRaster(sum_all_KUD.Colony, filename = "test_UD_id.tif", format = "GTiff",overwrite=TRUE)



# ça marche ça ? (au dessus)





###### from gwen

for (i in levels(TracksYS.sf$colony_name)) {
  Tracks.Colony[[i]]<-as(TracksYS.sf[TracksYS.sf$colony_name %in% i,], "Spatial")
  Tracks.Colony[[i]]@data<-droplevels(Tracks.Colony[[i]]@data)
  KUD.Colony[[i]]<-kernelUD(Tracks.Colony[[i]][, c("track_id")], h=0.2, grid=as(Grid, "SpatialPixels"))
  stk_KUD.Colony[[i]] <-stack(estUDm2spixdf(KUD.Colony[[i]]))
  sum_all_KUD.Colony[[i]] <- overlay(stk_KUD.Colony[[i]], fun = mean)
  sum_all_KUD.Colony[[i]] <- sum_all_KUD.Colony[[i]]/sum(getValues(sum_all_KUD.Colony[[i]]))
}

rm(KUD.Colony,stk_KUD.Colony,Tracks.Colony)

sum(getValues(sum_all_KUD.Colony))

#WEIGTH UDs PER POP SIZE OF COLONIES

TracksYS.sf[!TracksYS.sf$colony_name %in% PopColony$colony_name,] #ok all find colony size

data.frame(TracksYS.sf) %>% group_by(colony_name) %>% summarize(NbTracks=n_distinct(track_id)) %>% data.frame()

KUD.Colony.weigh.season<-list()

for (i in levels(TracksYS.sf$colony_name)) {
  KUD.Colony.weigh.season[[i]]<-sum_all_KUD.Colony[[i]]*(PopColony[PopColony$colony_name %in% i,]$pop_size_best/sum(PopColony$pop_size_best))
}

KUD.Colony.weigh.season <- stack(KUD.Colony.weigh.season)
KUD.Colony.weigh.season <- overlay(KUD.Colony.weigh.season, fun = mean)
KUD.Colony.weigh.season <- KUD.Colony.weigh.season/sum(getValues(KUD.Colony.weigh.season))

sum(getValues(KUD.Colony.weigh.season),na.rm=TRUE)






















tmap_mode("view")
map_UD_1 <- tm_scale_bar() +
  tm_shape(dept_box) +
  tm_polygons(alpha = 0) +
  tm_shape(UD_dt_morning_1) +
  tm_raster() +
  tm_shape(BOX) +
  tm_polygons(col = "white", alpha = 0) +
  tm_shape(RMO) +
  tm_polygons(col = "green", 
              alpha = 0.2) ; map_UD_1

tmap_mode("view")
map_UD_night <- tm_scale_bar() + 
  tm_shape(dept_box) +
  tm_polygons(alpha = 0) +
  tm_shape(UD_dt_night_1) +
  tm_raster() +
  tm_shape(BOX) +
  tm_polygons(col = "white", alpha = 0) +
  tm_shape(RMO) +
  tm_polygons(col = "green", 
              alpha = 0.2) ; map_UD_night




all_point_facet <- tmap_arrange(map_UD_1, map_UD_night, 
                                nrow = 2)

tmap_save(all_point_facet, paste0(data_image_path, "/UDtest.png"), dpi = 600)







# UD_dt_morning_1 <- kernelUD(UD_dt_morning_spa_1, c("indiD"), grid = 500)







estUDm2spixdf(UD_dt_night_1)




# UD_1 <- kernelUD(points, c("id"))



# test matin/soir

UD_dt_1 <- points %>% 
  # data.frame() %>%
  dplyr::select(x, y, id)
UD_dt_sfc_1 = st_sfc(UD_dt_1$geom, crs = 4326)
UD_dt_spa_1 <- as(UD_dt_sfc_1, "Spatial")

UD_dt_1$sex <- rep(c("f","m","f"), length(UD_dt_1$x)/3)

UD_1 <- kernelUD(UD_dt_spa_1, grid = 500)

# image(UD_1)

# crop_UD_1 <- crop(UD_1, BOX) 
# crop_UD_2 <- raster(crop_UD_1)
# mask_UD_1 <- mask(crop_UD_2, BOX)

tmap_mode("view")
map_UD_1 <- tm_scale_bar() + 
  tm_shape(UD_1) +
  tm_raster() +
  tm_shape(dept_box) +
  tm_polygons(alpha = 0) +
  tm_shape(BOX) +
  tm_polygons(col = "white", alpha = 0) +
  tm_shape(RMO) +
  tm_polygons(col = "green", 
              alpha = 0.2) ; map_UD_1








# UD_1 <- kernelUD(points, c("id"))


##############################################################################################
###CREATE UTILISATION DISTRIBUTION (UD) PER COLONY FOR ALL SEASON

Tracks.Colony<-list()
KUD.Colony<-list()
stk_KUD.Colony<-list()
sum_all_KUD.Colony<-list()

for (i in levels(TracksYS.sf$colony_name)) {
  Tracks.Colony[[i]]<-as(TracksYS.sf[TracksYS.sf$colony_name %in% i,], "Spatial")
  Tracks.Colony[[i]]@data<-droplevels(Tracks.Colony[[i]]@data)
  KUD.Colony[[i]]<-kernelUD(Tracks.Colony[[i]][, c("track_id")], h=0.2, grid=as(Grid, "SpatialPixels"))
  stk_KUD.Colony[[i]] <-stack(estUDm2spixdf(KUD.Colony[[i]]))
  sum_all_KUD.Colony[[i]] <- overlay(stk_KUD.Colony[[i]], fun = mean)
  sum_all_KUD.Colony[[i]] <- sum_all_KUD.Colony[[i]]/sum(getValues(sum_all_KUD.Colony[[i]]))
}

rm(KUD.Colony,stk_KUD.Colony,Tracks.Colony)

sum(getValues(sum_all_KUD.Colony))

#WEIGTH UDs PER POP SIZE OF COLONIES

TracksYS.sf[!TracksYS.sf$colony_name %in% PopColony$colony_name,] #ok all find colony size

data.frame(TracksYS.sf) %>% group_by(colony_name) %>% summarize(NbTracks=n_distinct(track_id)) %>% data.frame()

KUD.Colony.weigh.season<-list()

for (i in levels(TracksYS.sf$colony_name)) {
  KUD.Colony.weigh.season[[i]]<-sum_all_KUD.Colony[[i]]*(PopColony[PopColony$colony_name %in% i,]$pop_size_best/sum(PopColony$pop_size_best))
}

KUD.Colony.weigh.season <- stack(KUD.Colony.weigh.season)
KUD.Colony.weigh.season <- overlay(KUD.Colony.weigh.season, fun = mean)
KUD.Colony.weigh.season <- KUD.Colony.weigh.season/sum(getValues(KUD.Colony.weigh.season))

sum(getValues(KUD.Colony.weigh.season),na.rm=TRUE)






# ################################################################################
# ### 1- Load data
# ################################################################################
# 
# XYcolonies<-read.table("C:/Users/gwenael.quaintenne/OneDrive - LPO/LIFE_PanPuffinus/DataLifePanPuffinusA3/BirdsData/ColonySizeData/XYcolonySize_YS.csv", header=TRUE, sep=";", encoding="UTF-8",stringsAsFactors = TRUE)
# 
# COUNTRY<-XYcolonies %>% group_by(Country_code,Country_name) %>% summarise(MinPop=sum(pop_size_min,na.rm = TRUE),MaxPop=sum(pop_size_max,na.rm = TRUE))
# COUNTRY$YlatDD<-c(41,37.5,42.35,39,43.5,39.5,35.7,36.3)
# COUNTRY$XlongDD<-c(16,6,5.5,25,16.5,10.6,15.4,10.2)
# COUNTRY$Country_nameFR<-as.factor(c("Albanie","Algérie","France","Grèce","Croatie","Italie","Malte","Tunisie"))
# COUNTRY$Label<-paste0(COUNTRY$Country_name," ",formatC(COUNTRY$MinPop, format="d", big.mark=" "),"-",formatC(COUNTRY$MaxPop, format="d", big.mark=" "), " p.")
# COUNTRY$LabelFR<-paste0(COUNTRY$Country_nameFR," ",formatC(COUNTRY$MinPop, format="d", big.mark=" "),"-",formatC(COUNTRY$MaxPop, format="d", big.mark=" "), " c.")
# 
# #XYcolonies<-st_as_sf(XYcolonies, coords = c("XlongDD", "YlatDD"), crs="EPSG:4326")
# 
# land<-st_read("C:/Users/gwenael.quaintenne/OneDrive - LPO/LIFE_PanPuffinus/SIG_A3LifePanPuffinus/FondCarto/Land/CNTR_RG_01M_2020_4326.shp",crs="EPSG:4326",stringsAsFactors =TRUE)
# 
# #Extend<-st_bbox(st_as_sf(XYcolonies, coords = c("XlongDD", "YlatDD"), crs="EPSG:4326"))
# #Extend[c(3,4)]<-Extend[c(3,4)]+2
# #Extend[c(1,2)]<-Extend[c(1,2)]-2
# #land<-st_crop(land, Extend)
# 
# TracksYS<-readRDS("InputR/YStracks/AllTracksYS.rds")
# 
# unique(TracksYS[!(TracksYS$colony_name %in% XYcolonies$colony_name & TracksYS$site_name %in% XYcolonies$site_name),]$site_name)
# #only Malta have tracks with no colony affiliation
# 
# NbTracks<-TracksYS %>% group_by(colony_name,site_name) %>% summarise(NbTracks=n_distinct(track_id))
# 
# XYcolonies<-left_join(XYcolonies,NbTracks,by=c("site_name","colony_name"))
# sum(XYcolonies$NbTracks,na.rm=TRUE)
# sum(NbTracks[!NbTracks$colony_name %in% c("Malta"),]$NbTracks) #ok the same

# ################################################################################
# ### 2- Map of breeding colonies at the scale of Mediterranean Sea
# ################################################################################
# 
# IntCol<-classIntervals(XYcolonies$pop_size_best, n=6, style = "jenks")
# 
# ggplot()+
#   geom_sf(data=land, fill=rgb(240, 240, 240, max=255),color=rgb(79, 79, 79, max=255))+
#   geom_point(aes(x = XlongDD, y = YlatDD), data = XYcolonies, fill=rgb(128, 128, 128,max=255), shape=3, size=0.8, col="black")+
#   geom_point(aes(x = XlongDD, y = YlatDD, size = pop_size_best), data = XYcolonies, fill=rgb(128, 128, 128,max=255), shape=19, col="#0d72a6", alpha=1)+
#   scale_size(breaks=IntCol$brks[c(2:7)],guide="legend", label=IntCol$brks[c(2:7)], range=c(2,15), name="Nb. pairs")+
#   geom_point(aes(x = XlongDD, y = YlatDD, size = pop_size_best), data = XYcolonies, col=rgb(128, 128, 128,max=255), shape=1, alpha=1)+
#   geom_point(aes(x = XlongDD, y = YlatDD, size = pop_size_best), data = XYcolonies[is.na(XYcolonies$NbTracks)==0,], col="#F55F19", shape=1, alpha=1,show.legend=FALSE)+
#   geom_label(aes(x = XlongDD, y = YlatDD, label=Label),data=COUNTRY,colour="black",hjust = 0,family="Source Sans Pro")+
#   geom_text(aes(x = Inf, y = Inf, label=" ©LIFE PanPuffinus! (2023)"),colour="#0d72a6",hjust = 0,vjust = 1, angle = -90,size=3,family="Source Sans Pro")+
#   geom_text(aes(x = XlongDD+0.8, y = YlatDD, label=NbTracks),data=XYcolonies,colour="#F55F19",hjust = 0,family="Source Sans Pro")+
#   xlim(4,28)+
#   ylim(34,44)+
#   coord_sf()+
#   ggtitle("Yelkouan Shearwater - Location of the known breeding colonies and population size (Updated: January 2023) & tracked sample")+
#   theme(text= element_text(family="Source Sans Pro"),plot.title = element_text(family="Source Sans Pro", size=12),legend.position="bottom",legend.title = element_text(family="Source Sans Pro Semibold", size=10),legend.margin=margin(t=-15),legend.background = element_blank(),legend.key = element_blank())+
#   guides(size = guide_legend(nrow = 1))+
#   labs(tag = "Tracked sample") +
#   theme(plot.tag.position = c(0.53, 0.045),plot.tag=element_text(family="Source Sans Pro Semibold", size=10,color="#F55F19"))
# 
# ggsave("OutputR/ColonySize/YSmapcoloniessize.wmf", dpi=320, width=1050, height=609)
# 
# ###For Thierry
# ggplot()+
#   geom_sf(data=land, fill=rgb(240, 240, 240, max=255),color=rgb(79, 79, 79, max=255))+
#   geom_point(aes(x = XlongDD, y = YlatDD), data = XYcolonies, fill=rgb(128, 128, 128,max=255), shape=3, size=0.8, col="black")+
#   geom_point(aes(x = XlongDD, y = YlatDD, size = pop_size_best), data = XYcolonies, fill=rgb(128, 128, 128,max=255), shape=19, col="#0d72a6", alpha=1)+
#   scale_size(breaks=IntCol$brks[c(2:7)],guide="legend", label=IntCol$brks[c(2:7)], range=c(2,15), name="Nb. couples")+
#   geom_point(aes(x = XlongDD, y = YlatDD, size = pop_size_best), data = XYcolonies, col=rgb(128, 128, 128,max=255), shape=1, alpha=1)+
#   geom_label(aes(x = XlongDD, y = YlatDD, label=LabelFR),data=COUNTRY,colour="#0d72a6",hjust = 0)+
#   geom_text(aes(x = Inf, y = Inf, label="©LPO Birdlife France (2023)"),colour="#0d72a6",hjust = 0,vjust = 1, angle = -90,size=3)+
#   xlim(4,28)+
#   ylim(34,44)+
#   coord_sf()+
#   ggtitle("Puffin Yelkouan - Localisation des colonies connues et taille des pop. (Mise à jour: Décembre 2022)")+
#   theme(plot.title = element_text(family="Source Code Pro Semibold", size=10),legend.position="bottom",legend.title = element_text(family="Source Code Pro Semibold", size=10),legend.margin=margin(t=-15),legend.background = element_blank(),legend.key = element_blank())+
#   guides(size = guide_legend(nrow = 1))
# 
# ggsave("OutputR/ColonySize/YSmapcoloniessizeFR.emf", dpi=320, width=1050, height=609)
# 
# 
# #with ggOceanMaps
# 
# ggOceanMaps::basemap(limits = c(2,30, 32, 45), bathymetry = TRUE,projection.grid = TRUE,legends = FALSE,grid.col="grey70",grid.size=0.05,land.col = "#f0f0f0",land.border.col ="#4f4f4f",land.size = 0.1)+
#   geom_sf(data=land, fill="#f0f0f0",color="#4f4f4f")+
#   annotation_scale(location = "br",text_family="Source Sans Pro",bar_cols = c("#4f4f4f", "white"),text_col = "#4f4f4f",line_col = "#4f4f4f") +
#   annotation_north_arrow(location = "tr", which_north = "true",style=north_arrow_orienteering(
#     line_width = 0.1,line_col = "#4f4f4f",fill = c("white", "#4f4f4f"),text_col = "#4f4f4f",text_family = "Source Sans Pro"))+
#   geom_point(aes(x = XlongDD, y = YlatDD), data = XYcolonies, fill=rgb(128, 128, 128,max=255), shape=3, size=0.8, col="#4f4f4f")+
#   geom_point(aes(x = XlongDD, y = YlatDD, size = pop_size_best), data = XYcolonies, fill=rgb(128, 128, 128,max=255), shape=19, col="#4f4f4f", alpha=1)+
#   scale_size(breaks=IntCol$brks[c(2:7)],guide="legend", label=IntCol$brks[c(2:7)], range=c(2,15), name="Nb. pairs")+
#   geom_point(aes(x = XlongDD, y = YlatDD, size = pop_size_best), data = XYcolonies, col=rgb(128, 128, 128,max=255), shape=1, alpha=1)+
#   geom_label(aes(x = XlongDD, y = YlatDD, label=Label),data=COUNTRY,colour="black",hjust = 0,family="Source Sans Pro")+
#   geom_text(aes(x = Inf, y = Inf, label="©LIFE PanPuffinus! (2023)"),colour="black",hjust = 0,vjust = 1, angle = -90,size=3,family="Source Sans Pro")+
#   xlim(4,28)+
#   ylim(34,44)+
#   coord_sf()+
#   ggtitle("Yelkouan Shearwater - Location of the known breeding colonies and population size (Updated: January 2023)")+
#   theme_map(base_family = "Source Sans Pro")+
#   theme(text=element_text(family="Source Sans Pro"),plot.title = element_text(family="Source Sans Pro Semibold", size=12),legend.position="bottom",legend.title = element_text(family="Source Sans Pro Semibold", size=10),legend.margin=margin(t=-15),legend.background = element_blank(),legend.key = element_blank())+
#   guides(size = guide_legend(nrow = 1))
# 
# #with bathy
# c('#ffff80ff','#8ff041ff','#3dd13bff','#2fa190ff','#215896ff','#0d1178ff')
# 
# ggOceanMaps::basemap(limits = c(2,30, 32, 45), bathymetry = TRUE, projection.grid = TRUE,legends = FALSE,grid.col="grey70",grid.size=0.05,land.col = "#f0f0f0",land.border.col ="#4f4f4f",land.size = 0.1)+
#   annotation_scale(location = "br",text_family="Source Sans Pro",bar_cols = c("#4f4f4f", "white"),text_col = "#4f4f4f",line_col = "#4f4f4f") +
#   annotation_north_arrow(location = "tr", which_north = "true",style=north_arrow_orienteering(
#     line_width = 0.1,line_col = "#4f4f4f",fill = c("white", "#4f4f4f"),text_col = "#4f4f4f",text_family = "Source Sans Pro"))+
#   geom_point(aes(x = XlongDD, y = YlatDD), data = XYcolonies, fill=rgb(128, 128, 128,max=255), shape=3, size=0.8, col="black")+
#   geom_point(aes(x = XlongDD, y = YlatDD, size = pop_size_best), data = XYcolonies, fill=rgb(128, 128, 128,max=255), shape=19, col="black", alpha=1)+
#   scale_size(breaks=IntCol$brks[c(2:7)],guide="legend", label=IntCol$brks[c(2:7)], range=c(2,15), name="Nb. pairs")+
#   geom_point(aes(x = XlongDD, y = YlatDD, size = pop_size_best), data = XYcolonies, col=rgb(128, 128, 128,max=255), shape=1, alpha=1)+
#   geom_point(aes(x = XlongDD, y = YlatDD, size = pop_size_best), data = XYcolonies[is.na(XYcolonies$NbTracks)==0,], col="cyan", shape=1, alpha=1,show.legend=FALSE,stroke = 1.5)+
#   geom_label(aes(x = XlongDD, y = YlatDD, label=Label),data=COUNTRY,colour="black",hjust = 0,family="Source Sans Pro")+
#   geom_text(aes(x = Inf, y = Inf, label="©LIFE PanPuffinus! (2023)"),colour="black",hjust = 0,vjust = 1, angle = -90,size=3,family="Source Sans Pro")+
#   geom_label(aes(x = XlongDD+0.6, y = YlatDD+0.4, label=NbTracks),data=XYcolonies,colour="cyan",hjust = 0,family="Source Sans Pro")+
#   xlim(4,28)+
#   ylim(34,44)+
#   coord_sf()+
#   ggtitle("Yelkouan Shearwater - Location of the known breeding colonies and population size (Updated: January 2023)")+
#   theme(text=element_text(family="Source Sans Pro"),plot.title = element_text(family="Source Sans Pro", size=12),legend.position="bottom",legend.title = element_text(family="Source Sans Pro Semibold", size=10),legend.margin=margin(t=-15),legend.background = element_blank(),legend.key = element_blank(),axis.text= element_text(family="Source Sans Pro Semibold", size=10),axis.title=element_blank())+
#   guides(size = guide_legend(nrow = 1))+
#   labs(tag = "Tracked sample") +
#   theme(plot.tag.position = c(0.82, 0.045),plot.tag=element_text(family="Source Sans Pro Semibold", size=10,color="cyan"))
# 
# rm(IntCol,COUNTRY)

# ################################################################################
# ### 3- Descriptive statistics on tracks deployment
# ################################################################################
# 
# ###Age classes deployment
# TracksYS %>% group_by(country_name,site_name,device,age) %>% summarize(MinYear=min(year(date)),MaxYear=max(year(date)),NbTracks=n_distinct(track_id)) %>% spread(key = age, value = NbTracks, fill = 0) %>% data.frame()
# 
# ###Month deployment
# TracksYS$month<-lubridate::month(TracksYS$date,label=TRUE,abbr=TRUE,locale="English_United States")
# unique(month(TracksYS$date))
# 
# TracksYS %>% group_by(country_name,site_name,device,month) %>% summarize(NbTracks=n_distinct(track_id)) %>% spread(key = month, value = NbTracks, fill = 0) %>% data.frame()
# 
# TracksYS %>% group_by(country_name,device,month) %>% summarize(NbTracks=n_distinct(track_id)) %>% spread(key = month, value = NbTracks, fill = 0) %>% data.frame()
# 
# ###Breed_stage and month
# unique(TracksYS$breed_stage)
# TracksYS$breed_stage<- ordered(sizes, levels = c("pre-egg", "incubation","brood-guard","post-guard","chick-rearing","breeding","fail (breeding season)","non-breeding","unknown"))
# 
# 
# TracksYS %>% filter(device=="GPS") %>% group_by(country_name,breed_stage,breed_status,month,device) %>% summarize(NbTracks=n_distinct(track_id)) %>% spread(key = month, value = NbTracks, fill = 0) %>% data.frame()
# 
# 
# TracksYS %>% filter(device=="GPS") %>% group_by(country_name,month,device) %>% summarize(NbTracks=n_distinct(track_id)) %>% spread(key = month, value = NbTracks, fill = 0) %>% data.frame()
# 
# ###Plot Country*Month
# 
# NbTracksYS.month.country<-TracksYS %>% filter(device=="GPS") %>% group_by(country_name,month,device) %>% summarize(NbTracks=n_distinct(track_id)) %>% data.frame()
# 
# source("C:/Users/gwenael.quaintenne/OneDrive - LPO/Enquete_AnatidesLimicolesNich/DataLIMAT2021-2022/1-CheckSamplingEffort/SourcesGraph.R",encoding ="UTF-8")
# 
# g<-c("#164D68","#777B7F","#EDEFE2","#88CCEE","#4E87B6","#B4BCCC")
# 
# ggplot(NbTracksYS.month.country, aes(x=month,y=NbTracks,fill=country_name)) + 
#   geom_bar(stat="identity")+
#   scale_fill_manual(values=g)+
#   ylab("Nb tracks") +
#   labs(fill="")+
#   theme_wsj(base_size=12,) +
#   ggtitle("GPS Month/Country's deployment")+
#   guides(fill = guide_legend(nrow = 1))
# #size output 713*445
# 
# ### Get time lag
# 
# TimeLag <- TracksYS %>% filter(device=="GPS") %>%
#   arrange(track_id, date) %>% 
#   group_by(track_id) %>%
#   mutate(time_lag = date-lag(date)) %>%
#   data.frame()
# 
# MeanTimeLag <- TimeLag %>% group_by(dataset_id,track_id) %>% Rmisc::summarySE(measurevar = "time_lag", groupvars = c("dataset_id","track_id"), na.rm=TRUE,conf.interval = 0.95) %>%
#   data.frame()
# 
# MeanTimeLag %>% Rmisc::summarySE(measurevar = "time_lag", na.rm=TRUE,conf.interval = 0.95) %>%
#   data.frame()
# 
# (3700.12-502.6121)/60 #53
# (3700.12+502.6121)/60 #70
# 3700.12/60 #62
# 
# 
# ggplot(MeanTimeLag, aes(x=time_lag/60)) + 
#   geom_histogram(fill="#4485ab")+
#   ylab("Nb tracks") +
#   theme_wsj(base_size=12) +
#   ggtitle("GPS sampling frequency (minutes)")+
#   geom_vline(aes(xintercept=3700.12/60),color="#777B7F", size=0.5)+
#   geom_vline(aes(xintercept=(3700.12-502.6121)/60),color="grey", linetype="dashed", size=0.5)+
#   geom_vline(aes(xintercept=(3700.12+502.6121)/60),color="grey", linetype="dashed", size=0.5)+
#   geom_text(aes(x=175,y=125,label="mean = 62 ±IC95% 8 min"),color="#777B7F",family="Source Sans Pro")
# 
# rm(list=ls()[! ls() %in% c("land","TracksYS","XYcolonies")])

# ################################################################################
# ### 4- Select data
# ################################################################################
# 
# #removed PTT tracks
# TracksYS<-droplevels(TracksYS[TracksYS$device=="GPS",]) # 229 402 obs.
# 
# wgs84 <- st_crs("EPSG:4326")
# 
# ##sf object
# TracksYS.sf <- st_as_sf(TracksYS, coords = c("longitude", "latitude"), crs=4326)
# 
# leaflet() %>% ## start leaflet plot
#   addProviderTiles(providers$Esri.WorldImagery, group = "World Imagery") %>% 
#   ## plot the points. Note: leaflet automatically finds lon / lat colonies
#   ## Colour accordingly.
#   addCircleMarkers(data = TracksYS.sf,
#                    radius = 3,
#                    fillColor = "cyan",
#                    fillOpacity = 0.5, stroke = F) 

################################################################################
### 5- McConnel Speedilter realistic velocity was set at 100 km/h
################################################################################

# ## create trip object (will removed duplicates)
# nrow(TracksYS[is.na(TracksYS$date),]) #one location without date???
# TracksYS<-TracksYS[!is.na(TracksYS$date),]
# 
# YStracks.trip <- TracksYS %>% 
#   group_by(track_id) %>% 
#   dplyr::select(x = longitude, 
#                 y = latitude, 
#                 DateTime = date, 
#                 everything()) %>% 
#   trip()
# 
# # McConnel Speedilter
# YStracks.trip$Filter <- speedfilter(YStracks.trip, max.speed = 100)  # speed in km/h
# summary(YStracks.trip$Filter) #ok 67 locations were removed
# 
# #It remains 229 273 locations
# 
# YStracks.trip <- data.frame(YStracks.trip) %>% mutate(longitude = x,latitude = y)
# 
# ## plot McConnel Removed values
# leaflet() %>%
#   addProviderTiles(providers$Esri.WorldImagery, group = "World Imagery") %>%
#   addCircleMarkers(data = subset(YStracks.trip, YStracks.trip$Filter == T),
#                    radius = 7,
#                    fillColor = "cyan",
#                    fillOpacity = 0.5, stroke = F) %>% 
#   addCircleMarkers(data = subset(YStracks.trip, YStracks.trip$Filter == F),
#                    #label = bird_track_gaps$nlocs, 
#                    radius = 5,
#                    fillColor = "purple",
#                    fillOpacity = 0.5, stroke = F) %>% 
#   addLegend(title="McConnel Speedilter", colors = c("cyan","purple"),
#             labels = c("keeped values","Removed values"))
# 
# 
# YStracks <- YStracks.trip %>% filter(Filter==TRUE) %>% data.frame() # 229 273 obs.

################################################################################
### 6- Linear interpolation locs every 30 min
################################################################################

# ## create ltraj object
# YStracks.ltraj <- as.ltraj(xy = bind_cols(x = YStracks$x, 
#                                           y = YStracks$y),
#                            date = YStracks$DateTime,
#                            id = YStracks$track_id)
# 
# ## re-sample tracks every 60 minutes (60*60 sec)
# YStracks.interp <- redisltraj(YStracks.ltraj, 60*60, type="time")
# YStracks.interp <- ld(YStracks.interp) %>% mutate(longitude = x,latitude = y)
# 
# #this results in 137 986 locations
# 
# leaflet() %>% 
#   addProviderTiles(providers$Esri.WorldImagery, group = "World Imagery") %>%
#   addCircleMarkers(data = YStracks,
#                    radius = 5,
#                    fillColor = "grey",
#                    fillOpacity = 0.5, stroke = F) %>%
#   addCircleMarkers(data = YStracks.interp,
#                    radius = 5,
#                    fillColor = "cyan",
#                    fillOpacity = 0.5, stroke = F) %>%
#   addLegend(title = "Rediscretization of trajectories",
#             colors = c("grey","cyan"),
#             labels = c("original locs","resampled locs"))
# #zoom on ok looks fine

################################################################################
### 7- Removed locations within 5km buffer of the colony
################################################################################

# YStracks.interp<-left_join(YStracks.interp,unique(YStracks[,c("track_id","site_name","colony_name","lon_colony","lat_colony","device")]),by = c("id" = "track_id"))
# 
# col<-unique(YStracks.interp[c("site_name","colony_name","lon_colony","lat_colony")])
# col$id_col<-factor(c(1:nrow(col)))
# YStracks.interp<-left_join(YStracks.interp,col)
# 
# YStracks.interp.sf<-st_as_sf(YStracks.interp, coords = c("longitude", "latitude"), crs=4326)
# YStracks.interp.sf.prj<-st_transform(YStracks.interp.sf, center=TRUE)
# 
# ## Creating a buffer around colonies
# YStracks.interp.buff <- data.frame()
# 
# st_over <- function(x, y) {
#   sapply(sf::st_intersects(x, y), function(z)
#     if (length(z) == 0) {
#       NA_integer_
#     } else {
#       z[1]
#     })
# }
# 
# for(i in levels(YStracks.interp.sf.prj$id_col)){
#   sub_col <- YStracks.interp.sf.prj[YStracks.interp.sf.prj$id_col %in% i,]
#   df_col <- data.frame(cbind(lon=sub_col$lon_colony[1], lat=sub_col$lat_colony[1]))
#   df_col <- st_as_sf(df_col, coords = c("lon", "lat"), crs=4326)
#   col_transf <- st_transform(df_col, st_crs(YStracks.interp.sf.prj))
#   buf_col <- st_buffer(col_transf, dist = 5000)
#   YStracks.interp.buff.col <- sub_col[is.na(st_over(sub_col,buf_col)),]
#   YStracks.interp.buff <- rbind(YStracks.interp.buff,data.frame(YStracks.interp.buff.col))
# }
# 
# YStracks.interp.buff$longitude<-YStracks.interp.buff$x
# YStracks.interp.buff$latitude<-YStracks.interp.buff$y
# YStracks.interp.buff.sf<-st_as_sf(YStracks.interp.buff, coords = c("longitude", "latitude"), crs=4326)
# 
# nrow(YStracks.interp)-nrow(YStracks.interp.buff) # 38 937 locations removed, 99 049 remained
# 
# rm(list=ls()[! ls() %in% c("st_over","land","YStracks","YStracks.interp.buff")])

################################################################################
### 8- Removed locations over mainland
################################################################################

# #load precise coastline
# coast<-st_read("C:/Users/gwenael.quaintenne/OneDrive - LPO/LIFE_PanPuffinus/SIG_A3LifePanPuffinus/FondCarto/Land/CoastCutMedit.shp",crs="EPSG:4326",stringsAsFactors =TRUE)
# coast<-st_make_valid(coast)
# 
# YStracks.interp.buff<-st_as_sf(YStracks.interp.buff, coords = c("longitude", "latitude"), crs=4326)
# 
# YStracks.interp.buff.land <- YStracks.interp.buff[is.na(st_over(YStracks.interp.buff,coast)),]
# 
# nrow(YStracks.interp.buff)-nrow(YStracks.interp.buff.land) #2677 locations over land removed (96 372 remained)
# 
# leaflet() %>% 
#   addProviderTiles(providers$Esri.WorldImagery, group = "World Imagery") %>%
#   addCircleMarkers(data = YStracks.interp.buff.land,
#                    radius = 5,
#                    fillColor = "cyan",
#                    fillOpacity = 0.5, stroke = F) %>%
#   addCircleMarkers(data = YStracks.interp.buff[!is.na(st_over(YStracks.interp.buff,coast)),],
#                    radius = 5,
#                    fillColor = "purple",
#                    fillOpacity = 0.5, stroke = F) %>%
#   addLegend(title = "Removed locations over land",
#             colors = c("cyan","purple"),
#             labels = c("keeped locs","removed locs"))
# 
# rm(list=ls()[! ls() %in% c("land","YStracks","YStracks.interp.buff.land")])

################################################################################
### 9- Format cleaned & resampled locations
################################################################################

#add attributes of birds

library(data.table)
BirdStat.day <- setDT(YStracks)[, .(day = seq.Date(min(date_gmt), max(date_gmt), by = 'day')), by = c("dataset_id","track_id","age","sex","breed_stage","breed_status")]
BirdStat.day<-data.frame(BirdStat.day)

BirdStat.day[BirdStat.day$track_id %in% c("PY_FT67737","PY_FT67757"),c("sex")]<-"male"

BirdStat.day<-unique(BirdStat.day[c("dataset_id","track_id","age","sex","breed_stage","breed_status","day")])

duplicates <- BirdStat.day %>% group_by(track_id,day) %>% summarise(NbId=length(sex)) %>% filter(NbId>1) %>% data.frame() %>% droplevels()

levels(duplicates$track_id) #ok no duplicates

YStracks.interp.buff.land$date2<-as.Date(YStracks.interp.buff.land$date)

YStracks.interp.buff.land<-left_join(YStracks.interp.buff.land,BirdStat.day,by=c("id"="track_id","date2"="day"))

nrow(YStracks.interp.buff.land[is.na(YStracks.interp.buff.land$dataset_id),])
###96 rows don't find event

unique(YStracks.interp.buff.land[is.na(YStracks.interp.buff.land$dataset_id),]$id) #"EE04096" 
#ok a gap in a tracks of 4 days between incubation and chick-rearing

#leaflet() %>% 
#  addProviderTiles(providers$Esri.WorldImagery, group = "World Imagery") %>%
#  addCircleMarkers(data = YStracks.interp.buff2[YStracks.interp.buff2$id=="EE04096",],
#                   radius = 5,
#                   fillColor = "cyan",
#                   fillOpacity = 0.5, stroke = F)
#ok removed it

YStracks.interp.buff.land<-YStracks.interp.buff.land[!is.na(YStracks.interp.buff.land$dataset_id),]

names(YStracks.interp.buff.land)

#format final files

YStracks.clean<-YStracks.interp.buff.land[c("dataset_id","site_name","colony_name","lon_colony" ,"lat_colony","device","id","age","sex","breed_stage","breed_status","date","x","y")]

unique(data.frame(YStracks)[c("dataset_id","scientific_name","common_name","country_code","country_name")])

YStracks.clean<-left_join(YStracks.clean, unique(data.frame(YStracks)[c("dataset_id","scientific_name","common_name","country_code","country_name")]))

YStracks.clean<-YStracks.clean[c("dataset_id","scientific_name","common_name","country_code","country_name","site_name","colony_name","lon_colony" ,"lat_colony","device","id","age","sex","breed_stage","breed_status","date","x","y")]

colnames(YStracks.clean)[colnames(YStracks.clean) == "id"] ="track_id"
colnames(YStracks.clean)[colnames(YStracks.clean) == "x"] ="longitude"
colnames(YStracks.clean)[colnames(YStracks.clean) == "y"] ="latitude"

head(YStracks.clean)
nrow(YStracks.clean)

saveRDS(YStracks.clean,"OutputR/YSCleanInterpTracks/InterpolatedGPSTracksYS.rds")

write.table(YStracks.clean,"OutputR/YSCleanInterpTracks/InterpolatedGPSTracksYS.csv",fileEncoding="UTF-8",row.names=FALSE,dec = ".",sep=";",na="")


################################################################################
### 10- Plot it = f(country_name)
################################################################################

rm(list=ls())
setwd("C:/Users/gwenael.quaintenne/OneDrive - LPO/LIFE_PanPuffinus/AnalysisA3")

TracksYS.interp<-readRDS("OutputR/YSCleanInterpTracks/InterpolatedGPSTracksYS.rds")

library(move)
library(raster)
library(sf)
library(lubridate)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(leaflet)

CountryPalette <- colorFactor(palette = c("#164D68","#777B7F","#EDEFE2","#88CCEE","#4E87B6","#B4BCCC"),levels=levels(TracksYS.interp$country_name))

TracksYS.interp %>% group_by(country_name) %>% summarise(NbTracks=n_distinct(track_id))

leaflet() %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "World Imagery") %>%
  addCircleMarkers(data = TracksYS.interp,
                   radius = 5,
                   fillColor = CountryPalette(TracksYS.interp$country_name),
                   fillOpacity = 0.8, stroke = F) %>%
  addLegend(colors = c("#164D68","#777B7F","#EDEFE2","#88CCEE","#4E87B6","#B4BCCC"),
            labels = c("Croatia n=58","France n=18","Greece n=27","Italy n=53","Malta n=264","Tunisia n=13"),
            title = "Yelkouan Shearwater GPS tracks",
            position = "bottomleft")




################################################################################
### 11- Construct kernelUD weighted per colony size
################################################################################

rm(list=ls())

library(raster)
library(sf)
library(lubridate)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(adehabitatHR)
library(viridis)

setwd("C:/Users/gwenael.quaintenne/OneDrive - LPO/LIFE_PanPuffinus/AnalysisA3")

###LOAD CLEANED TRACKS

TracksYS<-readRDS("OutputR/YSCleanInterpTracks/InterpolatedGPSTracksYS.rds") # 94 192 locs
TracksYS$track_id<-factor(TracksYS$track_id)

###CREATE GRID (same as GFW 0.1°Grid)

Grid<-raster(xmn=-5.65, xmx=43.05, ymn=30.55, ymx=47.95, resolution=0.1,crs="EPSG:4326")

#writeRaster(Grid, filename="OutputR/GFWraster/GFW_GRID.tiff", options="INTERLEAVE=BAND", overwrite=TRUE)

###CHECK MONTHLY DISTRIBUTION

TracksYS$month<-month(TracksYS$date)

source("C:/Users/gwenael.quaintenne/OneDrive - LPO/Enquete_AnatidesLimicolesNich/DataLIMAT2021-2022/1-CheckSamplingEffort/SourcesGraph.R")

ggplot(TracksYS, aes(x=month,fill=country_name)) + 
  geom_histogram(stat="count")+
  scale_fill_manual(values=c("#164D68","#777B7F","#EDEFE2","#88CCEE","#4E87B6","#B4BCCC"))+
  ylab("Nb locs") +
  scale_x_continuous(breaks=seq(2, 9, 1), labels=c("Feb","March","Apr","May","June","July","Aug","Sept"))+
  labs(fill="Month")+
  theme_wsj(base_size=12) +
  ggtitle("GPS Locs")

###REMOVED TRACKS WITH TOO FEW LOCATIONS nb locs should be >5 for Kernel, better to have >10
TracksYS<-data.frame(TracksYS)

Sel<-TracksYS %>% group_by(track_id) %>% summarise(NbLocs=length(dataset_id)) %>% filter(NbLocs<10)

TracksYS<-droplevels(TracksYS[!TracksYS$track_id %in% Sel$track_id,])

rm(Sel)

TracksYS.sf<-st_as_sf(TracksYS, coords = c("longitude", "latitude"), crs="EPSG:4326")

#GET COLONY WEIGHTS

XYcolonies<-read.table("C:/Users/gwenael.quaintenne/OneDrive - LPO/LIFE_PanPuffinus/DataLifePanPuffinusA3/BirdsData/ColonySizeData/XYcolonySize_YS.csv", header=TRUE, sep=";", encoding="UTF-8",stringsAsFactors = TRUE)

PopColony<-XYcolonies[XYcolonies$colony_name %in% levels(TracksYS.sf$colony_name),c("colony_name","pop_size_best")]
PopColony<-rbind(PopColony,data.frame(colony_name="Malta",pop_size_best=338.5))
PopColony

#load coast
land<-st_read("C:/Users/gwenael.quaintenne/OneDrive - LPO/LIFE_PanPuffinus/SIG_A3LifePanPuffinus/FondCarto/Land/CNTR_RG_01M_2020_4326.shp",crs="EPSG:4326",stringsAsFactors =TRUE)

land<-st_read("InputR/GIS/CNTR_RG_01M_2020_4326.shp",crs="EPSG:4326",stringsAsFactors =TRUE)

##############################################################################################
###CREATE UTILISATION DISTRIBUTION (UD) PER COLONY FOR ALL SEASON

Tracks.Colony<-list()
KUD.Colony<-list()
stk_KUD.Colony<-list()
sum_all_KUD.Colony<-list()

for (i in levels(TracksYS.sf$colony_name)) {
  Tracks.Colony[[i]]<-as(TracksYS.sf[TracksYS.sf$colony_name %in% i,], "Spatial")
  Tracks.Colony[[i]]@data<-droplevels(Tracks.Colony[[i]]@data)
  KUD.Colony[[i]]<-kernelUD(Tracks.Colony[[i]][, c("track_id")], h=0.2, grid=as(Grid, "SpatialPixels"))
  stk_KUD.Colony[[i]] <-stack(estUDm2spixdf(KUD.Colony[[i]]))
  sum_all_KUD.Colony[[i]] <- overlay(stk_KUD.Colony[[i]], fun = mean)
  sum_all_KUD.Colony[[i]] <- sum_all_KUD.Colony[[i]]/sum(getValues(sum_all_KUD.Colony[[i]]))
}

rm(KUD.Colony,stk_KUD.Colony,Tracks.Colony)

sum(getValues(sum_all_KUD.Colony))

#WEIGTH UDs PER POP SIZE OF COLONIES

TracksYS.sf[!TracksYS.sf$colony_name %in% PopColony$colony_name,] #ok all find colony size

data.frame(TracksYS.sf) %>% group_by(colony_name) %>% summarize(NbTracks=n_distinct(track_id)) %>% data.frame()

KUD.Colony.weigh.season<-list()

for (i in levels(TracksYS.sf$colony_name)) {
  KUD.Colony.weigh.season[[i]]<-sum_all_KUD.Colony[[i]]*(PopColony[PopColony$colony_name %in% i,]$pop_size_best/sum(PopColony$pop_size_best))
}

KUD.Colony.weigh.season <- stack(KUD.Colony.weigh.season)
KUD.Colony.weigh.season <- overlay(KUD.Colony.weigh.season, fun = mean)
KUD.Colony.weigh.season <- KUD.Colony.weigh.season/sum(getValues(KUD.Colony.weigh.season))

sum(getValues(KUD.Colony.weigh.season),na.rm=TRUE)

##############################################################################################
###CREATE UTILISATION DISTRIBUTION (UD) PER COLONY PER MONTH

###MARCH-APRIL
TracksYS.sf.march.apr<-TracksYS.sf[TracksYS.sf$month %in% c(3:4),]

Sel <- data.frame(TracksYS.sf.march.apr) %>% group_by(track_id) %>% summarise(NbLocs=length(dataset_id)) %>% filter(NbLocs<10)
TracksYS.sf.march.apr<-TracksYS.sf.march.apr[!TracksYS.sf.march.apr$track_id %in% Sel$track_id,]
TracksYS.sf.march.apr$colony_name<-droplevels(TracksYS.sf.march.apr$colony_name)
TracksYS.sf.march.apr$track_id<-droplevels(TracksYS.sf.march.apr$track_id)

Tracks.Colony<-list()
KUD.Colony<-list()
stk_KUD.Colony<-list()
sum_all_KUD.Colony<-list()

for (i in levels(TracksYS.sf.march.apr$colony_name)) {
  Tracks.Colony[[i]]<-as(TracksYS.sf.march.apr[TracksYS.sf.march.apr$colony_name %in% i,], "Spatial")
  Tracks.Colony[[i]]@data<-droplevels(Tracks.Colony[[i]]@data)
  KUD.Colony[[i]]<-kernelUD(Tracks.Colony[[i]][, c("track_id")], h=0.2, grid=as(Grid, "SpatialPixels"))
  stk_KUD.Colony[[i]] <- stack(estUDm2spixdf(KUD.Colony[[i]]))
  sum_all_KUD.Colony[[i]] <- overlay(stk_KUD.Colony[[i]], fun = mean)
  sum_all_KUD.Colony[[i]] <- sum_all_KUD.Colony[[i]]/sum(getValues(sum_all_KUD.Colony[[i]]))
}

rm(KUD.Colony,stk_KUD.Colony,Tracks.Colony)

#WEIGTH UDs PER POP SIZE OF COLONIES

TracksYS.sf.march.apr[!TracksYS.sf.march.apr$colony_name %in% PopColony$colony_name,] #ok all find colony size

data.frame(TracksYS.sf.march.apr) %>% group_by(colony_name) %>% summarize(NbTracks=n_distinct(track_id)) %>% data.frame()

KUD.Colony.weigh.march.april<-list()

for (i in levels(TracksYS.sf.march.apr$colony_name)) {
  KUD.Colony.weigh.march.april[[i]]<-sum_all_KUD.Colony[[i]]*(PopColony[PopColony$colony_name %in% i,]$pop_size_best/sum(PopColony$pop_size_best))
}

KUD.Colony.weigh.march.april <- stack(KUD.Colony.weigh.march.april)
KUD.Colony.weigh.march.april <- overlay(KUD.Colony.weigh.march.april, fun = mean)
KUD.Colony.weigh.march.april <- KUD.Colony.weigh.march.april/sum(getValues(KUD.Colony.weigh.march.april))

sum(getValues(KUD.Colony.weigh.march.april))

###MAY-JUNE
TracksYS.sf.may.june<-TracksYS.sf[TracksYS.sf$month %in% c(5:6),]

Sel <- data.frame(TracksYS.sf.may.june) %>% group_by(track_id) %>% summarise(NbLocs=length(dataset_id)) %>% filter(NbLocs<10)
TracksYS.sf.may.june<-TracksYS.sf.may.june[!TracksYS.sf.may.june$track_id %in% Sel$track_id,]
TracksYS.sf.may.june$colony_name<-droplevels(TracksYS.sf.may.june$colony_name)
TracksYS.sf.may.june$track_id<-droplevels(TracksYS.sf.may.june$track_id)

Tracks.Colony<-list()
KUD.Colony<-list()
stk_KUD.Colony<-list()
sum_all_KUD.Colony<-list()

for (i in levels(TracksYS.sf.may.june$colony_name)) {
  Tracks.Colony[[i]]<-as(TracksYS.sf.may.june[TracksYS.sf.may.june$colony_name %in% i,], "Spatial")
  Tracks.Colony[[i]]@data<-droplevels(Tracks.Colony[[i]]@data)
  KUD.Colony[[i]]<-kernelUD(Tracks.Colony[[i]][, c("track_id")], h=0.2, grid=as(Grid, "SpatialPixels"))
  stk_KUD.Colony[[i]] <- stack(estUDm2spixdf(KUD.Colony[[i]]))
  sum_all_KUD.Colony[[i]] <- overlay(stk_KUD.Colony[[i]], fun = mean)
  sum_all_KUD.Colony[[i]] <- sum_all_KUD.Colony[[i]]/sum(getValues(sum_all_KUD.Colony[[i]]))
}

rm(KUD.Colony,stk_KUD.Colony,Tracks.Colony)

#WEIGTH UDs PER POP SIZE OF COLONIES

TracksYS.sf.may.june[!TracksYS.sf.may.june$colony_name %in% PopColony$colony_name,] #ok all find colony size

data.frame(TracksYS.sf.may.june) %>% group_by(colony_name) %>% summarize(NbTracks=n_distinct(track_id)) %>% data.frame()

KUD.Colony.weigh.may.june<-list()

for (i in levels(TracksYS.sf.may.june$colony_name)) {
  KUD.Colony.weigh.may.june[[i]]<-sum_all_KUD.Colony[[i]]*(PopColony[PopColony$colony_name %in% i,]$pop_size_best/sum(PopColony$pop_size_best))
}

KUD.Colony.weigh.may.june <- stack(KUD.Colony.weigh.may.june)
KUD.Colony.weigh.may.june <- overlay(KUD.Colony.weigh.may.june, fun = mean)
KUD.Colony.weigh.may.june <- KUD.Colony.weigh.may.june/sum(getValues(KUD.Colony.weigh.may.june))

sum(getValues(KUD.Colony.weigh.may.june))

###JULY-AUGUST
TracksYS.sf.juil.aug<-TracksYS.sf[TracksYS.sf$month %in% c(7:8),]

Sel <- data.frame(TracksYS.sf.juil.aug) %>% group_by(track_id) %>% summarise(NbLocs=length(dataset_id)) %>% filter(NbLocs<10)
TracksYS.sf.juil.aug<-TracksYS.sf.juil.aug[!TracksYS.sf.juil.aug$track_id %in% Sel$track_id,]
TracksYS.sf.juil.aug$colony_name<-droplevels(TracksYS.sf.juil.aug$colony_name)
TracksYS.sf.juil.aug$track_id<-droplevels(TracksYS.sf.juil.aug$track_id)

Tracks.Colony<-list()
KUD.Colony<-list()
stk_KUD.Colony<-list()
sum_all_KUD.Colony<-list()

for (i in levels(TracksYS.sf.juil.aug$colony_name)) {
  Tracks.Colony[[i]]<-as(TracksYS.sf.juil.aug[TracksYS.sf.juil.aug$colony_name %in% i,], "Spatial")
  Tracks.Colony[[i]]@data<-droplevels(Tracks.Colony[[i]]@data)
  KUD.Colony[[i]]<-kernelUD(Tracks.Colony[[i]][, c("track_id")], h=0.2, grid=as(Grid, "SpatialPixels"))
  stk_KUD.Colony[[i]] <- stack(estUDm2spixdf(KUD.Colony[[i]]))
  sum_all_KUD.Colony[[i]] <- overlay(stk_KUD.Colony[[i]], fun = mean)
  sum_all_KUD.Colony[[i]] <- sum_all_KUD.Colony[[i]]/sum(getValues(sum_all_KUD.Colony[[i]]))
}

rm(KUD.Colony,stk_KUD.Colony,Tracks.Colony)

#WEIGTH UDs PER POP SIZE OF COLONIES

TracksYS.sf.juil.aug[!TracksYS.sf.juil.aug$colony_name %in% PopColony$colony_name,] #ok all find colony size

data.frame(TracksYS.sf.juil.aug) %>% group_by(colony_name) %>% summarize(NbTracks=n_distinct(track_id)) %>% data.frame()

KUD.Colony.weigh.juil.aug<-list()

for (i in levels(TracksYS.sf.juil.aug$colony_name)) {
  KUD.Colony.weigh.juil.aug[[i]]<-sum_all_KUD.Colony[[i]]*(PopColony[PopColony$colony_name %in% i,]$pop_size_best/sum(PopColony$pop_size_best))
}

KUD.Colony.weigh.juil.aug <- stack(KUD.Colony.weigh.juil.aug)
KUD.Colony.weigh.juil.aug <- overlay(KUD.Colony.weigh.juil.aug, fun = mean)
KUD.Colony.weigh.juil.aug <- KUD.Colony.weigh.juil.aug/sum(getValues(KUD.Colony.weigh.juil.aug))

sum(getValues(KUD.Colony.weigh.juil.aug))

################################################################################
### 13- Save UD maps
################################################################################

ls()[sapply(ls(), function(i) class(get(i))) == "RasterLayer"]

writeRaster(KUD.Colony.weigh.season, filename = "OutputR/YSWeightedUD/UDweightColonyKernel_YS_Feb2Sept.tif", format = "GTiff",overwrite=TRUE)

writeRaster(KUD.Colony.weigh.march.april, filename = "OutputR/YSWeightedUD/UDweightColonyKernel_YS_March2April.tif", format = "GTiff",overwrite=TRUE)

writeRaster(KUD.Colony.weigh.may.june, filename = "OutputR/YSWeightedUD/UDweightColonyKernel_YS_May2June.tif", format = "GTiff",overwrite=TRUE)

writeRaster(KUD.Colony.weigh.juil.aug, filename = "OutputR/YSWeightedUD/UDweightColonyKernel_YS_Jul2Aug.tif", format = "GTiff",overwrite=TRUE)


################################################################################
### 14- Construct kernelUD weighted per country size
################################################################################

rm(list=ls())

library(raster)
library(sf)
library(lubridate)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(adehabitatHR)
library(viridis)

setwd("C:/Users/gwenael.quaintenne/OneDrive - LPO/LIFE_PanPuffinus/AnalysisA3")

###LOAD CLEANED TRACKS

length(unique(TracksYS$track_id))
TracksYS<-readRDS("OutputR/YSCleanInterpTracks/InterpolatedGPSTracksYS.rds") # 96 276 locs
TracksYS$month<-month(TracksYS$date)
TracksYS$track_id<-factor(TracksYS$track_id)

###CREATE GRID (same as GFW 0.1°Grid)

Grid<-raster(xmn=-5.65, xmx=43.05, ymn=30.55, ymx=47.95, resolution=0.1,crs="EPSG:4326")

#writeRaster(Grid, filename="OutputR/GFWraster/GFW_GRID.tiff", options="INTERLEAVE=BAND", overwrite=TRUE)

###REMOVED TRACKS WITH TOO FEW LOCATIONS nb locs should be >5 for Kernel, better to have >10
TracksYS<-data.frame(TracksYS)

Sel<-TracksYS %>% group_by(track_id) %>% summarise(NbLocs=length(dataset_id)) %>% filter(NbLocs<10)

TracksYS<-droplevels(TracksYS[!TracksYS$track_id %in% Sel$track_id,])

rm(Sel)

TracksYS.sf<-st_as_sf(TracksYS, coords = c("longitude", "latitude"), crs="EPSG:4326")

TracksYS.sf$month<-month(TracksYS.sf$date)

###CREATE WEIGHT

XYcolonies<-read.table("C:/Users/gwenael.quaintenne/OneDrive - LPO/LIFE_PanPuffinus/DataLifePanPuffinusA3/BirdsData/ColonySizeData/XYcolonySize_YS.csv", header=TRUE, sep=";", encoding="UTF-8",stringsAsFactors = TRUE)
COUNTRY<-XYcolonies %>% group_by(Country_code,Country_name) %>% summarise(MinPop=sum(pop_size_min,na.rm = TRUE),MaxPop=sum(pop_size_max,na.rm = TRUE),pop_size_best=sum(pop_size_best,na.rm=TRUE)) %>% data.frame()

data.frame(TracksYS.sf) %>% group_by(country_name) %>% summarize(NbTracks=n_distinct(track_id)) %>% data.frame()

##LOAD COAST
land<-st_read("C:/Users/gwenael.quaintenne/OneDrive - LPO/LIFE_PanPuffinus/SIG_A3LifePanPuffinus/FondCarto/Land/CNTR_RG_01M_2020_4326.shp",crs="EPSG:4326",stringsAsFactors =TRUE)

##############################################################################################
###CREATE UTILISATION DISTRIBUTION (UD) PER COUNTRY FOR ALL SEASON

Tracks.Country<-list()
KUD.Country<-list()
stk_KUD.Country<-list()
sum_all_KUD.Country<-list()

for (i in levels(TracksYS.sf$country_code)) {
  Tracks.Country[[i]]<-as(TracksYS.sf[TracksYS.sf$country_code %in% i,], "Spatial")
  Tracks.Country[[i]]@data<-droplevels(Tracks.Country[[i]]@data)
  KUD.Country[[i]]<-kernelUD(Tracks.Country[[i]][, c("track_id")], h=0.2, grid=as(Grid, "SpatialPixels"))
  stk_KUD.Country[[i]] <- stack(estUDm2spixdf(KUD.Country[[i]]))
  sum_all_KUD.Country[[i]] <- overlay(stk_KUD.Country[[i]], fun = mean)
  sum_all_KUD.Country[[i]] <- sum_all_KUD.Country[[i]]/sum(getValues(sum_all_KUD.Country[[i]]))
}

rm(KUD.Country,stk_KUD.Country,Tracks.Country)

#WEIGTH UDs PER POP SIZE IN COUNTRIES

data.frame(TracksYS.sf) %>% group_by(country_code,country_name) %>% summarise(NbTracks=n_distinct(track_id))

KUD.Country.weigh.season<-list()

for (i in names(sum_all_KUD.Country)) {
  KUD.Country.weigh.season[[i]]<-sum_all_KUD.Country[[i]]*(COUNTRY[COUNTRY$Country_code %in% i,]$pop_size_best/sum(COUNTRY[COUNTRY$Country_code %in% names(sum_all_KUD.Country),]$pop_size_best))
}

KUD.Country.weigh.season <- stack(KUD.Country.weigh.season)
KUD.Country.weigh.season <- overlay(KUD.Country.weigh.season, fun = mean)
KUD.Country.weigh.season <- KUD.Country.weigh.season/sum(getValues(KUD.Country.weigh.season))

sum(getValues(KUD.Country.weigh.season))

##############################################################################################
###CREATE UTILISATION DISTRIBUTION (UD) PER COLONY PER MONTH

###MARCH-APRIL
TracksYS.sf.march.apr<-TracksYS.sf[TracksYS.sf$month %in% c(3:4),]

Sel <- data.frame(TracksYS.sf.march.apr) %>% group_by(track_id) %>% summarise(NbLocs=length(dataset_id)) %>% filter(NbLocs<10)
TracksYS.sf.march.apr<-TracksYS.sf.march.apr[!TracksYS.sf.march.apr$track_id %in% Sel$track_id,]
TracksYS.sf.march.apr$country_code<-droplevels(TracksYS.sf.march.apr$country_code)
TracksYS.sf.march.apr$track_id<-droplevels(TracksYS.sf.march.apr$track_id)

Tracks.Country<-list()
KUD.Country<-list()
stk_KUD.Country<-list()
sum_all_KUD.Country<-list()

for (i in levels(TracksYS.sf.march.apr$country_code)) {
  Tracks.Country[[i]]<-as(TracksYS.sf.march.apr[TracksYS.sf.march.apr$country_code %in% i,], "Spatial")
  Tracks.Country[[i]]@data<-droplevels(Tracks.Country[[i]]@data)
  KUD.Country[[i]]<-kernelUD(Tracks.Country[[i]][, c("track_id")], h=0.2, grid=as(Grid, "SpatialPixels"))
  stk_KUD.Country[[i]] <- stack(estUDm2spixdf(KUD.Country[[i]]))
  sum_all_KUD.Country[[i]] <- overlay(stk_KUD.Country[[i]], fun = mean)
  sum_all_KUD.Country[[i]] <- sum_all_KUD.Country[[i]]/sum(getValues(sum_all_KUD.Country[[i]]))
}

rm(KUD.Country,stk_KUD.Country,Tracks.Country)

#WEIGTH UDs 

TracksYS.sf.march.apr[!TracksYS.sf.march.apr$country_code %in% COUNTRY$Country_code,] #ok all find colony size

data.frame(TracksYS.sf.march.apr) %>% group_by(country_code,country_name) %>% summarize(NbTracks=n_distinct(track_id)) %>% data.frame()

KUD.Country.weigh.march.april<-list()

for (i in names(sum_all_KUD.Country)) {
  KUD.Country.weigh.march.april[[i]]<-sum_all_KUD.Country[[i]]*(COUNTRY[COUNTRY$Country_code %in% i,]$pop_size_best/sum(COUNTRY[COUNTRY$Country_code %in% names(sum_all_KUD.Country),]$pop_size_best))
}

KUD.Country.weigh.march.april <- stack(KUD.Country.weigh.march.april)
KUD.Country.weigh.march.april <- overlay(KUD.Country.weigh.march.april, fun = mean)
KUD.Country.weigh.march.april <- KUD.Country.weigh.march.april/sum(getValues(KUD.Country.weigh.march.april))

sum(getValues(KUD.Country.weigh.march.april))

###MAY-JUNE
TracksYS.sf.may.june<-TracksYS.sf[TracksYS.sf$month %in% c(5:6),]

Sel <- data.frame(TracksYS.sf.may.june) %>% group_by(track_id) %>% summarise(NbLocs=length(dataset_id)) %>% filter(NbLocs<10)
TracksYS.sf.may.june<-TracksYS.sf.may.june[!TracksYS.sf.may.june$track_id %in% Sel$track_id,]
TracksYS.sf.may.june$country_code<-droplevels(TracksYS.sf.may.june$country_code)
TracksYS.sf.may.june$track_id<-droplevels(TracksYS.sf.may.june$track_id)

Tracks.Country<-list()
KUD.Country<-list()
stk_KUD.Country<-list()
sum_all_KUD.Country<-list()

for (i in levels(TracksYS.sf.may.june$country_code)) {
  Tracks.Country[[i]]<-as(TracksYS.sf.may.june[TracksYS.sf.may.june$country_code %in% i,], "Spatial")
  Tracks.Country[[i]]@data<-droplevels(Tracks.Country[[i]]@data)
  KUD.Country[[i]]<-kernelUD(Tracks.Country[[i]][, c("track_id")], h=0.2, grid=as(Grid, "SpatialPixels"))
  stk_KUD.Country[[i]] <- stack(estUDm2spixdf(KUD.Country[[i]]))
  sum_all_KUD.Country[[i]] <- overlay(stk_KUD.Country[[i]], fun = mean)
  sum_all_KUD.Country[[i]] <- sum_all_KUD.Country[[i]]/sum(getValues(sum_all_KUD.Country[[i]]))
}

rm(KUD.Country,stk_KUD.Country,Tracks.Country)

#WEIGTH UDs

TracksYS.sf.may.june[!TracksYS.sf.may.june$country_code %in% COUNTRY$Country_code,] #ok all find country pop size

data.frame(TracksYS.sf.may.june) %>% group_by(country_code) %>% summarize(NbTracks=n_distinct(track_id)) %>% data.frame()

KUD.Country.weigh.may.june<-list()

for (i in names(sum_all_KUD.Country)) {
  KUD.Country.weigh.may.june[[i]]<-sum_all_KUD.Country[[i]]*(COUNTRY[COUNTRY$Country_code %in% i,]$pop_size_best/sum(COUNTRY[COUNTRY$Country_code %in% names(sum_all_KUD.Country),]$pop_size_best))
}

KUD.Country.weigh.may.june <- stack(KUD.Country.weigh.may.june)
KUD.Country.weigh.may.june <- overlay(KUD.Country.weigh.may.june, fun = mean)
KUD.Country.weigh.may.june <- KUD.Country.weigh.may.june/sum(getValues(KUD.Country.weigh.may.june))

sum(getValues(KUD.Country.weigh.may.june))

###JULY-AUGUST
TracksYS.sf.july.aug<-TracksYS.sf[TracksYS.sf$month %in% c(7:8),]

Sel <- data.frame(TracksYS.sf.july.aug) %>% group_by(track_id) %>% summarise(NbLocs=length(dataset_id)) %>% filter(NbLocs<10)
TracksYS.sf.july.aug<-TracksYS.sf.july.aug[!TracksYS.sf.july.aug$track_id %in% Sel$track_id,]
TracksYS.sf.july.aug$country_code<-droplevels(TracksYS.sf.july.aug$country_code)
TracksYS.sf.july.aug$track_id<-droplevels(TracksYS.sf.july.aug$track_id)

Tracks.Country<-list()
KUD.Country<-list()
stk_KUD.Country<-list()
sum_all_KUD.Country<-list()

for (i in levels(TracksYS.sf.july.aug$country_code)) {
  Tracks.Country[[i]]<-as(TracksYS.sf.july.aug[TracksYS.sf.july.aug$country_code %in% i,], "Spatial")
  Tracks.Country[[i]]@data<-droplevels(Tracks.Country[[i]]@data)
  KUD.Country[[i]]<-kernelUD(Tracks.Country[[i]][, c("track_id")], h=0.2, grid=as(Grid, "SpatialPixels"))
  stk_KUD.Country[[i]] <- stack(estUDm2spixdf(KUD.Country[[i]]))
  sum_all_KUD.Country[[i]] <- overlay(stk_KUD.Country[[i]], fun = mean)
  sum_all_KUD.Country[[i]] <- sum_all_KUD.Country[[i]]/sum(getValues(sum_all_KUD.Country[[i]]))
}

rm(KUD.Country,stk_KUD.Country,Tracks.Country)

#WEIGTH UDs

TracksYS.sf.july.aug[!TracksYS.sf.july.aug$country_code %in% COUNTRY$Country_code,] #ok all find country pop size

data.frame(TracksYS.sf.july.aug) %>% group_by(country_code) %>% summarize(NbTracks=n_distinct(track_id)) %>% data.frame()

KUD.Country.weigh.july.aug<-list()

for (i in names(sum_all_KUD.Country)) {
  KUD.Country.weigh.july.aug[[i]]<-sum_all_KUD.Country[[i]]*(COUNTRY[COUNTRY$Country_code %in% i,]$pop_size_best/sum(COUNTRY[COUNTRY$Country_code %in% names(sum_all_KUD.Country),]$pop_size_best))
}

KUD.Country.weigh.july.aug <- stack(KUD.Country.weigh.july.aug)
KUD.Country.weigh.july.aug <- overlay(KUD.Country.weigh.july.aug, fun = mean)
KUD.Country.weigh.july.aug <- KUD.Country.weigh.july.aug/sum(getValues(KUD.Country.weigh.july.aug))

sum(getValues(KUD.Country.weigh.july.aug))

################################################################################
### 15- Save UD maps
################################################################################

ls()[sapply(ls(), function(i) class(get(i))) == "RasterLayer"]

writeRaster(KUD.Country.weigh.season, filename = "OutputR/YSWeightedUD/UDweightCountryKernel_YS_Feb2Sept.tif", format = "GTiff",overwrite=TRUE)

writeRaster(KUD.Country.weigh.march.april, filename = "OutputR/YSWeightedUD/UDweightCountryKernel_YS_March2April.tif", format = "GTiff",overwrite=TRUE)

writeRaster(KUD.Country.weigh.may.june, filename = "OutputR/YSWeightedUD/UDweightCountryKernel_YS_May2June.tif", format = "GTiff",overwrite=TRUE)

writeRaster(KUD.Country.weigh.july.aug, filename = "OutputR/YSWeightedUD/UDweightCountryKernel_YS_Jul2Aug.tif", format = "GTiff",overwrite=TRUE)

################################################################################
### 16- Arrange map output
################################################################################

rm(list=ls())
setwd("C:/Users/gwenael.quaintenne/OneDrive - LPO/LIFE_PanPuffinus/AnalysisA3")

library(dplyr)
library(tidyr)
library(lubridate)
library(sf)
library(ggplot2)
library(classInt)
library(extrafont)
library(ggOceanMaps)
library(remotes)
library(leaflet)
library(trip)
library(adehabitatLT)
library(extrafont)
library(ggthemes)
library(raster)
library(graticule)
loadfonts(device = "win")

land<-st_read("InputR/GIS/CNTR_RG_01M_2020_4326.shp",crs="EPSG:4326",stringsAsFactors =TRUE)

bathy<-st_read("InputR/GIS/med_batim_latlong_100m.shp",crs="EPSG:4326",stringsAsFactors =TRUE)

bathy<-bathy[bathy$NAME %in% c("-5000 m","-4500 m","-4000 m","-3500 m","-3000 m","-2500 m","-2000 m","-1500 m","-1000 m","-500 m"),]

UD.season<-raster("OutputR/YSWeightedUD/UDweightCountryKernel_YS_Feb2Sept.tif")
UD.season<-aggregate(UD.season, fact=5,fun=sum)
values(UD.season)[values(UD.season)<0.0001] = NA

XYcolonies<-read.table("C:/Users/gwenael.quaintenne/OneDrive - LPO/LIFE_PanPuffinus/DataLifePanPuffinusA3/BirdsData/ColonySizeData/XYcolonySize_YS.csv", header=TRUE, sep=";", encoding="UTF-8",stringsAsFactors = TRUE)

TracksYS<-readRDS("InputR/YStracks/AllTracksYS.rds")
XYcolonies<-XYcolonies %>% mutate(TrackSample=ifelse(XYcolonies$colony_name %in% levels(TracksYS$colony_name),TRUE,FALSE)) %>% data.frame()


XYcolonies<-st_as_sf(XYcolonies, coords = c("XlongDD", "YlatDD"), crs="EPSG:4326")

my_col<-c('#ffff80ff','#8ff041ff','#3dd13bff','#2fa190ff','#215896ff','#0d1178ff')

#theme_set(theme_map() + theme(text = element_text(family = "Source Sans Pro")))
par(mar=c(5.5,2,2,2),oma=c(2,1,1,1))
grat <- graticule(lons = seq(0, 45, by = 5), lats = c(30, 35, 40, 45,50))

jpeg(file="OutputR/YSWeightedUD/UDmed_YS-Feb2Sept.jpg", res=600, width=10.917, height=7.708, units="in", pointsize=12,type="cairo")
par(mar=c(6,2,2,2),oma=c(1,1,1,1))
plot(st_geometry(bathy),col="#e2e2e2",
     main = "Yelkouan Shearwater Utilisation Distribution Febr.-Sept.",
     axes=TRUE,
     xlim = extent(rasterToPolygons(UD.season))[1:2],
     ylim = extent(rasterToPolygons(UD.season))[3:4])
plot(grat, add = TRUE, border="#e2e2e2",lwd=0.5,lty=4)
plot(UD.season,
     col=my_col,
     add=TRUE,
     axes=TRUE,
     alpha=0.8,
     xlim = extent(rasterToPolygons(UD.season))[1:2],
     ylim = extent(rasterToPolygons(UD.season))[3:4],
     horizontal = TRUE)
plot(st_geometry(land),col="#e2e2e2",border="#6e6e6eff",
     add = TRUE,
     axes = TRUE,
     xlim = extent(rasterToPolygons(UD.season))[1,2], 
     ylim = extent(rasterToPolygons(UD.season))[2,4],
     extent=extent(UD.season))
plot(rasterToPolygons(UD.season),
     add=TRUE,
     border="#6e6e6eff",
     lwd=1,
     axes=TRUE,
     xlim = extent(rasterToPolygons(UD.season))[1,2], 
     ylim = extent(rasterToPolygons(UD.season))[2,4])
plot(st_geometry(XYcolonies),col=c("black","red")[factor(XYcolonies$TrackSample)],cex=c(0.5,1)[factor(XYcolonies$TrackSample)],add=TRUE,pch=19)
legend("bottomleft", legend=c("Sampled colony", "Unsampled colony"),col=c("red","black"), pt.cex=c(1,0.5),cex=0.8,pch=19,bty = "n")
box(col="black")
dev.off()

###Now loop it per for two months period

FilesList<-data.frame(Files=list.files("C:/Users/gwenael.quaintenne/OneDrive - LPO/LIFE_PanPuffinus/AnalysisA3/OutputR/YSWeightedUD",pattern=".tif",all.files=TRUE,full.names=FALSE,recursive=TRUE),ID=c(1:length(list.files("C:/Users/gwenael.quaintenne/OneDrive - LPO/LIFE_PanPuffinus/AnalysisA3/OutputR/YSWeightedUD",pattern=".tif",recursive=TRUE))))

FilesList<-FilesList[!grepl("Colony",FilesList$Files),]
FilesList$Period<-factor(c("Feb.-Sept.","July-Aug.","March-April","May-June"))
FilesList<-droplevels(FilesList[!FilesList$Period %in% c("Feb.-Sept."),])

TracksYS<-readRDS("InputR/YStracks/AllTracksYS.rds")
TracksYS<-TracksYS[TracksYS$device %in% c("GPS"),]
TracksYS$month<-month(TracksYS$date)
TracksYS$track_id<-factor(TracksYS$track_id)

st_drop_geometry(TracksYS) %>% group_by(month,country_code,country_name) %>% summarise(NbTracks=n_distinct(track_id)) %>% data.frame()

TracksYS<- TracksYS %>% mutate(Period=ifelse(month(date) %in% c(7:8),"July-Aug.",ifelse(month(date) %in% c(3:4),"March-April",ifelse(month(date) %in% c(5:6),"May-June",NA)))) 

NbTracksMonth <- st_drop_geometry(TracksYS) %>% group_by(colony_name,Period) %>% summarise(NbTracks=n_distinct(track_id)) %>% data.frame()

NbTracksMonth <- left_join(expand.grid(Period=levels(FilesList$Period),colony_name=levels(XYcolonies$colony_name)),NbTracksMonth,by=c("colony_name","Period")) %>% mutate(NbTracks = replace_na(NbTracks, 0))

NbTracksMonth<-left_join(NbTracksMonth,XYcolonies,by=c("colony_name"))  
NbTracksMonth$TrackSample<-ifelse(NbTracksMonth$NbTracks>0,TRUE,FALSE)
NbTracksMonth<-st_as_sf(NbTracksMonth, crs="EPSG:4326")
st_geometry(NbTracksMonth)[NbTracksMonth$colony_name %in% c("Malta")] <- st_point(c(14.35679,35.97265))

par(mar=c(5.5,2,2,2),oma=c(2,1,1,1))
grat <- graticule(lons = seq(0, 45, by = 5), lats = c(30, 35, 40, 45,50))

for (i in unique(NbTracksMonth$Period)) {
  UD<-raster(paste0("OutputR/YSWeightedUD/",FilesList[FilesList$Period %in% i,]$Files))
  UD<-aggregate(UD, fact=5,fun=sum)
  NbTracksMonth.i<-NbTracksMonth[NbTracksMonth$Period %in% i,]
  values(UD)[values(UD)==0] = NA
  jpeg(file=paste0("OutputR/YSWeightedUD/UDmed_YS-",i,".jpg"), res=600, width=10.917, height=7.708, units="in", pointsize=12,type="cairo")
  par(mar=c(6,2,2,2),oma=c(1,1,1,1))
  plot(st_geometry(bathy),col="#e2e2e2",
       main = paste0("Yelkouan Shearwater Utilisation Distribution ", i),
       axes=TRUE,
       xlim = extent(rasterToPolygons(UD.season))[1:2],
       ylim = extent(rasterToPolygons(UD.season))[3:4])
  plot(grat, add = TRUE, border="#e2e2e2",lwd=0.5,lty=4)
  plot(UD,
       col=my_col,
       add=TRUE,
       axes=TRUE,
       alpha=0.8,
       xlim = extent(rasterToPolygons(UD.season))[1:2],
       ylim = extent(rasterToPolygons(UD.season))[3:4],
       horizontal = TRUE)
  plot(st_geometry(land),col="#e2e2e2",border="#6e6e6eff",
       add = TRUE,
       axes = TRUE,
       xlim = extent(rasterToPolygons(UD.season))[1,2], 
       ylim = extent(rasterToPolygons(UD.season))[2,4],
       extent=extent(UD.season))
  plot(rasterToPolygons(UD),
       add=TRUE,
       border="#6e6e6eff",
       lwd=1,
       axes=TRUE,
       xlim = extent(rasterToPolygons(UD.season))[1,2], 
       ylim = extent(rasterToPolygons(UD.season))[2,4])
  plot(st_geometry(NbTracksMonth.i),col=c("black","red")[factor(NbTracksMonth.i$TrackSample)],cex=c(0.5,1)[factor(NbTracksMonth.i$TrackSample)],add=TRUE,pch=19)
  legend("bottomleft", legend=c("Sampled colony", "Unsampled colony"),col=c("red","black"), pt.cex=c(1,0.5),cex=0.8,pch=19,bty = "n")
  box(col="black")
  dev.off()
}

