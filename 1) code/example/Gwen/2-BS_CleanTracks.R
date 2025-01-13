################################################################################
########
## Balearic Shearwater tracks cleaning
## Object: tracks data cleaning prior to UDs 
# GQ - LPO for LIFE19 NAT/MT/000982 "LIFE PanPuffinus!"
# Creation      25/07/2022  v0
# Last Update   24/03/2023  v3  test Utilization distributions (UDs) of adehabitatHR package
########
################################################################################

rm(list=ls())
setwd("C:/Users/gwenael.quaintenne/OneDrive - LPO/LIFE_PanPuffinus/AnalysisA3")

library(move)
library(lubridate)
library(dplyr)
library(aniMotum)
library(sf)
library(tidyverse)
library(ggplot2)
library(rnaturalearth)
library(leaflet)
library(purrr)
library(furrr)
library(lubridate)
library(stats)
library(trip)
library(adehabitatLT)
library(ggplot2)
library(ggmap)
library(ggthemes)
library(sf)
library(lubridate)
library(raster)
library(ggspatial)
library(osmdata)
library(ceramic)

################################################################################
### 0- Load tracks
################################################################################

TracksBS<-readRDS("InputR/BStracks/AllTracksBS.RDS")

TracksBS %>% group_by(dataset_id,device,site_name,year(date),age) %>% summarize(NbTracks=n_distinct(track_id)) %>%spread(key = age, value = NbTracks, fill = 0) %>% data.frame()

#  site_name  `n_distinct(track_id)`
# Cabrera                        11
# Formentera                      2
# Ibiza                          62
# Mallorca                        5
# Menorca                        53


leaflet() %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "World Imagery") %>%
  addCircleMarkers(data = TracksBS,
                   radius = 5,
                   fillColor = "cyan",
                   fillOpacity = 0.5, stroke = F)

################################################################################
### 1- Assess Quality of PTT tracks
################################################################################

PTT<-TracksBS[TracksBS$device=="PTT",]

#summary
PTT %>% group_by(dataset_id,track_id,age,sex,breed_stage,breed_status) %>% summarise(Nblocs=length(latitude),MinDate=min(date),MaxDate=max(date)) %>% data.frame()

#create trip object
PTT.trip <- PTT %>% 
  group_by(track_id) %>% 
  dplyr::select(x = longitude, 
                y = latitude, 
                DateTime = date, 
                everything()) %>% 
  trip()

#create traj object
PTT.traj <- as.ltraj(xy = bind_cols(x = PTT.trip$x, 
                                       y = PTT.trip$y),
                        date = PTT.trip$DateTime,
                        id = PTT.trip$track_id)

#plot traj
Coast <- st_read("InputR/Coastline/CNTR_RG_01M_2020_4326_cutMed.shp")
plot(PTT.traj,id=PTT[PTT$age =="adult",]$track_id,spoldf=as(st_geometry(Coast), "Spatial"),colspoldf="grey",perani=TRUE,xlim=extent(Coast)[1:2],ylim=extent(Coast)[3:4],title="PTT tracks - Balearic Shearwater Juveniles")

#removed unrealistic velocity (>100km/h)
filter<-speedfilter(PTT.trip, max.speed = 100)  # speed in km/h
summary(filter) #one location removed

PTT<-subset(cbind(PTT,filter),filter==TRUE)
PTT.trip<-subset(PTT.trip,filter==TRUE)

PTT.traj <- as.ltraj(xy = bind_cols(x = PTT.trip$x, 
                                    y = PTT.trip$y),
                     date = PTT.trip$DateTime,
                     id = PTT.trip$track_id)

#linear interpolation
PTT.trip.interp<-redisltraj(PTT.traj, 1800, type="time")

#plot it
Coast <- st_read("InputR/Coastline/CNTR_RG_01M_2020_4326_cutMed.shp")
plot(PTT.trip.interp,spoldf=as(st_geometry(Coast), "Spatial"),colspoldf="grey",perani=TRUE,xlim=extent(Coast)[1:2],ylim=extent(Coast)[3:4])

#get time lag

TimeLag<-data.frame(PTT.trip) %>%
  arrange(track_id, DateTime) %>% 
  group_by(track_id) %>%
  mutate(time_lag = DateTime-lag(DateTime)) %>%
  Rmisc::summarySE(measurevar = "time_lag", groupvars = "dataset_id", na.rm=TRUE,conf.interval = 0.95) %>%
  data.frame()

TimeLag<-data.frame(PTT.trip) %>%
  arrange(track_id, DateTime) %>% 
  group_by(track_id) %>%
  mutate(time_lag = DateTime-lag(DateTime)) %>%
  data.frame()

TimeLag %>% group_by(dataset_id) %>% summarise(Nbtracks=n_distinct(track_id))

ggplot(TimeLag, aes(x=dataset_id, y=time_lag, fill=dataset_id)) + geom_boxplot() + 
  guides(fill=FALSE) + coord_flip()

#Compare time lag with GPS tracks

GPS<-TracksBS[TracksBS$device=="GPS",]

GPS <- GPS %>%
  arrange(track_id, date) %>% 
  group_by(track_id) %>%
  mutate(time_lag = date-lag(date)) %>%
  data.frame()

GPS %>% group_by(dataset_id) %>% Rmisc::summarySE(measurevar = "time_lag", groupvars = "dataset_id", na.rm=TRUE,conf.interval = 0.95) %>%
  data.frame()

GPS %>% group_by(dataset_id) %>% summarise(Nbtracks=n_distinct(track_id))

##decision to drop PTT from analysis, too low temporal resolution
##decision to standardized GPS tracks to location at 30 minutes intervals

rm(list=ls()[! ls() %in% c("Coast","TracksBS")])


################################################################################
### 2- Select data
################################################################################

#removed PTT tracks
TracksBS<-droplevels(TracksBS[TracksBS$device=="GPS",]) # 80 523 obs.

wgs84 <- st_crs("EPSG:4326")

##sf object
BStracks.sf <- st_as_sf(TracksBS, coords = c("longitude", "latitude"), crs=4326)


leaflet() %>% ## start leaflet plot
  addProviderTiles(providers$Esri.WorldImagery, group = "World Imagery") %>% 
  ## plot the points. Note: leaflet automatically finds lon / lat colonies
  ## Colour accordingly.
  addCircleMarkers(data = TracksBS,
                   radius = 3,
                   fillColor = "cyan",
                   fillOpacity = 0.5, stroke = F) 

################################################################################
### 3- McConnel Speedilter realistic velocity was set at 100 km/h
################################################################################

## create trip object
BStracks.trip <- TracksBS %>% 
  group_by(track_id) %>% 
  dplyr::select(x = longitude, 
                y = latitude, 
                DateTime = date, 
                everything()) %>% 
  trip()

# McConnel Speedilter
BStracks.trip$Filter <- speedfilter(BStracks.trip, max.speed = 100)  # speed in km/h
BStracks.trip <- data.frame(BStracks.trip) %>% mutate(longitude = x,latitude = y)
summary(BStracks.trip$Filter) #ok no one removed

## plot McConnel Removed values
leaflet() %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "World Imagery") %>%
  addCircleMarkers(data = subset(BStracks.trip, BStracks.trip$Filter == T),
                         radius = 7,
                         fillColor = "cyan",
                         fillOpacity = 0.5, stroke = F) %>% 
  addCircleMarkers(data = subset(BStracks.trip, BStracks.trip$Filter == F),
                   #label = bird_track_gaps$nlocs, 
                   radius = 5,
                   fillColor = "purple",
                   fillOpacity = 0.5, stroke = F) %>% 
  addLegend(colors = c("cyan","purple"),
            labels = c("keeped values","Removed values"))

################################################################################
### 4- Linear interpolation locs every 30 min
################################################################################

## create ltraj object
BStracks.ltraj <- as.ltraj(xy = bind_cols(x = BStracks.trip$x, 
                                       y = BStracks.trip$y),
                        date = BStracks.trip$DateTime,
                        id = BStracks.trip$track_id)

## re-sample tracks every 30 minutes (30*60 sec)
BStracks.interp <- redisltraj(BStracks.ltraj, 30*60, type="time")
BStracks.interp <- ld(BStracks.interp) %>% mutate(longitude = x,latitude = y)

leaflet() %>% 
  addProviderTiles(providers$Esri.WorldImagery, group = "World Imagery") %>%
  addCircleMarkers(data = TracksBS,
                   radius = 5,
                   fillColor = "grey",
                   fillOpacity = 0.5, stroke = F) %>% 
  addPolylines(lng = TracksBS$longitude,
               lat = TracksBS$latitude, weight = 1,
               color = "grey") %>%
  addCircleMarkers(data = BStracks.interp,
                   radius = 5,
                   fillColor = "cyan",
                   fillOpacity = 0.5, stroke = F) %>% 
  addPolylines(lng = BStracks.interp$longitude,
               lat = BStracks.interp$latitude, weight = 1,
               color = "cyan") %>%
  addLegend(colors = c("grey","cyan"),
            labels = c("original locs","resampled locs"))
#zoom on ok looks fine

################################################################################
### 5- Removed locations within 5km buffer of the colony
################################################################################

BStracks.interp<-left_join(BStracks.interp,unique(TracksBS[,c("track_id","site_name","colony_name","lon_colony","lat_colony","device")]),by = c("id" = "track_id"))

col<-unique(BStracks.interp[c("site_name","colony_name","lon_colony","lat_colony")])
col$id_col<-factor(c(1:nrow(col)))
BStracks.interp<-left_join(BStracks.interp,col)

BStracks.interp.sf<-st_as_sf(BStracks.interp, coords = c("longitude", "latitude"), crs=4326)
BStracks.interp.sf.prj<-st_transform(BStracks.interp.sf, center=TRUE)

## Creating a buffer around colonies
BStracks.interp.buff <- data.frame()

st_over <- function(x, y) {
  sapply(sf::st_intersects(x, y), function(z)
    if (length(z) == 0) {
      NA_integer_
    } else {
      z[1]
    })
}

for(i in levels(BStracks.interp.sf.prj$id_col)){
  sub_col <- BStracks.interp.sf.prj[BStracks.interp.sf.prj$id_col %in% i,]
  df_col <- data.frame(cbind(lon=sub_col$lon_colony[1], lat=sub_col$lat_colony[1]))
  df_col <- st_as_sf(df_col, coords = c("lon", "lat"), crs=4326)
  col_transf <- st_transform(df_col, crs(BStracks.interp.sf.prj))
  buf_col <- st_buffer(col_transf, dist = 5000)
  BStracks.interp.buff.col <- sub_col[is.na(st_over(sub_col,buf_col)),]
  BStracks.interp.buff <- rbind(BStracks.interp.buff,data.frame(BStracks.interp.buff.col))
}

BStracks.interp.buff$longitude<-BStracks.interp.buff$x
BStracks.interp.buff$latitude<-BStracks.interp.buff$y
BStracks.interp.buff.sf<-st_as_sf(BStracks.interp.buff, coords = c("longitude", "latitude"), crs=4326)

nrow(BStracks.interp)-nrow(BStracks.interp.buff) #1625 locations removed

rm(list=ls()[! ls() %in% c("Coast","TracksBS","BStracks.interp.buff")])

#add attributes of birds

BStracks.interp.buff<-left_join(BStracks.interp.buff,unique(TracksBS[,c("dataset_id","track_id","age","sex","breed_stage","breed_status")]),by = c("id" = "track_id"))

saveRDS(BStracks.interp.buff,"OutputR/BSCleanInterpTracks/InterpolatedGPSTracksBS.rds")

write.table(BStracks.interp.buff,"OutputR/BSCleanInterpTracks/InterpolatedGPSTracksBS.csv",fileEncoding="UTF-8",row.names=FALSE,dec = ".",sep=";",na="")

################################################################################
### 6- Plot it = f(site_name)
################################################################################

rm(list=ls())
setwd("C:/Users/gwenael.quaintenne/OneDrive - LPO/LIFE_PanPuffinus/AnalysisA3")

TracksBS<-readRDS("InputR/BStracks/AllTracksBS.RDS")
TracksBS<-droplevels(TracksBS[TracksBS$device =="GPS",])

TracksBS.interp<-readRDS("OutputR/BSCleanInterpTracks/InterpolatedGPSTracksBS.rds")
TracksBS.interp$track_id<-factor(TracksBS.interp$id)

library(move)
library(raster)
library(sf)
library(lubridate)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(leaflet)

SitePalette <- colorFactor(palette = c("#67A3F0","#F07F73","#5BEBF0","#4FF09B"),levels=levels(TracksBS.interp$site_name))

TracksBS.interp %>% group_by(site_name) %>% summarise(NbTracks=n_distinct(id))

leaflet() %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "World Imagery") %>%
  addCircleMarkers(data = TracksBS.interp,
                   radius = 5,
                   fillColor = SitePalette(TracksBS.interp$site_name),
                   fillOpacity = 0.5, stroke = F) %>%
  addLegend(colors = c("#4FF09B","#5BEBF0","#67A3F0","#F07F73"),
            labels = c("Menorca n=46","Mallorca n=2","Cabrera n=11","Ibiza n=55"),
            title = "Balearic Shearwater GPS tracks")

################################################################################
### 7- check phenology stage life-history
################################################################################

summary(TracksBS.interp$date)

#year' deployment
NbTracks.yrs<-TracksBS.interp %>% group_by(year(date),id,site_name) %>% summarise(NbLocs=length(x)) %>% data.frame()

source("C:/Users/gwenael.quaintenne/OneDrive - LPO/Enquete_AnatidesLimicolesNich/DataLIMAT2021-2022/1-CheckSamplingEffort/SourcesGraph.R",encoding ="UTF-8")
ggplot(NbTracks.yrs, aes(x=year.date.,fill=site_name)) + 
  geom_histogram(stat="count")+
  scale_fill_manual(values=c("#67A3F0","#F07F73","#5BEBF0","#4FF09B"))+
  ylab("Nb tracks") +
  scale_x_continuous(breaks=seq(2012, 2022, 1))+
  labs(fill="Site")+
  theme_wsj(base_size=12) +
  ggtitle("GPS Year's deployment")

TracksBS.interp$month<-month(TracksBS.interp$date)

#month' deployment
NbTracks.months <- TracksBS.interp %>% group_by(month,id,site_name) %>% summarise(NbLocs=length(x)) %>% data.frame()

ggplot(NbTracks.months, aes(x=month,fill=site_name)) + 
  geom_histogram(stat="count")+
  scale_fill_manual(values=c("#67A3F0","#F07F73","#5BEBF0","#4FF09B"))+
  ylab("Nb tracks") +
  labs(fill="Month")+
  theme_wsj(base_size=12) +
  scale_x_continuous(breaks=c(3:9),labels=c("March", "April", "May","June","July","August","September")) +
  ggtitle("GPS Month's deployment")

#By month

TracksBS.interp.sf<-st_as_sf(TracksBS.interp, coords = c("x", "y"), crs=4326)

month.labs <- c("March", "April", "May","June","July","August","September")
names(month.labs) <- c(3:9)

library(ceramic)
library(terrainr)

gq<-"pk.eyJ1IjoiZ3F1YWluIiwiYSI6ImNsZzUwZng4bDAyMG4zeG9oNTZ5NTZmeTQifQ.tmpSD1d0AXwbflcK3pQxDg"
Sys.setenv(MAPBOX_API_KEY=gq) 

im <- cc_location(extent(TracksBS.interp.sf)+0.1,type = "mapbox.satellite",buffer=50)
im2 <- cc_location(extent(TracksBS.interp.sf),type = "mapbox.satellite")

#raster::plotRGB(im)
im <- as.data.frame(im, xy = TRUE)
im <- setNames(im, c("x", "y", "red","green","blue"))

TracksBS.interp.sf2<-st_transform(TracksBS.interp.sf,crs=3857)

ggplot()+
  geom_spatial_rgb(im,mapping = aes(x = x,y = y,r = red,g = green, b = blue),alpha=0.8)+
  geom_sf(data = TracksBS.interp.sf2, mapping = aes(colour = site_name),size = 1,alpha = 0.6) +
  scale_colour_manual(values=c("#67A3F0","#F07F73","#5BEBF0","#4FF09B"),name = "")+
  facet_wrap(~month,labeller = labeller(month = month.labs))+
  theme_map(base_size = 12, base_family = "sans")+
  theme(legend.position = "top",strip.background = element_blank(),strip.text = element_text(size=14,hjust = 0,family="sans"),legend.background = element_blank())

rm(NbTracks.yrs,NbTracks.months)

#life history stages' deployment

NbTracks.months <- TracksBS.interp %>% group_by(month,id,site_name,age,sex,breed_stage,breed_status) %>% summarise(NbLocs=length(x)) %>% data.frame()

ggplot(NbTracks.months, aes(x=month,fill=site_name)) + 
  geom_histogram(stat="count")+
  scale_fill_manual(values=c("#67A3F0","#F07F73","#5BEBF0","#4FF09B"))+
  ylab("Nb tracks") +
  labs(fill="Month")+
  theme_wsj(base_size=12) +
  facet_wrap( ~ breed_stage, ncol=2) +
  scale_x_continuous(breaks=c(3:9),labels=c("March", "April", "May","June","July","August","September")) +
  ggtitle("GPS Month's deployment")

ggplot(NbTracks.months, aes(x=breed_stage,fill=site_name)) + 
  geom_histogram(stat="count")+
  scale_fill_manual(values=c("#67A3F0","#F07F73","#5BEBF0","#4FF09B"))+
  ylab("Nb tracks") +
  theme_wsj(base_size=12) +
  ggtitle("GPS Breed stage's deployment")

#map it per breed stage

TracksBS.interp %>% group_by(breed_stage,site_name) %>% summarise(Nbtracks=n_distinct(id))

#breed-stage "breeding"
unique(TracksBS.interp[TracksBS.interp$breed_stage %in% c("breeding"),]$month)
unique(TracksBS[TracksBS$breed_stage %in% c("breeding"),]$dataset_id) #879
unique(TracksBS[TracksBS$breed_stage %in% c("breeding"),]$breed_status)
unique(TracksBS[TracksBS$dataset_id %in% c("879"),]$breed_stage)

leaflet() %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "World Imagery") %>%
  addCircleMarkers(data = TracksBS.interp[TracksBS.interp$breed_stage %in% c("breeding"),],
                   radius = 5,
                   fillColor = SitePalette(TracksBS.interp[TracksBS.interp$breed_stage %in% c("breeding"),]$site_name),
                   fillOpacity = 0.5, stroke = F) %>%
  addLegend(colors = c("#F07F73"),
            labels = c("Ibiza n=7"),
            title = "Balearic Shearwater GPS tracks</br>Breeding May-June")

#reassign "breeding" to "chick-rearing
TracksBS[TracksBS$breed_stage %in% c("breeding"),]$breed_stage
levels(TracksBS$breed_stage)[levels(TracksBS$breed_stage)=="breeding"] <- "chick-rearing"
levels(TracksBS.interp$breed_stage)[levels(TracksBS.interp$breed_stage)=="breeding"] <- "chick-rearing"

###Incubation

unique(TracksBS.interp[TracksBS.interp$month %in% c(3:4),]$site_name)
unique(TracksBS.interp[TracksBS.interp$month %in% c(3:4),]$breed_stage)

leaflet() %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "World Imagery") %>%
  addCircleMarkers(data = TracksBS.interp[TracksBS.interp$breed_stage %in% c("incubation"),],
                   radius = 5,
                   fillColor = SitePalette(TracksBS.interp[TracksBS.interp$breed_stage %in% c("incubation"),]$site_name),
                   fillOpacity = 0.5, stroke = F) %>%
  addLegend(colors = c("#F07F73"),
            labels = c("Ibiza n=18"),
            title = "Balearic Shearwater GPS tracks</br>Incubation March-April")

###Chick-rearing

unique(TracksBS.interp[TracksBS.interp$breed_stage %in% c("chick-rearing"),]$month)

unique(TracksBS.interp[TracksBS.interp$breed_stage %in% c("chick-rearing"),]$breed_status)

leaflet() %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "World Imagery") %>%
  addCircleMarkers(data = TracksBS.interp[TracksBS.interp$breed_stage %in% c("chick-rearing"),],
                   radius = 5,
                   fillColor = SitePalette(TracksBS.interp[TracksBS.interp$breed_stage %in% c("chick-rearing"),]$site_name),
                   fillOpacity = 0.5, stroke = F) %>%
  addLegend(colors = c("#4FF09B","#5BEBF0","#67A3F0","#F07F73"),
            labels = c("Menorca n=34","Mallorca n=2","Cabrera n=11","Ibiza n=37"),
            title = "Balearic Shearwater GPS tracks</br>Chick-rearing May-July")

###Late chick-rearing/Dispersal

unique(TracksBS.interp[TracksBS.interp$breed_stage %in% c("Late chick-rearing/Dispersal"),]$month) #7-8

unique(TracksBS.interp[TracksBS.interp$breed_stage %in% c("Late chick-rearing/Dispersal"),]$breed_status)

leaflet() %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "World Imagery") %>%
  addCircleMarkers(data = TracksBS.interp[TracksBS.interp$breed_stage %in% c("Late chick-rearing/Dispersal"),],
                   radius = 5,
                   fillColor = SitePalette(TracksBS.interp[TracksBS.interp$breed_stage %in% c("Late chick-rearing/Dispersal"),]$site_name),
                   fillOpacity = 0.5, stroke = F) %>%
  addLegend(colors = c("#4FF09B"),
            labels = c("Menorca n=8"),
            title = "Balearic Shearwater GPS tracks</br>Late chick-rearing/Dispersal July-Aug.")

leaflet() %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "World Imagery") %>%
  addCircleMarkers(data = TracksBS.interp[TracksBS.interp$breed_stage %in% c("Late chick-rearing/Dispersal") & TracksBS.interp$month %in% 8,],
                   radius = 5,
                   fillColor = SitePalette(TracksBS.interp[TracksBS.interp$breed_stage %in% c("Late chick-rearing/Dispersal") & TracksBS.interp$month %in% 8,]$site_name),
                   fillOpacity = 0.5, stroke = F) %>%
  addLegend(colors = c("#4FF09B"),
            labels = c("Menorca n=8"),
            title = "Balearic Shearwater GPS tracks</br>Late chick-rearing/Dispersal Aug.")

###migration

unique(TracksBS.interp[TracksBS.interp$breed_stage %in% c("migration"),]$month) #7-8-9
unique(TracksBS.interp[TracksBS.interp$breed_stage %in% c("migration"),]$site_name) #menorca
unique(TracksBS.interp[TracksBS.interp$breed_stage %in% c("migration"),]$breed_status) #non-breeding
unique(TracksBS.interp[TracksBS.interp$breed_stage %in% c("migration"),]$id) #n=4 tracks
unique(TracksBS.interp[TracksBS.interp$breed_stage %in% c("migration"),]$age) #immature

leaflet() %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "World Imagery") %>%
  addCircleMarkers(data = TracksBS.interp[TracksBS.interp$breed_stage %in% c("migration"),],
                   radius = 5,
                   fillColor = SitePalette(TracksBS.interp[TracksBS.interp$breed_stage %in% c("migration"),]$site_name),
                   fillOpacity = 0.5, stroke = F) %>%
  addLegend(colors = c("#4FF09B"),
            labels = c("Menorca n=4"),
            title = "Balearic Shearwater GPS tracks</br>Migration/Non-breeding July-Sept.")

leaflet() %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "World Imagery") %>%
  addCircleMarkers(data = TracksBS.interp[TracksBS.interp$breed_stage %in% c("migration") & TracksBS.interp$month %in% 8,],
                   radius = 5,
                   fillColor = SitePalette(TracksBS.interp[TracksBS.interp$breed_stage %in% c("migration") & TracksBS.interp$month %in% 8,]$site_name),
                   fillOpacity = 0.5, stroke = F) %>%
  addLegend(colors = c("#4FF09B"),
            labels = c("Menorca n=4"),
            title = "Balearic Shearwater GPS tracks</br>Migration/Non-breeding Sept.")

TracksBS.interp[TracksBS.interp$month %in% c(5:7),] %>% group_by(breed_stage,site_name,age,breed_status) %>% summarise(Nbtracks=n_distinct(id)) %>% data.frame()

################################################################################
### 6- Construct UDs grid with Move Package
################################################################################

rm(list=ls())
setwd("C:/Users/gwenael.quaintenne/OneDrive - LPO/LIFE_PanPuffinus/AnalysisA3")

TracksBS<-readRDS("OutputR/BSCleanInterpTracks/InterpolatedGPSTracksBS.rds")
TracksBS$track_id<-factor(TracksBS$id)

library(move)
library(raster)
library(sf)
library(lubridate)
library(ggplot2)
library(ggthemes)
library(dplyr)

###CREATE GRID (same as GFW 0.1°Grid)

Grid <- raster(xmn=-5.65, xmx=12.05, ymn=34.85, ymx=44.65, resolution=0.1,crs="EPSG:4326")

###REMOVED LOCS OUTSIDE MED (i.e. grid)

TracksBS<-st_as_sf(TracksBS, coords = c("longitude", "latitude"), crs="EPSG:4326")
TracksBS<-st_crop(TracksBS, extent(Grid))
TracksBS$longitude<-st_coordinates(TracksBS)[,1]
TracksBS$latitude<-st_coordinates(TracksBS)[,2]
TracksBS<-data.frame(TracksBS)

###CHECK MONTHLY DISTRIBUTION

TracksBS$month<-month(TracksBS$date)

source("C:/Users/gwenael.quaintenne/OneDrive - LPO/Enquete_AnatidesLimicolesNich/DataLIMAT2021-2022/1-CheckSamplingEffort/SourcesGraph.R")

ggplot(TracksBS, aes(x=month(date),fill=site_name)) + 
  geom_histogram(stat="count")+
  scale_fill_manual(values=c("#4FF09B","#5BEBF0","#67A3F0","#F07F73"))+
  ylab("Nb locs") +
  labs(fill="Month")+
  theme_wsj(base_size=12) +
  ggtitle("GPS Locs")

#Select only may june and July (we don't have enough representation of site outside)
TracksBS<-TracksBS[TracksBS$month %in% c(5:7),]

### IMPORT DATA TO MOVE
BS <- move(x=TracksBS$longitude, y=TracksBS$latitude, time=TracksBS$date, proj=CRS("+init=epsg:4326"), data=TracksBS, animal=TracksBS$track_id)

#Transform it and center it
BS.proj <- spTransform(BS, center=TRUE)
Grid.proj<- projectRaster(Grid,crs = crs(BS.proj))

#burst by month
#BS.burst<-burst(x=BS.proj, f=TracksBS.M$month)

BS.dbbmm<-brownian.bridge.dyn(BS.proj,raster=Grid.proj,location.error=0.01)

plot(BS.dbbmm)
names(BS.dbbmm)

################################################################################
### 7- Weight by colony size (of island group)
################################################################################

#create island group
levels(TracksBS$site_name)
TracksBS$island_group<-factor(ifelse(TracksBS$site_name %in% c("Ibiza"),"Southern",ifelse(TracksBS$site_name %in% c("Cabrera","Mallorca"),"Central",ifelse(TracksBS$site_name %in% c("Menorca"),"Northern",""))))

TracksBS %>% group_by(island_group) %>% summarize(NbTracks=n_distinct(track_id))
#island_group NbTracks
#<fct>           <int>
#1 Central            13
#2 Northern           46
#3 Southern           37

##northern islands = 305 pairs (weight=305/2918), central=1375 p. (weight=1375/2918), southern=1238 p. (weight=1238/2918)

unique(TracksBS[TracksBS$island_group %in% c("Northern"),]$track_id)

#group UD's by island group and convert to relative density 0:1

BS.dbbmm.Northern<-mean(BS.dbbmm[[as.character(unique(TracksBS[TracksBS$island_group %in% c("Northern"),]$track_id))]])

BS.dbbmm.Northern@data@values<-BS.dbbmm.Northern@data@values/max(BS.dbbmm.Northern@data@values)

plot(BS.dbbmm.Northern)

BS.dbbmm.Central<-mean(BS.dbbmm[[as.character(unique(TracksBS[TracksBS$island_group %in% c("Central"),]$track_id))]])

BS.dbbmm.Central@data@values<-BS.dbbmm.Central@data@values/max(BS.dbbmm.Central@data@values)

plot(BS.dbbmm.Central)

BS.dbbmm.Southern<-mean(BS.dbbmm[[as.character(unique(TracksBS[TracksBS$island_group %in% c("Southern"),]$track_id))]])

BS.dbbmm.Southern@data@values<-BS.dbbmm.Southern@data@values/max(BS.dbbmm.Southern@data@values)

plot(BS.dbbmm.Southern)

#sum it with weigth

BS.dbbmm.Northern@data@values<-BS.dbbmm.Northern@data@values*(305/2918)
BS.dbbmm.Central@data@values<-BS.dbbmm.Central@data@values*(1375/2918)
BS.dbbmm.Southern@data@values<-BS.dbbmm.Southern@data@values*(1238/2918)

BS.dbbmm.Weight<-sum(BS.dbbmm.Northern,BS.dbbmm.Central,BS.dbbmm.Southern)
BS.dbbmm.Weight@data@values<-BS.dbbmm.Weight@data@values/max(BS.dbbmm.Weight@data@values)

plot(BS.dbbmm.Weight)

BS.dbbmm.Weight<-projectRaster(BS.dbbmm.Weight,crs = "EPSG:4326")

writeRaster(BS.dbbmm.Weight, filename = "OutputR/BSWeightedUD/UDweightDbbmm.tif", format = "GTiff",overwrite=TRUE)

################################################################################
### 8- Construct UDs grid with adehabitatHR package (i.e. kernel)
################################################################################

################################################################################
### 8.1- For May-June-July

library(adehabitatHR)

rm(list=ls())
setwd("C:/Users/gwenael.quaintenne/OneDrive - LPO/LIFE_PanPuffinus/AnalysisA3")

TracksBS<-readRDS("OutputR/BSCleanInterpTracks/InterpolatedGPSTracksBS.rds")
TracksBS$track_id<-factor(TracksBS$id)

#grid
Grid<-raster(xmn=-5.65, xmx=43.05, ymn=30.55, ymx=47.95, resolution=0.1,crs="EPSG:4326")

#boundary (coastline as barrier that cannot be crossed by the animals UDs)
#Coast <- st_read("InputR/Coastline/CoastlineWMedit_wgs84.shp")

#create island group
levels(TracksBS$site_name)
TracksBS$island_group<-factor(ifelse(TracksBS$site_name %in% c("Ibiza"),"Southern",ifelse(TracksBS$site_name %in% c("Cabrera","Mallorca"),"Central",ifelse(TracksBS$site_name %in% c("Menorca"),"Northern",""))))

TracksBS %>% group_by(island_group) %>% summarize(NbTracks=n_distinct(track_id))
#island_group NbTracks
#<fct>           <int>
#1 Central            13
#2 Northern           46
#3 Southern           55

#select month
TracksBS$month<-lubridate::month(TracksBS$date)

source("C:/Users/gwenael.quaintenne/OneDrive - LPO/Enquete_AnatidesLimicolesNich/DataLIMAT2021-2022/1-CheckSamplingEffort/SourcesGraph.R")

ggplot(TracksBS, aes(x=lubridate::month(date),fill=site_name)) + 
  geom_histogram(stat="count")+
  scale_fill_manual(values=c("#4FF09B","#5BEBF0","#67A3F0","#F07F73"))+
  ylab("Nb locs") +
  labs(fill="Month")+
  theme_wsj(base_size=12)+
  ggtitle("GPS Locs")

TracksBS<-TracksBS[TracksBS$month %in% c(5:7),] #Select only may june and July (we don't have enough representation of site outside)

#Removed locs outside med. (i.e. grid)
TracksBS<-st_as_sf(TracksBS, coords = c("longitude", "latitude"), crs="EPSG:4326")
TracksBS<-st_crop(TracksBS, extent(Grid))
TracksBS$longitude<-st_coordinates(TracksBS)[,1]
TracksBS$latitude<-st_coordinates(TracksBS)[,2]

#check nb locs should be >5 for Kernel
NbLocs<-TracksBS %>% group_by(island_group,track_id) %>% summarise(Nblocs=length(x))
NbLocs[NbLocs$Nblocs<5,] # tracks id "L000861_03" removed as only two locs
TracksBS<-TracksBS[!TracksBS$track_id %in% NbLocs[NbLocs$Nblocs<5,]$track_id,]

summary(TracksBS)

#UDs for Northern island

NI<-as(TracksBS[TracksBS$island_group %in% "Northern",], "Spatial")
NI@data<-droplevels(NI@data)

#K.NI<-kernelUD(NI[, c("track_id")], h=0.2, grid=as(Grid, "SpatialPixels"), boundary=as(Coast, "Spatial"))
#OK forget boudary to complicate as specified segment length should at least be equal to 3*h & the angle between two line segments should be greater that pi/2 or lower that -pi/2...

#for the season
K.NI.season<-kernelUD(NI[, c("track_id")], h=0.2, grid=as(Grid, "SpatialPixels")) #smoothing factor=0.2° around 20km
stk_NI.season <- stack(estUDm2spixdf(K.NI.season))
sum_all_NI.season <- overlay(stk_NI.season, fun = mean)
sum_all_NI.season <- sum_all_NI.season/sum(getValues(sum_all_NI.season))

#for month (forget it as sample tracks don't have good representation of island groups per month)
#K.NI.month<-list()
#stk_NI.month<-list()
#sum_all_NI.month

#for(i in unique(NI$month)) {
#  K.NI.month[[i]]<-kernelUD(NI[NI$month %in% i, c("track_id")], h=0.2, grid=as(Grid, "SpatialPixels"))
#  stk_NI.month[[i]] <- stack(estUDm2spixdf(K.NI.month[[i]]))
#  sum_all_NI.month[[i]] <- overlay(stk_NI.month[[i]], fun = mean)
#  sum_all_NI.month[[i]] <- sum_all_NI.month[[i]]/sum(getValues(sum_all_NI.month[[i]]))
#}

#UDs for Southern island
SI<-as(TracksBS[TracksBS$island_group %in% "Southern",], "Spatial")
SI@data<-droplevels(SI@data)

K.SI.season<-kernelUD(SI[, c("track_id")], h=0.2, grid=as(Grid, "SpatialPixels"))

stk_SI.season <- stack(estUDm2spixdf(K.SI.season))
sum_all_SI.season <- overlay(stk_SI.season, fun = mean)
sum_all_SI.season <- sum_all_SI.season/sum(getValues(sum_all_SI.season))

#UDs for Central island
CI<-as(TracksBS[TracksBS$island_group %in% "Central",], "Spatial")
CI@data<-droplevels(CI@data)

K.CI.season<-kernelUD(CI[, c("track_id")], h=0.2, grid=as(Grid, "SpatialPixels"))

stk_CI.season <- stack(estUDm2spixdf(K.CI.season))
sum_all_CI.season <- overlay(stk_CI.season, fun = mean)
sum_all_CI.season <- sum_all_CI.season/sum(getValues(sum_all_CI.season))

#Compute it with weight

UDweight<-sum_all_NI.season*(305/2918)+sum_all_CI.season*(1375/2918)+sum_all_SI.season*(1238/2918)
UDweight<-UDweight/max(getValues(UDweight))

writeRaster(UDweight, filename = "OutputR/BSWeightedUD/UDweightKernel_BS_May2July.tif", format = "GTiff",overwrite=TRUE)

plot(UDweight)
#we've got perhaps a problem at location close to the colony (buffer of 5 km around colony not enough large???)

################################################################################
### 8.2- For the whole period from March to September

library(adehabitatHR)

rm(list=ls())
setwd("C:/Users/gwenael.quaintenne/OneDrive - LPO/LIFE_PanPuffinus/AnalysisA3")

TracksBS<-readRDS("OutputR/BSCleanInterpTracks/InterpolatedGPSTracksBS.rds")
TracksBS$track_id<-factor(TracksBS$id)

#grid
Grid<-raster(xmn=-5.65, xmx=43.05, ymn=30.55, ymx=47.95, resolution=0.1,crs="EPSG:4326")

#boundary (coastline as barrier that cannot be crossed by the animals UDs)
#Coast <- st_read("InputR/Coastline/CoastlineWMedit_wgs84.shp")

#create island group
levels(TracksBS$site_name)
TracksBS$island_group<-factor(ifelse(TracksBS$site_name %in% c("Ibiza"),"Southern",ifelse(TracksBS$site_name %in% c("Cabrera","Mallorca"),"Central",ifelse(TracksBS$site_name %in% c("Menorca"),"Northern",""))))

TracksBS %>% group_by(island_group) %>% summarize(NbTracks=n_distinct(track_id))
#island_group NbTracks
#<fct>           <int>
#1 Central            13
#2 Northern           46
#3 Southern           55

#select month
TracksBS$month<-lubridate::month(TracksBS$date)

source("C:/Users/gwenael.quaintenne/OneDrive - LPO/Enquete_AnatidesLimicolesNich/DataLIMAT2021-2022/1-CheckSamplingEffort/SourcesGraph.R")

ggplot(TracksBS, aes(x=lubridate::month(date),fill=site_name)) + 
  geom_histogram(stat="count")+
  scale_fill_manual(values=c("#4FF09B","#5BEBF0","#67A3F0","#F07F73"))+
  ylab("Nb locs") +
  labs(fill="Month")+
  theme_wsj(base_size=12)+
  ggtitle("GPS Locs")

#Removed locs outside med. (i.e. grid)
TracksBS<-st_as_sf(TracksBS, coords = c("longitude", "latitude"), crs="EPSG:4326")
TracksBS<-st_crop(TracksBS, extent(Grid))
TracksBS$longitude<-st_coordinates(TracksBS)[,1]
TracksBS$latitude<-st_coordinates(TracksBS)[,2]

#check nb locs should be >5 for Kernel
NbLocs<-TracksBS %>% group_by(island_group,track_id) %>% summarise(Nblocs=length(x))
NbLocs[NbLocs$Nblocs<5,] # tracks id "L000861_03" removed as only two locs
TracksBS<-TracksBS[!TracksBS$track_id %in% NbLocs[NbLocs$Nblocs<5,]$track_id,]

summary(TracksBS)

#UDs for Northern island

NI<-as(TracksBS[TracksBS$island_group %in% "Northern",], "Spatial")
NI@data<-droplevels(NI@data)

#K.NI<-kernelUD(NI[, c("track_id")], h=0.2, grid=as(Grid, "SpatialPixels"), boundary=as(Coast, "Spatial"))
#OK forget boudary to complicate as specified segment length should at least be equal to 3*h & the angle between two line segments should be greater that pi/2 or lower that -pi/2...

#for the season
K.NI.season<-kernelUD(NI[, c("track_id")], h=0.2, grid=as(Grid, "SpatialPixels")) #smoothing factor=0.2° around 20km
stk_NI.season <- stack(estUDm2spixdf(K.NI.season))
sum_all_NI.season <- overlay(stk_NI.season, fun = mean)
sum_all_NI.season <- sum_all_NI.season/sum(getValues(sum_all_NI.season))

#UDs for Southern island
SI<-as(TracksBS[TracksBS$island_group %in% "Southern",], "Spatial")
SI@data<-droplevels(SI@data)

K.SI.season<-kernelUD(SI[, c("track_id")], h=0.2, grid=as(Grid, "SpatialPixels"))

stk_SI.season <- stack(estUDm2spixdf(K.SI.season))
sum_all_SI.season <- overlay(stk_SI.season, fun = mean)
sum_all_SI.season <- sum_all_SI.season/sum(getValues(sum_all_SI.season))

#UDs for Central island
CI<-as(TracksBS[TracksBS$island_group %in% "Central",], "Spatial")
CI@data<-droplevels(CI@data)

K.CI.season<-kernelUD(CI[, c("track_id")], h=0.2, grid=as(Grid, "SpatialPixels"))

stk_CI.season <- stack(estUDm2spixdf(K.CI.season))
sum_all_CI.season <- overlay(stk_CI.season, fun = mean)
sum_all_CI.season <- sum_all_CI.season/sum(getValues(sum_all_CI.season))

#Compute it with weight

UDweight<-sum_all_NI.season*(305/2918)+sum_all_CI.season*(1375/2918)+sum_all_SI.season*(1238/2918)
UDweight<-UDweight/max(getValues(UDweight))

writeRaster(UDweight, filename = "OutputR/BSWeightedUD/UDweightKernel_BS_March2September.tif", format = "GTiff",overwrite=TRUE)

plot(UDweight)


################################################################################
### 9- Compared UDs map with output of Marine Toolkite
################################################################################

rm(list=ls())
setwd("C:/Users/gwenael.quaintenne/OneDrive - LPO/LIFE_PanPuffinus/AnalysisA3")

#UD map classic computation
UDweight<-raster("OutputR/BSWeightedUD/UDweightKernel_BS_May2July.tif")

#UDs maps computed from outputs of Marine toolkite (linear and Crawl)
UDweight.linear<-raster("OutputR/BSWeightedUD/UDweightKernel_BS_May2July_LinearMarineToolkite.tif")
UDweight.Crawl<-raster("OutputR/BSWeightedUD/UDweightKernel_BS_May2July_CrawlMarineToolkite.tif")

par(mfrow=c(1,2))
legend('bottom')
plot(UDweight,main="UD linear interpolation\nBasic computation")
plot(UDweight.Crawl,main="UD Crawl interpolation\nMarine toolkite")
plot(UDweight.linear,main="UD linear interpolation\nMarine toolkite")
plot()

diff<-UDweight-UDweight.Crawl
diff2<-UDweight-UDweight.linear
diff3<-UDweight.Crawl-UDweight.linear
par(mfrow=c(1,1))
plot(diff3, main="UDweight.Crawl-UDweight.linear")


TracksBS<-readRDS("OutputR/BSCleanInterpTracks/InterpolatedGPSTracksBS.rds")
TracksBS$track_id<-factor(TracksBS$id)
TracksBSlinear<-readRDS("OutputR/BSCleanInterpTracks/LinearInterpolatedGPSTracksBS_MarineToolkite.rds")
TracksBScrawl<-readRDS("OutputR/BSCleanInterpTracks/CrawlInterpolatedGPSTracksBS_MarineToolkite.rds")

TracksBS<-TracksBS[month(TracksBS$date) %in% c(5:7),]
TracksBScrawl<-TracksBScrawl[month(TracksBScrawl$date) %in% c(5:7),]

TracksBS<-TracksBS[TracksBS$track_id %in% TracksBScrawl$track_id,]

length(unique(TracksBS$track_id)) #95
length(unique(TracksBSlinear$track_id)) #93
length(unique(TracksBScrawl$track_id)) #93


leaflet() %>% ## start leaflet plot
  addProviderTiles(providers$Esri.WorldImagery, group = "World Imagery") %>%
  ## Predicted 
  addCircleMarkers(data = TracksBS,
                   radius = 5,
                   fillColor = "#4FF09B",
                   fillOpacity = 0.5, stroke = F) %>%
  addCircleMarkers(data = TracksBSlinear,
                   radius = 5,
                   fillColor = "#F07F73",
                   fillOpacity = 0.5, stroke = F) %>%
  addCircleMarkers(data = TracksBScrawl,
                   radius = 5,
                   fillColor = "#5BEBF0",
                   fillOpacity = 0.5, stroke = F) %>%
  addLegend(colors = c("#4FF09B","#5BEBF0","#F07F73"),
            labels = c("Classic computation","Crawl MTLK","linear MTLK"),)

leaflet() %>% ## start leaflet plot
  addProviderTiles(providers$Esri.WorldImagery, group = "World Imagery") %>%
  ## Predicted 
  addCircleMarkers(data = TracksBS,
                   radius = 8,
                   fillColor = "#4FF09B",
                   fillOpacity = 0.5, stroke = F) %>%
  addCircleMarkers(data = TracksBSlinear,
                   radius = 5,
                   fillColor = "#F07F73",
                   fillOpacity = 0.5, stroke = F) %>%
  addCircleMarkers(data = TracksBScrawl,
                   radius = 5,
                   fillColor = "#5BEBF0",
                   fillOpacity = 0.5, stroke = F) %>%
  addLegend(colors = c("#4FF09B","#5BEBF0","#F07F73"),
            labels = c("Classic computation","Crawl MTLK","linear MTLK"),)

################################################################################
### 10- Plots UDs map
################################################################################

UDweight<-raster("OutputR/BSWeightedUD/UDweightKernel_BS_May2July.tif")
values(UDweight)[values(UDweight)== 0] = NA

par(mfrow=c(1,1))

plot(UDweight,
     main = "Balearic Shearwater Seasonal distribution map (May-July)",
     box = FALSE,
     colNA="white")
# add coastlines
plot(st_geometry(land),col="grey",border="white",
     add = TRUE,
     ext = extent(VMS.season.raster),
     axes = TRUE)

