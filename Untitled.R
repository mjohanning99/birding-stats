install.packages("ggmap")
install.packages("sf")
install.packages("ggrepel")

library("ggmap")
library(tidyverse)
library(sf)
library(mapview)
library(dplyr)
library(ggrepel)


register_google(key = "[AIzaSyDTykAdRVF2lO-7JKsLLZwoP534vRqyqI4]")

#myLocation<-c(8.50, 52.04, 8.61, 52)
#myLocation<-c(7, 47.75, 8.5, 48.25)
myLocation<-c(8.50, 52, 8.61, 52.04)
bw_map <- get_googlemap("bielefeld stauteich III", zoom = 14, maptype = "satellite")

setwd("/Users/mjo-air/Programming/R")

locations <- read.csv("ornitho.txt", sep = "\t")[,c('COORD_LAT', 'COORD_LON', 'TOTAL_COUNT', 'NAME_SPECIES', 'PLACE')]
locations <- locations %>% slice(-1)
print(locations)

locations <- read.csv("birding.csv", stringsAsFactors = F, na.strings="`")[,c('Location.1', 'Longitude', 'Latitude', 'Number', 'Common')]

locations <- read.csv("birding.csv", stringsAsFactors = F, na.strings="`")[,c('Location.1', 'Longitude', 'Latitude', 'Common')]


locations$Number<-gsub(">","",as.character(locations$Number))
locations$Number<-gsub("~","",as.character(locations$Number))

locations <- locations %>% mutate_at(4, ~replace_na(.,0))

locs <- as_tibble(locations)

locs <- locs %>% mutate_at(4, ~replace_na(.,0))

print(locs)

locs$COORD_LON %<>% as.double
locs$COORD_LAT %<>% as.double
locs$TOTAL_COUNT %<>% as.integer

locs$Number %<>% as.integer


locations_sf <- st_as_sf(locs, coords = c("COORD_LON", "COORD_LAT"), crs = 4326)

locations_sf <- st_as_sf(per_location, coords = c("Longitude", "Latitude"), crs = 4326)

mapview(locations_sf, zcol="sum")

per_location_no_dup <- locs[!duplicated(locs$Location.1),]

per_location_no_dup <- unique(locs)

per_location <- locs %>% 
  group_by(Location.1, Longitude, Latitude) %>% 
  summarise(sum = n()) %>% 
  arrange(desc(sum))

per_location <- per_location_no_dup %>% 
  group_by(Location.1, Longitude, Latitude) %>% 
  summarise(sum = n()) %>% 
  arrange(desc(sum))

per_location <- locs %>% 
  group_by(Location.1, Longitude, Latitude) %>% 
  summarise(sum = sum(Number)) %>% 
  arrange(desc(sum))

per_location[!duplicated(per_location$Common),]

print(per_location)

print(per_location_no_dup)

ggmap(bw_map) +
  geom_point(data = per_location,
             aes(x = Longitude, y = Latitude, size=sum),
             color = "red", alpha = 0.5) +
  geom_text_repel(data = per_location, aes(x = Longitude, y = Latitude, label = Location.1)) +
  guides(color = guide_legend(override.aes = list(size = 6))) +
  scale_size_continuous(range = c(2, 9))

barplot(height=per_location$sum, names=per_location$Location.1, las=2)

barplot(data$average , border=F , names.arg=data$name , 
                  las=2 , 
                  col=c(rgb(0.3,0.1,0.4,0.6) , rgb(0.3,0.5,0.4,0.6) , rgb(0.3,0.9,0.4,0.6) ,  rgb(0.3,0.9,0.4,0.6)) , 
                  ylim=c(0,13) , 
                  main="" )

ggmap(bw_map) +
  geom_point(data = locs, aes(x = lon, y = lat))

lon <- distinct(data, Longitude)
lat <- distinct(data, Latitude)

locs %>% 
  group_by(Location.1) %>% 
  summarise(across(everything(), sum))

mapview(c(lon, lat))

locations_sf <- st_as_sf(locations, coords = c("lon", "lat"), crs = 4326)

per_source <- locs %>% 
  group_by(TOTAL_COUNT) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))
print(per_source)

print(data)

retval <- subset(data, Location.2 == "Heeper Fichten")
print(retval$Common)

