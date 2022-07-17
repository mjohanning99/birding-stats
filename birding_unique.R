library("ggmap")
library(tidyverse)
library(sf)
library(mapview)
library(dplyr)
library(ggrepel)
library(grid)
library(tidyverse)
library(shadowtext)

#Working directory
setwd("/Users/mjo-air/Programming/R")

#Colours
BLUE <- "#076fa2"

#API key for Google Maps
register_google(key = "")

#Map of Bielefeld, centered on Stauteich 3
stauteich_3 <- get_googlemap("bielefeld stauteich III", zoom = 14, maptype = "satellite")

#The variable used to get the sum of all birds that were seen at a particular location
#all_birds_sum_var <- read.csv("birding.csv", stringsAsFactors = F, na.strings="`")[,c('Location.1', 'Longitude', 'Latitude', 'Number', 'Common')]

#The variable used to get the sum of unique birds that were seen at a particular location
unique_birds_sum_var <- read.csv("birding.csv", stringsAsFactors = F, na.strings="`")[,c('Location.1', 'Longitude', 'Latitude', 'Common')]

#Convert to tibble
locations <- as_tibble(unique_birds_sum_var)

#Find how many unique sightings there are in this location
per_location_unique <- unique(locations)

#Do some grouping
per_location <- per_location_unique %>% 
  group_by(Location.1, Longitude, Latitude) %>% 
  summarise(sum = n()) %>% 
  arrange(desc(sum))

#Show table
print(per_location)

#Do some converterinos
locations_sf <- st_as_sf(per_location, coords = c("Longitude", "Latitude"), crs = 4326)

#Show as HTML map
mapview(locations_sf, zcol="sum")

#Create barplot
barplot(height=per_location$sum, names=per_location$Location.1, las=2)

#Create hyperplot
ggplot(per_location) +
  geom_col(aes(sum, Location.1), fill = BLUE, width = 0.6)

#Google Maps map
ggmap(bw_map) +
geom_point(data = per_location,
           aes(x = Longitude, y = Latitude, size=sum),
           color = "red", alpha = 0.5) +
  geom_text_repel(data = per_location, aes(x = Longitude, y = Latitude, label = Location.1)) +
  guides(color = guide_legend(override.aes = list(size = 1))) +
  scale_size_continuous(range = c(1, 15))

