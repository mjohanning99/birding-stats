library("ggmap")
library(tidyverse)
library(sf)
library(mapview)
library(dplyr)
library(ggrepel)

#Set working directory
setwd("/Users/mjo-air/Programming/R")

#Choose Scythebird export
locations <- read.csv(file=file.choose(), stringsAsFactors = F)[,c('Location.1', 'Common', 'Number')]

#Remove approximate, bigger than etc. signs
locations$Number<-gsub(">","",as.character(locations$Number))
locations$Number<-gsub("~","",as.character(locations$Number))



#Do a tibblings
locations <- as_tibble(locations)

#Change data types from character to double and integer
locations$Number %<>% as.integer

#Total number of birds by species per location
per_location_by_species <- locations %>% 
  group_by(Common, Location.1) %>% 
  summarise(sum = sum(Number)) %>% 
  arrange(desc(sum))

per_location_by_species <- subset(per_location_by_species, Common!="Stockente")

#Bar plot
ggplot(per_location_by_species, aes(sum, Common, fill = Location.1)) +
  geom_bar(stat="identity", position = "dodge") +
  labs(title="Number of birds per location")

print(per_location_by_species)

# Create mapview map
locations_sf <- st_as_sf(per_location, coords = c("COORD_LON", "COORD_LAT"), crs = 4326)
mapview(locations_sf, zcol="sum")

print(locations)


