library("ggmap")
library(tidyverse)
library(sf)
library(mapview)
library(dplyr)
library(ggrepel)
library(magrittr)

#Google API key
register_google(key = "")

#Google Maps view
stauteich_3 <- get_googlemap("bielefeld stauteich III", zoom = 14, maptype = "satellite")

#Set working directory
setwd("/Users/mjo-air/Programming/R")

#Choose Ornihto export
locations <- read.csv(file=file.choose(), sep = "\t")[,c('COORD_LAT', 'COORD_LON', 'TOTAL_COUNT', 'NAME_SPECIES', 'PLACE')]

#Remove first row
locations <- locations %>% slice(-1)

#Do a tibblings
locations <- as_tibble(locations)

#Change data types from character to double and integer
locations$COORD_LON %<>% as.double
locations$COORD_LAT %<>% as.double
locations$TOTAL_COUNT %<>% as.integer

#Total number of birds per location
#per_location <- locations %>% 
 # group_by(PLACE, COORD_LAT, COORD_LON) %>% 
  #summarise(sum = sum(TOTAL_COUNT)) %>% 
  #arrange(desc(sum))

#Total number of birds if Psalm is stupid
per_location <- locations %>%
  group_by(PLACE) %>%
  mutate(across(starts_with("COORD_"), last)) %>%
  ungroup()

per_location <- per_location %>% 
  group_by(PLACE, COORD_LAT, COORD_LON) %>% 
  summarise(sum = sum(TOTAL_COUNT)) %>% 
  arrange(desc(sum))


#Remove text in brackets
per_location$PLACE <- gsub("\\s*\\[[^\\)]+\\]","",as.character(per_location$PLACE))

#Total number of birds by species per location
per_location_by_species <- locations %>% 
  group_by(NAME_SPECIES, PLACE) %>% 
  summarise(sum = sum(TOTAL_COUNT)) %>% 
  arrange(desc(sum))

#HYPERTEST
per_location_by_species <- locations %>% 
  group_by(NAME_SPECIES, PLACE) %>% 
  summarise(sum = sum(TOTAL_COUNT)) %>% 
  arrange(desc(sum))

#Bar plot
ggplot(per_location_by_species, aes(sum, NAME_SPECIES, fill = PLACE)) +
  geom_bar(stat="identity", position = "dodge") +
  labs(title="Number of birds per location")

print(per_location_by_species)

# Create mapview map
locations_sf <- st_as_sf(per_location, coords = c("COORD_LON", "COORD_LAT"), crs = 4326)
mapview(locations_sf, zcol="sum")

# Create Google Maps map
ggmap(stauteich_3) +
  geom_point(data = per_location,
             aes(x = COORD_LON, y = COORD_LAT, size=sum),
             color = "red", alpha = 0.5) +
  geom_text_repel(data = per_location, aes(x = COORD_LON, y = COORD_LAT, label = PLACE)) +
  guides(color = guide_legend(override.aes = list(size = 1))) +
  scale_size_continuous(range = c(1, 15))


print(per_location_by_species) %>% print(n=40)


