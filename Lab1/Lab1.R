#load packages and data
library(tidyverse)
library(ggplot2)
library(sf)

p.counties <- "./data/County_Boundaries.shp"
p.stations <- "./data/Non-Tidal_Water_Quality_Monitoring_Stations_in_the_Chesapeake_Bay.shp"

d.counties <- sf::read_sf(p.counties)
d.stations <- sf::read_sf(p.stations)

#check validity
d.stations %>% sf::st_is_valid()
d.counties %>% sf::st_is_valid()
d.counties <- d.counties %>% sf::st_make_valid()
d.counties %>% dplyr::select(GEOID10, ALAND10) %>% head()
d.counties %>% dplyr::select(-NAME10) %>% head()
#only shows the columns specified
d.counties %>% dplyr::select(GEOID10:CLASSFP10) %>% head()
#shows all columns except for the ones in this range
d.counties %>% dplyr::select(-(GEOID10:CLASSFP10)) %>% head()
#shows columns that have headings that start with C
d.counties %>% dplyr::select(starts_with("C")) %>% head()

#Grouping Stuff
d.counties %>% dplyr::group_by(STATEFP10) %>% mutate(stateLandArea = sum(ALAND10))

#convert to tibble and get the stuff we want
d.counties %>%
  as_tibble() %>% dplyr::select(-geometry) %>%
  group_by(STATEFP10) %>%
  summarise(stateLandArea = sum(ALAND10))

#plots:p

