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
d.counties %>% 
  ggplot(., aes(x = ALAND10, y = ALAND10)) +
  geom_boxplot(aes(fill = STATEFP10))

#spatial operations
#look at CRS
d.counties %>% sf::st_crs()
d.stations %>% sf::st_crs()
d.counties %>% sf::st_crs() == d.stations %>% sf::st_crs()

#only counties in delaware
del.counties <- d.counties %>% dplyr::filter(STATEFP10==10)
del.stations <- sf::st_intersection(d.stations,del.counties)
glimpse(del.stations)


#Task One!!
#1.1
d.counties <- d.counties %>% dplyr::group_by(STATEFP10) %>% mutate(stateArea = sum(ALAND10+AWATER10))
d.counties <- d.counties %>% mutate(LandAreaPrecent = (ALAND10/stateArea)*100)

#1.2
d.counties <- d.counties %>%
  mutate(CountyArea = (ALAND10+AWATER10))
d.counties <- d.counties %>%
  mutate(WaterProp = (AWATER10/CountyArea))
Max.Water <- d.counties %>%
  dplyr::group_by(STATEFP10) %>%
  slice_max(WaterProp)

#1.3
d.counties %>%
  dplyr::group_by(STATEFP10) %>%
  dplyr::count(STATEFP10)

#1.4
d.stations <- d.stations %>% mutate(length = (nchar(STATION_NA)))
d.stations %>% slice_min(length)





