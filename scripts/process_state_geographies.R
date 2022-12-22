library(tidyverse)
library(tidycensus)
library(leaflet)
library(leaflet.providers)
# library(tidyr)
library(sf)

# Get demographic data and geography for Census places
cal_places <- get_decennial(geography = "place", 
                            year = 2020,
                            output = 'wide',
                            variables = "P1_001N", 
                            state = "CA",
                            geometry = TRUE) %>%
  rename("population"="P1_001N") %>%
  janitor::clean_names()

# Get demographic data and geography for Census places
cal_counties <- get_decennial(geography = "county", 
                              year = 2020,
                              output = 'wide',
                              variables = "P1_001N", 
                              state = "CA",
                              geometry = TRUE) %>%
  rename("population"="P1_001N") %>%
  janitor::clean_names()

# Some adjustments to the fields in cal_places to merge to crime data
cal_places$place <- cal_places$name
cal_places$place <- str_replace(cal_places$place," CDP, California","")
cal_places$place <- str_replace(cal_places$place," city, California","")
cal_places$place <- str_replace(cal_places$place," town, California","")
cal_places$place <- str_replace(cal_places$place,"Big Bear City","Big Bear")

cal_places$place <- str_replace(cal_places$place,"La Cañada Flintridge","La Canada-Flintridge")
cal_places$place <- sub(" CDP.*", "\\1", cal_places$place)
cal_places$place <- ifelse(cal_places$geoid=="0665042","Ventura",cal_places$place)

cal_places$state <- "California"

# Some adjustments to the fields in cal_counties to merge to crime data
cal_counties$county <- cal_counties$name
cal_counties$county <- str_replace(cal_counties$county,", California","")

# Assign county names for filter and temporarily import Cal DOJ list for filter
counties <- c("Fresno County", "Kings County","Madera County","Mariposa County","Merced County","Tulare County")

valley_annual <- readRDS("scripts/rds/valley_annual.rds")

# Apply filters to assign places to counties, filter for counties and Cal DOJ agencies
valley_places <- st_join(cal_places, cal_counties %>% select(5), left = FALSE, largest = TRUE) %>%
  filter(county %in% counties) %>% filter(place %in% valley_annual$ncic_code)
# Round population to estimate, nearest hundred, consistent with LA Districts
valley_places$population <- round(valley_places$population,-2)

# Creating a singular file for making rural cutouts by county
# make sure to make the resulting file a valid sf file
all_valley_places <- valley_places %>%
  group_by(state) %>%
  summarise(geometry = sf::st_union(geometry)) %>%
  ungroup() %>% st_make_valid()

# Create five single polygon county files
fresno_county <- cal_counties %>% filter(county=="Fresno County") %>% st_make_valid()
kings_county <- cal_counties %>% filter(county=="Kings County") %>% st_make_valid()
madera_county <- cal_counties %>% filter(county=="Madera County") %>% st_make_valid()
mariposa_county <- cal_counties %>% filter(county=="Mariposa County") %>% st_make_valid()
merced_county <- cal_counties %>% filter(county=="Merced County") %>% st_make_valid()
tulare_county <- cal_counties %>% filter(county=="Tulare County") %>% st_make_valid()

# Make the rural "remnant" area polygons for each county
rural_fresno <- st_difference(fresno_county,all_valley_places)
rural_kings <- st_difference(kings_county,all_valley_places)
rural_madera <- st_difference(madera_county,all_valley_places)
rural_mariposa <- st_difference(mariposa_county,all_valley_places)
rural_merced <- st_difference(merced_county,all_valley_places)
rural_tulare <- st_difference(tulare_county,all_valley_places)

# Make the rural "remnant" area polygons for each county
rural_fresno$place <- "Fresno Co. Sheriff's Department"
rural_kings$place <- "Kings Co. Sheriff's Department"
rural_madera$place <- "Madera Co. Sheriff's Department"
rural_mariposa$place <- "Mariposa Co. Sheriff's Department"
rural_merced$place <- "Merced Co. Sheriff's Department"
rural_tulare$place <- "Tulare Co. Sheriff's Department"

# Make the rural "remnant" area polygons for each county
valley_county_pops <- valley_places %>% group_by(county) %>% summarise(pop=sum(population))
rural_fresno$population <- fresno_county$population - 842500
rural_kings$population <- kings_county$population - 121000
rural_madera$population <- madera_county$population - 85200
rural_mariposa$population <- mariposa_county$population - 0
rural_merced$population <- merced_county$population - 189900
rural_tulare$population <- tulare_county$population - 338300

# Add these rural sheriff's coverage areas back into socal_places
valley_places <- rbind(valley_places,rural_fresno,rural_kings,rural_madera,rural_mariposa,rural_merced,rural_tulare)

# Do some cleanup 
rm(rural_fresno,rural_kings,rural_madera,rural_mariposa,rural_merced,rural_tulare)
rm(fresno_county,kings_county,madera_county,mariposa_county,merced_county,tulare_county)
rm(cal_counties, cal_places, all_valley_places,valley_county_pops)

# Create new police_map
# la_districts <- readRDS(data/rds/la_county_police_districts.rds)
# Remove all LA County places from the so_cal places now
# police_map <- bind_rows(socal_places %>% filter(county!="Los Angeles County"),la_districts)
# Add upgraded LA County districts to the new region wide police districts map
# police_map$county <- ifelse(is.na(police_map$county),"Los Angeles County",police_map$county)
# Recode place field for all the LA county policing districts
# police_map$place <- case_when(police_map$agency=="OTHER" & police_map$commtype=="City" ~ police_map$place_name,
#                              police_map$agency=="LASD" & police_map$commtype=="City" ~ police_map$place_name,
#                              police_map$agency=="LASD" & police_map$commtype=="Unincorporated" ~ paste(police_map$place_name, police_map$district),
#                              police_map$agency=="LAPD" ~ paste(police_map$agency, police_map$district),
#                              TRUE ~ police_map$place)



# Set bins for beats pop map
popbins <- c(0,15000,25000,50000,70000,80000,90000,100000,110000,125000,200000,Inf)
poppal <- colorBin("viridis", valley_places$population, bins = popbins)
poplabel <- paste(sep = "<br>", valley_places$place,valley_places$county,valley_places$agency,valley_places$commtype,prettyNum(valley_places$population, big.mark = ","))
# Create map
valley_police_map <- leaflet(valley_places) %>%
  setView(-119.78, 36.737, zoom = 9.5) %>% 
  addProviderTiles(provider = "Esri.WorldImagery") %>%
  addProviderTiles(provider = "CartoDB.PositronOnlyLabels") %>%
  addPolygons(color = "white", popup = poplabel, weight = 2, smoothFactor = 0.5,
              opacity = 0.5, fillOpacity = 0.5,
              fillColor = ~poppal(`population`)) 
valley_police_map 

# Saving final product for reuse; police map includes full region
saveRDS(valley_places,"scripts/rds/police_map.rds")

# rm(valley_police_map)
