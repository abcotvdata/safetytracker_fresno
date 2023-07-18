library(tidyverse)
library(sf)
library(tidyr)

# May need to load the right map file here
# valley_places <- readRDS("scripts/rds/valley_places.rds")

# Read in the state crime file
california_annual <- read_csv("data/source/california_crime_annual_2022.csv", 
                                    col_types = cols(Year = col_character())) %>% janitor::clean_names()

# list of region's counties
counties <- c("Fresno County", "Kings County","Madera County","Mariposa County","Merced County","Tulare County")
sheriffs <- c("Fresno Co. Sheriff's Department", "Kings Co. Sheriff's Department",
              "Madera Co. Sheriff's Department","Mariposa Co. Sheriff's Department",
              "Merced Co. Sheriff's Department","Tulare Co. Sheriff's Department")

# Filter for crime incident counts in jurisdictions in our five counties 
valley_annual <- california_annual %>% filter(county %in% counties)

# Export to rds store so we always have for geography processing
saveRDS(valley_annual,"scripts/rds/valley_annual.rds")

# Create a baseline count file for so cal agencies, by crime category, for charts
valley_murder <- valley_annual %>% select(year,county,ncic_code,homicide_sum) %>% spread(year,homicide_sum) %>% select(1,2,28:40) %>% rename("place"="ncic_code")
valley_sexassault <- valley_annual %>% select(year,county,ncic_code,for_rape_sum) %>% spread(year,for_rape_sum) %>% select(1,2,28:40) %>% rename("place"="ncic_code")
valley_assault <- valley_annual %>% select(year,county,ncic_code,agg_assault_sum) %>% spread(year,agg_assault_sum) %>% select(1,2,28:40) %>% rename("place"="ncic_code")
valley_robbery <- valley_annual %>% select(year,county,ncic_code,robbery_sum) %>% spread(year,robbery_sum) %>% select(1,2,28:40) %>% rename("place"="ncic_code")
valley_burglary <- valley_annual %>% select(year,county,ncic_code,burglary_sum) %>% spread(year,burglary_sum) %>% select(1,2,28:40) %>% rename("place"="ncic_code")
valley_theft <- valley_annual %>% select(year,county,ncic_code,l_ttotal_sum) %>% spread(year,l_ttotal_sum) %>% select(1,2,28:40) %>% rename("place"="ncic_code")
valley_autotheft <- valley_annual %>% select(year,county,ncic_code,vehicle_theft_sum) %>% spread(year,vehicle_theft_sum) %>% select(1,2,28:40) %>% rename("place"="ncic_code")

# Before we start to make changes for maps we want to create countywide totals for charts/tracker text
countywide_murder <- valley_murder %>% group_by(county) %>% summarise(total22=sum(`2022`,na.rm=TRUE),total19=sum(`2019`,na.rm=TRUE),total10=sum(`2010`,na.rm=TRUE))
countywide_sexassault <- valley_sexassault %>% group_by(county) %>% summarise(total22=sum(`2022`,na.rm=TRUE),total19=sum(`2019`,na.rm=TRUE),total10=sum(`2010`,na.rm=TRUE))
countywide_assault <- valley_assault %>% group_by(county) %>% summarise(total22=sum(`2022`,na.rm=TRUE),total19=sum(`2019`,na.rm=TRUE),total10=sum(`2010`,na.rm=TRUE))
countywide_robbery <- valley_robbery %>% group_by(county) %>% summarise(total22=sum(`2022`,na.rm=TRUE),total19=sum(`2019`,na.rm=TRUE),total10=sum(`2010`,na.rm=TRUE))
countywide_burglary <- valley_burglary %>% group_by(county) %>% summarise(total22=sum(`2022`,na.rm=TRUE),total19=sum(`2019`,na.rm=TRUE),total10=sum(`2010`,na.rm=TRUE))
countywide_theft <- valley_theft %>% group_by(county) %>% summarise(total22=sum(`2022`,na.rm=TRUE),total19=sum(`2019`,na.rm=TRUE),total10=sum(`2010`,na.rm=TRUE))
countywide_autotheft <- valley_autotheft %>% group_by(county) %>% summarise(total22=sum(`2022`,na.rm=TRUE),total19=sum(`2019`,na.rm=TRUE),total10=sum(`2010`,na.rm=TRUE))

# sheriff_contracts <- c("Avalon","Carson","Lynwood","Cerritos","Compton",
#                       "La Canada Flintridge","Commerce","Cudahy","Maywood",
#                       "Industry","La Habra Heights","La Puente","Artesia",
#                       "Bellflower","Hawaiian Gardens","Lakewood",
#                       "Paramount","Lancaster","Lomita","Rancho Palos Verdes",
#                       "Rolling Hills","Rolling Hills Estates","Agoura Hills",
#                       "Calabasas","Hidden Hills","Malibu","Westlake Village",
#                       "La Mirada","Norwalk","Palmdale","Pico Rivera",
#                       "San Dimas","Santa Clarita","Lawndale","Bradbury",
#                       "Duarte","Rosemead","South El Monte","Temple City",
#                       "Diamond Bar","Walnut","West Hollywood")

## Add in LAPD districts and LASD unincorporated districts 
#valley_murder <- bind_rows(valley_murder %>% filter(!place %in% sheriff_contracts),lapd_murder,lasheriff_murder)
#valley_sexassault <- bind_rows(valley_sexassault %>% filter(!place %in% sheriff_contracts),lapd_sexassault,lasheriff_sexassault)
#valley_assault <- bind_rows(valley_assault %>% filter(!place %in% sheriff_contracts),lapd_assault,lasheriff_assault)
#valley_robbery <- bind_rows(valley_robbery %>% filter(!place %in% sheriff_contracts),lapd_robbery,lasheriff_robbery)
#valley_burglary <- bind_rows(valley_burglary %>% filter(!place %in% sheriff_contracts),lapd_burglary,lasheriff_burglary)
#valley_theft <- bind_rows(valley_theft %>% filter(!place %in% sheriff_contracts),lapd_theft,lasheriff_theft)
#valley_autotheft <- bind_rows(valley_autotheft %>% filter(!place %in% sheriff_contracts),lapd_autotheft,lasheriff_autotheft)

# make quick tables that we can use for a quick simple map to be improved later
valley_murder <- inner_join(valley_places %>% select(3:5),valley_murder,by="place")
valley_sexassault <- inner_join(valley_places %>% select(3:5),valley_sexassault,by="place")
valley_assault <- inner_join(valley_places %>% select(3:5),valley_assault,by="place")
valley_robbery <- inner_join(valley_places %>% select(3:5),valley_robbery,by="place")
valley_burglary <- inner_join(valley_places %>% select(3:5),valley_burglary,by="place")
valley_theft <- inner_join(valley_places %>% select(3:5),valley_theft,by="place")
valley_autotheft <- inner_join(valley_places %>% select(3:5),valley_autotheft,by="place")


# MURDERS
# By Place add change columns for maps
# add 3-year totals and annualized average over three years
valley_murder$total_prior4years <- valley_murder$`2019` + valley_murder$`2020` + valley_murder$`2021` + valley_murder$`2022`
valley_murder$avg_prior4years <- round((valley_murder$total_prior4years/4),1)
# now add the increases or change percentages vs prepandemic vs last decade
valley_murder$inc_19to22 <- round(valley_murder$`2022`/valley_murder$`2019`*100-100,1)
valley_murder$inc_10to22 <- round(valley_murder$`2022`/valley_murder$`2010`*100-100,1)
# add crime rates for each year
valley_murder$rate19 <- round((valley_murder$`2019`/valley_murder$population)*100000,1)
valley_murder$rate20 <- round((valley_murder$`2020`/valley_murder$population)*100000,1)
valley_murder$rate21 <- round((valley_murder$`2021`/valley_murder$population)*100000,1)
valley_murder$rate22 <- round((valley_murder$`2022`/valley_murder$population)*100000,1)
valley_murder$rate_prior4years <- round((valley_murder$avg_prior4years/valley_murder$population)*100000,1)
# for map/table making purposes, changing Inf and NaN in calc fields to NA
valley_murder <- valley_murder %>%
  mutate_if(is.numeric, ~ifelse(. == Inf, NA, .))
valley_murder <- valley_murder %>%
  mutate_if(is.numeric, ~ifelse(. == "NaN", NA, .))
# elimate rates for districts with fewer than 1,000 estimated population
valley_murder$rate19 <- ifelse(valley_murder$population<1000,NA,valley_murder$rate19)
valley_murder$rate20 <- ifelse(valley_murder$population<1000,NA,valley_murder$rate20)
valley_murder$rate21 <- ifelse(valley_murder$population<1000,NA,valley_murder$rate21)
valley_murder$rate22 <- ifelse(valley_murder$population<1000,NA,valley_murder$rate22)
valley_murder$rate_prior4years <- ifelse(valley_murder$population<1000,NA,valley_murder$rate_prior4years)

# SEXUAL ASSAULTS
# By Place add change columns for maps
# add 3-year totals and annualized average over three years
valley_sexassault$total_prior4years <- valley_sexassault$`2019` + valley_sexassault$`2020` + valley_sexassault$`2021` + valley_sexassault$`2022`
valley_sexassault$avg_prior4years <- round((valley_sexassault$total_prior4years/4),1)
# now add the increases or change percentages vs prepandemic vs last decade
valley_sexassault$inc_19to22 <- round(valley_sexassault$`2022`/valley_sexassault$`2019`*100-100,1)
valley_sexassault$inc_10to22 <- round(valley_sexassault$`2022`/valley_sexassault$`2010`*100-100,1)
# add crime rates for each year
valley_sexassault$rate19 <- round((valley_sexassault$`2019`/valley_sexassault$population)*100000,1)
valley_sexassault$rate20 <- round((valley_sexassault$`2020`/valley_sexassault$population)*100000,1)
valley_sexassault$rate21 <- round((valley_sexassault$`2021`/valley_sexassault$population)*100000,1)
valley_sexassault$rate22 <- round((valley_sexassault$`2022`/valley_sexassault$population)*100000,1)
valley_sexassault$rate_prior4years <-round((valley_sexassault$avg_prior4years/valley_sexassault$population)*100000,1)
# for map/table making purposes, changing Inf and NaN in calc fields to NA
valley_sexassault <- valley_sexassault %>%
  mutate_if(is.numeric, ~ifelse(. == Inf, NA, .))
valley_sexassault <- valley_sexassault %>%
  mutate_if(is.numeric, ~ifelse(. == "NaN", NA, .))
# elimate rates for districts with fewer than 1,000 estimated population
valley_sexassault$rate19 <- ifelse(valley_sexassault$population<1000,NA,valley_sexassault$rate19)
valley_sexassault$rate20 <- ifelse(valley_sexassault$population<1000,NA,valley_sexassault$rate20)
valley_sexassault$rate21 <- ifelse(valley_sexassault$population<1000,NA,valley_sexassault$rate21)
valley_sexassault$rate22 <- ifelse(valley_sexassault$population<1000,NA,valley_sexassault$rate22)
valley_sexassault$rate_prior4years <- ifelse(valley_sexassault$population<1000,NA,valley_sexassault$rate_prior4years)

# ROBBERIES
# By Place add change columns for maps
# add 3-year totals and annualized average over three years
valley_robbery$total_prior4years <- valley_robbery$`2019` + valley_robbery$`2020` + valley_robbery$`2021` + valley_robbery$`2022`
valley_robbery$avg_prior4years <- round((valley_robbery$total_prior4years/4),1)
# now add the increases or change percentages vs prepandemic vs last decade
valley_robbery$inc_19to22 <- round(valley_robbery$`2022`/valley_robbery$`2019`*100-100,1)
valley_robbery$inc_10to22 <- round(valley_robbery$`2022`/valley_robbery$`2010`*100-100,1)
# add crime rates for each year
valley_robbery$rate19 <- round((valley_robbery$`2019`/valley_robbery$population)*100000,1)
valley_robbery$rate20 <- round((valley_robbery$`2020`/valley_robbery$population)*100000,1)
valley_robbery$rate21 <- round((valley_robbery$`2021`/valley_robbery$population)*100000,1)
valley_robbery$rate22 <- round((valley_robbery$`2022`/valley_robbery$population)*100000,1)
valley_robbery$rate_prior4years <-round((valley_robbery$avg_prior4years/valley_robbery$population)*100000,1)
# for map/table making purposes, changing Inf and NaN in calc fields to NA
valley_robbery <- valley_robbery %>%
  mutate_if(is.numeric, ~ifelse(. == Inf, NA, .))
valley_robbery <- valley_robbery %>%
  mutate_if(is.numeric, ~ifelse(. == "NaN", NA, .))
# elimate rates for districts with fewer than 1,000 estimated population
valley_robbery$rate19 <- ifelse(valley_robbery$population<1000,NA,valley_robbery$rate19)
valley_robbery$rate20 <- ifelse(valley_robbery$population<1000,NA,valley_robbery$rate20)
valley_robbery$rate21 <- ifelse(valley_robbery$population<1000,NA,valley_robbery$rate21)
valley_robbery$rate22 <- ifelse(valley_robbery$population<1000,NA,valley_robbery$rate22)
valley_robbery$rate_prior4years <- ifelse(valley_robbery$population<1000,NA,valley_robbery$rate_prior4years)

# ASSAULTS
# By Place add change columns for maps
# add 3-year totals and annualized average over three years
valley_assault$total_prior4years <- valley_assault$`2019` + valley_assault$`2020` + valley_assault$`2021` + valley_assault$`2022`
valley_assault$avg_prior4years <- round((valley_assault$total_prior4years/4),1)
# now add the increases or change percentages vs prepandemic vs last decade
valley_assault$inc_19to22 <- round(valley_assault$`2022`/valley_assault$`2019`*100-100,1)
valley_assault$inc_10to22 <- round(valley_assault$`2022`/valley_assault$`2010`*100-100,1)
# add crime rates for each year
valley_assault$rate19 <- round((valley_assault$`2019`/valley_assault$population)*100000,1)
valley_assault$rate20 <- round((valley_assault$`2020`/valley_assault$population)*100000,1)
valley_assault$rate21 <- round((valley_assault$`2021`/valley_assault$population)*100000,1)
valley_assault$rate22 <- round((valley_assault$`2022`/valley_assault$population)*100000,1)
valley_assault$rate_prior4years <-round((valley_assault$avg_prior4years/valley_assault$population)*100000,1)
# for map/table making purposes, changing Inf and NaN in calc fields to NA
valley_assault <- valley_assault %>%
  mutate_if(is.numeric, ~ifelse(. == Inf, NA, .))
valley_assault <- valley_assault %>%
  mutate_if(is.numeric, ~ifelse(. == "NaN", NA, .))
# elimate rates for districts with fewer than 1,000 estimated population
valley_assault$rate19 <- ifelse(valley_assault$population<1000,NA,valley_assault$rate19)
valley_assault$rate20 <- ifelse(valley_assault$population<1000,NA,valley_assault$rate20)
valley_assault$rate21 <- ifelse(valley_assault$population<1000,NA,valley_assault$rate21)
valley_assault$rate22 <- ifelse(valley_assault$population<1000,NA,valley_assault$rate22)
valley_assault$rate_prior4years <- ifelse(valley_assault$population<1000,NA,valley_assault$rate_prior4years)

# BURGLARIES
# By Place add change columns for maps
# add 3-year totals and annualized average over three years
valley_burglary$total_prior4years <- valley_burglary$`2019` + valley_burglary$`2020` + valley_burglary$`2021` + valley_burglary$`2022`
valley_burglary$avg_prior4years <- round((valley_burglary$total_prior4years/4),1)
# now add the increases or change percentages vs prepandemic vs last decade
valley_burglary$inc_19to22 <- round(valley_burglary$`2022`/valley_burglary$`2019`*100-100,1)
valley_burglary$inc_10to22 <- round(valley_burglary$`2022`/valley_burglary$`2010`*100-100,1)
# add crime rates for each year
valley_burglary$rate19 <- round((valley_burglary$`2019`/valley_burglary$population)*100000,1)
valley_burglary$rate20 <- round((valley_burglary$`2020`/valley_burglary$population)*100000,1)
valley_burglary$rate21 <- round((valley_burglary$`2021`/valley_burglary$population)*100000,1)
valley_burglary$rate22 <- round((valley_burglary$`2022`/valley_burglary$population)*100000,1)
valley_burglary$rate_prior4years <-round((valley_burglary$avg_prior4years/valley_burglary$population)*100000,1)
# for map/table making purposes, changing Inf and NaN in calc fields to NA
valley_burglary <- valley_burglary %>%
  mutate_if(is.numeric, ~ifelse(. == Inf, NA, .))
valley_burglary <- valley_burglary %>%
  mutate_if(is.numeric, ~ifelse(. == "NaN", NA, .))
# elimate rates for districts with fewer than 1,000 estimated population
valley_burglary$rate19 <- ifelse(valley_burglary$population<1000,NA,valley_burglary$rate19)
valley_burglary$rate20 <- ifelse(valley_burglary$population<1000,NA,valley_burglary$rate20)
valley_burglary$rate21 <- ifelse(valley_burglary$population<1000,NA,valley_burglary$rate21)
valley_burglary$rate22 <- ifelse(valley_burglary$population<1000,NA,valley_burglary$rate22)
valley_burglary$rate_prior4years <- ifelse(valley_burglary$population<1000,NA,valley_burglary$rate_prior4years)

# VEHICLE THEFTS
# By Place add change columns for maps
# add 3-year totals and annualized average over three years
valley_autotheft$total_prior4years <- valley_autotheft$`2019` + valley_autotheft$`2020` + valley_autotheft$`2021` + valley_autotheft$`2022`
valley_autotheft$avg_prior4years <- round((valley_autotheft$total_prior4years/4),1)
# now add the increases or change percentages vs prepandemic vs last decade
valley_autotheft$inc_19to22 <- round(valley_autotheft$`2022`/valley_autotheft$`2019`*100-100,1)
valley_autotheft$inc_10to22 <- round(valley_autotheft$`2022`/valley_autotheft$`2010`*100-100,1)
# add crime rates for each year
valley_autotheft$rate19 <- round((valley_autotheft$`2019`/valley_autotheft$population)*100000,1)
valley_autotheft$rate20 <- round((valley_autotheft$`2020`/valley_autotheft$population)*100000,1)
valley_autotheft$rate21 <- round((valley_autotheft$`2021`/valley_autotheft$population)*100000,1)
valley_autotheft$rate22 <- round((valley_autotheft$`2022`/valley_autotheft$population)*100000,1)
valley_autotheft$rate_prior4years <-round((valley_autotheft$avg_prior4years/valley_autotheft$population)*100000,1)
# for map/table making purposes, changing Inf and NaN in calc fields to NA
valley_autotheft <- valley_autotheft %>%
  mutate_if(is.numeric, ~ifelse(. == Inf, NA, .))
valley_autotheft <- valley_autotheft %>%
  mutate_if(is.numeric, ~ifelse(. == "NaN", NA, .))
# elimate rates for districts with fewer than 1,000 estimated population
valley_autotheft$rate19 <- ifelse(valley_autotheft$population<1000,NA,valley_autotheft$rate19)
valley_autotheft$rate20 <- ifelse(valley_autotheft$population<1000,NA,valley_autotheft$rate20)
valley_autotheft$rate21 <- ifelse(valley_autotheft$population<1000,NA,valley_autotheft$rate21)
valley_autotheft$rate22 <- ifelse(valley_autotheft$population<1000,NA,valley_autotheft$rate22)
valley_autotheft$rate_prior4years <- ifelse(valley_autotheft$population<1000,NA,valley_autotheft$rate_prior4years)

# THEFTS
# By Place add change columns for maps
# add 3-year totals and annualized average over three years
valley_theft$total_prior4years <- valley_theft$`2019` + valley_theft$`2020` + valley_theft$`2021` + valley_theft$`2022`
valley_theft$avg_prior4years <- round((valley_theft$total_prior4years/4),1)
# now add the increases or change percentages vs prepandemic vs last decade
valley_theft$inc_19to22 <- round(valley_theft$`2022`/valley_theft$`2019`*100-100,1)
valley_theft$inc_10to22 <- round(valley_theft$`2022`/valley_theft$`2010`*100-100,1)
# add crime rates for each year
valley_theft$rate19 <- round((valley_theft$`2019`/valley_theft$population)*100000,1)
valley_theft$rate20 <- round((valley_theft$`2020`/valley_theft$population)*100000,1)
valley_theft$rate21 <- round((valley_theft$`2021`/valley_theft$population)*100000,1)
valley_theft$rate22 <- round((valley_theft$`2022`/valley_theft$population)*100000,1)
valley_theft$rate_prior4years <-round((valley_theft$avg_prior4years/valley_theft$population)*100000,1)
# for map/table making purposes, changing Inf and NaN in calc fields to NA
valley_theft <- valley_theft %>%
  mutate_if(is.numeric, ~ifelse(. == Inf, NA, .))
valley_theft <- valley_theft %>%
  mutate_if(is.numeric, ~ifelse(. == "NaN", NA, .))
# elimate rates for districts with fewer than 1,000 estimated population
valley_theft$rate19 <- ifelse(valley_theft$population<1000,NA,valley_theft$rate19)
valley_theft$rate20 <- ifelse(valley_theft$population<1000,NA,valley_theft$rate20)
valley_theft$rate21 <- ifelse(valley_theft$population<1000,NA,valley_theft$rate21)
valley_theft$rate22 <- ifelse(valley_theft$population<1000,NA,valley_theft$rate22)
valley_theft$rate_prior4years <- ifelse(valley_theft$population<1000,NA,valley_theft$rate_prior4years)

# Add notations for Central Valley area departments in markdown for charts
valley_murder$place_chart <- paste0(valley_murder$place,"^",valley_murder$county,"^")
valley_sexassault$place_chart <- paste0(valley_sexassault$place,"^",valley_sexassault$county,"^")
valley_assault$place_chart <- paste0(valley_assault$place,"^",valley_assault$county,"^")
valley_robbery$place_chart <- paste0(valley_robbery$place,"^",valley_robbery$county,"^")
valley_burglary$place_chart <- paste0(valley_burglary$place,"^",valley_burglary$county,"^")
valley_theft$place_chart <- paste0(valley_theft$place,"^",valley_theft$county,"^")
valley_autotheft$place_chart <- paste0(valley_autotheft$place,"^",valley_autotheft$county,"^")

# Add notations for Central Valley area departments in markdown for charts
valley_murder$place_chart <- ifelse(valley_murder$place_chart == "Fresno^Fresno County^", "City of Fresno^Fresno County^", valley_murder$place_chart)
valley_sexassault$place_chart <- ifelse(valley_sexassault$place_chart == "Fresno^Fresno County^", "City of Fresno^Fresno County^", valley_sexassault$place_chart)
valley_assault$place_chart <- ifelse(valley_assault$place_chart == "Fresno^Fresno County^", "City of Fresno^Fresno County^", valley_assault$place_chart)
valley_robbery$place_chart <- ifelse(valley_robbery$place_chart == "Fresno^Fresno County^", "City of Fresno^Fresno County^", valley_robbery$place_chart)
valley_burglary$place_chart <- ifelse(valley_burglary$place_chart == "Fresno^Fresno County^", "City of Fresno^Fresno County^", valley_burglary$place_chart)
valley_theft$place_chart <- ifelse(valley_theft$place_chart == "Fresno^Fresno County^", "City of Fresno^Fresno County^", valley_theft$place_chart)
valley_autotheft$place_chart <- ifelse(valley_autotheft$place_chart == "Fresno^Fresno County^", "City of Fresno^Fresno County^", valley_autotheft$place_chart)

# For Datawrapper charts
# VALLEY WIDE FOR PUBLISHING LOOKUP CHARTS FOR EACH CRIME CATEGORY
valley_murder %>% st_drop_geometry() %>% select(26,4:16,19,20,24) %>% rename('Change since 2019'=inc_19to22,'Change since 2010'=inc_10to22,'2022 rate per 100K'=rate22) %>% write_csv("data/output/annual/valley_murder.csv")
valley_sexassault %>% st_drop_geometry() %>% select(26,4:16,19,20,24) %>% rename('Change since 2019'=inc_19to22,'Change since 2010'=inc_10to22,'2022 rate per 100K'=rate22) %>% write_csv("data/output/annual/valley_sexassault.csv")
valley_assault %>% st_drop_geometry() %>% select(26,4:16,19,20,24) %>% rename('Change since 2019'=inc_19to22,'Change since 2010'=inc_10to22,'2022 rate per 100K'=rate22) %>% write_csv("data/output/annual/valley_assault.csv")
valley_robbery %>% st_drop_geometry() %>% select(26,4:16,19,20,24) %>% rename('Change since 2019'=inc_19to22,'Change since 2010'=inc_10to22,'2022 rate per 100K'=rate22) %>% write_csv("data/output/annual/valley_robbery.csv")
valley_burglary %>% st_drop_geometry() %>% select(26,4:16,19,20,24) %>% rename('Change since 2019'=inc_19to22,'Change since 2010'=inc_10to22,'2022 rate per 100K'=rate22) %>% write_csv("data/output/annual/valley_burglary.csv")
valley_theft %>% st_drop_geometry() %>% select(26,4:16,19,20,24) %>% rename('Change since 2019'=inc_19to22,'Change since 2010'=inc_10to22,'2022 rate per 100K'=rate22) %>% write_csv("data/output/annual/valley_theft.csv")
valley_autotheft %>% st_drop_geometry() %>% select(26,4:16,19,20,24) %>% rename('Change since 2019'=inc_19to22,'Change since 2010'=inc_10to22,'2022 rate per 100K'=rate22) %>% write_csv("data/output/annual/valley_autotheft.csv")

# FRESNO COUNTY ONLY FOR INTERNAL USE ONLY
valley_murder %>% st_drop_geometry() %>% filter(county=="Fresno County") %>% select(26,4:16,19,20,24) %>% write_csv("data/output/annual/fresno_murder.csv")
valley_sexassault %>% st_drop_geometry() %>% filter(county=="Fresno County") %>% select(26,4:16,19,20,24) %>% write_csv("data/output/annual/fresno_sexassault.csv")
valley_assault %>% st_drop_geometry() %>% filter(county=="Fresno County") %>% select(26,4:16,19,20,24) %>% write_csv("data/output/annual/fresno_assault.csv")
valley_robbery %>% st_drop_geometry() %>% filter(county=="Fresno County") %>% select(26,4:16,19,20,24) %>% write_csv("data/output/annual/fresno_robbery.csv")
valley_burglary %>% st_drop_geometry() %>% filter(county=="Fresno County") %>% select(26,4:16,19,20,24) %>% write_csv("data/output/annual/fresno_burglary.csv")
valley_theft %>% st_drop_geometry() %>% filter(county=="Fresno County") %>% select(26,4:16,19,20,24) %>% write_csv("data/output/annual/fresno_theft.csv")
valley_autotheft %>% st_drop_geometry() %>% filter(county=="Fresno County") %>% select(26,4:16,19,20,24) %>% write_csv("data/output/annual/fresno_autotheft.csv")

# Create rds files for building the trackers
valley_murder %>% saveRDS("scripts/rds/valley_murder.rds")
valley_sexassault %>% saveRDS("scripts/rds/valley_sexassault.rds")
valley_assault %>% saveRDS("scripts/rds/valley_assault.rds")
valley_robbery %>% saveRDS("scripts/rds/valley_robbery.rds")
valley_burglary %>% saveRDS("scripts/rds/valley_burglary.rds")
valley_theft %>% saveRDS("scripts/rds/valley_theft.rds")
valley_autotheft %>% saveRDS("scripts/rds/valley_autotheft.rds")