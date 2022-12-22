library(tidyverse)
library(sf)
library(tidyr)

# May need to load the right map file here
# valley_places <- readRDS("scripts/rds/valley_places.rds")

# Read in the state crime file
california_annual <- read_csv("data/source/california_crime_annual.csv", 
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
valley_murder <- valley_annual %>% select(year,county,ncic_code,homicide_sum) %>% spread(year,homicide_sum) %>% select(1,2,28:39) %>% rename("place"="ncic_code")
valley_sexassault <- valley_annual %>% select(year,county,ncic_code,for_rape_sum) %>% spread(year,for_rape_sum) %>% select(1,2,28:39) %>% rename("place"="ncic_code")
valley_assault <- valley_annual %>% select(year,county,ncic_code,agg_assault_sum) %>% spread(year,agg_assault_sum) %>% select(1,2,28:39) %>% rename("place"="ncic_code")
valley_robbery <- valley_annual %>% select(year,county,ncic_code,robbery_sum) %>% spread(year,robbery_sum) %>% select(1,2,28:39) %>% rename("place"="ncic_code")
valley_burglary <- valley_annual %>% select(year,county,ncic_code,burglary_sum) %>% spread(year,burglary_sum) %>% select(1,2,28:39) %>% rename("place"="ncic_code")
valley_theft <- valley_annual %>% select(year,county,ncic_code,l_ttotal_sum) %>% spread(year,l_ttotal_sum) %>% select(1,2,28:39) %>% rename("place"="ncic_code")
valley_autotheft <- valley_annual %>% select(year,county,ncic_code,vehicle_theft_sum) %>% spread(year,vehicle_theft_sum) %>% select(1,2,28:39) %>% rename("place"="ncic_code")

# Before we start to make changes for maps we want to create countywide totals for charts/tracker text
countywide_murder <- valley_murder %>% group_by(county) %>% summarise(total21=sum(`2021`,na.rm=TRUE),total19=sum(`2019`,na.rm=TRUE),total10=sum(`2010`,na.rm=TRUE))
countywide_sexassault <- valley_sexassault %>% group_by(county) %>% summarise(total21=sum(`2021`,na.rm=TRUE),total19=sum(`2019`,na.rm=TRUE),total10=sum(`2010`,na.rm=TRUE))
countywide_assault <- valley_assault %>% group_by(county) %>% summarise(total21=sum(`2021`,na.rm=TRUE),total19=sum(`2019`,na.rm=TRUE),total10=sum(`2010`,na.rm=TRUE))
countywide_robbery <- valley_robbery %>% group_by(county) %>% summarise(total21=sum(`2021`,na.rm=TRUE),total19=sum(`2019`,na.rm=TRUE),total10=sum(`2010`,na.rm=TRUE))
countywide_burglary <- valley_burglary %>% group_by(county) %>% summarise(total21=sum(`2021`,na.rm=TRUE),total19=sum(`2019`,na.rm=TRUE),total10=sum(`2010`,na.rm=TRUE))
countywide_theft <- valley_theft %>% group_by(county) %>% summarise(total21=sum(`2021`,na.rm=TRUE),total19=sum(`2019`,na.rm=TRUE),total10=sum(`2010`,na.rm=TRUE))
countywide_autotheft <- valley_autotheft %>% group_by(county) %>% summarise(total21=sum(`2021`,na.rm=TRUE),total19=sum(`2019`,na.rm=TRUE),total10=sum(`2010`,na.rm=TRUE))

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
valley_murder$total_prior3years <- valley_murder$`2019` + valley_murder$`2020` + valley_murder$`2021`
valley_murder$avg_prior3years <- round((valley_murder$total_prior3years/3),1)
# now add the increases or change percentages vs prepandemic vs last decade
valley_murder$inc_19to21 <- round(valley_murder$`2021`/valley_murder$`2019`*100-100,1)
valley_murder$inc_10to21 <- round(valley_murder$`2021`/valley_murder$`2010`*100-100,1)
# add crime rates for each year
valley_murder$rate19 <- round((valley_murder$`2019`/valley_murder$population)*100000,1)
valley_murder$rate20 <- round((valley_murder$`2020`/valley_murder$population)*100000,1)
valley_murder$rate21 <- round((valley_murder$`2021`/valley_murder$population)*100000,1)
valley_murder$rate_prior3years <- round((valley_murder$avg_prior3years/valley_murder$population)*100000,1)
# for map/table making purposes, changing Inf and NaN in calc fields to NA
valley_murder <- valley_murder %>%
  mutate(across(where(is.numeric), ~na_if(., Inf)))
valley_murder <- valley_murder %>%
  mutate(across(where(is.numeric), ~na_if(., "NaN")))
# elimate rates for districts with fewer than 1,000 estimated population
valley_murder$rate19 <- ifelse(valley_murder$population<1000,NA,valley_murder$rate19)
valley_murder$rate20 <- ifelse(valley_murder$population<1000,NA,valley_murder$rate20)
valley_murder$rate21 <- ifelse(valley_murder$population<1000,NA,valley_murder$rate21)
valley_murder$rate_prior3years <- ifelse(valley_murder$population<1000,NA,valley_murder$rate_prior3years)

# SEXUAL ASSAULTS
# By Place add change columns for maps
# add 3-year totals and annualized average over three years
valley_sexassault$total_prior3years <- valley_sexassault$`2019` + valley_sexassault$`2020` + valley_sexassault$`2021`
valley_sexassault$avg_prior3years <- round((valley_sexassault$total_prior3years/3),1)
# now add the increases or change percentages vs prepandemic vs last decade
valley_sexassault$inc_19to21 <- round(valley_sexassault$`2021`/valley_sexassault$`2019`*100-100,1)
valley_sexassault$inc_10to21 <- round(valley_sexassault$`2021`/valley_sexassault$`2010`*100-100,1)
# add crime rates for each year
valley_sexassault$rate19 <- round((valley_sexassault$`2019`/valley_sexassault$population)*100000,1)
valley_sexassault$rate20 <- round((valley_sexassault$`2020`/valley_sexassault$population)*100000,1)
valley_sexassault$rate21 <- round((valley_sexassault$`2021`/valley_sexassault$population)*100000,1)
valley_sexassault$rate_prior3years <-round((valley_sexassault$avg_prior3years/valley_sexassault$population)*100000,1)
# for map/table making purposes, changing Inf and NaN in calc fields to NA
valley_sexassault <- valley_sexassault %>%
  mutate(across(where(is.numeric), ~na_if(., Inf)))
valley_sexassault <- valley_sexassault %>%
  mutate(across(where(is.numeric), ~na_if(., "NaN")))
# elimate rates for districts with fewer than 1,000 estimated population
valley_sexassault$rate19 <- ifelse(valley_sexassault$population<1000,NA,valley_sexassault$rate19)
valley_sexassault$rate20 <- ifelse(valley_sexassault$population<1000,NA,valley_sexassault$rate20)
valley_sexassault$rate21 <- ifelse(valley_sexassault$population<1000,NA,valley_sexassault$rate21)
valley_sexassault$rate_prior3years <- ifelse(valley_sexassault$population<1000,NA,valley_sexassault$rate_prior3years)

# ROBBERIES
# By Place add change columns for maps
# add 3-year totals and annualized average over three years
valley_robbery$total_prior3years <- valley_robbery$`2019` + valley_robbery$`2020` + valley_robbery$`2021`
valley_robbery$avg_prior3years <- round((valley_robbery$total_prior3years/3),1)
# now add the increases or change percentages vs prepandemic vs last decade
valley_robbery$inc_19to21 <- round(valley_robbery$`2021`/valley_robbery$`2019`*100-100,1)
valley_robbery$inc_10to21 <- round(valley_robbery$`2021`/valley_robbery$`2010`*100-100,1)
# add crime rates for each year
valley_robbery$rate19 <- round((valley_robbery$`2019`/valley_robbery$population)*100000,1)
valley_robbery$rate20 <- round((valley_robbery$`2020`/valley_robbery$population)*100000,1)
valley_robbery$rate21 <- round((valley_robbery$`2021`/valley_robbery$population)*100000,1)
valley_robbery$rate_prior3years <-round((valley_robbery$avg_prior3years/valley_robbery$population)*100000,1)
# for map/table making purposes, changing Inf and NaN in calc fields to NA
valley_robbery <- valley_robbery %>%
  mutate(across(where(is.numeric), ~na_if(., Inf)))
valley_robbery <- valley_robbery %>%
  mutate(across(where(is.numeric), ~na_if(., "NaN")))
# elimate rates for districts with fewer than 1,000 estimated population
valley_robbery$rate19 <- ifelse(valley_robbery$population<1000,NA,valley_robbery$rate19)
valley_robbery$rate20 <- ifelse(valley_robbery$population<1000,NA,valley_robbery$rate20)
valley_robbery$rate21 <- ifelse(valley_robbery$population<1000,NA,valley_robbery$rate21)
valley_robbery$rate_prior3years <- ifelse(valley_robbery$population<1000,NA,valley_robbery$rate_prior3years)

# ASSAULTS
# By Place add change columns for maps
# add 3-year totals and annualized average over three years
valley_assault$total_prior3years <- valley_assault$`2019` + valley_assault$`2020` + valley_assault$`2021`
valley_assault$avg_prior3years <- round((valley_assault$total_prior3years/3),1)
# now add the increases or change percentages vs prepandemic vs last decade
valley_assault$inc_19to21 <- round(valley_assault$`2021`/valley_assault$`2019`*100-100,1)
valley_assault$inc_10to21 <- round(valley_assault$`2021`/valley_assault$`2010`*100-100,1)
# add crime rates for each year
valley_assault$rate19 <- round((valley_assault$`2019`/valley_assault$population)*100000,1)
valley_assault$rate20 <- round((valley_assault$`2020`/valley_assault$population)*100000,1)
valley_assault$rate21 <- round((valley_assault$`2021`/valley_assault$population)*100000,1)
valley_assault$rate_prior3years <-round((valley_assault$avg_prior3years/valley_assault$population)*100000,1)
# for map/table making purposes, changing Inf and NaN in calc fields to NA
valley_assault <- valley_assault %>%
  mutate(across(where(is.numeric), ~na_if(., Inf)))
valley_assault <- valley_assault %>%
  mutate(across(where(is.numeric), ~na_if(., "NaN")))
# elimate rates for districts with fewer than 1,000 estimated population
valley_assault$rate19 <- ifelse(valley_assault$population<1000,NA,valley_assault$rate19)
valley_assault$rate20 <- ifelse(valley_assault$population<1000,NA,valley_assault$rate20)
valley_assault$rate21 <- ifelse(valley_assault$population<1000,NA,valley_assault$rate21)
valley_assault$rate_prior3years <- ifelse(valley_assault$population<1000,NA,valley_assault$rate_prior3years)

# BURGLARIES
# By Place add change columns for maps
# add 3-year totals and annualized average over three years
valley_burglary$total_prior3years <- valley_burglary$`2019` + valley_burglary$`2020` + valley_burglary$`2021`
valley_burglary$avg_prior3years <- round((valley_burglary$total_prior3years/3),1)
# now add the increases or change percentages vs prepandemic vs last decade
valley_burglary$inc_19to21 <- round(valley_burglary$`2021`/valley_burglary$`2019`*100-100,1)
valley_burglary$inc_10to21 <- round(valley_burglary$`2021`/valley_burglary$`2010`*100-100,1)
# add crime rates for each year
valley_burglary$rate19 <- round((valley_burglary$`2019`/valley_burglary$population)*100000,1)
valley_burglary$rate20 <- round((valley_burglary$`2020`/valley_burglary$population)*100000,1)
valley_burglary$rate21 <- round((valley_burglary$`2021`/valley_burglary$population)*100000,1)
valley_burglary$rate_prior3years <-round((valley_burglary$avg_prior3years/valley_burglary$population)*100000,1)
# for map/table making purposes, changing Inf and NaN in calc fields to NA
valley_burglary <- valley_burglary %>%
  mutate(across(where(is.numeric), ~na_if(., Inf)))
valley_burglary <- valley_burglary %>%
  mutate(across(where(is.numeric), ~na_if(., "NaN")))
# elimate rates for districts with fewer than 1,000 estimated population
valley_burglary$rate19 <- ifelse(valley_burglary$population<1000,NA,valley_burglary$rate19)
valley_burglary$rate20 <- ifelse(valley_burglary$population<1000,NA,valley_burglary$rate20)
valley_burglary$rate21 <- ifelse(valley_burglary$population<1000,NA,valley_burglary$rate21)
valley_burglary$rate_prior3years <- ifelse(valley_burglary$population<1000,NA,valley_burglary$rate_prior3years)

# VEHICLE THEFTS
# By Place add change columns for maps
# add 3-year totals and annualized average over three years
valley_autotheft$total_prior3years <- valley_autotheft$`2019` + valley_autotheft$`2020` + valley_autotheft$`2021`
valley_autotheft$avg_prior3years <- round((valley_autotheft$total_prior3years/3),1)
# now add the increases or change percentages vs prepandemic vs last decade
valley_autotheft$inc_19to21 <- round(valley_autotheft$`2021`/valley_autotheft$`2019`*100-100,1)
valley_autotheft$inc_10to21 <- round(valley_autotheft$`2021`/valley_autotheft$`2010`*100-100,1)
# add crime rates for each year
valley_autotheft$rate19 <- round((valley_autotheft$`2019`/valley_autotheft$population)*100000,1)
valley_autotheft$rate20 <- round((valley_autotheft$`2020`/valley_autotheft$population)*100000,1)
valley_autotheft$rate21 <- round((valley_autotheft$`2021`/valley_autotheft$population)*100000,1)
valley_autotheft$rate_prior3years <-round((valley_autotheft$avg_prior3years/valley_autotheft$population)*100000,1)
# for map/table making purposes, changing Inf and NaN in calc fields to NA
valley_autotheft <- valley_autotheft %>%
  mutate(across(where(is.numeric), ~na_if(., Inf)))
valley_autotheft <- valley_autotheft %>%
  mutate(across(where(is.numeric), ~na_if(., "NaN")))
# elimate rates for districts with fewer than 1,000 estimated population
valley_autotheft$rate19 <- ifelse(valley_autotheft$population<1000,NA,valley_autotheft$rate19)
valley_autotheft$rate20 <- ifelse(valley_autotheft$population<1000,NA,valley_autotheft$rate20)
valley_autotheft$rate21 <- ifelse(valley_autotheft$population<1000,NA,valley_autotheft$rate21)
valley_autotheft$rate_prior3years <- ifelse(valley_autotheft$population<1000,NA,valley_autotheft$rate_prior3years)

# THEFTS
# By Place add change columns for maps
# add 3-year totals and annualized average over three years
valley_theft$total_prior3years <- valley_theft$`2019` + valley_theft$`2020` + valley_theft$`2021`
valley_theft$avg_prior3years <- round((valley_theft$total_prior3years/3),1)
# now add the increases or change percentages vs prepandemic vs last decade
valley_theft$inc_19to21 <- round(valley_theft$`2021`/valley_theft$`2019`*100-100,1)
valley_theft$inc_10to21 <- round(valley_theft$`2021`/valley_theft$`2010`*100-100,1)
# add crime rates for each year
valley_theft$rate19 <- round((valley_theft$`2019`/valley_theft$population)*100000,1)
valley_theft$rate20 <- round((valley_theft$`2020`/valley_theft$population)*100000,1)
valley_theft$rate21 <- round((valley_theft$`2021`/valley_theft$population)*100000,1)
valley_theft$rate_prior3years <-round((valley_theft$avg_prior3years/valley_theft$population)*100000,1)
# for map/table making purposes, changing Inf and NaN in calc fields to NA
valley_theft <- valley_theft %>%
  mutate(across(where(is.numeric), ~na_if(., Inf)))
valley_theft <- valley_theft %>%
  mutate(across(where(is.numeric), ~na_if(., "NaN")))
# elimate rates for districts with fewer than 1,000 estimated population
valley_theft$rate19 <- ifelse(valley_theft$population<1000,NA,valley_theft$rate19)
valley_theft$rate20 <- ifelse(valley_theft$population<1000,NA,valley_theft$rate20)
valley_theft$rate21 <- ifelse(valley_theft$population<1000,NA,valley_theft$rate21)
valley_theft$rate_prior3years <- ifelse(valley_theft$population<1000,NA,valley_theft$rate_prior3years)

# Add notations for LA area departments in markdown for charts
# valley_murder$place_chart <- case_when(valley_murder$agency=="LASD" ~ paste0(valley_murder$place,"^Area policed by ",valley_murder$agency,"^"),
#                                      valley_murder$agency=="LAPD" ~ paste0(valley_murder$place,"^Division of Los Angeles PD^"),
#                                      TRUE ~ valley_murder$place)
#valley_sexassault$place_chart <- case_when(valley_sexassault$agency=="LASD" ~ paste0(valley_sexassault$place,"^Area policed by ",valley_sexassault$agency,"^"),
#                                          valley_sexassault$agency=="LAPD" ~ paste0(valley_sexassault$place,"^Division of Los Angeles PD^"),
#                                          TRUE ~ valley_murder$place)
#valley_assault$place_chart <- case_when(valley_assault$agency=="LASD" ~ paste0(valley_assault$place,"^Area policed by ",valley_assault$agency,"^"),
#                                       valley_assault$agency=="LAPD" ~ paste0(valley_assault$place,"^Division of Los Angeles PD^"),
#                                       TRUE ~ valley_murder$place)
#valley_robbery$place_chart <- case_when(valley_robbery$agency=="LASD" ~ paste0(valley_robbery$place,"^Area policed by ",valley_robbery$agency,"^"),
#                                       valley_robbery$agency=="LAPD" ~ paste0(valley_robbery$place,"^Division of Los Angeles PD^"),
#                                       TRUE ~ valley_murder$place)
#valley_burglary$place_chart <- case_when(valley_burglary$agency=="LASD" ~ paste0(valley_burglary$place,"^Area policed by ",valley_burglary$agency,"^"),
#                                        valley_burglary$agency=="LAPD" ~ paste0(valley_burglary$place,"^Division of Los Angeles PD^"),
#                                        TRUE ~ valley_murder$place)
#valley_theft$place_chart <- case_when(valley_theft$agency=="LASD" ~ paste0(valley_theft$place,"^Area policed by ",valley_theft$agency,"^"),
#                                     valley_theft$agency=="LAPD" ~ paste0(valley_theft$place,"^Division of Los Angeles PD^"),
#                                     TRUE ~ valley_murder$place)
#valley_autotheft$place_chart <- case_when(valley_autotheft$agency=="LASD" ~ paste0(valley_autotheft$place,"^Area policed by ",valley_autotheft$agency,"^"),
#                                         valley_autotheft$agency=="LAPD" ~ paste0(valley_autotheft$place,"^Division of Los Angeles PD^"),
#                                         TRUE ~ valley_murder$place)

# Output county level files for each crime category
# FRESNO
valley_murder %>% st_drop_geometry() %>% filter(county=="Fresno County") %>% select(2,3:14,19,20,23) %>% write_csv("data/output/annual/fresno_murder.csv")
valley_sexassault %>% st_drop_geometry() %>% filter(county=="Fresno County") %>% select(2,3:14,19,20,23) %>% write_csv("data/output/annual/fresno_sexassault.csv")
valley_assault %>% st_drop_geometry() %>% filter(county=="Fresno County") %>% select(2,3:14,19,20,23) %>% write_csv("data/output/annual/fresno_assault.csv")
valley_robbery %>% st_drop_geometry() %>% filter(county=="Fresno County") %>% select(2,3:14,19,20,23) %>% write_csv("data/output/annual/fresno_robbery.csv")
valley_burglary %>% st_drop_geometry() %>% filter(county=="Fresno County") %>% select(2,3:14,19,20,23) %>% write_csv("data/output/annual/fresno_burglary.csv")
valley_theft %>% st_drop_geometry() %>% filter(county=="Fresno County") %>% select(2,3:14,19,20,23) %>% write_csv("data/output/annual/fresno_theft.csv")
valley_autotheft %>% st_drop_geometry() %>% filter(county=="Fresno County") %>% select(2,3:14,19,20,23) %>% write_csv("data/output/annual/fresno_autotheft.csv")
# KINGS
valley_murder %>% st_drop_geometry() %>% filter(county=="Kings County") %>% select(2,3:14,19,20,23) %>% write_csv("data/output/annual/kings_murder.csv")
valley_sexassault %>% st_drop_geometry() %>% filter(county=="Kings County") %>% select(2,3:14,19,20,23) %>% write_csv("data/output/annual/kings_sexassault.csv")
valley_assault %>% st_drop_geometry() %>% filter(county=="Kings County") %>% select(2,3:14,19,20,23) %>% write_csv("data/output/annual/kings_assault.csv")
valley_robbery %>% st_drop_geometry() %>% filter(county=="Kings County") %>% select(2,3:14,19,20,23) %>% write_csv("data/output/annual/kings_robbery.csv")
valley_burglary %>% st_drop_geometry() %>% filter(county=="Kings County") %>% select(2,3:14,19,20,23) %>% write_csv("data/output/annual/kings_burglary.csv")
valley_theft %>% st_drop_geometry() %>% filter(county=="Kings County") %>% select(2,3:14,19,20,23) %>% write_csv("data/output/annual/kings_theft.csv")
valley_autotheft %>% st_drop_geometry() %>% filter(county=="Kings County") %>% select(2,3:14,19,20,23) %>% write_csv("data/output/annual/kings_autotheft.csv")
# MADERA
valley_murder %>% st_drop_geometry() %>% filter(county=="Madera County") %>% select(2,3:14,19,20,23) %>% write_csv("data/output/annual/madera_murder.csv")
valley_sexassault %>% st_drop_geometry() %>% filter(county=="Madera County") %>% select(2,3:14,19,20,23) %>% write_csv("data/output/annual/madera_sexassault.csv")
valley_assault %>% st_drop_geometry() %>% filter(county=="Madera County") %>% select(2,3:14,19,20,23) %>% write_csv("data/output/annual/madera_assault.csv")
valley_robbery %>% st_drop_geometry() %>% filter(county=="Madera County") %>% select(2,3:14,19,20,23) %>% write_csv("data/output/annual/madera_robbery.csv")
valley_burglary %>% st_drop_geometry() %>% filter(county=="Madera County") %>% select(2,3:14,19,20,23) %>% write_csv("data/output/annual/madera_burglary.csv")
valley_theft %>% st_drop_geometry() %>% filter(county=="Madera County") %>% select(2,3:14,19,20,23) %>% write_csv("data/output/annual/madera_theft.csv")
valley_autotheft %>% st_drop_geometry() %>% filter(county=="Madera County") %>% select(2,3:14,19,20,23) %>% write_csv("data/output/annual/madera_autotheft.csv")
# MARIPOSA
valley_murder %>% st_drop_geometry() %>% filter(county=="Mariposa County") %>% select(2,3:14,19,20,23) %>% write_csv("data/output/annual/mariposa_murder.csv")
valley_sexassault %>% st_drop_geometry() %>% filter(county=="Mariposa County") %>% select(2,3:14,19,20,23) %>% write_csv("data/output/annual/mariposa_sexassault.csv")
valley_assault %>% st_drop_geometry() %>% filter(county=="Mariposa County") %>% select(2,3:14,19,20,23) %>% write_csv("data/output/annual/mariposa_assault.csv")
valley_robbery %>% st_drop_geometry() %>% filter(county=="Mariposa County") %>% select(2,3:14,19,20,23) %>% write_csv("data/output/annual/mariposa_robbery.csv")
valley_burglary %>% st_drop_geometry() %>% filter(county=="Mariposa County") %>% select(2,3:14,19,20,23) %>% write_csv("data/output/annual/mariposa_burglary.csv")
valley_theft %>% st_drop_geometry() %>% filter(county=="Mariposa County") %>% select(2,3:14,19,20,23) %>% write_csv("data/output/annual/mariposa_theft.csv")
valley_autotheft %>% st_drop_geometry() %>% filter(county=="Mariposa County") %>% select(2,3:14,19,20,23) %>% write_csv("data/output/annual/mariposa_autotheft.csv")
# MERCED
valley_murder %>% st_drop_geometry() %>% filter(county=="Merced County") %>% select(2,3:14,19,20,23) %>% write_csv("data/output/annual/merced_murder.csv")
valley_sexassault %>% st_drop_geometry() %>% filter(county=="Merced County") %>% select(2,3:14,19,20,23) %>% write_csv("data/output/annual/merced_sexassault.csv")
valley_assault %>% st_drop_geometry() %>% filter(county=="Merced County") %>% select(2,3:14,19,20,23) %>% write_csv("data/output/annual/merced_assault.csv")
valley_robbery %>% st_drop_geometry() %>% filter(county=="Merced County") %>% select(2,3:14,19,20,23) %>% write_csv("data/output/annual/merced_robbery.csv")
valley_burglary %>% st_drop_geometry() %>% filter(county=="Merced County") %>% select(2,3:14,19,20,23) %>% write_csv("data/output/annual/merced_burglary.csv")
valley_theft %>% st_drop_geometry() %>% filter(county=="Merced County") %>% select(2,3:14,19,20,23) %>% write_csv("data/output/annual/merced_theft.csv")
valley_autotheft %>% st_drop_geometry() %>% filter(county=="Merced County") %>% select(2,3:14,19,20,23) %>% write_csv("data/output/annual/merced_autotheft.csv")
# TULARE
valley_murder %>% st_drop_geometry() %>% filter(county=="Tulare County") %>% select(2,3:14,19,20,23) %>% write_csv("data/output/annual/tulare_murder.csv")
valley_sexassault %>% st_drop_geometry() %>% filter(county=="Tulare County") %>% select(2,3:14,19,20,23) %>% write_csv("data/output/annual/tulare_sexassault.csv")
valley_assault %>% st_drop_geometry() %>% filter(county=="Tulare County") %>% select(2,3:14,19,20,23) %>% write_csv("data/output/annual/tulare_assault.csv")
valley_robbery %>% st_drop_geometry() %>% filter(county=="Tulare County") %>% select(2,3:14,19,20,23) %>% write_csv("data/output/annual/tulare_robbery.csv")
valley_burglary %>% st_drop_geometry() %>% filter(county=="Tulare County") %>% select(2,3:14,19,20,23) %>% write_csv("data/output/annual/tulare_burglary.csv")
valley_theft %>% st_drop_geometry() %>% filter(county=="Tulare County") %>% select(2,3:14,19,20,23) %>% write_csv("data/output/annual/tulare_theft.csv")
valley_autotheft %>% st_drop_geometry() %>% filter(county=="Tulare County") %>% select(2,3:14,19,20,23) %>% write_csv("data/output/annual/tulare_autotheft.csv")
# VALLEY WIDE
valley_murder %>% st_drop_geometry() %>% select(2,3:14,19,20,23) %>% write_csv("data/output/annual/valley_murder.csv")
valley_sexassault %>% st_drop_geometry() %>% select(2,3:14,19,20,23) %>% write_csv("data/output/annual/valley_sexassault.csv")
valley_assault %>% st_drop_geometry() %>% select(2,3:14,19,20,23) %>% write_csv("data/output/annual/valley_assault.csv")
valley_robbery %>% st_drop_geometry() %>% select(2,3:14,19,20,23) %>% write_csv("data/output/annual/valley_robbery.csv")
valley_burglary %>% st_drop_geometry() %>% select(2,3:14,19,20,23) %>% write_csv("data/output/annual/valley_burglary.csv")
valley_theft %>% st_drop_geometry() %>% select(2,3:14,19,20,23) %>% write_csv("data/output/annual/valley_theft.csv")
valley_autotheft %>% st_drop_geometry() %>% select(2,3:14,19,20,23) %>% write_csv("data/output/annual/valley_autotheft.csv")

# Create rds files for building the trackers
valley_murder %>% saveRDS("scripts/rds/valley_murder.rds")
valley_sexassault %>% saveRDS("scripts/rds/valley_sexassault.rds")
valley_assault %>% saveRDS("scripts/rds/valley_assault.rds")
valley_robbery %>% saveRDS("scripts/rds/valley_robbery.rds")
valley_burglary %>% saveRDS("scripts/rds/valley_burglary.rds")
valley_theft %>% saveRDS("scripts/rds/valley_theft.rds")
valley_autotheft %>% saveRDS("scripts/rds/valley_autotheft.rds")