#Script for getting appropriate swallowtail data
#Keaton Wilson
#keatonwilson@me.com
#2019-02-15

#Loading appropriate packages
library(tidyverse)
library(spocc)
library(mapr)
library(ggmap)
library(scrubr)
library(lubridate)
library(readxl)

#register google api for mapping stuff
register_google(key = "AIzaSyDyAqUc4o9p_DOBSF_JOXH5c_JXPqoU4Yw")

#Let's query inat and GBIF for swallowtail records
swallowtail = occ(query = "Papilio cresphontes*", from = c("gbif", "inat"),
                  has_coords = TRUE, limit = 10000)

#Filtering out anything from inat that isn't research grade
swallowtail$inat$data$`Papilio_cresphontes*` = swallowtail$inat$data$`Papilio_cresphontes*` %>%
  filter(quality_grade == "research")
  
#Aggregating records
swallowtail_df = occ2df(swallowtail)

#Initial mapping
swallowtail_df %>%
  mutate(longitude = as.numeric(longitude),
         latitude = as.numeric(latitude)) %>%
  map_ggmap()

#Lots of naming errors (let's do some filtering)
swallowtail_df = swallowtail_df %>%
  filter(name == 'Papilio cresphontes')

#let's check out time periods
swallowtail_df %>%
  mutate(year = year(date)) %>%
  filter(year >= 1960) %>%
  group_by(year) %>%
  summarize(n()) %>%
  print(n = 59)

#Let's pull in Kent's data from ebutterfly, maine atlas, maritime atlas and MA butterfly club
ebutterfly = read_xlsx(path = "./data/e_butterfly.xlsx")
maine = read_xlsx(path = "./data/maine_butterfly_atlas.xlsx")
maritime = read_xlsx(path = "./data/maritime_atlas.xlsx")
ma_club = read_xlsx(path = "./data/ma_butterfly_club.xlsx")

#cleaning and merging
ebutterfly = ebutterfly %>%
  select(OccuranceID, 'Date Observed', Latitude, Longitude) %>%
  select(Latitude, Longitude, Date = 'Date Observed') %>%
  mutate(date = as.Date(Date)) %>%
  select(-Date)

maine = maine %>%
  select(Latitude, Longitude, Year, Month, Day) %>%
  filter(!is.na(Latitude) & !is.na(Longitude)) %>%
  mutate(date = date(paste(Year, Month, Day, sep = "-"))) %>%
  select(-c(Year, Month, Day))

maritime = maritime %>%
  select(Latitude, Longitude, Year, Month, Day) %>%
  filter(Day != "XX") %>%
  mutate(date = date(paste(Year, Month, Day, sep = "-"))) %>%
  select(-c(Year, Month, Day))

ma_club = ma_club %>%
  select(Latitude, Longitude, Date) %>%
  mutate(date = date(Date)) %>%
  select(-Date)

swallowtail_df = swallowtail_df %>%
  select(Latitude = Latitude, Longitude = Longitude, date) %>%
  mutate(Latitude = as.numeric(Latitude), 
         Longitude = as.numeric(Longitude))

#binding together
swallowtail_master = bind_rows("inat_gbif" = swallowtail_df, 
                               "ebutterfly" = ebutterfly,
                               "maine" = maine, 
                               "maritime" = maritime,
                               "ma_club" = ma_club,
                               .id = "data_source")


#So, lots more records as time has progressed - seems like probably an inat phenomenom. In the original manuscript, only went up to 2010. Would be nice to include more recent data

#Can we just build a simple faceted plot by decade (doesn't include new data from Kent)

#removing duplicates, filtering older data and restricting data to the chunk of the NE we're interested in
swallowtail_master = swallowtail_master %>%
  filter(year(date) > 1959) %>%
  distinct() %>%
  filter(Latitude < 50 & Latitude > 22) %>%
  filter(Longitude < -50 & Longitude > -94)

northeast = get_map("New York", zoom = 3)
ggmap(northeast) +
  geom_point(data = swallowtail_master, aes(x = Longitude, y = Latitude))

#Writing butterfly records
write.csv(swallowtail_master, "./data/swallowtail_data.csv")

#-----------------------------------------------------------------------------------
#HOST PLANT DATA

#Pull in host plant data
host_plant = occ(query = "Zanthoxylum americanum", has_coords = TRUE, from = c("inat", "gbif"), limit = 10000)

#Filtering out anything from inat that isn't research grade
host_plant$inat$data$`Zanthoxylum` = host_plant$inat$data$`Zanthoxylum` %>%
  filter(quality_grade == "research")

#Aggregating records
host_plant_df = occ2df(host_plant)

#filtering names
host_plant_df = host_plant_df %>%
  filter(name == "Zanthoxylum americanum") %>%
  mutate(Longitude = as.numeric(longitude), 
         Latitude = as.numeric(latitude))

hostplant_master = host_plant_df %>%
  filter(year(date) > 1959) %>%
  distinct() %>%
  filter(Latitude < 50 & Latitude > 22) %>%
  filter(Longitude < -50 & Longitude > -94)

#quick map
ggmap(northeast) +
  geom_point(data = hostplant_master, aes(x = Longitude, y = Latitude))

#Writing hostplant records
write.csv(hostplant_master, "./data/hostplant_data.csv")