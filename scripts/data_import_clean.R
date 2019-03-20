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
bamona = read_csv("./data/bamona_data.csv")

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

bamona = bamona %>%
  select(Latitude = `Lat/Long`, Longitude = Longitude, date =`Observation Date`) %>%
  mutate(date = as.Date(date, "%m/%d/%Y"))

swallowtail_df = swallowtail_df %>%
  select(Latitude = latitude, Longitude = longitude, date) %>%
  mutate(Latitude = as.numeric(Latitude), 
         Longitude = as.numeric(Longitude))

#binding together
swallowtail_master = bind_rows("inat_gbif" = swallowtail_df, 
                               "ebutterfly" = ebutterfly,
                               "maine" = maine, 
                               "maritime" = maritime,
                               "ma_club" = ma_club,
                               "bamona" = bamona,
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

#Final steps 

# Importing Data and Cleaning ---------------------------------------------
#Pulling in data that has been cleaned in previous script

#Generating some features that will be useful later on
swallowtail_master = swallowtail_master %>%
  mutate(year = year(date), 
         time_frame = ifelse(year >= 2000, "T2", "T1")) %>%
  rename(latitude = Latitude, longitude = Longitude) %>%
  select(-data_source)

hostplant_master = hostplant_master %>%
  mutate(year = year(date), 
         time_frame = ifelse(year >= 2000, "T2", "T1"),
         longitude = as.numeric(longitude), 
         latitude = as.numeric(latitude)) %>%
  select(-name, -prov, -Longitude, -Latitude, -key)

#Filtering the data to include stuff east of texas (94º), and in the US, Canada. Should be done from data_import_clean script, but good to double check
lon_min = -94
lon_max = -65
lat_min = 25
lat_max = 55

swallowtail_master = swallowtail_master %>%
  filter(latitude >= lat_min & latitude <= lat_max) %>%
  filter(longitude >= lon_min & longitude <= lon_max)

hostplant_master = hostplant_master %>%
  filter(latitude >= lat_min & latitude <= lat_max) %>%
  filter(longitude >= lon_min & longitude <= lon_max)

#mapping to check
st = get_map("North Carolina", zoom = 4, maptype = "toner-background")
ggmap(st, maptype = "toner-background", extent = "panel") +
  geom_point(data = swallowtail_master, aes(x = longitude, y = latitude, color = time_frame), alpha = 0.5) +
  scale_color_discrete(name = "Time Frame", labels = c("Pre-2000", "Post-2000")) +
  labs(x = "Longitude (º) ", y = "Latitude (º)")  

# Importing Bioclim Data and Cropping -------------------------------------
#We can do this from the dismo package - Interesting point here, this is different from original methods. These climate data are representative of "current" conditions - averaged between 1970 and 2000 (https://www.researchgate.net/publication/316999789_WorldClim_2_New_1-km_spatial_resolution_climate_surfaces_for_global_land_areas). 

bioclim.data <- raster::getData(name = "worldclim",
                                var = "bio",
                                res = 2.5,
                                path = "./data/")

# Determine geographic extent of our data
max_lat_swallowtail <- ceiling(max(swallowtail_master$latitude))
min_lat_swallowtail <- floor(min(swallowtail_master$latitude))
max_lon_swallowtail <- ceiling(max(swallowtail_master$longitude))
min_lon_swallowtail <- floor(min(swallowtail_master$longitude))
geographic.extent <- extent(x = c(min_lon_swallowtail, max_lon_swallowtail, min_lat_swallowtail, max_lat_swallowtail))

# Crop bioclim data to geographic extent of swallowtails
bioclim.data <- crop(x = bioclim.data, y = geographic.extent)

#writing bioclim data
saveRDS(bioclim.data, "./data/bioclim.rds")

#Writing hostplant records
write.csv(hostplant_master, "./data/hostplant_data.csv")

#Writing butterfly records
write.csv(swallowtail_master, "./data/swallowtail_data.csv")
