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

#So, lots more records as time has progressed - seems like probably an inat phenomenom. In the original manuscript, only went up to 2010. Would be nice to include more recent data

#Can we just build a simple faceted plot by decade?

northeast = get_map("New York", zoom = 5)


swallowtail_df = swallowtail_df %>%
  mutate(decade = year(date) - year(date) %% 10) %>%
  filter(year(date) > 1959) %>%
  mutate(latitude = as.numeric(latitude), 
         longitude = as.numeric(longitude))

ggmap(northeast) +
  geom_point(data = swallowtail_df, aes(x = longitude, y = latitude)) +
  facet_wrap(~ decade)

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
  mutate(longitude = as.numeric(longitude), 
         latitude = as.numeric(latitude))
#quick map
ggmap(northeast) +
  geom_point(data = host_plant_df, aes(x = longitude, y = latitude))

#Let's combine and plot together just to see everything
combined_df = bind_rows(host_plant_df, swallowtail_df)

east = get_map("West Virginia", zoom = 5)
ggmap(east) +
  geom_point(data = combined_df, aes(x = longitude, y = latitude, color = name), alpha = 0.2, size = 1) +
  scale_color_brewer(palette = "Accent") +
  stat_density_2d(data = combined_df, aes(x = longitude, y = latitude, fill = name), geom = "polygon", alpha = 0.2) +
  facet_wrap(~ name)

#----------------------------------------------------------------------

#Ok, so now we need to figure out how to weight 
#The original manuscript is a little vague - but I think they set up 10 km by 10 km cells, and then count the number of occurences per cell.If a cell has a lot of occurences, it's downweighted, if only a few it's upweighted. 
#
#Can we use a distance matrix to do a similar thing with more resolution? Weight a particular value by the average distance to its closest 5 neighbors?

library(rgeos) 
library(sp)
library(geosphere)

#New dataframe to work with
sp_swallowtail = swallowtail_df

#Changing the swallowtail dataframe to an sp object
coordinates(sp_swallowtail) = ~longitude + latitude

#Constructing a pairwise distance matrix
d <- distm(sp_swallowtail)

#Closest 5 distances
min_1 = apply(d, 1, function(x) sort(x, TRUE)[2])
min_2 = apply(d, 1, function(x) sort(x, TRUE)[3])
min_3 = apply(d, 1, function(x) sort(x, TRUE)[4])
min_4 = apply(d, 1, function(x) sort(x, TRUE)[5])
min_5 = apply(d, 1, function(x) sort(x, TRUE)[6])

#Binding into a dataframe
mins = data.frame(min_1, min_2, min_3, min_4, min_5)

#binding onto original data
swallowtail_df = bind_cols(swallowtail_df, mins)

#Calculating averages per occurence and normalizing to generate weights
#i.e. high average values = points that are on their own, low average values = points that are clumped
swallowtail_df %>%
  mutate(avg_min = (min_1 + min_2 + min_3 + min_4 + min_5)/5,
  norm_avg_min = (avg_min - mean(avg_min))/sd(avg_min)) %>%
  select(-c(min_1:min_5))

#Simple Regression - generating summary dataframe
swallowtail_df_summary = swallowtail_df %>%
  mutate(year = year(date)) %>%
  group_by(year) %>%
  summarize(n = n(), 
            max_lat = max(latitude))

#plotting
ggplot(swallowtail_df_summary, aes(x = year, y = max_lat, size = n)) +
  geom_point(alpha = 0.5) +
  geom_smooth(data = swallowtail_df_summary %>%
                filter(year < 2000), method = "lm", color = "black", show.legend = FALSE) +
  geom_smooth(data = swallowtail_df_summary %>%
                filter(year > 2000), method = "lm", color = "black", show.legend = FALSE) +
  theme_bw() +
  labs(x = "Year", y = "Maximum Latitude (º)") +
  scale_color_discrete() +
  scale_size_continuous(breaks = c(5, 25, 100, 500, 1000))
  
#trying to mimic the map in Figure 1 from the manuscript
swallowtail_df_map = swallowtail_df %>%
  mutate(year = year(date), 
         time_frame = as.factor(ifelse(year >= 2000, 2, 1)))

qmap("west virgnia",zoom = 5, maptype = "toner-background") +
  geom_point(data = swallowtail_df_map, aes(x = longitude, y = latitude, color = time_frame), alpha = 0.5) +
  scale_color_discrete(name = "Time Frame", labels = c("Pre-2000", "Post-2000")) +
  labs(x = "Longitude (º) ", y = "Latitude (º)")

