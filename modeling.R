#Script to explore SDMs of Swallowtail and Ash
#Keaton Wilson
#keatonwilson@me.com
#2019-02-21

#libraries
library(tidyverse)
library(dismo)
library(caret)
library(lubridate)
library(rgdal)
library(sp)
library(raster)
library(maptools)
library(ggmap)
library(viridis)
library(ggthemes)
library(rgeos)
library(maps)

#register google api for mapping stuff
register_google(key = "AIzaSyDyAqUc4o9p_DOBSF_JOXH5c_JXPqoU4Yw")


#Pulling in data that has been cleaned in previous script
swallowtail = read_csv("./data/swallowtail_data.csv")
host_plant = read_csv("./data/host_plant_data.csv")

swallowtail = swallowtail %>%
  dplyr::select(-X1)

#Generating some features that will be useful later on
swallowtail = swallowtail %>%
  mutate(year = year(date), 
         time_frame = ifelse(year >= 2000, "T2", "T1")) %>%
  rename(latitude = Latitude, longitude = Longitude)
  
  #dplyr::select(-avg_min)

host_plant = host_plant %>%
  mutate(year = year(date), 
         time_frame = ifelse(year >= 2000, "T2", "T1"))
  #dplyr::select(-avg_min)

#Filtering the data to include stuff east of texas (94º), and in the US, Canada

lon_min = -94
lon_max = -65
lat_min = 25
lat_max = 50

swallowtail = swallowtail %>%
  filter(latitude >= lat_min & latitude <= lat_max) %>%
  filter(longitude >= lon_min & longitude <= lon_max)

host_plant = host_plant %>%
  filter(latitude >= lat_min & latitude <= lat_max) %>%
  filter(longitude >= lon_min & longitude <= lon_max)

#mapping to check
st = get_map("North Carolina", zoom = 4, maptype = "toner-background")
ggmap(st, maptype = "toner-background", extent = "panel") +
  geom_point(data = swallowtail, aes(x = longitude, y = latitude, color = time_frame), alpha = 0.5) +
  scale_color_discrete(name = "Time Frame", labels = c("Pre-2000", "Post-2000")) +
  labs(x = "Longitude (º) ", y = "Latitude (º)")  

#Let's deal with climate data
#From the manuscript - Climate data for northeast North American from  t1  and t2 at 10-km grid horizontal resolution were obtained from McKenney et al. (2006). We selected six bioclimatic variables (Table 1) to model species distributions. We averaged annual values to define the climatic conditions for t1 and t2 .

#Ok, so this gets a bit confusing, but... generally, we have a model that looks something like: 
#Occurence ~ location + time_frame + (bioclim1 : bioclim6)

#So the first big challenge is to import climate data and then associate each occurence point with each variable. 

#We can do this from the dismo package
bioclim.data <- raster::getData(name = "worldclim",
                        var = "bio",
                        res = 2.5,
                        path = "./data/")

# Determine geographic extent of our data
max_lat_swallowtail <- ceiling(max(swallowtail$latitude))
min_lat_swallowtail <- floor(min(swallowtail$latitude))
max_lon_swallowtail <- ceiling(max(swallowtail$longitude))
min_lon_swallowtail <- floor(min(swallowtail$longitude))
geographic.extent <- extent(x = c(min_lon_swallowtail, max_lon_swallowtail, min_lat_swallowtail, max_lat_swallowtail))

# Crop bioclim data to geographic extent of swallowtails
bioclim.data <- crop(x = bioclim.data, y = geographic.extent)

#Modifying data frame to feed into dismo bioclim() function
swallowtail_short = swallowtail %>%
  dplyr::select(longitude, latitude)

#bioclim model 
bc.model = dismo::bioclim(x = bioclim.data, p = swallowtail_short)
predict_presence = dismo::predict(object = bc.model, x = bioclim.data, ext = geographic.extent)

test_spdf <- as(predict_presence, "SpatialPixelsDataFrame")
test_df <- as.data.frame(test_spdf)
colnames(test_df) <- c("value", "x", "y")


#This works, but it... doesn't look great
data("wrld_simpl")
plot(wrld_simpl, 
     xlim = c(min_lon_swallowtail, max_lon_swallowtail),
     ylim = c(min_lat_swallowtail, max_lat_swallowtail),
     axes = TRUE, 
     col = "grey95")
plot(predict_presence, add = TRUE)
plot(wrld_simpl, add = TRUE, border = "grey5")

#Getting map data
usa = getData(country = 'USA', level = 1)

#extract states (need to uppercase everything)
to_remove = c("Alaska", "Hawaii", "North Dakota", "South Dakota", "Montana", 
             "Wyoming", "Idaho", "Washington", "Oregon", "Nevada", "California", 
             "Arizona", "Utah", "New Mexico", "Colorado", "Nebraska", "Texas", 
             "Oklahoma", "Kansas")

#filtering
mapping = usa[-match(toupper(to_remove), toupper(usa$NAME_1)),]

#simplying polygons
simple_map_US = gSimplify(mapping, tol = 0.01, topologyPreserve = TRUE)

#Pulling Canada Province data
can = getData(country = 'CAN', level = 1)
province = c("Ontario")
can_mapping = can[match(toupper(c("Ontario", "Québec")), toupper(can$NAME_1)),]
simple_map_can = gSimplify(can_mapping, tol = 0.01, topologyPreserve = TRUE)

#Testing to see what this looks like
ggplot() +
  geom_polygon(data = simple_map_US, aes(x = long, y = lat, group = group), color = "grey50", size = 0.25, fill = "#440154FF") +
geom_polygon(data = simple_map_can, aes(x = long, y = lat, group = group), color = "grey50", size = 0.25, fill = "#440154FF")


#Full plot with bioclim model
ggplot() +  
  geom_polygon(data=simple_map_US, aes(x=long, y=lat, group=group), 
               color=NA, size=0.25, fill = "#440154FF") +
  geom_polygon(data = simple_map_can, aes(x = long, y = lat, group = group), color = NA, size = 0.25, fill = "#440154FF") +
  geom_tile(data=test_df, aes(x=x, y=y, fill=value)) + 
  geom_polygon(data=simple_map_US, aes(x=long, y=lat, group=group), 
               color="grey50", size=0.25, fill = NA) +
  geom_polygon(data = simple_map_can, aes(x = long, y = lat, group = group), color = "grey50", size = 0.25, fill = NA) +
  scale_fill_viridis() +
  theme(legend.position="bottom") +
  theme(legend.key.width=unit(2, "cm")) +
  coord_equal(ylim = c(22, 50), xlim = c(-100, -65)) +
  theme_map()
  
#We can also compare distribution models of two time frames
#pre-2000
pre_2000 = swallowtail %>%
  filter(time_frame == "T1") %>%
  select(longitude, latitude)

post_2000 = swallowtail %>%
  filter(time_frame == "T2") %>%
  select(longitude, latitude)

bc_model_pre = dismo::bioclim(x = bioclim.data, p = pre_2000)
predict_presence_pre = dismo::predict(object = bc_model_pre, x = bioclim.data, ext = geographic.extent)

bc_model_post = dismo::bioclim(x = bioclim.data, p = post_2000)
predict_presence_post = dismo::predict(object = bc_model_post, x = bioclim.data, ext = geographic.extent)

test_spdf_pre <- as(predict_presence_pre, "SpatialPixelsDataFrame")
test_df_pre <- as.data.frame(test_spdf_pre)
colnames(test_df_pre) <- c("value", "x", "y")

test_spdf_post <- as(predict_presence_post, "SpatialPixelsDataFrame")
test_df_post <- as.data.frame(test_spdf_post)
colnames(test_df_post) <- c("value", "x", "y")

#Pre-2000 map
g1 = ggplot() +  
  geom_polygon(data=simple_map_US, aes(x=long, y=lat, group=group), 
               color=NA, size=0.25, fill = "#440154FF") +
  geom_polygon(data = simple_map_can, aes(x = long, y = lat, group = group), color = NA, size = 0.25, fill = "#440154FF") +
  geom_tile(data=test_df_pre, aes(x=x, y=y, fill=value)) + 
  geom_polygon(data=simple_map_US, aes(x=long, y=lat, group=group), 
               color="grey50", size=0.25, fill = NA) +
  geom_polygon(data = simple_map_can, aes(x = long, y = lat, group = group), color = "grey50", size = 0.25, fill = NA) +
  scale_fill_viridis() +
  theme(legend.position="bottom") +
  theme(legend.key.width=unit(2, "cm")) +
  coord_equal(ylim = c(22, 50), xlim = c(-100, -65)) +
  theme_map()

#Post-2000 map
g2 = ggplot() +  
  geom_polygon(data=simple_map_US, aes(x=long, y=lat, group=group), 
               color=NA, size=0.25, fill = "#440154FF") +
  geom_polygon(data = simple_map_can, aes(x = long, y = lat, group = group), color = NA, size = 0.25, fill = "#440154FF") +
  geom_tile(data=test_df_post, aes(x=x, y=y, fill=value)) + 
  geom_polygon(data=simple_map_US, aes(x=long, y=lat, group=group), 
               color="grey50", size=0.25, fill = NA) +
  geom_polygon(data = simple_map_can, aes(x = long, y = lat, group = group), color = "grey50", size = 0.25, fill = NA) +
  scale_fill_viridis() +
  theme(legend.position="bottom") +
  theme(legend.key.width=unit(2, "cm")) +
  coord_equal(ylim = c(22, 50), xlim = c(-100, -65)) +
  theme_map()

library(ggpubr)
#sidebyside

ggarrange(g1, g2, common.legend = TRUE, labels = c("1960-2000", "2000-2018"))

#-------------------------------------------------------------------------------
#MaxEnt

maxent = dismo::maxent(x = bioclim.data, p = as.matrix(pre_2000))
predict_presence_pre = dismo::predict(object = maxent, x = bioclim.data, ext = geographic.extent)

maxent_post = dismo::maxent(x = bioclim.data, p = as.matrix(post_2000))
predict_presence_post = dismo::predict(object = maxent_post, x = bioclim.data, ext = geographic.extent)

test_spdf_pre <- as(predict_presence_pre, "SpatialPixelsDataFrame")
test_df_pre <- as.data.frame(test_spdf_pre)
colnames(test_df_pre) <- c("value", "x", "y")

test_spdf_post <- as(predict_presence_post, "SpatialPixelsDataFrame")
test_df_post <- as.data.frame(test_spdf_post)
colnames(test_df_post) <- c("value", "x", "y")

g1 = ggplot() +  
  geom_polygon(data=simple_map_US, aes(x=long, y=lat, group=group), 
               color=NA, size=0.25, fill = "#440154FF") +
  geom_polygon(data = simple_map_can, aes(x = long, y = lat, group = group), color = NA, size = 0.25, fill = "#440154FF") +
  geom_tile(data=test_df_pre, aes(x=x, y=y, fill=value)) + 
  geom_polygon(data=simple_map_US, aes(x=long, y=lat, group=group), 
               color="grey50", size=0.25, fill = NA) +
  geom_polygon(data = simple_map_can, aes(x = long, y = lat, group = group), color = "grey50", size = 0.25, fill = NA) +
  scale_fill_viridis() +
  theme(legend.position="bottom") +
  theme(legend.key.width=unit(2, "cm")) +
  coord_equal(ylim = c(22, 50), xlim = c(-100, -65)) +
  theme_map()

g2 = ggplot() +  
  geom_polygon(data=simple_map_US, aes(x=long, y=lat, group=group), 
               color=NA, size=0.25, fill = "#440154FF") +
  geom_polygon(data = simple_map_can, aes(x = long, y = lat, group = group), color = NA, size = 0.25, fill = "#440154FF") +
  geom_tile(data=test_df_post, aes(x=x, y=y, fill=value)) + 
  geom_polygon(data=simple_map_US, aes(x=long, y=lat, group=group), 
               color="grey50", size=0.25, fill = NA) +
  geom_polygon(data = simple_map_can, aes(x = long, y = lat, group = group), color = "grey50", size = 0.25, fill = NA) +
  scale_fill_viridis() +
  theme(legend.position="bottom") +
  theme(legend.key.width=unit(2, "cm")) +
  coord_equal(ylim = c(22, 50), xlim = c(-100, -65)) +
  theme_map()

ggarrange(g1, g2, common.legend = TRUE, labels = c("1960-2000", "2000-2018"))

#Playing with maxent parameters
plot(maxent_post)
response(maxent_post)

#Model testing - right now it's using all the data. Way overfit - we'll need to take the Machine Learning approach of testing and training. The evaluate function also incorporates background data

#background data
bg = randomPoints(bioclim.data, 1000)
