#Maps and Figures from Full Maxent Models
#Keaton Wilson
#keatonwilson@me.com
#2019-03-25

#packages and libraries
library(tidyverse)
library(dismo)
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
library(ggpubr)
library(blockCV)
library(ENMeval)

#Loading in raw occurence data
swallowtail = read_csv("./data/swallowtail_data.csv")
swallowtail = swallowtail[,-1] %>%
  select(longitude, latitude, date, year, time_frame)

#hostplant
hostplant = read_csv("./data/hostplant_data.csv")
hostplant = hostplant[,-1]

#bioclim environmental variables
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

#Loading in model objects
mx_best_hp_t2 = readRDS("./models/full_best_hp_t2.rds")
mx_best_hp_t1 = readRDS("./models/full_best_hp_t1.rds")
mx_best_st_t1 = readRDS("./models/full_best_st_t1.rds")
mx_best_st_t2 = readRDS("./models/full_best_st_t2.rds")

# Building Predictions and Plotting ---------------------------------------

#Predictions from full model (Swallowtail T1)
predict_presence_st_t1 = dismo::predict(object = mx_best_st_t1, x = bioclim.data, ext = geographic.extent)

pred_sp_st_t1 <- as(predict_presence_st_t1, "SpatialPixelsDataFrame")
pred_sp_df_st_t1 <- as.data.frame(pred_sp_st_t1)
colnames(pred_sp_df_st_t1) <- c("value", "x", "y")

#Pulling in polygons for states and provinces
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

#Plotting - T1 First
g1 = ggplot() +  
  geom_polygon(data=simple_map_US, aes(x=long, y=lat, group=group), 
               color=NA, size=0.25, fill = "#440154FF") +
  geom_polygon(data = simple_map_can, aes(x = long, y = lat, group = group), color = NA, size = 0.25, fill = "#440154FF") +
  geom_tile(data=pred_sp_df_st_t1, aes(x=x, y=y, fill=value)) + 
  geom_polygon(data=simple_map_US, aes(x=long, y=lat, group=group), 
               color="grey50", size=0.25, fill = NA) +
  geom_polygon(data = simple_map_can, aes(x = long, y = lat, group = group), color = "grey50", size = 0.25, fill = NA) +
  scale_fill_viridis() +
  theme(legend.position="bottom") +
  theme(legend.key.width=unit(2, "cm")) +
  coord_equal(ylim = c(22, 50), xlim = c(-100, -65)) +
  theme_map() +
  ggtitle("1960-1999")

#Predictions from full model (Swallowtail T2)
predict_presence_st_t2 = dismo::predict(object = mx_best_st_t2, x = bioclim.data, ext = geographic.extent)

pred_sp_st_t2 <- as(predict_presence_st_t2, "SpatialPixelsDataFrame")
pred_sp_df_st_t2 <- as.data.frame(pred_sp_st_t2)
colnames(pred_sp_df_st_t2) <- c("value", "x", "y")

#Plotting Swallowtail T2
g2 = ggplot() +  
  geom_polygon(data=simple_map_US, aes(x=long, y=lat, group=group), 
               color=NA, size=0.25, fill = "#440154FF") +
  geom_polygon(data = simple_map_can, aes(x = long, y = lat, group = group), color = NA, size = 0.25, fill = "#440154FF") +
  geom_tile(data=pred_sp_df_st_t2, aes(x=x, y=y, fill=value)) + 
  geom_polygon(data=simple_map_US, aes(x=long, y=lat, group=group), 
               color="grey50", size=0.25, fill = NA) +
  geom_polygon(data = simple_map_can, aes(x = long, y = lat, group = group), color = "grey50", size = 0.25, fill = NA) +
  scale_fill_viridis() +
  theme(legend.position="bottom") +
  theme(legend.key.width=unit(2, "cm")) +
  coord_equal(ylim = c(22, 50), xlim = c(-100, -65)) +
  theme_map() +
  ggtitle("2000-2019")

maxent_raw_st = ggarrange(g1, g2, common.legend = TRUE)
maxent_raw_st

#Predictions from full model (Hostplant T1)
predict_presence_hp_t1 = dismo::predict(object = mx_best_hp_t1, x = bioclim.data, ext = geographic.extent)

pred_sp_hp_t1 <- as(predict_presence_hp_t1, "SpatialPixelsDataFrame")
pred_sp_df_hp_t1 <- as.data.frame(pred_sp_hp_t1)
colnames(pred_sp_df_hp_t1) <- c("value", "x", "y")


#Plotting 
g3 = ggplot() +  
  geom_polygon(data=simple_map_US, aes(x=long, y=lat, group=group), 
               color=NA, size=0.25, fill = "#440154FF") +
  geom_polygon(data = simple_map_can, aes(x = long, y = lat, group = group), color = NA, size = 0.25, fill = "#440154FF") +
  geom_tile(data=pred_sp_df_hp_t1, aes(x=x, y=y, fill=value)) + 
  geom_polygon(data=simple_map_US, aes(x=long, y=lat, group=group), 
               color="grey50", size=0.25, fill = NA) +
  geom_polygon(data = simple_map_can, aes(x = long, y = lat, group = group), color = "grey50", size = 0.25, fill = NA) +
  scale_fill_viridis() +
  theme(legend.position="bottom") +
  theme(legend.key.width=unit(2, "cm")) +
  coord_equal(ylim = c(22, 50), xlim = c(-100, -65)) +
  ggtitle("1960-1999") +
  theme_map()
  

#Predictions from full model (Swallowtail T2)
predict_presence_hp_t2 = dismo::predict(object = mx_best_hp_t2, x = bioclim.data, ext = geographic.extent)

pred_sp_hp_t2 <- as(predict_presence_hp_t2, "SpatialPixelsDataFrame")
pred_sp_df_hp_t2 <- as.data.frame(pred_sp_hp_t2)
colnames(pred_sp_df_hp_t2) <- c("value", "x", "y")

#Plotting Swallowtail T2
g4 = ggplot() +  
  geom_polygon(data=simple_map_US, aes(x=long, y=lat, group=group), 
               color=NA, size=0.25, fill = "#440154FF") +
  geom_polygon(data = simple_map_can, aes(x = long, y = lat, group = group), color = NA, size = 0.25, fill = "#440154FF") +
  geom_tile(data=pred_sp_df_hp_t2, aes(x=x, y=y, fill=value)) + 
  geom_polygon(data=simple_map_US, aes(x=long, y=lat, group=group), 
               color="grey50", size=0.25, fill = NA) +
  geom_polygon(data = simple_map_can, aes(x = long, y = lat, group = group), color = "grey50", size = 0.25, fill = NA) +
  scale_fill_viridis() +
  theme(legend.position="bottom") +
  theme(legend.key.width=unit(2, "cm")) +
  coord_equal(ylim = c(22, 50), xlim = c(-100, -65)) +
  ggtitle("2000-2019") +
  theme_map()
  
maxent_raw_hp = ggarrange(g3, g4, common.legend = TRUE)
maxent_raw_hp

# Threshold Maps and Density Figures --------------------------------------

#Threshold maps

#Evaluate on test data (mirrored from other script -will probably need to pull in all of these objects from "./scripts/modeling_big_script.R)
ev_st_t1 = evaluate(p_st_t1_test, a = bg_swallowtail_t1,  model = best_st_t1, x = bioclim.data)
ev_st_t2 = evaluate(p_st_t2_test, a = bg_swallowtail_t2,  model = best_st_t2, x = bioclim.data)
ev_hp_t1 = evaluate(p_hp_t1_test, a = bg_hostplant_t1,  model = best_hp_t1, x = bioclim.data)
ev_hp_t2 = evaluate(p_hp_t2_test, a = bg_hostplant_t2, model = best_hp_t2, x = bioclim.data)

#finding the threshold for presence/absence for each model
st_t1_threshold = threshold(ev_st_t1, 'spec_sens')
st_t2_threshold = threshold(ev_st_t2, 'spec_sens')
hp_t1_threshold = threshold(ev_hp_t1, 'spec_sens')
hp_t2_threshold = threshold(ev_hp_t2, 'spec_sens')

#building filtered dataframes of predictions
st_t1_threshold = pred_sp_df_st_t1 %>%
  filter(value > st_t1_threshold)

st_t2_threshold = pred_sp_df_st_t2 %>%
  filter(value > st_t2_threshold)

hp_t1_threshold = pred_sp_df_hp_t1 %>%
  filter(value > hp_t1_threshold)

hp_t2_threshold = pred_sp_df_hp_t2 %>%
  filter(value > hp_t2_threshold)

#binding
threshold_df_st = bind_rows("t1" = st_t1_threshold, "t2" = st_t2_threshold, .id = "timeframe")
threshold_df_hp = bind_rows("t1" = hp_t1_threshold, "t2" = hp_t2_threshold, .id = "timeframe")

#plotting st
g5 = ggplot(threshold_df_st, aes(x = y, fill = timeframe)) +
  geom_density(alpha = 0.8) +
  theme_classic() +
  labs(x = "Latitude", y = "Kernel Density Estimate") +
  scale_fill_discrete(name = "Time Frame", labels = c("1960-1999", "2000-2019")) +
  ggtitle("Swallowtail")

#plotting hp
g6 = ggplot(threshold_df_hp, aes(x = y, fill = timeframe)) +
  geom_density(alpha = 0.8) +
  theme_classic() +
  labs(x = "Latitude", y = "Kernel Density Estimate") +
  scale_fill_discrete(name = "Time Frame", labels = c("1960-1999", "2000-2019")) +
  ggtitle("Hostplant")

histograms_plot = ggarrange(g5, g6, common.legend = TRUE)
histograms_plot

#threshold maps
#Swallowtail T1
g7 = ggplot() +  
  geom_polygon(data=simple_map_US, aes(x=long, y=lat, group=group), 
               color=NA, size=0.25, fill = "grey10") +
  geom_polygon(data = simple_map_can, aes(x = long, y = lat, group = group), color = NA, size = 0.25, fill = "grey10") +
  geom_tile(data = st_t1_threshold, aes(x=x, y=y), fill = "lightgrey") + 
  geom_polygon(data=simple_map_US, aes(x=long, y=lat, group=group), 
               color="grey75", size=0.25, fill = NA) +
  geom_polygon(data = simple_map_can, aes(x = long, y = lat, group = group), color = "grey50", size = 0.25, fill = NA) +
  geom_point(data = swallowtail_t1, aes(x = longitude, y = latitude), alpha = 0.5, color = "yellow", shape = 3) +
  theme(legend.position="bottom") +
  theme(legend.key.width=unit(2, "cm")) +
  coord_equal(ylim = c(22, 50), xlim = c(-100, -65)) +
  theme_map() +
  ggtitle("1960-1999")

#T2
g8 = ggplot() +  
  geom_polygon(data=simple_map_US, aes(x=long, y=lat, group=group), 
               color=NA, size=0.25, fill = "grey10") +
  geom_polygon(data = simple_map_can, aes(x = long, y = lat, group = group), color = NA, size = 0.25, fill = "grey10") +
  geom_tile(data=st_t2_threshold, aes(x=x, y=y), fill = "lightgrey") + 
  geom_polygon(data=simple_map_US, aes(x=long, y=lat, group=group), 
               color="grey75", size=0.25, fill = NA) +
  geom_polygon(data = simple_map_can, aes(x = long, y = lat, group = group), color = "grey50", size = 0.25, fill = NA) +
  geom_point(data = swallowtail_t2, aes(x = longitude, y = latitude), alpha = 0.2, color = "yellow", shape = 3) +
  theme(legend.position="bottom") +
  theme(legend.key.width=unit(2, "cm")) +
  coord_equal(ylim = c(22, 50), xlim = c(-100, -65)) +
  theme_map() +
  ggtitle("2000-2019")

maxent_th_st = ggarrange(g7, g8, common.legend = TRUE)
maxent_th_st
