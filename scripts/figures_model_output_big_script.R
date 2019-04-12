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
library(ggridges)

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
mx_best_st_t1 = readRDS("./models/full_best_st_t1.rds")
mx_best_st_t2 = readRDS("./models/full_best_st_t2.rds")
mx_best_hp_1_t2 = readRDS("./models/full_best_hp_1_t2.rds")
mx_best_hp_1_t1 = readRDS("./models/full_best_hp_1_t1.rds")
mx_best_hp_2_t2 = readRDS("./models/full_best_hp_2_t2.rds")
mx_best_hp_2_t1 = readRDS("./models/full_best_hp_2_t1.rds")
mx_best_hp_3_t2 = readRDS("./models/full_best_hp_3_t2.rds")
mx_best_hp_3_t1 = readRDS("./models/full_best_hp_3_t1.rds")

# Building Predictions and Plotting ---------------------------------------

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
can_mapping = can[match(toupper(c("Ontario", "Québec", "New Brunswick", "Prince Edward Island", "Nova Scotia")), toupper(can$NAME_1)),]
simple_map_can = gSimplify(can_mapping, tol = 0.01, topologyPreserve = TRUE)

#Great lakes issues
lakes <- rgdal::readOGR("./data/10m_physical/ne_10m_lakes.shp")
lakes = lakes[lakes$scalerank==0,]
lakes = crop(lakes, geographic.extent)


#Testing geographic polygons
ggplot() +  
  geom_polygon(data=simple_map_US, aes(x=long, y=lat, group=group), 
               color=NA, size=0.25, fill = "#440154FF") +
  geom_polygon(data = simple_map_can, aes(x = long, y = lat, group = group), color = NA, size = 0.25, fill = "#440154FF") +
  geom_polygon(data=simple_map_US, aes(x=long, y=lat, group=group), 
               color="grey50", size=0.25, fill = NA) +
  geom_polygon(data = simple_map_can, aes(x = long, y = lat, group = group), color = "grey50", size = 0.25, fill = NA) +
  geom_polygon(data = lakes, aes(x = long, y = lat, group = group), fill = "white", size = 0.25, fill = NA)

#Predictions from full model (Swallowtail T1)
predict_presence_st_t1 = dismo::predict(object = mx_best_st_t1, x = bioclim.data, ext = geographic.extent)

pred_sp_st_t1 <- as(predict_presence_st_t1, "SpatialPixelsDataFrame")
pred_sp_df_st_t1 <- as.data.frame(pred_sp_st_t1)
colnames(pred_sp_df_st_t1) <- c("value", "x", "y")


#Predictions from full model (Swallowtail T2)
predict_presence_st_t2 = dismo::predict(object = mx_best_st_t2, x = bioclim.data, ext = geographic.extent)

pred_sp_st_t2 <- as(predict_presence_st_t2, "SpatialPixelsDataFrame")
pred_sp_df_st_t2 <- as.data.frame(pred_sp_st_t2)
colnames(pred_sp_df_st_t2) <- c("value", "x", "y")

#Plotting - T1 First
g1 = ggplot() +  
  geom_polygon(data=simple_map_US, aes(x=long, y=lat, group=group), 
               color=NA, size=0.25, fill = "#440154FF") +
  geom_polygon(data = simple_map_can, aes(x = long, y = lat, group = group), color = NA, size = 0.25, fill = "#440154FF") +
  geom_tile(data=pred_sp_df_st_t1, aes(x=x, y=y, fill=value)) + 
  geom_polygon(data=simple_map_US, aes(x=long, y=lat, group=group), 
               color="grey50", size=0.25, fill = NA) +
  geom_polygon(data = simple_map_can, aes(x = long, y = lat, group = group), color = "grey50", size = 0.25, fill = NA) +
  geom_polygon(data = lakes, aes(x = long, y = lat, group = group), fill = "white", size = 0.25) +
  scale_fill_viridis(name = "Probability of Occurence") +
  theme(legend.position="bottom") +
  theme(legend.key.width=unit(2, "cm"),
        plot.title = element_text(hjust = 0.5, size = 24)) +
  #coord_equal(ylim = c(22, 50), xlim = c(-100, -65)) +
  theme_nothing(legend = TRUE) +
  ggtitle("1960 - 1999") +
  coord_quickmap()


#Plotting Swallowtail T2
g2 = ggplot() +  
  geom_polygon(data=simple_map_US, aes(x=long, y=lat, group=group), 
               color=NA, size=0.25, fill = "#440154FF") +
  geom_polygon(data = simple_map_can, aes(x = long, y = lat, group = group), color = NA, size = 0.25, fill = "#440154FF") +
  geom_tile(data=pred_sp_df_st_t2, aes(x=x, y=y, fill=value)) + 
  geom_polygon(data=simple_map_US, aes(x=long, y=lat, group=group), 
               color="grey50", size=0.25, fill = NA) +
  geom_polygon(data = simple_map_can, aes(x = long, y = lat, group = group), color = "grey50", size = 0.25, fill = NA) +
  geom_polygon(data = lakes, aes(x = long, y = lat, group = group), fill = "white", size = 0.25) +
  scale_fill_viridis(name = "Probability of Occurence") +
  theme(legend.position="bottom") +
  theme(legend.key.width=unit(2, "cm"),
        plot.title = element_text(hjust = 0.5, size = 24)) +
  theme_nothing(legend = TRUE) +
  ggtitle("2000 - 2019") +
  coord_quickmap()

maxent_raw_st = ggarrange(g1, g2, common.legend = TRUE)
maxent_raw_st

ggsave(plot = maxent_raw_st, filename = "./output/swallowtail_maxent_raw.png", device = "png")

#Predictions from full model (Hostplant 1 T1)
predict_presence_hp_1_t1 = dismo::predict(object = mx_best_hp_1_t1, x = bioclim.data, ext = geographic.extent)

pred_sp_hp_1_t1 <- as(predict_presence_hp_1_t1, "SpatialPixelsDataFrame")
pred_sp_df_hp_1_t1 <- as.data.frame(pred_sp_hp_1_t1)
colnames(pred_sp_df_hp_1_t1) <- c("value", "x", "y")

#Predictions from full model (Hostplant T2)
predict_presence_hp_1_t2 = dismo::predict(object = mx_best_hp_1_t2, x = bioclim.data, ext = geographic.extent)

pred_sp_hp_1_t2 <- as(predict_presence_hp_1_t2, "SpatialPixelsDataFrame")
pred_sp_df_hp_1_t2 <- as.data.frame(pred_sp_hp_1_t2)
colnames(pred_sp_df_hp_1_t2) <- c("value", "x", "y")

#Plotting 
g3 = ggplot() +  
  geom_polygon(data=simple_map_US, aes(x=long, y=lat, group=group), 
               color=NA, size=0.25, fill = "#440154FF") +
  geom_polygon(data = simple_map_can, aes(x = long, y = lat, group = group), color = NA, size = 0.25, fill = "#440154FF") +
  geom_tile(data=pred_sp_df_hp_1_t1, aes(x=x, y=y, fill=value)) + 
  geom_polygon(data=simple_map_US, aes(x=long, y=lat, group=group), 
               color="grey50", size=0.25, fill = NA) +
  geom_polygon(data = simple_map_can, aes(x = long, y = lat, group = group), color = "grey50", size = 0.25, fill = NA) +
  geom_polygon(data = lakes, aes(x = long, y = lat, group = group), fill = "white", size = 0.25) +
  scale_fill_viridis(name = "Probability of Occurence") +
  theme(legend.position="bottom") +
  theme(legend.key.width=unit(2, "cm"),
        plot.title = element_text(hjust = 0.5, size = 24)) +
  theme_nothing(legend = TRUE) +
  ggtitle("1959 - 1999") +
  coord_quickmap()
  


#Plotting Swallowtail T2
g4 = ggplot() +  
  geom_polygon(data=simple_map_US, aes(x=long, y=lat, group=group), 
               color=NA, size=0.25, fill = "#440154FF") +
  geom_polygon(data = simple_map_can, aes(x = long, y = lat, group = group), color = NA, size = 0.25, fill = "#440154FF") +
  geom_tile(data=pred_sp_df_hp_1_t2, aes(x=x, y=y, fill=value)) + 
  geom_polygon(data=simple_map_US, aes(x=long, y=lat, group=group), 
               color="grey50", size=0.25, fill = NA) +
  geom_polygon(data = simple_map_can, aes(x = long, y = lat, group = group), color = "grey50", size = 0.25, fill = NA) +
  geom_polygon(data = lakes, aes(x = long, y = lat, group = group), fill = "white", size = 0.25) +
  scale_fill_viridis(name = "Probability of Occurence") +
  theme(legend.position="bottom") +
  theme(legend.key.width=unit(2, "cm"),
        plot.title = element_text(hjust = 0.5, size = 24)) +
  theme_nothing(legend = TRUE) +
  ggtitle("2000 - 2019") +
  coord_quickmap()

maxent_raw_hp_1 = ggarrange(g3, g4, common.legend = TRUE)
maxent_raw_hp_1

ggsave(plot = maxent_raw_hp_1, filename = "./output/hostplant_1_maxent_raw.png", device = "png")

#Predictions from full model (Hostplant 2 T1)
predict_presence_hp_2_t1 = dismo::predict(object = mx_best_hp_2_t1, x = bioclim.data, ext = geographic.extent)

pred_sp_hp_2_t1 <- as(predict_presence_hp_2_t1, "SpatialPixelsDataFrame")
pred_sp_df_hp_2_t1 <- as.data.frame(pred_sp_hp_2_t1)
colnames(pred_sp_df_hp_2_t1) <- c("value", "x", "y")

#Predictions from full model (Hostplant 2 T2)
predict_presence_hp_2_t2 = dismo::predict(object = mx_best_hp_2_t2, x = bioclim.data, ext = geographic.extent)

pred_sp_hp_2_t2 <- as(predict_presence_hp_2_t2, "SpatialPixelsDataFrame")
pred_sp_df_hp_2_t2 <- as.data.frame(pred_sp_hp_2_t2)
colnames(pred_sp_df_hp_2_t2) <- c("value", "x", "y")

#Plotting hostplant 2 t1
g5 = ggplot() +  
  geom_polygon(data=simple_map_US, aes(x=long, y=lat, group=group), 
               color=NA, size=0.25, fill = "#440154FF") +
  geom_polygon(data = simple_map_can, aes(x = long, y = lat, group = group), color = NA, size = 0.25, fill = "#440154FF") +
  geom_tile(data=pred_sp_df_hp_2_t1, aes(x=x, y=y, fill=value)) + 
  geom_polygon(data=simple_map_US, aes(x=long, y=lat, group=group), 
               color="grey50", size=0.25, fill = NA) +
  geom_polygon(data = simple_map_can, aes(x = long, y = lat, group = group), color = "grey50", size = 0.25, fill = NA) +
  geom_polygon(data = lakes, aes(x = long, y = lat, group = group), fill = "white", size = 0.25) +
  scale_fill_viridis(name = "Probability of Occurence") +
  theme(legend.position="bottom") +
  theme(legend.key.width=unit(2, "cm"),
        plot.title = element_text(hjust = 0.5, size = 24)) +
  theme_nothing(legend = TRUE) +
  ggtitle("1959 - 1999") +
  coord_quickmap()



#Plotting hostplant 2 T2
g6 = ggplot() +  
  geom_polygon(data=simple_map_US, aes(x=long, y=lat, group=group), 
               color=NA, size=0.25, fill = "#440154FF") +
  geom_polygon(data = simple_map_can, aes(x = long, y = lat, group = group), color = NA, size = 0.25, fill = "#440154FF") +
  geom_tile(data=pred_sp_df_hp_2_t2, aes(x=x, y=y, fill=value)) + 
  geom_polygon(data=simple_map_US, aes(x=long, y=lat, group=group), 
               color="grey50", size=0.25, fill = NA) +
  geom_polygon(data = simple_map_can, aes(x = long, y = lat, group = group), color = "grey50", size = 0.25, fill = NA) +
  geom_polygon(data = lakes, aes(x = long, y = lat, group = group), fill = "white", size = 0.25) +
  scale_fill_viridis(name = "Probability of Occurence") +
  theme(legend.position="bottom") +
  theme(legend.key.width=unit(2, "cm"),
        plot.title = element_text(hjust = 0.5, size = 24)) +
  theme_nothing(legend = TRUE) +
  ggtitle("2000 - 2019") +
  coord_quickmap()

maxent_raw_hp_2 = ggarrange(g5, g6, common.legend = TRUE)
maxent_raw_hp_2

ggsave(plot = maxent_raw_hp_2, filename = "./output/hostplant_2_maxent_raw.png", device = "png")

#Predictions from full model (Hostplant 3 T1)
predict_presence_hp_3_t1 = dismo::predict(object = mx_best_hp_3_t1, x = bioclim.data, ext = geographic.extent)

pred_sp_hp_3_t1 <- as(predict_presence_hp_3_t1, "SpatialPixelsDataFrame")
pred_sp_df_hp_3_t1 <- as.data.frame(pred_sp_hp_3_t1)
colnames(pred_sp_df_hp_3_t1) <- c("value", "x", "y")

#Predictions from full model (Hostplant 3 T2)
predict_presence_hp_3_t2 = dismo::predict(object = mx_best_hp_3_t2, x = bioclim.data, ext = geographic.extent)

pred_sp_hp_3_t2 <- as(predict_presence_hp_3_t2, "SpatialPixelsDataFrame")
pred_sp_df_hp_3_t2 <- as.data.frame(pred_sp_hp_3_t2)
colnames(pred_sp_df_hp_3_t2) <- c("value", "x", "y")

#Plotting 
g7 = ggplot() +  
  geom_polygon(data=simple_map_US, aes(x=long, y=lat, group=group), 
               color=NA, size=0.25, fill = "#440154FF") +
  geom_polygon(data = simple_map_can, aes(x = long, y = lat, group = group), color = NA, size = 0.25, fill = "#440154FF") +
  geom_tile(data=pred_sp_df_hp_3_t1, aes(x=x, y=y, fill=value)) + 
  geom_polygon(data=simple_map_US, aes(x=long, y=lat, group=group), 
               color="grey50", size=0.25, fill = NA) +
  geom_polygon(data = simple_map_can, aes(x = long, y = lat, group = group), color = "grey50", size = 0.25, fill = NA) +
  geom_polygon(data = lakes, aes(x = long, y = lat, group = group), fill = "white", size = 0.25) +
  scale_fill_viridis(name = "Probability of Occurence") +
  theme(legend.position="bottom") +
  theme(legend.key.width=unit(2, "cm"),
        plot.title = element_text(hjust = 0.5, size = 24)) +
  theme_nothing(legend = TRUE) +
  ggtitle("1959 - 1999") +
  coord_quickmap()



#Plotting Swallowtail T2
g8 = ggplot() +  
  geom_polygon(data=simple_map_US, aes(x=long, y=lat, group=group), 
               color=NA, size=0.25, fill = "#440154FF") +
  geom_polygon(data = simple_map_can, aes(x = long, y = lat, group = group), color = NA, size = 0.25, fill = "#440154FF") +
  geom_tile(data=pred_sp_df_hp_3_t2, aes(x=x, y=y, fill=value)) + 
  geom_polygon(data=simple_map_US, aes(x=long, y=lat, group=group), 
               color="grey50", size=0.25, fill = NA) +
  geom_polygon(data = simple_map_can, aes(x = long, y = lat, group = group), color = "grey50", size = 0.25, fill = NA) +
  geom_polygon(data = lakes, aes(x = long, y = lat, group = group), fill = "white", size = 0.25) +
  scale_fill_viridis(name = "Probability of Occurence") +
  theme(legend.position="bottom") +
  theme(legend.key.width=unit(2, "cm"),
        plot.title = element_text(hjust = 0.5, size = 24)) +
  theme_nothing(legend = TRUE) +
  ggtitle("2000 - 2019") +
  coord_quickmap()

maxent_raw_hp_3 = ggarrange(g7, g8, common.legend = TRUE)
maxent_raw_hp_3

ggsave(plot = maxent_raw_hp_3, filename = "./output/hostplant_3_maxent_raw.png", device = "png")

# Threshold Maps and Density Figures --------------------------------------

#Threshold maps

#Loading the evaluate objects from the model building script

ev_st_t1 = readRDS("./data/ev_st_t1.RDS")
ev_st_t2 = readRDS("./data/ev_st_t2.RDS")
ev_hp_1_t1 = readRDS("./data/ev_hp_1_t1.RDS")
ev_hp_1_t2 = readRDS("./data/ev_hp_1_t2.RDS")
ev_hp_2_t1 = readRDS("./data/ev_hp_2_t1.RDS")
ev_hp_2_t2 = readRDS("./data/ev_hp_2_t2.RDS")
ev_hp_3_t1 = readRDS("./data/ev_hp_3_t1.RDS")
ev_hp_3_t2 = readRDS("./data/ev_hp_3_t2.RDS")

#finding the threshold for presence/absence for each model
st_t1_threshold = threshold(ev_st_t1, 'spec_sens')
st_t2_threshold = threshold(ev_st_t2, 'spec_sens')
hp_1_t1_threshold = threshold(ev_hp_1_t1, 'spec_sens')
hp_1_t2_threshold = threshold(ev_hp_1_t2, 'spec_sens')
hp_2_t1_threshold = threshold(ev_hp_2_t1, 'spec_sens')
hp_2_t2_threshold = threshold(ev_hp_2_t2, 'spec_sens')
hp_3_t1_threshold = threshold(ev_hp_3_t1, 'spec_sens')
hp_3_t2_threshold = threshold(ev_hp_3_t2, 'spec_sens')

#building filtered dataframes of predictions
st_t1_threshold = pred_sp_df_st_t1 %>%
  filter(value > st_t1_threshold)

st_t2_threshold = pred_sp_df_st_t2 %>%
  filter(value > st_t2_threshold)

hp_1_t1_threshold = pred_sp_df_hp_1_t1 %>%
  filter(value > hp_1_t1_threshold)

hp_1_t2_threshold = pred_sp_df_hp_1_t2 %>%
  filter(value > hp_1_t2_threshold)

hp_2_t1_threshold = pred_sp_df_hp_2_t1 %>%
  filter(value > hp_2_t1_threshold)

hp_2_t2_threshold = pred_sp_df_hp_2_t2 %>%
  filter(value > hp_2_t2_threshold)

hp_3_t1_threshold = pred_sp_df_hp_3_t1 %>%
  filter(value > hp_3_t1_threshold)

hp_3_t2_threshold = pred_sp_df_hp_3_t2 %>%
  filter(value > hp_3_t2_threshold)

#binding
threshold_df_st = bind_rows("t1" = st_t1_threshold, "t2" = st_t2_threshold, .id = "timeframe")
threshold_df_hp_1 = bind_rows("t1" = hp_1_t1_threshold, "t2" = hp_1_t2_threshold, .id = "timeframe")
threshold_df_hp_2 = bind_rows("t1" = hp_2_t1_threshold, "t2" = hp_2_t2_threshold, .id = "timeframe")
threshold_df_hp_3 = bind_rows("t1" = hp_3_t1_threshold, "t2" = hp_3_t2_threshold, .id = "timeframe")

#plotting st
g9 = ggplot(threshold_df_st, aes(x = y, fill = timeframe)) +
  geom_density(alpha = 0.8) +
  theme_classic() +
  labs(x = "Latitude", y = "Kernel Density Estimate") +
  scale_fill_discrete(name = "Time Frame", labels = c("1960-1999", "2000-2019")) +
  ggtitle("Papilio cresphontes") +
  xlim(c(25,50)) +
  theme(title = element_text(face = "italic"))

#plotting hp 1
g10 = ggplot(threshold_df_hp_1, aes(x = y, fill = timeframe)) +
  geom_density(alpha = 0.8) +
  theme_classic() +
  labs(x = "Latitude", y = "Kernel Density Estimate") +
  scale_fill_discrete(name = "Time Frame", labels = c("1960-1999", "2000-2019")) +
  ggtitle("Zanthoxylum americanum") +
  xlim(c(25,50)) +
  theme(title = element_text(face = "italic"))

g11 = ggplot(threshold_df_hp_2, aes(x = y, fill = timeframe)) +
  geom_density(alpha = 0.8) +
  theme_classic() +
  labs(x = "Latitude", y = "Kernel Density Estimate") +
  scale_fill_discrete(name = "Time Frame", labels = c("1960-1999", "2000-2019")) +
  ggtitle("Zanthoxylum clava-herculis") +
  xlim(c(25,50)) +
  theme(title = element_text(face = "italic"))

g12 = ggplot(threshold_df_hp_3, aes(x = y, fill = timeframe)) +
  geom_density(alpha = 0.8) +
  theme_classic() +
  labs(x = "Latitude", y = "Kernel Density Estimate") +
  scale_fill_discrete(name = "Time Frame", labels = c("1960-1999", "2000-2019")) +
  ggtitle("Ptelea trifoliata") +
  xlim(c(25,50)) +
  theme(title = element_text(face = "italic"))


histograms_plot = ggarrange(g9,g10,g11,g12, common.legend = TRUE, nrow = 4)
histograms_plot

ggsave(plot = histograms_plot, filename = "./output/swallowtail_density_plot.png", device = "png")

swallowtail_t1 = swallowtail %>%
  filter(time_frame == "T1")

swallowtail_t2 = swallowtail %>%
  filter(time_frame == "T2")
#threshold maps
#Swallowtail T1
g13 = ggplot() +  
  geom_polygon(data=simple_map_US, aes(x=long, y=lat, group=group), 
               color=NA, size=0.25, fill = "grey10") +
  geom_polygon(data = simple_map_can, aes(x = long, y = lat, group = group), color = NA, size = 0.25, fill = "grey10") +
  geom_tile(data = st_t1_threshold, aes(x=x, y=y), fill = "lightgrey") + 
  geom_polygon(data=simple_map_US, aes(x=long, y=lat, group=group), 
               color="grey75", size=0.25, fill = NA) +
  geom_polygon(data = simple_map_can, aes(x = long, y = lat, group = group), color = "grey50", size = 0.25, fill = NA) +
  geom_point(data = swallowtail_t1, aes(x = longitude, y = latitude), alpha = 0.5, color = "yellow", shape = 3) +
  geom_polygon(data = lakes, aes(x = long, y = lat, group = group), fill = "white", size = 0.25) +
  theme(legend.position="bottom") +
  theme(legend.key.width=unit(2, "cm")) +
  theme_nothing(legend = TRUE) +
  ggtitle("1960-1999") +
  coord_quickmap()

#T2
g14 = ggplot() +  
  geom_polygon(data=simple_map_US, aes(x=long, y=lat, group=group), 
               color=NA, size=0.25, fill = "grey10") +
  geom_polygon(data = simple_map_can, aes(x = long, y = lat, group = group), color = NA, size = 0.25, fill = "grey10") +
  geom_tile(data=st_t2_threshold, aes(x=x, y=y), fill = "lightgrey") + 
  geom_polygon(data=simple_map_US, aes(x=long, y=lat, group=group), 
               color="grey75", size=0.25, fill = NA) +
  geom_polygon(data = simple_map_can, aes(x = long, y = lat, group = group), color = "grey50", size = 0.25, fill = NA) +
  geom_point(data = swallowtail_t2, aes(x = longitude, y = latitude), alpha = 0.2, color = "yellow", shape = 3) +
  geom_polygon(data = lakes, aes(x = long, y = lat, group = group), fill = "white", size = 0.25) +
  theme(legend.position="bottom") +
  theme(legend.key.width=unit(2, "cm")) +
  theme_nothing(legend = TRUE) +
  ggtitle("2000-2019") +
  coord_quickmap()

maxent_th_st = ggarrange(g13, g14, common.legend = TRUE)
maxent_th_st

ggsave(plot = maxent_th_st, filename = "./output/swallowtail_threshold_occurence.png", device = "png")
#Environmental Variable Importance

#Swallowtail time-frame 1
df = var.importance(mx_best_st_t1)
df$variable = factor(df$variable, levels = c("bio1", "bio2", "bio3", "bio4", "bio5", "bio6", "bio7",
                                             "bio8", "bio9", "bio10", "bio11", "bio12", "bio13", 
                                             "bio14", "bio15", "bio16", "bio17", "bio18", "bio19"))
env_plot_1 = ggplot(df, aes(x = variable, y = percent.contribution)) +
  geom_col() +
  theme_classic() +
  labs(x = "Environmental Variable", 
       y = "Percent Contribution") +
  ggtitle("Swallowtail T1") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Swallowtail time-frame 2
df = var.importance(mx_best_st_t2)
df$variable = factor(df$variable, levels = c("bio1", "bio2", "bio3", "bio4", "bio5", "bio6", "bio7",
                                             "bio8", "bio9", "bio10", "bio11", "bio12", "bio13", 
                                             "bio14", "bio15", "bio16", "bio17", "bio18", "bio19"))
env_plot_2 = ggplot(df, aes(x = variable, y = percent.contribution)) +
  geom_col() +
  theme_classic() +
  labs(x = "Environmental Variable", 
       y = "Percent Contribution") +
  ggtitle("Swallowtail T2") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#hostplant time-frame 1 - need to do these for all host plants
df = var.importance(mx_best_hp_t1)
df$variable = factor(df$variable, levels = c("bio1", "bio2", "bio3", "bio4", "bio5", "bio6", "bio7",
                                             "bio8", "bio9", "bio10", "bio11", "bio12", "bio13", 
                                             "bio14", "bio15", "bio16", "bio17", "bio18", "bio19"))
env_plot_3 = ggplot(df, aes(x = variable, y = percent.contribution)) +
  geom_col() +
  theme_classic() +
  labs(x = "Environmental Variable", 
       y = "Percent Contribution") +
  ggtitle("Hostplant T1") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#hostplant time-frame 2
df = var.importance(mx_best_hp_t2)
df$variable = factor(df$variable, levels = c("bio1", "bio2", "bio3", "bio4", "bio5", "bio6", "bio7",
                                             "bio8", "bio9", "bio10", "bio11", "bio12", "bio13", 
                                             "bio14", "bio15", "bio16", "bio17", "bio18", "bio19"))
env_plot_4 = ggplot(df, aes(x = variable, y = percent.contribution)) +
  geom_col() +
  theme_classic() +
  labs(x = "Environmental Variable", 
       y = "Percent Contribution") +
  ggtitle("Hostplant T2") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

env_plot = ggarrange(env_plot_1, env_plot_2, env_plot_3, env_plot_4, common.legend = TRUE)
env_plot

ggsave(plot = env_plot, filename = "./output/environmental_contribution.png", device = "png")

#Maximum latitude by year figure
swallowtail_inset = swallowtail %>%
  group_by(year) %>%
  summarize(max_lat = max(latitude), 
            n = n()) %>%
  filter(max_lat > 35) #Filtering - there are some weird years that only have a few records at really low lattitudes.

g11 = ggplot(data = swallowtail_inset, aes(x = year, y = max_lat, size = n)) +
  geom_point(alpha = 0.8) +
  # geom_smooth(data = swallowtail_inset %>%
  #               filter(year < 2000), aes(x = year, y = max_lat), method = "lm", show.legend = FALSE) +
  # geom_smooth(data = swallowtail_inset %>%
  #               filter(year >= 2000), aes(x = year, y = max_lat), method = "lm", show.legend = FALSE) +
    geom_smooth(data = swallowtail_inset, show.legend = FALSE) +
  theme_classic() +
  scale_size_continuous(name = "Number of Observations") +
  labs(x = "Year", y = "Maximum Latitude (º)") +
  geom_vline(xintercept = 2000, lty = 2) +
  annotate(geom = "text", label = "Timeframe Break Point", x = 1994, y = 47.5)

g11

ggsave(plot = g11, filename = "./output/max_swallowtail_lat_by_year.png", device = "png")

#Ridgeplot
years = swallowtail %>%
  group_by(year) %>%
  summarize(n = n()) %>%
  filter(n > 5) %>%
  filter(year != 2019) %>%
  select(year) %>%
  pull()

#Ridge plot of occurence data - with city references
#Quebec City - 46.8139
#Ottawa  - 45.4215
#Toronto - 43.6532
#Detroit - 42.3314
#Indianapolis - 39.7684
ridge_plot = swallowtail %>%
  filter(year %in% years) %>%
ggplot(aes(y = factor(year), x = latitude)) +
  geom_density_ridges(scale = 4) +
  geom_linerange(x = 46.8139, ymin = 1, ymax = 38, lty = 2) +
  geom_linerange(x = 45.4215, ymin = 1, ymax = 39, lty = 2) +
  geom_linerange(x= 43.6532, ymin = 1, ymax = 40, lty = 2) +
  geom_linerange(x = 39.7684, ymin = 1, ymax = 37, lty = 2) +
  theme_classic() +
  annotate(geom = "text", 
           label = "Indianapolis", 
           x = 39.7684,
           y = 38.5) +
  annotate(geom = "text", 
           label = "Toronto", 
           x = 43.6532,
           y = 41) +
  annotate(geom = "text", 
           label = "Ottawa", 
           x = 45.4215,
           y = 39.5) +
  annotate(geom = "text", 
           label = "Quebec City", 
           x = 48.5,
           y = 39) +
  coord_cartesian(ylim = c(0,41)) +
  xlab("Latitude (º)") +
  ylab("Year") +
  theme(text = element_text(size = 18))
  
ggsave(plot = ridge_plot, filename ="./output/ridge_plot.png", device = "png")
                      

#Stacked Threshold map for hostplants
#T1

hostplant_clean = 
  hostplant %>%
  mutate(name = ifelse(str_detect(name, "Zanthoxylum americanum"), "Zanthoxylum americanum",
                       ifelse(str_detect(name, "Zanthoxylum clava-herculis"), "Zanthoxylum claca-herculis",
                              "Ptelea trifoliata")))
hp_thresholds_df = 
  bind_rows(hp_1_t1_threshold, hp_2_t1_threshold, hp_3_t1_threshold, .id = "id") %>%
  mutate(name = ifelse(id == 1, "Zanthoxylum americanum",
                       ifelse(id == 2, "Zanthoxylum clava-herculis", "Ptelea trifoliata"))) %>%
  select(-id)


g13 = ggplot() +  
  geom_polygon(data=simple_map_US, aes(x=long, y=lat, group=group), 
               color=NA, size=0.25, fill = "grey10") +
  geom_polygon(data = simple_map_can, aes(x = long, y = lat, group = group), color = NA, size = 0.25, fill = "grey10") +
  geom_tile(data = hp_thresholds_df, aes(x = x, y = y, fill = name), alpha = 0.7) +
  geom_polygon(data=simple_map_US, aes(x=long, y=lat, group=group), 
               color="grey75", size=0.25, fill = NA) +
  geom_polygon(data = simple_map_can, aes(x = long, y = lat, group = group), color = "grey50", size = 0.25, fill = NA) +
  # geom_point(data = hostplant %>%
  #              filter(time_frame == "T1") %>%
  #              filter(str_detect(name, "Zanthoxylum americanum")),
  #            aes(x = longitude, y = latitude), color ="#F8766D",  alpha = 0.5, shape = 3) +
  geom_polygon(data = lakes, aes(x = long, y = lat, group = group), fill = "white", size = 0.25) +
  theme(legend.key.width=unit(2, "cm"),
        plot.title = element_text(hjust = 0.5, size = 24)) +
  theme_nothing(legend = TRUE) +
  scale_fill_discrete(name = "Species", 
                      breaks = c("Zanthoxylum americanum", 
                                 "Zanthoxylum clava-herculis", 
                                 "Ptelea trifoliata")) +
  ggtitle("1960-1999") +
  coord_quickmap()

hp_thresholds_df = 
  bind_rows(hp_1_t2_threshold, hp_2_t2_threshold, hp_3_t2_threshold, .id = "id") %>%
  mutate(name = ifelse(id == 1, "Zanthoxylum americanum",
                       ifelse(id == 2, "Zanthoxylum clava-herculis", "Ptelea trifoliata"))) %>%
  select(-id)
#T2
g14 = ggplot() +  
  geom_polygon(data=simple_map_US, aes(x=long, y=lat, group=group), 
               color=NA, size=0.25, fill = "grey10") +
  geom_polygon(data = simple_map_can, aes(x = long, y = lat, group = group), color = NA, size = 0.25, fill = "grey10") +
  geom_tile(data = hp_thresholds_df, aes(x = x, y = y, fill = name), alpha = 0.7) +
  geom_polygon(data=simple_map_US, aes(x=long, y=lat, group=group), 
               color="grey75", size=0.25, fill = NA) +
  geom_polygon(data = simple_map_can, aes(x = long, y = lat, group = group), color = "grey50", size = 0.25, fill = NA) +
  # geom_point(data = swallowtail_t2, aes(x = longitude, y = latitude), alpha = 0.2, color = "yellow", shape = 3) +
  geom_polygon(data = lakes, aes(x = long, y = lat, group = group), fill = "white", size = 0.25) +
  theme(legend.position="bottom") +
  theme(legend.key.width=unit(2, "cm"),
        plot.title = element_text(hjust = 0.5, size = 24)) +
  theme_nothing(legend = TRUE) +
  scale_fill_discrete(name = "Species", 
                      breaks = c("Zanthoxylum americanum", 
                                 "Zanthoxylum clava-herculis", 
                                 "Ptelea trifoliata")) +
  ggtitle("2000-2019") +
  coord_quickmap()

maxent_th_hp = ggarrange(g13, g14, common.legend = TRUE)
maxent_th_hp

ggsave(plot = maxent_th_hp, filename = "./output/hostplants_threshold_occurence.png", device = "png")
