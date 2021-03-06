#Maxent Modeling following the methods outlined in the original manuscript
#Keaton Wilson
#keatonwilson@me.com
#2019-02-27

#Appropriate packages
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
library(ggpubr)

#sourcing the prepPara function
source("./scripts/prepPara_function.R")

#register google api for mapping stuff
register_google(key = "AIzaSyDyAqUc4o9p_DOBSF_JOXH5c_JXPqoU4Yw")

# Importing Data and Cleaning ---------------------------------------------
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

#Filtering the data to include stuff east of texas (94º), and in the US, Canada. Should be done from data_import_clean script, but good to double check
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

# Importing Bioclim Data and Cropping -------------------------------------
#We can do this from the dismo package - Interesting point here, this is different from original methods. These climate data are representative of "current" conditions - averaged between 1970 and 2000 (https://www.researchgate.net/publication/316999789_WorldClim_2_New_1-km_spatial_resolution_climate_surfaces_for_global_land_areas). 


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

# Swallowtail Model and Figures -------------------------------------------
#Splitting into train and test - need recipes package
library(recipes)
library(rsample)

#Split the data into t1 and t2
swallowtail_t1 = swallowtail %>%
  filter(time_frame == "T1")

swallowtail_t2 = swallowtail %>%
  filter(time_frame == "T2")

#Split into train and test
pre_train_test_split = initial_split(swallowtail_t1, prop = 0.80)
pre_swallowtail_train = training(pre_train_test_split)
pre_swallowtail_test = testing(pre_train_test_split)

#Split into train and test
post_train_test_split = initial_split(swallowtail_t2, prop = 0.80)
post_swallowtail_train = training(post_train_test_split)
post_swallowtail_test = testing(post_train_test_split)

#background data
bg = randomPoints(bioclim.data, 1000)
colnames(bg) = c("lon", "lat")
bg_split = initial_split(bg, prop = 0.80)
bg_train = training(bg_split)
bg_test = testing(bg_split)

#making the a matrix for maxent
pre_swallowtail_matrix_train = as.matrix(pre_swallowtail_train %>%
                                       select(longitude, latitude))
pre_swallowtail_matrix_test = as.matrix(pre_swallowtail_test %>%
                                      select(longitude, latitude))
post_swallowtail_matrix_train = as.matrix(post_swallowtail_train %>%
                                           select(longitude, latitude))
post_swallowtail_matrix_test = as.matrix(post_swallowtail_test %>%
                                          select(longitude, latitude))

#t1 model - testing with replication
xm_pre <- maxent(bioclim.data, pre_swallowtail_matrix_train, args = c("replicates=5", "replicatetype=crossvalidate"))

#t2 model - testing with replication
xm_post = maxent(bioclim.data, post_swallowtail_matrix_train, args = c("replicates=5"))

xm_pre
xm_post

#They both seem reasonable, with AUC scores that are good to excellent - let's build full models
xm_pre_final = maxent(bioclim.data, pre_swallowtail_matrix_train)
xm_post_final = maxent(bioclim.data, post_swallowtail_matrix_train)

xm_pre_final
xm_post_final
#The AUC for T2 isn't fantastic on training data... maybe we need to do some feature selection down the line. For now, let's just see how it does on test data. 

#evaluating models on test data
e_pre <- evaluate(p = pre_swallowtail_matrix_test, bg_test, xm_pre_final, bioclim.data)
plot(e_pre, 'ROC')

e_post = evaluate(post_swallowtail_matrix_test, a = bg_test,  model = xm_post_final, x = bioclim.data)
plot(e_post, 'ROC')

#building predictions and mapping
predict_presence_pre = dismo::predict(object = xm_pre_final, x = bioclim.data, ext = geographic.extent)

predict_presence_post = dismo::predict(object = xm_post_final, x = bioclim.data, ext = geographic.extent)

test_spdf_pre <- as(predict_presence_pre, "SpatialPixelsDataFrame")
test_df_pre <- as.data.frame(test_spdf_pre)
colnames(test_df_pre) <- c("value", "x", "y")

test_spdf_post <- as(predict_presence_post, "SpatialPixelsDataFrame")
test_df_post <- as.data.frame(test_spdf_post)
colnames(test_df_post) <- c("value", "x", "y")

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
  geom_tile(data=test_df_pre, aes(x=x, y=y, fill=value)) + 
  geom_polygon(data=simple_map_US, aes(x=long, y=lat, group=group), 
               color="grey50", size=0.25, fill = NA) +
  geom_polygon(data = simple_map_can, aes(x = long, y = lat, group = group), color = "grey50", size = 0.25, fill = NA) +
  scale_fill_viridis() +
  theme(legend.position="bottom") +
  theme(legend.key.width=unit(2, "cm")) +
  coord_equal(ylim = c(22, 50), xlim = c(-100, -65)) +
  theme_map()

#Plotting - T2 Second
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

#Combining
ggarrange(g1, g2, common.legend = TRUE, labels = c("1960-2000", "2000-2018"))

test_df_post

#Threshold maps
tr_pre = threshold(e_pre, 'spec_sens')
tr_post = threshold(e_post, 'spec_sens')

pre_threshold = test_df_pre %>%
  filter(value > tr_pre)
post_threshold = test_df_post %>%
  filter(value > tr_post)

#binding
threshold_df = bind_rows("t1" = pre_threshold, "t2" = post_threshold, .id = "timeframe")

#plotting
ggplot(threshold_df, aes(x = y, fill = timeframe)) +
  geom_density(alpha = 0.8) +
  theme_classic() +
  labs(x = "Latitude", y = "Kernel Density Estimate") +
  scale_fill_discrete(name = "Time Frame", labels = c("1960-2000", "2000-2018"))

#T1 
#
ggplot() +  
  geom_polygon(data=simple_map_US, aes(x=long, y=lat, group=group), 
               color=NA, size=0.25, fill = "grey10") +
  geom_polygon(data = simple_map_can, aes(x = long, y = lat, group = group), color = NA, size = 0.25, fill = "grey10") +
  geom_tile(data=pre_threshold, aes(x=x, y=y), fill = "lightgrey") + 
  geom_polygon(data=simple_map_US, aes(x=long, y=lat, group=group), 
               color="grey75", size=0.25, fill = NA) +
  geom_polygon(data = simple_map_can, aes(x = long, y = lat, group = group), color = "grey50", size = 0.25, fill = NA) +
  geom_point(data = swallowtail_t1, aes(x = longitude, y = latitude), alpha = 0.6, color = "yellow") +
  theme(legend.position="bottom") +
  theme(legend.key.width=unit(2, "cm")) +
  coord_equal(ylim = c(22, 50), xlim = c(-100, -65)) +
  theme_map()

#T2
ggplot() +  
  geom_polygon(data=simple_map_US, aes(x=long, y=lat, group=group), 
               color=NA, size=0.25, fill = "grey10") +
  geom_polygon(data = simple_map_can, aes(x = long, y = lat, group = group), color = NA, size = 0.25, fill = "grey10") +
  geom_tile(data=post_threshold, aes(x=x, y=y), fill = "lightgrey") + 
  geom_polygon(data=simple_map_US, aes(x=long, y=lat, group=group), 
               color="grey75", size=0.25, fill = NA) +
  geom_polygon(data = simple_map_can, aes(x = long, y = lat, group = group), color = "grey50", size = 0.25, fill = NA) +
  geom_point(data = swallowtail_t2, aes(x = longitude, y = latitude), alpha = 0.4, color = "yellow") +
  theme(legend.position="bottom") +
  theme(legend.key.width=unit(2, "cm")) +
  coord_equal(ylim = c(22, 50), xlim = c(-100, -65)) +
  theme_map()

#Inset Figure
swallowtail_inset = swallowtail %>%
  group_by(year) %>%
  summarize(max_lat = max(latitude), 
            n = n()) %>%
  filter(max_lat > 35)

  ggplot(data = swallowtail_inset, aes(x = year, y = max_lat, size = n)) +
  geom_point(alpha = 0.8) +
  geom_smooth(data = swallowtail_inset %>%
                filter(year < 2000), aes(x = year, y = max_lat), method = "lm", show.legend = FALSE) +
  geom_smooth(data = swallowtail_inset %>%
                filter(year >= 2000), aes(x = year, y = max_lat), method = "lm", show.legend = FALSE) +
  theme_classic() +
  scale_size_continuous(name = "Number of Observations") +
  labs(x = "Year", y = "Maximum Latitude (º)")
  

# Host plant models and Figures -------------------------------------------

  #Split the data into t1 and t2
  hostplant_t1 = host_plant %>%
    filter(time_frame == "T1")
  
  hostplant_t2 = host_plant %>%
    filter(time_frame == "T2")
  
  #Split into train and test
  pre_train_test_split_hp = initial_split(hostplant_t1, prop = 0.80)
  pre_hostplant_train = training(pre_train_test_split_hp)
  pre_hostplant_test = testing(pre_train_test_split_hp)
  
  #Split into train and test
  post_train_test_split_hp = initial_split(hostplant_t2, prop = 0.80)
  post_hostplant_train = training(post_train_test_split_hp)
  post_hostplant_test = testing(post_train_test_split_hp)
  
  #background data
  bg = randomPoints(bioclim.data, 1000)
  colnames(bg) = c("lon", "lat")
  bg_split = initial_split(bg, prop = 0.80)
  bg_train = training(bg_split)
  bg_test = testing(bg_split)
  
  #making the a matrix for maxent
  pre_hostplant_matrix_train = as.matrix(pre_hostplant_train %>%
                                             select(longitude, latitude))
  pre_hostplant_matrix_test = as.matrix(pre_hostplant_test %>%
                                            select(longitude, latitude))
  post_hostplant_matrix_train = as.matrix(post_hostplant_train %>%
                                              select(longitude, latitude))
  post_hostplant_matrix_test = as.matrix(post_hostplant_test %>%
                                             select(longitude, latitude))
  
  #t1 model - testing with replication
  xm_pre_hp <- maxent(bioclim.data, pre_hostplant_matrix_train, args = c("replicates=5", "replicatetype=crossvalidate"))
  
  #t2 model - testing with replication
  xm_post_hp = maxent(bioclim.data, post_hostplant_matrix_train, args = c("replicates=5"))
  
  xm_pre_hp
  xm_post_hp
  
  #They both seem reasonable, with AUC scores that are good to excellent - let's build full models
  xm_pre_final_hp = maxent(bioclim.data, pre_hostplant_matrix_train)
  xm_post_final_hp = maxent(bioclim.data, post_hostplant_matrix_train)
  
  xm_pre_final_hp
  xm_post_final_hp

  
  #evaluating models on test data
  e_pre_hp <- evaluate(p = pre_hostplant_matrix_test, bg_test, xm_pre_final_hp, bioclim.data)
  plot(e_pre, 'ROC')
  
  e_post_hp = evaluate(post_hostplant_matrix_test, a = bg_test,  model = xm_post_final_hp, x = bioclim.data)
  plot(e_post, 'ROC')

  #building predictions and mapping
  predict_presence_pre_hp = dismo::predict(object = xm_pre_final_hp, x = bioclim.data, ext = geographic.extent)
  
  predict_presence_post_hp = dismo::predict(object = xm_post_final_hp, x = bioclim.data, ext = geographic.extent)
  
  test_spdf_pre_hp <- as(predict_presence_pre_hp, "SpatialPixelsDataFrame")
  test_df_pre_hp <- as.data.frame(test_spdf_pre_hp)
  colnames(test_df_pre_hp) <- c("value", "x", "y")
  
  test_spdf_post_hp <- as(predict_presence_post_hp, "SpatialPixelsDataFrame")
  test_df_post_hp <- as.data.frame(test_spdf_post_hp)
  colnames(test_df_post_hp) <- c("value", "x", "y")
  
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
  g1_hp = ggplot() +  
    geom_polygon(data=simple_map_US, aes(x=long, y=lat, group=group), 
                 color=NA, size=0.25, fill = "#440154FF") +
    geom_polygon(data = simple_map_can, aes(x = long, y = lat, group = group), color = NA, size = 0.25, fill = "#440154FF") +
    geom_tile(data=test_df_pre_hp, aes(x=x, y=y, fill=value)) + 
    geom_polygon(data=simple_map_US, aes(x=long, y=lat, group=group), 
                 color="grey50", size=0.25, fill = NA) +
    geom_polygon(data = simple_map_can, aes(x = long, y = lat, group = group), color = "grey50", size = 0.25, fill = NA) +
    scale_fill_viridis() +
    theme(legend.position="bottom") +
    theme(legend.key.width=unit(2, "cm")) +
    coord_equal(ylim = c(22, 50), xlim = c(-100, -65)) +
    theme_map()
  
  #Plotting - T2 Second
  g2_hp = ggplot() +  
    geom_polygon(data=simple_map_US, aes(x=long, y=lat, group=group), 
                 color=NA, size=0.25, fill = "#440154FF") +
    geom_polygon(data = simple_map_can, aes(x = long, y = lat, group = group), color = NA, size = 0.25, fill = "#440154FF") +
    geom_tile(data=test_df_post_hp, aes(x=x, y=y, fill=value)) + 
    geom_polygon(data=simple_map_US, aes(x=long, y=lat, group=group), 
                 color="grey50", size=0.25, fill = NA) +
    geom_polygon(data = simple_map_can, aes(x = long, y = lat, group = group), color = "grey50", size = 0.25, fill = NA) +
    scale_fill_viridis() +
    theme(legend.position="bottom") +
    theme(legend.key.width=unit(2, "cm")) +
    coord_equal(ylim = c(22, 50), xlim = c(-100, -65)) +
    theme_map()
  
  #Combining
  ggarrange(g1_hp, g2_hp, common.legend = TRUE, labels = c("1960-2000", "2000-2018"))
  
  #Threshold maps
  tr_pre_hp = threshold(e_pre_hp, 'spec_sens')
  tr_post_hp = threshold(e_post_hp, 'spec_sens')
  
  pre_threshold_hp = test_df_pre_hp %>%
    filter(value > tr_pre_hp)
  post_threshold_hp = test_df_post_hp %>%
    filter(value > tr_post_hp)
  
  #binding
  threshold_df_hp = bind_rows("t1" = pre_threshold_hp, "t2" = post_threshold_hp, .id = "timeframe")
  
  #plotting
  ggplot(threshold_df_hp, aes(x = y, fill = timeframe)) +
    geom_density(alpha = 0.8) +
    theme_classic() +
    labs(x = "Latitude", y = "Kernel Density Estimate") +
    scale_fill_discrete(name = "Time Frame", labels = c("1960-2000", "2000-2018"))
  
  #T1 
  #
  ggplot() +  
    geom_polygon(data=simple_map_US, aes(x=long, y=lat, group=group), 
                 color=NA, size=0.25, fill = "grey10") +
    geom_polygon(data = simple_map_can, aes(x = long, y = lat, group = group), color = NA, size = 0.25, fill = "grey10") +
    geom_tile(data=pre_threshold_hp, aes(x=x, y=y), fill = "lightgrey") + 
    geom_polygon(data=simple_map_US, aes(x=long, y=lat, group=group), 
                 color="grey75", size=0.25, fill = NA) +
    geom_polygon(data = simple_map_can, aes(x = long, y = lat, group = group), color = "grey50", size = 0.25, fill = NA) +
    geom_point(data = hostplant_t1, aes(x = longitude, y = latitude), alpha = 0.6, color = "yellow") +
    theme(legend.position="bottom") +
    theme(legend.key.width=unit(2, "cm")) +
    coord_equal(ylim = c(22, 50), xlim = c(-100, -65)) +
    theme_map()
  
  #T2
  ggplot() +  
    geom_polygon(data=simple_map_US, aes(x=long, y=lat, group=group), 
                 color=NA, size=0.25, fill = "grey10") +
    geom_polygon(data = simple_map_can, aes(x = long, y = lat, group = group), color = NA, size = 0.25, fill = "grey10") +
    geom_tile(data=post_threshold_hp, aes(x=x, y=y), fill = "lightgrey") + 
    geom_polygon(data=simple_map_US, aes(x=long, y=lat, group=group), 
                 color="grey75", size=0.25, fill = NA) +
    geom_polygon(data = simple_map_can, aes(x = long, y = lat, group = group), color = "grey50", size = 0.25, fill = NA) +
    geom_point(data = hostplant_t2, aes(x = longitude, y = latitude), alpha = 0.4, color = "yellow") +
    theme(legend.position="bottom") +
    theme(legend.key.width=unit(2, "cm")) +
    coord_equal(ylim = c(22, 50), xlim = c(-100, -65)) +
    theme_map()

  #Inset Figure - HP
  hp_inset = host_plant %>%
    group_by(year) %>%
    summarize(max_lat = max(latitude), 
              n = n()) %>%
    filter(max_lat > 35 & year > 1959)
  
  ggplot(data = hp_inset, aes(x = year, y = max_lat, size = n)) +
    geom_point(alpha = 0.8) +
    geom_smooth(method = "lm", show.legend = FALSE) +
    theme_classic() +
    scale_size_continuous(name = "Number of Observations") +
    labs(x = "Year", y = "Maximum Latitude (º)") 
  