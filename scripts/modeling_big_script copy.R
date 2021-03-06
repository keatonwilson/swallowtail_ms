#blockCV and Modeling Script for Swallowtail and hostplant data
#Keaton Wilson
#keatonwilson@me.com
#2019-03-21

#libraries
library(blockCV)
library(tidyverse)
library(raster)
library(maxnet)
library(dismo)
library(ENMeval)


# Data Preparation --------------------------------------------------------

#importing swallowtail, hostplant and environmental data
#butterfly
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

#Dividing host plant and swallowtail into respective time periods
#We essentially will have 4 models (HPT1, HPT2, STT1, STT2)

swallowtail_t1 = swallowtail %>%
  filter(time_frame == "T1")

swallowtail_t2 = swallowtail %>%
  filter(time_frame == "T2")

hostplant_t1 = hostplant %>%
  filter(time_frame == "T1")

hostplant_t2 = hostplant %>%
  filter(time_frame == "T2")

#Generating background points
#background data
bg_swallowtail_t1 = dismo::randomPoints(bioclim.data, 10000)
colnames(bg_swallowtail_t1) = c("longitude", "latitude")

bg_swallowtail_t2 = randomPoints(bioclim.data, 10000)
colnames(bg_swallowtail_t2) = c("longitude", "latitude")

bg_hostplant_t1 = randomPoints(bioclim.data, 10000)
colnames(bg_hostplant_t1) = c("longitude", "latitude")

bg_hostplant_t2 = randomPoints(bioclim.data, 10000)
colnames(bg_hostplant_t2) = c("longitude", "latitude")

#Merging background and occurence data for blockCV
df_st_t1 = data.frame(swallowtail_t1) %>%
  mutate(pb = 1) %>%
  select(pb, longitude, latitude) %>%
  bind_rows(data.frame(bg_swallowtail_t1) %>% 
              mutate(pb = 0)) %>%
  mutate(Species = as.integer(pb)) %>%
  select(-pb)

df_st_t2 = data.frame(swallowtail_t2) %>%
  mutate(pb = 1) %>%
  select(pb, longitude, latitude) %>%
  bind_rows(data.frame(bg_swallowtail_t2) %>% 
              mutate(pb = 0)) %>%
  mutate(Species = as.integer(pb)) %>%
  select(-pb)

df_hp_t1 = data.frame(hostplant_t1) %>%
  mutate(pb = 1) %>%
  select(pb, longitude, latitude) %>%
  bind_rows(data.frame(bg_hostplant_t1) %>% 
              mutate(pb = 0)) %>%
  mutate(Species = as.integer(pb)) %>%
  select(-pb)

df_hp_t2 = data.frame(hostplant_t1) %>%
  mutate(pb = 1) %>%
  select(pb, longitude, latitude) %>%
  bind_rows(data.frame(bg_hostplant_t1) %>% 
              mutate(pb = 0)) %>%
  mutate(Species = as.integer(pb)) %>%
  select(-pb)

#Spatialpoints
dfspstt1 = SpatialPointsDataFrame(df_st_t1[,c("longitude","latitude")], 
                                  df_st_t1, 
                                  proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

dfspstt2 = SpatialPointsDataFrame(df_st_t2[,c("longitude","latitude")], 
                                  df_st_t2, 
                                  proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

dfsphpt1 = SpatialPointsDataFrame(df_hp_t1[,c("longitude","latitude")], 
                                  df_hp_t1, 
                                  proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
dfsphpt2 = SpatialPointsDataFrame(df_hp_t2[,c("longitude","latitude")], 
                                  df_hp_t2, 
                                  proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

# blockCV Train-Test Split for all 4 models ------------------------------------------------
sb_st_t1 <- spatialBlock(speciesData = dfspstt1,
                         species = "Species",
                         rasterLayer = bioclim.data,
                         theRange = 400000, # size of the blocks
                         k = 5,
                         selection = "random",
                         iteration = 250, # find evenly dispersed folds
                         biomod2Format = TRUE,
                         xOffset = 0, # shift the blocks horizontally
                         yOffset = 0,
                         progress = T)

sb_st_t2 <- spatialBlock(speciesData = dfspstt2,
                         species = "Species",
                         rasterLayer = bioclim.data,
                         theRange = 400000, # size of the blocks
                         k = 5,
                         selection = "random",
                         iteration = 250, # find evenly dispersed folds
                         biomod2Format = TRUE,
                         xOffset = 0, # shift the blocks horizontally
                         yOffset = 0,
                         progress = T)

sb_hp_t1 <- spatialBlock(speciesData = dfspstt2,
                         species = "Species",
                         rasterLayer = bioclim.data,
                         theRange = 400000, # size of the blocks
                         k = 5,
                         selection = "random",
                         iteration = 250, # find evenly dispersed folds
                         biomod2Format = TRUE,
                         xOffset = 0, # shift the blocks horizontally
                         yOffset = 0,
                         progress = T)

sb_hp_t2 <- spatialBlock(speciesData = dfspstt2,
                         species = "Species",
                         rasterLayer = bioclim.data,
                         theRange = 400000, # size of the blocks
                         k = 5,
                         selection = "random",
                         iteration = 250, # find evenly dispersed folds
                         biomod2Format = TRUE,
                         xOffset = 0, # shift the blocks horizontally
                         yOffset = 0,
                         progress = T)

#Getting dataframes to feed into the model (dropping NAs)
data_st_t1 = raster::extract(bioclim.data, df_st_t1[,-3], df = TRUE) %>%
  bind_cols(df_st_t1) %>%
  drop_na() %>%
  select(-ID, Species, longitude, latitude, bio1:bio19)

data_st_t2 = raster::extract(bioclim.data, df_st_t2[,-3], df = TRUE) %>%
  bind_cols(df_st_t2) %>%
  drop_na() %>%
  select(-ID, Species, longitude, latitude, bio1:bio19)

data_hp_t1 = raster::extract(bioclim.data, df_hp_t1[,-3], df = TRUE) %>%
  bind_cols(df_hp_t1) %>%
  drop_na() %>%
  select(-ID, Species, longitude, latitude, bio1:bio19)

data_hp_t2 = raster::extract(bioclim.data, df_hp_t2[,-3], df = TRUE) %>%
  bind_cols(df_hp_t2) %>%
  drop_na() %>%
  select(-ID, Species, longitude, latitude, bio1:bio19)

#vectors of presence-background
pb_st_t1 = data_st_t1$Species
pb_st_t2 = data_st_t2$Species
pb_hp_t1 = data_hp_t1$Species
pb_hp_t2 = data_hp_t2$Species

#folds for each model
sb_st_t1_folds = sb_st_t1$folds
sb_st_t2_folds = sb_st_t2$folds
sb_hp_t1_folds = sb_hp_t1$folds
sb_hp_t2_folds = sb_hp_t2$folds

#Breaking into training and testing 
for(k in 1:length(sb_st_t1_folds)){
  pb_st_t1_train_index <- unlist(sb_st_t1_folds[[k]][1]) # extract the training set indices
  pb_st_t1_test_index <- unlist(sb_st_t1_folds[[k]][2]) # extract the test set indices
}

for(k in 1:length(sb_st_t2_folds)){
  pb_st_t2_train_index <- unlist(sb_st_t2_folds[[k]][1]) # extract the training set indices
  pb_st_t2_test_index <- unlist(sb_st_t2_folds[[k]][2]) # extract the test set indices
}

for(k in 1:length(sb_hp_t1_folds)){
  pb_hp_t1_train_index <- unlist(sb_hp_t1_folds[[k]][1]) # extract the training set indices
  pb_hp_t1_test_index <- unlist(sb_hp_t1_folds[[k]][2]) # extract the test set indices
}

for(k in 1:length(sb_hp_t2_folds)){
  pb_hp_t2_train_index <- unlist(sb_hp_t2_folds[[k]][1]) # extract the training set indices
  pb_hp_t2_test_index <- unlist(sb_hp_t2_folds[[k]][2]) # extract the test set indices
}

#training and testing
pb_st_t1_train_data = data_st_t1[pb_st_t1_train_index,]
pb_st_t1_test_data = data_st_t1[pb_st_t1_test_index,]

pb_st_t2_train_data = data_st_t2[pb_st_t2_train_index,]
pb_st_t2_test_data = data_st_t2[pb_st_t2_test_index,]

pb_hp_t1_train_data = data_hp_t1[pb_hp_t1_train_index,]
pb_hp_t1_test_data = data_hp_t1[pb_hp_t1_test_index,]

pb_hp_t2_train_data = data_hp_t2[pb_hp_t2_train_index,]
pb_hp_t2_test_data = data_hp_t2[pb_hp_t2_test_index,]

#Quality control
dim(pb_st_t1_train_data)
dim(pb_st_t1_test_data) #Looks good - test data is about 20%

#Formatting occurences and background for sending to ENMevaluate
#Train and test for swallowtail t1
p_st_t1_train = pb_st_t1_train_data %>%
  filter(Species == 1) %>%
  select(longitude, latitude)

p_st_t1_test = pb_st_t1_test_data %>%
  filter(Species == 1) %>%
  select(longitude, latitude)

#train and test for swallowtail t2
p_st_t2_train = pb_st_t2_train_data %>%
  filter(Species == 1) %>%
  select(longitude, latitude)

p_st_t2_test = pb_st_t2_test_data %>%
  filter(Species == 1) %>%
  select(longitude, latitude)

#train and test for hostplant t1
p_hp_t1_train = pb_hp_t1_train_data %>%
  filter(Species == 1) %>%
  select(longitude, latitude)

p_hp_t1_test = pb_hp_t2_train_data %>%
  filter(Species == 1) %>%
  select(longitude, latitude)

#train and test for hostplant t2
p_hp_t2_train = pb_hp_t2_train_data %>%
  filter(Species == 1) %>%
  select(longitude, latitude)

p_hp_t2_test = pb_hp_t2_test_data %>%
  filter(Species == 1) %>%
  select(longitude, latitude)


# Modeling ----------------------------------------------------------------
if (Sys.getenv("JAVA_HOME")!="")
  Sys.setenv(JAVA_HOME="")
library(rJava)


#Swallowtail t1 
eval_st_t1 = ENMevaluate(occ = p_st_t1_train, 
                         bg.coords = bg_swallowtail_t1, 
                         env = bioclim.data, 
                         method = 'randomkfold', 
                         kfolds = 5, 
                         algorithm = 'maxent.jar')

#Swallowtail t2
eval_st_t2 = ENMevaluate(occ = p_st_t2_train, 
                         bg.coords = bg_swallowtail_t2, 
                         env = bioclim.data, 
                         method = 'randomkfold', 
                         kfolds = 5, 
                         algorithm = 'maxent.jar')

#Hostplant t1
eval_hp_t1 = ENMevaluate(occ = p_hp_t1_train, 
                         bg.coords = bg_hostplant_t1, 
                         env = bioclim.data, 
                         method = 'randomkfold', 
                         kfolds = 5, 
                         algorithm = 'maxent.jar')

#Hostplant t2
eval_hp_t2 = ENMevaluate(occ = p_hp_t2_train, 
                         bg.coords = bg_hostplant_t2, 
                         env = bioclim.data, 
                         method = 'randomkfold', 
                         kfolds = 5, 
                         algorithm = 'maxent.jar')

#saving model objects
saveRDS(eval_st_t1, "./models/eval_st_t1.rds")
saveRDS(eval_st_t2, "./models/eval_st_t2.rds")
saveRDS(eval_hp_t1, "./models/eval_hp_t1.rds")
saveRDS(eval_hp_t2, "./models/eval_hp_t2.rds")


# Model Evaluation --------------------------------------------------------

#Function to build set of evaluation plots - just plug in the appropriate eval model object from above

#reading in models - otherwise things will take too much time
eval_st_t1 = readRDS("./models/eval_st_t1.rds")
eval_st_t2 = readRDS("./models/eval_st_t2.rds")
eval_hp_t1 = readRDS("./models/eval_hp_t1.rds")
eval_hp_t2 = readRDS("./models/eval_hp_t2.rds")

eval_plots = function(eval_object = NULL) {
  par(mfrow=c(2,3))
  eval.plot(eval_object@results)
  eval.plot(eval_object@results, 'avg.test.AUC', legend = F)
  eval.plot(eval_object@results, 'avg.diff.AUC', legend = F)
  eval.plot(eval_object@results, 'avg.test.or10pct', legend = F)
  eval.plot(eval_object@results, 'avg.test.orMTP', legend = F)
  plot(eval_object@results$avg.test.AUC, eval_object@results$delta.AICc, bg=eval_object@results$features, pch=21, cex= eval_object@results$rm/2, xlab = "avg.test.AUC", ylab = 'delta.AICc', cex.lab = 1.5)
  legend("topright", legend=unique(eval_object@results$features), pt.bg=eval_object@results$features, pch=21)
  mtext("Circle size proportional to regularization multiplier value", cex = 0.6)
}

#Evaluation plots
eval_plots(eval_st_t1)
eval_plots(eval_st_t2)
eval_plots(eval_hp_t1)
eval_plots(eval_hp_t2)

#Picking the best model based on highest AUC for each set
#Pulling out indices of the "best" model based on AUC scores - if there are two models that are equal, it pulls the first.
best_index_st_t1 = as.numeric(row.names(eval_st_t1@results[which(eval_st_t1@results$avg.test.AUC== max(eval_st_t1@results$avg.test.AUC)),]))[1]

best_index_st_t2 = as.numeric(row.names(eval_st_t2@results[which(eval_st_t2@results$avg.test.AUC== max(eval_st_t2@results$avg.test.AUC)),]))[1]

best_index_hp_t1 = as.numeric(row.names(eval_hp_t1@results[which(eval_hp_t1@results$avg.test.AUC== max(eval_hp_t1@results$avg.test.AUC)),]))[1]

best_index_hp_t2 = as.numeric(row.names(eval_hp_t2@results[which(eval_hp_t2@results$avg.test.AUC== max(eval_hp_t2@results$avg.test.AUC)),]))[1]

#Using indices generated above to pull out the model objects
best_st_t1 = eval_st_t1@models[[best_index_st_t1]]
best_st_t2 = eval_st_t2@models[[best_index_st_t2]]
best_hp_t1 = eval_hp_t1@models[[best_index_hp_t1]]
best_hp_t2 = eval_hp_t2@models[[best_index_hp_t2]]

#Evaluate on test data
ev_st_t1 = evaluate(p_st_t1_test, a = bg_swallowtail_t1,  model = best_st_t1, x = bioclim.data)
ev_st_t2 = evaluate(p_st_t2_test, a = bg_swallowtail_t2,  model = best_st_t2, x = bioclim.data)
ev_hp_t1 = evaluate(p_hp_t1_test, a = bg_hostplant_t1,  model = best_hp_t1, x = bioclim.data)
ev_hp_t2 = evaluate(p_hp_t2_test, a = bg_hostplant_t2, model = best_hp_t2, x = bioclim.data)



# Selecting Final Models and Running on All Data --------------------------
#Let's build final models

#Swallowtail T1
#Pulling out features
auc_mod = eval_st_t1@results[best_index_st_t1,]
FC_best = as.character(auc_mod$features[1])
rm_best = auc_mod$rm

#setting maxent arguments
maxent.args = ENMeval::make.args(RMvalues = rm_best, fc = FC_best)

#Full Swallowtail T1 Model
mx_best_st_t1 = maxent(bioclim.data, as.matrix(swallowtail_t1[,1:2]), args = maxent.args[[1]])

#save model
saveRDS(mx_best_st_t1, "./models/full_best_st_t1.rds")

#Swallowtail T2
#Pulling out features
auc_mod = eval_st_t2@results[best_index_st_t2,]
FC_best = as.character(auc_mod$features[1])
rm_best = auc_mod$rm

#setting maxent arguments
maxent.args = ENMeval::make.args(RMvalues = rm_best, fc = FC_best)

#Full Swallowtail T2 Model
mx_best_st_t2 = maxent(bioclim.data, as.matrix(swallowtail_t2[,1:2]), args = maxent.args[[1]])

#save model
saveRDS(mx_best_st_t2, "./models/full_best_st_t2.rds")

#Hostplant T1
#Pulling out features
auc_mod = eval_hp_t1@results[best_index_hp_t1,]
FC_best = as.character(auc_mod$features[1])
rm_best = auc_mod$rm

#setting maxent arguments
maxent.args = ENMeval::make.args(RMvalues = rm_best, fc = FC_best)

#Full Hostplant T1 Model
mx_best_hp_t1 = maxent(bioclim.data, as.matrix(hostplant_t1[,1:2]), args = maxent.args[[1]])

#save model
saveRDS(mx_best_hp_t1, "./models/full_best_hp_t1.rds")

#Hostplant T2
#Pulling out features
auc_mod = eval_st_t2@results[best_index_st_t2,]
FC_best = as.character(auc_mod$features[1])
rm_best = auc_mod$rm

#setting maxent arguments
maxent.args = ENMeval::make.args(RMvalues = rm_best, fc = FC_best)

#Full Hostplant T2 Model
mx_best_hp_t2 = maxent(bioclim.data, as.matrix(hostplant_t2[,1:2]), args = maxent.args[[1]])

#save model
saveRDS(mx_best_hp_t2, "./models/full_best_hp_t2.rds")
