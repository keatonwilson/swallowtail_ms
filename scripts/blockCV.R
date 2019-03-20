#Playing with blockCV
#Keaton Wilson
#keatonwilson@me.com
#2019-03-18

#libraries
library(blockCV)
library(tidyverse)
library(raster)
library(maxnet)
library(dismo)
library(ENMeval)

#importing data
#butterfly
swallowtail = read_csv("./data/swallowtail_data.csv")
swallowtail = swallowtail[,-1] %>%
  select(longitude, latitude, date, year, time_frame)

#hostplant
hostplant = read_csv("./data/hostplant_data.csv")
hostplant = hostplant[,-1]

#bioclim environmental variables
bioclim = readRDS("./data/bioclim.rds")

#Generating background points
#background data
bg = randomPoints(bioclim, 10000)
colnames(bg) = c("longitude", "latitude")

#Presence/BG data
df = data.frame(swallowtail) %>%
  mutate(pb = 1) %>%
  select(pb, longitude, latitude) %>%
  bind_rows(data.frame(bg) %>% 
              mutate(pb = 0)) %>%
  mutate(Species = as.integer(pb)) %>%
  select(-pb)

#Spatialpoints
df_sp = SpatialPointsDataFrame(df[,c("longitude","latitude")], 
                               df, 
                               proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

#species occurence to feed in
sac_in = raster::extract(bioclim, df[,-3])
df_sac_in = bind_cols(df, data.frame(sac_in)) %>%
  drop_na()
df_sac_in_sp = SpatialPointsDataFrame(df_sac_in[,c("longitude","latitude")], 
                                      df_sac_in, 
                                      proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

sac <- spatialAutoRange(rasterLayer = bioclim,
                        speciesData = df_sac_in_sp,
                        border = NULL,
                        showPlots = TRUE,
                        plotVariograms = FALSE,
                        doParallel = TRUE)

rangeExplorer(rasterLayer = bioclim,
              speciesData = df_sac_in_sp,
              species = "Species")

#Let's do a spatial block design with fairly large blocks - Eyeballing this because something is wonky with the way it's being done with the spatial auto range function. 
sb <- spatialBlock(speciesData = df_sac_in_sp,
                   species = "Species",
                   rasterLayer = bioclim,
                   theRange = 400000, # size of the blocks
                   k = 5,
                   selection = "random",
                   iteration = 250, # find evenly dispersed folds
                   biomod2Format = TRUE,
                   xOffset = 0, # shift the blocks horizontally
                   yOffset = 0,
                   progress = T)

#Getting a dataframe to feed into the model
data = raster::extract(bioclim, df[,-3], df = TRUE) %>%
  bind_cols(df) %>%
  drop_na() %>%
  select(-ID, Species, longitude, latitude, bio1:bio19)

#vector of presence-background
pb = data$Species

#folds
folds = sb$folds

#Breaking into training and testing 
for(k in 1:length(folds)){
  trainSet <- unlist(folds[[k]][1]) # extract the training set indices
  testSet <- unlist(folds[[k]][2]) # extract the test set indices
}

#training and testing
train_data = data[trainSet,]
test_data = data[testSet,]

#Formatting occurences and background for sending to ENMevaluate
p_train = train_data %>%
  filter(Species == 1) %>%
  select(longitude, latitude)

a_train = train_data %>%
  filter(Species == 0) %>%
  select(longitude, latitude)

#First attempt at a model - took ~115 minutes to run
eval_1 = ENMevaluate(occ = p_train, bg.coords = a_train, env = bioclim, method = 'randomkfold', kfolds = 5)

#Best model according to one metric - average AUC on test data - LQHPT with a 0.5 regularization multiplier
eval_1@results[which(eval_1@results$avg.test.AUC== max(eval_1@results$avg.test.AUC)),]
eval.plot(eval_1@results, 'avg.test.AUC', var = 'var.test.AUC')

#So we can make predictions on the spatially explicit test data now.
eval_1@results[which(eval_1@results$avg.test.AUC== max(eval_1@results$avg.test.AUC)),]

#Predictions
big_test_p = predict(eval_1@models[[6]], newdata = test_data, type = "cloglog")

#Building ROC curve and calculating AUC on the test data
library(ROCR)
pred=prediction(big_test_p, test_data$Species)

perf=performance(pred,"tpr","fpr")
plot(perf)
performance(pred,"auc")

