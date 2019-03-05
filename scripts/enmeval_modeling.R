#Exploring the ENMeval package which implements maxent but gives a bit more flexibility on model testing and evaluation, as well as output
#Keaton Wilson
#keatonwilson@me.com
#2019-03-01

#source maining modeling code so we have the data objects to load in below
source("./scripts/models.R")

#Maybe a better package to do this - still testing this. It's taking forever (46 minutes without parallelization and only 3 folds).
library(ENMeval)
xm_pre_test = ENMevaluate(pre_swallowtail_matrix_train, env = bioclim.data, method = "randomkfold", kfolds = 3)

xm_pre_test_preds = xm_pre_test@predictions[[which(xm_pre_test@results$delta.AICc ==0)]]
xm_pre_test_preds_sp = as(xm_pre_test_preds, "SpatialPixelsDataFrame")
test_df_pre <- as.data.frame(xm_pre_test_preds_sp)
colnames(test_df_pre) <- c("value", "x", "y")

ggplot() +  
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

#This more robust model-fitting approach is kind of bonkers in it's specificity - maybe not super-useful. It could be that we don't really have enough data pre 2000 for this kind of model-testing. 
