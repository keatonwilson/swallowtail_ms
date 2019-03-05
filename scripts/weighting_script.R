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
swallowtail_df = swallowtail_df %>%
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
swallowtail_df_map = swallowtail_master %>%
  mutate(year = year(date), 
         time_frame = as.factor(ifelse(year >= 2000, 2, 1)))

qmap("west virgnia",zoom = 5, maptype = "toner-background") +
  geom_point(data = swallowtail_df_map, aes(x = Longitude, y = Latitude, color = time_frame), alpha = 0.5) +
  scale_color_discrete(name = "Time Frame", labels = c("Pre-2000", "Post-2000")) +
  labs(x = "Longitude (º) ", y = "Latitude (º)")



#need to do weighting for hostplant

#New dataframe to work with
sp_hostplant = host_plant_df

#Changing the swallowtail dataframe to an sp object
coordinates(sp_hostplant) = ~longitude + latitude

#Constructing a pairwise distance matrix
d <- distm(sp_hostplant)

#Closest 5 distances
min_1 = apply(d, 1, function(x) sort(x, TRUE)[2])
min_2 = apply(d, 1, function(x) sort(x, TRUE)[3])
min_3 = apply(d, 1, function(x) sort(x, TRUE)[4])
min_4 = apply(d, 1, function(x) sort(x, TRUE)[5])
min_5 = apply(d, 1, function(x) sort(x, TRUE)[6])

#Binding into a dataframe
mins = data.frame(min_1, min_2, min_3, min_4, min_5)

#binding onto original data
host_plant_df = bind_cols(host_plant_df, mins)

#Calculating averages per occurence and normalizing to generate weights
#i.e. high average values = points that are on their own, low average values = points that are clumped
host_plant_df = host_plant_df %>%
  mutate(avg_min = (min_1 + min_2 + min_3 + min_4 + min_5)/5,
         norm_avg_min = (avg_min - mean(avg_min))/sd(avg_min)) %>%
  select(-c(min_1:min_5))
