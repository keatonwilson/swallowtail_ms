#Back to daymet 

library(daymetr)
library(ncdf4)
library(raster)
library(tidyverse)
library(dismo)

#Filtering the data to include stuff east of texas (94º), and in the US, Canada. Should be done from data_import_clean script, but good to double check
lon_min = -94
lon_max = -65
lat_min = 25
lat_max = 55

download_daymet_ncss(location = c(55, -94, 25, -65),
                     start = 1981,
                     end = 2000, 
                     param = c("tmin", "tmax", "prcp"),
                     frequency = "monthly",
                     path = "./data/daymet/")

#Loop through batch one, combine and average - biovar does one year at a time
file_names_prcp = paste0("./data/daymet/prcp_monttl_", seq(from = 1980, to = 2000, by = 1), "_ncss.nc")
file_names_tmax = paste0("./data/daymet/tmax_monavg_", seq(from = 1980, to = 2000, by = 1), "_ncss.nc")
file_names_tmin = paste0("./data/daymet/tmin_monavg_", seq(from = 1980, to = 2000, by = 1), "_ncss.nc")

biovar_t1 = raster::brick(nrows = 3648, ncols = 2880, crs = "+proj=lcc +lon_0=-100 +lat_0=42.5 +x_0=0 +y_0=0 +lat_1=25 +a=60 +rf=6378137 +lat_2=45")

for(i in 1:length(1980:2000)) {
  biovar_t1[[i]] = biovars(prcp = raster::stack(x = file_names_prcp[i]),
                            tmax = raster::stack(x = file_names_tmax[i]),
                            tmin = raster::stack(x = file_names_tmin[i]))
}

test_prcp = nc_open(file_names_prcp[1])
test_tmax= nc_open(file_names_tmax[1])
test_tmin = nc_open(file_names_tmin[1])

prcp_matrix = ncvar_get(test_prcp, raw_datavals = TRUE)
tmax_matrix = ncvar_get(test_tmax, raw_datavals = TRUE)
tmin_matrix = ncvar_get(test_tmin, raw_datavals = TRUE)

biovar_test = biovars(prcp = prcp_matrix,
                      tmin = tmin_matrix,
                      tamx = tmax_matrix)


#testing out an alternate approach
test <- raster::getData(name = "worldclim",
                                var = "tmin",
                                res = 2.5,
                                path = "./data/")
