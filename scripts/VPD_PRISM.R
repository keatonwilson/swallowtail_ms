rm(list = ls())
library(ncdf4)
library(ncdf)
library(fields)
library(Hmisc)
c <- open.ncdf("~/Documents/Climate Data/tmax_prism.nc")
a <- open.ncdf("~/Documents/Climate Data/tmin_prism.nc")
b <- open.ncdf("~/Documents/Climate Data/tdmean_prism.nc")

summary(b)
summary(a)
xdim <- round(a$dim[[1]]$vals, digits = 5)# lon
ydim <- round(a$dim[[2]]$vals, digits = 5) # lat
zdim <- a$dim[[3]]$vals # time


xs <- which(xdim == -117.00000)
ys <-  which(ydim == 28.00000)

dim(zdim)/12
timespan <- dim(zdim)
zs <- 1

ptm <- proc.time()
tmax<- get.var.ncdf(c, start = c(xs,ys,zs), count = c(400, 400,timespan))     #1895-2015
SatVap_max =(0.6108*exp((17.27*tmax)/(tmax+273.3)))*10
rm(tmax)
rm(c)
proc.time() - ptm

ptm <- proc.time()
tmin<- get.var.ncdf(a, start = c(xs,ys,zs), count = c(400, 400,timespan))     #1895-2015
SatVap_min =(0.6108*exp((17.27*tmin)/(tmin+273.3)))*10
rm(tmin)
rm(a)
proc.time() - ptm



ptm <- proc.time()
tdmean <- get.var.ncdf(b, start = c(xs,ys,zs), count = c(400, 400,timespan))     #1895-2015
vapp <- (0.6108*exp((17.27*tdmean)/(tdmean+273.3)))*10
rm(tdmean)
rm(b)
proc.time() - ptm

ptm <- proc.time()
vpd_PRISM_max = SatVap_max-vapp
vpd_PRISM_min = SatVap_min-vapp
vpd_PRISM= (vpd_PRISM_max+vpd_PRISM_min)/2
proc.time() - ptm

rm(vpd_PRISM_max)
   rm(SatVap_max) 
      rm(vpd_PRISM_min) 
         rm(SatVap_min) 
            rm(vapp)



vpd_PRISM <- round(vpd_PRISM, digits = 4)
# calculation of VPD Clausius-Clapeyron equation IN kPa..

longitude <- xdim[c(xs:(xs+399))]
latitude <- ydim[c(ys:(ys+399))]
time <- timespan #=dim(zdim)

dimX <- dim.def.ncdf("Lon", "degrees", longitude)
dimY <- dim.def.ncdf("Lat", "degrees", latitude)
dimT <- dim.def.ncdf("Time", "months_since_start1959", seq(1,timespan,1), unlim = FALSE)
mv <- -9999
var1d <- var.def.ncdf( "var1d", "units", dimX, mv,prec="double")
var2d <- var.def.ncdf( "var2d", "units", list(dimX,dimY), mv,prec="double")
var3d <- var.def.ncdf( "var3d", "kPa", list(dimX,dimY,dimT),missval =  mv,prec="double")

vpd.nc <- create.ncdf( "~/Documents/Climate Data/PRISM_vpd_USA_kPa_1959-2015.nc", list(var1d,var2d,var3d))
put.var.ncdf(vpd.nc, var3d, vpd_PRISM)
close.ncdf(vpd.nc)


rm(list = ls())


