#Script to process spatial data for ssDFs 

#truly unsure if all these needed. ah well. 
library(dplyr)
library(car)
library(oSCR)
library(reshape2)
library(raster)
library(rgdal)
library(maptools)
library(ggplot2)
library(sp)
library(rgeos)
library(tibble)
library(lubridate)
library(BiocManager)
library(fuzzyjoin)
library(IRanges)
library(spatialEco)
library(here)

#this folder path gets from socal_bcat up 3 folders to the reproducible research overarching folders
#(./../../../)

##WATER (dist to in m)
water_dist<- raster("./../../../Original Data/Original spatial data from Megan/dist_water.tif")
plot(water_dist)
#have to change the rasters to the correct projection
projection<-"+proj=utm +zone=11 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
#projecting from 30 m res to 10 m res
water_dist_10 <- projectRaster(water_dist, crs=projection, res=10)
#so that this can then be aggregated to 500x500m resolution to match the state space 
water_dist_500<- aggregate(water_dist_10, fact=50)
##plot to make sure it all looks good 
plot(water_dist_500)

##used scale funciton in raster package to scale each site/covariate 
##(in a test model run w/ unscaled elev covs, model failed immediately. that's what made me remember had to scale)
water_dist_500.scale<-scale(water_dist_500)
##use one to check that scaling went correctly
plot(water_dist_500.scale)

###ELEVATION (m)
elev<- raster("./../../../Original Data/Original spatial data from Megan/elevation_NAD83_UTM11N.tif")
plot(elev)
#have to change the rasters to the correct projection
projection<-"+proj=utm +zone=11 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
#projecting from 30 m res to 10 m res
elev_10 <- projectRaster(elev, crs=projection, res=10)
#so that this can then be aggregated to 500x500m resolution to match the state space 
elev_500<- aggregate(elev_10, fact=50)
##plot to make sure it all looks good 
plot(elev_500)

##used scale funciton in raster package to scale each site/covariate 
##(in a test model run w/ unscaled elev covs, model failed immediately. that's what made me remember had to scale)
elev_500.scale<-scale(elev_500)
##use one to check that scaling went correctly
plot(elev_500.scale)

###IMPERVIOUSNESS (percent)
imperv<- raster("./../../../Original Data/Original spatial data from Megan/SD_CombImprv.tif")
plot(imperv)
#have to change the rasters to the correct projection
projection<-"+proj=utm +zone=11 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
#projecting from 30 m res to 10 m res
imperv_10 <- projectRaster(imperv, crs=projection, res=10)
#so that this can then be aggregated to 500x500m resolution to match the state space 
imperv_500<- aggregate(imperv_10, fact=50)
##plot to make sure it all looks good 
plot(imperv_500)

##used scale funciton in raster package to scale each site/covariate 
##(in a test model run w/ unscaled elev covs, model failed immediately. that's what made me remember had to scale)
imperv_500.scale<-scale(imperv_500)
##use one to check that scaling went correctly
plot(imperv_500.scale)


#Road related covs 
road465<- raster("./../../../Original Data/Original spatial data from Megan/all_roads_scale_465_m.tif")
road1000<- raster("./../../../Original Data/Original spatial data from Megan/all_roads_scale_1000_m.tif")

projection<-"+proj=utm +zone=11 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"

road465.1 <-projectRaster(road465, crs=projection)
road1000.1<-projectRaster(road1000,crs=projection)
## i THINK the resolution on these doesn't need to be changed because they are already aggregated
# even if the pixel size doesn't match the state space res 

#scale func from raster package to scale 
road465.1.scale<-scale(road465.1)
road1000.1.scale<-scale(road1000.1)



###then save
save(water_dist_500.scale,elev_500.scale,imperv_500.scale,
     road465.1.scale,road1000.1.scale,
     file = "./../../../Processed Data/Spatial Data/socalbcat_ssDFcovrasters.rda")
getwd()
save(imperv_500,file = "./../../../Processed Data/Spatial Data/unscaled_imperv.rda")
