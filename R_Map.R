library(envirem)
library(ggmap)
library(rgbif)
library(fs)
require("maptools")
library(rgdal)
require(sp)
library(plyr)
library(dplyr)
library(tidyverse)
library(sf)
# load libraries
library(raster)
library(ggplot2)
library(broom)
library(RColorBrewer)
library(rgeos)
library(dplyr)
# note that you don't need to call maptools to run the code below but it needs to be installed.
library(maptools)
# to add a north arrow and a scale bar to the map
library(ggsn)
# set factors to false
options(stringsAsFactors = FALSE)
library(devtools)

# retrieve the raster from the your folder containing the data desider 
MAT <- raster("Present.Surface.Temperature.Mean.asc")
Range <- raster("Present.Surface.Temperature.Range.asc")
LTMAX <- raster("Present.Surface.Temperature.Lt.max.asc")
LTMIN <- raster("Present.Surface.Temperature.Lt.min.asc")
pH <- raster("Present.Surface.pH.BOv2_2.asc")
Salinity <- raster("Present.Surface.Salinity.Mean.asc")

plot(MAT)
# set the coordinates of your survey: remember to convert the grade to decimal lat long. Look for example https://www.latlong.net/lat-long-dms.html
latlong<-read.csv("Tara.csv")
extcoords <- matrix(c(latlong$Long,latlong$Lat),ncol = 2)
vals_environment_tmin_Jan1980<- Salinity[cellFromXY(Salinity,extcoords)]


                   
write.table(vals_environment_tmin_Jan1980,"Tara_Salinity.txt",sep='\t')


#This work as weel
test.dat <- structure(list(latitude = c(-62.2231), longitude = c(-49.2139)), 
                      .Names = c("latitude","longitude"),
                      class = "data.frame", row.names = c(NA, 6L))


test.dat
points         <- cbind(test.dat$longitude,test.dat$latitude)

sppoints <- SpatialPoints(points, proj4string=CRS('+proj=longlat +datum=WGS84'))
tp <- spTransform(sppoints, crs(MAT))
e <- extract(aridityRaster,tp)
e


