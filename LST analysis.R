# TCI EJ analysis

# install.packages('rgdal', type='source'). DOES NOT WORK
# had to install GDAL/OGR from https://trac.osgeo.org/osgeo4w/
library(tmap)
library(tidyverse)
library(rgdal)
library(gdalUtils)
library(raster)
# read in HDF (Hierarchical Data Format) scientific data sets (SDSs)
sdsJune2019h12 <- get_subdatasets("LST/MOD11B3.A2019152.h12v04.006.2019184065854.hdf")
sdsJune2019h13 <- get_subdatasets("LST/MOD11B3.A2019152.h13v04.006.2019184065855.hdf")
# view available data sets
sdsJune2019h12
sdsJune2019h13
# retrieve desired data from dataset
LSTdayJune2019h12 <- readGDAL(sdsJune2019h12[1])
LSTnightJune2019h12 <- readGDAL(sdsJune2019h12[5])
LSTdayJune2019h13 <- readGDAL(sdsJune2019h13[1])
LSTnightJune2019h13 <- readGDAL(sdsJune2019h13[5])
# assign to raster image to manipulate data for visualization and analysis and convert from Kelvin to Fahrenheit
LSTdayJune2019h12r <- raster(LSTdayJune2019h12)*(9/5) - 459.67
LSTnightJune2019h12r <- raster(LSTnightJune2019h12)*(9/5) - 459.67
LSTdayJune2019h13r <- raster(LSTdayJune2019h13)*(9/5) - 459.67
LSTnightJune2019h13r <- raster(LSTnightJune2019h13)*(9/5) - 459.67
# Convert from Kelvin to Fahrenheit
# LSTday_rasterF <- LSTday_raster*(9/5) - 459.67
# LSTnight_rasterF <- LSTnight_raster*(9/5) - 459.67

# merge rasters
m1 <- mosaic(LSTdayJune2019h12r,LSTdayJune2019h13r,fun=mean)
# reproject to WGS84
newproj <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
m1WGS84 <- projectRaster(m1,crs = newproj)

# create avg of day and night
LSTavg_rasterF <- (LSTday_rasterF+LSTnight_rasterF)/2
# map the data
tm_shape(LSTday_rasterF) + tm_raster()
tm_shape(LSTnight_rasterF) + tm_raster()
tm_shape(LSTavg_rasterF) + tm_raster()
tm_shape(LSTdayJune2019h12r)+ tm_raster()
tm_shape(LSTdayJune2019h13r)+ tm_raster()
tm_shape(m1) + tm_raster()
tm_shape(m1WGS84) + tm_raster()

# find stats of raster
m1WGS84mean <- cellStats(x = m1WGS84, stat = "mean")
m1WGS84sd <- cellStats(x = m1WGS84, stat = "sd")
(threshold <- m1WGS84mean+m1WGS84sd*1.5)
hot <- m1WGS84 > threshold
tm_shape(hot) + tm_raster(alpha = 0.5)
tmap_mode("view")
tmap_mode("plot")