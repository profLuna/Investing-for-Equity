# TCI LST analysis
# The objectives of this analysis are: create temperature and urban heat island surfaces for New England; validate with NOAA weather station data and MA impervious surface layer; aggregate by Census block group; compare against demographics and air pollution data from EPASCREEN
# Need to match time period of LST to data for comparison:
# EJSCREEN air and cancer hazards and diesel particulate are 2014 annual
# EJSCREEN PM2.5 is annual average based on 2013 monitoring and modeling estimates
# EJSCREEN Ozone is May–September (summer/ ozone season) average of daily-maximum 8-hour-average ozone concentrations, in parts per billion (ppb) based on 2013 monitoring and modeling estimates
# NATA Cancer Risk and Respirtatory Hazard Risk data based on tract-level estimates using 2014 NEI
# Separately we will use 2018 summer data to identify heat islands and relationships between demographics and temps


# install.packages('rgdal', type='source'). DOES NOT WORK
# had to install GDAL/OGR from https://trac.osgeo.org/osgeo4w/
library(tmap)
library(tidyverse)
library(sf)
library(rgdal)
library(gdalUtils)
library(raster)
# library(MODIS)
# MODISoptions(gdalPath="C:/OSGeo4W64/bin")

# read in HDF (Hierarchical Data Format) scientific data sets (SDSs)
# sdsJune2019h12 <- get_subdatasets("LST/MOD11B3.A2019152.h12v04.006.2019184065854.hdf")
# sdsJune2019h13 <- get_subdatasets("LST/MOD11B3.A2019152.h13v04.006.2019184065855.hdf")
# MODIS:::checkTools()
# getSds("DATA/LST/MOD11A2.A2019209.h12v04.006.2019218045630.hdf")

sdsJuly282019h12 <- get_subdatasets("LST/MODIS8dayLST1km/2345441438/MOD11A2_A2019209_h12v04_006_2019218045630_HEGOUT.hdf")
sdsJuly282019h13 <- get_subdatasets("LST/MODIS8dayLST1km/2345441418/MOD11A2_A2019209_h13v04_006_2019218045706_HEGOUT.hdf")

# view available data sets
# sdsJune2019h12
# sdsJune2019h13
sdsJuly282019h12
sdsJuly282019h13
# retrieve desired data from dataset
# LSTdayJune2019h12 <- readGDAL(sdsJune2019h12[1])
# LSTnightJune2019h12 <- readGDAL(sdsJune2019h12[5])
# LSTdayJune2019h13 <- readGDAL(sdsJune2019h13[1])
# LSTnightJune2019h13 <- readGDAL(sdsJune2019h13[5])
LSTdayJuly282019h12 <- readGDAL(sdsJuly282019h12[1])
LSTnightJuly282019h12 <- readGDAL(sdsJuly282019h12[5])
LSTdayJuly282019h13 <- readGDAL(sdsJuly282019h13[1])
LSTnightJuly282019h13 <- readGDAL(sdsJuly282019h13[5])
# assign to raster image to manipulate data for visualization and analysis and convert from Kelvin to Fahrenheit
LSTdayJuly282019h12r <- raster(LSTdayJuly282019h12)*(9/5) - 459.67
LSTnightJuly282019h12r <- raster(LSTnightJuly282019h12)*(9/5) - 459.67
LSTdayJuly282019h13r <- raster(LSTdayJuly282019h13)*(9/5) - 459.67
LSTnightJuly282019h13r <- raster(LSTnightJuly282019h13)*(9/5) - 459.67
# Convert from Kelvin to Fahrenheit
# LSTday_rasterF <- LSTday_raster*(9/5) - 459.67
# LSTnight_rasterF <- LSTnight_raster*(9/5) - 459.67

# Make sure rasters coincide
tm_shape(LSTdayJuly282019h12r) + tm_raster() +
  tm_shape(LSTdayJuly282019h13r) + tm_raster()

# If origins are different, need to adjust tolerance to mosaic
origin(LSTdayJuly282019h12r)
origin(LSTdayJuly282019h13r)

# mosic rasters
# create LST day mosaic raster
LSTdayJuly2019r <- mosaic(LSTdayJuly282019h12r,LSTdayJuly282019h13r,
                          fun=mean, tolerance = 1)
tm_shape(LSTdayJuly2019r) + tm_raster()
# create LST night mosaic raster
LSTnightJuly2019r <- mosaic(LSTnightJuly282019h12r,LSTnightJuly282019h13r,
                          fun=mean, tolerance = 1)
tm_shape(LSTnightJuly2019r) + tm_raster()

# create average of day and night
LSTavgJuly2019r <- (LSTdayJuly2019r + LSTnightJuly2019r)/2

# clean up
rm(list = ls(pattern = "h12|h13"))

# Crop LST to New England
load("DATA/ne_layers.rds")
# define new projection
newproj <- st_crs(ne_blkgrp_sf)[[2]]

LSTavgJuly2019r_ne <- LSTavgJuly2019r %>% 
  projectRaster(., crs = newproj) %>% 
  crop(., ne_states_sf_cb)

LSTdayJuly2019r_ne <- LSTdayJuly2019r %>% 
  projectRaster(., crs = newproj) %>% 
  crop(., ne_states_sf_cb) 

LSTnightJuly2019r_ne <- LSTnightJuly2019r %>% 
  projectRaster(., crs = newproj) %>% 
  crop(., ne_states_sf_cb)

# Identify stats to compute thresholds
LSTavgJuly2019mean <- cellStats(x = LSTavgJuly2019r_ne, stat = "mean")
LSTdayJuly2019mean <- cellStats(x = LSTdayJuly2019r_ne, stat = "mean")
LSTnightJuly2019mean <- cellStats(x = LSTnightJuly2019r_ne, stat = "mean")
LSTavgJuly2019sd <- cellStats(x = LSTavgJuly2019r_ne, stat = "sd")
LSTdayJuly2019sd <- cellStats(x = LSTdayJuly2019r_ne, stat = "sd")
LSTnightJuly2019sd <- cellStats(x = LSTnightJuly2019r_ne, stat = "sd")

# Create rasters of cells exceeding threshold
LSTavgJulyHI <- LSTavgJuly2019r_ne > 
  (LSTavgJuly2019mean + LSTavgJuly2019sd * 3)
LSTdayJulyHI <- LSTdayJuly2019r_ne > 
  (LSTdayJuly2019mean + LSTdayJuly2019sd * 3)
LSTnightJulyHI <- LSTnightJuly2019r_ne > 
  (LSTnightJuly2019mean + LSTnightJuly2019sd * 3)

# Extract raster values to block groups
# check for empty geometries
any(is.na(st_dimension(ne_blkgrp_sf)))
# identiy empty geometries
empty_geo <- st_is_empty(ne_blkgrp_sf)
# filter out empty geometries
ne_blkgrp_sf <- ne_blkgrp_sf[!empty_geo,]
# clean up
rm(empty_geo)

# Extract mean LST values within each block group to a df
meanLSTavg_df <- ne_blkgrp_sf %>% 
  dplyr::select(GEOID) %>% 
  as_Spatial() %>% 
  extract(LSTavgJuly2019r_ne, ., 
          fun=mean, sp=TRUE, na.rm=TRUE, small=TRUE) %>% 
  as.data.frame() %>% 
  rename(meanAvgLST = layer)

meanLSTday_df <- ne_blkgrp_sf %>% 
  dplyr::select(GEOID) %>% 
  as_Spatial() %>% 
  extract(LSTdayJuly2019r_ne, ., 
          fun=mean, sp=TRUE, na.rm=TRUE, small=TRUE) %>% 
  as.data.frame() %>% 
  rename(meanDayLST = layer)

meanLSTnight_df <- ne_blkgrp_sf %>% 
  dplyr::select(GEOID) %>% 
  as_Spatial() %>% 
  extract(LSTnightJuly2019r_ne, ., 
          fun=mean, sp=TRUE, na.rm=TRUE, small=TRUE) %>% 
  as.data.frame() %>% 
  rename(meanNightLST = layer)

# Join LST statistics to block groups
ne_blkgrp_sf <- ne_blkgrp_sf %>% 
  left_join(., meanLSTavg_df, by = "GEOID") %>% 
  left_join(., meanLSTday_df, by = "GEOID") %>% 
  left_join(., meanLSTnight_df, by = "GEOID")

save(ne_blkgrp_sf, file = "DATA/ne_blkgrpLST_sf.rds")
