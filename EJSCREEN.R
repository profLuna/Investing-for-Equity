# Transportation related emissions and burdens from EPA's EJSCREEN and DART's on-road CO2 emissions
library(tidyverse)
library(sf)
library(tmap)
library(tmaptools)
library(maptools)
library(raster)
library(rgdal)
library(RColorBrewer)
library(sp)

# Download EJSCREEN Data from EPA. See https://www.epa.gov/ejscreen/download-ejscreen-data for details.
download.file(
  url = "ftp://newftp.epa.gov/EJSCREEN/2019/EJSCREEN_2019_USPR.csv.zip", 
  destfile = "DATA/EJSCREEN/EJSCREEN_2019_USPR.csv.zip")
download.file(
  url = "ftp://newftp.epa.gov/EJSCREEN/2015/EJSCREEN_20150505.csv.zip", 
  destfile = "DATA/EJSCREEN/EJSCREEN_20150505.csv.zip")
# Unzip the file
unzip("DATA/EJSCREEN/EJSCREEN_2019_USPR.csv.zip", exdir = "DATA/EJSCREEN")
unzip("DATA/EJSCREEN/EJSCREEN_20150505.csv.zip", exdir = "DATA/EJSCREEN")

# Read in EJSCREEN Data
EJSCREEN <- read_csv("EJSCREEN_2019_USPR.csv")
glimpse(EJSCREEN)
names(EJSCREEN)
head(EJSCREEN$ID)
# Isolate select air pollution related variables and join to sf block groups
EJSCREEN.air_sf <- EJSCREEN %>% 
  dplyr::select(ID,DSLPM,CANCER,RESP,PTRAF,OZONE,PM25) %>% 
  left_join(ne_blkgrp_sf, ., by = c("GEOID" = "ID"))
rm(EJSCREEN) # clean up after yourself

# Convert air pollution variables to numeric
names(EJSCREEN.air_sf)
EJSCREEN.air_sf <- EJSCREEN.air_sf %>% 
  mutate_at(c("DSLPM","CANCER","RESP","PTRAF","OZONE","PM25"), as.numeric)

# sapply does not work with sf
# EJSCREEN.air_sf[,9:14] <- sapply(EJSCREEN.air_sf[,9:14],as.numeric)

# Read in DARTE Annual On-road CO2 Emissions on a 1-km Grid. See https://daac.ornl.gov/cgi-bin/dsviewer.pl?ds_id=1735 
# view relevant tif tags embedded within a geotiff before openning it
GDALinfo("traffic/onroad_2017.tif")
# load the data
CO2_2017 <- raster("traffic/onroad_2017.tif")
# what's inside
CO2_2017
summary(CO2_2017, maxsamp = ncell(CO2_2017))
# check stats
cellStats(x = CO2_2017, stat = "mean")
cellStats(x = CO2_2017, stat = "min")
cellStats(x = CO2_2017, stat = "max")
# Explore palettes
palette_explorer()
# Set min and max values to allow color palette
CO2_2017 <- setMinMax(CO2_2017)
# Take a look at it
tm_shape(CO2_2017) + tm_raster(style = "quantile",
                               palette = "-Spectral", n = 50, midpoint = NA) + 
  tmap_options(max.raster = c(plot = 18049200, view = 18049200)) +
  tm_layout(bg.color = "black", 
            legend.text.color = "white", legend.show = FALSE)
# Crop to New England
# Create copy of ne_blkgrp_sf with same CRS
ne_blkgrp_sf_lcc <- st_transform(ne_blkgrp_sf, proj4string(CO2_2017))
# Crop raster to New England
CO2_2017ne <- crop(CO2_2017, ne_blkgrp_sf_lcc)
CO2_2017ne
summary(CO2_2017ne)
rm(CO2_2017) # clean up after yourself
# Create a df of raster to analyze data more closely
CO2_2017ne_df <- as.data.frame(CO2_2017ne, xy = TRUE)
str(CO2_2017ne_df)
summary(CO2_2017ne_df$onroad_2017)
# Create a histogram to see data distribution
ggplot() + geom_histogram(data = CO2_2017ne_df, aes(onroad_2017))
# Let's see if we can make data distribution more normal
ggplot() + geom_histogram(data = CO2_2017ne_df, aes(log10(onroad_2017)))
# Convert from kilograms/km2 to metric tons/km2
CO2_2017ne_tons <- CO2_2017ne/1000
# Create log10 version for normal distribution to improve mapping
CO2_2017ne_tons_log <- log(CO2_2017ne/1000)
summary(CO2_2017ne_tons_log)
# Replace -Inf values in raster with NA
CO2_2017ne_tons_log[!is.finite(CO2_2017ne_tons_log[])] <- NA
# Map it out
tm_shape(CO2_2017ne_tons) + 
  tm_raster(style = "log10_pretty", 
            palette = "-Spectral", n = 50, midpoint = NA, ) +
  tm_layout(bg.color = "black", 
            legend.text.color = "white", legend.show = FALSE)

# display.brewer.all()

# To extract raster values, need to first address empty geometries in polygon layer
# check for empty geometries
any(is.na(st_dimension(ne_blkgrp_sf_lcc)))
# identiy empty geometries
empty_geo <- st_is_empty(ne_blkgrp_sf_lcc)
# filter out empty geometries
ne_blkgrp_sf_lcc2 <- ne_blkgrp_sf_lcc[!empty_geo,]

# Extract mean CO2 values within each block group to an spdf
meanCO2 <- extract(CO2_2017ne_tons, as_Spatial(ne_blkgrp_sf_lcc2), 
                   fun=mean, sp=TRUE, na.rm=TRUE, small=TRUE)
# Map it
tm_shape(meanCO2) + tm_fill("onroad_2017", style = "quantile")


# Look at relationship of income to diesel
EJSCREEN.air_sf %>% 
  as.data.frame() %>% 
  drop_na(medhhincE) %>%  
  ggplot(aes(x = medhhincE, y = DSLPM)) + geom_point(alpha = 0.1) +
  facet_wrap("STATE")+ geom_smooth(method = "lm")

EJSCREEN.air_sf %>% 
  as.data.frame() %>% 
  filter(STATE == "Maine") %>% 
  drop_na(medhhincE) %>%  
  ggplot(aes(x = medhhincE, y = DSLPM)) + geom_point(alpha = 0.3) + 
  geom_smooth(method = "lm")

# Look at relationship of income to PM25
EJSCREEN.air_sf %>% 
  as.data.frame() %>% 
  drop_na(medhhincE) %>%  
  ggplot(aes(x = medhhincE, y = PM25)) + geom_point() +
  facet_wrap("STATE")

# Look at relationship of income to CANCER
EJSCREEN.air_sf %>% 
  as.data.frame() %>% 
  drop_na(medhhincE) %>%  
  ggplot(aes(x = medhhincE, y = CANCER)) + geom_point() +
  facet_wrap("STATE")

# Look at relationship of income to RESP
EJSCREEN.air_sf %>% 
  as.data.frame() %>% 
  drop_na(medhhincE) %>%  
  ggplot(aes(x = medhhincE, y = RESP)) + geom_point() +
  facet_wrap("STATE")

# Look at relationship of income to PTRAF
EJSCREEN.air_sf %>% 
  as.data.frame() %>% 
  drop_na(medhhincE) %>%  
  ggplot(aes(x = medhhincE, y = PTRAF)) + geom_point() +
  facet_wrap("STATE")

# Look at relationship of income to OZONE
EJSCREEN.air_sf %>% 
  as.data.frame() %>% 
  drop_na(medhhincE) %>%  
  ggplot(aes(x = medhhincE, y = OZONE)) + geom_point() +
  facet_wrap("STATE")

# Compute population-weighted mean by state
EJSCREEN.air_sf %>% 
  as.data.frame() %>% 
  group_by(STATE) %>% 
  summarize(DieselPM.wAvg = weighted.mean(DSLPM,totalpopE, na.rm = TRUE),
            DieselPM.Avg = mean(DSLPM, na.rm = TRUE))


# Example of demographic df for computing weighted means by column
df <- data.frame(
  White = as.integer(c(seq(from = 10, to = 20, length.out = 10))),
  Black = seq(10:1),
  Pollute = seq(0.1:1,length.out = 10))

# Convert df to long format to compute weighted means
# 'key' is new column with groups, 'value' is the value for each record, and 'White:Black' define the columns to convert to records in the Group column
df %>% 
  gather(key = Group, value = Pop, White:Black) %>% 
  group_by(Group) %>% 
  summarize(weighted.mean(x = Pollute, w = Pop))



# Impact of choosing different projections for mapping
# NAD83 / Conus Albers
ne_blkgrp_sf_5070 <- st_transform(ne_blkgrp_sf, crs = 5070)
tm_shape(ne_blkgrp_sf_5070) + tm_borders()
# US National Atlas Equal Area
ne_blkgrp_sf_2163 <- st_transform(ne_blkgrp_sf, crs = 2163)
tm_shape(ne_blkgrp_sf_2163) + tm_borders()
# NAD83 / UTM zone 19N
ne_blkgrp_sf_26919 <- st_transform(ne_blkgrp_sf, crs = 26919)
tm_shape(ne_blkgrp_sf_26919) + tm_borders()
