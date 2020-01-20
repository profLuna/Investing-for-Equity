# Transportation related emissions and burdens from EPA's EJSCREEN and DART's on-road CO2 emissions
library(tidyverse)
library(sf)
library(tmap)
library(tmaptools)
library(maptools)
# library(raster)
library(rgdal)
library(RColorBrewer)
library(sp)
library(CGPfunctions) # for slope graphs

load("DATA/ne_layers.rds")

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

# Read in EJSCREEN Data, isolate variables, fix types, append date to names
EJSCREEN19 <- read_csv("DATA/EJSCREEN/EJSCREEN_2019_USPR.csv") %>% 
  filter(ST_ABBREV %in% ne_states) %>% 
  dplyr::select(ID,DSLPM,CANCER,RESP,PTRAF,OZONE,PM25) %>% 
  mutate_at(c("DSLPM","CANCER","RESP","PTRAF","OZONE","PM25"), as.numeric) %>% 
  rename_all(paste0, "_19")
  
EJSCREEN15 <- read_csv("DATA/EJSCREEN/EJSCREEN_20150505.csv") %>% 
  filter(ST %in% ne_states) %>% 
  dplyr::select(c(1:19,28:30,32,37:39)) %>% 
  mutate_at(c("dpm", "cancer", "resp"), as.numeric) %>% 
  rename_all(paste0, "_15")

# Join data frames and compute percent changes
EJSCREEN_15_19 <- left_join(EJSCREEN15,EJSCREEN19, 
                            by = c("FIPS_15" = "ID_19")) %>% 
  mutate(OZONE_pctChange = (OZONE_19 - o3_15)/o3_15 * 100,
         PM25_pctChange = (PM25_19 - pm_15)/pm_15 * 100,
         PTRAF_pctChange = (PTRAF_19 - traffic.score_15)/traffic.score_15 * 100)
# clean up
rm(EJSCREEN15,EJSCREEN19)



# Import CO2 by block group from DARTE geodatabase. See https://daac.ornl.gov/cgi-bin/dsviewer.pl?ds_id=1735 
onroadCO2 <- st_read(dsn = "DATA/DARTE/DARTE_v2.gdb/DARTE_v2.gdb", 
                     layer = "DARTE_v2_blockgroup_kgco2_1980_2017") %>% 
  mutate(GEOID = as.character(GEOID))


# Compute CO2 by block group with percent change
# Read in DARTE Annual On-road CO2 Emissions on a 1-km Grid. See https://daac.ornl.gov/cgi-bin/dsviewer.pl?ds_id=1735 
# view relevant tif tags embedded within a geotiff before openning it
# GDALinfo("traffic/onroad_2017.tif")
# load the data
# CO2_2017 <- raster("DATA/DARTE/onroad_2017.tif")
# CO2_1990 <- raster("DATA/DARTE/onroad_1990.tif")
# 
# # Crop to New England
# # Create copy of ne_blkgrp_sf with same CRS
# ne_blkgrp_sf_lcc <- st_transform(ne_blkgrp_sf, proj4string(CO2_2017))
# 
# # Crop raster to New England and convert kilograms/km2 to metric tons/km2
# CO2_2017ne_tons <- crop(CO2_2017, ne_blkgrp_sf_lcc) %>% 
#   `/`(1000)
# CO2_1990ne_tons <- crop(CO2_1990, ne_blkgrp_sf_lcc) %>% 
#   `/`(1000)
# # clean up
# rm(CO2_1990,CO2_2017)
# 
# # To extract raster values, need to first address empty geometries in polygon layer
# # check for empty geometries
# any(is.na(st_dimension(ne_blkgrp_sf_lcc)))
# # identiy empty geometries
# empty_geo <- st_is_empty(ne_blkgrp_sf_lcc)
# # filter out empty geometries
# ne_blkgrp_sf_lcc <- ne_blkgrp_sf_lcc[!empty_geo,]
# # clean up
# rm(empty_geo)
# 
# # Extract mean CO2 values within each block group to an spdf
# meanCO2_17 <- extract(CO2_2017ne_tons, as_Spatial(ne_blkgrp_sf_lcc), 
#                    fun=mean, sp=TRUE, na.rm=TRUE, small=TRUE)
# 
# meanCO2_90 <- extract(CO2_1990ne_tons, as_Spatial(ne_blkgrp_sf_lcc), 
#                       fun=mean, sp=TRUE, na.rm=TRUE, small=TRUE)

# Map it
# tm_shape(meanCO2_17) + tm_fill("onroad_2017", style = "quantile")
# tm_shape(meanCO2_90) + tm_fill("onroad_1990", style = "quantile")

# Join CO2 values to EJSCREEN block groups
EJSCREEN_15_19 <- onroadCO2 %>%
  as.data.frame() %>%
  left_join(EJSCREEN_15_19, ., by = c("FIPS_15" = "GEOID"))

# clean up
rm(onroadCO2)

# Join EJSCREEN data to block groups with demographics
ne_blkgrp_sf <- left_join(ne_blkgrp_sf,EJSCREEN_15_19, 
                                by = c("GEOID" = "FIPS_15"))

# Join EJSCREEN data to towns
ne_towns_sf <- ne_blkgrp_sf %>% 
  st_transform(., st_crs(ne_towns_sf)) %>% 
  st_join(., ne_towns_sf, largest = TRUE) %>% 
  dplyr::select(GEOID.y, DSLPM_19:kgco2_2017) %>% 
  mutate(AvgBlkAREA = as.numeric(st_area(.))) %>% # convert from 'units' to numeric 
  group_by(GEOID.y) %>%  
  summarize_if(is.numeric, 
               list(~ weighted.mean(., w=AvgBlkAREA, na.rm = TRUE))) %>% 
  as.data.frame() %>% 
  left_join(ne_towns_sf, ., by = c("GEOID" = "GEOID.y"))


# clean up
rm(EJSCREEN_15_19)


# Assemble state boundaries and select cities for mapping
# Download cartographic boundary file of states from tigris
library(tigris)
ne_states_sf_cb <- states(cb = TRUE) %>% 
  st_as_sf() %>% 
  filter(STUSPS %in% ne_states)
# tm_shape(ne_states_sp) + tm_borders()

# Create point layer of state capitols for context
library(tigris)
options(tigris_class = "sf")
# Note cb=FALSE is necessary for extracting centroids from town polygons. Otherwise, if cb=TRUE, cannot extract centroids from multipolygon features.
ne_towns_sf_pts <- ne_towns_sf <- rbind_tigris(
  lapply(
    ne_states, function(x){
      county_subdivisions(state = x, cb = FALSE)})) %>% 
  filter((NAMELSAD == "Boston city" & STATEFP == "25") | 
           (NAMELSAD == "Portland city" & STATEFP == "23") |
           (NAMELSAD == "Hartford town" & STATEFP == "09") |
           (NAMELSAD == "Providence city" & STATEFP == "44") |
           (NAMELSAD == "Portsmouth city" & STATEFP == "33") |
           (NAMELSAD == "Montpelier city" & STATEFP == "50")) %>% 
  st_centroid(of_largest_polygon = TRUE)


# save original and joined spatial files with demographics
save(ne_blkgrp_sf,
     ne_blkgrp_sf90,
     ne_tracts_sf,
     ne_towns_sf,
     ne_towns_sf_pts,
     ne_states,
     ne_states_sf_cb,
     file = "DATA/ne_layers.rds")


# SUMMARY STATISTICS AND MAPS OF DEMOGRAPHICS/EJ INDICES BY REGION AND STATE
# Map of populations of concern for New England
# create a directory to save shapefiles
# dir.create("DATA/shapefiles")
# # download the zipped shapefile from MassGIS into shapefiles directory
# download.file(url = "http://download.massgis.digital.mass.gov/shapefiles/ne/newengland.zip",
#               destfile = "DATA/shapefiles/newengland.zip")
# 
# # unzip the downloaded shapefile into the shapefiles directory
# unzip(zipfile = "DATA/shapefiles/newengland.zip", exdir = "./DATA/shapefiles")
# 
# # read in shapefile using sf::st_read
# ne_states_sf <- st_read(dsn = "DATA/shapefiles", layer = "NEWENGLAND_POLY")
# 
# # Add state abbreviations to state layer for labeling
# state.names <- sort(as.character(unique(ne_states_sf$NAME)))
# state.names <- data.frame(
#   state.abbrev = c("CT","ME","MA","NH","RI","VT"),
#   state.names
# )
# ne_states_sf <- left_join(ne_states_sf, state.names, 
#                           by = c("NAME" = "state.names"))



# NEW ENGLAND
# Minorities
ne_blkgrp_sf_DemoEJ %>% 
  as.data.frame() %>% 
  summarize(Minorities = sum(minorityE),
            PctMinorities = paste0(
              round(sum(minorityE)/sum(totalpopE)*100,1),
              "%"))
# Map data
# version 1
# tmap_mode("plot")
# tm_shape(ne_blkgrp_sf_DemoEJ, unit = "mi") + 
#   tm_fill("minority_pctE", style = "pretty", title = "Percent",
#           legend.hist = TRUE, 
#           legend.is.portrait = FALSE) +
#   tm_shape(ne_states_sf) + tm_borders(alpha = 0.4) +
#   tm_compass(type = "arrow", position = c("left", "top"), text.size = 0.5) +
#   tm_scale_bar(breaks = c(0, 50, 100), text.size = 0.5, 
#                position = c(0.2,0.005)) +
#   tm_layout(title = "Minorities in New England\nby Census Block Group\n2013-2017", main.title.size = 0.8,
#             legend.position = c(.6, .005),
#             legend.height = 0.7,
#             legend.title.size = 0.7,
#             legend.hist.size = 0.5,
#             legend.text.size = 0.5,
#             legend.width = 0.8) 

# Map of minorities
# version 2
tmap_mode("plot")
tm_shape(ne_blkgrp_sf_DemoEJ, unit = "mi") + 
  tm_fill("minority_pctE", style = "pretty", title = "Percent",
          legend.hist = TRUE, 
          legend.is.portrait = FALSE) +
  tm_shape(ne_states_sf_cb) + tm_borders(alpha = 0.4) + 
  tm_text("STUSPS", size = 0.7, remove.overlap = TRUE, col = "gray") +
  tm_shape(ne_towns_sf_pts) + tm_dots() +
  tm_text("NAME", size = 0.5, col = "black", xmod = 0.7, ymod = 0.2) +
  tm_scale_bar(breaks = c(0, 50, 100), text.size = 0.5, 
               position = c(0.6,0.005)) +
  tm_layout(title = "Minorities in New England\nby Census Block Group\n2013-2017", frame = FALSE, main.title.size = 0.8,
            legend.outside = TRUE,
            legend.title.size = 0.7,
            legend.outside.position = c("right", "top"),
            legend.hist.width = 0.9) 

# Facet map of Minority by state
tm_shape(ne_blkgrp_sf_DemoEJ, unit = "mi") + 
  tm_fill("minority_pctE", style = "pretty", title = "Percent") +
  tm_facets(by = "STATE") +
  tm_shape(ne_states_sf_cb) + tm_borders(alpha = 0.4) + 
  tm_text("STUSPS", size = 0.7, remove.overlap = TRUE, col = "gray") +
  tm_shape(ne_towns_sf_pts) + tm_dots() +
  tm_text("NAME", size = 0.5, col = "black", xmod = 0.7, ymod = 0.2) +
  tm_scale_bar(text.size = 0.5, 
               position = c(0.6,0.005)) +
  tm_layout(title = "Minorities by Census \nBlock Group 2013-2017", frame = FALSE, main.title.size = 0.8,
            legend.title.size = 0.7,
            legend.hist.width = 0.9) 


# Low Income
ne_blkgrp_sf_DemoEJ %>% 
  as.data.frame() %>% 
  summarize(LowIncome = sum(num2povE),
            PctLowIncome = paste0(
              round(sum(num2povE)/sum(totalpopE)*100,1),
              "%"))

tm_shape(ne_blkgrp_sf_DemoEJ, unit = "mi") + 
  tm_fill("pct2povE", style = "pretty", title = "Percent",
          legend.hist = TRUE, 
          legend.is.portrait = FALSE) +
  tm_shape(ne_states_sf) + tm_borders(alpha = 0.4) + 
  tm_text("state.abbrev", size = 0.7, remove.overlap = TRUE, col = "gray") +
  tm_shape(ne_towns_sf_pts) + tm_dots() +
  tm_text("NAME", size = 0.5, col = "black", xmod = 0.7, ymod = 0.2) +
  tm_scale_bar(breaks = c(0, 50, 100), text.size = 0.5, 
               position = c(0.6,0.005)) +
  tm_layout(title = "Low Income People in New \nEngland by Census Block Group\n2013-2017", frame = FALSE, main.title.size = 0.8,
            legend.outside = TRUE,
            legend.title.size = 0.7,
            legend.outside.position = c("right", "top"),
            legend.hist.width = 0.9) 

# Facet map of Low Income by state
tm_shape(ne_blkgrp_sf_DemoEJ, unit = "mi") + 
  tm_fill("pct2povE", style = "pretty", title = "Percent") +
  tm_facets(by = "STATE") +
  tm_shape(ne_states_sf_cb) + tm_borders(alpha = 0.4) + 
  tm_text("STUSPS", size = 0.7, remove.overlap = TRUE, col = "gray") +
  tm_shape(ne_towns_sf_pts) + tm_dots() +
  tm_text("NAME", size = 0.5, col = "black", xmod = 0.7, ymod = 0.2) +
  tm_scale_bar(text.size = 0.5, 
               position = c(0.6,0.005)) +
  tm_layout(title = "Low Income People by \nCensus Block Group\n2013-2017", frame = FALSE, main.title.size = 0.8,
            legend.title.size = 0.7,
            legend.hist.width = 0.9) 


# Less than HS Education
ne_blkgrp_sf_DemoEJ %>% 
  as.data.frame() %>% 
  summarize(NoHSDip = sum(lthsE),
            PctNoHSDip = paste0(
              round(sum(lthsE)/sum(age25upE)*100,1),
              "%"))

tm_shape(ne_blkgrp_sf_DemoEJ, unit = "mi") + 
  tm_fill("pct_lthsE", style = "pretty", title = "Percent",
          legend.hist = TRUE, 
          legend.is.portrait = FALSE) +
  tm_shape(ne_states_sf) + tm_borders(alpha = 0.4) + 
  tm_text("state.abbrev", size = 0.7, remove.overlap = TRUE, col = "gray") +
  tm_shape(ne_towns_sf_pts) + tm_dots() +
  tm_text("NAME", size = 0.5, col = "black", xmod = 0.7, ymod = 0.2) +
  tm_scale_bar(breaks = c(0, 50, 100), text.size = 0.5, 
               position = c(0.6,0.005)) +
  tm_layout(title = "Adults with Less than a High \nSchool Education in New \nEngland by Census Block Group\n2013-2017", frame = FALSE, main.title.size = 0.8,
            legend.outside = TRUE,
            legend.title.size = 0.7,
            legend.outside.position = c("right", "top"),
            legend.hist.width = 0.9) 

# Facet map of Less than HS Education by state
tm_shape(ne_blkgrp_sf_DemoEJ, unit = "mi") + 
  tm_fill("pct_lthsE", style = "pretty", title = "Percent") +
  tm_facets(by = "STATE") +
  tm_shape(ne_states_sf_cb) + tm_borders(alpha = 0.4) + 
  tm_text("STUSPS", size = 0.7, remove.overlap = TRUE, col = "gray") +
  tm_shape(ne_towns_sf_pts) + tm_dots() +
  tm_text("NAME", size = 0.5, col = "black", xmod = 0.7, ymod = 0.2) +
  tm_scale_bar(text.size = 0.5, 
               position = c(0.6,0.005)) +
  tm_layout(title = "Adults with Less than a \nHigh School Education \nby Census Block Group\n2013-2017", frame = FALSE, main.title.size = 0.8,
            legend.title.size = 0.7,
            legend.hist.width = 0.9) 


# Linguistically Isolated
ne_blkgrp_sf_DemoEJ %>% 
  as.data.frame() %>% 
  summarize(NoEnglish = sum(eng_limitE),
            PctNoEnglish = paste0(
              round(sum(eng_limitE)/sum(eng_hhE)*100,1),
              "%"))

tm_shape(ne_blkgrp_sf_DemoEJ, unit = "mi") + 
  tm_fill("eng_limit_pctE", style = "pretty", title = "Percent",
          legend.hist = TRUE, 
          legend.is.portrait = FALSE) +
  tm_shape(ne_states_sf) + tm_borders(alpha = 0.4) + 
  tm_text("state.abbrev", size = 0.7, remove.overlap = TRUE, col = "gray") +
  tm_shape(ne_towns_sf_pts) + tm_dots() +
  tm_text("NAME", size = 0.5, col = "black", xmod = 0.7, ymod = 0.2) +
  tm_scale_bar(breaks = c(0, 50, 100), text.size = 0.5, 
               position = c(0.6,0.005)) +
  tm_layout(title = "Linguistically Isolated Households \nin New England by Census \nBlock Group 2013-2017", frame = FALSE, main.title.size = 0.8,
            legend.outside = TRUE,
            legend.title.size = 0.7,
            legend.outside.position = c("right", "top"),
            legend.hist.width = 0.9) 

# Facet map of Linguistically Isolated by state
tm_shape(ne_blkgrp_sf_DemoEJ, unit = "mi") + 
  tm_fill("eng_limit_pctE", style = "pretty", title = "Percent") +
  tm_facets(by = "STATE") +
  tm_shape(ne_states_sf_cb) + tm_borders(alpha = 0.4) + 
  tm_text("STUSPS", size = 0.7, remove.overlap = TRUE, col = "gray") +
  tm_shape(ne_towns_sf_pts) + tm_dots() +
  tm_text("NAME", size = 0.5, col = "black", xmod = 0.7, ymod = 0.2) +
  tm_scale_bar(text.size = 0.5, 
               position = c(0.6,0.005)) +
  tm_layout(title = "Linguistically Isolated Households \nby Census Block Group \n2013-2017", frame = FALSE, main.title.size = 0.8,
            legend.title.size = 0.7,
            legend.hist.width = 0.9) 


# Over Age 64
ne_blkgrp_sf_DemoEJ %>% 
  as.data.frame() %>% 
  summarize(Over64 = sum(over65E),
            PctOver64 = paste0(
              round(sum(over65E)/sum(totalpopE)*100,1),
              "%"))

tm_shape(ne_blkgrp_sf_DemoEJ, unit = "mi") + 
  tm_fill("pct_over65E", style = "pretty", title = "Percent",
          legend.hist = TRUE, 
          legend.is.portrait = FALSE) +
  tm_shape(ne_states_sf) + tm_borders(alpha = 0.4) + 
  tm_text("state.abbrev", size = 0.7, remove.overlap = TRUE, col = "gray") +
  tm_shape(ne_towns_sf_pts) + tm_dots() +
  tm_text("NAME", size = 0.5, col = "black", xmod = 0.7, ymod = 0.2) +
  tm_scale_bar(breaks = c(0, 50, 100), text.size = 0.5, 
               position = c(0.6,0.005)) +
  tm_layout(title = "People Over Age 64 \nin New England by Census \nBlock Group 2013-2017", frame = FALSE, main.title.size = 0.8,
            legend.outside = TRUE,
            legend.title.size = 0.7,
            legend.outside.position = c("right", "top"),
            legend.hist.width = 0.9) 

# Facet map of Over 64 by state
tm_shape(ne_blkgrp_sf_DemoEJ, unit = "mi") + 
  tm_fill("pct_over65E", style = "pretty", title = "Percent") +
  tm_facets(by = "STATE") +
  tm_shape(ne_states_sf_cb) + tm_borders(alpha = 0.4) + 
  tm_text("STUSPS", size = 0.7, remove.overlap = TRUE, col = "gray") +
  tm_shape(ne_towns_sf_pts) + tm_dots() +
  tm_text("NAME", size = 0.5, col = "black", xmod = 0.7, ymod = 0.2) +
  tm_scale_bar(text.size = 0.5, 
               position = c(0.6,0.005)) +
  tm_layout(title = "People Over Age 64\nby Census Block Group\n2013-2017", frame = FALSE, main.title.size = 0.8,
            legend.title.size = 0.7,
            legend.hist.width = 0.9)


# Under Age 5
ne_blkgrp_sf_DemoEJ %>% 
  as.data.frame() %>% 
  summarize(Under5 = sum(under5E),
            PctUnder5 = paste0(
              round(sum(under5E)/sum(totalpopE)*100,1),
              "%"))

tm_shape(ne_blkgrp_sf_DemoEJ, unit = "mi") + 
  tm_fill("pct_under5E", style = "pretty", title = "Percent",
          legend.hist = TRUE, 
          legend.is.portrait = FALSE) +
  tm_shape(ne_states_sf) + tm_borders(alpha = 0.4) + 
  tm_text("state.abbrev", size = 0.7, remove.overlap = TRUE, col = "gray") +
  tm_shape(ne_towns_sf_pts) + tm_dots() +
  tm_text("NAME", size = 0.5, col = "black", xmod = 0.7, ymod = 0.2) +
  tm_scale_bar(breaks = c(0, 50, 100), text.size = 0.5, 
               position = c(0.6,0.005)) +
  tm_layout(title = "Children Under Age 5 \nin New England by Census \nBlock Group 2013-2017", frame = FALSE, main.title.size = 0.8,
            legend.outside = TRUE,
            legend.title.size = 0.7,
            legend.outside.position = c("right", "top"),
            legend.hist.width = 0.9) 

# Facet map of Under 5 by state
tm_shape(ne_blkgrp_sf_DemoEJ, unit = "mi") + 
  tm_fill("pct_under5E", style = "pretty", title = "Percent") +
  tm_facets(by = "STATE") +
  tm_shape(ne_states_sf_cb) + tm_borders(alpha = 0.4) + 
  tm_text("STUSPS", size = 0.7, remove.overlap = TRUE, col = "gray") +
  tm_shape(ne_towns_sf_pts) + tm_dots() +
  tm_text("NAME", size = 0.5, col = "black", xmod = 0.7, ymod = 0.2) +
  tm_scale_bar(text.size = 0.5, 
               position = c(0.6,0.005)) +
  tm_layout(title = "Children Under Age 5\nby Census Block Group\n2013-2017", frame = FALSE, main.title.size = 0.8,
            legend.title.size = 0.7,
            legend.hist.width = 0.9)


# SUMMARY STATISTICS AND MAPS OF POLLUTION MEASURES BY REGION AND STATE (MIGHT WANT TO SHOW PERCENTILES HERE; OR STYLE = QUANTILE)
# PM2.5 in New England
ne_blkgrp_sf_DemoEJ %>% 
  as.data.frame() %>% 
  dplyr::select(pm_15, PM25_19) %>% 
  summary()

# Histogram of PM2.5 for New England
ne_blkgrp_sf_DemoEJ %>% 
  as.data.frame() %>% 
  ggplot(aes(x = PM25_19)) + geom_histogram() +
  theme_minimal() +
  ggtitle(expression(atop(paste("Histogram of ", PM[2.5], " Concentrations by"), "Census Block Group across New England, 2016"))) +
  xlab(expression(paste(PM[2.5]," (", mu, "g/", m^3, ")", sep = ""))) +
  ylab("Number of Block Groups")

# Map of PM2.5 across New England
tm_shape(ne_blkgrp_sf_DemoEJ, unit = "mi") + 
  tm_fill("PM25_19", style = "quantile", 
          title = expression(paste
                             (PM[2.5]," (", mu, "g/", m^3, ")", sep = "")),
          legend.hist = TRUE,
          colorNA = NULL,
          textNA = NULL,
          legend.format=list(list(digits=2)),
          legend.is.portrait = TRUE) +
  tm_shape(ne_states_sf) + tm_borders(alpha = 0.4) + 
  tm_text("state.abbrev", size = 0.7, remove.overlap = TRUE, col = "gray") +
  tm_shape(ne_towns_sf_pts) + tm_dots() +
  tm_text("NAME", size = 0.5, col = "black", xmod = 0.7, ymod = 0.2) +
  tm_scale_bar(breaks = c(0, 50, 100), text.size = 0.5, 
               position = c(0.6,0.005)) +
  tm_layout(title = "Annual PM2.5 \nConcentrations\n2016", 
            frame = FALSE, main.title.size = 0.8,
            legend.outside = TRUE,
            legend.title.size = 0.8,
            legend.outside.position = c("right", "top"),
            legend.hist.width = 0.9)

# Hotspot map of PM2.5 across New England. Getis-Ord Gi Statistic looks at neighbors within a defined proximity to identify where either high or low values cluster spatially. Statistically significant hot-spots are recognised as areas of high values where other areas within a neighborhood range also share high values too.
library(spdep)
# Get rid of empty geometries and NAs, and convert to spdf
empty_geo <- st_is_empty(ne_blkgrp_sf_DemoEJ)
ne_blkgrp_sp_DemoEJ <- ne_blkgrp_sf_DemoEJ[!empty_geo,] %>% 
  dplyr::select(GEOID,PM25_19) %>% 
  # st_transform(., crs = 2163) %>% # convert to US National Atlas Equal Area
  na.omit() %>% 
  as_Spatial()
# Calculate the Queen's case neighbors
neighborsQC <- poly2nb(ne_blkgrp_sp_DemoEJ, queen = TRUE)
# Calculate neighbors by distance
# creates centroid and joins neighbors within 0 and 150000 units
# neighborsDist <- dnearneigh(coordinates(ne_blkgrp_sp_DemoEJ),d1 = 0, d2 = 15000, longlat = TRUE)
# creates listw
# nb_lw <- nb2listw(nb, style = 'B')
# Compute neighbor weights
spdep::set.ZeroPolicyOption(TRUE)
listw <- nb2listw(neighborsQC, style = "W", zero.policy = TRUE)
# compute Getis-Ord Gi statistic
local_g <- localG(ne_blkgrp_sp_DemoEJ$PM25_19, listw)
local_g <- cbind(ne_blkgrp_sp_DemoEJ, as.matrix(local_g))
names(local_g)[3] <- "gstat"
# map the results
tm_shape(local_g, unit = "mi",) + 
  tm_fill("gstat",
          palette = "-RdBu", 
          style = "pretty",
          title = expression(paste("Getis-Ord ", G[i]^"*")),
          showNA = FALSE, alpha = 0.3) +
  tm_shape(ne_states_sf_cb) + tm_borders(alpha = 0.4) + 
  tm_text("STUSPS", size = 0.7, remove.overlap = TRUE, col = "gray") +
  tm_shape(ne_towns_sf_pts) + tm_dots() +
  tm_text("NAME", size = 0.5, col = "black", xmod = 0.7, ymod = 0.2) +
  tm_scale_bar(breaks = c(0, 50, 100), text.size = 0.5, 
               position = c(0.6,0.005)) +
  tm_layout(title = "Hot Spot Map of \nPM2.5 for\nNew England", frame = FALSE, main.title.size = 0.6,
            legend.position = c(.8,.2),
            legend.title.size = 0.7)



# boxplot of PM2.5 by state for 2016
ne_blkgrp_sf_DemoEJ %>% 
  as.data.frame() %>% 
  ggplot(aes(x = STATE, y = PM25_19, fill = STATE)) + 
  geom_boxplot(notch = TRUE) + 
  ggtitle(expression(paste(PM[2.5], " Annual Concentration by New England state, 2016", sep = ""))) +
  theme_minimal() +
  theme(legend.position = "none") + xlab(NULL) + 
  ylab(expression(paste(PM[2.5]," (", mu, "g/", m^3, ")", sep = "")))
# boxplot of PM2.5 by state for 2011
ne_blkgrp_sf_DemoEJ %>% 
  as.data.frame() %>% 
  ggplot(aes(x = STATE, y = pm_15, fill = STATE)) + 
  geom_boxplot(notch = TRUE) + 
  ggtitle(expression(paste(PM[2.5], " Annual Concentration by New England state, 2011", sep = ""))) +
  theme_minimal() +
  theme(legend.position = "none") + xlab(NULL) + 
  ylab(expression(paste(PM[2.5]," (", mu, "g/", m^3, ")", sep = "")))

# Slope graph of PM2.5 by state and region between 2011 and 2016
# Create df of 2019 actual values
PM19wSTAvgs_actual <- ne_blkgrp_sf_DemoEJ %>% 
  as.data.frame() %>% 
  group_by(STATE) %>% 
  summarize(PM25Mean = mean(PM25_19, na.rm = TRUE),
            PM25wMean = weighted.mean(x = PM25_19,
                                      w = totalpopE, na.rm = TRUE)) %>% 
  mutate(Year = 2016)
# Create df of 2015 values
PM15wSTAvgs_actual <- ne_blkgrp_sf_DemoEJ %>% 
  as.data.frame() %>% 
  group_by(STATE) %>% 
  summarize(PM25Mean = mean(pm_15, na.rm = TRUE),
            PM25wMean = weighted.mean(x = pm_15,
                                      w = pop_15, na.rm = TRUE)) %>% 
  mutate(Year = 2011)
# Create regional average benchmark
pm15NEavg <- ne_blkgrp_sf_DemoEJ %>% 
  as.data.frame() %>% 
  summarize(PM25Mean = mean(pm_15, na.rm = TRUE),
            PM25wMean = weighted.mean(x = pm_15,
                                      w = pop_15, na.rm = TRUE)) %>% 
  transmute(STATE = "NEW ENGLAND", 
            PM25Mean = PM25Mean, 
            PM25wMean = PM25wMean,
            Year = 2011)
pm19NEavg <- ne_blkgrp_sf_DemoEJ %>% 
  as.data.frame() %>% 
  summarize(PM25Mean = mean(PM25_19, na.rm = TRUE),
            PM25wMean = weighted.mean(x = PM25_19,
                                      w = totalpopE, na.rm = TRUE)) %>% 
  transmute(STATE = "NEW ENGLAND", 
            PM25Mean = PM25Mean, 
            PM25wMean = PM25wMean,
            Year = 2016)
#rbind tables
PM11_16wSTAvg_actual <- bind_rows(PM15wSTAvgs_actual,
                                PM19wSTAvgs_actual,
                                pm15NEavg,pm19NEavg) %>% 
  mutate(Year = as.factor(Year),
         PM25Mean = round(PM25wMean,2),
         PM25wMean = round(PM25wMean,2))
# make slope graph
newggslopegraph(dataframe = PM11_16wSTAvg_actual, 
                Times = Year, 
                Measurement = PM25Mean, 
                Grouping = STATE,
                Title = "Annual Average PM2.5 Concentrations",
                SubTitle = expression(paste
                                      (PM[2.5]," (", mu, "g/", m^3, ")", sep = "")),
                Caption = NULL,
                LineColor = c("NEW ENGLAND" = "#000000",
                              "Connecticut" = "#E69F00",
                              "Massachusetts" = "#56B4E9",
                              "Rhode Island" = "#009E73",
                              "New Hampshire" = "#F0E442",
                              "Vermont" = "#0072B2",
                              "Maine" = "#D55E00"),
                LineThickness = 0.7)


# SCATTER PLOTS AND CORRELATION MATRICES OF DEMO VS POLLUTION FOR NEW ENGLAND AND STATES (CORRECT FOR NON-NORMAL DISTRIBUTIONS; OR USE SPEARMAN'S RANK)
# Create a correlation matrix of PM25 and populations of concern
corrPM25 <- ne_blkgrp_sf_DemoEJ %>% 
  as.data.frame() %>% 
  dplyr::transmute(PM25 = PM25_19, 
                Minority = minority_pctE, 
                `Low Income` = pct2povE, 
                `Lang Isol` = eng_limit_pctE, 
                `No HS Dip` = pct_lthsE, 
                `Under 5` = pct_under5E, 
                `Over 64` = pct_over65E) %>% 
  drop_na() %>% 
  cor(method = "spearman")
corrPM25
library(ggcorrplot)
ggcorrplot(corrPM25, hc.order = TRUE, type = "lower", lab = TRUE, 
           title = "PM2.5 Correlation Matrix for New England, 2016", 
           legend.title = "Spearman's \nCorrelation\nCoefficient")

# Look at relationship of minority to PM25
# New England
ne_blkgrp_sf_DemoEJ %>% 
  as.data.frame() %>% 
  drop_na(minority_pctE, PM25_19) %>%  
  ggplot(aes(x = minority_pctE, y = PM25_19)) + geom_point() +
  # geom_smooth(method = "glm", formula = y~x, method.args = list(family = gaussian(link = 'log'))) +
  facet_wrap("STATE")
# By state
ne_blkgrp_sf_DemoEJ %>% 
  as.data.frame() %>% 
  drop_na(minority_pctE, PM25_19) %>%  
  ggplot(aes(x = minority_pctE, y = PM25_19)) + geom_point() +
  facet_wrap("STATE")

# Look at relationship of low income to PM25
# New England
ne_blkgrp_sf_DemoEJ %>% 
  as.data.frame() %>% 
  drop_na(pct2povE, PM25_19) %>%  
  ggplot(aes(x = pct2povE, y = PM25_19)) + geom_point()
# By state
ne_blkgrp_sf_DemoEJ %>% 
  as.data.frame() %>% 
  drop_na(pct2povE, PM25_19) %>%  
  ggplot(aes(x = pct2povE, y = PM25_19)) + geom_point() +
  facet_wrap("STATE")

# Look at relationship of language isolation to PM25
# New England
ne_blkgrp_sf_DemoEJ %>% 
  as.data.frame() %>% 
  drop_na(eng_limit_pctE, PM25_19) %>%  
  ggplot(aes(x = eng_limit_pctE, y = PM25_19)) + geom_point(alpha = 0.3)
# By state
ne_blkgrp_sf_DemoEJ %>% 
  as.data.frame() %>% 
  drop_na(eng_limit_pctE, PM25_19) %>%  
  ggplot(aes(x = eng_limit_pctE, y = PM25_19)) + geom_point(alpha = 0.3) +
  facet_wrap("STATE")

# Look at relationship of less than HS education to PM25
# New England
ne_blkgrp_sf_DemoEJ %>% 
  as.data.frame() %>% 
  drop_na(pct_lthsE, PM25_19) %>%  
  ggplot(aes(x = pct_lthsE, y = PM25_19)) + geom_point(alpha = 0.3) +
  theme_minimal()
# By state
ne_blkgrp_sf_DemoEJ %>% 
  as.data.frame() %>% 
  drop_na(pct_lthsE, PM25_19) %>%  
  ggplot(aes(x = pct_lthsE, y = PM25_19)) + geom_point(alpha = 0.3) +
  facet_wrap("STATE") +
  theme_minimal()

# Look at relationship of under 5 to PM25
# New England
ne_blkgrp_sf_DemoEJ %>% 
  as.data.frame() %>% 
  drop_na(pct_under5E, PM25_19) %>%  
  ggplot(aes(x = pct_under5E, y = PM25_19)) + geom_point(alpha = 0.3) +
  theme_minimal()
# By state
ne_blkgrp_sf_DemoEJ %>% 
  as.data.frame() %>% 
  drop_na(pct_under5E, PM25_19) %>%  
  ggplot(aes(x = pct_under5E, y = PM25_19)) + geom_point(alpha = 0.3) +
  facet_wrap("STATE") +
  theme_minimal()

# Look at relationship of over 64 to PM25
# New England
ne_blkgrp_sf_DemoEJ %>% 
  as.data.frame() %>% 
  drop_na(pct_over65E, PM25_19) %>%  
  ggplot(aes(x = pct_over65E, y = PM25_19)) + geom_point(alpha = 0.3) +
  theme_minimal()
# By state
ne_blkgrp_sf_DemoEJ %>% 
  as.data.frame() %>% 
  drop_na(pct_over65E, PM25_19) %>%  
  ggplot(aes(x = pct_over65E, y = PM25_19)) + geom_point(alpha = 0.3) +
  facet_wrap("STATE") +
  theme_minimal()



# POP WEIGHTED AVGS FOR POLLUTION FOR NEW ENGLAND AND STATES, INCLUDING EJ INDICES
# Pop Weighted avg of PM2.5 for all Groups in New England relative to NE average
ne_blkgrp_sf_DemoEJ %>% 
  as.data.frame() %>% 
  # filter(STATE == "Massachusetts") %>%
  dplyr::select(totalpopE,
                nhwhitepopE,
                minorityE,
                nhblackpopE,
                nhamerindpopE,
                nhasianpopE,
                nhnativhpopE,
                nhotherpopE,
                nh2morepopE,
                hisppopE,
                povknownE,
                num2povE, 
                eng_hhE,
                eng_limitE,
                age25upE,
                lthsE, 
                allAgesE, 
                under5E, 
                over65E, 
                PM25_19) %>% 
  gather(key = Group, value = Pop, totalpopE:over65E) %>% 
  group_by(Group) %>% 
  summarize(PM25wMean = weighted.mean(x = PM25_19, w = Pop, na.rm = TRUE),
            PM25Mean = mean(PM25_19, na.rm = TRUE)) %>% 
  spread(key = Group, value = PM25wMean) %>% 
  transmute(Minority = (minorityE/PM25Mean - 1)*100,
         #Minority_NHW = (minorityE/nhwhitepopE - 1)*100,
         `Lang Isol` = (eng_limitE/PM25Mean - 1)*100,
         Poverty = (num2povE/PM25Mean - 1)*100,
         `No HS` = (lthsE/PM25Mean - 1)*100,
         `Under 5` = (under5E/PM25Mean - 1)*100,
         `Over 65` = (over65E/PM25Mean - 1)*100) %>%
  gather(key = Group, value = Pct) %>% 
  ggplot(aes(x = reorder(Group, -Pct), y = Pct, fill = Group)) + 
  geom_bar(stat = "identity", position = "identity") +
  theme_minimal() +
  labs(x = "", y = "", title = expression(paste("Population-Weighted ", PM[2.5], " Exposure (relative to New England average)"))) + 
  theme(legend.position = 'none') +
  geom_text(aes(x = Group, y = Pct + 0.2 * sign(Pct), 
                label = paste0(round(Pct,2),"%")), 
            hjust = 0.5, size = 3,
            color=rgb(100,100,100, maxColorValue=255)) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  geom_hline(yintercept = 0)


# Pop Weighted avg of PM2.5 EXPOSURE CHANGE 2011 - 2016 for all Groups in New England relative to NE average. NOTE THAT ALL BLOCK GROUPS SHOWED NEGATIVE CHANGE (i.e. DECLINE), MEANING PM10 WENT DOWN. SINCE NUMERATOR AND DENOMINATORS ARE NEGATIVE, PERCENT CHANGE MEANS DECREASE RELATIVE TO AVERAGE. NEGATIVE VALUES INDICATE SLOWER DECLINE THAN AVERAGE; POSITIVE NUMBERS MEAN FASTER DECLINE THAN AVERAGE. 
ne_blkgrp_sf_DemoEJ %>% 
  as.data.frame() %>% 
  # filter(STATE == "Massachusetts") %>%
  dplyr::select(totalpopE,
                nhwhitepopE,
                minorityE,
                nhblackpopE,
                nhamerindpopE,
                nhasianpopE,
                nhnativhpopE,
                nhotherpopE,
                nh2morepopE,
                hisppopE,
                povknownE,
                num2povE, 
                eng_hhE,
                eng_limitE,
                age25upE,
                lthsE, 
                allAgesE, 
                under5E, 
                over65E, 
                PM25_pctChange) %>% 
  gather(key = Group, value = Pop, totalpopE:over65E) %>% 
  group_by(Group) %>% 
  summarize(PM25wMean = weighted.mean(x = PM25_pctChange, 
                                      w = Pop, na.rm = TRUE),
            PM25Mean = mean(PM25_pctChange, na.rm = TRUE)) %>% 
  spread(key = Group, value = PM25wMean) %>% 
  transmute(Minority = (minorityE/PM25Mean - 1)*100,
            #Minority_NHW = (minorityE/nhwhitepopE - 1)*100,
            `Lang Isol` = (eng_limitE/PM25Mean - 1)*100,
            Poverty = (num2povE/PM25Mean - 1)*100,
            `No HS` = (lthsE/PM25Mean - 1)*100,
            `Under 5` = (under5E/PM25Mean - 1)*100,
            `Over 65` = (over65E/PM25Mean - 1)*100) %>%
  gather(key = Group, value = Pct) %>% 
  ggplot(aes(x = reorder(Group, -Pct), y = Pct, fill = Group)) + 
  geom_bar(stat = "identity", position = "identity") +
  theme_minimal() +
  labs(x = "", y = "", title = expression(atop(paste("Population-Weighted ", PM[2.5], " Exposure CHANGE 2011 - 2016"), " (relative to New England average)"))) + 
  theme(legend.position = 'none') +
  geom_text(aes(x = Group, y = Pct + 0.2 * sign(Pct), 
                label = paste0(round(Pct,2),"%")), 
            hjust = 0.5, size = 3,
            color=rgb(100,100,100, maxColorValue=255)) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  geom_hline(yintercept = 0)


# Slope graph of change in pop-weighted PM2.5 exposure beteween 2011 and 2016
# Create df of 2019 values
PM19wAvgs <- ne_blkgrp_sf_DemoEJ %>% 
  as.data.frame() %>% 
  # filter(STATE == "Massachusetts") %>%
  dplyr::select(totalpopE,
                nhwhitepopE,
                minorityE,
                nhblackpopE,
                nhamerindpopE,
                nhasianpopE,
                nhnativhpopE,
                nhotherpopE,
                nh2morepopE,
                hisppopE,
                povknownE,
                num2povE,
                eng_hhE,
                eng_limitE,
                age25upE,
                lthsE,
                allAgesE, 
                under5E,
                over65E,
                PM25_19) %>% 
  gather(key = Group, value = Pop, totalpopE:over65E) %>% 
  group_by(Group) %>% 
  summarize(PM25wMean = weighted.mean(x = PM25_19, 
                                      w = Pop, na.rm = TRUE),
            PM25Mean = mean(PM25_19, na.rm = TRUE)) %>% 
  spread(key = Group, value = PM25wMean) %>% 
  transmute(Minority = (minorityE/PM25Mean - 1)*100,
            #Minority_NHW = (minorityE/nhwhitepopE - 1)*100,
            `Lang Isol` = (eng_limitE/PM25Mean - 1)*100,
            Poverty = (num2povE/PM25Mean - 1)*100,
            `No HS` = (lthsE/PM25Mean - 1)*100,
            `Under 5` = (under5E/PM25Mean - 1)*100,
            `Over 65` = (over65E/PM25Mean - 1)*100) %>%
  gather(key = Group, value = Pct) %>% 
  mutate(Year = 2016)
# Create df of 2015 values
PM15wAvgs <- ne_blkgrp_sf_DemoEJ %>% 
  as.data.frame() %>% 
  # filter(STATE == "Massachusetts") %>%
  dplyr::select(pop_15,
                mins_15,
                lowinc_15,
                lingiso_15,
                lths_15,
                under5_15,
                over64_15,
                pm_15) %>% 
  gather(key = Group, value = Pop, pop_15:over64_15) %>% 
  group_by(Group) %>% 
  summarize(PM25wMean = weighted.mean(x = pm_15, 
                                      w = Pop, na.rm = TRUE),
            PM25Mean = mean(pm_15, na.rm = TRUE)) %>% 
  spread(key = Group, value = PM25wMean) %>% 
  transmute(Minority = (mins_15/PM25Mean - 1)*100,
            #Minority_NHW = (minorityE/nhwhitepopE - 1)*100,
            `Lang Isol` = (lingiso_15/PM25Mean - 1)*100,
            Poverty = (lowinc_15/PM25Mean - 1)*100,
            `No HS` = (lths_15/PM25Mean - 1)*100,
            `Under 5` = (under5_15/PM25Mean - 1)*100,
            `Over 65` = (over64_15/PM25Mean - 1)*100) %>%
  gather(key = Group, value = Pct) %>% 
  mutate(Year = 2011)
# Create regional average benchmark
pmRegionalAvg <- data.frame(
  Group = c("REGIONAL AVG", "REGIONAL AVG"),
  Pct = c(0,0),
  Year = c(2011,2016)
)
#rbind tables
PM11_16wAvg <- bind_rows(PM15wAvgs,PM19wAvgs,pmRegionalAvg) %>% 
  mutate(Year = as.factor(Year),
         Pct = round(Pct,2))
# make slope graph
newggslopegraph(dataframe = PM11_16wAvg, 
                Times = Year, 
                Measurement = Pct, 
                Grouping = Group,
                Title = "Population-Weighted PM2.5 Exposure",
                SubTitle = "Relative to New England Average",
                Caption = "Values represent percentage of regional average PM2.5 levels. Positive values \nindicate exposure for those groups are higher than the regional average; negative \nvalues indicate exposure for those groups are less than the regional average.",
                LineColor = c("REGIONAL AVG" = "#000000",
                              "Lang Isol" = "#E69F00",
                              "Minority" = "#56B4E9",
                              "No HS" = "#009E73",
                              "Poverty" = "#F0E442",
                              "Under 5" = "#0072B2",
                              "Over 65" = "#D55E00"))


# Create df of 2019 actual values
PM19wAvgs_actual <- ne_blkgrp_sf_DemoEJ %>% 
  as.data.frame() %>% 
  # filter(STATE == "Massachusetts") %>%
  dplyr::select(minorityE,
                num2povE,
                eng_limitE,
                lthsE,
                under5E,
                over65E,
                PM25_19) %>% 
  gather(key = Group, value = Pop, minorityE:over65E) %>% 
  group_by(Group) %>% 
  summarize(PM25wMean = weighted.mean(x = PM25_19, 
                                      w = Pop, na.rm = TRUE)) %>% 
  mutate(Group = case_when(
    Group == "minorityE" ~ "Minority",
    Group == "eng_limitE" ~ "Lang Isol",
    Group == "num2povE" ~ "Poverty",
    Group == "lthsE" ~ "No HS",
    Group == "under5E" ~ "Under 5",
    Group == "over65E" ~ "Over 65")) %>% 
  mutate(Year = 2016)
# Create df of 2015 values
PM15wAvgs_actual <- ne_blkgrp_sf_DemoEJ %>% 
  as.data.frame() %>% 
  # filter(STATE == "Massachusetts") %>%
  dplyr::select(mins_15,
                lowinc_15,
                lingiso_15,
                lths_15,
                under5_15,
                over64_15,
                pm_15) %>% 
  gather(key = Group, value = Pop, mins_15:over64_15) %>% 
  group_by(Group) %>% 
  summarize(PM25wMean = weighted.mean(x = pm_15, 
                                      w = Pop, na.rm = TRUE)) %>%  
  mutate(Group = case_when(
    Group == "mins_15" ~ "Minority",
    Group == "lingiso_15" ~ "Lang Isol",
    Group == "lowinc_15" ~ "Poverty",
    Group == "lths_15" ~ "No HS",
    Group == "under5_15" ~ "Under 5",
    Group == "over64_15" ~ "Over 65")) %>% 
  mutate(Year = 2011)
# Create regional average benchmark
pm15NEavg <- mean(ne_blkgrp_sf_DemoEJ$pm_15, na.rm = TRUE)
pm19NEavg <- mean(ne_blkgrp_sf_DemoEJ$PM25_19, na.rm = TRUE)
pmRegionalAvg_actual <- data.frame(
  Group = c("REGIONAL AVG", "REGIONAL AVG"),
  PM25wMean = c(pm15NEavg,pm19NEavg),
  Year = c(2011,2016)
)
#rbind tables
PM11_16wAvg_actual <- bind_rows(PM15wAvgs_actual,
                                PM19wAvgs_actual,
                                pmRegionalAvg_actual) %>% 
  mutate(Year = as.factor(Year),
         PM25wMean = round(PM25wMean,1))
# make slope graph
newggslopegraph(dataframe = PM11_16wAvg_actual, 
                Times = Year, 
                Measurement = PM25wMean, 
                Grouping = Group,
                Title = "Population-Weighted PM2.5 Exposure",
                SubTitle = "in micrograms per cubic meter",
                Caption = NULL,
                LineColor = c("REGIONAL AVG" = "#000000",
                              "Lang Isol" = "#E69F00",
                              "Minority" = "#56B4E9",
                              "No HS" = "#009E73",
                              "Poverty" = "#F0E442",
                              "Under 5" = "#0072B2",
                              "Over 65" = "#D55E00"))


# Pop Weighted avg of PM2.5 for ALL GROUPS in New England relative to CONTRAST GROUPS
ne_blkgrp_sf_DemoEJ %>% 
  as.data.frame() %>% 
  # filter(STATE == "Massachusetts") %>%
  dplyr::select(totalpopE,
                nhwhitepopE,
                minorityE,
                nhblackpopE,
                nhamerindpopE,
                nhasianpopE,
                nhnativhpopE,
                nhotherpopE,
                nh2morepopE,
                hisppopE,
                povknownE,
                num2povE, 
                eng_hhE,
                eng_limitE,
                age25upE,
                lthsE, 
                collegeE,
                allAgesE, 
                under5E, 
                over65E, 
                PM25_19) %>% 
  mutate(NonLangIsol = eng_hhE - eng_limitE,  
         ovr5undr65 = allAgesE - (under5E + over65E),
         noPoverty = povknownE - num2povE) %>% 
  dplyr::select(PM25_19, everything()) %>% 
  gather(key = Group, value = Pop, totalpopE:noPoverty) %>% 
  group_by(Group) %>% 
  summarize(PM25wMean = weighted.mean(x = PM25_19, w = Pop, na.rm = TRUE),
            PM25Mean = mean(PM25_19, na.rm = TRUE)) %>% 
  spread(key = Group, value = PM25wMean) %>% 
  transmute(Minority = (minorityE/nhwhitepopE - 1)*100,
            `Lang Isol` = (eng_limitE/NonLangIsol - 1)*100,
            Poverty = (num2povE/noPoverty - 1)*100,
            `No HS` = (lthsE/collegeE - 1)*100,
            `Under 5` = (under5E/ovr5undr65 - 1)*100,
            `Over 65` = (over65E/ovr5undr65 - 1)*100) %>%
  gather(key = Group, value = Pct) %>% 
  ggplot(aes(x = reorder(Group, -Pct), y = Pct, fill = Group)) + 
  geom_bar(stat = "identity", position = "identity") +
  theme_minimal() +
  labs(x = "", y = "", title = expression(paste("Population-Weighted ", PM[2.5], " Exposure (relative to Contrast Groups)"))) + 
  theme(legend.position = 'none') +
  geom_text(aes(x = Group, y = Pct + 0.2 * sign(Pct), 
                label = paste0(round(Pct,2),"%")), 
            hjust = 0.5, size = 3,
            color=rgb(100,100,100, maxColorValue=255)) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  geom_hline(yintercept = 0)


# Pop Weighted avg of PM2.5 by RACE in New England relative to NE average
ne_blkgrp_sf_DemoEJ %>% 
  as.data.frame() %>% 
  # filter(STATE == "Massachusetts") %>%
  dplyr::select(totalpopE,
                nhwhitepopE,
                minorityE,
                nhblackpopE,
                nhamerindpopE,
                nhasianpopE,
                nhnativhpopE,
                nhotherpopE,
                nh2morepopE,
                hisppopE,
                povknownE,
                num2povE, 
                eng_hhE,
                eng_limitE,
                age25upE,
                lthsE, 
                allAgesE, 
                under5E, 
                over65E, 
                PM25_19) %>% 
  gather(key = Group, value = Pop, totalpopE:over65E) %>% 
  group_by(Group) %>% 
  summarize(PM25wMean = weighted.mean(x = PM25_19, w = Pop, na.rm = TRUE),
            PM25Mean = mean(PM25_19, na.rm = TRUE)) %>% 
  spread(key = Group, value = PM25wMean) %>% 
  transmute(White = (nhwhitepopE/PM25Mean - 1)*100,
            Black = (nhblackpopE/PM25Mean - 1)*100,
            Asian = (nhasianpopE/nhwhitepopE - 1)*100,
            AmerInd = (nhamerindpopE/PM25Mean - 1)*100,
            `Native PI/HI` = (nhnativhpopE/PM25Mean - 1)*100,
            Other = (nhotherpopE/PM25Mean - 1)*100,
            Multiracial = (nhnativhpopE/PM25Mean - 1)*100,
            Hispanic = (hisppopE/PM25Mean - 1)*100) %>%
  gather(key = Group, value = Pct) %>% 
  ggplot(aes(x = reorder(Group, -Pct), y = Pct, fill = Group)) + 
  geom_bar(stat = "identity", position = "identity") +
  theme_minimal() +
  labs(x = "", y = "", title = expression(atop(paste("Population-Weighted ", PM[2.5], " Exposure by Race for New England"), "(relative to New England average)"))) + 
  theme(legend.position = 'none') +
  geom_text(aes(x = Group, y = Pct + 0.3 * sign(Pct), 
                label = paste0(round(Pct,2),"%")), 
            hjust = 0.5, size = 3,
            color=rgb(100,100,100, maxColorValue=255)) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  geom_hline(yintercept = 0)


# Pop Weighted avg of PM2.5 by RACE in New England relative to White average
ne_blkgrp_sf_DemoEJ %>% 
  as.data.frame() %>% 
  # filter(STATE == "Massachusetts") %>%
  dplyr::select(totalpopE,
                nhwhitepopE,
                minorityE,
                nhblackpopE,
                nhamerindpopE,
                nhasianpopE,
                nhnativhpopE,
                nhotherpopE,
                nh2morepopE,
                hisppopE,
                povknownE,
                num2povE, 
                eng_hhE,
                eng_limitE,
                age25upE,
                lthsE, 
                allAgesE, 
                under5E, 
                over65E, 
                PM25_19) %>% 
  gather(key = Group, value = Pop, totalpopE:over65E) %>% 
  group_by(Group) %>% 
  summarize(PM25wMean = weighted.mean(x = PM25_19, w = Pop, na.rm = TRUE),
            PM25Mean = mean(PM25_19, na.rm = TRUE)) %>% 
  spread(key = Group, value = PM25wMean) %>% 
  transmute(Black = (nhblackpopE/nhwhitepopE - 1)*100,
            Asian = (nhasianpopE/nhwhitepopE - 1)*100,
            AmerInd = (nhamerindpopE/nhwhitepopE - 1)*100,
            `Native PI/HI` = (nhnativhpopE/nhwhitepopE - 1)*100,
            Other = (nhotherpopE/nhwhitepopE - 1)*100,
            Multiracial = (nhnativhpopE/nhwhitepopE - 1)*100,
            Hispanic = (hisppopE/nhwhitepopE - 1)*100) %>%
  gather(key = Group, value = Pct) %>% 
  ggplot(aes(x = reorder(Group, -Pct), y = Pct, fill = Group)) + 
  geom_bar(stat = "identity", position = "identity") +
  theme_minimal() +
  labs(x = "", y = "", title = expression(atop(paste("Population-Weighted ", PM[2.5], " Exposure by Race for New England"), "(relative to non-Hispanic White average)"))) + 
  theme(legend.position = 'none') +
  geom_text(aes(x = Group, y = Pct + 0.3 * sign(Pct), 
                label = paste0(round(Pct,2),"%")), 
            hjust = 0.5, size = 3,
            color=rgb(100,100,100, maxColorValue=255)) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  geom_hline(yintercept = 0)


# Pop Weighted avg of PM2.5 EXPOSURE CHANGE 2011 - 2016 by RACE in New England relative to White average
ne_blkgrp_sf_DemoEJ %>% 
  as.data.frame() %>% 
  # filter(STATE == "Massachusetts") %>%
  dplyr::select(totalpopE,
                nhwhitepopE,
                minorityE,
                nhblackpopE,
                nhamerindpopE,
                nhasianpopE,
                nhnativhpopE,
                nhotherpopE,
                nh2morepopE,
                hisppopE,
                povknownE,
                num2povE, 
                eng_hhE,
                eng_limitE,
                age25upE,
                lthsE, 
                allAgesE, 
                under5E, 
                over65E, 
                PM25_pctChange) %>% 
  gather(key = Group, value = Pop, totalpopE:over65E) %>% 
  group_by(Group) %>% 
  summarize(PM25wMean = weighted.mean(x = PM25_pctChange, 
                                      w = Pop, na.rm = TRUE),
            PM25Mean = mean(PM25_pctChange, na.rm = TRUE)) %>% 
  spread(key = Group, value = PM25wMean) %>% 
  transmute(Black = (nhblackpopE/nhwhitepopE - 1)*100,
            Asian = (nhasianpopE/nhwhitepopE - 1)*100,
            AmerInd = (nhamerindpopE/nhwhitepopE - 1)*100,
            `Native PI/HI` = (nhnativhpopE/nhwhitepopE - 1)*100,
            Other = (nhotherpopE/nhwhitepopE - 1)*100,
            Multiracial = (nhnativhpopE/nhwhitepopE - 1)*100,
            Hispanic = (hisppopE/nhwhitepopE - 1)*100) %>%
  gather(key = Group, value = Pct) %>% 
  ggplot(aes(x = reorder(Group, -Pct), y = Pct, fill = Group)) + 
  geom_bar(stat = "identity", position = "identity") +
  theme_minimal() +
  labs(x = "", y = "", title = expression(atop(paste("Population-Weighted ", PM[2.5], " Exposure CHANGE 2011 - 2016 by Race"), " for New England (relative to non-Hispanic White average)"))) + 
  theme(legend.position = 'none') +
  geom_text(aes(x = Group, y = Pct + 0.3 * sign(Pct), 
                label = paste0(round(Pct,2),"%")), 
            hjust = 0.5, size = 3,
            color=rgb(100,100,100, maxColorValue=255)) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  geom_hline(yintercept = 0)


# Pop Weighted avg of PM2.5 for all Groups in Connecticut relative to CT average
ne_blkgrp_sf_DemoEJ %>% 
  as.data.frame() %>% 
  filter(STATE == "Connecticut") %>%
  dplyr::select(totalpopE,
                nhwhitepopE,
                minorityE,
                nhblackpopE,
                nhamerindpopE,
                nhasianpopE,
                nhnativhpopE,
                nhotherpopE,
                nh2morepopE,
                hisppopE,
                povknownE,
                num2povE, 
                eng_hhE,
                eng_limitE,
                age25upE,
                lthsE, 
                allAgesE, 
                under5E, 
                over65E, 
                PM25_19) %>% 
  gather(key = Group, value = Pop, totalpopE:over65E) %>% 
  group_by(Group) %>% 
  summarize(PM25wMean = weighted.mean(x = PM25_19, w = Pop, na.rm = TRUE),
            PM25Mean = mean(PM25_19, na.rm = TRUE)) %>% 
  spread(key = Group, value = PM25wMean) %>% 
  transmute(Minority = (minorityE/PM25Mean - 1)*100,
            #Minority_NHW = (minorityE/nhwhitepopE - 1)*100,
            `Lang Isol` = (eng_limitE/PM25Mean - 1)*100,
            Poverty = (num2povE/PM25Mean - 1)*100,
            `No HS` = (lthsE/PM25Mean - 1)*100,
            `Under 5` = (under5E/PM25Mean - 1)*100,
            `Over 65` = (over65E/PM25Mean - 1)*100) %>%
  gather(key = Group, value = Pct) %>% 
  ggplot(aes(x = reorder(Group, -Pct), y = Pct, fill = Group)) + 
  geom_bar(stat = "identity", position = "identity") +
  theme_minimal() +
  labs(x = "", y = "", title = expression(paste("Population-Weighted ", PM[2.5], " Exposure (relative to CT average)"))) + 
  theme(legend.position = 'none') +
  geom_text(aes(x = Group, y = Pct + 0.2 * sign(Pct), 
                label = paste0(round(Pct,2),"%")), 
            hjust = 0.5, size = 3,
            color=rgb(100,100,100, maxColorValue=255)) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  geom_hline(yintercept = 0)


# Pop Weighted avg of PM2.5 CHANGE 2011 - 2016 for all Groups in Connecticut relative to CT average
ne_blkgrp_sf_DemoEJ %>% 
  as.data.frame() %>% 
  filter(STATE == "Connecticut") %>%
  dplyr::select(totalpopE,
                nhwhitepopE,
                minorityE,
                nhblackpopE,
                nhamerindpopE,
                nhasianpopE,
                nhnativhpopE,
                nhotherpopE,
                nh2morepopE,
                hisppopE,
                povknownE,
                num2povE, 
                eng_hhE,
                eng_limitE,
                age25upE,
                lthsE, 
                allAgesE, 
                under5E, 
                over65E, 
                PM25_pctChange) %>% 
  gather(key = Group, value = Pop, totalpopE:over65E) %>% 
  group_by(Group) %>% 
  summarize(PM25wMean = weighted.mean(x = PM25_pctChange, 
                                      w = Pop, na.rm = TRUE),
            PM25Mean = mean(PM25_pctChange, na.rm = TRUE)) %>% 
  spread(key = Group, value = PM25wMean) %>% 
  transmute(Minority = (minorityE/PM25Mean - 1)*100,
            #Minority_NHW = (minorityE/nhwhitepopE - 1)*100,
            `Lang Isol` = (eng_limitE/PM25Mean - 1)*100,
            Poverty = (num2povE/PM25Mean - 1)*100,
            `No HS` = (lthsE/PM25Mean - 1)*100,
            `Under 5` = (under5E/PM25Mean - 1)*100,
            `Over 65` = (over65E/PM25Mean - 1)*100) %>%
  gather(key = Group, value = Pct) %>% 
  ggplot(aes(x = reorder(Group, -Pct), y = Pct, fill = Group)) + 
  geom_bar(stat = "identity", position = "identity") +
  theme_minimal() +
  labs(x = "", y = "", title = expression(atop(paste("Population-Weighted ", PM[2.5], " Exposure CHANGE 2011 - 2016"), " (relative to CT average)"))) + 
  theme(legend.position = 'none') +
  geom_text(aes(x = Group, y = Pct + 0.2 * sign(Pct), 
                label = paste0(round(Pct,2),"%")), 
            hjust = 0.5, size = 3,
            color=rgb(100,100,100, maxColorValue=255)) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  geom_hline(yintercept = 0)


# Slope graph of change in pop-weighted PM2.5 exposure beteween 2011 and 2016
# Create df of 2019 actual values
PM19wAvgs_actualCT <- ne_blkgrp_sf_DemoEJ %>% 
  as.data.frame() %>% 
  filter(STATE == "Connecticut") %>%
  dplyr::select(minorityE,
                num2povE,
                eng_limitE,
                lthsE,
                under5E,
                over65E,
                PM25_19) %>% 
  gather(key = Group, value = Pop, minorityE:over65E) %>% 
  group_by(Group) %>% 
  summarize(PM25wMean = weighted.mean(x = PM25_19, 
                                      w = Pop, na.rm = TRUE)) %>% 
  mutate(Group = case_when(
    Group == "minorityE" ~ "Minority",
    Group == "eng_limitE" ~ "Lang Isol",
    Group == "num2povE" ~ "Poverty",
    Group == "lthsE" ~ "No HS",
    Group == "under5E" ~ "Under 5",
    Group == "over65E" ~ "Over 65")) %>% 
  mutate(Year = 2016)
# Create df of 2015 values
PM15wAvgs_actualCT <- ne_blkgrp_sf_DemoEJ %>% 
  as.data.frame() %>% 
  filter(STATE == "Connecticut") %>%
  dplyr::select(mins_15,
                lowinc_15,
                lingiso_15,
                lths_15,
                under5_15,
                over64_15,
                pm_15) %>% 
  gather(key = Group, value = Pop, mins_15:over64_15) %>% 
  group_by(Group) %>% 
  summarize(PM25wMean = weighted.mean(x = pm_15, 
                                      w = Pop, na.rm = TRUE)) %>%  
  mutate(Group = case_when(
    Group == "mins_15" ~ "Minority",
    Group == "lingiso_15" ~ "Lang Isol",
    Group == "lowinc_15" ~ "Poverty",
    Group == "lths_15" ~ "No HS",
    Group == "under5_15" ~ "Under 5",
    Group == "over64_15" ~ "Over 65")) %>% 
  mutate(Year = 2011)
# Create regional average benchmark
pm15NEavgCT <- ne_blkgrp_sf_DemoEJ %>% 
  as.data.frame() %>% 
  filter(STATE == "Connecticut") %>% 
  summarize(mean(pm_15, na.rm = TRUE))
pm19NEavgCT <- ne_blkgrp_sf_DemoEJ %>% 
  as.data.frame() %>% 
  filter(STATE == "Connecticut") %>% 
  summarize(mean(PM25_19, na.rm = TRUE))
pmRegionalAvg_actualCT <- data.frame(
  Group = c("REGIONAL AVG", "REGIONAL AVG"),
  PM25wMean = c(pm15NEavgCT[1,1],pm19NEavgCT[1,1]),
  Year = c(2011,2016)
)
#rbind tables
PM11_16wAvg_actualCT <- bind_rows(PM15wAvgs_actualCT,
                                PM19wAvgs_actualCT,
                                pmRegionalAvg_actual) %>% 
  mutate(Year = as.factor(Year),
         PM25wMean = round(PM25wMean,1))
# make slope graph
newggslopegraph(dataframe = PM11_16wAvg_actualCT, 
                Times = Year, 
                Measurement = PM25wMean, 
                Grouping = Group,
                Title = "Population-Weighted PM2.5 Exposure for CT",
                SubTitle = "in micrograms per cubic meter",
                Caption = NULL,
                LineColor = c("REGIONAL AVG" = "#000000",
                              "Lang Isol" = "#E69F00",
                              "Minority" = "#56B4E9",
                              "No HS" = "#009E73",
                              "Poverty" = "#F0E442",
                              "Under 5" = "#0072B2",
                              "Over 65" = "#D55E00"))


# Pop Weighted avg of PM2.5 by RACE in CT relative to White average
ne_blkgrp_sf_DemoEJ %>% 
  as.data.frame() %>% 
  filter(STATE == "Connecticut") %>%
  dplyr::select(totalpopE,
                nhwhitepopE,
                minorityE,
                nhblackpopE,
                nhamerindpopE,
                nhasianpopE,
                nhnativhpopE,
                nhotherpopE,
                nh2morepopE,
                hisppopE,
                povknownE,
                num2povE, 
                eng_hhE,
                eng_limitE,
                age25upE,
                lthsE, 
                allAgesE, 
                under5E, 
                over65E, 
                PM25_19) %>% 
  gather(key = Group, value = Pop, totalpopE:over65E) %>% 
  group_by(Group) %>% 
  summarize(PM25wMean = weighted.mean(x = PM25_19, w = Pop, na.rm = TRUE),
            PM25Mean = mean(PM25_19, na.rm = TRUE)) %>% 
  spread(key = Group, value = PM25wMean) %>% 
  transmute(Black = (nhblackpopE/nhwhitepopE - 1)*100,
            Asian = (nhasianpopE/nhwhitepopE - 1)*100,
            AmerInd = (nhamerindpopE/nhwhitepopE - 1)*100,
            `Native PI/HI` = (nhnativhpopE/nhwhitepopE - 1)*100,
            Other = (nhotherpopE/nhwhitepopE - 1)*100,
            Multiracial = (nhnativhpopE/nhwhitepopE - 1)*100,
            Hispanic = (hisppopE/nhwhitepopE - 1)*100) %>%
  gather(key = Group, value = Pct) %>% 
  ggplot(aes(x = reorder(Group, -Pct), y = Pct, fill = Group)) + 
  geom_bar(stat = "identity", position = "identity") +
  theme_minimal() +
  labs(x = "", y = "", title = expression(atop(paste("Population-Weighted ", PM[2.5], " Exposure by Race for Connecticut"), "(relative to non-Hispanic White average)"))) + 
  theme(legend.position = 'none') +
  geom_text(aes(x = Group, y = Pct + 0.3 * sign(Pct), 
                label = paste0(round(Pct,2),"%")), 
            hjust = 0.5, size = 3,
            color=rgb(100,100,100, maxColorValue=255)) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  geom_hline(yintercept = 0)


# Pop Weighted avg of PM2.5 CHANGE 2011 - 2016 by RACE in CT relative to White average
ne_blkgrp_sf_DemoEJ %>% 
  as.data.frame() %>% 
  filter(STATE == "Connecticut") %>%
  dplyr::select(totalpopE,
                nhwhitepopE,
                minorityE,
                nhblackpopE,
                nhamerindpopE,
                nhasianpopE,
                nhnativhpopE,
                nhotherpopE,
                nh2morepopE,
                hisppopE,
                povknownE,
                num2povE, 
                eng_hhE,
                eng_limitE,
                age25upE,
                lthsE, 
                allAgesE, 
                under5E, 
                over65E, 
                PM25_pctChange) %>% 
  gather(key = Group, value = Pop, totalpopE:over65E) %>% 
  group_by(Group) %>% 
  summarize(PM25wMean = weighted.mean(x = PM25_pctChange, 
                                      w = Pop, na.rm = TRUE),
            PM25Mean = mean(PM25_pctChange, na.rm = TRUE)) %>% 
  spread(key = Group, value = PM25wMean) %>% 
  transmute(Black = (nhblackpopE/nhwhitepopE - 1)*100,
            Asian = (nhasianpopE/nhwhitepopE - 1)*100,
            AmerInd = (nhamerindpopE/nhwhitepopE - 1)*100,
            `Native PI/HI` = (nhnativhpopE/nhwhitepopE - 1)*100,
            Other = (nhotherpopE/nhwhitepopE - 1)*100,
            Multiracial = (nhnativhpopE/nhwhitepopE - 1)*100,
            Hispanic = (hisppopE/nhwhitepopE - 1)*100) %>%
  gather(key = Group, value = Pct) %>% 
  ggplot(aes(x = reorder(Group, -Pct), y = Pct, fill = Group)) + 
  geom_bar(stat = "identity", position = "identity") +
  theme_minimal() +
  labs(x = "", y = "", title = expression(atop(paste("Population-Weighted ", PM[2.5], " Exposure CHANGE 2011 - 2016 by Race"), " for Connecticut (relative to non-Hispanic White average)"))) + 
  theme(legend.position = 'none') +
  geom_text(aes(x = Group, y = Pct + 0.3 * sign(Pct), 
                label = paste0(round(Pct,2),"%")), 
            hjust = 0.5, size = 3,
            color=rgb(100,100,100, maxColorValue=255)) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  geom_hline(yintercept = 0)


# # Pop Weighted avg of PM2.5 for CT_INCOME in Connecticut relative to CT average
ne_blkgrp_sf_DemoEJ %>% 
  as.data.frame() %>% 
  filter(STATE == "Connecticut") %>%
  dplyr::select(totalpopE,
                CT_INCOME, 
                CT_INCOME_UC, 
                CT_INCOME_LC, 
                PM25_19) %>% 
  group_by(CT_INCOME) %>% 
  summarize(PM25wMean = weighted.mean(x = PM25_19, 
                                      w = totalpopE, na.rm = TRUE)) %>% 
  add_column(., !!! # append df with unequal length for global mean
    ne_blkgrp_sf_DemoEJ %>% 
      as.data.frame() %>% 
      filter(STATE == "Connecticut") %>% 
      summarize(avgPM25 = mean(PM25_19, na.rm = TRUE))
  ) %>% 
  mutate(pctDiff = (PM25wMean/avgPM25-1)*100) %>% 
  ggplot(aes(x = reorder(CT_INCOME, -pctDiff), 
             y = pctDiff, fill = CT_INCOME)) + 
  geom_bar(stat = "identity", position = "identity") +
  theme_minimal() +
  labs(x = "", y = "", 
       title = expression(paste("Population-Weighted ", PM[2.5], " Exposure (relative to CT average)"))) + 
  theme(legend.position = 'none') +
  geom_text(aes(x = CT_INCOME, y = pctDiff + 0.2 * sign(pctDiff), 
                label = paste0(round(pctDiff,2),"%")), 
            hjust = 0.5, size = 3,
            color=rgb(100,100,100, maxColorValue=255)) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  geom_hline(yintercept = 0) +
  scale_x_discrete(labels = c("Low Income", "Not Low Income"))


# # Pop Weighted avg of PM2.5 CHANGE 2011 - 2016 for CT_INCOME in Connecticut relative to CT average
ne_blkgrp_sf_DemoEJ %>% 
  as.data.frame() %>% 
  filter(STATE == "Connecticut") %>%
  dplyr::select(totalpopE,
                CT_INCOME, 
                CT_INCOME_UC, 
                CT_INCOME_LC, 
                PM25_pctChange) %>% 
  group_by(CT_INCOME) %>% 
  summarize(PM25wMean = weighted.mean(x = PM25_pctChange, 
                                      w = totalpopE, na.rm = TRUE)) %>% 
  add_column(., !!! # append df with unequal length for global mean
               ne_blkgrp_sf_DemoEJ %>% 
               as.data.frame() %>% 
               filter(STATE == "Connecticut") %>% 
               summarize(avgPM25 = mean(PM25_pctChange, na.rm = TRUE))
  ) %>% 
  mutate(pctDiff = (PM25wMean/avgPM25-1)*100) %>% 
  ggplot(aes(x = reorder(CT_INCOME, -pctDiff), 
             y = pctDiff, fill = CT_INCOME)) + 
  geom_bar(stat = "identity", position = "identity") +
  theme_minimal() +
  labs(x = "", y = "", 
       title = expression(paste("Population-Weighted ", PM[2.5], " Exposure CHANGE 2011 - 2016 (relative to CT average)"))) + 
  theme(legend.position = 'none') +
  geom_text(aes(x = CT_INCOME, y = pctDiff + 0.2 * sign(pctDiff), 
                label = paste0(round(pctDiff,2),"%")), 
            hjust = 0.5, size = 3,
            color=rgb(100,100,100, maxColorValue=255)) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  geom_hline(yintercept = 0) +
  scale_x_discrete(labels = c("Low Income", "Not Low Income"))
  

# Pop Weighted avg of PM2.5 for all Groups in Massachusetts relative to MA average
ne_blkgrp_sf_DemoEJ %>% 
  as.data.frame() %>% 
  filter(STATE == "Massachusetts") %>%
  dplyr::select(totalpopE,
                nhwhitepopE,
                minorityE,
                nhblackpopE,
                nhamerindpopE,
                nhasianpopE,
                nhnativhpopE,
                nhotherpopE,
                nh2morepopE,
                hisppopE,
                povknownE,
                num2povE, 
                eng_hhE,
                eng_limitE,
                age25upE,
                lthsE, 
                allAgesE, 
                under5E, 
                over65E, 
                PM25_19) %>% 
  gather(key = Group, value = Pop, totalpopE:over65E) %>% 
  group_by(Group) %>% 
  summarize(PM25wMean = weighted.mean(x = PM25_19, w = Pop, na.rm = TRUE),
            PM25Mean = mean(PM25_19, na.rm = TRUE)) %>% 
  spread(key = Group, value = PM25wMean) %>% 
  transmute(Minority = (minorityE/PM25Mean - 1)*100,
            #Minority_NHW = (minorityE/nhwhitepopE - 1)*100,
            `Lang Isol` = (eng_limitE/PM25Mean - 1)*100,
            Poverty = (num2povE/PM25Mean - 1)*100,
            `No HS` = (lthsE/PM25Mean - 1)*100,
            `Under 5` = (under5E/PM25Mean - 1)*100,
            `Over 65` = (over65E/PM25Mean - 1)*100) %>%
  gather(key = Group, value = Pct) %>% 
  ggplot(aes(x = reorder(Group, -Pct), y = Pct, fill = Group)) + 
  geom_bar(stat = "identity", position = "identity") +
  theme_minimal() +
  labs(x = "", y = "", title = expression(paste("Population-Weighted ", PM[2.5], " Exposure (relative to MA average)"))) + 
  theme(legend.position = 'none') +
  geom_text(aes(x = Group, y = Pct + 0.2 * sign(Pct), 
                label = paste0(round(Pct,2),"%")), 
            hjust = 0.5, size = 3,
            color=rgb(100,100,100, maxColorValue=255)) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  geom_hline(yintercept = 0)


# Pop Weighted avg of PM2.5 CHANGE 2011 - 2016 for all Groups in Massachusetts relative to MA average
ne_blkgrp_sf_DemoEJ %>% 
  as.data.frame() %>% 
  filter(STATE == "Massachusetts") %>%
  dplyr::select(totalpopE,
                nhwhitepopE,
                minorityE,
                nhblackpopE,
                nhamerindpopE,
                nhasianpopE,
                nhnativhpopE,
                nhotherpopE,
                nh2morepopE,
                hisppopE,
                povknownE,
                num2povE, 
                eng_hhE,
                eng_limitE,
                age25upE,
                lthsE, 
                allAgesE, 
                under5E, 
                over65E, 
                PM25_pctChange) %>% 
  gather(key = Group, value = Pop, totalpopE:over65E) %>% 
  group_by(Group) %>% 
  summarize(PM25wMean = weighted.mean(x = PM25_pctChange, 
                                      w = Pop, na.rm = TRUE),
            PM25Mean = mean(PM25_pctChange, na.rm = TRUE)) %>% 
  spread(key = Group, value = PM25wMean) %>% 
  transmute(Minority = (minorityE/PM25Mean - 1)*100,
            #Minority_NHW = (minorityE/nhwhitepopE - 1)*100,
            `Lang Isol` = (eng_limitE/PM25Mean - 1)*100,
            Poverty = (num2povE/PM25Mean - 1)*100,
            `No HS` = (lthsE/PM25Mean - 1)*100,
            `Under 5` = (under5E/PM25Mean - 1)*100,
            `Over 65` = (over65E/PM25Mean - 1)*100) %>%
  gather(key = Group, value = Pct) %>% 
  ggplot(aes(x = reorder(Group, -Pct), y = Pct, fill = Group)) + 
  geom_bar(stat = "identity", position = "identity") +
  theme_minimal() +
  labs(x = "", y = "", title = expression(atop(paste("Population-Weighted ", PM[2.5], " Exposure CHANGE 2011 - 2016"), " (relative to MA average)"))) + 
  theme(legend.position = 'none') +
  geom_text(aes(x = Group, y = Pct + 0.2 * sign(Pct), 
                label = paste0(round(Pct,2),"%")), 
            hjust = 0.5, size = 3,
            color=rgb(100,100,100, maxColorValue=255)) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  geom_hline(yintercept = 0)


# Slope graph of change in pop-weighted PM2.5 exposure beteween 2011 and 2016
# Create df of 2019 actual values
PM19wAvgs_actualMA <- ne_blkgrp_sf_DemoEJ %>% 
  as.data.frame() %>% 
  filter(STATE == "Massachusetts") %>%
  dplyr::select(minorityE,
                num2povE,
                eng_limitE,
                lthsE,
                under5E,
                over65E,
                PM25_19) %>% 
  gather(key = Group, value = Pop, minorityE:over65E) %>% 
  group_by(Group) %>% 
  summarize(PM25wMean = weighted.mean(x = PM25_19, 
                                      w = Pop, na.rm = TRUE)) %>% 
  mutate(Group = case_when(
    Group == "minorityE" ~ "Minority",
    Group == "eng_limitE" ~ "Lang Isol",
    Group == "num2povE" ~ "Poverty",
    Group == "lthsE" ~ "No HS",
    Group == "under5E" ~ "Under 5",
    Group == "over65E" ~ "Over 65")) %>% 
  mutate(Year = 2016)
# Create df of 2015 values
PM15wAvgs_actualMA <- ne_blkgrp_sf_DemoEJ %>% 
  as.data.frame() %>% 
  filter(STATE == "Massachusetts") %>%
  dplyr::select(mins_15,
                lowinc_15,
                lingiso_15,
                lths_15,
                under5_15,
                over64_15,
                pm_15) %>% 
  gather(key = Group, value = Pop, mins_15:over64_15) %>% 
  group_by(Group) %>% 
  summarize(PM25wMean = weighted.mean(x = pm_15, 
                                      w = Pop, na.rm = TRUE)) %>%  
  mutate(Group = case_when(
    Group == "mins_15" ~ "Minority",
    Group == "lingiso_15" ~ "Lang Isol",
    Group == "lowinc_15" ~ "Poverty",
    Group == "lths_15" ~ "No HS",
    Group == "under5_15" ~ "Under 5",
    Group == "over64_15" ~ "Over 65")) %>% 
  mutate(Year = 2011)
# Create regional average benchmark
pm15NEavgMA <- ne_blkgrp_sf_DemoEJ %>% 
  as.data.frame() %>% 
  filter(STATE == "Massachusetts") %>% 
  summarize(mean(pm_15, na.rm = TRUE))
pm19NEavgMA <- ne_blkgrp_sf_DemoEJ %>% 
  as.data.frame() %>% 
  filter(STATE == "Massachusetts") %>% 
  summarize(mean(PM25_19, na.rm = TRUE))
pmRegionalAvg_actualMA <- data.frame(
  Group = c("REGIONAL AVG", "REGIONAL AVG"),
  PM25wMean = c(pm15NEavgMA[1,1],pm19NEavgMA[1,1]),
  Year = c(2011,2016)
)
#rbind tables
PM11_16wAvg_actualMA <- bind_rows(PM15wAvgs_actualMA,
                                  PM19wAvgs_actualMA,
                                  pmRegionalAvg_actual) %>% 
  mutate(Year = as.factor(Year),
         PM25wMean = round(PM25wMean,1))
# make slope graph
newggslopegraph(dataframe = PM11_16wAvg_actualMA, 
                Times = Year, 
                Measurement = PM25wMean, 
                Grouping = Group,
                Title = "Population-Weighted PM2.5 Exposure for MA",
                SubTitle = "in micrograms per cubic meter",
                Caption = NULL,
                LineColor = c("REGIONAL AVG" = "#000000",
                              "Lang Isol" = "#E69F00",
                              "Minority" = "#56B4E9",
                              "No HS" = "#009E73",
                              "Poverty" = "#F0E442",
                              "Under 5" = "#0072B2",
                              "Over 65" = "#D55E00"))


# Pop Weighted avg of PM2.5 by RACE in MA relative to White average
ne_blkgrp_sf_DemoEJ %>% 
  as.data.frame() %>% 
  filter(STATE == "Massachusetts") %>%
  dplyr::select(totalpopE,
                nhwhitepopE,
                minorityE,
                nhblackpopE,
                nhamerindpopE,
                nhasianpopE,
                nhnativhpopE,
                nhotherpopE,
                nh2morepopE,
                hisppopE,
                povknownE,
                num2povE, 
                eng_hhE,
                eng_limitE,
                age25upE,
                lthsE, 
                allAgesE, 
                under5E, 
                over65E, 
                PM25_19) %>% 
  gather(key = Group, value = Pop, totalpopE:over65E) %>% 
  group_by(Group) %>% 
  summarize(PM25wMean = weighted.mean(x = PM25_19, w = Pop, na.rm = TRUE),
            PM25Mean = mean(PM25_19, na.rm = TRUE)) %>% 
  spread(key = Group, value = PM25wMean) %>% 
  transmute(Black = (nhblackpopE/nhwhitepopE - 1)*100,
            Asian = (nhasianpopE/nhwhitepopE - 1)*100,
            AmerInd = (nhamerindpopE/nhwhitepopE - 1)*100,
            `Native PI/HI` = (nhnativhpopE/nhwhitepopE - 1)*100,
            Other = (nhotherpopE/nhwhitepopE - 1)*100,
            Multiracial = (nhnativhpopE/nhwhitepopE - 1)*100,
            Hispanic = (hisppopE/nhwhitepopE - 1)*100) %>%
  gather(key = Group, value = Pct) %>% 
  ggplot(aes(x = reorder(Group, -Pct), y = Pct, fill = Group)) + 
  geom_bar(stat = "identity", position = "identity") +
  theme_minimal() +
  labs(x = "", y = "", title = expression(atop(paste("Population-Weighted ", PM[2.5], " Exposure by Race for Massachusetts"), "(relative to non-Hispanic White average)"))) + 
  theme(legend.position = 'none') +
  geom_text(aes(x = Group, y = Pct + 0.3 * sign(Pct), 
                label = paste0(round(Pct,2),"%")), 
            hjust = 0.5, size = 3,
            color=rgb(100,100,100, maxColorValue=255)) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  geom_hline(yintercept = 0)


# Pop Weighted avg of PM2.5 CHANGE 2011 - 2016 by RACE in MA relative to White average
ne_blkgrp_sf_DemoEJ %>% 
  as.data.frame() %>% 
  filter(STATE == "Massachusetts") %>%
  dplyr::select(totalpopE,
                nhwhitepopE,
                minorityE,
                nhblackpopE,
                nhamerindpopE,
                nhasianpopE,
                nhnativhpopE,
                nhotherpopE,
                nh2morepopE,
                hisppopE,
                povknownE,
                num2povE, 
                eng_hhE,
                eng_limitE,
                age25upE,
                lthsE, 
                allAgesE, 
                under5E, 
                over65E, 
                PM25_pctChange) %>% 
  gather(key = Group, value = Pop, totalpopE:over65E) %>% 
  group_by(Group) %>% 
  summarize(PM25wMean = weighted.mean(x = PM25_pctChange, 
                                      w = Pop, na.rm = TRUE),
            PM25Mean = mean(PM25_pctChange, na.rm = TRUE)) %>% 
  spread(key = Group, value = PM25wMean) %>% 
  transmute(Black = (nhblackpopE/nhwhitepopE - 1)*100,
            Asian = (nhasianpopE/nhwhitepopE - 1)*100,
            AmerInd = (nhamerindpopE/nhwhitepopE - 1)*100,
            `Native PI/HI` = (nhnativhpopE/nhwhitepopE - 1)*100,
            Other = (nhotherpopE/nhwhitepopE - 1)*100,
            Multiracial = (nhnativhpopE/nhwhitepopE - 1)*100,
            Hispanic = (hisppopE/nhwhitepopE - 1)*100) %>%
  gather(key = Group, value = Pct) %>% 
  ggplot(aes(x = reorder(Group, -Pct), y = Pct, fill = Group)) + 
  geom_bar(stat = "identity", position = "identity") +
  theme_minimal() +
  labs(x = "", y = "", title = expression(atop(paste("Population-Weighted ", PM[2.5], " Exposure CHANGE 2011 - 2016 by Race"), " for Massachusetts (relative to non-Hispanic White average)"))) + 
  theme(legend.position = 'none') +
  geom_text(aes(x = Group, y = Pct + 0.3 * sign(Pct), 
                label = paste0(round(Pct,2),"%")), 
            hjust = 0.5, size = 3,
            color=rgb(100,100,100, maxColorValue=255)) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  geom_hline(yintercept = 0)


# # Pop Weighted avg of PM2.5 for Massachusetts EJ criteria in Massachusetts relative to MA average
# library(plyr)
ne_blkgrp_sf_DemoEJ %>% 
  as.data.frame() %>% 
  filter(STATE == "Massachusetts") %>%
  dplyr::select(totalpopE,
                MA_INCOME, 
                MA_MINORITY, 
                MA_ENGLISH, 
                PM25_19) %>% 
  gather(key = Group, value = Criterion, MA_INCOME:MA_ENGLISH) %>% 
  filter(Criterion != "NA") %>% 
  plyr::ddply("Group", summarize, 
        PM25wMean = weighted.mean(x = PM25_19, w = totalpopE, na.rm = TRUE)) %>%
  add_column(., !!! # append df with unequal length for global mean
               ne_blkgrp_sf_DemoEJ %>% 
               as.data.frame() %>% 
               filter(STATE == "Massachusetts") %>% 
               summarize(avgPM25 = mean(PM25_19, na.rm = TRUE))) %>% 
  mutate(pctDiff = (PM25wMean/avgPM25-1)*100) %>% 
  ggplot(aes(x = reorder(Group, -pctDiff), 
             y = pctDiff, fill = Group)) + 
  geom_bar(stat = "identity", position = "identity") +
  theme_minimal() +
  labs(x = "", y = "", 
       title = expression(paste("Population-Weighted ", PM[2.5], " Exposure for EJ Groups (relative to MA average)"))) + 
  theme(legend.position = 'none') +
  geom_text(aes(x = Group, y = pctDiff + 0.2 * sign(pctDiff), 
                label = paste0(round(pctDiff,2),"%")), 
            hjust = 0.5, size = 3,
            color=rgb(100,100,100, maxColorValue=255)) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  geom_hline(yintercept = 0) +
  scale_x_discrete(labels = c("Minority", "English Language Isolation",
                              "Income"))


# # Pop Weighted avg of PM2.5 CHANGE 2011 - 2016 for Massachusetts EJ criteria in Massachusetts relative to MA average
# library(plyr)
ne_blkgrp_sf_DemoEJ %>% 
  as.data.frame() %>% 
  filter(STATE == "Massachusetts") %>%
  dplyr::select(totalpopE,
                MA_INCOME, 
                MA_MINORITY, 
                MA_ENGLISH, 
                PM25_pctChange) %>% 
  gather(key = Group, value = Criterion, MA_INCOME:MA_ENGLISH) %>% 
  filter(Criterion != "NA") %>% 
  plyr::ddply("Group", summarize, 
              PM25wMean = weighted.mean(x = PM25_pctChange, 
                                        w = totalpopE, na.rm = TRUE)) %>%
  add_column(., !!! # append df with unequal length for global mean
               ne_blkgrp_sf_DemoEJ %>% 
               as.data.frame() %>% 
               filter(STATE == "Massachusetts") %>% 
               summarize(avgPM25 = mean(PM25_pctChange, na.rm = TRUE))) %>% 
  mutate(pctDiff = (PM25wMean/avgPM25-1)*100) %>% 
  ggplot(aes(x = reorder(Group, -pctDiff), 
             y = pctDiff, fill = Group)) + 
  geom_bar(stat = "identity", position = "identity") +
  theme_minimal() +
  labs(x = "", y = "", 
       title = expression(atop(paste("Population-Weighted ", PM[2.5], " Exposure CHANGE 2011 - 2016 for EJ Groups"), "(relative to MA average)"))) + 
  theme(legend.position = 'none') +
  geom_text(aes(x = Group, y = pctDiff + 0.2 * sign(pctDiff), 
                label = paste0(round(pctDiff,2),"%")), 
            hjust = 0.5, size = 3,
            color=rgb(100,100,100, maxColorValue=255)) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  geom_hline(yintercept = 0) +
  scale_x_discrete(labels = c("Minority", "English Language Isolation",
                              "Income"))


# Pop Weighted avg of PM2.5 for all Groups in Maine relative to ME average
ne_blkgrp_sf_DemoEJ %>% 
  as.data.frame() %>% 
  filter(STATE == "Maine") %>%
  dplyr::select(totalpopE,
                nhwhitepopE,
                minorityE,
                nhblackpopE,
                nhamerindpopE,
                nhasianpopE,
                nhnativhpopE,
                nhotherpopE,
                nh2morepopE,
                hisppopE,
                povknownE,
                num2povE, 
                eng_hhE,
                eng_limitE,
                age25upE,
                lthsE, 
                allAgesE, 
                under5E, 
                over65E, 
                PM25_19) %>% 
  gather(key = Group, value = Pop, totalpopE:over65E) %>% 
  group_by(Group) %>% 
  summarize(PM25wMean = weighted.mean(x = PM25_19, w = Pop, na.rm = TRUE),
            PM25Mean = mean(PM25_19, na.rm = TRUE)) %>% 
  spread(key = Group, value = PM25wMean) %>% 
  transmute(Minority = (minorityE/PM25Mean - 1)*100,
            #Minority_NHW = (minorityE/nhwhitepopE - 1)*100,
            `Lang Isol` = (eng_limitE/PM25Mean - 1)*100,
            Poverty = (num2povE/PM25Mean - 1)*100,
            `No HS` = (lthsE/PM25Mean - 1)*100,
            `Under 5` = (under5E/PM25Mean - 1)*100,
            `Over 65` = (over65E/PM25Mean - 1)*100) %>%
  gather(key = Group, value = Pct) %>% 
  ggplot(aes(x = reorder(Group, -Pct), y = Pct, fill = Group)) + 
  geom_bar(stat = "identity", position = "identity") +
  theme_minimal() +
  labs(x = "", y = "", title = expression(paste("Population-Weighted ", PM[2.5], " Exposure (relative to ME average)"))) + 
  theme(legend.position = 'none') +
  geom_text(aes(x = Group, y = Pct + 0.2 * sign(Pct), 
                label = paste0(round(Pct,2),"%")), 
            hjust = 0.5, size = 3,
            color=rgb(100,100,100, maxColorValue=255)) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  geom_hline(yintercept = 0)


# Pop Weighted avg of PM2.5 CHANGE 2011 - 2016 for all Groups in Maine relative to ME average
ne_blkgrp_sf_DemoEJ %>% 
  as.data.frame() %>% 
  filter(STATE == "Maine") %>%
  dplyr::select(totalpopE,
                nhwhitepopE,
                minorityE,
                nhblackpopE,
                nhamerindpopE,
                nhasianpopE,
                nhnativhpopE,
                nhotherpopE,
                nh2morepopE,
                hisppopE,
                povknownE,
                num2povE, 
                eng_hhE,
                eng_limitE,
                age25upE,
                lthsE, 
                allAgesE, 
                under5E, 
                over65E, 
                PM25_pctChange) %>% 
  gather(key = Group, value = Pop, totalpopE:over65E) %>% 
  group_by(Group) %>% 
  summarize(PM25wMean = weighted.mean(x = PM25_pctChange, 
                                      w = Pop, na.rm = TRUE),
            PM25Mean = mean(PM25_pctChange, na.rm = TRUE)) %>% 
  spread(key = Group, value = PM25wMean) %>% 
  transmute(Minority = (minorityE/PM25Mean - 1)*100,
            #Minority_NHW = (minorityE/nhwhitepopE - 1)*100,
            `Lang Isol` = (eng_limitE/PM25Mean - 1)*100,
            Poverty = (num2povE/PM25Mean - 1)*100,
            `No HS` = (lthsE/PM25Mean - 1)*100,
            `Under 5` = (under5E/PM25Mean - 1)*100,
            `Over 65` = (over65E/PM25Mean - 1)*100) %>%
  gather(key = Group, value = Pct) %>% 
  ggplot(aes(x = reorder(Group, -Pct), y = Pct, fill = Group)) + 
  geom_bar(stat = "identity", position = "identity") +
  theme_minimal() +
  labs(x = "", y = "", title = expression(atop(paste("Population-Weighted ", PM[2.5], " Exposure CHANGE 2011 - 2016"), " (relative to ME average)"))) + 
  theme(legend.position = 'none') +
  geom_text(aes(x = Group, y = Pct + 0.2 * sign(Pct), 
                label = paste0(round(Pct,2),"%")), 
            hjust = 0.5, size = 3,
            color=rgb(100,100,100, maxColorValue=255)) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  geom_hline(yintercept = 0)


# Slope graph of change in pop-weighted PM2.5 exposure beteween 2011 and 2016
# Create df of 2019 actual values
PM19wAvgs_actualME <- ne_blkgrp_sf_DemoEJ %>% 
  as.data.frame() %>% 
  filter(STATE == "Maine") %>%
  dplyr::select(minorityE,
                num2povE,
                eng_limitE,
                lthsE,
                under5E,
                over65E,
                PM25_19) %>% 
  gather(key = Group, value = Pop, minorityE:over65E) %>% 
  group_by(Group) %>% 
  summarize(PM25wMean = weighted.mean(x = PM25_19, 
                                      w = Pop, na.rm = TRUE)) %>% 
  mutate(Group = case_when(
    Group == "minorityE" ~ "Minority",
    Group == "eng_limitE" ~ "Lang Isol",
    Group == "num2povE" ~ "Poverty",
    Group == "lthsE" ~ "No HS",
    Group == "under5E" ~ "Under 5",
    Group == "over65E" ~ "Over 65")) %>% 
  mutate(Year = 2016)
# Create df of 2015 values
PM15wAvgs_actualME <- ne_blkgrp_sf_DemoEJ %>% 
  as.data.frame() %>% 
  filter(STATE == "Maine") %>%
  dplyr::select(mins_15,
                lowinc_15,
                lingiso_15,
                lths_15,
                under5_15,
                over64_15,
                pm_15) %>% 
  gather(key = Group, value = Pop, mins_15:over64_15) %>% 
  group_by(Group) %>% 
  summarize(PM25wMean = weighted.mean(x = pm_15, 
                                      w = Pop, na.rm = TRUE)) %>%  
  mutate(Group = case_when(
    Group == "mins_15" ~ "Minority",
    Group == "lingiso_15" ~ "Lang Isol",
    Group == "lowinc_15" ~ "Poverty",
    Group == "lths_15" ~ "No HS",
    Group == "under5_15" ~ "Under 5",
    Group == "over64_15" ~ "Over 65")) %>% 
  mutate(Year = 2011)
# Create regional average benchmark
pm15NEavgME <- ne_blkgrp_sf_DemoEJ %>% 
  as.data.frame() %>% 
  filter(STATE == "Maine") %>% 
  summarize(mean(pm_15, na.rm = TRUE))
pm19NEavgME <- ne_blkgrp_sf_DemoEJ %>% 
  as.data.frame() %>% 
  filter(STATE == "Maine") %>% 
  summarize(mean(PM25_19, na.rm = TRUE))
pmRegionalAvg_actualME <- data.frame(
  Group = c("REGIONAL AVG", "REGIONAL AVG"),
  PM25wMean = c(pm15NEavgME[1,1],pm19NEavgME[1,1]),
  Year = c(2011,2016)
)
#rbind tables
PM11_16wAvg_actualME <- bind_rows(PM15wAvgs_actualME,
                                  PM19wAvgs_actualME,
                                  pmRegionalAvg_actual) %>% 
  mutate(Year = as.factor(Year),
         PM25wMean = round(PM25wMean,1))
# make slope graph
newggslopegraph(dataframe = PM11_16wAvg_actualME, 
                Times = Year, 
                Measurement = PM25wMean, 
                Grouping = Group,
                Title = "Population-Weighted PM2.5 Exposure for ME",
                SubTitle = "in micrograms per cubic meter",
                Caption = NULL,
                LineColor = c("REGIONAL AVG" = "#000000",
                              "Lang Isol" = "#E69F00",
                              "Minority" = "#56B4E9",
                              "No HS" = "#009E73",
                              "Poverty" = "#F0E442",
                              "Under 5" = "#0072B2",
                              "Over 65" = "#D55E00"))


# Pop Weighted avg of PM2.5 by RACE in ME relative to White average
ne_blkgrp_sf_DemoEJ %>% 
  as.data.frame() %>% 
  filter(STATE == "Maine") %>%
  dplyr::select(totalpopE,
                nhwhitepopE,
                minorityE,
                nhblackpopE,
                nhamerindpopE,
                nhasianpopE,
                nhnativhpopE,
                nhotherpopE,
                nh2morepopE,
                hisppopE,
                povknownE,
                num2povE, 
                eng_hhE,
                eng_limitE,
                age25upE,
                lthsE, 
                allAgesE, 
                under5E, 
                over65E, 
                PM25_19) %>% 
  gather(key = Group, value = Pop, totalpopE:over65E) %>% 
  group_by(Group) %>% 
  summarize(PM25wMean = weighted.mean(x = PM25_19, w = Pop, na.rm = TRUE),
            PM25Mean = mean(PM25_19, na.rm = TRUE)) %>% 
  spread(key = Group, value = PM25wMean) %>% 
  transmute(Black = (nhblackpopE/nhwhitepopE - 1)*100,
            Asian = (nhasianpopE/nhwhitepopE - 1)*100,
            AmerInd = (nhamerindpopE/nhwhitepopE - 1)*100,
            `Native PI/HI` = (nhnativhpopE/nhwhitepopE - 1)*100,
            Other = (nhotherpopE/nhwhitepopE - 1)*100,
            Multiracial = (nhnativhpopE/nhwhitepopE - 1)*100,
            Hispanic = (hisppopE/nhwhitepopE - 1)*100) %>%
  gather(key = Group, value = Pct) %>% 
  ggplot(aes(x = reorder(Group, -Pct), y = Pct, fill = Group)) + 
  geom_bar(stat = "identity", position = "identity") +
  theme_minimal() +
  labs(x = "", y = "", title = expression(atop(paste("Population-Weighted ", PM[2.5], " Exposure by Race for Maine"), "(relative to non-Hispanic White average)"))) + 
  theme(legend.position = 'none') +
  geom_text(aes(x = Group, y = Pct + 0.3 * sign(Pct), 
                label = paste0(round(Pct,2),"%")), 
            hjust = 0.5, size = 3,
            color=rgb(100,100,100, maxColorValue=255)) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  geom_hline(yintercept = 0)


# Pop Weighted avg of PM2.5 CHANGE by RACE in ME relative to White average
ne_blkgrp_sf_DemoEJ %>% 
  as.data.frame() %>% 
  filter(STATE == "Maine") %>%
  dplyr::select(totalpopE,
                nhwhitepopE,
                minorityE,
                nhblackpopE,
                nhamerindpopE,
                nhasianpopE,
                nhnativhpopE,
                nhotherpopE,
                nh2morepopE,
                hisppopE,
                povknownE,
                num2povE, 
                eng_hhE,
                eng_limitE,
                age25upE,
                lthsE, 
                allAgesE, 
                under5E, 
                over65E, 
                PM25_pctChange) %>% 
  gather(key = Group, value = Pop, totalpopE:over65E) %>% 
  group_by(Group) %>% 
  summarize(PM25wMean = weighted.mean(x = PM25_pctChange, 
                                      w = Pop, na.rm = TRUE),
            PM25Mean = mean(PM25_pctChange, na.rm = TRUE)) %>% 
  spread(key = Group, value = PM25wMean) %>% 
  transmute(Black = (nhblackpopE/nhwhitepopE - 1)*100,
            Asian = (nhasianpopE/nhwhitepopE - 1)*100,
            AmerInd = (nhamerindpopE/nhwhitepopE - 1)*100,
            `Native PI/HI` = (nhnativhpopE/nhwhitepopE - 1)*100,
            Other = (nhotherpopE/nhwhitepopE - 1)*100,
            Multiracial = (nhnativhpopE/nhwhitepopE - 1)*100,
            Hispanic = (hisppopE/nhwhitepopE - 1)*100) %>%
  gather(key = Group, value = Pct) %>% 
  ggplot(aes(x = reorder(Group, -Pct), y = Pct, fill = Group)) + 
  geom_bar(stat = "identity", position = "identity") +
  theme_minimal() +
  labs(x = "", y = "", title = expression(atop(paste("Population-Weighted ", PM[2.5], " Exposure CHANGE 2011 - 2016 by Race"), " for Maine (relative to non-Hispanic White average)"))) + 
  theme(legend.position = 'none') +
  geom_text(aes(x = Group, y = Pct + 0.3 * sign(Pct), 
                label = paste0(round(Pct,2),"%")), 
            hjust = 0.5, size = 3,
            color=rgb(100,100,100, maxColorValue=255)) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  geom_hline(yintercept = 0)


# Pop Weighted avg of PM2.5 for all Groups in New Hampshire relative to NH average
ne_blkgrp_sf_DemoEJ %>% 
  as.data.frame() %>% 
  filter(STATE == "New Hampshire") %>%
  dplyr::select(totalpopE,
                nhwhitepopE,
                minorityE,
                nhblackpopE,
                nhamerindpopE,
                nhasianpopE,
                nhnativhpopE,
                nhotherpopE,
                nh2morepopE,
                hisppopE,
                povknownE,
                num2povE, 
                eng_hhE,
                eng_limitE,
                age25upE,
                lthsE, 
                allAgesE, 
                under5E, 
                over65E, 
                PM25_19) %>% 
  gather(key = Group, value = Pop, totalpopE:over65E) %>% 
  group_by(Group) %>% 
  summarize(PM25wMean = weighted.mean(x = PM25_19, w = Pop, na.rm = TRUE),
            PM25Mean = mean(PM25_19, na.rm = TRUE)) %>% 
  spread(key = Group, value = PM25wMean) %>% 
  transmute(Minority = (minorityE/PM25Mean - 1)*100,
            #Minority_NHW = (minorityE/nhwhitepopE - 1)*100,
            `Lang Isol` = (eng_limitE/PM25Mean - 1)*100,
            Poverty = (num2povE/PM25Mean - 1)*100,
            `No HS` = (lthsE/PM25Mean - 1)*100,
            `Under 5` = (under5E/PM25Mean - 1)*100,
            `Over 65` = (over65E/PM25Mean - 1)*100) %>%
  gather(key = Group, value = Pct) %>% 
  ggplot(aes(x = reorder(Group, -Pct), y = Pct, fill = Group)) + 
  geom_bar(stat = "identity", position = "identity") +
  theme_minimal() +
  labs(x = "", y = "", title = expression(paste("Population-Weighted ", PM[2.5], " Exposure (relative to NH average)"))) + 
  theme(legend.position = 'none') +
  geom_text(aes(x = Group, y = Pct + 0.2 * sign(Pct), 
                label = paste0(round(Pct,2),"%")), 
            hjust = 0.5, size = 3,
            color=rgb(100,100,100, maxColorValue=255)) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  geom_hline(yintercept = 0)


# Pop Weighted avg of PM2.5 CHANGE for all Groups in New Hampshire relative to NH average
ne_blkgrp_sf_DemoEJ %>% 
  as.data.frame() %>% 
  filter(STATE == "New Hampshire") %>%
  dplyr::select(totalpopE,
                nhwhitepopE,
                minorityE,
                nhblackpopE,
                nhamerindpopE,
                nhasianpopE,
                nhnativhpopE,
                nhotherpopE,
                nh2morepopE,
                hisppopE,
                povknownE,
                num2povE, 
                eng_hhE,
                eng_limitE,
                age25upE,
                lthsE, 
                allAgesE, 
                under5E, 
                over65E, 
                PM25_pctChange) %>% 
  gather(key = Group, value = Pop, totalpopE:over65E) %>% 
  group_by(Group) %>% 
  summarize(PM25wMean = weighted.mean(x = PM25_pctChange, 
                                      w = Pop, na.rm = TRUE),
            PM25Mean = mean(PM25_pctChange, na.rm = TRUE)) %>% 
  spread(key = Group, value = PM25wMean) %>% 
  transmute(Minority = (minorityE/PM25Mean - 1)*100,
            #Minority_NHW = (minorityE/nhwhitepopE - 1)*100,
            `Lang Isol` = (eng_limitE/PM25Mean - 1)*100,
            Poverty = (num2povE/PM25Mean - 1)*100,
            `No HS` = (lthsE/PM25Mean - 1)*100,
            `Under 5` = (under5E/PM25Mean - 1)*100,
            `Over 65` = (over65E/PM25Mean - 1)*100) %>%
  gather(key = Group, value = Pct) %>% 
  ggplot(aes(x = reorder(Group, -Pct), y = Pct, fill = Group)) + 
  geom_bar(stat = "identity", position = "identity") +
  theme_minimal() +
  labs(x = "", y = "", title = expression(atop(paste("Population-Weighted ", PM[2.5], " Exposure CHANGE 2011 - 2016"), " (relative to NH average)"))) + 
  theme(legend.position = 'none') +
  geom_text(aes(x = Group, y = Pct + 0.2 * sign(Pct), 
                label = paste0(round(Pct,2),"%")), 
            hjust = 0.5, size = 3,
            color=rgb(100,100,100, maxColorValue=255)) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  geom_hline(yintercept = 0)


# Slope graph of change in pop-weighted PM2.5 exposure beteween 2011 and 2016
# Create df of 2019 actual values
PM19wAvgs_actualNH <- ne_blkgrp_sf_DemoEJ %>% 
  as.data.frame() %>% 
  filter(STATE == "New Hampshire") %>%
  dplyr::select(minorityE,
                num2povE,
                eng_limitE,
                lthsE,
                under5E,
                over65E,
                PM25_19) %>% 
  gather(key = Group, value = Pop, minorityE:over65E) %>% 
  group_by(Group) %>% 
  summarize(PM25wMean = weighted.mean(x = PM25_19, 
                                      w = Pop, na.rm = TRUE)) %>% 
  mutate(Group = case_when(
    Group == "minorityE" ~ "Minority",
    Group == "eng_limitE" ~ "Lang Isol",
    Group == "num2povE" ~ "Poverty",
    Group == "lthsE" ~ "No HS",
    Group == "under5E" ~ "Under 5",
    Group == "over65E" ~ "Over 65")) %>% 
  mutate(Year = 2016)
# Create df of 2015 values
PM15wAvgs_actualNH <- ne_blkgrp_sf_DemoEJ %>% 
  as.data.frame() %>% 
  filter(STATE == "New Hampshire") %>%
  dplyr::select(mins_15,
                lowinc_15,
                lingiso_15,
                lths_15,
                under5_15,
                over64_15,
                pm_15) %>% 
  gather(key = Group, value = Pop, mins_15:over64_15) %>% 
  group_by(Group) %>% 
  summarize(PM25wMean = weighted.mean(x = pm_15, 
                                      w = Pop, na.rm = TRUE)) %>%  
  mutate(Group = case_when(
    Group == "mins_15" ~ "Minority",
    Group == "lingiso_15" ~ "Lang Isol",
    Group == "lowinc_15" ~ "Poverty",
    Group == "lths_15" ~ "No HS",
    Group == "under5_15" ~ "Under 5",
    Group == "over64_15" ~ "Over 65")) %>% 
  mutate(Year = 2011)
# Create regional average benchmark
pm15NEavgNH <- ne_blkgrp_sf_DemoEJ %>% 
  as.data.frame() %>% 
  filter(STATE == "New Hampshire") %>% 
  summarize(mean(pm_15, na.rm = TRUE))
pm19NEavgNH <- ne_blkgrp_sf_DemoEJ %>% 
  as.data.frame() %>% 
  filter(STATE == "New Hampshire") %>% 
  summarize(mean(PM25_19, na.rm = TRUE))
pmRegionalAvg_actualNH <- data.frame(
  Group = c("REGIONAL AVG", "REGIONAL AVG"),
  PM25wMean = c(pm15NEavgNH[1,1],pm19NEavgNH[1,1]),
  Year = c(2011,2016)
)
#rbind tables
PM11_16wAvg_actualNH <- bind_rows(PM15wAvgs_actualNH,
                                  PM19wAvgs_actualNH,
                                  pmRegionalAvg_actual) %>% 
  mutate(Year = as.factor(Year),
         PM25wMean = round(PM25wMean,1))
# make slope graph
newggslopegraph(dataframe = PM11_16wAvg_actualNH, 
                Times = Year, 
                Measurement = PM25wMean, 
                Grouping = Group,
                Title = "Population-Weighted PM2.5 Exposure for NH",
                SubTitle = "in micrograms per cubic meter",
                Caption = NULL,
                LineColor = c("REGIONAL AVG" = "#000000",
                              "Lang Isol" = "#E69F00",
                              "Minority" = "#56B4E9",
                              "No HS" = "#009E73",
                              "Poverty" = "#F0E442",
                              "Under 5" = "#0072B2",
                              "Over 65" = "#D55E00"))


# Pop Weighted avg of PM2.5 by RACE in NH relative to White average
ne_blkgrp_sf_DemoEJ %>% 
  as.data.frame() %>% 
  filter(STATE == "New Hampshire") %>%
  dplyr::select(totalpopE,
                nhwhitepopE,
                minorityE,
                nhblackpopE,
                nhamerindpopE,
                nhasianpopE,
                nhnativhpopE,
                nhotherpopE,
                nh2morepopE,
                hisppopE,
                povknownE,
                num2povE, 
                eng_hhE,
                eng_limitE,
                age25upE,
                lthsE, 
                allAgesE, 
                under5E, 
                over65E, 
                PM25_19) %>% 
  gather(key = Group, value = Pop, totalpopE:over65E) %>% 
  group_by(Group) %>% 
  summarize(PM25wMean = weighted.mean(x = PM25_19, w = Pop, na.rm = TRUE),
            PM25Mean = mean(PM25_19, na.rm = TRUE)) %>% 
  spread(key = Group, value = PM25wMean) %>% 
  transmute(Black = (nhblackpopE/nhwhitepopE - 1)*100,
            Asian = (nhasianpopE/nhwhitepopE - 1)*100,
            AmerInd = (nhamerindpopE/nhwhitepopE - 1)*100,
            `Native PI/HI` = (nhnativhpopE/nhwhitepopE - 1)*100,
            Other = (nhotherpopE/nhwhitepopE - 1)*100,
            Multiracial = (nhnativhpopE/nhwhitepopE - 1)*100,
            Hispanic = (hisppopE/nhwhitepopE - 1)*100) %>%
  gather(key = Group, value = Pct) %>% 
  ggplot(aes(x = reorder(Group, -Pct), y = Pct, fill = Group)) + 
  geom_bar(stat = "identity", position = "identity") +
  theme_minimal() +
  labs(x = "", y = "", title = expression(atop(paste("Population-Weighted ", PM[2.5], " Exposure by Race for New Hampshire"), "(relative to non-Hispanic White average)"))) + 
  theme(legend.position = 'none') +
  geom_text(aes(x = Group, y = Pct + 0.3 * sign(Pct), 
                label = paste0(round(Pct,2),"%")), 
            hjust = 0.5, size = 3,
            color=rgb(100,100,100, maxColorValue=255)) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  geom_hline(yintercept = 0)


# Pop Weighted avg of PM2.5 CHANGE 2011 - 2016 by RACE in NH relative to White average
ne_blkgrp_sf_DemoEJ %>% 
  as.data.frame() %>% 
  filter(STATE == "New Hampshire") %>%
  dplyr::select(totalpopE,
                nhwhitepopE,
                minorityE,
                nhblackpopE,
                nhamerindpopE,
                nhasianpopE,
                nhnativhpopE,
                nhotherpopE,
                nh2morepopE,
                hisppopE,
                povknownE,
                num2povE, 
                eng_hhE,
                eng_limitE,
                age25upE,
                lthsE, 
                allAgesE, 
                under5E, 
                over65E, 
                PM25_pctChange) %>% 
  gather(key = Group, value = Pop, totalpopE:over65E) %>% 
  group_by(Group) %>% 
  summarize(PM25wMean = weighted.mean(x = PM25_pctChange, 
                                      w = Pop, na.rm = TRUE),
            PM25Mean = mean(PM25_pctChange, na.rm = TRUE)) %>% 
  spread(key = Group, value = PM25wMean) %>% 
  transmute(Black = (nhblackpopE/nhwhitepopE - 1)*100,
            Asian = (nhasianpopE/nhwhitepopE - 1)*100,
            AmerInd = (nhamerindpopE/nhwhitepopE - 1)*100,
            `Native PI/HI` = (nhnativhpopE/nhwhitepopE - 1)*100,
            Other = (nhotherpopE/nhwhitepopE - 1)*100,
            Multiracial = (nhnativhpopE/nhwhitepopE - 1)*100,
            Hispanic = (hisppopE/nhwhitepopE - 1)*100) %>%
  gather(key = Group, value = Pct) %>% 
  ggplot(aes(x = reorder(Group, -Pct), y = Pct, fill = Group)) + 
  geom_bar(stat = "identity", position = "identity") +
  theme_minimal() +
  labs(x = "", y = "", title = expression(atop(paste("Population-Weighted ", PM[2.5], " Exposure CHANGE 2011 - 2016 by Race"), " for New Hampshire (relative to non-Hispanic White average)"))) + 
  theme(legend.position = 'none') +
  geom_text(aes(x = Group, y = Pct + 0.3 * sign(Pct), 
                label = paste0(round(Pct,2),"%")), 
            hjust = 0.5, size = 3,
            color=rgb(100,100,100, maxColorValue=255)) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  geom_hline(yintercept = 0)


# Pop Weighted avg of PM2.5 for all Groups in Rhode Island relative to RI average
ne_blkgrp_sf_DemoEJ %>% 
  as.data.frame() %>% 
  filter(STATE == "Rhode Island") %>%
  dplyr::select(totalpopE,
                nhwhitepopE,
                minorityE,
                nhblackpopE,
                nhamerindpopE,
                nhasianpopE,
                nhnativhpopE,
                nhotherpopE,
                nh2morepopE,
                hisppopE,
                povknownE,
                num2povE, 
                eng_hhE,
                eng_limitE,
                age25upE,
                lthsE, 
                allAgesE, 
                under5E, 
                over65E, 
                PM25_19) %>% 
  gather(key = Group, value = Pop, totalpopE:over65E) %>% 
  group_by(Group) %>% 
  summarize(PM25wMean = weighted.mean(x = PM25_19, w = Pop, na.rm = TRUE),
            PM25Mean = mean(PM25_19, na.rm = TRUE)) %>% 
  spread(key = Group, value = PM25wMean) %>% 
  transmute(Minority = (minorityE/PM25Mean - 1)*100,
            #Minority_NHW = (minorityE/nhwhitepopE - 1)*100,
            `Lang Isol` = (eng_limitE/PM25Mean - 1)*100,
            Poverty = (num2povE/PM25Mean - 1)*100,
            `No HS` = (lthsE/PM25Mean - 1)*100,
            `Under 5` = (under5E/PM25Mean - 1)*100,
            `Over 65` = (over65E/PM25Mean - 1)*100) %>%
  gather(key = Group, value = Pct) %>% 
  ggplot(aes(x = reorder(Group, -Pct), y = Pct, fill = Group)) + 
  geom_bar(stat = "identity", position = "identity") +
  theme_minimal() +
  labs(x = "", y = "", title = expression(paste("Population-Weighted ", PM[2.5], " Exposure (relative to RI average)"))) + 
  theme(legend.position = 'none') +
  geom_text(aes(x = Group, y = Pct + 0.2 * sign(Pct), 
                label = paste0(round(Pct,2),"%")), 
            hjust = 0.5, size = 3,
            color=rgb(100,100,100, maxColorValue=255)) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  geom_hline(yintercept = 0)


# Pop Weighted avg of PM2.5 CHANGE 2011 - 2016 for all Groups in Rhode Island relative to RI average
ne_blkgrp_sf_DemoEJ %>% 
  as.data.frame() %>% 
  filter(STATE == "Rhode Island") %>%
  dplyr::select(totalpopE,
                nhwhitepopE,
                minorityE,
                nhblackpopE,
                nhamerindpopE,
                nhasianpopE,
                nhnativhpopE,
                nhotherpopE,
                nh2morepopE,
                hisppopE,
                povknownE,
                num2povE, 
                eng_hhE,
                eng_limitE,
                age25upE,
                lthsE, 
                allAgesE, 
                under5E, 
                over65E, 
                PM25_pctChange) %>% 
  gather(key = Group, value = Pop, totalpopE:over65E) %>% 
  group_by(Group) %>% 
  summarize(PM25wMean = weighted.mean(x = PM25_pctChange, 
                                      w = Pop, na.rm = TRUE),
            PM25Mean = mean(PM25_pctChange, na.rm = TRUE)) %>% 
  spread(key = Group, value = PM25wMean) %>% 
  transmute(Minority = (minorityE/PM25Mean - 1)*100,
            #Minority_NHW = (minorityE/nhwhitepopE - 1)*100,
            `Lang Isol` = (eng_limitE/PM25Mean - 1)*100,
            Poverty = (num2povE/PM25Mean - 1)*100,
            `No HS` = (lthsE/PM25Mean - 1)*100,
            `Under 5` = (under5E/PM25Mean - 1)*100,
            `Over 65` = (over65E/PM25Mean - 1)*100) %>%
  gather(key = Group, value = Pct) %>% 
  ggplot(aes(x = reorder(Group, -Pct), y = Pct, fill = Group)) + 
  geom_bar(stat = "identity", position = "identity") +
  theme_minimal() +
  labs(x = "", y = "", title = expression(atop(paste("Population-Weighted ", PM[2.5], " Exposure CHANGE 2011 - 2016"), " (relative to RI average)"))) + 
  theme(legend.position = 'none') +
  geom_text(aes(x = Group, y = Pct + 0.2 * sign(Pct), 
                label = paste0(round(Pct,2),"%")), 
            hjust = 0.5, size = 3,
            color=rgb(100,100,100, maxColorValue=255)) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  geom_hline(yintercept = 0)


# Slope graph of change in pop-weighted PM2.5 exposure beteween 2011 and 2016
# Create df of 2019 actual values
PM19wAvgs_actualRI <- ne_blkgrp_sf_DemoEJ %>% 
  as.data.frame() %>% 
  filter(STATE == "Rhode Island") %>%
  dplyr::select(minorityE,
                num2povE,
                eng_limitE,
                lthsE,
                under5E,
                over65E,
                PM25_19) %>% 
  gather(key = Group, value = Pop, minorityE:over65E) %>% 
  group_by(Group) %>% 
  summarize(PM25wMean = weighted.mean(x = PM25_19, 
                                      w = Pop, na.rm = TRUE)) %>% 
  mutate(Group = case_when(
    Group == "minorityE" ~ "Minority",
    Group == "eng_limitE" ~ "Lang Isol",
    Group == "num2povE" ~ "Poverty",
    Group == "lthsE" ~ "No HS",
    Group == "under5E" ~ "Under 5",
    Group == "over65E" ~ "Over 65")) %>% 
  mutate(Year = 2016)
# Create df of 2015 values
PM15wAvgs_actualRI <- ne_blkgrp_sf_DemoEJ %>% 
  as.data.frame() %>% 
  filter(STATE == "Rhode Island") %>%
  dplyr::select(mins_15,
                lowinc_15,
                lingiso_15,
                lths_15,
                under5_15,
                over64_15,
                pm_15) %>% 
  gather(key = Group, value = Pop, mins_15:over64_15) %>% 
  group_by(Group) %>% 
  summarize(PM25wMean = weighted.mean(x = pm_15, 
                                      w = Pop, na.rm = TRUE)) %>%  
  mutate(Group = case_when(
    Group == "mins_15" ~ "Minority",
    Group == "lingiso_15" ~ "Lang Isol",
    Group == "lowinc_15" ~ "Poverty",
    Group == "lths_15" ~ "No HS",
    Group == "under5_15" ~ "Under 5",
    Group == "over64_15" ~ "Over 65")) %>% 
  mutate(Year = 2011)
# Create regional average benchmark
pm15NEavgRI <- ne_blkgrp_sf_DemoEJ %>% 
  as.data.frame() %>% 
  filter(STATE == "Rhode Island") %>% 
  summarize(mean(pm_15, na.rm = TRUE))
pm19NEavgRI <- ne_blkgrp_sf_DemoEJ %>% 
  as.data.frame() %>% 
  filter(STATE == "Rhode Island") %>% 
  summarize(mean(PM25_19, na.rm = TRUE))
pmRegionalAvg_actualRI <- data.frame(
  Group = c("REGIONAL AVG", "REGIONAL AVG"),
  PM25wMean = c(pm15NEavgRI[1,1],pm19NEavgRI[1,1]),
  Year = c(2011,2016)
)
#rbind tables
PM11_16wAvg_actualRI <- bind_rows(PM15wAvgs_actualRI,
                                  PM19wAvgs_actualRI,
                                  pmRegionalAvg_actual) %>% 
  mutate(Year = as.factor(Year),
         PM25wMean = round(PM25wMean,1))
# make slope graph
newggslopegraph(dataframe = PM11_16wAvg_actualRI, 
                Times = Year, 
                Measurement = PM25wMean, 
                Grouping = Group,
                Title = "Population-Weighted PM2.5 Exposure for RI",
                SubTitle = "in micrograms per cubic meter",
                Caption = NULL,
                LineColor = c("REGIONAL AVG" = "#000000",
                              "Lang Isol" = "#E69F00",
                              "Minority" = "#56B4E9",
                              "No HS" = "#009E73",
                              "Poverty" = "#F0E442",
                              "Under 5" = "#0072B2",
                              "Over 65" = "#D55E00"))


# Pop Weighted avg of PM2.5 by RACE in RI relative to White average
ne_blkgrp_sf_DemoEJ %>% 
  as.data.frame() %>% 
  filter(STATE == "Rhode Island") %>%
  dplyr::select(totalpopE,
                nhwhitepopE,
                minorityE,
                nhblackpopE,
                nhamerindpopE,
                nhasianpopE,
                nhnativhpopE,
                nhotherpopE,
                nh2morepopE,
                hisppopE,
                povknownE,
                num2povE, 
                eng_hhE,
                eng_limitE,
                age25upE,
                lthsE, 
                allAgesE, 
                under5E, 
                over65E, 
                PM25_19) %>% 
  gather(key = Group, value = Pop, totalpopE:over65E) %>% 
  group_by(Group) %>% 
  summarize(PM25wMean = weighted.mean(x = PM25_19, w = Pop, na.rm = TRUE),
            PM25Mean = mean(PM25_19, na.rm = TRUE)) %>% 
  spread(key = Group, value = PM25wMean) %>% 
  transmute(Black = (nhblackpopE/nhwhitepopE - 1)*100,
            Asian = (nhasianpopE/nhwhitepopE - 1)*100,
            AmerInd = (nhamerindpopE/nhwhitepopE - 1)*100,
            `Native PI/HI` = (nhnativhpopE/nhwhitepopE - 1)*100,
            Other = (nhotherpopE/nhwhitepopE - 1)*100,
            Multiracial = (nhnativhpopE/nhwhitepopE - 1)*100,
            Hispanic = (hisppopE/nhwhitepopE - 1)*100) %>%
  gather(key = Group, value = Pct) %>% 
  ggplot(aes(x = reorder(Group, -Pct), y = Pct, fill = Group)) + 
  geom_bar(stat = "identity", position = "identity") +
  theme_minimal() +
  labs(x = "", y = "", title = expression(atop(paste("Population-Weighted ", PM[2.5], " Exposure by Race for Rhode Island"), "(relative to non-Hispanic White average)"))) + 
  theme(legend.position = 'none') +
  geom_text(aes(x = Group, y = Pct + 0.3 * sign(Pct), 
                label = paste0(round(Pct,2),"%")), 
            hjust = 0.5, size = 3,
            color=rgb(100,100,100, maxColorValue=255)) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  geom_hline(yintercept = 0)


# Pop Weighted avg of PM2.5 CHANGE 2011 - 2016 by RACE in RI relative to White average
ne_blkgrp_sf_DemoEJ %>% 
  as.data.frame() %>% 
  filter(STATE == "Rhode Island") %>%
  dplyr::select(totalpopE,
                nhwhitepopE,
                minorityE,
                nhblackpopE,
                nhamerindpopE,
                nhasianpopE,
                nhnativhpopE,
                nhotherpopE,
                nh2morepopE,
                hisppopE,
                povknownE,
                num2povE, 
                eng_hhE,
                eng_limitE,
                age25upE,
                lthsE, 
                allAgesE, 
                under5E, 
                over65E, 
                PM25_pctChange) %>% 
  gather(key = Group, value = Pop, totalpopE:over65E) %>% 
  group_by(Group) %>% 
  summarize(PM25wMean = weighted.mean(x = PM25_pctChange, 
                                      w = Pop, na.rm = TRUE),
            PM25Mean = mean(PM25_pctChange, na.rm = TRUE)) %>% 
  spread(key = Group, value = PM25wMean) %>% 
  transmute(Black = (nhblackpopE/nhwhitepopE - 1)*100,
            Asian = (nhasianpopE/nhwhitepopE - 1)*100,
            AmerInd = (nhamerindpopE/nhwhitepopE - 1)*100,
            `Native PI/HI` = (nhnativhpopE/nhwhitepopE - 1)*100,
            Other = (nhotherpopE/nhwhitepopE - 1)*100,
            Multiracial = (nhnativhpopE/nhwhitepopE - 1)*100,
            Hispanic = (hisppopE/nhwhitepopE - 1)*100) %>%
  gather(key = Group, value = Pct) %>% 
  ggplot(aes(x = reorder(Group, -Pct), y = Pct, fill = Group)) + 
  geom_bar(stat = "identity", position = "identity") +
  theme_minimal() +
  labs(x = "", y = "", title = expression(atop(paste("Population-Weighted ", PM[2.5], " Exposure CHANGE 2011 - 2016 by Race"), " for Rhode Island (relative to non-Hispanic White average)"))) + 
  theme(legend.position = 'none') +
  geom_text(aes(x = Group, y = Pct + 0.3 * sign(Pct), 
                label = paste0(round(Pct,2),"%")), 
            hjust = 0.5, size = 3,
            color=rgb(100,100,100, maxColorValue=255)) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  geom_hline(yintercept = 0)


# # Pop Weighted avg of PM2.5 for Rhode Island EJ criteria in Rhode Island relative to RI average
# library(plyr)
ne_blkgrp_sf_DemoEJ %>% 
  as.data.frame() %>% 
  filter(STATE == "Rhode Island") %>%
  dplyr::select(totalpopE,
                RI_INCOME, 
                RI_MINORITY, 
                PM25_19) %>% 
  gather(key = Group, value = Criterion, RI_INCOME:RI_MINORITY) %>% 
  filter(Criterion != "NA") %>% 
  plyr::ddply("Group", summarize, 
              PM25wMean = weighted.mean(x = PM25_19, w = totalpopE, na.rm = TRUE)) %>%
  add_column(., !!! # append df with unequal length for global mean
               ne_blkgrp_sf_DemoEJ %>% 
               as.data.frame() %>% 
               filter(STATE == "Rhode Island") %>% 
               summarize(avgPM25 = mean(PM25_19, na.rm = TRUE))) %>% 
  mutate(pctDiff = (PM25wMean/avgPM25-1)*100) %>% 
  ggplot(aes(x = reorder(Group, -pctDiff), 
             y = pctDiff, fill = Group)) + 
  geom_bar(stat = "identity", position = "identity") +
  theme_minimal() +
  labs(x = "", y = "", 
       title = expression(paste("Population-Weighted ", PM[2.5], " Exposure for EJ Groups (relative to RI average)"))) + 
  theme(legend.position = 'none') +
  geom_text(aes(x = Group, y = pctDiff + 0.2 * sign(pctDiff), 
                label = paste0(round(pctDiff,2),"%")), 
            hjust = 0.5, size = 3,
            color=rgb(100,100,100, maxColorValue=255)) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  geom_hline(yintercept = 0) +
  scale_x_discrete(labels = c("Minority", "Income"))


# # Pop Weighted avg of PM2.5 CHANGE 2011 - 2016 for Rhode Island EJ criteria in Rhode Island relative to RI average
# library(plyr)
ne_blkgrp_sf_DemoEJ %>% 
  as.data.frame() %>% 
  filter(STATE == "Rhode Island") %>%
  dplyr::select(totalpopE,
                RI_INCOME, 
                RI_MINORITY, 
                PM25_pctChange) %>% 
  gather(key = Group, value = Criterion, RI_INCOME:RI_MINORITY) %>% 
  filter(Criterion != "NA") %>% 
  plyr::ddply("Group", summarize, 
              PM25wMean = weighted.mean(x = PM25_pctChange, 
                                        w = totalpopE, na.rm = TRUE)) %>%
  add_column(., !!! # append df with unequal length for global mean
               ne_blkgrp_sf_DemoEJ %>% 
               as.data.frame() %>% 
               filter(STATE == "Rhode Island") %>% 
               summarize(avgPM25 = mean(PM25_pctChange, na.rm = TRUE))) %>% 
  mutate(pctDiff = (PM25wMean/avgPM25-1)*100) %>% 
  ggplot(aes(x = reorder(Group, -pctDiff), 
             y = pctDiff, fill = Group)) + 
  geom_bar(stat = "identity", position = "identity") +
  theme_minimal() +
  labs(x = "", y = "", 
       title = expression(atop(paste("Population-Weighted ", PM[2.5], " Exposure CHANGE 2011 - 2016 for EJ Groups"), " (relative to RI average)"))) + 
  theme(legend.position = 'none') +
  geom_text(aes(x = Group, y = pctDiff + 0.2 * sign(pctDiff), 
                label = paste0(round(pctDiff,2),"%")), 
            hjust = 0.5, size = 3,
            color=rgb(100,100,100, maxColorValue=255)) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  geom_hline(yintercept = 0) +
  scale_x_discrete(labels = c("Minority", "Income"))


# Pop Weighted avg of PM2.5 for all Groups in Vermont relative to VT average
ne_blkgrp_sf_DemoEJ %>% 
  as.data.frame() %>% 
  filter(STATE == "Vermont") %>%
  dplyr::select(totalpopE,
                nhwhitepopE,
                minorityE,
                nhblackpopE,
                nhamerindpopE,
                nhasianpopE,
                nhnativhpopE,
                nhotherpopE,
                nh2morepopE,
                hisppopE,
                povknownE,
                num2povE, 
                eng_hhE,
                eng_limitE,
                age25upE,
                lthsE, 
                allAgesE, 
                under5E, 
                over65E, 
                PM25_19) %>% 
  gather(key = Group, value = Pop, totalpopE:over65E) %>% 
  group_by(Group) %>% 
  summarize(PM25wMean = weighted.mean(x = PM25_19, w = Pop, na.rm = TRUE),
            PM25Mean = mean(PM25_19, na.rm = TRUE)) %>% 
  spread(key = Group, value = PM25wMean) %>% 
  transmute(Minority = (minorityE/PM25Mean - 1)*100,
            #Minority_NHW = (minorityE/nhwhitepopE - 1)*100,
            `Lang Isol` = (eng_limitE/PM25Mean - 1)*100,
            Poverty = (num2povE/PM25Mean - 1)*100,
            `No HS` = (lthsE/PM25Mean - 1)*100,
            `Under 5` = (under5E/PM25Mean - 1)*100,
            `Over 65` = (over65E/PM25Mean - 1)*100) %>%
  gather(key = Group, value = Pct) %>% 
  ggplot(aes(x = reorder(Group, -Pct), y = Pct, fill = Group)) + 
  geom_bar(stat = "identity", position = "identity") +
  theme_minimal() +
  labs(x = "", y = "", title = expression(paste("Population-Weighted ", PM[2.5], " Exposure (relative to VT average)"))) + 
  theme(legend.position = 'none') +
  geom_text(aes(x = Group, y = Pct + 0.2 * sign(Pct), 
                label = paste0(round(Pct,2),"%")), 
            hjust = 0.5, size = 3,
            color=rgb(100,100,100, maxColorValue=255)) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  geom_hline(yintercept = 0)


# Pop Weighted avg of PM2.5 CHANGE 2011 - 2016 for all Groups in Vermont relative to VT average
ne_blkgrp_sf_DemoEJ %>% 
  as.data.frame() %>% 
  filter(STATE == "Vermont") %>%
  dplyr::select(totalpopE,
                nhwhitepopE,
                minorityE,
                nhblackpopE,
                nhamerindpopE,
                nhasianpopE,
                nhnativhpopE,
                nhotherpopE,
                nh2morepopE,
                hisppopE,
                povknownE,
                num2povE, 
                eng_hhE,
                eng_limitE,
                age25upE,
                lthsE, 
                allAgesE, 
                under5E, 
                over65E, 
                PM25_pctChange) %>% 
  gather(key = Group, value = Pop, totalpopE:over65E) %>% 
  group_by(Group) %>% 
  summarize(PM25wMean = weighted.mean(x = PM25_pctChange, 
                                      w = Pop, na.rm = TRUE),
            PM25Mean = mean(PM25_pctChange, na.rm = TRUE)) %>% 
  spread(key = Group, value = PM25wMean) %>% 
  transmute(Minority = (minorityE/PM25Mean - 1)*100,
            #Minority_NHW = (minorityE/nhwhitepopE - 1)*100,
            `Lang Isol` = (eng_limitE/PM25Mean - 1)*100,
            Poverty = (num2povE/PM25Mean - 1)*100,
            `No HS` = (lthsE/PM25Mean - 1)*100,
            `Under 5` = (under5E/PM25Mean - 1)*100,
            `Over 65` = (over65E/PM25Mean - 1)*100) %>%
  gather(key = Group, value = Pct) %>% 
  ggplot(aes(x = reorder(Group, -Pct), y = Pct, fill = Group)) + 
  geom_bar(stat = "identity", position = "identity") +
  theme_minimal() +
  labs(x = "", y = "", title = expression(atop(paste("Population-Weighted ", PM[2.5], " Exposure CHANGE 2011 - 2016"), " (relative to VT average)"))) + 
  theme(legend.position = 'none') +
  geom_text(aes(x = Group, y = Pct + 0.2 * sign(Pct), 
                label = paste0(round(Pct,2),"%")), 
            hjust = 0.5, size = 3,
            color=rgb(100,100,100, maxColorValue=255)) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  geom_hline(yintercept = 0)


# Slope graph of change in pop-weighted PM2.5 exposure beteween 2011 and 2016
# Create df of 2019 actual values
PM19wAvgs_actualVT <- ne_blkgrp_sf_DemoEJ %>% 
  as.data.frame() %>% 
  filter(STATE == "Vermont") %>%
  dplyr::select(minorityE,
                num2povE,
                eng_limitE,
                lthsE,
                under5E,
                over65E,
                PM25_19) %>% 
  gather(key = Group, value = Pop, minorityE:over65E) %>% 
  group_by(Group) %>% 
  summarize(PM25wMean = weighted.mean(x = PM25_19, 
                                      w = Pop, na.rm = TRUE)) %>% 
  mutate(Group = case_when(
    Group == "minorityE" ~ "Minority",
    Group == "eng_limitE" ~ "Lang Isol",
    Group == "num2povE" ~ "Poverty",
    Group == "lthsE" ~ "No HS",
    Group == "under5E" ~ "Under 5",
    Group == "over65E" ~ "Over 65")) %>% 
  mutate(Year = 2016)
# Create df of 2015 values
PM15wAvgs_actualVT <- ne_blkgrp_sf_DemoEJ %>% 
  as.data.frame() %>% 
  filter(STATE == "Vermont") %>%
  dplyr::select(mins_15,
                lowinc_15,
                lingiso_15,
                lths_15,
                under5_15,
                over64_15,
                pm_15) %>% 
  gather(key = Group, value = Pop, mins_15:over64_15) %>% 
  group_by(Group) %>% 
  summarize(PM25wMean = weighted.mean(x = pm_15, 
                                      w = Pop, na.rm = TRUE)) %>%  
  mutate(Group = case_when(
    Group == "mins_15" ~ "Minority",
    Group == "lingiso_15" ~ "Lang Isol",
    Group == "lowinc_15" ~ "Poverty",
    Group == "lths_15" ~ "No HS",
    Group == "under5_15" ~ "Under 5",
    Group == "over64_15" ~ "Over 65")) %>% 
  mutate(Year = 2011)
# Create regional average benchmark
pm15NEavgVT <- ne_blkgrp_sf_DemoEJ %>% 
  as.data.frame() %>% 
  filter(STATE == "Vermont") %>% 
  summarize(mean(pm_15, na.rm = TRUE))
pm19NEavgVT <- ne_blkgrp_sf_DemoEJ %>% 
  as.data.frame() %>% 
  filter(STATE == "Vermont") %>% 
  summarize(mean(PM25_19, na.rm = TRUE))
pmRegionalAvg_actualVT <- data.frame(
  Group = c("REGIONAL AVG", "REGIONAL AVG"),
  PM25wMean = c(pm15NEavgVT[1,1],pm19NEavgVT[1,1]),
  Year = c(2011,2016)
)
#rbind tables
PM11_16wAvg_actualVT <- bind_rows(PM15wAvgs_actualVT,
                                  PM19wAvgs_actualVT,
                                  pmRegionalAvg_actual) %>% 
  mutate(Year = as.factor(Year),
         PM25wMean = round(PM25wMean,1))
# make slope graph
newggslopegraph(dataframe = PM11_16wAvg_actualVT, 
                Times = Year, 
                Measurement = PM25wMean, 
                Grouping = Group,
                Title = "Population-Weighted PM2.5 Exposure for VT",
                SubTitle = "in micrograms per cubic meter",
                Caption = NULL,
                LineColor = c("REGIONAL AVG" = "#000000",
                              "Lang Isol" = "#E69F00",
                              "Minority" = "#56B4E9",
                              "No HS" = "#009E73",
                              "Poverty" = "#F0E442",
                              "Under 5" = "#0072B2",
                              "Over 65" = "#D55E00"))
# clean up
rm(list = ls(pattern = "pmRegion|PM15|pm15|PM19|pm19|PM11"))

# Pop Weighted avg of PM2.5 by RACE in RI relative to White average
ne_blkgrp_sf_DemoEJ %>% 
  as.data.frame() %>% 
  filter(STATE == "Vermont") %>%
  dplyr::select(totalpopE,
                nhwhitepopE,
                minorityE,
                nhblackpopE,
                nhamerindpopE,
                nhasianpopE,
                nhnativhpopE,
                nhotherpopE,
                nh2morepopE,
                hisppopE,
                povknownE,
                num2povE, 
                eng_hhE,
                eng_limitE,
                age25upE,
                lthsE, 
                allAgesE, 
                under5E, 
                over65E, 
                PM25_19) %>% 
  gather(key = Group, value = Pop, totalpopE:over65E) %>% 
  group_by(Group) %>% 
  summarize(PM25wMean = weighted.mean(x = PM25_19, w = Pop, na.rm = TRUE),
            PM25Mean = mean(PM25_19, na.rm = TRUE)) %>% 
  spread(key = Group, value = PM25wMean) %>% 
  transmute(Black = (nhblackpopE/nhwhitepopE - 1)*100,
            Asian = (nhasianpopE/nhwhitepopE - 1)*100,
            AmerInd = (nhamerindpopE/nhwhitepopE - 1)*100,
            `Native PI/HI` = (nhnativhpopE/nhwhitepopE - 1)*100,
            Other = (nhotherpopE/nhwhitepopE - 1)*100,
            Multiracial = (nhnativhpopE/nhwhitepopE - 1)*100,
            Hispanic = (hisppopE/nhwhitepopE - 1)*100) %>%
  gather(key = Group, value = Pct) %>% 
  ggplot(aes(x = reorder(Group, -Pct), y = Pct, fill = Group)) + 
  geom_bar(stat = "identity", position = "identity") +
  theme_minimal() +
  labs(x = "", y = "", title = expression(atop(paste("Population-Weighted ", PM[2.5], " Exposure by Race for Vermont"), "(relative to non-Hispanic White average)"))) + 
  theme(legend.position = 'none') +
  geom_text(aes(x = Group, y = Pct + 0.3 * sign(Pct), 
                label = paste0(round(Pct,2),"%")), 
            hjust = 0.5, size = 3,
            color=rgb(100,100,100, maxColorValue=255)) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  geom_hline(yintercept = 0)


# Pop Weighted avg of PM2.5 CHANGE 2011 - 2016 by RACE in RI relative to White average
ne_blkgrp_sf_DemoEJ %>% 
  as.data.frame() %>% 
  filter(STATE == "Vermont") %>%
  dplyr::select(totalpopE,
                nhwhitepopE,
                minorityE,
                nhblackpopE,
                nhamerindpopE,
                nhasianpopE,
                nhnativhpopE,
                nhotherpopE,
                nh2morepopE,
                hisppopE,
                povknownE,
                num2povE, 
                eng_hhE,
                eng_limitE,
                age25upE,
                lthsE, 
                allAgesE, 
                under5E, 
                over65E, 
                PM25_pctChange) %>% 
  gather(key = Group, value = Pop, totalpopE:over65E) %>% 
  group_by(Group) %>% 
  summarize(PM25wMean = weighted.mean(x = PM25_pctChange, 
                                      w = Pop, na.rm = TRUE),
            PM25Mean = mean(PM25_pctChange, na.rm = TRUE)) %>% 
  spread(key = Group, value = PM25wMean) %>% 
  transmute(Black = (nhblackpopE/nhwhitepopE - 1)*100,
            Asian = (nhasianpopE/nhwhitepopE - 1)*100,
            AmerInd = (nhamerindpopE/nhwhitepopE - 1)*100,
            `Native PI/HI` = (nhnativhpopE/nhwhitepopE - 1)*100,
            Other = (nhotherpopE/nhwhitepopE - 1)*100,
            Multiracial = (nhnativhpopE/nhwhitepopE - 1)*100,
            Hispanic = (hisppopE/nhwhitepopE - 1)*100) %>%
  gather(key = Group, value = Pct) %>% 
  ggplot(aes(x = reorder(Group, -Pct), y = Pct, fill = Group)) + 
  geom_bar(stat = "identity", position = "identity") +
  theme_minimal() +
  labs(x = "", y = "", title = expression(atop(paste("Population-Weighted ", PM[2.5], " Exposure CHANGE 2011 - 2016 by Race"), " for Vermont (relative to non-Hispanic White average)"))) + 
  theme(legend.position = 'none') +
  geom_text(aes(x = Group, y = Pct + 0.3 * sign(Pct), 
                label = paste0(round(Pct,2),"%")), 
            hjust = 0.5, size = 3,
            color=rgb(100,100,100, maxColorValue=255)) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  geom_hline(yintercept = 0)


# # Pop Weighted avg of PM2.5 for VT_INCOME in Connecticut relative to VT average at TOWN level
ne_towns_sf_EJ %>% 
  as.data.frame() %>% 
  filter(STATE == "Vermont") %>%
  dplyr::select(totalpopE,
                VT_INCOME, 
                VT_INCOME_UC, 
                VT_INCOME_LC, 
                PM25_19) %>% 
  group_by(VT_INCOME) %>% 
  summarize(PM25wMean = weighted.mean(x = PM25_19, 
                                      w = totalpopE, na.rm = TRUE)) %>% 
  add_column(., !!! # append df with unequal length for global mean
               ne_towns_sf_EJ %>% 
               as.data.frame() %>% 
               filter(STATE == "Vermont") %>% 
               summarize(avgPM25 = mean(PM25_19, na.rm = TRUE))
  ) %>% 
  mutate(pctDiff = (PM25wMean/avgPM25-1)*100) %>% 
  ggplot(aes(x = reorder(VT_INCOME, -pctDiff), 
             y = pctDiff, fill = VT_INCOME)) + 
  geom_bar(stat = "identity", position = "identity") +
  theme_minimal() +
  labs(x = "", y = "", 
       title = expression(paste("Population-Weighted ", PM[2.5], " Exposure (relative to VT average)"))) + 
  theme(legend.position = 'none') +
  geom_text(aes(x = VT_INCOME, y = pctDiff + 0.2 * sign(pctDiff), 
                label = paste0(round(pctDiff,2),"%")), 
            hjust = 0.5, size = 3,
            color=rgb(100,100,100, maxColorValue=255)) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  geom_hline(yintercept = 0) +
  scale_x_discrete(labels = c("Low Income", "Not Low Income"))


# # Pop Weighted avg of PM2.5 CHANGE 2011 - 2016 for VT_INCOME in Connecticut relative to VT average at TOWN level
ne_towns_sf_EJ %>% 
  as.data.frame() %>% 
  filter(STATE == "Vermont") %>%
  dplyr::select(totalpopE,
                VT_INCOME, 
                VT_INCOME_UC, 
                VT_INCOME_LC, 
                PM25_pctChange) %>% 
  group_by(VT_INCOME) %>% 
  summarize(PM25wMean = weighted.mean(x = PM25_pctChange, 
                                      w = totalpopE, na.rm = TRUE)) %>% 
  add_column(., !!! # append df with unequal length for global mean
               ne_towns_sf_EJ %>% 
               as.data.frame() %>% 
               filter(STATE == "Vermont") %>% 
               summarize(avgPM25 = mean(PM25_pctChange, na.rm = TRUE))
  ) %>% 
  mutate(pctDiff = (PM25wMean/avgPM25-1)*100) %>% 
  ggplot(aes(x = reorder(VT_INCOME, -pctDiff), 
             y = pctDiff, fill = VT_INCOME)) + 
  geom_bar(stat = "identity", position = "identity") +
  theme_minimal() +
  labs(x = "", y = "", 
       title = expression(atop(paste("Population-Weighted ", PM[2.5], " Exposure CHANGE 2011 - 2016"), "(relative to VT average)"))) + 
  theme(legend.position = 'none') +
  geom_text(aes(x = VT_INCOME, y = pctDiff + 0.2 * sign(pctDiff), 
                label = paste0(round(pctDiff,2),"%")), 
            hjust = 0.5, size = 3,
            color=rgb(100,100,100, maxColorValue=255)) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  geom_hline(yintercept = 0) +
  scale_x_discrete(labels = c("Low Income", "Not Low Income"))


# T-TEST OF DIFFERENCES FOR EJ INDICES; BOXPLOTS WITH NOTCHES

# HOT SPOT MAP OF POLLUTION AND PCT CHANGE


# Isolate select air pollution related variables and join to sf block groups
EJSCREEN19.air_sf <- EJSCREEN %>% 
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
