# Demographics from 1990 decennial census

library(tidyverse)
library(tmap)
library(sf)

# Download data from NHGIS.
# Documentation for NHGIS datasets is available at https://www.nhgis.org/documentation.
# Research using NHGIS data should cite it as:
#   Steven Manson, Jonathan Schroeder, David Van Riper, and Steven Ruggles. IPUMS National Historical Geographic Information System: Version 14.0 [Database]. Minneapolis, MN: IPUMS. 2019. http://doi.org/10.18128/D050.V14.0
# For policy briefs or articles in the popular press, we recommend that you cite the use of NHGIS data as follows:
#   IPUMS NHGIS, University of Minnesota, www.nhgis.org

# read in tabular data
ne_blkgrp_stf1 <- read_csv("DATA/nhgis0001_csv/nhgis0001_ds120_1990_blck_grp.csv") %>% 
  transmute(GISJOIN = GISJOIN,
            YEAR = YEAR,
            COUNTY = COUNTY,
            STATE = STATE,
            totalpop = ET1001,
            households = EUO001,
            nhwhite = ET2001,
            nhblack = ET2002,
            nhamerind = ET2003,
            nhasian = ET2004,
            nhother = ET2005,
            hispanic = ET2006 + ET2007 + ET2008 + ET2009 + ET2010,
            minority = nhblack + nhamerind + nhasian + nhother + hispanic,
            under5 = ET3001 + ET3002 + ET3003,
            over64 = ET3027 + ET3028 + ET3029 + ET3030 + ET3031)

ne_blkgrp_stf3 <- read_csv("DATA/nhgis0001_csv/nhgis0003_ds123_1990_blck_grp.csv") %>% 
  transmute(GISJOIN = GISJOIN,
            langIsolHH = E27002 + E27004 + E27006,
            noHSdip = E33001 + E33002,
            lowInc = E1C001 + E1C002 + E1C003 + E1C004 + E1C005 + 
              E1C006 + E1C007 + E1C008)

# read in 1990 block group shapefiles and rbind into one sf
ne_blkgrp_sf90 <- list.files(path = "DATA/shapefiles/nhgis0001_shape",
                             pattern = "*.shp$") %>% 
  tools::file_path_sans_ext() %>% 
  lapply(., function(x){
           st_read(dsn = "DATA/shapefiles/nhgis0001_shape",
                   layer = x) %>% 
             mutate(GISJOIN = as.character(GISJOIN),
                    GISJOIN2 = as.character(GISJOIN2))
         }) %>%
  do.call(rbind, .) %>% 
  lwgeom::st_make_valid()

# Join demographics to geometry
ne_blkgrp_sf90 <- ne_blkgrp_sf90 %>% 
  left_join(., ne_blkgrp_stf1, by = "GISJOIN") %>% 
  left_join(., ne_blkgrp_stf3, by = "GISJOIN")

# check for missing data
sum(is.na(ne_blkgrp_sf90$totalpop))
tmap_mode("view")
tm_shape(ne_blkgrp_sf90) + tm_fill("totalpop", alpha = 0.3) + tm_borders()




# Compute CO2 by block group
# Read in DARTE Annual On-road CO2 Emissions on a 1-km Grid. See https://daac.ornl.gov/cgi-bin/dsviewer.pl?ds_id=1735 
# view relevant tif tags embedded within a geotiff before openning it
# GDALinfo("traffic/onroad_2017.tif")
# load the data
CO2_1990 <- raster("DATA/DARTE/onroad_1990.tif")

# Crop to New England
# Create copy of ne_blkgrp_sf with same CRS
ne_blkgrps_sf90_lcc <- st_transform(ne_blkgrps_sf90, proj4string(CO2_1990))

# Crop raster to New England and convert kilograms/km2 to metric tons/km2
CO2_1990ne_tons <- crop(CO2_1990, ne_blkgrps_sf90_lcc) %>% 
  `/`(1000)
# clean up
rm(CO2_1990)

# To extract raster values, need to first address empty geometries in polygon layer
# check for empty geometries
any(is.na(st_dimension(ne_blkgrps_sf90_lcc)))
# identiy empty geometries
# empty_geo <- st_is_empty(ne_blkgrp_sf_lcc)
# # filter out empty geometries
# ne_blkgrp_sf_lcc <- ne_blkgrp_sf_lcc[!empty_geo,]
# # clean up
# rm(empty_geo)

# Extract mean CO2 values within each block group to an spdf
meanCO2_90 <- extract(CO2_1990ne_tons, as_Spatial(ne_blkgrps_sf90_lcc), 
                      fun=mean, sp=TRUE, na.rm=TRUE, small=TRUE)

# Convert to sf
ne_blkgrps_sf90_CO2 <- st_as_sf(meanCO2_90)

# clean up
rm(CO2_1990ne_tons, meanCO2_90, meanCO2_90sf, ne_blkgrp_90, ne_blkgrps_sf90, ne_blkgrps_sf90_lcc)

# save with other files
save(ne_blkgrp_sf,
     ne_blkgrp_sf_DEMOG,
     ne_tracts_sf,
     ne_tracts_sf_DEMOG,
     ne_towns_sf, 
     ne_towns_sf_EJ,
     ne_towns_sf_pts,
     ne_states,
     ne_states_sf_cb,
     ne_blkgrp_sf_DemoEJ,
     ne_state_90,
     file = "DATA/ne_layers.rds")
