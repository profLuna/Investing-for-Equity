# Demographics from 1990 decennial census

library(tidyverse)
library(tmap)
library(sf)
library(areal) # for areal weighted interpolation

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

# read in 1990 block group shapefiles for NE states and rbind into one sf
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
  st_make_valid()

# Join demographics to geometry
ne_blkgrp_sf90 <- ne_blkgrp_sf90 %>% 
  left_join(., ne_blkgrp_stf1, by = "GISJOIN") %>% 
  left_join(., ne_blkgrp_stf3, by = "GISJOIN")

# check for missing data
sum(is.na(ne_blkgrp_sf90$totalpop))
# tmap_mode("view")
# tm_shape(ne_blkgrp_sf90) + tm_fill("totalpop", alpha = 0.3) + tm_borders()


# Use areal weighted interpolation to assign CO2 values to 1990 block groups from 2017 block groups on which onroadCO2 is based.  
# First, transform to projected CRS; Albers Equal Area
ne_blkgrp_sf_aea <- ne_blkgrp_sf %>% 
  select(GEOID,kgco2_1990) %>% 
  st_transform(., crs = 2163)
ne_blkgrp_sf90_aea <- st_transform(ne_blkgrp_sf90, crs = 2163)
# Execute areal weighted interpolation
ne_blkgrp_df90_CO2 <- ne_blkgrp_sf90_aea %>% 
  select(GISJOIN) %>% 
  aw_interpolate(.,
                 tid = GISJOIN,
                 source = ne_blkgrp_sf_aea,
                 sid = GEOID,
                 weight = "sum",
                 output = "tibble",
                 extensive = "kgco2_1990")

# check for NAs
sum(is.na(ne_blkgrp_df90_CO2$kgco2_1990))

# join CO2 values to 1990 block groups
ne_blkgrp_sf90 <- left_join(ne_blkgrp_sf90, ne_blkgrp_df90_CO2, by="GISJOIN")

# clean up
rm(list = ls(pattern = "stf|aea|df90"))

# load("DATA/ne_layers.rds")

# save with other files
save(ne_blkgrp_sf,
     ne_blkgrp_sf90,
     ne_tracts_sf,
     ne_towns_sf, 
     ne_towns_sf_pts,
     ne_states_sf_cb,
     ne_states_df,
     ne_states,
     file = "DATA/ne_layers.rds")
