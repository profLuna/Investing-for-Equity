# Demographics from 1990 decennial census

library(tidycensus)
library(tidyverse)
library(tmap)
library(sf)
library(tigris)
options(tigris_use_cache = TRUE, tigris_class = "sf")
library(raster)

census_api_key("f2776fbc29cf847505de9308a82c8d65290d16b3")

# Load list of variables to identify tables of interest
v90 <- load_variables(1990, "sf3", cache = TRUE)

### CENSUS POLYGONS

# create vector of New England states
ne_states <- c("CT","MA","ME","NH","RI","VT")

# use purrr::reduce function in combination with sf::rbind to download block groups for list of states and then bind them to one sf. 
ne_blkgrp_90 <- reduce(
  map(ne_states, function(x) {
    get_decennial(geography = "block group",
                  sumfile = "sf3",
                  year = 1990,
            variables = c(totalpop = "P0010001", 
                          households = "P0050001",
                          hispanic = "P0100001",
                          nhwhite = "P0120001",
                          nhblack = "P0120002",
                          nhamerind = "P0120003",
                          nhasian = "P0120004",
                          nhother = "P0120005",
                          age_u1 = "P0130001",
                          age1_2 = "P0130002",
                          age3_4 = "P0130003",
                          age65_69 = "P0130027",
                          age70_74 = "P0130028",
                          age75_79 = "P0130029",
                          age80_84 = "P0130030",
                          age85 = "P0130031",
                          spanLangIsolHH = "P0290002",
                          asianLangIsolHH = "P0290004",
                          othrLangIsolHH = "P0290006",
                          edLt9th = "P0570001",
                          ed9_12 = "P0570002",
                          incTopovUndr.5 = "P1210001",
                          incTopov.5_.74 = "P1210002",
                          incTopov.75_.99 = "P1210003",
                          incTopov1_1.24 = "P1210004",
                          incTopov1.25_1.49 = "P1210005",
                          incTopov1.5_1.74 = "P1210006",
                          incTopov1.75_1.84 = "P1210007",
                          incTopov1.85_1.99 = "P1210008"),
            state = x, output = "wide")
  }),
  rbind
)


# use purrr::reduce function in combination with sf::rbind to download states for list of states and then bind them to one sf. 
ne_state_90 <- reduce(
  map(ne_states, function(x) {
    get_decennial(geography = "state",
                  sumfile = "sf3",
                  year = 1990,
                  variables = c(totalpop = "P0010001", 
                                households = "P0050001",
                                hispanic = "P0100001",
                                nhwhite = "P0120001",
                                nhblack = "P0120002",
                                nhamerind = "P0120003",
                                nhasian = "P0120004",
                                nhother = "P0120005",
                                age_u1 = "P0130001",
                                age1_2 = "P0130002",
                                age3_4 = "P0130003",
                                age65_69 = "P0130027",
                                age70_74 = "P0130028",
                                age75_79 = "P0130029",
                                age80_84 = "P0130030",
                                age85 = "P0130031",
                                spanLangIsolHH = "P0290002",
                                asianLangIsolHH = "P0290004",
                                othrLangIsolHH = "P0290006",
                                edLt9th = "P0570001",
                                ed9_12 = "P0570002",
                                incTopovUndr.5 = "P1210001",
                                incTopov.5_.74 = "P1210002",
                                incTopov.75_.99 = "P1210003",
                                incTopov1_1.24 = "P1210004",
                                incTopov1.25_1.49 = "P1210005",
                                incTopov1.5_1.74 = "P1210006",
                                incTopov1.75_1.84 = "P1210007",
                                incTopov1.85_1.99 = "P1210008"),
                  state = x, output = "wide")
  }),
  rbind
)


# Compute estimates for populations of interest
ne_blkgrp_90 <- ne_blkgrp_90 %>% 
  mutate(minority = hispanic + 
           nhwhite + nhblack + nhasian + nhamerind + nhother,
         under5 = age_u1 + age1_2 + age3_4,
         over64 = age65_69 + age70_74 + age75_79 + age80_84 + age85,
         langisol = spanLangIsolHH + asianLangIsolHH + othrLangIsolHH,
         noHSdip = edLt9th  + ed9_12,
         lowInc = incTopovUndr.5 + incTopov.5_.74 + incTopov.75_.99 +
           incTopov1_1.24 + incTopov1.25_1.49 + incTopov1.25_1.49 +
           incTopov1.5_1.74 + incTopov1.75_1.84 + incTopov1.85_1.99)

# acquire the polygons for 1990 block groups using tigris::rbind_tigris
ne_blkgrps_sf90 <- rbind_tigris(
  lapply(
    ne_states, function(x){
      block_groups(state = x, cb = TRUE, year = 1990)
    }
  )
)

# join df to sf
ne_blkgrps_sf90b <- ne_blkgrps_sf90 %>% 
  dplyr::select(-NAME) %>%
  left_join(., ne_blkgrp_90, by = c("GEOID" = "GEOID"))

sum(is.na(ne_blkgrps_sf90b$totalpop))
rm(ne_blkgrps_sf90b)

# Add in state names and abbreviations based on STATEFP
ne_blkgrps_sf90 <- ne_blkgrps_sf90 %>% 
  mutate(STATE = case_when(
    STATEFP == "09" ~ "Connecticut",
    STATEFP == "23" ~ "Maine",
    STATEFP == "25" ~ "Massachusetts",
    STATEFP == "33" ~ "New Hampshire",
    STATEFP == "44" ~ "Rhode Island",
    STATEFP == "50" ~ "Vermont"),
  STABBR = case_when(
    STATEFP == "09" ~ "CT",
    STATEFP == "23" ~ "ME",
    STATEFP == "25" ~ "MA",
    STATEFP == "33" ~ "NH",
    STATEFP == "44" ~ "RI",
    STATEFP == "50" ~ "VT"))

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
