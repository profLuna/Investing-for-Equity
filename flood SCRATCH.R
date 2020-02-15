# read in National Land Cover Database raster data
nlcd <- raster("DATA/NLCD_2016_Land_Cover_L48_20190424.img") 

# create polygon layer in same crs as raster to crop
ri_polys <- st_transform(ri_blkgrp_2840, crs = projection(nlcd))

# create a function to reclassify pixels that are not 'developed' as NA
rc <- function(x) {
  ifelse(x >= 21 & x <= 24, 1, NA)
}

# Define the Proj.4 spatial reference 
# http://spatialreference.org/ref/epsg/2840/proj4/
newCRS <- "+proj=tmerc +lat_0=41.08333333333334 +lon_0=-71.5 +k=0.99999375 +x_0=100000 +y_0=0 +ellps=GRS80 +units=m +no_defs" 

# crop raster to region and reproject to state plane
start_time <- Sys.time()
ri_nlcd_2840 <- crop(nlcd, ri_polys) %>% 
  calc(., fun=rc) %>% 
  projectRaster(., crs = newCRS)
end_time <- Sys.time()
end_time - start_time 

# count number of NLCD cells per polygon and calc pop per cell
start_time <- Sys.time()
ri_blkgrp_cnt <- ri_blkgrp_2840 %>% 
  dplyr::select(GEOID, totalpopE) %>% 
  as_Spatial() %>% 
  extract(ri_nlcd_2840, ., 
          fun = function(x, ...) length(na.omit(x)), 
          sp=TRUE, na.rm=TRUE, small=TRUE) %>% 
  st_as_sf() %>% 
  rename(cells = layer) %>% 
  mutate(popCell = totalpopE/cells)
end_time <- Sys.time()
end_time - start_time

# convert block group polygons to raster and mask out NA values
library(fasterize)
start_time <- Sys.time()
ri_blkgrp_ras <- fasterize(sf = ri_blkgrp_cnt, raster = ri_nlcd_2840, 
                           field = "popCell", fun = "first") %>% 
  mask(., ri_nlcd_2840)
end_time <- Sys.time()
end_time - start_time

# extract sum of population within flood zones
start_time <- Sys.time()
ri_fhza_pop <- RI_FHZA_2840 %>% 
  as_Spatial() %>% 
  extract(ri_blkgrp_ras, ., fun = sum,
                       sp = TRUE, na.rm = TRUE, small = TRUE) %>% 
  st_as_sf() %>% 
  mutate(pop = layer)
end_time <- Sys.time()
end_time - start_time

# get stats of population within flood zones
ri_fhza_pop %>% 
  as.data.frame() %>% 
  group_by(FLD_ZONE) %>% 
  summarize(sum(layer))

# map it out
ri_fhza_pop %>% 
  # filter(FLD_ZONE == "X") %>% 
  tm_shape(ri_blkgrp_ras) + tm_raster(alpha = 0.8) +
  tm_shape(ri_fhza_pop) + tm_borders(col = "blue")



# SEE THE EFFECT OF FURTHER SCREENING WATER AND OS FROM BGs
start_time <- Sys.time()
ri_blkrps_fix <- st_difference(ri_blkgrp_2840,ri_awater_sf) %>% 
  st_difference(., ri_localCon_sf) %>% 
  st_difference(., ri_stateCon_sf)
end_time <- Sys.time()
end_time - start_time # takes about 7.6 mins

# create polygon layer in same crs as raster to crop
ri_polys_fix <- st_transform(ri_blkrps_fix, crs = projection(nlcd))

# crop raster to region and reproject to state plane
start_time <- Sys.time()
ri_nlcd_2840_fix <- crop(nlcd, ri_polys_fix) %>% 
  calc(., fun=rc) %>% 
  projectRaster(., crs = newCRS)
end_time <- Sys.time()
end_time - start_time 

# count number of NLCD cells per polygon and calc pop per cell
start_time <- Sys.time()
ri_blkgrp_cnt_fix <- ri_blkrps_fix %>% 
  dplyr::select(GEOID, totalpopE) %>% 
  as_Spatial() %>% 
  extract(ri_nlcd_2840_fix, ., 
          fun = function(x, ...) length(na.omit(x)), 
          sp=TRUE, na.rm=TRUE, small=TRUE) %>% 
  st_as_sf() %>% 
  rename(cells = layer) %>% 
  mutate(popCell = totalpopE/cells)
end_time <- Sys.time()
end_time - start_time # takes about 3.6 mins

# convert block group polygons to raster and mask out NA values
library(fasterize)
start_time <- Sys.time()
ri_blkgrp_ras_fix <- fasterize(sf = ri_blkgrp_cnt_fix, 
                               raster = ri_nlcd_2840_fix, 
                           field = "popCell", 
                           fun = "first") %>% 
  mask(., ri_nlcd_2840_fix)
end_time <- Sys.time()
end_time - start_time

# extract sum of population within flood zones
start_time <- Sys.time()
ri_fhza_pop_fix <- RI_FHZA_2840 %>% 
  as_Spatial() %>% 
  extract(ri_blkgrp_ras_fix, ., fun = sum,
          sp = TRUE, na.rm = TRUE, small = TRUE) %>% 
  st_as_sf() %>% 
  mutate(pop = layer)
end_time <- Sys.time()
end_time - start_time # taks about 4.7 mins

# get stats of population within flood zones
ri_fhza_pop_fix %>% 
  as.data.frame() %>% 
  group_by(FLD_ZONE) %>% 
  summarize(sum(layer))

# map it out
ri_fhza_pop %>% 
  # filter(FLD_ZONE == "X") %>% 
  tm_shape(ri_blkgrp_ras_fix) + tm_raster(alpha = 0.8) +
  tm_shape(ri_fhza_pop_fix) + tm_borders(col = "blue")