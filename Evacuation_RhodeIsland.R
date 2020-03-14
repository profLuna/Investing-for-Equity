# Analysis of flooding and hurricane evacuation risks for populations of concern in Rhode Island
library(tidyverse)
library(sf)
library(tmap)
library(tmaptools)
library(lwgeom)
library(tigris)
options(tigris_use_cache = TRUE, tigris_class = "sf")

load("DATA/ne_layers.rds")

# Download Census TIGERLine hydrography for RI
## First, extract list of county names to use with tigris::water
ri_counties <- counties("RI") %>% 
  pull(NAME)
# Next, download water features for each county and rbind to one layer
ri_awater_sf <- rbind_tigris(
  lapply(
    ri_counties, function(x) area_water(state = "RI", county = x)
  )
) %>% 
  st_union() %>% 
  st_as_sf() %>% 
  st_transform(., crs = 2840)

# Read in NFHL for RI. Data comes from FEMA. 
# List available layers in geodatabase
# st_layers("DATA/FEMA/RI/NFHL_44_20181118.gdb")
# Read in flood hazard areas
ri_fhza_2840 <- st_read(dsn = "DATA/FEMA/RI/NFHL_44_20181118.gdb", 
                        layer = "S_Fld_Haz_Ar") %>% 
  filter(FLD_ZONE != "OPEN WATER" & 
           !ZONE_SUBTY %in% c("AREA OF MINIMAL FLOOD HAZARD",
                              "AREA WITH REDUCED FLOOD RISK DUE TO LEVEE")) %>%
  mutate(FLD_ZONE = as.character(FLD_ZONE)) %>% # omit unused factor levels
  st_transform(., crs = 2840) %>% 
  st_make_valid() %>% 
  group_by(FLD_ZONE) %>% # aggregate flood zone polygons
  summarize(count = n()) %>% 
  mutate(Area = st_area(.),
         Interval = case_when(
           FLD_ZONE  == "A" ~ "100-year",
           FLD_ZONE  == "AE" ~ "100-year",
           FLD_ZONE  == "AH" ~ "100-year",
           FLD_ZONE  == "AO" ~ "100-year",
           FLD_ZONE  == "VE" ~ "100-year",
           FLD_ZONE == "X" ~ "500-year"))

# crop flood zones to land areas only
ri_state_sf <- ne_states_sf_cb %>% 
  filter(NAME == "Rhode Island")
ri_fhza_2840_land <- ri_fhza_2840 %>% 
  crop_shape(., ri_state_sf, polygon = TRUE) %>% 
  st_difference(., ri_awater_sf) %>% 
  mutate(Area = st_area(.))

# Percentage of RI land within flood zones
ri_area <- as.numeric(ri_state_sf$ALAND)

# Total and percentage of land area of RI within flood zones
ri_fhza_2840_land %>% 
  as.data.frame() %>% 
  group_by(Interval) %>% 
  summarize(SqKm = round(as.numeric(sum(Area)/10^6),1), 
            SqMi = round(as.numeric(SqKm/2.59),1),
            PctArea = paste0(as.character(round(as.numeric(sum(Area)/ri_area*100),1)),"%"))

# read in hurricane evacuation zone layer
ri_hea_sf <- st_read(dsn = "DATA/FEMA/RI", 
                     layer = "Hurricane_Evacuation_Areas") %>% 
  mutate(EVAC = as.character(EVAC)) %>% 
  filter(EVAC %in% c("A","B","C")) %>% 
  st_transform(., crs = 2840) %>% 
  st_make_valid() %>% 
  mutate(Area = st_area(.))

# Total and percentage of land area with hurricane evacuation zones
ri_hea_sf %>% 
  as.data.frame() %>% 
  group_by(EVAC) %>% 
  summarize(SqKm = round(as.numeric(sum(Area)/10^6),1), 
            SqMi = round(as.numeric(SqKm/2.59),1),
            PctArea = paste0(as.character(round(as.numeric(sum(Area)/ri_area*100),1)),"%"))

# Convert to projected local CRS EPSG:2840: NAD83(HARN) / Rhode Island
ri_blkgrp_2840 <- ne_blkgrp_sf %>% 
  filter(STATE == "Rhode Island") %>% 
  st_transform(., crs = 2840)

ri_tracts_2840 <- ne_tracts_sf %>% 
  filter(STATE == "Rhode Island") %>% 
  st_transform(., crs = 2840)

# Get rid of empty geometries
empty_geo <- st_is_empty(ri_fhza_2840)
ri_fhza_2840 <- ri_fhza_2840[!empty_geo,]
empty_geo <- st_is_empty(ri_blkgrp_2840)
ri_blkgrp_2840 <- ri_blkgrp_2840[!empty_geo,]
empty_geo <- st_is_empty(ri_tracts_2840)
ri_tracts_2840 <- ri_tracts_2840[!empty_geo,]


# Write out block groups for processing in ArcGIS
ri_blkgrp_2840 %>%
  dplyr::select(GEOID) %>%
  st_write(., "DATA/FEMA/RI/ri_blkgrp_2840.shp", delete_layer = TRUE)

# repeat for tracts
ri_tracts_2840 %>% 
  dplyr::select(GEOID) %>%
  st_write(., "DATA/FEMA/RI/ri_tracts_2840.shp", delete_layer = TRUE)

# Use dasymetric mapping to calculate populations within flood zones. Approach follows method used by Qiang (2019) to eliminate unpopulated areas of census polygons and then reallocate populations to developed areas as identified in National Land Cover Dataset (NLCD). 
# Perform NLCD raster-to-vector conversion, vector erase/difference, and vector intersections in ArcMap because it takes too long in R. 
# In ArcMap:
# Convert NLCD raster to shapefile. Isolate undeveloped areas. 
# Erase areas of ri_blkgrp_2840 and ri_tracts_2840 that overlap with undeveloped areas in NLCD shapefiles. Compute OldArea of erased polygons in sqm to identify area of developed polygons remaining. 
# Intersect erased ri_blkgrps and erased ri_tracts with NFHZA and Hurricane evacuation zones. Read back into R.

# read in processed ri_blkgrps and ri_tracts
st_layers(dsn = "DATA/FEMA/RI")

ri_blkgrps_nfhza <- st_read(dsn = "DATA/FEMA/RI", 
                            layer = "ri_blkgrps_nfhza") %>% 
  left_join(., as.data.frame(ri_blkgrp_2840), by = "GEOID") %>% 
  st_transform(., crs = 2840) %>% 
  mutate(NewArea = st_area(.)) %>%
  st_make_valid()

ri_blkgrps_hevac <- st_read(dsn = "DATA/FEMA/RI", 
                            layer = "ri_blkgrps_hevac") %>% 
  left_join(., as.data.frame(ri_blkgrp_2840), by = "GEOID") %>%
  st_transform(., crs = 2840) %>% 
  mutate(NewArea = st_area(.)) %>%
  st_make_valid()

ri_tracts_nfhza <- st_read(dsn = "DATA/FEMA/RI", 
                           layer = "ri_tracts_nfhza") %>% 
  left_join(., as.data.frame(ri_tracts_2840), by = "GEOID") %>% 
  st_transform(., crs = 2840) %>% 
  mutate(NewArea = st_area(.)) %>%
  st_make_valid()

ri_tracts_hevac <- st_read(dsn = "DATA/FEMA/RI", 
                           layer = "ri_tracts_hevac") %>% 
  left_join(., as.data.frame(ri_tracts_2840), by = "GEOID") %>%
  st_transform(., crs = 2840) %>% 
  mutate(NewArea = st_area(.)) %>% 
  st_make_valid()

# Apportion populations based on geographic proportion of intersect
ri_blkgrps_nfhza <- ri_blkgrps_nfhza %>%
  mutate(RI_LOWINC = if_else(RI_INCOME == "I",totalpopE,0)) %>%
  mutate(RI_LOWINC = replace_na(RI_LOWINC,0)) %>%
  mutate(RI_MINORITIES = if_else(RI_MINORITY == "M", totalpopE,0)) %>%
  mutate(RI_MINORITIES = replace_na(RI_MINORITIES,0)) %>%
  mutate(Proportion = as.numeric(NewArea/OldArea),
         NewPop = totalpopE*Proportion,
         NewMinority = minorityE*Proportion,
         NewUnder5 = under5E*Proportion,
         NewOver64 = over64E*Proportion,
         NewUnder18 = under18E*Proportion,
         NewEng_limit = eng_limitE*Proportion,
         NewPov = num2povE*Proportion,
         NewLths = lthsE*Proportion,
         NewRI_LOWINC = RI_LOWINC*Proportion,
         NewRI_MINORITIES = RI_MINORITIES*Proportion)

ri_blkgrps_hevac <- ri_blkgrps_hevac %>%
  mutate(RI_LOWINC = if_else(RI_INCOME == "I",totalpopE,0)) %>%
  mutate(RI_LOWINC = replace_na(RI_LOWINC,0)) %>%
  mutate(RI_MINORITIES = if_else(RI_MINORITY == "M", totalpopE,0)) %>%
  mutate(RI_MINORITIES = replace_na(RI_MINORITIES,0)) %>%
  mutate(Proportion = as.numeric(NewArea/OldArea),
         NewPop = totalpopE*Proportion,
         NewMinority = minorityE*Proportion,
         NewUnder5 = under5E*Proportion,
         NewOver64 = over64E*Proportion,
         NewUnder18 = under18E*Proportion,
         NewEng_limit = eng_limitE*Proportion,
         NewPov = num2povE*Proportion,
         NewLths = lthsE*Proportion,
         NewRI_LOWINC = RI_LOWINC*Proportion,
         NewRI_MINORITIES = RI_MINORITIES*Proportion)

ri_tracts_nfhza <- ri_tracts_nfhza %>%
  mutate(Proportion = as.numeric(NewArea/OldArea),
         NewDisabled = disabledOver18E*Proportion,
         NewNoCar = HHnoCarE*Proportion)

ri_tracts_hevac <- ri_tracts_hevac %>%
  mutate(Proportion = as.numeric(NewArea/OldArea),
         NewDisabled = disabledOver18E*Proportion,
         NewNoCar = HHnoCarE*Proportion)

# Compute total block group populations within flood zones
ri_flood_blkgrp_df <- ri_blkgrps_nfhza %>%
  as.data.frame() %>%
  summarize(`Total Pop` = as.integer(sum(NewPop)),
            Minority = as.integer(sum(NewMinority)),
            `Under 5` = as.integer(sum(NewUnder5)),
            `Over 64` = as.integer(sum(NewOver64)),
            `Under 18` = as.integer(sum(NewUnder18)),
            `Limited English HH` = as.integer(sum(NewEng_limit)),
            `Low Income` = as.integer(sum(NewPov)),
            `No HS Dip` = as.integer(sum(NewLths)),
            `RI Low Income` = as.integer(sum(NewRI_LOWINC)),
            `RI Minority` = as.integer(sum(NewRI_MINORITIES))) %>%
  gather(key = Group, value = FloodPop)

# Compute total block group populations within hurricane evac zones
ri_hevac_blkgrp_df <- ri_blkgrps_hevac %>%
  as.data.frame() %>%
  summarize(`Total Pop` = as.integer(sum(NewPop)),
            Minority = as.integer(sum(NewMinority)),
            `Under 5` = as.integer(sum(NewUnder5)),
            `Over 64` = as.integer(sum(NewOver64)),
            `Under 18` = as.integer(sum(NewUnder18)),
            `Limited English HH` = as.integer(sum(NewEng_limit)),
            `Low Income` = as.integer(sum(NewPov)),
            `No HS Dip` = as.integer(sum(NewLths)),
            `RI Low Income` = as.integer(sum(NewRI_LOWINC)),
            `RI Minority` = as.integer(sum(NewRI_MINORITIES))) %>%
  gather(key = Group, value = HevacPop)

# Compute total tract populations within flood zones
ri_flood_tracts_df <- ri_tracts_nfhza %>%
  as.data.frame() %>%
  summarize(`Disabled` = as.integer(sum(NewDisabled)),
            `No Car HH` = as.integer(sum(NewNoCar))) %>%
  gather(key = Group, value = FloodPop)

ri_hevac_tracts_df <- ri_tracts_hevac %>%
  as.data.frame() %>%
  summarize(`Disabled` = as.integer(sum(NewDisabled)),
            `No Car HH` = as.integer(sum(NewNoCar))) %>%
  gather(key = Group, value = HevacPop)

# Compute total tract populations within the state for same groups
ri_tract_flood_pops_df <- ri_tracts_2840 %>%
  as.data.frame() %>%
  summarize(`Disabled` = sum(disabledOver18E),
            `No Car HH` = sum(HHnoCarE)) %>%
  gather(key = Group, value = RIPop) %>%
  left_join(.,ri_flood_tracts_df, by = "Group")

ri_tract_hevac_pops_df <- ri_tracts_2840 %>%
  as.data.frame() %>%
  summarize(`Disabled` = sum(disabledOver18E),
            `No Car HH` = sum(HHnoCarE)) %>%
  gather(key = Group, value = RIPop) %>%
  left_join(.,ri_hevac_tracts_df, by = "Group")

# Compute populations for state,and join with flood pops
ri_FloodPops_df <- ri_blkgrp_2840 %>%
  as.data.frame() %>%
  mutate(RI_LOWINC = if_else(RI_INCOME == "I",totalpopE,0)) %>%
  mutate(RI_LOWINC = replace_na(RI_LOWINC,0)) %>%
  mutate(RI_MINORITIES = if_else(RI_MINORITY == "M",totalpopE,0)) %>%
  mutate(RI_MINORITIES = replace_na(RI_MINORITIES,0)) %>%
  summarize(`Total Pop` = sum(totalpopE),
            Minority = sum(minorityE),
            `Under 5` = sum(under5E),
            `Over 64` = sum(over64E),
            `Under 18` = sum(under18E),
            `Limited English HH` = sum(eng_limitE),
            `Low Income` = sum(num2povE),
            `No HS Dip` = sum(lthsE),
            `RI Low Income` = sum(RI_LOWINC, na.rm = TRUE),
            `RI Minority` = sum(RI_MINORITIES, na.rm = TRUE)) %>%
  gather(key = Group, value = RIPop) %>%
  left_join(., ri_flood_blkgrp_df, by = "Group") %>%
  rbind(.,ri_tract_flood_pops_df) %>%
  mutate(PctFlood = FloodPop/RIPop*100)

# Compute populations for state,  and join with hurricane evac pops
ri_HevacPops_df <- ri_blkgrp_2840 %>%
  as.data.frame() %>%
  mutate(RI_LOWINC = if_else(RI_INCOME == "I",totalpopE,0)) %>%
  mutate(RI_LOWINC = replace_na(RI_LOWINC,0)) %>%
  mutate(RI_MINORITIES = if_else(RI_MINORITY == "M",totalpopE,0)) %>%
  mutate(RI_MINORITIES = replace_na(RI_MINORITIES,0)) %>%
  summarize(`Total Pop` = sum(totalpopE),
            Minority = sum(minorityE),
            `Under 5` = sum(under5E),
            `Over 64` = sum(over64E),
            `Under 18` = sum(under18E),
            `Limited English HH` = sum(eng_limitE),
            `Low Income` = sum(num2povE),
            `No HS Dip` = sum(lthsE),
            `RI Low Income` = sum(RI_LOWINC, na.rm = TRUE),
            `RI Minority` = sum(RI_MINORITIES, na.rm = TRUE)) %>%
  gather(key = Group, value = RIPop) %>%
  left_join(., ri_hevac_blkgrp_df, by = "Group") %>%
  rbind(.,ri_tract_hevac_pops_df) %>%
  mutate(PctHevac = HevacPop/RIPop*100)

# Show table of pops within flood zones
ri_FloodPops_df %>% 
  arrange(-FloodPop)

# Create lollipop plot of pops within flood zones
ri_FloodPops_df %>% 
  ggplot(aes(x = reorder(Group,-PctFlood), 
             y = PctFlood)) +
  geom_segment(aes(x = reorder(Group,-PctFlood), 
                   xend = reorder(Group,-PctFlood),
                   y = ri_FloodPops_df[1,4], yend = PctFlood), 
               color = "skyblue") +
  geom_point(color = "blue", size = 4, alpha = 0.8) +
  coord_flip() + xlab("") + ylab("") + 
  ggtitle("Rhode Island Populations within Flood Zones") + theme_light() +
  theme(panel.grid.major.y = element_blank(),
        panel.border = element_blank(),
        axis.ticks.y = element_blank()) +
  geom_text(aes(x = Group, y = PctFlood + 0.2 * sign(PctFlood), 
                label = paste0(round(PctFlood,1),"%")), 
            hjust = 0.1, vjust = -0.5, size = 3,
            color=rgb(100,100,100, maxColorValue=255)) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  geom_hline(yintercept = ri_FloodPops_df[1,4], linetype = "dashed") +
  geom_text(aes(x = "Disabled", y = 9.1, label = "Below state avg"),
            color = "gray48") +
  geom_segment(aes(x = "No Car HH", xend = "No Car HH", y = 10, yend = 8.2), 
               arrow = arrow(length = unit(0.3,"cm"))) +
  geom_text(aes(x = "Low Income", y = 11.3, label = "Above state avg"),
            color = "gray48") +
  geom_segment(aes(x = "Under 18", xend = "Under 18", y = 10.4, yend = 12.2), 
               arrow = arrow(length = unit(0.3,"cm")))

# Create a dot density map of total populations and overlay on flood zones
# Create point layer of major cities for context
# Note cb=FALSE is necessary for extracting centroids from town polygons. Otherwise, if cb=TRUE, cannot extract centroids from multipolygon features.
ri_towns_sf_pts <- county_subdivisions(state = "RI", cb = TRUE) %>% 
  filter(NAME %in% c("Providence", 
                     "Woonsocket", 
                     "Pawtucket", 
                     "Warwick", 
                     "Bristol", 
                     "Portsmouth", 
                     "Newport", 
                     "Narragansett", 
                     "North Kingstown", 
                     "Charlestown", 
                     "Situate",
                     "Glocester")) %>% 
  st_transform(., crs = 2840) %>% 
  st_centroid(of_largest_polygon = TRUE)

# Create road layer for context
ri_highways <- tigris::primary_roads() %>% 
  filter(FULLNAME %in% c("I- 95","I- 195","I- 295", "US Hwy 6")) %>%
  tmaptools::crop_shape(., ne_states_sf_cb) %>% 
  st_transform(., crs = 2840)

# Extract highway segments for labeling
I95roadSegment <- ri_highways %>% 
  filter(LINEARID == "110468245978")

I95roadSegment2 <- ri_highways %>% 
  filter(LINEARID == "1107052605232")

I295roadSegment <- ri_highways %>% 
  filter(LINEARID == "1104755623349")

I195roadSegment <- ri_highways %>% 
  filter(LINEARID == "110448466166")

# Create custom icons of highway shields
I95 <- tmap_icons(file = "https://upload.wikimedia.org/wikipedia/commons/thumb/6/61/I-95.svg/200px-I-95.svg.png")
I195 <- tmap_icons("https://upload.wikimedia.org/wikipedia/commons/thumb/f/f7/I-195.svg/200px-I-195.svg.png")
I295 <- tmap_icons("https://upload.wikimedia.org/wikipedia/commons/thumb/4/41/I-295.svg/200px-I-295.svg.png")

# Create random points, with 1 point for every 100 people
ri_totalpop_pts <- ri_blkgrp_2840 %>% 
  select(totalpopE) %>% 
  filter(totalpopE >= 100) %>% 
  st_sample(., size = round(.$totalpopE/100)) %>% # create 1 random point for every 100 people
  st_sf(.) %>% 
  mutate(Group = "Total Pop")

# Map totalpop and flood zones
tm_layout(bg.color = "#e6f3f7") +
  tm_shape(ri_blkgrp_2840, unit = "mi") + tm_fill(col = "white") +
  tm_shape(ne_states_sf_cb) + tm_fill(col="white") +
  tm_shape(ri_awater_sf) + tm_fill(col = "#e6f3f7") + 
  tm_shape(ne_states_sf_cb) + tm_borders() +
  tm_shape(ri_totalpop_pts) + tm_dots(col = "forestgreen",
                                      labels = "1 dot = 100 persons") +
  tm_shape(ri_fhza_2840_land) + 
  tm_fill(col = "Interval", 
          palette = c("gold", "goldenrod3"), 
          labels = c("1% AEP (100-year)", "0.2% AEP (500-year)"), 
          title = "FEMA Flood Zones",
          alpha = 0.6,
          border.alpha = 0) +
  tm_shape(ri_fhza_2840_land) +
  tm_borders(col = "goldenrod1",
             lwd = 0.5,
             alpha = 0.6) +
  tm_shape(ne_states_sf_cb) + tm_borders() +
  tm_shape(ri_highways) + tm_lines(col = "seashell4", lwd = 2) +
  tm_shape(I95roadSegment) + 
  tm_symbols(shape = I95, border.lwd = NA, size = 0.25) +
  tm_shape(I95roadSegment2) + 
  tm_symbols(shape = I95, border.lwd = NA, size = 0.25) +
  tm_shape(I195roadSegment) +
  tm_symbols(shape = I195, border.lwd = NA, size = 0.25) +
  tm_shape(I295roadSegment) + 
  tm_symbols(shape = I295, border.lwd = NA, size = 0.25) +
  tm_shape(ri_towns_sf_pts) + tm_dots() +
  tm_text("NAME", size = 0.5, col = "black", 
          xmod = 0.7, ymod = 0.2, shadow = TRUE) +
  tm_scale_bar(breaks = c(0, 5, 10), text.size = 0.5, 
               position = c(0.6,0.005)) +
  tm_add_legend(type = "fill", col = "forestgreen",
                border.col = "white", border.alpha = 0,
                labels = "1 dot = 100 persons",
                title = "Total Population") +
  tm_layout(title = "Population Distribution \nand Flood Zones", 
            frame = TRUE, main.title.size = 0.8,
            legend.outside = TRUE,
            legend.title.size = 0.8,
            legend.outside.position = c("right", "top"))

# Create a dot density map of transit-dependent populations in flood zone
# Create random points, with 1 point for every 5 people
Over64_nfhza_pts <- ri_blkgrps_nfhza %>% 
  dplyr::select(NewOver64) %>% 
  filter(NewOver64 >= 5) %>% 
  st_sample(., size = round(.$NewOver64/5)) %>% # create 1 random point for every 5 people
  st_sf(.) %>% 
  mutate(Group = "Over 64")

Disabled_nfhza_pts <- ri_tracts_nfhza %>% 
  dplyr::select(NewDisabled) %>% 
  filter(NewDisabled >= 5) %>% 
  st_sample(., size = round(.$NewDisabled/5)) %>% 
  st_sf(.) %>% 
  mutate(Group = "Disabled")

NoCarHH_nfhza_pts <- ri_tracts_nfhza %>% 
  dplyr::select(NewNoCar) %>% 
  filter(NewNoCar >= 5) %>% 
  st_sample(., size = round(.$NewNoCar/5)) %>% 
  st_sf(.) %>% 
  mutate(Group = "No Car HH")

# Bring them together
ri_nfhza_vulnerable <- rbind(Over64_nfhza_pts,Disabled_nfhza_pts,
                             NoCarHH_nfhza_pts) %>% 
  slice(sample(1:n())) # randomise order to avoid bias in plotting order

# Map transit-dependent pops and flood zones
tm_layout(bg.color = "#e6f3f7") +
  tm_shape(ri_blkgrp_2840, unit = "mi") + tm_fill(col = "white") +
  tm_shape(ne_states_sf_cb) + tm_fill(col="white") +
  tm_shape(ri_awater_sf) + tm_fill(col = "#e6f3f7") + 
  tm_shape(ri_fhza_2840_land) + 
  tm_fill(col = "Interval", 
          palette = c("gold", "goldenrod3"), 
          labels = c("1% AEP (100-year)", "0.2% AEP (500-year)"), 
          title = "FEMA Flood Zones",
          border.alpha = 0) +
  tm_shape(ne_states_sf_cb) + tm_borders() +
  tm_shape(ri_nfhza_vulnerable) + tm_dots(col = "Group", 
                                          palette = c("green","red","blue"),
                                          legend.show = FALSE) +
  tm_shape(ne_states_sf_cb) + tm_borders() +
  tm_shape(ri_highways) + tm_lines(col = "seashell4", lwd = 2) +
  tm_shape(I95roadSegment) + 
  tm_symbols(shape = I95, border.lwd = NA, size = 0.25) +
  tm_shape(I95roadSegment2) + 
  tm_symbols(shape = I95, border.lwd = NA, size = 0.25) +
  tm_shape(I195roadSegment) +
  tm_symbols(shape = I195, border.lwd = NA, size = 0.25) +
  tm_shape(I295roadSegment) + 
  tm_symbols(shape = I295, border.lwd = NA, size = 0.25) +
  tm_shape(ri_towns_sf_pts) + tm_dots() +
  tm_text("NAME", size = 0.5, col = "black", 
          xmod = 0.7, ymod = 0.2, shadow = TRUE) +
  tm_scale_bar(breaks = c(0, 5, 10), text.size = 0.5, 
               position = c(0.6,0.005)) +
  tm_add_legend(type = "fill", col = c("green","red","blue"),
                border.col = "white", border.alpha = 0,
                labels = c("Disabled", "No Car HH", "Over 64"),
                title = "Population\nGroup*") +
  tm_layout(title = "Transit Dependent Populations \nwithin Flood Zones", 
            frame = TRUE, main.title.size = 0.8,
            legend.outside = TRUE,
            legend.title.size = 0.8,
            legend.outside.position = c("right", "top"))

# Show table of pops within hurricane evacuation zones
ri_HevacPops_df %>% 
  arrange(-HevacPop)

# Create lollipop plot of pops within hurricane evac zones
ri_HevacPops_df %>% 
  ggplot(aes(x = reorder(Group,-PctHevac), 
             y = PctHevac)) +
  geom_segment(aes(x = reorder(Group,-PctHevac), 
                   xend = reorder(Group,-PctHevac),
                   y = ri_HevacPops_df[1,4], yend = PctHevac), 
               color = "skyblue") +
  geom_point(color = "blue", size = 4, alpha = 0.8) +
  coord_flip() + xlab("") + ylab("") + 
  ggtitle("Rhode Island Populations within Hurricane Evacuation Zones") +
  theme_light() +
  theme(panel.grid.major.y = element_blank(),
        panel.border = element_blank(),
        axis.ticks.y = element_blank()) +
  geom_text(aes(x = Group, y = PctHevac + 0.2 * sign(PctHevac), 
                label = paste0(round(PctHevac,1),"%")), 
            hjust = 0.1, vjust = -0.5, size = 3,
            color=rgb(100,100,100, maxColorValue=255)) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  geom_hline(yintercept = ri_HevacPops_df[1,4], linetype = "dashed") +
  geom_text(aes(x = "Disabled", y = 10.3, label = "Below state avg"),
            color = "gray48") +
  geom_segment(aes(x = "No Car HH", xend = "No Car HH", y = 11.4, yend = 9.6), 
               arrow = arrow(length = unit(0.3,"cm"))) +
  geom_text(aes(x = "Low Income", y = 13, label = "Above state avg"),
            color = "gray48") +
  geom_segment(aes(x = "Under 18", xend = "Under 18", y = 11.8, yend = 13.6), 
               arrow = arrow(length = unit(0.3,"cm")))

# create simplified version of hurricane evac polygons
ri_hea_sf_agg <- 
  ri_hea_sf %>% 
  group_by(EVAC) %>% 
  summarize()

r_hea_cata <- ri_hea_sf_agg %>% 
  filter(EVAC == "A")

r_hea_catb <- ri_hea_sf_agg %>% 
  filter(EVAC == "B")

# Map totalpop and hurricane evacuation zones
tm_layout(bg.color = "#e6f3f7") +
  tm_shape(ri_blkgrp_2840, unit = "mi") + tm_fill(col = "white") +
  tm_shape(ne_states_sf_cb) + tm_fill(col="white") +
  tm_shape(ri_awater_sf) + tm_fill(col = "#e6f3f7") + 
  tm_shape(ne_states_sf_cb) + tm_borders() +
  tm_shape(ri_totalpop_pts) + tm_dots(col = "forestgreen",
                                      labels = "1 dot = 100 persons") +
  tm_shape(ri_hea_sf_agg) + 
  tm_fill(col = "EVAC",
          palette = c("darkkhaki", "rosybrown1", "khaki"),
          labels = c("A: Catgeory 1 - 2",
                     "B: Category 3 - 4",
                     "C: Category 5"),
          title = "Evacuation Zone and\nHurricane Category",
          alpha = 0.6) +
  tm_shape(r_hea_cata) +
  tm_borders(col = "darkgoldenrod",
             lwd = 0.5,
             alpha = 0.6) +
  tm_shape(r_hea_catb) +
  tm_borders(col = "rosybrown3",
             lwd = 0.5,
             alpha = 0.6) +
  tm_shape(ne_states_sf_cb) + tm_borders() +
  tm_shape(ri_highways) + tm_lines(col = "seashell4", lwd = 2) +
  tm_shape(I95roadSegment) + 
  tm_symbols(shape = I95, border.lwd = NA, size = 0.25) +
  tm_shape(I95roadSegment2) + 
  tm_symbols(shape = I95, border.lwd = NA, size = 0.25) +
  tm_shape(I195roadSegment) +
  tm_symbols(shape = I195, border.lwd = NA, size = 0.25) +
  tm_shape(I295roadSegment) + 
  tm_symbols(shape = I295, border.lwd = NA, size = 0.25) +
  tm_shape(ri_towns_sf_pts) + tm_dots() +
  tm_text("NAME", size = 0.5, col = "black", 
          xmod = 0.7, ymod = 0.2, shadow = TRUE) +
  tm_scale_bar(breaks = c(0, 5, 10), text.size = 0.5, 
               position = c(0.6,0.005)) +
  tm_add_legend(type = "fill", col = "forestgreen",
                border.col = "white", border.alpha = 0,
                labels = "1 dot = 100 persons",
                title = "Total Population") +
  tm_layout(title = "Population Distribution \nand Hurricane\nEvacuation Zones", 
            frame = TRUE, main.title.size = 0.8,
            legend.outside = TRUE,
            legend.title.size = 0.8,
            legend.outside.position = c("right", "top"))

# Create a dot density of transit-dependent populations within hurricane evacuation zones
# Create random points, with 1 point for every 5 people
Over64_hevac_pts <- ri_blkgrps_hevac %>% 
  dplyr::select(NewOver64) %>% 
  filter(NewOver64 >= 5) %>% 
  st_sample(., size = round(.$NewOver64/5)) %>% # create 1 random point for every 5 people
  st_sf(.) %>% 
  mutate(Group = "Over 64")

Disabled_hevac_pts <- ri_tracts_hevac %>% 
  dplyr::select(NewDisabled) %>% 
  filter(NewDisabled >= 5) %>% 
  st_sample(., size = round(.$NewDisabled/5)) %>% 
  st_sf(.) %>% 
  mutate(Group = "Disabled")

NoCarHH_hevac_pts <- ri_tracts_hevac %>% 
  dplyr::select(NewNoCar) %>% 
  filter(NewNoCar >= 5) %>% 
  st_sample(., size = round(.$NewNoCar/5)) %>% 
  st_sf(.) %>% 
  mutate(Group = "No Car HH")

# Bring them together
ri_hevac_vulnerable <- rbind(Over64_hevac_pts,Disabled_hevac_pts,
                             NoCarHH_hevac_pts) %>% 
  slice(sample(1:n())) # randomise order to avoid bias in plotting order

# Map them out
tm_layout(bg.color = "#e6f3f7") +
  tm_shape(ri_blkgrp_2840, unit = "mi") + tm_fill(col = "white") +
  tm_shape(ne_states_sf_cb) + tm_fill(col="white") +
  tm_shape(ri_awater_sf) + tm_fill(col = "#e6f3f7") + 
  tm_shape(ri_hea_sf) + 
  tm_fill(col = "EVAC",
          palette = c("darkkhaki", "rosybrown1", "khaki"),
          labels = c("A: Catgeory 1 - 2",
                     "B: Category 3 - 4",
                     "C: Category 5"),
          title = "Evacuation Zone and\nHurricane Category") +
  tm_shape(ne_states_sf_cb) + tm_borders() +
  tm_text("STUSPS", size = 0.7, remove.overlap = TRUE, col = "gray") +
  tm_shape(ri_hevac_vulnerable) + tm_dots(col = "Group", 
                                          palette = c("green","red","blue"),
                                          legend.show = FALSE) +
  tm_shape(ne_states_sf_cb) + tm_borders() +
  tm_shape(ri_highways) + tm_lines(col = "seashell4", lwd = 2) +
  tm_shape(I95roadSegment) + 
  tm_symbols(shape = I95, border.lwd = NA, size = 0.25) +
  tm_shape(I95roadSegment2) + 
  tm_symbols(shape = I95, border.lwd = NA, size = 0.25) +
  tm_shape(I195roadSegment) +
  tm_symbols(shape = I195, border.lwd = NA, size = 0.25) +
  tm_shape(I295roadSegment) + 
  tm_symbols(shape = I295, border.lwd = NA, size = 0.25) +
  tm_shape(ri_towns_sf_pts) + tm_dots() +
  tm_text("NAME", size = 0.5, col = "black", 
          xmod = 0.7, ymod = 0.2, shadow = TRUE) +
  tm_scale_bar(breaks = c(0, 5, 10), text.size = 0.5, 
               position = c(0.6,0.005)) +
  tm_add_legend(type = "fill", col = c("green","red","blue"),
                border.col = "white", border.alpha = 0,
                labels = c("Disabled", "No Car HH", "Over 64"),
                title = "Population\nGroup*") +
  tm_layout(title = "Transit Dependent Populations \nwithin Hurricane Evacuation\nZones", 
            frame = TRUE, main.title.size = 0.8,
            legend.outside = TRUE,
            legend.title.size = 0.8,
            legend.outside.position = c("right", "top"))