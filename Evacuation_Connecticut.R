# Analysis of flooding and hurricane evacuation risks for populations of concern in Connecticut
library(tidyverse)
library(sf)
library(tmap)
library(tmaptools)
library(lwgeom)
library(tigris)
options(tigris_use_cache = TRUE, tigris_class = "sf")

load("DATA/ne_layers.rds")

# Extract state census units and convert to projected local CRS EPSG:2775: NAD83(HARN) / Connecticut. See https://spatialreference.org/ref/epsg/2775/
ct_blkgrp_2775 <- ne_blkgrp_sf %>% 
  filter(STATE == "Connecticut") %>% 
  st_transform(., crs = 2775)

ct_tracts_2775 <- ne_tracts_sf %>% 
  filter(STATE == "Connecticut") %>% 
  st_transform(., crs = 2775)

# Get ctd of empty geometctes
empty_geo <- st_is_empty(ct_blkgrp_2775)
ct_blkgrp_2775 <- ct_blkgrp_2775[!empty_geo,]
empty_geo <- st_is_empty(ct_tracts_2775)
ct_tracts_2775 <- ct_tracts_2775[!empty_geo,]

# Extract state boundary
ct_state_sf <- ne_states_sf_cb %>% 
  filter(NAME == "Connecticut") %>% 
  st_transform(., crs = 2775)

#### Processing in ArcGIS #####
# Write out block groups for processing in ArcGIS
ct_blkgrp_2775 %>%
  dplyr::select(GEOID) %>%
  st_write(., "DATA/FEMA/CT/ct_blkgrp_2775.shp", delete_layer = TRUE)

# repeat for tracts
ct_tracts_2775 %>% 
  dplyr::select(GEOID) %>%
  st_write(., "DATA/FEMA/CT/ct_tracts_2775.shp", delete_layer = TRUE)

# repeat for state boundary
ct_state_sf %>% 
  st_write(., "DATA/FEMA/CT/ct_state_2775.shp", delete_layer = TRUE)

# Use dasymetric mapping to calculate populations within flood zones. Approach follows method used by Qiang (2019) to eliminate unpopulated areas of census polygons and then reallocate populations to developed areas as identified in National Land Cover Dataset (NLCD). 
# Perform NLCD raster-to-vector conversion, vector erase/difference, and vector intersections in ArcMap because it takes too long in R. 
# In ArcMap:
# Convert NLCD raster to shapefile. Clip to state.
# Isolate undeveloped areas (gridcode NOT 22 - 24). 
# Erase areas of ct_blkgrp_2775 and ct_tracts_2775 that overlap with undeveloped areas in NLCD shapefiles. Compute OldArea of erased polygons in sqm to identify area of developed polygons remaining. 
# Intersect erased ct_blkgrps and erased ct_tracts with NFHZA and Hurricane evacuation zones. Read back into R.

##### Return to working in R ######
# read in processed ct_blkgrps and ct_tracts
st_layers(dsn = "DATA/FEMA/CT")

ct_blkgrps_nfhza <- st_read(dsn = "DATA/FEMA/CT", 
                            layer = "ct_blkgrps_nfhza") %>% 
  left_join(., as.data.frame(ct_blkgrp_2775), by = "GEOID") %>% 
  st_transform(., crs = 2775) %>% 
  mutate(NewArea = st_area(.)) %>%
  st_make_valid()

ct_tracts_nfhza <- st_read(dsn = "DATA/FEMA/CT", 
                           layer = "ct_tracts_nfhza") %>% 
  left_join(., as.data.frame(ct_tracts_2775), by = "GEOID") %>% 
  st_transform(., crs = 2775) %>% 
  mutate(NewArea = st_area(.)) %>%
  st_make_valid()

# Apportion populations based on geographic proportion of intersect
ct_blkgrps_nfhza <- ct_blkgrps_nfhza %>%
  mutate(CT_LOWINC = if_else(CT_INCOME == "I",totalpopE,0)) %>%
  mutate(CT_LOWINC = replace_na(CT_LOWINC,0)) %>%
  mutate(Proportion = as.numeric(NewArea/OldArea),
         NewPop = totalpopE*Proportion,
         NewMinority = minorityE*Proportion,
         NewUnder5 = under5E*Proportion,
         NewOver64 = over64E*Proportion,
         NewUnder18 = under18E*Proportion,
         NewEng_limit = eng_limitE*Proportion,
         NewPov = num2povE*Proportion,
         NewLths = lthsE*Proportion,
         NewCT_LOWINC = CT_LOWINC*Proportion)

ct_tracts_nfhza <- ct_tracts_nfhza %>%
  mutate(Proportion = as.numeric(NewArea/OldArea),
         NewDisabled = disabledOver18E*Proportion,
         NewNoCar = HHnoCarE*Proportion)

# Compute total block group populations within flood zones
ct_flood_blkgrp_df <- ct_blkgrps_nfhza %>%
  as.data.frame() %>%
  summarize(`Total Pop` = as.integer(sum(NewPop)),
            Minority = as.integer(sum(NewMinority)),
            `Under 5` = as.integer(sum(NewUnder5)),
            `Over 64` = as.integer(sum(NewOver64)),
            `Under 18` = as.integer(sum(NewUnder18)),
            `Limited English HH` = as.integer(sum(NewEng_limit)),
            `Low Income` = as.integer(sum(NewPov)),
            `No HS Dip` = as.integer(sum(NewLths)),
            `CT Low Income` = as.integer(sum(NewCT_LOWINC))) %>%
  gather(key = Group, value = FloodPop)

ct_flood_tracts_df <- ct_tracts_nfhza %>%
  as.data.frame() %>%
  summarize(`Disabled` = as.integer(sum(NewDisabled)),
            `No Car HH` = as.integer(sum(NewNoCar))) %>%
  gather(key = Group, value = FloodPop)

# Compute total tract populations within the state for same groups
ct_tract_flood_pops_df <- ct_tracts_2775 %>%
  as.data.frame() %>%
  summarize(`Disabled` = sum(disabledOver18E),
            `No Car HH` = sum(HHnoCarE)) %>%
  gather(key = Group, value = CTPop) %>%
  left_join(.,ct_flood_tracts_df, by = "Group")

# Compute populations for state,and join with flood pops
ct_FloodPops_df <- ct_blkgrp_2775 %>%
  as.data.frame() %>%
  mutate(CT_LOWINC = if_else(CT_INCOME == "I",totalpopE,0)) %>%
  mutate(CT_LOWINC = replace_na(CT_LOWINC,0)) %>%
  summarize(`Total Pop` = sum(totalpopE),
            Minority = sum(minorityE),
            `Under 5` = sum(under5E),
            `Over 64` = sum(over64E),
            `Under 18` = sum(under18E),
            `Limited English HH` = sum(eng_limitE),
            `Low Income` = sum(num2povE),
            `No HS Dip` = sum(lthsE),
            `CT Low Income` = sum(CT_LOWINC, na.rm = TRUE)) %>%
  gather(key = Group, value = CTPop) %>%
  left_join(., ct_flood_blkgrp_df, by = "Group") %>%
  rbind(.,ct_tract_flood_pops_df) %>%
  mutate(PctFlood = FloodPop/CTPop*100)

# Read in NFHL for CT. Data comes from FEMA. 
# List available layers in geodatabase
# st_layers("DATA/FEMA/CT/NFHL_09_20200116/NFHL_09_20200116.gdb")
# Read in flood hazard areas
ct_nfhza_2775 <- st_read(dsn = "DATA/FEMA/CT/NFHL_09_20200116/NFHL_09_20200116.gdb", layer = "S_FLD_HAZ_AR") %>% 
  filter(!FLD_ZONE %in% c("OPEN WATER","AREA NOT INCLUDED") & 
           !ZONE_SUBTY %in% c("AREA OF MINIMAL FLOOD HAZARD",
                              "AREA WITH REDUCED FLOOD RISK DUE TO LEVEE")) %>%
  mutate(FLD_ZONE = as.character(FLD_ZONE)) %>% # omit unused factor levels
  st_transform(., crs = 2775) %>% 
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

# get rid of empty geometries
empty_geo <- st_is_empty(ct_nfhza_2775)
ct_nfhza_2775 <- ct_nfhza_2775[!empty_geo,]


# download Census TIGERLine hydrography for CT
## First, extract list of county names to use with tigris::water
ct_counties <- counties("CT") %>% 
  pull(NAME)
# Next, download water features for each county and rbind to one layer
ct_awater_sf <- rbind_tigris(
  lapply(
    ct_counties, function(x) area_water(state = "CT", county = x)
  )
) %>% 
  st_union() %>% 
  st_as_sf() %>% 
  st_transform(., crs = 2775)

# crop flood zones to land areas only
start_time <- Sys.time()
ct_nfhza_2775_land <- ct_nfhza_2775 %>% 
  crop_shape(., ct_state_sf, polygon = TRUE) %>% 
  st_difference(., ct_awater_sf) %>% 
  mutate(Area = st_area(.)) %>% 
  st_make_valid()
end_time <- Sys.time()
end_time - start_time # takes about 12.2 mins

# Create a dot density map of total populations and overlay on flood zones
# Create point layer of major cities for context
# Note cb=FALSE is necessary for extracting centroids from town polygons. Otherwise, if cb=TRUE, cannot extract centroids from multipolygon features.
ct_towns_sf_pts <- county_subdivisions(state = "CT", cb = TRUE) %>% 
  filter(NAME %in% c("Hartford", 
                     "Stamford", 
                     "Fairfield", 
                     "Milford", 
                     "New Haven", 
                     "Danbury", 
                     "Hamden", 
                     "Meriden", 
                     "Waterbury", 
                     "New Britain",
                     "New London",
                     "Norwich",
                     "North Canaan",
                     "Old Saybrook")) %>% 
  st_transform(., crs = 2775) %>% 
  st_centroid(of_largest_polygon = TRUE)

# Create road layer for context
ct_highways <- primary_roads() %>% 
  filter(FULLNAME %in% c("I- 84","I- 91","I- 95","I- 395")) %>% 
  tmaptools::crop_shape(., ne_states_sf_cb) %>% 
  st_transform(., crs = 2775)

# Extract highway segments for labeling
I84roadSegment <- ct_highways %>% 
  filter(LINEARID == "1105281313426")

I91roadSegment <- ct_highways %>% 
  filter(LINEARID == "1104470395750")

I95roadSegment <- ct_highways %>% 
  filter(LINEARID == "1105068628699")

I95roadSegment2 <- ct_highways %>% 
  filter(LINEARID == "1105281377007")

I395roadSegment <- ct_highways %>% 
  filter(LINEARID == "1104493001068")

# Create custom icons of highway shields
I95 <- tmap_icons(file = "https://upload.wikimedia.org/wikipedia/commons/thumb/6/61/I-95.svg/200px-I-95.svg.png")
I395 <- tmap_icons("https://upload.wikimedia.org/wikipedia/commons/thumb/1/16/I-395.svg/200px-I-395.svg.png")
I91 <- tmap_icons("https://upload.wikimedia.org/wikipedia/commons/thumb/9/90/I-91.svg/200px-I-91.svg.png")
I84 <- tmap_icons("https://upload.wikimedia.org/wikipedia/commons/thumb/5/51/I-84.svg/200px-I-84.svg.png")

# Bring in outline of New York for western boundary
ny_state_sf <- tigris::states(cb = TRUE) %>% 
  filter(NAME == "New York")

# Create polygons to show where there are no FEMA maps
nofemapolys <- st_read(dsn = "DATA/FEMA/CT/NFHL_09_20200116/NFHL_09_20200116.gdb", layer = "S_FLD_HAZ_AR") %>% 
  group_by() %>% 
  summarize() %>% # aggregate all flood zone polygons into one
  st_transform(., crs = st_crs(ct_state_sf)) %>% 
  st_make_valid() %>% 
  st_difference(ct_state_sf,.) %>% # extract areas with no flood coverage
  st_cast(., "POLYGON") %>% # disaggregate multipolygon feature
  mutate(Area = st_area(.), 
         LABEL = "No FEMA\nFlood Data") %>% # calc area of each polygon
  filter(as.numeric(Area)/10^6 > 1000) # remove polygons < 1000km2

# # Create points to label areas where there are no FEMA maps
# # == TEXT WRAPPING FUNCTION I GOT FROM STACK OVERFLOW AND USE OFTEN ===========
# #http://stackoverflow.com/questions/20241065/r-barplot-wrapping-long-text-labels
# # Core wrapping function
# wrap.it <- function(x, len)
# { 
#   sapply(x, function(y) paste(strwrap(y, len), 
#                               collapse = "\n"), 
#          USE.NAMES = FALSE)
# }
# 
# # Call this function with a list or vector
# wrap.labels <- function(x, len)
# {
#   if (is.list(x))
#   {
#     lapply(x, wrap.it, len)
#   } else {
#     wrap.it(x, len)
#   }
# }
# 
# nofemapoints <- data.frame(name = c("No FEMA Flood Data","No FEMA Flood Data"),
#                            x = c(-73.300735,-72.192279), 
#                            y = c(41.813769,41.809703),
#                            stringsAsFactors = FALSE) %>% 
#   mutate(name = wrap.labels(name,10)) %>% 
#   st_as_sf(., coords = c("x","y"), crs = 4326)

# Create random points, with 1 point for every 500 people
ct_totalpop_pts <- ct_blkgrp_2775 %>% 
  select(totalpopE) %>% 
  filter(totalpopE >= 500) %>% 
  st_sample(., size = round(.$totalpopE/500)) %>% # create 1 random point for every 500 people
  st_sf(.) %>% 
  group_by() %>% 
  summarize() # reduce to one multipoint object

# Create a simplified version of NFHZA polygons to speed up mapping
ct_nfhza_2775_land_simple <- ct_nfhza_2775_land %>% 
  select(Interval) %>% 
  st_cast(., "MULTIPOLYGON") %>% # homogenize type
  st_cast(., "POLYGON") %>% 
  st_simplify(., dTolerance = 100) %>% # reduce vertices
  st_make_valid() %>%
  group_by(Interval) %>% 
  summarize() %>% 
  mutate(Area = st_area(.))

# # map it
# start_time <- Sys.time()
# tm_shape(ct_nfhza_2775_land_simple) + tm_fill(col = "Interval")
# end_time <- Sys.time()
# end_time - start_time


# Map totalpop and flood zones
start_time <- Sys.time()
tm_layout(bg.color = "#e6f3f7") +
  tm_shape(ct_blkgrp_2775, unit = "mi") + tm_fill(col = "white") +
  tm_shape(ne_states_sf_cb) + tm_fill(col="white") +
  tm_shape(ny_state_sf) + tm_fill(col="white") +
  tm_shape(ct_awater_sf) + tm_fill(col = "#e6f3f7") + 
  tm_shape(ne_states_sf_cb) + tm_borders() +
  tm_shape(ny_state_sf) + tm_borders() +
  tm_shape(ct_totalpop_pts) + tm_dots(col = "darkgoldenrod3",
                                      labels = "1 dot = 500 people",
                                      alpha = 0.6) +
  tm_shape(ct_nfhza_2775_land_simple) + 
  tm_fill(col = "Interval", 
          palette = c("dodgerblue", "blueviolet"), 
          labels = c("1% AEP (100-year)", "0.2% AEP (500-year)"), 
          title = "FEMA Flood Zones",
          alpha = 0.6,
          border.alpha = 0) +
  # tm_shape(ct_nfhza_2775_land_simple) +
  # tm_borders(col = "goldenrod1",
  #            lwd = 0.5,
  #            alpha = 0.6) +
  tm_shape(ne_states_sf_cb) + tm_borders() +
  tm_shape(ny_state_sf) + tm_borders() +
  tm_shape(ct_highways) + tm_lines(col = "seashell4", lwd = 2) +
  tm_shape(I95roadSegment) + 
  tm_symbols(shape = I95, border.lwd = NA, size = 0.25) +
  tm_shape(I95roadSegment2) + 
  tm_symbols(shape = I95, border.lwd = NA, size = 0.25) +
  tm_shape(I395roadSegment) +
  tm_symbols(shape = I395, border.lwd = NA, size = 0.25) +
  tm_shape(I91roadSegment) + 
  tm_symbols(shape = I91, border.lwd = NA, size = 0.25) +
  tm_shape(I84roadSegment) + 
  tm_symbols(shape = I84, border.lwd = NA, size = 0.25) +
  tm_shape(ct_towns_sf_pts) + tm_dots() +
  tm_text("NAME", size = 0.5, col = "black", 
          xmod = 0.7, ymod = 0.2, shadow = TRUE) +
  tm_shape(nofemapolys) + tm_fill(col = "gray", alpha = 0.4) +
  tm_text("LABEL", alpha = 0.5) +
  tm_scale_bar(breaks = c(0, 10, 20), text.size = 0.5, 
               position = c(0.6,0.005)) +
  tm_add_legend(type = "fill", col = "darkgoldenrod3",
                alpha = 0.6,
                border.col = "white", border.alpha = 0,
                labels = "1 dot = 500 people",
                title = "Total Population") +
  tm_layout(title = "Population Distribution \nand Flood Zones", 
            frame = TRUE, main.title.size = 0.8,
            legend.outside = TRUE,
            legend.title.size = 0.8,
            legend.outside.position = c("right", "top"))
end_time <- Sys.time()
end_time - start_time # takes about 12 mins originally; 14 secs after simplification!

# Total land in CT
ct_area <- as.numeric(ct_state_sf$ALAND)

# Total and percentage of land area of RI within flood zones
ct_nfhza_2775_land %>% 
  as.data.frame() %>% 
  group_by(Interval) %>% 
  summarize(SqKm = round(as.numeric(sum(Area)/10^6),1), 
            SqMi = round(as.numeric(SqKm/2.59),1),
            PctArea = paste0(as.character(round(as.numeric(sum(Area)/ct_area*100),1)),"%")) %>% 
  kableExtra::kable(longtable = T, booktabs = T,
                    format.args = list(big.mark = ','),
                    caption = "Connecticut Land Area within Flood Zones",
                    digits = 0, align = "r") %>% 
  kableExtra::kable_styling(latex_options = c("repeat_header"))

# Show table of pops within flood zones
ct_FloodPops_df %>% 
  mutate(PctFlood = paste0(as.character(round(PctFlood,1)),"%")) %>% 
  arrange(-FloodPop) %>% 
  kableExtra::kable(longtable = T, booktabs = T,
                    format.args = list(big.mark = ','),
                    caption = "Connecticut Populations within Flood Zones",
                    digits = 0, align = "r") %>% 
  kableExtra::kable_styling(latex_options = c("repeat_header"))

# Create lollipop plot of pops within flood zones
ct_FloodPops_df %>% 
  ggplot(aes(x = reorder(Group,PctFlood), 
             y = PctFlood)) +
  geom_segment(aes(x = reorder(Group,PctFlood), 
                   xend = reorder(Group,PctFlood),
                   y = ct_FloodPops_df[1,4], yend = PctFlood), 
               color = "skyblue") +
  geom_point(color = "blue", size = 4, alpha = 0.8) +
  coord_flip() + xlab("") + ylab("") + 
  ggtitle("Connecticut Populations within Flood Zones") + theme_light() +
  theme(panel.grid.major.y = element_blank(),
        panel.border = element_blank(),
        axis.ticks.y = element_blank()) +
  geom_text(aes(x = Group, y = PctFlood + 0.2 * sign(PctFlood), 
                label = paste0(round(PctFlood,1),"%")), 
            hjust = 1.8, vjust = -1, size = 3,
            color=rgb(100,100,100, maxColorValue=255)) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  geom_hline(yintercept = ct_FloodPops_df[1,4], linetype = "dashed") +
  geom_text(aes(x = "No HS Dip", y = 7.65, label = "Above state avg"),
            color = "gray48") +
  geom_segment(aes(x = "Total Pop", xend = "Total Pop", y = 7.2, yend = 7.6), 
               arrow = arrow(length = unit(0.3,"cm")))

# Create a dot density map of transit-dependent populations in flood zone
# Create random points, with 1 point for every 5 people
NoCarHH_nfhza_pts <- ct_tracts_nfhza %>% 
  dplyr::select(NewNoCar) %>% 
  filter(NewNoCar >= 5) %>% 
  st_sample(., size = round(.$NewNoCar/5)) %>% 
  st_sf(.) %>% 
  mutate(Group = "No Car HH")

CTLowInc_nfhza_pts <- ct_blkgrps_nfhza %>% 
  select(NewCT_LOWINC) %>% 
  filter(NewCT_LOWINC >= 5) %>% 
  st_sample(., size = round(.$NewCT_LOWINC/5)) %>% # create 1 random point for every 5 people
  st_sf(.) %>% 
  mutate(Group = "CT Low Income")

LimitEngHH_nfhza_pts <- ct_blkgrps_nfhza %>% 
  select(NewEng_limit) %>% 
  filter(NewEng_limit >= 5) %>% 
  st_sample(., size = round(.$NewEng_limit/5)) %>% # create 1 random point for every 5 people
  st_sf(.) %>% 
  mutate(Group = "Limited English HH")

Disabled_nfhza_pts <- ct_tracts_nfhza %>% 
  dplyr::select(NewDisabled) %>% 
  filter(NewDisabled >= 5) %>% 
  st_sample(., size = round(.$NewDisabled/5)) %>% 
  st_sf(.) %>% 
  mutate(Group = "Disabled")

LowInc_nfhza_pts <- ct_blkgrps_nfhza %>% 
  select(NewPov) %>% 
  filter(NewPov >= 5) %>% 
  st_sample(., size = round(.$NewPov/5)) %>% # create 1 random point for every 5 people
  st_sf(.) %>% 
  mutate(Group = "Low Income")

Minor_nfhza_pts <- ct_blkgrps_nfhza %>% 
  select(NewMinority) %>% 
  filter(NewMinority >= 5) %>% 
  st_sample(., size = round(.$NewMinority/5)) %>% # create 1 random point for every 5 people
  st_sf(.) %>% 
  mutate(Group = "Minority")

Over64_nfhza_pts <- ct_blkgrps_nfhza %>% 
  dplyr::select(NewOver64) %>% 
  filter(NewOver64 >= 5) %>% 
  st_sample(., size = round(.$NewOver64/5)) %>% # create 1 random point for every 5 people
  st_sf(.) %>% 
  mutate(Group = "Over 64")

Under5_nfhza_pts <- ct_blkgrps_nfhza %>% 
  select(NewUnder5) %>% 
  filter(NewUnder5 >= 5) %>% 
  st_sample(., size = round(.$NewUnder5/5)) %>% # create 1 random point for every 5 people
  st_sf(.) %>% 
  mutate(Group = "Under 5")

NoHSdip_nfhza_pts <- ct_blkgrps_nfhza %>% 
  select(NewLths) %>% 
  filter(NewLths >= 5) %>% 
  st_sample(., size = round(.$NewLths/5)) %>% # create 1 random point for every 5 people
  st_sf(.) %>% 
  mutate(Group = "No HS Dip")

# Bring them together
ct_nfhza_vulnerable <- rbind(NoCarHH_nfhza_pts, 
                             CTLowInc_nfhza_pts, 
                             LimitEngHH_nfhza_pts,
                             Disabled_nfhza_pts,
                             LowInc_nfhza_pts,
                             Minor_nfhza_pts,
                             Over64_nfhza_pts,
                             Under5_nfhza_pts,
                             NoHSdip_nfhza_pts) %>% 
  # slice(sample(1:n())) %>% # randomise order to avoid bias in plotting order
  group_by(Group) %>% 
  summarize()

# clean up pts layers
rm(list = ls(pattern = "_nfhza_pts"))

##### CAN'T GET THIS NSE FUNCTION TO WORK
# Iterate through list of overrepresented populations, create random point layers for each, and then rbind to one layer
# create a vector of variable names over which to iterate
ORfields_tracts <- names(as.data.frame(ct_tracts_nfhza)[60:61])
ORfields_blkgrps <- names(as.data.frame(ct_blkgrps_nfhza)[c(249:251,253:256)])
# create a function to select variable, filter records, and create sample points
polys2points <- function(sf, x){
  x1 <- x
  x <- enquo(x)
  xlabel <- as_label(x)
  sf %>% 
    select(!!x) %>% 
    filter(!!x >= 5) %>% 
    st_sample(., size = !!.data$x/5) %>% 
    st_sf() %>% 
    mutate(Group = quo_name(x))
}

testing <- polys2points(ct_tracts_nfhza, x = NewDisabled)

testing <- subset(ct_tracts_nfhza, NewDisabled >= 5)

# create list of tracts
ORtracts_list <- lapply(ORfields_tracts, FUN = function(x) {
  ct_tracts_nfhza %>%
    select(x) %>%
    filter(x >= 5)
})


# Map over-represented pops and flood zones
start_time <- Sys.time()
tm_layout(bg.color = "#e6f3f7") +
  tm_shape(ct_blkgrp_2775, unit = "mi") + tm_fill(col = "white") +
  tm_shape(ne_states_sf_cb) + tm_fill(col="white") +
  tm_shape(ny_state_sf) + tm_fill(col="white") +
  tm_shape(ct_awater_sf) + tm_fill(col = "#e6f3f7") + 
  tm_shape(ct_nfhza_vulnerable) + 
  tm_dots(col = "darkgoldenrod3", 
          labels = "1 dot = 500 people",
          alpha = 0.6) +
  tm_facets(by = "Group") +
  tm_shape(ct_nfhza_2775_land_simple) + 
  tm_fill(col = "Interval", 
          palette = c("dodgerblue", "blueviolet"), 
          labels = c("1% AEP (100-year)", "0.2% AEP (500-year)"), 
          title = "FEMA Flood Zones",
          alpha = 0.6,
          border.alpha = 0) +
  tm_shape(ne_states_sf_cb) + tm_borders() +
  tm_shape(ny_state_sf) + tm_borders() +
  tm_shape(ct_highways) + tm_lines(col = "seashell4", lwd = 2) +
  tm_shape(I95roadSegment) + 
  tm_symbols(shape = I95, border.lwd = NA, size = 0.25) +
  tm_shape(I95roadSegment2) + 
  tm_symbols(shape = I95, border.lwd = NA, size = 0.25) +
  tm_shape(I395roadSegment) +
  tm_symbols(shape = I395, border.lwd = NA, size = 0.25) +
  tm_shape(I91roadSegment) + 
  tm_symbols(shape = I91, border.lwd = NA, size = 0.25) +
  tm_shape(I84roadSegment) + 
  tm_symbols(shape = I84, border.lwd = NA, size = 0.25) +
  tm_shape(ct_towns_sf_pts) + tm_dots() +
  tm_text("NAME", size = 0.5, col = "black", 
          xmod = 0.7, ymod = 0.2, shadow = TRUE) +
  tm_shape(nofemapolys) + tm_fill(col = "gray", alpha = 0.4) +
  tm_text("LABEL", alpha = 0.5) +
  tm_scale_bar(breaks = c(0, 10, 20), text.size = 0.5, 
               position = c(0.6,0.005)) +
  tm_add_legend(type = "fill", col = "darkgoldenrod3",
                alpha = 0.6,
                border.col = "white", border.alpha = 0,
                labels = "1 dot = 5 people",
                title = "Population\nGroup") +
  tm_layout(title = "Over-represented\nPopulations\nwithin\nFlood Zones", 
            frame = TRUE, main.title.size = 0.8,
            legend.outside = TRUE,
            legend.title.size = 0.8,
            legend.outside.position = c("right", "top"))
end_time <- Sys.time()
end_time - start_time


### Analysis of Hurricane Evacuation Risks ####
# read in hurricane evacuation zone layer
ct_hea_sf <- st_read(dsn = "DATA/FEMA/CT/Hurricane Evacuation Zones (2014)", 
                     layer = "geo_export_ca0640ec-4b94-4f5e-b1be-5a02638d3722") %>% 
  mutate(zone = as.character(zone)) %>% 
  filter(zone %in% c("A","B")) %>% 
  st_transform(., crs = 2775) %>% 
  st_make_valid() %>% 
  mutate(Area = st_area(.))

# read in processed ct_blkgrps and ct_tracts
ct_blkgrps_hevac <- st_read(dsn = "DATA/FEMA/CT", 
                            layer = "ct_blkgrps_hevac") %>% 
  left_join(., as.data.frame(ct_blkgrp_2775), by = "GEOID") %>%
  st_transform(., crs = 2775) %>% 
  mutate(NewArea = st_area(.)) %>%
  st_make_valid()

ct_tracts_hevac <- st_read(dsn = "DATA/FEMA/CT", 
                           layer = "ct_tracts_hevac") %>% 
  left_join(., as.data.frame(ct_tracts_2775), by = "GEOID") %>%
  st_transform(., crs = 2775) %>% 
  mutate(NewArea = st_area(.)) %>% 
  st_make_valid()

# Apportion populations based on geographic proportion of intersect
ct_blkgrps_hevac <- ct_blkgrps_hevac %>%
  mutate(CT_LOWINC = if_else(CT_INCOME == "I",totalpopE,0)) %>%
  mutate(CT_LOWINC = replace_na(CT_LOWINC,0)) %>%
  mutate(Proportion = as.numeric(NewArea/OldArea),
         NewPop = totalpopE*Proportion,
         NewMinority = minorityE*Proportion,
         NewUnder5 = under5E*Proportion,
         NewOver64 = over64E*Proportion,
         NewUnder18 = under18E*Proportion,
         NewEng_limit = eng_limitE*Proportion,
         NewPov = num2povE*Proportion,
         NewLths = lthsE*Proportion,
         NewCT_LOWINC = CT_LOWINC*Proportion)

ct_tracts_hevac <- ct_tracts_hevac %>%
  mutate(Proportion = as.numeric(NewArea/OldArea),
         NewDisabled = disabledOver18E*Proportion,
         NewNoCar = HHnoCarE*Proportion)

# Compute total populations within hurricane evac zones
ct_hevac_blkgrp_df <- ct_blkgrps_hevac %>%
  as.data.frame() %>%
  summarize(`Total Pop` = as.integer(sum(NewPop)),
            Minority = as.integer(sum(NewMinority)),
            `Under 5` = as.integer(sum(NewUnder5)),
            `Over 64` = as.integer(sum(NewOver64)),
            `Under 18` = as.integer(sum(NewUnder18)),
            `Limited English HH` = as.integer(sum(NewEng_limit)),
            `Low Income` = as.integer(sum(NewPov)),
            `No HS Dip` = as.integer(sum(NewLths)),
            `CT Low Income` = as.integer(sum(NewCT_LOWINC))) %>%
  gather(key = Group, value = HevacPop)

ct_hevac_tracts_df <- ct_tracts_hevac %>%
  as.data.frame() %>%
  summarize(`Disabled` = as.integer(sum(NewDisabled)),
            `No Car HH` = as.integer(sum(NewNoCar))) %>%
  gather(key = Group, value = HevacPop)

# Compute total tract populations within the state for same groups
ct_tract_hevac_pops_df <- ct_tracts_2775 %>%
  as.data.frame() %>%
  summarize(`Disabled` = sum(disabledOver18E),
            `No Car HH` = sum(HHnoCarE)) %>%
  gather(key = Group, value = CTPop) %>%
  left_join(.,ct_hevac_tracts_df, by = "Group")

# Compute populations for state,  and join with hurricane evac pops
ct_HevacPops_df <- ct_blkgrp_2775 %>%
  as.data.frame() %>%
  mutate(CT_LOWINC = if_else(CT_INCOME == "I",totalpopE,0)) %>%
  mutate(CT_LOWINC = replace_na(CT_LOWINC,0)) %>%
  summarize(`Total Pop` = sum(totalpopE),
            Minority = sum(minorityE),
            `Under 5` = sum(under5E),
            `Over 64` = sum(over64E),
            `Under 18` = sum(under18E),
            `Limited English HH` = sum(eng_limitE),
            `Low Income` = sum(num2povE),
            `No HS Dip` = sum(lthsE),
            `CT Low Income` = sum(CT_LOWINC, na.rm = TRUE)) %>%
  gather(key = Group, value = CTPop) %>%
  left_join(., ct_hevac_blkgrp_df, by = "Group") %>%
  rbind(.,ct_tract_hevac_pops_df) %>%
  mutate(PctHevac = HevacPop/CTPop*100)

# Create a simplified version of NFHZA polygons to speed up mapping
ct_hea_sf_simple <- ct_hea_sf %>% 
  select(zone) %>% 
  st_cast(., "MULTIPOLYGON") %>% # homogenize type
  st_cast(., "POLYGON") %>% 
  st_simplify(., dTolerance = 10) %>% # reduce vertices
  st_make_valid() %>%
  group_by(zone) %>% 
  summarize() %>% 
  mutate(Area = st_area(.))

# Map totalpop and hurricane evacuation zones
start_time <- Sys.time()
tm_layout(bg.color = "#e6f3f7") +
  tm_shape(ct_blkgrp_2775, unit = "mi") + tm_fill(col = "white") +
  tm_shape(ne_states_sf_cb) + tm_fill(col="white") +
  tm_shape(ny_state_sf) + tm_fill(col="white") +
  tm_shape(ct_awater_sf) + tm_fill(col = "#e6f3f7") + 
  tm_shape(ne_states_sf_cb) + tm_borders() +
  tm_shape(ny_state_sf) + tm_borders() +
  tm_shape(ct_totalpop_pts) + 
  tm_dots(col = "darkgoldenrod3",
          labels = "1 dot = 500 people",
          alpha = 0.6) +
  tm_shape(ct_hea_sf_simple) + 
  tm_fill(col = "zone", 
          palette = c("deeppink", "purple3"), 
          labels = c("A: Category 1 - 2", 
                     "B: Category 3 - 4"), 
          title = "Evacuation Zone and\nHurricane Category",
          alpha = 0.6,
          border.alpha = 0) +
  tm_shape(ne_states_sf_cb) + tm_borders() +
  tm_shape(ny_state_sf) + tm_borders() +
  tm_shape(ct_highways) + tm_lines(col = "seashell4", lwd = 2) +
  tm_shape(I95roadSegment) + 
  tm_symbols(shape = I95, border.lwd = NA, size = 0.25) +
  tm_shape(I95roadSegment2) + 
  tm_symbols(shape = I95, border.lwd = NA, size = 0.25) +
  tm_shape(I395roadSegment) +
  tm_symbols(shape = I395, border.lwd = NA, size = 0.25) +
  tm_shape(I91roadSegment) + 
  tm_symbols(shape = I91, border.lwd = NA, size = 0.25) +
  tm_shape(I84roadSegment) + 
  tm_symbols(shape = I84, border.lwd = NA, size = 0.25) +
  tm_shape(ct_towns_sf_pts) + tm_dots() +
  tm_text("NAME", size = 0.5, col = "black", 
          xmod = 0.7, ymod = 0.2, shadow = TRUE) +
  tm_scale_bar(breaks = c(0, 10, 20), text.size = 0.5, 
               position = c(0.6,0.005)) +
  tm_add_legend(type = "fill", col = "darkgoldenrod3",
                alpha = 0.6,
                border.col = "white", border.alpha = 0,
                labels = "1 dot = 500 people",
                title = "Total Population") +
  tm_layout(title = "Population Distribution \nand Hurricane\nEvacuation Zones", 
            frame = TRUE, main.title.size = 0.8,
            legend.outside = TRUE,
            legend.title.size = 0.8,
            legend.outside.position = c("right", "top"))
end_time <- Sys.time()
end_time - start_time 

# Total and percentage of land area of RI within hurricane evacuation zones
ct_hea_sf %>% 
  as.data.frame() %>% 
  group_by(zone) %>% 
  summarize(SqKm = round(as.numeric(sum(Area)/10^6),1), 
            SqMi = round(as.numeric(SqKm/2.59),1),
            PctArea = paste0(as.character(round(as.numeric(sum(Area)/ct_area*100),1)),"%")) %>% 
  kableExtra::kable(longtable = T, booktabs = T,
                    format.args = list(big.mark = ','),
                    caption = "Connecticut Land Area within Hurricane Evacuation Zones",
                    digits = 0, align = "r") %>% 
  kableExtra::kable_styling(latex_options = c("repeat_header"))

# Show table of pops within hurricane evacuation zones
ct_HevacPops_df %>% 
  mutate(PctHevac = paste0(as.character(round(PctHevac,1)),"%")) %>% 
  arrange(-HevacPop) %>% 
  kableExtra::kable(longtable = T, booktabs = T,
                    format.args = list(big.mark = ','),
                    caption = "Connecticut Populations within Hurricane Evacuation Zones",
                    digits = 0, align = "r") %>% 
  kableExtra::kable_styling(latex_options = c("repeat_header"))

# Create lollipop plot of pops within flood zones
ct_HevacPops_df %>% 
  ggplot(aes(x = reorder(Group,PctHevac), 
             y = PctHevac)) +
  geom_segment(aes(x = reorder(Group,PctHevac), 
                   xend = reorder(Group,PctHevac),
                   y = ct_HevacPops_df[1,4], yend = PctHevac), 
               color = "skyblue") +
  geom_point(color = "blue", size = 4, alpha = 0.8) +
  coord_flip() + xlab("") + ylab("") + 
  ggtitle("Connecticut Populations within Hurricane Evacuation Zones") + theme_light() +
  theme(panel.grid.major.y = element_blank(),
        panel.border = element_blank(),
        axis.ticks.y = element_blank()) +
  geom_text(aes(x = Group, y = PctHevac + 0.2 * sign(PctHevac), 
                label = paste0(round(PctHevac,1),"%")), 
            hjust = 1.8, vjust = -1, size = 3,
            color=rgb(100,100,100, maxColorValue=255)) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  geom_hline(yintercept = ct_HevacPops_df[1,4], linetype = "dashed") +
  geom_text(aes(x = "Over 64", y = 10.5, label = "Above state avg"),
            color = "gray48") +
  geom_segment(aes(x = "Total Pop", xend = "Total Pop", y = 9.7, yend = 10.5), 
               arrow = arrow(length = unit(0.3,"cm"))) +
  expand_limits(y = c(9,14.5))

# Create a dot density map of over-represented populations in hurricane evacuation zones
# Create random points, with 1 point for every 5 people
NoCarHH_hevac_pts <- ct_tracts_hevac %>% 
  dplyr::select(NewNoCar) %>% 
  filter(NewNoCar >= 5) %>% 
  st_sample(., size = round(.$NewNoCar/5)) %>% 
  st_sf(.) %>% 
  mutate(Group = "No Car HH")

CTLowInc_hevac_pts <- ct_blkgrps_hevac %>% 
  select(NewCT_LOWINC) %>% 
  filter(NewCT_LOWINC >= 5) %>% 
  st_sample(., size = round(.$NewCT_LOWINC/5)) %>% # create 1 random point for every 5 people
  st_sf(.) %>% 
  mutate(Group = "CT Low Income")

LimitEngHH_hevac_pts <- ct_blkgrps_hevac %>% 
  select(NewEng_limit) %>% 
  filter(NewEng_limit >= 5) %>% 
  st_sample(., size = round(.$NewEng_limit/5)) %>% # create 1 random point for every 5 people
  st_sf(.) %>% 
  mutate(Group = "Limited English HH")

Disabled_hevac_pts <- ct_tracts_hevac %>% 
  dplyr::select(NewDisabled) %>% 
  filter(NewDisabled >= 5) %>% 
  st_sample(., size = round(.$NewDisabled/5)) %>% 
  st_sf(.) %>% 
  mutate(Group = "Disabled")

LowInc_hevac_pts <- ct_blkgrps_hevac %>% 
  select(NewPov) %>% 
  filter(NewPov >= 5) %>% 
  st_sample(., size = round(.$NewPov/5)) %>% # create 1 random point for every 5 people
  st_sf(.) %>% 
  mutate(Group = "Low Income")

Minor_hevac_pts <- ct_blkgrps_hevac %>% 
  select(NewMinority) %>% 
  filter(NewMinority >= 5) %>% 
  st_sample(., size = round(.$NewMinority/5)) %>% # create 1 random point for every 5 people
  st_sf(.) %>% 
  mutate(Group = "Minority")

Over64_hevac_pts <- ct_blkgrps_hevac %>% 
  dplyr::select(NewOver64) %>% 
  filter(NewOver64 >= 5) %>% 
  st_sample(., size = round(.$NewOver64/5)) %>% # create 1 random point for every 5 people
  st_sf(.) %>% 
  mutate(Group = "Over 64")

Under5_hevac_pts <- ct_blkgrps_hevac %>% 
  select(NewUnder5) %>% 
  filter(NewUnder5 >= 5) %>% 
  st_sample(., size = round(.$NewUnder5/5)) %>% # create 1 random point for every 5 people
  st_sf(.) %>% 
  mutate(Group = "Under 5")

NoHSdip_hevac_pts <- ct_blkgrps_hevac %>% 
  select(NewLths) %>% 
  filter(NewLths >= 5) %>% 
  st_sample(., size = round(.$NewLths/5)) %>% # create 1 random point for every 5 people
  st_sf(.) %>% 
  mutate(Group = "No HS Dip")

# Bring them together
ct_hevac_vulnerable <- rbind(NoCarHH_hevac_pts, 
                             CTLowInc_hevac_pts, 
                             LimitEngHH_hevac_pts,
                             Disabled_hevac_pts,
                             LowInc_hevac_pts,
                             Minor_hevac_pts,
                             Over64_hevac_pts,
                             Under5_hevac_pts,
                             NoHSdip_hevac_pts) %>% 
  # slice(sample(1:n())) %>% # randomise order to avoid bias in plotting order
  group_by(Group) %>% 
  summarize()

# clean up pts layers
rm(list = ls(pattern = "_hevac_pts"))

# Map over-represented pops and hurricane evacuation zones
start_time <- Sys.time()
tm_layout(bg.color = "#e6f3f7") +
  tm_shape(ct_blkgrp_2775, unit = "mi") + tm_fill(col = "white") +
  tm_shape(ne_states_sf_cb) + tm_fill(col="white") +
  tm_shape(ny_state_sf) + tm_fill(col="white") +
  tm_shape(ct_awater_sf) + tm_fill(col = "#e6f3f7") + 
  tm_shape(ct_hevac_vulnerable) + 
  tm_dots(col = "darkgoldenrod3", 
          labels = "1 dot = 500 people",
          alpha = 0.6) +
  tm_facets(by = "Group") +
  tm_shape(ct_hea_sf_simple) + 
  tm_fill(col = "zone", 
          palette = c("deeppink", "purple3"), 
          labels = c("A: Category 1 - 2", 
                     "B: Category 3 - 4"), 
          title = "Evacuation Zone and\nHurricane Category",
          alpha = 0.6,
          border.alpha = 0) +
  tm_shape(ne_states_sf_cb) + tm_borders() +
  tm_shape(ny_state_sf) + tm_borders() +
  tm_shape(ct_highways) + tm_lines(col = "seashell4", lwd = 2) +
  tm_shape(I95roadSegment) + 
  tm_symbols(shape = I95, border.lwd = NA, size = 0.25) +
  tm_shape(I95roadSegment2) + 
  tm_symbols(shape = I95, border.lwd = NA, size = 0.25) +
  tm_shape(I395roadSegment) +
  tm_symbols(shape = I395, border.lwd = NA, size = 0.25) +
  tm_shape(I91roadSegment) + 
  tm_symbols(shape = I91, border.lwd = NA, size = 0.25) +
  tm_shape(I84roadSegment) + 
  tm_symbols(shape = I84, border.lwd = NA, size = 0.25) +
  tm_shape(ct_towns_sf_pts) + tm_dots() +
  tm_text("NAME", size = 0.5, col = "black", 
          xmod = 0.7, ymod = 0.2, shadow = TRUE) +
  tm_scale_bar(breaks = c(0, 10, 20), text.size = 0.5, 
               position = c(0.6,0.005)) +
  tm_add_legend(type = "fill", col = "darkgoldenrod3",
                alpha = 0.6,
                border.col = "white", border.alpha = 0,
                labels = "1 dot = 5 people",
                title = "Population\nGroup") +
  tm_layout(title = "Over-represented\nPopulations\nwithin Hurricane\nEvacuation Zones", 
            frame = TRUE, main.title.size = 0.8,
            legend.outside = TRUE,
            legend.title.size = 0.8,
            legend.outside.position = c("right", "top"))
end_time <- Sys.time()
end_time - start_time