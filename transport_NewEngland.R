# New England transportation analysis
# library(RCurl)
# url <- "ftp://pubftp.nh.gov/DOT/Planning%20and%20Community%20Assistance/Road%20Data/NHDOT_GIS_2019_0404.gdb/"
# filenames = getURL(url, ftp.use.epsv = FALSE, dirlistonly = TRUE)
# filenames <- strsplit(filenames, "\r\n")
# filenames = unlist(filenames)
# filenames
# for (filename in filenames) {
#   download.file(paste(url, filename, sep = ""), 
#                 paste(getwd(),
#                       "/DATA/transport/transportNHDOT_GIS_2019_0404", 
#                       filename, sep = ""))
# }

library(tidyverse)
library(sf)
library(tmap)
library(tidytransit)

load("DATA/ne_layers.rds")

# Look at feedlist of available GTFS
view(feedlist)
feedlist_sf <- st_as_sf(feedlist,
                        coords = c("loc_lng","loc_lat"),
                        crs = 4326)
tm_shape(feedlist_sf) + tm_dots(col = "red")


### Read in CT transit data. MISSING ABOUT 9 SMALLER TRANSIT SERVICES. 
# read in GTFS for CTransit for Hartford-New Haven-Stamford-New Britain-Waterbury-Meriden
cttransit <- read_gtfs("https://www.cttransit.com/sites/default/files/gtfs/googlect_transit.zip") %>% 
  gtfs_as_sf()
# read in GTFS for Greater Bridgeport Transit
ctgbtransit <- feedlist %>% 
  filter(t == "Greater Bridgeport Transit GTFS") %>% 
  pull(url_d) %>% 
  read_gtfs() %>% 
  gtfs_as_sf()
# read in GTFS for Estuary Transit District d.b.a. 9 Town Transit
ct9towntransit <- feedlist %>% 
  filter(t == "9 Town Transit GTFS") %>% 
  pull(url_d) %>% 
  read_gtfs() %>% 
  gtfs_as_sf()
# read in Shoreline East commuter rail
ctshorelineeast <- read_gtfs("https://shorelineeast.com/google_transit.zip") %>% 
  gtfs_as_sf()
# read in Hartford Line commuter rail
cthartfordlinerail <- read_gtfs("http://www.hartfordline.com/files/gtfs/gtfs.zip") %>% 
  gtfs_as_sf()
summary(cttransit)
summary(ctgbtransit)
summary(ct9towntransit)
summary(ctshorelineeast)
names(cttransit)
head(cttransit$stops)
head(cttransit$shapes)
# Extract df of stops
cttransit_stops_df <- cttransit$stops
# Create a point layer
# cttransit_sf <- st_as_sf(cttransit_stops_df, 
#                          coords = c("stop_lon","stop_lat"),
#                          crs = 4326)

# Map it out
tmap_mode("view")
tm_shape(cttransit$shapes) + tm_lines(col = "red") + 
  tm_shape(ctgbtransit$shapes) + tm_lines(col = "green") + 
  tm_shape(ct9towntransit$shapes) + tm_lines(col = "blue") +
  tm_shape(ctshorelineeast$stops) + tm_dots() +
  tm_shape(cthartfordlinerail$stops) + tm_dots(col = "gray")


### Analysis of Rhode Island transportation options
library(tidyverse)
library(sf)
library(tmap)
library(tidytransit)
library(sp)
library(tigris)
options(tigris_use_cache = TRUE, tigris_class = "sf")
library(lubridate)
library(maptools)
library(spdep)

load("DATA/ne_layers.rds")

### Read in RI transit data. See http://www.rigis.org/
# read in RIPTA bus routes
ri_busroutes_sf <- st_read(dsn = "DATA/transport/RIPTA_Bus_Routes",
                           layer = "RIPTA_Bus_Routes")
# read in RIPTA bus stops
ri_busstops_sf <- st_read(dsn = "DATA/transport/RIPTA_Bus_Stops",
                          layer = "RIPTA_Bus_Stops")
# read in RI ferry routes
ri_ferryroutes_sf <- st_read(dsn = "DATA/transport/RI_Ferry_Routes",
                             layer = "Ferry_Routes")
# read in RI railroad rights of way
ri_railrow_sf <- st_read(dsn = "DATA/transport/RI_Railroad_Rights_of_Way",
                         layer = "Railroad_Rights_of_Way") %>% 
  filter(STATUS == "Active" & RAILUSE == "Passenger")
# read in RI park and ride stops
ri_parknride_sf <- st_read(dsn = "DATA/transport/RIPTA_Park_and_Ride_Stops",
                           layer = "RIPTA_Park_and_Ride_Stops")

# Download Census TIGERLine roads for RI
## First, extract list of county names to use with tigris::roads
ri_counties <- counties("RI") %>% 
  pull(NAME)
# Next, download roads for each county and rbind to one layer
ri_roads_sf <- rbind_tigris(
  lapply(
    ri_counties, function(x) roads(state = "RI", county = x)
  )
)

# Convert bus stops, roads, and block groups to projected local CRS EPSG:2840: NAD83(HARN) / Rhode Island
ri_roads_sf <- st_transform(ri_roads_sf, crs = 2840) 
# as_Spatial() %>% 
# SpatialLinesNetwork()
ri_busstops_sf <- st_transform(ri_busstops_sf, crs = 2840) 
# ri_busstops_sp <- as_Spatial(ri_busstops_sf) 
ri_blkgrps_sf <- ne_blkgrp_sf %>% 
  filter(STATE == "Rhode Island") %>% 
  st_transform(., crs = 2840)
ri_tracts_sf <- ne_tracts_sf %>% 
  filter(STATE == "Rhode Island") %>% 
  st_transform(., crs = 2840)
# ri_blkgrps_sp <- as_Spatial(ri_blkgrps_sf)
# Get rid of empty geometries
empty_geo <- st_is_empty(ri_blkgrps_sf)
ri_blkgrps_sf <- ri_blkgrps_sf[!empty_geo,]
empty_geo <- st_is_empty(ri_tracts_sf)
ri_tracts_sf <- ri_tracts_sf[!empty_geo,]
# clean up
rm(list = ls(pattern = "ne_"))

# Create a 400m buffer around bus stops
ri_busBuff400m <- st_buffer(ri_busstops_sf,dist = 400) %>% 
  st_union() %>% 
  st_as_sf()

# Use areal interpolation to calculate populations of concern within buffer of accessibility
ri_busBuff400m_sf <- ri_blkgrps_sf %>% 
  mutate(RI_LOWINC = if_else(RI_INCOME == "I",totalpopE,0)) %>% 
  mutate(RI_LOWINC = replace_na(RI_LOWINC,0)) %>% 
  mutate(RI_MINORITIES = if_else(RI_MINORITY == "M",totalpopE,0)) %>% 
  mutate(RI_MINORITIES = replace_na(RI_MINORITIES,0)) %>% 
  select(GEOID,
         totalpopE,
         minorityE,
         under5E,
         over64E,
         under18E,
         eng_limitE,
         num2povE,
         lthsE,
         RI_LOWINC,
         RI_MINORITIES) %>% 
  mutate(OldArea = st_area(.)) %>% 
  st_intersection(ri_busBuff400m,.) %>% 
  mutate(NewArea = st_area(.),
         Proportion = NewArea/OldArea,
         NewPop = as.integer(totalpopE*Proportion),
         NewMinority = as.integer(minorityE*Proportion),
         NewUnder5 = as.integer(under5E*Proportion),
         NewOver64 = as.integer(over64E*Proportion),
         NewUnder18 = as.integer(under18E*Proportion),
         NewEng_limit = as.integer(eng_limitE*Proportion),
         NewPov = as.integer(num2povE*Proportion),
         NewLths = as.integer(lthsE*Proportion),
         NewRI_LOWINC = as.integer(RI_LOWINC*Proportion),
         NewRI_MINORITIES = as.integer(RI_MINORITIES))

# Repeat for tracts
ri_busBuff400mTracts_sf <- ri_tracts_sf %>% 
  select(GEOID,
         disabledOver18E,
         HHnoCarE) %>% 
  mutate(OldArea = st_area(.)) %>% 
  st_intersection(ri_busBuff400m,.) %>% 
  mutate(NewArea = st_area(.),
         Proportion = NewArea/OldArea,
         NewDisabled = as.integer(disabledOver18E*Proportion),
         NewNoCar = as.integer(HHnoCarE*Proportion))

# Compute total block group populations within bus stop buffer
ri_busBuff400m_df <- ri_busBuff400m_sf %>% 
  as.data.frame() %>% 
  summarize(`Total Pop` = sum(NewPop),
            Minority = sum(NewMinority),
            `Under 5` = sum(NewUnder5),
            `Over 64` = sum(NewOver64),
            `Under 18` = sum(NewUnder18),
            `Limited English HH` = sum(NewEng_limit),
            `Low Income` = sum(NewPov),
            `No HS Dip` = sum(NewLths),
            `RI Low Income` = sum(NewRI_LOWINC),
            `RI Minority` = sum(NewRI_MINORITIES)) %>% 
  gather(key = Group, value = BusPop)

# Compute total tract populations within bus stop buffer
ri_busBuff400mTracts_df <- ri_busBuff400mTracts_sf %>% 
  as.data.frame() %>% 
  summarize(`Disabled` = sum(NewDisabled),
            `No Car HH` = sum(NewNoCar)) %>% 
  gather(key = Group, value = BusPop)
# Compute total tract populations within the state for same groups
ri_tract_pops_df <- ri_tracts_sf %>% 
  as.data.frame() %>% 
  summarize(`Disabled` = sum(disabledOver18E),
            `No Car HH` = sum(HHnoCarE)) %>% 
  gather(key = Group, value = RIPop) %>% 
  left_join(.,ri_busBuff400mTracts_df, by = "Group")
  

# Compute populations for state, join with buffer pops, and create plot
ri_busAccessPops_df <- ri_blkgrps_sf %>% 
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
  left_join(., ri_busBuff400m_df, by = "Group") %>% 
  rbind(.,ri_tract_pops_df) %>% 
  mutate(PctBus = BusPop/RIPop*100)

# Create bar plot
ri_busAccessPops_df %>% 
  ggplot(aes(x = reorder(Group,PctBus), 
             y = PctBus, fill = Group, alpha = 0.9)) +
  geom_bar(stat = "identity") +
  coord_flip() + xlab("") + ylab("") + ggtitle("Rhode Island Residents within Walking Distance (400m) \nof Bus Stops") + theme_minimal() +
  theme(legend.position = 'none') +
  geom_text(aes(x = Group, y = PctBus + 0.2 * sign(PctBus), 
                label = paste0(round(PctBus,0),"%")), 
            hjust = 1.1, size = 3,
            color=rgb(100,100,100, maxColorValue=255)) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  geom_hline(yintercept = 51.34, linetype = "dashed") 

# Create lollipop plot
ri_busAccessPops_df %>% 
  ggplot(aes(x = reorder(Group,-PctBus), 
             y = PctBus)) +
  geom_segment(aes(x = reorder(Group,-PctBus), xend = reorder(Group,-PctBus),
                   y = ri_busAccessPops_df[1,4], yend = PctBus), 
               color = "skyblue") +
  geom_point(color = "blue", size = 4, alpha = 0.6) +
  coord_flip() + xlab("") + ylab("") + ggtitle("Rhode Island Populations within Walking Distance (400m) \nof Bus Stops") + theme_light() +
  theme(panel.grid.major.y = element_blank(),
        panel.border = element_blank(),
        axis.ticks.y = element_blank()) +
  geom_text(aes(x = Group, y = PctBus + 0.2 * sign(PctBus), 
                label = paste0(round(PctBus,0),"%")), 
            hjust = -0.5, size = 3,
            color=rgb(100,100,100, maxColorValue=255)) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) 

# See table of same
ri_busAccessPops_df %>% 
  mutate(`% Bus Access` = paste0(round(PctBus,1),"%")) %>% 
  select(-PctBus)


# Read in RI GTFS data
ri_transit <- read_gtfs("https://www.ripta.com/stuff/contentmgr/files/0/3cda81dfa140edbe9aae214b26245b4a/files/google_transit.zip") %>% 
  gtfs_as_sf()
summary(ri_transit)

# add a table to the feed that indicates which service_id runs on which date. This is later useful for linking dates and trips via service_id.
ri_transit <- set_date_service_table(ri_transit)
head(ri_transit$.$date_service_table)

# To understand service patterns better we need information on weekdays and holidays. With a calendar table we know the weekday and possible holidays for each date.
holidays = tribble(~date, ~holiday,
                   ymd("2020-07-04"), "Independence Day",
                   ymd("2020-09-07"), "Labor Day",
                   ymd("2020-12-25"), "Christmas")

calendar = tibble(date = unique(ri_transit$.$date_service_table$date)) %>% 
  mutate(
    weekday = (function(date) {
      c("Sunday", "Monday", "Tuesday", 
        "Wednesday", "Thursday", "Friday", 
        "Saturday")[as.POSIXlt(date)$wday + 1]
    })(date)
  )

calendar <- calendar %>% left_join(holidays, by = "date")
head(calendar)

# To analyse on which dates trips run and to group similar services we use service patterns. Such a pattern simply lists all dates a trip runs on. To handle these patterns we create a servicepattern_id using a hash function. 
ri_transit <- set_servicepattern(ri_transit)
# Our gtfs feed now contains the data frame service_pattern which links each servicepattern_id to an existing service_id (and by extension trip_id).
head(ri_transit$.$service_pattern)

# compare the number of service patterns to the number of services.
# service ids used
(n_services <- length(unique(ri_transit$trips$service_id)))
# unique date patterns
(n_servicepatterns <- length(unique(ri_transit$.$service_pattern$servicepattern_id)))

# Understand patterns of service by visualising the data.
date_servicepattern_table <- ri_transit$.$date_servicepattern_table %>% 
  left_join(calendar, by = "date")

ggplot(date_servicepattern_table) + theme_bw() +
  geom_point(aes(x = date, y = servicepattern_id, color = weekday), size = 1) +
  scale_x_date(breaks = scales::date_breaks("1 month")) +
  theme(legend.position = "bottom")

# Use service pattern id for daily M-F service selected based on graphic
service_ids <- ri_transit$.$service_pattern %>% 
  filter(servicepattern_id == "s_33b3c49") %>% 
  pull(service_id)
head(service_ids)

# service_id’s are also a field on the trips table, which describes all the trips taken in the system. See how many trips fall under each of these service_id’s on the trips table, and how they relate to routes.
ri_transit$trips %>%
  filter(service_id %in% service_ids) %>%
  group_by(service_id, route_id) %>%
  summarise(count = n())

# now that we’ve identified the set of service_id’s that refer to all weekday trips, we can summarize service between 6 am and 5 pm for bus service on weekdays.
daily_stop_freq <- get_stop_frequency(ri_transit, start_hour = 6, end_hour = 21,
                                      service_ids = service_ids)
head(daily_stop_freq)

# Convert stops to points for mapping
ri_transit_stops_sf <- stops_as_sf(ri_transit$stops)

# Join headway frequencies to stops
ri_transit_stops_sf <- ri_transit_stops_sf %>% 
  inner_join(daily_stop_freq, by = "stop_id")

# map it out
tmap_mode("view")
tm_shape(ri_transit_stops_sf) + tm_dots(col = "headway", alpha = 0.6)

# use the get_route_frequency function to summarise transit service by route, for the same time period.
daily_route_freq <- get_route_frequency(ri_transit, service_ids = service_ids,
                                        start_hour = 6, end_hour = 21)
head(daily_route_freq)

# Join the route frequencies to geometry for mapping
# get_route_geometry needs a gtfs object that includes shapes as simple feature data frames
ri_routes_sf <- get_route_geometry(ri_transit, service_ids = service_ids)

# join calculated frequencies to geometry
ri_routes_sf <- ri_routes_sf %>% 
  inner_join(daily_route_freq, by = "route_id")

# map it out
tmap_mode("view")
tm_shape(ri_routes_sf) + tm_lines(col = "mean_headways")

# Calculate average headway for block groups within walking distance of bus stops and then compute weighted average headway by population group
ri_transit_stopHeadway_df <- ri_transit_stops_sf %>% 
  st_transform(., crs = 2840) %>% 
  st_join(., ri_busBuff400m_sf) %>% 
  as.data.frame() %>% 
  group_by(GEOID) %>% 
  summarize(AvgStopHeadway = mean(headway,na.rm = TRUE))
# Do the same for tracts
ri_transit_stopHeadwayTracts_df <- ri_transit_stops_sf %>% 
  st_transform(., crs = 2840) %>% 
  st_join(., ri_busBuff400mTracts_sf) %>% 
  as.data.frame() %>% 
  group_by(GEOID) %>% 
  summarize(AvgStopHeadway = mean(headway,na.rm = TRUE)) %>% 
  as.data.frame() %>% 
  left_join(ri_busBuff400mTracts_sf,.,by="GEOID") %>% 
  as.data.frame() %>% 
  transmute(Disabled = disabledOver18E,
            `No Car HH` = HHnoCarE,
            AvgStopHeadway) %>% 
  gather(key = Group, value = Pop, Disabled:`No Car HH`) %>% 
  group_by(Group) %>% 
  summarize(AvgWHeadway = weighted.mean(x = AvgStopHeadway, 
                                        w = Pop, na.rm = TRUE))

# Join stop average headway to block groups within buffer
ri_busHeadwayPops_df <- ri_busBuff400m_sf %>% 
  as.data.frame() %>% 
  left_join(.,ri_transit_stopHeadway_df, by = "GEOID") %>% 
  transmute(`Total Pop` = NewPop,
            Minority = NewMinority,
            `Under 5` = NewUnder5,
            `Over 64` = NewOver64,
            `Under 18` = NewUnder18,
            `English Limited HH` = NewEng_limit,
            `Low Income` = NewPov,
            `No HS Dip` = NewLths,
            `RI Low Income` = NewRI_LOWINC,
            `RI Minority` = NewRI_MINORITIES,
            AvgStopHeadway) %>% 
  gather(key = Group, value = Pop, `Total Pop`:`RI Minority`) %>% 
  group_by(Group) %>% 
  summarize(AvgWHeadway = weighted.mean(x = AvgStopHeadway, 
                                        w = Pop, na.rm = TRUE)) %>% 
  rbind(.,ri_transit_stopHeadwayTracts_df)

# Create lollipop graph of headway
ri_busHeadwayPops_df %>% 
  ggplot(aes(x = reorder(Group,-AvgWHeadway), y = AvgWHeadway)) +
  geom_segment(aes(x = reorder(Group,-AvgWHeadway), 
                   xend = reorder(Group,-AvgWHeadway),
                   y = pull(ri_busHeadwayPops_df[8,2]), yend = AvgWHeadway), 
               color = "tan1") +
  geom_point(color = "orange", size = 4, alpha = 0.6) +
  coord_flip() + xlab("") + ylab("minutes") + ggtitle("Population-Weighted Average Stop Headway for Rhode Island \nGroups within Walking Distance (400m) of Bus Stops") + theme_light() +
  theme(panel.grid.major.y = element_blank(),
        panel.border = element_blank(),
        axis.ticks.y = element_blank()) +
  geom_text(aes(x = Group, y = AvgWHeadway + 0.2 * sign(AvgWHeadway), 
                label = round(AvgWHeadway,0)), 
            hjust = 1.8, size = 3,
            color=rgb(100,100,100, maxColorValue=255))


# create maps of transit dependent populations
tm_shape(ri_busroutes_sf) + tm_lines(col = "red") +
  # tm_shape(ri_busstops_sf) + tm_dots(col = "brown4") +
  tm_shape(ri_ferryroutes_sf) + tm_lines(col = "blue") +
  tm_shape(ri_railrow_sf) + tm_lines(col = "black") +
  tm_shape(ri_parknride_sf) + tm_dots(col = "green")


# Analyze Walkability Index for RI. See https://edg.epa.gov/metadata/catalog/search/resource/details.page?uuid=%7B251AFDD9-23A7-4068-9B27-A3048A7E6012%7D
# Import walkability layer
walkability <- st_read(dsn = "DATA/Walkability/Natl_WI_SHP",
        layer = "Natl_WI") 

ri_blkgrp_walkability_sf <- walkability %>% 
  as.data.frame() %>% 
  select(GEOID10,SFIPS,CFIPS,TRFIPS,NatWalkInd) %>% 
  left_join(ri_blkgrps_sf,., by = c("GEOID" = "GEOID10"))

# Create summarized df of population weighted avg walkability by tract pop
ri_tract_walkability_df <- ri_blkgrp_walkability_sf %>% 
  as.data.frame() %>% 
  select(SFIPS,CFIPS,TRFIPS,NatWalkInd) %>% 
  mutate(TRACTID = paste0(SFIPS,CFIPS,TRFIPS)) %>% 
  group_by(TRACTID) %>% 
  summarize(NatWalkInd = mean(NatWalkInd)) %>% 
  left_join(as.data.frame(ri_tracts_sf),., by = c("GEOID" = "TRACTID")) %>% 
  transmute(Disabled = disabledOver18E,
            `No Car HH` = HHnoCarE,
            NatWalkInd) %>% 
  gather(key = Group, value = Pop, Disabled:`No Car HH`) %>% 
  group_by(Group) %>% 
  summarize(wNatWalkIndex = weighted.mean(NatWalkInd, Pop, na.rm = TRUE))



# Create a map of walkability
tm_shape(ri_blkgrp_walkability_sf) + tm_fill("NatWalkInd")


# Hot spot map of walkability
# Get rid of empty geometries and NAs, and convert to spdf
empty_geo <- st_is_empty(ri_blkgrp_walkability_sf)
ri_blkgrp_walkability_sp <- ri_blkgrp_walkability_sf[!empty_geo,] %>% 
  dplyr::select(GEOID,NatWalkInd) %>% 
  # st_transform(., crs = 2163) %>% # convert to US National Atlas Equal Area
  na.omit() %>% 
  as_Spatial()
# Calculate Queen's case neighbors
neighborsQC <- poly2nb(ri_blkgrp_walkability_sp, queen = TRUE)
# Compute neighbor weights
# spdep::set.ZeroPolicyOption(TRUE)
listw <- nb2listw(neighborsQC, style = "W", zero.policy = TRUE)
# compute Getis-Ord Gi statistic
local_g <- localG(ri_blkgrp_walkability_sp$NatWalkInd, listw)
local_g <- cbind(ri_blkgrp_walkability_sp, as.matrix(local_g))
names(local_g)[3] <- "gstat"
# map the results
tm_shape(local_g, unit = "mi",) + 
  tm_fill("gstat",
          palette = "-RdBu", 
          style = "pretty",
          title = expression(paste("Getis-Ord ", G[i]^"*")),
          showNA = FALSE,
          midpoint = NA,
          labels = c("Significant Clusters","of Low Values","No Significant Clusters","Significant Clusters","of High Values")) +
  tm_scale_bar(breaks = c(0, 5, 10), text.size = 0.5, 
               position = c(0.6,0.005)) +
  tm_layout(title = "Hot Spot Map \nof Walkability Index for \nRhode Island", 
            frame = FALSE, main.title.size = 0.8,
            legend.outside = TRUE,
            legend.title.size = 0.8,
            legend.outside.position = c("right", "top"))


# Populated-Weighted Average Walkability Index for Populations of Concern
ri_wAvgWalkability_df <- ri_blkgrp_walkability_sf %>% 
  as.data.frame() %>% 
  mutate(RI_LOWINC = if_else(RI_INCOME == "I",totalpopE,0)) %>% 
  mutate(RI_LOWINC = replace_na(RI_LOWINC,0)) %>% 
  mutate(RI_MINORITIES = if_else(RI_MINORITY == "M",totalpopE,0)) %>% 
  mutate(RI_MINORITIES = replace_na(RI_MINORITIES,0)) %>%
  transmute(`Total Pop` = totalpopE,
            Minority = minorityE,
            `Low Income` = num2povE,
            `Under 5` = under5E,
            `Under 18` = under18E,
            `Over 64` = over64E,
            `Limited English HH` = eng_limitE,
            `No HS Dip` = lthsE,
            `RI Low Income` = RI_LOWINC,
            `RI Minority` = RI_MINORITIES,
            NatWalkInd) %>% 
  gather(key = Group, value = Pop, `Total Pop`:`RI Minority`) %>% 
  group_by(Group) %>% 
  summarize(wNatWalkIndex = weighted.mean(NatWalkInd, Pop, na.rm = TRUE)) %>% 
  rbind(., ri_tract_walkability_df)

# Create lollipop graph of pop-weighted average walk index
ri_wAvgWalkability_df %>% 
  ggplot(aes(x = reorder(Group,wNatWalkIndex), y = wNatWalkIndex)) +
  geom_segment(aes(x = reorder(Group,wNatWalkIndex), 
                   xend = reorder(Group,wNatWalkIndex),
                   y = pull(ri_wAvgWalkability_df[8,2]), yend = wNatWalkIndex), 
               color = "green4") +
  geom_point(color = "green", size = 4, alpha = 0.6) +
  coord_flip() + xlab("") + ylab("Walk Index") + ggtitle("Population-Weighted Walk Index") + theme_light() +
  theme(panel.grid.major.y = element_blank(),
        panel.border = element_blank(),
        axis.ticks.y = element_blank()) +
  geom_text(aes(x = Group, y = wNatWalkIndex + 0.2 * sign(wNatWalkIndex), 
                label = round(wNatWalkIndex,1)), 
            hjust = 0.6, size = 3,
            color=rgb(100,100,100, maxColorValue=255))



### Read in VT transit data. See http://geodata.vermont.gov/
# read in VT bus routes
vt_busroutes_sf <- st_read(dsn = "DATA/transport/VT_Data__PublicTransit_Routes_from_GTFS_DataFeeds",layer = "VT_Data__PublicTransit_Routes_from_GTFS_DataFeeds")
# read in VT bus stops
vt_busstops_sf <- st_read(dsn = "DATA/transport/VT_Data__PublicTransit_Stops_from_GTFS_DataFeeds",layer = "VT_Data__PublicTransit_Stops_from_GTFS_DataFeeds")
# read in VT park and rides
vt_parkandride_sf <- st_read(dsn = "DATA/transport/VT_Park_and_Rides",
                             layer = "VT_Park_and_Rides")
# read in VT rail
vt_rail_sf <- st_read(dsn = "DATA/transport/VT_Rail_Lines",
                      layer = "VT_Rail_Lines") %>% 
  filter(RailTrail == "N")

# Map it out
tm_shape(vt_busroutes_sf) + tm_lines(col = "red") +
  tm_shape(vt_parkandride_sf) + tm_dots(col = "green") +
  tm_shape(vt_rail_sf) + tm_lines(col = "black")



### Read in ME GTFS transit data. INCOMPLETE DATA FOR STATE. 
# read in GTFS for Lakes Region Explorer
melreransit <- feedlist %>% 
  filter(t == "Lakes Region Explorer GTFS") %>% 
  pull(url_d) %>% 
  read_gtfs() %>% 
  gtfs_as_sf()
# read in GTFS for Casco Bay Lines
mecasco_transit_sf <- read_gtfs("http://smttracker.com/downloads/gtfs/cascobaylines-portland-me-usa.zip") %>% 
  gtfs_as_sf()
# read in GTFS for Greater Portland Metro transit
megrtrportland_transit_sf <- read_gtfs("http://www.smttracker.com/downloads/gtfs/greater-portland-me.zip") %>% 
  gtfs_as_sf()
# read in GTFS for South Portland Bus
mesouthportland_bus_sf <- read_gtfs("http://www.smttracker.com/downloads/gtfs/south-portland-me-us.zip") %>% 
  gtfs_as_sf()

# map it out
tm_shape(melreransit$shapes) + tm_lines(col = "brown4") +
  tm_shape(megrtrportland_transit_sf$shapes) + tm_lines(col = "green") +
  tm_shape(mecasco_transit_sf$shapes) + tm_lines(col = "red") +
  tm_shape(mesouthportland_bus_sf$shapes) + tm_lines(col = "blue")



#### NH Transit Data. No local bus service data.



### MA Transit data.


### Example with stplanr
library(stplanr)
library(tidyverse)
library(sf)
library(sp)
library(tmap)
library(tigris)
options(tigris_use_cache = TRUE, tigris_class = "sf")

# Download Census TIGERLine roads for RI
## First, extract list of county names to use with tigris::roads
ri_counties <- counties("RI") %>% 
  pull(NAME)
# Next, download roads for each county and rbind to one layer
ri_roads_sf <- rbind_tigris(
  lapply(
    ri_counties, function(x) roads(state = "RI", county = x)
  )
)

# Convert bus stops, roads, and block groups EPSG:2840: NAD83(HARN) / Rhode Island
ri_roads_sf <- st_transform(ri_roads_sf, crs = 2840) 
  # as_Spatial() %>% 
  # SpatialLinesNetwork()
ri_busstops_sf <- st_transform(ri_busstops_sf, crs = 2840) 
ri_busstops_sp <- as_Spatial(ri_busstops_sf) 
ri_blkgrps_sf <- ne_blkgrp_sf %>% 
  filter(STATE == "Rhode Island") %>% 
  st_transform(., crs = 2840)
ri_blkgrps_sp <- as_Spatial(ri_blkgrps_sf)
# Get rid of empty geometries
empty_geo <- st_is_empty(ri_blkgrps_sf)
ri_blkgrps_sf <- ri_blkgrps_sf[!empty_geo,]

# Derive simple catchment area around bus stops
start_time <- Sys.time()
ri_bus_catch400m <- calc_catchment(
  polygonlayer = ri_blkgrps_sp,
  targetlayer = ri_busstops_sp,
  calccols = c("totalpopE","minorityE","under5E","over64E","under18E","eng_limitE","num2povE","lthsE"),
  distance = 400,
  dissolve = TRUE
)
end_time <- Sys.time()
end_time - start_time
library(sp)
library(lwgeom)
ri_bus_catch400m_sf <- st_as_sf(ri_bus_catch400m) %>% 
  st_make_valid()


# Derive network catchment area around bus stops
start_time <- Sys.time()
ri_bus_netcatch400m <- calc_network_catchment(
  sln = ri_roads_sln,
  polygonlayer = ri_blkgrps_sp,
  targetlayer = ri_busstops_sp,
  calccols = c("totalpopE","minorityE","under5E","over64E","under18E","eng_limitE","num2povE","lthsE"),
  maximpedance = 800,
  distance = 400,
  retainAreaProportion = TRUE
)
end_time <- Sys.time()
end_time - start_time

# map it out
tmap_mode("view")
tm_shape(ri_blkgrps_sf) + tm_fill("totalpopE") +
  # tm_shape(ri_blkgrps_sf) + tm_borders(col = "blue") +
  # tm_shape(ri_busstops_sf) + tm_dots(col = "red") +
  tm_shape(bufs) + tm_borders(col = "blue")

ri_blkgrps_sf %>% 
  as.data.frame() %>% 
  summarize(sum(totalpopE), sum(minorityE), sum(num2povE))

ri_bus_catch400m_sf %>% 
  as.data.frame() %>% 
  summarize(sum(totalpopE))

# Create a 400m buffer around bus stops
bufs_sf <- st_buffer(ri_busstops_sf,dist = 400) %>% 
  st_union() %>% 
  st_as_sf()

# Test with a subset of bus stops
ri_busstops_sf2 <- ri_busstops_sf[1:100,]
bufs_sf2 <- st_buffer(ri_busstops_sf2,dist = 400) %>% 
  st_union() %>% 
  st_as_sf()
tm_shape(ri_blkgrps_sf) + tm_fill("totalpopE") +
  tm_shape(bufintersect) + tm_fill("NewPop") +
  tm_shape(bufs_sf2) + tm_borders(col = "blue")
ri_bus_pop_sf2 <- bufs_sf2 %>% 
  mutate(ID = "total") %>% 
  aw_interpolate(.,
                 tid = ID,
                 source = ri_blkgrps_sf,
                 sid = GEOID,
                 weight = "sum",
                 output = "tibble",
                 extensive = c("totalpopE","minorityE","under5E","over64E","under18E","eng_limitE","num2povE","lthsE"))
# Do it manually to see what happens
bufintersect <- ri_blkgrps_sf %>% 
  select(totalpopE) %>% 
  mutate(OldArea = st_area(.)) %>% 
  st_intersection(bufs_sf2,.) %>% 
  mutate(NewArea = st_area(.),
         Proportion = NewArea/OldArea,
         NewPop = as.integer(totalpopE*Proportion))
bufintersect %>% 
  as.data.frame() %>% 
  summarize(sum(NewPop))

library(areal)
# Execute areal weighted interpolation
ri_bus_pop_sf <- bufs_sf %>% 
  mutate(ID = "total") %>% 
  aw_interpolate(.,
                 tid = ID,
                 source = ri_blkgrps_sf,
                 sid = GEOID,
                 weight = "sum",
                 output = "tibble",
                 extensive = c("totalpopE","minorityE","under5E","over64E","under18E","eng_limitE","num2povE","lthsE"))
