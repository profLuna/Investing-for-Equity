# Map NH counties and municipalities served by paratransit

# download counties geography
nh_counties_sf <- counties(state = "NH") %>% 
  st_transform(crs = 2823)

# read in list of municipalities with paratransit
nh_cosubs_ptransit_df <- readxl::read_xlsx("DATA/transport/NH/New Hampshire jurisdictions served by transit.xlsx", sheet = "Municipalities") %>% 
  mutate(PtransitMuni = "Y")

# read in list of counties with paratransit
nh_counties_ptransit_df <- readxl::read_xlsx("DATA/transport/NH/New Hampshire jurisdictions served by transit.xlsx", sheet = "Counties") %>% 
  mutate(PtransCnty = "Y")

# join list of county paratransit to county geography
nh_counties_sf <- nh_counties_sf %>% 
  transmute(CNTYGEOID = GEOID, COUNTY = NAME, CNTYLSAD = NAMELSAD) %>% 
  left_join(., nh_counties_ptransit_df, by = c("COUNTY" = "County"))

# join list of municipal paratransit, spatial join counties, and code new variable to identify coverage
nh_towns_sf <- nh_towns_sf %>% 
  left_join(., nh_cosubs_ptransit_df, by = c("NAME" = "Municipality")) %>% 
  st_join(., nh_counties_sf, largest = TRUE) %>% 
  mutate(PTRANSIT = case_when(
    PtransitMuni == "Y" & is.na(PtransCnty) ~ "Municipal",
    PtransCnty == "Y" & is.na(PtransitMuni) ~ "County",
    PtransitMuni == "Y" & PtransCnty == "Y" ~ "County and Municipal"
  ))
  
# make a formal map
tmap_mode("plot")
m <- tm_shape(nh_towns_sf, unit = "mi") + 
  tm_polygons(col = "PTRANSIT", border.col = "gray", title = "",
              showNA = FALSE, colorNA = "white") +
  tm_shape(ne_states_sf_cb) + tm_borders(lwd = 0.2, alpha = 0.8) + 
  tm_text("STUSPS", size = 0.7, remove.overlap = TRUE, col = "gray") +
  tm_shape(nh_highways) + tm_lines(col = "seashell4", lwd = 1.5) +
  tm_shape(I89roadSegment) +
  tm_symbols(shape = I89, border.lwd = NA, size = 0.15) +
  tm_shape(I91roadSegment) +
  tm_symbols(shape = I91, border.lwd = NA, size = 0.15) +
  tm_shape(I93roadSegment) +
  tm_symbols(shape = I93, border.lwd = NA, size = 0.15) +
  tm_shape(I95roadSegment) +
  tm_symbols(shape = I95, border.lwd = NA, size = 0.15) +
  tm_shape(nh_towns_sf_pts) + tm_dots() +
  tm_text("NAME", size = 0.5, col = "black", 
          xmod = 0.7, ymod = 0.2, shadow = TRUE) +
  tm_scale_bar(breaks = c(0, 10, 20), text.size = 0.5, 
               position = c("left","TOP")) +
  tm_layout(title = "Paratransit\nProvision", 
            frame = FALSE, main.title.size = 0.8,
            legend.outside = TRUE,
            legend.title.size = 0.8,
            legend.outside.position = c("right", "top"))

tmap_save(m, "DATA/transport/NH/figures/nh_ptransit.png",
          height = 8, width = 8, units = "in", dpi = 600)

knitr::include_graphics("DATA/transport/NH/figures/nh_ptransit.png")


# how many people are covered under each category of paratransit
nh_towns_sf %>% 
  as.data.frame() %>% 
  select(NAME, PTRANSIT) %>% 
  mutate(PTRANSIT = if_else(is.na(PTRANSIT), "None", PTRANSIT)) %>% 
  left_join(townpopsdf, ., by = "NAME") %>% 
  group_by(PTRANSIT) %>% 
  summarize(Pop = sum(TotalPop), LEH = sum(TotalLEH), MIN = sum(TotalMin), LowInc = sum(TotalLowInc), NoHS = sum(TotalNoHS), Over64 = sum(TotalOver64), Disabled = sum(TotalDisabled, na.rm = TRUE), NoCar = sum(TotalNoCar, na.rm = TRUE)) %>% 
  gather(key = Group, value, Pop:NoCar) %>% 
  spread(PTRANSIT, value) %>% 
  mutate(everybody = County+`County and Municipal`+Municipal+None,
         PctCnty = County/everybody*100,
         PctCM = `County and Municipal`/everybody*100,
         PctMuni = Municipal/everybody*100,
         PctTotal = (1 - None/everybody)*100) %>% 
  select(-None,-everybody)

