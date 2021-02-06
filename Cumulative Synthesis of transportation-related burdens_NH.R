## Cumulative Synthesis of transportation-related burdens

library(tidyverse)
library(tidycensus)
library(sf)
library(tmap)
library(maptools)
library(janitor)
library(kableExtra)
library(tigris)
options(tigris_use_cache = TRUE, tigris_class = "sf")
library(geojsonio)

load("DATA/ne_layers.rds")

# Transportation
# # Transportation Cost Burden
# tmap_mode("view")
# lai_nh_blkgrp %>% 
#   filter(percent_rank(hh7_t) > 0.8 & 
#            (NH_MINORITY == "M" | NH_INCOME == "I" | NH_ENGLISH == "E")) %>% 
#   tm_shape(.) + tm_fill(col = "red")
# 
# lai_nh_blkgrp %>% 
#   filter(hh7_t > 20 & hh7_h > 30 &
#            (NH_MINORITY == "M" | NH_INCOME == "I" | NH_ENGLISH == "E")) %>% 
#   tm_shape(.) + tm_fill(col = "red")



# bring in various burden measures and join to one layer

# isolate MA census geography and emissions variables
nh_blkgrps_sf <- ne_blkgrp_sf %>% 
  filter(STATE == "New Hampshire") %>% 
  select(GEOID, NAME, STATE, bg_area_m2, totalpopE, minorityE, minority_pctE, under5E, pct_under5E, under18E, pct_under18E, over64E, pct_over64E, householdsE, eng_hhE, eng_limitE, eng_limit_pctE, age25upE, lthsE, pct_lthsE, povknownE, num2povE, pct2povE, PM25_19, OZONE_19, DSLPM_19, CANCER_19, RESP_19, PTRAF_19) %>%
  st_transform(., crs = 2823) %>% 
  filter(!st_is_empty(.)) 

nh_tracts_sf <- ne_tracts_sf %>% 
  filter(STATE == "New Hampshire") %>% 
  select(GEOID, NAME, totalpopE, STATE, Over18E, disabledOver18E, pct_disabilityOver18E, totalHHE, HHnoCarE, pct_HHnoCarE) %>% 
  st_transform(., crs = 2823) %>% 
  filter(!st_is_empty(.))


# Read in no transit access. Whole census units with no transit access.  
load("DATA/transport/NH/noTransit.Rds")

# # Read in 80th percentile headways. Whole census units with average headways exceeding 80th percentile headway for all routes.
# load("DATA/transport/NH/headway80th.Rds")

# Read in walkability data. Whole census units with walkability scores. Need to isolate least walkable as <= 5.8
load("DATA/transport/NH/walkability.Rds")

# Read in transportation cost burden data. Whole census units. Need to isolate 80th percentile. 
load("DATA/transport/NH/costBurden.Rds")

# Read in LST data. Whole block group units.
load("DATA/LST/nh_blkgrpLST_sf.rds")

# Read in evacuation risk data. Developed parts of census units; need to filter and join.
load("DATA/FEMA/NH/nfhza_census.Rds")
load("DATA/FEMA/NH/hevac_census.Rds")


# join burdens to a common blkgrp sf and create indicator variable
nh_blkgrps_sf <- nh_blkgrps_sf_noBus %>% 
  as.data.frame() %>% 
  transmute(GEOID = GEOID, NoTransitAccess = "Y") %>% 
  left_join(nh_blkgrps_sf,., by = "GEOID")

# nh_blkgrps_sf <- headway80thblkgrps %>% 
#   as.data.frame() %>% 
#   transmute(GEOID = GEOID, headway80th_bus = "Y") %>% 
#   left_join(nh_blkgrps_sf,., by = "GEOID")

nh_blkgrps_sf <- nh_blkgrp_walkability_sf %>% 
  as.data.frame() %>% 
  filter(NatWalkInd <= 5.8) %>% 
  transmute(GEOID = GEOID, leastWalkable = "Y") %>% 
  left_join(nh_blkgrps_sf,., by = "GEOID")

nh_blkgrps_sf <- lai_nh_blkgrp %>% 
  as.data.frame() %>% 
  filter(percent_rank(hh7_t) > 0.8) %>% 
  transmute(GEOID = GEOID, costBurdened = "Y") %>% 
  left_join(nh_blkgrps_sf,., by = "GEOID")

nh_blkgrps_sf <- nh_blkgrpLST_sf %>% 
  as.data.frame() %>% 
  select(GEOID, meanAvgLST:UHINight) %>% 
  left_join(nh_blkgrps_sf,., by="GEOID")

# join NFHZA flood hazard and hurricane evac records where percentage of pop of concern in flood or hurricane evac prone areas is 80th percentile
nh_blkgrps_sf <- nh_blkgrps_nfhza %>% 
  as.data.frame() %>% 
  group_by(GEOID) %>% 
  mutate(NewEngHH = Proportion*eng_hhE,
         NewAge25Up = Proportion*age25upE) %>% 
  summarize(across(NewPop:NewAge25Up, ~ sum(.x, na.rm = T), 
                   .names = "sum_{.col}")) %>% 
  filter(percent_rank(sum_NewMinority/sum_NewPop) >= 0.8 | 
           percent_rank(sum_NewUnder5/sum_NewPop) >= 0.8 |
           percent_rank(sum_NewOver64/sum_NewPop) >= 0.8 |
           percent_rank(sum_NewUnder18/sum_NewPop) >= 0.8 |
           percent_rank(sum_NewEng_limit/sum_NewEngHH) >= 0.8 |
           percent_rank(sum_NewPov/sum_NewPop) >= 0.8 |
           percent_rank(sum_NewLths/sum_NewAge25Up) >= 0.8) %>% 
  transmute(GEOID = GEOID, nfhzaRisk = "F") %>% 
  left_join(nh_blkgrps_sf,., by="GEOID")

nh_blkgrps_sf <- nh_blkgrps_hevac %>% 
  as.data.frame() %>% 
  group_by(GEOID) %>% 
  mutate(NewEngHH = Proportion*eng_hhE,
         NewAge25Up = Proportion*age25upE) %>% 
  summarize(across(NewPop:NewAge25Up, ~ sum(.x, na.rm = T), 
                   .names = "sum_{.col}")) %>% 
  filter(percent_rank(sum_NewMinority/sum_NewPop) >= 0.8 | 
           percent_rank(sum_NewUnder5/sum_NewPop) >= 0.8 |
           percent_rank(sum_NewOver64/sum_NewPop) >= 0.8 |
           percent_rank(sum_NewUnder18/sum_NewPop) >= 0.8 |
           percent_rank(sum_NewEng_limit/sum_NewEngHH) >= 0.8 |
           percent_rank(sum_NewPov/sum_NewPop) >= 0.8 |
           percent_rank(sum_NewLths/sum_NewAge25Up) >= 0.8) %>% 
  transmute(GEOID = GEOID, hevacRisk = "H") %>% 
  left_join(nh_blkgrps_sf,., by="GEOID")

# repeat for census tracts
nh_tracts_sf <- nh_tracts_sf_noBus %>% 
  as.data.frame() %>% 
  transmute(GEOID = GEOID, NoTransitAccess = "Y") %>% 
  left_join(nh_tracts_sf,., by = "GEOID") 
  
# nh_tracts_sf <- headway80thtracts %>% 
#   as.data.frame() %>% 
#   transmute(GEOID = GEOID, headway80th_bus = "Y") %>% 
#   left_join(nh_tracts_sf,., by = "GEOID")

nh_tracts_sf <- nh_tract_walkability_sf %>% 
  as.data.frame() %>% 
  transmute(GEOID = GEOID, leastWalkable = "Y") %>% 
  left_join(nh_tracts_sf,., by = "GEOID")

nh_tracts_sf <- lai_nh_tract %>% 
  as.data.frame() %>% 
  transmute(GEOID = GEOID, costBurdened = "Y") %>% 
  left_join(nh_tracts_sf,., by = "GEOID")

# join NFHZA flood hazard and hurricane evac records where percentage of pop of concern in flood or hurricane evac prone areas is 80th percentile
nh_tracts_sf <- nh_tracts_nfhza %>% 
  as.data.frame() %>% 
  group_by(GEOID) %>% 
  mutate(NewOver18 = Proportion*Over18E,
         NewtotalHHE = Proportion*totalHHE) %>% 
  summarize(across(NewDisabled:NewtotalHHE, ~ sum(.x, na.rm = T), 
                   .names = "sum_{.col}")) %>% 
  filter(percent_rank(sum_NewDisabled/sum_NewOver18) >= 0.8 | 
           percent_rank(sum_NewNoCar/sum_NewtotalHHE) >= 0.8) %>% 
  transmute(GEOID = GEOID, nfhzaRisk = "F") %>% 
  left_join(nh_tracts_sf,., by="GEOID")

nh_tracts_sf <- nh_tracts_hevac %>% 
  as.data.frame() %>% 
  group_by(GEOID) %>% 
  mutate(NewOver18 = Proportion*Over18E,
         NewtotalHHE = Proportion*totalHHE) %>% 
  summarize(across(NewDisabled:NewtotalHHE, ~ sum(.x, na.rm = T), 
                   .names = "sum_{.col}")) %>% 
  filter(percent_rank(sum_NewDisabled/sum_NewOver18) >= 0.8 | 
           percent_rank(sum_NewNoCar/sum_NewtotalHHE) >= 0.8) %>% 
  transmute(GEOID = GEOID, hevacRisk = "H") %>% 
  left_join(nh_tracts_sf,., by="GEOID")

# replace na's with "N"
nh_blkgrps_sf <- nh_blkgrps_sf %>% 
  mutate(across(NoTransitAccess:costBurdened, 
                ~replace_na(.x, "N"))
         )

nh_tracts_sf <- nh_tracts_sf %>% 
  mutate(across(NoTransitAccess:costBurdened, 
                ~replace_na(.x, "N"))
  )

# add indicator variables for any kind of transport burden AND pop of concern above 80th percentile, followed by code for burdens where both conditions are met, followed by combination indicator and count of indicators satisfied. Final code indicators: P = Pollution or Emissions, T = Transportation, H = Heat, E = Evacuation
nh_blkgrps_sf <- nh_blkgrps_sf %>% 
  mutate(
    AnyPopConcern = case_when(
    percent_rank(minority_pctE) >= 0.8 | 
      percent_rank(pct_under5E) >= 0.8 | 
      percent_rank(pct_under18E) >= 0.8 | 
      percent_rank(pct_over64E) >= 0.8 | 
      percent_rank(pct_lthsE) >= 0.8 | 
      percent_rank(pct2povE) >= 0.8 | 
      percent_rank(eng_limit_pctE) >= 0.8 ~ "Y"
  ),
  AnyEmissionsBurden = case_when(
    percent_rank(PM25_19) >= 0.8 | 
      percent_rank(OZONE_19) >= 0.8 | 
      percent_rank(DSLPM_19) >= 0.8 | 
      percent_rank(CANCER_19) >= 0.8 | 
      percent_rank(RESP_19) >= 0.8 | 
      percent_rank(PTRAF_19) >= 0.8 ~ "Y"
  ),
  AnyTransportBurden = case_when(
    NoTransitAccess == "Y" |  
      leastWalkable == "Y" | 
      costBurdened == "Y" ~ "Y"
  ),
  AnyHeatBurden = case_when(
    percent_rank(meanAvgLST) >= 0.8 | 
      percent_rank(meanDayLST) >= 0.8 | 
      percent_rank(meanNightLST) >= 0.8 | 
      percent_rank(UHI24avg) >= 0.8 | 
      percent_rank(UHIDay) >= 0.8 | 
      percent_rank(UHINight) >= 0.8 ~ "Y"
  ),
  EmissionsBurden = if_else(AnyEmissionsBurden == "Y" & 
                              AnyPopConcern == "Y", "P", " "),
  TransportBurden = if_else(AnyTransportBurden == "Y" & 
                              AnyPopConcern == "Y", "T", " "),
  HeatBurden = if_else(AnyHeatBurden == "Y" & 
                         AnyPopConcern == "Y", "H", " "),
  EvacBurden = if_else(nfhzaRisk == "F" | hevacRisk == "H", "E", " "),
  BurdenCombo = str_remove_all(
    paste(EmissionsBurden,
          TransportBurden,
          HeatBurden,
          EvacBurden, sep = ""),"NA"),
  BurdenCount = nchar(BurdenCombo)
  )

# save output
saveRDS(nh_blkgrps_sf, file = "DATA/nh_blkgrps_sf_CUM.Rds")

# repeat for tracts
nh_tracts_sf <- nh_tracts_sf %>% 
  mutate(
    AnyPopConcern = case_when(
      percent_rank(pct_disabilityOver18E) >= 0.8 | 
        percent_rank(pct_HHnoCarE) >= 0.8 ~ "Y"
    ),
    AnyTransportBurden = case_when(
      NoTransitAccess == "Y" |  
        leastWalkable == "Y" | 
        costBurdened == "Y" ~ "Y"
    ),
    TransportBurden = if_else(AnyTransportBurden == "Y" & 
                                AnyPopConcern == "Y", "T", " "),
    EvacBurden = if_else(nfhzaRisk == "F" | hevacRisk == "H", "E", " "),
    BurdenCombo = str_remove_all(
      paste(TransportBurden,
            EvacBurden, sep = ""),"NA"),
    BurdenCount = nchar(BurdenCombo)
  )

# table showing how many and what percentage of each population of concern falls within block groups meeting 1, 2, 3, or 4 of the burdens
cum_burden_df <- nh_blkgrps_sf %>% 
  as.data.frame() %>% 
  mutate(BurdenCount = as.character(BurdenCount)) %>%
  group_by(BurdenCount) %>% 
  summarize(across(c(totalpopE,householdsE,minorityE,num2povE,eng_limitE,under5E,under18E,over64E,lthsE), ~ sum(.x, na.rm = T), .names = "{col}")) %>% 
  pivot_longer(., cols = totalpopE:lthsE, names_to = "Group") %>% 
  pivot_wider(., names_from = BurdenCount, names_prefix = "Burdens_") %>% 
  rowwise() %>% 
  mutate(Total = sum(c_across(Burdens_0:Burdens_4))) %>% 
  # adorn_totals(., where = c("row","col"), na.rm = T)
  mutate(PctB1 = Burdens_1/Total*100,
         PctB2 = Burdens_2/Total*100,
         PctB3 = Burdens_3/Total*100,
         PctB4 = Burdens_4/Total*100) %>% 
  select(Group,Burdens_1:PctB4) %>% 
  mutate(Group = case_when(
    Group == "totalpopE" ~ "Total Pop",
    Group == "householdsE" ~ "Total HH",
    Group == "minorityE" ~ "Minority",
    Group == "num2povE" ~ "Low Income",
    Group == "eng_limitE" ~ "Limited English HH",
    Group == "under5E" ~ "Under 5",
    Group == "under18E" ~ "Under 18",
    Group == "over64E" ~ "Over 64",
    Group == "lthsE" ~ "No HS Dip"
  ))
cum_burden_df %>% kable(longtable = T, booktabs = T, 
        format.args = list(big.mark = ','), align = "r",
        caption = "Cumulative Burdens", digits = 1,
        col.names = c("Group","1","2","3","4","Total in State","1","2","3","4")) %>% 
  # column_spec(1:3, width = "1.7cm") %>%
  # column_spec(4:10, width = "1.2cm") %>%
  add_header_above(c(" " = 1, "Total Pop and Burdens" = 4, " " = 1, "Pct Pop and Burdens" = 4)) %>% 
  footnote(general = "Based on 2018 American Community Survey 5-year estimates at Block Group level.") %>% 
  kable_styling(latex_options = c("repeat_header","striped"))
# save to csv
write_csv(cum_burden_df,"tables/NH_cum_burden.csv")

# create a stacked bar chart to compare cumulative burdens
cum_burden_df %>% 
  mutate(Group = recode(Group, "Minority" = "People of Color",
                        "No HS Dip" = "No HS Diploma")) %>%
  select(Group,PctB1:PctB4) %>% 
  pivot_longer(.,cols = starts_with("Pct"), names_to = "Burdens") %>% 
  mutate(Burdens = as.factor(Burdens)) %>% 
  ggplot(aes(x = reorder(Group,value), y = value, fill = fct_rev(Burdens))) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(x = "", y = "Percentage", fill = "Burden\nCategories",
       title = "Percentage of New Hampshire Population within\nCumulative Burden Categories") + 
  theme_light() +
  theme(panel.grid.major.y = element_blank(),
        panel.border = element_blank(),
        axis.ticks.y = element_blank()) +
  scale_fill_discrete(labels = c("4 categories", "3 categories", 
                                 "2 categories", "1 category"))
# save plot as png
ggsave("images/NH_CUM_BURDEN_Graph.png")


# map showing block groups by number of cumulative burdens (i.e., high concentration of pop of concern & 1 or more burdens in 80th percentile)
# tmap_mode("view")
# nh_blkgrps_sf %>%
#   filter(BurdenCount > 0) %>%
#   tm_shape(.) + tm_fill(col = "BurdenCount", alpha = 0.5, palette = "YlOrRd")

tmap_mode("plot")
# grab neighboring state boundaries for context
ne_states_sf_cb <- states(cb = TRUE) %>% 
  filter(STUSPS %in% c("MA","CT","RI","NY","NH","NH","ME"))

# grab municipal boundaries
nh_towns_sf <- county_subdivisions(state = "NH", cb = TRUE) %>% 
  st_transform(., crs = 2823)

# create point layer of towns for context
nh_towns_sf_pts <- county_subdivisions(state = "NH", cb = TRUE) %>% 
  filter(NAME %in% c("Nashua",
                     "Portsmouth",
                     "Manchester",
                     "Concord",
                     "Laconia",
                     "Lebanon",
                     "Conway",
                     "Woodstock",
                     "Franconia",
                     "Berlin",
                     "Lancaster",
                     "Pittsburg",
                     "Keene",
                     "Seabrook",
                     "Salem",
                     "Dover",
                     "Farmington")) %>%
  st_transform(., crs = 2823) %>% 
  st_centroid(of_largest_polygon = TRUE)

# Create road layer for context
nh_highways <- primary_roads() %>% 
  filter(FULLNAME %in% c("I- 89","I- 91","I- 93","I- 95")) %>%
  tmaptools::crop_shape(., ne_states_sf_cb) %>% 
  st_transform(., crs = 2823)

# Extract highway segments for labeling
I89roadSegment <- nh_highways %>% 
  filter(LINEARID == "1105281262324")

I91roadSegment <- nh_highways %>% 
  filter(LINEARID == "110373954766")

I95roadSegment <- nh_highways %>% 
  filter(LINEARID == "1105569136123")

I93roadSegment <- nh_highways %>% 
  filter(LINEARID == "1105598909781")

# Create custom icons of highway shields
I89 <- tmap_icons(file = "https://upload.wikimedia.org/wikipedia/commons/thumb/f/f4/I-89.svg/200px-I-89.svg.png")
I91 <- tmap_icons("https://upload.wikimedia.org/wikipedia/commons/thumb/9/90/I-91.svg/200px-I-91.svg.png")
I95 <- tmap_icons(file = "https://upload.wikimedia.org/wikipedia/commons/thumb/6/61/I-95.svg/200px-I-95.svg.png")
I93 <- tmap_icons("https://upload.wikimedia.org/wikipedia/commons/thumb/0/0d/I-93.svg/200px-I-93.svg.png")

m <- nh_blkgrps_sf %>% 
  filter(BurdenCount > 0) %>% 
  tm_shape(., unit = "mi") + 
  tm_fill(col = "BurdenCount", palette = "YlOrRd", 
          title = "Number of\nBurden\nCategories") +
  tm_shape(nh_blkgrps_sf) + tm_borders(lwd = 0.1) +
  tm_shape(ne_states_sf_cb) + tm_borders(lwd = 0.2, alpha = 0.8) +
  tm_text("STUSPS", size = 0.7, remove.overlap = TRUE, col = "gray") +
  tm_shape(nh_highways) + tm_lines(col = "seashell4", lwd = 1.5) +
  tm_shape(I89roadSegment) +
  tm_symbols(shape = I89, border.lwd = NA, size = 0.1) +
  tm_shape(I91roadSegment) +
  tm_symbols(shape = I91, border.lwd = NA, size = 0.1) +
  tm_shape(I93roadSegment) +
  tm_symbols(shape = I93, border.lwd = NA, size = 0.1) +
  tm_shape(I95roadSegment) +
  tm_symbols(shape = I95, border.lwd = NA, size = 0.1) +
  tm_shape(nh_towns_sf_pts) + tm_dots() +
  tm_text("NAME", size = 0.4, col = "black",
          xmod = 0.7, ymod = 0.2, shadow = TRUE) +
  tm_scale_bar(breaks = c(0, 10, 20), text.size = 0.5, 
               position = c("left","TOP")) +
  tm_layout(title = "Cumulative\nBurdens\nby Census\nBlock Group",
            frame = FALSE, main.title.size = 0.8,
            legend.outside = TRUE,
            legend.title.size = 0.8,
            legend.outside.position = c("right", "top"))

tmap_save(m, "images/NH_CUM_BURDEN_map.png",
          height = 7, width = 8, units = "in", dpi = 600)

# # replicate BG map for MA EJ only 
# mEJ <- nh_blkgrps_sf %>% 
#   filter(NH_MINORITY == "M" | NH_INCOME == "I") %>% 
#   filter(BurdenCount > 0) %>% 
#   tm_shape(., unit = "mi") + 
#   tm_fill(col = "BurdenCount", palette = "YlOrRd", 
#           title = "Number of\nBurdens") +
#   # tm_shape(nh_blkgrps_sf) + tm_borders(lwd = 0.1) +
#   tm_shape(nh_towns_sf) + tm_borders(lwd = 0.1) +
#   tm_shape(ne_states_sf_cb) + tm_borders(lwd = 0.2, alpha = 0.8) +
#   tm_text("STUSPS", size = 0.7, remove.overlap = TRUE, col = "gray") +
#   tm_shape(nh_highways) + tm_lines(col = "seashell4", lwd = 1) +
#   tm_shape(I95roadSegment) +
#   tm_symbols(shape = I95, border.lwd = NA, size = 0.1) +
#   tm_shape(I95roadSegment2) +
#   tm_symbols(shape = I95, border.lwd = NA, size = 0.1) +
#   tm_shape(I195roadSegment) +
#   tm_symbols(shape = I195, border.lwd = NA, size = 0.1) +
#   tm_shape(I295roadSegment) +
#   tm_symbols(shape = I295, border.lwd = NA, size = 0.1) +
#   tm_shape(nh_towns_sf_pts) + tm_dots() +
#   tm_text("NAME", size = 0.4, col = "black",
#           xmod = 0.7, ymod = 0.2, shadow = TRUE) +
#   tm_scale_bar(breaks = c(0,5,10), position = c(0.55,0.005)) +
#   tm_layout(title = "Cumulative\nBurdens\nby Census\nBlock Group for\nMinority or\nLow Income\nCommunities",
#             frame = FALSE, main.title.size = 0.8,
#             legend.outside = TRUE,
#             legend.title.size = 0.8,
#             legend.outside.position = c("right", "top"))
# 
# tmap_save(mEJ, "images/NH_CUM_BURDEN_EJ_map.png",
#           height = 7, width = 8, units = "in", dpi = 600)
# 
# # what percentage of EJ BGs have 1 or more burdens?
# EJbgs <- nh_blkgrps_sf %>% 
#   filter(NH_MINORITY == "M" | NH_INCOME == "I") %>% 
#   nrow()
# EJbgs1_4 <- nh_blkgrps_sf %>% 
#   filter(BurdenCount > 0 & 
#            (NH_MINORITY == "M" | NH_INCOME == "I")) %>% 
#   nrow()
# # wha percentage of EJ BGs are high burden?
# EJbgs1_4/EJbgs*100
# # what percentage of all high burden BGs are these?
# hiBurdenBGs <- nh_blkgrps_sf %>% 
#   filter(BurdenCount > 0) %>% 
#   nrow()
# hiBurdenBGs/nrow(nh_blkgrps_sf)*100
# EJbgs1_4/hiBurdenBGs*100

# table showing how many and what percentage of each population of concern falls under each type of burden: Emissions, Transport, Heat, Evacuation (non-exclusive)
Pburden <- nh_blkgrps_sf %>% 
  as.data.frame() %>% 
  # filter(EmissionsBurden != "NA") %>%
  group_by(EmissionsBurden) %>% 
  summarize(across(c(totalpopE,householdsE,minorityE,num2povE,eng_limitE,under5E,under18E,over64E,lthsE), ~ sum(.x, na.rm = T), .names = "{col}")) %>% 
  rename(Burden = EmissionsBurden) %>% 
  replace_na(list(Burden = "NotP"))

Tburden <- nh_blkgrps_sf %>% 
  as.data.frame() %>%
  # filter(TransportBurden != "NA") %>%
  group_by(TransportBurden) %>% 
  summarize(across(c(totalpopE,householdsE,minorityE,num2povE,eng_limitE,under5E,under18E,over64E,lthsE), ~ sum(.x, na.rm = T), .names = "{col}")) %>% 
  rename(Burden = TransportBurden) %>% 
  replace_na(list(Burden = "NotT"))

Hburden <- nh_blkgrps_sf %>% 
  as.data.frame() %>% 
  # filter(HeatBurden != "NA") %>%
  group_by(HeatBurden) %>% 
  summarize(across(c(totalpopE,householdsE,minorityE,num2povE,eng_limitE,under5E,under18E,over64E,lthsE), ~ sum(.x, na.rm = T), .names = "{col}")) %>% 
  rename(Burden = HeatBurden) %>% 
  replace_na(list(Burden = "NotH"))

Eburden <- nh_blkgrps_sf %>% 
  as.data.frame() %>% 
  # filter(EvacBurden != "NA") %>%
  group_by(EvacBurden) %>% 
  summarize(across(c(totalpopE,householdsE,minorityE,num2povE,eng_limitE,under5E,under18E,over64E,lthsE), ~ sum(.x, na.rm = T), .names = "{col}")) %>% 
  rename(Burden = EvacBurden) %>% 
  replace_na(list(Burden = "NotE"))

burden_types_df <- rbind(Pburden,Tburden,Hburden,Eburden) %>% 
  pivot_longer(cols = totalpopE:lthsE, names_to = "Group") %>% 
  pivot_wider(names_from = Burden) %>% 
  rename(Emissions = P, Transport = T, Heat = H, Evacuation = E) %>% 
  mutate(PctEmissions = Emissions/(Emissions+NotP)*100,
         PctTransport = Transport/(Transport+NotT)*100,
         PctHeat = Heat/(Heat+NotH)*100,
         PctEvacuation = Evacuation/(Evacuation+NotE)*100) %>% 
  select(Group,Emissions,Transport,Heat,Evacuation,PctEmissions:PctEvacuation) %>% 
  mutate(Group = case_when(
    Group == "totalpopE" ~ "Total Pop",
    Group == "householdsE" ~ "Total HH",
    Group == "minorityE" ~ "Minority",
    Group == "num2povE" ~ "Low Income",
    Group == "eng_limitE" ~ "Limited English HH",
    Group == "under5E" ~ "Under 5",
    Group == "under18E" ~ "Under 18",
    Group == "over64E" ~ "Over 64",
    Group == "lthsE" ~ "No HS Dip"
  ))
burden_types_df %>% kable(longtable = T, booktabs = T, 
        format.args = list(big.mark = ','), align = "r",
        caption = "Burdens", digits = 1,
        col.names = c("Group","Emissions","Transport","Heat","Evacuation","Emissions","Transport","Heat","Evacuation")) %>% 
  # column_spec(1:3, width = "1.7cm") %>%
  # column_spec(4:10, width = "1.2cm") %>%
  add_header_above(c(" " = 1, "Total Pop and Burden" = 4, "Pct Pop and Burden" = 4)) %>% 
  footnote(general = "Based on 2018 American Community Survey 5-year estimates at Block Group level.") %>% 
  kable_styling(latex_options = c("repeat_header","striped"))
# save as csv
write_csv(burden_types_df,"tables/NH_burden_types.csv")

# table showing towns with block groups with 3 or 4 burdens
# download town pops
town_pops <- get_acs(geography = "county subdivision", 
                     variables = c(totalpop = "B03002_001"),
                     state = "NH", output = "wide", year = 2018) %>% 
  select(GEOID, totalpopE)

# grab municipal boundaries. set cb = FALSE in order to get NAMELSAD variable. NH, like Vermont, has duplicate NAME values for cosub. 
nh_towns_sf <- county_subdivisions(state = "NH") %>% 
  st_transform(., crs = 2823)

# create df with town names from tigris and pops from tidycensus
town_names_pops <- nh_towns_sf %>% 
  as.data.frame() %>% 
  left_join(., town_pops, by = "GEOID") %>% 
  select(NAMELSAD, totalpopE)

# calculate total and pct of population in towns that meet cumulative burden categories for 3+
burdens_town_df <- nh_blkgrps_sf %>% 
  filter(BurdenCount >= 3) %>% 
  select(GEOID, BurdenCount, totalpopE) %>% 
  mutate(OldArea = as.numeric(st_area(.)),
         BurdenCount = as.character(BurdenCount)) %>% 
  st_intersection(., nh_towns_sf) %>%  
  mutate(NewArea = as.numeric(st_area(.)),
         Proportion = NewArea/OldArea,
         NewPop = totalpopE*Proportion) %>% 
  st_drop_geometry() %>%
  group_by(NAMELSAD, BurdenCount) %>% 
  summarize(Pop = sum(NewPop)) %>% 
  pivot_wider(id_cols = NAMELSAD, names_from = BurdenCount, values_from = Pop) %>% 
  as.data.frame() %>% # change from rowwise_df back to regular df
  mutate(across(everything(), ~replace_na(.x, 0))) %>%
  transmute(`City/Town` = NAMELSAD, `3 Burdens` = `3`, `4 Burdens` = `4`) %>% 
  rowwise() %>% 
  mutate(`3+ Burdens` = sum(c_across(`3 Burdens`:`4 Burdens`))) %>% 
  left_join(., town_names_pops, by = c("City/Town" = "NAMELSAD")) %>% 
  mutate(`3 Burdens` = case_when(
    `3 Burdens` < totalpopE & `3 Burdens` >= 1 ~ `3 Burdens`,
    `3 Burdens` > totalpopE ~ totalpopE,
    `3 Burdens` < 1 ~ 0),
    `4 Burdens` = case_when(
      `4 Burdens` < totalpopE & `4 Burdens` >= 1 ~ `4 Burdens`,
      `4 Burdens` > totalpopE ~ totalpopE,
      `4 Burdens` < 1 ~ 0),
    `3+ Burdens` = if_else(`3+ Burdens` > totalpopE, totalpopE, 
                           `3+ Burdens`)) %>% 
  mutate(`Pct 3 Burdens` = `3 Burdens`/totalpopE*100, .after = `3 Burdens`) %>% 
  mutate(`Pct 4 Burdens` = `4 Burdens`/totalpopE*100, .after = `4 Burdens`) %>% 
  mutate(`Pct 3+ Burdens` = `3+ Burdens`/totalpopE*100, 
         .after = `3+ Burdens`) %>% 
  select(-totalpopE)

# identify towns that did not intersect and bind to df so that all municipalities are in the df
burdens_town_df <- nh_towns_sf %>% 
  as.data.frame() %>% 
  anti_join(., burdens_town_df, by = c("NAMELSAD" = "City/Town")) %>% 
  transmute(`City/Town` = NAMELSAD) %>% 
  bind_rows(burdens_town_df, .) %>% 
  mutate(across(everything(), ~replace_na(.x, 0))) %>% 
  arrange(`City/Town`)

burdens_town_df %>% kable(longtable = T, booktabs = T,
                          format.args = list(big.mark = ','), 
                          digits = 0,
                          caption = "Municipalities with Block Groups meeting 3 or 4 Cumulative Burdens", align = "r", 
                          col.names = c("City/Town", "Pop", "Pct", "Pop", "Pct", "Pop", "Pct")) %>% 
  kable_styling(latex_options = c("repeat_header","striped")) %>% 
  add_header_above(c(" " = 1, "3 Burdens" = 2, "4 Burdens" = 2, 
                     "3+ Burdens" = 2))

# save as csv
write_csv(burdens_town_df, "tables/NH_burdens_town.csv")


# # table showing EJ towns with block groups with 3 or 4 burdens
# burdens_EJtown_df <- nh_blkgrps_sf %>%
#   filter(NH_MINORITY == "M" | NH_INCOME == "I") %>%
#   transmute(BurdenCount = as.character(BurdenCount)) %>% 
#   st_centroid(.) %>% 
#   st_intersection(.,nh_towns_sf) %>% 
#   as.data.frame() %>% 
#   group_by(BurdenCount, NAME) %>%
#   summarize(`Block Groups` = n()) %>% 
#   pivot_wider(id_cols = NAME, names_from = BurdenCount, 
#               values_from = `Block Groups`) %>% 
#   mutate(across(everything(), ~replace_na(.x, 0))) %>% 
#   # rowwise() %>% 
#   # mutate(Total = sum(c_across(2:5))) %>% 
#   as.data.frame() %>% # coerce to df again otherwise percent_rank doesn't work!
#   # mutate(Pct1 = `1`/Total*100,
#   #        Rank1 = round(percent_rank(Pct1)*100,0),
#   #        Pct2 = `2`/Total*100,
#   #        Rank2 = round(percent_rank(Pct2)*100,0),
#   #        Pct3 = `3`/Total*100,
#   #        Rank3 = round(percent_rank(Pct3)*100,0),
#   #        Pct4 = `4`/Total*100,
#   #        Rank4 = round(percent_rank(Pct4)*100,0)) %>% 
#   transmute("EJ City/Town" = NAME, `3 Burdens` = `3`, `4 Burdens` = `4`) %>% 
#   filter(`3 Burdens` > 0 | `4 Burdens` > 0) %>%
#   rowwise() %>% 
#   mutate(`3+ Burdens` = sum(c_across(2:3)))
# # select(`City/Town`,Pct1:Rank4)
# burdens_EJtown_df %>% kable(longtable = T, booktabs = T,
#                           format.args = list(big.mark = ','), 
#                           digits = 1,
#                           caption = "Low Income or Minority Municipalities with Block Groups meeting 3 or 4 Cumulative Burdens", align = "r") %>% 
#   # col.names = c(names(.)[1:2],"Number of Over 64 in Block Groups","Pct of Over 64 in City/Town")) %>% 
#   # column_spec(3:4, width = "4cm") %>%
#   kable_styling(latex_options = c("repeat_header","striped")) 
# # save as csv
# write_csv(burdens_EJtown_df, "tables/NH_burdens_EJtown.csv")
# 
# # how many EJ municipalities are there?
# EJtowns <- nh_blkgrps_sf %>%
#   select(-NAME) %>% 
#   filter(NH_MINORITY == "M" | NH_INCOME == "I") %>%
#   st_centroid(.) %>% 
#   st_intersection(.,nh_towns_sf) %>% 
#   as.data.frame() %>% 
#   group_by(NAME) %>%
#   summarize(`Block Groups` = n())
# # save as csv
# write_csv(EJtowns, "tables/NH_EJtowns.csv")
# # what percentage of EJ municipalites have high burden BGs?
# nrow(burdens_EJtown_df)/nrow(EJtowns)*100


# map of towns with block groups meeting 3 - 4 burdens
tmap_mode("plot")
m <- nh_towns_sf %>% 
  right_join(.,burdens_town_df, by = c("NAME" = "City/Town")) %>% 
  tm_shape(., unit = "mi", bbox = nh_towns_sf) + 
  tm_fill(col = "red", alpha = 0.5) +
  tm_text("NAME", size = 0.4, col = "black", remove.overlap = TRUE,
          xmod = 0.7, ymod = 0.2, shadow = TRUE) +
  tm_shape(nh_towns_sf) + tm_borders(col = "gray", lwd = 0.2) +
  tm_shape(ne_states_sf_cb) + tm_borders(lwd = 0.2, alpha = 0.8) +
  tm_text("STUSPS", size = 0.7, remove.overlap = TRUE, col = "gray") +
  tm_shape(nh_highways) + tm_lines(col = "seashell4", lwd = 1.5) +
  tm_shape(I89roadSegment) +
  tm_symbols(shape = I89, border.lwd = NA, size = 0.1) +
  tm_shape(I91roadSegment) +
  tm_symbols(shape = I91, border.lwd = NA, size = 0.1) +
  tm_shape(I93roadSegment) +
  tm_symbols(shape = I93, border.lwd = NA, size = 0.1) +
  tm_shape(I95roadSegment) +
  tm_symbols(shape = I95, border.lwd = NA, size = 0.1) +
  # tm_shape(nh_towns_sf_pts) + tm_dots() +
  # tm_text("NAME", size = 0.4, col = "black",
  #         xmod = 0.7, ymod = 0.2, shadow = TRUE) +
  tm_scale_bar(breaks = c(0, 10, 20), text.size = 0.5, 
               position = c("left","TOP")) +
  tm_layout(main.title = "Municipalities with Block Groups meeting\n3 - 4 Cumulative Burden Categories",
            main.title.position = c("center","TOP"),
            frame = FALSE, main.title.size = 0.9,
            legend.outside = TRUE,
            legend.title.size = 0.8,
            legend.outside.position = c("right", "top"),
            title.position = c("left", "bottom"))

tmap_save(m, "images/NH_CUM_BURDEN_TOWN_map.png",
          height = 7, width = 8, units = "in", dpi = 600)


# table showing state senate district that have 3 - 4 burden categories
# senate_districts <- st_read("DATA/shapefiles/New_Hampshire_Senate_District_Boundaries_-_2012-shp",
#                             "New_Hampshire_Senate_District_Boundaries_-_2012") %>%
#   st_transform(., crs = 2823) %>%
#   st_make_valid()
# Use API from NH Geodata portal
senate_districts <- geojson_read("https://opendata.arcgis.com/datasets/df824f020357423b9f23384f80d9e2d1_4.geojson", what = "sp") %>% 
  st_as_sf(.) %>% 
  st_transform(., crs = 2823) %>% 
  st_make_valid()

# create pop totals for each district
senate_names_pops <- nh_blkgrps_sf %>% 
  select(totalpopE) %>% 
  mutate(OldArea = as.numeric(st_area(.))) %>% 
  st_intersection(., senate_districts) %>% 
  mutate(NewArea = as.numeric(st_area(.)),
         Proportion = NewArea/OldArea,
         NewPop = totalpopE*Proportion) %>% 
  as.data.frame() %>% 
  group_by(Senate2012) %>% 
  summarize(totalpopE = sum(NewPop))

# calculate total and pct of pop for each district
burdens_senate_df <- nh_blkgrps_sf %>%
  filter(BurdenCount >= 3) %>% 
  select(GEOID, BurdenCount, totalpopE) %>% 
  mutate(OldArea = as.numeric(st_area(.)),
         BurdenCount = as.character(BurdenCount)) %>% 
  st_intersection(., senate_districts) %>%  
  mutate(NewArea = as.numeric(st_area(.)),
         Proportion = NewArea/OldArea,
         NewPop = totalpopE*Proportion) %>% 
  st_drop_geometry() %>%
  group_by(Senate2012, BurdenCount) %>% 
  summarize(Pop = sum(NewPop)) %>% 
  pivot_wider(id_cols = Senate2012, names_from = BurdenCount, 
              values_from = Pop) %>% 
  as.data.frame() %>% # change from rowwise_df back to regular df
  mutate(across(everything(), ~replace_na(.x, 0))) %>%
  transmute(`Senate District` = Senate2012, `3 Burdens` = `3`, 
            `4 Burdens` = `4`) %>% 
  rowwise() %>% 
  mutate(`3+ Burdens` = sum(c_across(`3 Burdens`:`4 Burdens`))) %>% 
  left_join(., senate_names_pops, by = c("Senate District" = "Senate2012")) %>% 
  mutate(`Pct 3 Burdens` = `3 Burdens`/totalpopE*100, .after = `3 Burdens`) %>% 
  mutate(`Pct 4 Burdens` = `4 Burdens`/totalpopE*100, .after = `4 Burdens`) %>% 
  mutate(`Pct 3+ Burdens` = `3+ Burdens`/totalpopE*100, 
         .after = `3+ Burdens`) %>% 
  select(-totalpopE)

# identify districts that did not intersect and bind to df so that all districts are in the df
burdens_senate_df <- senate_districts %>% 
  as.data.frame() %>% 
  anti_join(., burdens_senate_df, by = c("Senate2012" = "Senate District")) %>% 
  transmute(`Senate District` = Senate2012) %>% 
  bind_rows(burdens_senate_df, .) %>% 
  mutate(across(everything(), ~replace_na(.x, 0))) %>% 
  arrange(`Senate District`)

burdens_senate_df %>% kable(longtable = T, booktabs = T,
                            format.args = list(big.mark = ','), 
                            digits = 0,
                            caption = "State Senate District with Block Groups meeting 3 or 4 Cumulative Burdens", align = "r", 
                            col.names = c("Senate District", "Pop", "Pct", "Pop", "Pct", "Pop", "Pct")) %>% 
  kable_styling(latex_options = c("repeat_header","striped")) %>% 
  add_header_above(c(" " = 1, "3 Burdens" = 2, "4 Burdens" = 2, 
                     "3+ Burdens" = 2))
# save as csv
write_csv(burdens_senate_df, "tables/NH_burdens_senate.csv")

# create table of BG counts by district
burdensCnt_senate_df <- nh_blkgrps_sf %>%
  transmute(BurdenCount = as.character(BurdenCount)) %>% 
  st_centroid(.) %>% 
  st_intersection(.,senate_districts) %>% 
  as.data.frame() %>% 
  group_by(BurdenCount, Senate2012) %>%
  summarize(`Block Groups` = n()) %>% 
  pivot_wider(id_cols = Senate2012, names_from = BurdenCount, 
              values_from = `Block Groups`) %>% 
  mutate(across(everything(), ~replace_na(.x, 0))) %>% 
  # rowwise() %>% 
  # mutate(Total = sum(c_across(2:5))) %>% 
  as.data.frame() %>% # coerce to df again otherwise percent_rank doesn't work!
  # mutate(Pct1 = `1`/Total*100,
  #        Rank1 = round(percent_rank(Pct1)*100,0),
  #        Pct2 = `2`/Total*100,
  #        Rank2 = round(percent_rank(Pct2)*100,0),
  #        Pct3 = `3`/Total*100,
  #        Rank3 = round(percent_rank(Pct3)*100,0),
  #        Pct4 = `4`/Total*100,
  #        Rank4 = round(percent_rank(Pct4)*100,0)) %>% 
  transmute(`Senate District` = Senate2012, 
            `3 Burdens` = `3`, `4 Burdens` = `4`) %>% 
  # filter(`3 Burdens` > 0 | `4 Burdens` > 0) %>%
  rowwise() %>% 
  mutate(`3+ Burdens` = sum(c_across(2:3)))

burdensCnt_senate_df %>% kable(longtable = T, booktabs = T,
        format.args = list(big.mark = ','), 
        digits = 1,
        caption = "State Senate District with Block Groups meeting 3 or 4 Cumulative Burdens", align = "r") %>% 
  # col.names = c(names(.)[1:2],"Number of Over 64 in Block Groups","Pct of Over 64 in City/Town")) %>% 
  # column_spec(3:4, width = "4cm") %>%
  kable_styling(latex_options = c("repeat_header","striped")) 
# save as csv
write_csv(burdensCnt_senate_df, "tables/NH_burdensCnt_senate.csv")

# map of senate districts with block groups meeting 3 - 4 burdens
tmap_mode("plot")
m <- senate_districts %>% 
  right_join(.,burdens_senate_df, by = c("Senate2012" = "Senate District")) %>% 
  tm_shape(., unit = "mi", bbox = senate_districts) + 
  tm_fill(col = "red", alpha = 0.5) +
  tm_shape(senate_districts) + tm_borders(col = "seashell4") +
  tm_shape(ne_states_sf_cb) + tm_borders(lwd = 0.2, alpha = 0.8) +
  tm_text("STUSPS", size = 0.7, remove.overlap = TRUE, col = "gray") +
  tm_shape(nh_highways) + tm_lines(col = "seashell4", lwd = 1.5) +
  tm_shape(I89roadSegment) +
  tm_symbols(shape = I89, border.lwd = NA, size = 0.1) +
  tm_shape(I91roadSegment) +
  tm_symbols(shape = I91, border.lwd = NA, size = 0.1) +
  tm_shape(I93roadSegment) +
  tm_symbols(shape = I93, border.lwd = NA, size = 0.1) +
  tm_shape(I95roadSegment) +
  tm_symbols(shape = I95, border.lwd = NA, size = 0.1) +
  tm_shape(nh_towns_sf_pts) + tm_dots() +
  tm_text("NAME", size = 0.4, col = "black",
          xmod = 0.7, ymod = 0.2, shadow = TRUE) +
  tm_scale_bar(breaks = c(0, 10, 20), text.size = 0.5, 
               position = c("left","TOP")) +
  tm_layout(main.title = "Senate Districts with Block Groups meeting\n3 - 4 Cumulative Burden Categories",
            main.title.position = c("center","TOP"),
            frame = FALSE, main.title.size = 0.9,
            legend.outside = TRUE,
            legend.title.size = 0.8,
            legend.outside.position = c("right", "top"),
            title.position = c("left","bottom"))

tmap_save(m, "images/NH_CUM_BURDEN_SENATE_map.png",
          height = 7, width = 8, units = "in", dpi = 600)


# table showing block groups by state house district that have 3 - 4 burden categories
# house_districts <- st_read("DATA/shapefiles/NH_House_Districts-shp",
#                             "House_Districts") %>% 
#   st_transform(., crs = 2823) %>% 
#   st_make_valid()
# Use API from NH Geodata portal
house_districts <- geojson_read("https://opendata.arcgis.com/datasets/f264be37ee714132b268924c44b91803_2.geojson", what = "sp") %>% 
  st_as_sf(.) %>% 
  st_transform(., crs = 2823) %>% 
  st_make_valid()

# create pop totals for each district
house_names_pops <- nh_blkgrps_sf %>% 
  select(totalpopE) %>% 
  mutate(OldArea = as.numeric(st_area(.))) %>% 
  st_intersection(., house_districts) %>% 
  mutate(NewArea = as.numeric(st_area(.)),
         Proportion = NewArea/OldArea,
         NewPop = totalpopE*Proportion) %>% 
  as.data.frame() %>% 
  group_by(NHHouse201) %>% 
  summarize(totalpopE = sum(NewPop))

# calculate total and pct of pop for each district
burdens_house_df <- nh_blkgrps_sf %>%
  filter(BurdenCount >= 3) %>% 
  select(GEOID, BurdenCount, totalpopE) %>% 
  mutate(OldArea = as.numeric(st_area(.)),
         BurdenCount = as.character(BurdenCount)) %>% 
  st_intersection(., house_districts) %>%  
  mutate(NewArea = as.numeric(st_area(.)),
         Proportion = NewArea/OldArea,
         NewPop = totalpopE*Proportion) %>% 
  st_drop_geometry() %>%
  group_by(NHHouse201, BurdenCount) %>% 
  summarize(Pop = sum(NewPop)) %>% 
  pivot_wider(id_cols = NHHouse201, names_from = BurdenCount, 
              values_from = Pop) %>% 
  as.data.frame() %>% # change from rowwise_df back to regular df
  mutate(across(everything(), ~replace_na(.x, 0))) %>%
  transmute(`House District` = NHHouse201, `3 Burdens` = `3`, 
            `4 Burdens` = `4`) %>% 
  rowwise() %>% 
  mutate(`3+ Burdens` = sum(c_across(`3 Burdens`:`4 Burdens`))) %>% 
  left_join(., house_names_pops, by = c("House District" = "NHHouse201")) %>% 
  mutate(`Pct 3 Burdens` = `3 Burdens`/totalpopE*100, .after = `3 Burdens`) %>% 
  mutate(`Pct 4 Burdens` = `4 Burdens`/totalpopE*100, .after = `4 Burdens`) %>% 
  mutate(`Pct 3+ Burdens` = `3+ Burdens`/totalpopE*100, 
         .after = `3+ Burdens`) %>% 
  select(-totalpopE)

# identify districts that did not intersect and bind to df so that all districts are in the df
burdens_house_df <- house_districts %>% 
  as.data.frame() %>% 
  anti_join(., burdens_house_df, by = c("NHHouse201" = "House District")) %>% 
  transmute(`House District` = NHHouse201) %>% 
  bind_rows(burdens_house_df, .) %>% 
  mutate(across(everything(), ~replace_na(.x, 0))) %>% 
  arrange(`House District`)

burdens_house_df %>% kable(longtable = T, booktabs = T,
                           format.args = list(big.mark = ','), 
                           digits = 0,
                           caption = "State House District with Block Groups meeting 3 or 4 Cumulative Burdens", align = "r", 
                           col.names = c("House District", "Pop", "Pct", "Pop", "Pct", "Pop", "Pct")) %>% 
  kable_styling(latex_options = c("repeat_header","striped")) %>% 
  add_header_above(c(" " = 1, "3 Burdens" = 2, "4 Burdens" = 2, 
                     "3+ Burdens" = 2))
# save as csv
write_csv(burdens_house_df, "tables/NH_burdens_house.csv")

# create table of BG counts by district
burdensCnt_house_df <- nh_blkgrps_sf %>%
  transmute(BurdenCount = as.character(BurdenCount)) %>% 
  st_centroid(.) %>% 
  st_intersection(.,house_districts) %>% 
  as.data.frame() %>% 
  group_by(BurdenCount, NHHouse201) %>%
  summarize(`Block Groups` = n()) %>% 
  pivot_wider(id_cols = NHHouse201, names_from = BurdenCount, 
              values_from = `Block Groups`) %>% 
  mutate(across(everything(), ~replace_na(.x, 0))) %>% 
  # rowwise() %>% 
  # mutate(Total = sum(c_across(2:5))) %>% 
  as.data.frame() %>% # coerce to df again otherwise percent_rank doesn't work!
  # mutate(Pct1 = `1`/Total*100,
  #        Rank1 = round(percent_rank(Pct1)*100,0),
  #        Pct2 = `2`/Total*100,
  #        Rank2 = round(percent_rank(Pct2)*100,0),
  #        Pct3 = `3`/Total*100,
  #        Rank3 = round(percent_rank(Pct3)*100,0),
  #        Pct4 = `4`/Total*100,
  #        Rank4 = round(percent_rank(Pct4)*100,0)) %>% 
  transmute(`House District` = NHHouse201, 
            `3 Burdens` = `3`, `4 Burdens` = `4`) %>% 
  # filter(`3 Burdens` > 0 | `4 Burdens` > 0) %>%
  rowwise() %>% 
  mutate(`3+ Burdens` = sum(c_across(2:3)))

burdensCnt_house_df %>% kable(longtable = T, booktabs = T,
        format.args = list(big.mark = ','), 
        digits = 1,
        caption = "State House District with Block Groups meeting 3 or 4 Cumulative Burdens", align = "r") %>% 
  # col.names = c(names(.)[1:2],"Number of Over 64 in Block Groups","Pct of Over 64 in City/Town")) %>% 
  # column_spec(3:4, width = "4cm") %>%
  kable_styling(latex_options = c("repeat_header","striped")) 
# save as csv
write_csv(burdensCnt_house_df, "tables/NH_burdensCnt_house.csv")

# map of house districts with block groups meeting 3 - 4 burdens
tmap_mode("plot")
m <- house_districts %>% 
  right_join(.,burdens_house_df, by = c("NHHouse201" = "House District")) %>% 
  tm_shape(., unit = "mi", bbox = senate_districts) + 
  tm_fill(col = "red", alpha = 0.5) +
  tm_shape(house_districts) + tm_borders(col = "seashell4") +
  tm_shape(ne_states_sf_cb) + tm_borders(lwd = 0.2, alpha = 0.8) +
  tm_text("STUSPS", size = 0.7, remove.overlap = TRUE, col = "gray") +
  tm_shape(nh_highways) + tm_lines(col = "seashell4", lwd = 1.5) +
  tm_shape(I89roadSegment) +
  tm_symbols(shape = I89, border.lwd = NA, size = 0.1) +
  tm_shape(I91roadSegment) +
  tm_symbols(shape = I91, border.lwd = NA, size = 0.1) +
  tm_shape(I93roadSegment) +
  tm_symbols(shape = I93, border.lwd = NA, size = 0.1) +
  tm_shape(I95roadSegment) +
  tm_symbols(shape = I95, border.lwd = NA, size = 0.1) +
  tm_shape(nh_towns_sf_pts) + tm_dots() +
  tm_text("NAME", size = 0.4, col = "black",
          xmod = 0.7, ymod = 0.2, shadow = TRUE) +
  tm_scale_bar(breaks = c(0, 10, 20), text.size = 0.5, 
               position = c("left","TOP")) +
  tm_layout(main.title = "House Districts with Block Groups\nmeeting 3 - 4 Cumulative Burden\nCategories",
            main.title.position = c("center","TOP"),
            frame = FALSE, main.title.size = 0.9,
            legend.outside = TRUE,
            legend.title.size = 0.8,
            legend.outside.position = c("right", "bottom"),
            title.position = c("left","bottom"))

tmap_save(m, "images/NH_CUM_BURDEN_HOUSE_map.png",
          height = 7, width = 8, units = "in", dpi = 600)






# # map with block groups by burden count and senate districts
# # extract centorid of districts for labeling
# senate_districts_centroid <- st_centroid(senate_districts, 
#                                          of_largest_polygon = T)
# 
# mS <- nh_blkgrps_sf %>% 
#   filter(BurdenCount > 0) %>% 
#   tm_shape(., unit = "mi") + 
#   tm_fill(col = "BurdenCount", palette = "YlOrRd", 
#           title = "Number of\nBurdens") +
#   tm_shape(nh_blkgrps_sf) + tm_borders(lwd = 0.1) +
#   tm_shape(ne_states_sf_cb) + tm_borders(lwd = 0.2, alpha = 0.8) +
#   tm_text("STUSPS", size = 0.7, remove.overlap = TRUE, col = "gray") +
#   tm_shape(nh_highways) + tm_lines(col = "seashell4", lwd = 1, alpha = 0.5) +
#   tm_shape(nh_highways2nd) + tm_lines(col = "seashell4", lwd = 1, alpha = 0.5) +
#   tm_shape(I95roadSegment) +
#   tm_symbols(shape = I95, border.lwd = NA, size = .1) +
#   tm_shape(I95roadSegment2) +
#   tm_symbols(shape = I95, border.lwd = NA, size = .1) +
#   tm_shape(I395roadSegment) +
#   tm_symbols(shape = I395, border.lwd = NA, size = .1) +
#   tm_shape(I91roadSegment) +
#   tm_symbols(shape = I91, border.lwd = NA, size = .1) +
#   tm_shape(I495roadSegment) +
#   tm_symbols(shape = I495, border.lwd = NA, size = .1) +
#   tm_shape(I495roadSegment2) +
#   tm_symbols(shape = I495, border.lwd = NA, size = .1) +
#   tm_shape(I495roadSegment3) +
#   tm_symbols(shape = I495, border.lwd = NA, size = .1) +
#   tm_shape(I90roadSegment) +
#   tm_symbols(shape = I90, border.lwd = NA, size = .1) +
#   tm_shape(I90roadSegment2) +
#   tm_symbols(shape = I90, border.lwd = NA, size = .1) +
#   tm_shape(nh_towns_sf_pts) + tm_dots() +
#   tm_text("NAME", size = 0.4, col = "black",
#           xmod = 0.7, ymod = 0.2, shadow = TRUE) +
#   tm_shape(senate_districts) + tm_borders(col = "blue", lwd = 1, alpha = 0.5) +
#   tm_shape(senate_districts_centroid) + tm_dots(alpha = 0) +
#   tm_text("SENDISTNUM", size = 0.7, col = "blue", shadow = T, 
#           alpha = 0.5, remove.overlap = T) +
#   tm_scale_bar(breaks = c(0,10,20), position = c(0.55,0.005)) +
#   tm_layout(title = "Cumulative\nBurdens\nby Census\nBlock Group\nand State\nSenate District",
#             frame = FALSE, main.title.size = 0.8,
#             legend.outside = TRUE,
#             legend.title.size = 0.8,
#             legend.outside.position = c("right", "top"))
# 
# tmap_save(mS, "images/CUM_BURDEN_Senate_map.png",
#           height = 4, width = 8, units = "in", dpi = 600)
# 
# 
# 
# # test out different maps
# tmap_mode("view")
# nh_towns_sf %>% 
#   left_join(.,burdens_town_df, by = c("NAME" = "City/Town")) %>% 
#   filter(Pct3 >0 | Pct4>0) %>% 
#   tm_shape(.) + tm_fill(col = "red", alpha = 0.5)
