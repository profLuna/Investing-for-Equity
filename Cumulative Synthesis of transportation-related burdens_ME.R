## Cumulative Synthesis of transportation-related burdens

library(tidyverse)
library(sf)
library(tmap)
library(maptools)
library(janitor)
library(kableExtra)
library(tigris)
options(tigris_use_cache = TRUE, tigris_class = "sf")

load("DATA/ne_layers.rds")

# Transportation
# # Transportation Cost Burden
# tmap_mode("view")
# lai_me_blkgrp %>% 
#   filter(percent_rank(hh7_t) > 0.8 & 
#            (ME_MINORITY == "M" | ME_INCOME == "I" | ME_ENGLISH == "E")) %>% 
#   tm_shape(.) + tm_fill(col = "red")
# 
# lai_me_blkgrp %>% 
#   filter(hh7_t > 20 & hh7_h > 30 &
#            (ME_MINORITY == "M" | ME_INCOME == "I" | ME_ENGLISH == "E")) %>% 
#   tm_shape(.) + tm_fill(col = "red")



# bring in various burden measures and join to one layer

# isolate MA census geography and emissions variables
me_blkgrps_sf <- ne_blkgrp_sf %>% 
  filter(STATE == "Maine") %>% 
  select(GEOID, NAME, STATE, bg_area_m2, totalpopE, minorityE, minority_pctE, under5E, pct_under5E, under18E, pct_under18E, over64E, pct_over64E, householdsE, eng_hhE, eng_limitE, eng_limit_pctE, age25upE, lthsE, pct_lthsE, povknownE, num2povE, pct2povE, PM25_19, OZONE_19, DSLPM_19, CANCER_19, RESP_19, PTRAF_19) %>%
  st_transform(., crs = 2802) %>% 
  filter(!st_is_empty(.)) 

me_tracts_sf <- ne_tracts_sf %>% 
  filter(STATE == "Maine") %>% 
  select(GEOID, NAME, totalpopE, STATE, Over18E, disabledOver18E, pct_disabilityOver18E, totalHHE, HHnoCarE, pct_HHnoCarE) %>% 
  st_transform(., crs = 2802) %>% 
  filter(!st_is_empty(.))


# Read in no transit access. Whole census units with no transit access.  
load("DATA/transport/ME/noTransit.Rds")

# # Read in 80th percentile headways. Whole census units with average headways exceeding 80th percentile headway for all routes.
# load("DATA/transport/ME/headway80th.Rds")

# Read in walkability data. Whole census units with walkability scores. Need to isolate least walkable as <= 5.8
load("DATA/transport/ME/walkability.Rds")

# Read in transportation cost burden data. Whole census units. Need to isolate 80th percentile. 
load("DATA/transport/ME/costBurden.Rds")

# Read in LST data. Whole block group units.
load("DATA/LST/me_blkgrpLST_sf.rds")

# Read in evacuation risk data. Developed parts of census units; need to filter and join.
load("DATA/FEMA/ME/nfhza_census.Rds")
load("DATA/FEMA/ME/hevac_census.Rds")


# join burdens to a common blkgrp sf and create indicator variable
me_blkgrps_sf <- me_blkgrps_sf_noBus %>% 
  as.data.frame() %>% 
  transmute(GEOID = GEOID, NoTransitAccess = "Y") %>% 
  left_join(me_blkgrps_sf,., by = "GEOID")

# me_blkgrps_sf <- headway80thblkgrps %>% 
#   as.data.frame() %>% 
#   transmute(GEOID = GEOID, headway80th_bus = "Y") %>% 
#   left_join(me_blkgrps_sf,., by = "GEOID")

me_blkgrps_sf <- me_blkgrp_walkability_sf %>% 
  as.data.frame() %>% 
  filter(NatWalkInd <= 5.8) %>% 
  transmute(GEOID = GEOID, leastWalkable = "Y") %>% 
  left_join(me_blkgrps_sf,., by = "GEOID")

me_blkgrps_sf <- lai_me_blkgrp %>% 
  as.data.frame() %>% 
  filter(percent_rank(hh7_t) > 0.8) %>% 
  transmute(GEOID = GEOID, costBurdened = "Y") %>% 
  left_join(me_blkgrps_sf,., by = "GEOID")

me_blkgrps_sf <- me_blkgrpLST_sf %>% 
  as.data.frame() %>% 
  select(GEOID, meanAvgLST:UHINight) %>% 
  left_join(me_blkgrps_sf,., by="GEOID")

# join NFHZA flood hazard and hurricane evac records where percentage of pop of concern in flood or hurricane evac prone areas is 80th percentile
me_blkgrps_sf <- me_blkgrps_nfhza %>% 
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
  left_join(me_blkgrps_sf,., by="GEOID")

me_blkgrps_sf <- me_blkgrps_hevac %>% 
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
  left_join(me_blkgrps_sf,., by="GEOID")

# repeat for census tracts
me_tracts_sf <- me_tracts_sf_noBus %>% 
  as.data.frame() %>% 
  transmute(GEOID = GEOID, NoTransitAccess = "Y") %>% 
  left_join(me_tracts_sf,., by = "GEOID") 
  
# me_tracts_sf <- headway80thtracts %>% 
#   as.data.frame() %>% 
#   transmute(GEOID = GEOID, headway80th_bus = "Y") %>% 
#   left_join(me_tracts_sf,., by = "GEOID")

me_tracts_sf <- me_tract_walkability_sf %>% 
  as.data.frame() %>% 
  transmute(GEOID = GEOID, leastWalkable = "Y") %>% 
  left_join(me_tracts_sf,., by = "GEOID")

me_tracts_sf <- lai_me_tract %>% 
  as.data.frame() %>% 
  transmute(GEOID = GEOID, costBurdened = "Y") %>% 
  left_join(me_tracts_sf,., by = "GEOID")

# join NFHZA flood hazard and hurricane evac records where percentage of pop of concern in flood or hurricane evac prone areas is 80th percentile
me_tracts_sf <- me_tracts_nfhza %>% 
  as.data.frame() %>% 
  group_by(GEOID) %>% 
  mutate(NewOver18 = Proportion*Over18E,
         NewtotalHHE = Proportion*totalHHE) %>% 
  summarize(across(NewDisabled:NewtotalHHE, ~ sum(.x, na.rm = T), 
                   .names = "sum_{.col}")) %>% 
  filter(percent_rank(sum_NewDisabled/sum_NewOver18) >= 0.8 | 
           percent_rank(sum_NewNoCar/sum_NewtotalHHE) >= 0.8) %>% 
  transmute(GEOID = GEOID, nfhzaRisk = "F") %>% 
  left_join(me_tracts_sf,., by="GEOID")

me_tracts_sf <- me_tracts_hevac %>% 
  as.data.frame() %>% 
  group_by(GEOID) %>% 
  mutate(NewOver18 = Proportion*Over18E,
         NewtotalHHE = Proportion*totalHHE) %>% 
  summarize(across(NewDisabled:NewtotalHHE, ~ sum(.x, na.rm = T), 
                   .names = "sum_{.col}")) %>% 
  filter(percent_rank(sum_NewDisabled/sum_NewOver18) >= 0.8 | 
           percent_rank(sum_NewNoCar/sum_NewtotalHHE) >= 0.8) %>% 
  transmute(GEOID = GEOID, hevacRisk = "H") %>% 
  left_join(me_tracts_sf,., by="GEOID")

# replace na's with "N"
me_blkgrps_sf <- me_blkgrps_sf %>% 
  mutate(across(NoTransitAccess:costBurdened, 
                ~replace_na(.x, "N"))
         )

me_tracts_sf <- me_tracts_sf %>% 
  mutate(across(NoTransitAccess:costBurdened, 
                ~replace_na(.x, "N"))
  )

# add indicator variables for any kind of transport burden AND pop of concern above 80th percentile, followed by code for burdens where both conditions are met, followed by combination indicator and count of indicators satisfied. Final code indicators: P = Pollution or Emissions, T = Transportation, H = Heat, E = Evacuation
me_blkgrps_sf <- me_blkgrps_sf %>% 
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

# repeat for tracts
me_tracts_sf <- me_tracts_sf %>% 
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
cum_burden_df <- me_blkgrps_sf %>% 
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
write_csv(cum_burden_df,"tables/ME_cum_burden.csv")

# create a stacked bar chart to compare cumulative burdens
cum_burden_df %>% 
  select(Group,PctB1:PctB4) %>% 
  pivot_longer(.,cols = starts_with("Pct"), names_to = "Burdens") %>% 
  mutate(Burdens = as.factor(Burdens)) %>% 
  ggplot(aes(x = reorder(Group,value), y = value, fill = fct_rev(Burdens))) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(x = "", y = "Percentage", fill = "Burden\nCategories",
       title = "Percentage of Maine Population within\nCumulative Burden Categories") + 
  theme_light() +
  theme(panel.grid.major.y = element_blank(),
        panel.border = element_blank(),
        axis.ticks.y = element_blank()) +
  scale_fill_discrete(labels = c("4 categories", "3 categories", 
                                 "2 categories", "1 category"))
# save plot as png
ggsave("images/ME_CUM_BURDEN_Graph.png")


# map showing block groups by number of cumulative burdens (i.e., high concentration of pop of concern & 1 or more burdens in 80th percentile)
# tmap_mode("view")
# me_blkgrps_sf %>%
#   filter(BurdenCount > 0) %>%
#   tm_shape(.) + tm_fill(col = "BurdenCount", alpha = 0.5, palette = "YlOrRd")

tmap_mode("plot")
# grab neighboring state boundaries for context
ne_states_sf_cb <- states(cb = TRUE) %>% 
  filter(STUSPS %in% c("MA","CT","RI","NY","ME","VT","ME"))

# grab municipal boundaries
me_towns_sf <- county_subdivisions(state = "ME", cb = TRUE) %>% 
  st_transform(., crs = 2802)

# create point layer of towns for context
me_towns_sf_pts <- county_subdivisions(state = "ME", cb = TRUE) %>% 
  filter(NAME %in% c("Portland",
                     "Brunswick",
                     "Lewiston",
                     "Augusta",
                     "Waterville",
                     "Farmington",
                     "Bangor",
                     "Rockland",
                     "Belfast",
                     "Bar Harbor",
                     "Machias",
                     "Danforth",
                     "Houlton",
                     "Medway",
                     "Allagash",
                     "Caribou",
                     "Presque Isle",
                     "Jackman",
                     "Saco",
                     "Kittery",
                     "Bethel")) %>%
  st_transform(., crs = 2802) %>% 
  st_centroid(of_largest_polygon = TRUE)

# Create road layer for context
me_highways2nd <- primary_secondary_roads("ME") %>%
  filter(FULLNAME %in% c("I- 295","I- 95","US Hwy 201","US Hwy 202","US Hwy 1","US Hwy 2","Maine Tpke")) %>%
  st_transform(., crs = 2802)

# Extract highway segments for labeling
I295roadSegment <- me_highways2nd %>% 
  filter(LINEARID == "1102638666973")

I95roadSegment <- me_highways2nd %>% 
  filter(LINEARID == "1105084080161")

I95roadSegment2 <- me_highways2nd %>% 
  filter(LINEARID == "1104470496492")

# Create custom icons of highway shields
I295 <- tmap_icons(file = "https://upload.wikimedia.org/wikipedia/commons/thumb/4/41/I-295.svg/200px-I-295.svg.png")
I95 <- tmap_icons(file = "https://upload.wikimedia.org/wikipedia/commons/thumb/6/61/I-95.svg/200px-I-95.svg.png")

m <- me_blkgrps_sf %>% 
  filter(BurdenCount > 0) %>% 
  tm_shape(., unit = "mi") + 
  tm_fill(col = "BurdenCount", palette = "YlOrRd", 
          title = "Number of\nBurden\nCategories") +
  tm_shape(me_blkgrps_sf) + tm_borders(lwd = 0.1) +
  tm_shape(ne_states_sf_cb) + tm_borders(lwd = 0.2, alpha = 0.8) +
  tm_text("STUSPS", size = 0.7, remove.overlap = TRUE, col = "gray") +
  tm_shape(me_highways2nd) + tm_lines(col = "seashell4", lwd = 0.5) +
  tm_shape(I295roadSegment) +
  tm_symbols(shape = I295, border.lwd = NA, size = 0.1) +
  tm_shape(I95roadSegment) +
  tm_symbols(shape = I95, border.lwd = NA, size = 0.1) +
  tm_shape(I95roadSegment2) +
  tm_symbols(shape = I95, border.lwd = NA, size = 0.1) +
  tm_shape(me_towns_sf_pts) + tm_dots() +
  tm_text("NAME", size = 0.4, col = "black",
          xmod = 0.7, ymod = 0.2, shadow = TRUE) +
  tm_scale_bar(breaks = c(0, 25, 50), text.size = 0.5, 
               position = c("right","bottom")) +
  tm_layout(title = "Cumulative\nBurdens\nby Census\nBlock Group",
            frame = FALSE, main.title.size = 0.8,
            legend.outside = TRUE,
            legend.title.size = 0.8,
            legend.outside.position = c("right", "top"))

tmap_save(m, "images/ME_CUM_BURDEN_map.png",
          height = 7, width = 8, units = "in", dpi = 600)

# # replicate BG map for MA EJ only 
# mEJ <- me_blkgrps_sf %>% 
#   filter(ME_MINORITY == "M" | ME_INCOME == "I") %>% 
#   filter(BurdenCount > 0) %>% 
#   tm_shape(., unit = "mi") + 
#   tm_fill(col = "BurdenCount", palette = "YlOrRd", 
#           title = "Number of\nBurdens") +
#   # tm_shape(me_blkgrps_sf) + tm_borders(lwd = 0.1) +
#   tm_shape(me_towns_sf) + tm_borders(lwd = 0.1) +
#   tm_shape(ne_states_sf_cb) + tm_borders(lwd = 0.2, alpha = 0.8) +
#   tm_text("STUSPS", size = 0.7, remove.overlap = TRUE, col = "gray") +
#   tm_shape(me_highways) + tm_lines(col = "seashell4", lwd = 1) +
#   tm_shape(I95roadSegment) +
#   tm_symbols(shape = I95, border.lwd = NA, size = 0.1) +
#   tm_shape(I95roadSegment2) +
#   tm_symbols(shape = I95, border.lwd = NA, size = 0.1) +
#   tm_shape(I195roadSegment) +
#   tm_symbols(shape = I195, border.lwd = NA, size = 0.1) +
#   tm_shape(I295roadSegment) +
#   tm_symbols(shape = I295, border.lwd = NA, size = 0.1) +
#   tm_shape(me_towns_sf_pts) + tm_dots() +
#   tm_text("NAME", size = 0.4, col = "black",
#           xmod = 0.7, ymod = 0.2, shadow = TRUE) +
#   tm_scale_bar(breaks = c(0,5,10), position = c(0.55,0.005)) +
#   tm_layout(title = "Cumulative\nBurdens\nby Census\nBlock Group for\nMinority or\nLow Income\nCommunities",
#             frame = FALSE, main.title.size = 0.8,
#             legend.outside = TRUE,
#             legend.title.size = 0.8,
#             legend.outside.position = c("right", "top"))
# 
# tmap_save(mEJ, "images/ME_CUM_BURDEN_EJ_map.png",
#           height = 7, width = 8, units = "in", dpi = 600)
# 
# # what percentage of EJ BGs have 1 or more burdens?
# EJbgs <- me_blkgrps_sf %>% 
#   filter(ME_MINORITY == "M" | ME_INCOME == "I") %>% 
#   nrow()
# EJbgs1_4 <- me_blkgrps_sf %>% 
#   filter(BurdenCount > 0 & 
#            (ME_MINORITY == "M" | ME_INCOME == "I")) %>% 
#   nrow()
# # wha percentage of EJ BGs are high burden?
# EJbgs1_4/EJbgs*100
# # what percentage of all high burden BGs are these?
# hiBurdenBGs <- me_blkgrps_sf %>% 
#   filter(BurdenCount > 0) %>% 
#   nrow()
# hiBurdenBGs/nrow(me_blkgrps_sf)*100
# EJbgs1_4/hiBurdenBGs*100

# table showing how many and what percentage of each population of concern falls under each type of burden: Emissions, Transport, Heat, Evacuation (non-exclusive)
Pburden <- me_blkgrps_sf %>% 
  as.data.frame() %>% 
  # filter(EmissionsBurden != "NA") %>%
  group_by(EmissionsBurden) %>% 
  summarize(across(c(totalpopE,householdsE,minorityE,num2povE,eng_limitE,under5E,under18E,over64E,lthsE), ~ sum(.x, na.rm = T), .names = "{col}")) %>% 
  rename(Burden = EmissionsBurden) %>% 
  replace_na(list(Burden = "NotP"))

Tburden <- me_blkgrps_sf %>% 
  as.data.frame() %>%
  # filter(TransportBurden != "NA") %>%
  group_by(TransportBurden) %>% 
  summarize(across(c(totalpopE,householdsE,minorityE,num2povE,eng_limitE,under5E,under18E,over64E,lthsE), ~ sum(.x, na.rm = T), .names = "{col}")) %>% 
  rename(Burden = TransportBurden) %>% 
  replace_na(list(Burden = "NotT"))

Hburden <- me_blkgrps_sf %>% 
  as.data.frame() %>% 
  # filter(HeatBurden != "NA") %>%
  group_by(HeatBurden) %>% 
  summarize(across(c(totalpopE,householdsE,minorityE,num2povE,eng_limitE,under5E,under18E,over64E,lthsE), ~ sum(.x, na.rm = T), .names = "{col}")) %>% 
  rename(Burden = HeatBurden) %>% 
  replace_na(list(Burden = "NotH"))

Eburden <- me_blkgrps_sf %>% 
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
write_csv(burden_types_df,"tables/ME_burden_types.csv")

# table showing towns with block groups with 3 or 4 burdens
burdens_town_df <- me_blkgrps_sf %>%
  transmute(BurdenCount = as.character(BurdenCount)) %>% 
  st_centroid(.) %>% 
  st_intersection(.,me_towns_sf) %>% 
  as.data.frame() %>% 
  group_by(BurdenCount, NAME) %>%
  summarize(`Block Groups` = n()) %>% 
  pivot_wider(id_cols = NAME, names_from = BurdenCount, 
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
  transmute("City/Town" = NAME, `3 Burdens` = `3`, `4 Burdens` = `4`) %>% 
  filter(`3 Burdens` > 0 | `4 Burdens` > 0) %>%
  rowwise() %>% 
  mutate(`3+ Burdens` = sum(c_across(2:3)))
  # select(`City/Town`,Pct1:Rank4)
burdens_town_df %>% kable(longtable = T, booktabs = T,
                    format.args = list(big.mark = ','), 
                    digits = 1,
                    caption = "Municipalities with Block Groups meeting 3 or 4 Cumulative Burdens", align = "r") %>% 
                    # col.names = c(names(.)[1:2],"Number of Over 64 in Block Groups","Pct of Over 64 in City/Town")) %>% 
  # column_spec(3:4, width = "4cm") %>%
  kable_styling(latex_options = c("repeat_header","striped")) 
# save as csv
write_csv(burdens_town_df, "tables/ME_burdens_town.csv")


# # table showing EJ towns with block groups with 3 or 4 burdens
# burdens_EJtown_df <- me_blkgrps_sf %>%
#   filter(ME_MINORITY == "M" | ME_INCOME == "I") %>%
#   transmute(BurdenCount = as.character(BurdenCount)) %>% 
#   st_centroid(.) %>% 
#   st_intersection(.,me_towns_sf) %>% 
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
# write_csv(burdens_EJtown_df, "tables/ME_burdens_EJtown.csv")
# 
# # how many EJ municipalities are there?
# EJtowns <- me_blkgrps_sf %>%
#   select(-NAME) %>% 
#   filter(ME_MINORITY == "M" | ME_INCOME == "I") %>%
#   st_centroid(.) %>% 
#   st_intersection(.,me_towns_sf) %>% 
#   as.data.frame() %>% 
#   group_by(NAME) %>%
#   summarize(`Block Groups` = n())
# # save as csv
# write_csv(EJtowns, "tables/ME_EJtowns.csv")
# # what percentage of EJ municipalites have high burden BGs?
# nrow(burdens_EJtown_df)/nrow(EJtowns)*100


# map of towns with block groups meeting 3 - 4 burdens
tmap_mode("plot")
m <- me_towns_sf %>% 
  right_join(.,burdens_town_df, by = c("NAME" = "City/Town")) %>% 
  tm_shape(., unit = "mi", bbox = me_towns_sf) + 
  tm_fill(col = "red", alpha = 0.5) +
  tm_text("NAME", size = 0.4, col = "black", remove.overlap = TRUE,
          xmod = 0.7, ymod = 0.2, shadow = TRUE) +
  tm_shape(me_towns_sf) + tm_borders(col = "gray", lwd = 0.2) +
  tm_shape(ne_states_sf_cb) + tm_borders(lwd = 0.2, alpha = 0.8) +
  tm_text("STUSPS", size = 0.7, remove.overlap = TRUE, col = "gray") +
  tm_shape(me_highways2nd) + tm_lines(col = "seashell4", lwd = 0.5) +
  tm_shape(I295roadSegment) +
  tm_symbols(shape = I295, border.lwd = NA, size = 0.1) +
  tm_shape(I95roadSegment) +
  tm_symbols(shape = I95, border.lwd = NA, size = 0.1) +
  tm_shape(I95roadSegment2) +
  tm_symbols(shape = I95, border.lwd = NA, size = 0.1) +
  # tm_shape(me_towns_sf_pts) + tm_dots() +
  # tm_text("NAME", size = 0.4, col = "black",
  #         xmod = 0.7, ymod = 0.2, shadow = TRUE) +
  tm_scale_bar(breaks = c(0, 25, 50), text.size = 0.5, 
               position = c("right","bottom")) +
  tm_layout(main.title = "Municipalities with Block Groups meeting\n3 - 4 Cumulative Burden Categories",
            main.title.position = c("center","TOP"),
            frame = FALSE, main.title.size = 0.9,
            legend.outside = TRUE,
            legend.title.size = 0.8,
            legend.outside.position = c("right", "top"),
            title.position = c("left", "bottom"))

tmap_save(m, "images/ME_CUM_BURDEN_TOWN_map.png",
          height = 7, width = 8, units = "in", dpi = 600)


# table showing state senate district that have 3 - 4 burden categories
senate_districts <- st_read("DATA/shapefiles/Maine_State_Senate_Districts_2013_GeoLibrary-shp",
                            "Maine_State_Senate_Districts_2013_GeoLibrary") %>%
  st_transform(., crs = 2802) %>%
  st_make_valid() %>% 
  tmaptools::crop_shape(., ne_states_sf_cb, polygon = TRUE)
# # Use API from ME Geodata portal
# library(geojsonio)
# senate_districts <- geojson_read("https://opendata.arcgis.com/datasets/df824f020357423b9f23384f80d9e2d1_4.geojson", what = "sp") %>% 
#   st_as_sf(.) %>% 
#   st_transform(., crs = 2802) %>% 
#   st_make_valid()

burdens_senate_df <- me_blkgrps_sf %>%
  transmute(BurdenCount = as.character(BurdenCount)) %>% 
  st_centroid(.) %>% 
  st_intersection(.,senate_districts) %>% 
  as.data.frame() %>% 
  group_by(BurdenCount, SD2013) %>%
  summarize(`Block Groups` = n()) %>% 
  pivot_wider(id_cols = SD2013, names_from = BurdenCount, 
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
  transmute(`Senate District` = SD2013, 
            `3 Burdens` = `3`, `4 Burdens` = `4`) %>% 
  filter(`3 Burdens` > 0 | `4 Burdens` > 0) %>%
  rowwise() %>% 
  mutate(`3+ Burdens` = sum(c_across(2:3)))

burdens_senate_df %>% kable(longtable = T, booktabs = T,
        format.args = list(big.mark = ','), 
        digits = 1,
        caption = "State Senate District with Block Groups meeting 3 or 4 Cumulative Burdens", align = "r") %>% 
  # col.names = c(names(.)[1:2],"Number of Over 64 in Block Groups","Pct of Over 64 in City/Town")) %>% 
  # column_spec(3:4, width = "4cm") %>%
  kable_styling(latex_options = c("repeat_header","striped")) 
# save as csv
write_csv(burdens_senate_df, "tables/ME_burdens_senate.csv")

# map of senate districts with block groups meeting 3 - 4 burdens
tmap_mode("plot")
m <- senate_districts %>% 
  right_join(.,burdens_senate_df, by = c("SD2013" = "Senate District")) %>% 
  tm_shape(., unit = "mi", bbox = senate_districts) + 
  tm_fill(col = "red", alpha = 0.5) +
  tm_shape(senate_districts) + tm_borders(col = "seashell4") +
  tm_shape(ne_states_sf_cb) + tm_borders(lwd = 0.2, alpha = 0.8) +
  tm_text("STUSPS", size = 0.7, remove.overlap = TRUE, col = "gray") +
  tm_shape(me_highways2nd) + tm_lines(col = "seashell4", lwd = 0.5) +
  tm_shape(I295roadSegment) +
  tm_symbols(shape = I295, border.lwd = NA, size = 0.1) +
  tm_shape(I95roadSegment) +
  tm_symbols(shape = I95, border.lwd = NA, size = 0.1) +
  tm_shape(I95roadSegment2) +
  tm_symbols(shape = I95, border.lwd = NA, size = 0.1) +
  tm_shape(me_towns_sf_pts) + tm_dots() +
  tm_text("NAME", size = 0.4, col = "black",
          xmod = 0.7, ymod = 0.2, shadow = TRUE) +
  tm_scale_bar(breaks = c(0, 25, 50), text.size = 0.5, 
               position = c("right","bottom")) +
  tm_layout(main.title = "Senate Districts with Block Groups meeting\n3 - 4 Cumulative Burden Categories",
            main.title.position = c("center","TOP"),
            frame = FALSE, main.title.size = 0.9,
            legend.outside = TRUE,
            legend.title.size = 0.8,
            legend.outside.position = c("right", "top"),
            title.position = c("left","bottom"))

tmap_save(m, "images/ME_CUM_BURDEN_SENATE_map.png",
          height = 7, width = 8, units = "in", dpi = 600)


# table showing block groups by state house district that have 3 - 4 burden categories
house_districts <- st_read("DATA/shapefiles/Maine_House_Districts_2013_GeoLibrary-shp",
                            "Maine_House_Districts_2013_GeoLibrary") %>%
  st_transform(., crs = 2802) %>%
  st_make_valid() %>% 
  tmaptools::crop_shape(., ne_states_sf_cb, polygon = TRUE)
# # Use API from ME Geodata portal
# library(geojsonio)
# house_districts <- geojson_read("https://opendata.arcgis.com/datasets/f264be37ee714132b268924c44b91803_2.geojson", what = "sp") %>% 
#   st_as_sf(.) %>% 
#   st_transform(., crs = 2802) %>% 
#   st_make_valid()

burdens_house_df <- me_blkgrps_sf %>%
  transmute(BurdenCount = as.character(BurdenCount)) %>% 
  st_centroid(.) %>% 
  st_intersection(.,house_districts) %>% 
  as.data.frame() %>% 
  group_by(BurdenCount, HD2013) %>%
  summarize(`Block Groups` = n()) %>% 
  pivot_wider(id_cols = HD2013, names_from = BurdenCount, 
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
  transmute(`House District` = HD2013, 
            `3 Burdens` = `3`, `4 Burdens` = `4`) %>% 
  filter(`3 Burdens` > 0 | `4 Burdens` > 0) %>%
  rowwise() %>% 
  mutate(`3+ Burdens` = sum(c_across(2:3)))

burdens_house_df %>% kable(longtable = T, booktabs = T,
        format.args = list(big.mark = ','), 
        digits = 1,
        caption = "State House District with Block Groups meeting 3 or 4 Cumulative Burdens", align = "r") %>% 
  # col.names = c(names(.)[1:2],"Number of Over 64 in Block Groups","Pct of Over 64 in City/Town")) %>% 
  # column_spec(3:4, width = "4cm") %>%
  kable_styling(latex_options = c("repeat_header","striped")) 
# save as csv
write_csv(burdens_house_df, "tables/ME_burdens_house.csv")

# map of house districts with block groups meeting 3 - 4 burdens
tmap_mode("plot")
m <- house_districts %>% 
  right_join(.,burdens_house_df, by = c("HD2013" = "House District")) %>% 
  tm_shape(., unit = "mi", bbox = senate_districts) + 
  tm_fill(col = "red", alpha = 0.5) +
  tm_shape(house_districts) + tm_borders(col = "seashell4") +
  tm_shape(ne_states_sf_cb) + tm_borders(lwd = 0.2, alpha = 0.8) +
  tm_text("STUSPS", size = 0.7, remove.overlap = TRUE, col = "gray") +
  tm_shape(me_highways2nd) + tm_lines(col = "seashell4", lwd = 0.5) +
  tm_shape(I295roadSegment) +
  tm_symbols(shape = I295, border.lwd = NA, size = 0.1) +
  tm_shape(I95roadSegment) +
  tm_symbols(shape = I95, border.lwd = NA, size = 0.1) +
  tm_shape(I95roadSegment2) +
  tm_symbols(shape = I95, border.lwd = NA, size = 0.1) +
  tm_shape(me_towns_sf_pts) + tm_dots() +
  tm_text("NAME", size = 0.4, col = "black",
          xmod = 0.7, ymod = 0.2, shadow = TRUE) +
  tm_scale_bar(breaks = c(0, 25, 50), text.size = 0.5, 
               position = c("right","bottom")) +
  tm_layout(main.title = "House Districts with Block Groups\nmeeting 3 - 4 Cumulative Burden\nCategories",
            main.title.position = c("center","TOP"),
            frame = FALSE, main.title.size = 0.9,
            legend.outside = TRUE,
            legend.title.size = 0.8,
            legend.outside.position = c("right", "bottom"),
            title.position = c("left","bottom"))

tmap_save(m, "images/ME_CUM_BURDEN_HOUSE_map.png",
          height = 7, width = 8, units = "in", dpi = 600)






# # map with block groups by burden count and senate districts
# # extract centorid of districts for labeling
# senate_districts_centroid <- st_centroid(senate_districts, 
#                                          of_largest_polygon = T)
# 
# mS <- me_blkgrps_sf %>% 
#   filter(BurdenCount > 0) %>% 
#   tm_shape(., unit = "mi") + 
#   tm_fill(col = "BurdenCount", palette = "YlOrRd", 
#           title = "Number of\nBurdens") +
#   tm_shape(me_blkgrps_sf) + tm_borders(lwd = 0.1) +
#   tm_shape(ne_states_sf_cb) + tm_borders(lwd = 0.2, alpha = 0.8) +
#   tm_text("STUSPS", size = 0.7, remove.overlap = TRUE, col = "gray") +
#   tm_shape(me_highways) + tm_lines(col = "seashell4", lwd = 1, alpha = 0.5) +
#   tm_shape(me_highways2nd) + tm_lines(col = "seashell4", lwd = 1, alpha = 0.5) +
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
#   tm_shape(me_towns_sf_pts) + tm_dots() +
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
# me_towns_sf %>% 
#   left_join(.,burdens_town_df, by = c("NAME" = "City/Town")) %>% 
#   filter(Pct3 >0 | Pct4>0) %>% 
#   tm_shape(.) + tm_fill(col = "red", alpha = 0.5)
