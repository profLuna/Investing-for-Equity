## Cumulative Synthesis of transportation-related burdens

library(tidyverse)
library(sf)
library(tmap)
library(maptools)
library(janitor)
library(kableExtra)
library(tigris)
options(tigris_use_cache = TRUE, tigris_class = "sf")
library(tidycensus)

load("DATA/ne_layers.rds")

# Transportation
# # Transportation Cost Burden
# tmap_mode("view")
# lai_ma_blkgrp %>% 
#   filter(percent_rank(hh7_t) > 0.8 & 
#            (MA_MINORITY == "M" | MA_INCOME == "I" | MA_ENGLISH == "E")) %>% 
#   tm_shape(.) + tm_fill(col = "red")
# 
# lai_ma_blkgrp %>% 
#   filter(hh7_t > 20 & hh7_h > 30 &
#            (MA_MINORITY == "M" | MA_INCOME == "I" | MA_ENGLISH == "E")) %>% 
#   tm_shape(.) + tm_fill(col = "red")



# bring in various burden measures and join to one layer

# isolate MA census geography and emissions variables
ma_blkgrps_sf <- ne_blkgrp_sf %>% 
  filter(STATE == "Massachusetts") %>% 
  select(GEOID, NAME, STATE, bg_area_m2, totalpopE, minorityE, minority_pctE, under5E, pct_under5E, under18E, pct_under18E, over64E, pct_over64E, householdsE, eng_hhE, eng_limitE, eng_limit_pctE, age25upE, lthsE, pct_lthsE, povknownE, num2povE, pct2povE, MA_MINORITY, MA_INCOME, MA_ENGLISH,
         PM25_19, OZONE_19, DSLPM_19, CANCER_19, RESP_19, PTRAF_19) %>%
  st_transform(., crs = 26986) %>% 
  filter(!st_is_empty(.)) 

ma_tracts_sf <- ne_tracts_sf %>% 
  filter(STATE == "Massachusetts") %>% 
  select(GEOID, NAME, totalpopE, STATE, Over18E, disabledOver18E, pct_disabilityOver18E, totalHHE, HHnoCarE, pct_HHnoCarE) %>% 
  st_transform(., crs = 26986) %>% 
  filter(!st_is_empty(.))


# Read in no transit access. Whole census units with no transit access.  
load("DATA/transport/MA/noTransit.Rds")

# Read in 80th percentile headways. Whole census units with average headways exceeding 80th percentile headway for all routes.
load("DATA/transport/MA/headway80th.Rds")

# Read in walkability data. Whole census units with walkability scores. Need to isolate least walkable as <= 5.8
load("DATA/transport/MA/walkability.Rds")

# Read in transportation cost burden data. Whole census units. Need to isolate 80th percentile. 
load("DATA/transport/MA/costBurden.Rds")

# Read in LST data. Whole block group units.
load("DATA/LST/ma_blkgrpLST_sf.rds")

# Read in evacuation risk data. Developed parts of census units; need to filter and join.
load("DATA/FEMA/MA/nfhza_census.Rds")
load("DATA/FEMA/MA/hevac_census.Rds")


# join burdens to a common blkgrp sf and create indicator variable
ma_blkgrps_sf <- ma_blkgrps_sf_noTransit %>% 
  as.data.frame() %>% 
  transmute(GEOID = GEOID, NoTransitAccess = "Y") %>% 
  left_join(ma_blkgrps_sf,., by = "GEOID")

ma_blkgrps_sf <- headway80thblkgrps_bus %>% 
  as.data.frame() %>% 
  transmute(GEOID = GEOID, headway80th_bus = "Y") %>% 
  left_join(ma_blkgrps_sf,., by = "GEOID")

ma_blkgrps_sf <- headway80thblkgrps_RT %>% 
  as.data.frame() %>%
  transmute(GEOID = GEOID, headway80th_RT = "Y") %>% 
  left_join(ma_blkgrps_sf,., by = "GEOID")

ma_blkgrps_sf <- headway80thblkgrps_CR %>% 
  as.data.frame() %>% 
  transmute(GEOID = GEOID, headway80th_CR = "Y") %>% 
  left_join(ma_blkgrps_sf,., by = "GEOID")

ma_blkgrps_sf <- ma_blkgrp_walkability_sf %>% 
  as.data.frame() %>% 
  filter(NatWalkInd <= 5.8) %>% 
  transmute(GEOID = GEOID, leastWalkable = "Y") %>% 
  left_join(ma_blkgrps_sf,., by = "GEOID")

ma_blkgrps_sf <- lai_ma_blkgrp %>% 
  as.data.frame() %>% 
  filter(percent_rank(hh7_t) > 0.8) %>% 
  transmute(GEOID = GEOID, costBurdened = "Y") %>% 
  left_join(ma_blkgrps_sf,., by = "GEOID")

ma_blkgrps_sf <- ma_blkgrpLST_sf %>% 
  as.data.frame() %>% 
  select(GEOID, meanAvgLST:UHINight) %>% 
  left_join(ma_blkgrps_sf,., by="GEOID")

# join NFHZA flood hazard and hurricane evac records where percentage of pop of concern in flood or hurricane evac prone areas is 80th percentile
ma_blkgrps_sf <- ma_blkgrps_nfhza %>% 
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
           percent_rank(sum_NewLths/sum_NewAge25Up) >= 0.8 |
           percent_rank(sum_NewMA_LOWINC/sum_NewPop) >= 0.8 |
           percent_rank(sum_NewMA_MINORITY/sum_NewPop) >= 0.8 |
           percent_rank(sum_NewMA_NOENGLISH/sum_NewPop) >= 0.8) %>% 
  transmute(GEOID = GEOID, nfhzaRisk = "F") %>% 
  left_join(ma_blkgrps_sf,., by="GEOID")

ma_blkgrps_sf <- ma_blkgrps_hevac %>% 
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
           percent_rank(sum_NewLths/sum_NewAge25Up) >= 0.8 |
           percent_rank(sum_NewMA_LOWINC/sum_NewPop) >= 0.8 |
           percent_rank(sum_NewMA_MINORITY/sum_NewPop) >= 0.8 |
           percent_rank(sum_NewMA_NOENGLISH/sum_NewPop) >= 0.8) %>% 
  transmute(GEOID = GEOID, hevacRisk = "H") %>% 
  left_join(ma_blkgrps_sf,., by="GEOID")

# repeat for census tracts
ma_tracts_sf <- ma_tracts_sf_noTransit %>% 
  as.data.frame() %>% 
  transmute(GEOID = GEOID, NoTransitAccess = "Y") %>% 
  left_join(ma_tracts_sf,., by = "GEOID") 
  
ma_tracts_sf <- headway80thtracts_bus %>% 
  as.data.frame() %>% 
  transmute(GEOID = GEOID, headway80th_bus = "Y") %>% 
  left_join(ma_tracts_sf,., by = "GEOID")

ma_tracts_sf <- headway80thtracts_RT %>% 
  as.data.frame() %>% 
  transmute(GEOID = GEOID, headway80th_RT = "Y") %>% 
  left_join(ma_tracts_sf,., by = "GEOID")

ma_tracts_sf <- headway80thtracts_CR %>% 
  as.data.frame() %>% 
  transmute(GEOID = GEOID, headway80th_CR = "Y") %>% 
  left_join(ma_tracts_sf,., by = "GEOID")

ma_tracts_sf <- ma_tract_walkability_sf %>% 
  as.data.frame() %>% 
  transmute(GEOID = GEOID, leastWalkable = "Y") %>% 
  left_join(ma_tracts_sf,., by = "GEOID")

ma_tracts_sf <- lai_ma_tract %>% 
  as.data.frame() %>% 
  transmute(GEOID = GEOID, costBurdened = "Y") %>% 
  left_join(ma_tracts_sf,., by = "GEOID")

# join NFHZA flood hazard and hurricane evac records where percentage of pop of concern in flood or hurricane evac prone areas is 80th percentile
ma_tracts_sf <- ma_tracts_nfhza %>% 
  as.data.frame() %>% 
  group_by(GEOID) %>% 
  mutate(NewOver18 = Proportion*Over18E,
         NewtotalHHE = Proportion*totalHHE) %>% 
  summarize(across(NewDisabled:NewtotalHHE, ~ sum(.x, na.rm = T), 
                   .names = "sum_{.col}")) %>% 
  filter(percent_rank(sum_NewDisabled/sum_NewOver18) >= 0.8 | 
           percent_rank(sum_NewNoCar/sum_NewtotalHHE) >= 0.8) %>% 
  transmute(GEOID = GEOID, nfhzaRisk = "F") %>% 
  left_join(ma_tracts_sf,., by="GEOID")

ma_tracts_sf <- ma_tracts_hevac %>% 
  as.data.frame() %>% 
  group_by(GEOID) %>% 
  mutate(NewOver18 = Proportion*Over18E,
         NewtotalHHE = Proportion*totalHHE) %>% 
  summarize(across(NewDisabled:NewtotalHHE, ~ sum(.x, na.rm = T), 
                   .names = "sum_{.col}")) %>% 
  filter(percent_rank(sum_NewDisabled/sum_NewOver18) >= 0.8 | 
           percent_rank(sum_NewNoCar/sum_NewtotalHHE) >= 0.8) %>% 
  transmute(GEOID = GEOID, hevacRisk = "H") %>% 
  left_join(ma_tracts_sf,., by="GEOID")

# replace na's with "N"
ma_blkgrps_sf <- ma_blkgrps_sf %>% 
  mutate(across(NoTransitAccess:costBurdened, 
                ~replace_na(.x, "N"))
         )

ma_tracts_sf <- ma_tracts_sf %>% 
  mutate(across(NoTransitAccess:costBurdened, 
                ~replace_na(.x, "N"))
  )

# add indicator variables for any kind of transport burden AND pop of concern above 80th percentile, followed by code for burdens where both conditions are met, followed by combination indicator and count of indicators satisfied. Final code indicators: P = Pollution or Emissions, T = Transportation, H = Heat, E = Evacuation
ma_blkgrps_sf <- ma_blkgrps_sf %>% 
  mutate(
    AnyPopConcern = case_when(
    percent_rank(minority_pctE) >= 0.8 | 
      percent_rank(pct_under5E) >= 0.8 | 
      percent_rank(pct_under18E) >= 0.8 | 
      percent_rank(pct_over64E) >= 0.8 | 
      percent_rank(pct_lthsE) >= 0.8 | 
      percent_rank(pct2povE) >= 0.8 | 
      percent_rank(eng_limit_pctE) >= 0.8 | 
      MA_INCOME == "I" | 
      MA_ENGLISH == "E" | 
      MA_MINORITY == "M" ~ "Y"
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
      headway80th_bus == "Y" | 
      headway80th_RT == "Y" | 
      headway80th_CR == "Y" | 
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
saveRDS(ma_blkgrps_sf, file = "DATA/ma_blkgrps_sf_CUM.Rds")

# repeat for tracts
ma_tracts_sf <- ma_tracts_sf %>% 
  mutate(
    AnyPopConcern = case_when(
      percent_rank(pct_disabilityOver18E) >= 0.8 | 
        percent_rank(pct_HHnoCarE) >= 0.8 ~ "Y"
    ),
    AnyTransportBurden = case_when(
      NoTransitAccess == "Y" | 
        headway80th_bus == "Y" | 
        headway80th_RT == "Y" | 
        headway80th_CR == "Y" | 
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

# save output
saveRDS(ma_tracts_sf, file = "DATA/ma_tracts_sf_CUM.Rds")

# table showing how many and what percentage of each population of concern falls within block groups meeting 1, 2, 3, or 4 of the burdens
cum_burden_df <- ma_blkgrps_sf %>% 
  as.data.frame() %>% 
  mutate(MA_LOWINC = if_else(MA_INCOME == "I", totalpopE, 0)) %>% 
  mutate(MA_LOWINC = replace_na(MA_LOWINC,0)) %>% 
  mutate(MA_MINORITIES = if_else(MA_MINORITY == "M", totalpopE,0)) %>%
  mutate(MA_MINORITIES = replace_na(MA_MINORITIES,0)) %>% 
  mutate(MA_NOENGLISH = if_else(MA_ENGLISH == "E", totalpopE,0)) %>%
  mutate(MA_NOENGLISH = replace_na(MA_NOENGLISH,0),
         BurdenCount = as.character(BurdenCount)) %>%
  group_by(BurdenCount) %>% 
  summarize(across(c(totalpopE,householdsE,minorityE,num2povE,eng_limitE,under5E,under18E,over64E,lthsE,MA_MINORITIES,MA_LOWINC,MA_NOENGLISH), ~ sum(.x, na.rm = T), .names = "{col}")) %>% 
  pivot_longer(., cols = totalpopE:MA_NOENGLISH, names_to = "Group") %>% 
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
    Group == "lthsE" ~ "No HS Dip",
    Group == "MA_MINORITIES" ~ "MA Minority",
    Group == "MA_LOWINC" ~ "MA Low Income",
    Group == "MA_NOENGLISH" ~ "MA Limited English HH"
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
write_csv(cum_burden_df,"tables/cum_burden.csv")

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
       title = "Percentage of Massachusetts Population within\nCumulative Burden Categories") + 
  theme_light() +
  theme(panel.grid.major.y = element_blank(),
        panel.border = element_blank(),
        axis.ticks.y = element_blank()) +
  scale_fill_discrete(labels = c("4 categories", "3 categories", 
                                 "2 categories", "1 category"))
# save plot as png
ggsave("images/CUM_BURDEN_Graph.png")


# map showing block groups by number of cumulative burdens (i.e., high concentration of pop of concern & 1 or more burdens in 80th percentile)
# tmap_mode("view")
# ma_blkgrps_sf %>%
#   filter(BurdenCount > 0) %>%
#   tm_shape(.) + tm_fill(col = "BurdenCount", alpha = 0.5, palette = "YlOrRd")

tmap_mode("plot")
# grab neighboring state boundaries for context
ne_states_sf_cb <- states(cb = TRUE) %>% 
  filter(STUSPS %in% c("MA","CT","RI","NY","NH","VT","ME"))

# grab municipal boundaries
ma_towns_sf <- county_subdivisions(state = "MA", cb = TRUE) %>% 
  st_transform(., crs = 26986)

# create point layer of towns for context
ma_towns_sf_pts <- county_subdivisions(state = "MA", cb = TRUE) %>% 
  filter(NAME %in% c("Boston",
                     "Lawrence",
                     "Lowell",
                     "Brockton",
                     "New Bedford",
                     "Plymouth",
                     "Worcester",
                     "Springfield",
                     "Pittsfield",
                     "Fitchburg",
                     "Holyoke",
                     "Rowe",
                     "Wendell",
                     "Stockbridge",
                     "Fall River",
                     "Bourne",
                     "Lynn",
                     "Randolph",
                     "Webster",
                     "Attleboro",
                     "Medford",
                     "Chicopee",
                     "Falmouth",
                     "Sturbridge",
                     "Longmeadow")) %>% 
  st_transform(., crs = 26986) %>% 
  st_centroid(of_largest_polygon = TRUE)

# create a separate point for Eastham so that it can be repositioned
eastham <- county_subdivisions(state = "MA", cb = TRUE) %>% 
  filter(NAME == "Eastham") %>% 
  st_transform(., crs = 26986) %>% 
  st_centroid(of_largest_polygon = TRUE)

# Create road layer for context
ma_highways <- primary_roads() %>% 
  filter(FULLNAME %in% c("I- 84","I- 90","I- 91","I- 95","I- 190","I- 195","I- 290","I- 395","I- 495","US Hwy 6","US Hwy 202","Mohawk Trl","George W Stanton Hwy","State Rte 2","Mass State Hwy","Concord Tpke","State Rte 25")) %>% 
  tmaptools::crop_shape(., ma_blkgrps_sf, polygon = TRUE) %>% 
  st_transform(., crs = 26986)

ma_highways2nd <- primary_secondary_roads("MA") %>% 
  filter(FULLNAME %in% c("US Hwy 6","Mohawk Trl","State Rte 2","Cambridge Tpke")) %>% 
  st_transform(., crs = 26986)

# Extract highway segments for labeling
I90roadSegment <- ma_highways %>% 
  filter(LINEARID == "1103745154991")

I90roadSegment2 <- ma_highways %>% 
  filter(LINEARID == "110340769311")

I91roadSegment <- ma_highways %>% 
  filter(LINEARID == "1104748241453")

I95roadSegment <- ma_highways %>% 
  filter(LINEARID == "1105569136116")

I95roadSegment2 <- ma_highways %>% 
  filter(LINEARID == "1103737956638")

I195roadSegment2 <- ma_highways %>% 
  filter(LINEARID == "1101922014382")

I395roadSegment <- ma_highways %>% 
  filter(LINEARID == "1104259933162")

I495roadSegment <- ma_highways %>% 
  filter(LINEARID == "1103745404033")

I495roadSegment2 <- ma_highways %>% 
  filter(LINEARID == "1105589457557")

I495roadSegment3 <- ma_highways %>% 
  filter(LINEARID == "1101922014436")

StRt2Segment <- ma_highways2nd %>% 
  filter(LINEARID == "1106087431756")

USHwy6Segment <- ma_highways %>% 
  filter(LINEARID == "1109096413415")

# Create custom icons of highway shields
I90 <- tmap_icons(file = "https://upload.wikimedia.org/wikipedia/commons/thumb/c/ca/I-90.svg/200px-I-90.svg.png")
I95 <- tmap_icons(file = "https://upload.wikimedia.org/wikipedia/commons/thumb/6/61/I-95.svg/200px-I-95.svg.png")
I395 <- tmap_icons("https://upload.wikimedia.org/wikipedia/commons/thumb/1/16/I-395.svg/200px-I-395.svg.png")
I91 <- tmap_icons("https://upload.wikimedia.org/wikipedia/commons/thumb/9/90/I-91.svg/200px-I-91.svg.png")
I495 <- tmap_icons("https://upload.wikimedia.org/wikipedia/commons/thumb/8/8c/I-495.svg/200px-I-495.svg.png")
Hwy2 <- tmap_icons("https://upload.wikimedia.org/wikipedia/commons/thumb/5/54/MA_Route_2.svg/240px-MA_Route_2.svg.png")
Hwy6 <- tmap_icons("https://upload.wikimedia.org/wikipedia/commons/thumb/e/ef/US_6.svg/200px-US_6.svg.png")

m <- ma_blkgrps_sf %>% 
  filter(BurdenCount > 0) %>% 
  tm_shape(., unit = "mi") + 
  tm_fill(col = "BurdenCount", palette = "YlOrRd", 
          title = "Number of\nBurden\nCategories") +
  tm_shape(ma_blkgrps_sf) + tm_borders(lwd = 0.1) +
  tm_shape(ne_states_sf_cb) + tm_borders(lwd = 0.2, alpha = 0.8) +
  tm_text("STUSPS", size = 0.7, remove.overlap = TRUE, col = "gray") +
  tm_shape(ma_highways) + tm_lines(col = "seashell4", lwd = 1, alpha = 0.5) +
  tm_shape(ma_highways2nd) + tm_lines(col = "seashell4", lwd = 1, alpha = 0.5) +
  tm_shape(I95roadSegment) +
  tm_symbols(shape = I95, border.lwd = NA, size = .1) +
  tm_shape(I95roadSegment2) +
  tm_symbols(shape = I95, border.lwd = NA, size = .1) +
  tm_shape(I395roadSegment) +
  tm_symbols(shape = I395, border.lwd = NA, size = .1) +
  tm_shape(I91roadSegment) +
  tm_symbols(shape = I91, border.lwd = NA, size = .1) +
  tm_shape(I495roadSegment) +
  tm_symbols(shape = I495, border.lwd = NA, size = .1) +
  tm_shape(I495roadSegment2) +
  tm_symbols(shape = I495, border.lwd = NA, size = .1) +
  tm_shape(I495roadSegment3) +
  tm_symbols(shape = I495, border.lwd = NA, size = .1) +
  tm_shape(I90roadSegment) +
  tm_symbols(shape = I90, border.lwd = NA, size = .1) +
  tm_shape(I90roadSegment2) +
  tm_symbols(shape = I90, border.lwd = NA, size = .1) +
  tm_shape(ma_towns_sf_pts) + tm_dots() +
  tm_text("NAME", size = 0.4, col = "black",
          xmod = 0.7, ymod = 0.2, shadow = TRUE) +
  tm_shape(eastham) + tm_dots() +
  tm_text("NAME", size = 0.4, col = "black",
          xmod = -0.7, ymod = 0.2, shadow = TRUE) +
  tm_scale_bar(breaks = c(0,10,20), position = c(0.55,0.005)) +
  tm_layout(title = "Cumulative\nBurdens\nby Census\nBlock Group",
            frame = FALSE, main.title.size = 0.8,
            legend.outside = TRUE,
            legend.title.size = 0.8,
            legend.outside.position = c("right", "top"))

tmap_save(m, "images/CUM_BURDEN_map.png",
          height = 4, width = 8, units = "in", dpi = 600)

# replicate BG map for MA EJ only 
mEJ <- ma_blkgrps_sf %>% 
  filter(MA_MINORITY == "M" | MA_INCOME == "I" | MA_ENGLISH == "E") %>% 
  filter(BurdenCount > 0) %>% 
  tm_shape(., unit = "mi") + 
  tm_fill(col = "BurdenCount", palette = "YlOrRd", 
          title = "Number of\nBurdens") +
  # tm_shape(ma_blkgrps_sf) + tm_borders(lwd = 0.1) +
  tm_shape(ma_towns_sf) + tm_borders(lwd = 0.1) +
  tm_shape(ne_states_sf_cb) + tm_borders(lwd = 0.2, alpha = 0.8) +
  tm_text("STUSPS", size = 0.7, remove.overlap = TRUE, col = "gray") +
  tm_shape(ma_highways) + tm_lines(col = "seashell4", lwd = 1, alpha = 0.5) +
  tm_shape(ma_highways2nd) + tm_lines(col = "seashell4", lwd = 1, alpha = 0.5) +
  tm_shape(I95roadSegment) +
  tm_symbols(shape = I95, border.lwd = NA, size = .1) +
  tm_shape(I95roadSegment2) +
  tm_symbols(shape = I95, border.lwd = NA, size = .1) +
  tm_shape(I395roadSegment) +
  tm_symbols(shape = I395, border.lwd = NA, size = .1) +
  tm_shape(I91roadSegment) +
  tm_symbols(shape = I91, border.lwd = NA, size = .1) +
  tm_shape(I495roadSegment) +
  tm_symbols(shape = I495, border.lwd = NA, size = .1) +
  tm_shape(I495roadSegment2) +
  tm_symbols(shape = I495, border.lwd = NA, size = .1) +
  tm_shape(I495roadSegment3) +
  tm_symbols(shape = I495, border.lwd = NA, size = .1) +
  tm_shape(I90roadSegment) +
  tm_symbols(shape = I90, border.lwd = NA, size = .1) +
  tm_shape(I90roadSegment2) +
  tm_symbols(shape = I90, border.lwd = NA, size = .1) +
  tm_shape(ma_towns_sf_pts) + tm_dots() +
  tm_text("NAME", size = 0.4, col = "black",
          xmod = 0.7, ymod = 0.2, shadow = TRUE) +
  tm_scale_bar(breaks = c(0,10,20), position = c(0.55,0.005)) +
  tm_layout(title = "Cumulative\nBurdens\nby Census\nBlock Group for\nEJ Communities",
            frame = FALSE, main.title.size = 0.8,
            legend.outside = TRUE,
            legend.title.size = 0.8,
            legend.outside.position = c("right", "top"))

tmap_save(mEJ, "images/CUM_BURDEN_EJ_map.png",
          height = 4, width = 8, units = "in", dpi = 600)

# what percentage of EJ BGs have 1 or more burdens?
EJbgs <- ma_blkgrps_sf %>% 
  filter(MA_MINORITY == "M" | MA_INCOME == "I" | MA_ENGLISH == "E") %>% 
  nrow()
EJbgs1_4 <- ma_blkgrps_sf %>% 
  filter(BurdenCount > 0 & 
           (MA_MINORITY == "M" | MA_INCOME == "I" | MA_ENGLISH == "E")) %>% 
  nrow()
# wha percentage of EJ BGs are high burden?
EJbgs1_4/EJbgs*100
# what percentage of all high burden BGs are these?
hiBurdenBGs <- ma_blkgrps_sf %>% 
  filter(BurdenCount > 0) %>% 
  nrow()
hiBurdenBGs/nrow(ma_blkgrps_sf)*100
EJbgs1_4/hiBurdenBGs*100

# table showing how many and what percentage of each population of concern falls under each type of burden: Emissions, Transport, Heat, Evacuation (non-exclusive)
Pburden <- ma_blkgrps_sf %>% 
  as.data.frame() %>% 
  mutate(MA_LOWINC = if_else(MA_INCOME == "I", totalpopE, 0)) %>% 
  mutate(MA_LOWINC = replace_na(MA_LOWINC,0)) %>% 
  mutate(MA_MINORITIES = if_else(MA_MINORITY == "M", totalpopE,0)) %>%
  mutate(MA_MINORITIES = replace_na(MA_MINORITIES,0)) %>% 
  mutate(MA_NOENGLISH = if_else(MA_ENGLISH == "E", totalpopE,0)) %>%
  mutate(MA_NOENGLISH = replace_na(MA_NOENGLISH,0)) %>%
  # filter(EmissionsBurden != "NA") %>%
  group_by(EmissionsBurden) %>% 
  summarize(across(c(totalpopE,householdsE,minorityE,num2povE,eng_limitE,under5E,under18E,over64E,lthsE,MA_MINORITIES,MA_LOWINC,MA_NOENGLISH), ~ sum(.x, na.rm = T), .names = "{col}")) %>% 
  rename(Burden = EmissionsBurden) %>% 
  replace_na(list(Burden = "NotP"))

Tburden <- ma_blkgrps_sf %>% 
  as.data.frame() %>% 
  mutate(MA_LOWINC = if_else(MA_INCOME == "I", totalpopE, 0)) %>% 
  mutate(MA_LOWINC = replace_na(MA_LOWINC,0)) %>% 
  mutate(MA_MINORITIES = if_else(MA_MINORITY == "M", totalpopE,0)) %>%
  mutate(MA_MINORITIES = replace_na(MA_MINORITIES,0)) %>% 
  mutate(MA_NOENGLISH = if_else(MA_ENGLISH == "E", totalpopE,0)) %>%
  mutate(MA_NOENGLISH = replace_na(MA_NOENGLISH,0)) %>%
  # filter(TransportBurden != "NA") %>%
  group_by(TransportBurden) %>% 
  summarize(across(c(totalpopE,householdsE,minorityE,num2povE,eng_limitE,under5E,under18E,over64E,lthsE,MA_MINORITIES,MA_LOWINC,MA_NOENGLISH), ~ sum(.x, na.rm = T), .names = "{col}")) %>% 
  rename(Burden = TransportBurden) %>% 
  replace_na(list(Burden = "NotT"))

Hburden <- ma_blkgrps_sf %>% 
  as.data.frame() %>% 
  mutate(MA_LOWINC = if_else(MA_INCOME == "I", totalpopE, 0)) %>% 
  mutate(MA_LOWINC = replace_na(MA_LOWINC,0)) %>% 
  mutate(MA_MINORITIES = if_else(MA_MINORITY == "M", totalpopE,0)) %>%
  mutate(MA_MINORITIES = replace_na(MA_MINORITIES,0)) %>% 
  mutate(MA_NOENGLISH = if_else(MA_ENGLISH == "E", totalpopE,0)) %>%
  mutate(MA_NOENGLISH = replace_na(MA_NOENGLISH,0)) %>%
  # filter(HeatBurden != "NA") %>%
  group_by(HeatBurden) %>% 
  summarize(across(c(totalpopE,householdsE,minorityE,num2povE,eng_limitE,under5E,under18E,over64E,lthsE,MA_MINORITIES,MA_LOWINC,MA_NOENGLISH), ~ sum(.x, na.rm = T), .names = "{col}")) %>% 
  rename(Burden = HeatBurden) %>% 
  replace_na(list(Burden = "NotH"))

Eburden <- ma_blkgrps_sf %>% 
  as.data.frame() %>% 
  mutate(MA_LOWINC = if_else(MA_INCOME == "I", totalpopE, 0)) %>% 
  mutate(MA_LOWINC = replace_na(MA_LOWINC,0)) %>% 
  mutate(MA_MINORITIES = if_else(MA_MINORITY == "M", totalpopE,0)) %>%
  mutate(MA_MINORITIES = replace_na(MA_MINORITIES,0)) %>% 
  mutate(MA_NOENGLISH = if_else(MA_ENGLISH == "E", totalpopE,0)) %>%
  mutate(MA_NOENGLISH = replace_na(MA_NOENGLISH,0)) %>%
  # filter(EvacBurden != "NA") %>%
  group_by(EvacBurden) %>% 
  summarize(across(c(totalpopE,householdsE,minorityE,num2povE,eng_limitE,under5E,under18E,over64E,lthsE,MA_MINORITIES,MA_LOWINC,MA_NOENGLISH), ~ sum(.x, na.rm = T), .names = "{col}")) %>% 
  rename(Burden = EvacBurden) %>% 
  replace_na(list(Burden = "NotE"))

burden_types_df <- rbind(Pburden,Tburden,Hburden,Eburden) %>% 
  pivot_longer(cols = totalpopE:MA_NOENGLISH, names_to = "Group") %>% 
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
    Group == "lthsE" ~ "No HS Dip",
    Group == "MA_MINORITIES" ~ "MA Minority",
    Group == "MA_LOWINC" ~ "MA Low Income",
    Group == "MA_NOENGLISH" ~ "MA Limited English HH"
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
write_csv(burden_types_df,"tables/burden_types.csv")

# table showing towns with block groups with 3 or 4 burdens
# download town pops
town_pops <- get_acs(geography = "county subdivision", 
                     variables = c(totalpop = "B03002_001"),
                     state = "MA", output = "wide", year = 2018) %>% 
  select(GEOID, totalpopE)

# grab municipal boundaries
ma_towns_sf <- county_subdivisions(state = "MA", cb = TRUE) %>% 
  st_transform(., crs = 26986)

# create df with town names from tigris and pops from tidycensus
town_names_pops <- ma_towns_sf %>% 
  as.data.frame() %>% 
  left_join(., town_pops, by = "GEOID") %>% 
  select(NAME, totalpopE)

# calculate total and pct of population in towns that meet cumulative burden categories for 3+
burdens_town_df <- ma_blkgrps_sf %>% 
  filter(BurdenCount >= 3) %>% 
  select(GEOID, BurdenCount, totalpopE) %>% 
  mutate(OldArea = as.numeric(st_area(.)),
         BurdenCount = as.character(BurdenCount)) %>% 
  st_intersection(., ma_towns_sf) %>%  
  mutate(NewArea = as.numeric(st_area(.)),
         Proportion = NewArea/OldArea,
         NewPop = totalpopE*Proportion) %>% 
  st_drop_geometry() %>%
  group_by(NAME, BurdenCount) %>% 
  summarize(Pop = sum(NewPop)) %>% 
  pivot_wider(id_cols = NAME, names_from = BurdenCount, values_from = Pop) %>% 
  as.data.frame() %>% # change from rowwise_df back to regular df
  mutate(across(everything(), ~replace_na(.x, 0))) %>%
  transmute(`City/Town` = NAME, `3 Burdens` = `3`, `4 Burdens` = `4`) %>% 
  rowwise() %>% 
  mutate(`3+ Burdens` = sum(c_across(`3 Burdens`:`4 Burdens`))) %>% 
  left_join(., town_names_pops, by = c("City/Town" = "NAME")) %>% 
  mutate(`Pct 3 Burdens` = `3 Burdens`/totalpopE*100, .after = `3 Burdens`) %>% 
  mutate(`Pct 4 Burdens` = `4 Burdens`/totalpopE*100, .after = `4 Burdens`) %>% 
  mutate(`Pct 3+ Burdens` = `3+ Burdens`/totalpopE*100, 
         .after = `3+ Burdens`) %>% 
  select(-totalpopE)

# identify towns that did not intersect and bind to df so that all municipalities are in the df
burdens_town_df <- ma_towns_sf %>% 
  as.data.frame() %>% 
  anti_join(., burdens_town_df, by = c("NAME" = "City/Town")) %>% 
  transmute(`City/Town` = NAME) %>% 
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
write_csv(burdens_town_df, "tables/burdens_town.csv")


# table showing EJ towns with block groups with 3 or 4 burdens
burdens_EJtown_df <- ma_blkgrps_sf %>%
  filter(MA_MINORITY == "M" | MA_INCOME == "I" | MA_ENGLISH == "E") %>%
  transmute(BurdenCount = as.character(BurdenCount)) %>% 
  st_centroid(.) %>% 
  st_intersection(.,ma_towns_sf) %>% 
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
  transmute("EJ City/Town" = NAME, `3 Burdens` = `3`, `4 Burdens` = `4`) %>% 
  filter(`3 Burdens` > 0 | `4 Burdens` > 0) %>%
  rowwise() %>% 
  mutate(`3+ Burdens` = sum(c_across(2:3)))
# select(`City/Town`,Pct1:Rank4)
burdens_EJtown_df %>% kable(longtable = T, booktabs = T,
                          format.args = list(big.mark = ','), 
                          digits = 1,
                          caption = "Municipalities with Environmental Justice Block Groups meeting 3 or 4 Cumulative Burdens", align = "r") %>% 
  # col.names = c(names(.)[1:2],"Number of Over 64 in Block Groups","Pct of Over 64 in City/Town")) %>% 
  # column_spec(3:4, width = "4cm") %>%
  kable_styling(latex_options = c("repeat_header","striped")) 
# save as csv
write_csv(burdens_EJtown_df, "tables/burdens_EJtown.csv")

# how many EJ municipalities are there?
EJtowns <- ma_blkgrps_sf %>%
  select(-NAME) %>% 
  filter(MA_MINORITY == "M" | MA_INCOME == "I" | MA_ENGLISH == "E") %>%
  st_centroid(.) %>% 
  st_intersection(.,ma_towns_sf) %>% 
  as.data.frame() %>% 
  group_by(NAME) %>%
  summarize(`Block Groups` = n())
# save as csv
write_csv(EJtowns, "tables/EJtowns.csv")
# what percentage of EJ municipalites have high burden BGs?
nrow(burdens_EJtown_df)/nrow(EJtowns)*100


# map of towns with block groups meeting 3 - 4 burdens
# create point layer of towns for context
ma_EJtowns_sf_pts <- county_subdivisions(state = "MA", cb = TRUE) %>% 
  filter(NAME %in% c("Boston",
                     "Lawrence",
                     "Lowell",
                     "Brockton",
                     "New Bedford",
                     "Plymouth",
                     "Worcester",
                     "Springfield",
                     "Pittsfield",
                     "Holyoke",
                     "Stockbridge",
                     "Fall River",
                     "Bourne",
                     "Lynn",
                     "Randolph",
                     "Webster",
                     "Attleboro",
                     "Medford",
                     "Chicopee",
                     "Falmouth",
                     "Eastham",
                     "Sturbridge",
                     "Longmeadow",
                     "Westfield",
                     "Framingham",
                     "Foxborough",
                     "Barnstable",
                     "Brewster",
                     "Reading",
                     "Gloucester",
                     "Quincy",
                     "Danvers",
                     "North Andover",
                     "Holliston",
                     "Sutton",
                     "Salem",
                     "Natick",
                     "Mashpee")) %>% 
  st_transform(., crs = 26986) %>% 
  st_centroid(of_largest_polygon = TRUE)

tmap_mode("plot")
m <- burdens_town_df %>% 
  filter(`3+ Burdens` >= 1) %>% 
  right_join(ma_towns_sf,., by = c("NAME" = "City/Town")) %>% 
  tm_shape(., unit = "mi") + 
  tm_fill(col = "red", alpha = 0.5) +
  # tm_text("NAME", size = 0.25, col = "black", remove.overlap = TRUE) +
  tm_shape(ma_towns_sf) + tm_borders(col = "gray", lwd = 0.2) +
  tm_shape(ne_states_sf_cb) + tm_borders(lwd = 1, alpha = 0.8) +
  tm_text("STUSPS", size = 0.7, remove.overlap = TRUE, col = "gray") +
  tm_shape(ma_highways) + tm_lines(col = "seashell4", lwd = 0.5, alpha = 0.5) +
  tm_shape(ma_highways2nd) + tm_lines(col = "seashell4", lwd = 0.5, alpha = 0.5) +
  tm_shape(I95roadSegment) +
  tm_symbols(shape = I95, border.lwd = NA, size = .1) +
  tm_shape(I95roadSegment2) +
  tm_symbols(shape = I95, border.lwd = NA, size = .1) +
  tm_shape(I395roadSegment) +
  tm_symbols(shape = I395, border.lwd = NA, size = .1) +
  tm_shape(I91roadSegment) +
  tm_symbols(shape = I91, border.lwd = NA, size = .1) +
  tm_shape(I495roadSegment) +
  tm_symbols(shape = I495, border.lwd = NA, size = .1) +
  tm_shape(I495roadSegment2) +
  tm_symbols(shape = I495, border.lwd = NA, size = .1) +
  tm_shape(I495roadSegment3) +
  tm_symbols(shape = I495, border.lwd = NA, size = .1) +
  tm_shape(I90roadSegment) +
  tm_symbols(shape = I90, border.lwd = NA, size = .1) +
  tm_shape(I90roadSegment2) +
  tm_symbols(shape = I90, border.lwd = NA, size = .1) +
  tm_shape(ma_EJtowns_sf_pts) + 
  # tm_dots() +
  tm_text("NAME", size = 0.4, col = "black",
          shadow = TRUE) +
  tm_scale_bar(breaks = c(0,10,20), position = c(0.55,0.005)) +
  tm_layout(title = "Municipalities with Block Groups\nmeeting 3 - 4 Cumulative Burden\nCategories",
            frame = FALSE, main.title.size = 0.8,
            legend.outside = TRUE,
            legend.title.size = 0.8,
            legend.outside.position = c("right", "top"),
            title.position = c("left", "bottom"))

tmap_save(m, "images/CUM_BURDEN_TOWN_map2.png",
          height = 4, width = 8, units = "in", dpi = 600)


# table showing state senate district that have 3 - 4 burden categories
senate_districts <- st_read("DATA/shapefiles/senate2012",
                            "SENATE2012_POLY") %>% 
  st_transform(., crs = 26986) %>% 
  st_make_valid()

# create pop totals for each district
senate_names_pops <- ma_blkgrps_sf %>% 
  select(totalpopE) %>% 
  mutate(OldArea = as.numeric(st_area(.))) %>% 
  st_intersection(., senate_districts) %>% 
  mutate(NewArea = as.numeric(st_area(.)),
         Proportion = NewArea/OldArea,
         NewPop = totalpopE*Proportion) %>% 
  as.data.frame() %>% 
  group_by(SEN_DIST) %>% 
  summarize(totalpopE = sum(NewPop))

# calculate total and pct of pop for each district
burdens_senate_df <- ma_blkgrps_sf %>%
  filter(BurdenCount >= 3) %>% 
  select(GEOID, BurdenCount, totalpopE) %>% 
  mutate(OldArea = as.numeric(st_area(.)),
         BurdenCount = as.character(BurdenCount)) %>% 
  st_intersection(., senate_districts) %>%  
  mutate(NewArea = as.numeric(st_area(.)),
         Proportion = NewArea/OldArea,
         NewPop = totalpopE*Proportion) %>% 
  st_drop_geometry() %>%
  group_by(SEN_DIST, BurdenCount) %>% 
  summarize(Pop = sum(NewPop)) %>% 
  pivot_wider(id_cols = SEN_DIST, names_from = BurdenCount, 
              values_from = Pop) %>% 
  as.data.frame() %>% # change from rowwise_df back to regular df
  mutate(across(everything(), ~replace_na(.x, 0))) %>%
  transmute(`Senate District` = SEN_DIST, `3 Burdens` = `3`, 
            `4 Burdens` = `4`) %>% 
  rowwise() %>% 
  mutate(`3+ Burdens` = sum(c_across(`3 Burdens`:`4 Burdens`))) %>% 
  left_join(., senate_names_pops, by = c("Senate District" = "SEN_DIST")) %>% 
  mutate(`Pct 3 Burdens` = `3 Burdens`/totalpopE*100, .after = `3 Burdens`) %>% 
  mutate(`Pct 4 Burdens` = `4 Burdens`/totalpopE*100, .after = `4 Burdens`) %>% 
  mutate(`Pct 3+ Burdens` = `3+ Burdens`/totalpopE*100, 
         .after = `3+ Burdens`) %>% 
  select(-totalpopE)

# identify districts that did not intersect and bind to df so that all districts are in the df
burdens_senate_df <- senate_districts %>% 
  as.data.frame() %>% 
  anti_join(., burdens_senate_df, by = c("SEN_DIST" = "Senate District")) %>% 
  transmute(`Senate District` = SEN_DIST) %>% 
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
write_csv(burdens_senate_df, "tables/burdens_senate.csv")


# create a table of counts of block gruops by jurisdiction for 3+ burdens
burdensCnt_senate_df <- ma_blkgrps_sf %>%
  transmute(BurdenCount = as.character(BurdenCount)) %>% 
  st_centroid(.) %>% 
  st_intersection(.,senate_districts) %>% 
  as.data.frame() %>% 
  group_by(BurdenCount, SEN_DIST) %>%
  summarize(`Block Groups` = n()) %>% 
  pivot_wider(id_cols = SEN_DIST, names_from = BurdenCount, 
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
  transmute(`Senate District` = SEN_DIST, 
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
write_csv(burdensCnt_senate_df, "tables/burdensCnt_senate.csv")


# map of senate districts with block groups meeting 3 - 4 burdens
tmap_mode("plot")
m <- senate_districts %>% 
  right_join(.,burdensCnt_senate_df, 
             by = c("SEN_DIST" = "Senate District")) %>% 
  filter(`3+ Burdens` > 0) %>% 
  tm_shape(., unit = "mi", bbox = senate_districts) + 
  tm_fill(col = "red", alpha = 0.5) +
  tm_shape(senate_districts) + tm_borders(col = "seashell4") +
  tm_shape(ne_states_sf_cb) + tm_borders(lwd = 0.2, alpha = 0.8) +
  tm_text("STUSPS", size = 0.7, remove.overlap = TRUE, col = "gray") +
  tm_shape(ma_highways) + tm_lines(col = "gray", lwd = 1, alpha = 0.5) +
  tm_shape(ma_highways2nd) + tm_lines(col = "gray", lwd = 1, alpha = 0.5) +
  tm_shape(I95roadSegment) +
  tm_symbols(shape = I95, border.lwd = NA, size = .1) +
  tm_shape(I95roadSegment2) +
  tm_symbols(shape = I95, border.lwd = NA, size = .1) +
  tm_shape(I395roadSegment) +
  tm_symbols(shape = I395, border.lwd = NA, size = .1) +
  tm_shape(I91roadSegment) +
  tm_symbols(shape = I91, border.lwd = NA, size = .1) +
  tm_shape(I495roadSegment) +
  tm_symbols(shape = I495, border.lwd = NA, size = .1) +
  tm_shape(I495roadSegment2) +
  tm_symbols(shape = I495, border.lwd = NA, size = .1) +
  tm_shape(I495roadSegment3) +
  tm_symbols(shape = I495, border.lwd = NA, size = .1) +
  tm_shape(I90roadSegment) +
  tm_symbols(shape = I90, border.lwd = NA, size = .1) +
  tm_shape(I90roadSegment2) +
  tm_symbols(shape = I90, border.lwd = NA, size = .1) +
  tm_shape(ma_towns_sf_pts) + tm_dots() +
  tm_text("NAME", size = 0.4, col = "black",
          xmod = 0.7, ymod = 0.2, shadow = TRUE) +
  tm_scale_bar(breaks = c(0,10,20), position = c(0.55,0.005)) +
  tm_layout(title = "Senate Districts with Block Groups\nmeeting 3 - 4 Cumulative Burden\nCategories",
            frame = FALSE, main.title.size = 0.8,
            legend.outside = TRUE,
            legend.title.size = 0.8,
            legend.outside.position = c("right", "top"),
            title.position = c("left","bottom"))

tmap_save(m, "images/CUM_BURDEN_SENATE_map.png",
          height = 4, width = 8, units = "in", dpi = 600)


# table showing block groups by state house district that have 3 - 4 burden categories
house_districts <- st_read("DATA/shapefiles/house2012",
                           "HOUSE2012_POLY") %>% 
  st_transform(., crs = 26986) %>% 
  st_make_valid()

# create pop totals for each district
house_names_pops <- ma_blkgrps_sf %>% 
  select(totalpopE) %>% 
  mutate(OldArea = as.numeric(st_area(.))) %>% 
  st_intersection(., house_districts) %>% 
  mutate(NewArea = as.numeric(st_area(.)),
         Proportion = NewArea/OldArea,
         NewPop = totalpopE*Proportion) %>% 
  as.data.frame() %>% 
  group_by(REP_DIST) %>% 
  summarize(totalpopE = sum(NewPop))

burdens_house_df <- ma_blkgrps_sf %>%
  filter(BurdenCount >= 3) %>% 
  select(GEOID, BurdenCount, totalpopE) %>% 
  mutate(OldArea = as.numeric(st_area(.)),
         BurdenCount = as.character(BurdenCount)) %>% 
  st_intersection(., house_districts) %>%  
  mutate(NewArea = as.numeric(st_area(.)),
         Proportion = NewArea/OldArea,
         NewPop = totalpopE*Proportion) %>% 
  st_drop_geometry() %>%
  group_by(REP_DIST, BurdenCount) %>% 
  summarize(Pop = sum(NewPop)) %>% 
  pivot_wider(id_cols = REP_DIST, names_from = BurdenCount, 
              values_from = Pop) %>% 
  as.data.frame() %>% # change from rowwise_df back to regular df
  mutate(across(everything(), ~replace_na(.x, 0))) %>%
  transmute(`House District` = REP_DIST, `3 Burdens` = `3`, 
            `4 Burdens` = `4`) %>% 
  rowwise() %>% 
  mutate(`3+ Burdens` = sum(c_across(`3 Burdens`:`4 Burdens`))) %>% 
  left_join(., house_names_pops, by = c("House District" = "REP_DIST")) %>% 
  mutate(`Pct 3 Burdens` = `3 Burdens`/totalpopE*100, .after = `3 Burdens`) %>% 
  mutate(`Pct 4 Burdens` = `4 Burdens`/totalpopE*100, .after = `4 Burdens`) %>% 
  mutate(`Pct 3+ Burdens` = `3+ Burdens`/totalpopE*100, 
         .after = `3+ Burdens`) %>% 
  select(-totalpopE)

# identify districts that did not intersect and bind to df so that all districts are in the df
burdens_house_df <- house_districts %>% 
  as.data.frame() %>% 
  anti_join(., burdens_house_df, by = c("REP_DIST" = "House District")) %>% 
  transmute(`House District` = REP_DIST) %>% 
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
write_csv(burdens_house_df, "tables/burdens_house.csv")


# create a table of counts of block gruops by jurisdiction for 3+ burdens
burdensCnt_house_df <- ma_blkgrps_sf %>%
  transmute(BurdenCount = as.character(BurdenCount)) %>% 
  st_centroid(.) %>% 
  st_intersection(.,house_districts) %>% 
  as.data.frame() %>% 
  group_by(BurdenCount, REP_DIST) %>%
  summarize(`Block Groups` = n()) %>% 
  pivot_wider(id_cols = REP_DIST, names_from = BurdenCount, 
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
  transmute(`House District` = REP_DIST, 
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
write_csv(burdensCnt_house_df, "tables/burdensCnt_house.csv")



# map of house districts with block groups meeting 3 - 4 burdens
tmap_mode("plot")
m <- house_districts %>% 
  right_join(.,burdens_house_df, by = c("REP_DIST" = "House District")) %>% 
  filter(`3+ Burdens` > 0) %>%
  tm_shape(., unit = "mi", bbox = senate_districts) + 
  tm_fill(col = "red", alpha = 0.5) +
  tm_shape(house_districts) + tm_borders(col = "seashell4") +
  tm_shape(ne_states_sf_cb) + tm_borders(lwd = 0.2, alpha = 0.8) +
  tm_text("STUSPS", size = 0.7, remove.overlap = TRUE, col = "gray") +
  tm_shape(ma_highways) + tm_lines(col = "gray", lwd = 1, alpha = 0.5) +
  tm_shape(ma_highways2nd) + tm_lines(col = "gray", lwd = 1, alpha = 0.5) +
  tm_shape(I95roadSegment) +
  tm_symbols(shape = I95, border.lwd = NA, size = .1) +
  tm_shape(I95roadSegment2) +
  tm_symbols(shape = I95, border.lwd = NA, size = .1) +
  tm_shape(I395roadSegment) +
  tm_symbols(shape = I395, border.lwd = NA, size = .1) +
  tm_shape(I91roadSegment) +
  tm_symbols(shape = I91, border.lwd = NA, size = .1) +
  tm_shape(I495roadSegment) +
  tm_symbols(shape = I495, border.lwd = NA, size = .1) +
  tm_shape(I495roadSegment2) +
  tm_symbols(shape = I495, border.lwd = NA, size = .1) +
  tm_shape(I495roadSegment3) +
  tm_symbols(shape = I495, border.lwd = NA, size = .1) +
  tm_shape(I90roadSegment) +
  tm_symbols(shape = I90, border.lwd = NA, size = .1) +
  tm_shape(I90roadSegment2) +
  tm_symbols(shape = I90, border.lwd = NA, size = .1) +
  tm_shape(ma_towns_sf_pts) + tm_dots() +
  tm_text("NAME", size = 0.4, col = "black",
          xmod = 0.7, ymod = 0.2, shadow = TRUE) +
  tm_scale_bar(breaks = c(0,10,20), position = c(0.55,0.005)) +
  tm_layout(title = "House Districts with Block Groups\nmeeting 3 - 4 Cumulative Burden\nCategories",
            frame = FALSE, main.title.size = 0.8,
            legend.outside = TRUE,
            legend.title.size = 0.8,
            legend.outside.position = c("right", "bottom"),
            title.position = c("left","bottom"))

tmap_save(m, "images/CUM_BURDEN_HOUSE_map.png",
          height = 4, width = 8, units = "in", dpi = 600)






# map with block groups by burden count and senate districts
# extract centorid of districts for labeling
senate_districts_centroid <- st_centroid(senate_districts, 
                                         of_largest_polygon = T)

mS <- ma_blkgrps_sf %>% 
  filter(BurdenCount > 0) %>% 
  tm_shape(., unit = "mi") + 
  tm_fill(col = "BurdenCount", palette = "YlOrRd", 
          title = "Number of\nBurdens") +
  tm_shape(ma_blkgrps_sf) + tm_borders(lwd = 0.1) +
  tm_shape(ne_states_sf_cb) + tm_borders(lwd = 0.2, alpha = 0.8) +
  tm_text("STUSPS", size = 0.7, remove.overlap = TRUE, col = "gray") +
  tm_shape(ma_highways) + tm_lines(col = "seashell4", lwd = 1, alpha = 0.5) +
  tm_shape(ma_highways2nd) + tm_lines(col = "seashell4", lwd = 1, alpha = 0.5) +
  tm_shape(I95roadSegment) +
  tm_symbols(shape = I95, border.lwd = NA, size = .1) +
  tm_shape(I95roadSegment2) +
  tm_symbols(shape = I95, border.lwd = NA, size = .1) +
  tm_shape(I395roadSegment) +
  tm_symbols(shape = I395, border.lwd = NA, size = .1) +
  tm_shape(I91roadSegment) +
  tm_symbols(shape = I91, border.lwd = NA, size = .1) +
  tm_shape(I495roadSegment) +
  tm_symbols(shape = I495, border.lwd = NA, size = .1) +
  tm_shape(I495roadSegment2) +
  tm_symbols(shape = I495, border.lwd = NA, size = .1) +
  tm_shape(I495roadSegment3) +
  tm_symbols(shape = I495, border.lwd = NA, size = .1) +
  tm_shape(I90roadSegment) +
  tm_symbols(shape = I90, border.lwd = NA, size = .1) +
  tm_shape(I90roadSegment2) +
  tm_symbols(shape = I90, border.lwd = NA, size = .1) +
  tm_shape(ma_towns_sf_pts) + tm_dots() +
  tm_text("NAME", size = 0.4, col = "black",
          xmod = 0.7, ymod = 0.2, shadow = TRUE) +
  tm_shape(senate_districts) + tm_borders(col = "blue", lwd = 1, alpha = 0.5) +
  tm_shape(senate_districts_centroid) + tm_dots(alpha = 0) +
  tm_text("SENDISTNUM", size = 0.7, col = "blue", shadow = T, 
          alpha = 0.5, remove.overlap = T) +
  tm_scale_bar(breaks = c(0,10,20), position = c(0.55,0.005)) +
  tm_layout(title = "Cumulative\nBurdens\nby Census\nBlock Group\nand State\nSenate District",
            frame = FALSE, main.title.size = 0.8,
            legend.outside = TRUE,
            legend.title.size = 0.8,
            legend.outside.position = c("right", "top"))

tmap_save(mS, "images/CUM_BURDEN_Senate_map.png",
          height = 4, width = 8, units = "in", dpi = 600)



# test out different maps
tmap_mode("view")
ma_towns_sf %>% 
  left_join(.,burdens_town_df, by = c("NAME" = "City/Town")) %>% 
  filter(Pct3 >0 | Pct4>0) %>% 
  tm_shape(.) + tm_fill(col = "red", alpha = 0.5)
