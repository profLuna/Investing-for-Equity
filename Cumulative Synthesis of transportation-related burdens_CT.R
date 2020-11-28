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
# lai_ct_blkgrp %>% 
#   filter(percent_rank(hh7_t) > 0.8 & 
#            (MA_MINORITY == "M" | MA_INCOME == "I" | MA_ENGLISH == "E")) %>% 
#   tm_shape(.) + tm_fill(col = "red")
# 
# lai_ct_blkgrp %>% 
#   filter(hh7_t > 20 & hh7_h > 30 &
#            (MA_MINORITY == "M" | MA_INCOME == "I" | MA_ENGLISH == "E")) %>% 
#   tm_shape(.) + tm_fill(col = "red")



# bring in various burden measures and join to one layer

# isolate MA census geography and emissions variables
ct_blkgrps_sf <- ne_blkgrp_sf %>% 
  filter(STATE == "Connecticut") %>% 
  select(GEOID, NAME, STATE, bg_area_m2, totalpopE, minorityE, minority_pctE, under5E, pct_under5E, under18E, pct_under18E, over64E, pct_over64E, householdsE, eng_hhE, eng_limitE, eng_limit_pctE, age25upE, lthsE, pct_lthsE, povknownE, num2povE, pct2povE, CT_INCOME, PM25_19, OZONE_19, DSLPM_19, CANCER_19, 
         RESP_19, PTRAF_19) %>%
  st_transform(., crs = 2775) %>% 
  filter(!st_is_empty(.)) 

ct_tracts_sf <- ne_tracts_sf %>% 
  filter(STATE == "Connecticut") %>% 
  select(GEOID, NAME, totalpopE, STATE, Over18E, disabledOver18E, pct_disabilityOver18E, totalHHE, HHnoCarE, pct_HHnoCarE) %>% 
  st_transform(., crs = 2775) %>% 
  filter(!st_is_empty(.))


# Read in no transit access. Whole census units with no transit access.  
load("DATA/transport/CT/noTransit.Rds")

# Read in 80th percentile headways. Whole census units with average headways exceeding 80th percentile headway for all routes.
load("DATA/transport/CT/headway80th.Rds")

# Read in walkability data. Whole census units with walkability scores. Need to isolate least walkable as <= 5.8
load("DATA/transport/CT/walkability.Rds")

# Read in transportation cost burden data. Whole census units. Need to isolate 80th percentile. 
load("DATA/transport/CT/costBurden.Rds")

# Read in LST data. Whole block group units.
load("DATA/LST/ct_blkgrpLST_sf.rds")

# Read in evacuation risk data. Developed parts of census units; need to filter and join.
load("DATA/FEMA/CT/nfhza_census.Rds")
load("DATA/FEMA/CT/hevac_census.Rds")


# join burdens to a common blkgrp sf and create indicator variable
ct_blkgrps_sf <- ct_blkgrps_sf_noTransit %>% 
  as.data.frame() %>% 
  transmute(GEOID = GEOID, NoTransitAccess = "Y") %>% 
  left_join(ct_blkgrps_sf,., by = "GEOID")

ct_blkgrps_sf <- headway80thblkgrps_bus %>% 
  as.data.frame() %>% 
  transmute(GEOID = GEOID, headway80th_bus = "Y") %>% 
  left_join(ct_blkgrps_sf,., by = "GEOID")

ct_blkgrps_sf <- headway80thblkgrps_CR %>% 
  as.data.frame() %>% 
  transmute(GEOID = GEOID, headway80th_CR = "Y") %>% 
  left_join(ct_blkgrps_sf,., by = "GEOID")

ct_blkgrps_sf <- ct_blkgrp_walkability_sf %>% 
  as.data.frame() %>% 
  filter(NatWalkInd <= 5.8) %>% 
  transmute(GEOID = GEOID, leastWalkable = "Y") %>% 
  left_join(ct_blkgrps_sf,., by = "GEOID")

ct_blkgrps_sf <- lai_ct_blkgrp %>% 
  as.data.frame() %>% 
  filter(percent_rank(hh7_t) > 0.8) %>% 
  transmute(GEOID = GEOID, costBurdened = "Y") %>% 
  left_join(ct_blkgrps_sf,., by = "GEOID")

ct_blkgrps_sf <- ct_blkgrpLST_sf %>% 
  as.data.frame() %>% 
  select(GEOID, meanAvgLST:UHINight) %>% 
  left_join(ct_blkgrps_sf,., by="GEOID")

# join NFHZA flood hazard and hurricane evac records where percentage of pop of concern in flood or hurricane evac prone areas is 80th percentile
ct_blkgrps_sf <- ct_blkgrps_nfhza %>% 
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
           percent_rank(sum_NewCT_LOWINC/sum_NewPop) >= 0.8) %>% 
  transmute(GEOID = GEOID, nfhzaRisk = "F") %>% 
  left_join(ct_blkgrps_sf,., by="GEOID")

ct_blkgrps_sf <- ct_blkgrps_hevac %>% 
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
           percent_rank(sum_NewCT_LOWINC/sum_NewPop) >= 0.8) %>% 
  transmute(GEOID = GEOID, hevacRisk = "H") %>% 
  left_join(ct_blkgrps_sf,., by="GEOID")

# repeat for census tracts
ct_tracts_sf <- ct_tracts_sf_noTransit %>% 
  as.data.frame() %>% 
  transmute(GEOID = GEOID, NoTransitAccess = "Y") %>% 
  left_join(ct_tracts_sf,., by = "GEOID") 
  
ct_tracts_sf <- headway80thtracts_bus %>% 
  as.data.frame() %>% 
  transmute(GEOID = GEOID, headway80th_bus = "Y") %>% 
  left_join(ct_tracts_sf,., by = "GEOID")

ct_tracts_sf <- headway80thtracts_CR %>% 
  as.data.frame() %>% 
  transmute(GEOID = GEOID, headway80th_CR = "Y") %>% 
  left_join(ct_tracts_sf,., by = "GEOID")

ct_tracts_sf <- ct_tract_walkability_sf %>% 
  as.data.frame() %>% 
  transmute(GEOID = GEOID, leastWalkable = "Y") %>% 
  left_join(ct_tracts_sf,., by = "GEOID")

ct_tracts_sf <- lai_ct_tract %>% 
  as.data.frame() %>% 
  transmute(GEOID = GEOID, costBurdened = "Y") %>% 
  left_join(ct_tracts_sf,., by = "GEOID")

# join NFHZA flood hazard and hurricane evac records where percentage of pop of concern in flood or hurricane evac prone areas is 80th percentile
ct_tracts_sf <- ct_tracts_nfhza %>% 
  as.data.frame() %>% 
  group_by(GEOID) %>% 
  mutate(NewOver18 = Proportion*Over18E,
         NewtotalHHE = Proportion*totalHHE) %>% 
  summarize(across(NewDisabled:NewtotalHHE, ~ sum(.x, na.rm = T), 
                   .names = "sum_{.col}")) %>% 
  filter(percent_rank(sum_NewDisabled/sum_NewOver18) >= 0.8 | 
           percent_rank(sum_NewNoCar/sum_NewtotalHHE) >= 0.8) %>% 
  transmute(GEOID = GEOID, nfhzaRisk = "F") %>% 
  left_join(ct_tracts_sf,., by="GEOID")

ct_tracts_sf <- ct_tracts_hevac %>% 
  as.data.frame() %>% 
  group_by(GEOID) %>% 
  mutate(NewOver18 = Proportion*Over18E,
         NewtotalHHE = Proportion*totalHHE) %>% 
  summarize(across(NewDisabled:NewtotalHHE, ~ sum(.x, na.rm = T), 
                   .names = "sum_{.col}")) %>% 
  filter(percent_rank(sum_NewDisabled/sum_NewOver18) >= 0.8 | 
           percent_rank(sum_NewNoCar/sum_NewtotalHHE) >= 0.8) %>% 
  transmute(GEOID = GEOID, hevacRisk = "H") %>% 
  left_join(ct_tracts_sf,., by="GEOID")

# replace na's with "N"
ct_blkgrps_sf <- ct_blkgrps_sf %>% 
  mutate(across(NoTransitAccess:costBurdened, 
                ~replace_na(.x, "N"))
         )

ct_tracts_sf <- ct_tracts_sf %>% 
  mutate(across(NoTransitAccess:costBurdened, 
                ~replace_na(.x, "N"))
  )

# add indicator variables for any kind of transport burden AND pop of concern above 80th percentile, followed by code for burdens where both conditions are met, followed by combination indicator and count of indicators satisfied. Final code indicators: P = Pollution or Emissions, T = Transportation, H = Heat, E = Evacuation
ct_blkgrps_sf <- ct_blkgrps_sf %>% 
  mutate(
    AnyPopConcern = case_when(
    percent_rank(minority_pctE) >= 0.8 | 
      percent_rank(pct_under5E) >= 0.8 | 
      percent_rank(pct_under18E) >= 0.8 | 
      percent_rank(pct_over64E) >= 0.8 | 
      percent_rank(pct_lthsE) >= 0.8 | 
      percent_rank(pct2povE) >= 0.8 | 
      percent_rank(eng_limit_pctE) >= 0.8 | 
      CT_INCOME == "I" ~ "Y"
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
saveRDS(ct_blkgrps_sf, file = "DATA/ct_blkgrps_sf_CUM.Rds")

# repeat for tracts
ct_tracts_sf <- ct_tracts_sf %>% 
  mutate(
    AnyPopConcern = case_when(
      percent_rank(pct_disabilityOver18E) >= 0.8 | 
        percent_rank(pct_HHnoCarE) >= 0.8 ~ "Y"
    ),
    AnyTransportBurden = case_when(
      NoTransitAccess == "Y" | 
        headway80th_bus == "Y" | 
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

# table showing how many and what percentage of each population of concern falls within block groups meeting 1, 2, 3, or 4 of the burdens
cum_burden_df <- ct_blkgrps_sf %>% 
  as.data.frame() %>% 
  mutate(CT_LOWINC = if_else(CT_INCOME == "I", totalpopE, 0)) %>% 
  mutate(CT_LOWINC = replace_na(CT_LOWINC,0)) %>% 
  mutate(BurdenCount = as.character(BurdenCount)) %>%
  group_by(BurdenCount) %>% 
  summarize(across(c(totalpopE,householdsE,minorityE,num2povE,eng_limitE,under5E,under18E,over64E,lthsE,CT_LOWINC), ~ sum(.x, na.rm = T), .names = "{col}")) %>% 
  pivot_longer(., cols = totalpopE:CT_LOWINC, names_to = "Group") %>% 
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
    Group == "CT_LOWINC" ~ "CT Low Income"
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
write_csv(cum_burden_df,"tables/CT_cum_burden.csv")

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
       title = "Percentage of Connecticut Population within\nCumulative Burden Categories") + 
  theme_light() +
  theme(panel.grid.major.y = element_blank(),
        panel.border = element_blank(),
        axis.ticks.y = element_blank()) +
  scale_fill_discrete(labels = c("4 categories", "3 categories", 
                                 "2 categories", "1 category"))
# save plot as png
ggsave("images/CT_CUM_BURDEN_Graph.png")


# map showing block groups by number of cumulative burdens (i.e., high concentration of pop of concern & 1 or more burdens in 80th percentile)
# tmap_mode("view")
# ct_blkgrps_sf %>%
#   filter(BurdenCount > 0) %>%
#   tm_shape(.) + tm_fill(col = "BurdenCount", alpha = 0.5, palette = "YlOrRd")

tmap_mode("plot")
# grab neighboring state boundaries for context
ne_states_sf_cb <- states(cb = TRUE) %>% 
  filter(STUSPS %in% c("MA","CT","RI","NY","NH","VT","ME"))

# grab municipal boundaries
ct_towns_sf <- county_subdivisions(state = "CT", cb = TRUE) %>% 
  st_transform(., crs = 2775)

# create point layer of towns for context
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
                     "Old Saybrook",
                     "Greenwich")) %>% 
  st_transform(., crs = 2775) %>% 
  st_centroid(of_largest_polygon = TRUE)

# Create road layer for context
ct_highways <- primary_roads() %>% 
  filter(FULLNAME %in% c("I- 84","I- 91","I- 95","I- 395")) %>% 
  tmaptools::crop_shape(., ne_states_sf_cb) %>% 
  st_transform(., crs = 2775)
# group_by(FULLNAME) %>% 
# summarize(RTTYP = unique(RTTYP),
#           MTFCC = unique(MTFCC))

# Extract highway segments for labeling
I84roadSegment <- ct_highways %>% 
  filter(LINEARID == "1105281313426")

I91roadSegment <- ct_highways %>% 
  filter(LINEARID == "1104470395750")

I95roadSegment <- ct_highways %>% 
  filter(LINEARID == "11013782168181")

I95roadSegment2 <- ct_highways %>% 
  filter(LINEARID == "1105281377007")

I395roadSegment <- ct_highways %>% 
  filter(LINEARID == "1104493001068")

# Create custom icons of highway shields
I95 <- tmap_icons(file = "https://upload.wikimedia.org/wikipedia/commons/thumb/6/61/I-95.svg/200px-I-95.svg.png")
I395 <- tmap_icons("https://upload.wikimedia.org/wikipedia/commons/thumb/1/16/I-395.svg/200px-I-395.svg.png")
I91 <- tmap_icons("https://upload.wikimedia.org/wikipedia/commons/thumb/9/90/I-91.svg/200px-I-91.svg.png")
I84 <- tmap_icons("https://upload.wikimedia.org/wikipedia/commons/thumb/5/51/I-84.svg/200px-I-84.svg.png")

m <- ct_blkgrps_sf %>% 
  filter(BurdenCount > 0) %>% 
  tm_shape(., unit = "mi") + 
  tm_fill(col = "BurdenCount", palette = "YlOrRd", 
          title = "Number of\nBurden\nCategories") +
  tm_shape(ct_blkgrps_sf) + tm_borders(lwd = 0.1) +
  tm_shape(ne_states_sf_cb) + tm_borders(lwd = 0.2, alpha = 0.8) +
  tm_text("STUSPS", size = 0.7, remove.overlap = TRUE, col = "gray") +
  tm_shape(ct_highways) + tm_lines(col = "seashell4", alpha = 0.7) +
  tm_shape(I95roadSegment) +
  tm_symbols(shape = I95, border.lwd = NA, size = 0.1) +
  tm_shape(I95roadSegment2) +
  tm_symbols(shape = I95, border.lwd = NA, size = 0.1) +
  tm_shape(I395roadSegment) +
  tm_symbols(shape = I395, border.lwd = NA, size = 0.1) +
  tm_shape(I91roadSegment) +
  tm_symbols(shape = I91, border.lwd = NA, size = 0.1) +
  tm_shape(I84roadSegment) +
  tm_symbols(shape = I84, border.lwd = NA, size = 0.1) +
  tm_shape(ct_towns_sf_pts) + tm_dots() +
  tm_text("NAME", size = 0.4, col = "black",
          xmod = 0.7, ymod = 0.2, shadow = TRUE) +
  tm_scale_bar(breaks = c(0,10,20), position = c(0.55,0.005)) +
  tm_layout(title = "Cumulative\nBurdens\nby Census\nBlock Group",
            frame = FALSE, main.title.size = 0.8,
            legend.outside = TRUE,
            legend.title.size = 0.8,
            legend.outside.position = c("right", "top"))

tmap_save(m, "images/CT_CUM_BURDEN_map.png",
          height = 4, width = 8, units = "in", dpi = 600)

# replicate BG map for MA EJ only 
mEJ <- ct_blkgrps_sf %>% 
  filter(CT_INCOME == "I") %>% 
  filter(BurdenCount > 0) %>% 
  tm_shape(., unit = "mi") + 
  tm_fill(col = "BurdenCount", palette = "YlOrRd", 
          title = "Number of\nBurdens") +
  # tm_shape(ct_blkgrps_sf) + tm_borders(lwd = 0.1) +
  tm_shape(ct_towns_sf) + tm_borders(lwd = 0.1) +
  tm_shape(ne_states_sf_cb) + tm_borders(lwd = 0.2, alpha = 0.8) +
  tm_text("STUSPS", size = 0.7, remove.overlap = TRUE, col = "gray") +
  tm_shape(ct_highways) + tm_lines(col = "seashell4", alpha = 0.7) +
  tm_shape(I95roadSegment) +
  tm_symbols(shape = I95, border.lwd = NA, size = 0.1) +
  tm_shape(I95roadSegment2) +
  tm_symbols(shape = I95, border.lwd = NA, size = 0.1) +
  tm_shape(I395roadSegment) +
  tm_symbols(shape = I395, border.lwd = NA, size = 0.1) +
  tm_shape(I91roadSegment) +
  tm_symbols(shape = I91, border.lwd = NA, size = 0.1) +
  tm_shape(I84roadSegment) +
  tm_symbols(shape = I84, border.lwd = NA, size = 0.1) +
  tm_shape(ct_towns_sf_pts) + tm_dots() +
  tm_text("NAME", size = 0.4, col = "black",
          xmod = 0.7, ymod = 0.2, shadow = TRUE) +
  tm_scale_bar(breaks = c(0,10,20), position = c(0.55,0.005)) +
  tm_layout(title = "Cumulative\nBurdens\nby Census\nBlock Group for\nCT Low Income\nCommunities",
            frame = FALSE, main.title.size = 0.8,
            legend.outside = TRUE,
            legend.title.size = 0.8,
            legend.outside.position = c("right", "top"))

tmap_save(mEJ, "images/CT_CUM_BURDEN_EJ_map.png",
          height = 4, width = 8, units = "in", dpi = 600)

# what percentage of EJ BGs have 1 or more burdens?
EJbgs <- ct_blkgrps_sf %>% 
  filter(CT_INCOME == "I") %>% 
  nrow()
EJbgs1_4 <- ct_blkgrps_sf %>% 
  filter(BurdenCount > 0 & 
           (CT_INCOME == "I")) %>% 
  nrow()
# wha percentage of EJ BGs are high burden?
EJbgs1_4/EJbgs*100
# what percentage of all high burden BGs are these?
hiBurdenBGs <- ct_blkgrps_sf %>% 
  filter(BurdenCount > 0) %>% 
  nrow()
hiBurdenBGs/nrow(ct_blkgrps_sf)*100
EJbgs1_4/hiBurdenBGs*100

# table showing how many and what percentage of each population of concern falls under each type of burden: Emissions, Transport, Heat, Evacuation (non-exclusive)
Pburden <- ct_blkgrps_sf %>% 
  as.data.frame() %>% 
  mutate(CT_LOWINC = if_else(CT_INCOME == "I", totalpopE, 0)) %>% 
  mutate(CT_LOWINC = replace_na(CT_LOWINC,0)) %>% 
  # filter(EmissionsBurden != "NA") %>%
  group_by(EmissionsBurden) %>% 
  summarize(across(c(totalpopE,householdsE,minorityE,num2povE,eng_limitE,under5E,under18E,over64E,lthsE,CT_LOWINC), ~ sum(.x, na.rm = T), .names = "{col}")) %>% 
  rename(Burden = EmissionsBurden) %>% 
  replace_na(list(Burden = "NotP"))

Tburden <- ct_blkgrps_sf %>% 
  as.data.frame() %>% 
  mutate(CT_LOWINC = if_else(CT_INCOME == "I", totalpopE, 0)) %>% 
  mutate(CT_LOWINC = replace_na(CT_LOWINC,0)) %>% 
  # filter(TransportBurden != "NA") %>%
  group_by(TransportBurden) %>% 
  summarize(across(c(totalpopE,householdsE,minorityE,num2povE,eng_limitE,under5E,under18E,over64E,lthsE,CT_LOWINC), ~ sum(.x, na.rm = T), .names = "{col}")) %>% 
  rename(Burden = TransportBurden) %>% 
  replace_na(list(Burden = "NotT"))

Hburden <- ct_blkgrps_sf %>% 
  as.data.frame() %>% 
  mutate(CT_LOWINC = if_else(CT_INCOME == "I", totalpopE, 0)) %>% 
  mutate(CT_LOWINC = replace_na(CT_LOWINC,0)) %>%
  # filter(HeatBurden != "NA") %>%
  group_by(HeatBurden) %>% 
  summarize(across(c(totalpopE,householdsE,minorityE,num2povE,eng_limitE,under5E,under18E,over64E,lthsE,CT_LOWINC), ~ sum(.x, na.rm = T), .names = "{col}")) %>% 
  rename(Burden = HeatBurden) %>% 
  replace_na(list(Burden = "NotH"))

Eburden <- ct_blkgrps_sf %>% 
  as.data.frame() %>% 
  mutate(CT_LOWINC = if_else(CT_INCOME == "I", totalpopE, 0)) %>% 
  mutate(CT_LOWINC = replace_na(CT_LOWINC,0)) %>% 
  # filter(EvacBurden != "NA") %>%
  group_by(EvacBurden) %>% 
  summarize(across(c(totalpopE,householdsE,minorityE,num2povE,eng_limitE,under5E,under18E,over64E,lthsE,CT_LOWINC), ~ sum(.x, na.rm = T), .names = "{col}")) %>% 
  rename(Burden = EvacBurden) %>% 
  replace_na(list(Burden = "NotE"))

burden_types_df <- rbind(Pburden,Tburden,Hburden,Eburden) %>% 
  pivot_longer(cols = totalpopE:CT_LOWINC, names_to = "Group") %>% 
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
    Group == "CT_LOWINC" ~ "CT Low Income"
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
write_csv(burden_types_df,"tables/CT_burden_types.csv")

# table showing towns with block groups with 3 or 4 burdens
burdens_town_df <- ct_blkgrps_sf %>%
  transmute(BurdenCount = as.character(BurdenCount)) %>% 
  st_centroid(.) %>% 
  st_intersection(.,ct_towns_sf) %>% 
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
write_csv(burdens_town_df, "tables/CT_burdens_town.csv")


# table showing EJ towns with block groups with 3 or 4 burdens
burdens_EJtown_df <- ct_blkgrps_sf %>%
  filter(CT_INCOME == "I") %>%
  transmute(BurdenCount = as.character(BurdenCount)) %>% 
  st_centroid(.) %>% 
  st_intersection(.,ct_towns_sf) %>% 
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
  transmute("CT Low Inc City/Town" = NAME, `3 Burdens` = `3`, `4 Burdens` = `4`) %>% 
  filter(`3 Burdens` > 0 | `4 Burdens` > 0) %>%
  rowwise() %>% 
  mutate(`3+ Burdens` = sum(c_across(2:3)))
# select(`City/Town`,Pct1:Rank4)
burdens_EJtown_df %>% kable(longtable = T, booktabs = T,
                          format.args = list(big.mark = ','), 
                          digits = 1,
                          caption = "CT Low Income Municipalities with Block Groups meeting 3 or 4 Cumulative Burdens", align = "r") %>% 
  # col.names = c(names(.)[1:2],"Number of Over 64 in Block Groups","Pct of Over 64 in City/Town")) %>% 
  # column_spec(3:4, width = "4cm") %>%
  kable_styling(latex_options = c("repeat_header","striped")) 
# save as csv
write_csv(burdens_EJtown_df, "tables/CT_burdens_EJtown.csv")

# how many EJ municipalities are there?
EJtowns <- ct_blkgrps_sf %>%
  select(-NAME) %>% 
  filter(CT_INCOME == "I") %>%
  st_centroid(.) %>% 
  st_intersection(.,ct_towns_sf) %>% 
  as.data.frame() %>% 
  group_by(NAME) %>%
  summarize(`Block Groups` = n())
# save as csv
write_csv(EJtowns, "tables/CT_EJtowns.csv")
# what percentage of EJ municipalites have high burden BGs?
nrow(burdens_EJtown_df)/nrow(EJtowns)*100


# map of towns with block groups meeting 3 - 4 burdens
tmap_mode("plot")
m <- ct_towns_sf %>% 
  right_join(.,burdens_town_df, by = c("NAME" = "City/Town")) %>% 
  tm_shape(., unit = "mi", bbox = ct_towns_sf) + 
  tm_fill(col = "red", alpha = 0.5) +
  tm_text("NAME", size = 0.4, remove.overlap = TRUE, col = "black") +
  tm_shape(ct_towns_sf) + tm_borders(col = "gray", lwd = 0.2) +
  tm_shape(ne_states_sf_cb) + tm_borders(lwd = 0.2, alpha = 0.8) +
  tm_text("STUSPS", size = 0.7, remove.overlap = TRUE, col = "gray") +
  tm_shape(ct_highways) + tm_lines(col = "seashell4", alpha = 0.7) +
  tm_shape(I95roadSegment) +
  tm_symbols(shape = I95, border.lwd = NA, size = 0.1) +
  tm_shape(I95roadSegment2) +
  tm_symbols(shape = I95, border.lwd = NA, size = 0.1) +
  tm_shape(I395roadSegment) +
  tm_symbols(shape = I395, border.lwd = NA, size = 0.1) +
  tm_shape(I91roadSegment) +
  tm_symbols(shape = I91, border.lwd = NA, size = 0.1) +
  tm_shape(I84roadSegment) +
  tm_symbols(shape = I84, border.lwd = NA, size = 0.1) +
  # tm_shape(ct_towns_sf_pts) + tm_dots() +
  # tm_text("NAME", size = 0.4, col = "black",
  #         xmod = 0.7, ymod = 0.2, shadow = TRUE) +
  tm_scale_bar(breaks = c(0,10,20), position = c(0.55,0.005)) +
  # tm_add_legend(type = "fill", col = "white") +
  tm_layout(main.title = "Municipalities with Block Groups meeting\n3 - 4 Cumulative Burden Categories",
            main.title.position = c("center", "TOP"),
            main.title.size = 0.9,
            frame = FALSE, legend.outside = TRUE,
            legend.title.size = 0.8,
            legend.outside.position = c("right", "top"))

tmap_save(m, "images/CT_CUM_BURDEN_TOWN_map.png",
          height = 4, width = 8, units = "in", dpi = 600)


# table showing state senate district that have 3 - 4 burden categories
senate_districts <- st_read("DATA/shapefiles/senatect_37800_0000_2010_s100_census_1_shp/nad83",
                            "senatect_37800_0000_2010_s100_census_1_shp_nad83_meters") %>% 
  st_transform(., crs = 2775) %>% 
  st_make_valid()

burdens_senate_df <- ct_blkgrps_sf %>%
  transmute(BurdenCount = as.character(BurdenCount)) %>% 
  st_centroid(.) %>% 
  st_intersection(.,senate_districts) %>% 
  as.data.frame() %>% 
  group_by(BurdenCount, NAMELSAD10) %>%
  summarize(`Block Groups` = n()) %>% 
  pivot_wider(id_cols = NAMELSAD10, names_from = BurdenCount, 
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
  transmute(`Senate District` = NAMELSAD10, 
            `3 Burdens` = `3`, `4 Burdens` = `4`) %>% 
  # filter(`3 Burdens` > 0 | `4 Burdens` > 0) %>%
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
write_csv(burdens_senate_df, "tables/CT_burdens_senate.csv")

# map of senate districts with block groups meeting 3 - 4 burdens
tmap_mode("plot")
m <- senate_districts %>% 
  right_join(.,burdens_senate_df, by = c("NAMELSAD10" = "Senate District")) %>% 
  tm_shape(., unit = "mi", bbox = senate_districts) + 
  tm_fill(col = "red", alpha = 0.5) +
  tm_shape(senate_districts) + tm_borders(col = "seashell4") +
  tm_shape(ne_states_sf_cb) + tm_borders(lwd = 0.2, alpha = 0.8) +
  tm_text("STUSPS", size = 0.7, remove.overlap = TRUE, col = "gray") +
  tm_shape(ct_highways) + tm_lines(col = "seashell4", alpha = 0.7) +
  tm_shape(I95roadSegment) +
  tm_symbols(shape = I95, border.lwd = NA, size = 0.1) +
  tm_shape(I95roadSegment2) +
  tm_symbols(shape = I95, border.lwd = NA, size = 0.1) +
  tm_shape(I395roadSegment) +
  tm_symbols(shape = I395, border.lwd = NA, size = 0.1) +
  tm_shape(I91roadSegment) +
  tm_symbols(shape = I91, border.lwd = NA, size = 0.1) +
  tm_shape(I84roadSegment) +
  tm_symbols(shape = I84, border.lwd = NA, size = 0.1) +
  tm_shape(ct_towns_sf_pts) + tm_dots() +
  tm_text("NAME", size = 0.4, col = "black",
          xmod = 0.7, ymod = 0.2, shadow = TRUE) +
  tm_scale_bar(breaks = c(0,10,20), position = c(0.55,0.005)) +
  tm_layout(main.title = "Senate Districts with Block Groups meeting\n3 - 4 Cumulative Burden Categories",
            main.title.position = c("center","TOP"),
            frame = FALSE, main.title.size = 0.9,
            legend.outside = TRUE,
            legend.title.size = 0.8,
            legend.outside.position = c("right", "top"),
            title.position = c("left","bottom"))

tmap_save(m, "images/CT_CUM_BURDEN_SENATE_map.png",
          height = 4, width = 8, units = "in", dpi = 600)


# table showing block groups by state house district that have 3 - 4 burden categories
house_districts <- st_read("DATA/shapefiles/housect_37800_0000_2010_s100_census_1_shp/nad83",
                            "housect_37800_0000_2010_s100_census_1_shp_nad83_meters") %>% 
  st_transform(., crs = 2775) %>% 
  st_make_valid()

burdens_house_df <- ct_blkgrps_sf %>%
  transmute(BurdenCount = as.character(BurdenCount)) %>% 
  st_centroid(.) %>% 
  st_intersection(.,house_districts) %>% 
  as.data.frame() %>% 
  group_by(BurdenCount, NAMELSAD10) %>%
  summarize(`Block Groups` = n()) %>% 
  pivot_wider(id_cols = NAMELSAD10, names_from = BurdenCount, 
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
  transmute(`House District` = NAMELSAD10, 
            `3 Burdens` = `3`, `4 Burdens` = `4`) %>% 
  # filter(`3 Burdens` > 0 | `4 Burdens` > 0) %>%
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
write_csv(burdens_house_df, "tables/CT_burdens_house.csv")

# map of house districts with block groups meeting 3 - 4 burdens
tmap_mode("plot")
m <- house_districts %>% 
  right_join(.,burdens_house_df, by = c("NAMELSAD10" = "House District")) %>% 
  tm_shape(., unit = "mi", bbox = senate_districts) + 
  tm_fill(col = "red", alpha = 0.5) +
  tm_shape(house_districts) + tm_borders(col = "seashell4") +
  tm_shape(ne_states_sf_cb) + tm_borders(lwd = 0.2, alpha = 0.8) +
  tm_text("STUSPS", size = 0.7, remove.overlap = TRUE, col = "gray") +
  tm_shape(ct_highways) + tm_lines(col = "seashell4", alpha = 0.7) +
  tm_shape(I95roadSegment) +
  tm_symbols(shape = I95, border.lwd = NA, size = 0.1) +
  tm_shape(I95roadSegment2) +
  tm_symbols(shape = I95, border.lwd = NA, size = 0.1) +
  tm_shape(I395roadSegment) +
  tm_symbols(shape = I395, border.lwd = NA, size = 0.1) +
  tm_shape(I91roadSegment) +
  tm_symbols(shape = I91, border.lwd = NA, size = 0.1) +
  tm_shape(I84roadSegment) +
  tm_symbols(shape = I84, border.lwd = NA, size = 0.1) +
  tm_shape(ct_towns_sf_pts) + tm_dots() +
  tm_text("NAME", size = 0.4, col = "black",
          xmod = 0.7, ymod = 0.2, shadow = TRUE) +
  tm_scale_bar(breaks = c(0,10,20), position = c(0.55,0.005)) +
  tm_layout(main.title = "House Districts with Block Groups meeting\n3 - 4 Cumulative Burden Categories",
            main.title.position = c("center","TOP"),
            frame = FALSE, main.title.size = 0.9,
            legend.outside = TRUE,
            legend.title.size = 0.8,
            legend.outside.position = c("right", "bottom"),
            title.position = c("left","bottom"))

tmap_save(m, "images/CT_CUM_BURDEN_HOUSE_map.png",
          height = 4, width = 8, units = "in", dpi = 600)






# # map with block groups by burden count and senate districts
# # extract centorid of districts for labeling
# senate_districts_centroid <- st_centroid(senate_districts, 
#                                          of_largest_polygon = T)
# 
# mS <- ct_blkgrps_sf %>% 
#   filter(BurdenCount > 0) %>% 
#   tm_shape(., unit = "mi") + 
#   tm_fill(col = "BurdenCount", palette = "YlOrRd", 
#           title = "Number of\nBurdens") +
#   tm_shape(ct_blkgrps_sf) + tm_borders(lwd = 0.1) +
#   tm_shape(ne_states_sf_cb) + tm_borders(lwd = 0.2, alpha = 0.8) +
#   tm_text("STUSPS", size = 0.7, remove.overlap = TRUE, col = "gray") +
#   tm_shape(ct_highways) + tm_lines(col = "seashell4", lwd = 1, alpha = 0.5) +
#   tm_shape(ct_highways2nd) + tm_lines(col = "seashell4", lwd = 1, alpha = 0.5) +
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
#   tm_shape(ct_towns_sf_pts) + tm_dots() +
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
# ct_towns_sf %>% 
#   left_join(.,burdens_town_df, by = c("NAME" = "City/Town")) %>% 
#   filter(Pct3 >0 | Pct4>0) %>% 
#   tm_shape(.) + tm_fill(col = "red", alpha = 0.5)
