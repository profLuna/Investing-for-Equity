## Cumulative Synthesis of transportation-related burdens

library(tidyverse)
library(sf)
library(tmap)
library(maptools)

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
