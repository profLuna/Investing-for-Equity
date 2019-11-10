# TCI Analysis of Demographic Data

library(tidycensus)
library(tidyverse)
library(tmap)
library(sf)
library(tigris)
options(tigris_use_cache = TRUE)

census_api_key("f2776fbc29cf847505de9308a82c8d65290d16b3")

# Load list of variables to identify tables of interest
v17 <- load_variables(2017, "acs5", cache = TRUE)

# tidycensus df with stat codes
# unique(fips_codes$state)

# create vector of New England states
ne_states <- c("CT","MA","ME","NH","RI","VT")

# use purrr::reduce function in combination with sf::rbind to download block groups for list of states and then bind them to one sf. 
ne_pop_sf <- reduce(
  map(ne_states, function(x) {
    get_acs(geography = "block group", 
            variables = c(totalpop = "B03002_001", 
                          whitepop = "B03002_003",
                          medhhinc = "B19013_001"),
            state = x, output = "wide", geometry = TRUE)
    }),
  rbind
)

# Extract the state names to a new column
# provide 1 or multiple + whole words \\b, not [ ] in a string that ends in a comma ^, until the end of the string $. (so basically, provide all whole words after comma) 
ne_pop_sf <- ne_pop_sf %>% 
  mutate(STATE = str_extract(NAME, '\\b[^,]+$'))

# map it out
tm_shape(ne_pop_sf) + tm_polygons(col = "STATE")


# Do the same for towns across New England, although note that tidycensus does not support that geography yet so we need to import shapefiles separately with tigris and then join
# use purrr::map_df to download df and bind vector of states
ne_towns_df <- map_df(ne_states, function(x) {
    get_acs(geography = "county subdivision", 
            variables = c(totalpop = "B03002_001", 
                          whitepop = "B03002_003",
                          medhhinc = "B19013_001"),
            state = x, output = "wide")
  })

# acquire the polygons for county subdivisions for ne_towns using tigris::rbind_tigris
ne_towns_sp <- rbind_tigris(
  lapply(
    ne_states, function(x){
      county_subdivisions(state = x, cb = FALSE)
    }
  )
)
# plot(ne_towns_sp)

# join the demographics to the polygons
ne_towns_sp <- geo_join(spatial_data = ne_towns_sp,
                        data_frame = ne_towns,
                        by_sp = "GEOID", by_df = "GEOID")
# tm_shape(ne_towns_sp) + tm_polygons("totalpopE")
# convert to sf for easier handling
ne_towns_sf <- st_as_sf(ne_towns_sp)
rm(ne_towns_sp)
# add column with state names
ne_towns_sf <- ne_towns_sf %>% 
  mutate(STATE = str_extract(NAME.1, '\\b[^,]+$'))
tm_shape(ne_towns_sf) + tm_polygons("STATE")


# download table of counts of household income categories, sum up households in categories below statewide median of $74,167
maACS17_blkgrp_medhhinc <- get_acs(geography = "block group", 
                                   table = "B19001",
                                   state = "MA", output = "wide") %>% 
  transmute(GEOID = GEOID,
            households = B19001_001E,
            medhhinc_lt50 = B19001_002E+
              B19001_003E+
              B19001_004E+
              B19001_005E+
              B19001_006E+
              B19001_007E+
              B19001_008E+
              B19001_009E+
              B19001_010E,
            medhhinc_lt75 = medhhinc_lt50+
              B19001_011E+
              B19001_012E,
            pctmedhhinc_lt50 = ifelse(
              households==0,0,medhhinc_lt50/households*100
              ), # 65% of median is $48,208. Closest range is 45 - 49,9
            pctmedhhinc_lt75 = ifelse(
              households==0,0,medhhinc_lt75/households*100
              )
            ) # statewide median of $74,167. Closest range is 60 - 74,9

# download language isolation variables by block group
# lang_vars <- paste0("C16002_0",c("01","04","07","10","13"))
# neACS17blkgrp_langIsol <- map_df(ne_states, function(x) {
#   get_acs(geography = "block group", variables = c("C16002"),
#           state = x, output = "wide")
#   }) %>% 
#   transmute(GEOID = GEOID,
#             # NAME = NAME,
#             # STATE = str_extract(NAME, '\\b[^,]+$'),
#             eli_households = C16002_001E,
#             eli_limited = C16002_004E + C16002_007E + C16002_010E + C16002_013E,
#             pct_eli_limited = ifelse(
#               eli_households == 0,0,eli_limited/eli_households*100)
#             )


# Download C16002. Household Language by Household Limited English Speaking Status
eng_limited <- map_df(ne_states, function(x) {
  get_acs(geography = "block group", variables = c("C16002_001","C16002_004",
                                                   "C16002_007","C16002_010",
                                                   "C16002_013"),
          state = x)})
# Isolate limited English speaking households and compute derived estimates and MOEs
eng_limited_est <- eng_limited %>% 
  filter(variable != "C16002_001") %>% 
  group_by(GEOID) %>% 
  summarize(eng_liE = sum(estimate),
            eng_liM = moe_sum(moe,estimate))
# Join with total households and calculate derived proportions and MOEs, along with upper and lower confidence interval values from MOE
eng_limited_pct <- eng_limited %>% 
  filter(variable == "C16002_001") %>% 
  group_by(GEOID) %>% 
  left_join(., eng_limited_est, by = c("GEOID","GEOID")) %>% 
  mutate(eng_li_pE = ifelse(estimate==0,0,eng_liE/estimate),
         eng_li_pM = moe_prop(eng_liE,estimate,eng_liM,moe),
         eng_li_pctE = eng_li_pE*100,
         eng_li_pctM = eng_li_pM*100,
         eng_li_pctE_UC = eng_li_pctE + eng_li_pctM,
         eng_li_pctE_LC = ifelse(
           eng_li_pctE < eng_li_pctM, 0, eng_li_pctE - eng_li_pctM))




# Download English language isolation variables and compute derived estimates as well as derived MOEs
# eng_limited <- map_df(ne_states, function(x) {
#   get_acs(geography = "block group", variables = c("C16002_001","C16002_004",
#                                                    "C16002_007","C16002_010",
#                                                    "C16002_013"),
#           state = x, output = "wide")}) %>% 
#   transmute(GEOID = GEOID,
#             lang_hhE = C16002_001E,
#             lang_hhM = C16002_001M,
#          lang_isolE = 
#            C16002_004E + C16002_007E + C16002_010E + C16002_013E,
#          lang_isolM = 
#           sqrt(C16002_004M^2+C16002_007M^2+C16002_010M^2+C16002_013M^2),
#          lang_isolE_LC = ifelse(
#            lang_isolE < lang_isolM, 0, lang_isolE - lang_isolM),
#          langisolE_UC = lang_isolE + lang_isolM,
#          pct_lang_isolE = ifelse(lang_hhE==0,0,lang_isolE/lang_hhE)*100,
#          pct_lang_isolM = 
#            1/lang_hhE*sqrt(
#              lang_isolM^2-((pct_lang_isolE/100)^2*lang_hhM^2))*100,
#          pct_lang_isolE_UC = pct_lang_isolE + pct_lang_isolM,
#          pct_lang_isolE_LC = ifelse(
#            pct_lang_isolE < pct_lang_isolM,0,pct_lang_isolE - pct_lang_isolM))


# join English language isolation to block groups
ne_pop_sf <- ne_pop_sf %>% 
  left_join(neACS17blkgrp_langIsol, by = c("GEOID","GEOID"))
# tmap_mode("view")
# tm_shape(ne_pop_sf) + tm_polygons("pct_eli_limited")
# tmap_mode("plot")
