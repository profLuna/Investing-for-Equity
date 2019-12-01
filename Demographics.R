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
                          medhhinc = "B19013_001"),
            state = x, output = "wide")
  }) %>% 
  mutate(medhhincE_UC = medhhincE + medhhincM,
         medhhincE_LC = ifelse(
           medhhincE < medhhincM, 0, medhhincE - medhhincM),
         STATE = str_extract(NAME, '\\b[^,]+$'))
# Calculate statewide average median household income
ne_towns_df %>% 
  group_by(STATE) %>% 
  summarize(avg_medhhinc = mean(medhhincE, na.rm = TRUE))
# add variables to identify EJ criteria thresholds
ne_towns_df <- ne_towns_df %>% 
  mutate(VT_INCOME = if_else(medhhincE < 58231, "I", NULL),
         VT_INCOME_UC = if_else(medhhincE_UC < 58231, "I", NULL),
         VT_INCOME_LC = if_else(medhhincE_LC < 58231, "I", NULL))

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


###### DEMOGRAPHIC DATA FRAMES ##############

### HOUSEHOLDS BY INCOME
# download table of counts of household income categories, sum up households in categories below 65% of MA statewide median of $74,167
B19001 <- map_df(ne_states, function(x) {
  get_acs(geography = "block group", table = "B19001", state = x)})
# Isolate estimate of total households
medhhinc_total <- B19001 %>% 
  filter(variable == "B19001_001") %>% 
  transmute(GEOID = GEOID,
            householdsE = estimate,
            householdsM = moe)
# Isolate household counts less than 65% of statewide median of $74,167, which is $48,208. Closest range is 45 - 49,9.
# create vector of patterns for medhhinc levels below 50k
med_strings <- rep(c(2:10)) %>% 
  formatC(width = 3, format = "d", flag = "0") # add leading 0s
# filter cases by patterns, compute derived sum estimates and MOEs
medhhinclt50 <- B19001 %>% 
  filter(str_detect(variable,paste(med_strings,collapse = "|"))) %>% 
  group_by(GEOID) %>% 
  summarize(medhhinclt50E = sum(estimate),
            medhhinclt50M = moe_sum(moe, estimate)) %>% 
  mutate(medhhinclt50_UC = medhhinclt50E + medhhinclt50M,
         medhhinclt50_LC = ifelse(
           medhhinclt50E < medhhinclt50M, 0, medhhinclt50E - medhhinclt50M))
# Join total households and compute derived proportions
medhhinclt50 <- medhhinclt50 %>% 
  left_join(., medhhinc_total, by = "GEOID") %>% 
  mutate(r2medhhincE = ifelse(householdsE <= 0, 0, medhhinclt50E/householdsE),
    r2medhhincM = moe_prop(medhhinclt50E,householdsE,medhhinclt50M,householdsM),
    pct_medhhinclt50E = r2medhhincE*100,
    pct_medhhinclt50M = r2medhhincM*100,
    pct_medhhinclt50E_UC = pct_medhhinclt50E + pct_medhhinclt50M,
    pct_medhhinclt50E_LC = ifelse(
      pct_medhhinclt50E < pct_medhhinclt50M, 0, 
      pct_medhhinclt50E - pct_medhhinclt50M)) %>% 
  select(-starts_with("r2m"))
# add variables to identify EJ criteria thresholds
medhhinclt50 <- medhhinclt50 %>% 
  mutate(MA_INCOME = if_else(pct_medhhinclt50E >= 25, "I", NULL),
         MA_INCOME_UC = if_else(pct_medhhinclt50E_UC >= 25, "I", NULL),
         MA_INCOME_LC = if_else(pct_medhhinclt50E_LC >= 25, "I", NULL))

### RATIO OF INCOME TO POVERTY LEVEL
# Download ratio of income to poverty level in the past 12 months to calculate the number or percent of a block group’s population in households where the household income is less than or equal to twice the federal “poverty level.” More precisely, percent low-income is calculated as a percentage of those for whom the poverty ratio was known, as reported by the Census Bureau, which may be less than the full population in some block groups. More information on the federally-defined poverty threshold is available at http://www.census.gov/hhes/www/poverty/methods/definitions.html. Note also that poverty status is not determined for people living in institutional group quarters (i.e. prisons, college dormitories, military barracks, nursing homes), so these populations are not included in the poverty estimates (https://www.census.gov/topics/income-poverty/poverty/guidance/poverty-measures.html).
# First, download table of ratio of income to poverty level
C17002 <- map_df(ne_states, function(x) {
  get_acs(geography = "block group", table = "C17002", state = x)
})
# Isolate universe pop for whom poverty status is known
povknown <- C17002 %>% 
  filter(variable == "C17002_001") %>% 
  transmute(GEOID = GEOID,
            povknownE = estimate,
            povknownM = moe,
            povknownE_UC = povknownE + povknownM,
            povknownE_LC = ifelse(
              povknownE < povknownM, 0, povknownE - povknownM))
# Isolate population less than 2x poverty level and compute derived sum esimate along with MOE and UC and LC
num2pov <- C17002 %>% 
  filter(!variable %in% c("C17002_001", "C17002_008")) %>% 
  group_by(GEOID) %>% 
  summarize(num2povE = sum(estimate),
            num2povM = moe_sum(moe, estimate)) %>% 
  mutate(num2povE_UC = num2povE + num2povM,
         num2povE_LC = ifelse(
           num2povE < num2povM, 0, num2povE - num2povM))
# Join tables and compute derived ratios and MOEs and then pcts with UC and LC
povRatio <- povknown %>% 
  left_join(., num2pov, by = "GEOID") %>% 
  mutate(r2povE = ifelse(
    povknownE == 0, 0, num2povE/povknownE),
    r2povM = moe_ratio(num2povE,povknownE,num2povM,povknownM),
    pct2povE = r2povE * 100,
    pct2povM = r2povM * 100,
    pct2povE_UC = pct2povE + pct2povM,
    pct2povE_LC = ifelse(
      pct2povE < pct2povM, 0, pct2povE - pct2povM)) %>% 
  select(-starts_with("r2p"))
# add variables to identify EJ criteria thresholds
povRatio <- povRatio %>% 
  mutate(pctile = percent_rank(pct2povE),
         pctile_UC = percent_rank(pct2povE_UC),
         pctile_LC = percent_rank(pct2povE_LC),
         RI_INCOME = if_else(pctile >= 0.85, "I", NULL),
         RI_INCOME_UC = if_else(pctile_UC >= 0.85, "I", NULL),
         RI_INCOME_LC = if_else(pctile_LC >= 0.85, "I", NULL),
         CT_INCOME = if_else(pct2povE >= 30, "I", NULL),
         CT_INCOME_UC = if_else(pct2povE_UC >= 30, "I", NULL),
         CT_INCOME_LC = if_else(pct2povE_LC >= 30, "I", NULL))



### RACE AND ETHNICITY
# Download B03002 HISPANIC OR LATINO ORIGIN BY RACE in two sets. 
# Start with total pop and white pop in wide format and compute upper and lower confidence values. 
B03002_totwhite <- map_df(ne_states, function(x) {
  get_acs(geography = "block group", variables = c(
    totalpop = "B03002_001",
    nhwhitepop = "B03002_003"),
    state = x, output = "wide")}) %>% 
  transmute(GEOID = GEOID,
            totalpopE = totalpopE,
            totalpopM = totalpopM,
            totalpopE_UC = totalpopE + totalpopM,
            totalpopE_LC = ifelse(
              totalpopE < totalpopM, 0, totalpopE - totalpopM),
            nhwhitepopE = nhwhitepopE,
            nhwhitepopM = nhwhitepopM,
            nhwhitepopE_UC = nhwhitepopE + nhwhitepopM,
            nhwhitepopE_LC = ifelse(
              nhwhitepopE < nhwhitepopM, 0, nhwhitepopE - nhwhitepopM)
            )
# Next acquire estimates for Hispanic and nonWhite groups in long format
B03002_nonwhite <- map_df(ne_states, function(x) {
  get_acs(geography = "block group", variables = c(
    nhblackpop = "B03002_004",
    nhamerindpop = "B03002_005",
    nhasianpop = "B03002_006",
    nhnativhpop = "B03002_007",
    nhotherpop = "B03002_008",
    nh2morepop = "B03002_009",
    hisppop = "B03002_012"),
          state = x)})
# Compute derived sum of minority and MOE, as well UC and LC values
nonwhite_est <- B03002_nonwhite %>% 
  group_by(GEOID) %>% 
  summarize(minorityE = sum(estimate),
            minorityM = moe_sum(moe,estimate)) %>% 
  mutate(minorityE_UC = minorityE + minorityM,
         minorityE_LC = ifelse(
           minorityE < minorityM, 0, minorityE - minorityM))
# Join with total and white pops and compute derived proportions
minority_pct <- B03002_totwhite %>% 
  left_join(., nonwhite_est, by = "GEOID") %>% 
  mutate(minority_pE = ifelse(
    totalpopE == 0, 0, minorityE/totalpopE),
    minority_pM = moe_prop(num = minorityE, 
                           denom = totalpopE, 
                           moe_num = minorityM, 
                           moe_denom = totalpopM),
    minority_pctE = minority_pE*100,
    minority_pctM = minority_pM*100,
    minority_pctE_UC = minority_pctE + minority_pctM,
    minority_pctE_LC = ifelse(
      minority_pctE < minority_pctM, 0, minority_pctE - minority_pctM)) %>% 
  select(-minority_pE,-minority_pM)
# add variables to identify EJ criteria thresholds
minority_pct <- minority_pct %>% 
  mutate(MA_MINORITY = if_else(minority_pctE >= 25, "M", NULL),
         MA_MINORITY_UC = if_else(minority_pctE_UC >= 25, "M", NULL),
         MA_MINORITY_LC = if_else(minority_pctE_LC >= 25, "M", NULL))


### ENGLISH LANGUAGE ISOLATION
# Download C16002. Household Language by Household Limited English Speaking Status. Note that this table is a collapsed version of table B16002. EPA and MA use the latter, but there is no significant difference since we are not interested in disaggregating categories.
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
# Join with total households and calculate derived proportions and MOEs, along with upper and lower confidence interval values from MOE. Rename columns and remove proportion variables. 
eng_limited_pct <- eng_limited %>% 
  filter(variable == "C16002_001") %>% 
  group_by(GEOID) %>% 
  left_join(., eng_limited_est, by = c("GEOID","GEOID")) %>% 
  transmute(NAME = NAME,
            eng_hhE = estimate,
            eng_hhM = moe,
            eng_hh_UC = estimate + moe,
            eng_hh_LC = ifelse(estimate < moe, 0, estimate - moe),
            eng_li_pE = ifelse(estimate==0,0,eng_liE/estimate),
            eng_li_pM = moe_prop(eng_liE,estimate,eng_liM,moe),
            eng_li_pctE = eng_li_pE*100,
            eng_li_pctM = eng_li_pM*100,
            eng_li_pctE_UC = eng_li_pctE + eng_li_pctM,
            eng_li_pctE_LC = ifelse(
              eng_li_pctE < eng_li_pctM, 0, eng_li_pctE - eng_li_pctM)) %>% 
  select(-eng_li_pE,-eng_li_pM)
# add variables to identify EJ criteria thresholds
eng_limited_pct <- eng_limited_pct %>% 
  mutate(MA_ENGLISH = if_else(eng_li_pctE >= 25, "E", NULL),
         MA_ENGLISH_UC = if_else(eng_li_pctE_UC >= 25, "E", NULL),
         MA_ENGLISH_LC = if_else(eng_li_pctE_LC >= 25, "E", NULL))


### EDUCATIONAL ATTAINMENT FOR THOSE AGE 25+
# Less than high school education: The number or percent of people age 25 or older in a block group whose education is short of a high school diploma.
# Download Table B15002 SEX BY EDUCATIONAL ATTAINMENT FOR THE POPULATION 25 YEARS AND OVER
B15002 <- map_df(ne_states, function(x) {
  get_acs(geography = "block group", table = "B15002", state = x)
})
# Isolate universe of population 25+
age25up <- B15002 %>% 
  filter(variable == "B15002_001") %>% 
  transmute(GEOID = GEOID,
            age25upE = estimate,
            age25upM = moe,
            age25upE_UC = age25upE + age25upM,
            age25upE_LC = ifelse(
              age25upE < age25upM, 0, age25upE - age25upM))
# Isolate populations with less than HS diploma
# create vector of patterns for male and female variables less than HS
lths_strings <- rep(c(3:10,20:27)) %>% 
  formatC(width = 3, format = "d", flag = "0") # add leading 0s
# filter cases by patterns, compute derived sum estimates and MOEs
lths_num <- B15002 %>% 
  filter(str_detect(variable,paste(lths_strings,collapse = "|"))) %>% 
  group_by(GEOID) %>% 
  summarize(lthsE = sum(estimate),
            lthsM = moe_sum(moe,estimate)) %>% 
  mutate(lthsE_UC = lthsE + lthsM,
         lthsE_LC = ifelse(
           lthsE < lthsM, 0, lthsE - lthsM))
# Join tables and compute derived proportion and MOE
lths <- age25up %>% 
  left_join(.,lths_num, by = "GEOID") %>% 
  mutate(r_lthsE = ifelse(
    age25upE == 0, 0, lthsE/age25upE),
    r_lthsM = moe_ratio(lthsE,age25upE,lthsM,age25upM),
    pct_lthsE = r_lthsE * 100,
    pct_lthsM = r_lthsM * 100,
    pct_lthsE_UC = pct_lthsE + pct_lthsM,
    pct_lthsE_LC = ifelse(
      pct_lthsE < pct_lthsM, 0, pct_lthsE - pct_lthsM)) %>% 
  select(-starts_with("r_"))



### AGE UNDER 5 AND 65+
# Individuals under age 5: The number or percent of people in a block group under the age of 5.
# Individuals over age 64: The number or percent of people in a block group over the age of 64.
# Download Table B01001 TOTAL POPULATION COUNTS AND AGES
B01001 <- map_df(ne_states, function(x) {
  get_acs(geography = "block group", table = "B01001", state = x)
})
# Isolate universe of population for all sex and ages
allAges <- B01001 %>% 
  filter(variable == "B01001_001") %>% 
  transmute(GEOID = GEOID,
            allAgesE = estimate,
            allAgesM = moe,
            allAgesE_UC = allAgesE + allAgesM,
            allAgesE_LC = ifelse(
              allAgesE < allAgesM, 0, allAgesE - allAgesM))
# Isolate under 5 pop, compute derived sum estimates and MOEs
under5 <- B01001 %>% 
  filter(variable %in% c("B01001_003", "B01001_027")) %>% 
  group_by(GEOID) %>% 
  summarize(under5E = sum(estimate),
            under5M = moe_sum(moe,estimate)) %>% 
  mutate(under5E_UC = under5E + under5M,
         under5E_LC = ifelse(
           under5E < under5M, 0, under5E - under5M))
# Isolate 65+ pop
# create vector of patterns for male and female variables 65+
ovr65_strings <- rep(c(20:25,44:49)) %>% 
  formatC(width = 3, format = "d", flag = "0") # add leading 0s
# filter cases by patterns, compute derived sum estimates and MOEs
over65 <- B01001 %>% 
  filter(str_detect(variable,paste(ovr65_strings,collapse = "|"))) %>% 
  group_by(GEOID) %>% 
  summarize(over65E = sum(estimate),
            over65M = moe_sum(moe,estimate)) %>% 
  mutate(over65E_UC = over65E + over65M,
         over65E_LC = ifelse(
           over65E < over65M, 0, over65E - over65M))
# Join the tables and compute derived proportions with MOEs
age5_65 <- allAges %>% 
  left_join(., under5, by = "GEOID") %>% 
  mutate(r_under5E = ifelse(
    allAgesE == 0, 0, under5E/allAgesE),
    r_under5M = moe_ratio(under5E,allAgesE,under5M,allAgesM),
    pct_under5E = r_under5E * 100,
    pct_under5M = r_under5M * 100,
    pct_under5E_UC = pct_under5E + pct_under5M,
    pct_under5E_LC = ifelse(
      pct_under5E < pct_under5M, 0, pct_under5E - pct_under5M)) %>% 
  left_join(., over65, by = "GEOID") %>% 
  mutate(r_over65E = ifelse(
    allAgesE == 0, 0, over65E/allAgesE),
    r_over65M = moe_ratio(over65E,allAgesE,over65M,allAgesM),
    pct_over65E = r_over65E * 100,
    pct_over65M = r_over65M * 100,
    pct_over65E_UC = pct_over65E + pct_over65M,
    pct_over65E_LC = ifelse(
      pct_over65E < pct_over65M, 0, pct_over65E - pct_over65M)) %>% 
  select(-starts_with("r_"))



# join demographic df to block groups
ne_pop_sf <- ne_pop_sf %>% 
  left_join(neACS17blkgrp_langIsol, by = c("GEOID","GEOID"))
# tmap_mode("view")
# tm_shape(ne_pop_sf) + tm_polygons("pct_eli_limited")
# tmap_mode("plot")
