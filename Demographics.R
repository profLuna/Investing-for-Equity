# TCI Analysis of Demographic Data

library(tidycensus)
library(tidyverse)
library(tmap)
library(sf)
library(tigris)
options(tigris_use_cache = TRUE, tigris_class = "sf")

census_api_key("f2776fbc29cf847505de9308a82c8d65290d16b3", install = TRUE)

# Load list of variables to identify tables of interest
v18 <- load_variables(2018, "acs5", cache = TRUE)

# tidycensus df with stat codes
# unique(fips_codes$state)

### CENSUS POLYGONS

# create vector of New England states
ne_states <- c("CT","MA","ME","NH","RI","VT")

# use purrr::reduce function in combination with sf::rbind to download block groups for list of states and then bind them to one sf. 
ne_blkgrp_sf <- reduce(
  map(ne_states, function(x) {
    get_acs(geography = "block group", 
            variables = c(totalpop = "B03002_001", 
                          medhhinc = "B19013_001",
                          households = "B19001_001"),
            state = x, output = "wide", geometry = TRUE)
    }),
  rbind
)

# Extract the state names to a new column
# provide 1 or multiple + whole words \\b, not [ ] in a string that ends in a comma ^, until the end of the string $. (so basically, provide all whole words after last comma) 
ne_blkgrp_sf <- ne_blkgrp_sf %>% 
  mutate(STATE = str_extract(NAME, '\\b[^,]+$'))

# use purrr::reduce function in combination with sf::rbind to download tracts for list of states and then bind them to one sf. 
ne_tracts_sf <- reduce(
  map(ne_states, function(x) {
    get_acs(geography = "tract", 
            variables = c(totalpop = "B03002_001", 
                          medhhinc = "B19013_001"),
            state = x, output = "wide", geometry = TRUE)
  }),
  rbind
)

# Extract the state names to a new column
# provide 1 or multiple + whole words \\b, not [ ] in a string that ends in a comma ^, until the end of the string $. (so basically, provide all whole words after comma) 
ne_tracts_sf <- ne_tracts_sf %>% 
  mutate(STATE = str_extract(NAME, '\\b[^,]+$'))

# Do the same for towns across New England, although note that tidycensus does not support that geography yet so we need to import shapefiles separately with tigris and then join
# use purrr::map_df to download df and bind vector of states
ne_towns_df <- map_df(ne_states, function(x) {
    get_acs(geography = "county subdivision", 
            variables = c(totalpop = "B03002_001",
                          medhhinc = "B19013_001"),
            state = x, output = "wide")
  }) %>% 
  mutate(totalpopE_UC = totalpopE + totalpopM,
         totalpopE_LC = ifelse(
           totalpopE < totalpopM, 0, totalpopE - totalpopM),
         medhhincE_UC = medhhincE + medhhincM,
         medhhincE_LC = ifelse(
           medhhincE < medhhincM, 0, medhhincE - medhhincM),
         STATE = str_extract(NAME, '\\b[^,]+$'))

# Calculate statewide average median household income
ne_states_df <- map_df(ne_states, function(x) {
  get_acs(geography = "state", 
          variables = c(totalpop = "B03002_001",
                        medhhinc = "B19013_001"),
          state = x, output = "wide")
})

# Isolate VT statewide median household income
VT_MED_HHINC <- ne_states_df %>% 
  filter(NAME == "Vermont") %>% 
  dplyr::select(medhhincE) %>% 
  pull()

# Isolate MA statewide median hh income
MA_MED_HHINC <- ne_states_df %>% 
  filter(NAME == "Massachusetts") %>% 
  dplyr::select(medhhincE) %>% 
  pull()

# add variables to identify EJ criteria thresholds
ne_towns_df <- ne_towns_df %>% 
  mutate(VT_INCOME = if_else(medhhincE < VT_MED_HHINC, "I", NULL),
         VT_INCOME_UC = if_else(medhhincE_UC < VT_MED_HHINC, "I", NULL),
         VT_INCOME_LC = if_else(medhhincE_LC < VT_MED_HHINC, "I", NULL),
         MA_INC_BELOW150 = if_else(medhhincE <= 1.5*MA_MED_HHINC,"Y","N"),
         MA_INC_BELOW150_UC = if_else(medhhincE_UC <= 1.5*MA_MED_HHINC,"Y","N"),
         MA_INC_BELOW150_LC = if_else(medhhincE_LC <= 1.5*MA_MED_HHINC,"Y","N"))

# acquire the polygons for county subdivisions for ne_towns using tigris::rbind_tigris
ne_towns_sf <- rbind_tigris(
  lapply(
    ne_states, function(x){
      county_subdivisions(state = x, cb = TRUE, year = 2019)
    }
  )
)

# join the demographics to the polygons
ne_towns_sf <- ne_towns_sf %>% 
  select(-NAME) %>% 
  left_join(., ne_towns_df, by = "GEOID")

# clean up
rm(ne_towns_df, VT_MED_HHINC)


###### DEMOGRAPHIC DATA FRAMES ##############

### HOUSEHOLDS BY INCOME
# # this is for 2017 MA EJ Policy
# # download table of counts of household income categories, sum up households in categories below 65% of MA statewide median
# B19001 <- map_df(ne_states, function(x) {
#   get_acs(geography = "block group", table = "B19001", state = x)})
# # Isolate estimate of total households
# medhhinc_total <- B19001 %>% 
#   filter(variable == "B19001_001") %>% 
#   transmute(GEOID = GEOID,
#             householdsE = estimate,
#             householdsM = moe)
# # Isolate household counts less than 65% of MA statewide median of $74,167, which is $48,208. Closest range is 45 - 49,9.
# # create vector of patterns for medhhinc levels below 50k
# med_strings <- rep(c(2:10)) %>% 
#   formatC(width = 3, format = "d", flag = "0") # add leading 0s
# # filter cases by patterns, compute derived sum estimates and MOEs
# medhhinclt50 <- B19001 %>% 
#   filter(str_detect(variable,paste(med_strings,collapse = "|"))) %>% 
#   group_by(GEOID) %>% 
#   summarize(medhhinclt50E = sum(estimate),
#             medhhinclt50M = moe_sum(moe, estimate)) %>% 
#   mutate(medhhinclt50_UC = medhhinclt50E + medhhinclt50M,
#          medhhinclt50_LC = ifelse(
#            medhhinclt50E < medhhinclt50M, 0, medhhinclt50E - medhhinclt50M))
# # Join total households and compute derived proportions
# medhhinclt50_pct <- medhhinclt50 %>% 
#   left_join(., medhhinc_total, by = "GEOID") %>% 
#   mutate(r2medhhincE = ifelse(householdsE <= 0, 0, medhhinclt50E/householdsE),
#     r2medhhincM = moe_prop(medhhinclt50E,householdsE,medhhinclt50M,householdsM),
#     pct_medhhinclt50E = r2medhhincE*100,
#     pct_medhhinclt50M = r2medhhincM*100,
#     pct_medhhinclt50E_UC = pct_medhhinclt50E + pct_medhhinclt50M,
#     pct_medhhinclt50E_LC = ifelse(
#       pct_medhhinclt50E < pct_medhhinclt50M, 0, 
#       pct_medhhinclt50E - pct_medhhinclt50M)) %>% 
#   select(-starts_with("r2m"))
# # add variables to identify EJ criteria thresholds
# medhhinclt50_pct <- medhhinclt50_pct %>% 
#   mutate(MA_INCOME = if_else(pct_medhhinclt50E >= 25, "I", NULL),
#          MA_INCOME_UC = if_else(pct_medhhinclt50E_UC >= 25, "I", NULL),
#          MA_INCOME_LC = if_else(pct_medhhinclt50E_LC >= 25, "I", NULL))
# # clean up
# rm(B19001,med_strings,medhhinc_total,medhhinclt50)


# join town data with MA income threshold to block groups in order to allow for definition of minority threshold for 2020 MA EJ POLICY
ne_blkgrp_sf <- ne_towns_sf %>% 
  transmute(TOWN = NAME, MA_INC_BELOW150 = MA_INC_BELOW150, 
            MA_INC_BELOW150_UC = MA_INC_BELOW150_UC,
            MA_INC_BELOW150_LC = MA_INC_BELOW150_LC) %>% 
  st_join(ne_blkgrp_sf, ., largest = TRUE)
# add variables to identify EJ criteria thresholds
ne_blkgrp_sf <- ne_blkgrp_sf %>%
  mutate(medhhincE_UC = medhhincE + medhhincM,
         medhhincE_LC = medhhincE - medhhincM,
         MA_INCOME = if_else(medhhincE <= .65*MA_MED_HHINC, "I", NULL),
         MA_INCOME_UC = if_else(medhhincE_UC <= .65*MA_MED_HHINC, "I", NULL),
         MA_INCOME_LC = if_else(medhhincE_LC <= .65*MA_MED_HHINC, "I", NULL))

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
poverty_pct <- povknown %>% 
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
poverty_pct <- poverty_pct %>% 
  mutate(pov_pctile = percent_rank(pct2povE),
         pov_pctile_UC = percent_rank(pct2povE_UC),
         pov_pctile_LC = percent_rank(pct2povE_LC),
         RI_INCOME = if_else(pov_pctile >= 0.85, "I", NULL),
         RI_INCOME_UC = if_else(pov_pctile_UC >= 0.85, "I", NULL),
         RI_INCOME_LC = if_else(pov_pctile_LC >= 0.85, "I", NULL),
         CT_INCOME = if_else(pct2povE >= 30, "I", NULL),
         CT_INCOME_UC = if_else(pct2povE_UC >= 30, "I", NULL),
         CT_INCOME_LC = if_else(pct2povE_LC >= 30, "I", NULL))
# clean up
rm(C17002,num2pov,povknown)


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
# clean up
rm(nonwhite_est)
# add variables to identify EJ criteria thresholds
minority_pct <- minority_pct %>% 
  mutate(minority_pctile = percent_rank(minority_pctE),
         minority_pctile_UC = percent_rank(minority_pctE_UC),
         minority_pctile_LC = percent_rank(minority_pctE_LC),
         # MA_MINORITY = if_else(minority_pctE >= 25, "M", NULL),
         # MA_MINORITY_UC = if_else(minority_pctE_UC >= 25, "M", NULL),
         # MA_MINORITY_LC = if_else(minority_pctE_LC >= 25, "M", NULL),
         RI_MINORITY = if_else(minority_pctile >= 0.85, "M", NULL),
         RI_MINORITY_UC = if_else(minority_pctile_UC >= 0.85, "M", NULL),
         RI_MINORITY_LC = if_else(minority_pctile_LC >= 0.85, "M", NULL))
# join non-white group estimates
# first download nonwhite estimates in wide format for easier joining
B03002_nonwhite_wide <- map_df(ne_states, function(x) {
  get_acs(geography = "block group", variables = c(
    nhblackpop = "B03002_004",
    nhamerindpop = "B03002_005",
    nhasianpop = "B03002_006",
    nhnativhpop = "B03002_007",
    nhotherpop = "B03002_008",
    nh2morepop = "B03002_009",
    hisppop = "B03002_012"),
    state = x, output = "wide")})
# join to minority_pct
minority_pct <- B03002_nonwhite_wide %>% 
  select(-NAME) %>% 
  left_join(minority_pct, ., by = "GEOID")
# clean up
rm(list = ls(pattern = "B03002"))

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
  summarize(eng_limitE = sum(estimate),
            eng_limitM = moe_sum(moe,estimate))
# Join with total households and calculate derived proportions and MOEs, along with upper and lower confidence interval values from MOE. Rename columns and remove proportion variables. 
eng_limited_pct <- eng_limited %>% 
  filter(variable == "C16002_001") %>% 
  group_by(GEOID) %>% 
  left_join(., eng_limited_est, by = "GEOID") %>% 
  transmute(eng_hhE = estimate,
            eng_hhM = moe,
            eng_hh_UC = estimate + moe,
            eng_hh_LC = ifelse(estimate < moe, 0, estimate - moe),
            eng_limitE = eng_limitE,
            eng_limitM = eng_limitM,
            eng_li_pE = ifelse(estimate==0,0,eng_limitE/estimate),
            eng_li_pM = moe_prop(eng_limitE,estimate,eng_limitM,moe),
            eng_limit_pctE = eng_li_pE*100,
            eng_limit_pctM = eng_li_pM*100,
            eng_limit_pctE_UC = eng_limit_pctE + eng_limit_pctM,
            eng_limit_pctE_LC = ifelse(
              eng_limit_pctE < eng_limit_pctM, 0, 
              eng_limit_pctE - eng_limit_pctM)) %>% 
  select(-eng_li_pE,-eng_li_pM)
# add variables to identify EJ criteria thresholds
eng_limited_pct <- eng_limited_pct %>% 
  mutate(MA_ENGLISH = if_else(eng_limit_pctE >= 25, "E", NULL),
         MA_ENGLISH_UC = if_else(eng_limit_pctE_UC >= 25, "E", NULL),
         MA_ENGLISH_LC = if_else(eng_limit_pctE_LC >= 25, "E", NULL))
# clean up
rm(eng_limited,eng_limited_est)

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
# Isolate populations with college degree or higher
# create vector of patterns for male and female variables with college+
col_strings <- rep(c(15:18,32:35)) %>% 
  formatC(width = 3, format = "d", flag = "0") # add leading 0s
# filter cases by patterns, compute derived sum estimates and MOEs
col_num <- B15002 %>% 
  filter(str_detect(variable,paste(col_strings,collapse = "|"))) %>% 
  group_by(GEOID) %>% 
  summarize(collegeE = sum(estimate),
            collegeM = moe_sum(moe,estimate)) %>% 
  mutate(collegeE_UC = collegeE + collegeM,
         collegeE_LC = ifelse(
           collegeE < collegeM, 0, collegeE - collegeM))
# Join tables and compute derived proportion and MOE
lths_pct <- age25up %>% 
  left_join(.,lths_num, by = "GEOID") %>% 
  mutate(r_lthsE = ifelse(
    age25upE == 0, 0, lthsE/age25upE),
    r_lthsM = moe_ratio(lthsE,age25upE,lthsM,age25upM),
    pct_lthsE = r_lthsE * 100,
    pct_lthsM = r_lthsM * 100,
    pct_lthsE_UC = pct_lthsE + pct_lthsM,
    pct_lthsE_LC = ifelse(
      pct_lthsE < pct_lthsM, 0, pct_lthsE - pct_lthsM)) %>% 
  left_join(.,col_num, by = "GEOID") %>% 
  mutate(r_collegeE = ifelse(
    age25upE == 0, 0, collegeE/age25upE),
    r_collegeM = moe_ratio(collegeE,age25upE,collegeM,age25upM),
    pct_collegeE = r_collegeE * 100,
    pct_collegeM = r_collegeM * 100,
    pct_collegeE_UC = pct_collegeE + pct_collegeM,
    pct_collegeE_LC = ifelse(
      pct_collegeE < pct_collegeM, 0, pct_collegeE - pct_collegeM)) %>% 
  select(-starts_with("r_"))
# clean up
rm(age25up,B15002,lths_num,lths_strings,col_num,col_strings)


### AGE UNDER 5, UNDER 18, AND 64+
# Individuals under age 5: The number or percent of people in a block group under the age of 5.
# Individuals under age 18: The number or percent of people in a block group under the age of 18. NOTE THAT CENSUS DATA RANGE IS 15 - 17. 
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
# Isolate under 18 pop, compute derived sum estimates and MOEs
under18 <- B01001 %>% 
  filter(variable %in% 
           c("B01001_003", "B01001_004", "B01001_005", "B01001_006",
             "B01001_027", "B01001_028", "B01001_029", "B01001_030")) %>% 
  group_by(GEOID) %>% 
  summarize(under18E = sum(estimate),
            under18M = moe_sum(moe,estimate)) %>% 
  mutate(under18E_UC = under18E + under18M,
         under18E_LC = ifelse(
           under18E < under18M, 0, under18E - under18M))
# Isolate over 64 pop
# create vector of patterns for male and female variables 65+
ovr64_strings <- rep(c(20:25,44:49)) %>% 
  formatC(width = 3, format = "d", flag = "0") # add leading 0s
# filter cases by patterns, compute derived sum estimates and MOEs
over64 <- B01001 %>% 
  filter(str_detect(variable,paste(ovr64_strings,collapse = "|"))) %>% 
  group_by(GEOID) %>% 
  summarize(over64E = sum(estimate),
            over64M = moe_sum(moe,estimate)) %>% 
  mutate(over64E_UC = over64E + over64M,
         over64E_LC = ifelse(
           over64E < over64M, 0, over64E - over64M))
# Join the tables and compute derived proportions with MOEs
age5_18_64_pct <- allAges %>% 
  left_join(., under5, by = "GEOID") %>% 
  mutate(r_under5E = ifelse(
    allAgesE == 0, 0, under5E/allAgesE),
    r_under5M = moe_ratio(under5E,allAgesE,under5M,allAgesM),
    pct_under5E = r_under5E * 100,
    pct_under5M = r_under5M * 100,
    pct_under5E_UC = pct_under5E + pct_under5M,
    pct_under5E_LC = ifelse(
      pct_under5E < pct_under5M, 0, pct_under5E - pct_under5M)) %>% 
  left_join(.,under18, by = "GEOID") %>% 
  mutate(r_under18E = ifelse(
    allAgesE == 0, 0, under18E/allAgesE),
    r_under18M = moe_ratio(under18E,allAgesE,under18M,allAgesM),
    pct_under18E = r_under18E * 100,
    pct_under18M = r_under18M * 100,
    pct_under18E_UC = pct_under18E + pct_under18M,
    pct_under18E_LC = ifelse(
      pct_under18E < pct_under18M, 0, pct_under18E - pct_under18M)) %>% 
  left_join(., over64, by = "GEOID") %>% 
  mutate(r_over64E = ifelse(
    allAgesE == 0, 0, over64E/allAgesE),
    r_over64M = moe_ratio(over64E,allAgesE,over64M,allAgesM),
    pct_over64E = r_over64E * 100,
    pct_over64M = r_over64M * 100,
    pct_over64E_UC = pct_over64E + pct_over64M,
    pct_over64E_LC = ifelse(
      pct_over64E < pct_over64M, 0, pct_over64E - pct_over64M)) %>% 
  select(-starts_with("r_"))
# clean up
rm(allAges,B01001,over64,under18,under5,ovr64_strings)


### DISABILITY
# NOTE THAT THIS DATA IS ONLY AVAILABLE AT TRACT LEVEL, NOT BLKGRP
# Number and percent of individuals 18+ with a disability
# Download Table B18101 SEX BY AGE BY DISABILITY STATUS
B18101 <- map_df(ne_states, function(x) {
  get_acs(geography = "tract", table = "B18101", state = x)
})
# Isolate disabled and non-disabled population 18+
# create vector of patterns for male and female variables 18+
ovr18_strings <- rep(c(9:20,28:39)) %>% 
  formatC(width = 3, format = "d", flag = "0") # add leading 0s
# filter cases by patterns, compute derived sum estimates and MOEs
over18 <- B18101 %>% 
  filter(str_detect(variable,paste(ovr18_strings,collapse = "|"))) %>% 
  group_by(GEOID) %>% 
  summarize(Over18E = sum(estimate),
            Over18M = moe_sum(moe,estimate)) %>% 
  mutate(Over18E_UC = Over18E + Over18M,
         Over18E_LC = ifelse(
           Over18E < Over18M, 0, 
           Over18E - Over18M))
# compute derived sum and moe for those over 18 with a disability only
# create vector of patterns for male and female variables 18+ with disability
disabledOvr18_strings <- sort(c(seq(from = 10, to = 19, by = 3), 
                              seq(from = 29, to = 38, by = 3))) %>% 
  formatC(width = 3, format = "d", flag = "0") # add leading 0s
# filter cases by patterns, compute derived sum estimates and MOEs
disabledOver18 <- B18101 %>%
  filter(str_detect(variable,paste(disabledOvr18_strings,collapse = "|"))) %>% 
  group_by(GEOID) %>% 
  summarize(disabledOver18E = sum(estimate),
            disabledOver18M = moe_sum(moe,estimate)) %>% 
  mutate(disabledOver18E_UC = disabledOver18E + disabledOver18M,
         disabledOver18E_LC = ifelse(
           disabledOver18E < disabledOver18M, 0, 
           disabledOver18E - disabledOver18M))
# Join the tables and compute derived proportions with MOEs
disabilityOver18_pct <- over18 %>% 
  left_join(., disabledOver18, by = "GEOID") %>% 
  mutate(r_disabilityOver18E = ifelse(
    Over18E == 0, 0, disabledOver18E/Over18E),
    r_disabilityOver18M = moe_ratio(
      disabledOver18E,Over18E,disabledOver18M,Over18M),
    pct_disabilityOver18E = r_disabilityOver18E * 100,
    pct_disabilityOver18M = r_disabilityOver18M * 100,
    pct_disabilityOver18E_UC = pct_disabilityOver18E + pct_disabilityOver18M,
    pct_disabilityOver18E_LC = ifelse(
      pct_disabilityOver18E < pct_disabilityOver18M, 0, 
      pct_disabilityOver18E - pct_disabilityOver18M))%>% 
  select(-starts_with("r_"))
# clean up
rm(B18101,disabledOver18,disabledOvr18_strings,over18,ovr18_strings)


### HOUSEHOLDS WITHOUT ACCESS TO CAR
# NOTE THAT THIS DATA IS ONLY AVAILABLE AT TRACT LEVEL, NOT BLKGRP
# Number and percent of households without access to a car
# Download Variables from Table B08201 HOUSEHOLD SIZE BY VEHICLES AVAILABLE
noCarHH_pct <- map_df(ne_states, function(x) {
  get_acs(geography = "tract", variables = c(totalHH = "B08201_001",
                                             HHnoCar = "B08201_002"),
          state = x, output = "wide")
}) %>% # compute upper and lower confidences, and derived ratios and moe
  mutate(HHnoCarE_UC = HHnoCarE + HHnoCarM,
         HHnoCarE_LC = ifelse(
           HHnoCarE < HHnoCarM, 0, HHnoCarE - HHnoCarM),
         r_HHnoCarE = ifelse(
    totalHHE == 0, 0, HHnoCarE/totalHHE),
    r_HHnoCarM = moe_prop(HHnoCarE,totalHHE,HHnoCarM,totalHHM),
    pct_HHnoCarE = r_HHnoCarE * 100,
    pct_HHnoCarM = r_HHnoCarM * 100,
    pct_HHnoCarE_UC = pct_HHnoCarE + pct_HHnoCarM,
    pct_HHnoCarE_LC = ifelse(
      pct_HHnoCarE < pct_HHnoCarM, 0, pct_HHnoCarE - pct_HHnoCarM)) %>% 
  select(-starts_with("r_"),-NAME)


######### JOIN DATA FRAMES TO POLYGONS #############

# join demographic df to block groups
ne_blkgrp_sf <- ne_blkgrp_sf %>% 
  select(-starts_with("total")) %>% 
  left_join(., minority_pct, by = "GEOID") %>% 
  left_join(., age5_18_64_pct, by = "GEOID") %>% 
  left_join(., eng_limited_pct, by = "GEOID") %>% 
  left_join(., poverty_pct, by = "GEOID") %>% 
  left_join(., lths_pct, by = "GEOID") %>% 
  # left_join(., medhhinclt50_pct, by = "GEOID") %>% 
  mutate(MA_MINORITY = 
           if_else(minority_pctE >= 40 | (minority_pctE >= 25 & 
                                          MA_INC_BELOW150 == "Y"), "M", NULL),
         MA_MINORITY_UC = 
           if_else(minority_pctE_UC >= 40 | (minority_pctE_UC >= 25 & 
                                            MA_INC_BELOW150_UC == "Y"), 
                   "M", NULL),
         MA_MINORITY_LC = 
           if_else(minority_pctE_LC >= 40 | (minority_pctE_LC >= 25 & 
                                               MA_INC_BELOW150_LC == "Y"), 
                   "M", NULL))

# join demographic df to tracts
ne_tracts_sf <- ne_tracts_sf %>% 
  left_join(., disabilityOver18_pct, by = "GEOID") %>% 
  left_join(., noCarHH_pct, by = "GEOID")

# clean up
rm(list = ls(pattern = "_pct"))

# save original and joined spatial files with demographics
save(ne_blkgrp_sf,
     ne_tracts_sf,
     ne_towns_sf, 
     ne_states,
     file = "DATA/ne_layers.rds")
