# TCI Analysis of Demographic Data

library(tidycensus)
library(tidyverse)
library(tmap)
library(sf)
options(tigris_use_cache = TRUE)

census_api_key("f2776fbc29cf847505de9308a82c8d65290d16b3")

# Load list of variables to identify tables of interest
v17 <- load_variables(2017, "acs5", cache = TRUE)

# tidycensus df with stat codes
# unique(fips_codes$state)

# create vector of states to download
ne_states <- c("CT","MA","ME","NH","RI","VT")

# use purrr::reduce function in combination with sf::rbind to download block groups for list of states and then bind them to one sf. 
ne_pop_sf <- reduce(
  map(ne_states, function(x) {
    get_acs(geography = "block group", 
            variables = c(totalpop = "B03002_001", 
                          whitepop = "B03002_003"),
            state = x, output = "wide", geometry = TRUE)
    }),
  rbind
)

# Extract the state names to a new column
# provide 1 or multiple + whole words \\b, not [ ] in a string that ends in a comma ^, until the end of the string $. (so basically, provide all whole words after comma) 
ne_pop_sf <- ne_pop_sf %>% 
  mutate(STATE = str_extract(NAME, '\\b[^,]+$'))

# map it out
tm_shape(ne_pop_sf) + tm_polygons(col = "totalpopE")
