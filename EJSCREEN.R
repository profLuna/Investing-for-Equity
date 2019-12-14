# Transportation related emissions and burdens 
library(tidyverse)

# Read in EJSCREEN Data
EJSCREEN <- read_csv("EJSCREEN_Full_USPR_2018.csv")
glimpse(EJSCREEN)
names(EJSCREEN)
head(EJSCREEN$ID)
# Join to select air pollution related variables to sf block groups
EJSCREEN.air_sf <- EJSCREEN %>% 
  select(ID,DSLPM,CANCER,RESP,PTRAF,OZONE,PM25) %>% 
  left_join(ne_blkgrp_sf, ., by = c("GEOID" = "ID"))

# Convert air pollution variables to numeric
names(EJSCREEN.air_sf)
EJSCREEN.air_sf <- EJSCREEN.air_sf %>% 
  mutate_at(c("DSLPM","CANCER","RESP","PTRAF","OZONE","PM25"), as.numeric)

# EJSCREEN.air_sf[,9:14] <- sapply(EJSCREEN.air_sf[,9:14],as.numeric)

# Look at relationship of income to diesel
EJSCREEN.air_sf %>% 
  drop_na(medhhincE) %>%  
  ggplot(aes(x = medhhincE, y = DSLPM)) + geom_point() +
  facet_wrap("STATE")

# Look at relationship of income to PM25
EJSCREEN.air_sf %>% 
  drop_na(medhhincE) %>%  
  ggplot(aes(x = medhhincE, y = PM25)) + geom_point() +
  facet_wrap("STATE")

# Look at relationship of income to CANCER
EJSCREEN.air_sf %>% 
  drop_na(medhhincE) %>%  
  ggplot(aes(x = medhhincE, y = CANCER)) + geom_point() +
  facet_wrap("STATE")

# Look at relationship of income to RESP
EJSCREEN.air_sf %>% 
  drop_na(medhhincE) %>%  
  ggplot(aes(x = medhhincE, y = RESP)) + geom_point() +
  facet_wrap("STATE")

# Look at relationship of income to PTRAF
EJSCREEN.air_sf %>% 
  drop_na(medhhincE) %>%  
  ggplot(aes(x = medhhincE, y = PTRAF)) + geom_point() +
  facet_wrap("STATE")

# Look at relationship of income to OZONE
EJSCREEN.air_sf %>% 
  as.data.frame() %>% 
  drop_na(medhhincE) %>%  
  ggplot(aes(x = medhhincE, y = OZONE)) + geom_point() +
  facet_wrap("STATE")

# Compute weighted mean by state
EJSCREEN.air_sf %>% 
  as.data.frame() %>% 
  group_by(STATE) %>% 
  summarize(DieselPM.wAvg = weighted.mean(DSLPM,totalpopE, na.rm = TRUE),
            DieselPM.Avg = mean(DSLPM, na.rm = TRUE))


# Example of demographic df for computing weighted means by column
df <- data.frame(
  White = as.integer(c(seq(from = 10, to = 20, length.out = 10))),
  Black = seq(10:1),
  Pollute = seq(0.1:1,length.out = 10))

lapply(df[ , -ncol(df)], function(x) weighted.mean(df$Pollute, w = df$White))

df %>% 
  gather(key = Group, value = Pop, White:Black) %>% 
  group_by(Group) %>% 
  summarize(weighted.mean(x = Pollute, w = Pop))
