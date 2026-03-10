#--------------------------------------------------------------------------------------------------------------
# all loaded packages come here
#--------------------------------------------------------------------------------------------------------------
suppressMessages({
  library(tidyverse)
  library(haven)
})

#--------------------------------------------------------------------------------------------------------------
# construct dataset of study units
#--------------------------------------------------------------------------------------------------------------
# load treatment assignment
lbr_treatment_assignment <- read.csv("data/in/liberia/02_Randomization/LIB_MK4_sample.csv", stringsAsFactors = F)
# load census data for population weights
lbr_census <- read.csv("data/in/liberia/06_Raw Administrative Data/LIB_MK4_2008_census.csv")
# load community sample frame
lbr_community_sample_frame <- read.csv("data/in/liberia/02_Randomization/LIB_MK4_assignment_data.csv", stringsAsFactors = F)

#--------------------------------------------------------------------------------------------------------------
# construct sampling frame work dataset
#--------------------------------------------------------------------------------------------------------------
lbr_unit <- 
  lbr_community_sample_frame %>% 
  rename(police_zones = Police.Zone, towncode = commcode) %>% 
  full_join(lbr_treatment_assignment %>% mutate(sampled = 1L), by = "towncode") %>% 
  replace_na(list(sampled = 0L)) %>% 
  rename(communities = towncode, Z = treatment) %>% 
  # join and merge in population weights
  left_join(lbr_census, by = c("communities" = "towncode")) %>% 
  as_tibble() %>% 
  # clean police zones
  mutate(police_zones = str_replace_all(police_zones, " ", "")) %>% 
  # generate weights
  mutate(
    S_communities_inclusion_prob = case_when(
      Z == 1 & police_zones == "ZONE1" ~ 4 / 9,
      Z == 0 & police_zones == "ZONE1" ~ 5 / 9,
      Z == 1 & police_zones == "ZONE2" ~ 6 / 13,
      Z == 0 & police_zones == "ZONE2" ~ 7 / 13,
      Z == 1 & police_zones == "ZONE3" ~ 8 / 15,
      Z == 0 & police_zones == "ZONE3" ~ 7 / 15,
      Z == 1 & police_zones == "ZONE4" ~ 5 / 10,
      Z == 0 & police_zones == "ZONE4" ~ 5 / 10,
      Z == 1 & police_zones == "ZONE5" ~ 4 / 9,
      Z == 0 & police_zones == "ZONE5" ~ 5 / 9,
      Z == 1 & police_zones == "ZONE10" ~ 4 / 9,
      Z == 0 & police_zones == "ZONE10" ~ 5 / 9,
      Z == 1 & police_zones == "ZONE7" ~ 4 / 7,
      Z == 0 & police_zones == "ZONE7" ~ 3 / 7,
      Z == 1 & police_zones == "ZONE8" ~ 4 / 9,
      Z == 0 & police_zones == "ZONE8" ~ 5 / 9,
      Z == 1 & police_zones == "ZONE9" ~ 6 / 12,
      Z == 0 & police_zones == "ZONE9" ~ 6 / 12,
      TRUE ~ NA_real_),
    S_citizens_inclusion_prob = 20 / 3 * (localitypop / num_blocks))

# save final joined file
saveRDS(lbr_unit,  file = "data/out/lbr-unit-clean.RDS")
