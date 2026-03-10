#--------------------------------------------------------------------------------------------------------------
# R packages and code
#--------------------------------------------------------------------------------------------------------------
suppressMessages({
  library(tidyverse)
  library(haven)
})

#--------------------------------------------------------------------------------------------------------------
# get raw data
#--------------------------------------------------------------------------------------------------------------
pak_randomization <- read_dta("data/in/pakistan/02_Randomization/PK_MK4_actual_assignment.dta")
pak_sampling_frame <-  read_dta("data/in/pakistan/02_Randomization/PK_MK4_sampling_frame.dta")
pak_population <- read_dta("data/in/pakistan/07_Processed Data/a_First Stage Processing/Survey Data/PK_MK4_Beat_Pop.dta")

#--------------------------------------------------------------------------------------------------------------
# create weights (citizen)
#--------------------------------------------------------------------------------------------------------------
pak_weights <- 
  pak_randomization %>% 
  select(block_ID, Cluster_ID, type_sample) %>% 
  mutate(sampled = TRUE) %>% 
  right_join(pak_sampling_frame, by = c("block_ID", "Cluster_ID")) %>% 
  arrange(block_ID, Cluster_ID) %>% 
  group_by(block_ID) %>% 
  mutate(
    ps_beat_count_overall = n(),
    ps_S1beat_count = na_if(sum(type_sample == 1, na.rm = TRUE), 0L),
    ps_S2beat_count = na_if(sum(type_sample == 2, na.rm = TRUE), 0L),
  ) %>% 
  ungroup %>% 
  mutate(
    prob_S1 = ps_S1beat_count / ps_beat_count_overall,
    prob_S2 = if_else(type_sample == 2, 27/75, NA_real_),
    Z_multistage_assignment_prob = case_when(
      type_sample == 1 ~ prob_S1, 
      type_sample == 2 ~ (1 - prob_S1) * prob_S2,
      TRUE ~ NA_real_
    )
  ) %>% 
  filter(sampled == TRUE) %>% 
  inner_join(pak_population, by = c("block_name", "cluster_name")) %>% 
  mutate(S_multistage_inclusion_survey = (32 / (Population / 7))) %>% 
  rename(population = Population)

#--------------------------------------------------------------------------------------------------------------
# randomization code: keep, store, merge
#--------------------------------------------------------------------------------------------------------------
# clean sampling frame

pak_sampling_frame <- 
  pak_sampling_frame %>% 
  rename(beat = cluster_name, policestation = block_name)

# clean randomization files
pak_unit <- 
  pak_randomization %>% 
  transmute(
    Cluster_ID,
    block_ID,
    Z_alt,
    Z_common,
    Z_control, 
    in_sample = 1L,
    Z = case_when(
      Z_common == 0 & Z_alt == 0 ~ 0L,
      Z_common == 1 & Z_alt == 0 ~ 1L,
      Z_common == 0 & Z_alt == 1 ~ 2L)) %>% 
  inner_join(pak_weights, by = c("block_ID", "Cluster_ID")) %>% 
  full_join(pak_sampling_frame %>% select(-geo_unit_ID), by = c("block_ID", "Cluster_ID")) %>% 
  mutate(in_sampling = if_else(is.na(Z_common), 0, 1)) %>% 
  select(geo_unit_ID, population, cluster_id = Cluster_ID, block_ID, Z_common, Z_alt, Z_control, Z, stations = policestation, beats = beat, Z_multistage_assignment_prob, S_multistage_inclusion_survey, in_sample)

#--------------------------------------------------------------------------------------------------------------
# randomziation code: keep, store, merge
#--------------------------------------------------------------------------------------------------------------
saveRDS(pak_unit,  file = "data/out/pak-unit-clean.RDS")
