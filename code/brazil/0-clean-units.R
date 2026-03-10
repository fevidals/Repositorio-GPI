library(tidyverse)
library(randomizr)
library(haven)
library(readxl)

# replicate the old R random seed generator
RNGkind(sample.kind = "Rounding")

# start with the master sheets which we treat as the population of units at each stage 
points1 <- read_csv(file = "data/in/brazil/02_Randomization/RdV Campaign Mastersheet - RdV points-16042018.csv") 
points2 <- read_csv(file = "data/in/brazil/02_Randomization/RdV Campaign Mastersheet - RdV points_20042018.csv")
points3 <- read_csv(file = "data/in/brazil/02_Randomization/RdV Campaign Mastersheet - RdV points_27042018.csv")
points4 <- read_csv(file = "data/in/brazil/02_Randomization/RdV Campaign Mastersheet - RdV points-03052018.csv")

# create a single data frame with all of the points by round
points_df <- bind_rows(
  `1` = points1 %>% mutate(munsurvey = as.numeric(recode(munsurvey, `o` = "0"))),
  `2` = points2, 
  `3` = points3 %>% rename(muncode = mun_code), 
  `4` = points4 %>% rename(muncode = mun_code),
  .id = "points_round") %>% 
  rename(treat_original = treatment)

# data frame with the municipalities that were deemed `ready` for randomization at each round
# this means the police commander was ready to participate
ready_municipality_df <- bind_rows(
  `1` = tibble(muncode = c(4204608, 4205407, 4207304, 4208906, 4209102, 4218707, 4202008, 4203204, 4205605, 4214805, 4215695, 4215703, 4216107, 4219705)),
  `2` = tibble(muncode = c(4209409, 4215802, 4211900, 4215000, 4202909, 4213401)),
  `3` = tibble(muncode = c(4202800, 4203006, 4204202)), 
  `4` = tibble(muncode = c(4209300)), 
  .id = "points_round"
) %>% 
  mutate(municipality_available = 1)

# merge points with the ready indicator
points_df <- points_df %>% 
  left_join(ready_municipality_df, by = c("muncode", "points_round")) %>% 
  replace_na(list(municipality_available = 0))

# seeds for each round selected by the Brazil team
# we exactly replicate the randomization below 
# (we verify the replication based on files saved contemporaneously at the bottom of this file)
seeds <- c("16042018", "20042018", "27042018", "03052018")

# assign treatment, which is done round-by-round
points_df$treat <- NA
for(rand_round in 1:4) { 
  
  set.seed(seeds[rand_round])
  
  # pull the population of units that are ready in this round, as determined by the Brazil team
  round_ready <- points_df %>% filter(points_round == rand_round & municipality_available == TRUE) 
  
  # pull the set of municipalities that are included in the round (these two steps were done separately because of the specific order of the municipalities that will replicate the randomization exactly)
  round_municipalities <- ready_municipality_df %>% filter(points_round == rand_round) %>% pull(muncode)
  
  # randomize treatment for this round, which is block-randomized by municipality
  points_treated <- unlist(lapply(round_municipalities, function(x)
    sample(subset(round_ready, muncode == x)$id_rdvpoint, round(sum(round_ready$muncode == x)/2))))
  
  # add the assigned treatment vector into the points data frame
  points_df <- points_df %>% mutate(treat = case_when(
    points_round == rand_round & municipality_available == TRUE & id_rdvpoint %in% points_treated ~ TRUE,
    points_round == rand_round & municipality_available == TRUE & !(id_rdvpoint %in% points_treated) ~ FALSE,
    TRUE ~ treat
  ))
}

# clean data, construct variables for issues in implementation
points_df <- 
  points_df %>% 
  filter(municipality_available == 1) %>% 
  transmute(points_round, id_rdvpoint, lat = address_meeting_lat, lon = address_meeting_long, muncode, municipality, treat_assigned = treat) %>% 
  mutate(dropped_municipalities = if_else(municipality %in% c(
    # following "whole municipalities dropped"
    "Brusque", "Camboriu", "Galvao",
    # "Itajai" not included in the data
    "Laguna", "Ponte Serrada", "Palhoça", "Santiago do Sul", "São Domingos", "Xaxim"), 1, 0),
    problem_municipalities = if_else(municipality %in% c(
      # Caçador municipality only holding 2 events (instead of the suggested 5), hence losing 3 T units
      "Caçador",
      
      # Chapecó municipality held meetings prior to the official launch of FB campaigns (affecting 5 T units)
      "Chapecó",
      
      # 4218707-10 Tubarão – event was canceled
      "Tubarão"), 1, 0)
  ) %>% 
  # Four points were moved. In all cases (points 4209102-9, 4205407-1, 4205407-6 and 4205407-9) this was due to a significant shift in the address of the meeting. To make things clear, we recorded this situation as removing one point and adding an artificial meeting number with the same characteristics (and treatment status) but with different address. This also explains why there are additional points in those municipalities. The only purpose was to keep clarity and to prevent the situation where survey observations, if any were conducted at those places prior to the shifting of the position of the meetings, would be mismatched to the randomisation status. This also explains points 4205407-11 and 4205407-12.
  mutate(replaced_point = id_rdvpoint %in% c("4209102-9", "4205407-1", "4205407-6", "4205407-9"))

# open administrative data to obtain implemented treatment status (there were transcription errors)
bra_admin <- read_dta("data/in/brazil/07_Processed Data/administrative.dta")

bra_admin <- 
  bra_admin %>% 
  select(meeting_id, meeting, rdvdate, treat, shareardv_201810, shareardv_201806) %>%
  distinct %>% 
  rename_at(vars(everything(), -shareardv_201810, -shareardv_201806, -meeting_id), ~paste0(., "_admin")) %>% 
  # two points are included in admin data that are replacement points for two of the four replaced points
  # (only two were assigned to treatment, the two controls did not receive replacement points since the police did were not assigned to hold meetings)
  mutate(replacement_point = meeting_id %in% c("4205407-11", "4205407-12")) 

survey_points <- read_excel("data/in/brazil/02_Randomization/survey_points-v2.xlsx", sheet = "full_list") 

survey_points <- survey_points %>%
  transmute(meeting_id = `ID event`, survey_pt_selected = `Survey Point`, survey_mun_selected = `Survey Municipality`) %>% 
  mutate(
    survey_pt_selected = case_when(
      is.na(survey_pt_selected) & survey_mun_selected == 0 ~ 0,
      is.na(survey_pt_selected) & survey_mun_selected == 1 ~ 1,
      !is.na(survey_pt_selected) ~ survey_pt_selected
    )
  )

# create master file
bra_units <- 
  points_df %>%
  
  # join in admin data
  rename(meeting_id = id_rdvpoint) %>%
  full_join(bra_admin, by = c("meeting_id")) %>%
  
  # if point is not in admin data, a meeting was not held
  replace_na(list(meeting_admin = 0)) %>% 
  
  # a second round of randomization not part of the study was conducted in Florianopolis 
  # we note these here and later exclude from analyses
  mutate(municipality = replace(municipality, meeting_id %in% c('4205407-11', '4205407-12', '4205407-21', '4205407-22', '4205407-23', '4205407-24', '4205407-25', '4205407-26', '4205407-41', '4205407-42', '4205407-43', '4205407-44', '4205407-45', '4205407-46', '4205407-47', '4205407-48'), "Florianopolis (x2)")) %>% 
  mutate(Z = if_else(municipality == "Florianopolis (x2)", treat_admin, as.numeric(treat_assigned))) %>% 
  
  # join in surveyed points data
  left_join(survey_points, by = "meeting_id") %>% 
  
  # if point did not have a meeting held and is missing from the survey sampled dataset, it was not sampled for surveys (exclude Florianopolis x2)
  mutate(survey_pt_selected2 = if_else(is.na(survey_pt_selected) & meeting_admin == 0 & municipality != "Florianopolis (x2)", 0, survey_pt_selected)) %>% 
  
  # create treatment received measures
  mutate(
    groupformed_area_201806 = shareardv_201806,
    groupformed_area_201810 = shareardv_201810,
    groupformed_presence_201806 = case_when(
      groupformed_area_201806 > 0 ~ 1L, 
      groupformed_area_201806 == 0 ~ 0L, 
      is.na(groupformed_area_201806) ~ NA_integer_),
    groupformed_presence_201810 = case_when(
      groupformed_area_201810 > 0 ~ 1L, 
      groupformed_area_201810 == 0 ~ 0L, 
      is.na(groupformed_area_201810) ~ NA_integer_)
  )

#--------------------------------------------------------------------------------------------------------------
# 4. save clean data
#--------------------------------------------------------------------------------------------------------------
saveRDS(bra_units,  file = "data/out/bra-units-clean.RDS")

