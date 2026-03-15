#--------------------------------------------------------------------------------------------------------------
# all loaded packages come here
#--------------------------------------------------------------------------------------------------------------
suppressMessages({
  library(tidyverse)
  library(haven)
})

#--------------------------------------------------------------------------------------------------------------
# load raw admin and study units data
#--------------------------------------------------------------------------------------------------------------
lbr_admin <- read_delim("data/in/liberia/06_Raw Administrative Data/LIB_MK4_raw_data_lnp_admin.csv", "\t", escape_double = FALSE, trim_ws = TRUE)
lbr_unit <- read_rds("data/out/lbr-unit-clean.RDS")

#--------------------------------------------------------------------------------------------------------------
# clean raw data
#--------------------------------------------------------------------------------------------------------------
lbr_admin <-
  lbr_admin %>%
  mutate(
    # waves are categorized as baseline and endline following information from the study teams' code. This code can be found at "./mkiv-analysis/liberia/08_Code/LIB_MK4_07_cleaning_code_LNP_admin.do" on lines 45 - 61
    wave = case_when(
      (year == 2017 & month > 7) | (year == 2018 & month == 1) ~ "baseline",
      (year == 2018 & month > 4) ~ "endline",
      TRUE ~ NA_character_),
    n_days_wave = case_when(
      wave == "baseline" ~ as.numeric(as.Date("2017-02-10") - as.Date("2016-08-10")),
      wave == "endline" ~ as.numeric(as.Date("2018-07-23") - as.Date("2018-01-23")),
      TRUE ~ NA_real_)) %>% 
  filter(!is.na(wave)) %>% 
  # we calculate the average number of crimes per day for the study
  select(
    communities = towncode,
    wave,
    # rename variables per MPAP
    aarmedrob_num = armedrob / n_days_wave,
    aburglary_num  = burglary / n_days_wave,
    aaggassault_num  = aggassault / n_days_wave,
    asimpleassault_num  = simpleassault / n_days_wave,
    asexual_num  = sexual / n_days_wave,
    adomestic_phys_num  = domestic_phys / n_days_wave,
    adomestic_verbal_num = domestic_verbal / n_days_wave,
    aland_num  = land / n_days_wave,
    aland_violent_num  = land_violent / n_days_wave,
    amob_num  = mob / n_days_wave,
    ariot_num  = riot / n_days_wave,
    amurder_num  = murder / n_days_wave,
    aother_num = other / n_days_wave
  ) %>% 
  # summarise the data at the level of of the communities at baseline or endline
  group_by(communities, wave) %>%
  summarise_all(mean, na.rm = TRUE) %>%
  ungroup %>% 
  # restructure the data to be at the community level 
  pivot_wider(names_from = wave, values_from = aarmedrob_num:aother_num) %>% 
  # replace all missing crimes with zero crimes. This is documented in github issue number 197
  mutate(across(aarmedrob_num_baseline:aother_num_endline, ~replace_na(., 0))) %>%
  rename_at(vars(ends_with("_endline")), ~str_replace(., "_endline", ""))

#--------------------------------------------------------------------------------------------------------------
# join admin data with unit level sampling/treatment assignment data
#--------------------------------------------------------------------------------------------------------------
lbr_admin <-
  lbr_admin %>%
  left_join(lbr_unit, by = "communities") %>% 
  filter(police_zones != "ZONE 6") # refer to issue number 78 on github (this was removed due to a staffing issue)

# save finalized data
saveRDS(lbr_admin,  file = "data/out/lbr-admin-clean.RDS")
