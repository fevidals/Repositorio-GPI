#--------------------------------------------------------------------------------------------------------------
# all loaded packages come here (resolve warnings in the file)
#--------------------------------------------------------------------------------------------------------------
suppressMessages({
  library(tidyverse)
  library(haven)
})

#--------------------------------------------------------------------------------------------------------------
# read clean data
#--------------------------------------------------------------------------------------------------------------
bra_admin <- read_dta("data/in/brazil/07_Processed Data/administrative.dta")

bra_units <- readRDS("data/out/bra-units-clean.RDS")

bra_admin <- 
  bra_admin %>% 
  select(
    meeting_id,
    timevec,
    
    # tips collected as just atip
    # atips_hline, 
    # atips_box,
    atips = atip,
    
    aarmedrob_num = aarmedrob, 
    # aaggassault_num, 
    # asimpleassault_num, 
    # simple and aggravted assaulted are dissolved into one variable: assault
    aassault,
    asexual_num = asexual, 
    adomestic_phys_num = adomestic, 
    amurder_num = amurder, 
    aother_num_violent = aother_crime_violent,
    aburglary_num = aburglary, 
    aother_num_nonviolent = aother_crime_nonviolent
    
    # aviolent_hline, 
    # anonviolent_hline,
    # aviolent_station, 
    # anonviolent_station
    # these variables were not collected
    ) %>% 
  
  # collapse the data at the level of baseline and endline
  # using until June 2018 as the time period for baseline and the rest to be endline (confirmed from Pedro Souza on email Aug 6, 2020)
  mutate(
    month = case_when(
      timevec >= 1 & timevec <= 12 ~ as.integer(timevec), 
      timevec > 12 & timevec <= 24 ~ as.integer(timevec) - 12L,
      timevec > 24 ~ as.integer(timevec) - 24L,
      TRUE ~ NA_integer_),
    year = case_when(
      timevec >= 1 & timevec <= 12 ~ 2017, 
      timevec > 12 & timevec <= 24 ~ 2018,
      timevec > 24 ~ 2019),
    wave = case_when(
      timevec >= 1 & timevec < 18 ~ "baseline",
      timevec >= 18 ~ "endline",
      TRUE ~ NA_character_),
    n_days_wave = case_when(
      wave == "baseline" ~ 18 * 30,
      wave == "endline" ~ 9 * 30,
      TRUE ~ NA_real_)) %>% 
  
  group_by(meeting_id, wave) %>% 
  summarize(
    # calculating the average number of crime per month
    aarmedrob_num = sum(aarmedrob_num, na.rm = TRUE) / first(n_days_wave),
    aassault_num = sum(aassault, na.rm = TRUE) / first(n_days_wave),
    asexual_num = sum(asexual_num, na.rm = TRUE) / first(n_days_wave),
    amurder_num = sum(amurder_num, na.rm = TRUE) / first(n_days_wave),
    aother_num_violent = sum(aother_num_violent, na.rm = TRUE) / first(n_days_wave),
    aburglary_num = sum(aburglary_num, na.rm = TRUE) / first(n_days_wave),
    aother_num_nonviolent = sum(aother_num_nonviolent, na.rm = TRUE) / first(n_days_wave), 
    atips = sum(atips, na.rm = TRUE) / first(n_days_wave)) %>% 
  ungroup() %>% 
  pivot_wider(id_cols = meeting_id, names_from = wave, values_from = aarmedrob_num:aother_num_nonviolent) %>% 
  rename_at(vars(ends_with("_endline")), ~str_replace(., "_endline", "")) %>% 
  left_join(bra_units, by = c("meeting_id"))
  
saveRDS(bra_admin,  file = "data/out/bra-admin-clean.RDS")


