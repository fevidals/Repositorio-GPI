#--------------------------------------------------------------------------------------------------------------
# all loaded packages come here (resolve warnings in the file)
#--------------------------------------------------------------------------------------------------------------
suppressMessages({
  library(tidyverse)
  library(readxl)
  library(haven)
})

#----------------------------------------------------------------------------------------------
# 1. get all raw data
#----------------------------------------------------------------------------------------------
blotter_files <- list.files("data/in/philippines/06_Raw Administrative Data/pnp_blotter")
pnp_blotter_ids <- read_dta("data/in/philippines/06_Raw Administrative Data/PNP Blotter LGUs matched.dta")
phl_units <- readRDS("data/out/phl-unit-clean.RDS")

#--------------------------------------------------------------------------------------------------------------
# clean data with survey data
#--------------------------------------------------------------------------------------------------------------
blotter_df <- 
  # input all the blotter files
  str_c("data/in/philippines/06_Raw Administrative Data/pnp_blotter/", blotter_files) %>% 
  map(read_excel) %>% 
  setNames(blotter_files) %>% 
  map(mutate, across(c(`Date Encoded (MPS)`, `Blotter Time`, `Incident Time`, `Victims Count`, `Suspects Count`, `Location`, `Summary`), as.character)) %>% 
  bind_rows(.id = "file") %>% 
  mutate(
    station = case_when(
      Station == "Pto.Diaz" ~ "Prieto Diaz Mps",
      TRUE ~ str_to_title(Station)),
    
    mun = case_when(
      station == "Barcelona Mps" ~ "Barcelona",
      station == "Bulan Mps" ~ "Bulan",
      station == "Bulusan Mps" ~ "Bulusan",
      station == "Casiguran Mps" ~ "Casiguran",
      station == "Castilla Mps" ~ "Castilla",
      station == "Donsol Mps" ~ "Donsol",
      station == "Gubat Mps" ~ "Gubat",
      station == "Irosin Mps" ~ "Irosin",
      station == "Juban Mps" ~ "Juban",
      station == "Magallanes Mps" ~ "Magallanes",
      station == "Matnog Mps" ~ "Matnog",
      station == "Pilar Mps" ~ "Pilar",
      station == "Prieto Diaz Mps" ~ "Prieto Diaz",
      station == "Santa Magdalena Mps" ~ "Santa Magdalena", 
      
      str_detect(Barangay, "Bacon") & station == "Sorsogon City Ps" ~ "Sorsogon City (Bacon)",
      str_detect(Barangay, "East") & station == "Sorsogon City Ps" ~ "Sorsogon City (East)",
      str_detect(Barangay, "West") & station == "Sorsogon City Ps" ~ "Sorsogon City (West)", 
      str_detect(Barangay, "Bacon") & station=="Sorcps" ~ "Sorsogon City (Bacon)",
      str_detect(Barangay, "East") & station=="Sorcps" ~ "Sorsogon City (East)",
      str_detect(Barangay, "West") & station=="Sorcps" ~ "Sorsogon City (West)", 
      
      TRUE ~ NA_character_),
    
    offensetype = `Offense Type`,
    offensetype = case_when(
      offensetype = str_detect(offensetype, "10")  ~ 10L,
      offensetype = str_detect(offensetype, "murder")  ~ 13L,
      offensetype = str_detect(offensetype, "Murder")  ~ 13L,
      offensetype = str_detect(offensetype, "MURDER")  ~ 13L,
      offensetype = str_detect(offensetype, "homicide")  ~ 13L,
      offensetype = str_detect(offensetype, "Homicide")  ~ 13L,
      offensetype = str_detect(offensetype, "12")  ~ 12L,
      offensetype == "17-Shooting Incident" ~ 12L,
      offensetype = str_detect(offensetype, "carnapping")  ~ 11L,
      offensetype = str_detect(offensetype, "Carnapping")  ~ 11L,
      offensetype = str_detect(offensetype, "drug")  ~ 4L,
      offensetype = str_detect(offensetype, "Drug")  ~ 4L,
      offensetype = str_detect(offensetype, "drugs")  ~ 4L,
      offensetype = str_detect(offensetype, "Drugs")  ~ 4L,
      offensetype = str_detect(offensetype, "Tokhang")  ~ 4L,
      offensetype = str_detect(offensetype, "Failure To Return Tricycle")  ~ 1L,
      offensetype = str_detect(offensetype, "Rape")  ~ 8L,
      offensetype = str_detect(offensetype, "rape")  ~ 8L,
      offensetype = str_detect(offensetype, "Stabbing")  ~ 12L,
      offensetype = str_detect(offensetype, "stabbing")  ~ 12L,
      offensetype = str_detect(offensetype, "Theft")  ~ 1L,
      offensetype = str_detect(offensetype, "theft")  ~ 1L,
      TRUE ~ NA_integer_),
    
    aarmedrob = if_else(offensetype == 11L, 1, 0),
    aburglary = if_else(offensetype == 1L, 1, 0),
    asexual = if_else(offensetype == 7L, 1, 0),
    amurder = if_else(offensetype == 13L, 1, 0),
    aaggassault = if_else(offensetype == 12L, 1, 0),
    aother = if_else(aarmedrob == 0 & aburglary == 0 & aaggassault == 0 & asexual == 0 & amurder == 0, 1, 0),
    
    wave = case_when(
      `Incident Date` >= as.Date("2016-08-10") & `Incident Date` <= as.Date("2017-02-10") ~ "baseline", 
      `Incident Date` >= as.Date("2018-01-23") & `Incident Date` <= as.Date("2018-07-23") ~ "endline", 
      TRUE ~ NA_character_),
    n_days_wave = case_when(
      wave == "baseline" ~ as.numeric(as.Date("2017-02-10") - as.Date("2016-08-10")),
      wave == "endline" ~ as.numeric(as.Date("2018-07-23") - as.Date("2018-01-23")),
      TRUE ~ NA_real_)) %>% 
  rename(barangay = Barangay) %>% 
  mutate(
    # clean up whitespace
    mun_merge = trimws(gsub("\\s+", " ", tolower(mun))),
    barangay_merge = trimws(gsub("\\s+", " ", tolower(barangay))),
    # in the IDs file, sorsogon city names are different - they don't have the part after the comma (which says , Sorsogon City and variations)
    barangay_merge = if_else(str_detect(tolower(barangay_merge), "sorsogon city") & str_detect(barangay_merge, ","), str_replace(str_extract(barangay_merge, "^(.+?),"), ",", ""), barangay_merge)) %>% 
  left_join(
    pnp_blotter_ids %>% 
      mutate(barangay = if_else(str_detect(tolower(barangay), "sorsogon city") & str_detect(barangay, ","), str_replace(str_extract(barangay, "^(.+?),"), ",", ""), barangay)) %>% 
      transmute(psgc, lgu_code, mun_merge = trimws(gsub("\\s+", " ", tolower(mun))), barangay_merge =  trimws(gsub("\\s+", " ", tolower(barangay))), in_id = 1),
    by = c("mun_merge", "barangay_merge")) %>% 
  select(-mun_merge, -barangay_merge) %>% 
  filter(!is.na(in_id)) 

phl_admin <- 
  blotter_df %>% 
  filter(!is.na(wave)) %>% 
  group_by(psgc, wave) %>% 
  summarize(
    aarmedrob_num = sum(aarmedrob, na.rm = TRUE) / first(n_days_wave),
    aaggassault_num = sum(aaggassault, na.rm = TRUE) / first(n_days_wave),
    asexual_num = sum(asexual, na.rm = TRUE) / first(n_days_wave),
    amurder_num = sum(amurder, na.rm = TRUE) / first(n_days_wave),
    aburglary_num = sum(aburglary, na.rm = TRUE) / first(n_days_wave),
    aother_num = sum(aother, na.rm = TRUE)  / first(n_days_wave)
  ) %>% 
  ungroup() %>% 
  pivot_wider(id_cols = psgc, names_from = wave, values_from = aarmedrob_num:aother_num) %>%
  rename_at(vars(ends_with("_endline")), ~str_replace(., "_endline", "")) %>% 
  right_join(phl_units, by = "psgc") %>% 
  mutate(across(aarmedrob_num:aother_num_baseline, ~replace_na(., 0))) %>% 
  filter(safe_for_research == 1) # filter on psgc's which were supposed to be safe for research. This follows through a conversation with the team on email. 

#--------------------------------------------------------------------------------------------------------------
# save data
#--------------------------------------------------------------------------------------------------------------
saveRDS(phl_admin,  file = "data/out/phl-admin-clean.RDS")
