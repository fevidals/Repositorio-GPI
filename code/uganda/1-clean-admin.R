#--------------------------------------------------------------------------------------------------------------
# all loaded packages come here
#--------------------------------------------------------------------------------------------------------------
suppressMessages({
  library(tidyverse)
  library(lubridate)
})

#--------------------------------------------------------------------------------------------------------------
# load baseline/endline/unit dataframes
#--------------------------------------------------------------------------------------------------------------
# load baseline raw data (from surveyCTO)
uga_admin_baseline <- read.csv("data/in/uganda/06_Raw Administrative Data/UGA_MK4_raw_data_baseline_admin.csv", stringsAsFactors = F)
# load endline raw data (from surveyCTO)
uga_admin_endline <- read.csv("data/in/uganda/06_Raw Administrative Data/UGA_MK4_raw_data_endline_admin_crime.csv", stringsAsFactors = F)
uga_admin_endline_unit <-read.csv("data/in/uganda/06_Raw Administrative Data/UGA_MK4_raw_data_endline_admin_unit.csv", stringsAsFactors = F)
uga_unit <- readRDS("data/out/uga-unit-clean.RDS")

#--------------------------------------------------------------------------------------------------------------
# get admin endline ready for cleaning
#--------------------------------------------------------------------------------------------------------------
uga_admin_endline <- 
  uga_admin_endline %>% 
  left_join(uga_admin_endline_unit, by = "unit_id") %>% 
  mutate(date_clean = parse_date_time(date_report_r, orders = c("Ymd")),
         n_days = as.Date("2020-12-20") - as.Date("2018-11-03")) %>% 
  group_by(unit_id) %>%
  summarise(
    aarmedrob_num = sum(crime_r %in% c(27, 3)) / first(n_days),
    aburglary_num = sum(crime_r %in% c(1, 2)) / first(n_days),
    aaggassault_num = sum(crime_r == 4) / first(n_days), 
    # aggravated assault coded as assault only (GH issue number 100)
    asexual_num = sum(crime_r %in% c(17, 23, 32)) / first(n_days),
    adomestic_phys_num = sum(crime_r == 18) / first(n_days),
    aland_num = sum(crime_r == 9) / first(n_days),
    amob_num = sum(crime_r == 11) / first(n_days),
    ariot_num = sum(crime_r == 5) / first(n_days),
    amurder_num = sum(crime_r == 20) / first(n_days),
    aother_num_violent = sum(crime_r %in% c(12, 15, 22, 37)) / first(n_days),
    aother_num_nonviolent = sum(crime_r %in% c(24, 6, 7, 8, 10, 13, 14, 19, 25, 28, 29, 30, 34, 32, 35, 36)) / first(n_days),
    apolvtm_station = case_when(any(crime_r ==48) ~ 1,
                                any(dpc_complaint ==1) ~ 1,
                                TRUE ~ 0)) %>% 
  ungroup()

uga_admin_baseline <- 
  uga_admin_baseline %>% 
  mutate(date_clean = parse_date_time(date_report_r, orders = c("Ymd")),
         n_days = as.Date("2019-04-10") - as.Date("2017-04-17")) %>%
  group_by(unit_id) %>%
  summarize(
    aarmedrob_num = sum(crime_r %in% c(3)) / first(n_days),
    aburglary_num = sum(crime_r %in% c(1, 2)) / first(n_days),
    aaggassault_num = sum(crime_r == 4) / first(n_days),
    # aggravated assault coded as assault only (GH issue number 100)
    asexual_num = sum(crime_r %in% c(17, 23)) / first(n_days),
    adomestic_phys_num = sum(crime_r == 18) / first(n_days),
    aland_num = sum(crime_r == 9) / first(n_days),
    amob_num = sum(crime_r == 11) / first(n_days),
    ariot_num = sum(crime_r == 5) / first(n_days),
    amurder_num = sum(crime_r == 20) / first(n_days),
    aother_num_violent = sum(crime_r %in% c(12, 15, 22)) / first(n_days),
    aother_num_nonviolent = sum(crime_r %in% c(24, 6, 7, 8, 10, 13, 14, 19)) / first(n_days)) %>% 
  ungroup()

uga_admin <- 
  uga_admin_baseline %>% 
  bind_rows(uga_admin_endline, 
            .id = "wave") %>% 
  mutate(wave = if_else(wave == 1, "baseline", "endline")) %>% 
  pivot_wider(id_cols = unit_id, names_from = wave, values_from = aarmedrob_num:apolvtm_station) %>% 
  rename_with(~gsub("_endline", "", .), everything()) %>% 
  rename(station_id = unit_id) %>% 
  right_join(uga_unit, by = "station_id")

# save the data
saveRDS(uga_admin,  file = "data/out/uga-admin-clean.RDS")
