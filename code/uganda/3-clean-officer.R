#--------------------------------------------------------------------------------------------------------------
# all loaded packages come here 
#--------------------------------------------------------------------------------------------------------------
suppressMessages({
  library(tidyverse)
  library(haven)
})

#--------------------------------------------------------------------------------------------------------------
# get raw data
#--------------------------------------------------------------------------------------------------------------

uga_officer_baseline <- read.csv("data/in/uganda/05_Raw De-Identified Data/UGA_MK4_raw_data_baseline_officers.csv", stringsAsFactors = F)
uga_officer_endline <- read.csv("data/in/uganda/05_Raw De-Identified Data/UGA_MK4_raw_data_endline_officers.csv", stringsAsFactors = F)
uga_unit <- readRDS("data/out/uga-unit-clean.RDS")

#--------------------------------------------------------------------------------------------------------------
# baseline: clean
#--------------------------------------------------------------------------------------------------------------

uga_officer_baseline <- 
  uga_officer_baseline %>% 
  select(
    officer_id, 
    station_id = unit_id,
    
    empathy_complaints,
    empathy_reports,
    
    account_pol_matter,
    hypothetical2_punishment, 
    hypothetical2_reportself, 
    hypothetical2_reportothers,
    hypothetical2_corruptself, 
    hypothetical2_corruptother, 
    
    hypothetical3_punishment,
    hypothetical3_reportself, 
    hypothetical3_reportothers,
    hypothetical3_corruptself,
    hypothetical3_corruptother,
    
    hypothetical5_punishment,
    hypothetical5_reportself, 
    hypothetical5_reportothers,
    hypothetical5_abuseself,
    hypothetical5_abuseother
  ) %>% 
  setNames(c(names(.)[1], paste0(names(.)[-1], "_baseline"))) %>% 
  mutate(station_id = station_id_baseline)

#--------------------------------------------------------------------------------------------------------------
# endline: clean (all vars required are collected at baseline and endline)
#--------------------------------------------------------------------------------------------------------------

uga_officer_endline <- 
  uga_officer_endline %>% 
  select(
    officer_id, 
    station_id = unit_id,
    
    empathy_complaints,
    empathy_reports,
    
    account_pol_matter,
    hypothetical2_punishment, 
    hypothetical2_reportself, 
    hypothetical2_reportothers,
    hypothetical2_corruptself, 
    hypothetical2_corruptother, 
    
    hypothetical3_punishment,
    hypothetical3_reportself, 
    hypothetical3_reportothers,
    hypothetical3_corruptself,
    hypothetical3_corruptother,
    
    hypothetical5_punishment,
    hypothetical5_reportself, 
    hypothetical5_reportothers,
    hypothetical5_abuseself,
    hypothetical5_abuseother
  )

#--------------------------------------------------------------------------------------------------------------
# merge the two
#--------------------------------------------------------------------------------------------------------------

uga_officer <- 
  uga_officer_endline %>% 
  left_join(uga_officer_baseline %>% mutate(in_baseline = 1), by = c("officer_id", "station_id")) %>% 
  right_join(uga_unit %>% distinct(station_id, .keep_all = TRUE), by = "station_id") %>% 
  mutate(hypothetical3_reportothers = na_if(hypothetical3_reportothers, 777L))


saveRDS(uga_officer,  file = "data/out/uga-officer-clean.RDS")
