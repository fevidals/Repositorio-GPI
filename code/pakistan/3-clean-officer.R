#--------------------------------------------------------------------------------------------------------------
# load packages
#--------------------------------------------------------------------------------------------------------------
suppressMessages({
  library(tidyverse)
  library(haven)
  library(readxl)
})

#--------------------------------------------------------------------------------------------------------------
# get raw data
#--------------------------------------------------------------------------------------------------------------
pak_officer_baseline <- read_dta("data/in/pakistan/05_Raw De-Indentified Data/PK_MK4_raw_data_officers_firstwave.dta")
pak_officer_endline <- read_dta("data/in/pakistan/05_Raw De-Indentified Data/PK_MK4_raw_data_officers_secondwave.dta")
pak_randomization <- readRDS("data/out/pak-unit-clean.RDS")
pak_officer_code <- read_excel("data/in/pakistan/07_Processed Data/a_First Stage Processing/Survey Data/Final Pre Fill List_English_new.xlsx", sheet = "Sheet1")
pak_officer_kasur_codes <- read_excel("data/in/pakistan/07_Processed Data/a_First Stage Processing/Survey Data/kasur_beat_codes.xls")
kasur_coded_data <- read_dta("data/in/pakistan/07_Processed Data/a_First Stage Processing/Survey Data/kasur_coded_data.dta")

pak_units <- readRDS("data/out/pak-unit-clean.RDS")


#--------------------------------------------------------------------------------------------------------------
# clean endline-data
#--------------------------------------------------------------------------------------------------------------

pak_officer_code <- 
  pak_officer_code %>% 
  rename(police_code = "Police Officer Code")

pak_officer_endline <- 
  pak_officer_endline %>% 
  filter(district_p != "Kasur") %>% 
  bind_rows(kasur_coded_data %>% select(-sr_no, -a)) %>% 
  rename(block_name = ps_name, cluster_name = beat_name_p, geo_unit = district_p) %>% 
  group_by(police_code) %>% 
  mutate(sr_no = 1:n(), a = max(sr_no)) %>% 
  ungroup %>% 
  mutate(geo_unit = if_else(geo_unit == "", "Kasur", geo_unit)) %>% 
  filter((a == 1) | (a == 2 & consent == 1)) %>% 
  distinct(police_code, .keep_all = TRUE) %>% 
  filter(!is.na(police_code)) %>% 
  inner_join(pak_officer_code, by = "police_code") %>% 
  filter(consent == 1) %>% 
  mutate(police_code_chr = as.character(police_code),
         police_id = as.integer(str_sub(police_code_chr, 6, 6))) %>% 
  distinct(geo_unit_ID, block_ID, Cluster_ID, police_id, .keep_all = TRUE) %>% 
  mutate(across(.cols = c("Cluster_ID", "block_ID", "geo_unit_ID"), as.integer)) %>% 
  select(
    geo_unit_ID, 
    block_ID, 
    Cluster_ID,
    police_id,
    
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
    hypothetical5_abuseother)


pak_officer_baseline_orig <- 
  pak_officer_baseline %>% 
  group_by(police_station, beat) %>% 
  mutate(n = 1 : n()) %>% 
  ungroup %>% 
  mutate(
    beat = case_when(
      beat == "2.Ferozwala" & n == 2 ~ "7.Labanwala",
      police_station == "Sadar Sangla Hill" & beat == "other" & n == 2 ~ "3. Badhomali", 
      TRUE ~ beat),
    police_station = str_replace(police_station, " ", ""),
    police_station = str_replace(police_station, " ", ""),
    police_station = str_to_sentence(police_station),
    beat = str_replace(beat, " ", ""),
    beat = str_replace(beat, " ", ""),
    beat = str_to_sentence(beat)) %>% 
  rename(cluster_name = beat) %>% 
  distinct(cluster_name, .keep_all = TRUE)


pak_officer_code_base <- 
  pak_officer_code %>% 
  select(cluster_name, Cluster_ID, block_ID, geo_unit_ID) %>% 
  filter(geo_unit_ID != "3") %>% 
  distinct(cluster_name, .keep_all = TRUE) %>% 
  mutate(cluster_name = str_replace(cluster_name, " ", ""),
         cluster_name = str_replace(cluster_name, " ", ""),
         cluster_name = str_to_sentence(cluster_name))

pak_officer_baseline <- 
  pak_officer_baseline_orig %>% 
  filter(district != "Kasur") %>% 
  left_join(pak_officer_code_base %>% mutate(in_using = 1), by = "cluster_name") %>% 
  mutate(
    Cluster_ID = if_else(is.na(in_using), "10584", Cluster_ID),
    block_ID = if_else(is.na(in_using), "105", block_ID),
    geo_unit_ID = if_else(is.na(in_using), "1", geo_unit_ID)) %>% 
  mutate(across(.cols = c("Cluster_ID", "block_ID", "geo_unit_ID"), as.integer)) %>% 
  bind_rows(pak_officer_baseline_orig %>% 
              filter(district == "Kasur") %>% 
              left_join(pak_officer_kasur_codes %>% 
                          rename(cluster_name = beat), by = c("police_station", "cluster_name"))) %>% 
  group_by(geo_unit_ID, block_ID, Cluster_ID) %>% 
  summarise_all(mean, na.rm = TRUE) %>% 
  ungroup %>% 
  select(
    geo_unit_ID, 
    block_ID, 
    Cluster_ID,
    
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
    hypothetical5_abuseother) %>% 
  rename_at(vars(everything(), -geo_unit_ID, -block_ID, -Cluster_ID), ~paste0(., "_baseline"))

pak_officer <- 
  pak_officer_endline %>% 
  left_join(pak_officer_baseline %>% mutate(in_baseline = 1), by = c("geo_unit_ID", "block_ID", "Cluster_ID")) %>% 
  rename(cluster_ID = Cluster_ID) %>% 
  right_join(pak_units %>% rename(cluster_ID = cluster_id), by = c("geo_unit_ID", "block_ID", "cluster_ID")) %>% 
  # all responses representing do not know are recorded as 97 in the survey questionnaire
  mutate(across(empathy_complaints:hypothetical5_abuseother_baseline, na_if, 97)) %>% 
  # all responses representing refuse to answer are recorded as 97 in the survey questionnaire
  mutate(across(empathy_complaints:hypothetical5_abuseother_baseline, na_if, 98))

saveRDS(pak_officer,  file = "data/out/pak-officer-clean.RDS")
