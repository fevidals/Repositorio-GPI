#--------------------------------------------------------------------------------------------------------------
                      # all loaded packages come here (resolve warnings in the file)
#--------------------------------------------------------------------------------------------------------------
suppressMessages({
  library(tidyverse)
  library(haven)
})

#--------------------------------------------------------------------------------------------------------------
                      # get raw data
#--------------------------------------------------------------------------------------------------------------
uga_citizen_baseline <- read.csv("data/in/uganda/05_Raw De-Identified Data/UGA_MK4_raw_data_baseline_citizens.csv", stringsAsFactors = F)
uga_citizen_other_indivi <- read.csv("data/in/uganda/05_Raw De-Identified Data/UGA_MK4_raw_data_baseline_citizens_coding_individual_victimization_other.csv", stringsAsFactors = F)
uga_citizen_other_commun <- read.csv("data/in/uganda/05_Raw De-Identified Data/UGA_MK4_raw_data_baseline_citizens_coding_community_victimization_other.csv", stringsAsFactors = F)
uga_citizen_endline <- read.csv("data/in/uganda/05_Raw De-Identified Data/UGA_MK4_raw_data_endline_citizens.csv", stringsAsFactors = F)
uga_unit <- readRDS("data/out/uga-unit-clean.RDS")

#--------------------------------------------------------------------------------------------------------------
                      # other_files: clean
#--------------------------------------------------------------------------------------------------------------
# uga_citizen_other_indivi <- 
#   uga_citizen_other_indivi %>% 
#   mutate(value = if_else(!is.na(type_crime), 1L, NA_integer_)) %>% 
#   pivot_wider(names_from = type_crime, values_from = value, id_cols = citizen_id) %>% 
#   select(citizen_id, other_any_violent = violent,other_any_nonviolent = `non-violent`) 
# 
# uga_citizen_other_commun <- 
#   uga_citizen_other_commun %>% 
#   mutate(value = if_else(!is.na(type_crime), 1L, NA_integer_)) %>% 
#   pivot_wider(names_from = type_crime, values_from = value) %>% 
#   select(citizen_id, cother_any_violent = violent,cother_any_nonviolent = `non-violent`)
# 
#--------------------------------------------------------------------------------------------------------------
                      # endline: clean
#--------------------------------------------------------------------------------------------------------------

# select relevant variables
uga_citizen_endline <- 
  uga_citizen_endline %>% 
  # code other crimes at endline 
  mutate(
    other_any_violent = case_when(
      str_detect(other_specify, paste0("\\b", "2", "\\b")) & other_any == 1 ~ 1L,
      str_detect(other_specify, paste0("\\b", "3", "\\b")) & other_any == 1 ~ 1L,
      TRUE ~ 0L),
    other_any_nonviolent = case_when(
      str_detect(other_specify, paste0("\\b", "1", "\\b")) & other_any == 1 ~ 1L,
      str_detect(other_specify, paste0("\\b", "7", "\\b")) & other_any == 1 ~ 1L,
      str_detect(other_specify, paste0("\\b", "11", "\\b")) & other_any == 1 ~ 1L,
      str_detect(other_specify, paste0("\\b", "12", "\\b")) & other_any == 1 ~ 1L,
      str_detect(other_specify, paste0("\\b", "13", "\\b")) & other_any == 1 ~ 1L,
      str_detect(other_specify, paste0("\\b", "14", "\\b")) & other_any == 1 ~ 1L,
      TRUE ~ 0L),
    cother_any_violent = case_when(
      str_detect(cother_list, paste0("\\b", "2", "\\b")) & cother_any == 1 ~ 1L,
      str_detect(cother_list, paste0("\\b", "3", "\\b")) & cother_any == 1 ~ 1L,
      TRUE ~ 0L),
    cother_any_nonviolent = case_when(
      str_detect(cother_list, paste0("\\b", "1", "\\b")) &  cother_any == 1 ~ 1L,
      str_detect(cother_list, paste0("\\b", "7", "\\b")) &  cother_any == 1 ~ 1L,
      str_detect(cother_list, paste0("\\b", "11", "\\b")) & cother_any == 1 ~ 1L,
      str_detect(cother_list, paste0("\\b", "12", "\\b")) & cother_any == 1 ~ 1L,
      str_detect(cother_list, paste0("\\b", "13", "\\b")) & cother_any == 1 ~ 1L,
      str_detect(cother_list, paste0("\\b", "14", "\\b")) & cother_any == 1 ~ 1L,
      TRUE ~ 0L),
    other_report_violent = case_when(
      str_detect(other_specify, paste0("\\b", "2", "\\b")) & other_any == 1 & str_detect(other_report, paste0("\\b", "1", "\\b")) ~ 1L,
      str_detect(other_specify, paste0("\\b", "3", "\\b")) & other_any == 1 & str_detect(other_report, paste0("\\b", "1", "\\b")) ~ 1L,
      TRUE ~ 0L),
    other_report_nonviolent = case_when(
      str_detect(other_specify, paste0("\\b", "1", "\\b")) & other_any == 1 & str_detect(other_report, paste0("\\b", "1", "\\b")) ~ 1L,
      str_detect(other_specify, paste0("\\b", "7", "\\b")) & other_any == 1 & str_detect(other_report, paste0("\\b", "1", "\\b")) ~ 1L,
      str_detect(other_specify, paste0("\\b", "11", "\\b")) & other_any == 1 & str_detect(other_report, paste0("\\b", "1", "\\b")) ~ 1L,
      str_detect(other_specify, paste0("\\b", "12", "\\b")) & other_any == 1 & str_detect(other_report, paste0("\\b", "1", "\\b")) ~ 1L,
      str_detect(other_specify, paste0("\\b", "13", "\\b")) & other_any == 1 & str_detect(other_report, paste0("\\b", "1", "\\b")) ~ 1L,
      str_detect(other_specify, paste0("\\b", "14", "\\b")) & other_any == 1 & str_detect(other_report, paste0("\\b", "1", "\\b")) ~ 1L,
      TRUE ~ 0L),
    cother_report_violent = case_when(
      str_detect(cother_list, paste0("\\b", "2", "\\b")) & cother_any == 1 & str_detect(cother_report, paste0("\\b", "1", "\\b")) ~ 1L,
      str_detect(cother_list, paste0("\\b", "3", "\\b")) & cother_any == 1 & str_detect(cother_report, paste0("\\b", "1", "\\b")) ~ 1L,
      TRUE ~ 0L),
    cother_report_nonviolent = case_when(
      str_detect(cother_list, paste0("\\b", "1", "\\b")) &  cother_any == 1 & str_detect(cother_report, paste0("\\b", "1", "\\b")) ~ 1L,
      str_detect(cother_list, paste0("\\b", "7", "\\b")) &  cother_any == 1 & str_detect(cother_report, paste0("\\b", "1", "\\b")) ~ 1L,
      str_detect(cother_list, paste0("\\b", "11", "\\b")) & cother_any == 1 & str_detect(cother_report, paste0("\\b", "1", "\\b")) ~ 1L,
      str_detect(cother_list, paste0("\\b", "12", "\\b")) & cother_any == 1 & str_detect(cother_report, paste0("\\b", "1", "\\b")) ~ 1L,
      str_detect(cother_list, paste0("\\b", "13", "\\b")) & cother_any == 1 & str_detect(cother_report, paste0("\\b", "1", "\\b")) ~ 1L,
      str_detect(cother_list, paste0("\\b", "14", "\\b")) & cother_any == 1 & str_detect(cother_report, paste0("\\b", "1", "\\b")) ~ 1L,
      cother_list %in% c("1", "7", "11", "12", "13", "14") & cother_any == 1 &  str_detect(cother_report, "1") ~ 1L,
      TRUE ~ 0L)) %>% 
  select(
    station_id = unit_id,
    village_id,
    citizen_id,

    # 1a
    armedrob_num,
    simpleassault_num,
    other_any_violent,
  
    burglary_num,
    other_any_nonviolent,

    carmedrob_num,
    caggassault_num,
    csimpleassault_num,
    csexual_num,
    cdomestic_phys_num,
    cmurder_num,
    cother_any_violent,

    cburglary_num,
    cother_any_nonviolent,
    
    other_any, 
    other_specify, 
    cother_any, 
    cother_list, 
    
    armedrob_any,
    simpleassault_any,
    burglary_any,
    carmedrob_any,
    caggassault_any,
    csimpleassault_any,
    csexual_any,
    cdomestic_phys_any,
    cmurder_any,
    cburglary_any,
    
    # 1a (ii)
    aggassault_num = aggattack_num,
    aggassault_any = aggattack_any, 
    sexual_any = rape_any, 
    sexual_num = rape_num,
    domestic_phys_any = phyabuse_any,
    domestic_phys_num = phyabuse_num,
    domestic_verbal_any = verbabuse_any,
    domestic_verbal_num = verbabuse_num,
    land_any = house_conflict,
    cmob_num,
    cmob_any,
    cdomestic_verbal_num,
    cland_any,

    # 1b
    fear_violent,
    fear_nonviolent,
    feared_walk,
    
    # 2
    satis_trust,
    satis_general,
    
    # 3b
    policeabuse_phys_any,
    policeabuse_verbal_any,
    policeabuse_phys_num,
    policeabuse_verbal_num,
    bribe_freq, 
    bribe_amt,
    
    # 4a
    burglaryres,
    dviolres,
    armedrobres,
    
    armedrob_report,
    simpleassault_report,
    other_report_violent,
    
    burglary_report,
    other_report_nonviolent,
    
    carmedrob_report,
    caggassault_report,
    csimpleassault_report,
    csexual_report,
    cdomestic_phys_report,
    cmurder_report,
    cother_report_violent,
    
    cburglary_report,
    cother_report_nonviolent,
    
    other_report,
    cother_report,
    
    # 4b
    contact_pol_susp_activity,
    give_info_pol_investigation,
    
    # 4c
    policebeating_report,
    policeabuse_verbal_report,
    policeabuse_phys_report,
    dutydrink_report,
    
    # M1a
    polcaseserious = polcaseserious_armedrobbery,
    polcasefair = polcasefair_armedrobbery,
    polint_corrupt,
    polint_quality,
    
    # M1b
    know_law_suspect,
    know_law_lawyer,
    know_law_fees,
    know_law_vaw,
    know_report_followup = know_law_phone,
    know_report_station,
    
    # M1c
    reportnorm_theft,
    reportnorm_abuse,
    obeynorm,
    
    # M2a
    polcap_timely,
    polcap_investigate,
    
    # M2b
    responsive_act,
    
    # S1
    # legit_trust, # legit trust was not collected in Uga as per their PAP
    
    # S2
    trust_community,
    
    # C
    compliance_patrol,
    compliance_freq,
    compliance_meeting,
    compliance_attend = meetings_attended
  ) %>% 
  mutate(
    armedrob_num = case_when(
      armedrob_any == 0 ~ 0L, 
      is.na(armedrob_any) ~ NA_integer_, 
      TRUE ~ as.integer(armedrob_num)),
    simpleassault_num = case_when(
      simpleassault_any == 0 ~ 0L, 
      is.na(simpleassault_any) ~ NA_integer_, 
      TRUE ~ as.integer(simpleassault_num)),
    burglary_num = case_when(
      burglary_any == 0 ~ 0L, 
      is.na(burglary_any) ~ NA_integer_, 
      TRUE ~ as.integer(burglary_num)),
    carmedrob_num = case_when(
      carmedrob_any == 0 ~ 0L, 
      is.na(carmedrob_any) ~ NA_integer_, 
      TRUE ~ as.integer(carmedrob_num)),
    caggassault_num = case_when(
      caggassault_any == 0 ~ 0L, 
      is.na(caggassault_any) ~ NA_integer_,
      TRUE ~ as.integer(caggassault_num)),
    csimpleassault_num = case_when(
      csimpleassault_any == 0 ~ 0L, 
      is.na(csimpleassault_any) ~ NA_integer_, 
      TRUE ~ as.integer(csimpleassault_num)),
    csexual_num = case_when(
      csexual_any == 0 ~ 0L, 
      is.na(csexual_any) ~ NA_integer_, 
      TRUE ~ as.integer(csexual_num)),
    cdomestic_phys_num = case_when(
      cdomestic_phys_any == 0 ~ 0L, 
      is.na(cdomestic_phys_any) ~ NA_integer_, 
      TRUE ~ as.integer(cdomestic_phys_num)),
    cmurder_num = case_when(
      cmurder_any == 0 ~ 0L, 
      is.na(cmurder_any) ~ NA_integer_, 
      TRUE ~ as.integer(cmurder_num)),
    cburglary_num = case_when(
      cburglary_any == 0 ~ 0L, 
      is.na(cburglary_any) ~ NA_integer_,
      TRUE ~ as.integer(cburglary_num)),
    other_any_violent = case_when(
      other_any == 0 ~ 0L, 
      is.na(other_any) ~ NA_integer_,  
      other_any == 777 ~ NA_integer_, 
      TRUE ~ as.integer(other_any_violent)),
    other_any_nonviolent = case_when(
      other_any == 0 ~ 0L, 
      is.na(other_any) ~ NA_integer_,  
      other_any == 777 ~ NA_integer_, 
      TRUE ~ as.integer(other_any_nonviolent)),
    cother_any_violent = case_when(
      cother_any == 0 ~ 0L, 
      is.na(cother_any) ~ NA_integer_,  
      cother_any == 777 ~ NA_integer_, 
      TRUE ~ as.integer(cother_any_violent)),
    cother_any_nonviolent = case_when(
      cother_any == 0 ~ 0L, 
      is.na(cother_any) ~ NA_integer_,  
      cother_any == 777 ~ NA_integer_, 
      TRUE ~ as.integer(cother_any_nonviolent)),
    compliance_attend = case_when(
      compliance_attend > 0 ~ 1L, 
      compliance_attend == 0 ~ 0L, 
      TRUE ~ compliance_attend))

#--------------------------------------------------------------------------------------------------------------
# baseline: clean
#--------------------------------------------------------------------------------------------------------------

# select relevant variables
uga_citizen_baseline <- 
  uga_citizen_baseline %>% 
  left_join(uga_citizen_other_indivi, by = "citizen_id") %>% 
  mutate(
    other_any_violent = case_when(
      type_crime == "violent" & other_any == 1 ~ 1L, 
      type_crime == "non-violent" & other_any == 1 ~ 0L, 
      type_crime == "no crime" & other_any == 1 ~ 0L, 
      other_any == 0 ~ 0L,
      TRUE ~ NA_integer_),
    other_any_nonviolent = case_when(
      type_crime == "violent" & other_any == 1 ~ 0L, 
      type_crime == "non-violent" & other_any == 1 ~ 1L, 
      type_crime == "no crime" & other_any == 1 ~ 0L, 
      other_any == 0 ~ 0L,
      TRUE ~ NA_integer_)) %>% 
  select(-type_crime) %>% 
  left_join(uga_citizen_other_commun, by = "citizen_id") %>% 
  mutate(
    cother_any_violent = case_when(
      type_crime == "violent" & cother_any == 1 ~ 1L, 
      type_crime == "non-violent" & cother_any == 1 ~ 0L, 
      type_crime == "no crime" & cother_any == 1 ~ 0L, 
      other_any == 0 ~ 0L,
      TRUE ~ NA_integer_),
    cother_any_nonviolent = case_when(
      type_crime == "violent" & cother_any == 1 ~ 0L, 
      type_crime == "non-violent" & cother_any == 1 ~ 1L, 
      type_crime == "no crime" & cother_any == 1 ~ 0L, 
      other_any == 0 ~ 0L,
      TRUE ~ NA_integer_)) %>% 
  mutate(
    other_report_violent = case_when(
      other_any_violent == 1 & other_any == 1 & str_detect(other_report, paste0("\\b", "1", "\\b")) ~ 1L,
      TRUE ~ 0L),
    other_report_nonviolent = case_when(
      other_any_nonviolent == 1 & other_any == 1 & str_detect(other_report, paste0("\\b", "1", "\\b")) ~ 1L,
      TRUE ~ 0L),
    cother_report_violent = case_when(
      cother_any_violent == 1 & cother_any == 1 & str_detect(cother_report, paste0("\\b", "1", "\\b")) ~ 1L,
      TRUE ~ 0L),
    cother_report_nonviolent = case_when(
      cother_any_nonviolent == 1 & cother_any == 1 & str_detect(cother_report, paste0("\\b", "1", "\\b")) ~ 1L,
      TRUE ~ 0L)) %>% 
  select(
    village_id,
    citizen_id,

    # 1a
    armedrob_num,
    simpleassault_num,
    other_any_violent,
    other_any,
    cother_any,
    
    burglary_num,
    other_any_nonviolent,
    
    carmedrob_num,
    caggassault_num,
    csimpleassault_num,
    csexual_num,
    cdomestic_phys_num,
    cmurder_num,
    cother_any_violent,
    
    cburglary_num,
    cother_any_nonviolent,
    
    armedrob_any,
    simpleassault_any = simattack_any,
    burglary_any = burg_any,
    carmedrob_any,
    caggassault_any,
    csimpleassault_any,
    csexual_any,
    cdomestic_phys_any,
    cmurder_any = murder_any,
    cburglary_any,
    
    # 1a (ii)
    aggassault_num = aggattack_num,
    aggassault_any = aggattack_any, 
    # sexual_any = rape_any, # see github issue # 101
    # sexual_num = rape_num,
    domestic_phys_any = domviol_any,
    domestic_phys_num = domviol_num,
    land_any = house_conflict,
    cmob_num = mob_num,
    cmob_any = mob_any,
    
    # 1b
    fear_violent,
    fear_nonviolent,
    feared_walk,
    
    # 2
    satis_trust,
    satis_general,
    
    # 3b
    policeabuse_phys_any,
    policeabuse_verbal_any,
    policeabuse_phys_num,
    policeabuse_verbal_num,
    bribe_freq, 
    bribe_amt,
    
    # 4a
    burglaryres,
    dviolres,
    armedrobres = armedrobbery_report,
    
    armedrob_report,
    simpleassault_report,
    other_report_violent,
    
    burglary_report,
    other_report_nonviolent,
    
    carmedrob_report,
    caggassault_report,
    csimpleassault_report,
    csexual_report,
    cdomestic_phys_report,
    cmurder_report,
    cother_report_violent,
    
    cburglary_report,
    cother_report_nonviolent,
    
    # 4b
    contact_pol_susp_activity,
    give_info_pol_investigation,
    
    # 4c
    policebeating_report,
    policeabuse_verbal_report,
    policeabuse_phys_report,
    dutydrink_report,
    
    # M1a
    polcaseserious,
    polcasefair,
    polint_corrupt,
    polint_quality,
    
    # M1b
    know_law_suspect,
    know_law_lawyer,
    know_law_fees,
    know_law_vaw,
    know_report_followup = inperson_followup,
    know_report_station,
    
    # M1c
    reportnorm_theft,
    reportnorm_abuse,
    obeynorm,
    
    # M2a
    polcap_timely,
    polcap_investigate,
    
    # M2b
    responsive_act,
    
    # S1
    # legit_trust,
    
    # S2
    trust_community,
    
    # C
    compliance_patrol,
    compliance_freq,
    compliance_meeting,
    
  ) %>% 
  mutate(
    armedrob_num = case_when(
      armedrob_any == 0 ~ 0L, 
      is.na(armedrob_any) ~ NA_integer_, 
      TRUE ~ as.integer(armedrob_num)),
    simpleassault_num = case_when(
      simpleassault_any == 0 ~ 0L, 
      is.na(simpleassault_any) ~ NA_integer_, 
      TRUE ~ as.integer(simpleassault_num)),
    burglary_num = case_when(
      burglary_any == 0 ~ 0L, 
      is.na(burglary_any) ~ NA_integer_, 
      TRUE ~ as.integer(burglary_num)),
    carmedrob_num = case_when(
      carmedrob_any == 0 ~ 0L, 
      is.na(carmedrob_any) ~ NA_integer_, 
      TRUE ~ as.integer(carmedrob_num)),
    caggassault_num = case_when(
      caggassault_any == 0 ~ 0L, 
      is.na(caggassault_any) ~ NA_integer_,
      TRUE ~ as.integer(caggassault_num)),
    csimpleassault_num = case_when(
      csimpleassault_any == 0 ~ 0L, 
      is.na(csimpleassault_any) ~ NA_integer_, 
      TRUE ~ as.integer(csimpleassault_num)),
    csexual_num = case_when(
      csexual_any == 0 ~ 0L, 
      is.na(csexual_any) ~ NA_integer_, 
      TRUE ~ as.integer(csexual_num)),
    cdomestic_phys_num = case_when(
      cdomestic_phys_any == 0 ~ 0L, 
      is.na(cdomestic_phys_any) ~ NA_integer_, 
      TRUE ~ as.integer(cdomestic_phys_num)),
    cmurder_num = case_when(
      cmurder_any == 0 ~ 0L, 
      is.na(cmurder_any) ~ NA_integer_, 
      TRUE ~ as.integer(cmurder_num)),
    cburglary_num = case_when(
      cburglary_any == 0 ~ 0L, 
      is.na(cburglary_any) ~ NA_integer_,
      TRUE ~ as.integer(cburglary_num)),
    other_any_violent = case_when(
      other_any == 0 ~ 0L,
      is.na(other_any) ~ NA_integer_,  
      other_any == 777 ~ NA_integer_, 
      TRUE ~ as.integer(other_any_violent)),
    other_any_nonviolent = case_when(
      other_any == 0 ~ 0L, 
      is.na(other_any) ~ NA_integer_,  
      other_any == 777 ~ NA_integer_, 
      TRUE ~ as.integer(other_any_nonviolent)),
    cother_any_violent = case_when(
      cother_any == 0 ~ 0L, 
      is.na(cother_any) ~ NA_integer_, 
      cother_any == 777 ~ NA_integer_, 
      TRUE ~ as.integer(cother_any_violent)),
    cother_any_nonviolent = case_when(
      cother_any == 0 ~ 0L, 
      is.na(cother_any) ~ NA_integer_,  
      cother_any == 777 ~ NA_integer_, 
      TRUE ~ as.integer(cother_any_nonviolent))) %>% 
  rename_at(vars(everything(), -citizen_id, -village_id), ~paste0(., "_baseline"))

saveRDS(uga_citizen_baseline,  file = "data/out/uga-citizen-clean-baseline.RDS")
saveRDS(uga_citizen_endline,  file = "data/out/uga-citizen-clean-endline.RDS")


# merge the two waves
uga_citizen <- 
  uga_citizen_endline %>% 
  mutate(in_endline = 1) %>% 
  left_join(uga_unit, by = c("village_id", "station_id")) %>%
  full_join(uga_citizen_baseline %>% mutate(in_baseline = 1), by = c("village_id", "citizen_id")) %>% 
  replace_na(list(in_endline = 0, in_baseline = 0)) %>% 
  filter(in_endline == 1) %>%   # note removes 510 observations that were interviewed at baseline but could not be recontacted at endline
  # replace baseline values with missing and non responses and other responses
  mutate(across(fear_violent:compliance_meeting, na_if, 999)) %>% 
  mutate(across(fear_violent:compliance_meeting, na_if, 777)) %>% 
  mutate(across(fear_violent:compliance_meeting, na_if, 888)) %>% 
  mutate(across(fear_violent_baseline:compliance_meeting_baseline, na_if, 999)) %>% 
  mutate(across(fear_violent_baseline:compliance_meeting_baseline, na_if, 777)) %>% 
  mutate(across(fear_violent_baseline:compliance_meeting_baseline, na_if, 888))

saveRDS(uga_citizen,  file = "data/out/uga-citizen-clean.RDS")
  