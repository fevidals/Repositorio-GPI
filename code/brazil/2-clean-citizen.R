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
bra_citizen <- read_dta("data/in/brazil/07_Processed Data/survey2.dta") 

bra_units <- readRDS("data/out/bra-units-clean.RDS")

#--------------------------------------------------------------------------------------------------------------
# fix faulty GPS readings and correct meeting IDs 
#--------------------------------------------------------------------------------------------------------------

centroid_distance <- read.table("data/in/brazil/07_Processed Data/mindist.csv") %>% as_tibble

centroid_distance_bl <- centroid_distance %>% 
  transmute(meeting_id_bl = id, mindist_bl = mindist, metainstanceid = bl_metainstanceID) %>% 
  # to deal with duplicates, select the one with the mindist
  group_by(metainstanceid) %>% 
  slice_min(order_by = mindist_bl, n = 1, with_ties = FALSE) %>%
  ungroup

centroid_distance_el <- centroid_distance %>% 
  transmute(meeting_id_el = id, mindist_el = mindist, el_metainstanceid = el_metainstanceID) %>% 
  # to deal with duplicates, select the one with the mindist
  group_by(el_metainstanceid) %>% 
  slice_min(order_by = mindist_el, n = 1, with_ties = FALSE) %>%
  ungroup

bra_citizen <- bra_citizen %>% 
  left_join(centroid_distance_bl, by = "metainstanceid") %>% 
  left_join(centroid_distance_el, by = "el_metainstanceid") %>% 
  # replace meeting ID with IDs based on alternative calculation, use original otherwise
  mutate(meeting_id = case_when(
    !is.na(meeting_id_bl) ~ meeting_id_bl,
    !is.na(meeting_id_el) ~ meeting_id_el,
    TRUE ~ meeting_id
  ),
  mindist = case_when(
    !is.na(mindist_bl) ~ mindist_bl,
    !is.na(mindist_el) ~ mindist_el,
    TRUE ~ mindist
  )) 

#--------------------------------------------------------------------------------------------------------------
# subset to those within 300 m
#--------------------------------------------------------------------------------------------------------------

bra_citizen <- bra_citizen %>% 
  # subset for mindist < 300 (note there are still 68 observations with missing mindist, which are also dropped)
  filter(mindist < 300 & !is.na(mindist))

#--------------------------------------------------------------------------------------------------------------
# keep selected variables
#--------------------------------------------------------------------------------------------------------------
bra_citizen_endline <- 
  bra_citizen %>% 
  
  # wave: 0 baseline, 1 endline. there is 1 erroneous NA observation.
  filter(wave == 1 & !is.na(wave)) %>% 
  
  # drop refusals or people not could be recontacted
  filter(type_endline_interview %in% c(3, 4, 6, 7)) %>% 
  
  select(
    wave,
    hhid,
    balancedsample,
    round,
    meeting_id,
    know_rdv,

    # 1a
    armedrob_num,
    simpleassault_num = assault_num, # Note: deviation from the mpap
    other_any_violent,
    
    burglary_num,
    other_any_nonviolent,
    
    carmedrob_num,
    # caggassault_num, Note: this does not exist in the data
    csimpleassault_num = cassault_num, # Note: deviation from the mpap
    csexual_num,
    cdomestic_phys_num,
    cmurder_num,
    cother_any_violent,
    
    cburglary_num,
    cother_any_nonviolent,
    
    armedrob_any = armedrob_bin,
    assault_any = assault_bin,  # Note: deviation from the mpap
    burglary_any = burglary_bin,
    carmedrob_any = carmedrob_bin,
    # caggassault_any = caggassault_bin,
    cassault_any = cassault_bin,  # Note: deviation from the mpap
    csexual_any = csexual_bin,
    cdomestic_phys_any = cdomestic_phys_bin,
    cmurder_any = cmurder_bin,
    cburglary_any = cburglary_bin,
    
    # 1a (ii)
    # aggassault_num, # Note: deviation from the mpap, not collected
    # aggassault_any, # Note: deviation from the mpap, not collected
    # sexual_any = sexual_bin, # Note: deviation from the mpap, not collected
    sexual_num,
    # domestic_phys_any,# Note: deviation from the mpap, not collected
    # domestic_phys_num,# Note: deviation from the mpap, not collected
    # domestic_verbal_any,# Note: deviation from the mpap, not collected
    # domestic_verbal_num,# Note: deviation from the mpap, not collected
    # land_any,# Note: deviation from the mpap, not collected
    # cmob_num,# Note: deviation from the mpap, not collected
    # cmob_any,# Note: deviation from the mpap, not collected
    # cdomestic_verbal_num,# Note: deviation from the mpap, not collected
    # cland_any,# Note: deviation from the mpap, not collected
    
    # 1b
    fear_violent,
    fear_nonviolent,
    feared_walk,
    
    # 2
    satis_trust,
    satis_general,
    
    # 3b
    # policeabuse_phys_any,# Note: deviation from the mpap, not collected
    # policeabuse_verbal_any,# Note: deviation from the mpap, not collected
    # policeabuse_phys_num,# Note: deviation from the mpap, not collected
    # policeabuse_verbal_num,# Note: deviation from the mpap, not collected
    cpoliceabuse_num,
    cpoliceabuse_any,
    bribe_freq, 
    bribe_amt,
    
    # 4a
    burglaryres,
    dviolres,
    armedrobres,
    
    armedrob_report,
    simpleassault_report = assault_report,
    other_report_violent,
    
    burglary_report,
    other_report_nonviolent,
    
    carmedrob_report,
    # caggassault_report,# Note: deviation from the mpap, not collected
    csimpleassault_report = cassault_report,
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
    give_info_pol_investigation = prev_com_pol,
    
    # 4c
    policebeating_report,
    # policeabuse_verbal_report,# Note: deviation from the mpap, not collected
    # policeabuse_phys_report,# Note: deviation from the mpap, not collected
    policeabuse_report = cpoliceabuse_report, # Note: this is measured at the community level
    dutydrink_report,
    
    # M1a
    polcaseserious,
    polcasefair,
    polint_corrupt,
    polint_quality,
    
    # M1b
    # know_law_suspect, # Note: deviation from the mpap, not collected
    know_law_lawyer,
    know_law_fees,
    know_law_vaw,
    # know_report_followup, # Note: deviation from the mpap, not collected
    know_report_station,
    
    # M1c
    # reportnorm_theft, # Note: deviation from the mpap, not collected
    # reportnorm_abuse, # Note: deviation from the mpap, not collected
    obeynorm,
    
    # M2a
    polcap_timely,
    polcap_investigate,
    
    # M2b
    responsive_act,
    
    # S1
    legit_trust,
    
    # S2
    trust_community,
    
    # C
    compliance_patrol,
    # compliance_freq, # Note: deviation from the mpap, not collected
    compliance_meeting, 
    compliance_attend = compliance_meeting_present,
    
    
    # survey-based treatment received variable
    groupformed_attend = compliance_meeting_rdv
    
  ) %>% 
  mutate(
    compliance_attend = case_when(
      compliance_attend > 0 ~ 1L, 
      compliance_attend == 0 ~ 0L))

bra_citizen_baseline <- 
  bra_citizen %>% 
  filter(wave == 0) %>% 
  select(
    hhid,
    meeting_id,
    
    # 1a
    armedrob_num,
    simpleassault_num = assault_num, # Note: deviation from the mpap
    other_any_violent,
    
    burglary_num,
    other_any_nonviolent,
    
    carmedrob_num,
    # caggassault_num, Note: this does not exist in the data
    csimpleassault_num = cassault_num, # Note: deviation from the mpap
    csexual_num,
    cdomestic_phys_num,
    cmurder_num,
    cother_any_violent,
    
    cburglary_num,
    cother_any_nonviolent,
    
    armedrob_any = armedrob_bin,
    assault_any = assault_bin,  # Note: deviation from the mpap
    burglary_any = burglary_bin,
    carmedrob_any = carmedrob_bin,
    # caggassault_any = caggassault_bin,
    cassault_any = cassault_bin,  # Note: deviation from the mpap
    csexual_any = csexual_bin,
    cdomestic_phys_any = cdomestic_phys_bin,
    cmurder_any = cmurder_bin,
    cburglary_any = cburglary_bin,
    
    # 1b
    fear_violent,
    fear_nonviolent,
    feared_walk,
    
    # 2
    satis_trust,
    satis_general,
    
    # 3b
    # policeabuse_phys_any, # Note: deviation from the mpap, not collected
    # policeabuse_verbal_any, # Note: deviation from the mpap, not collected
    # policeabuse_phys_num, # Note: deviation from the mpap, not collected
    # policeabuse_verbal_num, # Note: deviation from the mpap, not collected
    cpoliceabuse_num,
    cpoliceabuse_any,
    bribe_freq, 
    bribe_amt,
    
    # 4a
    burglaryres,
    dviolres,
    armedrobres,
    
    armedrob_report,
    simpleassault_report = assault_report,
    other_report_violent,
    
    burglary_report,
    other_report_nonviolent,
    
    carmedrob_report,
    # caggassault_report, # Note: deviation from the mpap, not collected
    csimpleassault_report = cassault_report,
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
    give_info_pol_investigation = prev_com_pol,
    
    # 4c
    policebeating_report,
    # policeabuse_verbal_report,# Note: deviation from the mpap, not collected
    # policeabuse_phys_report,# Note: deviation from the mpap, not collected
    policeabuse_report = cpoliceabuse_report, # Note: this is measured at the community level
    dutydrink_report,
    
    # M1a
    polcaseserious,
    polcasefair,
    polint_corrupt,
    polint_quality,
    
    # M1b
    # know_law_suspect, # Note: deviation from the mpap, not collected
    know_law_lawyer,
    know_law_fees,
    know_law_vaw,
    # know_report_followup, # Note: deviation from the mpap, not collected
    know_report_station,
    
    # M1c
    # reportnorm_theft, # Note: deviation from the mpap, not collected
    # reportnorm_abuse, # Note: deviation from the mpap, not collected
    obeynorm,
    
    # M2a
    polcap_timely,
    polcap_investigate,
    
    # M2b
    responsive_act,
    
    # S1
    legit_trust,
    
    # S2
    trust_community,
    
    # C
    compliance_patrol,
    # compliance_freq, # Note: deviation from the mpap, not collected
    compliance_meeting,
    compliance_meeting_present,
    
    sexual_num
    ) %>% 
  rename_at(vars(everything(), -meeting_id, -hhid), ~paste0(., "_baseline"))

saveRDS(bra_citizen_baseline,  file = "data/out/bra-citizen-baseline-clean.RDS")
saveRDS(bra_citizen_endline,  file = "data/out/bra-citizen-endline-clean.RDS")

# reshape for analysis 
bra_citizen <- 
  bra_citizen_endline %>% mutate(in_endline = 1) %>% 
  left_join(bra_citizen_baseline %>% select(-meeting_id) %>% mutate(in_baseline = 1), by = "hhid") %>% mutate(in_survey = 1) %>%
  left_join(bra_units %>% mutate(in_units = 1), by = c("meeting_id")) %>% 
  mutate(across(c(everything(), -wave, -hhid, -balancedsample, -round, -meeting_id), na_if, 97)) %>% 
  mutate(across(c(everything(), -wave, -hhid, -balancedsample, -round, -meeting_id), na_if, 98)) %>% 
  # drop the surveys that were accidentally conducted in areas not selected for surveys
  filter(survey_mun_selected == 1 & survey_pt_selected == 1) %>%
  # drop interviews conducted in areas of Florianopolis not included in the study design
  filter(municipality != "Florianopolis (x2)") %>% 
  # drop replacement points, which were selected posttreatment
  filter(replacement_point != TRUE) 

saveRDS(bra_citizen,  file = "data/out/bra-citizen-clean.RDS")
