#--------------------------------------------------------------------------------------------------------------
# all loaded packages come here (resolve warnings in the file)
#--------------------------------------------------------------------------------------------------------------
suppressMessages({
  library(tidyverse)
  library(haven)
})

#----------------------------------------------------------------------------------------------
                          # 1. get all raw data
#----------------------------------------------------------------------------------------------
# endline raw data
phl_citizen_endline <- read.csv("data/in/philippines/05_Raw De-Indentified Data/Endline_Raw.csv")
phl_citizen_midline <- read.csv("data/in/philippines/05_Raw De-Indentified Data/Midline_Raw.csv")

phl_citizen_midline <- read_dta("data/in/philippines/07_Processed Data/PHI_MK4_CitizenMidline2.dta")
phl_citizen_endline <- read_dta("data/in/philippines/07_Processed Data/PHI_MK4_CitizenEndline2.dta")

# randomization
phl_unit <- readRDS("data/out/phl-unit-clean.RDS")

#----------------------------------------------------------------------------------------------
# 2. clean endline data
#----------------------------------------------------------------------------------------------
phl_citizen_endline <-
  phl_citizen_endline %>%
  mutate(
    armedrob_num = vicnum_robbery,
    burglary_num = vicnum_theft,
    simpleassault_num = vicnum_assault,
    other_any = vic_other,
    carmedrob_num = comvnum_robbery,
    cburglary_num = comvnum_theft,
    caggassault_num = comvnum_vassault,
    csimpleassault_num = comvnum_assault,
    csexual_num = comvnum_sexab,
    cdomestic_phys_num = comvnum_physab,
    cmurder_num = comvnum_murder,
    
    armedrob_any = vic_robbery,
    burglary_any = vic_theft,
    simpleassault_any = vic_assault,
    other_any = vic_other,
    carmedrob_any = comv_robbery,
    cburglary_any = comv_theft,
    caggassault_any = comv_vassault,
    csimpleassault_any = comv_assault,
    csexual_any = comv_sexab,
    cdomestic_phys_any = comv_physab,
    cmurder_any = comv_murder,
    
    cother_any = case_when(comv_other == "No" ~ 0L,
                           comv_other == "Yes" ~ 1L, 
                           TRUE ~ NA_integer_),
    
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
    # other crimes as adapted from the teams' code
    other_any_violent = case_when(
      vic_othertype == "Sexual Harassment" ~ 1L,
      vic_othertype == "attempted murder"  ~ 1L,
      vic_othertype == "away nag pamilya" ~ 1L,
      vic_othertype == "domestic abuse" ~ 1L,
      vic_othertype == "gang ramble" ~ 1L,
      vic_othertype == "holdap" ~ 1L,
      vic_othertype == "murder" ~ 1L,
      vic_othertype == "panununtok" ~ 1L,
      vic_othertype == "rape" ~ 1L,
      vic_othertype == "sexual harassment" ~ 1L,
      TRUE ~ 0L),
    other_any_nonviolent = if_else(other_any_violent == 1, 0, as.numeric(other_any)),
    cother_any_violent = case_when(
      comv_othertype=="away ng mgkakapitbahay" ~ 1L,
      comv_othertype=="away at saksakan" ~ 1L,
      comv_othertype=="binaril" ~ 1L,
      comv_othertype=="carnapping" ~ 1L,
      comv_othertype=="frustrated murder" ~ 1L,
      comv_othertype=="hit and run" ~ 1L,
      comv_othertype=="humantraficking " ~ 1L,
      comv_othertype=="murder" ~ 1L,
      comv_othertype=="pag patay" ~ 1L,
      comv_othertype=="pananaksak" ~ 1L,
      comv_othertype=="tinaga ang kamay" ~ 1L,
      TRUE ~ 0L),
    cother_any_nonviolent = if_else(cother_any_violent == 1, 0L, cother_any),
    
    policeabuse_phys_any = pnpabuse_phys,
    policeabuse_verbal_any = pnpabuse_verb,
    policeabuse_verbal_num = pnpabuse_verbnum,
    policeabuse_phys_num = pnpabuse_physnum,
    policeabuse_verbal_report = case_when(
      pnpabuse_verbrep_1 == 1 ~ 1L,
      pnpabuse_verbrep_2 == 1 ~ 1L,
      pnpabuse_verbrep_3 == 1 ~ 1L,
      pnpabuse_verbrep_4 == 1 ~ 1L,
      pnpabuse_verbrep_5 == 1 ~ 1L,
      pnpabuse_verbrep_6 == 1 ~ 1L,
      TRUE ~ 0L),
    policeabuse_phys_report = case_when(
      pnpabuse_physrep_1 == 1 ~ 1L,
      pnpabuse_physrep_2 == 1 ~ 1L,
      pnpabuse_physrep_3 == 1 ~ 1L,
      pnpabuse_physrep_4 == 1 ~ 1L,
      pnpabuse_physrep_5 == 1 ~ 1L,
      pnpabuse_physrep_6 == 1 ~ 1L,
      TRUE ~ 0L),
    armedrob_report = if_else(
      vicrep_robbery_1 > 0 & armedrob_num > 0, 1, 0), 
    burglary_report = if_else(
      vicrep_theft_1 > 0 & burglary_num > 0, 1, 0), 
    simpleassault_report = if_else(
      vicrep_assault_1 > 0, 1, 0), 
    other_report = if_else(
      vicrep_other_1 > 0, 1, 0), 
    carmedrob_report = if_else(
      comvrep_robbery_1 > 0, 1, 0), 
    cburglary_report = if_else(
      comvrep_theft_1 > 0, 1, 0), 
    caggassault_report = if_else(
      comvrep_vassault_1 > 0, 1, 0), 
    csimpleassault_report = if_else(
      comvrep_assault_1 > 0, 1, 0), 
    csexual_report = if_else(
      comvrep_sexab_1 > 0, 1, 0), 
    cdomestic_phys_report = if_else(
      comvrep_physab_1 > 0, 1, 0),  
    cmurder_report = if_else(
      comvrep_murder_1 > 0, 1, 0),  
    cother_report = if_else(
      comvrep_other_1 > 0, 1, 0), 
    other_report_violent = case_when(
      other_report == 1 & other_any_violent == 1 ~ 1L, 
      other_report == 0 ~ 0L,
      TRUE ~ NA_integer_),
    cother_report_violent = case_when(
      cother_report == 0 ~ 0L,
      cother_report == 1 & cother_any_violent == 1 ~ 1L,
      TRUE ~ NA_integer_),
    other_report_nonviolent = case_when(
      other_report == 0 ~ 0L, 
      other_report == 1 & other_any_nonviolent == 1 ~ 1L,
      TRUE ~ NA_integer_),
    cother_report_nonviolent = case_when(
      cother_report == 0 ~ 0L,
      cother_report == 1 & cother_any_nonviolent == 1 ~ 1L,
      TRUE ~ NA_integer_),

    contact_pol_susp_activity = contactpol_tip,
    give_info_pol_investigation = contactpol_inves,
    dutydrink_report = report_drink,
    policebeating_report = report_abuse,
    compliance_patrol = comply_patrol,
    compliance_freq = comply_vehicle,
    compliance_meeting = comply_meet,
    endline = 1, midline_surveyed = midline, midline = 0) %>%
  bind_rows(phl_citizen_midline %>% mutate(midline = 1, endline = 0)) %>% 
  filter(endline == 1) %>% # dropping midline responses as per conversation with Dotan GH issue # 104
  select(
    # unit levels vars
      midline, psgc, bgy, lgu, midline_surveyed, endline, sid,
      
      armedrob_num,
      burglary_num,
      simpleassault_num,
      other_any_nonviolent,
      other_any_violent,
      # Note: aggassault_num, sexual_num,  domestic_phys_num, domestic_verbal_num, land_any were not collected
      carmedrob_num,
      cburglary_num,
      caggassault_num,
      csimpleassault_num,
      csexual_num,
      cdomestic_phys_num,
      cmurder_num,
      # Note: cland_any, cdomestic_verbal_any, cmob_num, cdomestic_verbal_num were not collected
      cother_any_nonviolent,
      cother_any_violent,
      
      fear_violent,
      fear_nonviolent,
      feared_walk,
      
      satis_trust,
      satis_general,
      
      policeabuse_verbal_any,
      policeabuse_phys_any,
      policeabuse_verbal_num,
      policeabuse_phys_num,
      
      policeabuse_verbal_report_raw = policeabuse_verbal_report,
      policeabuse_phys_report_raw = policeabuse_phys_report,
      
      bribe_freq,
      bribe_amt,
      
      armedrob_report,
      burglary_report,
      simpleassault_report,
      other_report,
      other_report_violent,
      
      carmedrob_report,
      cburglary_report,
      caggassault_report,
      csimpleassault_report,
      csexual_report,
      cdomestic_phys_report,
      cmurder_report,
      cother_report,
      cother_report_violent,
      
      burglaryres,
      dviolres,
      armedrobres,
      
      contact_pol_susp_activity,
      give_info_pol_investigation,
      
      dutydrink_report,
      policebeating_report,
      polcaseserious,
      polcasefair,
      polint_corrupt_raw = polint_corrupt,
      polint_quality,
      
      knowlaw1,
      knowlaw2,
      knowproc,
      # Note: know_law_suspect, know_law_lawyer, know_law_fees, know_law_vaw, know_report_followup, know_report_station were not collected in phl
    
      reportnorm_theft_raw = reportnorm_theft,
      reportnorm_abuse_raw = reportnorm_abuse,
      obeynorm_raw = obeynorm1,
      
      polcap_timely,
      polcap_investigate = polcap_invest,
      
      compliance_patrol,
      compliance_freq,
      compliance_meeting,
      compliance_attend = os_engaged,
      
      responsive_act,
      legit_trust,
      
      trust_community) %>% 
  mutate(
    compliance_attend = case_when(
      compliance_attend > 0 ~ 1L, 
      compliance_attend == 0 ~ 0L, 
      TRUE ~ as.integer(compliance_attend)))

#--------------------------------------------------------------------------------------------------------------
# merge unit data with survey data
#--------------------------------------------------------------------------------------------------------------
phl_citizen <-
  phl_citizen_endline %>% 
  inner_join(phl_unit, by = "psgc") %>% 
  # the survey questionnaire records all numeric crimes with do not know responses as 97 (the following makes that changes)
  mutate(across(armedrob_num:cmurder_num, na_if, 97)) %>% 
  # in the survey questionnaire all responses with 88 represent "other", 98 represent refuse to answer
  mutate(across(fear_violent:trust_community, na_if, 88)) %>% 
  mutate(across(fear_violent:trust_community, na_if, 98))

saveRDS(phl_citizen,  file = "data/out/phl-citizen-clean.RDS")

