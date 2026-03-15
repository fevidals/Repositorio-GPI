#--------------------------------------------------------------------------------------------------------------
          # 1. all loaded packages come here (resolve warnings in the file)
#--------------------------------------------------------------------------------------------------------------
suppressMessages({
  library(tidyverse)
  library(haven)
})

#--------------------------------------------------------------------------------------------------------------
          # 2. get raw data
#--------------------------------------------------------------------------------------------------------------
pak_citizen_baseline <- read_dta("data/in/pakistan/05_Raw De-Indentified Data/PK_MK4_raw_data_citizens_baseline.dta")
pak_citizen_endline <- read_dta("data/in/pakistan/05_Raw De-Indentified Data/PK_MK4_raw_data_citizens_endline.dta")
pak_randomization <- readRDS("data/out/pak-unit-clean.RDS")

#--------------------------------------------------------------------------------------------------------------
          # 3. baseline: keep relevant variables
#--------------------------------------------------------------------------------------------------------------
pak_citizen_baseline <- 
  pak_citizen_baseline %>%
  mutate(
    other_any_violent =  case_when(
        other_any_type == "Agwah" ~ 1L,
        other_any_type == "Family member ka qatal hoa" ~ 1L,
        other_any_type == "Harasment" ~ 1L,
        other_any_type == "Woman harsment" ~ 1L,
        TRUE ~ 0L),
    cother_any_violent = case_when(
        cother_name == "Murder" ~ 1L,
        TRUE ~ 0L),
    other_any_nonviolent = case_when(
        other_any_type == "Bhains  chori" ~ 1L,
        other_any_type == "Divorse case" ~ 1L,
        other_any_type == "Ghar k afrad ki larrai" ~ 1L,
        other_any_type == "Larrai jhagra" ~ 1L,
        other_any_type == "Mobile snatching" ~ 1L,
        TRUE ~ 0L),
    cother_any_nonviolent = case_when(
        cother_name == "Cow piracy" ~ 1L,
        TRUE ~ 0L),
    other_report_violent = if_else(other_report == "1" & other_any_violent == 1L, 1L, 0L),
    other_report_nonviolent = if_else(other_report == "1" & other_any_nonviolent == 1L, 1L, 0L),
    cother_report_violent = if_else(cother_report == "1" & cother_any_violent == 1L, 1L, 0L),
    cother_report_nonviolent = if_else(cother_report == "1" & cother_any_nonviolent == 1L, 1L, 0L),
    
    armedrob_num = case_when(armedrob_any == 0 ~ 0L, TRUE ~ as.integer(armedrob_num)),
    simpleassault_num = case_when(simpleassault_any == 0 ~ 0L, TRUE ~ as.integer(simpleassault_num)),
    burglary_num = case_when(burglary_any == 0 ~ 0L, TRUE ~ as.integer(burglary_num)),
    carmedrob_num = case_when(carmedrob_any == 0 ~ 0L, TRUE ~ as.integer(carmedrob_num)),
    caggassault_num = case_when(caggassault_any == 0 ~ 0L, TRUE ~ as.integer(caggassault_num)),
    csimpleassault_num = case_when(csimpleassault_any == 0 ~ 0L, TRUE ~ as.integer(csimpleassault_num)),
    csexual_num = case_when(csexual_any == 0 ~ 0L, TRUE ~ as.integer(csexual_num)),
    cdomestic_phys_num = case_when(cdomestic_phys_any == 0 ~ 0L, TRUE ~ as.integer(cdomestic_phys_num)),
    cmurder_num = case_when(cmurder_any == 0 ~ 0L, TRUE ~ as.integer(cmurder_num)),
    cburglary_num = case_when(cburglary_any == 0 ~ 0L, TRUE ~ as.integer(cburglary_num))) %>% 
  select(
    beats = "beat",
    stations = Police_Station,
    citizen_survey_id = id_ahsan,
    
    armedrob_num,
    burglary_num,
    simpleassault_num,
    other_any_nonviolent,
    other_any_violent,
    # Note: cdomestic_verbal_num,  sexual_num,  domestic_phys_num,  domestic_verbal_num, cland_any,  cdomestic_verbal_any were not collected
    land_any,
    carmedrob_num,
    cburglary_num,
    csimpleassault_num,
    csexual_any,
    csexual_num,
    cdomestic_phys_num,
    cmurder_num,
    caggassault_num,
    
    armedrob_any,
    burglary_any,
    simpleassault_any,
    carmedrob_num,
    cburglary_num,
    csimpleassault_num,
    csexual_num,
    csexual_any,
    cdomestic_phys_num,
    cmurder_num,
    cmob_num,
    carmedrob_any,
    cburglary_any,
    csimpleassault_any,
    csexual_any,
    csexual_any,
    cdomestic_phys_any,
    cmurder_any,
    cmob_any,
    
    cmob_num,
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
    policeabuse_verbal_report,
    policeabuse_phys_report,
    bribe_freq,
    bribe_amt,
    armedrob_report,
    burglary_report,
    simpleassault_report,
    other_report,
    other_report_violent,
    other_report_nonviolent,
    
    carmedrob_report = carmedrob_report_1,
    cburglary_report = cburglary_report_1,
    caggassault_report = caggassault_report_1,
    csimpleassault_report = csimpleassault_report_1,
    csexual_report = csexual_report_1,
    cdomestic_phys_report = cdomestic_phys_report_1,
    cmurder_report = cmurder_report_1,
    cother_report = cother_report_1,
    cother_report_violent,
    cother_report_nonviolent,
    burglaryres,
    dviolres,
    armedrobres,
    contact_pol_susp_activity,
    give_info_pol_investigation,
    dutydrink_report,
    policebeating_report,
    polcaseserious,
    polcasefair,
    polint_corrupt = polint_corrupt,
    polint_quality,
    know_law_suspect,
    know_law_lawyer,
    know_law_fees,
    know_law_vaw,
    know_report_followup = know_cjs_act,
    know_report_station,
    reportnorm_theft,
    reportnorm_abuse,
    obeynorm,
    polcap_timely,
    polcap_investigate,
    compliance_patrol,
    compliance_freq,
    compliance_meeting,
    responsive_act,
    legit_trust,
    trust_community) %>%
  mutate(
    armedrob_num = case_when(armedrob_any == 0 ~ 0L, TRUE ~ as.integer(armedrob_num)),
    simpleassault_num = case_when(simpleassault_any == 0 ~ 0L, TRUE ~ as.integer(simpleassault_num)),
    burglary_num = case_when(burglary_any == 0 ~ 0L, TRUE ~ as.integer(burglary_num)),
    carmedrob_num = case_when(carmedrob_any == 0 ~ 0L, TRUE ~ as.integer(carmedrob_num)),
    csimpleassault_num = case_when(csimpleassault_any == 0 ~ 0L, TRUE ~ as.integer(csimpleassault_num)),
    csexual_num = case_when(csexual_any == 0 ~ 0L, TRUE ~ as.integer(csexual_num)),
    cdomestic_phys_num = case_when(cdomestic_phys_any == 0 ~ 0L, TRUE ~ as.integer(cdomestic_phys_num)),
    cmurder_num = case_when(cmurder_any == 0 ~ 0L, TRUE ~ as.integer(cmurder_num)),
    cburglary_num = case_when(cburglary_any == 0 ~ 0L, TRUE ~ as.integer(cburglary_num))) %>% 
  rename_at(vars(everything(), -beats, -citizen_survey_id, -stations), ~paste0(., "_baseline"))

pak_citizen_endline <- 
  pak_citizen_endline %>%
  mutate(
    other_any_nonviolent = case_when(
        other_what == "Drug sales issue      cocain" ~ 1L,
        other_what == "Fake fir" ~ 1L,
        other_what == "Faraad" ~ 1L,
        other_what == "Larae hoi the" ~ 1L,
        other_what == "My brother not a criminal then he is .." ~ 1L,
        other_what == "Sharab peene ka waqia" ~ 1L,
        other_what == "dispute" ~ 1L,
        other_what == "mobile and purse snatching" ~ 1L,
        other_what == "pick pocket" ~ 1L,
        other_what == "pocket pick from animal market" ~ 1L,
        other_what == "street fight" ~ 1L,
        other_what == "water dispute" ~ 1L,
        TRUE ~ 0L),
    
    other_any_violent = case_when(
        other_what == "Accident" ~ 1L,
        other_what == "fight dispute" ~ 1L,
        TRUE ~ 0L),
    
    cother_any_violent = case_when(
        cother_exp == "sexual harrasment" ~ 1L,
        cother_exp == "Tushahdad" ~ 1L,
        cother_exp == "DHQ Hospital m Qatal howy" ~ 1L,
        cother_exp == "Murder" ~ 1L,
        TRUE ~ 0L),
    
    cother_any_nonviolent = case_when(
        cother_exp == "Palat per qabza" ~ 1L,
        cother_exp == "Plot" ~ 1L,
        cother_exp == "Political influance" ~ 1L,
        cother_exp == "Robberyin shop" ~ 1L,
        cother_exp == "Accident" ~ 1L,
        cother_exp == "Was not crime" ~ 1L,
        cother_exp == "a milk sales man arrecsted due to mix.." ~ 1L,
        cother_exp == "faraad" ~ 1L,
        cother_exp == "fraud" ~ 1L,
        cother_exp == "fraud property" ~ 1L,
        cother_exp == "illegal money transfer from account" ~ 1L,
        cother_exp == "land dispute" ~ 1L,
        cother_exp == "purse snatching" ~ 1L,
        TRUE ~ 0L),
    
    other_report_violent = if_else(other_report == "1" & other_any_violent == 1L, 1L, 0L),
    other_report_nonviolent = if_else(other_report == "1" & other_any_nonviolent == 1L, 1L, 0L),
    cother_report_violent = if_else(cother_report == "1" & cother_any_violent == 1L, 1L, 0L),
    cother_report_nonviolent = if_else(cother_report == "1" & cother_any_nonviolent == 1L, 1L, 0L),

    armedrob_num = case_when(armedrob_any == 0 ~ 0L, TRUE ~ as.integer(armedrob_num)),
    simpleassault_num = case_when(simpleassault_any == 0 ~ 0L, TRUE ~ as.integer(simpleassault_num)),
    burglary_num = case_when(burglary_any == 0 ~ 0L, TRUE ~ as.integer(burglary_num)),
    carmedrob_num = case_when(carmedrob_any == 0 ~ 0L, TRUE ~ as.integer(carmedrob_num)),
    caggassault_num = case_when(caggassault_any == 0 ~ 0L, TRUE ~ as.integer(caggassault_num)),
    csimpleassault_num = case_when(csimpleassault_any == 0 ~ 0L, TRUE ~ as.integer(csimpleassault_num)),
    csexual_num = case_when(csexual_any == 0 ~ 0L, TRUE ~ as.integer(csexual_num)),
    cdomestic_phys_num = case_when(cdomestic_phys_any == 0 ~ 0L, TRUE ~ as.integer(cdomestic_phys_num)),
    cmurder_num = case_when(cmurder_any == 0 ~ 0L, TRUE ~ as.integer(cmurder_num)),
    cburglary_num = case_when(cburglary_any == 0 ~ 0L, TRUE ~ as.integer(cburglary_num))) %>% 
  select(
    beats = "beat",
    citizen_survey_id = id_ahsan,
    
    armedrob_num,
    burglary_num,
    simpleassault_num,
    other_any_nonviolent,
    other_any_violent,
    # Note: aggassault_num, sexual_num, domestic_phys_num, domestic_verbal_num were not collected
    land_any,
    
    armedrob_any,
    burglary_any,
    simpleassault_any,
    carmedrob_num,
    cburglary_num,
    csimpleassault_num,
    csexual_num,
    csexual_any,
    cdomestic_phys_num,
    cmurder_num,
    cmob_num,
    carmedrob_any,
    cburglary_any,
    csimpleassault_any,
    csexual_any,
    csexual_any,
    cdomestic_phys_any,
    cmurder_any,
    cmob_any,

    caggassault_num,
    
    cother_any_nonviolent,
    cother_any_violent,
    # Note: cland_any, cdomestic_verbal_any, cdomestic_verbal_num were not collected
    
    fear_violent,
    fear_nonviolent,
    feared_walk,
    
    satis_trust,
    satis_general,
    
    policeabuse_verbal_any,
    policeabuse_phys_any,
    policeabuse_verbal_num,
    policeabuse_phys_num,
    
    policeabuse_verbal_report,
    policeabuse_phys_report,
    
    bribe_freq,
    bribe_amt,
    
    armedrob_report,
    burglary_report,
    simpleassault_report,
    other_report,
    other_report_violent,
    other_report_nonviolent,
    
    carmedrob_report = carmedrob_report_1,
    cburglary_report = cburglary_report_1,
    caggassault_report = caggassault_report_1,
    csimpleassault_report = csimpleassault_report_1,
    csexual_report = csexual_report_1,
    cdomestic_phys_report = cdomestic_phys_report_1,
    cmurder_report = cmurder_report_1,
    cother_report = cother_report_1,
    cother_report_violent,
    cother_report_nonviolent,
    
    burglaryres,
    dviolres,
    armedrobres,
    
    contact_pol_susp_activity,
    give_info_pol_investigation,
    
    dutydrink_report,
    policebeating_report,
    polcaseserious,
    polcasefair,
    polint_corrupt= polint_corrupt,
    polint_quality,
    
    know_law_suspect,
    know_law_lawyer,
    know_law_fees,
    know_law_vaw,
    know_report_followup = know_law_15fir,
    know_report_station,
    
    reportnorm_theft,
    reportnorm_abuse,
    obeynorm,
    
    polcap_timely,
    polcap_investigate,
    
    compliance_patrol,
    compliance_freq,
    compliance_meeting,
    compliance_attend = chowk_org_meetatt,
    
    responsive_act,
    legit_trust = legit_trust_local,
    
    trust_community) %>% 
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
      TRUE ~ as.integer(cburglary_num)))

saveRDS(pak_citizen_endline,  file = "data/out/pak-citizen-clean-endline.RDS")
saveRDS(pak_citizen_baseline,  file = "data/out/pak-citizen-clean-baseline.RDS")

#--------------------------------------------------------------------------------------------------------------
                          # merge baseline and endline data
#--------------------------------------------------------------------------------------------------------------
# citizen data

pak_citizen <- 
  pak_citizen_endline %>% 
  left_join(pak_citizen_baseline %>% mutate(in_baseline = 1) %>% rename(beats_baseline = beats), by = c("citizen_survey_id")) %>% 
  replace_na(list(in_baseline = 0)) %>% 
  select(-stations) %>% 
  mutate(beats = str_to_title(beats)) %>% 
  inner_join(pak_randomization %>% mutate(beats = str_to_title(beats)), by = c("beats")) %>% 
  
  # 1. Convert everything to numeric first to avoid the "character vs double" error
  mutate(across(c(fear_violent:trust_community, 
                  fear_violent_baseline:trust_community_baseline, 
                  carmedrob_num_baseline:cmob_any_baseline), as.numeric)) %>% 
  
  # 2. Efficiently replace 97 and 98 across all relevant ranges at once
  mutate(across(c(fear_violent:trust_community, 
                  fear_violent_baseline:trust_community_baseline, 
                  carmedrob_num_baseline:cmob_any_baseline), 
                ~na_if(.x, 97) %>% na_if(98))) %>% 
  
  # 3. Specific handling for the 999 code (GH issue #142)
  mutate(armedrob_num = na_if(as.numeric(armedrob_num), 999),
         armedrob_num_baseline = na_if(as.numeric(armedrob_num_baseline), 999))

# save data
saveRDS(pak_citizen,  file = "data/out/pak-citizen-clean.RDS")
