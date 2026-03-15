#--------------------------------------------------------------------------------------------------------------
                      # all loaded packages come here (resolve warnings in the file)
#--------------------------------------------------------------------------------------------------------------
suppressMessages({
  library(tidyverse)
  library(haven)
})

#--------------------------------------------------------------------------------------------------------------
                      # read baseline/endline data
#--------------------------------------------------------------------------------------------------------------
lbr_citizen_baseline <- read_rds("data/in/liberia/05_Raw De-Indentified Data/LIB_MK4_raw_data_baseline_deidentified.rds")
lbr_citizen_endline <- read_delim("data/in/liberia/05_Raw De-Indentified Data/LIB_MK4_raw_data_endline.csv", "\t", escape_double = FALSE, trim_ws = TRUE)
lbr_unit <- readRDS("data/out/lbr-unit-clean.RDS")

#--------------------------------------------------------------------------------------------------------------
                      # baseline: clean
#--------------------------------------------------------------------------------------------------------------
lbr_citizen_baseline <- 
  lbr_citizen_baseline %>%
  mutate(
    other_any_violent = if_else(respid %in% c("10051404", "20041601", "20051402", "20051404", "20161404", "30001403", "30211401", "40191401", "70021403", "90272201", "100021401", "100022204", "100141401", "100201601", "100201604"), 1, 0),
    other_any_nonviolent = if_else(other_any_violent == 1, 0, other_any),
    other_report_violent = if_else(respid %in% c("10051404", "20041601", "20051402", "20051404", "20161404", "30001403", "30211401", "40191401", "70021403", "90272201", "100021401", "100022204", "100141401", "100201601", "100201604"), 1, 0),
    other_report_nonviolent  = if_else(other_report_violent == 1, 0, 1),
    cother_any_violent = if_else(respid %in% c("10011502", "10081202", "10111904", "10141703", "10221201", "10221202", "10221203", "10221204", "10221205", "10221601", "10221602", "10221603", "10221604", "10221605", "20011701", "20011703", "20011704", "20041602", "20042303", "20042304", "20052201", "20052204", "20161403", 20232205, 20251203, 20271702, 20271703, 20281202, 20282301, 20321704, 20372301, 20401701, 30001303, 30001304, 30021704, 30021705, 30051701, 30051702, 30051705, 30092302, 30121204, 30121502, 30241702, 30242102, 30271802, 40052001, 40052101, 40141902, 40191405, 40242304, 40271305, 50051905, 50131302, 50131303, 50131305, 50131403, 50131901, 60032202, 60111202, 60111503, 60111505, 60112301, 70021405, 70032302, 70051902, 70111301, 70111304, 70111903, 80062004, 80151202, 90271304, 90281702, 100021302, 100022203, 100041702, 100141301, 100141304, 100141405, 100161703, 100201603, 100201605, 100341701), 1, 0),
    cother_any_nonviolent = if_else(cother_any_violent == 1, 0, 1),
    cother_report_violent = if_else(respid %in% c(10011502, 10081202, 10111904, 10141703, 10221201, 10221202, 10221203, 10221204, 10221205, 10221601, 10221602, 10221603, 10221604, 10221605, 20011701, 20011703, 20011704, 20041602, 20042303, 20042304, 20052201, 20052204, 20161403, 20232205, 20251203, 20271702, 20271703, 20281202, 20282301, 20321704, 20372301, 20401701, 30001303, 30001304, 30021704, 30021705, 30051701, 30051702, 30051705, 30092302, 30121204, 30121502, 30241702, 30242102, 30271802, 40052001, 40052101, 40141902, 40191405, 40242304, 40271305, 50051905, 50131302, 50131303, 50131305, 50131403, 50131901, 60032202, 60111202, 60111503, 60111505, 60112301, 70021405, 70032302, 70051902, 70111301, 70111304, 70111903, 80062004, 80151202, 90271304, 90281702, 100021302, 100022203, 100041702, 100141301, 100141304, 100141405, 100161703, 100201603, 100201605, 100341701), 1, 0),
    cother_report_nonviolent = if_else(cother_any_nonviolent == 1, 0, 1),
# recode numeric crimes to be == 0 if crime_any == 0 (this is done in the cleaning file for lbr because we summarise lbr baseline at the community level of baseline before joining with endline)
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
      carmedrob_any == 0 ~ "0", 
      is.na(carmedrob_any) ~ NA_character_, 
      TRUE ~ as.character(carmedrob_num)),
    caggassault_num = case_when(
      caggassault_any == 0 ~ "0", 
      is.na(caggassault_any) ~ NA_character_,
      TRUE ~ as.character(caggassault_num)),
    sexual_num = case_when(
      sexual_any == 0 ~ 0L, 
      is.na(sexual_any) ~ NA_integer_, 
      TRUE ~ as.integer(sexual_num)),
    csimpleassault_num = case_when(
      csimpleassault_any == 0 ~ "0", 
      is.na(csimpleassault_any) ~ NA_character_,
      TRUE ~ as.character(csimpleassault_num)),
    csexual_num = case_when(
      csexual_any == 0 ~ "0", 
      is.na(csexual_any) ~ NA_character_, 
      TRUE ~ as.character(csexual_num)),
    cdomestic_phys_num = case_when(
      cdomestic_phys_any == 0 ~ "0", 
      is.na(cdomestic_phys_any) ~ NA_character_, 
      TRUE ~ as.character(cdomestic_phys_num)),
    cmurder_num = case_when(
      cmurder_any == "N" ~ "0", 
      is.na(cmurder_any) ~ NA_character_, 
      TRUE ~ as.character(cmurder_num)),
    cburglary_num = case_when(
      cburglary_any == 0 ~ "0", 
      is.na(cburglary_any) ~ NA_character_, 
      TRUE ~ as.character(cburglary_num)),
    other_any_violent = case_when(
      other_any == 0 ~ 0L, 
      is.na(other_any) ~ NA_integer_,
      TRUE ~ as.integer(other_any_violent)),
    other_any_nonviolent = case_when(
      other_any == 0 ~ 0L, 
      is.na(other_any) ~ NA_integer_, 
      TRUE ~ as.integer(other_any_nonviolent)),
    cother_any_violent = case_when(
      cother_any == 0 ~ 0L, 
      is.na(cother_any) ~ NA_integer_, 
      TRUE ~ as.integer(cother_any_violent)),
    cother_any_nonviolent = case_when(
      cother_any == 0 ~ 0L, 
      is.na(cother_any) ~ NA_integer_, 
      TRUE ~ as.integer(cother_any_nonviolent))) %>% 
  # keep only the variables we need
  select(
    # unit level vars
    communities = towncode,
    # vars needed for indices
    armedrob_num,
    burglary_num,
    simpleassault_num,
    other_any_violent,
    other_any_nonviolent,

    aggassault_num,
    sexual_num,
    domestic_phys_num,
    domestic_verbal_num,
    land_any,

    carmedrob_num,
    cburglary_num,
    caggassault_num,
    csimpleassault_num,
    csexual_num,
    cdomestic_phys_num,
    cmurder_num,
    cland_any,
    cdomestic_verbal_any,
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

    carmedrob_report,
    cburglary_report,
    caggassault_report,
    csimpleassault_report,
    csexual_report,
    cdomestic_phys_report,
    # cmurder_report, # not collected for liberia
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
    polint_corrupt,
    polint_quality,

    know_law_suspect,
    know_law_lawyer,
    know_law_fees,
    # know_law_vaw, # this is missing because according to the mpap this was only collected at the endline in Colombia
    # know_report_followup, # no functioning hotline in Colombia; please refer to Liberia PAP Appendix page 3, footnote 11. 
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

    trust_community,
    
    cdomestic_verbal_num) %>% 
  rename_at(vars(-communities), ~paste0(., "_baseline")) %>% 
  mutate(
    know_law_fees_rescaled_baseline = case_when(
        know_law_fees_baseline == 0 ~ 1L,
        know_law_fees_baseline == 1 ~ 0L,
        TRUE ~ as.integer(know_law_fees_baseline)),
    know_law_suspect_rescaled_baseline = case_when(
        know_law_suspect_baseline == 0 ~ 1L,
        know_law_suspect_baseline == 1 ~ 0L,
        TRUE ~ as.integer(know_law_suspect_baseline))
  )

lbr_citizen_baseline <-
  lbr_citizen_baseline %>%
  mutate_at(
    .vars = vars(cmurder_num_baseline,
            carmedrob_num_baseline,
            csimpleassault_num_baseline,
            caggassault_num_baseline,
            csexual_num_baseline,
            cdomestic_phys_num_baseline,
            cburglary_num_baseline,
            cdomestic_verbal_num_baseline),
      ~case_when(
        . == "0" ~ 0,
        . == "1-Once" ~ 1,
        . == "2-Two to three times" ~ 2.5,
        . == "3-Four to five times" ~ 4.5,
        . == "4-Six to ten times" ~ 8,
        . == "5-More than ten times" ~ 10,
        . == "97-Do not know" ~ NA_real_)) %>% 
  mutate_at(
    .vars = vars(fear_violent_baseline,
                policeabuse_verbal_report_baseline,
                policeabuse_phys_report_baseline,
                fear_nonviolent_baseline,
                feared_walk_baseline,
                satis_trust_baseline,
                satis_general_baseline,
                bribe_freq_baseline,
                armedrob_report_baseline,
                burglary_report_baseline,
                simpleassault_report_baseline,
                other_report_baseline,
                carmedrob_report_baseline,
                cburglary_report_baseline,
                caggassault_report_baseline,
                csimpleassault_report_baseline,
                csexual_report_baseline,
                cdomestic_phys_report_baseline,
                cother_report_baseline,
                burglaryres_baseline,
                dviolres_baseline,
                armedrobres_baseline,
                dutydrink_report_baseline,
                policebeating_report_baseline,
                polcaseserious_baseline,
                polcasefair_baseline,
                polint_corrupt_baseline,
                polint_quality_baseline,
                know_law_suspect_baseline,
                know_law_lawyer_baseline,
                know_law_fees_baseline,
                reportnorm_theft_baseline,
                reportnorm_abuse_baseline,
                obeynorm_baseline,
                polcap_timely_baseline,
                polcap_investigate_baseline,
                compliance_patrol_baseline,
                compliance_freq_baseline,
                responsive_act_baseline,
                legit_trust_baseline,
                trust_community_baseline,
                cdomestic_verbal_num_baseline),
                ~as.numeric(str_extract(., "[[:digit:]]+"))) %>% 
  mutate_at(
    .vars = vars(cdomestic_verbal_any_baseline,
                 policeabuse_verbal_any_baseline,
                 policeabuse_phys_any_baseline,
                 contact_pol_susp_activity_baseline,
                 give_info_pol_investigation_baseline,
                 know_report_station_baseline,
                 compliance_meeting_baseline),
    ~case_when(
        . == "Y" ~ 1, 
        . == "N" ~ 0)) %>% 
  # replace don't know, refuse to answer or no answer in the survey
  mutate(across(fear_violent_baseline:know_law_suspect_rescaled_baseline, na_if, 99)) %>% 
  mutate(across(fear_violent_baseline:know_law_suspect_rescaled_baseline, na_if, 98)) %>% 
  mutate(across(fear_violent_baseline:know_law_suspect_rescaled_baseline, na_if, 97)) %>% 
  mutate(across(fear_violent_baseline:know_law_suspect_rescaled_baseline, na_if, 88)) %>% 
  # recode compliance numbers
  mutate(compliance_patrol_rescaled_baseline = (6L - compliance_patrol_baseline),
         compliance_freq_rescaled_baseline = (6L - compliance_freq_baseline),
         compliance_meeting_rescaled_baseline = (compliance_meeting_baseline)) %>% 
  left_join(lbr_unit %>% select(Z, communities), by = "communities") 


# collapse to the community mean
lbr_citizen_baseline_summarized <- 
  lbr_citizen_baseline %>% 
  select(-Z) %>% 
  group_by(communities) %>% 
  summarise_all(mean, na.rm = TRUE) %>% 
  ungroup 

saveRDS(lbr_citizen_baseline,  file = "data/out/lbr-citizen-baseline-clean.RDS")

#--------------------------------------------------------------------------------------------------------------
                    # endline: keep relevant variables
#--------------------------------------------------------------------------------------------------------------

lbr_citizen_endline <- 
  lbr_citizen_endline %>%
  # generate other crimes (following sutdy teams' code)
  mutate(
    # mutate other_* crimes
    other_any = crime_any_10,
    cother_any = ccrime_any_12,
    other_report = crime_report_10,
    cother_report = ccrime_num_12,
    
    other_any_violent = if_else(respid %in% c(5000253, 9020145, 4014281, 4027161, 6004232, 1008281, 1002313, 1004215, 1007233, 2027273, 3014302), 1, 0),
    other_any_nonviolent = if_else(other_any_violent==1, 0, other_any),
    cother_any_violent = if_else(respid %in% c(5013302, 9028282, 5010151, 4024294, 9999202, 1002285, 1004164, 6666291, 2005315, 2037303, 3024162, 3019236), 1, 0),
    cother_any_nonviolent = if_else(cother_any_violent == 1, 0, cother_any),
    other_report_violent = if_else(other_report == 1 & other_any_violent == 1, 1, 0),
    cother_report_violent = if_else(cother_report == 1 & cother_any_violent == 1, 1, 0),
    other_report_nonviolent = if_else(other_report == 1 & other_any_nonviolent == 1, 1, 0),
    cother_report_nonviolent = if_else(other_report == 1 & cother_any_nonviolent == 1, 1, 0)
  ) %>% 
  filter(consent == 1) %>%  # drop people who do not give consent
  mutate( # This code follows Lbr code cleaning (LIB_MK4_05_cleaning_code_endline_survey.do)
    towncode = ifelse(towncode == 5555 | towncode == 6666 | towncode == 7777 | towncode == 8888 | towncode == 9999, community, towncode),
    towncode = ifelse(towncode == 2002 | towncode == 2017, community, towncode),
    towncode = ifelse(respid == 7006202 & community == 7006, 7006, towncode),
    towncode = ifelse(respid == 9029202 & community == 9029, 9029, towncode),
    towncode = ifelse(respid == 5005313 & community == 5005, 5005, towncode),
    towncode = ifelse(respid == 5005314 & community == 5005, 5005, towncode),
    towncode = ifelse(respid == 5015135 & community == 5015, 5015, towncode),
    towncode = if_else(respid == 2032313 & community == 2032, 2032, towncode),
    towncode = if_else(towncode == 30022, 3022, towncode),
    towncode = if_else(respid == 3020214 & community == 3028, 3028, towncode),
    towncode = if_else(respid == 9010281, 8005, towncode),
    towncode = if_else(respid == 9010282, 8005, towncode),
    towncode = if_else(respid == 9010283, 8005, towncode),
    towncode = if_else(respid == 9010284, 8005, towncode),
    towncode = if_else(respid == 9010285, 8005, towncode),
    towncode = if_else(respid == 7777195, 1002, towncode),
    towncode = if_else(respid == 7777193, 1002, towncode),
    towncode = if_else(respid == 777192, 1002, towncode),
    towncode = if_else(respid == 8888195, 1001, towncode),
    towncode = if_else(respid == 8888194, 1001, towncode),
    towncode = if_else(respid == 7777194, 1002, towncode),
    towncode = if_else(respid == 8888193, 1001, towncode),
    towncode = if_else(respid == 8888192, 1001, towncode),
    towncode = if_else(respid == 3028299, 3024, towncode),
    towncode = if_else(respid == 3028255, 3024, towncode),
    towncode = if_else(respid == 2025155, 2025, towncode),
    towncode = if_else(respid == 8888191, 1001, towncode),
    towncode = if_else(respid == 3021311, 3021, towncode),
    
    respid = ifelse(respid == 7002202 & community == 7006, 7006202, respid),
    respid = ifelse(respid == 9020202 & community == 9029, 9029202, respid),
    respid = ifelse(respid == 8005313 & community == 5005, 5005313, respid),
    respid = ifelse(respid == 8005314 & community == 5005, 5005314, respid),
    respid = ifelse(respid == 8015135 & community == 5015, 5015135, respid),
    respid = if_else(respid == 2004201 & starttime == "Feb 12, 2019 5:03:57 AM", 2004299, respid),
    respid = if_else(respid == 3005315 & starttime == "Feb 16, 2019 11:22:52 AM", 3005399, respid),
    respid = if_else(respid == 3010253 & starttime == "Feb 18, 2019 6:36:58 AM", 3010299, respid),
    respid = if_else(respid == 3028254 & starttime == "Feb 14, 2019 8:07:08 AM", 3028299, respid),
    respid = if_else(respid == 3028255 & starttime == "Feb 15, 2019 9:29:36 AM", 3028298, respid),
    respid = if_else(respid == 6000204 & starttime == "Feb 4, 2019 8:31:00 AM", 6000299, respid),
    respid = if_else(respid == 6666291 & starttime == "Feb 8, 2019 4:36:07 AM", 6666299, respid),
    respid = if_else(respid == 8005134 & starttime == "Jan 19, 2019 8:16:35 AM", 8005199, respid),
    respid = if_else(respid == 8010302 & starttime == "Jan 18, 2019 5:30:05 AM", 8010399, respid),
    respid = if_else(respid == 8010303 & starttime == "Jan 18, 2019 6:00:26 AM", 8010398, respid),
    respid = if_else(respid == 8017181 & starttime == "Jan 17, 2019 3:32:56 AM", 8017199, respid),
    respid = if_else(respid == 8017301 & starttime == "Jan 17, 2019 3:50:10 AM", 8017399, respid),
    respid = if_else(respid == 9021291 & starttime == "Jan 23, 2019 6:35:25 AM", 9021299, respid),
    respid = if_else(respid == 2027155 & community == 2025, 2025155, respid),
    respid = if_else(respid == 9010281, 8005281, respid),
    respid = if_else(respid == 9010282, 8005282, respid), 
    respid = if_else(respid == 9010283, 8005283, respid), 
    respid = if_else(respid == 9010284, 8005284, respid), 
    respid = if_else(respid == 9010285, 8005285, respid), 
    
    community = if_else(respid == 8010304, 8017, community),
    community = if_else(respid == 9006173, 9006, community),
    community = if_else(respid == 9028292, 9028, community),
    community = if_else(respid == 9028182, 9028, community),
    community = if_else(respid == 9028132, 9028, community),
    community = if_else(respid == 9028138, 9028, community),
    community = if_else(respid == 8010304, 8010, community),
    community = if_else(respid == 5005135, 5005, community),
    community = if_else(respid == 3010175, 3010, community),
    community = if_else(respid == 8888191, 10014, community),
    community = if_else(respid == 8888192, 10014, community),
    community = if_else(respid == 8888193, 10014, community),
    community = if_else(respid == 8888194, 10014, community),
    community = if_else(respid == 8888195, 10014, community),
    community = if_else(respid == 777192, 10020, community),
    community = if_else(respid == 7777194, 10020, community),
    community = if_else(respid == 7777193, 10020, community),
    community = if_else(respid == 7777195, 10020, community),
    community = if_else(respid == 3009181, 3009, community),
    community = if_else(respid == 9010281, 8005, community),
    community = if_else(respid == 9010282, 8005, community),
    community = if_else(respid == 9010283, 8005, community),
    community = if_else(respid == 9010284, 8005, community),
    community = if_else(respid == 9010285, 8005, community)) %>%
  filter(respid != 2027155 & starttime !="Feb 14, 2019 8:40:48 AM") %>% 
  mutate(
    armedrob_num = crime_num_1,
    burglary_num = crime_num_2,
    aggassault_num = crime_num_3,
    simpleassault_num = crime_num_4,
    sexual_num = crime_num_5,
    domestic_phys_num = crime_num_6,
    domestic_verbal_num = crime_num_7,
    land_viol_num = crime_num_8,
    land_nviol_num = crime_num_9,
    other_num = crime_num_10,
    carmedrob_num = ccrime_num_1,
    cburglary_num = ccrime_num_2,
    caggassault_num = ccrime_num_3,
    csimpleassault_num = ccrime_num_4,
    csexual_num = ccrime_num_5,
    cdomestic_phys_num = ccrime_num_6,
    cdomestic_verbal_num = ccrime_num_7,
    cland_viol_num = ccrime_num_8,
    cland_nviol_num = ccrime_num_9,
    cmurder_num = ccrime_num_10,
    cchildabuse_num = ccrime_num_11,
    cother_num = ccrime_num_12,
    
    armedrob_any = crime_any_1,
    burglary_any = crime_any_2,
    aggassault_any = crime_any_3,
    simpleassault_any = crime_any_4,
    sexual_any = crime_any_5,
    domestic_phys_any = crime_any_6,
    domestic_verbal_any = crime_any_7,
    land_viol_any = crime_any_8,
    land_nviol_any = crime_any_9,
    other_any = crime_any_10,
    carmedrob_any = ccrime_any_1,
    cburglary_any = ccrime_any_2,
    caggassault_any = ccrime_any_3,
    csimpleassault_any = ccrime_any_4,
    csexual_any = ccrime_any_5,
    cdomestic_phys_any = ccrime_any_6,
    cdomestic_verbal_any = ccrime_any_7,
    cland_viol_any = ccrime_any_8,
    cland_nviol_any = ccrime_any_9,
    cmurder_any = ccrime_any_10,
    cchildabuse_any = ccrime_any_11,
    cother_any = ccrime_any_12,
    
    armedrob_report = crime_report_1,
    burglary_report = crime_report_2,
    aggassault_report = crime_report_3,
    simpleassault_report = crime_report_4,
    sexual_report = crime_report_5,
    domestic_phys_report = crime_report_6,
    domestic_verbal_report = crime_report_7,
    land_viol_report = crime_report_8,
    land_nviol_report = crime_report_9,
    other_report = crime_report_10,
    carmedrob_report = ccrime_report_1,
    cburglary_report = ccrime_report_2,
    caggassault_report = ccrime_report_3,
    csimpleassault_report = ccrime_report_4,
    csexual_report = ccrime_report_5,
    cdomestic_phys_report = ccrime_report_6,
    cdomestic_verbal_report = ccrime_report_7,
    cland_viol_report = ccrime_report_8,
    cland_nviol_report = ccrime_report_9,
    cmurder_report = ccrime_report_10,
    cchildabuse_report = ccrime_report_11,
    cother_report = ccrime_num_12) %>%
  mutate(csexual_num = case_when(csexual_any == 0 ~ 0, TRUE ~ as.numeric(csexual_num))) %>% 
  mutate(
    armedrob_num = case_when(
      armedrob_any == 0 ~ 0L,
      is.na(armedrob_any) ~ NA_integer_, 
      TRUE ~ as.integer(armedrob_num)),
    simpleassault_num = case_when(
      simpleassault_any == 0 ~ 0L, 
      is.na(simpleassault_any) ~ NA_integer_,
      TRUE ~ as.integer(simpleassault_num)),
    sexual_num = case_when(
      sexual_any == 0 ~ 0L, 
      is.na(sexual_any) ~ NA_integer_, 
      TRUE ~ as.integer(sexual_num)),
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
      TRUE ~ as.integer(other_any_violent)),
    other_any_nonviolent = case_when(
      other_any == 0 ~ 0L, 
      is.na(other_any) ~ NA_integer_, 
      TRUE ~ as.integer(other_any_nonviolent)),
    cother_any_violent = case_when(
      cother_any == 0 ~ 0L, 
      is.na(cother_any) ~ NA_integer_, 
      TRUE ~ as.integer(cother_any_violent)),
    cother_any_nonviolent = case_when(
      cother_any == 0 ~ 0L, 
      is.na(cother_any) ~ NA_integer_, 
      TRUE ~ as.integer(cother_any_nonviolent))) %>% 
  select(
    communities = towncode, 
    respid,
    
    armedrob_num,
    burglary_num,
    simpleassault_num,
    other_any_nonviolent,
    other_any_violent,
    
    aggassault_num,
    sexual_num,
    domestic_phys_num,
    domestic_verbal_num,
    land_viol_num, # land_any for lbr is measured as a part of two violent and non-violent indices
    land_nviol_num, # land_any for lbr is measured as a part of two violent and non-violent indices
    carmedrob_num,
    cburglary_num,
    caggassault_num,
    csimpleassault_num,
    csexual_num,
    cdomestic_phys_num,
    cmurder_num,
    cland_viol_num,# cland_any for lbr is measured as a part of two violent and non-violent indices
    cland_nviol_num,  # cland_any for lbr is measured as a part of two violent and non-violent indices  
    cdomestic_verbal_num,
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
    polint_corrupt,
    polint_quality,
    
    know_law_suspect,
    know_law_lawyer,
    know_law_fees,
    # know_law_vaw,
    # know_report_followup,
    know_report_station = know_polstation,
    
    reportnorm_theft,
    reportnorm_abuse,
    obeynorm,
    
    polcap_timely,
    polcap_investigate,
    
    compliance_patrol,
    compliance_freq,
    compliance_meeting,
    compliance_attend = polcommmeetattend6m,
    
    responsive_act,
    legit_trust,
    
    trust_community,
    
    cdomestic_verbal_num)

saveRDS(lbr_citizen_endline,  file = "data/out/lbr-citizen-endline-clean.RDS")

#--------------------------------------------------------------------------------------------------------------
                          # join baseline and endline data
#--------------------------------------------------------------------------------------------------------------

lbr_citizen <-
  lbr_citizen_baseline_summarized %>%
  inner_join(lbr_citizen_endline, by = "communities") %>% 
  left_join(lbr_unit, by = "communities") %>% 
  # 1. Convert the range to numeric first to avoid type mismatch
  mutate(across(fear_violent:trust_community, as.numeric)) %>% 
  # 2. Now replace the codes with NA
  mutate(across(fear_violent:trust_community, ~na_if(.x, 99) %>% 
                  na_if(98) %>% 
                  na_if(97) %>% 
                  na_if(88)))

#--------------------------------------------------------------------------------------------------------------
# changes to data suggested by the team # refer to issues number 78 on github 
#--------------------------------------------------------------------------------------------------------------

lbr_citizen <-
  lbr_citizen %>% 
  filter(police_zones != "ZONE6") 

saveRDS(lbr_citizen,  file = "data/out/lbr-citizen-clean.RDS")
