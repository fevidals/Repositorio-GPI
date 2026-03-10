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
col_citizen_baseline <- read.csv("data/in/colombia/05_Raw De-Indentified Data/COL_MK4_raw_data_baseline_citizen.csv")
col_citizen_endline <- read.csv("data/in/colombia/05_Raw De-Indentified Data/COL_MK4_raw_data_endline_citizen.csv")
col_units <- readRDS("data/out/col-unit-clean.RDS")

#--------------------------------------------------------------------------------------------------------------
                  # clean variable names for baseline and endline
#--------------------------------------------------------------------------------------------------------------
var_names <-
  data.frame(stub = gsub("\\..*","", names(col_citizen_baseline))) %>%
  group_by(stub) %>%
  mutate(n = 1:n(), total = n()) %>%
  ungroup() %>%
  mutate(stub2 = if_else(total == 1, as.character(stub), paste0(stub, "_", n)))

names(col_citizen_baseline) <- var_names$stub2

#--------------------------------------------------------------------------------------------------------------
                                  # Endline variables
#--------------------------------------------------------------------------------------------------------------
col_citizen_baseline <-
  col_citizen_baseline %>%
  mutate(
    armedrob_num = if_else(grepl(P16_1, pattern = "1") & P18 == 1, 1, 0),
    burglary_num = if_else((grepl(P16_1, pattern = "1") & P18 != 1) | grepl(P16_6, pattern = "1"), 1, 0),
    simpleassault_num = if_else(grepl(P16_8, pattern = "1"), 1, 0),
    carmedrob_num = if_else(grepl(P16_1, pattern = "2") & P18 == 1, 1, 0),
    cburglary_num = if_else((grepl(P16_1, pattern = "2") & P18 != 1) | grepl(P16_6, pattern = "2"), 1, 0),
    csimpleassault_num = if_else(grepl(P16_8, pattern = "2"), 1, 0),
    csimpleassault_bin = csimpleassault_num,
    csexual_num = if_else(grepl(P16_11, pattern = "2"), 1, 0),
    cdomestic_phys_num = if_else(grepl(P16_10, pattern = "2"), 1, 0),
    cmurder_num = if_else(grepl(P16_9, pattern = "2"), 1, 0),
    fear_violent = if_else(P5 == 999, NA_integer_, P5),
    feared_walk = if_else(P96_1_1_1 == 999, NA_integer_, -P96_1_1_1 + 2L),
    satis_trust = if_else(P6_4 == 999, NA_integer_, P6_4),
    policeabuse_phys_any = if_else(P16_4 != "", 1, 0),
    policeabuse_verbal_any = if_else(P16_5 != "", 1, 0),
    policeabuse_any = if_else((policeabuse_verbal_any + policeabuse_phys_any) > 0, 1, 0),
    policeabuse_phys_num = policeabuse_phys_any,
    policeabuse_verbal_num = policeabuse_verbal_any,
    policeabuse_num = policeabuse_verbal_num + policeabuse_phys_num,
    temp_pvr = grepl(P58_2, pattern = "no s", ignore.case = T),
    policeabuse_verbal_report = if_else(policeabuse_verbal_any > 0 & P56 == 1 & !temp_pvr & P58_1 != 8, 1, 0),
    temp_ppr = grepl(P50_2, pattern = "no s", ignore.case = T),
    policeabuse_phys_report = if_else(policeabuse_phys_any > 0 & P56 == 1 & !temp_ppr & P50_1 != 8, 1, 0),
    policeabuse_report = policeabuse_verbal_report + policeabuse_phys_report,
    temp_robr = 1 * (grepl(P22_2, pattern = "no s", ignore.case = T) | grepl(P22_2, pattern = "no r", ignore.case = T)),
    armedrob_report = if_else(armedrob_num > 0 & P18 == 1 & !is.na(P18) & !temp_robr & !is.na(P22_1) & P22_1 != 8, 1, 0),
    burglary_report = if_else(burglary_num > 0 & P18 != 1 & !is.na(P18) & !temp_robr & !is.na(P22_1) & P22_1 != 8, 1, 0),
    temp_sar = if_else(grepl(P83_2, pattern = "no s") | grepl(P83_2, pattern = "no r"), 1, 0),
    simpleassault_report = if_else(simpleassault_num > 0 & !is.na(P83_1) & !temp_sar & P83_1 != 8, 1, 0),
    carmedrob_report = if_else(carmedrob_num > 0 & P18 == 1 & !is.na(P18) & !temp_robr & !is.na(P22_1) & P22_1 != 8, 1, 0),
    cburglary_report = if_else(cburglary_num > 0 & P18 != 1 & !is.na(P18) & !temp_robr & !is.na(P22_1) & P22_1 != 8, 1, 0),
    csimpleassault_report = if_else(csimpleassault_num > 0 & !is.na(P83_1) &  !temp_sar & P83_1 != 8, 1, 0),
    temp_sex = 1 * (grepl(P33_2, pattern = "no s")|grepl(P33_2, pattern = "nos")),
    csexual_report = if_else(csexual_num > 0 & !is.na(P33_1) & !temp_sex & P33_1 != 8, 1, 0),
    temp_dv = if_else(grepl(P41_2, pattern = "no s", ignore.case = T) | grepl(P41_2, pattern = "no r", ignore.case = T) | grepl(P41_2, pattern = "nos", ignore.case = TRUE), 1, 0),
    cdomestic_phys_report = if_else(cdomestic_phys_num > 0 & !is.na(P41_1) & !temp_dv & P41_1 != 8, 1, 0),
    burglaryres = if_else(!is.na(P22_1) & P22_1 != 8 & !grepl(P22_2, pattern = "no s", ignore.case = TRUE) & !grepl(P22_2, pattern = "no r", ignore.case = TRUE),  1, 0),
    dviolres = if_else(!is.na(P38_1) & P38_1 != 8 & !grepl(P38_2, pattern = "no s", ignore.case = TRUE) & !grepl(P38_2, pattern = "no r", ignore.case = TRUE), 1, 0),
    contact_pol_susp_activity = if_else(P84 == 1, 1, 0),
    give_info_pol_investigation = if_else(P85 == 1, 1, 0),
    policebeating_report = if_else(P89 == 999, NA_integer_, P89),
    polcaseserious = if_else(P8_2 == 999, NA_integer_, P8_2 - 1L),
    polint_corrupt = if_else(P8_3 == 999, NA_integer_, -P8_3 + 5L), # reverse code so less corruption is higher
    polint_quality = if_else(P8_5 == 999, NA_integer_, P8_5 - 1L),
    reportnorm_theft = if_else(P90 == 999, NA_integer_, -P90 + 4L),
    reportnorm_abuse = if_else(P92 == 999, NA_integer_, if_else(P92 == 5, 4L, P92)),
    reportnorm_abuse = -reportnorm_abuse + 5L,
    polcap_timely = if_else(P8_6 == 999, NA_integer_, P8_6 - 1L),
    polcap_investigate = if_else(P8_7 == 999, NA_integer_, P8_7 - 1L),
    responsive_act = if_else(P8_1 == 999, NA_integer_, P8_1 - 1L),
    legit_trust = if_else(P6_1 == 999, NA_integer_, P6_1 - 1L),
    trust_community = if_else(P95 == 999, NA_integer_, P95-1L),
    compliance_patrol = if_else(P101_1 == 999, NA_integer_, P101_1),
    compliance_freq = if_else(P101_2 == 999, NA_integer_, P101_2),
    compliance_meeting = if_else(P104 > 0 |  P105 > 0, 1, 0),
    compliance_meeting = if_else(is.na(compliance_meeting), 0, 1),
    cuadrante = as.integer(CUADRANTE),
    in_baseline = 1) %>% 
  select(c(cuadrante,
           REGISTRO_BD = Número,
           armedrob_num,
           burglary_num,
           simpleassault_num, in_baseline,
           # Note: The following crimes were not collected for col baseline personal: aggassault_num,  sexual_num, domestic_phys_num, domestic_verbal_num, land_any, all other crimes.
           carmedrob_num,
           cburglary_num,
           csimpleassault_num,
           csexual_num,
           cdomestic_phys_num,
           cmurder_num,
           # Note: The following crimes were not collected for col baseline community: caggassault_num, cland_any, cdomestic_verbal_any, cmob_num, cother_any_nonviolent, cdomestic_verbal_num, cother_any_violent,
           fear_violent,
           # Note: fear_nonviolent was not collected
           feared_walk,
           satis_trust,
           # Note: satis_general was not collected
           policeabuse_verbal_any,
           policeabuse_phys_any,
           policeabuse_verbal_num,
           policeabuse_phys_num,
           policeabuse_verbal_report,
           policeabuse_phys_report,
           # Note: bribe_freq, bribe_amt was not collected in col baseline
           armedrob_report,
           burglary_report,
           simpleassault_report,
           # Note: all other_report, other_report_violent were not collected in col
           carmedrob_report,
           cburglary_report,
           csimpleassault_report,
           csexual_report,
           cdomestic_phys_report,
           # Note: The following crimes were not collected for col baseline community reporting: caggassault_report, cmurder_report, cother_report, cother_report_violent,
           burglaryres = burglaryres,
           dviolres = dviolres,
           # Note: armedrobres was not collected
           contact_pol_susp_activity,
           give_info_pol_investigation,
           # Note: dutydrink_report was not collected
           policebeating_report,
           polcaseserious,
           # Note: polcasefair was not collected
           polint_corrupt = polint_corrupt,
           polint_quality,
           # Note: none of the vars for know_idx were collected in Colombia's baseline
           reportnorm_theft = reportnorm_theft,
           reportnorm_abuse = reportnorm_abuse,
           # Note: obeynorm was not collected in Colombia's baseline
           polcap_timely,
           polcap_investigate,
           compliance_patrol,
           compliance_freq,
           compliance_meeting,
           responsive_act,
           legit_trust,
           trust_community)) %>% 
  rename_at(vars(-cuadrante, -REGISTRO_BD, -in_baseline), ~paste0(., "_baseline"))

#--------------------------------------------------------------------------------------------------------------
# rename endline variables (copied from Col teams's code, please check)
#--------------------------------------------------------------------------------------------------------------
col_citizen_endline <- 
  col_citizen_endline %>% 
  mutate(
    armedrob_num = if_else(grepl(P16_1, pattern = "1") & P18 == 1, 1, 0),
    burglary_num = if_else((grepl(P16_1, pattern = "1") & P18 != 1) | grepl(P16_6, pattern = "1"), 1, 0),
    simpleassault_num = if_else(grepl(P16_8, pattern = "1"), 1, 0),
    carmedrob_num = if_else(grepl(P16_1, pattern = "2") & P18 == 1, 1, 0),
    cburglary_num = if_else((grepl(P16_1, pattern = "2") & P18 != 1) | grepl(P16_6, pattern = "2"), 1, 0),
    csimpleassault_num = if_else(grepl(P16_8, pattern = "2"), 1, 0),
    csexual_num = if_else(grepl(P16_11, pattern = "2"), 1, 0),
    cdomestic_phys_num = if_else(grepl(P16_10, pattern = "2"), 1, 0),
    cmurder_num = if_else(grepl(P16_9, pattern = "2"),1 , 0),
    fear_violent = if_else(P5 == 999, NA_integer_, P5),
    feared_walk = if_else(P96_1_1_1 == 999, NA_integer_, -P96_1_1_1  + 2L),
    satis_trust = if_else(P6_4 == 999, NA_integer_, P6_4 - 1L), 
    satis_general = if_else(P92_1_1_1 == 999, NA_integer_, P92_1_1_1 - 1L),
    policeabuse_phys_any = if_else(P16_4 != "", 1, 0),
    policeabuse_verbal_any = if_else(P16_5 != "", 1, 0),
    policeabuse_any = 1 * ((policeabuse_verbal_any + policeabuse_phys_any) > 0),
    policeabuse_verbal_report = 1 * (policeabuse_verbal_any > 0 & P52 == 1),
    bribe_freq = case_when(
      !is.na(P16_12) & P76_1_1 < 999 ~ P76_1_1,
      !is.na(P16_12) & P76_1_1 ==999 ~ 1L,
      TRUE ~ 0L),
    bribe_amt = if_else(!is.na(P76_amount), P76_amount, 0L),
    temp_robr = if_else(grepl(P22_OTROS1, pattern = "no s", ignore.case = TRUE) | grepl(P22_OTROS1, pattern = "no r", ignore.case = TRUE), 1, 0),
    armedrob_report = if_else(armedrob_num > 0 & P18 == 1 & !is.na(P18) & !temp_robr & !is.na(P22) & P22 != 8, 1, 0),
    burglary_report = if_else(burglary_num > 0 & P18 != 1 & !is.na(P18) & !temp_robr & !is.na(P22) & P22 != 8, 1, 0),
    temp_sar = if_else(grepl(P80, pattern = "no s", ignore.case = T) | grepl(P80, pattern = "no r", ignore.case = T), 1, 0),
    simpleassault_report = if_else(simpleassault_num > 0 & !is.na(P80) & !temp_sar & P80 != 8, 1, 0),
    temp_robr = if_else(grepl(P25_OTROS1, pattern = "no s", ignore.case = T) | grepl(P25_OTROS1, pattern = "no r", ignore.case = TRUE), 1, 0),
    carmedrob_report = if_else(carmedrob_num > 0 & P18 == 1 & !is.na(P18) & !temp_robr & !is.na(P25) & P25 != 8, 1, 0),
    cburglary_report = if_else(cburglary_num > 0 & P18 != 1 & !is.na(P18) &  !temp_robr & !is.na(P25) & P25 != 8, 1, 0),
    temp_sar = if_else(grepl(P83_OTROS1, pattern = "no s", ignore.case = T) | grepl(P83_OTROS1, pattern = "no r", ignore.case = TRUE), 1, 0),
    csimpleassault_report = if_else(csimpleassault_num > 0 & !is.na(P83) & !temp_sar & P83 != 8, 1, 0),
    temp_sex = if_else(grepl(P33_OTROS1, pattern = "no s", ignore.case = T) | grepl(P33_OTROS1, pattern = "nos", ignore.case = T), 1, 0),
    csexual_report = if_else(csexual_num > 0 & !is.na(P33) & !temp_sex & P33 != 8, 1, 0),
    temp_dv = if_else(grepl(P41_OTROS1, pattern = "no s", ignore.case = TRUE) | grepl(P41_OTROS1, pattern = "no r", ignore.case = TRUE) | grepl(P41_OTROS1, pattern = "nos", ignore.case = TRUE), 1, 0),
    cdomestic_phys_report = if_else(cdomestic_phys_num > 0 & !is.na(P41) & !temp_dv & P41 != 8, 1, 0),
    burglaryres = if_else(!is.na(P22) & P22 != 8 & !grepl(P22_OTROS1, pattern = "no s", ignore.case = TRUE) & !grepl(P22_OTROS1, pattern = "no r", ignore.case = TRUE), 1, 0),
    dviolres = if_else(!is.na(P38) & P38 != 8 & !grepl(P38_OTROS1, pattern = "no s", ignore.case = TRUE) & !grepl(P38_OTROS1, pattern = "no r", ignore.case = TRUE), 1, 0),
    contact_pol_susp_activity = if_else(P84 == 1, 1, 0),
    give_info_pol_investigation = if_else(P85 == 1, 1, 0),
    policebeating_report = if_else(P89 == 999, NA_integer_, P89),
    polcaseserious = if_else(P8_2 == 999, NA_integer_, P8_2 - 1L),
    polcasefair = if_else(P8_17 == 999, NA_integer_, P8_17 - 1L),
    polint_corrupt = if_else(P8_3 == 999, NA_integer_, -P8_3 + 5L),
    polint_quality = if_else(P8_5 == 999, NA_integer_, P8_5 - 1L),
    know_law_suspect = if_else(P98_1_1_2 == 1, 1L, 0L),
    know_law_lawyer = if_else(P98_1_1_3 == 1, 1L, 0L),
    know_law_fees = if_else(P98_1_1_4 == 1, 1L, 0L),
    know_law_vaw = if_else(P98_1_1_5 == 1, 1L, 0L),
    know_report_followup = if_else(P98_1_1_6 == 1, 1L, 0L),
    know_report_station = if_else(P103_1_1 == 1, 1L, 0L),
    reportnorm_theft = if_else(P90 == 999, NA_integer_, -P90 + 4L),
    reportnorm_abuse = if_else(P92 == 999, NA_integer_, if_else(P92 == 5, 4L, P92)),
    reportnorm_abuse = -reportnorm_abuse + 5L,
    obeynorm = if_else(P92_1_1_2 == 999, NA_integer_, P92_1_1_2),
    polcap_timely = if_else(P8_6 == 999, NA_integer_, P8_6 - 1L),
    polcap_investigate = if_else(P8_7 == 999, NA_integer_, P8_7 - 1L),
    responsive_act = if_else(P8_1 == 999, NA_integer_, P8_1 - 1L),
    legit_trust = if_else(P6_1 == 999, NA_integer_, P6_1 - 1L),
    trust_community = if_else(P95 == 999, NA_integer_, P95 - 1L),
    compliance_patrol = if_else(P101_1 == 999, NA_integer_,P101_1),
    compliance_freq = if_else(P101_2 == 999, NA_integer_, P101_2),
    compliance_meeting = if_else(P107_1_1 == 1 | P107_1_2 == 1 | P107_1_3 == 1, 1L, 0L),
    compliance_meeting = if_else(is.na(compliance_meeting), 0L, compliance_meeting),
    compliance_attend = if_else(P107_1_3 == 1, 1L, 0L),
    compliance_attend = if_else(is.na(compliance_attend), 0L, compliance_attend),
    cuadrante = as.integer(CUADRANTE),
    in_endline = 1, 
    in_baseline = 0) %>% 
  select(c(cuadrante,
           REGISTRO_BD,
           armedrob_num,
           burglary_num,
           simpleassault_num, in_endline,
           # Note: aggassault_num, sexual_num, domestic_phys_num, domestic_verbal_num, land_any  and all other crimes were not collected
           carmedrob_num,
           cburglary_num,
           csimpleassault_num,
           csexual_num,
           cdomestic_phys_num,
           cmurder_num,
           # Note: caggassault_num, cland_any, cdomestic_verbal_any, cmob_num, cother_any_nonviolent, cother_any_violent were not collected
           fear_violent,
           # Note: fear_nonviolent were not collected. 
           feared_walk,
           satis_trust,
           satis_general,
           policeabuse_verbal_any,
           policeabuse_phys_any,
           # Note: policeabuse_verbal_num, policeabuse_phys_num, cdomestic_verbal_num, policeabuse_phys_report were not collected
           policeabuse_verbal_report,
           bribe_freq,
           bribe_amt,
           armedrob_report,
           burglary_report,
           simpleassault_report,
           # Note: other_report, other_report_violent
           carmedrob_report,
           cburglary_report,
           csimpleassault_report,
           csexual_report,
           cdomestic_phys_report,
           # Note: caggassault_report, cmurder_report, cother_report, cother_report_violent were not collected
           burglaryres,
           dviolres,
           # Note: armedrobres, dutydrink_report were not collected
           contact_pol_susp_activity,
           give_info_pol_investigation,
           policebeating_report,
           polcaseserious,
           polcasefair,
           polint_corrupt,
           polint_quality,
           know_law_suspect,
           know_law_lawyer,
           know_law_fees,
           know_law_vaw,
           know_report_followup,
           know_report_station,
           reportnorm_theft,
           reportnorm_abuse,
           obeynorm = obeynorm,
           polcap_timely,
           polcap_investigate,
           compliance_patrol,
           compliance_freq,
           compliance_meeting,
           compliance_attend,
           responsive_act,
           legit_trust,
           trust_community))

saveRDS(col_citizen_baseline, file = "data/out/col-citizen-baseline-clean.RDS")
saveRDS(col_citizen_endline, file = "data/out/col-citizen-endline-clean.RDS")

#--------------------------------------------------------------------------------------------------------------
          # merge baseline and endline
#--------------------------------------------------------------------------------------------------------------
col_citizen <-
  col_citizen_endline %>%
  left_join(col_units, by = "cuadrante") %>%
  left_join(col_citizen_baseline %>% select(-cuadrante), by = "REGISTRO_BD") %>%
  mutate(across(c("in_endline", "in_baseline"), replace_na, 0)) %>% 
  mutate(endline_only = if_else(in_endline == 1 & in_baseline == 0, 1L, 0L)) %>%
  group_by(cuadrante) %>%
  mutate(S_citizens_inclusion_prob = n()/population) %>% 
  ungroup

saveRDS(col_citizen, file = "data/out/col-citizen-clean.RDS")
