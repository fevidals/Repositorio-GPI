#--------------------------------------------------------------------------------------------------------------
# R PACKAGES
#--------------------------------------------------------------------------------------------------------------
suppressMessages({
  library(tidyverse)
  library(DeclareDesign)
  # library(stdidx)
})
source("code/shared/index-construction.R")

#--------------------------------------------------------------------------------------------------------------
# CLEANED DATA
#--------------------------------------------------------------------------------------------------------------
phl_citizen <- readRDS("data/out/phl-citizen-clean.RDS")
phl_admin <- readRDS("data/out/phl-admin-clean.RDS")

#--------------------------------------------------------------------------------------------------------------
# 1. Hypotheses 1(a): crimevictim_idx
#--------------------------------------------------------------------------------------------------------------
phl_citizen <- 
  phl_citizen %>%
  rowwise() %>% 
  mutate(
    # generate crime categories
    violentcrime_num = sum(armedrob_num, simpleassault_num,other_any_violent, na.rm = TRUE),
    nonviolentcrime_num = sum(burglary_num, other_any_nonviolent, na.rm = TRUE),
    cviolentcrime_num = sum(carmedrob_num, caggassault_num, csimpleassault_num, csexual_num, cdomestic_phys_num, cmurder_num, cother_any_violent, na.rm = TRUE),
    cnonviolentcrime_num = sum(cburglary_num, cother_any_nonviolent, na.rm = TRUE)) %>% 
  ungroup() %>%
  mutate(
    # standardize crime categories
    violentcrime_num_std = stdize(violentcrime_num, condition = Z_common == 0),
    nonviolentcrime_num_std = stdize(nonviolentcrime_num, condition = Z_common == 0),
    cviolentcrime_num_std = stdize(cviolentcrime_num, condition = Z_common == 0),
    cnonviolentcrime_num_std = stdize(cnonviolentcrime_num, condition = Z_common == 0),
    
    # standardize crimes
    armedrob_num_std = stdize(armedrob_num, condition = Z_common == 0),
    simpleassault_num_std = stdize(simpleassault_num, condition = Z_common == 0),
    other_any_violent_std = stdize(other_any_violent, condition = Z_common == 0),
    burglary_num_std = stdize(burglary_num, condition = Z_common == 0),
    other_any_nonviolent_std = stdize(other_any_nonviolent, condition = Z_common == 0),
    carmedrob_num_std = stdize(carmedrob_num, condition = Z_common == 0),
    caggassault_num_std = stdize(caggassault_num, condition = Z_common == 0),
    csimpleassault_num_std = stdize(csimpleassault_num, condition = Z_common == 0),
    csexual_num_std = stdize(csexual_num, condition = Z_common == 0),
    cdomestic_phys_num_std = stdize(cdomestic_phys_num, condition = Z_common == 0),
    cmurder_num_std = stdize(cmurder_num, condition = Z_common == 0),
    cother_any_violent_std = stdize(cother_any_violent, condition = Z_common == 0),
    cburglary_num_std = stdize(cburglary_num, condition = Z_common == 0),
    cother_any_nonviolent_std = stdize(cother_any_nonviolent, condition = Z_common == 0),
    
    # create index
    crime_victim_idx = idx_mean(violentcrime_num_std, nonviolentcrime_num_std, cviolentcrime_num_std, cnonviolentcrime_num_std, tx = Z),
    # re-standardize
    crime_victim_idx = stdize(crime_victim_idx, condition = Z_common == 0)
  )

# 2. Common analysis
phl_citizen <-
  phl_citizen %>%
  rowwise() %>%
  mutate(
    # sum all crimes into categories
    cviolentcrime_num_common = sum(carmedrob_num, csimpleassault_num, csexual_num, cdomestic_phys_num, cother_any_violent, na.rm = TRUE)) %>% 
  ungroup() %>%
  mutate(
    # standardize categories
    cviolentcrime_num_common_std = stdize(cviolentcrime_num_common, condition = Z_common == 0),
    # generate crime index
    crime_victim_idx_common = idx_mean(violentcrime_num_std, nonviolentcrime_num_std, cviolentcrime_num_common_std, cnonviolentcrime_num_std, tx = Z),
    # re-standardize
    crime_victim_idx_common = stdize(crime_victim_idx_common, condition = Z_common == 0))

# 3. List-wise deletion
phl_citizen <-
  phl_citizen %>%
  mutate(
    crime_victim_idx_listwise = idx_mean_listwise(violentcrime_num_std, nonviolentcrime_num_std, cviolentcrime_num_common_std, cnonviolentcrime_num_std),
    # re-standardize
    crime_victim_idx_listwise = stdize(crime_victim_idx_listwise, condition = Z_common == 0))

#--------------------------------------------------------------------------------------------------------------
# 2. Hypotheses 1(b): future_insecurity_idx
#--------------------------------------------------------------------------------------------------------------
phl_citizen <- 
  phl_citizen %>%
  mutate(
    # rescale variables
    fear_violent = fear_violent + 2,
    fear_nonviolent = fear_nonviolent + 2,
    feared_walk = feared_walk + 2,
    
    # standardize variables
    fear_violent_std = stdize(fear_violent, condition = Z_common == 0),
    fear_nonviolent_std = stdize(fear_nonviolent, condition = Z_common == 0),
    feared_walk_std = stdize(feared_walk, condition = Z_common == 0),
    
    # generate index
    future_insecurity_idx = idx_mean(fear_violent_std, fear_nonviolent_std, feared_walk_std, tx = Z),
    # re-standardize
    future_insecurity_idx = stdize(future_insecurity_idx, condition = Z_common == 0))


# 2. COMMON ANALYSIS
phl_citizen <- 
  phl_citizen %>%
  mutate(
    # Note: we have dropped fear_nonviolent following committee decision
    # standardize vars
    fear_violent_std_common = stdize(fear_violent, condition = Z_common == 0),
    feared_walk_std_common = stdize(feared_walk, condition = Z_common == 0),
    # calculate index
    future_insecurity_idx_common = idx_mean(fear_violent_std_common, feared_walk_std_common, tx = Z),
    # re-standardize
    future_insecurity_idx_common = stdize(future_insecurity_idx_common, condition = Z_common == 0)
  )

# 3. List-wise deletion
phl_citizen <-
  phl_citizen %>%
  mutate(
    future_insecurity_idx_listwise = idx_mean_listwise(fear_violent_std_common, feared_walk_std_common),
    # re-standardize
    future_insecurity_idx_listwise = stdize(future_insecurity_idx_listwise, condition = Z_common == 0))

#--------------------------------------------------------------------------------------------------------------
# 3. Hypotheses 2: satis_idx
#--------------------------------------------------------------------------------------------------------------
phl_citizen <- 
  phl_citizen %>%
  mutate(
    # rescale variables
    satis_trust = satis_trust + 2,
    satis_general = satis_general + 2,
    
    # standardize variables
    satis_trust_std = stdize(satis_trust, condition = Z_common == 0),
    satis_general_std = stdize(satis_general, condition = Z_common == 0),
    
    # generate index
    satis_idx =  idx_mean(satis_trust_std, satis_general_std, tx = Z),
    # re-standardize
    satis_idx = stdize(satis_idx, condition = Z_common == 0)
  )

# 3. List-wise deletion
phl_citizen <-
  phl_citizen %>%
  mutate(
    satis_idx_listwise = idx_mean_listwise(satis_trust_std, satis_general_std),
    # re-standardize
    satis_idx_listwise = stdize(satis_idx_listwise, condition = Z_common == 0))


# --------------------------------------------------------------------------------------------------------------
# Hypotheses 3(a): police_abuse_idx officer_attitude_idx is not calculated for Phl because we specify in the mpap not to conduct any non-experimental comparisons
# --------------------------------------------------------------------------------------------------------------

# --------------------------------------------------------------------------------------------------------------
# 4. Hypotheses 3(b): police_abuse_idx
# --------------------------------------------------------------------------------------------------------------
phl_citizen <- 
  phl_citizen %>% 
  mutate(
    policeabuse_any = case_when(
      policeabuse_verbal_any > 0 | policeabuse_phys_any > 0 ~ 1L,
      !is.na(policeabuse_verbal_any) & !is.na(policeabuse_phys_any) ~ 0L)) %>% 
  rowwise() %>% 
  mutate(
    policeabuse_num = sum(policeabuse_verbal_num, policeabuse_phys_num, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(
    # bribe amount is divided by the rate of dollar (GH issue number 49)
    bribe_amt_raw = bribe_amt,
    bribe_amt = bribe_amt / 50.5,
    # different construction that the M-PAP (GH issues number 31)
    bribe_amt = if_else(bribe_freq == 1, 0, bribe_amt),
    
    # standardize vars
    policeabuse_any_std = stdize(policeabuse_any, condition = Z_common == 0),
    policeabuse_num_std = stdize(policeabuse_num, condition = Z_common == 0),
    bribe_freq_std = stdize(bribe_freq, condition = Z_common == 0),
    bribe_amt_std = stdize(bribe_amt, condition = Z_common == 0)) %>% 
  mutate(
    # calculate index
    police_abuse_idx = idx_mean(policeabuse_any_std, policeabuse_num_std, bribe_freq_std, bribe_amt_std, tx = Z),
    # re-standardize
    police_abuse_idx = stdize(police_abuse_idx, condition = Z_common == 0),
    police_abuse_idx_common = idx_mean(policeabuse_any_std, bribe_freq_std, bribe_amt_std, tx = Z),
    # re-standardize
    police_abuse_idx_common = stdize(police_abuse_idx_common, condition = Z_common == 0))

# 3. List-wise deletion
phl_citizen <-
  phl_citizen %>%
  mutate(
    police_abuse_idx_listwise = idx_mean_listwise(policeabuse_any_std, bribe_freq_std, bribe_amt_std),
    # re-standardize
    police_abuse_idx_listwise = stdize(police_abuse_idx_listwise, condition = Z_common == 0))


#--------------------------------------------------------------------------------------------------------------
#                     # 5. Hypotheses 4(a): crime_reporting_idx
#--------------------------------------------------------------------------------------------------------------
phl_citizen <-
  phl_citizen %>%
  mutate(
    armedrob_report = case_when(
      armedrob_num == 0 | armedrob_report == 0 ~ 0,
      armedrob_num > 0 & armedrob_report == 1 ~ 1),
    burglary_report = case_when(
      burglary_num == 0 | burglary_report == 0 ~ 0,
      burglary_num > 0 & burglary_report == 1 ~ 1),
    simpleassault_report = case_when(
      simpleassault_num == 0 | simpleassault_report == 0 ~ 0,
      simpleassault_num > 0 & simpleassault_report == 1 ~ 1),
    carmedrob_report = case_when(
      carmedrob_num == 0 | carmedrob_report == 0 ~ 0,
      carmedrob_num > 0 & carmedrob_report == 1 ~ 1),
    cburglary_report = case_when(
      cburglary_num == 0 | cburglary_report == 0 ~ 0,
      cburglary_num > 0 & cburglary_report == 1 ~ 1),
    caggassault_report = case_when(
      caggassault_num == 0 | caggassault_report == 0 ~ 0,
      caggassault_num > 0 & caggassault_report == 1 ~ 1),
    csimpleassault_report = case_when(
      csimpleassault_num == 0 | csimpleassault_report == 0 ~ 0,
      csimpleassault_num > 0 & csimpleassault_report == 1 ~ 1),
    csexual_report = case_when(
      csexual_num == 0 | csexual_report == 0 ~ 0,
      csexual_num > 0 & csexual_report == 1 ~ 1),
    cdomestic_phys_report = case_when(
      cdomestic_phys_num == 0 | cdomestic_phys_report == 0 ~ 0,
      cdomestic_phys_num > 0 & cdomestic_phys_report == 1 ~ 1),
    cmurder_report = case_when(
      cmurder_num == 0 | cmurder_report == 0 ~ 0,
      cmurder_num > 0 & cmurder_report == 1 ~ 1),
    
    burglaryres = case_when(
      burglaryres == "Police" ~ 1L,
      burglaryres == "Court" ~ 1L,
      TRUE ~ 0L),
    dviolres = case_when(
      dviolres == "Police"~ 1L,
      dviolres == "Court" ~ 1L,
      TRUE ~ 0L),
    armedrobres = case_when(
      armedrobres == "Police" ~ 1L,
      armedrobres == "Court"  ~ 1L,
      TRUE ~ 0L)) %>%
  rowwise() %>%
  mutate(
    violentcrime_report_num = sum(armedrob_report, simpleassault_report, other_report_violent, na.rm = TRUE),
    nonviolentcrime_report_num = sum(burglary_report,
                                     # other_report_nonviolent,
                                     na.rm = TRUE),
    cviolentcrime_report_num = sum(carmedrob_report, caggassault_report, csimpleassault_report, csexual_report, cdomestic_phys_report, cmurder_report, cother_report_violent, na.rm = TRUE),
    cnonviolentcrime_report_num = sum(cburglary_report,
                                      # cother_report_nonviolent,
                                      na.rm = TRUE)) %>%
  ungroup() 

phl_citizen <- 
  phl_citizen %>%
  mutate(
    # standardize vars
    burglaryres_std = stdize(burglaryres, condition = Z_common == 0),
    dviolres_std = stdize(dviolres, condition = Z_common == 0),
    armedrobres_std = stdize(armedrobres, condition = Z_common == 0),
    
    violentcrime_report_num_std = stdize(violentcrime_report_num, condition = Z_common == 0),
    nonviolentcrime_report_num_std = stdize(nonviolentcrime_report_num, condition = Z_common == 0),
    cviolentcrime_report_num_std = stdize(cviolentcrime_report_num, condition = Z_common == 0),
    cnonviolentcrime_report_num_std = stdize(cnonviolentcrime_report_num, condition = Z_common == 0),
    
    armedrob_report_std = stdize(armedrob_report, condition = Z_common == 0),
    simpleassault_report_std = stdize(simpleassault_report, condition = Z_common == 0),
    other_report_violent_std = stdize(other_report_violent, condition = Z_common == 0),
    burglary_report_std = stdize(burglary_report, condition = Z_common == 0),
    carmedrob_report_std = stdize(carmedrob_report, condition = Z_common == 0),
    caggassault_report_std = stdize(caggassault_report, condition = Z_common == 0),
    csimpleassault_report_std = stdize(csimpleassault_report, condition = Z_common == 0),
    csexual_report_std = stdize(csexual_report, condition = Z_common == 0),
    cdomestic_phys_report_std = stdize(cdomestic_phys_report, condition = Z_common == 0),
    cother_report_violent_std = stdize(cother_report_violent, condition = Z_common == 0),
    cburglary_report_std = stdize(cburglary_report, condition = Z_common == 0),
    
    # calculate sub-index
    crimeres_idx = idx_mean( burglaryres_std, dviolres_std, armedrobres_std, tx = Z),
    # re-standardize
    crimeres_idx = stdize(crimeres_idx, condition = Z_common == 0),
    
    # calculate index
    crime_reporting_idx = idx_mean(violentcrime_report_num_std, nonviolentcrime_report_num_std, cviolentcrime_report_num_std, cnonviolentcrime_report_num_std, crimeres_idx, tx = Z),
    # re-standardize
    crime_reporting_idx = stdize(crime_reporting_idx, condition = Z_common == 0)
    
  )

# 2. common analysis
phl_citizen <- 
  phl_citizen %>% 
  rowwise() %>% 
  mutate(
    violentcrime_report_num_common = sum(armedrob_report, simpleassault_report, na.rm = TRUE),
    nonviolentcrime_report_num_common = burglary_report,
    cviolentcrime_report_num_common = sum(carmedrob_report, csimpleassault_report, csexual_report, cdomestic_phys_report, na.rm = TRUE),
    cnonviolentcrime_report_num_common = cburglary_report) %>% 
  ungroup() %>%
  mutate(    
    # standarduze all vars
    violentcrime_report_num_common_std = stdize(violentcrime_report_num, condition = Z_common == 0),
    nonviolentcrime_report_num_common_std = stdize(nonviolentcrime_report_num, condition = Z_common == 0),
    cviolentcrime_report_num_common_std = stdize(cviolentcrime_report_num, condition = Z_common == 0),
    cnonviolentcrime_report_num_common_std = stdize(cnonviolentcrime_report_num, condition = Z_common == 0),
    # calculate sub-index
    crimeres_idx_common = idx_mean(burglaryres_std, dviolres_std, tx = Z),
    # re-standardize
    crimeres_idx_common = stdize(crimeres_idx_common, condition = Z_common == 0),
    
    # calculate index
    crime_reporting_idx_common = idx_mean(violentcrime_report_num_common_std, nonviolentcrime_report_num_common_std, cviolentcrime_report_num_common_std, cnonviolentcrime_report_num_common_std, crimeres_idx_common, tx = Z),
    # re-standardize
    crime_reporting_idx_common = stdize(crime_reporting_idx_common, condition = Z_common == 0)
  )

# 3. List-wise deletion
phl_citizen <-
  phl_citizen %>%
  mutate(
    # calculate sub-index
    crimeres_idx_listwise = idx_mean_listwise(burglaryres_std, dviolres_std),
    # re-standardize
    crimeres_idx_listwise = stdize(crimeres_idx_listwise, condition = Z_common == 0),
    
    # calculate index
    crime_reporting_idx_listwise = idx_mean_listwise(violentcrime_report_num_common_std, nonviolentcrime_report_num_common_std, cviolentcrime_report_num_common_std, cnonviolentcrime_report_num_common_std, crimeres_idx_common),
    # re-standardize
    crime_reporting_idx_listwise = stdize(crime_reporting_idx_listwise, condition = Z_common == 0))

#--------------------------------------------------------------------------------------------------------------
# 7. Hypotheses 4(b): tips_idx
#--------------------------------------------------------------------------------------------------------------
phl_citizen <- 
  phl_citizen %>%
  mutate(
    # standardize vars
    contact_pol_susp_activity_std = stdize(contact_pol_susp_activity, condition = Z_common == 0),
    give_info_pol_investigation_std = stdize(give_info_pol_investigation, condition = Z_common == 0),
    # generate index
    crime_tips_idx = idx_mean(contact_pol_susp_activity_std, give_info_pol_investigation_std, tx = Z),
    # re-standardize
    crime_tips_idx = stdize(crime_tips_idx, condition = Z_common == 0),
    
    # make names consistent with mpap
    tips_idx = crime_tips_idx)

# 3. List-wise deletion
phl_citizen <-
  phl_citizen %>%
  mutate(
    crime_tips_idx_listwise = idx_mean_listwise(contact_pol_susp_activity_std, give_info_pol_investigation_std),
    # re-standardize
    crime_tips_idx_listwise = stdize(crime_tips_idx_listwise, condition = Z_common == 0),
    
    # make names consistent with mpap
    tips_idx_listwise = crime_tips_idx_listwise)

#--------------------------------------------------------------------------------------------------------------
# 8. Hypotheses 4(c): police_abuse_report_idx
#--------------------------------------------------------------------------------------------------------------
phl_citizen <- 
  phl_citizen %>%
  mutate(
    # rescale variables
    dutydrink_report_raw = dutydrink_report,
    dutydrink_report = dutydrink_report + 2,
    dutydrink_report_rescaled = dutydrink_report + 2,
    policebeating_report_raw = policebeating_report,
    policebeating_report = policebeating_report + 2,
    policebeating_report_rescaled = policebeating_report + 2,
    policeabuse_report = case_when(
      policeabuse_verbal_report_raw == 1 | policeabuse_phys_report_raw == 1 ~ 1L,
      TRUE ~ 0L), 
    # standardize vars
    policeabuse_report_std = stdize(policeabuse_report, condition = Z_common == 0),
    dutydrink_report_std = stdize(dutydrink_report_rescaled, condition = Z_common == 0),
    policebeating_report_std = stdize(policebeating_report_rescaled, condition = Z_common == 0),
    # calculate index
    police_abuse_report_idx = idx_mean(policeabuse_report_std, dutydrink_report_std, policebeating_report_std, tx = Z),
    # re-standardize
    police_abuse_report_idx = stdize(police_abuse_report_idx, condition = Z_common == 0))

phl_citizen <- 
  phl_citizen %>%
  mutate(
    # general police abuse components
    policeabuse_report_common = case_when(
      policeabuse_verbal_report_raw == 1 ~ 1L,
      TRUE ~ 0L), 
    # standardize vars
    policeabuse_report_common_std = stdize(policeabuse_report_common, condition = Z_common == 0),
    # calculate index
    police_abuse_report_idx_common = idx_mean(policeabuse_report_common_std, policebeating_report_std, tx = Z),
    # re-standardize
    police_abuse_report_idx_common = stdize(police_abuse_report_idx_common, condition = Z_common == 0))

# 3. List-wise deletion
phl_citizen <-
  phl_citizen %>%
  mutate(
    police_abuse_report_idx_listwise = idx_mean_listwise(policeabuse_report_common_std, policebeating_report_std),
    # re-standardize
    police_abuse_report_idx_listwise = stdize(police_abuse_report_idx_listwise, condition = Z_common == 0))

#--------------------------------------------------------------------------------------------------------------
# 8. Hypotheses M1a: intentions_idx
#--------------------------------------------------------------------------------------------------------------
phl_citizen <- 
  phl_citizen %>%
  mutate(
    # reverse order of variables to be consistent with the direction of the MPAP
    polcaseserious_raw = polcaseserious,
    polcasefair_raw = polcasefair, 
    polint_quality_raw = polint_quality, 
    
    polcaseserious = polcaseserious + 2, 
    polint_corrupt = polint_corrupt_raw + 2, 
    polcasefair = polcasefair + 2,
    polint_quality = polint_quality + 2,
    
    # standardize vars
    polint_corrupt_std = stdize(polint_corrupt, condition = Z_common == 0),
    polint_quality_std = stdize(polint_quality, condition = Z_common == 0),
    
    polcaseserious_std = stdize(polcaseserious, condition = Z_common == 0),
    polcasefair_std =  stdize(polcasefair, condition = Z_common == 0),
    
    # construct sub-indices
    polint_idx =  idx_mean(polint_corrupt_std, polint_quality_std, tx = Z),
    # re-standardize
    polint_idx = stdize(polint_idx, condition = Z_common == 0),
    
    # construct index
    intentions_idx = idx_mean(polint_idx, polcaseserious_std, polcasefair_std, tx = Z),
    # re-standardize
    intentions_idx = stdize(intentions_idx, condition = Z_common == 0)
  )

# 3. List-wise deletion
phl_citizen <-
  phl_citizen %>%
  mutate(
    polint_idx_listwise = idx_mean_listwise(polint_corrupt_std, polint_quality_std),
    # re-standardize
    polint_idx_listwise = stdize(polint_idx_listwise, condition = Z_common == 0),
    
    intentions_idx_listwise = idx_mean_listwise(polint_idx_listwise, polcaseserious_std, polcasefair_std),
    # re-standardize
    intentions_idx_listwise = stdize(intentions_idx_listwise, condition = Z_common == 0))

#--------------------------------------------------------------------------------------------------------------
# 9. Hypotheses M1b: know_idx
#--------------------------------------------------------------------------------------------------------------
phl_citizen <-
  phl_citizen %>%
  mutate(
    # rename variables
    know_law_resp = knowlaw1,
    know_law_sexual = knowlaw2,
    know_process = knowproc,
    
    # standardize variables
    know_law_resp_std = stdize(know_law_resp, condition = Z_common == 0),
    know_law_sexual_std = stdize(know_law_sexual, condition = Z_common == 0),
    know_process_std = stdize(know_process, condition = Z_common == 0),
    
    # calculate index
    know_idx_deviate = idx_mean(know_law_resp_std, know_law_sexual_std, know_process_std, tx = Z),
    # re-standardize
    know_idx_deviate = stdize(know_idx_deviate, condition = Z_common == 0))

#--------------------------------------------------------------------------------------------------------------
# 10. Hypotheses M1c: norm_idx
#--------------------------------------------------------------------------------------------------------------

phl_citizen <- 
  phl_citizen %>%
  mutate(
    # reverse order of variables to be consistent with the direction (MPAP page 75-76)
    reportnorm_theft = 4 - (2 + reportnorm_theft_raw),
    reportnorm_abuse = 4 - (2 + reportnorm_abuse_raw),
    obeynorm = 
      # 4 - 
      (2 + obeynorm_raw),
    
    # standardize vars
    reportnorm_theft_std = stdize(reportnorm_theft, condition = Z_common == 0),
    reportnorm_abuse_std = stdize(reportnorm_abuse, condition = Z_common == 0),
    obeynorm_std = stdize(obeynorm, condition = Z_common == 0),
    
    # construct index
    norm_idx = idx_mean(reportnorm_theft_std, reportnorm_abuse_std, obeynorm_std, tx = Z),
    # re-standardize
    norm_idx = stdize(norm_idx, condition = Z_common == 0))

# 3. List-wise deletion
phl_citizen <-
  phl_citizen %>%
  mutate(
    norm_idx_listwise = idx_mean_listwise(reportnorm_theft_std, reportnorm_abuse_std, obeynorm_std),
    # re-standardize
    norm_idx_listwise = stdize(norm_idx_listwise, condition = Z_common == 0))

#--------------------------------------------------------------------------------------------------------------
# 11. Hypotheses M2a: police_capacity_idx
#--------------------------------------------------------------------------------------------------------------
phl_citizen <- 
  phl_citizen %>%
  mutate(
    # reverse order of variables to be consistent with the direction of the MPAP (MPAP page 75-76)
    polcap_timely_raw = polcap_timely, 
    polcap_investigate_raw = polcap_investigate, 
    
    polcap_timely = polcap_timely + 2, 
    polcap_investigate = polcap_investigate + 2, 
    
    # standardize the vars
    polcap_timely_std = stdize(polcap_timely, condition = Z_common == 0),
    polcap_investigate_std = stdize(polcap_investigate, condition = Z_common == 0),
    
    # calculate index
    police_capacity_idx = idx_mean(polcap_timely_std, polcap_investigate_std, tx = Z),
    # re-standardize
    police_capacity_idx = stdize(police_capacity_idx, condition = Z_common == 0)
  )

# 3. List-wise deletion
phl_citizen <-
  phl_citizen %>%
  mutate(
    police_capacity_idx_listwise = idx_mean_listwise(polcap_timely_std, polcap_investigate_std),
    # re-standardize
    police_capacity_idx_listwise = stdize(police_capacity_idx_listwise, condition = Z_common == 0))

#--------------------------------------------------------------------------------------------------------------
# 12. Hypotheses M2b: responsive_act
#--------------------------------------------------------------------------------------------------------------
phl_citizen <- 
  phl_citizen %>%
  mutate(
    # rescale the variables
    responsive_act_raw = responsive_act, 
    responsive_act = responsive_act + 2,
    
    # standardize the vars
    responsive_act_std = stdize(responsive_act, condition = Z_common == 0))

#--------------------------------------------------------------------------------------------------------------
# 13. Hypotheses S1: legit_trust
#--------------------------------------------------------------------------------------------------------------

phl_citizen <- 
  phl_citizen %>%
  mutate(
    # rescale the variables
    legit_trust_raw = legit_trust,
    legit_trust_rescaled = case_when(
      legit_trust == -2 ~ 1,
      legit_trust == -1 ~ 2,
      legit_trust == 1 ~ 3,
      legit_trust == 2 ~ 4,
      legit_trust == 0 ~ 2.5,
      TRUE ~ legit_trust),
    
    # standardize the vars
    legit_trust_std = stdize(legit_trust_rescaled, condition = Z_common == 0))

#--------------------------------------------------------------------------------------------------------------
# 14. Hypotheses S2: trust_community
#--------------------------------------------------------------------------------------------------------------

phl_citizen <- 
  phl_citizen %>%
  mutate(
    # rescale the variables
    trust_community_rescaled = case_when(
      trust_community == -2 ~ 0,
      trust_community == -1 ~ 1,
      trust_community == 1 ~ 2,
      trust_community == 2 ~ 3,
      trust_community == 0 ~ 1.5,
      TRUE ~ trust_community),
    
    # standardize the vars
    trust_community_std = stdize(trust_community_rescaled, condition = Z_common == 0))

#--------------------------------------------------------------------------------------------------------------
# 15. Hypotheses C: compliance_idx
#--------------------------------------------------------------------------------------------------------------
phl_citizen <- 
  phl_citizen %>%
  mutate(
    # reverse order of variables to be consistent with the direction of the MPAP (MPAP page 75-76)
    compliance_patrol_raw = compliance_patrol,
    compliance_freq_raw = compliance_freq,
    compliance_meeting_raw = compliance_meeting,
    
    compliance_patrol_rescaled = as.integer(6 - compliance_patrol),
    compliance_freq_rescaled = as.integer(6 - compliance_freq),
    compliance_meeting_rescaled = as.integer(compliance_meeting),
    
    # standardize the vars
    compliance_patrol_std = stdize(compliance_patrol_rescaled, condition = Z_common == 0),
    compliance_freq_std = stdize(compliance_freq_rescaled, condition = Z_common == 0),
    compliance_meeting_std = stdize(compliance_meeting_rescaled, condition = Z_common == 0),
    
    # calculate index
    compliance_idx = idx_mean(compliance_patrol_std, compliance_freq_std, compliance_meeting_std, tx = Z),
    # re-standardize
    compliance_idx = stdize(compliance_idx, condition = Z_common == 0))

# 3. List-wise deletion
phl_citizen <-
  phl_citizen %>%
  mutate(
    compliance_idx_listwise = idx_mean_listwise(compliance_patrol_std, compliance_freq_std, compliance_meeting_std),
    # re-standardize
    compliance_idx_listwise = stdize(compliance_idx_listwise, condition = Z_common == 0))

#--------------------------------------------------------------------------------------------------------------
# 16. Hyp 1a. (alt. i): crime_victim_idx_admin
#--------------------------------------------------------------------------------------------------------------
phl_admin <-
  phl_admin %>%
  rowwise() %>%
  mutate(
    # generate categories of crimes
    aviolentcrime_num = sum(aarmedrob_num, aaggassault_num, asexual_num, amurder_num, na.rm = TRUE),
    # Note: asimpleassault_num, adomestic_phys_num, aother_num_violent, aother_num_nonviolent
    anonviolentcrime_num = sum(aburglary_num, na.rm = TRUE),
    aviolentcrime_num_baseline = sum(aarmedrob_num_baseline, aaggassault_num_baseline, asexual_num_baseline, amurder_num_baseline, na.rm = TRUE),
    # Note: asimpleassault_num, adomestic_phys_num, aother_num_violent, aother_num_nonviolent
    anonviolentcrime_num_baseline = sum(aburglary_num_baseline, na.rm = TRUE)) %>%
  ungroup %>%
  mutate(
    # standardize crimes
    aviolentcrime_num_std = stdize(aviolentcrime_num, to = aviolentcrime_num_baseline),
    anonviolentcrime_num_std = stdize(anonviolentcrime_num, to = anonviolentcrime_num_baseline),
    aviolentcrime_num_std_baseline = stdize(aviolentcrime_num_baseline, to = aviolentcrime_num_baseline),
    anonviolentcrime_num_std_baseline = stdize(anonviolentcrime_num_baseline, to = anonviolentcrime_num_baseline),
    
    aarmedrob_num_std = stdize(aarmedrob_num, to = aarmedrob_num_baseline),
    aaggassault_num_std = stdize(aaggassault_num, to = aaggassault_num_baseline),
    asexual_num_std = stdize(asexual_num, to = asexual_num_baseline),
    amurder_num_std = stdize(amurder_num, to = amurder_num_baseline),
    aburglary_num_std = stdize(aburglary_num, to = aburglary_num_baseline),
    
    aarmedrob_num_std_baseline = stdize(aarmedrob_num_baseline, to = aarmedrob_num_baseline),
    aaggassault_num_std_baseline = stdize(aaggassault_num_baseline, to = aaggassault_num_baseline),
    asexual_num_std_baseline = stdize(asexual_num_baseline, to = asexual_num_baseline),
    amurder_num_std_baseline = stdize(amurder_num_baseline, to = amurder_num_baseline),
    aburglary_num_std_baseline = stdize(aburglary_num_baseline, to = aburglary_num_baseline),
    
    # generate index
    crime_victim_idx_admin_baseline = idx_mean(aviolentcrime_num_std_baseline, anonviolentcrime_num_std_baseline, tx = Z),
    # re-standardize
    crime_victim_idx_admin_baseline = stdize(crime_victim_idx_admin_baseline, to = crime_victim_idx_admin_baseline),
    
    # generate index
    crime_victim_idx_admin = idx_mean(aviolentcrime_num_std, anonviolentcrime_num_std, tx = Z),
    # re-standardize
    crime_victim_idx_admin = stdize(crime_victim_idx_admin, to = crime_victim_idx_admin_baseline)) %>%
  ungroup


# 3. List-wise deletion
phl_admin <-
  phl_admin %>%
  mutate(
    crime_victim_idx_admin_listwise_baseline = idx_mean_listwise(aviolentcrime_num_std_baseline, anonviolentcrime_num_std_baseline),
    # re-standardize
    crime_victim_idx_admin_listwise_baseline = stdize(crime_victim_idx_admin_listwise_baseline, to = crime_victim_idx_admin_listwise_baseline),
    
    crime_victim_idx_admin_listwise = idx_mean_listwise(aviolentcrime_num_std, anonviolentcrime_num_std),
    # re-standardize
    crime_victim_idx_admin_listwise = stdize(crime_victim_idx_admin_listwise, to = crime_victim_idx_admin_listwise_baseline))

# 
# #--------------------------------------------------------------------------------------------------------------
# # 17. Hyp 1a. (alt. ii): crime_victim_idx_exp
# #--------------------------------------------------------------------------------------------------------------
phl_citizen <-
  phl_citizen %>%
  rowwise() %>%
  mutate(
    # generate categories of crimes
    violentcrime_num_exp = sum(armedrob_num, simpleassault_num, other_any_violent, na.rm = TRUE),
    nonviolentcrime_num_exp = sum(burglary_num, other_any_nonviolent, na.rm = TRUE),
    cviolentcrime_num_exp = sum(carmedrob_num, caggassault_num, csimpleassault_num, csexual_num, cdomestic_phys_num, cmurder_num, cother_any_violent, na.rm = TRUE),
    cnonviolentcrime_num_exp = sum(cburglary_num, cother_any_nonviolent, na.rm = TRUE)
    # Note: aggassault_num, sexual_num,  domestic_phys_num, cdomestic_verbal_num, domestic_verbal_num, land_any, cland_any, cmob_num were not collected
  ) %>%
  ungroup() %>%
  mutate(
    # standardize crimes
    violentcrime_num_exp_std = stdize(violentcrime_num_exp, condition = Z_common == 0),
    nonviolentcrime_num_exp_std = stdize(nonviolentcrime_num_exp, condition = Z_common == 0),
    cviolentcrime_num_exp_std = stdize(cviolentcrime_num_exp, condition = Z_common == 0),
    cnonviolentcrime_num_exp_std = stdize(cnonviolentcrime_num_exp, condition = Z_common == 0),
    
    armedrob_num_std = stdize(armedrob_num, condition = Z_common == 0),
    simpleassault_num_std = stdize(simpleassault_num, condition = Z_common == 0),
    other_any_violent_std = stdize(other_any_violent, condition = Z_common == 0),
    burglary_num_std = stdize(burglary_num, condition = Z_common == 0),
    other_any_nonviolent_std = stdize(other_any_nonviolent, condition = Z_common == 0),
    carmedrob_num_std = stdize(carmedrob_num, condition = Z_common == 0),
    caggassault_num_std = stdize(caggassault_num, condition = Z_common == 0),
    csimpleassault_num_std = stdize(csimpleassault_num, condition = Z_common == 0),
    csexual_num_std = stdize(csexual_num, condition = Z_common == 0),
    cdomestic_phys_num_std = stdize(cdomestic_phys_num, condition = Z_common == 0),
    cmurder_num_std = stdize(cmurder_num, condition = Z_common == 0),
    cother_any_violent_std = stdize(cother_any_violent, condition = Z_common == 0),
    cburglary_num_std = stdize(cburglary_num, condition = Z_common == 0),
    cother_any_nonviolent_std = stdize(cother_any_nonviolent, condition = Z_common == 0),
    
    # generate index
    crime_victim_idx_exp = idx_mean(violentcrime_num_exp_std, nonviolentcrime_num_exp_std, cviolentcrime_num_exp_std, cnonviolentcrime_num_exp_std, tx = Z),
    # re-standardize
    crime_victim_idx_exp = stdize(crime_victim_idx_exp, condition = Z_common == 0))

# 3. List-wise deletion
phl_citizen <-
  phl_citizen %>%
  mutate(
    crime_victim_idx_exp_listwise = idx_mean_listwise(violentcrime_num_exp_std, nonviolentcrime_num_exp_std, cviolentcrime_num_exp_std, cnonviolentcrime_num_exp_std),
    # re-standardize
    crime_victim_idx_exp_listwise = stdize(crime_victim_idx_exp_listwise, condition = Z_common == 0))

# #--------------------------------------------------------------------------------------------------------------
# # 18. Hyp 1a. (alt. iii) crime_victim_idx_bin
# #--------------------------------------------------------------------------------------------------------------
phl_citizen <-
  phl_citizen %>%
  mutate(
    armedrob_bin = case_when(
      armedrob_num > 0 ~ 1L,
      armedrob_num == 0 ~ 0L,
      TRUE ~ NA_integer_),
    simpleassault_bin =  case_when(
      simpleassault_num > 0 ~ 1L,
      simpleassault_num == 0 ~ 0L,
      TRUE ~ NA_integer_),
    burglary_bin = case_when(
      burglary_num > 0 ~ 1L,
      burglary_num == 0 ~ 0L,
      TRUE ~ NA_integer_),
    carmedrob_bin = case_when(
      carmedrob_num > 0 ~ 1L,
      carmedrob_num == 0 ~ 0L,
      TRUE ~ NA_integer_),
    caggassault_bin = case_when(
      caggassault_num > 0 ~ 1L,
      caggassault_num == 0 ~ 0L,
      TRUE ~ NA_integer_),
    csexual_bin = case_when(
      csexual_num > 0 ~ 1L,
      csexual_num == 0 ~ 0L,
      TRUE ~ NA_integer_),
    cdomestic_phys_bin = case_when(
      cdomestic_phys_num > 0 ~ 1L,
      cdomestic_phys_num == 0 ~ 0L,
      TRUE ~ NA_integer_),
    cmurder_bin = case_when(
      cmurder_num > 0 ~ 1L,
      cmurder_num == 0 ~ 0L,
      TRUE ~ NA_integer_),
    cburglary_bin = case_when(
      cburglary_num > 0 ~ 1L,
      cburglary_num == 0 ~ 0L,
      TRUE ~ NA_integer_),
    csimpleassault_bin = case_when(
      csimpleassault_num > 0 ~ 1L,
      csimpleassault_num == 0 ~ 0L,
      TRUE ~ NA_integer_)) %>%
  rowwise() %>%
  mutate(
    # sum all crimes into categories
    violentcrime_bin =  sum(armedrob_bin, simpleassault_bin, other_any_violent, na.rm = TRUE),
    nonviolentcrime_bin = sum(burglary_bin, other_any_nonviolent, na.rm = TRUE),
    cviolentcrime_bin = sum(carmedrob_bin, caggassault_bin, csimpleassault_bin, csexual_bin, cdomestic_phys_bin, cmurder_bin, cother_any_violent, na.rm = TRUE),
    cnonviolentcrime_bin = sum(cburglary_bin, cother_any_nonviolent, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(
    violentcrime_bin_std = stdize(violentcrime_bin, condition = Z_common == 0),
    nonviolentcrime_bin_std = stdize(nonviolentcrime_bin, condition = Z_common == 0),
    cviolentcrime_bin_std = stdize(cviolentcrime_bin, condition = Z_common == 0),
    cnonviolentcrime_bin_std = stdize(cnonviolentcrime_bin, condition = Z_common == 0),
    
    armedrob_bin_std = stdize(armedrob_bin, condition = Z_common == 0),
    simpleassault_bin_std = stdize(simpleassault_bin, condition = Z_common == 0),
    other_any_violent_std = stdize(other_any_violent, condition = Z_common == 0),
    burglary_bin_std = stdize(burglary_bin, condition = Z_common == 0),
    other_any_nonviolent_std = stdize(other_any_nonviolent, condition = Z_common == 0),
    carmedrob_bin_std = stdize(carmedrob_bin, condition = Z_common == 0),
    caggassault_bin_std = stdize(caggassault_bin, condition = Z_common == 0),
    csimpleassault_bin_std = stdize(csimpleassault_bin, condition = Z_common == 0),
    csexual_bin_std = stdize(csexual_bin, condition = Z_common == 0),
    cdomestic_phys_bin_std = stdize(cdomestic_phys_bin, condition = Z_common == 0),
    cmurder_bin_std = stdize(cmurder_bin, condition = Z_common == 0),
    cother_any_violent_std = stdize(cother_any_violent, condition = Z_common == 0),
    cburglary_bin_std = stdize(cburglary_bin, condition = Z_common == 0),
    cother_any_nonviolent_std = stdize(cother_any_nonviolent, condition = Z_common == 0),
    
    crime_victim_idx_bin = idx_mean(violentcrime_bin_std, nonviolentcrime_bin_std, cviolentcrime_bin_std, cnonviolentcrime_bin_std, tx = Z),
    # re-standardize
    crime_victim_idx_bin = stdize(crime_victim_idx_bin, condition = Z_common == 0))

# 3. List-wise deletion
phl_citizen <-
  phl_citizen %>%
  mutate(
    crime_victim_idx_bin_listwise = idx_mean_listwise(violentcrime_bin_std, nonviolentcrime_bin_std, cviolentcrime_bin_std, cnonviolentcrime_bin_std),
    # re-standardize
    crime_victim_idx_bin_listwise = stdize(crime_victim_idx_bin_listwise, condition = Z_common == 0))


#--------------------------------------------------------------------------------------------------------------
# 21. REMOVE ALL NON RESCALED VARIABLES
#--------------------------------------------------------------------------------------------------------------
rescaled_vars <-
  names(phl_citizen)[str_ends(names(phl_citizen), "_rescaled")] %>%
  str_replace("_rescaled", "")

phl_citizen <- 
  phl_citizen %>% 
  select(-rescaled_vars)


#--------------------------------------------------------------------------------------------------------------
# Data not collected for hypothesis 4(a): crime_reporting_idx_admin
#--------------------------------------------------------------------------------------------------------------

# save citizen, admin, and officer files
saveRDS(phl_citizen,
        file = "data/out/phl-citizen-construct.RDS")
saveRDS(phl_admin,
        file = "data/out/phl-admin-construct.RDS")
