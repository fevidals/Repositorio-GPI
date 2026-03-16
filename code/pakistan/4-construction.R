#--------------------------------------------------------------------------------------------------------------
                        # R packages and code
#--------------------------------------------------------------------------------------------------------------
suppressMessages({
  library(tidyverse)
  library(DeclareDesign)
})
source("code/shared/index-construction.R")

#--------------------------------------------------------------------------------------------------------------
                        # Read Cleaned Data
#--------------------------------------------------------------------------------------------------------------
pak_citizen <- readRDS("data/out/pak-citizen-clean.RDS")
pak_officer <- readRDS("data/out/pak-officer-clean.RDS")
pak_admin <- readRDS("data/out/pak-admin-clean.RDS")

#--------------------------------------------------------------------------------------------------------------
                        # 1. Hypotheses 1(a): crimevictim_idx
#--------------------------------------------------------------------------------------------------------------
pak_citizen <-
  pak_citizen %>%
  mutate(armedrob_num = if_else(armedrob_num == 999, NA_integer_, armedrob_num)) %>% 
  rowwise() %>%
  mutate(
    # sum all crimes into categories
    violentcrime_num = sum(armedrob_num, simpleassault_num, other_any_violent, na.rm = TRUE),
    nonviolentcrime_num = sum(burglary_num, other_any_nonviolent, na.rm = TRUE),
    cviolentcrime_num = sum(carmedrob_num, csimpleassault_num, csexual_num, cdomestic_phys_num, cmurder_num, cother_any_violent, na.rm = TRUE),
    cnonviolentcrime_num = sum(cburglary_num, cother_any_nonviolent, na.rm = TRUE),
    violentcrime_num_baseline = sum(armedrob_num_baseline, simpleassault_num_baseline, other_any_violent_baseline, na.rm = TRUE),
    nonviolentcrime_num_baseline = sum(burglary_num_baseline, other_any_nonviolent_baseline, na.rm = TRUE),
    cviolentcrime_num_baseline = sum(carmedrob_num_baseline, csimpleassault_num_baseline, csexual_num_baseline, cdomestic_phys_num_baseline, cmurder_num_baseline, cother_any_violent_baseline, na.rm = TRUE),
    cnonviolentcrime_num_baseline = sum(cburglary_num_baseline, cother_any_nonviolent_baseline, na.rm = TRUE),
    cnonviolentcrime_num_baseline = as.numeric(cnonviolentcrime_num_baseline)) %>%
  ungroup() %>%
  mutate(across(c(violentcrime_num_baseline, nonviolentcrime_num_baseline, cviolentcrime_num_baseline, cnonviolentcrime_num_baseline), ~ if_else(in_baseline == 0, NA_real_, as.numeric(.)))) %>% 
  mutate(
    # standardize categories
    violentcrime_num_std = stdize(violentcrime_num, to = violentcrime_num_baseline),
    nonviolentcrime_num_std = stdize(nonviolentcrime_num, to = nonviolentcrime_num_baseline),
    cviolentcrime_num_std = stdize(cviolentcrime_num, to = cviolentcrime_num_baseline),
    cnonviolentcrime_num_std = stdize(cnonviolentcrime_num, to = cnonviolentcrime_num_baseline),
    violentcrime_num_std_baseline = stdize(violentcrime_num_baseline, to = violentcrime_num_baseline),
    nonviolentcrime_num_std_baseline = stdize(nonviolentcrime_num_baseline, to = nonviolentcrime_num_baseline),
    cviolentcrime_num_std_baseline = stdize(cviolentcrime_num_baseline, to = cviolentcrime_num_baseline),
    cnonviolentcrime_num_std_baseline = stdize(cnonviolentcrime_num_baseline, to = cnonviolentcrime_num_baseline),
    # standardize individual crimes
    armedrob_num_std = stdize(armedrob_num, to = armedrob_num_baseline),
    simpleassault_num_std = stdize(simpleassault_num, to = simpleassault_num_baseline),
    other_any_violent_std = stdize(other_any_violent, to = other_any_violent_baseline),
    burglary_num_std = stdize(burglary_num, to = burglary_num_baseline),
    other_any_nonviolent_std = stdize(other_any_nonviolent, to = other_any_nonviolent_baseline),
    carmedrob_num_std = stdize(carmedrob_num, to = carmedrob_num_baseline),
    csimpleassault_num_std = stdize(csimpleassault_num, to = csimpleassault_num_baseline),
    csexual_num_std = stdize(csexual_num, to = csexual_num_baseline),
    cdomestic_phys_num_std = stdize(cdomestic_phys_num, to = cdomestic_phys_num_baseline),
    cmurder_num_std = stdize(cmurder_num, to = cmurder_num_baseline),
    cother_any_violent_std = stdize(cother_any_violent, to = cother_any_violent_baseline),
    cburglary_num_std = stdize(cburglary_num, to = cburglary_num_baseline),
    cother_any_nonviolent_std = stdize(cother_any_nonviolent, to = cother_any_nonviolent_baseline),
    
    armedrob_num_std_baseline = stdize(armedrob_num_baseline, to = armedrob_num_baseline),
    simpleassault_num_std_baseline = stdize(simpleassault_num_baseline, to = simpleassault_num_baseline),
    other_any_violent_std_baseline = stdize(other_any_violent_baseline, to = other_any_violent_baseline),
    burglary_num_std_baseline = stdize(burglary_num_baseline, to = burglary_num_baseline),
    other_any_nonviolent_std_baseline = stdize(other_any_nonviolent_baseline, to = other_any_nonviolent_baseline),
    carmedrob_num_std_baseline = stdize(carmedrob_num_baseline, to = carmedrob_num_baseline),
    csimpleassault_num_std_baseline = stdize(csimpleassault_num_baseline, to = csimpleassault_num_baseline),
    csexual_num_std_baseline = stdize(csexual_num_baseline, to = csexual_num_baseline),
    cdomestic_phys_num_std_baseline = stdize(cdomestic_phys_num_baseline, to = cdomestic_phys_num_baseline),
    cmurder_num_std_baseline = stdize(cmurder_num_baseline, to = cmurder_num_baseline),
    cother_any_violent_std_baseline = stdize(cother_any_violent_baseline, to = cother_any_violent_baseline),
    cburglary_num_std_baseline = stdize(cburglary_num_baseline, to = cburglary_num_baseline),
    cother_any_nonviolent_std_baseline = stdize(cother_any_nonviolent_baseline, to = cother_any_nonviolent_baseline),
    # generate crime index
    crime_victim_idx = idx_mean(violentcrime_num_std, nonviolentcrime_num_std, cviolentcrime_num_std, cnonviolentcrime_num_std, tx = Z, fe = stations),
    crime_victim_idx_baseline = idx_mean(violentcrime_num_std_baseline, nonviolentcrime_num_std_baseline, cviolentcrime_num_std_baseline, cnonviolentcrime_num_std_baseline, tx = Z, fe = stations),
    # re-standardize
    crime_victim_idx = stdize(crime_victim_idx, to = crime_victim_idx_baseline),
    crime_victim_idx_baseline = stdize(crime_victim_idx_baseline, to = crime_victim_idx_baseline)
  ) %>% 
  # generate crime at the cluster level het effects
  group_by(beats) %>% 
  mutate(crime_victim_idx_cluster_baseline = mean(crime_victim_idx_baseline, na.rm = TRUE)) %>% 
  ungroup

# 2. Common analysis
pak_citizen <-
  pak_citizen %>%
  rowwise() %>%
  mutate(
    # sum all crimes into categories
    cviolentcrime_num_common = sum(carmedrob_num, csimpleassault_num, csexual_num, cdomestic_phys_num, cother_any_violent, na.rm = TRUE),
    cviolentcrime_num_common_baseline = sum(carmedrob_num_baseline, csimpleassault_num_baseline, csexual_num_baseline, cdomestic_phys_num_baseline, cother_any_violent_baseline, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(
    # standardize categories
    cviolentcrime_num_common_std = stdize(cviolentcrime_num_common, to = cviolentcrime_num_common_baseline),
    cviolentcrime_num_common_std_baseline = stdize(cviolentcrime_num_common_baseline, to = cviolentcrime_num_common_baseline),
    # generate crime index
    crime_victim_idx_common = idx_mean(violentcrime_num_std, nonviolentcrime_num_std, cviolentcrime_num_common_std, cnonviolentcrime_num_std, tx = Z, fe = stations),
    crime_victim_idx_common_baseline = idx_mean(violentcrime_num_std_baseline, nonviolentcrime_num_std_baseline, cviolentcrime_num_common_std_baseline, cnonviolentcrime_num_std_baseline, tx = Z, fe = stations),
    # re-standardize
    crime_victim_idx_common = stdize(crime_victim_idx_common, to = crime_victim_idx_common_baseline),
    crime_victim_idx_common_baseline = stdize(crime_victim_idx_common_baseline, to = crime_victim_idx_common_baseline)
  )

# 3. List-wise deletion
pak_citizen <-
  pak_citizen %>%
  mutate(
    crime_victim_idx_listwise = idx_mean_listwise(violentcrime_num_std, nonviolentcrime_num_std, cviolentcrime_num_common_std, cnonviolentcrime_num_std),
    crime_victim_idx_listwise_baseline = idx_mean_listwise(violentcrime_num_std_baseline, nonviolentcrime_num_std_baseline, cviolentcrime_num_common_std_baseline, cnonviolentcrime_num_std_baseline),
    # re-standardize
    crime_victim_idx_listwise = stdize(crime_victim_idx_listwise, to = crime_victim_idx_listwise_baseline),
    crime_victim_idx_listwise_baseline = stdize(crime_victim_idx_listwise_baseline, to = crime_victim_idx_listwise_baseline)
    )

#--------------------------------------------------------------------------------------------------------------
                      # 2. Hypotheses 1(b): future_insecurity_idx
#--------------------------------------------------------------------------------------------------------------
pak_citizen <- 
  pak_citizen %>%
  mutate(
    # reverse order of variables to be consistent with the direction of the MPAP
    fear_nonviolent_raw_baseline = fear_nonviolent_baseline,
    fear_nonviolent_rescaled_baseline = as.integer(fear_nonviolent_baseline) - 1L,
    fear_violent_rescaled_baseline = as.integer(fear_violent_baseline) - 1L,
    
    feared_walk_rescaled = as.integer(feared_walk) - 1L, 
    feared_walk_rescaled_baseline = as.integer(feared_walk_baseline) - 1L,
    # standardize vars
    fear_violent_std = stdize(fear_violent, to = fear_violent_rescaled_baseline),
    fear_nonviolent_std = stdize(fear_nonviolent, to = fear_nonviolent_rescaled_baseline),
    feared_walk_std = stdize(feared_walk_rescaled, to = feared_walk_rescaled_baseline),
    fear_violent_std_baseline = stdize(fear_violent_rescaled_baseline, to = fear_violent_rescaled_baseline),
    fear_nonviolent_std_baseline = stdize(fear_nonviolent_rescaled_baseline, to = fear_nonviolent_rescaled_baseline),
    feared_walk_std_baseline = stdize(feared_walk_rescaled_baseline, to = feared_walk_rescaled_baseline),
    # calculate index
    future_insecurity_idx = idx_mean(fear_violent_std, fear_nonviolent_std, feared_walk_std, tx = Z, fe = stations),
    future_insecurity_idx_baseline = idx_mean(fear_violent_std_baseline, fear_nonviolent_std_baseline, feared_walk_std_baseline, tx = Z, fe = stations),
    # restandardize
    future_insecurity_idx = stdize(future_insecurity_idx, to = future_insecurity_idx_baseline),
    future_insecurity_idx_baseline = stdize(future_insecurity_idx_baseline, to = future_insecurity_idx_baseline)
  )

# 2. COMMON ANALYSIS
pak_citizen <- 
  pak_citizen %>%
  mutate(
    # Note: we have dropped fear_nonviolent following committee decision
    # standardize vars
    fear_violent_std_common = stdize(fear_violent, to = fear_violent_rescaled_baseline),
    feared_walk_std_common = stdize(feared_walk_rescaled, to = feared_walk_rescaled_baseline),
    fear_violent_std_common_baseline = stdize(fear_violent_rescaled_baseline, to = fear_violent_rescaled_baseline),
    feared_walk_std_common_baseline = stdize(feared_walk_rescaled_baseline, to = feared_walk_rescaled_baseline),
    # calculate index
    future_insecurity_idx_common = idx_mean(fear_violent_std_common, feared_walk_std_common, tx = Z, fe = stations),
    future_insecurity_idx_common_baseline = idx_mean(fear_violent_std_common_baseline, feared_walk_std_common_baseline, tx = Z, fe = stations),
    # restandardize
    future_insecurity_idx_common = stdize(future_insecurity_idx_common, to = future_insecurity_idx_common_baseline),
    future_insecurity_idx_common_baseline = stdize(future_insecurity_idx_common_baseline, to = future_insecurity_idx_common_baseline)
  )

# 3. List-wise deletion
pak_citizen <-
  pak_citizen %>%
  mutate(
    future_insecurity_idx_listwise = idx_mean_listwise(fear_violent_std_common, feared_walk_std_common),
    future_insecurity_idx_listwise_baseline = idx_mean_listwise(fear_violent_std_common_baseline, feared_walk_std_common_baseline),
    # restandardize
    future_insecurity_idx_listwise = stdize(future_insecurity_idx_listwise, to = future_insecurity_idx_listwise_baseline),
    future_insecurity_idx_listwise_baseline = stdize(future_insecurity_idx_listwise_baseline, to = future_insecurity_idx_listwise_baseline))

#--------------------------------------------------------------------------------------------------------------
# 3. Hypotheses 2: satis_idx
#--------------------------------------------------------------------------------------------------------------
pak_citizen <- 
  pak_citizen %>%
  mutate(
    # standardize vars
    satis_trust_std = stdize(satis_trust, to = satis_trust_baseline),
    satis_general_std = stdize(satis_general, to = satis_general_baseline),
    satis_trust_std_baseline = stdize(satis_trust_baseline, to = satis_trust_baseline),
    satis_general_std_baseline = stdize(satis_general_baseline, to = satis_general_baseline),
    # calculate index
    satis_idx = idx_mean(satis_trust_std, satis_general_std, tx = Z, fe = stations),
    satis_idx_baseline = idx_mean(satis_trust_std_baseline, satis_general_std_baseline, tx = Z, fe = stations),
    # restandardize
    satis_idx = stdize(satis_idx, to = satis_idx_baseline),
    satis_idx_baseline = stdize(satis_idx_baseline, to = satis_idx_baseline)
    
  )

# 3. List-wise deletion
pak_citizen <-
  pak_citizen %>%
  mutate(
    satis_idx_listwise = idx_mean_listwise(satis_trust_std, satis_general_std),
    satis_idx_listwise_baseline = idx_mean_listwise(satis_trust_std_baseline, satis_general_std_baseline),  
    # restandardize
    satis_idx_listwise = stdize(satis_idx_listwise, to = satis_idx_listwise_baseline),
    satis_idx_listwise_baseline = stdize(satis_idx_listwise_baseline, to = satis_idx_listwise_baseline))

#--------------------------------------------------------------------------------------------------------------
# Hypotheses 3(a): officer_attitude_idx
#--------------------------------------------------------------------------------------------------------------
pak_officer <- 
  pak_officer %>% 
  mutate(
    empathy_complaints_rescaled = empathy_complaints - 1,
    empathy_reports_rescaled = empathy_reports - 1,
    empathy_complaints_baseline_rescaled = empathy_complaints_baseline - 1,
    empathy_reports_baseline_rescaled = empathy_reports_baseline - 1,
    
    empathy_complaints_std = stdize(empathy_complaints_rescaled, to = empathy_complaints_baseline_rescaled),
    empathy_reports_std = stdize(empathy_reports_rescaled, to = empathy_reports_baseline_rescaled),
    empathy_complaints_std_baseline = stdize(empathy_complaints_baseline, to = empathy_complaints_baseline_rescaled),
    empathy_reports_std_baseline = stdize(empathy_reports_baseline_rescaled, to = empathy_reports_baseline_rescaled),
    
    empathy_idx = idx_mean_listwise(empathy_complaints_std, empathy_reports_std), #, tx = Z, fe = stations),
    empathy_idx_baseline = idx_mean_listwise(empathy_complaints_std_baseline, empathy_reports_std_baseline), #, tx = Z, fe = stations),
    
    account_pol_matter_std = stdize(account_pol_matter, to = account_pol_matter_baseline),
    account_pol_matter_std_baseline = stdize(account_pol_matter, to = account_pol_matter_baseline),
    
    hypothetical2_punishment_std = stdize(hypothetical2_punishment, to = hypothetical2_punishment_baseline),
    hypothetical3_punishment_std = stdize(hypothetical3_punishment, to = hypothetical3_punishment_baseline),
    hypothetical5_punishment_std = stdize(hypothetical5_punishment, to = hypothetical5_punishment_baseline),
    hypothetical2_punishment_std_baseline = stdize(hypothetical2_punishment_baseline, to = hypothetical2_punishment_baseline),
    hypothetical3_punishment_std_baseline = stdize(hypothetical3_punishment_baseline, to = hypothetical3_punishment_baseline),
    hypothetical5_punishment_std_baseline = stdize(hypothetical5_punishment_baseline, to = hypothetical5_punishment_baseline),
    
    hypothetical2_reportself_std = stdize(hypothetical2_reportself, to = hypothetical2_reportself_baseline),
    hypothetical3_reportself_std = stdize(hypothetical3_reportself, to = hypothetical3_reportself_baseline),
    hypothetical5_reportself_std = stdize(hypothetical5_reportself, to = hypothetical5_reportself_baseline),
    hypothetical2_reportself_std_baseline = stdize(hypothetical2_reportself_baseline, to = hypothetical2_reportself_baseline),
    hypothetical3_reportself_std_baseline = stdize(hypothetical3_reportself_baseline, to = hypothetical3_reportself_baseline),
    hypothetical5_reportself_std_baseline = stdize(hypothetical5_reportself_baseline, to = hypothetical5_reportself_baseline),
    
    hypothetical2_reportothers_std = stdize(hypothetical2_reportothers, to = hypothetical2_reportothers_baseline),
    hypothetical3_reportothers_std = stdize(hypothetical3_reportothers, to = hypothetical3_reportothers_baseline),
    hypothetical5_reportothers_std = stdize(hypothetical5_reportothers, to = hypothetical5_reportothers_baseline),
    hypothetical2_reportothers_std_baseline = stdize(hypothetical2_reportothers_baseline, to = hypothetical2_reportothers_baseline),
    hypothetical3_reportothers_std_baseline = stdize(hypothetical3_reportothers_baseline, to = hypothetical3_reportothers_baseline),
    hypothetical5_reportothers_std_baseline = stdize(hypothetical5_reportothers_baseline, to = hypothetical5_reportothers_baseline),
    
    accountability_idx = idx_mean_listwise(account_pol_matter_std, hypothetical2_reportothers_std, hypothetical3_reportothers_std, hypothetical5_reportothers_std, hypothetical5_reportself_std, hypothetical2_reportself_std, hypothetical3_reportself_std, hypothetical2_punishment_std, hypothetical3_punishment_std, hypothetical5_punishment_std), #, tx = Z, fe = stations),
    accountability_idx_common = idx_mean_listwise(account_pol_matter_std, hypothetical2_reportothers_std, hypothetical3_reportothers_std, hypothetical5_reportothers_std, hypothetical5_reportself_std, hypothetical2_reportself_std, hypothetical3_reportself_std), #, tx = Z, fe = stations),
    accountability_idx_baseline = idx_mean_listwise(account_pol_matter_std_baseline, hypothetical2_reportothers_std_baseline, hypothetical3_reportothers_std_baseline, hypothetical5_reportothers_std_baseline, hypothetical5_reportself_std_baseline, hypothetical2_reportself_std_baseline, hypothetical3_reportself_std_baseline, hypothetical2_punishment_std_baseline, hypothetical3_punishment_std_baseline, hypothetical5_punishment_std_baseline), #, tx = Z, fe = stations),
    accountability_idx_common_baseline = idx_mean_listwise(account_pol_matter_std_baseline, hypothetical2_reportothers_std_baseline, hypothetical3_reportothers_std_baseline, hypothetical5_reportothers_std_baseline, hypothetical5_reportself_std_baseline, hypothetical2_reportself_std_baseline, hypothetical3_reportself_std_baseline), #, tx = Z, fe = stations),
    
    hypothetical5_abuseself_std = stdize(hypothetical5_abuseself, to = hypothetical5_abuseself_baseline),
    hypothetical5_abuseother_std = stdize(hypothetical5_abuseother, to = hypothetical5_abuseother_baseline),
    hypothetical5_abuseself_std_baseline = stdize(hypothetical5_abuseself_baseline, to = hypothetical5_abuseself_baseline),
    hypothetical5_abuseother_std_baseline = stdize(hypothetical5_abuseother_baseline, to = hypothetical5_abuseother_baseline),
    
    abuse_idx = idx_mean_listwise(hypothetical5_abuseself_std, hypothetical5_abuseother_std), #, tx = Z, fe = stations),
    abuse_idx_baseline = idx_mean_listwise(hypothetical5_abuseself_std_baseline, hypothetical5_abuseother_std_baseline), #, tx = Z, fe = stations),
    
    hypothetical2_corruptself_std = stdize(hypothetical2_corruptself, to = hypothetical2_corruptself_baseline),
    hypothetical2_corruptother_std = stdize(hypothetical2_corruptother, to = hypothetical2_corruptother_baseline),
    hypothetical2_corruptself_std_baseline = stdize(hypothetical2_corruptself_baseline, to = hypothetical2_corruptself_baseline),
    hypothetical2_corruptother_std_baseline = stdize(hypothetical2_corruptother_baseline, to = hypothetical2_corruptother_baseline),
    
    hypothetical3_corruptself_std = stdize(hypothetical3_corruptself, to = hypothetical3_corruptself_baseline),
    hypothetical3_corruptother_std = stdize(hypothetical3_corruptother, to = hypothetical3_corruptother_baseline),
    hypothetical3_corruptself_std_baseline = stdize(hypothetical3_corruptself_baseline, to = hypothetical3_corruptself_baseline),
    hypothetical3_corruptother_std_baseline = stdize(hypothetical3_corruptother_baseline, to = hypothetical3_corruptother_baseline),
    
    corrupt_idx = idx_mean_listwise(hypothetical2_corruptself_std, hypothetical2_corruptother_std, hypothetical3_corruptself_std, hypothetical3_corruptother_std), #, tx = Z, fe = stations),
    corrupt_idx_baseline = idx_mean_listwise(hypothetical2_corruptself_std_baseline, hypothetical2_corruptother_std_baseline, hypothetical3_corruptself_std_baseline, hypothetical3_corruptother_std_baseline), #, tx = Z, fe = stations),
    
    officer_attitude_idx = idx_mean_listwise(empathy_idx, corrupt_idx, abuse_idx, accountability_idx), #, tx = Z, fe = stations),
    officer_attitude_idx_baseline = idx_mean_listwise(empathy_idx_baseline, corrupt_idx_baseline, abuse_idx_baseline, accountability_idx_baseline), #, tx = Z, fe = stations)
    # restandardize
    officer_attitude_idx = stdize(officer_attitude_idx, to = officer_attitude_idx_baseline),
    officer_attitude_idx_baseline = stdize(officer_attitude_idx_baseline, to = officer_attitude_idx_baseline)
  )

# --------------------------------------------------------------------------------------------------------------
                        # 4. Hypotheses 3(b): police_abuse_idx
# --------------------------------------------------------------------------------------------------------------
pak_citizen <-
  pak_citizen %>%
  mutate(
    # general police abuse
    policeabuse_any = if_else(policeabuse_verbal_any > 0 | policeabuse_phys_any > 0, 1, 0),
    policeabuse_any_baseline = if_else(policeabuse_verbal_any_baseline > 0 | policeabuse_phys_any_baseline > 0, 1, 0)) %>% 
  rowwise() %>% 
  mutate(
    policeabuse_num = sum(policeabuse_verbal_num, policeabuse_phys_num, na.rm = TRUE),
    policeabuse_num_baseline = sum(policeabuse_verbal_num_baseline, policeabuse_phys_num_baseline, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(
    # bribe amount is divided by the rate of dollar (GH issue number 49)
    bribe_amt_raw = bribe_amt,
    bribe_amt_raw_baseline = bribe_amt_baseline,
    bribe_amt = bribe_amt / 155,
    bribe_amt_baseline = bribe_amt_baseline / 155,
    # different construction that the M-PAP (GH issues number 31)
    bribe_amt = if_else(bribe_freq == 1, 0, bribe_amt),
    bribe_amt_baseline = if_else(bribe_freq_baseline == 1, 0, bribe_amt_baseline),
    # standardize vars
    policeabuse_any_std = stdize(policeabuse_any, to = policeabuse_any_baseline),
    policeabuse_num_std = stdize(policeabuse_num, to = policeabuse_num_baseline),
    bribe_freq_std = stdize(bribe_freq, to = bribe_freq_baseline),
    bribe_amt_std = stdize(bribe_amt, to = bribe_amt_baseline),
    policeabuse_any_std_baseline = stdize(policeabuse_any_baseline, to = policeabuse_any_baseline),
    policeabuse_num_std_baseline = stdize(policeabuse_num_baseline, to = policeabuse_num_baseline),
    bribe_freq_std_baseline = stdize(bribe_freq_baseline, to = bribe_freq_baseline),
    bribe_amt_std_baseline = stdize(bribe_amt_baseline, to = bribe_amt_baseline),
    # calculate index
    police_abuse_idx = idx_mean(policeabuse_any_std, policeabuse_num_std, bribe_freq_std, bribe_amt_std, tx = Z, fe = stations),
    police_abuse_idx_baseline = idx_mean(policeabuse_any_std_baseline, policeabuse_num_std_baseline, bribe_freq_std_baseline, bribe_amt_std_baseline, tx = Z, fe = stations),
    # re-standardize
    police_abuse_idx = stdize(police_abuse_idx, to = police_abuse_idx_baseline),
    police_abuse_idx_baseline = stdize(police_abuse_idx_baseline, to = police_abuse_idx_baseline)
  )

# 2. Common analysis
pak_citizen <-
  pak_citizen %>%
  mutate(
    police_abuse_idx_common = idx_mean(policeabuse_any_std, bribe_freq_std, bribe_amt_std, tx = Z, fe = stations),
    police_abuse_idx_common_baseline = idx_mean(policeabuse_any_std_baseline, bribe_freq_std_baseline, bribe_amt_std_baseline, tx = Z, fe = stations),
    # re-standardize
    police_abuse_idx_common = stdize(police_abuse_idx_common, to = police_abuse_idx_common_baseline),
    police_abuse_idx_common_baseline = stdize(police_abuse_idx_common_baseline, to = police_abuse_idx_common_baseline)
    
    )

# 3. List-wise deletion
pak_citizen <-
  pak_citizen %>%
  mutate(
    police_abuse_idx_listwise = idx_mean_listwise(policeabuse_any_std, bribe_freq_std, bribe_amt_std),
    police_abuse_idx_listwise_baseline = idx_mean_listwise(policeabuse_any_std_baseline, bribe_freq_std_baseline, bribe_amt_std_baseline),
    # re-standardize
    police_abuse_idx_listwise = stdize(police_abuse_idx_listwise, to = police_abuse_idx_listwise_baseline),
    police_abuse_idx_listwise_baseline = stdize(police_abuse_idx_listwise_baseline, to = police_abuse_idx_listwise_baseline))



#--------------------------------------------------------------------------------------------------------------
#                     # 5. Hypotheses 4(a): crime_reporting_idx
#--------------------------------------------------------------------------------------------------------------
pak_citizen <- pak_citizen %>%
  mutate(
    # generate crime reporting categories
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
    
    armedrob_report_baseline = case_when(
      armedrob_num_baseline == 0 | armedrob_report_baseline == 0 ~ 0,
      armedrob_num_baseline > 0 & armedrob_report_baseline == 1 ~ 1),
    burglary_report_baseline = case_when(
      burglary_num_baseline == 0 | burglary_report_baseline == 0 ~ 0,
      burglary_num_baseline > 0 & burglary_report_baseline == 1 ~ 1),
    simpleassault_report_baseline = case_when(
      simpleassault_num_baseline == 0 | simpleassault_report_baseline == 0 ~ 0,
      simpleassault_num_baseline > 0 & simpleassault_report_baseline == 1 ~ 1),
    carmedrob_report = case_when(
      carmedrob_num_baseline == 0 | carmedrob_report_baseline == 0 ~ 0,
      carmedrob_num_baseline > 0 & carmedrob_report_baseline == 1 ~ 1),
    cburglary_report_baseline = case_when(
      cburglary_num_baseline == 0 | cburglary_report_baseline == 0 ~ 0,
      cburglary_num_baseline > 0 & cburglary_report_baseline == 1 ~ 1),
    csimpleassault_report_baseline = case_when(
      csimpleassault_num_baseline == 0 | csimpleassault_report_baseline == 0 ~ 0,
      csimpleassault_num_baseline > 0 & csimpleassault_report_baseline == 1 ~ 1),
    csexual_report_baseline = case_when(
      csexual_num_baseline == 0 | csexual_report_baseline == 0 ~ 0,
      csexual_num_baseline > 0 & csexual_report_baseline == 1 ~ 1),
    cdomestic_phys_report_baseline = case_when(
      cdomestic_phys_num_baseline == 0 | cdomestic_phys_report_baseline == 0 ~ 0,
      cdomestic_phys_num_baseline > 0 & cdomestic_phys_report_baseline == 1 ~ 1),
    cmurder_report_baseline = case_when(
      cmurder_num_baseline == 0 | cmurder_report_baseline == 0 ~ 0,
      cmurder_num_baseline > 0 & cmurder_report_baseline == 1 ~ 1),
    
    burglaryres = case_when(
      burglaryres == 1 ~ 1L,
      burglaryres == 2 ~ 1L,
      !is.na(burglaryres) ~ 0L),
    dviolres = case_when(
      dviolres == 1 ~ 1L,
      dviolres == 2 ~ 1L,
      !is.na(dviolres) ~ 0L),
    armedrobres = case_when(
      armedrobres == 1 ~ 1L,
      armedrobres == 2 ~ 1L,
      !is.na(armedrobres) ~ 0L),
    
    burglaryres_baseline = case_when(
      burglaryres_baseline == 1 ~ 1L,
      burglaryres_baseline == 2 ~ 1L,
      !is.na(burglaryres_baseline) ~ 0L),
    dviolres_baseline = case_when(
      dviolres_baseline == 1 ~ 1L,
      dviolres_baseline == 2 ~ 1L,
      !is.na(dviolres_baseline) ~ 0L),
    armedrobres_baseline = case_when(
      armedrobres_baseline == 1 ~ 1L,
      armedrobres_baseline == 2 ~ 1L,
      !is.na(armedrobres_baseline) ~ 0L)) %>% 
  rowwise() %>% 
  mutate(
    # calculate crime categories
    violentcrime_report_num = sum(armedrob_report, simpleassault_report, other_report_violent, na.rm = TRUE),
    nonviolentcrime_report_num = sum(burglary_report, 
                                     # other_report_nonviolent, 
                                     na.rm = TRUE),
    cviolentcrime_report_num = sum(carmedrob_report, csimpleassault_report, csexual_report, cdomestic_phys_report, cmurder_report, cother_report_violent, na.rm = TRUE),
    cnonviolentcrime_report_num = sum(cburglary_report, 
                                      # cother_report_nonviolent, 
                                      na.rm = TRUE),
    violentcrime_report_num_baseline = sum(armedrob_report_baseline, simpleassault_report_baseline, other_report_violent_baseline, na.rm = TRUE),
    nonviolentcrime_report_num_baseline = sum(burglary_report_baseline, 
                                              # other_report_nonviolent_baseline, 
                                              na.rm = TRUE),
    cviolentcrime_report_num_baseline = sum(carmedrob_report_baseline, csimpleassault_report_baseline, csexual_report_baseline, cdomestic_phys_report_baseline, 
                                            # cmurder_report_baseline, # cmurder_baseline was not collected as per mpap
                                            cother_report_violent_baseline, na.rm = TRUE),
    cnonviolentcrime_report_num_baseline = sum(cburglary_report_baseline, 
                                               # cother_report_nonviolent_baseline, 
                                               na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(
    # standarduze all vars
    burglaryres_std = stdize(burglaryres, to = burglaryres_baseline),
    dviolres_std = stdize(dviolres, to = dviolres_baseline),
    armedrobres_std = stdize(armedrobres, to = armedrobres_baseline),
    violentcrime_report_num_std = stdize(violentcrime_report_num, to = violentcrime_report_num_baseline),
    nonviolentcrime_report_num_std = stdize(nonviolentcrime_report_num, to = nonviolentcrime_report_num_baseline),
    cviolentcrime_report_num_std = stdize(cviolentcrime_report_num, to = cviolentcrime_report_num_baseline),
    
    burglaryres_std_baseline = stdize(burglaryres_baseline, to = burglaryres_baseline),
    dviolres_std_baseline = stdize(dviolres_baseline, to = dviolres_baseline),
    armedrobres_std_baseline = stdize(armedrobres_baseline, to = armedrobres_baseline),
    cnonviolentcrime_report_num_std = stdize(cnonviolentcrime_report_num, to = cnonviolentcrime_report_num_baseline),
    violentcrime_report_num_std_baseline = stdize(violentcrime_report_num_baseline, to = violentcrime_report_num_baseline),
    nonviolentcrime_report_num_std_baseline = stdize(nonviolentcrime_report_num_baseline, to = nonviolentcrime_report_num_baseline),
    cviolentcrime_report_num_std_baseline = stdize(cviolentcrime_report_num_baseline, to = cviolentcrime_report_num_baseline),
    cnonviolentcrime_report_num_std_baseline = stdize(cnonviolentcrime_report_num_baseline, to = cnonviolentcrime_report_num_baseline),
    # standardize individual crimes' reporting
    armedrob_report_std = stdize(armedrob_report, to = armedrob_report_baseline),
    simpleassault_report_std = stdize(simpleassault_report, to = simpleassault_report_baseline),
    other_report_violent_std = stdize(other_report_violent, to = other_report_violent_baseline),
    burglary_report_std = stdize(burglary_report, to = burglary_report_baseline),
    carmedrob_report_std = stdize(carmedrob_report, to = carmedrob_report_baseline),
    csimpleassault_report_std = stdize(csimpleassault_report, to = csimpleassault_report_baseline),
    csexual_report_std = stdize(csexual_report, to = csexual_report_baseline),
    cdomestic_phys_report_std = stdize(cdomestic_phys_report, to = cdomestic_phys_report_baseline),
    cother_report_violent_std = stdize(cother_report_violent, to = cother_report_violent_baseline),
    cburglary_report_std = stdize(cburglary_report, to = cburglary_report_baseline),
    
    armedrob_report_std_baseline = stdize(armedrob_report_baseline, to = armedrob_report_baseline),
    simpleassault_report_std_baseline = stdize(simpleassault_report_baseline, to = simpleassault_report_baseline),
    other_report_violent_std_baseline = stdize(other_report_violent_baseline, to = other_report_violent_baseline),
    burglary_report_std_baseline = stdize(burglary_report_baseline, to = burglary_report_baseline),
    carmedrob_report_std_baseline = stdize(carmedrob_report_baseline, to = carmedrob_report_baseline),
    csimpleassault_report_std_baseline = stdize(csimpleassault_report_baseline, to = csimpleassault_report_baseline),
    csexual_report_std_baseline = stdize(csexual_report_baseline, to = csexual_report_baseline),
    cdomestic_phys_report_std_baseline = stdize(cdomestic_phys_report_baseline, to = cdomestic_phys_report_baseline),
    cother_report_violent_std_baseline = stdize(cother_report_violent_baseline, to = cother_report_violent_baseline),
    cburglary_report_std_baseline = stdize(cburglary_report_baseline, to = cburglary_report_baseline),
    # calculate sub-indices
    crimeres_idx = idx_mean(burglaryres_std, dviolres_std, armedrobres_std, tx = Z, fe = stations),
    crimeres_idx_baseline = idx_mean(burglaryres_std_baseline, dviolres_std_baseline, armedrobres_std_baseline, tx = Z, fe = stations),
    # restandardize
    crimeres_idx = stdize(crimeres_idx, to = crimeres_idx_baseline),
    crimeres_idx_baseline = stdize(crimeres_idx_baseline, to = crimeres_idx_baseline),
    # calculate indices
    crime_reporting_idx = idx_mean(violentcrime_report_num_std, nonviolentcrime_report_num_std, cviolentcrime_report_num_std, cnonviolentcrime_report_num_std, crimeres_idx, tx = Z, fe = stations),
    crime_reporting_idx_baseline = idx_mean(violentcrime_report_num_std_baseline, nonviolentcrime_report_num_std_baseline, cviolentcrime_report_num_std_baseline, cnonviolentcrime_report_num_std_baseline, crimeres_idx_baseline, tx = Z, fe = stations),
    # restandardize
    crime_reporting_idx = stdize(crime_reporting_idx, to = crime_reporting_idx_baseline), 
    crime_reporting_idx_baseline = stdize(crime_reporting_idx_baseline, to = crime_reporting_idx_baseline)
  )

# 2. common analysis
pak_citizen <- 
  pak_citizen %>% 
  rowwise() %>% 
  mutate(
    violentcrime_report_num_common = sum(armedrob_report, simpleassault_report, na.rm = TRUE),
    nonviolentcrime_report_num_common = burglary_report,
    cviolentcrime_report_num_common = sum(carmedrob_report, csimpleassault_report, csexual_report, cdomestic_phys_report, na.rm = TRUE),
    cnonviolentcrime_report_num_common = cburglary_report,
    
    violentcrime_report_num_common_baseline = sum(armedrob_report_baseline, simpleassault_report_baseline, na.rm = TRUE),
    nonviolentcrime_report_num_common_baseline = burglary_report_baseline,
    cviolentcrime_report_num_common_baseline = sum(carmedrob_report_baseline, csimpleassault_report_baseline, csexual_report_baseline, cdomestic_phys_report_baseline, na.rm = TRUE),
    cnonviolentcrime_report_num_common_baseline = cburglary_report_baseline) %>%
  ungroup() %>%
  mutate(    
    # standarduze all vars
    violentcrime_report_num_common_std = stdize(violentcrime_report_num, to = violentcrime_report_num_baseline),
    nonviolentcrime_report_num_common_std = stdize(nonviolentcrime_report_num, to = nonviolentcrime_report_num_baseline),
    cviolentcrime_report_num_common_std = stdize(cviolentcrime_report_num, to = cviolentcrime_report_num_baseline),
    cnonviolentcrime_report_num_common_std = stdize(cnonviolentcrime_report_num, to = cnonviolentcrime_report_num_baseline),
    
    violentcrime_report_num_common_std_baseline = stdize(violentcrime_report_num_baseline, to = violentcrime_report_num_baseline),
    nonviolentcrime_report_num_common_std_baseline = stdize(nonviolentcrime_report_num_baseline, to = nonviolentcrime_report_num_baseline),
    cviolentcrime_report_num_common_std_baseline = stdize(cviolentcrime_report_num_baseline, to = cviolentcrime_report_num_baseline),
    cnonviolentcrime_report_num_common_std_baseline = stdize(cnonviolentcrime_report_num_baseline, to = cnonviolentcrime_report_num_baseline),
    # calculate sub-index
    crimeres_idx_common = idx_mean(burglaryres_std, dviolres_std, tx = Z, fe = stations),
    crimeres_idx_common_baseline = idx_mean(burglaryres_std_baseline, dviolres_std_baseline, tx = Z, fe = stations),
    # restandardize
    crimeres_idx_common = stdize(crimeres_idx_common, to = crimeres_idx_common_baseline),
    crimeres_idx_common_baseline = stdize(crimeres_idx_common, to = crimeres_idx_common_baseline),
    # calculate index
    crime_reporting_idx_common = idx_mean(violentcrime_report_num_common_std, nonviolentcrime_report_num_common_std, cviolentcrime_report_num_common_std, cnonviolentcrime_report_num_common_std, crimeres_idx_common, tx = Z, fe = stations),
    crime_reporting_idx_common_baseline = idx_mean(violentcrime_report_num_common_std_baseline, nonviolentcrime_report_num_common_std_baseline, cviolentcrime_report_num_common_std_baseline, cnonviolentcrime_report_num_common_std_baseline, crimeres_idx_common_baseline, tx = Z, fe = stations),
    # restandardize
    crime_reporting_idx_common = stdize(crime_reporting_idx_common, to = crime_reporting_idx_common_baseline), 
    crime_reporting_idx_common_baseline = stdize(crime_reporting_idx_common_baseline, to = crime_reporting_idx_common_baseline)
    
  )

# 3. List-wise deletion
pak_citizen <-
  pak_citizen %>%
  mutate(
    # calculate sub-index
    crimeres_idx_listwise = idx_mean_listwise(burglaryres_std, dviolres_std),
    crimeres_idx_listwise_baseline = idx_mean_listwise(burglaryres_std_baseline, dviolres_std_baseline),
    # restandardize
    crimeres_idx_listwise = stdize(crimeres_idx_listwise, to = crimeres_idx_listwise_baseline),
    crimeres_idx_listwise_baseline = stdize(crimeres_idx_listwise, to = crimeres_idx_listwise_baseline),
    # calculate index
    crime_reporting_idx_listwise = idx_mean_listwise(violentcrime_report_num_common_std, nonviolentcrime_report_num_common_std, cviolentcrime_report_num_common_std, cnonviolentcrime_report_num_common_std, crimeres_idx_common),
    crime_reporting_idx_listwise_baseline = idx_mean_listwise(violentcrime_report_num_common_std_baseline, nonviolentcrime_report_num_common_std_baseline, cviolentcrime_report_num_common_std_baseline, cnonviolentcrime_report_num_common_std_baseline, crimeres_idx_common_baseline),
    # restandardize
    crime_reporting_idx_listwise = stdize(crime_reporting_idx_listwise, to = crime_reporting_idx_listwise_baseline), 
    crime_reporting_idx_listwise_baseline = stdize(crime_reporting_idx_listwise_baseline, to = crime_reporting_idx_listwise_baseline)
  )


#--------------------------------------------------------------------------------------------------------------
# 7. Hypotheses 4(b): tips_idx
#--------------------------------------------------------------------------------------------------------------
pak_citizen <- 
  pak_citizen %>%
  mutate(
    # standardize vars
    contact_pol_susp_activity_std = stdize(contact_pol_susp_activity, to = contact_pol_susp_activity_baseline),
    give_info_pol_investigation_std = stdize(give_info_pol_investigation, to = give_info_pol_investigation_baseline),
    contact_pol_susp_activity_std_baseline = stdize(contact_pol_susp_activity_baseline, to = contact_pol_susp_activity_baseline),
    give_info_pol_investigation_std_baseline = stdize(give_info_pol_investigation_baseline, to = give_info_pol_investigation_baseline),
    # generate index
    crime_tips_idx = idx_mean(contact_pol_susp_activity_std, give_info_pol_investigation_std, tx = Z, fe = stations),
    crime_tips_idx_baseline = idx_mean(contact_pol_susp_activity_std_baseline, give_info_pol_investigation_std_baseline, tx = Z, fe = stations),
    # restandardize
    crime_tips_idx = stdize(crime_tips_idx, to = crime_tips_idx_baseline),
    crime_tips_idx_baseline = stdize(crime_tips_idx_baseline, to = crime_tips_idx_baseline),
    
    # make names consistent with mpap
    tips_idx = crime_tips_idx,
    tips_idx_baseline = crime_tips_idx_baseline
  )


# 3. List-wise deletion
pak_citizen <-
  pak_citizen %>%
  mutate(
    crime_tips_idx_listwise = idx_mean_listwise(contact_pol_susp_activity_std, give_info_pol_investigation_std),
    crime_tips_idx_listwise_baseline = idx_mean_listwise(contact_pol_susp_activity_std_baseline, give_info_pol_investigation_std_baseline),
    # restandardize
    crime_tips_idx_listwise = stdize(crime_tips_idx_listwise, to = crime_tips_idx_listwise_baseline),
    crime_tips_idx_listwise_baseline = stdize(crime_tips_idx_listwise_baseline, to = crime_tips_idx_listwise_baseline),
    # make names consistent with mpap
    tips_idx_listwise = crime_tips_idx_listwise,
    tips_idx_listwise_baseline = crime_tips_idx_listwise_baseline)



#--------------------------------------------------------------------------------------------------------------
# 8. Hypotheses 4(c): police_abuse_report_idx
#--------------------------------------------------------------------------------------------------------------
pak_citizen <-
  pak_citizen %>%
  mutate(
    # general police abuse components
    policeabuse_report = case_when(
      policeabuse_verbal_report == 1 | policeabuse_phys_report == 1 ~ 1L,
      !is.na(policeabuse_verbal_report) & !is.na(policeabuse_phys_report) ~ 0L), 
    policeabuse_report_baseline = case_when(
      policeabuse_verbal_report_baseline == 1 |
        policeabuse_phys_report_baseline == 1 ~ 1L,
      !is.na(policeabuse_verbal_report_baseline) & !is.na(policeabuse_phys_report_baseline) ~ 0L), 
    # standardize vars
    policeabuse_report_std = stdize(policeabuse_report, to = policeabuse_report_baseline),
    dutydrink_report_std = stdize(dutydrink_report, to = dutydrink_report_baseline),
    policebeating_report_std = stdize(policebeating_report, to = policebeating_report_baseline),
    policeabuse_report_std_baseline = stdize(policeabuse_report_baseline, to = policeabuse_report_baseline),
    dutydrink_report_std_baseline = stdize(dutydrink_report_baseline, to = dutydrink_report_baseline),
    policebeating_report_std_baseline = stdize(policebeating_report_baseline, to = policebeating_report_baseline),
    # calculate index
    police_abuse_report_idx = idx_mean(policeabuse_report_std, dutydrink_report_std, policebeating_report_std, tx = Z, fe = stations),
    police_abuse_report_idx_baseline = idx_mean(policeabuse_report_std_baseline, dutydrink_report_std_baseline, policebeating_report_std_baseline, tx = Z, fe = stations),
    # restandardize
    police_abuse_report_idx = stdize(police_abuse_report_idx, to = police_abuse_report_idx_baseline),
    police_abuse_report_idx_baseline = stdize(police_abuse_report_idx_baseline, to = police_abuse_report_idx_baseline)
    
  )

# 2. Common analysis
pak_citizen <- 
  pak_citizen %>%
  mutate(
    # general police abuse components
    policeabuse_report_common = case_when(
      policeabuse_verbal_report == 1 ~ 1L,
      !is.na(policeabuse_verbal_report) ~ 0L), 
    policeabuse_report_common_baseline = case_when(
      policeabuse_verbal_report_baseline == 1 ~ 1L,
      !is.na(policeabuse_verbal_report_baseline) ~ 0L), 
    # standardize vars
    policeabuse_report_common_std = stdize(policeabuse_report_common, to = policeabuse_report_common_baseline),
    policeabuse_report_common_std_baseline = stdize(policeabuse_report_common_baseline, to = policeabuse_report_common_baseline),
    # calculate index
    police_abuse_report_idx_common = idx_mean(policeabuse_report_common_std, policebeating_report_std, tx = Z, fe = stations),
    police_abuse_report_idx_common_baseline = idx_mean(policeabuse_report_common_std_baseline, policebeating_report_std_baseline, tx = Z, fe = stations),
    # restandardize
    police_abuse_report_idx_common = stdize(police_abuse_report_idx_common, to = police_abuse_report_idx_common_baseline),
    police_abuse_report_idx_common_baseline = stdize(police_abuse_report_idx_common_baseline, to = police_abuse_report_idx_common_baseline)
  )


# 3. List-wise deletion
pak_citizen <-
  pak_citizen %>%
  mutate(
    police_abuse_report_idx_listwise = idx_mean_listwise(policeabuse_report_common_std, policebeating_report_std),
    police_abuse_report_idx_listwise_baseline = idx_mean_listwise(policeabuse_report_common_std_baseline, policebeating_report_std_baseline),
    # restandardize
    police_abuse_report_idx_listwise = stdize(police_abuse_report_idx_listwise, to = police_abuse_report_idx_listwise_baseline),
    police_abuse_report_idx_listwise_baseline = stdize(police_abuse_report_idx_listwise_baseline, to = police_abuse_report_idx_listwise_baseline))


#--------------------------------------------------------------------------------------------------------------
# 8. Hypotheses M1a: intentions_idx
#--------------------------------------------------------------------------------------------------------------
pak_citizen <-
  pak_citizen %>%
  mutate(
    # reverse order of variables to be consistent with the direction of the MPAP
    polcaseserious_raw = polcaseserious,
    polcaseserious_rescaled = (polcaseserious - 1),
    polcaseserious_raw_baseline = polcaseserious_baseline,
    polcaseserious_rescaled_baseline = (polcaseserious_baseline - 1),
    
    polcasefair_raw = polcasefair,
    polcasefair_rescaled = (polcasefair - 1),
    polcasefair_raw_baseline = polcasefair_baseline,
    polcasefair_rescaled_baseline = (polcasefair_baseline - 1),
    
    polint_quality_raw = polint_quality,
    polint_quality_rescaled = (polint_quality - 1),
    polint_quality_raw = polint_quality,
    polint_quality_rescaled_baseline = (polint_quality_baseline - 1),
    
    polint_corrupt_raw = polint_corrupt,
    polint_corrupt_rescaled = 4 - (polint_corrupt - 1),
    polint_corrupt_raw_baseline = polint_corrupt_baseline,
    polint_corrupt_rescaled_baseline = 4 - (polint_corrupt_baseline - 1),
    # standardize vars
    polint_corrupt_std = stdize(polint_corrupt_rescaled, to = polint_corrupt_rescaled_baseline),
    polint_quality_std = stdize(polint_quality_rescaled, to = polint_quality_rescaled_baseline),
    polint_corrupt_std_baseline = stdize(polint_corrupt_rescaled_baseline, to = polint_corrupt_rescaled_baseline),
    polint_quality_std_baseline = stdize(polint_quality_rescaled_baseline, to = polint_quality_rescaled_baseline),
    
    polcaseserious_std = stdize(polcaseserious_rescaled, to = polcaseserious_rescaled_baseline),
    polcasefair_std = stdize(polcasefair_rescaled, to = polcasefair_rescaled_baseline),
    polcaseserious_std_baseline = stdize(polcaseserious_rescaled_baseline, to = polcaseserious_rescaled_baseline),
    polcasefair_std_baseline = stdize(polcasefair_rescaled_baseline, to = polcasefair_rescaled_baseline), 
    # construct sub-indices
    polint_idx = idx_mean(polint_corrupt_std, polint_quality_std, tx = Z, fe = stations),
    polint_idx_baseline = idx_mean(polint_corrupt_std_baseline, polint_quality_std_baseline, tx = Z, fe = stations),
    # construct index
    intentions_idx = idx_mean(polint_idx, polcaseserious_std, polcasefair_std, tx = Z, fe = stations),
    intentions_idx_baseline = idx_mean(polint_idx_baseline, polcaseserious_std_baseline, polcasefair_std_baseline, tx = Z, fe = stations),
    # restandardize
    intentions_idx = stdize(intentions_idx, to = intentions_idx_baseline),
    intentions_idx_baseline = stdize(intentions_idx_baseline, to = intentions_idx_baseline)
    
  )

# 3. List-wise deletion
pak_citizen <-
  pak_citizen %>%
  mutate(
    polint_idx_listwise = idx_mean_listwise(polint_corrupt_std, polint_quality_std),
    polint_idx_listwise_baseline = idx_mean_listwise(polint_corrupt_std_baseline, polint_quality_std_baseline),
    intentions_idx_listwise = idx_mean_listwise(polint_idx, polcaseserious_std, polcasefair_std),
    intentions_idx_listwise_baseline = idx_mean_listwise(polint_idx_baseline, polcaseserious_std_baseline, polcasefair_std_baseline),
    # restandardize
    intentions_idx_listwise = stdize(intentions_idx_listwise, to = intentions_idx_listwise_baseline),
    intentions_idx_listwise_baseline = stdize(intentions_idx_listwise_baseline, to = intentions_idx_listwise_baseline))

#--------------------------------------------------------------------------------------------------------------
# 9. Hypotheses M1b: know_idx
#--------------------------------------------------------------------------------------------------------------
pak_citizen <-
  pak_citizen %>%
  mutate(
    # standardize vars
    know_law_suspect_std = stdize(know_law_suspect, to = know_law_suspect_baseline),
    know_law_lawyer_std = stdize(know_law_lawyer, to = know_law_lawyer_baseline),
    know_law_fees_std = stdize(know_law_fees, to = know_law_fees_baseline),
    know_law_vaw_std = stdize(know_law_vaw, to = know_law_vaw_baseline),
    know_report_station_std = stdize(know_report_station, to = know_report_station_baseline),
    know_report_followup_std = stdize(know_report_followup, to = know_report_followup_baseline),
    
    know_law_suspect_std_baseline = stdize(know_law_suspect_baseline, to = know_law_suspect_baseline),
    know_law_lawyer_std_baseline = stdize(know_law_lawyer_baseline, to = know_law_lawyer_baseline),
    know_law_fees_std_baseline = stdize(know_law_fees_baseline, to = know_law_fees_baseline),
    know_law_vaw_baseline_std = stdize(know_law_vaw_baseline, to = know_law_vaw_baseline),
    know_report_station_std_baseline = stdize(know_report_station_baseline, to = know_report_station_baseline),
    know_report_followup_std_baseline = stdize(know_report_followup_baseline, to = know_report_followup_baseline),
    
    # construct sub-index
    know_law_idx = idx_mean(know_law_suspect_std, know_law_lawyer_std, know_law_fees_std, know_law_vaw_std, tx = Z, fe = stations),
    know_law_idx_baseline = idx_mean(know_law_suspect_std_baseline, know_law_lawyer_std_baseline, know_law_fees_std_baseline, know_law_vaw_baseline_std, tx = Z, fe = stations),
    know_report_idx = idx_mean(know_report_followup_std, know_report_station_std, tx = Z, fe = stations),
    know_report_idx_baseline = idx_mean(know_report_followup_std_baseline, know_report_station_std_baseline, tx = Z, fe = stations),
    # construct index
    know_idx = idx_mean(know_law_idx, know_report_idx, tx = Z, fe = stations),
    know_idx_baseline = idx_mean(know_law_idx_baseline, know_report_idx_baseline, tx = Z, fe = stations),
    # restandardize
    know_idx = stdize(know_idx, to = know_idx_baseline),
    know_idx_baseline = stdize(know_idx_baseline, to = know_idx_baseline)
    
  )

# 2. common
pak_citizen <-
  pak_citizen %>%
  mutate(
    # calculate sub-index
    know_law_idx_common = idx_mean(know_law_suspect_std, know_law_lawyer_std, know_law_fees_std, tx = Z, fe = stations),
    know_law_idx_common_baseline = idx_mean(know_law_suspect_std_baseline, know_law_lawyer_std_baseline, know_law_fees_std_baseline, tx = Z, fe = stations),
    # calculate index
    know_idx_common = idx_mean(know_law_idx_common, know_report_idx, tx = Z, fe = stations),
    know_idx_common_baseline = idx_mean(know_law_idx_common_baseline, know_report_idx_baseline, tx = Z, fe = stations),
    # restandardize
    know_idx_common = stdize(know_idx_common, to = know_idx_common_baseline),
    know_idx_common_baseline = stdize(know_idx_common_baseline, to = know_idx_common_baseline)
  )

# 3. List-wise deletion
pak_citizen <-
  pak_citizen %>%
  mutate(
    know_report_idx_listwise = know_report_station_std,
    know_report_idx_listwise_baseline = know_report_station_std_baseline,
    
    know_law_idx_listwise = idx_mean_listwise(know_law_suspect_std, know_law_lawyer_std, know_law_fees_std),
    know_law_idx_listwise_baseline = idx_mean_listwise(know_law_suspect_std_baseline, know_law_lawyer_std_baseline, know_law_fees_std_baseline),
    
    know_idx_listwise = idx_mean_listwise(know_law_idx, know_report_station_std),
    know_idx_listwise_baseline = idx_mean_listwise(know_law_idx, know_report_station_std),
    
    # restandardize
    know_idx_listwise = stdize(know_idx_listwise, to = know_idx_listwise_baseline),
    know_idx_listwise_baseline = stdize(know_idx_listwise_baseline, to = know_idx_listwise_baseline)
    
    )

#--------------------------------------------------------------------------------------------------------------
# 10. Hypotheses M1c: norm_idx
#--------------------------------------------------------------------------------------------------------------
pak_citizen <- 
  pak_citizen %>%
  mutate(
    # reverse order of variables to be consistent with the direction of the MPAP (MPAP page 75-76)
    reportnorm_theft_rescaled = 5 - reportnorm_theft,
    reportnorm_abuse_rescaled = 5 - reportnorm_abuse,
    obeynorm_rescaled = obeynorm - 1, # refer issue to 84
    reportnorm_theft_rescaled_baseline = 5 - reportnorm_theft_baseline,
    reportnorm_abuse_rescaled_baseline = 5 - reportnorm_abuse_baseline,
    obeynorm_rescaled_baseline = obeynorm_baseline - 1,
    # standardize the vars
    reportnorm_theft_std = stdize(reportnorm_theft_rescaled, to = reportnorm_theft_rescaled_baseline),
    reportnorm_abuse_std = stdize(reportnorm_abuse_rescaled, to = reportnorm_abuse_rescaled_baseline),
    obeynorm_std = stdize(obeynorm_rescaled, to = obeynorm_rescaled_baseline),
    reportnorm_theft_std_baseline = stdize(reportnorm_theft_rescaled_baseline, to = reportnorm_theft_rescaled_baseline),
    reportnorm_abuse_std_baseline = stdize(reportnorm_abuse_rescaled_baseline, to = reportnorm_abuse_rescaled_baseline),
    obeynorm_std_baseline = stdize(obeynorm_baseline, to = obeynorm_baseline),
    # calculate index
    norm_idx = idx_mean(reportnorm_theft_std, reportnorm_abuse_std, obeynorm_std, tx = Z, fe = stations),
    norm_idx_baseline = idx_mean(reportnorm_theft_std_baseline, reportnorm_abuse_std_baseline, obeynorm_std_baseline, tx = Z, fe = stations),
    # restandardize
    norm_idx = stdize(norm_idx, to = norm_idx_baseline),
    norm_idx_baseline = stdize(norm_idx_baseline, to = norm_idx_baseline)
    
  )

# 3. List-wise deletion
pak_citizen <-
  pak_citizen %>%
  mutate(
    norm_idx_listwise = idx_mean_listwise(reportnorm_theft_std, reportnorm_abuse_std, obeynorm_std),
    norm_idx_listwise_baseline = idx_mean_listwise(reportnorm_theft_std_baseline, reportnorm_abuse_std_baseline, obeynorm_std_baseline),
    # restandardize
    norm_idx_listwise = stdize(norm_idx_listwise, to = norm_idx_listwise_baseline),
    norm_idx_listwise_baseline = stdize(norm_idx_listwise_baseline, to = norm_idx_listwise_baseline)
  )

#--------------------------------------------------------------------------------------------------------------
                        # 11. Hypotheses M2a: police_capacity_idx
#--------------------------------------------------------------------------------------------------------------
pak_citizen <- 
  pak_citizen %>%
  mutate(
    # standardize the vars
    polcap_timely_std = stdize(polcap_timely, to = polcap_timely_baseline),
    polcap_investigate_std = stdize(polcap_investigate, to = polcap_investigate_baseline),
    polcap_timely_std_baseline = stdize(polcap_timely_baseline, to = polcap_timely_baseline),
    polcap_investigate_std_baseline = stdize(polcap_investigate_baseline, to = polcap_investigate_baseline),
    # calculate index
    police_capacity_idx = idx_mean(polcap_timely_std, polcap_investigate_std, tx = Z, fe = stations),
    police_capacity_idx_baseline = idx_mean(polcap_timely_std_baseline, polcap_investigate_std_baseline, tx = Z, fe = stations),
    # restandardize
    police_capacity_idx = stdize(police_capacity_idx, to = police_capacity_idx_baseline),
    police_capacity_idx_baseline = stdize(police_capacity_idx_baseline, to = police_capacity_idx_baseline)
  )

# 3. List-wise deletion
pak_citizen <-
  pak_citizen %>%
  mutate(
    police_capacity_idx_listwise = idx_mean_listwise(polcap_timely_std, polcap_investigate_std),
    police_capacity_idx_listwise_baseline = idx_mean_listwise(polcap_timely_std_baseline, polcap_investigate_std_baseline),
    # restandardize
    police_capacity_idx_listwise = stdize(police_capacity_idx_listwise, to = police_capacity_idx_listwise_baseline),
    police_capacity_idx_listwise_baseline = stdize(police_capacity_idx_listwise_baseline, to = police_capacity_idx_listwise_baseline)
    
    )

#--------------------------------------------------------------------------------------------------------------
                        # 12. Hypotheses M2b: responsive_act
#--------------------------------------------------------------------------------------------------------------
pak_citizen <- 
  pak_citizen %>%
  mutate(
    # retain the raw definition
    responsive_act_raw = responsive_act, 
    responsive_act_raw_baseline = responsive_act_baseline,
    # standardize the vars
    responsive_act_std = stdize(responsive_act, to = responsive_act_baseline),
    responsive_act_std_baseline = stdize(responsive_act_baseline, to = responsive_act_baseline),
    # generate similar name to the one on the mpap
    responsive_act = responsive_act_std,
    responsive_act_baseline = responsive_act_std_baseline
  )

#--------------------------------------------------------------------------------------------------------------
                          # 13. Hypotheses S1: legit_trust
#--------------------------------------------------------------------------------------------------------------
pak_citizen <- 
  pak_citizen %>%
  mutate(
    # retain the raw definition
    legit_trust_raw = legit_trust, 
    legit_trust_raw_baseline = legit_trust_baseline,
    # standardize the vars
    legit_trust_std = stdize(legit_trust, to = legit_trust_baseline),
    legit_trust_std_baseline = stdize(legit_trust_baseline, to = legit_trust_baseline),
    # generate similar name to the one on the mpap
    legit_trust = legit_trust_std,
    legit_trust_baseline = legit_trust_std_baseline
  )

#--------------------------------------------------------------------------------------------------------------
                          # 14. Hypotheses S2: trust_community
#--------------------------------------------------------------------------------------------------------------
pak_citizen <- 
  pak_citizen %>%
  mutate(
    # retain the raw definition
    trust_community_raw = trust_community, 
    trust_community_raw_baseline = trust_community_baseline,
    trust_community_rescaled = case_when(
      trust_community == 1 ~ 0,
      trust_community == 2 ~ 1,
      trust_community == 3 ~ 1.5,
      trust_community == 4 ~ 2,
      trust_community == 5 ~ 3,
      TRUE ~ as.numeric(trust_community)),
    
    trust_community_rescaled_baseline =  case_when(
      trust_community_baseline == 1 ~ 0,
      trust_community_baseline == 2 ~ 1,
      trust_community_baseline == 3 ~ 1.5,
      trust_community_baseline == 4 ~ 2,
      trust_community_baseline == 5 ~ 3,
      TRUE ~ as.numeric(trust_community_baseline)),
    # standardize the vars
    trust_community_std = stdize(trust_community_rescaled, to = trust_community_rescaled_baseline),
    trust_community_std_baseline = stdize(trust_community_rescaled_baseline, to = trust_community_rescaled_baseline),
    # generate similar name to the one on the mpap
    trust_community = trust_community_std,
    trust_community_baseline = trust_community_std_baseline
  )

#--------------------------------------------------------------------------------------------------------------
                          # 15. Hypotheses C: compliance_idx
#--------------------------------------------------------------------------------------------------------------
pak_citizen <- 
  pak_citizen %>%
  mutate(
    compliance_patrol = as.numeric(compliance_patrol),
    compliance_freq = as.numeric(compliance_freq),
    compliance_patrol = if_else(compliance_patrol == 6, 5, compliance_patrol),
    compliance_freq = if_else(compliance_freq == 6, 5, compliance_freq),
    compliance_patrol_baseline = if_else(compliance_patrol_baseline == 6, 5, as.numeric(compliance_patrol_baseline)),
    compliance_freq_baseline = if_else(compliance_freq_baseline == 6, 5, as.numeric(compliance_freq_baseline)),
    
    # reverse order of variables to be consistent with the direction of the MPAP (MPAP page 75-76)
    compliance_patrol_rescaled = as.numeric(6 - compliance_patrol),
    compliance_freq_rescaled = as.numeric(6 - compliance_freq),
    compliance_meeting_rescaled = as.numeric(compliance_meeting),
    compliance_patrol_rescaled_baseline = as.numeric(6 - compliance_patrol_baseline),
    compliance_freq_rescaled_baseline = as.numeric(6 - compliance_freq_baseline),
    compliance_meeting_rescaled_baseline = as.numeric(compliance_meeting_baseline),
    # standardize the vars
    compliance_patrol_std = stdize(compliance_patrol_rescaled, to = compliance_patrol_rescaled_baseline),
    compliance_freq_std = stdize(compliance_freq_rescaled, to = compliance_freq_rescaled_baseline),
    compliance_meeting_std = stdize(compliance_meeting_rescaled, to = compliance_meeting_rescaled_baseline),
    compliance_patrol_std_baseline = stdize(compliance_patrol_rescaled_baseline, to = compliance_patrol_rescaled_baseline),
    compliance_freq_std_baseline = stdize(compliance_freq_rescaled_baseline, to = compliance_freq_rescaled_baseline),
    compliance_meeting_std_baseline = stdize(compliance_meeting_rescaled_baseline, to = compliance_meeting_rescaled_baseline),
    # calculate index
    compliance_idx = idx_mean(compliance_patrol_std, compliance_freq_std, compliance_meeting_std, tx = Z, fe = stations),
    compliance_idx_baseline = idx_mean(compliance_patrol_std_baseline, compliance_freq_std_baseline, compliance_meeting_std_baseline, tx = Z, fe = stations),
    # restandardize
    compliance_idx = stdize(compliance_idx, to = compliance_idx_baseline),
    compliance_idx_baseline = stdize(compliance_idx_baseline, to = compliance_idx_baseline)
  )

# 3. List-wise deletion
pak_citizen <-
  pak_citizen %>%
  mutate(
    compliance_idx_listwise = idx_mean_listwise(compliance_patrol_std, compliance_freq_std, compliance_meeting_std),
    compliance_idx_listwise_baseline = idx_mean_listwise(compliance_patrol_std_baseline, compliance_freq_std_baseline, compliance_meeting_std_baseline),
    # restandardize
    compliance_idx_listwise = stdize(compliance_idx_listwise, to = compliance_idx_listwise_baseline),
    compliance_idx_listwise_baseline = stdize(compliance_idx_listwise_baseline, to = compliance_idx_listwise_baseline)
  )

#--------------------------------------------------------------------------------------------------------------
# SECONDARY HYPOTHESES
#--------------------------------------------------------------------------------------------------------------

#--------------------------------------------------------------------------------------------------------------
# 16. Hyp 1a. (alt. i): crime_victim_idx_admin
#--------------------------------------------------------------------------------------------------------------
pak_admin <-
  pak_admin %>%
  rowwise() %>%
  mutate(
    # generate categories of crimes
    aviolentcrime_num = sum(aarmedrob_num, asimpleassault_num, aaggassault_num, asexual_num, adomestic_phys_num, amurder_num, aother_num_violent, na.rm = TRUE),
    anonviolentcrime_num = sum(aburglary_num, aother_num_nonviolent, na.rm = TRUE),
    # other crimes are not differentiated based on violent and non-violent
    aviolentcrime_num_baseline = sum(aarmedrob_num_baseline, aaggassault_num_baseline, asexual_num_baseline, adomestic_phys_num_baseline, amurder_num_baseline, aother_num_violent_baseline, na.rm = TRUE),
    anonviolentcrime_num_baseline = sum(aburglary_num_baseline, aother_num_nonviolent_baseline, na.rm = TRUE)) %>%
  ungroup %>%
  mutate(
    # standardize crimes
    aviolentcrime_num_std = stdize(aviolentcrime_num, to = aviolentcrime_num_baseline),
    anonviolentcrime_num_std = stdize(anonviolentcrime_num, to = anonviolentcrime_num_baseline),

    aviolentcrime_num_std_baseline = stdize(aviolentcrime_num_baseline, to = aviolentcrime_num_baseline),
    anonviolentcrime_num_std_baseline = stdize(anonviolentcrime_num_baseline, to = anonviolentcrime_num_baseline),

    aarmedrob_num_std = stdize(aarmedrob_num, to = aarmedrob_num_baseline),
    asimpleassault_num_std = stdize(asimpleassault_num, to = asimpleassault_num_baseline),
    aaggassault_num_std = stdize(aaggassault_num, to = aaggassault_num_baseline),
    asexual_num_std = stdize(asexual_num, to = asexual_num_baseline),
    adomestic_phys_num_std = stdize(adomestic_phys_num, to = adomestic_phys_num_baseline),
    amurder_num_std = stdize(amurder_num, to = amurder_num_baseline),
    aother_num_violent_std = stdize(aother_num_violent, to = aother_num_violent_baseline),
    aburglary_num_std = stdize(aburglary_num, to = aburglary_num_baseline),
    aother_num_nonviolent_std = stdize(aother_num_nonviolent, to = aother_num_nonviolent_baseline),

    aarmedrob_num_std_baseline = stdize(aarmedrob_num_baseline, to = aarmedrob_num_baseline),
    asimpleassault_num_std_baseline = stdize(asimpleassault_num_baseline, to = asimpleassault_num_baseline),
    aaggassault_num_std_baseline = stdize(aaggassault_num_baseline, to = aaggassault_num_baseline),
    asexual_num_std_baseline = stdize(asexual_num_baseline, to = asexual_num_baseline),
    adomestic_phys_num_std_baseline = stdize(adomestic_phys_num_baseline, to = adomestic_phys_num_baseline),
    amurder_num_std_baseline = stdize(amurder_num_baseline, to = amurder_num_baseline),
    aother_num_violent_std_baseline = stdize(aother_num_violent_baseline, to = aother_num_violent_baseline),
    aburglary_num_std_baseline = stdize(aburglary_num_baseline, to = aburglary_num_baseline),
    aother_num_nonviolent_std_baseline = stdize(aother_num_nonviolent_baseline, to = aother_num_nonviolent_baseline),

    # generate index
    crime_victim_idx_admin = idx_mean(aviolentcrime_num_std, anonviolentcrime_num_std, tx = Z, fe = stations),
    crime_victim_idx_admin_baseline = idx_mean(aviolentcrime_num_std_baseline, anonviolentcrime_num_std_baseline, tx = Z, fe = stations),
    # restandardize
    crime_victim_idx_admin = stdize(crime_victim_idx_admin, to = crime_victim_idx_admin_baseline),
    crime_victim_idx_admin_baseline = stdize(crime_victim_idx_admin_baseline, to = crime_victim_idx_admin_baseline)
    
  )

# 3. List-wise deletion
pak_admin <-
  pak_admin %>%
  mutate(
    crime_victim_idx_admin_listwise = idx_mean_listwise(aviolentcrime_num_std, anonviolentcrime_num_std),
    crime_victim_idx_admin_listwise_baseline = idx_mean_listwise(aviolentcrime_num_std_baseline, anonviolentcrime_num_std_baseline),
    # restandardize
    crime_victim_idx_admin_listwise = stdize(crime_victim_idx_admin_listwise, to = crime_victim_idx_admin_listwise_baseline),
    crime_victim_idx_admin_listwise_baseline = stdize(crime_victim_idx_admin_listwise_baseline, to = crime_victim_idx_admin_listwise_baseline)
    
    )

#--------------------------------------------------------------------------------------------------------------
# 17. Hyp 1a. (alt. ii): crime_victim_idx_exp
#--------------------------------------------------------------------------------------------------------------
pak_citizen <- 
  pak_citizen %>% 
  rowwise() %>% 
  mutate(
    # generate categories of crimes
    violentcrime_num_exp = sum(armedrob_num, simpleassault_num, other_any_violent, na.rm = TRUE),
    nonviolentcrime_num_exp = sum(burglary_num, land_any, other_any_nonviolent, na.rm = TRUE),
    cviolentcrime_num_exp = sum(carmedrob_num, caggassault_num, csimpleassault_num, csexual_num, cdomestic_phys_num, cmurder_num, cmob_num, cother_any_violent, na.rm = TRUE),
    cnonviolentcrime_num_exp = sum(cburglary_num, cother_any_nonviolent, na.rm = TRUE),
    # Note: aggassault_num, sexual_num, domestic_phys_num, domestic_verbal_num, cdomestic_verbal_num, cland_any were not collected
    
    violentcrime_num_exp_baseline = sum(armedrob_num_baseline, simpleassault_num_baseline, other_any_violent_baseline, na.rm = TRUE),
    nonviolentcrime_num_exp_baseline = sum(burglary_num_baseline, land_any_baseline, other_any_nonviolent_baseline, na.rm = TRUE),
    cviolentcrime_num_exp_baseline = sum(carmedrob_num_baseline, caggassault_num_baseline, csimpleassault_num_baseline, csexual_num_baseline, cdomestic_phys_num_baseline, cmurder_num_baseline, cmob_num_baseline, cother_any_violent_baseline, na.rm = TRUE),
    cnonviolentcrime_num_exp_baseline = sum(cburglary_num_baseline, cother_any_nonviolent_baseline, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(
    # standardize crimes
    violentcrime_num_exp_std = stdize(violentcrime_num_exp, to = violentcrime_num_exp_baseline),
    nonviolentcrime_num_exp_std = stdize(nonviolentcrime_num_exp, to = nonviolentcrime_num_exp_baseline),
    cviolentcrime_num_exp_std = stdize(cviolentcrime_num_exp, to = cviolentcrime_num_exp_baseline),
    cnonviolentcrime_num_exp_std = stdize(cnonviolentcrime_num_exp, to = cnonviolentcrime_num_exp_baseline),
    
    violentcrime_num_exp_std_baseline = stdize(violentcrime_num_exp_baseline, to = violentcrime_num_exp_baseline),
    nonviolentcrime_num_exp_std_baseline = stdize(nonviolentcrime_num_exp_baseline, to = nonviolentcrime_num_exp_baseline),
    cviolentcrime_num_exp_std_baseline = stdize(cviolentcrime_num_exp_baseline, to = cviolentcrime_num_exp_baseline),
    cnonviolentcrime_num_exp_std_baseline = stdize(cnonviolentcrime_num_exp_baseline, to = cnonviolentcrime_num_exp_baseline),
    
    armedrob_num_std = stdize(armedrob_num, to = armedrob_num_baseline),
    # aggassault_num_std = stdize(aggassault_num, to = aggassault_num_baseline),
    # sexual_num_std = stdize(sexual_num, sexual_num_baseline),
    # domestic_phys_num_std = stdize(domestic_phys_num, to = domestic_phys_num_baseline),
    simpleassault_num_std = stdize(simpleassault_num, to = simpleassault_num_baseline),
    other_any_violent_std = stdize(other_any_violent, to = other_any_violent_baseline),
    burglary_num_std = stdize(burglary_num, to = burglary_num_baseline),
    # domestic_verbal_num_std = stdize(domestic_verbal_num, domestic_verbal_num_baseline),
    land_any_std = stdize(land_any, to = land_any_baseline),
    other_any_nonviolent_std = stdize(other_any_nonviolent, to = other_any_nonviolent_baseline),
    carmedrob_num_std = stdize(carmedrob_num, to = carmedrob_num_baseline),
    caggassault_num_std = stdize(caggassault_num, to = caggassault_num_baseline),
    csimpleassault_num_std = stdize(csimpleassault_num, to = csimpleassault_num_baseline),
    csexual_num_std = stdize(csexual_num, to = csexual_num_baseline),
    cdomestic_phys_num_std = stdize(cdomestic_phys_num, to = cdomestic_phys_num_baseline),
    cmurder_num_std = stdize(cmurder_num, to = cmurder_num_baseline),
    cmob_num_std = stdize(cmob_num, to = cmob_num_baseline),
    cother_any_violent_std = stdize(cother_any_violent, to = cother_any_violent_baseline),
    cburglary_num_std = stdize(cburglary_num, to = cburglary_num_baseline),
    # cdomestic_verbal_num_std = stdize(cdomestic_verbal_num, cdomestic_verbal_num_baseline),
    # cland_any_std = stdize(cland_any, to = cland_any_baseline),
    cother_any_nonviolent_std = stdize(cother_any_nonviolent, to = cother_any_nonviolent_baseline),
    
    armedrob_num_std_baseline = stdize(armedrob_num_baseline, to = armedrob_num_baseline),
    # aggassault_num_std_baseline = stdize(aggassault_num_baseline, to = aggassault_num_baseline),
    # domestic_phys_num_std_baseline = stdize(domestic_phys_num_baseline, to = domestic_phys_num_baseline),
    simpleassault_num_std_baseline = stdize(simpleassault_num_baseline, to = simpleassault_num_baseline),
    other_any_violent_std_baseline = stdize(other_any_violent_baseline, to = other_any_violent_baseline),
    burglary_num_std_baseline = stdize(burglary_num_baseline, to = burglary_num_baseline),
    land_any_std_baseline = stdize(land_any_baseline, to = land_any_baseline),
    other_any_nonviolent_std_baseline = stdize(other_any_nonviolent_baseline, to = other_any_nonviolent_baseline),
    carmedrob_num_std_baseline = stdize(carmedrob_num_baseline, to = carmedrob_num_baseline),
    caggassault_num_std_baseline = stdize(caggassault_num_baseline, to = caggassault_num_baseline),
    csimpleassault_num_std_baseline = stdize(csimpleassault_num_baseline, to = csimpleassault_num_baseline),
    csexual_num_std_baseline = stdize(csexual_num_baseline, to = csexual_num_baseline),
    cmurder_num_std_baseline = stdize(cmurder_num_baseline, to = cmurder_num_baseline),
    cmob_num_std_baseline = stdize(cmob_num_baseline, to = cmob_num_baseline),
    cother_any_violent_std_baseline = stdize(cother_any_violent_baseline, to = cother_any_violent_baseline),
    cburglary_num_std_baseline = stdize(cburglary_num_baseline, to = cburglary_num_baseline),
    cother_any_nonviolent_std_baseline = stdize(cother_any_nonviolent_baseline, to = cother_any_nonviolent_baseline),
    # domestic_verbal_num_std_baseline = stdize(domestic_verbal_num_baseline, to = domestic_verbal_num_baseline),
    # cdomestic_verbal_num_std_baseline = stdize(cdomestic_verbal_num_baseline, to = cdomestic_verbal_num_baseline),
    # cland_any_std_baseline = stdize(cland_any_baseline, to = cland_any_baseline),
    
    # generate index
    crime_victim_idx_exp = idx_mean(violentcrime_num_exp_std, nonviolentcrime_num_exp_std, cviolentcrime_num_exp_std, cnonviolentcrime_num_exp_std, tx = Z, fe = stations),
    crime_victim_idx_exp_baseline = idx_mean(violentcrime_num_exp_std_baseline, nonviolentcrime_num_exp_std_baseline, cviolentcrime_num_exp_std_baseline, cnonviolentcrime_num_exp_std_baseline, tx = Z, fe = stations),
    # restandardize
    crime_victim_idx_exp = stdize(crime_victim_idx_exp, to = crime_victim_idx_exp_baseline),
    crime_victim_idx_exp_baseline = stdize(crime_victim_idx_exp_baseline, to = crime_victim_idx_exp_baseline)
    
  )

# 3. List-wise deletion
pak_citizen <-
  pak_citizen %>%
  mutate(
    crime_victim_idx_exp_listwise = idx_mean_listwise(violentcrime_num_exp_std, nonviolentcrime_num_exp_std, cviolentcrime_num_exp_std, cnonviolentcrime_num_exp_std),
    crime_victim_idx_exp_listwise_baseline = idx_mean_listwise(violentcrime_num_exp_std_baseline, nonviolentcrime_num_exp_std_baseline, cviolentcrime_num_exp_std_baseline, cnonviolentcrime_num_exp_std_baseline),
    # restandardize
    crime_victim_idx_exp_listwise = stdize(crime_victim_idx_exp_listwise, to = crime_victim_idx_exp_listwise_baseline),
    crime_victim_idx_exp_listwise_baseline = stdize(crime_victim_idx_exp_listwise_baseline, to = crime_victim_idx_exp_listwise_baseline)
  )

#--------------------------------------------------------------------------------------------------------------
# 18. Hyp 1a. (alt. iii) crime_victim_idx_bin
#--------------------------------------------------------------------------------------------------------------
pak_citizen <- 
  pak_citizen %>% 
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
      TRUE ~ NA_integer_),
    
    armedrob_bin_baseline = case_when(
      armedrob_num_baseline > 0 ~ 1L,
      armedrob_num_baseline == 0 ~ 0L,
      TRUE ~ NA_integer_),
    simpleassault_bin_baseline = case_when(
      simpleassault_num_baseline > 0 ~ 1L,
      simpleassault_num_baseline == 0 ~ 0L,
      TRUE ~ NA_integer_),
    burglary_bin_baseline = case_when(
      burglary_num_baseline > 0 ~ 1L,
      burglary_num_baseline == 0 ~ 0L,
      TRUE ~ NA_integer_),
    carmedrob_bin_baseline = case_when(
      carmedrob_num_baseline > 0 ~ 1L,
      carmedrob_num_baseline == 0 ~ 0L,
      TRUE ~ NA_integer_),
    caggassault_bin_baseline = case_when(
      caggassault_num_baseline > 0 ~ 1L,
      caggassault_num_baseline == 0 ~ 0L,
      TRUE ~ NA_integer_),
    csexual_bin_baseline = case_when(
      csexual_num_baseline > 0 ~ 1L,
      csexual_num_baseline == 0 ~ 0L,
      TRUE ~ NA_integer_),
    cdomestic_phys_bin_baseline = case_when(
      cdomestic_phys_num_baseline > 0 ~ 1L,
      cdomestic_phys_num_baseline == 0 ~ 0L,
      TRUE ~ NA_integer_),
    cmurder_bin_baseline = case_when(
      cmurder_num_baseline > 0 ~ 1L,
      cmurder_num_baseline == 0 ~ 0L,
      TRUE ~ NA_integer_),
    cburglary_bin_baseline = case_when(
      cburglary_num_baseline > 0 ~ 1L,
      cburglary_num_baseline == 0 ~ 0L,
      TRUE ~ NA_integer_),
    csimpleassault_bin_baseline = case_when(
      csimpleassault_num_baseline > 0 ~ 1L,
      csimpleassault_num_baseline == 0 ~ 0L,
      TRUE ~ NA_integer_)) %>% 
  rowwise() %>% 
  mutate(
    # sum all crimes into categories
    violentcrime_bin =  sum(armedrob_bin, simpleassault_bin, other_any_violent, na.rm = TRUE),
    nonviolentcrime_bin = sum(burglary_bin, other_any_nonviolent, na.rm = TRUE),
    cviolentcrime_bin = sum(carmedrob_bin, caggassault_bin, csimpleassault_bin, csexual_bin, cdomestic_phys_bin, cmurder_bin, cother_any_violent, na.rm = TRUE),
    cnonviolentcrime_bin = sum(cburglary_bin, cother_any_nonviolent, na.rm = TRUE),
    
    violentcrime_bin_baseline = sum(armedrob_bin_baseline, simpleassault_bin_baseline, other_any_violent_baseline, na.rm = TRUE),
    nonviolentcrime_bin_baseline = sum(burglary_bin_baseline, other_any_nonviolent_baseline, na.rm = TRUE),
    cviolentcrime_bin_baseline = sum(carmedrob_bin_baseline, caggassault_bin_baseline, csimpleassault_bin_baseline, csexual_bin_baseline, cdomestic_phys_bin_baseline, cmurder_bin_baseline, cother_any_violent_baseline, na.rm = TRUE),
    cnonviolentcrime_bin_baseline = sum(cburglary_bin_baseline, cother_any_nonviolent_baseline, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(
    violentcrime_bin_std = stdize(violentcrime_bin, to = violentcrime_bin_baseline),
    nonviolentcrime_bin_std = stdize(nonviolentcrime_bin, to = nonviolentcrime_bin_baseline),
    cviolentcrime_bin_std = stdize(cviolentcrime_bin, to = cviolentcrime_bin_baseline),
    cnonviolentcrime_bin_std = stdize(cnonviolentcrime_bin, to = cnonviolentcrime_bin_baseline),
    
    violentcrime_bin_std_baseline = stdize(violentcrime_bin_baseline, to = violentcrime_bin_baseline),
    nonviolentcrime_bin_std_baseline = stdize(nonviolentcrime_bin_baseline, to = nonviolentcrime_bin_baseline),
    cviolentcrime_bin_std_baseline = stdize(cviolentcrime_bin_baseline, to = cviolentcrime_bin_baseline),
    cnonviolentcrime_bin_std_baseline = stdize(cnonviolentcrime_bin_baseline, to = cnonviolentcrime_bin_baseline),
    
    armedrob_bin_std = stdize(armedrob_bin, to = armedrob_bin_baseline),
    simpleassault_bin_std = stdize(simpleassault_bin, to = simpleassault_bin_baseline),
    other_any_violent_std = stdize(other_any_violent, to = other_any_violent_baseline),
    burglary_bin_std = stdize(burglary_bin, to = burglary_bin_baseline),
    other_any_nonviolent_std = stdize(other_any_nonviolent, to = other_any_nonviolent_baseline),
    carmedrob_bin_std = stdize(carmedrob_bin, to = carmedrob_bin_baseline),
    caggassault_bin_std = stdize(caggassault_bin, to = caggassault_bin_baseline),
    csimpleassault_bin_std = stdize(csimpleassault_bin, to = csimpleassault_bin_baseline),
    csexual_bin_std = stdize(csexual_bin, to = csexual_bin_baseline),
    cdomestic_phys_bin_std = stdize(cdomestic_phys_bin, to = cdomestic_phys_bin_baseline),
    cmurder_bin_std = stdize(cmurder_bin, to = cmurder_bin_baseline),
    cother_any_violent_std = stdize(cother_any_violent, to = cother_any_violent_baseline),
    cburglary_bin_std = stdize(cburglary_bin, to = cburglary_bin_baseline),
    cother_any_nonviolent_std = stdize(cother_any_nonviolent, to = cother_any_nonviolent_baseline),
    
    armedrob_bin_std_baseline = stdize(armedrob_bin_baseline, to = armedrob_bin_baseline),
    simpleassault_bin_std_baseline = stdize(simpleassault_bin_baseline, to = simpleassault_bin_baseline),
    other_any_violent_std_baseline = stdize(other_any_violent_baseline, to = other_any_violent_baseline),
    burglary_bin_std_baseline = stdize(burglary_bin_baseline, to = burglary_bin_baseline),
    other_any_nonviolent_std_baseline = stdize(other_any_nonviolent_baseline, to = other_any_nonviolent_baseline),
    carmedrob_bin_std_baseline = stdize(carmedrob_bin_baseline, to = carmedrob_bin_baseline),
    caggassault_bin_std_baseline = stdize(caggassault_bin_baseline, to = caggassault_bin_baseline),
    csimpleassault_bin_std_baseline = stdize(csimpleassault_bin_baseline, to = csimpleassault_bin_baseline),
    csexual_bin_std_baseline = stdize(csexual_bin_baseline, to = csexual_bin_baseline),
    cdomestic_phys_bin_std_baseline = stdize(cdomestic_phys_bin_baseline, to = cdomestic_phys_bin_baseline),
    cmurder_bin_std_baseline = stdize(cmurder_bin_baseline, to = cmurder_bin_baseline),
    cother_any_violent_std_baseline = stdize(cother_any_violent_baseline, to = cother_any_violent_baseline),
    cburglary_bin_std_baseline = stdize(cburglary_bin_baseline, to = cburglary_bin_baseline),
    cother_any_nonviolent_std_baseline = stdize(cother_any_nonviolent_baseline, to = cother_any_nonviolent_baseline),
    
    crime_victim_idx_bin = idx_mean(violentcrime_bin_std, nonviolentcrime_bin_std, cviolentcrime_bin_std, cnonviolentcrime_bin_std, tx = Z, fe = stations),
    crime_victim_idx_bin_baseline = idx_mean(violentcrime_bin_std_baseline, nonviolentcrime_bin_std_baseline, cviolentcrime_bin_std_baseline, cnonviolentcrime_bin_std_baseline, tx = Z, fe = stations),
    # restandardize
    crime_victim_idx_bin = stdize(crime_victim_idx_bin, to = crime_victim_idx_bin_baseline),
    crime_victim_idx_bin_baseline = stdize(crime_victim_idx_bin_baseline, to = crime_victim_idx_bin_baseline)
    
  )

# 3. List-wise deletion
pak_citizen <-
  pak_citizen %>%
  mutate(
    crime_victim_idx_bin_listwise = idx_mean_listwise(violentcrime_bin_std, nonviolentcrime_bin_std, cviolentcrime_bin_std, cnonviolentcrime_bin_std),
    crime_victim_idx_bin_listwise_baseline = idx_mean_listwise(violentcrime_bin_std_baseline, nonviolentcrime_bin_std_baseline, cviolentcrime_bin_std_baseline, cnonviolentcrime_bin_std_baseline),
    # restandardize
    crime_victim_idx_bin_listwise = stdize(crime_victim_idx_bin_listwise, to = crime_victim_idx_bin_listwise_baseline),
    crime_victim_idx_bin_listwise_baseline = stdize(crime_victim_idx_bin_listwise_baseline, to = crime_victim_idx_bin_listwise_baseline)
  )


#--------------------------------------------------------------------------------------------------------------
# 21. REMOVE ALL NON RESCALED VARIABLES
#--------------------------------------------------------------------------------------------------------------
rescaled_vars <-
  names(pak_citizen)[str_ends(names(pak_citizen), "_rescaled")] %>%
  str_replace("_rescaled", "")

pak_citizen <- 
  pak_citizen %>% 
  select(-rescaled_vars)

#--------------------------------------------------------------------------------------------------------------
# Save admin and citizen data
#--------------------------------------------------------------------------------------------------------------
saveRDS(pak_citizen, file = "data/out/pak-citizen-construct.RDS")
saveRDS(pak_admin, file = "data/out/pak-admin-construct.RDS")
saveRDS(pak_officer, file = "data/out/pak-officer-construct.RDS")
