#--------------------------------------------------------------------------------------------------------------
                              # R PACKAGES
#--------------------------------------------------------------------------------------------------------------
suppressMessages({
  library(tidyverse)
  library(DeclareDesign)
})

source("code/shared/index-construction.R")

#--------------------------------------------------------------------------------------------------------------
                              # CLEANED DATA
#--------------------------------------------------------------------------------------------------------------
uga_citizen <-readRDS("data/out/uga-citizen-clean.RDS")
uga_admin <- readRDS("data/out/uga-admin-clean.RDS")
uga_officer <- readRDS("data/out/uga-officer-clean.RDS")

#--------------------------------------------------------------------------------------------------------------
                            # 1. Hypotheses 1(a): crimevictim_idx
#--------------------------------------------------------------------------------------------------------------
uga_citizen <- 
  uga_citizen %>%
  mutate(
    # recode community level crimes
    carmedrob_num = case_when(
      carmedrob_num == 0 ~ 0L,
      carmedrob_num == 1 ~ 1L, 
      carmedrob_num == 2 | carmedrob_num == 3 ~ 2L, 
      carmedrob_num == 4 | carmedrob_num == 5 ~ 3L, 
      carmedrob_num >= 6 & carmedrob_num <= 10 ~ 4L, 
      carmedrob_num > 10 ~ 5L, 
      TRUE ~ NA_integer_),
    caggassault_num = case_when(
      caggassault_num == 0 ~ 0L,
      caggassault_num == 1 ~ 1L, 
      caggassault_num == 2 | caggassault_num == 3 ~ 2L, 
      caggassault_num == 4 | caggassault_num == 5 ~ 3L, 
      caggassault_num >= 6 & caggassault_num <= 10 ~ 4L, 
      caggassault_num > 10 ~ 5L, 
      TRUE ~ NA_integer_),
    csimpleassault_num = case_when(
      csimpleassault_num == 0 ~ 0L,
      csimpleassault_num == 1 ~ 1L, 
      csimpleassault_num == 2 | csimpleassault_num == 3 ~ 2L, 
      csimpleassault_num == 4 | csimpleassault_num == 5 ~ 3L, 
      csimpleassault_num >= 6 & csimpleassault_num <= 10 ~ 4L, 
      csimpleassault_num > 10 ~ 5L, 
      TRUE ~ NA_integer_),
    csexual_num = case_when(
      csexual_num == 0 ~ 0L,
      csexual_num == 1 ~ 1L, 
      csexual_num == 2 | csexual_num == 3 ~ 2L, 
      csexual_num == 4 | csexual_num == 5 ~ 3L, 
      csexual_num >= 6 & csexual_num <= 10 ~ 4L, 
      csexual_num > 10 ~ 5L, 
      TRUE ~ NA_integer_),
    cdomestic_phys_num = case_when(
      cdomestic_phys_num == 0 ~ 0L,
      cdomestic_phys_num == 1 ~ 1L, 
      cdomestic_phys_num == 2 | cdomestic_phys_num == 3 ~ 2L, 
      cdomestic_phys_num == 4 | cdomestic_phys_num == 5 ~ 3L, 
      cdomestic_phys_num >= 6 & cdomestic_phys_num <= 10 ~ 4L, 
      cdomestic_phys_num > 10 ~ 5L, 
      TRUE ~ NA_integer_),
    cmurder_num = case_when(
      cmurder_num == 0 ~ 0L,
      cmurder_num == 1 ~ 1L, 
      cmurder_num == 2 | cmurder_num == 3 ~ 2L, 
      cmurder_num == 4 | cmurder_num == 5 ~ 3L, 
      cmurder_num >= 6 & cmurder_num <= 10 ~ 4L, 
      cmurder_num > 10 ~ 5L, 
      TRUE ~ NA_integer_),
    cburglary_num = case_when(
      cburglary_num == 0 ~ 0L,
      cburglary_num == 1 ~ 1L, 
      cburglary_num == 2 | cburglary_num == 3 ~ 2L, 
      cburglary_num == 4 | cburglary_num == 5 ~ 3L, 
      cburglary_num >= 6 & cburglary_num <= 10 ~ 4L, 
      cburglary_num > 10 ~ 5L, 
      TRUE ~ NA_integer_),
    # recode community level crimes
    carmedrob_num_baseline = case_when(
      carmedrob_num_baseline == 0 ~ 0L,
      carmedrob_num_baseline == 1 ~ 1L, 
      carmedrob_num_baseline == 2 | carmedrob_num_baseline == 3 ~ 2L, 
      carmedrob_num_baseline == 4 | carmedrob_num_baseline == 5 ~ 3L, 
      carmedrob_num_baseline >= 6 & carmedrob_num_baseline <= 10 ~ 4L, 
      carmedrob_num_baseline > 10 ~ 5L, 
      TRUE ~ NA_integer_),
    caggassault_num_baseline = case_when(
      caggassault_num_baseline == 0 ~ 0L,
      caggassault_num_baseline == 1 ~ 1L, 
      caggassault_num_baseline == 2 | caggassault_num_baseline == 3 ~ 2L, 
      caggassault_num_baseline == 4 | caggassault_num_baseline == 5 ~ 3L, 
      caggassault_num_baseline >= 6 & caggassault_num_baseline <= 10 ~ 4L, 
      caggassault_num_baseline > 10 ~ 5L, 
      TRUE ~ NA_integer_),
    csimpleassault_num_baseline = case_when(
      csimpleassault_num_baseline == 0 ~ 0L,
      csimpleassault_num_baseline == 1 ~ 1L, 
      csimpleassault_num_baseline == 2 | csimpleassault_num_baseline == 3 ~ 2L, 
      csimpleassault_num_baseline == 4 | csimpleassault_num_baseline == 5 ~ 3L, 
      csimpleassault_num_baseline >= 6 & csimpleassault_num_baseline <= 10 ~ 4L, 
      csimpleassault_num_baseline > 10 ~ 5L, 
      TRUE ~ NA_integer_),
    csexual_num_baseline = case_when(
      csexual_num_baseline == 0 ~ 0L,
      csexual_num_baseline == 1 ~ 1L, 
      csexual_num_baseline == 2 | csexual_num_baseline == 3 ~ 2L, 
      csexual_num_baseline == 4 | csexual_num_baseline == 5 ~ 3L, 
      csexual_num_baseline >= 6 & csexual_num_baseline <= 10 ~ 4L, 
      csexual_num_baseline > 10 ~ 5L, 
      TRUE ~ NA_integer_),
    cdomestic_phys_num_baseline = case_when(
      cdomestic_phys_num_baseline == 0 ~ 0L,
      cdomestic_phys_num_baseline == 1 ~ 1L, 
      cdomestic_phys_num_baseline == 2 | cdomestic_phys_num_baseline == 3 ~ 2L, 
      cdomestic_phys_num_baseline == 4 | cdomestic_phys_num_baseline == 5 ~ 3L, 
      cdomestic_phys_num_baseline >= 6 & cdomestic_phys_num_baseline <= 10 ~ 4L, 
      cdomestic_phys_num_baseline > 10 ~ 5L, 
      TRUE ~ NA_integer_),
    cmurder_num_baseline = case_when(
      cmurder_num_baseline == 0 ~ 0L,
      cmurder_num_baseline == 1 ~ 1L, 
      cmurder_num_baseline == 2 | cmurder_num_baseline == 3 ~ 2L, 
      cmurder_num_baseline == 4 | cmurder_num_baseline == 5 ~ 3L, 
      cmurder_num_baseline >= 6 & cmurder_num_baseline <= 10 ~ 4L, 
      cmurder_num_baseline > 10 ~ 5L, 
      TRUE ~ NA_integer_),
    cburglary_num_baseline = case_when(
      cburglary_num_baseline == 0 ~ 0L,
      cburglary_num_baseline == 1 ~ 1L, 
      cburglary_num_baseline == 2 | cburglary_num_baseline == 3 ~ 2L, 
      cburglary_num_baseline == 4 | cburglary_num_baseline == 5 ~ 3L, 
      cburglary_num_baseline >= 6 & cburglary_num_baseline <= 10 ~ 4L, 
      cburglary_num_baseline > 10 ~ 5L, 
      TRUE ~ NA_integer_)) %>% 
  rowwise() %>% 
  mutate(
    # sum all crimes into categories
    violentcrime_num = sum(armedrob_num, simpleassault_num, other_any_violent, na.rm = TRUE),
    nonviolentcrime_num = sum(burglary_num, other_any_nonviolent, na.rm = TRUE),
    cviolentcrime_num = sum(carmedrob_num, caggassault_num, csimpleassault_num, csexual_num, cdomestic_phys_num, cmurder_num, cother_any_violent, na.rm = TRUE),
    cnonviolentcrime_num = sum(cburglary_num, cother_any_nonviolent, na.rm = TRUE),
    violentcrime_num_baseline = sum(armedrob_num_baseline, simpleassault_num_baseline, other_any_violent_baseline, na.rm = TRUE),
    nonviolentcrime_num_baseline = sum(burglary_num_baseline, other_any_nonviolent_baseline, na.rm = TRUE),
    cviolentcrime_num_baseline = sum(carmedrob_num_baseline, caggassault_num_baseline, csimpleassault_num_baseline, csexual_num_baseline, cdomestic_phys_num_baseline, cmurder_num_baseline, cother_any_violent_baseline, na.rm = TRUE),
    cnonviolentcrime_num_baseline =  sum(cburglary_num_baseline, cother_any_nonviolent_baseline, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(across(c(violentcrime_num_baseline, nonviolentcrime_num_baseline, cviolentcrime_num_baseline, cnonviolentcrime_num_baseline), ~ if_else(in_baseline == 0, NA_integer_, .))) %>% 
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
    caggassault_num_std = stdize(caggassault_num, to = caggassault_num_baseline),
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
    caggassault_num_std_baseline = stdize(caggassault_num_baseline, to = caggassault_num_baseline),
    csimpleassault_num_std_baseline = stdize(csimpleassault_num_baseline, to = csimpleassault_num_baseline),
    csexual_num_std_baseline = stdize(csexual_num_baseline, to = csexual_num_baseline),
    cdomestic_phys_num_std_baseline = stdize(cdomestic_phys_num_baseline, to = cdomestic_phys_num_baseline),
    cmurder_num_std_baseline = stdize(cmurder_num_baseline, to = cmurder_num_baseline),
    cother_any_violent_std_baseline = stdize(cother_any_violent_baseline, to = cother_any_violent_baseline),
    cburglary_num_std_baseline = stdize(cburglary_num_baseline, to = cburglary_num_baseline),
    cother_any_nonviolent_std_baseline = stdize(cother_any_nonviolent_baseline, to = cother_any_nonviolent_baseline),
    # generate crime index
    crime_victim_idx = idx_mean(violentcrime_num_std, nonviolentcrime_num_std, cviolentcrime_num_std, cnonviolentcrime_num_std, tx = Z, fe = block_ID),
    crime_victim_idx_baseline = idx_mean(violentcrime_num_std_baseline, nonviolentcrime_num_std_baseline, cviolentcrime_num_std_baseline, cnonviolentcrime_num_std_baseline, tx = Z, fe = block_ID),
    # re-standardize
    crime_victim_idx_ = stdize(crime_victim_idx, to = crime_victim_idx_baseline),
    crime_victim_idx_baseline = stdize(crime_victim_idx_baseline, to = crime_victim_idx_baseline)
    ) %>% 
  # generate crime at the cluster level het effects
  group_by(station_id) %>% 
  mutate(crime_victim_idx_cluster_baseline = mean(crime_victim_idx_baseline, na.rm = TRUE)) %>% 
  ungroup


# 2. Common analysis
uga_citizen <-
  uga_citizen %>%
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
    crime_victim_idx_common = idx_mean(violentcrime_num_std, nonviolentcrime_num_std, cviolentcrime_num_common_std, cnonviolentcrime_num_std, tx = Z, fe = block_ID),
    crime_victim_idx_common_baseline = idx_mean(violentcrime_num_std_baseline, nonviolentcrime_num_std_baseline, cviolentcrime_num_common_std_baseline, cnonviolentcrime_num_std_baseline, tx = Z, fe = block_ID),
    # re-standardize
    crime_victim_idx_common = stdize(crime_victim_idx_common, to = crime_victim_idx_common_baseline),
    crime_victim_idx_common_baseline = stdize(crime_victim_idx_common_baseline, to = crime_victim_idx_common_baseline)
  )

# 3. List-wise deletion
uga_citizen <-
  uga_citizen %>%
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
uga_citizen <- 
  uga_citizen %>%
  mutate(
    # change the scales of the vars to be consistent with the mpap
    fear_violent_raw = fear_violent,
    fear_violent_rescaled = fear_violent - 1, 
    fear_nonviolent_raw = fear_nonviolent,
    fear_nonviolent_rescaled = fear_nonviolent - 1, 
    
    fear_violent_raw_baseline = fear_violent_baseline,
    fear_violent_rescaled_baseline = fear_violent_baseline - 1, 
    fear_nonviolent_raw_baseline = fear_nonviolent_baseline,
    fear_nonviolent_rescaled_baseline = fear_nonviolent_baseline - 1,
    # standardize vars
    fear_violent_std = stdize(fear_violent_rescaled, to = fear_violent_rescaled_baseline),
    fear_nonviolent_std = stdize(fear_nonviolent_rescaled, to = fear_nonviolent_rescaled_baseline),
    feared_walk_std = stdize(feared_walk, to = feared_walk_baseline),
    
    fear_violent_std_baseline = stdize(fear_violent_rescaled_baseline, to = fear_violent_rescaled_baseline),
    fear_nonviolent_std_baseline = stdize(fear_nonviolent_rescaled_baseline, to = fear_nonviolent_rescaled_baseline),
    feared_walk_std_baseline = stdize(feared_walk_baseline, to = feared_walk_baseline),
    # calculate index
    future_insecurity_idx = idx_mean(fear_violent_std, fear_nonviolent_std, feared_walk_std, tx = Z, fe = block_ID),
    future_insecurity_idx_baseline = idx_mean(fear_violent_std_baseline, fear_nonviolent_std_baseline, feared_walk_std_baseline, tx = Z, fe = block_ID),
    # re-standardize
    future_insecurity_idx = stdize(future_insecurity_idx, to = future_insecurity_idx_baseline),
    future_insecurity_idx_baseline = stdize(future_insecurity_idx_baseline, to = future_insecurity_idx_baseline)
    
  )

# 2. COMMON ANALYSIS
uga_citizen <- 
  uga_citizen %>%
  mutate(
    # Note: we have dropped fear_nonviolent following the decisions by the committee
    # standardize vars
    fear_violent_std_common = stdize(fear_violent_rescaled, to = fear_violent_rescaled_baseline),
    feared_walk_std_common = stdize(feared_walk, to = feared_walk_baseline),
    fear_violent_std_common_baseline = stdize(fear_violent_rescaled_baseline, to = fear_violent_rescaled_baseline),
    feared_walk_std_common_baseline = stdize(feared_walk_baseline, to = feared_walk_baseline),
    # calculate index
    future_insecurity_idx_common = idx_mean(fear_violent_std_common, feared_walk_std_common, tx = Z, fe = block_ID),
    future_insecurity_idx_common_baseline = idx_mean(fear_violent_std_common_baseline, feared_walk_std_common_baseline, tx = Z, fe = block_ID),
    # re-standardize
    future_insecurity_idx_common = stdize(future_insecurity_idx_common, to = future_insecurity_idx_common_baseline),
    future_insecurity_idx_common_baseline = stdize(future_insecurity_idx_common_baseline, to = future_insecurity_idx_common_baseline)
    
  )

# 3. List-wise deletion
uga_citizen <-
  uga_citizen %>%
  mutate(
    future_insecurity_idx_listwise = idx_mean_listwise(fear_violent_std_common, feared_walk_std_common),
    future_insecurity_idx_listwise_baseline = idx_mean_listwise(fear_violent_std_common_baseline, feared_walk_std_common_baseline),
    # re-standardize
    future_insecurity_idx_listwise = stdize(future_insecurity_idx_listwise, to = future_insecurity_idx_listwise_baseline),
    future_insecurity_idx_listwise_baseline = stdize(future_insecurity_idx_listwise_baseline, to = future_insecurity_idx_listwise_baseline))

#--------------------------------------------------------------------------------------------------------------
                            # 3. Hypotheses 2: satis_idx
#--------------------------------------------------------------------------------------------------------------
uga_citizen <- 
  uga_citizen %>%
  mutate(
    # change the scales of the vars to be consistent with the mpap
    satis_trust_raw = satis_trust, 
    satis_trust_rescaled = satis_trust - 1, 
    satis_general_raw = satis_general,
    satis_general_rescaled = satis_general - 1,
    
    satis_trust_raw_baseline = satis_trust_baseline, 
    satis_trust_rescaled_baseline = satis_trust_baseline - 1, 
    satis_general_raw_baseline = satis_general_baseline,
    satis_general_rescaled_baseline = satis_general_baseline - 1,
    # standardize vars
    satis_trust_std = stdize(satis_trust_rescaled, to = satis_trust_rescaled_baseline),
    satis_general_std = stdize(satis_general_rescaled, to = satis_general_rescaled_baseline),
    
    satis_trust_std_baseline = stdize(satis_trust_rescaled_baseline, to = satis_trust_rescaled_baseline),
    satis_general_std_baseline = stdize(satis_general_rescaled_baseline, to = satis_general_rescaled_baseline),
    # calculate index
    satis_idx = idx_mean(satis_trust_std, satis_general_std, tx = Z, fe = block_ID),
    satis_idx_baseline = idx_mean(satis_trust_std_baseline, satis_general_std_baseline, tx = Z, fe = block_ID),
    # re-standardize
    satis_idx = stdize(satis_idx, to = satis_idx_baseline),
    satis_idx_baseline = stdize(satis_idx_baseline, to = satis_idx_baseline)
  )

# 3. List-wise deletion
uga_citizen <-
  uga_citizen %>%
  mutate(
    satis_idx_listwise = idx_mean_listwise(satis_trust_std, satis_general_std),
    satis_idx_listwise_baseline = idx_mean_listwise(satis_trust_std_baseline, satis_general_std_baseline),
    # re-standardize
    satis_idx_listwise = stdize(satis_idx_listwise, to = satis_idx_listwise_baseline),
    satis_idx_listwise_baseline = stdize(satis_idx_listwise_baseline, to = satis_idx_listwise_baseline))

#--------------------------------------------------------------------------------------------------------------
                        # Hypotheses 3(a): officer_attitude_idx
#--------------------------------------------------------------------------------------------------------------
uga_officer <- 
  uga_officer %>% 
  mutate(
    empathy_complaints_rescaled = case_when(
      empathy_complaints == 1 ~ 0L,
      empathy_complaints == 2 ~ 1L,
      empathy_complaints == 3 ~ 1L,
      empathy_complaints == 4 ~ 2L,
      empathy_complaints == 5 ~ 3L,
      TRUE ~ empathy_complaints),
    
    empathy_complaints_rescaled_baseline = case_when(
      empathy_complaints_baseline == 1 ~ 0L,
      empathy_complaints_baseline == 2 ~ 1L,
      empathy_complaints_baseline == 3 ~ 1L,
      empathy_complaints_baseline == 4 ~ 2L,
      empathy_complaints_baseline == 5 ~ 3L,
      TRUE ~ empathy_complaints_baseline),
    
    empathy_reports_rescaled = case_when(
      empathy_reports == 1 ~ 0L,
      empathy_reports == 2 ~ 1L,
      empathy_reports == 3 ~ 1L,
      empathy_reports == 4 ~ 2L,
      empathy_reports == 5 ~ 3L,
      TRUE ~ empathy_reports),
    
    empathy_reports_rescaled_baseline = case_when(
      empathy_reports_baseline == 1 ~ 0L,
      empathy_reports_baseline == 2 ~ 1L,
      empathy_reports_baseline == 3 ~ 1L,
      empathy_reports_baseline == 4 ~ 2L,
      empathy_reports_baseline == 5 ~ 3L,
      TRUE ~ empathy_reports_baseline),
    
    
    empathy_complaints_std = stdize(empathy_complaints_rescaled, to = empathy_complaints_rescaled_baseline),
    empathy_reports_std = stdize(empathy_reports, to = empathy_reports_baseline),
    empathy_complaints_std_baseline = stdize(empathy_complaints_rescaled_baseline, to = empathy_complaints_rescaled_baseline),
    empathy_reports_std_baseline = stdize(empathy_reports_baseline, to = empathy_reports_baseline),
    
    empathy_idx = idx_mean(empathy_complaints_std, empathy_reports_std, tx = Z, fe = block_ID),
    empathy_idx_baseline = idx_mean(empathy_complaints_std_baseline, empathy_reports_std_baseline, tx = Z, fe = block_ID),
    
    account_pol_matter_rescaled = account_pol_matter - 2L,
    account_pol_matter_rescaled_baseline = account_pol_matter_baseline - 2L,
    
    account_pol_matter_std = stdize(account_pol_matter_rescaled, to = account_pol_matter_rescaled_baseline),
    account_pol_matter_std_baseline = stdize(account_pol_matter_rescaled_baseline, to = account_pol_matter_rescaled_baseline),

    hypothetical2_punishment = case_when(
      str_detect(hypothetical2_punishment, paste0("\\b", "8", "\\b")) ~ 8L,
      str_detect(hypothetical2_punishment, paste0("\\b", "7", "\\b")) ~ 7L,
      str_detect(hypothetical2_punishment, paste0("\\b", "6", "\\b")) ~ 6L,
      str_detect(hypothetical2_punishment, paste0("\\b", "5", "\\b")) ~ 5L,
      str_detect(hypothetical2_punishment, paste0("\\b", "4", "\\b")) ~ 4L,
      str_detect(hypothetical2_punishment, paste0("\\b", "3", "\\b")) ~ 3L,
      str_detect(hypothetical2_punishment, paste0("\\b", "2", "\\b")) ~ 2L,
      str_detect(hypothetical2_punishment, paste0("\\b", "1", "\\b")) ~ 1L,
      TRUE ~ NA_integer_),
    hypothetical3_punishment = case_when(
      str_detect(hypothetical3_punishment, paste0("\\b", "8", "\\b")) ~ 8L,
      str_detect(hypothetical3_punishment, paste0("\\b", "7", "\\b")) ~ 7L,
      str_detect(hypothetical3_punishment, paste0("\\b", "6", "\\b")) ~ 6L,
      str_detect(hypothetical3_punishment, paste0("\\b", "5", "\\b")) ~ 5L,
      str_detect(hypothetical3_punishment, paste0("\\b", "4", "\\b")) ~ 4L,
      str_detect(hypothetical3_punishment, paste0("\\b", "3", "\\b")) ~ 3L,
      str_detect(hypothetical3_punishment, paste0("\\b", "2", "\\b")) ~ 2L,
      str_detect(hypothetical3_punishment, paste0("\\b", "1", "\\b")) ~ 1L,
      TRUE ~ NA_integer_),
    hypothetical5_punishment = case_when(
      str_detect(hypothetical5_punishment, paste0("\\b", "8", "\\b")) ~ 8L,
      str_detect(hypothetical5_punishment, paste0("\\b", "7", "\\b")) ~ 7L,
      str_detect(hypothetical5_punishment, paste0("\\b", "6", "\\b")) ~ 6L,
      str_detect(hypothetical5_punishment, paste0("\\b", "5", "\\b")) ~ 5L,
      str_detect(hypothetical5_punishment, paste0("\\b", "4", "\\b")) ~ 4L,
      str_detect(hypothetical5_punishment, paste0("\\b", "3", "\\b")) ~ 3L,
      str_detect(hypothetical5_punishment, paste0("\\b", "2", "\\b")) ~ 2L,
      str_detect(hypothetical5_punishment, paste0("\\b", "1", "\\b")) ~ 1L,
      TRUE ~ NA_integer_),
    
    hypothetical2_punishment_baseline = case_when(
      str_detect(hypothetical2_punishment_baseline, paste0("\\b", "8", "\\b")) ~ 8L,
      str_detect(hypothetical2_punishment_baseline, paste0("\\b", "7", "\\b")) ~ 7L,
      str_detect(hypothetical2_punishment_baseline, paste0("\\b", "6", "\\b")) ~ 6L,
      str_detect(hypothetical2_punishment_baseline, paste0("\\b", "5", "\\b")) ~ 5L,
      str_detect(hypothetical2_punishment_baseline, paste0("\\b", "4", "\\b")) ~ 4L,
      str_detect(hypothetical2_punishment_baseline, paste0("\\b", "3", "\\b")) ~ 3L,
      str_detect(hypothetical2_punishment_baseline, paste0("\\b", "2", "\\b")) ~ 2L,
      str_detect(hypothetical2_punishment_baseline, paste0("\\b", "1", "\\b")) ~ 1L,
      TRUE ~ NA_integer_),
    hypothetical3_punishment_baseline = case_when(
      str_detect(hypothetical3_punishment_baseline, paste0("\\b", "8", "\\b")) ~ 8L,
      str_detect(hypothetical3_punishment_baseline, paste0("\\b", "7", "\\b")) ~ 7L,
      str_detect(hypothetical3_punishment_baseline, paste0("\\b", "6", "\\b")) ~ 6L,
      str_detect(hypothetical3_punishment_baseline, paste0("\\b", "5", "\\b")) ~ 5L,
      str_detect(hypothetical3_punishment_baseline, paste0("\\b", "4", "\\b")) ~ 4L,
      str_detect(hypothetical3_punishment_baseline, paste0("\\b", "3", "\\b")) ~ 3L,
      str_detect(hypothetical3_punishment_baseline, paste0("\\b", "2", "\\b")) ~ 2L,
      str_detect(hypothetical3_punishment_baseline, paste0("\\b", "1", "\\b")) ~ 1L,
      TRUE ~ NA_integer_),
    hypothetical5_punishment_baseline = case_when(
      str_detect(hypothetical5_punishment_baseline, paste0("\\b", "8", "\\b")) ~ 8L,
      str_detect(hypothetical5_punishment_baseline, paste0("\\b", "7", "\\b")) ~ 7L,
      str_detect(hypothetical5_punishment_baseline, paste0("\\b", "6", "\\b")) ~ 6L,
      str_detect(hypothetical5_punishment_baseline, paste0("\\b", "5", "\\b")) ~ 5L,
      str_detect(hypothetical5_punishment_baseline, paste0("\\b", "4", "\\b")) ~ 4L,
      str_detect(hypothetical5_punishment_baseline, paste0("\\b", "3", "\\b")) ~ 3L,
      str_detect(hypothetical5_punishment_baseline, paste0("\\b", "2", "\\b")) ~ 2L,
      str_detect(hypothetical5_punishment_baseline, paste0("\\b", "1", "\\b")) ~ 1L,
      TRUE ~ NA_integer_),
    
    hypothetical2_punishment_std = stdize(hypothetical2_punishment, to = hypothetical2_punishment_baseline),
    hypothetical3_punishment_std = stdize(hypothetical3_punishment, to = hypothetical3_punishment_baseline),
    hypothetical5_punishment_std = stdize(hypothetical5_punishment, to = hypothetical5_punishment_baseline),
    hypothetical2_punishment_std_baseline = stdize(hypothetical2_punishment_baseline, to = hypothetical2_punishment_baseline),
    hypothetical3_punishment_std_baseline = stdize(hypothetical3_punishment_baseline, to = hypothetical3_punishment_baseline),
    hypothetical5_punishment_std_baseline = stdize(hypothetical5_punishment_baseline, to = hypothetical5_punishment_baseline),
    
    hypothetical2_reportself_rescaled = hypothetical2_reportself - 1L,
    hypothetical3_reportself_rescaled = hypothetical3_reportself - 1L,
    hypothetical5_reportself_rescaled = hypothetical5_reportself - 1L,
    hypothetical2_reportself_rescaled_baseline = hypothetical2_reportself_baseline - 1L,
    hypothetical3_reportself_rescaled_baseline = hypothetical3_reportself_baseline - 1L,
    hypothetical5_reportself_rescaled_baseline = hypothetical5_reportself_baseline - 1L,
    hypothetical2_reportothers_rescaled = hypothetical2_reportothers - 1L,
    hypothetical3_reportothers_rescaled = hypothetical3_reportothers - 1L,
    hypothetical5_reportothers_rescaled = hypothetical5_reportothers - 1L,
    hypothetical2_reportothers_rescaled_baseline = hypothetical2_reportothers_baseline - 1L,
    hypothetical3_reportothers_rescaled_baseline = hypothetical3_reportothers_baseline - 1L,
    hypothetical5_reportothers_rescaled_baseline = hypothetical5_reportothers_baseline - 1L,
    hypothetical5_abuseself_rescaled = hypothetical5_abuseself - 1L,
    hypothetical5_abuseother_rescaled = hypothetical5_abuseother - 1L,
    hypothetical5_abuseself_rescaled_baseline = hypothetical5_abuseself_baseline - 1L,
    hypothetical5_abuseother_rescaled_baseline = hypothetical5_abuseother_baseline - 1L,
    hypothetical3_corruptself_rescaled = hypothetical3_corruptself - 1L,
    hypothetical3_corruptother_rescaled = hypothetical3_corruptother - 1L,
    hypothetical3_corruptself_rescaled_baseline = hypothetical3_corruptself_baseline - 1L,
    hypothetical3_corruptother_rescaled_baseline = hypothetical3_corruptother_baseline - 1L,
    hypothetical2_corruptself_rescaled = hypothetical2_corruptself - 1L,
    hypothetical2_corruptother_rescaled = hypothetical2_corruptother - 1L,
    hypothetical2_corruptself_rescaled_baseline = hypothetical2_corruptself_baseline - 1L,
    hypothetical2_corruptother_rescaled_baseline = hypothetical2_corruptother_baseline - 1L,
    
    hypothetical2_reportself_std = stdize(hypothetical2_reportself_rescaled, to = hypothetical2_reportself_rescaled_baseline),
    hypothetical3_reportself_std = stdize(hypothetical3_reportself_rescaled, to = hypothetical3_reportself_rescaled_baseline),
    hypothetical5_reportself_std = stdize(hypothetical5_reportself_rescaled, to = hypothetical5_reportself_rescaled_baseline),
    hypothetical2_reportself_std_baseline = stdize(hypothetical2_reportself_rescaled_baseline, to = hypothetical2_reportself_rescaled_baseline),
    hypothetical3_reportself_std_baseline = stdize(hypothetical3_reportself_rescaled_baseline, to = hypothetical3_reportself_rescaled_baseline),
    hypothetical5_reportself_std_baseline = stdize(hypothetical5_reportself_rescaled_baseline, to = hypothetical5_reportself_rescaled_baseline),
    
    hypothetical2_reportothers_std = stdize(hypothetical2_reportothers_rescaled, to = hypothetical2_reportothers_rescaled_baseline),
    hypothetical3_reportothers_std = stdize(hypothetical3_reportothers_rescaled, to = hypothetical3_reportothers_rescaled_baseline),
    hypothetical5_reportothers_std = stdize(hypothetical5_reportothers_rescaled, to = hypothetical5_reportothers_rescaled_baseline),
    hypothetical2_reportothers_std_baseline = stdize(hypothetical2_reportothers_rescaled_baseline, to = hypothetical2_reportothers_rescaled_baseline),
    hypothetical3_reportothers_std_baseline = stdize(hypothetical3_reportothers_rescaled_baseline, to = hypothetical3_reportothers_rescaled_baseline),
    hypothetical5_reportothers_std_baseline = stdize(hypothetical5_reportothers_rescaled_baseline, to = hypothetical5_reportothers_rescaled_baseline),
    
    hypothetical5_abuseself_std = stdize(hypothetical5_abuseself_rescaled, to = hypothetical5_abuseself_rescaled_baseline),
    hypothetical5_abuseother_std = stdize(hypothetical5_abuseother_rescaled, to = hypothetical5_abuseother_rescaled_baseline),
    hypothetical5_abuseself_std_baseline = stdize(hypothetical5_abuseself_rescaled_baseline, to = hypothetical5_abuseself_rescaled_baseline),
    hypothetical5_abuseother_std_baseline = stdize(hypothetical5_abuseother_rescaled_baseline, to = hypothetical5_abuseother_rescaled_baseline),
    
    hypothetical2_corruptself_std = stdize(hypothetical2_corruptself_rescaled, to = hypothetical2_corruptself_rescaled_baseline),
    hypothetical2_corruptother_std = stdize(hypothetical2_corruptother_rescaled, to = hypothetical2_corruptother_rescaled_baseline),
    hypothetical2_corruptself_std_baseline = stdize(hypothetical2_corruptself_rescaled_baseline, to = hypothetical2_corruptself_rescaled_baseline),
    hypothetical2_corruptother_std_baseline = stdize(hypothetical2_corruptother_rescaled_baseline, to = hypothetical2_corruptother_rescaled_baseline),
    
    hypothetical3_corruptself_std = stdize(hypothetical3_corruptself_rescaled, to = hypothetical3_corruptself_rescaled_baseline),
    hypothetical3_corruptother_std = stdize(hypothetical3_corruptother_rescaled, to = hypothetical3_corruptother_rescaled_baseline),
    hypothetical3_corruptself_std_baseline = stdize(hypothetical3_corruptself_rescaled_baseline, to = hypothetical3_corruptself_rescaled_baseline),
    hypothetical3_corruptother_std_baseline = stdize(hypothetical3_corruptother_rescaled_baseline, to = hypothetical3_corruptother_rescaled_baseline),
    
    accountability_idx = idx_mean_listwise(account_pol_matter_std, hypothetical2_reportothers_std, hypothetical3_reportothers_std, hypothetical5_reportothers_std, hypothetical5_reportself_std, hypothetical2_reportself_std, hypothetical3_reportself_std, hypothetical2_punishment_std, hypothetical3_punishment_std, hypothetical5_punishment_std), # , tx = Z, fe = block_ID),
    accountability_idx_common = idx_mean_listwise(account_pol_matter_std, hypothetical2_reportothers_std, hypothetical3_reportothers_std, hypothetical5_reportothers_std, hypothetical5_reportself_std, hypothetical2_reportself_std, hypothetical3_reportself_std), #, tx = Z, fe = block_ID),
    accountability_idx_baseline = idx_mean_listwise(account_pol_matter_std_baseline, hypothetical2_reportothers_std_baseline, hypothetical3_reportothers_std_baseline, hypothetical5_reportothers_std_baseline, hypothetical5_reportself_std_baseline, hypothetical2_reportself_std_baseline, hypothetical3_reportself_std_baseline, hypothetical2_punishment_std_baseline, hypothetical3_punishment_std_baseline, hypothetical5_punishment_std_baseline), #, tx = Z, fe = block_ID),
    accountability_idx_common_baseline = idx_mean_listwise(account_pol_matter_std_baseline, hypothetical2_reportothers_std_baseline, hypothetical3_reportothers_std_baseline, hypothetical5_reportothers_std_baseline, hypothetical5_reportself_std_baseline, hypothetical2_reportself_std_baseline, hypothetical3_reportself_std_baseline), #, tx = Z, fe = block_ID),
    
    abuse_idx = idx_mean(hypothetical5_abuseself_std, hypothetical5_abuseother_std, tx = Z, fe = block_ID),
    abuse_idx_baseline = idx_mean(hypothetical5_abuseself_std_baseline, hypothetical5_abuseother_std_baseline, tx = Z, fe = block_ID),
    
    corrupt_idx = idx_mean(hypothetical2_corruptself_std, hypothetical2_corruptother_std, hypothetical3_corruptself_std, hypothetical3_corruptother_std, tx = Z, fe = block_ID),
    corrupt_idx_baseline = idx_mean(hypothetical2_corruptself_std_baseline, hypothetical2_corruptother_std_baseline, hypothetical3_corruptself_std_baseline, hypothetical3_corruptother_std_baseline, tx = Z, fe = block_ID),
    
    officer_attitude_idx = idx_mean(empathy_idx, corrupt_idx, abuse_idx, accountability_idx_common, tx = Z, fe = block_ID),
    officer_attitude_idx_baseline = idx_mean(empathy_idx_baseline, corrupt_idx_baseline, abuse_idx_baseline, accountability_idx_common_baseline, tx = Z, fe = block_ID),
    # # re-standardize
    officer_attitude_idx = stdize(officer_attitude_idx, to = officer_attitude_idx_baseline),
    officer_attitude_idx_baseline = stdize(officer_attitude_idx_baseline, to = officer_attitude_idx_baseline))

# --------------------------------------------------------------------------------------------------------------
                        # 4. Hypotheses 3(b): police_abuse_idx
# --------------------------------------------------------------------------------------------------------------
uga_citizen <-
  uga_citizen %>%
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
    bribe_amt = bribe_amt / 3714,
    bribe_amt_baseline = bribe_amt_baseline / 3714,
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
    police_abuse_idx = idx_mean(policeabuse_any_std, policeabuse_num_std,  bribe_freq_std, bribe_amt_std, tx = Z, fe = block_ID),
    police_abuse_idx_baseline = idx_mean(policeabuse_any_std_baseline, policeabuse_num_std_baseline, bribe_freq_std_baseline, bribe_amt_std_baseline, tx = Z, fe = block_ID),
    # re-standardize
    police_abuse_idx = stdize(police_abuse_idx, to = police_abuse_idx_baseline),
    police_abuse_idx_baseline = stdize(police_abuse_idx_baseline, to = police_abuse_idx_baseline))

# 2. Common analysis
uga_citizen <-
  uga_citizen %>%
  mutate(
    police_abuse_idx_common = idx_mean(policeabuse_any_std, bribe_freq_std, bribe_amt_std, tx = Z, fe = block_ID),
    police_abuse_idx_common_baseline = idx_mean(policeabuse_any_std_baseline, bribe_freq_std_baseline, bribe_amt_std_baseline, tx = Z, fe = block_ID),
    # re-standardize
    police_abuse_idx_common = stdize(police_abuse_idx_common, to = police_abuse_idx_common_baseline),
    police_abuse_idx_common_baseline = stdize(police_abuse_idx_common_baseline, to = police_abuse_idx_common_baseline))

# 3. List-wise deletion
uga_citizen <-
  uga_citizen %>%
  mutate(
    police_abuse_idx_listwise = idx_mean_listwise(policeabuse_any_std, bribe_freq_std, bribe_amt_std),
    police_abuse_idx_listwise_baseline = idx_mean_listwise(policeabuse_any_std_baseline, bribe_freq_std_baseline, bribe_amt_std_baseline),
    # re-standardize
    police_abuse_idx_listwise = stdize(police_abuse_idx_listwise, to = police_abuse_idx_listwise_baseline),
    police_abuse_idx_listwise_baseline = stdize(police_abuse_idx_listwise_baseline, to = police_abuse_idx_listwise_baseline))

#--------------------------------------------------------------------------------------------------------------
#                     # 5. Hypotheses 4(a): crime_reporting_idx
#--------------------------------------------------------------------------------------------------------------
uga_citizen <-
  uga_citizen %>%
  mutate(
    # generate crime reporting categories
    armedrob_report = case_when(
        armedrob_num == 0 | str_detect(armedrob_report, paste0("\\b", "0", "\\b")) ~ 0,
        armedrob_num > 0 &  str_detect(armedrob_report, paste0("\\b", "1", "\\b")) ~ 1),
    burglary_report = case_when(
        burglary_num == 0 | str_detect(burglary_report, paste0("\\b", "0", "\\b")) ~ 0,
        burglary_num > 0 &  str_detect(burglary_report, paste0("\\b", "1", "\\b")) ~ 1),
    simpleassault_report = case_when(
        simpleassault_num == 0 | str_detect(simpleassault_report, paste0("\\b", "0", "\\b")) ~ 0,
        simpleassault_num > 0 & str_detect(simpleassault_report, paste0("\\b", "1", "\\b")) ~ 1),
    carmedrob_report = case_when(
        carmedrob_num == 0 | str_detect(carmedrob_report, paste0("\\b", "0", "\\b")) ~ 0,
        carmedrob_num > 0 &  str_detect(carmedrob_report, paste0("\\b", "1", "\\b")) ~ 1),
    cburglary_report = case_when(
        cburglary_num == 0 | str_detect(cburglary_report, paste0("\\b", "0", "\\b")) ~ 0,
        cburglary_num > 0 &  str_detect(cburglary_report, paste0("\\b", "1", "\\b")) ~ 1),
    caggassault_report = case_when(
        caggassault_num == 0 | str_detect(caggassault_report, paste0("\\b", "0", "\\b")) ~ 0,
        caggassault_num > 0 &  str_detect(caggassault_report, paste0("\\b", "1", "\\b")) ~ 1),
    csimpleassault_report = case_when(
        csimpleassault_num == 0 | str_detect(csimpleassault_report, paste0("\\b", "0", "\\b")) ~ 0,
        csimpleassault_num > 0 &  str_detect(csimpleassault_report, paste0("\\b", "1", "\\b")) ~ 1),
    csexual_report = case_when(
        csexual_num == 0 | str_detect(csexual_report, paste0("\\b", "0", "\\b")) ~ 0,
        csexual_num > 0 &  str_detect(csexual_report, paste0("\\b", "1", "\\b")) ~ 1),
    cdomestic_phys_report = case_when(
        cdomestic_phys_num == 0 | str_detect(cdomestic_phys_report, paste0("\\b", "0", "\\b")) ~ 0,
        cdomestic_phys_num > 0 &  str_detect(cdomestic_phys_report, paste0("\\b", "1", "\\b")) ~ 1),
    cmurder_report = case_when(
       cmurder_num == 0 | str_detect(cmurder_report, paste0("\\b", "0", "\\b")) ~ 0,
       cmurder_num > 0 &  str_detect(cmurder_report, paste0("\\b", "1", "\\b")) ~ 1),

    armedrob_report_baseline = case_when(
        armedrob_num_baseline == 0 | str_detect(armedrob_report_baseline, paste0("\\b", "0", "\\b")) ~ 0,
        armedrob_num_baseline > 0 &  str_detect(armedrob_report_baseline, paste0("\\b", "1", "\\b")) ~ 1),
    burglary_report_baseline = case_when(
        burglary_num_baseline == 0 | str_detect(burglary_report_baseline, paste0("\\b", "0", "\\b")) ~ 0,
        burglary_num_baseline > 0 &  str_detect(burglary_report_baseline, paste0("\\b", "1", "\\b")) ~ 1),
    simpleassault_report_baseline = case_when(
        simpleassault_num_baseline == 0 | str_detect(simpleassault_report_baseline, paste0("\\b", "0", "\\b")) ~ 0,
        simpleassault_num_baseline > 0 &  str_detect(simpleassault_report_baseline, paste0("\\b", "1", "\\b")) ~ 1),
    carmedrob_report_baseline = case_when(
        carmedrob_num_baseline == 0 | str_detect(carmedrob_report_baseline, paste0("\\b", "0", "\\b")) ~ 0,
        carmedrob_num_baseline > 0 &  str_detect(carmedrob_report_baseline, paste0("\\b", "1", "\\b")) ~ 1),
    cburglary_report_baseline = case_when(
        cburglary_num_baseline == 0 | str_detect(cburglary_report_baseline, paste0("\\b", "0", "\\b")) ~ 0,
        cburglary_num_baseline > 0 &  str_detect(cburglary_report_baseline, paste0("\\b", "1", "\\b")) ~ 1),
    caggassault_report_baseline = case_when(
        caggassault_num_baseline == 0 | str_detect(caggassault_report_baseline, paste0("\\b", "0", "\\b")) ~ 0,
        caggassault_num_baseline > 0 &  str_detect(caggassault_report_baseline, paste0("\\b", "1", "\\b")) ~ 1),
    csimpleassault_report_baseline = case_when(
        csimpleassault_num_baseline == 0 | str_detect(csimpleassault_report_baseline, paste0("\\b", "0", "\\b")) ~ 0,
        csimpleassault_num_baseline > 0 &  str_detect(csimpleassault_report_baseline, paste0("\\b", "1", "\\b")) ~ 1),
    csexual_report_baseline = case_when(
        csexual_num_baseline == 0 | str_detect(csexual_report_baseline, paste0("\\b", "0", "\\b")) ~ 0,
        csexual_num_baseline > 0 &  str_detect(csexual_report_baseline, paste0("\\b", "1", "\\b")) ~ 1),
    cdomestic_phys_report_baseline = case_when(
        cdomestic_phys_num_baseline == 0 | str_detect(cdomestic_phys_report_baseline, paste0("\\b", "0", "\\b")) ~ 0,
        cdomestic_phys_num_baseline > 0 &  str_detect(cdomestic_phys_report_baseline, paste0("\\b", "1", "\\b")) ~ 1),
    cmurder_report_baseline = case_when(
        cmurder_num_baseline == 0 | str_detect(cmurder_report_baseline, paste0("\\b", "0", "\\b")) ~ 0,
        cmurder_num_baseline > 0 &  str_detect(cmurder_report_baseline, paste0("\\b", "1", "\\b")) ~ 1),

    burglaryres = case_when(
        burglaryres %in% c("1", "2") ~ 1L,
        !is.na(burglaryres) ~ 0L),
    dviolres = case_when(
        dviolres %in% c("1", "2") ~ 1L,
        !is.na(dviolres) ~ 0L),
    armedrobres = case_when(
        armedrobres %in% c("1", "2") ~ 1L,
        !is.na(armedrobres) ~ 0L),
    
    burglaryres_baseline = case_when(
      burglaryres_baseline %in% c("1", "2") ~ 1L,
      !is.na(burglaryres_baseline) ~ 0L),
    dviolres_baseline = case_when(
      dviolres_baseline %in% c("1", "2") ~ 1L,
      !is.na(dviolres_baseline) ~ 0L),
    armedrobres_baseline = case_when(
      armedrobres_baseline %in% c("1", "2") ~ 1L,
      !is.na(armedrobres_baseline) ~ 0L)) %>%
  rowwise() %>%
  mutate(
    violentcrime_report_num = sum(armedrob_report, simpleassault_report, other_report_violent, na.rm = TRUE),
    nonviolentcrime_report_num = sum(burglary_report, other_report_nonviolent, na.rm = TRUE),
    cviolentcrime_report_num = sum(carmedrob_report, caggassault_report, csimpleassault_report, csexual_report, cdomestic_phys_report, cmurder_report, cother_report_violent, na.rm = TRUE),
    cnonviolentcrime_report_num = sum(cburglary_report, cother_report_nonviolent, na.rm = TRUE),

    violentcrime_report_num_baseline = sum(armedrob_report_baseline, simpleassault_report_baseline, other_report_violent_baseline, na.rm = TRUE),
    nonviolentcrime_report_num_baseline = sum(burglary_report_baseline, other_report_nonviolent_baseline, na.rm = TRUE),
    cviolentcrime_report_num_baseline = sum(carmedrob_report_baseline, caggassault_report_baseline, csimpleassault_report_baseline, csexual_report_baseline, cdomestic_phys_report_baseline, cmurder_report_baseline, cother_report_violent_baseline, na.rm = TRUE),
    cnonviolentcrime_report_num_baseline = sum(cburglary_report_baseline, cother_report_nonviolent_baseline, na.rm = TRUE),
  ) %>%
  ungroup() %>%
  mutate(    
    # standarduze all vars
    burglaryres_std = stdize(burglaryres, to = burglaryres_baseline),
    dviolres_std = stdize(dviolres, to = dviolres_baseline),
    armedrobres_std = stdize(armedrobres, to = armedrobres_baseline),

    burglaryres_std_baseline = stdize(burglaryres_baseline, to = burglaryres_baseline),
    dviolres_std_baseline = stdize(dviolres_baseline, to = dviolres_baseline),
    armedrobres_std_baseline = stdize(armedrobres_baseline, to = armedrobres_baseline),

    violentcrime_report_num_std = stdize(violentcrime_report_num, to = violentcrime_report_num_baseline),
    nonviolentcrime_report_num_std = stdize(nonviolentcrime_report_num, to = nonviolentcrime_report_num_baseline),
    cviolentcrime_report_num_std = stdize(cviolentcrime_report_num, to = cviolentcrime_report_num_baseline),
    cnonviolentcrime_report_num_std = stdize(cnonviolentcrime_report_num, to = cnonviolentcrime_report_num_baseline),
    
    violentcrime_report_num_std_baseline = stdize(violentcrime_report_num_baseline, to = violentcrime_report_num_baseline),
    nonviolentcrime_report_num_std_baseline = stdize(nonviolentcrime_report_num_baseline, to = nonviolentcrime_report_num_baseline),
    cviolentcrime_report_num_std_baseline = stdize(cviolentcrime_report_num_baseline, to = cviolentcrime_report_num_baseline),
    cnonviolentcrime_report_num_std_baseline = stdize(cnonviolentcrime_report_num_baseline, to = cnonviolentcrime_report_num_baseline),
    # standarduze individual crimes
    armedrob_report_std = stdize(armedrob_report, to = armedrob_report_baseline),
    simpleassault_report_std = stdize(simpleassault_report, to = simpleassault_report_baseline),
    other_report_violent_std = stdize(other_report_violent, to = other_report_violent_baseline),
    burglary_report_std = stdize(burglary_report, to = burglary_report_baseline),
    other_report_nonviolent_std = stdize(other_report_nonviolent, to = other_report_nonviolent_baseline),
    carmedrob_report_std = stdize(carmedrob_report, to = carmedrob_report_baseline),
    caggassault_report_std = stdize(caggassault_report, to = caggassault_report_baseline),
    csimpleassault_report_std = stdize(csimpleassault_report, to = csimpleassault_report_baseline),
    csexual_report_std = stdize(csexual_report, to = csexual_report_baseline),
    cdomestic_phys_report_std = stdize(cdomestic_phys_report, to = cdomestic_phys_report_baseline),
    cother_report_violent_std = stdize(cother_report_violent, to = cother_report_violent_baseline),
    cburglary_report_std = stdize(cburglary_report, to = cburglary_report_baseline),
    cother_report_violent_std = stdize(cother_report_violent, to = cother_report_violent_baseline),
    cother_report_nonviolent_std = stdize(cother_report_nonviolent, to = cother_report_nonviolent_baseline),

    armedrob_report_std_baseline = stdize(armedrob_report_baseline, to = armedrob_report_baseline),
    simpleassault_report_std_baseline = stdize(simpleassault_report_baseline, to = simpleassault_report_baseline),
    other_report_violent_std_baseline = stdize(other_report_violent_baseline, to = other_report_violent_baseline),
    burglary_report_std_baseline = stdize(burglary_report_baseline, to = burglary_report_baseline),
    other_report_nonviolent_std_baseline = stdize(other_report_nonviolent_baseline, to = other_report_nonviolent_baseline),
    carmedrob_report_std_baseline = stdize(carmedrob_report_baseline, to = carmedrob_report_baseline),
    caggassault_report_std_baseline = stdize(caggassault_report_baseline, to = caggassault_report_baseline),
    csimpleassault_report_std_baseline = stdize(csimpleassault_report_baseline, to = csimpleassault_report_baseline),
    csexual_report_std_baseline = stdize(csexual_report_baseline, to = csexual_report_baseline),
    cdomestic_phys_report_std_baseline = stdize(cdomestic_phys_report_baseline, to = cdomestic_phys_report_baseline),
    cother_report_violent_std_baseline = stdize(cother_report_violent_baseline, to = cother_report_violent_baseline),
    cburglary_report_std_baseline = stdize(cburglary_report_baseline, to = cburglary_report_baseline),
    cother_report_violent_std_baseline = stdize(cother_report_violent_baseline, to = cother_report_violent_baseline),
    cother_report_nonviolent_std_baseline = stdize(cother_report_nonviolent_baseline, to = cother_report_nonviolent_baseline),
    # calculate sub-index
    crimeres_idx = idx_mean(burglaryres_std, dviolres_std, armedrobres_std, tx = Z, fe = block_ID),
    crimeres_idx_baseline = idx_mean(burglaryres_std_baseline, dviolres_std_baseline, armedrobres_std_baseline, tx = Z, fe = block_ID),
    # calculate index
    crime_reporting_idx = idx_mean(violentcrime_report_num_std, nonviolentcrime_report_num_std, cviolentcrime_report_num_std, cnonviolentcrime_report_num_std, crimeres_idx, tx = Z, fe = block_ID),
    crime_reporting_idx_baseline = idx_mean(violentcrime_report_num_std_baseline, nonviolentcrime_report_num_std_baseline, cviolentcrime_report_num_std_baseline, cnonviolentcrime_report_num_std_baseline, crimeres_idx_baseline, tx = Z, fe = block_ID),
    # re-standardize
    crime_reporting_idx = stdize(crime_reporting_idx, to = crime_reporting_idx_baseline),
    crime_reporting_idx_baseline = stdize(crime_reporting_idx_baseline, to = crime_reporting_idx_baseline)
  )

# 2. common analysis
uga_citizen <- 
  uga_citizen %>% 
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
    crimeres_idx_common = idx_mean(burglaryres_std, dviolres_std, tx = Z, fe = block_ID),
    crimeres_idx_common_baseline = idx_mean(burglaryres_std_baseline, dviolres_std_baseline, tx = Z, fe = block_ID),
    # calculate index
    crime_reporting_idx_common = idx_mean(violentcrime_report_num_common_std, nonviolentcrime_report_num_common_std, cviolentcrime_report_num_common_std, cnonviolentcrime_report_num_common_std, crimeres_idx_common, tx = Z, fe = block_ID),
    crime_reporting_idx_common_baseline = idx_mean(violentcrime_report_num_common_std_baseline, nonviolentcrime_report_num_common_std_baseline, cviolentcrime_report_num_common_std_baseline, cnonviolentcrime_report_num_common_std_baseline, crimeres_idx_common_baseline, tx = Z, fe = block_ID),
    # re-standardize
    crime_reporting_idx_common = stdize(crime_reporting_idx_common, to = crime_reporting_idx_common_baseline),
    crime_reporting_idx_common_baseline = stdize(crime_reporting_idx_common_baseline, to = crime_reporting_idx_common_baseline)
  )

# 3. List-wise deletion
uga_citizen <-
  uga_citizen %>%
  mutate(
    # calculate sub-index
    crimeres_idx_listwise = idx_mean_listwise(burglaryres_std, dviolres_std),
    crimeres_idx_listwise_baseline = idx_mean_listwise(burglaryres_std_baseline, dviolres_std_baseline),
    # calculate index
    crime_reporting_idx_listwise = idx_mean_listwise(violentcrime_report_num_common_std, nonviolentcrime_report_num_common_std, cviolentcrime_report_num_common_std, cnonviolentcrime_report_num_common_std, crimeres_idx_common),
    crime_reporting_idx_listwise_baseline = idx_mean_listwise(violentcrime_report_num_common_std_baseline, nonviolentcrime_report_num_common_std_baseline, cviolentcrime_report_num_common_std_baseline, cnonviolentcrime_report_num_common_std_baseline, crimeres_idx_common_baseline),
    # re-standardize
    crime_reporting_idx_listwise = stdize(crime_reporting_idx_listwise, to = crime_reporting_idx_listwise_baseline),
    crime_reporting_idx_listwise_baseline = stdize(crime_reporting_idx_listwise_baseline, to = crime_reporting_idx_listwise_baseline))
    
#--------------------------------------------------------------------------------------------------------------
                            # 6. Hypotheses 4(b): tips_idx
#--------------------------------------------------------------------------------------------------------------
uga_citizen <- 
  uga_citizen %>%
  mutate(
    # standardize the vars
    contact_pol_susp_activity_std = stdize(contact_pol_susp_activity, to = contact_pol_susp_activity_baseline),
    give_info_pol_investigation_std = stdize(give_info_pol_investigation, to = give_info_pol_investigation_baseline),
    contact_pol_susp_activity_std_baseline = stdize(contact_pol_susp_activity_baseline, to = contact_pol_susp_activity_baseline),
    give_info_pol_investigation_std_baseline = stdize(give_info_pol_investigation_baseline, to = give_info_pol_investigation_baseline),
    # calculate sub-index
    crime_tips_idx = idx_mean(contact_pol_susp_activity_std, give_info_pol_investigation_std, tx = Z, fe = block_ID),
    crime_tips_idx_baseline = idx_mean(contact_pol_susp_activity_std_baseline, give_info_pol_investigation_std_baseline, tx = Z, fe = block_ID),
    # re-standardize
    crime_tips_idx = stdize(crime_tips_idx, to = crime_tips_idx_baseline),
    crime_tips_idx_baseline = stdize(crime_tips_idx_baseline, to = crime_tips_idx_baseline),
    # calculate index
    tips_idx = crime_tips_idx,
    tips_idx_baseline = crime_tips_idx_baseline
    )


# 3. List-wise deletion
uga_citizen <-
  uga_citizen %>%
  mutate(
    crime_tips_idx_listwise = idx_mean_listwise(contact_pol_susp_activity_std, give_info_pol_investigation_std),
    crime_tips_idx_listwise_baseline = idx_mean_listwise(contact_pol_susp_activity_std_baseline, give_info_pol_investigation_std_baseline),
    # re-standardize
    crime_tips_idx_listwise = stdize(crime_tips_idx_listwise, to = crime_tips_idx_listwise_baseline),
    crime_tips_idx_listwise_baseline = stdize(crime_tips_idx_listwise_baseline, to = crime_tips_idx_listwise_baseline),
    # make names consistent with mpap
    tips_idx_listwise = crime_tips_idx_listwise,
    tips_idx_listwise_baseline = crime_tips_idx_listwise_baseline)

#--------------------------------------------------------------------------------------------------------------
                          # 7. Hypotheses 4(c): police_abuse_report_idx
#--------------------------------------------------------------------------------------------------------------
uga_citizen <- 
  uga_citizen %>%
  # join with admin data for admin vars
  left_join(uga_admin %>% distinct(station_id, .keep_all = TRUE) %>% mutate(apolvtm_station_std = stdize(apolvtm_station, condition = (Z_common == 0))) %>% select(station_id, apolvtm_station_std), by = "station_id") %>%
  mutate(
    policeabuse_report = case_when(
      str_detect(policeabuse_verbal_report, paste0("\\b", "1", "\\b")) | str_detect(policeabuse_phys_report, paste0("\\b", "1", "\\b")) ~ 1L,
        !is.na(policeabuse_verbal_any) ~ 0L), 
    policeabuse_report_baseline = case_when(
      str_detect(policeabuse_verbal_report_baseline, paste0("\\b", "1", "\\b")) | str_detect(policeabuse_phys_report_baseline, paste0("\\b", "1", "\\b")) ~ 1L,
      !is.na(policeabuse_verbal_any_baseline) ~ 0L), 
    # standardize the vars
    policeabuse_report_std = stdize(policeabuse_report, to = policeabuse_report_baseline),
    dutydrink_report_std = stdize(dutydrink_report, to = dutydrink_report_baseline),
    policebeating_report_std = stdize(policebeating_report, to = policebeating_report_baseline),
    policeabuse_report_std_baseline = stdize(policeabuse_report_baseline, to = policeabuse_report_baseline),
    dutydrink_report_std_baseline = stdize(dutydrink_report_baseline, to = dutydrink_report_baseline),
    policebeating_report_std_baseline = stdize(policebeating_report_baseline, to = policebeating_report_baseline),
    # calculate index
    police_abuse_report_idx = idx_mean(policeabuse_report_std, dutydrink_report_std, policebeating_report_std, apolvtm_station_std, tx = Z, fe = block_ID),
    police_abuse_report_idx_baseline = idx_mean(policeabuse_report_std_baseline, dutydrink_report_std_baseline, policebeating_report_std_baseline, tx = Z, fe = block_ID),
    # re-standardize
    police_abuse_report_idx = stdize(police_abuse_report_idx, to = police_abuse_report_idx_baseline),
    police_abuse_report_idx_baseline = stdize(police_abuse_report_idx_baseline, to = police_abuse_report_idx_baseline)
  )

uga_citizen <- 
  uga_citizen %>%
  mutate(
    # general police abuse components
    policeabuse_report_common = case_when(
      policeabuse_verbal_report == 1 ~ 1L,
      !is.na(policeabuse_verbal_any) ~ 0L), 
    policeabuse_report_common_baseline = case_when(
      policeabuse_verbal_report_baseline == 1 ~ 1L,
      !is.na(policeabuse_verbal_any_baseline) ~ 0L), 
    # standardize vars
    policeabuse_report_common_std = stdize(policeabuse_report_common, to = policeabuse_report_common_baseline),
    policeabuse_report_common_std_baseline = stdize(policeabuse_report_common_baseline, to = policeabuse_report_common_baseline),
    # calculate index
    police_abuse_report_idx_common = idx_mean(policeabuse_report_common_std, policebeating_report_std, tx = Z, fe = block_ID),
    police_abuse_report_idx_common_baseline = idx_mean(policeabuse_report_common_std_baseline, policebeating_report_std_baseline, tx = Z, fe = block_ID),
    # re-standardize
    police_abuse_report_idx_common = stdize(police_abuse_report_idx_common, to = police_abuse_report_idx_common_baseline),
    police_abuse_report_idx_common_baseline = stdize(police_abuse_report_idx_common_baseline, to = police_abuse_report_idx_common_baseline))

# 3. List-wise deletion
uga_citizen <-
  uga_citizen %>%
  mutate(
    police_abuse_report_idx_listwise = idx_mean_listwise(policeabuse_report_common_std, policebeating_report_std),
    police_abuse_report_idx_listwise_baseline = idx_mean_listwise(policeabuse_report_common_std_baseline, policebeating_report_std_baseline),
    # re-standardize
    police_abuse_report_idx_listwise = stdize(police_abuse_report_idx_listwise, to = police_abuse_report_idx_listwise_baseline),
    police_abuse_report_idx_listwise_baseline = stdize(police_abuse_report_idx_listwise_baseline, to = police_abuse_report_idx_listwise_baseline))

#--------------------------------------------------------------------------------------------------------------
                        # 8. Hypotheses M1a: intentions_idx
#--------------------------------------------------------------------------------------------------------------
uga_citizen <- 
  uga_citizen %>%
  mutate(
    # reverse order of variables to be consistent with the direction (MPAP page 75-76)
    polcasefair_raw = polcasefair,
    polcasefair_rescaled = (polcasefair - 1), 
    polcaseserious_raw = polcaseserious,
    polcaseserious_rescaled = (polcaseserious - 1), 
    polint_corrupt_raw = polint_corrupt,
    polint_corrupt_rescaled = 4 - (polint_corrupt - 1), 
    polint_quality_raw = polint_quality,
    polint_quality_rescaled = (polint_quality - 1), 
    
    polcasefair_raw_baseline = polcasefair_baseline,
    polcasefair_rescaled_baseline = (polcasefair_baseline - 1), 
    polcaseserious_raw_baseline = polcaseserious_baseline,
    polcaseserious_rescaled_baseline = (polcaseserious_baseline - 1), 
    polint_corrupt_raw_baseline = polint_corrupt_baseline,
    # following the procedure for how the baseline is asked for uga, the following rescaling for polint_corrupt_baseline in Uga is correct, please do not change
    polint_corrupt_rescaled_baseline = (polint_corrupt_baseline - 1), 
    polint_quality_raw_baseline = polint_quality_baseline,
    polint_quality_rescaled_baseline = (polint_quality_baseline - 1), 
    # standardize the vars
    polint_corrupt_std = stdize(polint_corrupt_rescaled, to = polint_corrupt_rescaled_baseline),
    polint_quality_std = stdize(polint_quality_rescaled, to = polint_quality_rescaled_baseline),
    polcaseserious_std = stdize(polcaseserious_rescaled, to = polcaseserious_rescaled_baseline),
    polcasefair_std = stdize(polcasefair_rescaled, to = polcasefair_rescaled_baseline),
    polcaseserious_std_baseline = stdize(polcaseserious_rescaled_baseline, to = polcaseserious_rescaled_baseline),
    polcasefair_std_baseline = stdize(polcasefair_rescaled_baseline, to = polcasefair_rescaled_baseline), 
    polint_corrupt_std_baseline = stdize(polint_corrupt_rescaled_baseline, to = polint_corrupt_rescaled_baseline),
    polint_quality_std_baseline = stdize(polint_quality_rescaled_baseline, to = polint_quality_rescaled_baseline),
    # calculate sub-index
    polint_idx = idx_mean(polint_corrupt_std, polint_quality_std, tx = Z, fe = block_ID),
    polint_idx_baseline = idx_mean(polint_corrupt_std_baseline, polint_quality_std_baseline, tx = Z, fe = block_ID),
    # calculate index
    intentions_idx = idx_mean(polint_idx, polcaseserious_std, polcasefair_std, tx = Z, fe = block_ID),
    intentions_idx_baseline = idx_mean(polint_idx_baseline, polcaseserious_std_baseline, polcasefair_std_baseline, tx = Z, fe = block_ID),
    # re-standardize
    intentions_idx = stdize(intentions_idx, to = intentions_idx_baseline),
    intentions_idx_baseline = stdize(intentions_idx_baseline, to = intentions_idx_baseline)
  )

# 3. List-wise deletion
uga_citizen <-
  uga_citizen %>%
  mutate(
    polint_idx_listwise = idx_mean_listwise(polint_corrupt_std, polint_quality_std),
    polint_idx_listwise_baseline = idx_mean_listwise(polint_corrupt_std_baseline, polint_quality_std_baseline),
    
    intentions_idx_listwise = idx_mean_listwise(polint_idx_listwise, polcaseserious_std, polcasefair_std),
    intentions_idx_listwise_baseline = idx_mean_listwise(polint_idx_listwise_baseline, polcaseserious_std_baseline, polcasefair_std_baseline),
    # re-standardize
    intentions_idx_listwise = stdize(intentions_idx_listwise, to = intentions_idx_listwise_baseline),
    intentions_idx_listwise_baseline = stdize(intentions_idx_listwise_baseline, to = intentions_idx_listwise_baseline))

#--------------------------------------------------------------------------------------------------------------
                              # 9. Hypotheses M1b: know_idx
#--------------------------------------------------------------------------------------------------------------
uga_citizen <-
  uga_citizen %>%
  mutate(
    # reverse order of variables to be consistent with the direction (MPAP page 75-76)
    know_law_suspect_rescaled = 1 - know_law_suspect,
    know_law_lawyer_rescaled = know_law_lawyer,
    know_law_fees_rescaled = 1 - know_law_fees,
    know_law_vaw_rescaled = know_law_vaw,
    
    know_law_suspect_rescaled_baseline = 1 - know_law_suspect_baseline,
    know_law_lawyer_rescaled_baseline = know_law_lawyer_baseline,
    know_law_fees_rescaled_baseline = 1 - know_law_fees_baseline,
    know_law_vaw_rescaled_baseline = know_law_vaw_baseline,
    
    know_report_followup_rescaled = 1 - know_report_followup,
    know_report_followup_rescaled_baseline = 1 - know_report_followup_baseline,
    know_report_station_rescaled = know_report_station,
    know_report_station_rescaled_baseline = know_report_station_baseline,
    # standardize the vars
    know_law_suspect_std = stdize(know_law_suspect_rescaled, to = know_law_suspect_rescaled_baseline),
    know_law_lawyer_std = stdize(know_law_lawyer_rescaled, to = know_law_lawyer_rescaled_baseline),
    know_law_fees_std = stdize(know_law_fees_rescaled, to = know_law_fees_rescaled_baseline),
    know_law_vaw_std = stdize(know_law_vaw_rescaled, to = know_law_vaw_rescaled_baseline),
    know_report_followup_std = stdize(know_report_followup_rescaled, to = know_report_followup_rescaled_baseline),
    know_report_station_std = stdize(know_report_station_rescaled, to = know_report_station_rescaled_baseline),
    
    know_law_suspect_std_baseline = stdize(know_law_suspect_rescaled_baseline, to = know_law_suspect_rescaled_baseline),
    know_law_lawyer_std_baseline = stdize(know_law_lawyer_rescaled_baseline, to = know_law_lawyer_rescaled_baseline),
    know_law_fees_std_baseline = stdize(know_law_fees_rescaled_baseline, to = know_law_fees_rescaled_baseline),
    know_law_vaw_std_baseline = stdize(know_law_vaw_rescaled_baseline, to = know_law_vaw_rescaled_baseline),
    know_report_std_followup = stdize(know_report_followup_rescaled, to = know_report_followup_rescaled_baseline),
    know_report_std_station = stdize(know_report_station_rescaled, to = know_report_station_rescaled_baseline),
    know_report_followup_std_baseline = stdize(know_report_followup_rescaled_baseline, to = know_report_followup_rescaled_baseline),
    know_report_station_std_baseline = stdize(know_report_station_rescaled_baseline, to = know_report_station_rescaled_baseline),
    # calculate sub-index
    know_law_idx = idx_mean(know_law_suspect_std, know_law_lawyer_std, know_law_fees_std, know_law_vaw_std, tx = Z, fe = block_ID),
    know_report_idx = idx_mean(know_report_followup_std, know_report_station_std, tx = Z, fe = block_ID),
    know_report_idx_baseline = idx_mean(know_report_followup_std_baseline, know_report_station_std_baseline, tx = Z, fe = block_ID),
    know_law_idx_baseline = idx_mean(know_law_suspect_std_baseline, know_law_lawyer_std_baseline, know_law_fees_std_baseline, know_law_vaw_std_baseline, tx = Z, fe = block_ID),
    # calculate index
    know_idx = idx_mean(know_law_idx, know_report_idx, tx = Z, fe = block_ID),
    know_idx_baseline = idx_mean(know_law_idx_baseline, know_report_idx_baseline, tx = Z, fe = block_ID),
    # re-standardize
    know_idx = stdize(know_idx, to = know_idx_baseline),
    know_idx_baseline = stdize(know_idx_baseline, to = know_idx_baseline)
  )

# 2. common
uga_citizen <-
  uga_citizen %>%
  mutate(
    # standardize the vars
    # calculate sub-index
    know_law_idx_common = idx_mean(know_law_suspect_std, know_law_lawyer_std, know_law_fees_std, tx = Z, fe = block_ID),
    know_law_idx_common_baseline = idx_mean(know_law_suspect_std_baseline, know_law_lawyer_std_baseline, know_law_fees_std_baseline, tx = Z, fe = block_ID),
    know_report_idx_common = know_report_station_std,
    know_report_idx_common_baseline = know_report_station_std_baseline,
    # calculate index
    know_idx_common = idx_mean(know_law_idx_common, know_report_idx_common, tx = Z, fe = block_ID),
    know_idx_common_baseline = idx_mean(know_law_idx_common_baseline, know_report_idx_common_baseline, tx = Z, fe = block_ID),
    # re-standardize
    know_idx_common = stdize(know_idx_common, to = know_idx_common_baseline),
    know_idx_common_baseline = stdize(know_idx_common_baseline, to = know_idx_common_baseline))

# 3. List-wise deletion
uga_citizen <-
  uga_citizen %>%
  mutate(
    know_report_idx_listwise = know_report_station_std,
    know_report_idx_listwise_baseline = know_report_station_std_baseline,
    
    know_law_idx_listwise = idx_mean_listwise(know_law_suspect_std, know_law_lawyer_std, know_law_fees_std),
    know_law_idx_listwise_baseline = idx_mean_listwise(know_law_suspect_std_baseline, know_law_lawyer_std_baseline, know_law_fees_std_baseline),
    
    know_idx_listwise = idx_mean_listwise(know_law_idx, know_report_station_std),
    know_idx_listwise_baseline = idx_mean_listwise(know_law_idx, know_report_station_std),
    # re-standardize
    know_idx_listwise = stdize(know_idx_listwise, to = know_idx_listwise_baseline),
    know_idx_listwise_baseline = stdize(know_idx_listwise_baseline, to = know_idx_listwise_baseline))

#--------------------------------------------------------------------------------------------------------------
                              # 10. Hypotheses M1c: norm_idx
#--------------------------------------------------------------------------------------------------------------
uga_citizen <- 
  uga_citizen %>%
  mutate(
    # reverse order of variables to be consistent with the direction (MPAP page 75-76)
    reportnorm_theft_raw = reportnorm_theft,
    reportnorm_theft_rescaled = 4 - (reportnorm_theft - 1),
    reportnorm_abuse_raw = reportnorm_abuse,
    reportnorm_abuse_rescaled = 4 - (reportnorm_abuse - 1),
    obeynorm_raw = obeynorm,
    obeynorm_rescaled = 
      # 4 - # removed following issue # 84 on GH
      (obeynorm - 1),

    reportnorm_theft_baseline_raw = reportnorm_theft_baseline,
    reportnorm_theft_rescaled_baseline =  4 - (reportnorm_theft_baseline - 1),
    reportnorm_theft_baseline = reportnorm_theft_rescaled_baseline, 
    reportnorm_abuse_baseline_raw = reportnorm_abuse_baseline,
    reportnorm_abuse_rescaled_baseline =  4 - (reportnorm_abuse_baseline - 1),
    reportnorm_abuse_baseline = reportnorm_abuse_rescaled_baseline,
    obeynorm_baseline_raw = obeynorm_baseline,
    obeynorm_rescaled_baseline =  
      # 4 - # removed following issue # 84 on GH
      (obeynorm_baseline - 1),
    obeynorm_baseline = obeynorm_rescaled_baseline,
    # standardize the vars
    reportnorm_theft_std = stdize(reportnorm_theft_rescaled, to = reportnorm_theft_rescaled_baseline),
    reportnorm_abuse_std = stdize(reportnorm_abuse_rescaled, to = reportnorm_abuse_rescaled_baseline),
    obeynorm_std = stdize(obeynorm_rescaled, to = obeynorm_rescaled_baseline),
    
    reportnorm_theft_std_baseline = stdize(reportnorm_theft_rescaled_baseline, to = reportnorm_theft_rescaled_baseline),
    reportnorm_abuse_std_baseline = stdize(reportnorm_abuse_rescaled_baseline, to = reportnorm_abuse_rescaled_baseline),
    obeynorm_std_baseline = stdize(obeynorm_rescaled_baseline, to = obeynorm_rescaled_baseline),
    # calculate index
    norm_idx = idx_mean(reportnorm_theft_std, reportnorm_abuse_std, obeynorm_std, tx = Z, fe = block_ID),
    norm_idx_baseline = idx_mean(reportnorm_theft_std_baseline, reportnorm_abuse_std_baseline, obeynorm_std_baseline, tx = Z, fe = block_ID),
    # re-standardize
    norm_idx = stdize(norm_idx, to = norm_idx_baseline),
    norm_idx_baseline = stdize(norm_idx_baseline, to = norm_idx_baseline)
    )

# 3. List-wise deletion
uga_citizen <-
  uga_citizen %>%
  mutate(
    norm_idx_listwise = idx_mean_listwise(reportnorm_theft_std, reportnorm_abuse_std, obeynorm_std),
    norm_idx_listwise_baseline = idx_mean_listwise(reportnorm_theft_std_baseline, reportnorm_abuse_std_baseline, obeynorm_std_baseline),
    # re-standardize
    norm_idx_listwise = stdize(norm_idx_listwise, to = norm_idx_listwise_baseline),
    norm_idx_listwise_baseline = stdize(norm_idx_listwise_baseline, to = norm_idx_listwise_baseline))

#--------------------------------------------------------------------------------------------------------------
                          # 11. Hypotheses M2a: police_capacity_idx
#--------------------------------------------------------------------------------------------------------------

uga_citizen <- 
  uga_citizen %>%
  mutate(
    # reverse order of variables to be consistent with the direction of the MPAP (MPAP page 75-76)
    polcap_timely_raw = polcap_timely,
    polcap_timely_rescaled = polcap_timely - 1,
    polcap_investigate_raw = polcap_investigate,
    polcap_investigate_rescaled = polcap_investigate - 1,
    
    polcap_timely_raw_baseline = polcap_timely_baseline,
    polcap_timely_rescaled_baseline = polcap_timely_baseline - 1,
    polcap_timely_baseline = polcap_timely_rescaled_baseline, 
    polcap_investigate_raw_baseline = polcap_investigate_baseline,
    polcap_investigate_rescaled_baseline = polcap_investigate_baseline - 1,
    polcap_investigate_baseline = polcap_investigate_rescaled_baseline,
    # standardize the vars
    polcap_timely_std = stdize(polcap_timely_rescaled, to = polcap_timely_rescaled_baseline),
    polcap_investigate_std = stdize(polcap_investigate_rescaled, to = polcap_investigate_rescaled_baseline),
    polcap_timely_std_baseline = stdize(polcap_timely_rescaled_baseline, to = polcap_timely_rescaled_baseline),
    polcap_investigate_std_baseline = stdize(polcap_investigate_rescaled_baseline, to = polcap_investigate_rescaled_baseline),
    # calculate index
    police_capacity_idx = idx_mean(polcap_timely_std, polcap_investigate_std, tx = Z, fe = block_ID),
    police_capacity_idx_baseline = idx_mean(polcap_timely_std_baseline, polcap_investigate_std_baseline, tx = Z, fe = block_ID),
    # re-standardize
    police_capacity_idx = stdize(police_capacity_idx, to = police_capacity_idx_baseline),
    police_capacity_idx_baseline = stdize(police_capacity_idx_baseline, to = police_capacity_idx_baseline)
  )

# 3. List-wise deletion
uga_citizen <-
  uga_citizen %>%
  mutate(
    police_capacity_idx_listwise = idx_mean_listwise(polcap_timely_std, polcap_investigate_std),
    police_capacity_idx_listwise_baseline = idx_mean_listwise(polcap_timely_std_baseline, polcap_investigate_std_baseline),
    # re-standardize
    police_capacity_idx_listwise = stdize(police_capacity_idx_listwise, to = police_capacity_idx_listwise_baseline),
    police_capacity_idx_listwise_baseline = stdize(police_capacity_idx_listwise_baseline, to = police_capacity_idx_listwise_baseline))

#--------------------------------------------------------------------------------------------------------------
                          # 12. Hypotheses M2b: responsive_act
#--------------------------------------------------------------------------------------------------------------

uga_citizen <- 
  uga_citizen %>%
  mutate(
    # reverse order of variables to be consistent with the direction of the MPAP (MPAP page 75-76)
    responsive_act_raw = responsive_act, 
    responsive_act_rescaled = responsive_act - 1,
    responsive_act_raw_baseline = responsive_act_baseline,
    responsive_act_rescaled_baseline = responsive_act_baseline - 1,
    responsive_act_baseline = responsive_act_rescaled_baseline,
    # calculate index
    responsive_act_std = stdize(responsive_act_rescaled, to = responsive_act_rescaled_baseline),
    responsive_act_std_baseline = stdize(responsive_act_rescaled_baseline, to = responsive_act_rescaled_baseline),
    )

#--------------------------------------------------------------------------------------------------------------
                          # 13. Hypotheses S1: legit_trust
#--------------------------------------------------------------------------------------------------------------

# not collected for Uganda

#--------------------------------------------------------------------------------------------------------------
                          # 14. Hypotheses S2: trust_community
#--------------------------------------------------------------------------------------------------------------
uga_citizen <- 
  uga_citizen %>%
  mutate(
    # calculate index
    trust_community_std = stdize(trust_community, to = trust_community_baseline),
    trust_community_std_baseline = stdize(trust_community_baseline, to = trust_community_baseline)
    )

#--------------------------------------------------------------------------------------------------------------
                          # 15. Hypotheses C: compliance_idx
#--------------------------------------------------------------------------------------------------------------
# compliance_freq and compliance_patrol are different from the mpap
uga_citizen <- 
  uga_citizen %>%
    mutate(
  # compliance vars need to be revised following github issue XX
      compliance_patrol = if_else(compliance_patrol == 6L, 5L, compliance_patrol),
      compliance_freq = if_else(compliance_freq == 6L, 5L, compliance_freq),
      compliance_patrol_baseline = if_else(compliance_patrol_baseline == 6L, 5L, compliance_patrol_baseline),
      compliance_freq_baseline = if_else(compliance_freq_baseline == 6L, 5L, compliance_freq_baseline),
      
      compliance_patrol_rescaled = 6L - compliance_patrol,
      compliance_freq_rescaled = 6L - compliance_freq,
      compliance_meeting_rescaled = compliance_meeting,
      compliance_patrol_rescaled_baseline = 6L - compliance_patrol_baseline,
      compliance_freq_rescaled_baseline = 6L - compliance_freq_baseline,
      compliance_meeting_rescaled_baseline = compliance_meeting_baseline,
      
      # standardize the vars
      compliance_patrol_std = stdize(compliance_patrol_rescaled, to = compliance_patrol_rescaled_baseline),
      compliance_freq_std = stdize(compliance_freq_rescaled, to = compliance_freq_rescaled_baseline),
      compliance_meeting_std = stdize(compliance_meeting_rescaled, to = compliance_meeting_rescaled_baseline),
      compliance_patrol_std_baseline = stdize(compliance_patrol_rescaled_baseline, to = compliance_patrol_rescaled_baseline),
      compliance_freq_std_baseline = stdize(compliance_freq_rescaled_baseline, to = compliance_freq_rescaled_baseline),
      compliance_meeting_std_baseline = stdize(compliance_meeting_rescaled_baseline, to = compliance_meeting_rescaled_baseline),
      # calculate index
      compliance_idx = idx_mean(compliance_patrol_std, compliance_freq_std, compliance_meeting_std, tx = Z, fe = block_ID),
      compliance_idx_baseline = idx_mean(compliance_patrol_std_baseline, compliance_freq_std_baseline, compliance_meeting_std_baseline, tx = Z, fe = block_ID),
      # re-standardize
      compliance_idx = stdize(compliance_idx, to = compliance_idx_baseline),
      compliance_idx_baseline = stdize(compliance_idx_baseline, to = compliance_idx_baseline)
    )

# 3. List-wise deletion
uga_citizen <-
  uga_citizen %>%
  mutate(
    compliance_idx_listwise = idx_mean_listwise(compliance_patrol_std, compliance_freq_std, compliance_meeting_std),
    compliance_idx_listwise_baseline = idx_mean_listwise(compliance_patrol_std_baseline, compliance_freq_std_baseline, compliance_meeting_std_baseline),
    # re-standardize
    compliance_idx_listwise = stdize(compliance_idx_listwise, to = crime_victim_idx_baseline),
    compliance_idx_listwise_baseline = stdize(compliance_idx_listwise_baseline, to = compliance_idx_listwise_baseline))

#--------------------------------------------------------------------------------------------------------------
                            # 16. Hyp 1a. (alt. i): crime_victim_idx_admin
#--------------------------------------------------------------------------------------------------------------
uga_admin <- 
  uga_admin %>% 
  rowwise() %>% 
  mutate(
    # generate categories of crimes
    aviolentcrime_num = sum(aarmedrob_num, aaggassault_num, asexual_num, adomestic_phys_num, amurder_num, aother_num_violent, na.rm = TRUE),
    # Note: asimpleassault_num not included  # assault coded as aggravated assault (GH issue number 100)
    anonviolentcrime_num = sum(aburglary_num, aother_num_nonviolent, na.rm = TRUE),
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
    aaggassault_num_std = stdize(aaggassault_num, to = aaggassault_num_baseline),
    asexual_num_std = stdize(asexual_num, to = asexual_num_baseline),
    adomestic_phys_num_std = stdize(adomestic_phys_num, to = adomestic_phys_num_baseline),
    amurder_num_std = stdize(amurder_num, to = amurder_num_baseline),
    aother_num_violent_std = stdize(aother_num_violent, to = aother_num_violent_baseline),
    aburglary_num_std = stdize(aburglary_num, to = aburglary_num_baseline),
    aother_num_nonviolent_std = stdize(aother_num_nonviolent, to = aother_num_nonviolent_baseline),
    # this is additional 
    apolvtm_station = stdize(apolvtm_station, condition = Z_control == 1),
    
    aarmedrob_num_std_baseline = stdize(aarmedrob_num_baseline, to = aarmedrob_num_baseline),
    aaggassault_num_std_baseline = stdize(aaggassault_num_baseline, to = aaggassault_num_baseline),
    asexual_num_std_baseline = stdize(asexual_num_baseline, to = asexual_num_baseline),
    adomestic_phys_num_std_baseline = stdize(adomestic_phys_num_baseline, to = adomestic_phys_num_baseline),
    amurder_num_std_baseline = stdize(amurder_num_baseline, to = amurder_num_baseline),
    aother_num_violent_std_baseline = stdize(aother_num_violent_baseline, to = aother_num_violent_baseline),
    aburglary_num_std_baseline = stdize(aburglary_num_baseline, to = aburglary_num_baseline),
    aother_num_nonviolent_std_baseline = stdize(aother_num_nonviolent_baseline, to = aother_num_nonviolent_baseline),
    
    # generate index
    crime_victim_idx_admin = idx_mean(aviolentcrime_num_std, anonviolentcrime_num_std, tx = Z, fe = block_ID),
    crime_victim_idx_admin_baseline = idx_mean(aviolentcrime_num_std_baseline, anonviolentcrime_num_std_baseline, tx = Z, fe = block_ID),
    # re-standardize
    crime_victim_idx_admin = stdize(crime_victim_idx_admin, to = crime_victim_idx_admin_baseline),
    crime_victim_idx_admin_baseline = stdize(crime_victim_idx_admin_baseline, to = crime_victim_idx_admin_baseline)
  )

# 3. List-wise deletion
uga_admin <-
  uga_admin %>%
  mutate(
    crime_victim_idx_admin_listwise = idx_mean_listwise(aviolentcrime_num_std, anonviolentcrime_num_std),
    crime_victim_idx_admin_listwise_baseline = idx_mean_listwise(aviolentcrime_num_std_baseline, anonviolentcrime_num_std_baseline),
    # re-standardize
    crime_victim_idx_admin_listwise = stdize(crime_victim_idx_admin_listwise, to = crime_victim_idx_admin_listwise_baseline),
    crime_victim_idx_admin_listwise_baseline = stdize(crime_victim_idx_admin_listwise_baseline, to = crime_victim_idx_admin_listwise_baseline))

#--------------------------------------------------------------------------------------------------------------
                          # 17. Hyp 1a. (alt. ii): crime_victim_idx_exp
#--------------------------------------------------------------------------------------------------------------
uga_citizen <- 
  uga_citizen %>% 
  rowwise() %>% 
  mutate(
    # generate categories of crimes
    violentcrime_num_exp = sum(armedrob_num, aggassault_num, sexual_num, domestic_phys_num, simpleassault_num, other_any_violent, na.rm = TRUE),
    nonviolentcrime_num_exp = sum(burglary_num, domestic_verbal_num, land_any, other_any_nonviolent, na.rm = TRUE),
    cviolentcrime_num_exp = sum(carmedrob_num, caggassault_num, csimpleassault_num, csexual_num, cdomestic_phys_num, cmurder_num, cmob_num, cother_any_violent, na.rm = TRUE),
    cnonviolentcrime_num_exp = sum(cburglary_num, cdomestic_verbal_num, cland_any, cother_any_nonviolent, na.rm = TRUE),

    violentcrime_num_exp_baseline = sum(armedrob_num_baseline, aggassault_num_baseline, domestic_phys_num_baseline, simpleassault_num_baseline, other_any_violent_baseline, na.rm = TRUE),
    # Note: sexual_num_baseline not asked # see github issue # 101
    nonviolentcrime_num_exp_baseline = sum(burglary_num_baseline, land_any_baseline, other_any_nonviolent_baseline, na.rm = TRUE),
    cviolentcrime_num_exp_baseline = sum(carmedrob_num_baseline, caggassault_num_baseline, csimpleassault_num_baseline, csexual_num_baseline, cdomestic_phys_num_baseline, cmurder_num_baseline, cmob_num_baseline, cother_any_violent_baseline, na.rm = TRUE),
    cnonviolentcrime_num_exp_baseline = sum(cburglary_num_baseline, cother_any_nonviolent_baseline, na.rm = TRUE)
    # Note: domestic_verbal_num, cdomestic_verbal_num_baseline was not asked and cland_any were not collected
    ) %>% 
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
    aggassault_num_std = stdize(aggassault_num, to = aggassault_num_baseline),
    sexual_num_std = stdize(sexual_num, condition = Z_control == 1),
    domestic_phys_num_std = stdize(domestic_phys_num, to = domestic_phys_num_baseline),
    simpleassault_num_std = stdize(simpleassault_num, to = simpleassault_num_baseline),
    other_any_violent_std = stdize(other_any_violent, to = other_any_violent_baseline),
    burglary_num_std = stdize(burglary_num, to = burglary_num_baseline),
    domestic_verbal_num_std = stdize(domestic_verbal_num, condition = Z_control == 1),
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
    cdomestic_verbal_num_std = stdize(cdomestic_verbal_num, condition = Z_control == 1),
    cland_any_std = stdize(cland_any, condition = Z_control == 1),
    cother_any_nonviolent_std = stdize(cother_any_nonviolent, to = cother_any_nonviolent_baseline),
    
    armedrob_num_std_baseline = stdize(armedrob_num_baseline, to = armedrob_num_baseline),
    aggassault_num_std_baseline = stdize(aggassault_num_baseline, to = aggassault_num_baseline),
    domestic_phys_num_std_baseline = stdize(domestic_phys_num_baseline, to = domestic_phys_num_baseline),
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
    
    # generate index
    crime_victim_idx_exp = idx_mean(violentcrime_num_exp_std, nonviolentcrime_num_exp_std, cviolentcrime_num_exp_std, cnonviolentcrime_num_exp_std, tx = Z, fe = block_ID),
    crime_victim_idx_exp_baseline = idx_mean(violentcrime_num_exp_std_baseline, nonviolentcrime_num_exp_std_baseline, cviolentcrime_num_exp_std_baseline, cnonviolentcrime_num_exp_std_baseline, tx = Z, fe = block_ID),
    # re-standardize
    crime_victim_idx_exp = stdize(crime_victim_idx_exp, to = crime_victim_idx_exp_baseline),
    crime_victim_idx_exp_baseline = stdize(crime_victim_idx_exp_baseline, to = crime_victim_idx_exp_baseline)
  )

# 3. List-wise deletion
uga_citizen <-
  uga_citizen %>%
  mutate(
    crime_victim_idx_exp_listwise = idx_mean_listwise(violentcrime_num_exp_std, nonviolentcrime_num_exp_std, cviolentcrime_num_exp_std, cnonviolentcrime_num_exp_std),
    crime_victim_idx_exp_listwise_baseline = idx_mean_listwise(violentcrime_num_exp_std_baseline, nonviolentcrime_num_exp_std_baseline, cviolentcrime_num_exp_std_baseline, cnonviolentcrime_num_exp_std_baseline),
    # re-standardize
    crime_victim_idx_exp_listwise = stdize(crime_victim_idx_exp_listwise, to = crime_victim_idx_exp_listwise_baseline),
    crime_victim_idx_exp_listwise_baseline = stdize(crime_victim_idx_exp_listwise_baseline, to = crime_victim_idx_exp_listwise_baseline))

#--------------------------------------------------------------------------------------------------------------
                          # 18. Hyp 1a. (alt. iii) crime_victim_idx_bin
#--------------------------------------------------------------------------------------------------------------
uga_citizen <- 
  uga_citizen %>% 
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
    
    crime_victim_idx_bin = idx_mean(violentcrime_bin_std, nonviolentcrime_bin_std, cviolentcrime_bin_std, cnonviolentcrime_bin_std, tx = Z, fe = block_ID),
    crime_victim_idx_bin_baseline = idx_mean(violentcrime_bin_std_baseline, nonviolentcrime_bin_std_baseline, cviolentcrime_bin_std_baseline, cnonviolentcrime_bin_std_baseline, tx = Z, fe = block_ID),
    # re-standardize
    crime_victim_idx_bin = stdize(crime_victim_idx_bin, to = crime_victim_idx_bin_baseline),
    crime_victim_idx_bin_baseline = stdize(crime_victim_idx_bin_baseline, to = crime_victim_idx_bin_baseline)
  )

# 3. List-wise deletion
uga_citizen <-
  uga_citizen %>%
  mutate(
    crime_victim_idx_bin_listwise = idx_mean_listwise(violentcrime_bin_std, nonviolentcrime_bin_std, cviolentcrime_bin_std, cnonviolentcrime_bin_std),
    crime_victim_idx_bin_listwise_baseline = idx_mean_listwise(violentcrime_bin_std_baseline, nonviolentcrime_bin_std_baseline, cviolentcrime_bin_std_baseline, cnonviolentcrime_bin_std_baseline),
    # re-standardize
    crime_victim_idx_bin_listwise = stdize(crime_victim_idx_bin_listwise, to = crime_victim_idx_bin_listwise_baseline),
    crime_victim_idx_bin_listwise_baseline = stdize(crime_victim_idx_bin_listwise_baseline, to = crime_victim_idx_bin_listwise_baseline))

#--------------------------------------------------------------------------------------------------------------
# Data not collected for hypothesis 4(a): crime_reporting_idx_admin
#--------------------------------------------------------------------------------------------------------------

#--------------------------------------------------------------------------------------------------------------
                            # Save admin and citizen data
#--------------------------------------------------------------------------------------------------------------
saveRDS(uga_citizen,
        file = "data/out/uga-citizen-construct.RDS")
saveRDS(uga_officer,
        file = "data/out/uga-officer-construct.RDS")
saveRDS(uga_admin,
        file = "data/out/uga-admin-construct.RDS")