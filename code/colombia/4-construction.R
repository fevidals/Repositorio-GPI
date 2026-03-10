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
col_citizen <- readRDS("data/out/col-citizen-clean.RDS")
col_admin <- readRDS("data/out/col-admin-clean.RDS")
col_officer <- readRDS("data/out/col-officer-clean.RDS")

#--------------------------------------------------------------------------------------------------------------
                          # 1. Hypotheses 1(a): crimevictim_idx
#--------------------------------------------------------------------------------------------------------------
col_citizen <- 
  col_citizen %>%
  rowwise() %>% 
  mutate(
    # sum all crimes into categories
    violentcrime_num =
      sum(armedrob_num, simpleassault_num,
          # other_any_violent,
          na.rm = TRUE),
    nonviolentcrime_num =
      sum(burglary_num,
          # other_any_nonviolent,
          na.rm = TRUE),
    cviolentcrime_num =
      sum(carmedrob_num, csimpleassault_num, csexual_num, cdomestic_phys_num, cmurder_num,
        # cother_any_violent, 
        na.rm = TRUE),
    # caggassault_num and all other crime were not collected in the colombia study
    cnonviolentcrime_num =
      sum(cburglary_num,
          # cother_any_nonviolent,
          na.rm = TRUE),
    violentcrime_num_baseline =
      sum(armedrob_num_baseline, simpleassault_num_baseline,
        # other_any_violent_baseline,
        na.rm = TRUE),
    nonviolentcrime_num_baseline =
      sum(burglary_num_baseline,
          # other_any_nonviolent_baseline,
          na.rm = TRUE),
    cviolentcrime_num_baseline =
      sum(carmedrob_num_baseline, csimpleassault_num_baseline, csexual_num_baseline, cdomestic_phys_num_baseline, cmurder_num_baseline,
        # cother_any_violent_baseline,
        na.rm = TRUE),
    # caggassault_num_baseline is not collected in the colombia study
    cviolentcrime_num_baseline = as.numeric(cviolentcrime_num_baseline),
    cnonviolentcrime_num_baseline =
      sum(cburglary_num_baseline,
          # cother_any_nonviolent_baseline,
          na.rm = TRUE),
    cnonviolentcrime_num_baseline = as.numeric(cnonviolentcrime_num_baseline)) %>% 
  ungroup() %>% 
  mutate(across(c(violentcrime_num_baseline, nonviolentcrime_num_baseline, cviolentcrime_num_baseline, cnonviolentcrime_num_baseline), ~ if_else(in_baseline == 0, NA_real_, .))) %>% 
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
    # other_any_violent_std = stdize(other_any_violent, to = other_any_violent_baseline),
    burglary_num_std = stdize(burglary_num, to = burglary_num_baseline),
    # other_any_nonviolent_std = stdize(other_any_nonviolent, to = other_any_nonviolent_baseline),
    carmedrob_num_std = stdize(carmedrob_num, to = carmedrob_num_baseline),
    csimpleassault_num_std = stdize(csimpleassault_num, to = csimpleassault_num_baseline),
    csexual_num_std = stdize(csexual_num, to = csexual_num_baseline),
    cdomestic_phys_num_std = stdize(cdomestic_phys_num, to = cdomestic_phys_num_baseline),
    cmurder_num_std = stdize(cmurder_num, to = cmurder_num_baseline),
    # cother_any_violent_std = stdize(cother_any_violent, to = cother_any_violent_baseline),
    cburglary_num_std = stdize(cburglary_num, to = cburglary_num_baseline),
    # cother_any_nonviolent_std = stdize(cother_any_nonviolent, to = cother_any_nonviolent_baseline),
    
    armedrob_num_std_baseline = stdize(armedrob_num_baseline, to = armedrob_num_baseline),
    simpleassault_num_std_baseline = stdize(simpleassault_num_baseline, to = simpleassault_num_baseline),
    # other_any_violent_std_baseline = stdize(other_any_violent_baseline, to = other_any_violent_baseline),
    burglary_num_std_baseline = stdize(burglary_num_baseline, to = burglary_num_baseline),
    # other_any_nonviolent_std_baseline = stdize(other_any_nonviolent_baseline, to = other_any_nonviolent_baseline),
    carmedrob_num_std_baseline = stdize(carmedrob_num_baseline, to = carmedrob_num_baseline),
    csimpleassault_num_std_baseline = stdize(csimpleassault_num_baseline, to = csimpleassault_num_baseline),
    csexual_num_std_baseline = stdize(csexual_num_baseline, to = csexual_num_baseline),
    cdomestic_phys_num_std_baseline = stdize(cdomestic_phys_num_baseline, to = cdomestic_phys_num_baseline),
    cmurder_num_std_baseline = stdize(cmurder_num_baseline, to = cmurder_num_baseline),
    # cother_any_violent_std_baseline = stdize(cother_any_violent_baseline, to = cother_any_violent_baseline),
    cburglary_num_std_baseline = stdize(cburglary_num_baseline, to = cburglary_num_baseline),
    # cother_any_nonviolent_std_baseline = stdize(cother_any_nonviolent_baseline, to = cother_any_nonviolent_baseline),
    # generate crime index
    crime_victim_idx = idx_mean(violentcrime_num_std, nonviolentcrime_num_std, cviolentcrime_num_std, cnonviolentcrime_num_std, tx = Z, fe = block_ID),
    crime_victim_idx_baseline = idx_mean(violentcrime_num_std_baseline, nonviolentcrime_num_std_baseline, cviolentcrime_num_std_baseline, cnonviolentcrime_num_std_baseline, tx = Z, fe = block_ID),
    # re-standardize
    crime_victim_idx = stdize(crime_victim_idx, to = crime_victim_idx_baseline),
    crime_victim_idx_baseline = stdize(crime_victim_idx_baseline, to = crime_victim_idx_baseline),
    # re-standardize
    crime_victim_idx = stdize(crime_victim_idx, to = crime_victim_idx_baseline),
    crime_victim_idx_baseline = stdize(crime_victim_idx_baseline, to = crime_victim_idx_baseline)) %>% 
  # generate crime at the cluster level het effects
  group_by(cuadrante) %>% 
  mutate(crime_victim_idx_cluster_baseline = mean(crime_victim_idx_baseline, na.rm = TRUE)) %>% 
  ungroup

# 2. Common analysis
col_citizen <-
  col_citizen %>%
  rowwise() %>%
  mutate(
    # sum all crimes into categories
    cviolentcrime_num_common = sum(carmedrob_num, csimpleassault_num, csexual_num, cdomestic_phys_num, na.rm = TRUE),
    cviolentcrime_num_common_baseline = sum(carmedrob_num_baseline, csimpleassault_num_baseline, csexual_num_baseline, cdomestic_phys_num_baseline, na.rm = TRUE)) %>%
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
col_citizen <-
  col_citizen %>%
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
col_citizen <- 
  col_citizen %>%
  mutate(
    # standardize vars
    fear_violent_std = stdize(fear_violent, to = fear_violent_baseline),
    feared_walk_std = stdize(feared_walk, to = feared_walk_baseline),
    fear_violent_std_baseline = stdize(fear_violent_baseline, to = fear_violent_baseline),
    feared_walk_std_baseline = stdize(feared_walk_baseline, to = feared_walk_baseline),
    # Note: fear_nonviolent_std is not collected in Colombia in either the baseline or the endline
    # calculate index
    future_insecurity_idx = idx_mean(fear_violent_std, feared_walk_std, tx = Z, fe = block_ID),
    future_insecurity_idx_baseline = idx_mean(fear_violent_std_baseline, feared_walk_std_baseline, tx = Z, fe = block_ID),
    # restandardize
    future_insecurity_idx = stdize(future_insecurity_idx, to = future_insecurity_idx_baseline),
    future_insecurity_idx_baseline = stdize(future_insecurity_idx_baseline, to = future_insecurity_idx_baseline)
    
  )

# 2. COMMON ANALYSIS
col_citizen <- 
  col_citizen %>%
  mutate(
    # Note: we have dropped fear_nonviolent following the decisions by the committee: https://docs.google.com/spreadsheets/d/1oh9L8aAuD6adFIdiEFrzJqYd7XGhoGIU6hf93WFvvh4/edit?usp=sharing
    # standardize vars
    fear_violent_std_common = stdize(fear_violent, to = fear_violent_baseline),
    feared_walk_std_common = stdize(feared_walk, to = feared_walk_baseline),
    fear_violent_std_common_baseline = stdize(fear_violent_baseline, to = fear_violent_baseline),
    feared_walk_std_common_baseline = stdize(feared_walk_baseline, to = feared_walk_baseline),
    # calculate index
    future_insecurity_idx_common = idx_mean(fear_violent_std_common, feared_walk_std_common, tx = Z, fe = block_ID),
    future_insecurity_idx_common_baseline = idx_mean(fear_violent_std_common_baseline, feared_walk_std_common_baseline, tx = Z, fe = block_ID),
    # restandardize
    future_insecurity_idx_common = stdize(future_insecurity_idx_common, to = future_insecurity_idx_common_baseline),
    future_insecurity_idx_common_baseline = stdize(future_insecurity_idx_common_baseline, to = future_insecurity_idx_common_baseline)
    
  )

# 3. List-wise deletion
col_citizen <-
  col_citizen %>%
  mutate(
    future_insecurity_idx_listwise = idx_mean_listwise(fear_violent_std_common, feared_walk_std_common),
    future_insecurity_idx_listwise_baseline = idx_mean_listwise(fear_violent_std_common_baseline, feared_walk_std_common_baseline),
    # restandardize
    future_insecurity_idx_listwise = stdize(future_insecurity_idx_listwise, to = future_insecurity_idx_listwise_baseline),
    future_insecurity_idx_listwise_baseline = stdize(future_insecurity_idx_listwise_baseline, to = future_insecurity_idx_listwise_baseline))


# # 4. Outlier deletion
# col_citizen <-
#   col_citizen %>%
#   mutate(
#     armedrob_num_outlier = na_if(armedrob_num, quantile(armedrob_num, 0.95, 0.95, na.rm = TRUE)),
#     simpleassault_num_outlier = na_if(simpleassault_num, quantile(simpleassault_num, 0.95, na.rm = TRUE)),
#     other_any_violent_outlier = na_if(other_any_violent, quantile(other_any_violent, 0.95, na.rm = TRUE)),
#     burglary_num_outlier = na_if(burglary_num, quantile(burglary_num, 0.95, na.rm = TRUE)),
#     other_any_nonviolent_outlier = na_if(other_any_nonviolent, quantile(other_any_nonviolent, 0.95, na.rm = TRUE)),
#     carmedrob_num_outlier = na_if(carmedrob_num, quantile(carmedrob_num, 0.95, na.rm = TRUE)),
#     caggassault_num_outlier = na_if(caggassault_num, quantile(caggassault_num, 0.95, na.rm = TRUE)),
#     csimpleassault_num_outlier = na_if(csimpleassault_num, quantile(csimpleassault_num, 0.95, na.rm = TRUE)),
#     csexual_num_outlier = na_if(csexual_num, quantile(csexual_num, 0.95, na.rm = TRUE)),
#     cdomestic_phys_num_outlier = na_if(cdomestic_phys_num, quantile(cdomestic_phys_num, 0.95, na.rm = TRUE)),
#     cmurder_num_outlier = na_if(cmurder_num, quantile(cmurder_num, 0.95, na.rm = TRUE)),
#     cother_any_violent_outlier = na_if(cother_any_violent, quantile(cother_any_violent, 0.95, na.rm = TRUE)),
#     cburglary_num_outlier = na_if(cburglary_num, quantile(cburglary_num, 0.95, na.rm = TRUE)),
#     cother_any_nonviolent_outlier = na_if(cother_any_nonviolent, quantile(cother_any_nonviolent, 0.95, na.rm = TRUE))) %>% 
#   rowwise() %>%
#   mutate(
#     # sum all crimes into categories
#     violentcrime_num = sum(armedrob_num, simpleassault_num, other_any_violent, na.rm = TRUE),
#     nonviolentcrime_num = sum(burglary_num, other_any_nonviolent, na.rm = TRUE),
#     cviolentcrime_num = sum(carmedrob_num, caggassault_num, csimpleassault_num, csexual_num, cdomestic_phys_num, cmurder_num, cother_any_violent, na.rm = TRUE),
#     cnonviolentcrime_num = sum(cburglary_num, cother_any_nonviolent, na.rm = TRUE),
#     violentcrime_num_baseline = sum( armedrob_num_baseline, simpleassault_num_baseline, other_any_violent_baseline, na.rm = TRUE),
#     nonviolentcrime_num_baseline = sum(burglary_num_baseline, other_any_nonviolent_baseline, na.rm = TRUE),
#     cviolentcrime_num_baseline = sum(carmedrob_num_baseline, caggassault_num_baseline, csimpleassault_num_baseline, sexual_num_baseline, cdomestic_phys_num_baseline, cmurder_num_baseline, cother_any_violent_baseline, na.rm = TRUE),
#     cnonviolentcrime_num_baseline = sum(cburglary_num_baseline, cother_any_nonviolent_baseline, na.rm = TRUE),
#     cnonviolentcrime_num_baseline = as.numeric(cnonviolentcrime_num_baseline, na.rm = TRUE)) %>%
#   ungroup() %>% 
#   mutate(
#     # standardize categories
#     violentcrime_num_outlier_std = stdize(violentcrime_num, to = violentcrime_num_baseline),
#     nonviolentcrime_num_outlier_std = stdize(nonviolentcrime_num, to = nonviolentcrime_num_baseline),
#     cviolentcrime_num_outlier_std = stdize(cviolentcrime_num, to = cviolentcrime_num_baseline),
#     cnonviolentcrime_num_outlier_std = stdize(cnonviolentcrime_num, to = cnonviolentcrime_num_baseline),
#     violentcrime_num_outlier_std_baseline = stdize(violentcrime_num_baseline, to = violentcrime_num_baseline),
#     nonviolentcrime_num_outlier_std_baseline = stdize(nonviolentcrime_num_baseline, to = nonviolentcrime_num_baseline),
#     cviolentcrime_num_outlier_std_baseline = stdize(cviolentcrime_num_baseline, to = cviolentcrime_num_baseline),
#     cnonviolentcrime_num_outlier_std_baseline = stdize(cnonviolentcrime_num_baseline, to = cnonviolentcrime_num_baseline),
#     # standardize individual crimes
#     armedrob_num_outlier_std = stdize(armedrob_num, to = armedrob_num_baseline),
#     simpleassault_num_outlier_std = stdize(simpleassault_num, to = simpleassault_num_baseline),
#     other_any_violent_outlier_std = stdize(other_any_violent, to = other_any_violent_baseline),
#     burglary_num_outlier_std = stdize(burglary_num, to = burglary_num_baseline),
#     other_any_nonviolent_outlier_std = stdize(other_any_nonviolent, to = other_any_nonviolent_baseline),
#     carmedrob_num_outlier_std = stdize(carmedrob_num, to = carmedrob_num_baseline),
#     caggassault_num_outlier_std = stdize(caggassault_num, to = caggassault_num_baseline),
#     csimpleassault_num_outlier_std = stdize(csimpleassault_num, to = csimpleassault_num_baseline),
#     csexual_num_outlier_std = stdize(csexual_num, to = csexual_num_baseline),
#     cdomestic_phys_num_outlier_std = stdize(cdomestic_phys_num, to = cdomestic_phys_num_baseline),
#     cmurder_num_outlier_std = stdize(cmurder_num, to = cmurder_num_baseline),
#     cother_any_violent_outlier_std = stdize(cother_any_violent, to = cother_any_violent_baseline),
#     cburglary_num_outlier_std = stdize(cburglary_num, to = cburglary_num_baseline),
#     cother_any_nonviolent_outlier_std = stdize(cother_any_nonviolent, to = cother_any_nonviolent_baseline),
#     
#     armedrob_num_std_outlier_baseline = stdize(armedrob_num_baseline, to = armedrob_num_baseline),
#     simpleassault_num_std_outlier_baseline = stdize(simpleassault_num_baseline, to = simpleassault_num_baseline),
#     other_any_violent_std_outlier_baseline = stdize(other_any_violent_baseline, to = other_any_violent_baseline),
#     burglary_num_std_outlier_baseline = stdize(burglary_num_baseline, to = burglary_num_baseline),
#     other_any_nonviolent_std_outlier_baseline = stdize(other_any_nonviolent_baseline, to = other_any_nonviolent_baseline),
#     carmedrob_num_std_outlier_baseline = stdize(carmedrob_num_baseline, to = carmedrob_num_baseline),
#     caggassault_num_std_outlier_baseline = stdize(caggassault_num_baseline, to = caggassault_num_baseline),
#     csimpleassault_num_std_outlier_baseline = stdize(csimpleassault_num_baseline, to = csimpleassault_num_baseline),
#     csexual_num_std_outlier_baseline = stdize(csexual_num_baseline, to = csexual_num_baseline),
#     cdomestic_phys_num_std_outlier_baseline = stdize(cdomestic_phys_num_baseline, to = cdomestic_phys_num_baseline),
#     cmurder_num_std_outlier_baseline = stdize(cmurder_num_baseline, to = cmurder_num_baseline),
#     cother_any_violent_std_outlier_baseline = stdize(cother_any_violent_baseline, to = cother_any_violent_baseline),
#     cburglary_num_std_outlier_baseline = stdize(cburglary_num_baseline, to = cburglary_num_baseline),
#     cother_any_nonviolent_std_outlier_baseline = stdize(cother_any_nonviolent_baseline, to = cother_any_nonviolent_baseline),
#     # generate crime index
#     crime_victim_idx_outlier = idx_mean(violentcrime_num_std, nonviolentcrime_num_std, cviolentcrime_num_std, cnonviolentcrime_num_std, tx = Z_common, fe = police_zones),
#     crime_victim_idx_outlier_baseline = idx_mean(violentcrime_num_std_baseline, nonviolentcrime_num_std_baseline, cviolentcrime_num_std_baseline, cnonviolentcrime_num_std_baseline, tx = Z_common, fe = police_zones))


#--------------------------------------------------------------------------------------------------------------
                          # 3. Hypotheses 2: satis_idx
#--------------------------------------------------------------------------------------------------------------
col_citizen <- 
  col_citizen %>%
  mutate(
    satis_trust_rescaled = case_when(
      satis_trust == 0 ~ 0L,
      satis_trust == 1 ~ 1L, 
      satis_trust == 2 ~ 3L, 
      satis_trust == 3 ~ 4L,
      TRUE ~ satis_trust),
    satis_trust_rescaled_baseline = case_when(
      satis_trust_baseline == 0 ~ 0L,
      satis_trust_baseline == 1 ~ 1L, 
      satis_trust_baseline == 2 ~ 3L, 
      satis_trust_baseline == 3 ~ 4L,
      TRUE ~ satis_trust_baseline),
    # standardize vars
    satis_trust_std = stdize(satis_trust_rescaled, to = satis_trust_rescaled_baseline),
    satis_general_std = stdize(satis_general, condition = (Z_control == 1)),
    satis_trust_std_baseline = stdize(satis_trust_rescaled_baseline, to = satis_trust_rescaled_baseline),
    # Note: satis_general is not collected in Colombia at the baseline
    # calculate index
    satis_idx = idx_mean(satis_trust_std, satis_general_std, tx = Z, fe = block_ID),
    satis_idx_baseline = satis_trust_std_baseline,
    # restandardize
    satis_idx = stdize(satis_idx, condition = (Z_control == 1))

  )

# 3. List-wise deletion
col_citizen <-
  col_citizen %>%
  mutate(
    satis_idx_listwise = idx_mean_listwise(satis_trust_std, satis_general_std),
    satis_idx_listwise_baseline = satis_trust_std_baseline,
    # restandardize
    satis_idx_listwise = stdize(satis_idx_listwise, condition = (Z_control == 1)),
)

#--------------------------------------------------------------------------------------------------------------
                          # 4. Hypotheses 3(a): officer_attitude_idx
#--------------------------------------------------------------------------------------------------------------
col_officer <- 
  col_officer %>% 
  mutate(
    empathy_complaints_rescaled = empathy_complaints,
    empathy_reports_rescaled = empathy_reports,
    
    empathy_complaints_std = stdize(empathy_complaints_rescaled, condition = (Z_control == 1)),
    empathy_reports_std = stdize(empathy_reports_rescaled, condition = (Z_control == 1)),

    empathy_idx = idx_mean_listwise(empathy_complaints_std, empathy_reports_std), # tx = Z, fe = block_ID),

    account_pol_matter_std = stdize(account_pol_matter,  condition = (Z_control == 1)),

    hypothetical2_punishment_std = stdize(hypothetical2_punishment, condition = (Z_control == 1)),
    hypothetical3_punishment_std = stdize(hypothetical3_punishment, condition = (Z_control == 1)),
    hypothetical5_punishment_std = stdize(hypothetical5_punishment, condition = (Z_control == 1)),

    hypothetical2_reportself_std = stdize(hypothetical2_reportself, condition = (Z_control == 1)),
    hypothetical3_reportself_std = stdize(hypothetical3_reportself, condition = (Z_control == 1)),
    hypothetical5_reportself_std = stdize(hypothetical5_reportself, condition = (Z_control == 1)),

    hypothetical2_reportothers_std = stdize(hypothetical2_reportothers, condition = (Z_control == 1)),
    hypothetical3_reportothers_std = stdize(hypothetical3_reportothers, condition = (Z_control == 1)),
    hypothetical5_reportothers_std = stdize(hypothetical5_reportothers, condition = (Z_control == 1)),

    accountability_idx = idx_mean_listwise(account_pol_matter_std, hypothetical2_reportothers_std, hypothetical3_reportothers_std, hypothetical5_reportothers_std, hypothetical5_reportself_std, hypothetical2_reportself_std, hypothetical3_reportself_std, hypothetical2_punishment_std, hypothetical3_punishment_std, hypothetical5_punishment_std), # tx = Z, fe = block_ID),

    hypothetical5_abuseself_std = stdize(hypothetical5_abuseself, condition = (Z_control == 1)),
    hypothetical5_abuseother_std = stdize(hypothetical5_abuseother, condition = (Z_control == 1)),

    abuse_idx = idx_mean_listwise(hypothetical5_abuseself_std, hypothetical5_abuseother_std), # tx = Z, fe = block_ID),

    hypothetical2_corruptself_std = stdize(hypothetical2_corruptself, condition = (Z_control == 1)),
    hypothetical2_corruptother_std = stdize(hypothetical2_corruptother, condition = (Z_control == 1)),

    hypothetical3_corruptself_std = stdize(hypothetical3_corruptself, condition = (Z_control == 1)),
    hypothetical3_corruptother_std = stdize(hypothetical3_corruptother, condition = (Z_control == 1)),

    corrupt_idx = idx_mean_listwise(hypothetical2_corruptself_std, hypothetical2_corruptother_std, hypothetical3_corruptself_std, hypothetical3_corruptother_std), # tx = Z, fe = block_ID),

    officer_attitude_idx = idx_mean_listwise(empathy_idx, corrupt_idx, abuse_idx, accountability_idx), #, tx = Z, fe = block_ID)
    # restandardize
    officer_attitude_idx = stdize(officer_attitude_idx, condition = (Z_control == 1))
    ) 

# --------------------------------------------------------------------------------------------------------------
                          # 5. Hypotheses 3(b): police_abuse_idx
# --------------------------------------------------------------------------------------------------------------
col_citizen <-
  col_citizen %>%
  mutate(
    # generate police abuse
    policeabuse_any = if_else(policeabuse_verbal_any > 0 | policeabuse_phys_any > 0, 1, 0),
    policeabuse_any_baseline = if_else(policeabuse_verbal_any_baseline > 0 | policeabuse_phys_any_baseline > 0, 1, 0),
    # note: policeabuse_num was not collected for Colombia
    # bribe amount is divided by the rate of dollar (GH issue number 49)
    bribe_amt = bribe_amt / 3333,
    # different construction that the M-PAP (GH issues number 31)
    bribe_amt = if_else(bribe_freq == 1, 0, bribe_amt),
    # Note: bribe_amt and bribe_freq was not collected for Col at baseline
    # standardize vars
    policeabuse_any_std = stdize(policeabuse_any, to = policeabuse_any_baseline),
    bribe_freq_std = stdize(bribe_freq, condition = (Z_control == 1)),
    bribe_amt_std = stdize(bribe_amt, condition = (Z_control == 1)),
    policeabuse_any_std_baseline = stdize(policeabuse_any_baseline, to = policeabuse_any_baseline),
    # calculate index
    police_abuse_idx = idx_mean(policeabuse_any_std, bribe_freq_std, bribe_amt_std, tx = Z, fe = block_ID),
    police_abuse_idx_baseline = policeabuse_any_std_baseline,
    # re-standardize
    police_abuse_idx = stdize(police_abuse_idx, to = police_abuse_idx_baseline),
    police_abuse_idx_baseline = policeabuse_any_std_baseline
  )

# 2. Common analysis
col_citizen <-
  col_citizen %>%
  mutate(
    police_abuse_idx_common = idx_mean(policeabuse_any_std, bribe_freq_std, bribe_amt_std, tx = Z, fe = block_ID),
    police_abuse_idx_common_baseline = policeabuse_any_std_baseline,
    # re-standardize
    police_abuse_idx_common = stdize(police_abuse_idx_common, to = police_abuse_idx_common_baseline)
  )

# 3. List-wise deletion
col_citizen <-
  col_citizen %>%
  mutate(
    police_abuse_idx_listwise = idx_mean_listwise(policeabuse_any_std, bribe_freq_std, bribe_amt_std),
    police_abuse_idx_listwise_baseline = policeabuse_any_std_baseline,
    # re-standardize
    police_abuse_idx_listwise = stdize(police_abuse_idx_listwise, to = police_abuse_idx_listwise_baseline))

#--------------------------------------------------------------------------------------------------------------
                          # 6. Hypotheses 4(a): police_abuse_idx
#--------------------------------------------------------------------------------------------------------------
col_citizen <- 
  col_citizen %>%
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
    # Note: caggassault_report is not collected for Colombia
    csimpleassault_report = case_when(
      csimpleassault_num == 0 | csimpleassault_report == 0 ~ 0,
      csimpleassault_num > 0 & csimpleassault_report == 1 ~ 1),
    csexual_report = case_when(
      csexual_num == 0 | csexual_report == 0 ~ 0,
      csexual_num > 0 & csexual_report == 1 ~ 1),
    cdomestic_phys_report = case_when(
      cdomestic_phys_num == 0 | cdomestic_phys_report == 0 ~ 0,
      cdomestic_phys_num > 0 & cdomestic_phys_report == 1 ~ 1),
    # Note: cmurder_report is not collected for Colombia
    
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
    # Note: caggassault_report is not collected for Colombia
    csimpleassault_report_baseline = case_when(
      csimpleassault_num_baseline == 0 | csimpleassault_report_baseline == 0 ~ 0,
      csimpleassault_num_baseline > 0 & csimpleassault_report_baseline == 1 ~ 1),
    csexual_report_baseline = case_when(
      csexual_num_baseline == 0 | csexual_report_baseline == 0 ~ 0,
      csexual_num_baseline > 0 & csexual_report_baseline == 1 ~ 1),
    cdomestic_phys_report_baseline = case_when(
      cdomestic_phys_num_baseline == 0 | cdomestic_phys_report_baseline == 0 ~ 0,
      cdomestic_phys_num_baseline > 0 & cdomestic_phys_report_baseline == 1 ~ 1),

    burglaryres = case_when(
      burglaryres == 1 ~ 1L,
      burglaryres == 2 ~ 1L,
      TRUE ~ 0L),
    dviolres = case_when(
      dviolres == 1 ~ 1L,
      dviolres == 2 ~ 1L,
      TRUE ~ 0L),
    # Note: armedrob_res is not collected for Colombia    
    burglaryres_baseline = case_when(
      burglaryres_baseline == 1 ~ 1L,
      burglaryres_baseline == 2 ~ 1L,
      TRUE ~ 0L),
    dviolres_baseline = case_when(
      dviolres_baseline == 1 ~ 1L,
      dviolres_baseline == 2 ~ 1L,
      TRUE ~ 0L)) %>% 
  rowwise() %>% 
  mutate(
    # calculate crime categories
    violentcrime_report_num = sum(armedrob_report, simpleassault_report,
        # other_report_violent,
        na.rm = TRUE),
    nonviolentcrime_report_num = sum(burglary_report,
        # other_report_nonviolent,
        na.rm = TRUE),
    cviolentcrime_report_num = sum(carmedrob_report, csimpleassault_report, csexual_report, cdomestic_phys_report,
        # cother_report_violent,
        na.rm = TRUE),
    cnonviolentcrime_report_num = sum(cburglary_report,
          # cother_report_nonviolent,
          na.rm = TRUE),
    
    violentcrime_report_num_baseline = sum(armedrob_report_baseline, simpleassault_report_baseline,
        # other_report_violent_baseline,
        na.rm = TRUE),
    nonviolentcrime_report_num_baseline = sum( burglary_report_baseline,
        # other_report_nonviolent_baseline,
        na.rm = TRUE),
    cviolentcrime_report_num_baseline = sum(carmedrob_report_baseline, csimpleassault_report_baseline, csexual_report_baseline, cdomestic_phys_report_baseline,
        # cother_report_violent_baseline,
        na.rm = TRUE),
    cnonviolentcrime_report_num_baseline =
      sum(cburglary_report_baseline,
          # cother_report_nonviolent_baseline,
          na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(
    # standarduze all vars
    burglaryres_std = stdize(burglaryres, to = burglaryres_baseline),
    dviolres_std = stdize(dviolres, to = dviolres_baseline),
    burglaryres_std_baseline = stdize(burglaryres_baseline, to = burglaryres_baseline),
    dviolres_std_baseline = stdize(dviolres_baseline, to = dviolres_baseline),

    violentcrime_report_num_std = stdize(violentcrime_report_num, to = violentcrime_report_num_baseline),
    nonviolentcrime_report_num_std = stdize(nonviolentcrime_report_num, to = nonviolentcrime_report_num_baseline),
    cviolentcrime_report_num_std = stdize(cviolentcrime_report_num, to = cviolentcrime_report_num_baseline),
    cnonviolentcrime_report_num_std = stdize(cnonviolentcrime_report_num, to = cnonviolentcrime_report_num_baseline),
    violentcrime_report_num_std_baseline = stdize(violentcrime_report_num_baseline, to = violentcrime_report_num_baseline),
    nonviolentcrime_report_num_std_baseline = stdize(nonviolentcrime_report_num_baseline, to = nonviolentcrime_report_num_baseline),
    cviolentcrime_report_num_std_baseline = stdize(cviolentcrime_report_num_baseline, to = cviolentcrime_report_num_baseline),
    cnonviolentcrime_report_num_std_baseline = stdize(cnonviolentcrime_report_num_baseline, to = cnonviolentcrime_report_num_baseline),
    # standardize individual crimes' reporting
    armedrob_report_std = stdize(armedrob_report, to = armedrob_report_baseline),
    simpleassault_report_std = stdize(simpleassault_report, to = simpleassault_report_baseline),
    # other_report_violent_std = stdize(other_report_violent, to = other_report_violent_baseline),
    burglary_report_std = stdize(burglary_report, to = burglary_report_baseline),
    carmedrob_report_std = stdize(carmedrob_report, to = carmedrob_report_baseline),
    # caggassault_report_std = stdize(caggassault_report, to = caggassault_report_baseline),
    csimpleassault_report_std = stdize(csimpleassault_report, to = csimpleassault_report_baseline),
    csexual_report_std = stdize(csexual_report, to = csexual_report_baseline),
    cdomestic_phys_report_std = stdize(cdomestic_phys_report, to = cdomestic_phys_report_baseline),
    # cother_report_violent_std = stdize(cother_report_violent, to = cother_report_violent_baseline),
    cburglary_report_std = stdize(cburglary_report, to = cburglary_report_baseline),
    
    armedrob_report_std_baseline = stdize(armedrob_report_baseline, to = armedrob_report_baseline),
    simpleassault_report_std_baseline = stdize(simpleassault_report_baseline, to = simpleassault_report_baseline),
    # other_report_violent_std_baseline = stdize(other_report_violent_baseline, to = other_report_violent_baseline),
    burglary_report_std_baseline = stdize(burglary_report_baseline, to = burglary_report_baseline),
    carmedrob_report_std_baseline = stdize(carmedrob_report_baseline, to = carmedrob_report_baseline),
    # caggassault_report_std_baseline = stdize(caggassault_report_baseline, to = caggassault_report_baseline),
    csimpleassault_report_std_baseline = stdize(csimpleassault_report_baseline, to = csimpleassault_report_baseline),
    csexual_report_std_baseline = stdize(csexual_report_baseline, to = csexual_report_baseline),
    cdomestic_phys_report_std_baseline = stdize(cdomestic_phys_report_baseline, to = cdomestic_phys_report_baseline),
    # cother_report_violent_std_baseline = stdize(cother_report_violent_baseline, to = cother_report_violent_baseline),
    cburglary_report_std_baseline = stdize(cburglary_report_baseline, to = cburglary_report_baseline),
    # calculate sub-indices
    crimeres_idx = idx_mean(burglaryres_std, dviolres_std, tx = Z, fe = block_ID),
    crimeres_idx_baseline = idx_mean(burglaryres_std_baseline, dviolres_std_baseline, tx = Z, fe = block_ID),
    # calculate indices
    crime_reporting_idx = idx_mean(violentcrime_report_num_std, nonviolentcrime_report_num_std, cviolentcrime_report_num_std, cnonviolentcrime_report_num_std, crimeres_idx, tx = Z, fe = block_ID),
    crime_reporting_idx_baseline = idx_mean(violentcrime_report_num_std_baseline, nonviolentcrime_report_num_std_baseline, cviolentcrime_report_num_std_baseline, cnonviolentcrime_report_num_std_baseline, crimeres_idx_baseline, tx = Z, fe = block_ID),
    # restandardize
    crime_reporting_idx = stdize(crime_reporting_idx, to = crime_reporting_idx_baseline), 
    crime_reporting_idx_baseline = stdize(crime_reporting_idx_baseline, to = crime_reporting_idx_baseline)
  )

# 2. common analysis
col_citizen <- 
  col_citizen %>% 
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
    # restandardize
    crime_reporting_idx_common = stdize(crime_reporting_idx_common, to = crime_reporting_idx_common_baseline), 
    crime_reporting_idx_common_baseline = stdize(crime_reporting_idx_common_baseline, to = crime_reporting_idx_common_baseline)
    
  )

# 3. List-wise deletion
col_citizen <-
  col_citizen %>%
  mutate(
    # calculate sub-index
    crimeres_idx_listwise = idx_mean_listwise(burglaryres_std, dviolres_std),
    crimeres_idx_listwise_baseline = idx_mean_listwise(burglaryres_std_baseline, dviolres_std_baseline),
    # calculate index
    crime_reporting_idx_listwise = idx_mean_listwise(violentcrime_report_num_common_std, nonviolentcrime_report_num_common_std, cviolentcrime_report_num_common_std, cnonviolentcrime_report_num_common_std, crimeres_idx_common),
    crime_reporting_idx_listwise_baseline = idx_mean_listwise(violentcrime_report_num_common_std_baseline, nonviolentcrime_report_num_common_std_baseline, cviolentcrime_report_num_common_std_baseline, cnonviolentcrime_report_num_common_std_baseline, crimeres_idx_common_baseline),
    # restandardize
    crime_reporting_idx_listwise = stdize(crime_reporting_idx_listwise, to = crime_reporting_idx_listwise_baseline), 
    crime_reporting_idx_listwise_baseline = stdize(crime_reporting_idx_listwise_baseline, to = crime_reporting_idx_listwise_baseline)
  )

#--------------------------------------------------------------------------------------------------------------
                          # 7. Hypotheses 4(a): tips_idx
#--------------------------------------------------------------------------------------------------------------
col_citizen <- 
  col_citizen %>%
  left_join(col_admin %>% select(cuadrante, atips_hline, atips_hline_baseline), by = "cuadrante") %>% 
  group_by(cuadrante) %>% 
  mutate(atips_hline = mean(atips_hline, na.rm = TRUE),
         atips_hline_baseline = mean(atips_hline_baseline, na.rm = TRUE)) %>% 
  ungroup %>% 
  mutate(
    # standardize vars
    contact_pol_susp_activity_std = stdize(contact_pol_susp_activity, to = contact_pol_susp_activity_baseline),
    give_info_pol_investigation_std = stdize(give_info_pol_investigation, to = give_info_pol_investigation_baseline),
    contact_pol_susp_activity_std_baseline = stdize(contact_pol_susp_activity_baseline, to = contact_pol_susp_activity_baseline),
    give_info_pol_investigation_std_baseline = stdize(give_info_pol_investigation_baseline, to = give_info_pol_investigation_baseline),
    atips_hline = stdize(atips_hline, to = atips_hline_baseline),
    atips_hline_baseline = stdize(atips_hline_baseline, to = atips_hline_baseline),
    # generate index
    crime_tips_idx = idx_mean(contact_pol_susp_activity_std, give_info_pol_investigation_std, tx = Z, fe = block_ID),
    crime_tips_idx_baseline = idx_mean(contact_pol_susp_activity_std_baseline, give_info_pol_investigation_std_baseline, tx = Z, fe = block_ID),
    # make names consistent with mpap
    tips_idx = idx_mean(crime_tips_idx, atips_hline, tx = Z, fe = block_ID),
    tips_idx_baseline = idx_mean(crime_tips_idx_baseline, atips_hline_baseline, tx = Z, fe = block_ID),
    # restandardize
    crime_tips_idx = stdize(crime_tips_idx, to = crime_tips_idx_baseline),
    crime_tips_idx_baseline = stdize(crime_tips_idx_baseline, to = crime_tips_idx_baseline),
    # make names consistent with mpap
    tips_idx = crime_tips_idx,
    tips_idx_baseline = crime_tips_idx_baseline
  )


# 3. List-wise deletion
col_citizen <-
  col_citizen %>%
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
col_citizen <- 
  col_citizen %>%
  mutate(
    # general police abuse components
    policeabuse_report = case_when(
      policeabuse_verbal_report == 1 ~ 1L,
      TRUE ~ 0L), 
    policeabuse_report_baseline = case_when(
      policeabuse_verbal_report_baseline == 1 ~ 1L,
      TRUE ~ 0L), 
    # Note: policeabuse_phys_report was not collected in Col
    # standardize vars
    policeabuse_report_std = stdize(policeabuse_report, to = policeabuse_report_baseline),
    policebeating_report_std = stdize(policebeating_report, to = policebeating_report_baseline),
    policeabuse_report_std_baseline = stdize(policeabuse_report_baseline, to = policeabuse_report_baseline),
    policebeating_report_std_baseline = stdize(policebeating_report_baseline, to = policebeating_report_baseline),
    # Note: duty_drink report is not collected for Colombia
    # calculate index
    police_abuse_report_idx = idx_mean(policeabuse_report_std, policebeating_report_std, tx = Z, fe = block_ID),
    police_abuse_report_idx_baseline = idx_mean(policeabuse_report_std_baseline, policebeating_report_std_baseline, tx = Z, fe = block_ID),
    
    # restandardize
    police_abuse_report_idx = stdize(police_abuse_report_idx, to = police_abuse_report_idx_baseline),
    police_abuse_report_idx_baseline = stdize(police_abuse_report_idx_baseline, to = police_abuse_report_idx_baseline)
  )

col_citizen <- 
  col_citizen %>%
  mutate(
    # general police abuse components
    policeabuse_report_common = case_when(
      policeabuse_verbal_report == 1 ~ 1L,
      TRUE ~ 0L), 
    policeabuse_report_common_baseline = case_when(
      policeabuse_verbal_report_baseline == 1 ~ 1L,
      TRUE ~ 0L), 
    # standardize vars
    policeabuse_report_common_std = stdize(policeabuse_report_common, to = policeabuse_report_common_baseline),
    policeabuse_report_common_std_baseline = stdize(policeabuse_report_common_baseline, to = policeabuse_report_common_baseline),
    # calculate index
    police_abuse_report_idx_common = idx_mean(policeabuse_report_common_std, policebeating_report_std, tx = Z, fe = block_ID),
    police_abuse_report_idx_common_baseline = idx_mean(policeabuse_report_common_std_baseline, policebeating_report_std_baseline, tx = Z, fe = block_ID),
    # restandardize
    police_abuse_report_idx_common = stdize(police_abuse_report_idx_common, to = police_abuse_report_idx_common_baseline),
    police_abuse_report_idx_common_baseline = stdize(police_abuse_report_idx_common_baseline, to = police_abuse_report_idx_common_baseline))

# 3. List-wise deletion
col_citizen <-
  col_citizen %>%
  mutate(
    police_abuse_report_idx_listwise = idx_mean_listwise(policeabuse_report_common_std, policebeating_report_std),
    police_abuse_report_idx_listwise_baseline = idx_mean_listwise(policeabuse_report_common_std_baseline, policebeating_report_std_baseline),
    # restandardize
    police_abuse_report_idx_listwise = stdize(police_abuse_report_idx_listwise, to = police_abuse_report_idx_listwise_baseline),
    police_abuse_report_idx_listwise_baseline = stdize(police_abuse_report_idx_listwise_baseline, to = police_abuse_report_idx_listwise_baseline))




#--------------------------------------------------------------------------------------------------------------
                          # 9. Hypotheses M1a: intentions_idx
#--------------------------------------------------------------------------------------------------------------
col_citizen <- 
  col_citizen %>%
  mutate(
    # rescale to recode into correct answer
    polint_corrupt = 5 - polint_corrupt, 
    polint_corrupt_baseline = 5 - polint_corrupt_baseline,
    # standardize vars
    polint_corrupt_std = stdize(polint_corrupt, to = polint_corrupt_baseline),
    polint_quality_std = stdize(polint_quality, to = polint_quality_baseline),
    polint_corrupt_std_baseline = stdize(polint_corrupt_baseline, to = polint_corrupt_baseline),
    polint_quality_std_baseline = stdize(polint_quality_baseline, to = polint_quality_baseline),
    polcaseserious_std = stdize(polcaseserious, to = polcaseserious_baseline),
    polcasefair_std = stdize(polcasefair, condition = (Z_control == 0)),
    polcaseserious_std_baseline = stdize(polcaseserious_baseline, to = polcaseserious_baseline),
    # Note: polcasefair was not collected in the baseline for Colombia
    # calculate sub-indices
    polint_idx = idx_mean(polint_corrupt_std, polint_quality_std, tx = Z, fe = block_ID),
    polint_idx_baseline = idx_mean(polint_corrupt_std_baseline, polint_quality_std_baseline, tx = Z, fe = block_ID),
    # calculate index
    intentions_idx = idx_mean(polint_idx, polcaseserious_std, polcasefair_std, tx = Z, fe = block_ID),
    intentions_idx_baseline = idx_mean(polint_idx_baseline, polcaseserious_std_baseline, tx = Z, fe = block_ID),
    # restandardize
    intentions_idx = stdize(intentions_idx, to = intentions_idx_baseline),
    intentions_idx_baseline = stdize(intentions_idx_baseline, to = intentions_idx_baseline)
  )

# 3. List-wise deletion
col_citizen <-
  col_citizen %>%
  mutate(
    polint_idx_listwise = idx_mean_listwise(polint_corrupt_std, polint_quality_std),
    polint_idx_listwise_baseline = idx_mean_listwise(polint_corrupt_std_baseline, polint_quality_std_baseline),
    intentions_idx_listwise = idx_mean_listwise(polint_idx, polcaseserious_std, polcasefair_std),
    intentions_idx_listwise_baseline = idx_mean_listwise(polint_idx_baseline, polcaseserious_std_baseline),
    # restandardize
    intentions_idx_listwise = stdize(intentions_idx_listwise, to = intentions_idx_listwise_baseline),
    intentions_idx_listwise_baseline = stdize(intentions_idx_listwise_baseline, to = intentions_idx_listwise_baseline))

#--------------------------------------------------------------------------------------------------------------
                          # 10. Hypotheses M1b: know_idx (Fatiq: verify correct answers in Col Code)
#--------------------------------------------------------------------------------------------------------------
col_citizen <- 
  col_citizen %>%
  mutate(
    # standardize the vars
    know_law_suspect_std = stdize(know_law_suspect, condition = (Z_control == 0)),
    know_law_lawyer_std = stdize(know_law_lawyer, condition = (Z_control == 0)),
    know_law_fees_std = stdize(know_law_fees, condition = (Z_control == 0)),
    know_law_vaw_std = stdize(know_law_vaw, condition = (Z_control == 0)),
    know_report_followup_std = stdize(know_report_followup, condition = (Z_control == 0)),
    know_report_station_std = stdize(know_report_station, condition = (Z_control == 0)),
    # calculate sub-index
    know_law_idx = idx_mean(know_law_suspect_std, know_law_lawyer_std, know_law_fees_std, know_law_vaw_std, tx = Z, fe = block_ID),
    know_report_idx = know_report_station_std,
    # calculate index
    know_idx = idx_mean(know_law_idx, know_report_idx, tx = Z, fe = block_ID),
    # restandardize
    know_idx = stdize(know_idx, condition = (Z_control == 0))
    # Note: none of the know_law items were collected at the baseline
  )

# 2. common
col_citizen <-
  col_citizen %>%
  mutate(
    # standardize the vars
    # calculate sub-index
    know_law_idx_common = idx_mean(know_law_suspect_std, know_law_lawyer_std, know_law_fees_std, tx = Z, fe = block_ID),
    # calculate index
    know_idx_common = idx_mean(know_law_idx_common, know_report_idx, tx = Z, fe = block_ID),
    # restandardize
    know_idx_common = stdize(know_idx_common, condition = (Z_control == 0)))

# 3. List-wise deletion
col_citizen <-
  col_citizen %>%
  mutate(
    know_report_idx_listwise = know_report_station_std,

    know_law_idx_listwise = idx_mean_listwise(know_law_suspect_std, know_law_lawyer_std, know_law_fees_std),

    know_idx_listwise = idx_mean_listwise(know_law_idx, know_report_station_std),
    # restandardize
    know_idx_listwise = stdize(know_idx_listwise, condition = (Z_control == 0)))

#--------------------------------------------------------------------------------------------------------------
                          # 11. Hypotheses M1c: norm_idx
#--------------------------------------------------------------------------------------------------------------
col_citizen <- 
  col_citizen %>%
  mutate(
    # reverse order of variables to be consistent with the direction (MPAP page 75-76)
    reportnorm_theft_rescaled = 4 - reportnorm_theft,
    reportnorm_abuse_rescaled = 4 - reportnorm_abuse,
    # obeynorm_rescaled = 4 - obeynorm, # removed following issue number
    reportnorm_theft_rescaled_baseline = 4 - reportnorm_theft_baseline,
    reportnorm_abuse_rescaled_baseline = 4 - reportnorm_abuse_baseline,
    # Note: oberynorm was not collected in Colombia    
    # standardize the vars
    reportnorm_theft_std = stdize(reportnorm_theft_rescaled, to = reportnorm_theft_rescaled_baseline),
    reportnorm_abuse_std = stdize(reportnorm_abuse_rescaled, to = reportnorm_abuse_rescaled_baseline),
    obeynorm_std = stdize(obeynorm, condition = (Z_control == 0)),
    reportnorm_theft_std_baseline = stdize(reportnorm_theft_rescaled_baseline, to = reportnorm_theft_rescaled_baseline),
    reportnorm_abuse_std_baseline = stdize(reportnorm_abuse_rescaled_baseline, to = reportnorm_abuse_rescaled_baseline),
    # calculate index
    norm_idx = idx_mean(reportnorm_theft_std, reportnorm_abuse_std, obeynorm_std, tx = Z, fe = block_ID),
    norm_idx_baseline = idx_mean(reportnorm_theft_std_baseline, reportnorm_abuse_std_baseline, tx = Z, fe = block_ID)
  )

# 3. List-wise deletion
col_citizen <-
  col_citizen %>%
  mutate(
    norm_idx_listwise = idx_mean_listwise(reportnorm_theft_std, reportnorm_abuse_std, obeynorm_std),
    norm_idx_listwise_baseline = idx_mean_listwise(reportnorm_theft_std_baseline, reportnorm_abuse_std_baseline),
    # restandardize
    norm_idx = stdize(norm_idx, to = norm_idx_baseline),
    norm_idx_baseline = stdize(norm_idx_baseline, to = norm_idx_baseline),
    # restandardize
    norm_idx_listwise = stdize(norm_idx_listwise, to = norm_idx_listwise_baseline),
    norm_idx_listwise_baseline = stdize(norm_idx_listwise_baseline, to = norm_idx_listwise_baseline)
    
  )

#--------------------------------------------------------------------------------------------------------------
                          # 12. Hypotheses M2a: police_capacity_idx
#--------------------------------------------------------------------------------------------------------------
col_citizen <- 
  col_citizen %>%
  mutate(
    # change the order of the variables to be consistent with the mpap
    polcap_timely_rescaled = polcap_timely - 1,
    polcap_investigate_rescaled = polcap_investigate - 1,
    polcap_timely_rescaled_baseline = polcap_timely_baseline - 1,
    polcap_investigate_rescaled_baseline = polcap_investigate_baseline - 1,
    # standardize the vars
    polcap_timely_std = stdize(polcap_timely_rescaled, to = polcap_timely_rescaled_baseline),
    polcap_investigate_std = stdize(polcap_investigate_rescaled, to = polcap_investigate_baseline),
    polcap_timely_std_baseline = stdize(polcap_timely_rescaled_baseline, to = polcap_timely_rescaled_baseline),
    polcap_investigate_std_baseline = stdize(polcap_investigate_rescaled_baseline, to = polcap_investigate_rescaled_baseline),
    # calculate index
    police_capacity_idx = idx_mean(polcap_timely_std, polcap_investigate_std, tx = Z, fe = block_ID),
    police_capacity_idx_baseline = idx_mean(polcap_timely_std_baseline, polcap_investigate_std_baseline, tx = Z, fe = block_ID),
    # restandardize
    police_capacity_idx = stdize(police_capacity_idx, to = police_capacity_idx_baseline),
    police_capacity_idx_baseline = stdize(police_capacity_idx_baseline, to = police_capacity_idx_baseline)
    
  )

# 3. List-wise deletion
col_citizen <-
  col_citizen %>%
  mutate(
    police_capacity_idx_listwise = idx_mean_listwise(polcap_timely_std, polcap_investigate_std),
    police_capacity_idx_listwise_baseline = idx_mean_listwise(polcap_timely_std_baseline, polcap_investigate_std_baseline),
    # restandardize
    police_capacity_idx_listwise = stdize(police_capacity_idx_listwise, to = police_capacity_idx_listwise_baseline),
    police_capacity_idx_listwise_baseline = stdize(police_capacity_idx_listwise_baseline, to = police_capacity_idx_listwise_baseline)
  )

#--------------------------------------------------------------------------------------------------------------
                          # 13. Hypotheses M2b: responsive_act
#--------------------------------------------------------------------------------------------------------------
col_citizen <- 
  col_citizen %>%
  mutate(
    # retain the raw definition
    responsive_act_raw = responsive_act, 
    responsive_act_raw_baseline = responsive_act_baseline,
    # change the order of the variables to be consistent with the mpap
    responsive_act_rescaled = responsive_act - 1,
    responsive_act_rescaled_baseline = responsive_act_baseline - 1,
    # standardize the vars
    responsive_act_std = stdize(responsive_act_rescaled, to = responsive_act_rescaled_baseline),
    responsive_act_std_baseline = stdize(responsive_act_rescaled_baseline, to = responsive_act_rescaled_baseline),
    # generate similar name to the one on the mpap
    responsive_act = responsive_act_std,
    responsive_act_baseline = responsive_act_std_baseline
  )

#--------------------------------------------------------------------------------------------------------------
                          # 14. Hypotheses S1: legit_trust
#--------------------------------------------------------------------------------------------------------------
col_citizen <- 
  col_citizen %>%
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
                          # 15. Hypotheses S2: trust_community
#--------------------------------------------------------------------------------------------------------------
col_citizen <- 
  col_citizen %>%
  mutate(
    # retain the raw definition
    trust_community_raw = trust_community, 
    trust_community_raw_baseline = trust_community_baseline,
    # standardize the vars
    trust_community_std = stdize(trust_community, to = trust_community_baseline),
    trust_community_std_baseline = stdize(trust_community_baseline, to = trust_community_baseline),
    # generate similar name to the one on the mpap
    trust_community = trust_community_std,
    trust_community_baseline = trust_community_std_baseline
  )

#--------------------------------------------------------------------------------------------------------------
                          # 16. Hypotheses C: compliance_idx
#--------------------------------------------------------------------------------------------------------------
col_citizen <- 
  col_citizen %>%
  mutate(
    compliance_patrol = if_else(compliance_patrol == 6L, 5L, compliance_patrol),
    compliance_freq = if_else(compliance_freq == 6L, 5L, compliance_freq),
    compliance_patrol_baseline = if_else(compliance_patrol_baseline == 6L, 5L, compliance_patrol_baseline),
    compliance_freq_baseline = if_else(compliance_freq_baseline == 6L, 5L, compliance_freq_baseline),
    # reverse order of variables to be consistent with the direction of the MPAP (MPAP page 75-76)
    compliance_patrol_rescaled = (6L - compliance_patrol),
    compliance_freq_rescaled = (6L - compliance_freq),
    compliance_patrol_rescaled_baseline = (6L - compliance_patrol_baseline),
    compliance_freq_rescaled_baseline = (6L - compliance_freq_baseline),
    # standardize the vars
    compliance_patrol_std = stdize(compliance_patrol_rescaled, to = compliance_patrol_rescaled_baseline),
    compliance_freq_std = stdize(compliance_freq_rescaled, to = compliance_freq_rescaled_baseline),
    compliance_meeting_std = stdize(compliance_meeting, to = compliance_meeting_baseline),
    compliance_patrol_std_baseline = stdize(compliance_patrol_rescaled_baseline, to = compliance_patrol_rescaled_baseline),
    compliance_freq_std_baseline = stdize(compliance_freq_rescaled_baseline, to = compliance_freq_rescaled_baseline),
    compliance_meeting_std_baseline = stdize(compliance_meeting_baseline, to = compliance_meeting_baseline),
    # calculate index
    compliance_idx = idx_mean(compliance_patrol_std, compliance_freq_std, compliance_meeting_std, tx = Z, fe = block_ID),
    compliance_idx_baseline = idx_mean(compliance_patrol_std_baseline, compliance_freq_std_baseline, compliance_meeting_std_baseline, tx = Z, fe = block_ID),
    # restandardize
    compliance_idx = stdize(compliance_idx, to = compliance_idx_baseline),
    compliance_idx_baseline = stdize(compliance_idx_baseline, to = compliance_idx_baseline)
    
  )

# 3. List-wise deletion
col_citizen <-
  col_citizen %>%
  mutate(
    compliance_idx_listwise = idx_mean_listwise(compliance_patrol_std, compliance_freq_std, compliance_meeting_std),
    compliance_idx_listwise_baseline = idx_mean_listwise(compliance_patrol_std_baseline, compliance_freq_std_baseline, compliance_meeting_std_baseline),
    # restandardize
    compliance_idx_listwise = stdize(compliance_idx_listwise, to = compliance_idx_listwise_baseline),
    compliance_idx_listwise_baseline = stdize(compliance_idx_listwise_baseline, to = compliance_idx_listwise_baseline)
  )

#--------------------------------------------------------------------------------------------------------------
                          # 17. Hyp 1a. (alt. i): crime_victim_idx_admin
#--------------------------------------------------------------------------------------------------------------
col_admin <- 
  col_admin %>% 
  rowwise() %>% 
  mutate(
    # generate categories of crimes
    aviolentcrime_num = adomestic_phys_num + aother_num_violent,
    # note: asimpleassault_num, amurder_num, aarmedrob_num, aaggassault_num, asexual_num were not collected in Col
    anonviolentcrime_num = aburglary_num + aother_num_nonviolent,
    aviolentcrime_num_baseline = adomestic_phys_num_baseline + aother_num_violent_baseline,
    anonviolentcrime_num_baseline = aburglary_num_baseline + aother_num_nonviolent_baseline) %>% 
  ungroup %>% 
  mutate(
    # standardize crimes
    aviolentcrime_num_std = stdize(aviolentcrime_num, to = aviolentcrime_num_baseline),
    anonviolentcrime_num_std = stdize(anonviolentcrime_num, to = anonviolentcrime_num_baseline),
    aviolentcrime_num_std_baseline = stdize(aviolentcrime_num_baseline, to = aviolentcrime_num_baseline),
    anonviolentcrime_num_std_baseline = stdize(anonviolentcrime_num_baseline, to = anonviolentcrime_num_baseline),
    
    adomestic_phys_num_std = stdize(adomestic_phys_num, to = adomestic_phys_num_baseline),
    aother_num_violent_std = stdize(aother_num_violent, to = aother_num_violent_baseline),
    aburglary_num_std = stdize(aburglary_num, to = aburglary_num_baseline),
    aother_num_nonviolent_std = stdize(aother_num_nonviolent, to = aother_num_nonviolent_baseline),
    adomestic_phys_num_std_baseline = stdize(adomestic_phys_num_baseline, to = adomestic_phys_num_baseline),
    aother_num_violent_std_baseline = stdize(aother_num_violent_baseline, to = aother_num_violent_baseline),
    aburglary_num_std_baseline = stdize(aburglary_num_baseline, to = aburglary_num_baseline),
    aother_num_nonviolent_std_baseline = stdize(aother_num_nonviolent_baseline, to = aother_num_nonviolent_baseline),
    
    
    # generate index
    crime_victim_idx_admin = idx_mean(aviolentcrime_num_std, anonviolentcrime_num_std, tx = Z, fe = block_ID),
    crime_victim_idx_admin_baseline = idx_mean(aviolentcrime_num_std_baseline, anonviolentcrime_num_std_baseline, tx = Z, fe = block_ID),
    # restandardize
    crime_victim_idx_admin = stdize(crime_victim_idx_admin, to = crime_victim_idx_admin_baseline),
    crime_victim_idx_admin_baseline = stdize(crime_victim_idx_admin_baseline, to = crime_victim_idx_admin_baseline)
  )

# 3. List-wise deletion
col_admin <-
  col_admin %>%
  mutate(
    crime_victim_idx_admin_listwise = idx_mean_listwise(aviolentcrime_num_std, anonviolentcrime_num_std),
    crime_victim_idx_admin_listwise_baseline = idx_mean_listwise(aviolentcrime_num_std_baseline, anonviolentcrime_num_std_baseline),
    # restandardize
    crime_victim_idx_admin_listwise = stdize(crime_victim_idx_admin_listwise, to = crime_victim_idx_admin_listwise_baseline),
    crime_victim_idx_admin_listwise_baseline = stdize(crime_victim_idx_admin_listwise_baseline, to = crime_victim_idx_admin_listwise_baseline))

#--------------------------------------------------------------------------------------------------------------
                          # 18. Hyp 1a. (alt. ii): crime_victim_idx_exp
#--------------------------------------------------------------------------------------------------------------
# this can not be estimates since only binary crimes were collected in col
#--------------------------------------------------------------------------------------------------------------
                          # 19. Hyp 1a. (alt. iii) crime_victim_idx_bin
#--------------------------------------------------------------------------------------------------------------
col_citizen <- 
  col_citizen %>% 
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
# Note: caggassault was not collected in Col
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
    violentcrime_bin =  armedrob_bin + simpleassault_bin,
    # note: all other crimes were not collected in colombia
    nonviolentcrime_bin = burglary_bin,
    cviolentcrime_bin = carmedrob_bin + csimpleassault_bin + csexual_bin + cdomestic_phys_bin + cmurder_bin,
    cnonviolentcrime_bin = cburglary_bin,
    
    violentcrime_bin_baseline = armedrob_bin_baseline + simpleassault_bin_baseline,
    nonviolentcrime_bin_baseline = burglary_bin_baseline,
    cviolentcrime_bin_baseline = carmedrob_bin_baseline + csimpleassault_bin_baseline + csexual_bin_baseline + cdomestic_phys_bin_baseline + cmurder_bin_baseline,
    cnonviolentcrime_bin_baseline = cburglary_bin_baseline) %>% 
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
    burglary_bin_std = stdize(burglary_bin, to = burglary_bin_baseline),
    carmedrob_bin_std = stdize(carmedrob_bin, to = carmedrob_bin_baseline),
    csimpleassault_bin_std = stdize(csimpleassault_bin, to = csimpleassault_bin_baseline),
    csexual_bin_std = stdize(csexual_bin, to = csexual_bin_baseline),
    cdomestic_phys_bin_std = stdize(cdomestic_phys_bin, to = cdomestic_phys_bin_baseline),
    cmurder_bin_std = stdize(cmurder_bin, to = cmurder_bin_baseline),
    cburglary_bin_std = stdize(cburglary_bin, to = cburglary_bin_baseline),

    armedrob_bin_std_baseline = stdize(armedrob_bin_baseline, to = armedrob_bin_baseline),
    simpleassault_bin_std_baseline = stdize(simpleassault_bin_baseline, to = simpleassault_bin_baseline),
    burglary_bin_std_baseline = stdize(burglary_bin_baseline, to = burglary_bin_baseline),
    carmedrob_bin_std_baseline = stdize(carmedrob_bin_baseline, to = carmedrob_bin_baseline),
    csimpleassault_bin_std_baseline = stdize(csimpleassault_bin_baseline, to = csimpleassault_bin_baseline),
    csexual_bin_std_baseline = stdize(csexual_bin_baseline, to = csexual_bin_baseline),
    cdomestic_phys_bin_std_baseline = stdize(cdomestic_phys_bin_baseline, to = cdomestic_phys_bin_baseline),
    cmurder_bin_std_baseline = stdize(cmurder_bin_baseline, to = cmurder_bin_baseline),
    cburglary_bin_std_baseline = stdize(cburglary_bin_baseline, to = cburglary_bin_baseline),

    crime_victim_idx_bin = idx_mean(violentcrime_bin_std, nonviolentcrime_bin_std, cviolentcrime_bin_std, cnonviolentcrime_bin_std, tx = Z, fe = block_ID),
    crime_victim_idx_bin_baseline = idx_mean(violentcrime_bin_std_baseline, nonviolentcrime_bin_std_baseline, cviolentcrime_bin_std_baseline, cnonviolentcrime_bin_std_baseline, tx = Z, fe = block_ID),
    # restandardize
    crime_victim_idx_bin = stdize(crime_victim_idx_bin, to = crime_victim_idx_bin_baseline),
    crime_victim_idx_bin_baseline = stdize(crime_victim_idx_bin_baseline, to = crime_victim_idx_bin_baseline)
    
  )

# 3. List-wise deletion
col_citizen <-
  col_citizen %>%
  mutate(
    crime_victim_idx_bin_listwise = idx_mean_listwise(violentcrime_bin_std, nonviolentcrime_bin_std, cviolentcrime_bin_std, cnonviolentcrime_bin_std),
    crime_victim_idx_bin_listwise_baseline = idx_mean_listwise(violentcrime_bin_std_baseline, nonviolentcrime_bin_std_baseline, cviolentcrime_bin_std_baseline, cnonviolentcrime_bin_std_baseline),
    # restandardize
    crime_victim_idx_bin_listwise = stdize(crime_victim_idx_bin_listwise, to = crime_victim_idx_bin_listwise_baseline),
    crime_victim_idx_bin_listwise_baseline = stdize(crime_victim_idx_bin_listwise_baseline, to = crime_victim_idx_bin_listwise_baseline)
  )


#--------------------------------------------------------------------------------------------------------------
                          # 20. hypothesis 4(a): crime_reporting_idx_admin
#--------------------------------------------------------------------------------------------------------------


#--------------------------------------------------------------------------------------------------------------
                          # 21. REMOVE ALL NON RESCALED VARIABLES
#--------------------------------------------------------------------------------------------------------------
rescaled_vars <-
  names(col_citizen)[str_ends(names(col_citizen), "_rescaled")] %>%
  str_replace("_rescaled", "")

col_citizen <- 
  col_citizen %>% 
  select(-rescaled_vars)

#--------------------------------------------------------------------------------------------------------------
                          # 21. SAVE CITIZEN AND ADMIN DATA
#--------------------------------------------------------------------------------------------------------------
saveRDS(col_citizen, file = "data/out/col-citizen-construct.RDS")
saveRDS(col_admin, file = "data/out/col-admin-construct.RDS")
saveRDS(col_officer, file = "data/out/col-officer-construct.RDS")
