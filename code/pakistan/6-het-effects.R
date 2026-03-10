#--------------------------------------------------------------------------------------------------------------
                      # 1. all loaded packages should come here
#--------------------------------------------------------------------------------------------------------------
suppressMessages({
  library(tidyverse)
  library(DeclareDesign)
})

#--------------------------------------------------------------------------------------------------------------
                      # 2. read raw data
#--------------------------------------------------------------------------------------------------------------
pak_citizen <- readRDS("data/out/pak-citizen-construct.RDS")
source("code/meta-analysis/0-variable-labels.R")

#--------------------------------------------------------------------------------------------------------------
                      # 3. estimates
#--------------------------------------------------------------------------------------------------------------
outcomes_citizen <-
  c(
    "crime_victim_idx_common",
    "future_insecurity_idx_common",
    "satis_idx",
    # "officer_attitude_idx",
    "police_abuse_idx_common",
    "crime_reporting_idx_common",
    "tips_idx",
    "police_abuse_report_idx_common",
    "intentions_idx",
    "know_idx_common",
    "norm_idx",
    "police_capacity_idx",
    "responsive_act_std",
    "legit_trust_std",
    "trust_community_std",
    "compliance_idx",
    
    "crime_victim_idx",
    "future_insecurity_idx",
    "police_abuse_idx",
    "crime_reporting_idx",
    "police_abuse_report_idx",
    "know_idx",
    
    # list-wise deletion
    "crime_victim_idx_listwise",
    "future_insecurity_idx_listwise",
    "satis_idx_listwise",
    # "officer_attitude_idx",
    "police_abuse_idx_listwise",
    "crime_reporting_idx_listwise",
    "tips_idx_listwise",
    "police_abuse_report_idx_listwise",
    "intentions_idx_listwise",
    "know_idx_listwise",
    "norm_idx_listwise",
    "police_capacity_idx_listwise",
    "compliance_idx_listwise",
    
    # secondary outcomes
    "crime_victim_idx_exp_listwise",
    "crime_victim_idx_bin_listwise",
    
    # sub-indices
    "crimeres_idx_listwise",
    "polint_idx_listwise",
    "know_law_idx_listwise",
    "know_law_idx_listwise",
    
    # secondary outcomes
    "crime_victim_idx_exp",
    "crime_victim_idx_bin",
    
    # 1a
    "violentcrime_num_std",
    "armedrob_num_std",
    "simpleassault_num_std",
    "other_any_violent_std",
    
    "nonviolentcrime_num_std",
    "burglary_num_std",
    "other_any_nonviolent_std",
    
    "cviolentcrime_num_std",
    "carmedrob_num_std",
    # "caggassault_num",
    "csimpleassault_num_std",
    "csexual_num_std",
    "cdomestic_phys_num_std",
    "cmurder_num_std",
    "cother_any_violent_std",
    
    "cnonviolentcrime_num_std",
    "cburglary_num_std",
    "cother_any_nonviolent_std",
    # 1b
    "fear_violent_std",
    "fear_nonviolent_std",
    "feared_walk_std",
    # 2
    "satis_trust_std",
    "satis_general_std",
    # 3b
    "policeabuse_any_std",
    "policeabuse_num_std",
    "policeabuse_report_std",
    "bribe_freq_std", 
    "bribe_amt_std",
    # 4a
    # "crime_report_num",
    "violentcrime_report_num_std",
    # "ccrime_report_num",
    "cviolentcrime_report_num_std",
    "crimeres_idx",
    "burglaryres_std",
    "dviolres_std",
    "armedrobres_std",
    # 4b
    "crime_tips_idx",
    "contact_pol_susp_activity_std",
    "give_info_pol_investigation_std",
    # 4c
    "dutydrink_report_std",
    "policebeating_report_std",
    "policeabuse_report_std",
    # M1a
    "polcaseserious_std",
    "polcasefair_std",
    "polint_idx",
    "polint_corrupt_std",
    "polcasefair_std",
    # M1b
    "know_law_idx",
    "know_report_idx",
    # M1c
    "reportnorm_theft_std",
    "reportnorm_abuse_std",
    "obeynorm_std",
    # M2a
    "polcap_timely_std",
    "polcap_investigate_std",
    # C
    "compliance_patrol_std",
    "compliance_freq_std",
    "compliance_meeting_std",
    
    # Hyp 1a. (alt. ii)
    "violentcrime_num_exp",
    "nonviolentcrime_num_exp",
    "cviolentcrime_num_exp",
    "cnonviolentcrime_num_exp",
    
    "armedrob_num_std",
    "simpleassault_num_std",
    "other_any_violent_std",
    "burglary_num_std",
    "land_any_std",
    "other_any_nonviolent_std",
    "carmedrob_num_std",
    "caggassault_num_std",
    "csimpleassault_num_std",
    "csexual_num_std",
    "cdomestic_phys_num_std",
    "cmurder_num_std",
    "cmob_num_std",
    "cother_any_violent_std",
    "cburglary_num_std",
    "cother_any_nonviolent_std",
    
    # Hyp 1a. (alt. iii)
    "violentcrime_bin_std",
    "nonviolentcrime_bin_std",
    "cviolentcrime_bin_std",
    "cnonviolentcrime_bin_std",
    
    "armedrob_bin_std",
    "simpleassault_bin_std",
    "other_any_violent_std",
    "burglary_bin_std",
    "other_any_nonviolent_std",
    "carmedrob_bin_std",
    "caggassault_bin_std",
    "csimpleassault_bin_std",
    "csexual_bin_std",
    "cdomestic_phys_bin_std",
    "cmurder_bin_std",
    "cother_any_violent_std",
    "cburglary_bin_std",
    "cother_any_nonviolent_std")


# citizen survey outcomes
estimates_list <- list()
for (var in outcomes_citizen) {
  estimation_df <- 
    pak_citizen %>%
    mutate(
      # variable for whether baseline value is missing
      baseline_var_NA = if_else(is.na(!!rlang::sym(var)), 1, 0), 
      # replace baseline values with zeros if missing
      baseline_var = replace_na(!!rlang::sym(paste0(var, "_baseline")), 0),
      # variable for whether baseline value is missing
      baseline_het_var_trust_NA = if_else(is.na(satis_trust_baseline), 1, 0), 
      # replace baseline values with zeros if missing
      baseline_het_var_trust = replace_na(satis_trust_baseline, 0),
      # variable for whether baseline value is missing
      baseline_het_var_legit_NA = if_else(is.na(legit_trust_std), 1, 0), 
      # replace baseline values with zeros if missing
      baseline_het_var_legit = replace_na(legit_trust_std, 0),
      # variable for whether baseline value is missing
      baseline_het_var_trustcom_NA = if_else(is.na(trust_community_std), 1, 0), 
      # replace baseline values with zeros if missing
      baseline_het_var_trustcom = replace_na(trust_community_std, 0),
      # variable for whether baseline value is missing
      baseline_het_var_crime_NA = if_else(is.na(crime_victim_idx_cluster_baseline), 1, 0), 
      # replace baseline values with zeros if missing
      baseline_het_var_crime = replace_na(crime_victim_idx_cluster_baseline, 0)
    )
  estimates_list[[paste0(var, "_het_trust")]] <- lm_robust(
    as.formula(glue::glue("{ var } ~ Z_common * (baseline_het_var_trust + baseline_het_var_trust_NA) + Z_alt + baseline_var + baseline_var_NA + as.factor(stations)")),
    clusters = beats,
    weights = 1 / Z_multistage_assignment_prob * S_multistage_inclusion_survey,
    data = estimation_df) %>%
    tidy %>%
    filter(term == "Z_common:baseline_het_var_trust")
  
  estimates_list[[paste0(var, "_het_trustcom")]] <- lm_robust(
    as.formula(glue::glue("{ var } ~ Z_common * (baseline_het_var_trustcom + baseline_het_var_trustcom_NA) + Z_alt + baseline_var + baseline_var_NA + as.factor(stations)")),
    clusters = beats,
    weights = 1 / Z_multistage_assignment_prob * S_multistage_inclusion_survey,
    data = estimation_df) %>%
    tidy %>%
    filter(term == "Z_common:baseline_het_var_trustcom")
  
  estimates_list[[paste0(var, "_het_legit")]] <- lm_robust(
    as.formula(glue::glue("{ var } ~ Z_common * (baseline_het_var_legit + baseline_het_var_legit_NA) + Z_alt + baseline_var + baseline_var_NA + as.factor(stations)")),
    clusters = beats,
    weights = 1 / Z_multistage_assignment_prob * S_multistage_inclusion_survey,
    data = estimation_df) %>%
    tidy %>%
    filter(term == "Z_common:baseline_het_var_legit")
  
  estimates_list[[paste0(var, "_het_crime")]] <- lm_robust(
    as.formula(glue::glue("{ var } ~ Z_common * (baseline_het_var_crime + baseline_het_var_crime_NA) + Z_alt + baseline_var + baseline_var_NA + as.factor(stations)")),
    clusters = beats,
    weights = 1 / Z_multistage_assignment_prob * S_multistage_inclusion_survey,
    data = estimation_df) %>%
    tidy %>%
    filter(term == "Z_common:baseline_het_var_crime")
  
  
}

estimates_df <- 
  bind_rows(estimates_list) %>% 
  mutate(study = "pak")

saveRDS(estimates_df,  file = "data/out/pak-estimates-het.RDS")
