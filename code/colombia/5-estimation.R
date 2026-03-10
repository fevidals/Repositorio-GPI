# All loaded packages should come here
suppressMessages({
  library(tidyverse)
  library(DeclareDesign)
})

# Load all the required data here
col_citizen <- readRDS("data/out/col-citizen-construct.RDS")
col_officer <- readRDS("data/out/col-officer-construct.RDS")
col_admin <- readRDS("data/out/col-admin-construct.RDS")

source("code/meta-analysis/0-variable-labels.R")

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
    
    # secondary outcomes
    "crime_victim_idx_bin",

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
    "norm_idx_listwise",
    "police_capacity_idx_listwise",
    "compliance_idx_listwise",
    
    # secondary outcomes
    "crime_victim_idx_bin_listwise",

    # sub-indices
    "crimeres_idx_listwise",
    "polint_idx_listwise",
    
    # 1a
    "violentcrime_num_std",
    "armedrob_num_std",
    "simpleassault_num_std",
    # "other_any_violent",
    
    "nonviolentcrime_num_std",
    "burglary_num_std",
    # "other_any_nonviolent",
    
    "cviolentcrime_num_std",
    "carmedrob_num_std",
    # "caggassault_num_std",
    "csimpleassault_num_std",
    "csexual_num_std",
    "cdomestic_phys_num_std",
    "cmurder_num_std",
    # "cother_any_violent_std",
    
    "cnonviolentcrime_num_std",
    "cburglary_num_std",
    # "cother_any_nonviolent",
    
    # 1b
    "fear_violent_std",
    # "fear_nonviolent_std",
    "feared_walk_std",
    
    # 2
    "satis_trust_std",

    # 3b
    "policeabuse_any_std",
    # "policeabuse_num_std",

    # 4a
    "violentcrime_report_num_std",
    "nonviolentcrime_report_num_std",
    "cviolentcrime_report_num_std",
    "cnonviolentcrime_report_num_std",
    
    "crimeres_idx",
    "burglaryres_std",
    "dviolres_std",
    # "armedrobres_std",
    
    "armedrob_report_std",
    "simpleassault_report_std",
    # "other_report_violent_std",
    "burglary_report_std",
    "carmedrob_report_std",
    # "caggassault_report_std",
    "csimpleassault_report_std",
    "csexual_report_std",
    "cdomestic_phys_report_std",
    # "cother_report_violent_std",
    "cburglary_report_std",
    
    # 4b
    "crime_tips_idx",
    "contact_pol_susp_activity_std",
    "give_info_pol_investigation_std",
    
    # 4c
    "policebeating_report_std",
    "policeabuse_report_std",
    # "dutydrink_report_std",
    
    # M1a
    "polcaseserious_std",
    "polint_idx",
    "polint_corrupt_std",

    # M1b
    
    # M1c
    "reportnorm_theft_std",
    "reportnorm_abuse_std",

    # M2a
    "polcap_timely_std",
    "polcap_investigate_std",
    
    # C
    "compliance_patrol_std",
    "compliance_freq_std",
    "compliance_meeting_std",
    
    # Hyp 1a. (alt. iii)
    "violentcrime_bin_std",
    "nonviolentcrime_bin_std",
    "cviolentcrime_bin_std",
    "cnonviolentcrime_bin_std",
    "armedrob_bin_std",
    "simpleassault_bin_std",
    "burglary_bin_std",
    "carmedrob_bin_std",
    "csimpleassault_bin_std",
    "csexual_bin_std",
    "cdomestic_phys_bin_std",
    "cmurder_bin_std",
    "cburglary_bin_std")

outcomes_citizen_no_baseline <-
  c(
    # indices
    "know_idx_common",
    "know_idx",
    "know_idx_listwise",
    

    # M1b
    "know_law_idx",
    "know_report_idx",
    "know_law_suspect_std",
    "know_law_lawyer_std",
    "know_law_fees_std",
    "know_report_followup",
    "know_report_station",
    "know_law_idx_listwise",
    "know_report_idx_listwise",
    
    "satis_general_std",
    
    "bribe_freq_std", 
    "bribe_amt_std",
    
    "polcasefair_std",
    
    "obeynorm_std")

outcomes_admin <- 
  c(# indices
    "crime_victim_idx_admin",
    # constituents
    "aviolentcrime_num_std",
    "adomestic_phys_num_std",
    "aother_num_violent_std",
    
    "anonviolentcrime_num_std",
    "aburglary_num_std",
    "aother_num_nonviolent_std",
    
    "crime_victim_idx_admin_listwise"
  )

outcomes_officer <-
  c(# indices
    "officer_attitude_idx",
    # sub-indices
    "corrupt_idx", 
    "abuse_idx", 
    "accountability_idx", 
    "empathy_idx",
    # constituents
    "hypothetical2_corruptself_std", 
    "hypothetical2_corruptother_std", 
    "hypothetical3_corruptself_std", 
    "hypothetical3_corruptother_std",
    
    "hypothetical5_abuseself_std", 
    "hypothetical5_abuseother_std",
    
    "hypothetical2_reportothers_std", 
    "hypothetical3_reportothers_std", 
    "hypothetical5_reportothers_std",
    "hypothetical5_reportself_std", 
    "hypothetical2_reportself_std", 
    "hypothetical3_reportself_std", 
    "hypothetical2_punishment_std", 
    "hypothetical3_punishment_std", 
    "hypothetical5_punishment_std",
    "account_pol_matter_std",
    
    "empathy_complaints_std", 
    "empathy_reports_std")


# citizen survey outcomes
estimates_list <- list()
for (var in outcomes_citizen) {
  estimation_df <- 
    col_citizen %>%
    mutate(
      # variable for whether baseline value is missing
      baseline_var_NA = if_else(is.na(!!rlang::sym(var)), 1, 0), 
      # replace baseline values with zeros if missing
      baseline_var = replace_na(!!rlang::sym(paste0(var, "_baseline")), 0)
    )
  
  # treatment effect estimation
  est <- lm_robust(
    as.formula(glue::glue("{ var } ~ Z_common + Z_alt + baseline_var + baseline_var_NA + as.factor(block_ID)")),
    clusters = cuadrante,
    weights = 1 / S_citizens_inclusion_prob,
    data = estimation_df) %>%
    tidy %>%
    filter(term == "Z_common")
  
  # test for differential attrition
  missing_est <- lm_robust(
    as.formula(glue::glue("is.na({ var }) ~ Z_common + Z_alt + baseline_var + baseline_var_NA + as.factor(block_ID)")),
    clusters = cuadrante,
    weights = 1 / S_citizens_inclusion_prob,
    data = estimation_df) %>%
    tidy %>%
    filter(term == "Z_common") %>% 
    select(missingness_statistic = statistic, missingness_p.value = p.value)
  
  # calculate proportion missing
  missing_prop <- col_citizen %>% summarize(across(all_of(var), ~mean(is.na(.)), .names = "missingness_proportion"))
  
  # combine estimates for this variable
  estimates_list[[var]] <-
    bind_cols(est, missing_prop, missing_est) %>% 
    mutate(across(c(missingness_statistic, missingness_p.value), ~if_else(missingness_proportion == 0, NA_real_, .)))
  
}

# survey *crime displacement* analysis
for (var in c("crime_victim_idx_common")) {
  
  # treatment effect estimation
  est_treatment <- lm_robust(
    as.formula(glue::glue("I({ var } - {var}_baseline) ~ as.factor(block_ID)")),
    weights = 1 / S_citizens_inclusion_prob,
    subset = Z == 1,
    data = col_citizen
  ) %>%
    tidy %>%
    filter(term == "(Intercept)")
  
  est_control <- lm_robust(
    as.formula(glue::glue("I({ var } - {var}_baseline) ~ as.factor(block_ID)")),
    weights = 1 / S_citizens_inclusion_prob,
    subset = Z == 0,
    data = col_citizen
  ) %>%
    tidy %>%
    filter(term == "(Intercept)")
  
  estimates_list[[paste0("displace_treatment_", var)]] <- est_treatment %>% mutate(outcome = paste0("displace_treatment_", var))
  estimates_list[[paste0("displace_control_", var)]] <- est_control %>% mutate(outcome = paste0("displace_control_", var))
}


# outcomes with only an indicator at the endline
for (var in outcomes_citizen_no_baseline) {
  # treatment effect estimation
  est <- lm_robust(
    as.formula(glue::glue("{ var } ~ Z_common + Z_alt + as.factor(block_ID)")),
    clusters = cuadrante,
    weights = 1 / S_citizens_inclusion_prob,
    data = col_citizen) %>%
    tidy %>%
    filter(term == "Z_common")
  
  # test for differential attrition
  missing_est <- lm_robust(
    as.formula(glue::glue("is.na({ var }) ~ Z_common + Z_alt + as.factor(block_ID)")),
    clusters = cuadrante,
    weights = 1 / S_citizens_inclusion_prob,
    data = col_citizen) %>%
    tidy %>%
    filter(term == "Z_common") %>% 
    select(missingness_statistic = statistic, missingness_p.value = p.value)
  
  # calculate proportion missing
  missing_prop <- col_citizen %>% summarize(across(all_of(var), ~mean(is.na(.)), .names = "missingness_proportion"))
  
  # combine estimates for this variable
  estimates_list[[var]] <-
    bind_cols(est, missing_prop, missing_est) %>% 
    mutate(across(c(missingness_statistic, missingness_p.value), ~if_else(missingness_proportion == 0, NA_real_, .)))
}

# admin outcomes
for (var in outcomes_admin) {
  estimation_df <- 
    col_admin %>%
    mutate(
      # variable for whether baseline value is missing
      baseline_var_NA = if_else(is.na(!!rlang::sym(paste0(var, "_baseline"))), 1, 0), 
      # replace baseline values with zeros if missing
      baseline_var = replace_na(!!rlang::sym(paste0(var, "_baseline")), 0)
    )
  
  # treatment effect estimation
  est <- lm_robust(
    as.formula(glue::glue("{ var } ~ Z_common + Z_alt + baseline_var + baseline_var_NA + as.factor(block_ID)")),
    data = estimation_df) %>%
    tidy %>%
    filter(term == "Z_common")
  
  # test for differential attrition
  missing_est <- lm_robust(
    as.formula(glue::glue("is.na({ var }) ~ Z_common + Z_alt + baseline_var + baseline_var_NA + as.factor(block_ID)")),
    data = estimation_df) %>%
    tidy %>%
    filter(term == "Z_common") %>% 
    select(missingness_statistic = statistic, missingness_p.value = p.value)
  
  # calculate proportion missing
  missing_prop <- col_admin %>% summarize(across(all_of(var), ~mean(is.na(.)), .names = "missingness_proportion"))
  
  # combine estimates for this variable
  estimates_list[[var]] <-
    bind_cols(est, missing_prop, missing_est) %>% 
    mutate(across(c(missingness_statistic, missingness_p.value), ~if_else(missingness_proportion == 0, NA_real_, .)))
}

# admin *crime displacement* analysis
for (var in c("crime_victim_idx_admin")) {
  
  # treatment effect estimation
  est_treatment <- lm_robust(
    as.formula(glue::glue("I({ var } - {var}_baseline) ~ 1")),
    subset = Z == 1,
    data = col_admin
  ) %>%
    tidy %>%
    filter(term == "(Intercept)")
  
  est_control <- lm_robust(
    as.formula(glue::glue("I({ var } - {var}_baseline) ~ 1")),
    subset = Z == 0,
    data = col_admin
  ) %>%
    tidy %>%
    filter(term == "(Intercept)")
  
  estimates_list[[paste0("displace_treatment_", var)]] <- est_treatment %>% mutate(outcome = paste0("displace_treatment_", var))
  estimates_list[[paste0("displace_control_", var)]] <- est_control %>% mutate(outcome = paste0("displace_control_", var))
}


# officer outcomes
for (var in outcomes_officer) {
  # treatment effect estimation
  est <- lm_robust(
    as.formula(glue::glue("{ var } ~ Z_common + Z_alt + as.factor(block_ID)")),
    clusters = cuadrante,
    data = col_officer) %>%
    tidy %>%
    filter(term == "Z_common")
  
  # test for differential attrition
  missing_est <- lm_robust(
    as.formula(glue::glue("is.na({ var }) ~ Z_common + Z_alt + as.factor(block_ID)")),
    clusters = cuadrante,
    data = col_officer) %>%
    tidy %>%
    filter(term == "Z_common") %>% 
    select(missingness_statistic = statistic, missingness_p.value = p.value)
  
  # calculate proportion missing
  missing_prop <- col_officer %>% summarize(across(all_of(var), ~mean(is.na(.)), .names = "missingness_proportion"))
  
  # combine estimates for this variable
  estimates_list[[var]] <-
    bind_cols(est, missing_prop, missing_est) %>% 
    mutate(across(c(missingness_statistic, missingness_p.value), ~if_else(missingness_proportion == 0, NA_real_, .)))
}


estimates_df <- 
  bind_rows(estimates_list) %>% 
  mutate(study = "col")

estimates_df_primary <- 
  estimates_df %>% 
  left_join(main_hypotheses, by = "outcome") %>% 
  filter(hypothesis %in% c("1a", "1b", "2", "3a", "3b", "4a", "4b", "4c")) %>% 
  mutate(p.value.adj = p.adjust(p.value, method = "BH"))

estimates_df_secondary_hyp <- 
  estimates_df %>% 
  inner_join(secondary_hypotheses, by = "outcome")

estimates_df_secondary_hyp_listwise <- 
  estimates_df %>% 
  inner_join(secondary_hypotheses_listwise, by = "outcome")

estimates_df_secondary <- 
  estimates_df %>% 
  left_join(main_hypotheses, by = "outcome") %>% 
  filter(!hypothesis %in% c("1a", "1b", "2", "3a", "3b", "4a", "4b", "4c") & !is.na(hypothesis))

estimates_df_primary_original <- 
  estimates_df %>% 
  inner_join(main_hypotheses_original, by = "outcome") %>% 
  filter(hypothesis %in% c("1a", "1b", "2", "3a", "3b", "4a", "4b", "4c")) %>% 
  mutate(p.value.adj = p.adjust(p.value, method = "BH"))

estimates_df_secondary_original <- 
  estimates_df %>% 
  inner_join(main_hypotheses_original, by = "outcome") %>% 
  filter(!hypothesis %in% c("1a", "1b", "2", "3a", "3b", "4a", "4b", "4c") & !is.na(hypothesis))

estimates_df_primary_listwise <- 
  estimates_df %>% 
  inner_join(main_hypotheses_listwise, by = "outcome") %>% 
  filter(hypothesis %in% c("1a", "1b", "2", "3a", "3b", "4a", "4b", "4c")) %>% 
  mutate(p.value.adj = p.adjust(p.value, method = "BH"))

estimates_df_secondary_listwise <- 
  estimates_df %>% 
  inner_join(main_hypotheses_listwise, by = "outcome") %>% 
  filter(!hypothesis %in% c("1a", "1b", "2", "3a", "3b", "4a", "4b", "4c") & !is.na(hypothesis))

estimates_df_components <- 
  estimates_df %>% 
  left_join(index_components, by = "outcome") %>% 
  filter(!is.na(hypothesis))

estimates_df_sub_components <- 
  estimates_df %>% 
  left_join(index_components, by = "outcome") %>% 
  filter(is.na(hypothesis)) %>% 
  filter(!str_detect(outcome, "_idx"))

estimates_df_components_primary <- 
  estimates_df_components %>% 
  filter(hypothesis %in% c("1a", "1b", "2", "3a", "3b", "4a", "4b", "4c")) %>% 
  group_by(hypothesis) %>% 
  mutate(p.value.adj = p.adjust(p.value, method = "BH")) %>% 
  ungroup()

estimates_df_components_secondary <- 
  estimates_df_components %>% 
  filter(!hypothesis %in% c("1a", "1b", "2", "3a", "3b", "4a", "4b", "4c") & !is.na(hypothesis))

estimates_df_displacement <- 
  estimates_df %>% 
  filter(str_detect(outcome, "displace"))

estimates_df <- 
  bind_rows(
    estimates_df_primary,
    estimates_df_primary_original,
    estimates_df_primary_listwise,
    estimates_df_secondary_hyp,
    estimates_df_secondary,
    estimates_df_secondary_original,
    estimates_df_secondary_listwise,
    estimates_df_secondary_hyp_listwise,
    estimates_df_components_primary %>% 
      filter(!outcome %in% c("crime_reporting_idx_common", "crime_victim_idx_common", "future_insecurity_idx_common", "police_abuse_idx_common", "police_abuse_report_idx_common", "satis_idx", "tips_idx", "compliance_idx", "intentions_idx", "know_idx_common", "legit_trust_std", "norm_idx", "police_capacity_idx", "responsive_act_std", "trust_community_std")),
    estimates_df_components_secondary %>% 
      filter(!outcome %in% c("crime_reporting_idx_common", "crime_victim_idx_common", "future_insecurity_idx_common", "police_abuse_idx_common", "police_abuse_report_idx_common", "satis_idx", "tips_idx", "compliance_idx", "intentions_idx", "know_idx_common", "legit_trust_std", "norm_idx", "police_capacity_idx", "responsive_act_std", "trust_community_std")),
    estimates_df_sub_components %>% 
      filter(!outcome %in% c("crime_reporting_idx_common", "crime_victim_idx_common", "future_insecurity_idx_common", "police_abuse_idx_common", "police_abuse_report_idx_common", "satis_idx", "tips_idx", "compliance_idx", "intentions_idx", "know_idx_common", "legit_trust_std", "norm_idx", "police_capacity_idx", "responsive_act_std", "trust_community_std")),
    estimates_df_displacement) %>% 
  select(-label, -hypothesis, -order) %>% 
  distinct(outcome, .keep_all = TRUE) 

saveRDS(estimates_df,  file = "data/out/col-estimates.RDS")

