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
pak_officer <- readRDS("data/out/pak-officer-construct.RDS")
pak_admin <- readRDS("data/out/pak-admin-construct.RDS")
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

outcomes_admin <- 
  c(
    # indices
    "crime_victim_idx_admin",
    "crime_victim_idx_admin_listwise",

    # constituents
    "anonviolentcrime_num_std", 
    "aviolentcrime_num_std",
    
    "aarmedrob_num_std",
    "asimpleassault_num_std",
    "aaggassault_num_std",
    "asexual_num_std",
    "adomestic_phys_num_std",
    "amurder_num_std",
    "aburglary_num_std"
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
    pak_citizen %>%
    mutate(
      # variable for whether baseline value is missing
      baseline_var_NA = if_else(is.na(!!rlang::sym(paste0(var, "_baseline"))), 1, 0), 
      # replace baseline values with zeros if missing
      baseline_var = replace_na(!!rlang::sym(paste0(var, "_baseline")), 0)
    )
  
  # treatment effect estimation
  est <- lm_robust(
    as.formula(glue::glue("{ var } ~ Z_common + Z_alt + baseline_var + baseline_var_NA + as.factor(stations)")),
    clusters = beats,
    weights = 1 / Z_multistage_assignment_prob * S_multistage_inclusion_survey,
    data = estimation_df) %>%
    tidy %>%
    filter(term == "Z_common")
  
  # test for differential attrition
  missing_est <- lm_robust(
    as.formula(glue::glue("is.na({ var }) ~ Z_common + Z_alt + baseline_var + baseline_var_NA + as.factor(stations)")),
    clusters = beats,
    weights = 1 / Z_multistage_assignment_prob * S_multistage_inclusion_survey,
    data = estimation_df) %>%
    tidy %>%
    filter(term == "Z_common") %>% 
    select(missingness_statistic = statistic, missingness_p.value = p.value)
  
  # calculate proportion missing
  missing_prop <- pak_citizen %>% summarize(across(all_of(var), ~mean(is.na(.)), .names = "missingness_proportion"))
  
  # combine estimates for this variable
  estimates_list[[var]] <-
    bind_cols(est, missing_prop, missing_est) %>% 
    mutate(across(c(missingness_statistic, missingness_p.value), ~if_else(missingness_proportion == 0, NA_real_, .)))
}

# citizen *crime displacement* analysis
for (var in c("crime_victim_idx_common")) {
  
  # treatment effect estimation
  est_treatment <- lm_robust(
    as.formula(glue::glue("I({ var } - {var}_baseline) ~ as.factor(stations)")),
    # clusters = beats,
    weights = 1 / Z_multistage_assignment_prob,
    subset = Z == 2,
    data = pak_citizen
  ) %>%
    tidy %>%
    filter(term == "(Intercept)")
  
  est_control <- lm_robust(
    as.formula(glue::glue("I({ var } - {var}_baseline) ~ as.factor(stations)")),
    # clusters = beats,
    # weights = 1 / Z_multistage_assignment_prob,
    subset = Z == 0,
    data = pak_citizen
  ) %>%
    tidy %>%
    filter(term == "(Intercept)")
  
  estimates_list[[paste0("displace_treatment_", var)]] <- est_treatment %>% mutate(outcome = paste0("displace_treatment_", var))
  estimates_list[[paste0("displace_control_", var)]] <- est_control %>% mutate(outcome = paste0("displace_control_", var))
}


# admin outcomes
for (var in outcomes_admin) {
  estimation_df <- 
    pak_admin %>%
    mutate(
      # variable for whether baseline value is missing
      baseline_var_NA = if_else(is.na(!!rlang::sym(var)), 1, 0), 
      # replace baseline values with zeros if missing
      baseline_var = replace_na(!!rlang::sym(paste0(var, "_baseline")), 0)
    )
  
  # treatment effect estimation
  est <- lm_robust(
    as.formula(glue::glue("{ var } ~ Z_common + Z_alt + baseline_var + baseline_var_NA + as.factor(stations)")),
    weights = 1 / Z_multistage_assignment_prob,
    data = estimation_df) %>%
    tidy %>%
    filter(term == "Z_common")
  
  # test for differential attrition
  missing_est <- lm_robust(
    as.formula(glue::glue("is.na({ var }) ~ Z_common + Z_alt + baseline_var + baseline_var_NA + as.factor(stations)")),
    weights = 1 / Z_multistage_assignment_prob,
    data = estimation_df) %>%
    tidy %>%
    filter(term == "Z_common") %>% 
    select(missingness_statistic = statistic, missingness_p.value = p.value)
  
  # calculate proportion missing
  missing_prop <- pak_admin %>% summarize(across(all_of(var), ~mean(is.na(.)), .names = "missingness_proportion"))
  
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
    weights = 1 / Z_multistage_assignment_prob,
    subset = Z == 2,
    data = pak_admin
  ) %>%
    tidy %>%
    filter(term == "(Intercept)")
  
  est_control <- lm_robust(
    as.formula(glue::glue("I({ var } - {var}_baseline) ~ 1")),
    weights = 1 / Z_multistage_assignment_prob,
    subset = Z == 0,
    data = pak_admin
  ) %>%
    tidy %>%
    filter(term == "(Intercept)")
  
  estimates_list[[paste0("displace_treatment_", var)]] <- est_treatment %>% mutate(outcome = paste0("displace_treatment_", var))
  estimates_list[[paste0("displace_control_", var)]] <- est_control %>% mutate(outcome = paste0("displace_control_", var))
}

for (var in outcomes_officer) {
  estimation_df <- 
    pak_officer %>%
    mutate(
      # variable for whether baseline value is missing
      baseline_var_NA = if_else(is.na(!!rlang::sym(var)), 1, 0), 
      # replace baseline values with zeros if missing
      baseline_var = replace_na(!!rlang::sym(paste0(var, "_baseline")), 0)
    )
  
  # treatment effect estimation
  est <- lm_robust(
    as.formula(glue::glue("{ var } ~ Z_common + baseline_var + baseline_var_NA + as.factor(block_ID)")),
    clusters = beats,
    data = estimation_df) %>%
    tidy %>%
    filter(term == "Z_common")
  
  # test for differential attrition
  missing_est <- lm_robust(
    as.formula(glue::glue("is.na({ var }) ~ Z_common + baseline_var + baseline_var_NA + as.factor(block_ID)")),
    clusters = beats,
    data = estimation_df
  ) %>%
    tidy %>%
    filter(term == "Z_common") %>% 
    select(missingness_statistic = statistic, missingness_p.value = p.value)
  
  # calculate proportion missing
  missing_prop <- pak_officer %>% summarize(across(all_of(var), ~mean(is.na(.)), .names = "missingness_proportion"))
  
  # combine estimates for this variable
  estimates_list[[var]] <-
    bind_cols(est, missing_prop, missing_est) %>% 
    mutate(across(c(missingness_statistic, missingness_p.value), ~if_else(missingness_proportion == 0, NA_real_, .)))
  
}

estimates_df <- 
  bind_rows(estimates_list) %>% 
  mutate(study = "pak")

estimates_df_primary <- 
  estimates_df %>% 
  left_join(main_hypotheses, by = "outcome") %>% 
  filter(hypothesis %in% c("1a", "1b", "2", "3a", "3b", "4a", "4b", "4c")) %>% 
  mutate(p.value.adj = p.adjust(p.value, method = "BH"))

estimates_df_secondary_hyp <- 
  estimates_df %>% 
  inner_join(secondary_hypotheses, by = "outcome")

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

estimates_df_secondary_hyp_listwise <- 
  estimates_df %>% 
  inner_join(secondary_hypotheses_listwise, by = "outcome")

estimates_df <- 
  bind_rows(
    estimates_df_primary,
    estimates_df_primary_original,
    estimates_df_primary_listwise,
    estimates_df_secondary_hyp,
    estimates_df_secondary_hyp_listwise,
    estimates_df_secondary,
    estimates_df_secondary_original,
    estimates_df_secondary_listwise,
    estimates_df_components_primary %>% 
      filter(!outcome %in% c("crime_reporting_idx_common", "crime_victim_idx_common", "future_insecurity_idx_common", "police_abuse_idx_common", "police_abuse_report_idx_common", "satis_idx", "tips_idx", "compliance_idx", "intentions_idx", "know_idx_common", "legit_trust_std", "norm_idx", "police_capacity_idx", "responsive_act_std", "trust_community_std")),
    estimates_df_components_secondary %>% 
      filter(!outcome %in% c("crime_reporting_idx_common", "crime_victim_idx_common", "future_insecurity_idx_common", "police_abuse_idx_common", "police_abuse_report_idx_common", "satis_idx", "tips_idx", "compliance_idx", "intentions_idx", "know_idx_common", "legit_trust_std", "norm_idx", "police_capacity_idx", "responsive_act_std", "trust_community_std")),
    estimates_df_sub_components %>% 
      filter(!outcome %in% c("crime_reporting_idx_common", "crime_victim_idx_common", "future_insecurity_idx_common", "police_abuse_idx_common", "police_abuse_report_idx_common", "satis_idx", "tips_idx", "compliance_idx", "intentions_idx", "know_idx_common", "legit_trust_std", "norm_idx", "police_capacity_idx", "responsive_act_std", "trust_community_std")),
    estimates_df_displacement) %>% 
  select(-label, -hypothesis, -order) %>% 
  distinct(outcome, .keep_all = TRUE)

saveRDS(estimates_df,  file = "data/out/pak-estimates.RDS")
