
#  All loaded packages should come here
suppressMessages({
  library(tidyverse)
  library(DeclareDesign)
})

bra_citizen <- readRDS("data/out/bra-citizen-construct.RDS")
bra_admin <- readRDS("data/out/bra-admin-construct.RDS")

source("code/meta-analysis/0-variable-labels.R")

outcomes_citizen <-
  c(
    # indices
    "crime_victim_idx_common",
    "future_insecurity_idx",
    "satis_idx",
    # "officer_attitude_idx",
    "police_abuse_idx_common",
    "crime_reporting_idx_common",
    "tips_idx",
    # "police_abuse_report_idx",
    "intentions_idx",
    # "know_idx_common",
    "norm_idx",
    "police_capacity_idx",
    "responsive_act_std",
    # "legit_trust_std",
    "trust_community_std",
    "compliance_idx",
    
    # secondary outcomes
    "crime_victim_idx_exp",
    "crime_victim_idx_bin",
    
    
    # list-wise deletion
    "crime_victim_idx_listwise",
    "future_insecurity_idx_listwise",
    "satis_idx_listwise",
    # "officer_attitude_idx",
    "police_abuse_idx_listwise",
    "crime_reporting_idx_listwise",
    "tips_idx_listwise",
    # "police_abuse_report_idx_listwise", # this was not indexed because of only one variable available
    "intentions_idx_listwise",
    # "know_idx_listwise", # this was not indexed because of only one variable available
    # "norm_idx_listwise", # this was not indexed because of only one variable available
    "police_capacity_idx_listwise",
    "compliance_idx_listwise",

    # secondary outcomes
    "crime_victim_idx_exp_listwise",
    "crime_victim_idx_bin_listwise",

    # sub-indices
    "crimeres_idx_listwise",
    "polint_idx_listwise",

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
    # "caggassault_num_std",
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
    "bribe_freq_std",
    "bribe_amt_std",
    # 4a
    "violentcrime_report_num_std",
    "nonviolentcrime_report_num_std",
    "cviolentcrime_report_num_std",
    "cnonviolentcrime_report_num_std",

    "crimeres_idx",
    "burglaryres_std",
    "dviolres_std",
    "armedrobres_std",

    # "armedrob_report_std",
    # "simpleassault_report_std",
    # "other_report_violent_std",
    # "burglary_report_std",
    # "other_report_nonviolent_std",
      
    # "carmedrob_report_std",
    # "caggassault_report_std",
    # "csimpleassault_report_std",
    # "csexual_report_std",
    # "cdomestic_phys_report_std",
    # "cother_report_violent_std",
    # "cburglary_report_std",
    # "cother_report_nonviolent_std",
    # 4b
    "crime_tips_idx",
    "contact_pol_susp_activity_std",
    "give_info_pol_investigation_std",
    # 4c
      # "policebeating_report_std", # not collected for Brazil
      # "policeabuse_report_std",
      # "dutydrink_report_std", # not collected for Brazil
      # "apolvtm_station_std", # not collected in the admin data
    # M1a
    "polcaseserious_std",
    "polcasefair_std",
    "polint_idx",
    "polint_quality_std",
    "polint_corrupt_std",
    "polcasefair_std",
    # M1b
      # The index know_idx only had one component
    # M1c
      # "reportnorm_theft_std", # not collected for Brazil
      # "reportnorm_abuse_std", # not collected for Brazil
    "obeynorm_std",
    # M2a
    "polcap_timely_std",
    "polcap_investigate_std",
    # S1
    "legit_trust_std",
    # S2 
    "trust_community_std",
    # C
    "compliance_patrol_std",
    "compliance_meeting_std"
    )

outcomes_admin <-
  c(# indices
    "crime_victim_idx_admin",
    "crime_victim_idx_admin_listwise",
    # constituents
    "aviolentcrime_num_std",
    "aarmedrob_num_std",
    "aaggassault_num_std",
    # "asimpleassault_num_std",
    "asexual_num_std",
    # "adomestic_phys_num_std",
    "amurder_num_std",
    "aother_num_violent_std",
    
    "anonviolentcrime_num_std",
    "aburglary_num_std",
    "aother_num_nonviolent_std"
  )

#--------------------------------------------------------------------------------------------------------------
# 1. presence_201810
#--------------------------------------------------------------------------------------------------------------

# citizen survey outcomes
estimates_list <- list()
for (var in outcomes_citizen) {
  estimation_df <- 
    bra_citizen %>% 
    filter(municipality != "Florianopolis (x2)") %>% 
    mutate(
      # variable for whether baseline value is missing
      baseline_var_NA = if_else(is.na(!!rlang::sym(paste0(var, "_baseline"))), 1, 0), 
      # replace baseline values with zeros if missing
      baseline_var = replace_na(!!rlang::sym(paste0(var, "_baseline")), 0)
    )
  
  print(var)
  if(nrow(distinct(na.omit(estimation_df[, var]))) == 1) stop("error")
  
  # treatment effect estimation
  est <- iv_robust(
    as.formula(glue::glue("{ var } ~ groupformed_presence_201810 + baseline_var + baseline_var_NA + as.factor(muncode) | Z + baseline_var + baseline_var_NA + as.factor(muncode)")),
    clusters = meeting_id,
    data = estimation_df) %>%
    tidy %>%
    filter(term == "groupformed_presence_201810")
  
  # first-stage results
 est_first <- iv_robust(
    as.formula(glue::glue("{ var } ~ groupformed_presence_201810 + baseline_var + baseline_var_NA + as.factor(muncode) | Z + baseline_var + baseline_var_NA + as.factor(muncode)")),
    diagnostics = TRUE,
    clusters = meeting_id,
    data = estimation_df) 
  
  first_stage <- est_first$diagnostic_first_stage_fstatistic %>% 
    t %>% 
    as_tibble %>% 
    rename_at(vars(everything()), ~paste0("first_stage_",.))
  
  # test for differential attrition
  missing_est <- lm_robust(
    as.formula(glue::glue("is.na({ var }) ~ Z + baseline_var + baseline_var_NA + as.factor(muncode)")),
    clusters = meeting_id,
    data = estimation_df) %>%
    tidy %>%
    filter(term == "Z") %>% 
    select(missingness_statistic = statistic, missingness_p.value = p.value)
  
  # calculate proportion missing
  missing_prop <- bra_citizen %>% summarize(across(all_of(var), ~mean(is.na(.)), .names = "missingness_proportion"))
  
  # combine estimates for this variable
  estimates_list[[var]] <-
    bind_cols(est, missing_prop, missing_est, first_stage) %>% 
    mutate(across(c(missingness_statistic, missingness_p.value), ~if_else(missingness_proportion == 0, NA_real_, .)))
  
}

for (var in outcomes_admin) {
  estimation_df <- 
    bra_admin %>% 
    filter(municipality != "Florianopolis (x2)") %>% 
    mutate(
      # variable for whether baseline value is missing
      baseline_var_NA = if_else(is.na(!!rlang::sym(paste0(var, "_baseline"))), 1, 0), 
      # replace baseline values with zeros if missing
      baseline_var = replace_na(!!rlang::sym(paste0(var, "_baseline")), 0)
    )
  # treatment effect estimation
  est <- iv_robust(
    as.formula(glue::glue("{ var } ~ groupformed_presence_201810 + baseline_var + baseline_var_NA + as.factor(muncode) | Z + baseline_var + baseline_var_NA + as.factor(muncode)")),
    data = estimation_df) %>%
    tidy %>%
    filter(term == "groupformed_presence_201810")
  
  # first-stage results
  est_first <- iv_robust(
    as.formula(glue::glue("{ var } ~ groupformed_presence_201810 + baseline_var + baseline_var_NA + as.factor(muncode) | Z + baseline_var + baseline_var_NA + as.factor(muncode)")),
    diagnostics = TRUE,
    data = estimation_df) 
  
  first_stage <- est_first$diagnostic_first_stage_fstatistic %>% 
    t %>% 
    as_tibble %>% 
    rename_at(vars(everything()), ~paste0("first_stage_",.))
  
  # test for differential attrition
  missing_est <- lm_robust(
    as.formula(glue::glue("is.na({ var }) ~ Z + baseline_var + baseline_var_NA + as.factor(muncode)")),
    data = estimation_df) %>%
    tidy %>%
    filter(term == "Z") %>% 
    select(missingness_statistic = statistic, missingness_p.value = p.value)
  
  # calculate proportion missing
  missing_prop <- bra_admin %>% summarize(across(all_of(var), ~mean(is.na(.)), .names = "missingness_proportion"))
  
  # combine estimates for this variable
  estimates_list[[var]] <-
    bind_cols(est, missing_prop, missing_est, first_stage) %>% 
    mutate(across(c(missingness_statistic, missingness_p.value), ~if_else(missingness_proportion == 0, NA_real_, .)))
  
}

# admin *crime displacement* analysis
for (var in c("crime_victim_idx_admin")) {
  
  # treatment effect estimation
  est_treatment <- lm_robust(
    as.formula(glue::glue("I({ var } - {var}_baseline) ~ as.factor(muncode)")),
    subset = Z == 1,
    data = bra_admin
  ) %>%
    tidy %>%
    filter(term == "(Intercept)")
  
  est_control <- lm_robust(
    as.formula(glue::glue("I({ var } - {var}_baseline) ~ as.factor(muncode)")),
    subset = Z == 0,
    data = bra_admin
  ) %>%
    tidy %>%
    filter(term == "(Intercept)")
  
  estimates_list[[paste0("displace_treatment_", var)]] <- est_treatment %>% mutate(outcome = paste0("displace_treatment_", var))
  estimates_list[[paste0("displace_control_", var)]] <- est_control %>% mutate(outcome = paste0("displace_control_", var))
}


# citizen *crime displacement* analysis
for (var in c("crime_victim_idx_common")) {

  # treatment effect estimation
  est_treatment <- lm_robust(
    as.formula(glue::glue("I({ var } - {var}_baseline) ~ as.factor(muncode)")),
    # clusters = station_id,
    subset = Z == 1,
    data = bra_citizen
  ) %>%
    tidy %>%
    filter(term == "(Intercept)")

  est_control <- lm_robust(
    as.formula(glue::glue("I({ var } - {var}_baseline) ~ as.factor(muncode)")),
    # clusters = station_id,
    subset = Z == 0,
    data = bra_citizen
  ) %>%
    tidy %>%
    filter(term == "(Intercept)")

  estimates_list[[paste0("displace_treatment_", var)]] <- est_treatment %>% mutate(outcome = paste0("displace_treatment_", var))
  estimates_list[[paste0("displace_control_", var)]] <- est_control %>% mutate(outcome = paste0("displace_control_", var))
}

estimates_df <- 
  bind_rows(estimates_list) %>% 
  mutate(study = "bra") %>% 
  mutate(IV = "presence_201810")

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

estimates_df <- 
  bind_rows(
    estimates_df_primary,
    estimates_df_primary_original,
    estimates_df_primary_listwise,
    estimates_df_secondary_hyp,
    estimates_df_secondary,
    estimates_df_secondary_original,
    estimates_df_secondary_listwise,
    estimates_df_components_primary,
    estimates_df_components_secondary,
    estimates_df_sub_components,
    estimates_df_displacement) %>% 
  select(-label, -hypothesis, -order) %>% 
  distinct(outcome, .keep_all = TRUE) 

saveRDS(estimates_df,  file = "data/out/bra-estimates.RDS")
