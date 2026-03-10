
#  All loaded packages should come here
suppressMessages({
  library(tidyverse)
  library(DeclareDesign)
})

lbr_citizen <- readRDS("data/out/lbr-citizen-construct.RDS")
lbr_admin <- readRDS("data/out/lbr-admin-construct.RDS")
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
    "police_abuse_report_idx_listwise",
    "intentions_idx_listwise",
    "norm_idx_listwise",
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
    # "other_any_nonviolent_std",

    "cviolentcrime_num_std",
    "carmedrob_num_std",
    "caggassault_num_std",
    "csimpleassault_num_std",
    "csexual_num_std",
    "cdomestic_phys_num_std",
    "cmurder_num_std",
    "cother_any_violent_std",

    "cnonviolentcrime_num_std",
    "cburglary_num_std",
    "cother_any_nonviolent_std",

    # # 1b
    "fear_violent_std",
    "fear_nonviolent_std",
    "feared_walk_std",

    # # 2
    "satis_trust_std",
    "satis_general_std",

    # # 3b
    "policeabuse_any_std",
    "policeabuse_num_std",
    "bribe_freq_std",
    "bribe_amt_std",

    # # 4a
    "violentcrime_report_num_std",
    "nonviolentcrime_report_num_std",
    "cviolentcrime_report_num_std",
    "cnonviolentcrime_report_num_std",

    "crimeres_idx",
    "burglaryres_std",
    "dviolres_std",
    "armedrobres_std",
    #
    "armedrob_report_std",
    "simpleassault_report_std",
    # "other_report_violent_std",
    "burglary_report_std",
    "carmedrob_report_std",
    "caggassault_report_std",
    "csimpleassault_report_std",
    "csexual_report_std",
    "cdomestic_phys_report_std",
    # "cother_report_violent_std",
    "cburglary_report_std",

    # # 4b
    "crime_tips_idx",
    "contact_pol_susp_activity_std",
    "give_info_pol_investigation_std",

    # # 4c
    "policebeating_report_std",
    "policeabuse_report_std",
    "dutydrink_report_std",

    # # M1a
    "polcaseserious_std",
    "polcasefair_std",
    "polint_idx",
    "polint_corrupt_std",
    "polcasefair_std",

    # # M1b
    "know_law_idx",
    # "know_report_idx",
    "know_law_suspect_std",
    "know_law_lawyer_std",
    "know_law_fees_std",
    #
    # # M1c
    "reportnorm_theft_std",
    "reportnorm_abuse_std",
    "obeynorm_std",

    # # M2a
    "polcap_timely_std",
    "polcap_investigate_std",

    # # C
    "compliance_patrol_std",
    "compliance_freq_std",
    "compliance_meeting_std",

    # Hyp 1a. (alt. ii)
    "violentcrime_num_exp",
    "nonviolentcrime_num_exp",
    "cviolentcrime_num_exp",
    "cnonviolentcrime_num_exp",

    "armedrob_num_std",
    "aggassault_num_std",
    "sexual_num_std",
    "domestic_phys_num_std",
    "simpleassault_num_std",
    "other_any_violent_std",
    "burglary_num_std",
    "domestic_verbal_num_std",
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
    "cdomestic_verbal_num_std",
    "cother_any_nonviolent_std"
    )

outcomes_admin <-
  c(# indices
    "crime_victim_idx_admin",
    # constituents
    "anonviolentcrime_num_std",
    "aviolentcrime_num_std",
    
    "aarmedrob_num_std",
    # "asimpleassault_num_std",
    "aaggassault_num_std",
    "asexual_num_std",
    # "adomestic_phys_num_std",
    "amurder_num_std",
    "aburglary_num_std",
    # listwise
    "crime_victim_idx_admin"
    )

# citizen survey outcomes
estimates_list <- list()
for (var in outcomes_citizen) {
  estimation_df <- 
    lbr_citizen %>%
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
      baseline_het_var_crime_NA = if_else(is.na(crime_victim_idx_baseline), 1, 0), 
      # replace baseline values with zeros if missing
      baseline_het_var_crime = replace_na(crime_victim_idx_baseline, 0)
    )
  estimates_list[[paste0(var, "_het_trust")]] <- lm_robust(
    as.formula(glue::glue("{ var } ~ Z * (baseline_het_var_trust + baseline_het_var_trust_NA) + baseline_var + baseline_var_NA + as.factor(police_zones)")),
    clusters = communities,
    weights = 1 / S_communities_inclusion_prob * S_citizens_inclusion_prob,
    data = estimation_df) %>%
    tidy %>%
    filter(term == "Z:baseline_het_var_trust")
  
  estimates_list[[paste0(var, "_het_trustcom")]] <- lm_robust(
    as.formula(glue::glue("{ var } ~ Z * (baseline_het_var_trustcom + baseline_het_var_trustcom_NA) + baseline_var + baseline_var_NA + as.factor(police_zones)")),
    clusters = communities,
    weights = 1 / S_communities_inclusion_prob * S_citizens_inclusion_prob,
    data = estimation_df) %>%
    tidy %>%
    filter(term == "Z:baseline_het_var_trustcom")
  
  estimates_list[[paste0(var, "_het_legit")]] <- lm_robust(
    as.formula(glue::glue("{ var } ~ Z * (baseline_het_var_legit + baseline_het_var_legit_NA) + baseline_var + baseline_var_NA + as.factor(police_zones)")),
    clusters = communities,
    weights = 1 / S_communities_inclusion_prob * S_citizens_inclusion_prob,
    data = estimation_df) %>%
    tidy %>%
    filter(term == "Z:baseline_het_var_legit")
  
  estimates_list[[paste0(var, "_het_crime")]] <- lm_robust(
    as.formula(glue::glue("{ var } ~ Z * (baseline_het_var_crime + baseline_het_var_crime_NA) + baseline_var + baseline_var_NA + as.factor(police_zones)")),
    clusters = communities,
    weights = 1 / S_communities_inclusion_prob * S_citizens_inclusion_prob,
    data = estimation_df) %>%
    tidy %>%
    filter(term == "Z:baseline_het_var_crime")
  
}

estimates_df <- 
  bind_rows(estimates_list) %>% 
  mutate(study = "lbr")

saveRDS(estimates_df,  file = "data/out/lbr-estimates-het.RDS")
