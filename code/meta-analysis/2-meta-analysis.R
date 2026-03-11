set.seed(343)
suppressMessages({
  library(tidyverse)
  library(metafor)
  library(glue)
})

source("code/meta-analysis/0-variable-labels.R")

meta_re_estimator <- function(data) {
  results <- try({rma(
    yi = estimate,
    # estimates from each study
    sei = std.error,
    # standard error from each study
    method = "REML",
    # maximum likelihood random-effects meta analysis estimator
    data = data
  )})
  if(!inherits(results, "try-error")) {
    tibble(
      estimate = results$b[, 1],
      std.error = results$se,
      statistic = results$zval,
      p.value = results$pval,
      conf.low = results$ci.lb,
      conf.high = results$ci.ub,
      tausq = results$tau2,
      tausq.std.error = results$se.tau2,
      qtest.p.value = results$QEp,
      N_studies = nrow(data)
    )
  } else {
    tibble(
      estimate = NA, std.error = NA, statistic = NA, p.value = NA, conf.low = NA, conf.high = NA, N_studies = nrow(data)
    )
  }
}

study_estimates_main_hypotheses <- readRDS("data/out/study-estimates-main-hypotheses.RDS")
study_estimates_main_hypotheses_listwise <- readRDS("data/out/study-estimates-main-hypotheses-listwise.RDS")
study_estimates_secondary_hypotheses_listwise <- readRDS("data/out/study-estimates-secondary-hypotheses-listwise.RDS")
study_estimates_all_indices_listwise <- readRDS("data/out/study-estimates-all-indices-listwise.RDS")
study_estimates_main_hypotheses_original <- readRDS("data/out/study-estimates-main-hypotheses-original.RDS")
study_estimates_secondary_hypotheses <- readRDS("data/out/study-estimates-secondary-hypotheses.RDS")
study_estimates_sub_hypotheses <- readRDS("data/out/study-estimates-sub-hypotheses.RDS")
study_estimates_combined <- readRDS("data/out/study-estimates-combined.RDS")
study_estimates_all_components <- readRDS("data/out/study-estimates-all-components.RDS")

suppressWarnings({
  meta_estimates_main_hypotheses <-
    study_estimates_main_hypotheses %>% 
    group_by(hypothesis, label)  %>%
    do(meta_re_estimator(.)) %>%
    ungroup()
  
  meta_estimates_main_hypotheses_original <-
    study_estimates_main_hypotheses_original %>%
    group_by(hypothesis, label)  %>%
    do(meta_re_estimator(.)) %>%
    ungroup()
  
  meta_estimates_main_hypotheses_listwise <-
    study_estimates_main_hypotheses_listwise %>%
    group_by(hypothesis, label)  %>%
    do(meta_re_estimator(.)) %>%
    ungroup()
  
  meta_estimates_secondary_hypotheses_listwise <-
    study_estimates_secondary_hypotheses_listwise %>%
    group_by(hypothesis, label)  %>%
    do(meta_re_estimator(.)) %>%
    ungroup()
  
  meta_estimates_all_indices_listwise <-
    study_estimates_all_indices_listwise %>%
    group_by(hypothesis, label)  %>%
    do(meta_re_estimator(.)) %>%
    ungroup()
  
  meta_estimates_secondary_hypotheses <-
    study_estimates_secondary_hypotheses %>%
    group_by(hypothesis, label, outcome, order)  %>%
    do(meta_re_estimator(.)) %>%
    ungroup()

  meta_estimates_main_hypotheses_primary <- 
    meta_estimates_main_hypotheses %>% 
    filter(hypothesis %in% c("1a", "1b", "2", "3a", "3b", "4a", "4b", "4c")) %>% 
    mutate(p.value.adj = p.adjust(p.value, method = "BH"))

  meta_estimates_main_hypotheses_secondary <- 
    meta_estimates_main_hypotheses %>% 
    filter(!hypothesis %in% c("1a", "1b", "2", "3a", "3b", "4a", "4b", "4c"))
  
  meta_estimates_main_hypotheses <- 
    bind_rows(
      meta_estimates_main_hypotheses_primary,
      meta_estimates_main_hypotheses_secondary)
  
  meta_estimates_main_hypotheses_primary_original <- 
    meta_estimates_main_hypotheses_original %>% 
    filter(hypothesis %in% c("1a", "1b", "2", "3a", "3b", "4a", "4b", "4c")) %>% 
    mutate(p.value.adj = p.adjust(p.value, method = "BH"))
  
  meta_estimates_main_hypotheses_secondary_original <- 
    meta_estimates_main_hypotheses_original %>% 
    filter(!hypothesis %in% c("1a", "1b", "2", "3a", "3b", "4a", "4b", "4c"))
  
  meta_estimates_main_hypotheses_original <- 
    bind_rows(
      meta_estimates_main_hypotheses_primary_original,
      meta_estimates_main_hypotheses_secondary_original)
  
  meta_estimates_main_hypotheses_primary_listwise <- 
    meta_estimates_main_hypotheses_listwise %>% 
    filter(hypothesis %in% c("1a", "1b", "2", "3a", "3b", "4a", "4b", "4c")) %>% 
    mutate(p.value.adj = p.adjust(p.value, method = "BH"))
  
  meta_estimates_main_hypotheses_secondary_listwise <- 
    meta_estimates_main_hypotheses_listwise %>% 
    filter(!hypothesis %in% c("1a", "1b", "2", "3a", "3b", "4a", "4b", "4c"))
  
  meta_estimates_all_indices_primary_listwise <- 
    meta_estimates_all_indices_listwise %>% 
    filter(hypothesis %in% c("1a", "1b", "2", "3a", "3b", "4a", "4b", "4c")) %>% 
    mutate(p.value.adj = p.adjust(p.value, method = "BH"))
  
  meta_estimates_all_indices_secondary_listwise <- 
    meta_estimates_all_indices_listwise %>% 
    filter(!hypothesis %in% c("1a", "1b", "2", "3a", "3b", "4a", "4b", "4c"))
  
  
  meta_estimates_main_hypotheses_listwise <- 
    bind_rows(
      meta_estimates_main_hypotheses_primary_listwise,
      meta_estimates_main_hypotheses_secondary_listwise)
  
  meta_estimates_main_hypotheses_listwise <- 
    bind_rows(
      meta_estimates_all_indices_primary_listwise,
      meta_estimates_all_indices_secondary_listwise)

  meta_estimates_sub_hypotheses <- 
    study_estimates_sub_hypotheses %>%
    group_by(outcome, label_component, idx)  %>%
    do(meta_re_estimator(.)) %>%
    ungroup %>%
    rename(idx_old = idx, label_component_old = label_component) %>% 
    left_join(sub_hypotheses, by = "outcome")
  
  meta_estimates_sub_hypotheses_primary <- 
    meta_estimates_sub_hypotheses %>% 
    filter(hypothesis %in% c("1a", "1b", "2", "3a", "3b", "4a", "4b", "4c")) %>% 
    group_by(hypothesis) %>% 
    mutate(p.value.adj = p.adjust(p.value, method = "BH")) %>% 
    ungroup()
  
  meta_estimates_sub_hypotheses_secondary <- 
    meta_estimates_sub_hypotheses %>% 
    filter(!hypothesis %in% c("1a", "1b", "2", "3a", "3b", "4a", "4b", "4c"))
  
  meta_estimates_sub_hypotheses <- 
    bind_rows(
      meta_estimates_sub_hypotheses_primary,
      meta_estimates_sub_hypotheses_secondary)

  meta_estimates_all_components <- 
    study_estimates_combined %>%
    group_by(outcome)  %>%
    do(meta_re_estimator(.)) %>%
    ungroup %>%
    inner_join(all_components, by = "outcome") %>%
    left_join(meta_estimates_sub_hypotheses %>% select(outcome, p.value.adj), by = "outcome") %>% 
    mutate(order = as.numeric(order))
})

write_rds(meta_estimates_main_hypotheses, file = "data/out/meta-estimates-main-hypotheses.RDS")
write_rds(meta_estimates_main_hypotheses_original, file = "data/out/meta-estimates-main-hypotheses-original.RDS")
write_rds(meta_estimates_main_hypotheses_listwise, file = "data/out/meta-estimates-main-hypotheses-listwise.RDS")
write_rds(meta_estimates_secondary_hypotheses_listwise, file = "data/out/meta-estimates-secondary-hypotheses-listwise.RDS")
write_rds(meta_estimates_all_indices_listwise, file = "data/out/meta-estimates-all-indices-listwise.RDS")
write_rds(meta_estimates_secondary_hypotheses, file = "data/out/meta-estimates-secondary-hypotheses.RDS")
write_rds(meta_estimates_sub_hypotheses, file = "data/out/meta-estimates-sub-hypotheses.RDS")
write_rds(meta_estimates_all_components, file = "data/out/meta-estimates-all-components.RDS")
write_rds(study_estimates_main_hypotheses, file = "data/out/study-estimates-main-hypotheses.RDS")
write_rds(study_estimates_secondary_hypotheses, file = "data/out/study-estimates-secondary-hypotheses.RDS")
write_rds(study_estimates_all_components, file = "data/out/study-estimates-all-components.RDS")
