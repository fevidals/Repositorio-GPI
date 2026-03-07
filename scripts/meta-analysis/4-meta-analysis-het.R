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
      conf.high = results$ci.ub
    )
  } else {
    tibble(
      estimate = NA, std.error = NA, statistic = NA, p.value = NA, conf.low = NA, conf.high = NA
    )
  }
}

study_estimates_main_hypotheses <- readRDS("data/out/study-estimates-main-hypotheses-het.RDS")

suppressWarnings({
  meta_estimates_main_hypotheses <-
    study_estimates_main_hypotheses %>%
    group_by(het_effects, hypothesis, label)  %>%
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
})

write_rds(meta_estimates_main_hypotheses, file = "data/out/meta-estimates-main-hypotheses-het.RDS")
write_rds(study_estimates_main_hypotheses, file = "data/out/study-estimates-main-hypotheses-het.RDS")
