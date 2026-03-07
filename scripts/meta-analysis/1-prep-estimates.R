suppressMessages({
  library(tidyverse)
  library(metafor)
  library(glue)
})

bra_estimates <- readRDS("data/out/bra-estimates.RDS")
col_estimates <- readRDS("data/out/col-estimates.RDS")
lbr_estimates <- readRDS("data/out/lbr-estimates.RDS")
pak_estimates <- readRDS("data/out/pak-estimates.RDS")
phl_estimates <- readRDS("data/out/phl-estimates.RDS")
uga_estimates <- readRDS("data/out/uga-estimates.RDS")

source("code/meta-analysis/0-variable-labels.R")

study_estimates_combined <-
  bind_rows(
    bra_estimates %>% filter(IV == "presence_201810"),
    col_estimates,
    lbr_estimates,
    pak_estimates,
    phl_estimates,
    uga_estimates
  )

study_estimates_main_hypotheses <-
  study_estimates_combined %>%
  inner_join(main_hypotheses, by = "outcome")

study_estimates_main_hypotheses_original <-
  study_estimates_combined %>%
  inner_join(main_hypotheses_original, by = "outcome")

study_estimates_main_hypotheses_listwise <-
  study_estimates_combined %>%
  inner_join(main_hypotheses_listwise, by = "outcome")

study_estimates_secondary_hypotheses_listwise <-
  study_estimates_combined %>%
  inner_join(secondary_hypotheses_listwise, by = "outcome")

study_estimates_all_indices_listwise <-
  study_estimates_combined %>%
  inner_join(all_indices_listwise, by = "outcome")

study_estimates_secondary_hypotheses <-
  study_estimates_combined %>%
  inner_join(secondary_hypotheses, by = "outcome")

study_estimates_sub_hypotheses <-
  study_estimates_combined %>%
  inner_join(sub_hypotheses, by = "outcome")

study_estimates_all_components <-
  study_estimates_combined %>%
  full_join(all_components, by = "outcome")

write_rds(study_estimates_main_hypotheses, file = "data/out/study-estimates-main-hypotheses.RDS")
write_rds(study_estimates_main_hypotheses_original, file = "data/out/study-estimates-main-hypotheses-original.RDS")
write_rds(study_estimates_main_hypotheses_listwise, file = "data/out/study-estimates-main-hypotheses-listwise.RDS")
write_rds(study_estimates_secondary_hypotheses_listwise, file = "data/out/study-estimates-secondary-hypotheses-listwise.RDS")
write_rds(study_estimates_all_indices_listwise, file = "data/out/study-estimates-all-indices-listwise.RDS")
write_rds(study_estimates_sub_hypotheses, file = "data/out/study-estimates-sub-hypotheses.RDS")
write_rds(study_estimates_combined, file = "data/out/study-estimates-combined.RDS")
write_rds(study_estimates_all_components, file = "data/out/study-estimates-all-components.RDS")
write_rds(study_estimates_secondary_hypotheses, file = "data/out/study-estimates-secondary-hypotheses.RDS")
