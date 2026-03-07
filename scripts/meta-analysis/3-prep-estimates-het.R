suppressMessages({
  library(tidyverse)
  library(metafor)
  library(glue)
})

bra_estimates <- readRDS("data/out/bra-estimates-het.RDS")
col_estimates_het <- readRDS("data/out/col-estimates-het.RDS")
lbr_estimates_het <- readRDS("data/out/lbr-estimates-het.RDS")
pak_estimates_het <- readRDS("data/out/pak-estimates-het.RDS")
uga_estimates_het <- readRDS("data/out/uga-estimates-het.RDS")

source("code/meta-analysis/0-variable-labels.R")

study_estimates_combined <-
  bind_rows(
    bra_estimates,
    col_estimates_het,
    lbr_estimates_het,
    pak_estimates_het,
    uga_estimates_het)

study_estimates_main_hypotheses <-
  study_estimates_combined %>%
  inner_join(main_hypotheses, by = "outcome")

study_estimates_main_hypotheses_correct <- 
  study_estimates_main_hypotheses %>% 
  filter(hypothesis %in% c("1a", "1b", "2", "3a", "3b", "4a", "4b", "4c")) %>% 
  group_by(study) %>% 
  mutate(p.value.adj = p.adjust(p.value, method = "BH")) %>% 
  ungroup

study_estimates_main_hypotheses_not_correct <- 
  study_estimates_main_hypotheses %>% 
  filter(!hypothesis %in% c("1a", "1b", "2", "3a", "3b", "4a", "4b", "4c"))

study_estimates_main_hypotheses <- 
  bind_rows(study_estimates_main_hypotheses_correct,
            study_estimates_main_hypotheses_not_correct) %>% 
  mutate(het_effects = case_when(
    term == "Z_common:baseline_het_var_trust" ~ "satis_trust", 
    term == "Z_common:baseline_het_var_crime" ~ "crime", 
    term == "Z_common:baseline_het_var_trustcom" ~ "trust_community", 
    term == "Z_common:baseline_het_var_legit" ~ "legit_trust"))

write_rds(study_estimates_main_hypotheses, file = "data/out/study-estimates-main-hypotheses-het.RDS")