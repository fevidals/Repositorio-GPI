
#  All loaded packages should come here
suppressMessages({
  library(tidyverse)
  library(DeclareDesign)
  library(ri2)
  library(nnet)
  library(lmtest)
})

set.seed(42)

uga_citizen <- readRDS("data/out/uga-citizen-construct.RDS")

balance_vars <- c("crime_victim_idx_common_baseline", 
                  "future_insecurity_idx_common_baseline", 
                  "satis_idx_baseline", 
                  "police_abuse_idx_common_baseline", 
                  "crime_reporting_idx_common_baseline", 
                  "tips_idx_baseline", 
                  "police_abuse_report_idx_common_baseline")

uga_balance_df <- uga_citizen %>% 
  select(station_id, block_ID, Z_common, all_of(balance_vars)) %>% 
  na.omit

uga_declaration <- 
  with(uga_balance_df,{
    declare_ra(
      blocks = block_ID,
      clusters = station_id,
      conditions = 0:1
    )
  })

regression_fun <- function(data) {
  lm_robust(as.formula(paste0("Z_common ~ as.factor(block_ID) + ", paste0(balance_vars, collapse = "+"))), clusters = station_id, data = data)
}

balance_fun <- function(data) {
  data %>% regression_fun %>% glance %>% pull(statistic)
}

balance_fun(uga_balance_df)

ri2_out <-
  conduct_ri(
    test_function = balance_fun,
    declaration = uga_declaration,
    assignment = "Z_common",
    sharp_hypothesis = 0,
    sims = 1000,
    data = uga_balance_df)

ri2_summ <- summary(ri2_out) %>% rename_with(~paste0(., "_F_ri"))

uga_balance_results <- uga_balance_df %>% 
  regression_fun() %>% 
  tidy %>% 
  filter(term %in% balance_vars) %>% 
  bind_cols(ri2_summ)

saveRDS(uga_balance_results, file = "data/out/uga-balance.RDS")

