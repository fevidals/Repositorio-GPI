
#  All loaded packages should come here
suppressMessages({
  library(tidyverse)
  library(DeclareDesign)
  library(ri2)
})

set.seed(42)

lbr_citizen <- readRDS("data/out/lbr-citizen-construct.RDS")

balance_vars <- c("crime_victim_idx_common_baseline", 
                  "future_insecurity_idx_common_baseline", 
                  "satis_idx_baseline", 
                  "police_abuse_idx_common_baseline", 
                  "crime_reporting_idx_common_baseline", 
                  "tips_idx_baseline", 
                  "police_abuse_report_idx_common_baseline")

lbr_balance_df <- lbr_citizen %>% 
  select(communities, police_zones, Z, balance_vars) %>% 
  na.omit

lbr_declaration <- 
  with(lbr_balance_df,{
    declare_ra(
      blocks = police_zones,
      clusters = communities,
      conditions = 0:1
    )
  })

regression_fun <- function(data) {
  lm_robust(as.formula(paste0("Z ~ as.factor(police_zones) + ", paste0(balance_vars, collapse = "+"))), clusters = communities, data = data)
}

balance_fun <- function(data) {
  glance(regression_fun(data)) %>% pull(statistic)
}

balance_fun(lbr_balance_df)

ri2_out <-
  conduct_ri(
    test_function = balance_fun,
    declaration = lbr_declaration,
    assignment = "Z",
    sharp_hypothesis = 0,
    sims = 1000,
    data = lbr_balance_df)

ri2_out$sims_df

ri2_summ <- summary(ri2_out) %>% rename_with(~paste0(., "_F_ri"))

lbr_balance_results <- lbr_balance_df %>% 
  regression_fun() %>% 
  tidy %>% 
  filter(term %in% balance_vars) %>% 
  bind_cols(ri2_summ)

saveRDS(lbr_balance_results, file = "data/out/lbr-balance.RDS")



