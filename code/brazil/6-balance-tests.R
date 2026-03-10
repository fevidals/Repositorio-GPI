
#  All loaded packages should come here
suppressMessages({
  library(tidyverse)
  library(DeclareDesign)
  library(ri2)
})

set.seed(42)

bra_citizen <- readRDS("data/out/bra-citizen-construct.RDS")

balance_vars <- c("crime_victim_idx_common_baseline", 
                  # "future_insecurity_idx_common_baseline", 
                  "satis_idx_baseline", 
                  # "police_abuse_idx_common_baseline", 
                  "crime_reporting_idx_common_baseline", 
                  "tips_idx_baseline")
                  # "police_abuse_report_idx_common_baseline")

bra_balance_df <- bra_citizen %>% 
  select(meeting_id, muncode, Z, balance_vars) %>% 
  na.omit

bra_declaration <- 
  with(bra_balance_df,{
    declare_ra(
      blocks = muncode,
      clusters = meeting_id,
      conditions = 0:1
    )
  })

regression_fun <- function(data) {
  lm_robust(as.formula(paste0("Z ~ as.factor(muncode) + ", paste0(balance_vars, collapse = "+"))), clusters = meeting_id, data = data)
}

balance_fun <- function(data) {
  data %>% regression_fun %>% glance %>% pull(statistic)
}

balance_fun(bra_balance_df)

ri2_out <-
  conduct_ri(
    test_function = balance_fun,
    declaration = bra_declaration,
    assignment = "Z",
    sharp_hypothesis = 0,
    sims = 1000,
    data = bra_balance_df)

ri2_summ <- summary(ri2_out) %>% rename_with(~paste0(., "_F_ri"))

bra_balance_results <- bra_balance_df %>% 
  regression_fun() %>% 
  tidy %>% 
  filter(term %in% balance_vars) %>% 
  bind_cols(ri2_summ)

saveRDS(bra_balance_results, file = "data/out/bra-balance.RDS")




