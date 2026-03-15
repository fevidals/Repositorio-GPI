
#  All loaded packages should come here
suppressMessages({
  library(tidyverse)
  library(DeclareDesign)
  library(ri2)
  library(nnet)
  library(lmtest)
})

set.seed(42)

pak_citizen <- readRDS("data/out/pak-citizen-construct.RDS")

balance_vars <- c("crime_victim_idx_common_baseline", 
                  "future_insecurity_idx_common_baseline", 
                  "satis_idx_baseline", 
                  "police_abuse_idx_common_baseline", 
                  "crime_reporting_idx_common_baseline", 
                  "tips_idx_baseline", 
                  "police_abuse_report_idx_common_baseline")

pak_balance_df <- pak_citizen %>% 
  select(beats, stations, Z, balance_vars) %>% 
  na.omit

pak_declaration <- 
  with(pak_balance_df,{
    declare_ra(
      blocks = stations,
      clusters = beats,
      conditions = 0:2
    )
  })

regression_fun <- function(data) {
  multinom(as.formula(paste0("Z ~ as.factor(stations) + ", paste0(balance_vars, collapse = "+"))), data = data)
}

balance_fun <- function(data) {
  fit1 <- multinom(Z ~ 1, data = data)
  fit2 <- regression_fun(data)
  lrtest(fit1, fit2)$Chisq[2]
}

balance_fun(pak_balance_df)

ri2_out <-
  conduct_ri(
    test_function = balance_fun,
    declaration = pak_declaration,
    assignment = "Z",
    sharp_hypothesis = 0,
    sims = 1000,
    data = pak_balance_df)

ri2_summ <- summary(ri2_out) %>% rename_with(~paste0(., "_F_ri"))

pak_balance_results <- pak_balance_df %>% 
  regression_fun() %>% 
  tidy(conf.int = TRUE) %>% 
  filter(term %in% balance_vars) %>% 
  bind_cols(ri2_summ)

saveRDS(pak_balance_results, file = "data/out/pak-balance.RDS")
