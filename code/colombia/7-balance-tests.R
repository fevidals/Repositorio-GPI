
#  All loaded packages should come here
suppressMessages({
  library(tidyverse)
  library(DeclareDesign)
  library(ri2)
  library(nnet)
  library(lmtest)
})

set.seed(42)

col_citizen <- readRDS("data/out/col-citizen-construct.RDS")

balance_vars <- c("crime_victim_idx_common_baseline", 
                  "future_insecurity_idx_common_baseline", 
                  "satis_idx_baseline", 
                  "police_abuse_idx_common_baseline", 
                  "crime_reporting_idx_common_baseline", 
                  "tips_idx_baseline", 
                  "police_abuse_report_idx_common_baseline")

col_balance_df <- col_citizen %>% 
  select(cuadrante, block_ID, Z, balance_vars) %>% 
  na.omit

col_declaration <- 
  with(col_balance_df,{
    declare_ra(
      blocks = block_ID,
      clusters = cuadrante,
      conditions = 0:3
    )
  })

regression_fun <- function(data){
  multinom(as.formula(paste0("Z ~ ", paste0(balance_vars, collapse = "+"))), data = data)
}

balance_fun <- function(data) {
  fit1 <- multinom(Z ~ 1, data = data)
  fit2 <- regression_fun(data)
  lrtest(fit1, fit2)$Chisq[2]
}

# balance_fun(col_balance_df)

ri2_out <-
  conduct_ri(
    test_function = balance_fun,
    declaration = col_declaration,
    assignment = "Z",
    sharp_hypothesis = 0,
    sims = 1000,
    data = col_balance_df)

ri2_summ <- summary(ri2_out) %>% rename_with(~paste0(., "_F_ri"))

col_balance_results <- col_balance_df %>% 
  regression_fun() %>% 
  tidy(conf.int = TRUE) %>% 
  filter(term %in% balance_vars) %>% 
  bind_cols(ri2_summ)

saveRDS(col_balance_results, file = "data/out/col-balance.RDS")

  