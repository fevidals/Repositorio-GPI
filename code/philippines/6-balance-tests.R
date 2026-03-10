
#  All loaded packages should come here
suppressMessages({
  library(tidyverse)
  library(DeclareDesign)
  library(ri2)
  library(nnet)
  library(lmtest)
})

set.seed(42)

source("code/shared/index-construction.R")

phl_units <- readRDS("data/out/phl-unit-clean.RDS") %>% 
  filter(!is.na(Z))

balance_vars <- c("crime_rate_std", "baseline_surveyed")

phl_balance_df <- phl_units %>% 
  mutate(municipality = as.numeric(as.factor(municipality)),
         Z = case_when(
           cep_group == "Control" ~ 0L,
           cep_group == "CEP_with_SMStipline" ~ 1L,
           cep_group == "CEP_no_SMStipline" ~ 2L
         )) %>%
  mutate(
    crime_rate_std = stdize(crime_rate, to = crime_rate, condition = Z_common == 0)
  ) %>% 
  replace_na(list(baseline_surveyed = 0)) %>% 
  select(psgc, municipality, crime_rate_q4, baseline_surveyed, Z, all_of(balance_vars)) %>% 
  na.omit %>% 
  mutate(strata = paste(municipality, crime_rate_q4, baseline_surveyed)) 

phl_declaration <- 
  with(phl_balance_df,{
    declare_ra(
      blocks = strata,
      clusters = psgc,
      conditions = 0:2
    )
  })

regression_fun <- function(data) {
  multinom(as.formula(paste0("Z ~ ", paste0(balance_vars, collapse = "+"))), data = data)
}

balance_fun <- function(data) {
  fit1 <- multinom(Z ~ 1, data = data)
  fit2 <- regression_fun(data)
  lrtest(fit1, fit2)$Chisq[2]
}

balance_fun(phl_balance_df)
regression_fun(phl_balance_df)

ri2_out <-
  conduct_ri(
    test_function = balance_fun,
    declaration = phl_declaration,
    assignment = "Z",
    sharp_hypothesis = 0,
    sims = 1000,
    data = phl_balance_df)

ri2_summ <- summary(ri2_out) %>% rename_with(~paste0(., "_F_ri"))

phl_balance_results <- phl_balance_df %>% 
  regression_fun() %>% 
  tidy(conf.int = TRUE) %>% 
  filter(term %in% balance_vars) %>% 
  bind_cols(ri2_summ)

saveRDS(phl_balance_results, file = "data/out/phl-balance.RDS")

