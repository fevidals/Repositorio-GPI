
#  All loaded packages should come here
suppressMessages({
  library(tidyverse)
  library(DeclareDesign)
  library(ri2)
})

set.seed(42)

phl_citizen <- readRDS("data/out/phl-citizen-construct.RDS")

source("code/meta-analysis/0-variable-labels.R")

main_outcomes <- main_hypotheses_original %>% slice(1:8) %>% filter(outcome != "officer_attitude_idx") %>% pull(outcome)

phl_citizen <- readRDS("data/out/phl-citizen-construct.RDS")

phl_variance_df <- phl_citizen %>% 
  select(psgc, strata, Z_common, any_of(main_outcomes)) %>% 
  na.omit

phl_declaration <- 
  with(phl_variance_df, {
    declare_ra(
      blocks = strata,
      clusters = psgc,
      conditions = 0:2
    )
  })

tidy_f_var_test <- function(data, var = "value", treat = "Z_common") {
  data <- as.data.frame(data)
  val <- try({var.test(data[data[, treat] == 1, var], data[data[, treat] == 0, var], alternative = "two.sided")})
  if(inherits(val, "try-error")){
    NA_real_
  } else {
    val$estimate
  }
}

ri_f_var_test <- function(data) {
  conduct_ri(
    test_function = tidy_f_var_test,
    declaration = phl_declaration,
    assignment = "Z_common",
    sharp_hypothesis = 0,
    sims = 1000,
    data = data)
}

phl_var_test_results <- phl_variance_df %>% 
  select(psgc, strata, Z_common, any_of(main_outcomes)) %>% 
  pivot_longer(cols = any_of(main_outcomes)) %>% 
  group_by(name) %>% 
  do(summary(ri_f_var_test(data = .))) %>% 
  ungroup

saveRDS(phl_var_test_results, file = "data/out/phl-var-test.RDS")
