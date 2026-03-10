
#  All loaded packages should come here
suppressMessages({
  library(tidyverse)
  library(DeclareDesign)
  library(ri2)
})

set.seed(42)

col_citizen <- readRDS("data/out/col-citizen-construct.RDS")

source("code/meta-analysis/0-variable-labels.R")

main_outcomes <- main_hypotheses_original %>% slice(1:8) %>% filter(outcome != "officer_attitude_idx") %>% pull(outcome)

col_citizen <- readRDS("data/out/col-citizen-construct.RDS")

col_variance_df <- col_citizen %>% 
  select(cuadrante, block_ID, Z, Z_common, any_of(main_outcomes)) %>% 
  na.omit

col_declaration <- 
  with(col_variance_df, {
    declare_ra(
      blocks = block_ID,
      clusters = cuadrante,
      conditions = 0:3
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
    declaration = col_declaration,
    assignment = "Z_common",
    sharp_hypothesis = 0,
    sims = 1000,
    data = data)
}

col_var_test_results <- col_variance_df %>% 
  select(cuadrante, Z_common, any_of(main_outcomes)) %>% 
  pivot_longer(cols = any_of(main_outcomes)) %>% 
  group_by(name) %>% 
  do(summary(ri_f_var_test(data = .))) %>% 
  ungroup

saveRDS(col_var_test_results, file = "data/out/col-var-test.RDS")
